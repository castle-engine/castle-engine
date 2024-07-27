{
  Copyright 2001-2024 Michalis Kamburelis, Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Recognize touch gestures. }
unit CastleInternalCameraGestures;

{$I castleconf.inc}

interface

uses Classes,
  CastleKeysMouse, CastleVectors;

type
  TCastleGestureType = (gtNone, gtPinch, gtPan);
  TCastleGestureRecognizerState = (grstInvalid, grstStarted, grstUpdate, grstFinished);

  { This gesture recognizer detects pan and pinch gesture, as both use two fingers,
    but cannot be used at the same time.

    Pass the input events using Press, Motion and Release functions, listen
    to recognized gestures in @link(OnGestureChanged) event. }
  TCastlePinchPanGestureRecognizer = class
  strict private
    FGesture: TCastleGestureType;
    FState: TCastleGestureRecognizerState;
    FPanOldOffset, FPanOffset: TVector2;
    FPinchScaleFactor: Single;
    //FPinchCenter: TVector2;

    FOnGestureChanged: TNotifyEvent;

    FFinger0Pressed, FFinger1Pressed: boolean;
    // stored position of the fingers, as we get only one of them in Motion event
    FFinger0Pos, FFinger1Pos: TVector2;
    // gesture start finger positions
    FFinger0StartPos, FFinger1StartPos: TVector2;

    { Recognizer state. When not detected any gesture, it is in grstInvalid,
      grstStarted is when the gesture is first recognized, grstFinished is the
      last event of the recognized gesture, grstUpdate are all events between
      started and finished. }
    RecognizerState: TCastleGestureRecognizerState;
  public
    constructor Create;

    { Functions to pass the input to the recognizer from some @link(TCastleUserInterface).
      @groupBegin }
    function Press(const Event: TInputPressRelease): boolean;
    function Release(const Event: TInputPressRelease): boolean;
    function Motion(const Event: TInputMotion; const Dpi: Single): boolean;
    { @groupEnd }

    { Gesture type once it's recognized. Check it inside OnGestureChanged event. }
    property Gesture: TCastleGestureType read FGesture;

    { Offset of the current pan gesture.
      To get the actual change, you have to calculate PanOffset - PanOldOffset. }
    property PanOffset: TVector2 read FPanOffset;

    { Previous pan gesture offset. }
    property PanOldOffset: TVector2 read FPanOldOffset;

    { Scale factor of the pinch gesture.
      - < 1.0 means zoom out
      - 1.0 means no change
      - > 1.0 means zoom in. }
    property PinchScaleFactor: Single read FPinchScaleFactor;

    { Coordinates of the pinch gesture center. }
    // unused
    //property PinchCenter: TVector2 read FPinchCenter;

    { Listen to this event to receive updates on recognized gestures. }
    property OnGestureChanged: TNotifyEvent
      read FOnGestureChanged write FOnGestureChanged;
  end;

implementation

uses Math,
  CastleUtils;

{ TCastlePinchPanGestureRecognizer ------------------------------------------- }

constructor TCastlePinchPanGestureRecognizer.Create;
begin
  inherited;
  FGesture := gtNone;
  FState := grstInvalid;
  FOnGestureChanged := nil;
  FFinger0Pressed := false;
  FFinger1Pressed := false;
end;

function TCastlePinchPanGestureRecognizer.Press(const Event: TInputPressRelease): boolean;
begin
  if Event.FingerIndex = 0 then
  begin
    FFinger0StartPos := Event.Position;
    FFinger0Pos := Event.Position;
    FFinger0Pressed := true;
  end else
  if Event.FingerIndex = 1 then
  begin
    FFinger1StartPos := Event.Position;
    FFinger1Pos := Event.Position;
    FFinger1Pressed := true;
  end;
  Result := FFinger0Pressed and FFinger1Pressed;
end;

function TCastlePinchPanGestureRecognizer.Release(const Event: TInputPressRelease): boolean;
begin
  Result := false;

  if Event.FingerIndex = 0 then
    FFinger0Pressed := false
  else
  if Event.FingerIndex = 1 then
    FFinger1Pressed := false;

  // end gesture when any finger up
  if FState <> grstInvalid then
  begin
    if Assigned(FOnGestureChanged) then
    begin
      // send 'Finished' event
      if Gesture = gtPinch then
        FPinchScaleFactor := 1.0
      else
      if Gesture = gtPan then
        FPanOffset := FPanOldOffset;
      FState := grstFinished;
      FOnGestureChanged(Self);
    end;
    FGesture := gtNone;
    FState := grstInvalid;
    Result := true;
  end;
end;

function TCastlePinchPanGestureRecognizer.Motion(const Event: TInputMotion;
  const Dpi: Single): boolean;

  { CosAngleBetweenVectors and AngleRadBetweenVectors are mostly copied from
    CastleVectors unit and adjusted for 2D vectors (and not make exception
    even on zero vectors, which may happen here). }

  function CosAngleBetweenVectors(const V1, V2: TVector2): Single;
  var
    LensSquared: Single;
  begin
    LensSquared := v1.LengthSqr * v2.LengthSqr;
    if IsZero(LensSquared) then
      Result := 1
    else
      Result := Clamped(TVector2.DotProduct(V1, V2) / Sqrt(LensSquared), -1.0, 1.0);
  end;

  function AngleRadBetweenVectors(const V1, V2: TVector2): Single;
  begin
    Result := ArcCos(CosAngleBetweenVectors(V1, V2));
  end;

var
  OldDist, NewDist: Single;
  Length0, Length1: Single;
  ParallelFingerMovement: Boolean;
begin
  Result := false;

  if Event.FingerIndex = 0 then
    FFinger0Pos := Event.Position
  else
  if Event.FingerIndex = 1 then
    FFinger1Pos := Event.Position
  else
    Exit(FState <> grstInvalid);  // moving with additional finger

  if (not FFinger0Pressed) or (not FFinger1Pressed) then
    Exit(false);

  if FState = grstInvalid then
  begin
    Length0 := PointsDistance(FFinger0Pos, FFinger0StartPos);
    Length1 := PointsDistance(FFinger1Pos, FFinger1StartPos);

    // angle less then 60 deg
    ParallelFingerMovement := AngleRadBetweenVectors(
      FFinger0Pos - FFinger0StartPos,
      FFinger1Pos - FFinger1StartPos) < 1.0;

    { For pinch gesture, the fingers move either closer or further apart.
      For pan gesture, the fingers move in the same direction. }
    if ParallelFingerMovement then
    begin
      // similar distance for both fingers
      if Min(Length0, Length1) *1.5 > Max(Length0, Length1) then
      begin
        FGesture := gtPan;
        FState := grstStarted;
        FPanOldOffset := FFinger0StartPos;
        FPanOffset := FFinger0Pos;

        if Assigned(FOnGestureChanged) then
          FOnGestureChanged(Self);

        FState := grstUpdate;
        Result := true;
      end;
      // else undecided gesture, wait for more movement
    end else
    begin
      OldDist := PointsDistance(FFinger0StartPos, FFinger1StartPos);
      NewDist := PointsDistance(FFinger0Pos, FFinger1Pos);

      FGesture := gtPinch;
      FState := grstStarted;
      //FPinchCenter := (FFinger0Pos + FFinger1Pos) / 2.0;
      FPinchScaleFactor := NewDist / OldDist;

      if Assigned(FOnGestureChanged) then
        FOnGestureChanged(Self);

      FState := grstUpdate;
      Result := true;
    end;
  end else
  if FState = grstUpdate then
  begin
    // update gestures
    if FGesture = gtPinch then
    begin
      NewDist := PointsDistance(FFinger0Pos, FFinger1Pos);
      if Event.FingerIndex = 0 then
        OldDist := PointsDistance(Event.OldPosition, FFinger1Pos)
      else
        OldDist := PointsDistance(FFinger0Pos, Event.OldPosition);

      FPinchScaleFactor := NewDist / OldDist;

      if Assigned(FOnGestureChanged) then
        FOnGestureChanged(Self);

      Result := true;
    end else
    if FGesture = gtPan then
    begin
      if Event.FingerIndex = 0 then // send only when 1st finger moved
      begin
        FPanOldOffset := Event.OldPosition;
        FPanOffset := Event.Position;

        if Assigned(FOnGestureChanged) then
          FOnGestureChanged(Self);
      end;
      Result := true;
    end;
  end;

  { Eat all 2 finger moves.
    Positive effect: camera does not change before the gesture is recognized.
    Negative effect: in theory, we might block some other two-finger gestures.
    Doesn't matter anymore, we start recognizing some gesture immediately now,
    results in faster reaction. }
  Result := true;
end;

end.
