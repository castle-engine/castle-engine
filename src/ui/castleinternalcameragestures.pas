{
  Copyright 2001-2021 Michalis Kamburelis, Jan Adamec.

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
    but cannot be done at the same time (to have only one active recognizer).

    Pass the input events using Press, Motion and Release functions, listen
    to recognized gestures in @link(OnGestureChanged) event.
    }
  TCastlePinchPanGestureRecognizer = class
  strict private
    FGesture: TCastleGestureType;
    FState: TCastleGestureRecognizerState;
    FPanOldOffset, FPanOffset: TVector2;   // for panning, use (PanOffset - PanOldOffset)
    FPinchScaleFactor: Single;
    FPinchCenter: TVector2;

    FOnGestureChanged: TNotifyEvent;

    FFinger0Pressed, FFinger1Pressed: boolean;
    FFinger0Pos, FFinger1Pos: TVector2;  // stored position of the fingers, we get only one of them in Motion event
    FFinger0StartPos, FFinger1StartPos: TVector2; // gesture start finger positions

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

    { Recognizer state. When not detected any gesture, it is in grstInvalid,
      grstStarted is when the gesture is first recognized, grstFinished is the
      last event of the recognized gesture, grstUpdate are all events between
      started and finished.

      As you get the @link(OnGestureChanged) event only when any gesture
      is recognized, you may ignore this RecognizerState property, as the
      gesture parameters are always valid and can be used for transformations.}
    property RecognizerState: TCastleGestureRecognizerState read FState;

    { Offset of the current pan gesture. To get the actual change, you have
    to calculate PanOffset - PanOldOffset. }
    property PanOffset: TVector2 read FPanOffset;

    { Previous pan gesture offset. }
    property PanOldOffset: TVector2 read FPanOldOffset;

    { Scale factor of the pinch gesture. I.e. 1.0 = no change, >1.0 zoom in. }
    property PinchScaleFactor: Single read FPinchScaleFactor;

    { Coordinates of the pinch gesture center. }
    property PinchCenter: TVector2 read FPinchCenter;

    { Listen to this event to receive updates on recognized gestures. }
    property OnGestureChanged: TNotifyEvent read FOnGestureChanged write FOnGestureChanged;
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
  end
  else if Event.FingerIndex = 1 then begin
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
  else if Event.FingerIndex = 1 then
    FFinger1Pressed := false;

  // end gesture when any finger up
  if FState <> grstInvalid then
  begin
    if Assigned(FOnGestureChanged) then
    begin
      // send 'Finished' event
      if Gesture = gtPinch then
        FPinchScaleFactor := 1.0
      else if Gesture = gtPan then
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
var
  OldDist, NewDist: Single;
  Length0, Length1: Single;
  DpiScale: Single;

  function CosAngleBetweenVectors(const V1, V2: TVector2): Single;
  var
    LensSquared: Float;
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

begin
  Result := false;

  if Event.FingerIndex = 0 then
    FFinger0Pos := Event.Position
  else if Event.FingerIndex = 1 then
    FFinger1Pos := Event.Position
  else
    Exit(FState <> grstInvalid);  // moving with additional finger

  if (not FFinger0Pressed) or (not FFinger1Pressed) then
    Exit(false);

  if FState = grstInvalid then
  begin
    DpiScale := Dpi / 96.0;
    // test if gesture started
    OldDist := PointsDistance(FFinger0StartPos, FFinger1StartPos);
    NewDist := PointsDistance(FFinger0Pos, FFinger1Pos);
    if Abs(OldDist - NewDist) > (20 * DpiScale) then
    begin
      // pinch gesture recognized
      FGesture := gtPinch;
      FState := grstStarted;
      FPinchCenter := (FFinger0Pos + FFinger1Pos) / 2.0;
      FPinchScaleFactor := NewDist / OldDist;

      if Assigned(FOnGestureChanged) then
        FOnGestureChanged(Self);

      FState := grstUpdate;
      Result := true;
    end;

    // ﻿test if it is pan gesture - it should be parralel movement of all fingers
    Length0 := PointsDistance(FFinger0Pos, FFinger0StartPos);
    Length1 := PointsDistance(FFinger1Pos, FFinger1StartPos);

    if (Max(Length0, Length1) > (10 * DpiScale)) and (Min(Length0, Length1)*1.5 > Max(Length0, Length1))
       and (AngleRadBetweenVectors(FFinger0Pos - FFinger0StartPos, FFinger1Pos - FFinger1StartPos) < 1.0) then // angle less then 60 deg
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
  end
  else if FState = grstUpdate then begin
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
    end
    else if FGesture = gtPan then begin
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
    Negative effect: in theory, we might block some other two-finger gestures. }
  Result := true;
end;

end.
