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

  { This gesture recognizer detects pan and pinch gesture, as both use two fingers,
    but cannot be used at the same time.

    Pass the input events using Press, Motion and Release functions, listen
    to recognized gestures in @link(OnGestureChanged) event. }
  TCastlePinchPanGestureRecognizer = class
  strict private
    FGesture: TCastleGestureType;
    FPanMove: TVector2;
    FPinchScaleFactor: Single;
    //FPinchCenter: TVector2;

    FOnGestureChanged: TNotifyEvent;

    FFinger0Pressed, FFinger1Pressed: Boolean;
    // stored position of the fingers, as we get only one of them in Motion event
    FFinger0Pos, FFinger1Pos: TVector2;
    // gesture start finger positions
    FFinger0StartPos, FFinger1StartPos: TVector2;
  public
    constructor Create;

    { Functions to pass the input to the recognizer from some @link(TCastleUserInterface).
      @groupBegin }
    function Press(const Event: TInputPressRelease): Boolean;
    function Release(const Event: TInputPressRelease): Boolean;
    function Motion(const Event: TInputMotion; const Dpi: Single): Boolean;
    { @groupEnd }

    { Gesture type once it's recognized. Check it inside OnGestureChanged event. }
    property Gesture: TCastleGestureType read FGesture;

    { Movement of the current pan gesture. }
    property PanMove: TVector2 read FPanMove;

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
end;

function TCastlePinchPanGestureRecognizer.Press(const Event: TInputPressRelease): Boolean;
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

function TCastlePinchPanGestureRecognizer.Release(const Event: TInputPressRelease): Boolean;
var
  OldBothPressed, NewBothPressed: Boolean;
begin
  Result := false;

  OldBothPressed := FFinger0Pressed and FFinger1Pressed;

  if Event.FingerIndex = 0 then
    FFinger0Pressed := false
  else
  if Event.FingerIndex = 1 then
    FFinger1Pressed := false;

  NewBothPressed := FFinger0Pressed and FFinger1Pressed;

  // finger release stopped the gesture, mark as handled
  if OldBothPressed and not NewBothPressed then
    Exit(true);
end;

function TCastlePinchPanGestureRecognizer.Motion(const Event: TInputMotion;
  const Dpi: Single): Boolean;

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
  // Length0, Length1: Single;
  ParallelFingerMovement: Boolean;
begin
  Result := false;

  if Event.FingerIndex = 0 then
  begin
    FFinger0StartPos := FFinger0Pos;
    FFinger0Pos := Event.Position
  end else
  if Event.FingerIndex = 1 then
  begin
    FFinger1StartPos := FFinger1Pos;
    FFinger1Pos := Event.Position
  end else
    Exit(false); // moving with additional finger

  if (not FFinger0Pressed) or (not FFinger1Pressed) then
    Exit(false);

  // Length0 := PointsDistance(FFinger0Pos, FFinger0StartPos);
  // Length1 := PointsDistance(FFinger1Pos, FFinger1StartPos);

  // angle less then 60 deg
  ParallelFingerMovement := AngleRadBetweenVectors(
    FFinger0Pos - FFinger0StartPos,
    FFinger1Pos - FFinger1StartPos) < 1.0;

  { For pinch gesture, the fingers move either closer or further apart.
    For pan gesture, the fingers move in the same direction. }
  if ParallelFingerMovement then
  begin
    FGesture := gtPan;

    { Note: both fingers movement determines pan. }
    FPanMove := 0.5 * (
      (FFinger0StartPos - FFinger0Pos) +
      (FFinger1StartPos - FFinger1Pos)
    );

    if Assigned(FOnGestureChanged) then
      FOnGestureChanged(Self);
    Result := true;
  end else
  begin
    FGesture := gtPinch;

    OldDist := PointsDistance(FFinger0StartPos, FFinger1StartPos);
    NewDist := PointsDistance(FFinger0Pos, FFinger1Pos);
    //FPinchCenter := (FFinger0Pos + FFinger1Pos) / 2.0;
    FPinchScaleFactor := NewDist / OldDist;

    if Assigned(FOnGestureChanged) then
      FOnGestureChanged(Self);
    Result := true;
  end;
end;

end.
