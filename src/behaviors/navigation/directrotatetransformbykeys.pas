{
  Copyright 2023-2024 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rotate transform directly by keys (@link(TDirectRotateTransformByKeys)). }
unit DirectRotateTransformByKeys;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors, CastleInputs, CastleClassUtils;

type
  { Rotates transform directly by keys. Do not use with physics
    objects/navigation. MAy be usefull in other scenarios so I leave it here. }
  TDirectRotateTransformByKeys = class(TCastleBehavior)
  strict private
    FRotationHorizontalSpeed, FRotationVerticalSpeed: Single;
    FAllowSlowerRotations: Boolean;

    FInput_RightRotate: TInputShortcut;
    FInput_LeftRotate: TInputShortcut;
    FInput_UpRotate: TInputShortcut;
    FInput_DownRotate: TInputShortcut;
  private
    MinAngleFromGravityUp: Single;
    const
      DefaultMinAngleFromGravityUp = Pi * 10 / 180;
      DefaultRotationHorizontalSpeed = Pi * 150 / 180;
      DefaultRotationVerticalSpeed = Pi * 100 / 180;

    procedure RotateAroundGravityUp(const Angle: Single);
    procedure RotateAroundUp(const Angle: Single);
    procedure RotateHorizontal(const Angle: Single);
    procedure RotateVertical(AngleRad: Single);
  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;

     function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Input_LeftRotate: TInputShortcut read FInput_LeftRotate;
    property Input_RightRotate: TInputShortcut read FInput_RightRotate;
    property Input_UpRotate: TInputShortcut read FInput_UpRotate;
    property Input_DownRotate: TInputShortcut read FInput_DownRotate;

    { Rotation keys speed, in radians per second.
      @groupBegin }
    property RotationHorizontalSpeed: Single
      read FRotationHorizontalSpeed write FRotationHorizontalSpeed
      {$ifdef FPC}default DefaultRotationHorizontalSpeed{$endif};

    property RotationVerticalSpeed: Single
      read FRotationVerticalSpeed write FRotationVerticalSpeed
      {$ifdef FPC}default DefaultRotationVerticalSpeed{$endif};

    { If @true then all rotation keys
      (Input_RightRotate, Input_LeftRotate, Input_UpRotate, Input_DownRotate)
      will work 10x slower when Ctrl modified is pressed. }
    property AllowSlowerRotations: boolean
      read FAllowSlowerRotations write FAllowSlowerRotations
      default true;

  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse;

{ TDirectRotateTransformByKeys }

procedure TDirectRotateTransformByKeys.RotateAroundGravityUp(const Angle: Single);
var
  GravityAxis,
    OldPosition, OldDirection, OldUp,
    NewPosition, NewDirection, NewUp: TVector3;
begin
  Parent.GetWorldView(OldPosition, OldDirection, OldUp);

  { We need to sometimes use GravityUp, sometimes -GravityUp,
    to intuitively keep rotations to left -> going left even
    when looking upside-down. }
  if AngleRadBetweenVectors(OldUp, World.GravityUp) > Pi/2 then
    GravityAxis := -World.GravityUp
  else
    GravityAxis := World.GravityUp;

  NewUp        := RotatePointAroundAxisRad(Angle,        OldUp, GravityAxis);
  NewDirection := RotatePointAroundAxisRad(Angle, OldDirection, GravityAxis);

  NewPosition := OldPosition; //AdjustPositionForRotationHorizontalPivot(OldPosition, OldDirection, NewDirection);

  Parent.SetWorldView(NewPosition, NewDirection, NewUp);
end;

procedure TDirectRotateTransformByKeys.RotateAroundUp(const Angle: Single);
var
  OldPosition, OldDirection, OldUp, NewPosition, NewDirection: TVector3;
begin
  Parent.GetWorldView(OldPosition, OldDirection, OldUp);

  { We know that RotatePointAroundAxisRad below doesn't change the length
    of the NewDirection (so it will remain normalized, like OldDirection)
    and it will keep NewDirection and OldUp vectors orthogonal. }
  NewDirection := RotatePointAroundAxisRad(Angle, OldDirection, OldUp);

  NewPosition := OldPosition;//AdjustPositionForRotationHorizontalPivot(OldPosition, OldDirection, NewDirection);

  Parent.SetWorldView(NewPosition, NewDirection, OldUp);
end;

procedure TDirectRotateTransformByKeys.RotateHorizontal(const Angle: Single);
begin
  if true {PreferGravityUpForRotations} then
    RotateAroundGravityUp(Angle)
  else
    RotateAroundUp(Angle);
end;

procedure TDirectRotateTransformByKeys.RotateVertical(AngleRad: Single);
var
  Side: TVector3;
  GravityAxis, OldPosition, OldDirection, OldUp, NewDirection, NewUp: TVector3;
  AngleRadBetween: Single;
begin
  Parent.GetWorldView(OldPosition, OldDirection, OldUp);

  { We need to sometimes use GravityUp, sometimes -GravityUp,
    to intuitively keep rotations to left -> going left even
    when looking upside-down. }
  if AngleRadBetweenVectors(OldUp, World.GravityUp) > Pi/2 then
    GravityAxis := -World.GravityUp
  else
    GravityAxis := World.GravityUp;

  if true {PreferGravityUpForRotations} then
  begin
    Side := TVector3.CrossProduct(OldDirection, GravityAxis);
    if Side.IsZero then
    begin
      { if OldDirection is parallel to GravityAxis,
        then realizing PreferGravityUpForRotations is not really possible.
        Allow rotation as if PreferGravityUpForRotations = false.
        This is important to do right, to allow in CGE editor to use mouse look
        right after pressing Top (7). }
      Side := TVector3.CrossProduct(OldDirection, OldUp);
    end else
    if MinAngleFromGravityUp <> 0 then
    begin
      { Calculate AngleRadBetween, and possibly adjust AngleRad. }
      AngleRadBetween := AngleRadBetweenVectors(OldDirection, GravityAxis);

      if AngleRadBetween - AngleRad < MinAngleFromGravityUp then
        AngleRad := AngleRadBetween - MinAngleFromGravityUp
      else
      if AngleRadBetween - AngleRad > Pi - MinAngleFromGravityUp then
        AngleRad := AngleRadBetween - (Pi - MinAngleFromGravityUp);
    end;
  end else
  begin
    Side := TVector3.CrossProduct(OldDirection, OldUp);
  end;

  { Rotate NewUp around Side }
  NewUp        := RotatePointAroundAxisRad(AngleRad, OldUp       , Side);
  { Rotate NewDirection around Side }
  NewDirection := RotatePointAroundAxisRad(AngleRad, OldDirection, Side);

  Parent.SetWorldView(OldPosition, NewDirection, NewUp);
end;

procedure TDirectRotateTransformByKeys.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  SpeedScale: Single;

begin
  if CastleApplicationMode = appDesign then
    Exit;

  if FocusedContainer = nil then
    Exit;

  { Because we use camera direction for move we can use the same code as DoDirect }
  if ModifiersDown(FocusedContainer.Pressed) = [mkCtrl] then
  begin
    if AllowSlowerRotations then
      SpeedScale := 0.1 {* RotationControlFactor(IsOnGroundBool)};
  end
  else
    SpeedScale := 1.0 {* RotationControlFactor(IsOnGroundBool)};

  if Input_RightRotate.IsPressed(FocusedContainer) then
    RotateHorizontal(-RotationHorizontalSpeed * SecondsPassed * SpeedScale);
  if Input_LeftRotate.IsPressed(FocusedContainer) then
    RotateHorizontal(+RotationHorizontalSpeed * SecondsPassed * SpeedScale);
  if Input_UpRotate.IsPressed(FocusedContainer) then
    RotateVertical(+RotationVerticalSpeed * SecondsPassed * SpeedScale);
  if Input_DownRotate.IsPressed(FocusedContainer) then
    RotateVertical(-RotationVerticalSpeed * SecondsPassed * SpeedScale);

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TDirectRotateTransformByKeys.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MinAngleFromGravityUp := DefaultMinAngleFromGravityUp;
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FRotationVerticalSpeed := DefaultRotationVerticalSpeed;
  FAllowSlowerRotations := true;

  FInput_LeftRotate := TInputShortcut.Create(Self);
  FInput_LeftRotate.SetSubComponent(true);
  FInput_LeftRotate.Assign(keyArrowLeft);
  FInput_LeftRotate.Name := 'InputLeftRotate';

  FInput_RightRotate := TInputShortcut.Create(Self);
  FInput_RightRotate.SetSubComponent(true);
  FInput_RightRotate.Assign(keyArrowRight);
  FInput_RightRotate.Name := 'InputRightRotate';

  FInput_UpRotate := TInputShortcut.Create(Self);
  FInput_UpRotate.SetSubComponent(true);
  FInput_UpRotate.Name := 'InputUpRotate';

  FInput_DownRotate := TInputShortcut.Create(Self);
  FInput_DownRotate.SetSubComponent(true);
  FInput_DownRotate.Name := 'InputDownRotate';
end;

function TDirectRotateTransformByKeys.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'AllowSlowerRotations', 'RotationVerticalSpeed', 'RotationHorizontalSpeed',
     'Input_LeftRotate', 'Input_RightRotate', 'Input_UpRotate', 'Input_DownRotate'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TDirectRotateTransformByKeys, ['Physics', 'Direct Rotate Transform By Keys']);

end.

