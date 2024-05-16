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

{ TODO docs. }
unit RotateRigidBody;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors,  CastleInputs, CastleInputAxis, CastleClassUtils;

type

  { Rotate horizontal rigid body using angular velocity. Should be added to
    transform with rigid body to control. }
  TRotateRigidBody = class(TCastleBehavior)
  strict private
    RBody: TCastleRigidBody;
    FAllowSlowerRotations: Boolean;

    FRotationHorizontalSpeed: Single;
    FHorizontalRotationInput: TCastleInputAxis;
    const
      DefaultMouseLookHorizontalSensitivity = 0.25;
      DefaultRotationHorizontalSpeed = 1.5;
  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;

     function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Left/right rotation input }
    property HorizontalRotationInput: TCastleInputAxis read FHorizontalRotationInput;

    { Rotation speed }
    property RotationHorizontalSpeed: Single
      read FRotationHorizontalSpeed write FRotationHorizontalSpeed
      {$ifdef FPC}default DefaultRotationHorizontalSpeed{$endif};

    { If @true then will work 10x slower when Ctrl modified is pressed. }
    property AllowSlowerRotations: boolean
      read FAllowSlowerRotations write FAllowSlowerRotations
      default true;
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TRotateRigidBody }

procedure TRotateRigidBody.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  HorizontalAxisValue: Single;
  SpeedScale: Single; // Ctrl support
begin
  if CastleApplicationMode = appDesign then
    Exit;

  if FocusedContainer = nil then
    Exit;

  RBody := Parent.RigidBody;
  if RBody = nil then
    Exit;

  { Do not allow the body to rotate by default }
  RBody.AngularVelocity := Vector3(0,0,0);

  if AllowSlowerRotations and (ModifiersDown(FocusedContainer.Pressed) = [mkCtrl]) then
    SpeedScale := 0.1 {* RotationControlFactor(IsOnGroundBool)}
  else
    SpeedScale := 1.0 {* RotationControlFactor(IsOnGroundBool)};

  HorizontalAxisValue := HorizontalRotationInput.Value(FocusedContainer);

  RBody.AngularVelocity := RBody.AngularVelocity +
  Vector3(0, -HorizontalAxisValue, 0) * RotationHorizontalSpeed * SpeedScale;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TRotateRigidBody.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;

  FAllowSlowerRotations := true;

  FHorizontalRotationInput := TCastleInputAxis.Create(Self);
  FHorizontalRotationInput.SetSubComponent(true);
  FHorizontalRotationInput.PositiveKey := keyArrowRight;
  FHorizontalRotationInput.NegativeKey := keyArrowLeft;
  FHorizontalRotationInput.MouseLook := true;
  FHorizontalRotationInput.MouseLookAxis := mlaHorizontal;
  FHorizontalRotationInput.MouseLookMultiplier := DefaultMouseLookHorizontalSensitivity;
  FHorizontalRotationInput.Multiplier := DefaultRotationHorizontalSpeed;
end;

function TRotateRigidBody.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'HorizontalRotationInput', 'RotationHorizontalSpeed', 'AllowSlowerRotations'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TRotateRigidBody, ['Navigation', 'Rotate Rigid Body By Mouse And Keys']);

end.

