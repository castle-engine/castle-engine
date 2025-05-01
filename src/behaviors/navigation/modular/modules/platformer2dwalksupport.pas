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
unit Platformer2DWalkSupport;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleInputAxis, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs, CastleClassUtils;

type

  { 2D platformer walk support for TModularMovement. In default settings
    it assumes that gravity strength in physics properties is set to about 1200 }
  TPlatformer2DWalkSupport = class(TAbstractMovementModule)
  strict private
    FWasJumpInput: Boolean;

    FHorizontalSpeed: Single;
    FJumpSpeed: Single;
  public
    const
      DefaultJumpSpeed = 680;
      DefaultHorizontalSpeed = 345;

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;
  published
    property JumpSpeed: Single read FJumpSpeed write FJumpSpeed
      {$ifdef FPC}default DefaultJumpSpeed{$endif};

    property HorizontalSpeed: Single read FHorizontalSpeed write FHorizontalSpeed
      {$ifdef FPC}default DefaultHorizontalSpeed{$endif};
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TPlatformer2DWalkSupport ------------------------------------------------------------ }

procedure TPlatformer2DWalkSupport.UpdateMovement(const MovementState: TModularMovementState);
var
  IntegratedVelocities: TVector3;
  InputDirection: TVector3;

  HorizontalVelocity: TVector3;
  VerticalVelocity: Single;

  PlayerRigidBody: TCastleRigidBody;
begin
  { Can be false by fly support }
  MovementState.RigidBody.Gravity := true;

  if not MovementState.IsPlayerOnGround then
    Exit;

  PlayerRigidBody := MovementState.RigidBody;
  InputDirection := MovementState.InputDirection;

  HorizontalVelocity := Vector3(1, 0, 0) * InputDirection.X;

  { Normalize and set velocity to handle faster movement when InputDirection is
    eg (1, 0, 1). }

  HorizontalVelocity :=  HorizontalVelocity.Normalize * HorizontalSpeed;

  { Jump support }
  if IsZero(InputDirection.Y) then
    FWasJumpInput := false;

  if (FWasJumpInput = false) and not IsZero(InputDirection.Y) then
  begin
    FWasJumpInput := true;
    VerticalVelocity := JumpSpeed;
    MovementState.IsFirstJumpingFrame := true;
  end else
    VerticalVelocity := PlayerRigidBody.LinearVelocity.Y;

  MovementState.IsMoving := not HorizontalVelocity.IsZero;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  PlayerRigidBody.LinearVelocity := IntegratedVelocities;
end;

constructor TPlatformer2DWalkSupport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWasJumpInput := false;
  FJumpSpeed := DefaultJumpSpeed;
  FHorizontalSpeed := DefaultHorizontalSpeed;
end;

function TPlatformer2DWalkSupport.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'JumpSpeed', 'HorizontalSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


initialization
  RegisterSerializableComponent(TPlatformer2DWalkSupport, ['Navigation', 'Modules', 'Platformer 2D Walk Support']);

end.

