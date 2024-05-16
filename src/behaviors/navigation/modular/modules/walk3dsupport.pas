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
unit Walk3DSupport;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleInputAxis, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs, CastleClassUtils;

type

  { Classic 3D walk on ground support for modular movement. Can be used for
    FPS and TPP games. It support horizontal movement and single jump only
    when player is on gorund. }
  TWalk3DSupport = class(TAbstractMovementModule)
  strict private
    FWasJumpInput: Boolean;

    FHorizontalSpeed: Single;
    FJumpSpeed: Single;
  public
    const
      DefaultJumpSpeed = 7.0;
      DefaultHorizontalSpeed = 5.0;

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

{ TWalk3DSupport ------------------------------------------------------------- }

procedure TWalk3DSupport.UpdateMovement(const MovementState: TModularMovementState);
var
  IntegratedVelocities: TVector3;
  InputDirection: TVector3;
  ForwardDirection: TVector3;
  RightDirection: TVector3;
  UpDirection: TVector3;

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
  ForwardDirection := MovementState.ForwardDirection;
  RightDirection := MovementState.RightDirection;
  UpDirection := MovementState.UpDirection;

  { Simplest code:  When input direction is 1.00 0.00 -1.00 this move faster:

    HorizontalVelocity := ForwardDirection * (InputDirection.Z * HorizontalSpeed)
    + RightDirection * (InputDirection.X * HorizontalSpeed);

    So we normalize HorizontalVelocity before applying speed.
  }

  HorizontalVelocity := ForwardDirection * InputDirection.Z
    + RightDirection * InputDirection.X;

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

constructor TWalk3DSupport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWasJumpInput := false;
  FJumpSpeed := DefaultJumpSpeed;
  FHorizontalSpeed := DefaultHorizontalSpeed;
end;

function TWalk3DSupport.PropertySections(const PropertyName: String
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
  RegisterSerializableComponent(TWalk3DSupport, ['Navigation', 'Modules', 'Fps Walk Support']);

end.

