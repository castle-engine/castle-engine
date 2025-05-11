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
unit InAir3DControl;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleInputAxis, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs, CastleClassUtils;

type

  { 3D player in air control for TModularMovement.
    With that movement module you can add ability to change player
    position in air while player is jumping.

    The strength you can set by HorizontalSpeedChangeInAir.
    Horizontal velocity is clamped (from -MaxHorizontalSpeed
    to MaxHorizontalSpeed).

    This module is not for flying but for making more arcade like 3d games.
    }
  TInAir3DControl = class(TAbstractMovementModule)
  strict private
    FHorizontalSpeedChangeInAir: Single;
    FMaxHorizontalSpeed: Single;
  public
    const
      DefaultHorizontalSpeedChangeInAir = 0.1;
      DefaultMaxHorizontalSpeed = 5;

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;
  published
    { How much change can be made while the player is in the air. }
    property HorizontalSpeedChangeInAir: Single read FHorizontalSpeedChangeInAir
      write FHorizontalSpeedChangeInAir {$ifdef FPC}default DefaultHorizontalSpeedChangeInAir{$endif};

    { Maximum speed of the player is in the air. Can be used to make some effects }
    property MaxHorizontalSpeed: Single read FMaxHorizontalSpeed
      write FMaxHorizontalSpeed {$ifdef FPC}default DefaultMaxHorizontalSpeed{$endif};
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TInAir3DControl ------------------------------------------------------------- }

procedure TInAir3DControl.UpdateMovement(const MovementState: TModularMovementState);
var
  InputDirection: TVector3;
  ForwardDirection: TVector3;
  RightDirection: TVector3;

  NewHorizontalVelocity: TVector3;
  NewHorizontalSpeed: Single; // lenght of NewHorizontalVelocity
  CurrentHorizontalVelocity: TVector3;

  ForwardDirectionVelocityComponent: Single;
  RightDirectionVelocityComponent: Single;

  PlayerRigidBody: TCastleRigidBody;
begin
  { Do not work when gravity is off }
  if MovementState.RigidBody.Gravity = false then
    Exit;

  { Only work when we are in air }
  if MovementState.IsPlayerOnGround then
    Exit;

  PlayerRigidBody := MovementState.RigidBody;
  InputDirection := MovementState.InputDirection;
  ForwardDirection := MovementState.ForwardDirection;
  RightDirection := MovementState.RightDirection;

  CurrentHorizontalVelocity := PlayerRigidBody.LinearVelocity;
  CurrentHorizontalVelocity.Y := 0;

  ForwardDirectionVelocityComponent := TVector3.DotProduct(CurrentHorizontalVelocity,
    ForwardDirection.Normalize);
  RightDirectionVelocityComponent := TVector3.DotProduct(CurrentHorizontalVelocity,
    RightDirection.Normalize);

  NewHorizontalVelocity :=  ForwardDirection * (ForwardDirectionVelocityComponent +
    InputDirection.Z * 60 * HorizontalSpeedChangeInAir * MovementState.SecondsPassed)
    + RightDirection * (RightDirectionVelocityComponent + InputDirection.X * 60 * HorizontalSpeedChangeInAir * MovementState.SecondsPassed);

  { Check the speed is not too big }
  NewHorizontalSpeed := NewHorizontalVelocity.Length;

  if (NewHorizontalSpeed < -MaxHorizontalSpeed) or
    (NewHorizontalSpeed > MaxHorizontalSpeed) then
  begin
    // new speed is too big
    NewHorizontalVelocity := NewHorizontalVelocity.Normalize * Clamped(NewHorizontalSpeed, -MaxHorizontalSpeed, MaxHorizontalSpeed);
  end;

  // Add vertical velocity and set velocity
  NewHorizontalVelocity.Y := PlayerRigidBody.LinearVelocity.Y;
  PlayerRigidBody.LinearVelocity := NewHorizontalVelocity;
end;

constructor TInAir3DControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxHorizontalSpeed := DefaultMaxHorizontalSpeed;
  FHorizontalSpeedChangeInAir := DefaultHorizontalSpeedChangeInAir;
end;

function TInAir3DControl.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'HorizontalSpeedChangeInAir', 'MaxHorizontalSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TInAir3DControl, ['Navigation', 'Modules', '3D Player In Air Control']);

end.

