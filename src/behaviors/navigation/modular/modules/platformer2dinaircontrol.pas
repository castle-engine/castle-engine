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
unit Platformer2DInAirControl;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleInputAxis, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs, CastleClassUtils;

type

  { 2D platformer player in air control for TModularMovement.
    With that movement module you can add ability to change player position in air
    while player is jumping. This is desirable in most arcade games.

    The strength you can set by HorizontalSpeedChangeInAir.
    Maximum horizontal velocity is clamped (-MaxHorizontalSpeed, MaxHorizontalSpeed).
  }
  TPlatformer2DInAirControl = class(TAbstractMovementModule)
  strict private
    FHorizontalSpeedChangeInAir: Single;
    FMaxHorizontalSpeed: Single;
  public
    const
      DefaultHorizontalSpeedChangeInAir = 20;
      DefaultMaxHorizontalSpeed = 345;

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

{ TPlatformer2DInAirControl ---------------------------------------------------- }

procedure TPlatformer2DInAirControl.UpdateMovement(const MovementState: TModularMovementState);
var
  InputDirection: TVector3;

  CurrentHorizontalVelocity: Single;
  CurrentVerticalVelocity: Single;

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

  CurrentHorizontalVelocity := PlayerRigidBody.LinearVelocity.X;
  CurrentVerticalVelocity := PlayerRigidBody.LinearVelocity.Y;

  { Why we multiply by 60?

    We need multiply HorizontalSpeedChangeInAir by SecondsPassed.
    Without that when game will run 120 FPS, player will accelerated
    twice faster than on 60 FPS.
    So HorizontalSpeedChangeInAir is designed and tested on 60 FPS so we need
    multiply HorizontalSpeedChangeInAir by 60 to get it.

    It's easy to realize when you know that for 60 FPS:

    HorizontalSpeedChangeInAir * SecondsPassed * 60 = 350
    21000 * (1/60) * 60 = 350
    21000 * 0.01666 * 60 = 350

    And for 120 FPS:
    21000 * (1/120) * 60 = 175
    21000 * 0.008333 * 60 = 175
    For 120 FPS every frame speed change up will be 175 but you have two times
    more frames (updates). So 175 * 2 = 350 like in 60 FPS.
  }

  CurrentHorizontalVelocity := Clamped(CurrentHorizontalVelocity +
    InputDirection.X * 60 * HorizontalSpeedChangeInAir * MovementState.SecondsPassed,
    -MaxHorizontalSpeed, MaxHorizontalSpeed);

  { Integrate velocities }
  PlayerRigidBody.LinearVelocity := Vector3(CurrentHorizontalVelocity, CurrentVerticalVelocity, 0);

  MovementState.IsMoving := not IsZero(CurrentHorizontalVelocity);
end;

constructor TPlatformer2DInAirControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxHorizontalSpeed := DefaultMaxHorizontalSpeed;
  FHorizontalSpeedChangeInAir := DefaultHorizontalSpeedChangeInAir;
end;

function TPlatformer2DInAirControl.PropertySections(const PropertyName: String
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
  RegisterSerializableComponent(TPlatformer2DInAirControl, ['Navigation', 'Modules', 'Platformer 2D Player In Air Control']);

end.

