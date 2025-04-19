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
unit StairsSupport;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform,
  CastleVectors, CastleClassUtils;

type
  { In most cases you don't need this behavior, just set the friction in your
    player collider (friction is not used in modular movement because we want
    to have as much control over the avatar as possible). Modular movement
    system always sets linear speed to zero when player is on ground and do
    not walk/move. This fixes some edge cases like sliding down slopes. }
  TStairsSupportByColliderCapsuleRadius = class(TAbstractMovementModule)
  strict private
    FWasWarning: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;

    function CanBePlayerMovedUp(const MovementState: TModularMovementState;
      const CapsuleHeight, CapsuleRadius, StepHeight: Single):Boolean;

    function PropertySections(const PropertyName: String): TPropertySections; override;
  end;


implementation

uses CastleUtils, CastleComponentSerialize, CastleLog;

{ TStairsSupportByColliderCapsuleRadius -------------------------------------- }

constructor TStairsSupportByColliderCapsuleRadius.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TStairsSupportByColliderCapsuleRadius.UpdateMovement(
  const MovementState: TModularMovementState);
var
  CapsuleCollider: TCastleCapsuleCollider;
  CapsuleRadius: Single;
  CapsuleHeight: Single;

  RayDirection: TVector3;
  RayOrigin: TVector3;
  StepHit: TPhysicsRayCastResult;
  StepHeight: Single;
begin
  { Works only when player on ground and moving }
  if MovementState.IsFirstJumpingFrame or (MovementState.IsPlayerOnGround = false) or
     (MovementState.IsMoving = false) then
    Exit;

  { Works only when collider is TCastleCapsuleCollider }
  if not (MovementState.Collider is TCastleCapsuleCollider) then
  begin
    if not FWasWarning then
    begin
      FWasWarning := true;
      WritelnWarning('TStairsSupportByColliderCapsuleRadius works only with TCastleCapsuleCollider.');
    end;
    Exit;
  end;

  CapsuleCollider := MovementState.Collider as TCastleCapsuleCollider;

  CapsuleRadius := CapsuleCollider.CalculateScaledRadius;
  CapsuleHeight := CapsuleCollider.CalculateScaledHeight;

  { Full collider height is CapsuleRadius * 2 + CapsuleHeight }

  RayDirection := Vector3(0, -1, 0);
  RayOrigin := CapsuleCollider.Middle() - Vector3(0, 0, CapsuleRadius * 0.95);

  StepHit := MovementState.RigidBody.PhysicsRayCast(RayOrigin, RayDirection,
  CapsuleHeight / 2  + CapsuleRadius * 0.95);

  if StepHit.Hit then
  begin
    //WritelnLog('Found step, step normal ' + StepHit.Normal.ToString);

    { Calculate step height }
    StepHeight := CapsuleRadius + CapsuleHeight / 2 - StepHit.Distance;
    //WritelnLog('Step height: ' + FloatToStr(StepHeight));

    { check we can teleport player a little up }

    if CanBePlayerMovedUp(MovementState, CapsuleHeight, CapsuleRadius, StepHeight) then
      Parent.Translation := Parent.Translation + Vector3(0, StepHeight * 0.90, 0);
  end;
end;

function TStairsSupportByColliderCapsuleRadius.CanBePlayerMovedUp(
  const MovementState: TModularMovementState; const CapsuleHeight,
  CapsuleRadius, StepHeight: Single): Boolean;
var
  Collider: TCastleCollider;
  RBody: TCastleRigidBody;

  CastDirection: TVector3;
  CastOrigin: TVector3;

  CastResult: TPhysicsRayCastResult;
begin
  Collider := MovementState.Collider;
  RBody := MovementState.RigidBody;

  CastDirection := Vector3(0, 1, 0);
  CastOrigin := Collider.Middle;

  CastResult := RBody.PhysicsSphereCast(CastOrigin, CapsuleRadius, CastDirection,
  CapsuleHeight / 2 + StepHeight); // no CapsuleRadius here because we add it and remove in one step

  Result := not CastResult.Hit;
  //WritelnLog(Iff(Result, 'can be moved up', 'no space to move up'));
end;

function TStairsSupportByColliderCapsuleRadius.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  Result := inherited PropertySections(PropertyName);
end;


initialization
  RegisterSerializableComponent(TStairsSupportByColliderCapsuleRadius, ['Navigation', 'Modules', 'Stairs Support By Collider Capsule Radius']);

end.

