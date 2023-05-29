unit StairsSupportByColliderCapsuleRadius;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform,
  CastleVectors, CastleClassUtils;

type
  TStairsSupportByColliderCapsuleRadius = class(TAbstractMovementModifier)
  strict private
    FWasWarning: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;

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

  RayDirection: TVector3;
  RayOrigin: TVector3;
  StepHit: TPhysicsRayCastResult;
begin
  { Works only when player on ground and moving }
  if MovementState.IsJumping or (MovementState.IsPlayerOnGround = false) or
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
  //ColliderBoundingBox:= CapsuleCollider.ScaledLocalBoundingBox;

  RayDirection := Vector3(0, -1, 0);
  RayOrigin := Parent.Translation - Vector3(0, 0, CapsuleCollider.Radius);

  StepHit := MovementState.RigidBody.PhysicsRayCast(RayOrigin, RayDirection, CapsuleCollider.Height/2 + CapsuleCollider.Radius * 0.95 );
  if StepHit.Hit then
  begin
    //WritelnLog('Found step, step normal ' + StepHit.Normal.ToString);
    Parent.Translation := Parent.Translation + Vector3(0, CapsuleCollider.Radius * 0.75, 0);
  end;
end;

function TStairsSupportByColliderCapsuleRadius.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  Result := inherited PropertySections(PropertyName);
end;


initialization
  RegisterSerializableComponent(TStairsSupportByColliderCapsuleRadius, ['Physics', 'Stairs Support By Collider Capsule Radius']);

end.

