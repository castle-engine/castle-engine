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
unit FpsCrouch;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs, CastleClassUtils;

type

  { Crouch support for TModularMovement.

    It change collider height and player translation.
    It supports only capsule collider because of edge cases that are
    difficult to meet for all types of colliders.

    This functionality assumes that the size of the collider is fixed and is
    not changed by any other mechanism.

    Add to player transform. }
  TFpsCrouch = class(TAbstractMovementModule)
  strict private
    FInputCrouch: TInputShortcut;
    FCrouchSpeed: Single;
    FIsCrouching: Boolean; { Are player crouching? }
    FWasCrouchingInput: Boolean; { Was input pressed in previous frame }
    FOriginalCapsuleHeight: Single; { Capsule height property when not crouching }
    FScaledOriginalCapusleHeight: Single; { Needed for rigid body translation }
    FScaledCrouchingCapusleHeight: Single; { Needed for rigid body translation }

    FWasColliderTypeWarning: Boolean; { Show collider type warning only once }
  protected
    procedure StartCrouching(const MovementState: TModularMovementState); virtual;
    procedure StopCrouching(const MovementState: TModularMovementState); virtual;

    function CanPlayerStandUp(const RBody: TCastleRigidBody;
      const Collider: TCastleCapsuleCollider): Boolean;
  public
    const
      DefaultCrouchSpeed = 3.0;
      MinCapsuleHeight = 0.02;

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;

    property IsCrouching: Boolean read FIsCrouching;
  published
    property InputCrouch: TInputShortcut read FInputCrouch;

    property CrouchSpeed: Single read FCrouchSpeed write FCrouchSpeed
      {$ifdef FPC}default DefaultCrouchSpeed{$endif};
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleBoxes,
  CastleLog;

{ TFpsCrouch ----------------------------------------------------------------- }

constructor TFpsCrouch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCrouchSpeed := DefaultCrouchSpeed;

  FInputCrouch := TInputShortcut.Create(Self);
  InputCrouch.SetSubComponent(true);
  InputCrouch.Assign(keyC);
  InputCrouch.Name := 'InputCrouch';

  FWasColliderTypeWarning := false;
end;

function TFpsCrouch.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'InputCrouch', 'CrouchSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TFpsCrouch.UpdateMovement(const MovementState: TModularMovementState);
var
  Velocity: TVector3;
  HorizontalSpeed: TVector3;
  VerticalSpeed: Single;

  function CheckColliderType(const Collider: TCastleCollider): Boolean;
  begin
    if not (Collider is TCastleCapsuleCollider) then
    begin
      if not FWasColliderTypeWarning then
      begin
        WritelnWarning('TFpsCrouch supports only player with capsule collider.');
        FWasColliderTypeWarning := true;
      end;
      Exit(false);
    end;
    Result := true;
  end;

begin
  if FocusedContainer = nil then
    Exit;

  if not CheckColliderType(MovementState.Collider) then
    Exit;

  if MovementState.IsFirstJumpingFrame or MovementState.IsPlayerOnGround = false then
    Exit;

  if not FWasCrouchingInput then
  begin
    if InputCrouch.IsPressed(FocusedContainer) then
    begin
      FWasCrouchingInput := true;
      if not FIsCrouching then
        StartCrouching(MovementState)
      else
        StopCrouching(MovementState);
    end;
  end else
  begin
    if not InputCrouch.IsPressed(FocusedContainer) then
      FWasCrouchingInput := false;
  end;

  { Modify rigid body speed }
  if FIsCrouching then
  begin
    Velocity := MovementState.RigidBody.LinearVelocity;
    if not Velocity.IsZero then
    begin
      VerticalSpeed := Velocity.Y;
      HorizontalSpeed := Velocity;
      HorizontalSpeed.Y := 0;

      HorizontalSpeed := HorizontalSpeed.Normalize * CrouchSpeed;
      Velocity := HorizontalSpeed;
      Velocity.Y := VerticalSpeed;

      MovementState.RigidBody.LinearVelocity := Velocity;
    end;
  end;
end;

procedure TFpsCrouch.StartCrouching(const MovementState: TModularMovementState);
var
  CapsuleCollider: TCastleCapsuleCollider;
  NewHeight: Single;
begin
  CapsuleCollider := MovementState.Collider as TCastleCapsuleCollider;

  FScaledOriginalCapusleHeight := CapsuleCollider.CalculateScaledHeight;
  FOriginalCapsuleHeight := CapsuleCollider.Height;
  { We try to change player height to 50% }
  NewHeight := FOriginalCapsuleHeight / 2 - CapsuleCollider.Radius;
  { When height / 2 is smaller than radius use minimal height value }
  if NewHeight < MinCapsuleHeight then
    NewHeight := MinCapsuleHeight;

  CapsuleCollider.Height := NewHeight;
  FScaledCrouchingCapusleHeight := CapsuleCollider.CalculateScaledHeight;

  { Stand player on ground - 0.90 to ensure player will be above ground }
  Parent.Translation := Parent.Translation + Vector3(0,
    -((FScaledOriginalCapusleHeight / 2 - FScaledCrouchingCapusleHeight / 2) * 0.90 ), 0);
  FIsCrouching := true;
end;

procedure TFpsCrouch.StopCrouching(const MovementState: TModularMovementState);
var
  CapsuleCollider: TCastleCapsuleCollider;
begin
  if FIsCrouching and CanPlayerStandUp(MovementState.RigidBody,
    MovementState.Collider as TCastleCapsuleCollider) then
  begin
    CapsuleCollider := MovementState.Collider as TCastleCapsuleCollider;

    { Set the player higher in the air before resizing the collider. }
    Parent.Translation := Parent.Translation + Vector3(0, (FScaledOriginalCapusleHeight / 2 - FScaledCrouchingCapusleHeight / 2) * 1.10, 0);
    CapsuleCollider.Height := FOriginalCapsuleHeight;
    FIsCrouching := false;
  end;
end;

function TFpsCrouch.CanPlayerStandUp(const RBody: TCastleRigidBody;
  const Collider: TCastleCapsuleCollider): Boolean;
var
  CastDirection: TVector3;
  CastOrigin: TVector3;
  CastResult: TPhysicsRayCastResult;
begin
  CastDirection := Vector3(0, 1, 0);
  CastOrigin := Collider.Middle;

  CastResult := RBody.PhysicsSphereCast(CastOrigin, Collider.CalculateScaledRadius, CastDirection,
  Collider.CalculateScaledHeight / 2 + (FScaledOriginalCapusleHeight / 2 - FScaledCrouchingCapusleHeight / 2 ) * 1.10);

  Result := not CastResult.Hit;
  WritelnLog(Iff(Result, 'Stop crouching: can be moved up',
    'Stop crouching: no space to move up'));
end;

initialization
  RegisterSerializableComponent(TFpsCrouch, ['Navigation', 'Modules', 'Fps Crouch support']);

end.

