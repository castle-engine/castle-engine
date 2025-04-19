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
unit ModularMovement;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, CastleTransform, CastleVectors, CastleInputAxis, CastleInputs,
    CastleClassUtils, Generics.Collections;

type
  { It describes the current input state and player movement state when using
    TFpsModularMovement.

    TFpsModularMovement do not implement any movement only checks some things
    like input or is player on ground. And make these things available for modules
    that implements different aspects of player move like TFpsWalkSupport, TFpsFlySupport,
    THeadBobbing etc. Thanks for that we do not have one big class that is
    very hard to change.}
  TModularMovementState = class
    SecondsPassed: Single;

    RigidBody: TCastleRigidBody;
    Collider: TCastleCollider;

    IsPlayerOnGround: Boolean;
    { Means that player starts jumping in that update, notice that
      IsPlayerOnGround can be true here - this is set only on first
      frame of jump. Can be used as a flag that player pressed jump input }
    IsFirstJumpingFrame: Boolean;

    { Means that player changes horizontal position (has horizontal velocity) }
    IsMoving: Boolean;

    InputDirection: TVector3;
    ForwardDirection: TVector3; // Forward direction with Y = 0;
    FullForwardDirection: TVector3; // Forward direction where Y can be <> 0
    RightDirection: TVector3;
    UpDirection: TVector3;
  end;

  { Abstract modular movement not used right now. }
  TAbstractModularMovement = class(TCastleBehavior)

  end;

  { Base class for all modular movement modules like TWalk3DSupport, TFly3DSupport,
    THeadBobbing etc. }
  TAbstractMovementModule = class(TCastleBehavior)
  strict private
    FExists: Boolean;
  protected
    procedure SetExists(const AValue: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    { Each module needs to implement that function }
    procedure UpdateMovement(const MovementState: TModularMovementState); virtual; abstract;

    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { When set to false it is not called at all }
    property Exists: Boolean read FExists write SetExists default true;
  end;

  { The Modular movement navigation system using dynamic rigid body with
    expandable architecture.

    It can be used for First Person 3D Perspective Games (FPS/FPP),
    Third Person 3D Perspective and also 2D games. It all depends on the modules
    used. Some of them works in all cases some of them only in one case.

    TModularMovement do not implement any movement only checks some things
    like input or is player on ground. And make these things available for modules
    that implements different aspects of player move like TWalk3DSupport, TFly3DSupport,
    THeadBobbing, etc. Thanks for that we do not have one big class that is
    very hard to change.

    FPS - How To Use it:
    - Player rigid body can rotate only in Y axis (horizontal), other axes
      should be blocked in rigid body
    - Y is always up
    - Friction in player collider should be 0 - with other friction values
      using stairs needs extra code like TStairsSupportByColliderCapsuleRadius,
      and can have other undesirable problems during contact, e.g. with walls
    - Rotate player horizontal using angular velocity (do not change transform
      Rotation directly that leads to physics objects synchronization and
      can make your player can fall off the level - especially when it's done
      every frame) You can use TRotateRigidBody behavior for that purpose.
    - Do not rotate player vertical - rotate camera. You can use TRotateCamera
      behavior for that.
    - Do not change player translation every frame - use rigid body velocities/forces
    - After adding TModularMovement add some movement modules e.g. TWalk3DSupport
    - Do not afraid to change/add your own movement modules - this class is
      designed for that :)
    - Camera should be rotated by pi in y axis for FPS movement
    - See /examples/physics/physics_3d_shooter
    - For mouse look use Container.StartMouseLook(Viewport); and Container.StopMouseLook;

    TPP - How To Use:
    - Player rigid body can rotate only in Y axis (horizontal), other axes
      should be blocked in rigid body
    - Y is always up
    - Friction in player collider should be 0 - with other friction values
      using stairs needs extra code like TStairsSupportByColliderCapsuleRadius,
      and can have other undesirable problems during contact, e.g. with walls
    - Rotate player horizontal using angular velocity (do not change transform
      Rotation directly that leads to physics objects synchronization and
      can make your player can fall off the level - especially when it's done
      every frame) You can use TRotateRigidBody behavior for that purpose.
    - For cammera following you can use TFollowingTargetForCamera behavior (add it
      to your camera )
    - After adding TModularMovement add some movement modules e.g. TWalk3DSupport
    - Do not afraid to change/add your own movement modules - this class is
      designed for that :)
    - For mouse look use Container.StartMouseLook(Viewport); and Container.StopMouseLook;
    - See /examples/physics/physics_3d_third_person

    2D Platformer Game - How To Use:
    - Player rigid body rotation should be blocked in all directions
    - Player translation in Z axis should be blocked
    - GravityStrength should be about 1200 (9.81 is good for 3D games)
    - After adding TModularMovement add some movement modules e.g. TPlatformer2DWalkSupport
    - Do not afraid to change/add your own movement modules - this class is
      designed for that :)
    - See /examples/physics/physics_2d_movement
    }
  TModularMovement = class(TAbstractModularMovement)
  strict private
    FForwardInputAxis: TCastleInputAxis;
    FSidewayInputAxis: TCastleInputAxis;
    FInputJump: TInputShortcut;
    FIsFirstJumpingFrame: Boolean;
    FIsPlayerOnGround: Boolean;
    FGroundPhysicsLayers: TPhysicsLayers;
  protected
    { Gets transform direction with Y component. }
    function GetFullForwardDirection: TVector3; virtual;
    { Gets transform direction and sets Y component to zero. }
    function GetForwardDirection: TVector3; virtual;

    { Gets direction from input, Y means jump }
    function GetDirectionFromInput: TVector3; virtual;

    { Checks player is on ground called in Update() }
    function CheckIsPlayerOnGround(const PlayerRigidBody: TCastleRigidBody;
      const PlayerCollider: TCastleCollider): Boolean; virtual;

    { Checks input, is player on ground, creates TModularMovementState and calls
     UpdateMovement() on all movement modules }
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Returns true when this is the first frame of jump, good moment to change
      animation or play jump sound. Note that IsPlayerOnGround can be true in
      that moment }
    property IsFirstJumpingFrame: Boolean read FIsFirstJumpingFrame;
    { Return true when player is on ground, GroundPhysicsLayers are used
      in ground checking }
    property IsPlayerOnGround: Boolean read FIsPlayerOnGround;
  published
    { Move forward/backward input axis }
    property ForwardInputAxis: TCastleInputAxis read FForwardInputAxis;
    { Move right/left input axis }
    property SidewayInputAxis: TCastleInputAxis read FSidewayInputAxis;
    { Input shortcut for jump }
    property InputJump: TInputShortcut read FInputJump;
    { Physical layers considered as ground - used for checking player is on ground }
    property GroundPhysicsLayers: TPhysicsLayers read FGroundPhysicsLayers
      write FGroundPhysicsLayers default AllLayers;
  end;

implementation

uses Math, CastleBoxes, CastleUtils, CastleComponentSerialize, CastleKeysMouse,
  CastleLog;

{ TAbstractMovementModule ---------------------------------------------------- }

constructor TAbstractMovementModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExists := true;
end;

function TAbstractMovementModule.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'Exists'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TAbstractMovementModule.SetExists(const AValue: Boolean);
begin
  FExists := AValue;
end;

{ TModularMovement ----------------------------------------------------------- }

constructor TModularMovement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGroundPhysicsLayers := AllLayers;

  FForwardInputAxis := TCastleInputAxis.Create(Self);
  FForwardInputAxis.SetSubComponent(true);
  FForwardInputAxis.PositiveKey := keyW;
  FForwardInputAxis.NegativeKey := keyS;

  FSidewayInputAxis := TCastleInputAxis.Create(Self);
  FSidewayInputAxis.SetSubComponent(true);
  FSidewayInputAxis.PositiveKey := keyD;
  FSidewayInputAxis.NegativeKey := keyA;

  FInputJump := TInputShortcut.Create(Self);
  InputJump.Assign(keySpace);
  InputJump.SetSubComponent(true);
  InputJump.Name := 'InputJump';
end;

function TModularMovement.GetFullForwardDirection: TVector3;
begin
  Result := Parent.Direction;
end;

function TModularMovement.GetForwardDirection: TVector3;
begin
  Result := GetFullForwardDirection;
  Result.Y := 0;
end;

function TModularMovement.GetDirectionFromInput: TVector3;
begin
  Result := Vector3(0, 0, 0);

  if FocusedContainer = nil then
    Exit;

  Result := Result + Vector3(SidewayInputAxis.Value(FocusedContainer), 0,
  FForwardInputAxis.Value(FocusedContainer));

  if InputJump.IsPressed(FocusedContainer) then
    Result := Result + Vector3(0, 1, 0);
end;

function TModularMovement.CheckIsPlayerOnGround(
  const PlayerRigidBody: TCastleRigidBody;
  const PlayerCollider: TCastleCollider): Boolean;
var
  ColliderBoundingBox: TBox3D;
  ColliderHeight: Single;
  ColliderRadius: Single;
  SphereCastOrigin: TVector3;
  { Needed to be non-zero when casted sphere is bigger or equal than
    ColliderHeight / 2 because kraft
    do not see any bodies that it hit while casting. This can happen when our
    player gently digs into the ground. }
  SphereCastOriginUpAdjustment: Single;

  DistanceToGround: Single;
  GroundSphereCast: TPhysicsRayCastResult;
begin
  { Check player is on ground, we use collider size multiplied by three to try
    found ground.

    We need add Collider.Translation because sometimes rigid body origin can be
    under the collider. And ray will be casted under the floor. }
  ColliderBoundingBox := PlayerCollider.ScaledLocalBoundingBox;
  ColliderHeight := ColliderBoundingBox.SizeY;
  { From testing average size is the best here, better than min or max size.
    We multiply it by 0.999 becouse it should be a little smaller than collider }
  ColliderRadius := ((ColliderBoundingBox.SizeX + ColliderBoundingBox.SizeZ) / 4) * 0.999;
  SphereCastOrigin := PlayerCollider.Middle;
  SphereCastOriginUpAdjustment := 0;

  { Another approach: When casting sphere is equal or bigger than ColliderHeight / 2
    we need move it up or reduce sphere size }
  {if not (ColliderRadius < ColliderHeight / 2 * 0.9) then
     ColliderRadius := ColliderHeight / 2 * 0.9;}

  { Adjust sphere cast origin when radius is equal or bigger than ColliderHeight / 2
    We use here ColliderHeight * 0.1 not simply 0.1 because simple value like 0.1
    has problems on moving up platforms - when ColliderRadius size is near
    ColliderHeight / 2. Then the casted sphere can intersect the platform on
    casting. That make the platform is not included in spherecast tests.

    Another way to fix that is raycast when rigid body moves up and sphere cast
    hits nothing. }
  if ColliderRadius - ColliderHeight / 2 > - ColliderHeight * 0.1  then
  begin
    SphereCastOriginUpAdjustment := ColliderRadius - ColliderHeight / 2 + ColliderHeight * 0.1;
    SphereCastOrigin.Y := SphereCastOrigin.Y + SphereCastOriginUpAdjustment;
  end;

  GroundSphereCast := PlayerRigidBody.PhysicsSphereCast(
    SphereCastOrigin,
    ColliderRadius,
    Vector3(0, -1, 0),
    ColliderHeight * 1.5,
    GroundPhysicsLayers
  );

  if GroundSphereCast.Hit then
  begin
    DistanceToGround := GroundSphereCast.Distance;

    { Remove half of full collider height and cast adjustment - we cast sphere
      from middle of collider with adjustment when casted sphere radius
      is equal or bigger than ColliderHeight / 2 }
    DistanceToGround  := DistanceToGround - (ColliderHeight / 2 + SphereCastOriginUpAdjustment);

    { When we use sphere cast we also should add its radius.
      Distance is from cast origin to "moved" casted sphere origin. }
    DistanceToGround := DistanceToGround + ColliderRadius;

    { Sometimes rigid body center point can be under the collider so
      the distance can be negative - mostly when player dig a little in ground }
    if DistanceToGround < 0 then
      DistanceToGround := 0;

    { We assume that the player is on the ground a little faster to allow
     smoother control }
    Result := DistanceToGround < ColliderHeight * 0.04;
    {if Result then
      WritelnLog('on ground (distance ' + FloatToStr(DistanceToGround) + ')')
    else
      WritelnLog('not on ground (distance ' + FloatToStr(DistanceToGround) + ')');}
  end else
  begin
    Result := false;
    {WritelnLog('not on ground');}
  end;
end;

procedure TModularMovement.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RBody: TCastleRigidBody;
  Collider: TCastleCollider;
  IsOnGroundBool: Boolean;

  InputDirection: TVector3;
  ForwardDirection: TVector3;
  RightDirection: TVector3;
  UpDirection: TVector3;

  Beh: TCastleBehavior;

  HorizontalVelocity: TVector3;

  MovementState: TModularMovementState;
  I: Integer;
begin
  RBody := Parent.RigidBody;
  if not Assigned(RBody) then
    Exit;

  Collider := Parent.Collider;
  if not Assigned(Collider) then
    Exit;

  IsOnGroundBool := CheckIsPlayerOnGround(RBody, Collider);

  { Get all directions }
  InputDirection := GetDirectionFromInput;
  ForwardDirection := GetForwardDirection;
  UpDirection := Parent.Up;
  RightDirection := TVector3.CrossProduct(ForwardDirection, UpDirection);

  { HorizontalVelocity and VerticalVelocity based on previous values }
  HorizontalVelocity := RBody.LinearVelocity;
  HorizontalVelocity.Y := 0;

  { Check for modules and launch them }
  MovementState := TModularMovementState.Create;
  try
    MovementState.SecondsPassed := SecondsPassed;
    MovementState.RigidBody := RBody;
    MovementState.Collider := Collider;
    MovementState.IsPlayerOnGround := IsOnGroundBool;
    MovementState.IsFirstJumpingFrame := false;
    MovementState.IsMoving := not HorizontalVelocity.IsZero;
    MovementState.FullForwardDirection := GetFullForwardDirection;
    MovementState.ForwardDirection := ForwardDirection;
    MovementState.RightDirection := RightDirection;
    MovementState.UpDirection := UpDirection;
    MovementState.InputDirection := InputDirection;

    for I := 0 to Parent.BehaviorsCount - 1 do
    begin
      Beh := Parent.Behaviors[I];
      if (Beh is TAbstractMovementModule) and
         (TAbstractMovementModule(Beh).Exists) then
        TAbstractMovementModule(Beh).UpdateMovement(MovementState);
    end;

    FIsPlayerOnGround := MovementState.IsPlayerOnGround;
    FIsFirstJumpingFrame := MovementState.IsFirstJumpingFrame;
  finally
    FreeAndNil(MovementState);
  end;

  inherited Update(SecondsPassed, RemoveMe);
end;

function TModularMovement.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'ForwardInputAxis', 'SidewayInputAxis',
     'InputJump', 'GroundPhysicsLayers'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TModularMovement, ['Navigation', 'Modular Player Movement System']);

end.

