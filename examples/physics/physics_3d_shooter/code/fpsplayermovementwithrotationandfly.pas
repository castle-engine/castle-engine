unit FpsPlayerMovementWithRotationAndFly;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleInputs,
  CastleVectors, CastleUIControls, CastleViewport, CastleClassUtils,
  GameInputAxis;

type

  { The simple FPS (First Person Shooter) physics movement using dynamic
    rigid body with rotation and direction from player.


    - Move right/left/forward/back
    - Constant speed
    - Player rigid body rotating only in Y axis (horizontal) - should be blocked in rigid body
    - Rotation (direction.XZ) from player, camera should be player child (should rotate only in horizontal)
    - No control in air
    - Uses parent.up(), never camera up to deremine direction of the velocity vector
    - fly support when F pressed
  }

  TFpsPlayerMovementWithRotationAndFly = class(TCastleBehavior)
  strict private
    FWasJumpInput: Boolean;

    FForwardInputAxis: TCastleInputAxis;
    FSidewayInputAxis: TCastleInputAxis;
    FFlyUpDownInputAxis: TCastleInputAxis;
    FInputJump: TInputShortcut;
    FInputFly: TInputShortcut;

    FHorizontalSpeed: Single;
    FJumpSpeed: Single;
    FFlyUpDownSpeed: Single;

    FIsFlying: Boolean;
  protected
    function GetForwardDirection: TVector3; virtual;

    function GetDirectionFromInput: TVector3; virtual;

    function IsPlayerOnGround(const PlayerRigidBody: TCastleRigidBody;
      const PlayerCollider: TCastleCollider): Boolean; virtual;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    procedure CheckIsFlying(const PlayerRigidBody: TCastleRigidBody);
  public
    const
      DefaultJumpSpeed = 7.0;
      DefaultHorizontalSpeed = 5.0;

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

  published
    property JumpSpeed: Single read FJumpSpeed write FJumpSpeed
      {$ifdef FPC}default DefaultJumpSpeed{$endif};

    property HorizontalSpeed: Single read FHorizontalSpeed write FHorizontalSpeed
      {$ifdef FPC}default DefaultHorizontalSpeed{$endif};

    property ForwardInputAxis: TCastleInputAxis read FForwardInputAxis;
    property SidewayInputAxis: TCastleInputAxis read FSidewayInputAxis;
    property FlyUpDownInputAxis: TCastleInputAxis read FFlyUpDownInputAxis;
    property InputJump: TInputShortcut read FInputJump;
    property InputFly: TInputShortcut read FInputFly;
  end;


implementation

uses Math, CastleBoxes, CastleKeysMouse, CastleComponentSerialize, CastleLog,
  CastleUtils;

function TFpsPlayerMovementWithRotationAndFly.GetForwardDirection: TVector3;
begin
  Result := Parent.Direction;
  if not FIsFlying then
    Result.Y := 0;
end;

function TFpsPlayerMovementWithRotationAndFly.GetDirectionFromInput: TVector3;
begin
  Result := Vector3(0, 0, 0);

  if FocusedContainer = nil then
    Exit;

  Result := Result + Vector3(-SidewayInputAxis.Value(FocusedContainer), 0,
  -FForwardInputAxis.Value(FocusedContainer));

  if InputJump.IsPressed(FocusedContainer) then
    Result := Result + Vector3(0, 1, 0);
end;

function TFpsPlayerMovementWithRotationAndFly.IsPlayerOnGround(
  const PlayerRigidBody: TCastleRigidBody;
  const PlayerCollider: TCastleCollider): Boolean;
var
  ColliderBoundingBox: TBox3D;
  ColliderHeight: Single;
  ColliderRadius: Single;
  SphereOrigin: TVector3;

  DistanceToGround: Single;
  GroundSphereCast: TPhysicsRayCastResult;
begin
  { Check player is on ground, we use collider size multiplied by three to try
    found ground.

    We need add Collider.Translation because sometimes rigid body origin can be
    under the collider. And ray will be casted under the floor. }
  ColliderBoundingBox := PlayerCollider.ScaledLocalBoundingBox;
  ColliderHeight := ColliderBoundingBox.SizeY;
  ColliderRadius := Iff(ColliderBoundingBox.SizeX > ColliderBoundingBox.SizeZ,
    ColliderBoundingBox.SizeX, ColliderBoundingBox.SizeZ);
  SphereOrigin := Parent.Translation + PlayerCollider.Translation;

  GroundSphereCast := PlayerRigidBody.PhysicsSphereCast(
    SphereOrigin,
    ColliderRadius,
    Vector3(0, -1, 0),
    ColliderHeight * 3
  );

  if GroundSphereCast.Hit then
  begin
    DistanceToGround := GroundSphereCast.Distance;

    { When collider has own translation we need substract it from distance
      becouse distance will be too big }
    DistanceToGround := DistanceToGround - PlayerCollider.Translation.Y;

    { When we use sphere cast we also should remove it radius }
    DistanceToGround := DistanceToGround - ColliderRadius;

    { Sometimes rigid body center point can be under the collider so
      the distance can be negative }
    if DistanceToGround < 0 then
      DistanceToGround := 0;

    { We use ColliderHeight / 2 because the cast origin is in
      center of collider and ColliderHeight * 0.1 to give player control
      a little faster }
    Result := DistanceToGround < (ColliderHeight / 2) + ColliderHeight * 0.1;
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

procedure TFpsPlayerMovementWithRotationAndFly.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RBody: TCastleRigidBody;
  Collider: TCastleCollider;
  IsOnGroundBool: Boolean;

  InputDirection: TVector3;
  ForwardDirection: TVector3;
  RightDirection: TVector3;
  UpDirection: TVector3;

  HorizontalVelocity: TVector3;
  VerticalVelocity: Single;

  IntegratedVelocities: TVector3;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if not Assigned(RBody) then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if not Assigned(Collider) then
    Exit;

  CheckIsFlying(RBody);

  IsOnGroundBool := IsPlayerOnGround(RBody, Collider);

  { No support for air movement }
  if (not IsOnGroundBool) and (not FIsFlying) then
    Exit;

  { Get all directions }
  InputDirection := GetDirectionFromInput;
  ForwardDirection := GetForwardDirection;
  UpDirection := Parent.Up;
  RightDirection := TVector3.CrossProduct(ForwardDirection, UpDirection);

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

  if IsOnGroundBool then
    FWasJumpInput := false;

  if (FWasJumpInput = false) and not IsZero(InputDirection.Y) then
  begin
    FWasJumpInput := true;
    VerticalVelocity := JumpSpeed;
  end else
    VerticalVelocity := RBody.LinearVelocity.Y;

  if FIsFlying then
  begin
    VerticalVelocity := VerticalVelocity + FFlyUpDownSpeed * SecondsPassed *
    FFlyUpDownInputAxis.Value(FocusedContainer);
  end;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  RBody.LinearVelocity := IntegratedVelocities;
  inherited Update(SecondsPassed, RemoveMe);
end;

procedure TFpsPlayerMovementWithRotationAndFly.CheckIsFlying(const PlayerRigidBody: TCastleRigidBody);
begin
  FIsFlying := InputFly.IsPressed(FocusedContainer);
  PlayerRigidBody.Gravity := not FIsFlying;
end;

constructor TFpsPlayerMovementWithRotationAndFly.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWasJumpInput := false;
  FJumpSpeed := DefaultJumpSpeed;
  FHorizontalSpeed := DefaultHorizontalSpeed;
  FFlyUpDownSpeed := 2;

  FForwardInputAxis := TCastleInputAxis.Create(Self);
  FForwardInputAxis.SetSubComponent(true);
  FForwardInputAxis.PositiveKey := keyW;
  FForwardInputAxis.NegativeKey := keyS;

  FSidewayInputAxis := TCastleInputAxis.Create(Self);
  FSidewayInputAxis.SetSubComponent(true);
  FSidewayInputAxis.PositiveKey := keyD;
  FSidewayInputAxis.NegativeKey := keyA;

  FFlyUpDownInputAxis := TCastleInputAxis.Create(Self);
  FFlyUpDownInputAxis.SetSubComponent(true);
  FFlyUpDownInputAxis.PositiveKey := keyT;
  FFlyUpDownInputAxis.NegativeKey := keyG;

  FInputJump := TInputShortcut.Create(Self);
  InputJump.Assign(keySpace);
  InputJump.SetSubComponent(true);
  InputJump.Name := 'Input_Jump';

  FInputFly := TInputShortcut.Create(Self);
  InputFly.Assign(keyF);
  InputFly.SetSubComponent(true);
  InputFly.Name := 'Input_Fly';
end;

function TFpsPlayerMovementWithRotationAndFly.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'HorizontalSpeed', 'JumpSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


initialization
  RegisterSerializableComponent(TFpsPlayerMovementWithRotationAndFly, ['Physics', 'Simple FPS Player Movement With Player Rotation And Fly Support']);

end.

