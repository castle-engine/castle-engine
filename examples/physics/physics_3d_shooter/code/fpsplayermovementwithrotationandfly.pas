unit FpsPlayerMovementWithRotationAndFly;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleInputs,
  CastleVectors, CastleUIControls, CastleViewport, CastleClassUtils;

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

    FInputForward: TInputShortcut;
    FInputBackward: TInputShortcut;
    FInputRightStrafe: TInputShortcut;
    FInputLeftStrafe: TInputShortcut;
    FInputJump: TInputShortcut;
    FInputFly: TInputShortcut;
    FInputFlyUp: TInputShortcut;
    FInputFlyDown: TInputShortcut;

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

    property InputForward: TInputShortcut read FInputForward;
    property InputBackward: TInputShortcut read FInputBackward;
    property InputLeftStrafe: TInputShortcut read FInputLeftStrafe;
    property InputRightStrafe: TInputShortcut read FInputRightStrafe;
    property InputJump: TInputShortcut read FInputJump;
    property InputFly: TInputShortcut read FInputFly;
    property InputFlyUp: TInputShortcut read FInputFlyUp;
    property InputFlyDown: TInputShortcut read FInputFlyDown;
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

  if InputForward.IsPressed(FocusedContainer) then
    Result := Result + Vector3(0, 0, -1);

  if InputBackward.IsPressed(FocusedContainer) then
    Result := Result + Vector3(0, 0, 1);

  if InputRightStrafe.IsPressed(FocusedContainer) then
    Result := Result + Vector3(-1, 0, 0);

  if InputLeftStrafe.IsPressed(FocusedContainer) then
    Result := Result + Vector3(1, 0, 0);

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
    if InputFlyUp.IsPressed(FocusedContainer) then
      VerticalVelocity := VerticalVelocity + FFlyUpDownSpeed * SecondsPassed;
    if InputFlyDown.IsPressed(FocusedContainer) then
      VerticalVelocity := VerticalVelocity - FFlyUpDownSpeed * SecondsPassed;
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

  FInputForward                 := TInputShortcut.Create(Self);
  FInputBackward                := TInputShortcut.Create(Self);
  FInputLeftStrafe              := TInputShortcut.Create(Self);
  FInputRightStrafe             := TInputShortcut.Create(Self);
  FInputJump                    := TInputShortcut.Create(Self);
  FInputFly                     := TInputShortcut.Create(Self);
  FInputFlyUp                   := TInputShortcut.Create(Self);
  FInputFlyDown                 := TInputShortcut.Create(Self);

  InputForward                 .Assign(keyW, keyArrowUp);
  InputBackward                .Assign(keyS, keyArrowDown);
  InputLeftStrafe              .Assign(keyA);
  InputRightStrafe             .Assign(keyD);
  { For move speed we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  InputJump                    .Assign(keySpace);
  InputFly                     .Assign(keyF);
  InputFlyUp                   .Assign(keyT);
  InputFlyDown                 .Assign(keyG);

  InputForward                .SetSubComponent(true);
  InputBackward               .SetSubComponent(true);
  InputLeftStrafe             .SetSubComponent(true);
  InputRightStrafe            .SetSubComponent(true);
  InputJump                   .SetSubComponent(true);
  InputFly                    .SetSubComponent(true);
  InputFlyUp                  .SetSubComponent(true);
  InputFlyDown                .SetSubComponent(true);

  InputForward                .Name := 'Input_Forward';
  InputBackward               .Name := 'Input_Backward';
  InputLeftStrafe             .Name := 'Input_LeftStrafe';
  InputRightStrafe            .Name := 'Input_RightStrafe';
  InputJump                   .Name := 'Input_Jump';
  InputFly                    .Name := 'Input_Fly';
  InputFlyUp                  .Name := 'Input_FlyUp';
  InputFlyDown                .Name := 'Input_FlyDown';
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

