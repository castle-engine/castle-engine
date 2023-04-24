unit Move3DPlayerDynamic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleInputs;

type

  TMove3DPlayerDynamic = class(TCastleBehavior)
  strict private
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_RightRotate: TInputShortcut;
    FInput_LeftRotate: TInputShortcut;
    FInput_RightStrafe: TInputShortcut;
    FInput_LeftStrafe: TInputShortcut;
    FInput_UpRotate: TInputShortcut;
    FInput_DownRotate: TInputShortcut;
    FInput_IncreasePreferredHeight: TInputShortcut;
    FInput_DecreasePreferredHeight: TInputShortcut;
    FInput_GravityUp: TInputShortcut;
    FInput_MoveSpeedInc: TInputShortcut;
    FInput_MoveSpeedDec: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FInput_Crouch: TInputShortcut;
    FInput_Run: TInputShortcut;

    FWasJumpInput: Boolean;
  private
    function GetDirection: TVector3;
    function Container: TCastleContainer;
  protected
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
    constructor Create(AOwner: TComponent); override;

    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_LeftRotate: TInputShortcut read FInput_LeftRotate;
    property Input_RightRotate: TInputShortcut read FInput_RightRotate;
    {$ifdef FPC}
    property Input_LeftRot: TInputShortcut read FInput_LeftRotate; deprecated 'use Input_LeftRotate';
    property Input_RightRot: TInputShortcut read FInput_RightRotate; deprecated 'use Input_RightRotate';
    {$endif}
    property Input_LeftStrafe: TInputShortcut read FInput_LeftStrafe;
    property Input_RightStrafe: TInputShortcut read FInput_RightStrafe;
    property Input_UpRotate: TInputShortcut read FInput_UpRotate;
    property Input_DownRotate: TInputShortcut read FInput_DownRotate;
    property Input_IncreasePreferredHeight: TInputShortcut read FInput_IncreasePreferredHeight;
    property Input_DecreasePreferredHeight: TInputShortcut read FInput_DecreasePreferredHeight;
    property Input_GravityUp: TInputShortcut read FInput_GravityUp;
    property Input_Run: TInputShortcut read FInput_Run;

    { Change the MoveSpeed.
      @groupBegin }
    property Input_MoveSpeedInc: TInputShortcut read FInput_MoveSpeedInc;
    property Input_MoveSpeedDec: TInputShortcut read FInput_MoveSpeedDec;
    { @groupEnd }

    { Jumping and crouching (when @link(Gravity) = @true) or flying up / down
      (when @link(Gravity) = @false).
      @groupBegin }
    property Input_Jump: TInputShortcut read FInput_Jump;
    property Input_Crouch: TInputShortcut read FInput_Crouch;
    { @groupEnd }
  end;


implementation

function TMove3DPlayerDynamic.GetDirection: TVector3;
var
  Camera: TCastleCamera;
  I: Integer;
begin
  for I := 0 to Parent.Count -1 do
  begin
    if Parent.Items[I] is TCastleCamera then
    begin
      Exit(Parent.Items[I].Direction);
    end;
  end;

  Result := Parent.Direction;
end;

function TMove3DPlayerDynamic.Container: TCastleContainer;
begin
  Result := TCastleViewport(Parent.World.Owner).Container;
end;

procedure TMove3DPlayerDynamic.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
type
  TIsOnGround = (igGround, igJumping, igFalling);
var
  RBody: TCastleRigidBody;
  Collider: TCastleCollider;
  IsOnGroundBool: Boolean;
  Vel: TVector3;
  VLength: Single;
  ColliderBoundingBox: TBox3D;
  ColliderHeight: Single;
  MaxHorizontalVelocityChange: Single;
  Acceleration: Single;
  HVelocity: TVector3;
  VVelocity: Single;
  MoveDirection: TVector3;
  GroundRayCast: TPhysicsRayCastResult;
  DistanceToGround: Single;
  Jump: Single;
  RayOrigin: TVector3;
  DeltaSpeed: Single;
  DeltaAngular: Single;
  MovingHorizontally: Boolean;
  Rotating: Boolean;
  IsOnGround: TIsOnGround;
const
  RotationSpeed: Single = Pi * 150 / 180;
  Speed: Single = 5;
  MoveVerticalSpeed: Single = 1;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;

  if not Assigned(RBody) then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if not Assigned(Collider) then
    Exit;

  { How fast should avatar change it's speed }
  Acceleration := Speed * 3 / 60;
  MaxHorizontalVelocityChange := Acceleration * 60;
  DeltaSpeed := 0;

  { Check player is on ground, we use avatar size multiplied by ten to try
    found ground. Distance is used to check we should set animation to fall
    or we are almost on ground so use default animation.

    We need add Collider.Translation because sometimes rigid body origin can be
    under the collider. And ray will be casted under the floor. }
  // TODO: what use as height, currently collider in camera
  ColliderBoundingBox := Collider.ScaledLocalBoundingBox;
  ColliderHeight :=  ColliderBoundingBox.SizeY;
  RayOrigin := Parent.Translation + Collider.Translation;

  { TODO: In the ideal world, the way we check for ground collisions
    (and determine Ground, IsOnGround)
    should be independent from ChangeTransformation.

    ChangeTransformation says how we change the transformation.

    We should still have option to use

    - PhysicsRayCast (maybe from TCastleAbstractRootTransform, as it should
      not require having TCastleRigidBody on avatar) to detect ground
    - or Height / WorldHeight calls that cooperate with old simple physics.

    And we should update IsOnGround in all ChangeTransformation modes.

    But in practice, now ctDirect forces to do gravity using old physics
    (because it forbids TCastleRigidBody on avatar),
    and ctVelocity forces to do gravity using new physics
    (because it requires TCastleRigidBody on avatar).

    So checking for ground (collisions) is not independent from ChangeTransformation.
    When ctVelocity, we have to check for ground using real physics (PhysicsRayCast),
    it would make no sense to use old simple physics. }

  GroundRayCast := RBody.PhysicsRayCast(
    RayOrigin,
    Vector3(0, -1, 0),
    ColliderHeight * 3
  );

  { Four more checks - player should slide down when player just
    on the edge, but sometimes it stay and center ray don't "see" that we are
    on ground }
  if not GroundRayCast.Hit then
    GroundRayCast := RBody.PhysicsRayCast(
      RayOrigin + Vector3(ColliderBoundingBox.SizeX * 0.49, 0, 0),
      Vector3(0, -1, 0),
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := RBody.PhysicsRayCast(
      RayOrigin + Vector3(-ColliderBoundingBox.SizeX * 0.49, 0, 0),
      Vector3(0, -1, 0),
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := RBody.PhysicsRayCast(
      RayOrigin + Vector3(0, 0, ColliderBoundingBox.SizeZ * 0.49),
      Vector3(0, -1, 0),
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := RBody.PhysicsRayCast(
      RayOrigin + Vector3(0, 0, -ColliderBoundingBox.SizeZ * 0.49),
      Vector3(0, -1, 0),
      ColliderHeight * 3
    );

  if GroundRayCast.Hit then
  begin
    DistanceToGround := GroundRayCast.Distance;

    { When collider has own translation we need substract it from distance
      becouse distance will be too big }
    DistanceToGround  := DistanceToGround - Collider.Translation.Y;

    { Sometimes rigid body center point can be under the collider so
      the distance can be negative }
    if DistanceToGround < 0 then
      DistanceToGround := 0;

    IsOnGroundBool := DistanceToGround < (ColliderHeight / 2) + ColliderHeight * 0.1;
  end else
  begin
    IsOnGroundBool := false;
    DistanceToGround := -1; // For animation checking
  end;

  {if IsOnGroundBool then
    WritelnLog(' On ground')
  else
    WritelnLog(' NOT on ground');}

  if Input_Forward.IsPressed(Container) then
  begin
    MovingHorizontally := true;
    DeltaSpeed := MaxHorizontalVelocityChange * SecondsPassed {* MovementControlFactor(IsOnGroundBool)};
    MoveDirection := GetDirection;
  end;
  if Input_Backward.IsPressed(Container) then
  begin
    MovingHorizontally := true;
    DeltaSpeed := MaxHorizontalVelocityChange * SecondsPassed {* MovementControlFactor(IsOnGroundBool)};
    MoveDirection := -GetDirection;
  end;
  if IsOnGroundBool and Input_RightStrafe.IsPressed(Container) then
  begin
    MovingHorizontally := true;
    DeltaSpeed := MaxHorizontalVelocityChange * SecondsPassed;
    MoveDirection := TVector3.CrossProduct(GetDirection, Parent.Up);
  end;
  if IsOnGroundBool and Input_LeftStrafe.IsPressed(Container) then
  begin
    MovingHorizontally := true;
    DeltaSpeed := MaxHorizontalVelocityChange * SecondsPassed {* MovementControlFactor(IsOnGroundBool)};
    MoveDirection := -TVector3.CrossProduct(GetDirection, Parent.Up);
  end;

  Jump := 0;
  if Input_Jump.IsPressed(Container) and (not FWasJumpInput) and IsOnGroundBool then
  begin
    //if  and (not FWasJumpInput) and IsOnGroundBool
    FWasJumpInput := true;
    MovingHorizontally := false;
    Jump := MoveVerticalSpeed * 2; ///JumpSpeed;
  end else
    FWasJumpInput := false;

  DeltaAngular := 0;
  if Input_RightRotate.IsPressed(Container) then
  begin
    DeltaAngular := -RotationSpeed * 60 * SecondsPassed {* RotationControlFactor(IsOnGroundBool)};
  end;
  if Input_LeftRotate.IsPressed(Container) then
  begin
    DeltaAngular := RotationSpeed * 60 * SecondsPassed {* RotationControlFactor(IsOnGroundBool)};
  end;

  // jumping
  if not IsZero(Jump) then
  begin
    Vel := RBody.LinearVelocity;
    Vel.Y := Jump;
    RBody.LinearVelocity := Vel;
  end else
  // moving
  if not IsZero(DeltaSpeed) then
  begin
    Vel := RBody.LinearVelocity;
    if IsOnGroundBool then
    begin
      { On ground we simply change direction to current one that's
        helps do things like strafe or fast change direction from
        forward to backward }
      HVelocity := Vel;
      HVelocity.Y := 0;
      VVelocity := Vel.Y;
      // maybe use LengthSqrt?
      VLength := HVelocity.Length;
      VLength := VLength + DeltaSpeed;
      if VLength > Speed then
          VLength := Speed;
      Vel := MoveDirection * VLength;

      if IsZero(Jump) then
        Vel.Y := VVelocity
      else
        Vel.Y := Jump;
    end else
    begin
      { In air we can't simply change movement direction, we will just
        modify current one a little based on FAirMovementControl factor.
        Notice that by default FAirMovementControl = 0 so no change
        will be made. }

      Vel := Vel + MoveDirection * DeltaSpeed;

      { Here we only check speed is not faster than max speed }
      HVelocity := Vel;
      HVelocity.Y := 0;
      VVelocity := Vel.Y;
      VLength := HVelocity.Length;
      { Check max speed }
      if VLength > Speed then
      begin
          VLength := Speed;
          Vel.Y := 0;
          Vel := Vel.Normalize * VLength;

          { Add gravity here }
          Vel.Y := VVelocity;
      end;
    end;

    RBody.LinearVelocity := Vel;
  end else
  if IsOnGroundBool then
  begin
    // slowing down the avatar only on ground
    Vel := RBody.LinearVelocity;
    Vel.X := 0;
    Vel.Z := 0;
    RBody.LinearVelocity := Vel;
  end;

  // rotation
  if not IsZero(DeltaAngular) then
  begin
    RBody.AngularVelocity := Vector3(0, 1, 0) * DeltaAngular;
    Rotating := true;
  end
  else
  begin
    RBody.AngularVelocity := Vector3(0, 0, 0);
    Rotating := false;
  end;

  IsOnGround := igGround;
  if not IsOnGroundBool then
  begin
    // TODO: 0.1 should not be hardcoded
    if RBody.LinearVelocity.Y > 0.1 then
      IsOnGround := igJumping
    else
    { When avatar falls we change animation to fall only when the distance
      to ground is smaller than 1/4 of avatar height. This fixes changing
      animation from walk to fall on small steps like in stairs.

      DistanceToGround < 0 means that we are in air and ground
      was not found.

      TODO: 0.25 should not be hardcoded. }
    if (DistanceToGround < 0) or (DistanceToGround > ColliderBoundingBox.SizeY * 0.25) then
      IsOnGround := igFalling;
  end;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TMove3DPlayerDynamic.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWasJumpInput := false;


  FInput_Forward                 := TInputShortcut.Create(Self);
  FInput_Backward                := TInputShortcut.Create(Self);
  FInput_LeftRotate              := TInputShortcut.Create(Self);
  FInput_RightRotate             := TInputShortcut.Create(Self);
  FInput_LeftStrafe              := TInputShortcut.Create(Self);
  FInput_RightStrafe             := TInputShortcut.Create(Self);
  FInput_UpRotate                := TInputShortcut.Create(Self);
  FInput_DownRotate              := TInputShortcut.Create(Self);
  FInput_IncreasePreferredHeight := TInputShortcut.Create(Self);
  FInput_DecreasePreferredHeight := TInputShortcut.Create(Self);
  FInput_GravityUp               := TInputShortcut.Create(Self);
  FInput_MoveSpeedInc            := TInputShortcut.Create(Self);
  FInput_MoveSpeedDec            := TInputShortcut.Create(Self);
  FInput_Jump                    := TInputShortcut.Create(Self);
  FInput_Crouch                  := TInputShortcut.Create(Self);
  FInput_Run                     := TInputShortcut.Create(Self);

  Input_Forward                 .Assign(keyW, keyArrowUp);
  Input_Backward                .Assign(keyS, keyArrowDown);
  Input_LeftRotate              .Assign(keyArrowLeft);
  Input_RightRotate             .Assign(keyArrowRight);
  Input_LeftStrafe              .Assign(keyA);
  Input_RightStrafe             .Assign(keyD);
  Input_UpRotate                .Assign(keyNone);
  Input_DownRotate              .Assign(keyNone);
  Input_IncreasePreferredHeight .Assign(keyNone);
  Input_DecreasePreferredHeight .Assign(keyNone);
  Input_GravityUp               .Assign(keyNone);
  { For move speed we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  Input_MoveSpeedInc            .Assign(keyNumpadPlus , keyNone, '+');
  Input_MoveSpeedDec            .Assign(keyNumpadMinus, keyNone, '-');
  Input_Jump                    .Assign(keySpace);
  Input_Crouch                  .Assign(keyC);
  Input_Run                     .Assign(keyShift);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_LeftRotate             .SetSubComponent(true);
  Input_RightRotate            .SetSubComponent(true);
  Input_LeftStrafe             .SetSubComponent(true);
  Input_RightStrafe            .SetSubComponent(true);
  Input_UpRotate               .SetSubComponent(true);
  Input_DownRotate             .SetSubComponent(true);
  Input_IncreasePreferredHeight.SetSubComponent(true);
  Input_DecreasePreferredHeight.SetSubComponent(true);
  Input_GravityUp              .SetSubComponent(true);
  Input_MoveSpeedInc           .SetSubComponent(true);
  Input_MoveSpeedDec           .SetSubComponent(true);
  Input_Jump                   .SetSubComponent(true);
  Input_Crouch                 .SetSubComponent(true);
  Input_Run                    .SetSubComponent(true);

  Input_Forward                .Name := 'Input_Forward';
  Input_Backward               .Name := 'Input_Backward';
  Input_LeftRotate             .Name := 'Input_LeftRotate';
  Input_RightRotate            .Name := 'Input_RightRotate';
  Input_LeftStrafe             .Name := 'Input_LeftStrafe';
  Input_RightStrafe            .Name := 'Input_RightStrafe';
  Input_UpRotate               .Name := 'Input_UpRotate';
  Input_DownRotate             .Name := 'Input_DownRotate';
  Input_IncreasePreferredHeight.Name := 'Input_IncreasePreferredHeight';
  Input_DecreasePreferredHeight.Name := 'Input_DecreasePreferredHeight';
  Input_GravityUp              .Name := 'Input_GravityUp';
  Input_MoveSpeedInc           .Name := 'Input_MoveSpeedInc';
  Input_MoveSpeedDec           .Name := 'Input_MoveSpeedDec';
  Input_Jump                   .Name := 'Input_Jump';
  Input_Crouch                 .Name := 'Input_Crouch';
  Input_Run                    .Name := 'Input_Run';
end;



initialization
  RegisterSerializableComponent(TMove3DPlayerDynamic, ['Physics', '3D Player Movement']);

end.

