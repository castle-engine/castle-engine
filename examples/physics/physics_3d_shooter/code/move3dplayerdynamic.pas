unit Move3DPlayerDynamic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleInputs,
  CastleVectors, CastleUIControls, CastleViewport;

type

  TMove3DPlayerDynamic = class(TCastleBehavior)
  strict private

    FWorldUpAxisIndex: Integer;
    FWorldUp: TVector3;

    FWasJumpInput: Boolean;

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
  private
    { Tries to find camera in parent children and get it direction or returns
      Parent direction }
    function GetDirection: TVector3;
    function Container: TCastleContainer;
  protected

    procedure WorldAfterAttach; override;

    { Returns true when there is any input for moving, Direction can be zero
      when no input, that's only keyboard input, maybe should be implemented on
      Container side }
    function GetMoveDirectionFromInput(const IsOnGround: Boolean;
      var MoveDirection: TVector3): Boolean; virtual;

    function GetSpeed: Single; virtual;
    function GetAcceleration: Single; virtual;
    function GetJumpSpeed: Single; virtual;

    function IsPlayerOnGround(const PlayerRigidBody: TCastleRigidBody;
      const PlayerCollider: TCastleCollider): Boolean; virtual;

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

uses Math, CastleBoxes, CastleKeysMouse, CastleComponentSerialize, CastleLog;

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

procedure TMove3DPlayerDynamic.WorldAfterAttach;

  procedure ConfigureUp;
  begin
    FWorldUpAxisIndex := Parent.World.GravityCoordinate;

    FWorldUp := TVector3.One[FWorldUpAxisIndex];
  end;

begin
  inherited WorldAfterAttach;

  { Support Y and Z up direction }
  ConfigureUp;
end;

function TMove3DPlayerDynamic.GetMoveDirectionFromInput(
  const IsOnGround: Boolean; var MoveDirection: TVector3): Boolean;
begin
  if FocusedContainer = nil then
    Exit(false);

  if Input_Forward.IsPressed(FocusedContainer) then
  begin
    MoveDirection := GetDirection;
    Exit(true);
  end;

  if Input_Backward.IsPressed(FocusedContainer) then
  begin
    MoveDirection := -GetDirection;
    Exit(true);
  end;

  if IsOnGround and Input_RightStrafe.IsPressed(FocusedContainer) then
  begin
    MoveDirection := TVector3.CrossProduct(GetDirection, Parent.Up);
    Exit(true);
  end;

  if IsOnGround and Input_LeftStrafe.IsPressed(FocusedContainer) then
  begin
    MoveDirection := -TVector3.CrossProduct(GetDirection, Parent.Up);
    Exit(true);
  end;

  Result := false;
end;

function TMove3DPlayerDynamic.GetSpeed: Single;
begin
  Result := 10.0;
end;

function TMove3DPlayerDynamic.GetAcceleration: Single;
begin
  Result := 1;
end;

function TMove3DPlayerDynamic.GetJumpSpeed: Single;
begin
  Result := 10.0;
end;

function TMove3DPlayerDynamic.IsPlayerOnGround(
  const PlayerRigidBody: TCastleRigidBody; const PlayerCollider: TCastleCollider
  ): Boolean;

var
  ColliderBoundingBox: TBox3D;
  ColliderHeight: Single;
  RayOrigin: TVector3;
  DistanceToGround: Single;
  GroundRayCast: TPhysicsRayCastResult;
begin
  { Check player is on ground, we use collider size multiplied by three to try
    found ground.

    We need add Collider.Translation because sometimes rigid body origin can be
    under the collider. And ray will be casted under the floor. }
  ColliderBoundingBox := PlayerCollider.ScaledLocalBoundingBox;
  ColliderHeight :=  ColliderBoundingBox.Size.Data[FWorldUpAxisIndex];
  RayOrigin := Parent.Translation + PlayerCollider.Translation;

  GroundRayCast := PlayerRigidBody.PhysicsRayCast(
    RayOrigin,
    -FWorldUp,
    ColliderHeight * 3
  );

  { Four more checks - player should slide down when player just
    on the edge, but sometimes it stay and center ray don't "see" that we are
    on ground }
  if not GroundRayCast.Hit then
    GroundRayCast := PlayerRigidBody.PhysicsRayCast(
      RayOrigin + Vector3(ColliderBoundingBox.SizeX * 0.49, 0, 0),
      -FWorldUp,
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := PlayerRigidBody.PhysicsRayCast(
      RayOrigin + Vector3(-ColliderBoundingBox.SizeX * 0.49, 0, 0),
      -FWorldUp,
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := PlayerRigidBody.PhysicsRayCast(
      RayOrigin + Vector3(0, 0, ColliderBoundingBox.SizeZ * 0.49),
      -FWorldUp,
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := PlayerRigidBody.PhysicsRayCast(
      RayOrigin + Vector3(0, 0, -ColliderBoundingBox.SizeZ * 0.49),
      -FWorldUp,
      ColliderHeight * 3
    );

  if GroundRayCast.Hit then
  begin
    DistanceToGround := GroundRayCast.Distance;

    { When collider has own translation we need substract it from distance
      becouse distance will be too big }
    DistanceToGround  := DistanceToGround - PlayerCollider.Translation.Y;

    { Sometimes rigid body center point can be under the collider so
      the distance can be negative }
    if DistanceToGround < 0 then
      DistanceToGround := 0;

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

procedure TMove3DPlayerDynamic.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  RBody: TCastleRigidBody;
  Collider: TCastleCollider;
  IsOnGroundBool: Boolean;
  Vel: TVector3;
  VLength: Single;
  HVelocity: TVector3;
  VVelocity: Single;
  MoveDirection: TVector3;
  JumpVelocity: Single;
  DeltaHVelocity: Single;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;

  if not Assigned(RBody) then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if not Assigned(Collider) then
    Exit;

  DeltaHVelocity := 0;

  IsOnGroundBool := IsPlayerOnGround(RBody, Collider);

  if GetMoveDirectionFromInput(IsOnGroundBool, MoveDirection) then
  begin
    { We get the acceleration value and that value is
      specified for 60 frames per seconds, then we must
      protect it's value to update's rate changes }
    DeltaHVelocity := GetAcceleration * 60 * SecondsPassed;
  end;

  JumpVelocity := 0;
  if (FocusedContainer <> nil) and (Input_Jump.IsPressed(Container))
    and (not FWasJumpInput) and IsOnGroundBool then
  begin
    FWasJumpInput := true;
    JumpVelocity := GetJumpSpeed; // one time event so no need Seconds Passed
  end else
    FWasJumpInput := false;

  { Integrate velocities }
  // jumping
  if not IsZero(JumpVelocity) then
  begin
    Vel := RBody.LinearVelocity;
    { Only set up (y or z) velocity to JumpVelocity }
    Vel.Data[FWorldUpAxisIndex] := JumpVelocity;
    RBody.LinearVelocity := Vel;
  end else
  // moving
  if not IsZero(DeltaHVelocity) then
  begin
    Vel := RBody.LinearVelocity;
    if IsOnGroundBool then
    begin
      { On ground we simply change direction to current one that's
        helps do things like strafe or fast change direction from
        forward to backward }
      HVelocity := Vel;
      HVelocity.Data[FWorldUpAxisIndex] := 0; // Remove up velocity to get only horizontal value
      VVelocity := Vel.Data[FWorldUpAxisIndex];
      // maybe use LengthSqrt?
      VLength := HVelocity.Length;
      VLength := VLength + DeltaHVelocity;
      if VLength > GetSpeed then
          VLength := GetSpeed;

      Vel := MoveDirection * VLength;

      if IsZero(JumpVelocity) then
        Vel.Data[FWorldUpAxisIndex] := VVelocity
      else
        Vel.Data[FWorldUpAxisIndex] := JumpVelocity;
    end else
    begin
      { In air we can't simply change movement direction, we will just
        modify current one a little based on FAirMovementControl factor.
        Notice that by default FAirMovementControl = 0 so no change
        will be made. }

      Vel := Vel + MoveDirection * DeltaHVelocity;

      { Here we only check speed is not faster than max speed }
      HVelocity := Vel;
      HVelocity.Data[FWorldUpAxisIndex] := 0;
      VVelocity := Vel.Data[FWorldUpAxisIndex];
      VLength := HVelocity.Length;
      { Check max speed }
      if VLength > GetSpeed then
      begin
          VLength := GetSpeed;
          Vel.Data[FWorldUpAxisIndex] := 0;
          Vel := Vel.Normalize * VLength;

          { Add gravity here }
          Vel.Data[FWorldUpAxisIndex] := VVelocity;
      end;
    end;

    RBody.LinearVelocity := Vel;
  end else
  if IsOnGroundBool then
  begin
    // slowing down the avatar only on ground
    Vel := RBody.LinearVelocity;

    if FWorldUpAxisIndex <> 0 then
      Vel.X := 0;
    if FWorldUpAxisIndex <> 1 then
      Vel.Y := 0;
    if FWorldUpAxisIndex <> 2 then
      Vel.Z := 0;

    RBody.LinearVelocity := Vel;
  end;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TMove3DPlayerDynamic.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenWorldChange := true;

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

