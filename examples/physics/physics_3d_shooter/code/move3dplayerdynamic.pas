unit Move3DPlayerDynamic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleInputs,
  CastleVectors, CastleUIControls, CastleViewport, CastleClassUtils;

type

  TMove3DPlayerDynamic = class(TCastleBehavior)
  strict private
    FWorldUpAxisIndex: Integer;
    FWorldUp: TVector3;

    FWasJumpInput: Boolean;

    FInputForward: TInputShortcut;
    FInputBackward: TInputShortcut;
    FInputRightStrafe: TInputShortcut;
    FInputLeftStrafe: TInputShortcut;
    FInputJump: TInputShortcut;

    { Zero we can't control player in air, one we have full control }
    FAirMovementControl: Single;
    FAirRotationControl: Single;

    FHorizontalSpeed: Single;
    FAcceleration: Single;
    FJumpSpeed: Single;
  private
    { Tries to find camera in parent children and get it direction or returns
      Parent direction }
    function GetParentCamera: TCastleCamera;
    function GetDirection: TVector3;
    function MovementControlFactor(const PlayerOnGround: Boolean): Single;
    function RotationControlFactor(const PlayerOnGround: Boolean): Single;
  protected
    procedure WorldAfterAttach; override;

    function GetDirectionFromInput: TVector3; virtual;

    function GetAcceleration: Single; virtual;

    function IsPlayerOnGround(const PlayerRigidBody: TCastleRigidBody;
      const PlayerCollider: TCastleCollider): Boolean; virtual;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
    const
      DefaultAirMovementControl = 0.20;
      DefaultAirRotationControl = 0.20;
      DefaultJumpSpeed = 7.0;
      DefaultHorizontalSpeed = 5.0;
      DefaultAcceleration = 1.0;

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

  published
    property JumpSpeed: Single read FJumpSpeed write FJumpSpeed
      {$ifdef FPC}default DefaultJumpSpeed{$endif};

    property HorizontalSpeed: Single read FHorizontalSpeed write FHorizontalSpeed
      {$ifdef FPC}default DefaultHorizontalSpeed{$endif};

    property Acceleration: Single read FAcceleration write FAcceleration
      {$ifdef FPC}default DefaultAcceleration{$endif};

    property InputForward: TInputShortcut read FInputForward;
    property InputBackward: TInputShortcut read FInputBackward;
    property InputLeftStrafe: TInputShortcut read FInputLeftStrafe;
    property InputRightStrafe: TInputShortcut read FInputRightStrafe;
    property InputJump: TInputShortcut read FInputJump;

    { Should we have control on the player movement in the air. Must be >= 0.

      @unorderedList(
        @item(0 -> no control in the air)
        @item(1 -> full control in the air, just like on the ground)
        @item(between 0 and 1 -> limited control, smoothly changes between no control and full control)
        @item(above 1 -> in the air you move even faster than on the ground)
      )
    }
    property AirMovementControl: Single read FAirMovementControl write FAirMovementControl
      {$ifdef FPC}default DefaultAirMovementControl{$endif};

    { Should we have control on the player rotation in the air.

      @unorderedList(
        @item(0 -> no control in the air)
        @item(1 -> full control in the air, just like on the ground)
        @item(between 0 and 1 -> limited control, smoothly changes between no control and full control)
        @item(above 1 -> in the air you rotate even faster than on the ground)
      )
    }
    property AirRotationControl: Single read FAirRotationControl write FAirRotationControl
      {$ifdef FPC}default DefaultAirRotationControl{$endif};

  end;


implementation

uses Math, CastleBoxes, CastleKeysMouse, CastleComponentSerialize, CastleLog,
  CastleUtils;

function TMove3DPlayerDynamic.GetParentCamera: TCastleCamera;
var
  I: Integer;
begin
  for I := 0 to Parent.Count -1 do
  begin
    if Parent.Items[I] is TCastleCamera then
      Exit(Parent.Items[I] as TCastleCamera);
  end;
end;

function TMove3DPlayerDynamic.GetDirection: TVector3;
var
  CastleCamera: TCastleCamera;
begin
  CastleCamera := GetParentCamera;
  if CastleCamera <> nil then
    Result := CastleCamera.Direction
  else
    Result := Parent.Direction;
end;

function TMove3DPlayerDynamic.MovementControlFactor(
  const PlayerOnGround: Boolean): Single;
begin
  if PlayerOnGround then
    Exit(1.0)
  else
    Result := FAirMovementControl;
end;

function TMove3DPlayerDynamic.RotationControlFactor(
  const PlayerOnGround: Boolean): Single;
begin
  if PlayerOnGround then
    Exit(1.0)
  else
    Result := FAirRotationControl;
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

function TMove3DPlayerDynamic.GetDirectionFromInput: TVector3;
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

  WritelnLog('Kierunek z inputu ' + Result.ToString);
end;

function TMove3DPlayerDynamic.GetAcceleration: Single;
begin
  Result := FAcceleration;
end;

function TMove3DPlayerDynamic.IsPlayerOnGround(
  const PlayerRigidBody: TCastleRigidBody;
  const PlayerCollider: TCastleCollider): Boolean;
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
  VelocityLength: Single;
  HVelocity: TVector3;
  VVelocity: Single;
  InputDirection: TVector3;
  JumpVelocity: Single;
  DeltaHVelocity: Single;
  HorizontalVelocity: TVector3;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if not Assigned(RBody) then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if not Assigned(Collider) then
    Exit;


  DeltaHVelocity := 0;

  IsOnGroundBool := IsPlayerOnGround(RBody, Collider);

  InputDirection := GetDirectionFromInput;

  WritelnLog('F ' + Parent.Direction.ToString);
  WritelnLog('R ' + TVector3.CrossProduct(Parent.Direction, Parent.Up).ToString);
  WritelnLog('Hmm ' + (Parent.Direction * (InputDirection.Z * HorizontalSpeed)).ToString);
  WritelnLog('Hmm2 ' + (TVector3.CrossProduct(Parent.Direction, Parent.Up) * (InputDirection.X * HorizontalSpeed)).ToString);


  { When input direction is 1.00 0.00 -1.00 this move faster }

  {HorizontalVelocity := Parent.Direction * (InputDirection.Z * HorizontalSpeed)
  + TVector3.CrossProduct(Parent.Direction, Parent.Up) * (InputDirection.X * HorizontalSpeed);}

  HorizontalVelocity := Parent.Direction * InputDirection.Z
    + TVector3.CrossProduct(Parent.Direction, Parent.Up) * InputDirection.X;

  HorizontalVelocity :=  HorizontalVelocity.Normalize * HorizontalSpeed;

  WritelnLog('Horizontal2 ' + HorizontalVelocity.ToString);

  { Jump support }

  if IsOnGroundBool then
    FWasJumpInput := false;




  RBody.LinearVelocity := HorizontalVelocity;
  Exit;

{  if GetDirectionFromInput(IsOnGroundBool, InputDirection) then
  begin
    { We get the acceleration value and that value is
      specified for 60 frames per seconds, then we must
      protect it's value to update's rate changes }
    DeltaHVelocity := GetAcceleration * 60 * SecondsPassed *
      MovementControlFactor(IsOnGroundBool);
  end;}

  JumpVelocity := 0;
  if (FocusedContainer <> nil) and (InputJump.IsPressed(FocusedContainer))
    and (not FWasJumpInput) and IsOnGroundBool then
  begin
    FWasJumpInput := true;
    JumpVelocity := JumpSpeed; // one time event so no need Seconds Passed
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
      VelocityLength := HVelocity.Length;
      VelocityLength := VelocityLength + DeltaHVelocity;
      if VelocityLength > HorizontalSpeed then
          VelocityLength := HorizontalSpeed;

      Vel := InputDirection * VelocityLength;

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

      Vel := Vel + (InputDirection * DeltaHVelocity);

      { Here we only check speed is not faster than max speed }
      HVelocity := Vel;
      HVelocity.Data[FWorldUpAxisIndex] := 0;
      VVelocity := Vel.Data[FWorldUpAxisIndex];
      VelocityLength := HVelocity.Length;
      { Check max speed }
      if VelocityLength > FHorizontalSpeed then
      begin
          VelocityLength := FHorizontalSpeed;
          Vel.Data[FWorldUpAxisIndex] := 0;
          Vel := Vel.Normalize * VelocityLength;

          { Add gravity here }
          Vel.Data[FWorldUpAxisIndex] := VVelocity;
      end;
    end;

    RBody.LinearVelocity := Vel;
  end else
  // fast stop
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
  FJumpSpeed := DefaultJumpSpeed;
  FAcceleration := DefaultAcceleration;
  FHorizontalSpeed := DefaultHorizontalSpeed;

  FAirMovementControl := DefaultAirMovementControl;
  FAirRotationControl := DefaultAirRotationControl;

  FInputForward                 := TInputShortcut.Create(Self);
  FInputBackward                := TInputShortcut.Create(Self);
  FInputLeftStrafe              := TInputShortcut.Create(Self);
  FInputRightStrafe             := TInputShortcut.Create(Self);
  FInputJump                    := TInputShortcut.Create(Self);

  InputForward                 .Assign(keyW, keyArrowUp);
  InputBackward                .Assign(keyS, keyArrowDown);
  InputLeftStrafe              .Assign(keyA);
  InputRightStrafe             .Assign(keyD);
  { For move speed we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  InputJump                    .Assign(keySpace);

  InputForward                .SetSubComponent(true);
  InputBackward               .SetSubComponent(true);
  InputLeftStrafe             .SetSubComponent(true);
  InputRightStrafe            .SetSubComponent(true);
  InputJump                   .SetSubComponent(true);

  InputForward                .Name := 'Input_Forward';
  InputBackward               .Name := 'Input_Backward';
  InputLeftStrafe             .Name := 'Input_LeftStrafe';
  InputRightStrafe            .Name := 'Input_RightStrafe';
  InputJump                   .Name := 'Input_Jump';
end;

function TMove3DPlayerDynamic.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'HorizontalSpeed', 'Acceleration', 'JumpSpeed', 'AirMovementControl',
     'AirRotationControl'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


initialization
  RegisterSerializableComponent(TMove3DPlayerDynamic, ['Physics', '3D Player Movement']);

end.

