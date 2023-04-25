unit Simplest3DPlayerMovement;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleInputs,
  CastleVectors, CastleUIControls, CastleViewport, CastleClassUtils;

type

  { The simplest 3d physics movement using dynamic rigid body.
    Designed for first person games.


    - Only move right/left/forward/back
    - Constant speed
    - Player not rotating (X, Y, Z) should be blocked in rigid body
    - Rotation (direction.XZ) from camera what is player child (no rotation when no camera in player)
    - No air control
    - Uses parent.up() (never camera up -  t can be changed by rotation)
  }

  TSimplest3DPlayerMovement = class(TCastleBehavior)
  strict private
    FWasJumpInput: Boolean;

    FInputForward: TInputShortcut;
    FInputBackward: TInputShortcut;
    FInputRightStrafe: TInputShortcut;
    FInputLeftStrafe: TInputShortcut;
    FInputJump: TInputShortcut;

    FHorizontalSpeed: Single;
    FJumpSpeed: Single;
  protected
    { Tries to find camera in parent children and get it direction or returns
      Parent direction }
    function GetParentCamera: TCastleCamera; virtual;
    function GetForwardDirection: TVector3; virtual;

    function GetDirectionFromInput: TVector3; virtual;

    function IsPlayerOnGround(const PlayerRigidBody: TCastleRigidBody;
      const PlayerCollider: TCastleCollider): Boolean; virtual;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
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
  end;


implementation

uses Math, CastleBoxes, CastleKeysMouse, CastleComponentSerialize, CastleLog,
  CastleUtils;

function TSimplest3DPlayerMovement.GetParentCamera: TCastleCamera;
var
  I: Integer;
begin
  for I := 0 to Parent.Count -1 do
  begin
    if Parent.Items[I] is TCastleCamera then
      Exit(Parent.Items[I] as TCastleCamera);
  end;
end;

function TSimplest3DPlayerMovement.GetForwardDirection: TVector3;
var
  CastleCamera: TCastleCamera;
begin
  CastleCamera := GetParentCamera;
  if CastleCamera <> nil then
  begin
    Result := -CastleCamera.Direction;
    { We don't want vertical camera rotation when moving. Only horizontal. }
    Result.Y := 0;
  end
  else
    Result := Parent.Direction;
end;

function TSimplest3DPlayerMovement.GetDirectionFromInput: TVector3;
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

function TSimplest3DPlayerMovement.IsPlayerOnGround(
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
  ColliderHeight :=  ColliderBoundingBox.SizeY;
  RayOrigin := Parent.Translation + PlayerCollider.Translation;

  GroundRayCast := PlayerRigidBody.PhysicsRayCast(
    RayOrigin,
    Vector3(0, -1, 0),
    ColliderHeight * 3
  );

  { Four more checks - player should slide down when player just
    on the edge, but sometimes it stay and center ray don't "see" that we are
    on ground }
  if not GroundRayCast.Hit then
    GroundRayCast := PlayerRigidBody.PhysicsRayCast(
      RayOrigin + Vector3(ColliderBoundingBox.SizeX * 0.49, 0, 0),
      Vector3(0, -1, 0),
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := PlayerRigidBody.PhysicsRayCast(
      RayOrigin + Vector3(-ColliderBoundingBox.SizeX * 0.49, 0, 0),
      Vector3(0, -1, 0),
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := PlayerRigidBody.PhysicsRayCast(
      RayOrigin + Vector3(0, 0, ColliderBoundingBox.SizeZ * 0.49),
      Vector3(0, -1, 0),
      ColliderHeight * 3
    );

  if not GroundRayCast.Hit then
    GroundRayCast := PlayerRigidBody.PhysicsRayCast(
      RayOrigin + Vector3(0, 0, -ColliderBoundingBox.SizeZ * 0.49),
      Vector3(0, -1, 0),
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

procedure TSimplest3DPlayerMovement.Update(const SecondsPassed: Single;
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

  IsOnGroundBool := IsPlayerOnGround(RBody, Collider);

  { No support for air movement }
  if not IsOnGroundBool then
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
  end
  else
    VerticalVelocity := RBody.LinearVelocity.Y;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  RBody.LinearVelocity := IntegratedVelocities;
  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TSimplest3DPlayerMovement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWasJumpInput := false;
  FJumpSpeed := DefaultJumpSpeed;
  FHorizontalSpeed := DefaultHorizontalSpeed;

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

function TSimplest3DPlayerMovement.PropertySections(const PropertyName: String
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
  RegisterSerializableComponent(TSimplest3DPlayerMovement, ['Physics', 'Simplest 3D Player Movement']);

end.

