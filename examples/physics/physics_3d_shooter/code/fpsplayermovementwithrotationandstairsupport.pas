unit FpsPlayerMovementWithRotationAndStairSupport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleInputs,
  CastleVectors, CastleUIControls, CastleViewport, CastleClassUtils, GameInputAxis;

type

  { FPS (First Person Shooter) physics movement using dynamic
    rigid body with rotation and direction from player.


    - Move right/left/forward/back
    - Constant speed
    - Player rigid body rotating only in Y axis (horizontal) - should be blocked in rigid body
    - Rotation (direction.XZ) from player, camera should be player child (should rotate only in horizontal)
    - No control in air
    - Uses parent.up(), never camera up to deremine direction of the velocity vector
    - stairs support
  }

  TFpsPlayerMovementWithRotationAndStairSupport = class(TCastleBehavior)
  strict private
    FWasJumpInput: Boolean;

    FForwardInputAxis: TCastleInputAxis;
    FSidewayInputAxis: TCastleInputAxis;
    FInputJump: TInputShortcut;

    FHorizontalSpeed: Single;
    FJumpSpeed: Single;
  protected
    function GetForwardDirection: TVector3; virtual;

    function GetDirectionFromInput: TVector3; virtual;

    function IsPlayerOnGround(const PlayerRigidBody: TCastleRigidBody;
      const PlayerCollider: TCastleCollider; var GroundNormal: TVector3): Boolean; virtual;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure UpdateStairsVelocity(const SecondsPassed: Single; var RemoveMe: TRemoveType);
    procedure UpdateStairsTranslation(const SecondsPassed: Single; var RemoveMe: TRemoveType);
    procedure UpdateStairsTranslation2(const SecondsPassed: Single; var RemoveMe: TRemoveType);
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
    property InputJump: TInputShortcut read FInputJump;
  end;


implementation

uses Math, CastleBoxes, CastleKeysMouse, CastleComponentSerialize, CastleLog,
  CastleUtils;

function TFpsPlayerMovementWithRotationAndStairSupport.GetForwardDirection: TVector3;
begin
  Result := Parent.Direction;
  Result.Y := 0;
end;

function TFpsPlayerMovementWithRotationAndStairSupport.GetDirectionFromInput: TVector3;
begin
  Result := Vector3(0, 0, 0);

  if FocusedContainer = nil then
    Exit;

  Result := Result + Vector3(-SidewayInputAxis.Value(FocusedContainer), 0,
  -FForwardInputAxis.Value(FocusedContainer));

  if InputJump.IsPressed(FocusedContainer) then
    Result := Result + Vector3(0, 1, 0);
end;

function TFpsPlayerMovementWithRotationAndStairSupport.IsPlayerOnGround(
  const PlayerRigidBody: TCastleRigidBody;
  const PlayerCollider: TCastleCollider; var GroundNormal: TVector3): Boolean;
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
    GroundNormal := GroundSphereCast.Normal;
    //WritelnLog('Ground Normal ' + GroundNormal.ToString);
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

procedure TFpsPlayerMovementWithRotationAndStairSupport.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  //UpdateStairsVelocity(SecondsPassed, RemoveMe);
  //UpdateStairsTranslation(SecondsPassed, RemoveMe);
  UpdateStairsTranslation2(SecondsPassed, RemoveMe);
  inherited Update(SecondsPassed, RemoveMe);
end;

procedure TFpsPlayerMovementWithRotationAndStairSupport.UpdateStairsVelocity(
  const SecondsPassed: Single; var RemoveMe: TRemoveType);
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

  GroundNormal: TVector3;
  RayDirection: TVector3;
  RayOrigin: TVector3;
  StepHit: TPhysicsRayCastResult;
  ColliderBoundingBox: TBox3D;

  StepVelocity: TVector3;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if not Assigned(RBody) then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if not Assigned(Collider) then
    Exit;

  IsOnGroundBool := IsPlayerOnGround(RBody, Collider, GroundNormal);
  //WritelnLog('Ground Normal 2 '+ GroundNormal.ToString);

  { No support for air movement }
  if not IsOnGroundBool then
    Exit;

  { Get all directions }

  InputDirection := GetDirectionFromInput;
  ForwardDirection := GetForwardDirection;
  UpDirection := Parent.Up;

  WritelnLog('UpDirection '+ UpDirection.ToString);
  WritelnLog('ForwardDirection ' + ForwardDirection.ToString);
  RightDirection := TVector3.CrossProduct(ForwardDirection, UpDirection);
  WritelnLog('RightDirection '+ RightDirection.ToString);

  { Forward ray direction to check }
  ColliderBoundingBox:= Collider.ScaledLocalBoundingBox;
  RayDirection := TVector3.CrossProduct(RightDirection, GroundNormal);
  WritelnLog('RayDirection '+ RayDirection.ToString);

  WritelnLog('Parent Translation ' + Parent.Translation.ToString);
  RayOrigin := Parent.Translation + Vector3(0,0.1,0) - Vector3(0, ColliderBoundingBox.SizeY /2 , 0);

  WritelnLog('RayOrigin ' + RayOrigin.ToString);

  StepHit := RBody.PhysicsRayCast(RayOrigin, RayDirection, 0.51);
  if StepHit.Hit then
  begin
    WritelnLog('Found step, step normal ' + StepHit.Normal.ToString);
    StepVelocity := Vector3(0, 0.5, 0);
  end
  else
    StepVelocity := TVector3.Zero;


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

  if HorizontalVelocity.IsZero then
    StepVelocity := TVector3.Zero;

  { Jump support }

  if IsOnGroundBool then
    FWasJumpInput := false;

  if (FWasJumpInput = false) and not IsZero(InputDirection.Y) then
  begin
    FWasJumpInput := true;
    VerticalVelocity := JumpSpeed;
    StepVelocity := TVector3.Zero;
  end
  else
    VerticalVelocity := RBody.LinearVelocity.Y;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  RBody.LinearVelocity := IntegratedVelocities + StepVelocity;
end;

procedure TFpsPlayerMovementWithRotationAndStairSupport.UpdateStairsTranslation(
  const SecondsPassed: Single; var RemoveMe: TRemoveType);
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

  GroundNormal: TVector3;
  RayDirection: TVector3;
  RayOrigin: TVector3;
  StepHit: TPhysicsRayCastResult;
  ColliderBoundingBox: TBox3D;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if not Assigned(RBody) then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if not Assigned(Collider) then
    Exit;

  IsOnGroundBool := IsPlayerOnGround(RBody, Collider, GroundNormal);
  //WritelnLog('Ground Normal 2 '+ GroundNormal.ToString);

  { No support for air movement }
  if not IsOnGroundBool then
    Exit;

  { Get all directions }

  InputDirection := GetDirectionFromInput;
  ForwardDirection := GetForwardDirection;
  UpDirection := Parent.Up;

  //WritelnLog('UpDirection '+ UpDirection.ToString);
  //WritelnLog('ForwardDirection ' + ForwardDirection.ToString);
  RightDirection := TVector3.CrossProduct(ForwardDirection, UpDirection);
  //WritelnLog('RightDirection '+ RightDirection.ToString);

  { Forward ray direction to check }
  ColliderBoundingBox:= Collider.ScaledLocalBoundingBox;
  RayDirection := TVector3.CrossProduct(RightDirection, GroundNormal);
  WritelnLog('RayDirection '+ RayDirection.ToString);

  //WritelnLog('Parent Translation ' + Parent.Translation.ToString);
  RayOrigin := Parent.Translation + Vector3(0,0.1,0) - Vector3(0, ColliderBoundingBox.SizeY /2 , 0);

  WritelnLog('RayOrigin ' + RayOrigin.ToString);

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

  { Direct translate player up when he is near a stairs step }
  if (FWasJumpInput = false) and (HorizontalVelocity.IsZero = false) then
  begin
    StepHit := RBody.PhysicsRayCast(RayOrigin, RayDirection, 0.51);
    if StepHit.Hit then
    begin
      WritelnLog('Found step, step normal ' + StepHit.Normal.ToString);
      Parent.Translation := Parent.Translation + Vector3(0, 0.3, 0); // maybe step height here
    end;
  end;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  RBody.LinearVelocity := IntegratedVelocities;
end;

procedure TFpsPlayerMovementWithRotationAndStairSupport.UpdateStairsTranslation2
  (const SecondsPassed: Single; var RemoveMe: TRemoveType);
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

  GroundNormal: TVector3;
  RayDirection: TVector3;
  RayOrigin: TVector3;
  StepHit: TPhysicsRayCastResult;
  ColliderBoundingBox: TBox3D;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if not Assigned(RBody) then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if not Assigned(Collider) then
    Exit;

  IsOnGroundBool := IsPlayerOnGround(RBody, Collider, GroundNormal);
  //WritelnLog('Ground Normal 2 '+ GroundNormal.ToString);

  { No support for air movement }
  if not IsOnGroundBool then
    Exit;

  { Get all directions }

  InputDirection := GetDirectionFromInput;
  ForwardDirection := GetForwardDirection;
  UpDirection := Parent.Up;

  //WritelnLog('UpDirection '+ UpDirection.ToString);
  //WritelnLog('ForwardDirection ' + ForwardDirection.ToString);
  RightDirection := TVector3.CrossProduct(ForwardDirection, UpDirection);
  //WritelnLog('RightDirection '+ RightDirection.ToString);

  { Forward ray direction to we are on step }
  ColliderBoundingBox:= Collider.ScaledLocalBoundingBox;
  RayDirection := Vector3(0, -1, 0);
  WritelnLog('RayDirection '+ RayDirection.ToString);

  WritelnLog('Parent Translation ' + Parent.Translation.ToString);
  RayOrigin := Parent.Translation - Vector3(0, 0, ColliderBoundingBox.SizeZ/2);


  WritelnLog('RayOrigin ' + RayOrigin.ToString);

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

  { Direct translate player up when he is near a stairs step }
  if (FWasJumpInput = false) and (HorizontalVelocity.IsZero = false) then
  begin
    StepHit := RBody.PhysicsRayCast(RayOrigin, RayDirection, 0.90);
    if StepHit.Hit then
    begin
      WritelnLog('Found step, step normal ' + StepHit.Normal.ToString);
      Parent.Translation := Parent.Translation + Vector3(0, 0.3, 0); // maybe step height here
    end;
  end;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  RBody.LinearVelocity := IntegratedVelocities;
end;

constructor TFpsPlayerMovementWithRotationAndStairSupport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWasJumpInput := false;
  FJumpSpeed := DefaultJumpSpeed;
  FHorizontalSpeed := DefaultHorizontalSpeed;

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
  InputJump.Name := 'Input_Jump';
end;

function TFpsPlayerMovementWithRotationAndStairSupport.PropertySections(const PropertyName: String
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
  RegisterSerializableComponent(TFpsPlayerMovementWithRotationAndStairSupport, ['Physics', 'FPS Player Movement With Player Rotation and Stairs Support']);

end.

