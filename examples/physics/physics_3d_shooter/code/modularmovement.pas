unit ModularMovement;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleVectors, GameInputAxis, CastleInputs,
    CastleClassUtils;

type
  TModularMovementState = class
    SecondsPassed: Single;

    RigidBody: TCastleRigidBody;
    Collider: TCastleCollider;

    IsPlayerOnGround: Boolean;
    { Means that player starts jumping in that update, notice that
      IsPlayerOnGround can be true here }
    IsJumping: Boolean;

    { Means that player changes horizontal position (has horizontal velocity) }
    IsMoving: Boolean;

    InputDirection: TVector3;
    ForwardDirection: TVector3; // Forward direction with Y = 0;
    FullForwardDirection: TVector3;
    RightDirection: TVector3;
    UpDirection: TVector3;
  end;

  TAbstractModularMovement = class(TCastleBehavior)

  end;

  TAbstractMovementModifier = class(TCastleBehavior)
  public
    procedure UpdateMovement(const MovementState: TModularMovementState); virtual; abstract;

    function ShouldDoDefaultMovement(const MovementState: TModularMovementState): Boolean; virtual;
  end;

  TFpsModularMovement = class(TAbstractModularMovement)
  strict private
    FWasJumpInput: Boolean;

    FForwardInputAxis: TCastleInputAxis;
    FSidewayInputAxis: TCastleInputAxis;
    FInputJump: TInputShortcut;

    FHorizontalSpeed: Single;
    FJumpSpeed: Single;
  protected
    function GetFullForwardDirection: TVector3; virtual;
    function GetForwardDirection: TVector3; virtual;

    function GetDirectionFromInput: TVector3; virtual;

    function IsPlayerOnGround(const PlayerRigidBody: TCastleRigidBody;
      const PlayerCollider: TCastleCollider): Boolean; virtual;

    procedure DefaultMovement(const PlayerRigidBody: TCastleRigidBody;
      const InputDirection, ForwardDirection, UpDirection, RightDirection: TVector3;
      var HorizontalVelocity: TVector3; var VerticalVelocity: Single); virtual;

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

    property ForwardInputAxis: TCastleInputAxis read FForwardInputAxis;
    property SidewayInputAxis: TCastleInputAxis read FSidewayInputAxis;
    property InputJump: TInputShortcut read FInputJump;
  end;



implementation

uses Math, CastleBoxes, CastleUtils, CastleComponentSerialize, CastleKeysMouse;

{ TAbstractMovementModifier -------------------------------------------------- }

function TAbstractMovementModifier.ShouldDoDefaultMovement(
  const MovementState: TModularMovementState): Boolean;
begin
  Result := true;
end;

{ TFpsModularMovement -------------------------------------------------------- }

function TFpsModularMovement.GetFullForwardDirection: TVector3;
begin
  Result := Parent.Direction;
end;

function TFpsModularMovement.GetForwardDirection: TVector3;
begin
  Result := GetFullForwardDirection;
  Result.Y := 0;
end;

function TFpsModularMovement.GetDirectionFromInput: TVector3;
begin
  Result := Vector3(0, 0, 0);

  if FocusedContainer = nil then
    Exit;

  Result := Result + Vector3(-SidewayInputAxis.Value(FocusedContainer), 0,
  -FForwardInputAxis.Value(FocusedContainer));

  if InputJump.IsPressed(FocusedContainer) then
    Result := Result + Vector3(0, 1, 0);
end;

function TFpsModularMovement.IsPlayerOnGround(
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

procedure TFpsModularMovement.DefaultMovement(
  const PlayerRigidBody: TCastleRigidBody; const InputDirection,
  ForwardDirection, UpDirection, RightDirection: TVector3;
  var HorizontalVelocity: TVector3; var VerticalVelocity: Single);
var
  IntegratedVelocities: TVector3;
begin
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
  FWasJumpInput := false;

  if (FWasJumpInput = false) and not IsZero(InputDirection.Y) then
  begin
    FWasJumpInput := true;
    VerticalVelocity := JumpSpeed;
  end else
    VerticalVelocity := PlayerRigidBody.LinearVelocity.Y;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  PlayerRigidBody.LinearVelocity := IntegratedVelocities;
end;

procedure TFpsModularMovement.Update(const SecondsPassed: Single;
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

  Beh: TCastleBehavior;

  MovementState: TModularMovementState;
  I: Integer;
  ShouldDoDefaultMovement: Boolean;
begin
  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if not Assigned(RBody) then
    Exit;

  Collider := Parent.FindBehavior(TCastleCollider) as TCastleCollider;
  if not Assigned(Collider) then
    Exit;

  IsOnGroundBool := IsPlayerOnGround(RBody, Collider);

  { Get all directions }
  InputDirection := GetDirectionFromInput;
  ForwardDirection := GetForwardDirection;
  UpDirection := Parent.Up;
  RightDirection := TVector3.CrossProduct(ForwardDirection, UpDirection);

  { HorizontalVelocity and VerticalVelocity based on previous values }
  HorizontalVelocity := RBody.LinearVelocity;
  HorizontalVelocity.Y := 0;
  VerticalVelocity := RBody.LinearVelocity.Y;

  ShouldDoDefaultMovement := true;

  { Check for modules and launch them }
  MovementState := TModularMovementState.Create;
  try
    MovementState.SecondsPassed := SecondsPassed;
    MovementState.RigidBody := RBody;
    MovementState.Collider := Collider;
    MovementState.IsPlayerOnGround := IsOnGroundBool;
    MovementState.IsJumping := FWasJumpInput;
    MovementState.IsMoving := not HorizontalVelocity.IsZero;
    MovementState.FullForwardDirection := GetFullForwardDirection;
    MovementState.ForwardDirection := ForwardDirection;
    MovementState.RightDirection := RightDirection;
    MovementState.UpDirection := UpDirection;
    MovementState.InputDirection := InputDirection;

    for I := 0 to Parent.BehaviorsCount - 1 do
    begin
      Beh := Parent.Behaviors[I];
      if Beh is TAbstractMovementModifier then
      begin
        ShouldDoDefaultMovement := TAbstractMovementModifier(Beh).
          ShouldDoDefaultMovement(MovementState);
        if not ShouldDoDefaultMovement then
          break;
      end;
    end;
  finally
    FreeAndNil(MovementState);
  end;

  { Default movement moving on ground and jumping }
  if IsOnGroundBool and ShouldDoDefaultMovement then
  begin
    DefaultMovement(RBody, InputDirection, ForwardDirection, UpDirection,
    RightDirection, HorizontalVelocity, VerticalVelocity);
  end;

  { Check for modules and launch them }
  MovementState := TModularMovementState.Create;
  try
    MovementState.SecondsPassed := SecondsPassed;
    MovementState.RigidBody := RBody;
    MovementState.Collider := Collider;
    MovementState.IsPlayerOnGround := IsOnGroundBool;
    MovementState.IsJumping := FWasJumpInput;
    MovementState.IsMoving := not HorizontalVelocity.IsZero;
    MovementState.FullForwardDirection := GetFullForwardDirection;
    MovementState.ForwardDirection := ForwardDirection;
    MovementState.RightDirection := RightDirection;
    MovementState.UpDirection := UpDirection;
    MovementState.InputDirection := InputDirection;

    for I := 0 to Parent.BehaviorsCount - 1 do
    begin
      Beh := Parent.Behaviors[I];
      if Beh is TAbstractMovementModifier then
        TAbstractMovementModifier(Beh).UpdateMovement(MovementState);
    end;
  finally
    FreeAndNil(MovementState);
  end;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TFpsModularMovement.Create(AOwner: TComponent);
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
  InputJump.Name := 'InputJump';
end;

function TFpsModularMovement.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'ForwardInputAxis', 'SidewayInputAxis',
     'InputJump', 'HorizontalSpeed', 'JumpSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TFpsModularMovement, ['Navigation', 'Modular FPS Player Movement']);

end.

