unit ModularMovement;

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
  strict private
    FExists: Boolean;
  protected
    procedure SetExists(const AValue: Boolean); virtual;
  public
    procedure UpdateMovement(const MovementState: TModularMovementState); virtual; abstract;

    constructor Create(AOwner: TComponent); override;
  published
    property Exists: Boolean read FExists write SetExists default true;
  end;

  TFpsModularMovement = class(TAbstractModularMovement)
  strict private
    FForwardInputAxis: TCastleInputAxis;
    FSidewayInputAxis: TCastleInputAxis;
    FInputJump: TInputShortcut;
  protected
    function GetFullForwardDirection: TVector3; virtual;
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
    property ForwardInputAxis: TCastleInputAxis read FForwardInputAxis;
    property SidewayInputAxis: TCastleInputAxis read FSidewayInputAxis;
    property InputJump: TInputShortcut read FInputJump;
  end;



implementation

uses Math, CastleBoxes, CastleUtils, CastleComponentSerialize, CastleKeysMouse,
  CastleLog;

{ TAbstractMovementModifier }

procedure TAbstractMovementModifier.SetExists(const AValue: Boolean);
begin
  FExists := AValue;
end;

constructor TAbstractMovementModifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExists := true;
end;

{ TAbstractMovementModifier -------------------------------------------------- }

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
  SphereCastOrigin: TVector3;
  { Needed when casted sphere is bigger or equal than ColliderHeight / 2 because kraft
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
  { From testing average size is the best here, better than min or max size. }
  ColliderRadius := (ColliderBoundingBox.SizeX + ColliderBoundingBox.SizeZ) / 2;
  SphereCastOrigin := PlayerCollider.Middle;

  { When casting sphere is equal or bigger than ColliderHeight / 2 we need
    move it up or reduce sphere size }
  {if not (ColliderRadius < ColliderHeight / 2 * 0.9) then
     ColliderRadius := ColliderHeight / 2 * 0.9;}

  { Adjust sphere cast origin when radius is equal or bigger than ColliderHeight / 2 }
  if ColliderRadius - ColliderHeight / 2 > -0.1  then
  begin
    SphereCastOriginUpAdjustment := ColliderRadius - ColliderHeight / 2 + 0.1;
    SphereCastOrigin.Y := SphereCastOrigin.Y + SphereCastOriginUpAdjustment;
  end;

  GroundSphereCast := PlayerRigidBody.PhysicsSphereCast(
    SphereCastOrigin,
    ColliderRadius,
    Vector3(0, -1, 0),
    ColliderHeight * 3
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
    Result := DistanceToGround < ColliderHeight * 0.1;
    if Result then
      WritelnLog('on ground (distance ' + FloatToStr(DistanceToGround) + ')')
    else
      WritelnLog('not on ground (distance ' + FloatToStr(DistanceToGround) + ')');
  end else
  begin
    Result := false;
    WritelnLog('not on ground');
  end;
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

  Beh: TCastleBehavior;

  HorizontalVelocity: TVector3;

  MovementState: TModularMovementState;
  I: Integer;
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

  { Check for modules and launch them }
  MovementState := TModularMovementState.Create;
  try
    MovementState.SecondsPassed := SecondsPassed;
    MovementState.RigidBody := RBody;
    MovementState.Collider := Collider;
    MovementState.IsPlayerOnGround := IsOnGroundBool;
    MovementState.IsJumping := false;
    MovementState.IsMoving := not HorizontalVelocity.IsZero;
    MovementState.FullForwardDirection := GetFullForwardDirection;
    MovementState.ForwardDirection := ForwardDirection;
    MovementState.RightDirection := RightDirection;
    MovementState.UpDirection := UpDirection;
    MovementState.InputDirection := InputDirection;

    for I := 0 to Parent.BehaviorsCount - 1 do
    begin
      Beh := Parent.Behaviors[I];
      if (Beh is TAbstractMovementModifier) and
         (TAbstractMovementModifier(Beh).Exists) then
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

