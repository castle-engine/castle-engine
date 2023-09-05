unit Platformer2DWalkSupport;

interface

uses
  Classes, SysUtils, ModularMovement, CastleInputAxis, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs, CastleClassUtils;

type

  { 2D platformer walk support for TModularMovement. }
  TPlatformer2DWalkSupport = class(TAbstractMovementModule)
  strict private
    FWasJumpInput: Boolean;

    FHorizontalSpeed: Single;
    FJumpSpeed: Single;

    { We need a flag to set when we executed event at first frame when player fall. }
    FWasFallEvent: Boolean;
    { We need a flag to set when we executed event at first frame when player move. }
    FWasMoveEventRight: Boolean;
    FWasMoveEventLeft: Boolean;
    { We need a flag to set when we executed event at first frame when player stops move. }
    FWasIdleEvent: Boolean;

    { Events that gives the game programmer the ability to react to a change caused by
      modular navigation (e.g. change of direction, jump) in his own code
      without need to always implement it as another TAbstractMovementModule }
    FJumpEventListener: TModularMovementEventList;
    FFallEventListener: TModularMovementEventList;
    FMoveEventListener: TModularMovementEventList;
    FIdleEventListener: TModularMovementEventList;

    procedure CallFallEvent(const MovementState: TModularMovementState);
    procedure CallMoveIdleEvent(const MovementState: TModularMovementState);
  public
    const
      DefaultJumpSpeed = 680;
      DefaultHorizontalSpeed = 345;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;

    procedure AddJumpListener(const EventCallback: TModularMovementEvent);
    procedure RemoveJumpListener(const EventCallback: TModularMovementEvent);
    procedure AddFallListener(const EventCallback: TModularMovementEvent);
    procedure RemoveFallListener(const EventCallback: TModularMovementEvent);
    procedure AddMoveListener(const EventCallback: TModularMovementEvent);
    procedure RemoveMoveListener(const EventCallback: TModularMovementEvent);
    procedure AddIdleListener(const EventCallback: TModularMovementEvent);
    procedure RemoveIdleListener(const EventCallback: TModularMovementEvent);
  published
    property JumpSpeed: Single read FJumpSpeed write FJumpSpeed
      {$ifdef FPC}default DefaultJumpSpeed{$endif};

    property HorizontalSpeed: Single read FHorizontalSpeed write FHorizontalSpeed
      {$ifdef FPC}default DefaultHorizontalSpeed{$endif};
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TPlatformer2DWalkSupport ------------------------------------------------------------ }

procedure TPlatformer2DWalkSupport.UpdateMovement(const MovementState: TModularMovementState);
var
  IntegratedVelocities: TVector3;
  InputDirection: TVector3;

  HorizontalVelocity: TVector3;
  VerticalVelocity: Single;

  PlayerRigidBody: TCastleRigidBody;
begin
  { Can be false by fly support }
  MovementState.RigidBody.Gravity := true;

  CallFallEvent(MovementState);
  if not MovementState.IsPlayerOnGround then
    Exit;

  PlayerRigidBody := MovementState.RigidBody;
  InputDirection := MovementState.InputDirection;

  HorizontalVelocity := Vector3(1, 0, 0) * InputDirection.X;

  { Normalize and set velocity to handle faster movement when InputDirection is
    eg (1, 0, 1). }

  HorizontalVelocity :=  HorizontalVelocity.Normalize * HorizontalSpeed;

  { Jump support }
  if IsZero(InputDirection.Y) then
    FWasJumpInput := false;

  if (FWasJumpInput = false) and not IsZero(InputDirection.Y) then
  begin
    FWasJumpInput := true;
    VerticalVelocity := JumpSpeed;
    MovementState.IsJumping := true;
    FJumpEventListener.ExecuteAll(Self, MovementState);
  end else
    VerticalVelocity := PlayerRigidBody.LinearVelocity.Y;

  MovementState.IsMoving := not HorizontalVelocity.IsZero;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  PlayerRigidBody.LinearVelocity := IntegratedVelocities;
  CallMoveIdleEvent(MovementState);
end;

procedure TPlatformer2DWalkSupport.CallFallEvent(
  const MovementState: TModularMovementState);
begin
  if MovementState.IsPlayerOnGround or (not FFallEventListener.Used) then
  begin
    FWasFallEvent := false;
    Exit;
  end;

  { Call fall event when needed }
  if (MovementState.RigidBody.LinearVelocity.Y > -5) then
  begin
    FWasFallEvent := false;
    Exit;
  end;

  { Call fall event only once per fall }
  if not FWasFallEvent then
  begin
    FFallEventListener.ExecuteAll(Self, MovementState);
    FWasFallEvent := true;
  end;
end;

procedure TPlatformer2DWalkSupport.CallMoveIdleEvent(
  const MovementState: TModularMovementState);
var
  HorzontalVelocity: Single;
begin
  { Is on ground functions react a little faster and when we start jumping
    on next frame we can be near ground but still going up so we check our
    vertical speed also }
  if MovementState.IsJumping or
    (MovementState.RigidBody.LinearVelocity.Y > JumpSpeed / 2) then
  begin
    FWasMoveEventLeft := false;
    FWasMoveEventRight := false;
    FWasIdleEvent := false;
    Exit;
  end;

  HorzontalVelocity := MovementState.RigidBody.LinearVelocity.X;
  if (abs(HorzontalVelocity) > 3) then
  begin
    FWasIdleEvent := false;
    if HorzontalVelocity > 3 then
    begin
      if not FWasMoveEventRight then
      begin
        FMoveEventListener.ExecuteAll(Self, MovementState);
        FWasMoveEventRight := true;
      end;
      FWasMoveEventLeft := false;
    end else
    begin
      if not FWasMoveEventLeft then
      begin
        FMoveEventListener.ExecuteAll(Self, MovementState);
        FWasMoveEventLeft := true;
      end;
      FWasMoveEventRight := false;
    end;
  end else
  begin
    if not FWasIdleEvent then
    begin
      FIdleEventListener.ExecuteAll(Self, MovementState);
      FWasIdleEvent := true;
    end;
    FWasMoveEventLeft := false;
    FWasMoveEventRight := false;
  end;
end;

constructor TPlatformer2DWalkSupport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWasJumpInput := false;
  FJumpSpeed := DefaultJumpSpeed;
  FHorizontalSpeed := DefaultHorizontalSpeed;

  FJumpEventListener := TModularMovementEventList.Create;
  FFallEventListener := TModularMovementEventList.Create;
  FMoveEventListener := TModularMovementEventList.Create;
  FIdleEventListener := TModularMovementEventList.Create;
end;

destructor TPlatformer2DWalkSupport.Destroy;
begin
  FreeAndNil(FJumpEventListener);
  FreeAndNil(FFallEventListener);
  FreeAndNil(FMoveEventListener);
  FreeAndNil(FIdleEventListener);
  inherited Destroy;
end;

function TPlatformer2DWalkSupport.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'JumpSpeed', 'HorizontalSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TPlatformer2DWalkSupport.AddJumpListener(
  const EventCallback: TModularMovementEvent);
begin
  FJumpEventListener.Add(EventCallback);
end;

procedure TPlatformer2DWalkSupport.RemoveJumpListener(
  const EventCallback: TModularMovementEvent);
begin
  FJumpEventListener.Remove(EventCallback);
end;

procedure TPlatformer2DWalkSupport.AddFallListener(
  const EventCallback: TModularMovementEvent);
begin
  FFallEventListener.Add(EventCallback);
end;

procedure TPlatformer2DWalkSupport.RemoveFallListener(
  const EventCallback: TModularMovementEvent);
begin
  FFallEventListener.Remove(EventCallback);
end;

procedure TPlatformer2DWalkSupport.AddMoveListener(
  const EventCallback: TModularMovementEvent);
begin
  FMoveEventListener.Add(EventCallback);
end;

procedure TPlatformer2DWalkSupport.RemoveMoveListener(
  const EventCallback: TModularMovementEvent);
begin
  FMoveEventListener.Remove(EventCallback);
end;

procedure TPlatformer2DWalkSupport.AddIdleListener(
  const EventCallback: TModularMovementEvent);
begin
  FIdleEventListener.Add(EventCallback);
end;

procedure TPlatformer2DWalkSupport.RemoveIdleListener(
  const EventCallback: TModularMovementEvent);
begin
  FIdleEventListener.Remove(EventCallback);
end;

initialization
  RegisterSerializableComponent(TPlatformer2DWalkSupport, ['Navigation', 'Modules', 'Platformer 2D Walk Support']);

end.

