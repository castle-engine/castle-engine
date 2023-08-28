unit DoubleJumpSupport;

interface

uses
  Classes, SysUtils, ModularMovement, CastleInputAxis, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs, CastleClassUtils;

type

  { Double jump support for TFpsModularMovement. }
  TDoubleJumpSupport = class(TAbstractMovementModifier)
  strict private
    FWasJumpInput: Boolean;
    FWasSecondJump: Boolean;

    FJumpSpeed: Single;
  public
    const
      DefaultJumpSpeed = 7.0;
      DefaultHorizontalSpeed = 5.0;

    constructor Create(AOwner: TComponent); override;

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;
  published
  property JumpSpeed: Single read FJumpSpeed write FJumpSpeed
    {$ifdef FPC}default DefaultJumpSpeed{$endif};
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TDoubleJumpSupport --------------------------------------------------------- }

procedure TDoubleJumpSupport.UpdateMovement(const MovementState: TModularMovementState);
var
  IntegratedVelocities: TVector3;
  InputDirection: TVector3;

  HorizontalVelocity: TVector3;
  VerticalVelocity: Single;

  PlayerRigidBody: TCastleRigidBody;
begin
  { Do not make double jump in that same frame that jump is made }
  if MovementState.IsJumping then
  begin
    { MovementState.IsJumping true means that in this update we made first
      jump so jump input is now pressed and should be ignored }
    FWasJumpInput := true;
    Exit;
  end;

  { Works only when gravity is on }
  if MovementState.RigidBody.Gravity = false then
  begin
    FWasSecondJump := false;
    Exit;
  end;

  { Works only when player is in air }
  if MovementState.IsPlayerOnGround then
  begin
    FWasSecondJump := false;
    Exit;
  end;

  PlayerRigidBody := MovementState.RigidBody;
  InputDirection := MovementState.InputDirection;

  HorizontalVelocity := PlayerRigidBody.LinearVelocity;
  HorizontalVelocity.Y := 0;

  { Jump support }
  if IsZero(InputDirection.Y) then
    FWasJumpInput := false;

  if (FWasJumpInput = false) and (not IsZero(InputDirection.Y)) and
    (not FWasSecondJump) then
  begin
    FWasJumpInput := true;
    FWasSecondJump := true;
    { In second jump just add diffrence between current Velocity and JumpVelocity }
    VerticalVelocity := PlayerRigidBody.LinearVelocity.Y + (JumpSpeed - PlayerRigidBody.LinearVelocity.Y);
    MovementState.IsJumping := true;
  end else
    VerticalVelocity := PlayerRigidBody.LinearVelocity.Y;

  MovementState.IsMoving := not HorizontalVelocity.IsZero;

  { Integrate velocities }
  IntegratedVelocities := HorizontalVelocity;
  IntegratedVelocities.Y := VerticalVelocity;

  {Set velocity to rigid body }
  PlayerRigidBody.LinearVelocity := IntegratedVelocities;
end;

constructor TDoubleJumpSupport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWasJumpInput := false;
  FJumpSpeed := DefaultJumpSpeed;
end;

function TDoubleJumpSupport.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'JumpSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TDoubleJumpSupport, ['Navigation', 'Modules', 'Double Jump Support']);

end.

