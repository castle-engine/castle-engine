unit FpsFlySupport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ModularMovement, GameInputAxis, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs;

type

  { Fly support

    Press

  }
  TFpsFlySupport = class(TAbstractMovementModifier)
  strict private
    FInputFly: TInputShortcut;
    FFlyUpDownInputAxis: TCastleInputAxis;
    FFlyForwardInputAxis: TCastleInputAxis;

    FWasFlyInput: Boolean;
    FFlyingForwardAcceleration: Single; { m/s^2 }
    FFlyingUpDownAcceleration: Single; { m/s^2 }
    FIsFlying: Boolean;
    FFlyingDumpFactor: Single;
  public
    const
      DefaultFlyingForwardAcceleration  = 10;
      DefaultFlyingUpDownAcceleration  = 5;

    constructor Create(AOwner: TComponent); override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;

    function ShouldDoDefaultMovement(const MovementState: TModularMovementState): Boolean; override;

    property IsFlying: Boolean read FIsFlying;
  published
    property InputFly: TInputShortcut read FInputFly;
    property FlyUpDownInputAxis: TCastleInputAxis read FFlyUpDownInputAxis;
    property FlyForwardInputAxis: TCastleInputAxis read FFlyForwardInputAxis;
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TFpsFlySupport ----------------------------------------------------------------- }

procedure TFpsFlySupport.UpdateMovement(const MovementState: TModularMovementState);
var
  FlyingDamping: Single;
  RigidBody: TCastleRigidBody;
begin
  if FocusedContainer = nil then
    Exit;

  { change flying mode }
  if (not FWasFlyInput) and InputFly.IsPressed(FocusedContainer) then
  begin
    FWasFlyInput := true;

    FIsFlying := not FIsFlying;
    { There are many ways how flying can be done in that example we
      disable Gravity and option to fly up or down }
    MovementState.RigidBody.Gravity := not FIsFlying;
  end;

  if not InputFly.IsPressed(FocusedContainer) then
    FWasFlyInput := false;

  if FIsFlying then
  begin
    { Max speed? Let the programer set it himself  }
    //MovementState.RigidBody.MaxLinearVelocity := FFlyingForwardMaxSpeed;

    { Special linear velocity dump every frame to reduce old velocity and make player
      more controllable. I don't want to use LinearVelocityDump from rigid body
      to do not change user dumping settings }

    FlyingDamping := (1 - FFlyingDumpFactor * MovementState.SecondsPassed);
    if FlyingDamping < 0 then
      FlyingDamping := 0;

    RigidBody := MovementState.RigidBody;
    RigidBody.LinearVelocity := RigidBody.LinearVelocity * FlyingDamping;

    { Simple use of F = m * a }

    RigidBody.AddForce(MovementState.ForwardDirection.Normalize *
      MovementState.Collider.GetCurrentMass * FFlyingForwardAcceleration *
      -FlyForwardInputAxis.Value(FocusedContainer), false);

    RigidBody.AddForce(MovementState.UpDirection.Normalize *
      MovementState.Collider.GetCurrentMass * FFlyingUpDownAcceleration *
      FlyUpDownInputAxis.Value(FocusedContainer), false);
  end;
end;

function TFpsFlySupport.ShouldDoDefaultMovement(
  const MovementState: TModularMovementState): Boolean;
begin
  if (not FWasFlyInput) and InputFly.IsPressed(FocusedContainer) then
  begin
    { In this case we  are not flying but we will start in that frame. }
    Result := FIsFlying;
  end else
    Result := not FIsFlying;
end;

constructor TFpsFlySupport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlyingForwardAcceleration := DefaultFlyingForwardAcceleration;
  FFlyingUpDownAcceleration := DefaultFlyingUpDownAcceleration;
  FFlyingDumpFactor := 1;

  FIsFlying := false;

  FInputFly := TInputShortcut.Create(Self);
  FInputFly.SetSubComponent(true);
  InputFly.Assign(keyF);
  InputFly.Name := 'InputFly';

  FFlyForwardInputAxis := TCastleInputAxis.Create(Self);
  FFlyForwardInputAxis.SetSubComponent(true);
  FFlyForwardInputAxis.PositiveKey := keyW;
  FFlyForwardInputAxis.NegativeKey := keyS;

  FFlyUpDownInputAxis := TCastleInputAxis.Create(Self);
  FFlyUpDownInputAxis.SetSubComponent(true);
  FFlyUpDownInputAxis.PositiveKey := keyQ;
  FFlyUpDownInputAxis.NegativeKey := keyE;
end;

initialization
  RegisterSerializableComponent(TFpsFlySupport, ['Navigation', 'Modules', 'Fps Fly support']);

end.

