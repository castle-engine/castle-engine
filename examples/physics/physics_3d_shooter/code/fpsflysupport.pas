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
    FFlyUpDownInputAxis: TCastleInputAxis;
    FFlyForwardInputAxis: TCastleInputAxis;

    FFlyingForwardAcceleration: Single; { m/s^2 }
    FFlyingUpDownAcceleration: Single; { m/s^2 }
    FFlyingDumpFactor: Single;
  public
    const
      DefaultFlyingForwardAcceleration  = 10;
      DefaultFlyingUpDownAcceleration  = 5;

    constructor Create(AOwner: TComponent); override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;
  published
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

  MovementState.RigidBody.Gravity := false;

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
    MovementState.Collider.GetEffectiveMass * FFlyingForwardAcceleration *
    -FlyForwardInputAxis.Value(FocusedContainer), false);

  RigidBody.AddForce(MovementState.UpDirection.Normalize *
    MovementState.Collider.GetEffectiveMass * FFlyingUpDownAcceleration *
    FlyUpDownInputAxis.Value(FocusedContainer), false);
end;

constructor TFpsFlySupport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlyingForwardAcceleration := DefaultFlyingForwardAcceleration;
  FFlyingUpDownAcceleration := DefaultFlyingUpDownAcceleration;
  FFlyingDumpFactor := 0.85;

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

