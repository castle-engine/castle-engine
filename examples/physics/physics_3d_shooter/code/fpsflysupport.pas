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
    FFlyingForwardMaxSpeed: Single; { m/s }
    FFlyingUpDownMaxSpeed: Single; { m/s }
  public
    const
      DefaultFlyingForwardAcceleration  = 5;
      DefaultFlyingUpDownAcceleration  = 2;

      DefaultFlyingForwardMaxSpeed = 15.0;
      DefaultFlyingUpDownMaxSpeed = 10.0;

    constructor Create(AOwner: TComponent); override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;

    function ShouldDoDefaultMovement(const MovementState: TModularMovementState): Boolean; override;

    property IsFlying: Boolean read FIsFlying;
  published
    property InputFly: TInputShortcut read FInputFly;
    property FlyUpDownInputAxis: TCastleInputAxis read FFlyUpDownInputAxis;
    property FlyForwardInputAxis: TCastleInputAxis read FFlyForwardInputAxis;

    { Max horizontal flying speed }
    property FlyingForwardMaxSpeed: Single read FFlyingForwardMaxSpeed write FFlyingForwardMaxSpeed
      {$ifdef FPC}default DefaultFlyingForwardMaxSpeed{$endif};

    property FlyingUpDownMaxSpeed: Single read FFlyingUpDownMaxSpeed write FFlyingUpDownMaxSpeed;
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TFpsFlySupport ----------------------------------------------------------------- }

procedure TFpsFlySupport.UpdateMovement(const MovementState: TModularMovementState);
var
  HorizontalVelocity: TVector3;
  HorizontalVelocityChange: TVector3;
  VerticalVelocity: Single;

  HorizontalVelocityDelta: Single;
  UpDownVelocityDelta: Single;

  NewHorizontalVelocity: Single;
  NewVerticalVelocity: Single;

  IntegratedVelocity: TVector3;

  Direction: TVector3;
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
  end
  else
    FWasFlyInput := false;

  if FIsFlying then
  begin
    { How the velocity will change based on inputs based on deltav = a*t }

    HorizontalVelocityDelta := {-FlyForwardInputAxis.Value(FocusedContainer) *}
      FFlyingForwardAcceleration * MovementState.SecondsPassed;
    UpDownVelocityDelta := FlyUpDownInputAxis.Value(FocusedContainer) *
      FFlyingUpDownAcceleration * MovementState.SecondsPassed;

    { get current horizontal and vertical velocity }
    HorizontalVelocity := MovementState.RigidBody.LinearVelocity;
    HorizontalVelocity.Y := 0;

    VerticalVelocity := MovementState.RigidBody.LinearVelocity.Y;

    HorizontalVelocity := -MovementState.ForwardDirection.Normalize * HorizontalVelocity.Length;
    HorizontalVelocityChange := -MovementState.ForwardDirection.Normalize * HorizontalVelocityDelta;
    WritelnLog('HorizontalVelocity' + HorizontalVelocity.ToString);
    WritelnLog('HorizontalVelocityChange' + HorizontalVelocityChange.ToString);

    NewVerticalVelocity := Min(VerticalVelocity + UpDownVelocityDelta, FFlyingForwardMaxSpeed);

    IntegratedVelocity := HorizontalVelocity +  HorizontalVelocityChange + Vector3(0, NewVerticalVelocity, 0);

    MovementState.RigidBody.LinearVelocity := IntegratedVelocity;
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
  FFlyingForwardMaxSpeed := DefaultFlyingForwardMaxSpeed;
  FFlyingUpDownMaxSpeed := DefaultFlyingUpDownMaxSpeed;

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

  FIsFlying := false;
end;

initialization
  RegisterSerializableComponent(TFpsFlySupport, ['Navigation', 'Modules', 'Fps Fly support']);

end.

