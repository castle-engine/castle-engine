{
  Copyright 2023-2024 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TODO docs. }
unit Fly3DSupport;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, ModularMovement, CastleInputAxis, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs, CastleClassUtils;

type
  { Fly support for TModularMovement.

    To start flying add this behavior. Fly start/stop should be controled from
    your game by change Exists property e.g.:

    @longCode(#
    if Assigned(FlySupport) and Assigned(WalkSupport) and Assigned(RotateRigidBody) then
    begin
      if Event.IsKey(keyF) then
      begin
        if FlySupport.Exists then
        begin
          FlySupport.Exists := false;
          WalkSupport.Exists := true;
          LabelFlyWalk.Caption := 'Walking';
          RotateRigidBody.HorizontalRotationInput.PositiveKey := keyArrowRight;
          RotateRigidBody.HorizontalRotationInput.NegativeKey := keyArrowLeft;
          RotateRigidBody.RotationHorizontalSpeed := 1.5;
        end else
        begin
          FlySupport.Exists := true;
          WalkSupport.Exists := false;
          LabelFlyWalk.Caption := 'Flying';
          RotateRigidBody.HorizontalRotationInput.PositiveKey := keyD;
          RotateRigidBody.HorizontalRotationInput.NegativeKey := keyA;
          RotateRigidBody.RotationHorizontalSpeed := 0.4;
        end;
      end;
    end;
    #)

    When TFly3DSupport.Exists = true TWalk3DSupport.Exists should be false. }
  TFly3DSupport = class(TAbstractMovementModule)
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

    function PropertySections(const PropertyName: String): TPropertySections; override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;
  published
    { Move up/down input axis. }
    property FlyUpDownInputAxis: TCastleInputAxis read FFlyUpDownInputAxis;
    { Move forward/backward input axis }
    property FlyForwardInputAxis: TCastleInputAxis read FFlyForwardInputAxis;
  end;

implementation

uses Math, CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TFly3DSupport -------------------------------------------------------------- }

procedure TFly3DSupport.UpdateMovement(const MovementState: TModularMovementState);
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
    MovementState.Collider.EffectiveMass * FFlyingForwardAcceleration *
    FlyForwardInputAxis.Value(FocusedContainer), false);

  RigidBody.AddForce(MovementState.UpDirection.Normalize *
    MovementState.Collider.EffectiveMass * FFlyingUpDownAcceleration *
    FlyUpDownInputAxis.Value(FocusedContainer), false);
end;

constructor TFly3DSupport.Create(AOwner: TComponent);
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

function TFly3DSupport.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
     'FlyUpDownInputAxis', 'FlyForwardInputAxis'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TFly3DSupport, ['Navigation', 'Modules', 'Fps Fly support']);

end.

