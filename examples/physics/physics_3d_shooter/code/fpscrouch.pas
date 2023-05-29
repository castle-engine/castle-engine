unit FpsCrouch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ModularMovement, CastleTransform, CastleBehaviors,
  CastleVectors, CastleInputs;

type

  { Crouch support by scale collider and change player translation }
  TFpsCrouch = class(TAbstractMovementModifier)
  strict private
    FInput_Crouch: TInputShortcut;
    FCrouchSpeed: Single;
    FIsCrouching: Boolean;
  private
    const
      DefaultCrouchSpeed = 3.0;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateMovement(const MovementState: TModularMovementState); override;

    property IsCrouching: Boolean read FIsCrouching;
  published
    property Input_Crouch: TInputShortcut read FInput_Crouch;

    property CrouchSpeed: Single
      read FCrouchSpeed write FCrouchSpeed
      {$ifdef FPC}default DefaultCrouchSpeed{$endif};
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse;

{ TRotateRigidBodyByKeys }

procedure TFpsCrouch.UpdateMovement(const MovementState: TModularMovementState);
var
  Velocity: TVector3;
  HorizontalSpeed: TVector3;
  VerticalSpeed: Single;
begin
  if FocusedContainer = nil then
    Exit;

  if (not FIsCrouching) and (Input_Crouch.IsPressed(FocusedContainer)) then
  begin
    { Start crouching }
    MovementState.Collider.SizeScale := 0.5;
    Parent.Translation := Parent.Translation + Vector3(0, -(MovementState.Collider.ScaledLocalBoundingBox.SizeY / 2 * 0.90), 0); // place player on ground, 0.99 to ensure player will be above ground
    FIsCrouching := true;
  end else
  if FIsCrouching and (not Input_Crouch.IsPressed(FocusedContainer)) then
  begin
    // TODO: maybe here check we can do it - cant be done in tight places

    Parent.Translation := Parent.Translation + Vector3(0, MovementState.Collider.ScaledLocalBoundingBox.SizeY * 1.01, 0); // place player on ground before scale change 1.01 to ensure player will be above ground
    MovementState.Collider.SizeScale := 1;
    FIsCrouching := false;
  end;

  if FIsCrouching then
  begin
    Velocity := MovementState.RigidBody.LinearVelocity;
    if not Velocity.IsZero then
    begin
      VerticalSpeed := Velocity.Y;
      HorizontalSpeed := Velocity;
      HorizontalSpeed.Y := 0;

      HorizontalSpeed := HorizontalSpeed.Normalize * CrouchSpeed;
      Velocity := HorizontalSpeed;
      Velocity.Y := VerticalSpeed;

      MovementState.RigidBody.LinearVelocity := Velocity;
    end;
  end;
end;

constructor TFpsCrouch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCrouchSpeed := DefaultCrouchSpeed;

  FInput_Crouch              := TInputShortcut.Create(Self);
  Input_Crouch              .Assign(keyC);
end;

initialization
  RegisterSerializableComponent(TFpsCrouch, ['Physics', 'Fps Crouch support']);

end.

