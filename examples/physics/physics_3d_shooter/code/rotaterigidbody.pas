unit RotateRigidBody;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors,  CastleInputs, GameInputAxis;

{ Rotate horizontal rigid body using angular velocity by mouse }

type
  TRotateRigidBody = class(TCastleBehavior)
  strict private
    RBody: TCastleRigidBody;
    SpeedScale: Single; // Ctrl support

    FAllowSlowerRotations: Boolean;

    FRotationHorizontalSpeed: Single;
    FHorizontalRotationInput: TCastleInputAxis;
    const
      DefaultMouseLookHorizontalSensitivity = 0.25;
      DefaultRotationHorizontalSpeed = 1.5;
  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;
  published
    property HorizontalRotationInput: TCastleInputAxis read FHorizontalRotationInput;

    { Rotation keys speed, in radians per second.
      @groupBegin }
    property RotationHorizontalSpeed: Single
      read FRotationHorizontalSpeed write FRotationHorizontalSpeed
      {$ifdef FPC}default DefaultRotationHorizontalSpeed{$endif};

    { If @true then all rotation keys
      (Input_RightRotate, Input_LeftRotate, Input_UpRotate, Input_DownRotate)
      will work 10x slower when Ctrl modified is pressed. }
    property AllowSlowerRotations: boolean
      read FAllowSlowerRotations write FAllowSlowerRotations
      default true;
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TRotateRigidBody }

procedure TRotateRigidBody.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  HorizontalAxisValue: Single;
  VerticalAxisValue: Single;
begin
  if CastleApplicationMode = appDesign then
    Exit;

  if FocusedContainer = nil then
    Exit;

  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RBody = nil then
    Exit;

  { Do not allow the body to rotate by default }
  RBody.AngularVelocity := Vector3(0,0,0);

  if ModifiersDown(FocusedContainer.Pressed) = [mkCtrl] then
  begin
    if AllowSlowerRotations then
      SpeedScale := 0.1 {* RotationControlFactor(IsOnGroundBool)};
  end
  else
    SpeedScale := 1.0 {* RotationControlFactor(IsOnGroundBool)};

  HorizontalAxisValue := HorizontalRotationInput.Value(FocusedContainer);

  RBody.AngularVelocity := RBody.AngularVelocity +
  Vector3(0, -HorizontalAxisValue, 0) * RotationHorizontalSpeed * SpeedScale;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TRotateRigidBody.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;

  FAllowSlowerRotations := true;

  FHorizontalRotationInput := TCastleInputAxis.Create(Self);
  FHorizontalRotationInput.SetSubComponent(true);
  FHorizontalRotationInput.PositiveKey := keyArrowRight;
  FHorizontalRotationInput.NegativeKey := keyArrowLeft;
  FHorizontalRotationInput.MouseLook := true;
  FHorizontalRotationInput.MouseLookAxis := mlaHorizontal;
  FHorizontalRotationInput.MouseLookMultiplier := DefaultMouseLookHorizontalSensitivity;
  FHorizontalRotationInput.Multiplier := DefaultRotationHorizontalSpeed;
end;

initialization
  RegisterSerializableComponent(TRotateRigidBody, ['Navigation', 'Rotate Rigid Body By Mouse And Keys']);

end.

