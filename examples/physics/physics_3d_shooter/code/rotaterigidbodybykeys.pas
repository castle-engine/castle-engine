unit RotateRigidBodyByKeys;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors, CastleInputs;

{ Rotates rigid body by changing AngularVelocity }

type
  TRotateRigidBodyByKeys = class(TCastleBehavior)
  strict private
    FRotationHorizontalSpeed: Single;
    FAllowSlowerRotations: Boolean;

    FInput_RightRotate: TInputShortcut;
    FInput_LeftRotate: TInputShortcut;
  private
    const
      DefaultRotationHorizontalSpeed = Pi * 150 / 180;

  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;
  published
    property Input_LeftRotate: TInputShortcut read FInput_LeftRotate;
    property Input_RightRotate: TInputShortcut read FInput_RightRotate;

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

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse;

{ TRotateRigidBodyByKeys }

procedure TRotateRigidBodyByKeys.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
var
  SpeedScale: Single;
  RBody: TCastleRigidBody;
begin
  if CastleApplicationMode = appDesign then
    Exit;

  if FocusedContainer = nil then
    Exit;

  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;

  if Rbody = nil then
    Exit;

  if ModifiersDown(FocusedContainer.Pressed) = [mkCtrl] then
  begin
    if AllowSlowerRotations then
      SpeedScale := 0.1 {* RotationControlFactor(IsOnGroundBool)};
  end
  else
    SpeedScale := 1.0 {* RotationControlFactor(IsOnGroundBool)};

  { Do not allow the body to rotate by default }
  RBody.AngularVelocity := Vector3(0,0,0);

  if Input_RightRotate.IsPressed(FocusedContainer) then
    RBody.AngularVelocity := RBody.AngularVelocity + Vector3(0,-1,0) * RotationHorizontalSpeed * SpeedScale;
  if Input_LeftRotate.IsPressed(FocusedContainer) then
    RBody.AngularVelocity := RBody.AngularVelocity + Vector3(0,1,0) * RotationHorizontalSpeed * SpeedScale;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TRotateRigidBodyByKeys.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FAllowSlowerRotations := true;

  FInput_LeftRotate              := TInputShortcut.Create(Self);
  FInput_RightRotate             := TInputShortcut.Create(Self);

  Input_LeftRotate              .Assign(keyArrowLeft);
  Input_RightRotate             .Assign(keyArrowRight);
end;

initialization
  RegisterSerializableComponent(TRotateRigidBodyByKeys, ['Physics', 'Rotate Rigid Body By Keys']);

end.

