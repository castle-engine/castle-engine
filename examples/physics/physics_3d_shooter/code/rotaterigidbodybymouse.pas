unit RotateRigidBodyByMouse;

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors,  CastleRenderOptions;

{ Rotate horizontal rigid body using angular velocity by mouse }

type
  TRotateRigidBodyByMouse = class(TCastleBehavior)
  strict private
    RBody: TCastleRigidBody;
    SpeedScale: Single; // Ctrl support
  private
    FRotationHorizontalSpeed: Single;
    FLastUpdateMousePosition: TVector2;
    FLastMousePositionIsSet: Boolean;
    const
      DefaultMouseLookHorizontalSensitivity = Pi * 0.2 / 180;
      DefaultRotationHorizontalSpeed = Pi * 150 / 180;

    procedure HandleMouseLook;
    procedure ProcessMouseLookDelta(const Delta: TVector2);
  protected
     procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  public
     constructor Create(AOwner: TComponent); override;
  end;

implementation

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TMouseCameraRotation }

procedure TRotateRigidBodyByMouse.HandleMouseLook;
var
  MouseChange: TVector2;
begin
  MouseChange := (FocusedContainer.MousePosition) - FLastUpdateMousePosition;

  if not MouseChange.IsPerfectlyZero then
  begin
//    if InvertVerticalMouseLook then
//      MouseChange.Y := -MouseChange.Y;
    MouseChange.X := MouseChange.X * DefaultMouseLookHorizontalSensitivity;
    MouseChange.Y := MouseChange.Y;
    ProcessMouseLookDelta(MouseChange);
  end
  else
    RBody.AngularVelocity := TVector3.Zero;
  FLastUpdateMousePosition := FocusedContainer.MousePosition;
end;

procedure TRotateRigidBodyByMouse.ProcessMouseLookDelta(const Delta: TVector2);
begin
  RBody.AngularVelocity := Vector3(0,-1,0) * Delta.X * SpeedScale * FRotationHorizontalSpeed * 50;
  WritelnLog('RBody.AngularVelocity ' + RBody.AngularVelocity.ToString);
end;

procedure TRotateRigidBodyByMouse.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
begin
  if CastleApplicationMode = appDesign then
    Exit;

  if FocusedContainer = nil then
    Exit;

  RBody := Parent.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if RBody = nil then
    Exit;

  if buttonRight in FocusedContainer.MousePressed then
  begin
    if not FLastMousePositionIsSet then
    begin
      FLastUpdateMousePosition := FocusedContainer.MousePosition;
      FLastMousePositionIsSet := true;
    end;

    if ModifiersDown(FocusedContainer.Pressed) = [mkCtrl] then
    begin
      //if AllowSlowerRotations then
        SpeedScale := 0.1 {* RotationControlFactor(IsOnGroundBool)};
    end
    else
      SpeedScale := 1.0 {* RotationControlFactor(IsOnGroundBool)};

    HandleMouseLook;
  end
  else
  begin
    FLastMousePositionIsSet := false;
    RBody.AngularVelocity := TVector3.Zero;
  end;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TRotateRigidBodyByMouse.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FLastUpdateMousePosition := Vector2(0, 0);
end;

initialization
  RegisterSerializableComponent(TRotateRigidBodyByMouse, ['Physics', 'Rotate Rigid Body By Mouse']);

end.

