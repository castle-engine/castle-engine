unit RotateRigidBody;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleBehaviors, CastleViewport, CastleUIControls,
  CastleVectors,  CastleRenderOptions, CastleInputs;

{ Rotate horizontal rigid body using angular velocity by mouse }

type
  TRotateRigidBody = class(TCastleBehavior)
  strict private
    RBody: TCastleRigidBody;
    SpeedScale: Single; // Ctrl support

    FAllowSlowerRotations: Boolean;

    FInput_RightRotate: TInputShortcut;
    FInput_LeftRotate: TInputShortcut;
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

uses CastleUtils, CastleComponentSerialize, CastleKeysMouse, CastleLog;

{ TRotateRigidBody }

procedure TRotateRigidBody.HandleMouseLook;
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
  end;
  FLastUpdateMousePosition := FocusedContainer.MousePosition;
end;

procedure TRotateRigidBody.ProcessMouseLookDelta(const Delta: TVector2);
begin
  RBody.AngularVelocity := Vector3(0,-1,0) * Delta.X * SpeedScale * FRotationHorizontalSpeed * 50;
  // WritelnLog('RBody.AngularVelocity ' + RBody.AngularVelocity.ToString);
end;

procedure TRotateRigidBody.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
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

  if buttonRight in FocusedContainer.MousePressed then
  begin
    if not FLastMousePositionIsSet then
    begin
      FLastUpdateMousePosition := FocusedContainer.MousePosition;
      FLastMousePositionIsSet := true;
    end;

    if ModifiersDown(FocusedContainer.Pressed) = [mkCtrl] then
    begin
      if AllowSlowerRotations then
        SpeedScale := 0.1 {* RotationControlFactor(IsOnGroundBool)};
    end
    else
      SpeedScale := 1.0 {* RotationControlFactor(IsOnGroundBool)};

    HandleMouseLook;
  end
  else
    FLastMousePositionIsSet := false;

  if RBody.AngularVelocity.IsPerfectlyZero then
  begin
    if Input_RightRotate.IsPressed(FocusedContainer) then
      RBody.AngularVelocity := RBody.AngularVelocity + Vector3(0,-1,0) * RotationHorizontalSpeed * SpeedScale;
    if Input_LeftRotate.IsPressed(FocusedContainer) then
      RBody.AngularVelocity := RBody.AngularVelocity + Vector3(0,1,0) * RotationHorizontalSpeed * SpeedScale;
  end;

  inherited Update(SecondsPassed, RemoveMe);
end;

constructor TRotateRigidBody.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FLastUpdateMousePosition := Vector2(0, 0);

  FAllowSlowerRotations := true;

  FInput_LeftRotate              := TInputShortcut.Create(Self);
  FInput_RightRotate             := TInputShortcut.Create(Self);

  Input_LeftRotate              .Assign(keyArrowLeft);
  Input_RightRotate             .Assign(keyArrowRight);
end;

initialization
  RegisterSerializableComponent(TRotateRigidBody, ['Physics', 'Rotate Rigid Body By Mouse And Keys']);

end.

