{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTransform;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    SceneArrow: TCastleScene;
    DynamicBodies: TCastleTransform;

    { Other components }
    RigidBodies: TCastleRigidBodyList;
    procedure AddForce;
    procedure AddCentralForce;
    procedure AddImpulse;
    procedure AddTorque;
    function ForceScale: Single;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, Math;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
var
  T: TCastleTransform;
  RBody: TCastleRigidBody;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  SceneArrow := DesignedComponent('SceneArrow') as TCastleScene;
  DynamicBodies := DesignedComponent('DynamicBodies') as TCastleTransform;

  RigidBodies := TCastleRigidBodyList.Create;

  for T in DynamicBodies do
  begin
    RBody := T.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if RBody <> nil then
      RigidBodies.Add(RBody);
  end;
end;

procedure TStateMain.Stop;
begin
  FreeAndNil(RigidBodies);
  inherited;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  MoveSpeed = 10;
  ScaleIncrease = 2;
  RotationSpeed = 10;
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Transform SceneArrow by keys }

  if Container.Pressed[keyW] then
    SceneArrow.Translation := SceneArrow.Translation + Vector3(0, 0, -1) * SecondsPassed * MoveSpeed;
  if Container.Pressed[keyS] then
    SceneArrow.Translation := SceneArrow.Translation + Vector3(0, 0,  1) * SecondsPassed * MoveSpeed;
  if Container.Pressed[keyA] then
    SceneArrow.Translation := SceneArrow.Translation + Vector3(-1, 0, 0) * SecondsPassed * MoveSpeed;
  if Container.Pressed[keyD] then
    SceneArrow.Translation := SceneArrow.Translation + Vector3( 1, 0, 0) * SecondsPassed * MoveSpeed;

  if Container.Pressed[keyQ] then
    SceneArrow.Scale := SceneArrow.Scale * Vector3(1, 1, Power(ScaleIncrease, SecondsPassed));
  if Container.Pressed[keyE] then
    SceneArrow.Scale := SceneArrow.Scale * Vector3(1, 1, Power(1 / ScaleIncrease, SecondsPassed));

  if Container.Pressed[keyZ] then
    SceneArrow.Rotation := Vector4(0, 1, 0, SceneArrow.Rotation.W + SecondsPassed * RotationSpeed);
  if Container.Pressed[keyC] then
    SceneArrow.Rotation := Vector4(0, 1, 0, SceneArrow.Rotation.W - SecondsPassed * RotationSpeed);

  if Container.Pressed[key7] then
    AddForce;
  if Container.Pressed[key8] then
    AddCentralForce;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(key9) then
  begin
    AddImpulse;
    Exit(true);
  end;
  if Event.IsKey(key0) then
  begin
    AddTorque;
    Exit(true);
  end;
end;

function TStateMain.ForceScale: Single;
begin
  Result := SceneArrow.Scale.Z * 1;
end;

procedure TStateMain.AddForce;
var
  RBody: TCastleRigidBody;
begin
  for RBody in RigidBodies do
    RBody.AddForce(SceneArrow.Direction * ForceScale, SceneArrow.Translation);
end;

procedure TStateMain.AddCentralForce;
var
  RBody: TCastleRigidBody;
  Dir: TVector3;
begin
  for RBody in RigidBodies do
  begin
    Dir := SceneArrow.Direction * ForceScale;
    Dir := RBody.Parent.WorldToLocalDirection(Dir);
    RBody.AddCentralForce(Dir);
  end;
end;

procedure TStateMain.AddImpulse;
var
  RBody: TCastleRigidBody;
begin
  for RBody in RigidBodies do
    RBody.ApplyImpulse(SceneArrow.Direction * ForceScale, SceneArrow.Translation);
end;

procedure TStateMain.AddTorque;
var
  RBody: TCastleRigidBody;
begin
  for RBody in RigidBodies do
    RBody.AddTorque(SceneArrow.Direction * ForceScale);
end;

end.
