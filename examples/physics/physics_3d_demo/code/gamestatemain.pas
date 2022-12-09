{
  Copyright 2017-2022 Michalis Kamburelis.

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
  CastleVectors, CastleUIState, CastleComponentSerialize, CastleCameras,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonLevelFlat, ButtonLevelComplex: TCastleButton;
    LevelFlat, LevelComplex: TCastleTransform;
    MainViewport: TCastleViewport;
    MainNavigation: TCastleWalkNavigation;
  private
    procedure ClickLevelFlat(Sender: TObject);
    procedure ClickLevelComplex(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.ClickLevelFlat(Sender: TObject);
begin
  LevelFlat.Exists := true;
  LevelComplex.Exists := false;
  MainViewport.AssignDefaultCamera;
end;

procedure TStateMain.ClickLevelComplex(Sender: TObject);
begin
  LevelFlat.Exists := false;
  LevelComplex.Exists := true;
  MainViewport.AssignDefaultCamera;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;

  procedure Spawn(const SpawnDesignUrl: String);
  var
    CameraPos, CameraDir, CameraUp: TVector3;
    SpawnOwner: TComponent;
    SpawnRigidBody: TCastleRigidBody;
    SpawnTransform: TCastleTransform;
  begin
    SpawnOwner := TComponent.Create(FreeAtStop);
    SpawnTransform := TransformLoad(SpawnDesignUrl, SpawnOwner);

    MainViewport.Camera.GetView(CameraPos, CameraDir, CameraUp);
    SpawnTransform.Translation := CameraPos + CameraDir * 2.0;
    SpawnTransform.Direction := CameraDir;

    MainViewport.Items.Add(SpawnTransform);

    SpawnRigidBody := SpawnOwner.FindRequiredComponent('SpawnRigidBody') as TCastleRigidBody;
    SpawnRigidBody.LinearVelocity := CameraDir * 8.0;
  end;

begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    Spawn('castle-data:/box_with_physics.castle-transform');
    Exit(true);
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    Spawn('castle-data:/sphere_with_physics.castle-transform');
    Exit(true);
  end;

  if Event.IsKey(keyF4) then
  begin
    MainNavigation.MouseLook := not MainNavigation.MouseLook;
    Exit(true);
  end;

  if Event.IsKey(keyF6) then
  begin
    MainViewport.Items.EnablePhysics := not MainViewport.Items.EnablePhysics;
    Exit(true);
  end;
end;

end.
