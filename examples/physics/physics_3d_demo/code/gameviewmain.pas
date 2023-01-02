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
{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleCameras,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
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
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonLevelFlat.OnClick := {$ifdef FPC}@{$endif} ClickLevelFlat;
  ButtonLevelComplex.OnClick := {$ifdef FPC}@{$endif} ClickLevelComplex;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickLevelFlat(Sender: TObject);
begin
  LevelFlat.Exists := true;
  LevelComplex.Exists := false;
  MainViewport.Camera.SetView(
    Vector3(0.00, 2.00, 4.00),
    Vector3(0, 0, -1),
    Vector3(0, 1, 0));
end;

procedure TViewMain.ClickLevelComplex(Sender: TObject);
begin
  LevelFlat.Exists := false;
  LevelComplex.Exists := true;
  MainViewport.Camera.SetView(
    Vector3(0.00, 2.00, 4.00),
    Vector3(0, 0, -1),
    Vector3(0, 1, 0));
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;

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
