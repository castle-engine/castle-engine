{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Spine".

  "Castle Spine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Spine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implements the game logic, independent from Android / standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils,
  CastleControls, CastleKeysMouse, CastleFilesUtils, Castle2DSceneManager,
  CastleVectors;

var
  SceneManager: T2DSceneManager;
  BackgroundScene: T2DScene;
  CameraView3D: TCastleButton;

type
  TButtonsHandler = class
    procedure CameraView3DClick(Sender: TObject);
  end;

procedure TButtonsHandler.CameraView3DClick(Sender: TObject);
const
  AnimateTime = 1.0;
begin
  { since this is really 3D, show alternative camera view where is clearly visible }
  CameraView3D.Pressed := not CameraView3D.Pressed;
  if not CameraView3D.Pressed then
    SceneManager.Camera.AnimateTo(
      { camera values like initialized by T2DSceneManager }
      { pos } Vector3Single(0, 0, 0),
      { dir } Vector3Single(0, 0, -1),
      { up } Vector3Single(0, 1, 0),
      AnimateTime) else
    SceneManager.Camera.AnimateTo(
      { hint: to pick camera values experimentally, use view3dscene
        and Console->Print Current Camera.. menu item }
      { pos } Vector3Single(8.8673858642578125, 1.2955703735351563, -19.951961517333984),
      { dir } Vector3Single(0.6533171534538269, -0.13534677028656006, -0.7448880672454834),
      { up } Vector3Single(0.10390207171440125, 0.99060958623886108, -0.088865458965301514),
      AnimateTime);
end;

{ One-time initialization. }
procedure ApplicationInitialize;
begin
  SceneManager := T2DSceneManager.Create(Application);
  SceneManager.Transparent := false; // show background color underneath scene manager
  Window.Controls.InsertFront(SceneManager);

  BackgroundScene := T2DScene.Create(Application);
  SceneManager.Items.Add(BackgroundScene);
  SceneManager.MainScene := BackgroundScene;
  BackgroundScene.Load(ApplicationData('background.x3dv'));

  { We always want to see full height of background.x3dv,
    we know it starts from bottom = 0.
    BoudingBox.Data[1][1] is the maximum Y value, i.e. our height.
    So projection height should adjust to background.x3dv height. }
  SceneManager.ProjectionAutoSize := false;
  SceneManager.ProjectionHeight := BackgroundScene.BoundingBox.Data[1][1];

  CameraView3D := TCastleButton.Create(Window);
  CameraView3D.Caption := '3D Camera View';
  CameraView3D.OnClick := @TButtonsHandler(nil).CameraView3DClick;
  CameraView3D.Toggle := true;
  CameraView3D.Left := 10;
  CameraView3D.Bottom := 10;
  Window.Controls.InsertFront(CameraView3D);
end;

procedure WindowOpen(Container: TUIContainer);
begin
end;

procedure WindowResize(Container: TUIContainer);
begin
end;

procedure WindowUpdate(Container: TUIContainer);
begin
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
  if Event.IsKey(K_Escape) then
    Application.Quit;

  if Event.IsMouseButton(mbLeft) then
  begin
  end;
end;

function MyGetApplicationName: string;
begin
  Result := 'castle_spine';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowCustom.Create(Application);
  Window.OnOpen := @WindowOpen;
  Window.OnPress := @WindowPress;
  Window.OnUpdate := @WindowUpdate;
  Window.OnResize := @WindowResize;
  Window.FpsShowOnCaption := true;
  Application.MainWindow := Window;
end.
