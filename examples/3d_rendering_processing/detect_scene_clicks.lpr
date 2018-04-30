{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example how to use SceneManager.MouseRayHit to detect clicks on
  a particular scene.

  Note that you could also look at SceneManager.TriangleHit for details of
  the triangle under mouse. You can also look at each scene properties

    PointingDeviceOverItem: PTriangle;
    PointingDeviceOverPoint: TVector3;
    PointingDeviceActive: boolean;

  Alternative way to capture clicks is to use TouchSensor X3D node
  (not shown in this example). }
program detect_scene_clicks;

uses SysUtils,
  CastleWindow, CastleScene, CastleNotifications, CastleSceneManager,
  CastleColors, CastleVectors, CastleFilesUtils, X3DNodes, CastleTransform,
  CastleKeysMouse, CastleUIControls;

var
  Window: TCastleWindow;
  Notifications: TCastleNotifications;

procedure AddHexagon(
  const Translation: TVector3; const Color: TCastleColorRGB; const SceneTag: Integer);
var
  Scene: TCastleScene;
  Material: TMaterialNode;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('hexagon.x3d'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Scene.Translation := Translation;
  Scene.Tag := SceneTag;

  Material := Scene.Node('MA_mat_hexagon') as TMaterialNode;
  Material.DiffuseColor := Color;

  Window.SceneManager.Items.Add(Scene);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  TopMostScene: TCastleTransform;
begin
  if Event.IsMouseButton(mbLeft) then
  begin
    if Window.SceneManager.MouseRayHit <> nil then
      TopMostScene := Window.SceneManager.MouseRayHit.First.Item
    else
      TopMostScene := nil;

    if TopMostScene <> nil then
      Notifications.Show('Clicked on scene ' + IntToStr(TopMostScene.Tag));
  end;
end;

function CreateMainSceneNode: TX3DRootNode;
var
  NavigationInfo: TNavigationInfoNode;
begin
  Result := TX3DRootNode.Create;

  NavigationInfo := TNavigationInfoNode.Create;
  NavigationInfo.Headlight := true;
  Result.AddChildren(NavigationInfo);
end;

var
  MainScene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.OnPress := @WindowPress;
  Window.Open;

  Notifications := TCastleNotifications.Create(Application);
  Notifications.Anchor(hpLeft, 10);
  Notifications.Anchor(vpBottom, 10);
  Window.Controls.InsertFront(Notifications);

  AddHexagon(Vector3(0, 0, 0), YellowRGB, 1);
  AddHexagon(Vector3(1.5, 0, 1), BlueRGB, 2);
  AddHexagon(Vector3(-1.5, 0, 1), RedRGB, 3);

  // add MainScene only to turn on headlight (light shining from camera)
  MainScene := TCastleScene.Create(Application);
  MainScene.Load(CreateMainSceneNode, true);
  Window.SceneManager.Items.Add(MainScene);
  Window.SceneManager.MainScene := MainScene;

  // configure initial camera view
  Window.SceneManager.WalkCamera.SetView(
    Vector3(0, 10, 0),
    Vector3(0, -1, 0),
    Vector3(0, 0, -1)
  );
  // do not allow user to move camera by arrow keys etc. by default
  Window.SceneManager.WalkCamera.Input := [];

  Application.Run;
end.
