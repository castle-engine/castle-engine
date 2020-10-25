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

{ Example how to use Viewport.MouseRayHit to detect clicks on
  a particular scene.

  Note that you could also look at Viewport.TriangleHit for details of
  the triangle under mouse. You can also look at each scene properties

    PointingDeviceOverItem: PTriangle;
    PointingDeviceOverPoint: TVector3;
    PointingDeviceActive: boolean;

  Alternative way to capture clicks is to use TouchSensor X3D node
  (not shown in this example). }
program detect_scene_clicks;

uses SysUtils,
  CastleWindow, CastleScene, CastleNotifications, CastleViewport,
  CastleColors, CastleVectors, CastleFilesUtils, X3DNodes, CastleTransform,
  CastleKeysMouse, CastleUIControls;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Notifications: TCastleNotifications;

procedure AddHexagon(
  const Translation: TVector3; const Color: TCastleColorRGB; const SceneTag: Integer);
var
  Scene: TCastleScene;
  Material: TMaterialNode;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/hexagon.x3d');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Scene.Translation := Translation;
  Scene.Tag := SceneTag;

  Material := Scene.Node('MA_mat_hexagon') as TMaterialNode;
  Material.DiffuseColor := Color;

  Viewport.Items.Add(Scene);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  TopMostScene: TCastleTransform;
begin
  if Event.IsMouseButton(buttonLeft) then
  begin
    TopMostScene := Viewport.TransformUnderMouse;
    if TopMostScene <> nil then
      Notifications.Show('Clicked on scene ' + IntToStr(TopMostScene.Tag))
    else
      Notifications.Show('Clicked on empty space')
  end;
end;

begin
  Window := TCastleWindowBase.Create(Application);
  Window.OnPress := @WindowPress;
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Window.Controls.InsertFront(Viewport);

  Notifications := TCastleNotifications.Create(Application);
  Notifications.Anchor(hpLeft, 10);
  Notifications.Anchor(vpBottom, 10);
  Window.Controls.InsertFront(Notifications);

  AddHexagon(Vector3(0, 0, 0), YellowRGB, 1);
  AddHexagon(Vector3(1.5, 0, 1), BlueRGB, 2);
  AddHexagon(Vector3(-1.5, 0, 1), RedRGB, 3);

  Viewport.Items.UseHeadlight := hlOn;

  // configure initial camera view
  Viewport.Camera.SetView(
    Vector3(0, 10, 0),
    Vector3(0, -1, 0),
    Vector3(0, 0, -1)
  );

  Application.Run;
end.
