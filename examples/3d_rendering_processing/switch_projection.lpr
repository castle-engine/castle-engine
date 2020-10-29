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

{ Use Viewport.Camera.ProjectionType to adjust the projection settings. }
program switch_projection;

uses SysUtils,
  CastleWindow, CastleSceneCore, CastleScene, CastleProjection, CastleControls,
  CastleCameras, CastleUIControls, CastleViewport, CastleVectors;

var
  Window: TCastleWindowBase;
  Scene: TCastleScene;
  Viewport: TCastleViewport;
  OrthographicProjection: Boolean = false;
  ButtonToggleOrthographic: TCastleButton;

type
  TEventsHandler = class
    class procedure ClickToggleOrthographic(Sender: TObject);
  end;

class procedure TEventsHandler.ClickToggleOrthographic(Sender: TObject);
begin
  OrthographicProjection := not OrthographicProjection;
  if OrthographicProjection then
    Viewport.Camera.ProjectionType := ptOrthographic
  else
    Viewport.Camera.ProjectionType := ptPerspective;
  ButtonToggleOrthographic.Pressed := OrthographicProjection;
end;


begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Window.Controls.InsertFront(Viewport);

  ButtonToggleOrthographic := TCastleButton.Create(Application);
  ButtonToggleOrthographic.Caption := 'Orthographic Projection';
  ButtonToggleOrthographic.Toggle := true;
  ButtonToggleOrthographic.OnClick := @TEventsHandler(nil).ClickToggleOrthographic;
  ButtonToggleOrthographic.Anchor(vpBottom, 10);
  ButtonToggleOrthographic.Anchor(hpLeft, 10);
  Window.Controls.InsertFront(ButtonToggleOrthographic);

  Scene := TCastleScene.Create(Application { Owner that will free the Scene });
  Scene.Load('castle-data:/bridge_final.x3dv');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  { force Examine navigation, Walk doesn't work comfortably
    in orthographic projection. }
  Viewport.NavigationType := ntExamine;

  Viewport.AssignDefaultCamera;
  Viewport.Camera.Orthographic.Width := 10;
  Viewport.Camera.Orthographic.Height := 10;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);

  Application.Run;
end.
