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

{ Use SceneManager.OnProjection to adjust the projection settings. }
program switch_projection;

uses SysUtils,
  CastleWindow, CastleSceneCore, CastleScene, CastleProjection, CastleControls,
  CastleCameras, CastleUIControls;

var
  OrthographicProjection: Boolean = false;
  ButtonToggleOrthographic: TCastleButton;

type
  TEventsHandler = class
    class procedure ClickToggleOrthographic(Sender: TObject);
    class procedure AdjustProjection(var Parameters: TProjection);
  end;

class procedure TEventsHandler.ClickToggleOrthographic(Sender: TObject);
begin
  OrthographicProjection := not OrthographicProjection;
  ButtonToggleOrthographic.Pressed := OrthographicProjection;
end;

class procedure TEventsHandler.AdjustProjection(var Parameters: TProjection);
begin
  { By default, Parameters.ProjectionType is based on existence on
    Viewpoint or OrthoViewpoint node in scene.
    We override it here, to based it on our Boolean OrthographicProjection
    variable.
    Note that we could also adjust other parameters,
    like Parameters.Dimensions that determine orthographic visible view. }

  if OrthographicProjection then
    Parameters.ProjectionType := ptOrthographic
  else
    Parameters.ProjectionType := ptPerspective;
end;

var
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  ButtonToggleOrthographic := TCastleButton.Create(Application);
  ButtonToggleOrthographic.Caption := 'Orthographic Projection';
  ButtonToggleOrthographic.Toggle := true;
  ButtonToggleOrthographic.OnClick := @TEventsHandler(nil).ClickToggleOrthographic;
  ButtonToggleOrthographic.Anchor(vpBottom, 10);
  ButtonToggleOrthographic.Anchor(hpLeft, 10);
  Window.Controls.InsertFront(ButtonToggleOrthographic);

  Scene := TCastleScene.Create(Application { Owner that will free the Scene });
  Scene.Load('data/bridge_final.x3dv');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  { force Examine navigation, Walk doesn't work comfortably
    in orthographic projection. }
  Window.SceneManager.NavigationType := ntExamine;

  Window.SceneManager.OnProjection := @TEventsHandler(nil).AdjustProjection;

  Application.Run;
end.
