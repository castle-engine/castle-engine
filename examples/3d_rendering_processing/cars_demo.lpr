{
  Copyright 2010-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of TCastleScene, scene manager and related functionality.
  Follow the relevant tutorial pages
  http://castle-engine.sourceforge.net/tutorial_load_3d.php
  http://castle-engine.sourceforge.net/tutorial_scene.php
}
program cars_demo;

uses SysUtils, CastleVectors,
  CastleFilesUtils, CastleWindow, CastleSceneCore, CastleScene;

var
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('car.x3d'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Window := TCastleWindow.Create(Application);
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Window.SceneManager.RequiredCamera.SetView(
    Vector3Single(-7.83,  6.15, -7.55),
    Vector3Single( 0.47, -0.30,  0.82),
    Vector3Single( 0.16,  0.95,  0.25)
  );

  Window.Open;
  Application.Run;
end.
