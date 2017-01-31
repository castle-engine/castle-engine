{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simplest demo of using our engine to load and render 3D model.
  Create and load a Scene,
  create a Window (which automatically also creates Window.SceneManager),
  add Scene to Window.SceneManager.

  This trivial program is a fully-capable VRML/X3D browser and viewer
  for all 3D models. "VRML/X3D browser" means that it not only renders
  the scene, it also animates it, allows you to interact with it
  (if a scene uses mouse/key sensors), allows you to navigate within it
  (with navigation mode adjusted to NavigationInfo.type, see view3dscene docs
  for keys/mouse to control Walk/Examine navigation
  [http://castle-engine.sourceforge.net/view3dscene.php]), with collision
  detection and generally with *everything* working. }
program view_3d_model_basic;

uses SysUtils, CastleWindow, CastleSceneCore, CastleScene;

var
  Window: TCastleWindow;
  Scene: TCastleScene;
begin
  Scene := TCastleScene.Create(Application { Owner that will free the Scene });
  Scene.Load('data/bridge_final.x3dv');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Window := TCastleWindow.Create(Application);
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Window.Open;
  Application.Run;
end.
