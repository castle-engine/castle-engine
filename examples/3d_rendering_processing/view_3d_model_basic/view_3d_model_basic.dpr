{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simplest demo of using our engine to load and render a model.

  - Create TCastleWindowBase,
  - Create TCastleViewport (rectangular area within window to display scene),
  - Create and load TCastleScene (model loaded from file).

  This trivial program is a fully capable, interactive
  browser of all model formats supported by Castle Game Engine
  ( https://castle-engine.io/creating_data_model_formats.php ).
  The model is displayed, animated, may be interactive (if using X3D sensors),
  you can navigate in it etc. }
program view_3d_model_basic;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils,
  CastleWindow, CastleSceneCore, CastleScene, CastleViewport, CastleCameras, CastleVectors;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;

  Viewport.AutoCamera := true;
  // Instead of using AutoCamera:=true, you could initialize camera explicitly:
  // Viewport.Camera.SetView(
  //   Vector3(-46.30, -4.49, 4.89), // position
  //   Vector3(0.96, 0.03, -0.27), // direction
  //   Vector3(-0.03, 1.00, 0.01), // up (current)
  //   Vector3(0.00, 1.00, 0.00) // gravity up
  // );

  Viewport.AutoNavigation := true;
  // Instead of using AutoNavigation:=true, you could initialize navigation explicitly:
  // Viewport.Navigation := TCastleExamineNavigation.Create(Application);

  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application { Owner that will free the Scene });
  Scene.Load(
    //'castle-data:/bridge_final.x3dv'
    'castle-data:/car.gltf'
  );
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.
