{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simplest demo of using our engine to load and render a model.

  - Create TCastleWindow,
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
  Window: TCastleWindow;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
begin
  Window := TCastleWindow.Create(Application);
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

  Viewport.InsertBack(TCastleExamineNavigation.Create(Application));

  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application { Owner that will free the Scene });
  Scene.Load(
    //'castle-data:/bridge_final.x3dv'
    'castle-data:/car.gltf'
  );

  { Initialize spatial structures, to allow
    - collision detection in a dynamic scene
    - faster frustum culling optimization when rendering. }
  Scene.PreciseCollisions := true;

  { X3D events will be processed.
    You seldom need to set "Scene.ProcessEvents := true" explicitly --
    as the main usage of events is animation, and doing
    "Scene.PlayAnimation" would set "Scene.ProcessEvents := true" anyway.
    But doing this explicitly makes sense if your model relies on other X3D events,
    like X3D touch/key sensors or X3D scripts. }
  Scene.ProcessEvents := true;

  { Adding the scene to Viewport.Items makes is actually visible and updated,
    as part of the given viewport. }
  Viewport.Items.Add(Scene);

  { Setting the scene as MainScene allows the engine to initialize some central
    things based on this scene:
    - Viewport.AutoCamera
      will look for camera/navigation information in the MainScene.
    - The headlight will, by default, follow configuration from the MainScene
      (though you can explicitly enable/disable it by Viewport.Items.Headlight too).
    - See TCastleRootTransform.MainScene documentation for details.

    This is optional, i.e. leaving Viewport.Items.MainScene as "nil" is also totall OK.
    Just be sure in that case to assign sensible camera/navigation using other methods. }
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.
