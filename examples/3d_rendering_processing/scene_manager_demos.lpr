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

{ Demo of using SceneManager. Shows how you can add many 3D objects to
  SceneManager.Items tree. For simpler usage,
  see view_3d_model_basic.lpr first.

  This reads a couple of 3D files from data/ subdirectory.
  Their URLs are in the source code below.
  Do not hesitate to experiment, just replace URLs of the 3D models
  with your own, and generally experiment freely with this program :)
}
program scene_manager_demos;

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses CastleUtils, CastleWindow, CastleVectors, CastleLog, CastleTransform,
  CastleSceneCore, CastleScene, X3DFields, X3DNodes, CastleApplicationProperties,
  CastleFilesUtils;

var
  Window: TCastleWindow;
  Scene, ParticlesScene, DinoScene: TCastleScene;
  DinoTransform1, DinoTransform2: TCastleTransform;
begin
  Window := TCastleWindow.Create(Application);
  Window.Open;
  Window.FpsShowOnCaption := true;

  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  { initialize first Scene }
  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('bridge_final.x3dv'));
  { This makes scene octrees, allowing collision detection in (possibly)
    dynamic scene (ssDynamicCollisions) and frustum culling
    optimization when rendering (ssRendering). }
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  { This makes our 3D scene interactive, reacting to key and mouse
    presses, processing VRML/X3D script nodes etc. }
  Scene.ProcessEvents := true;

  { add Scene to SceneManager }
  { Add your scene to SceneManager.Items. This is essential,
    it makes the scene actually rendered, animated etc. }
  Window.SceneManager.Items.Add(Scene);
  { Set the scene as SceneManager.MainScene. This is optional
    (you do have to set SceneManager.MainScene), but usually desired.
    MainScene is used in various moments when we need a single designated 3D
    object to do some task. For example, default camera location is taken
    from Viewpoint VRML/X3D node inside the MainScene. }
  Window.SceneManager.MainScene := Scene;

  { To have some more fun with SceneManager, let's add more 3D objects to it.
    Let's suppose we want to have SceneManager.Items to be a tree:

    SceneManager.Items
    |- Scene (class TCastleScene)
    |- ParticlesScene (class TCastleScene)
    |- DinoTransform1 (class TCastleTransform)
       |- DinoScene (class TCastleScene)
    |- DinoTransform2 (class TCastleTransform)
       |- DinoScene (class TCastleScene)

    Note that the DinoScene is present 2 times in the tree, but under
    different transformations. This is a very optimal way to render
    the same scene at many places in your world.
    However, both scene transformations will always show the same animation frame.
    If you want to show different frames, you would instead create two TCastleScene
    instances (you can create the 2nd one by using "Clone" method on the 1st one).
  }

  { initialize ParticlesScene }
  ParticlesScene := TCastleScene.Create(Application);
  ParticlesScene.Load(ApplicationData('castle_script_particles.x3dv'));
  ParticlesScene.Spatial := [ssRendering, ssDynamicCollisions];
  { Modify the loaded nodes graph, just to show that we can.
    We find a node called ParticleScript, and change it's "count" field.
    This affects the number of particles generated.
    We change it before the animation even starts (before activating ProcessEvents). }
  (ParticlesScene.Field('ParticleScript', 'count') as TSFInt32).Send(100);
  ParticlesScene.ProcessEvents := true;
  ParticlesScene.Translation := Vector3(-15, -4, 0);
  ParticlesScene.Rotation := Vector4(1, 0, 0, -Pi/2);
  Window.SceneManager.Items.Add(ParticlesScene);

  { initialize DinoScene }
  DinoScene := TCastleScene.Create(Application);
  DinoScene.Load(ApplicationData('raptor.castle-anim-frames'));
  DinoScene.ProcessEvents := true;
  DinoScene.Spatial := [ssRendering, ssDynamicCollisions];
  { render wireframe over a normal model. See TWireframeEffect docs
    for other options.  }
  DinoScene.Attributes.WireframeEffect := weSolidWireframe;
  DinoScene.Attributes.WireframeColor := Vector3(0, 0.25, 0); { dark green }
  // will be added to DinoTransform1 and DinoTransform2 later

  { initialize DinoTransform1 }
  DinoTransform1 := TCastleTransform.Create(Application);
  DinoTransform1.Translation := Vector3(-20, -1, -2);
  Window.SceneManager.Items.Add(DinoTransform1);

  { initialize DinoTransform2 }
  DinoTransform2 := TCastleTransform.Create(Application);
  DinoTransform2.Translation := Vector3(-30, -1, -2);
  Window.SceneManager.Items.Add(DinoTransform2);

  DinoTransform1.Add(DinoScene);
  DinoTransform2.Add(DinoScene);

  Application.Run;
end.
