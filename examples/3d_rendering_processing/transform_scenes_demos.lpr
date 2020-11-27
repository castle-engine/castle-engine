{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of using TCastleTransform hierarchy.
  Shows how you can add many objects to TCastleViewport.Items tree.

  This reads a couple of models from the data/ subdirectory.
  Their URLs are in the source code below.
  Do not hesitate to experiment, just replace URLs of the 3D models
  with your own, and generally experiment freely with this program :)
}
program transform_scenes_demos;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses CastleUtils, CastleWindow, CastleVectors, CastleLog, CastleTransform,
  CastleSceneCore, CastleScene, X3DFields, X3DNodes, CastleApplicationProperties,
  CastleFilesUtils, CastleViewport;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene, ParticlesScene, DinoScene: TCastleScene;
  DinoTransform1, DinoTransform2: TCastleTransform;
begin
  Window := TCastleWindowBase.Create(Application);
  Window.Open;
  Window.FpsShowOnCaption := true;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  { initialize first Scene }
  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/bridge_final.x3dv');
  { This makes scene octrees, allowing collision detection in (possibly)
    dynamic scene (ssDynamicCollisions) and frustum culling
    optimization when rendering (ssRendering). }
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  { This makes our 3D scene interactive, reacting to key and mouse
    presses, processing VRML/X3D script nodes etc. }
  Scene.ProcessEvents := true;

  { add Scene to Viewport.Items }
  { Add your scene to Viewport.Items. This is essential,
    it makes the scene actually rendered, animated etc. }
  Viewport.Items.Add(Scene);
  { Set the scene as Viewport.Items.MainScene.
    This is optional (you do have to set Viewport.Items.MainScene).
    Setting MainScene allows to e.g. determine initial camera by the
    X3D Viewpoint node inside the MainScene (in simple words:
    you can set the camera in Blender, and it will be used as default
    camera in the engine). }
  Viewport.Items.MainScene := Scene;

  { To have some more fun with TCastleTransform hierarchy, add more objects to it.
    We want the Viewport.Items to be a tree:

    Viewport.Items
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
  ParticlesScene.Load('castle-data:/castle_script_particles.x3dv');
  ParticlesScene.Spatial := [ssRendering, ssDynamicCollisions];
  { Modify the loaded nodes graph, just to show that we can.
    We find a node called ParticleScript, and change it's "count" field.
    This affects the number of particles generated.
    We change it before the animation even starts (before activating ProcessEvents). }
  (ParticlesScene.Field('ParticleScript', 'count') as TSFInt32).Send(100);
  ParticlesScene.ProcessEvents := true;
  ParticlesScene.Translation := Vector3(-15, -4, 0);
  ParticlesScene.Rotation := Vector4(1, 0, 0, -Pi/2);
  Viewport.Items.Add(ParticlesScene);

  { initialize DinoScene }
  DinoScene := TCastleScene.Create(Application);
  DinoScene.Load('castle-data:/raptor.castle-anim-frames');
  DinoScene.ProcessEvents := true;
  DinoScene.Spatial := [ssRendering, ssDynamicCollisions];
  { render wireframe over a normal model. See TWireframeEffect docs
    for other options.  }
  DinoScene.RenderOptions.WireframeEffect := weSolidWireframe;
  DinoScene.RenderOptions.WireframeColor := Vector3(0, 0.25, 0); { dark green }
  // will be added to DinoTransform1 and DinoTransform2 later

  { initialize DinoTransform1 }
  DinoTransform1 := TCastleTransform.Create(Application);
  DinoTransform1.Translation := Vector3(-20, -1, -2);
  Viewport.Items.Add(DinoTransform1);

  { initialize DinoTransform2 }
  DinoTransform2 := TCastleTransform.Create(Application);
  DinoTransform2.Translation := Vector3(-30, -1, -2);
  Viewport.Items.Add(DinoTransform2);

  DinoTransform1.Add(DinoScene);
  DinoTransform2.Add(DinoScene);

  Application.Run;
end.
