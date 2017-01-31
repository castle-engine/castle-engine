{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example how to animate (change, modify) the 3D model (it's VRML/X3D graph)
  using ObjectPascal code.
  Run this program without any parameters in this directory
  (it opens file "data/boxes.x3dv") and watch the trivial animation.
  You can rotate/move the scene by dragging with mouse,
  see view3dscene docs (we use the same "Examine" camera).

  For programmers:

  Generally, you just change VRML/X3D graph (rooted in Scene.RootNode)
  however you like, whenever you like. TX3DNode class has a lot of methods
  to find and change nodes within the graph, you can insert/delete/change
  any of their children nodes, fields, and generally do everything.
  Remember to call "Changed" on every changed field (or change it only
  by Send methods, see more info on
  [http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.scene.html#section.scene_caching].)

  Of course, in case of trivial animation in this program, we could
  also express it directly in VRML/X3D and just load the scene,
  setting Scene.ProcessEvents := true. This would make the scene "animate itself",
  without the need for any ObjectPascal code to do this. But, for example sake,
  we animate it here by our own code.
}

program animate_3d_model_by_code;

uses CastleVectors, X3DNodes, CastleWindow, CastleLog,
  CastleUtils, SysUtils, CastleGLUtils, CastleScene, CastleCameras,
  CastleFilesUtils, CastleParameters, CastleStringUtils, CastleKeysMouse,
  CastleApplicationProperties;

var
  Window: TCastleWindow;
  Scene: TCastleScene;

var
  TransformBox2: TTransformNode;
  TransformBox3: TTransformNode;
  TransformBox4: TTransformNode;

procedure Update(Container: TUIContainer);
begin
  { We want to keep track of current time here (for calculating rotations
    below). It's most natural to just use Scene.Time property for this.
    (Scene.Time is already incremented for us by SceneManager.) }

  TransformBox2.FdRotation.RotationRad := Scene.Time;
  TransformBox2.FdRotation.Changed;

  TransformBox3.FdRotation.RotationRad := Scene.Time * 2;
  TransformBox3.FdRotation.Changed;

  TransformBox4.FdRotation.RotationRad := Scene.Time * 4;
  TransformBox4.FdRotation.Changed;
end;

begin
  Window := TCastleWindow.Create(Application);

  Parameters.CheckHigh(0);
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Scene := TCastleScene.Create(nil);
  try
    Scene.Load(ApplicationData('boxes.x3dv'));
    TransformBox2 := Scene.RootNode.FindNodeByName(TTransformNode,
      'Box2Transform', true) as TTransformNode;
    TransformBox3 := Scene.RootNode.FindNodeByName(TTransformNode,
      'Box3Transform', true) as TTransformNode;
    TransformBox4 := Scene.RootNode.FindNodeByName(TTransformNode,
      'Box4Transform', true) as TTransformNode;

    { init SceneManager with our Scene }
    Window.SceneManager.MainScene := Scene;
    Window.SceneManager.Items.Add(Scene);

    { init SceneManager.Camera }
    Window.SceneManager.Camera := TExamineCamera.Create(Window);
    (Window.SceneManager.Camera as TExamineCamera).Init(Scene.BoundingBox, 0.1);

    Window.OnUpdate := @Update;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.OpenAndRun;
  finally Scene.Free end;
end.
