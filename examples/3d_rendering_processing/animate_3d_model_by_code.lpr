{
  Copyright 2008-2018 Michalis Kamburelis.

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
  [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.scene.html#section.scene_caching].)

  Of course, in case of trivial animation in this program, we could
  also express it directly in VRML/X3D and just load the scene,
  setting Scene.ProcessEvents := true. This would make the scene "animate itself",
  without the need for any ObjectPascal code to do this. But, for example sake,
  we animate it here by our own code.
}

program animate_3d_model_by_code;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses CastleVectors, X3DNodes, CastleWindow, CastleLog,
  CastleUtils, SysUtils, CastleGLUtils, CastleScene, CastleCameras,
  CastleFilesUtils, CastleParameters, CastleStringUtils, CastleKeysMouse,
  CastleApplicationProperties, CastleViewport, CastleTimeUtils;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;

var
  Time: TFloatTime;
  TransformBox2: TTransformNode;
  TransformBox3: TTransformNode;
  TransformBox4: TTransformNode;

procedure Update(Container: TUIContainer);
begin
  Time += Container.Fps.SecondsPassed;

  { change rotation angles (4th component of the vector),
    leaving the rotation axis (XYZ components) unchanged. }
  TransformBox2.Rotation := Vector4(TransformBox2.Rotation.XYZ, Time);
  TransformBox3.Rotation := Vector4(TransformBox2.Rotation.XYZ, Time * 2);
  TransformBox4.Rotation := Vector4(TransformBox2.Rotation.XYZ, Time * 4);
end;

begin
  InitializeLog;
  Application.ParseStandardParameters;
  Parameters.CheckHigh(0); // no command-line options specific to this program are allowed

  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/boxes.x3dv');
  TransformBox2 := Scene.RootNode.FindNodeByName(TTransformNode,
    'Box2Transform', true) as TTransformNode;
  TransformBox3 := Scene.RootNode.FindNodeByName(TTransformNode,
    'Box3Transform', true) as TTransformNode;
  TransformBox4 := Scene.RootNode.FindNodeByName(TTransformNode,
    'Box4Transform', true) as TTransformNode;

  { init Viewport with our Scene }
  Viewport.Items.MainScene := Scene;
  Viewport.Items.Add(Scene);

  { init Viewport.Navigation }
  Viewport.Navigation := TCastleExamineNavigation.Create(Application);

  Window.OnUpdate := @Update;
  Window.SetDemoOptions(keyF11, CharEscape, true);
  Application.Run;
end.
