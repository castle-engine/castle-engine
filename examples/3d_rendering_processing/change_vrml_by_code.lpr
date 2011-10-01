{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This is a simple example how to change the VRML graph
  from your program, that is using Pascal code.
  Run this program without any parameters in this directory
  (it opens file "models/boxes.x3dv") and watch the trivial animation.
  You can rotate/move the scene by dragging with mouse,
  see view3dscene docs (we use the same "Examine" camera).

  For programmers:

  Generally, you just change VRML graph (rooted in Scene.RootNode)
  however you like, whenever you like. TX3DNode class has a lot of methods
  to find and change nodes within the graph, you can insert/delete/change
  any of their children nodes, fields, and generally do everything.
  The only thing to keep in ming is to call one of Scene.ChangedXxx
  methods after changes. More info about this is on
  [http://castle-engine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.scene.html#section.scene_caching].

  Of course, in case of trivial animation in this program, we could
  also express it directly in VRML/X3D and just load the scene,
  setting Scene.ProcessEvents := true and then constantly calling
  Scene.IncreaseTime. This would make the scene "animate itself",
  without the need for any Pascal code to do this. But, for example sake,
  we animate it by code.
}

program change_vrml_by_code;

uses VectorMath, VRMLNodes, GL, GLU, CastleWindow, CastleWarnings,
  CastleUtils, SysUtils, CastleGLUtils, VRMLGLScene, Cameras, CastleSceneManager,
  CastleFilesUtils, CastleParameters, CastleStringUtils;

var
  Window: TCastleWindowCustom;
  SceneManager: TCastleSceneManager;
  Scene: T3DScene;

var
  TransformBox2: TTransformNode;
  TransformBox3: TTransformNode;
  TransformBox4: TTransformNode;

procedure Idle(Window: TCastleWindowBase);
begin
  { We want to keep track of current time here (for calculating rotations
    below). It's most natural to just use Scene.Time property for this.
    (Scene.Time is already incremented for us by SceneManager.) }

  { Note that in this simple example you could avoid calling ChangedField by using
      TransformBox2.FdRotation.Send(Vector4Single(1, 0, 0, Scene.Time));
    That is, changing field's value by Send automatically calls
    appropriate ChangedField method. It also uses appropriate event,
    in case VRML events are working and this is exposed field.

    Below we show the more tedious way, by manually calling ChangedField.
    Sometimes it's more efficient, as you can change the value many times
    and call ChangedField only once. }

  TransformBox2.FdRotation.RotationRad := Scene.Time.Seconds;
  Scene.ChangedField(TransformBox2.FdRotation);

  TransformBox3.FdRotation.RotationRad := Scene.Time.Seconds * 2;
  Scene.ChangedField(TransformBox3.FdRotation);

  TransformBox4.FdRotation.RotationRad := Scene.Time.Seconds * 4;
  Scene.ChangedField(TransformBox4.FdRotation);
end;

begin
  Window := TCastleWindowCustom.Create(Application);

  Parameters.CheckHigh(0);
  OnWarning := @OnWarningWrite;

  Scene := T3DScene.Create(nil);
  try
    Scene.Load('models' + PathDelim + 'boxes.x3dv');
    TransformBox2 := Scene.RootNode.FindNodeByName(TTransformNode,
      'Box2Transform', true) as TTransformNode;
    TransformBox3 := Scene.RootNode.FindNodeByName(TTransformNode,
      'Box3Transform', true) as TTransformNode;
    TransformBox4 := Scene.RootNode.FindNodeByName(TTransformNode,
      'Box4Transform', true) as TTransformNode;

    { init SceneManager with our Scene }
    SceneManager := TCastleSceneManager.Create(Window);
    Window.Controls.Add(SceneManager);
    SceneManager.MainScene := Scene;
    SceneManager.Items.Add(Scene);

    { init SceneManager.Camera }
    SceneManager.Camera := TExamineCamera.Create(Window);
    (SceneManager.Camera as TExamineCamera).Init(Scene.BoundingBox, 0.1);

    Window.OnIdle := @Idle;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.OpenAndRun;
  finally Scene.Free end;
end.
