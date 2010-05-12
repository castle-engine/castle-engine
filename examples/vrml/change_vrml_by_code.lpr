{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
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
  however you like, whenever you like. TVRMLNode class has a lot of methods
  to find and change nodes within the graph, you can insert/delete/change
  any of their children nodes, fields, and generally do everything.
  The only thing to keep in ming is to call one of Scene.ChangedXxx
  methods after changes. More info about this is on
  [http://vrmlengine.sourceforge.net/vrml_engine_doc/output/xsl/html/section.scene.html#section.scene_caching].

  Of course, in case of trivial animation in this program, we could
  also express it directly in VRML/X3D and just load the scene,
  setting Scene.ProcessEvents := true and then constantly calling
  Scene.IncreaseTime. This would make the scene "animate itself",
  without the need for any Pascal code to do this. But, for example sake,
  we animate it by code.
}

program change_vrml_by_code;

uses VectorMath, VRMLNodes, GL, GLU, GLWindow,
  KambiUtils, SysUtils, KambiGLUtils, VRMLGLScene, Cameras, KambiSceneManager,
  KambiFilesUtils, VRMLErrors;

var
  Glw: TGLUIWindow;
  SceneManager: TKamSceneManager;
  Scene: TVRMLGLScene;

var
  TransformBox2: TNodeTransform_2;
  TransformBox3: TNodeTransform_2;
  TransformBox4: TNodeTransform_2;

procedure Idle(glwin: TGLWindow);
begin
  { We want to keep track of current time here (for calculating rotations
    below). It's most natural to just use Scene.Time property for this.
    (Scene.Time is already incremented for us by SceneManager.) }

  { Note that in this simple example you could avoid calling ChangedXxx by using
      TransformBox2.FdRotation.Send(Vector4Single(1, 0, 0, Scene.Time));
    That is, changing field's value by Send automatically calls
    appropriate ChangedField method. It also uses appropriate event,
    in case VRML events are working and this is exposed field.

    Below we show the more tedious way, by manually calling ChangedField,
    as it's sometimes more efficient, as there are many ChangedXxx methods
    and sometimes (when really intensively changing VRML graph) you
    can avoid calling ChangedXxx after every change. E.g. you can
    just call ChangedFields(Node, nil) once after changing many fields
    of given node. }

  TransformBox2.FdRotation.RotationRad := Scene.Time.Seconds;
  Scene.ChangedField(TransformBox2.FdRotation);

  TransformBox3.FdRotation.RotationRad := Scene.Time.Seconds * 2;
  Scene.ChangedField(TransformBox3.FdRotation);

  TransformBox4.FdRotation.RotationRad := Scene.Time.Seconds * 4;
  Scene.ChangedField(TransformBox4.FdRotation);
end;

begin
  Glw := TGLUIWindow.Create(Application);

  Parameters.CheckHigh(0);
  VRMLWarning := @VRMLWarning_Write;

  Scene := TVRMLGLScene.Create(nil);
  try
    Scene.Load('models' + PathDelim + 'boxes.x3dv');
    TransformBox2 := Scene.RootNode.FindNodeByName(TNodeTransform_2,
      'Box2Transform', true) as TNodeTransform_2;
    TransformBox3 := Scene.RootNode.FindNodeByName(TNodeTransform_2,
      'Box3Transform', true) as TNodeTransform_2;
    TransformBox4 := Scene.RootNode.FindNodeByName(TNodeTransform_2,
      'Box4Transform', true) as TNodeTransform_2;

    { init SceneManager with our Scene }
    SceneManager := TKamSceneManager.Create(Glw);
    Glw.Controls.Add(SceneManager);
    SceneManager.MainScene := Scene;
    SceneManager.Items.Add(Scene);

    { init SceneManager.Camera }
    SceneManager.Camera := TExamineCamera.Create(Glw);
    (SceneManager.Camera as TExamineCamera).Init(Scene.BoundingBox);

    Glw.OnIdle := @Idle;
    Glw.InitAndRun;
  finally Scene.Free end;
end.
