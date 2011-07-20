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

{ Somewhat more involved version of change_vrml_by_code.lpr,
  this constructs VRML graph by code and then animates it by code
  (showing a little more interesting animation, sin*cos displayed in 3D). }

{ $define LOG}

program change_vrml_by_code_2;

uses VectorMath, VRMLNodes, GL, GLU, GLWindow,
  KambiUtils, SysUtils, KambiGLUtils, VRMLGLScene, Cameras, KambiSceneManager,
  KambiFilesUtils, VRMLErrors, Quaternions {$ifdef LOG} ,KambiLog {$endif};

var
  Window: TGLUIWindow;
  SceneManager: TKamSceneManager;
  Scene: TVRMLGLScene;

const
  XCount = 15;
  YCount = 15;

var
  Transform: array [0 .. XCount - 1, 0 .. YCount - 1] of TNodeTransform;

procedure Idle(Window: TGLWindow);
var
  I, J: Integer;
begin
  { We want to keep track of current time here (for calculating
    below). It's most natural to just use Scene.Time property for this.
    (Scene.Time is already incremented for us by SceneManager.) }

  for I := 0 to XCount - 1 do
    for J := 0 to YCount - 1 do
    begin
      Transform[I, J].FdTranslation.Value[2] := 2 *
        Sin(I / 2 + Scene.Time.Seconds) *
        Cos(J / 2 + Scene.Time.Seconds);
      Scene.ChangedField(Transform[I, J].FdTranslation);
    end;

  { Sometimes, for really large XCount * YCount, you may want to
    1. call ChangedAll once here and
    2. remove calls to ChangedField in the loop above.

    The single ChangedAll call may be faster than many ChangedField calls
    for *really* large XCount * YCount. That's because ChangedField for
    Transform fields tries to do intelligent analysis of what was changed
    by this transform. }
  { Scene.ChangedAll; }
end;

function CreateVrmlGraph: TNodeGroup;
var
  Shape: TNodeShape;
  Mat: TNodeMaterial;
  I, J: Integer;
begin
  Result := TNodeGroup.Create('', '');

  Mat := TNodeMaterial.Create('', '');
  Mat.FdDiffuseColor.Value := Vector3Single(1, 1, 0);

  Shape := TNodeShape.Create('', '');
  Shape.FdAppearance.Value := TNodeAppearance.Create('', '');
  Shape.Appearance.FdMaterial.Value := Mat;
  Shape.FdGeometry.Value := TNodeBox.Create('', '');

  for I := 0 to XCount - 1 do
    for J := 0 to YCount - 1 do
    begin
      Transform[I, J] := TNodeTransform.Create('', '');
      Transform[I, J].FdTranslation.Value := Vector3Single(I * 2, J * 2, 0);
      Transform[I, J].FdChildren.Add(Shape);

      Result.FdChildren.Add(Transform[I, J]);
    end;
end;

begin
  Window := TGLUIWindow.Create(Application);

  Parameters.CheckHigh(0);
  VRMLWarning := @VRMLWarning_Write;

  { We use a lot of boxes, so make their rendering fastest. }
  Detail_RectDivisions := 0;

  Scene := TVRMLGLScene.Create(nil);
  try
    Scene.Load(CreateVrmlGraph, true);

    {$ifdef LOG}
    InitializeLog('1.0');
    Scene.LogChanges := true;
    {$endif}

    { make SceneManager with our Scene }
    SceneManager := TKamSceneManager.Create(Window);
    Window.Controls.Add(SceneManager);
    SceneManager.MainScene := Scene;
    SceneManager.Items.Add(Scene);

    { init SceneManager.Camera }
    SceneManager.Camera := TExamineCamera.Create(Window);
    (SceneManager.Camera as TExamineCamera).Init(Scene.BoundingBox, 0.1);
    { set more interesting view by default }
    (SceneManager.Camera as TExamineCamera).Rotations := QuatFromAxisAngle(
      Normalized(Vector3Single(1, 1, 0)), Pi/4);

    Window.OnIdle := @Idle;
    Window.OpenAndRun;
  finally Scene.Free end;
end.
