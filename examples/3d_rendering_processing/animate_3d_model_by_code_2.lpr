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

{ More involved version of animate_3d_model_by_code.lpr:
  constructs 3D model (VRML/X3D graph) by code and then animates it by code
  (showing a little more interesting animation, sin*cos displayed in 3D). }

{ $define LOG}

program animate_3d_model_by_code_2;

uses CastleVectors, X3DNodes, CastleWindow, CastleLog,
  CastleUtils, SysUtils, CastleGLUtils, CastleScene, CastleCameras,
  CastleFilesUtils, CastleQuaternions {$ifdef LOG} ,CastleLog {$endif}, CastleParameters,
  CastleStringUtils, CastleKeysMouse, CastleApplicationProperties;

var
  Window: TCastleWindow;
  Scene: TCastleScene;

const
  XCount = 15;
  YCount = 15;

var
  Transform: array [0 .. XCount - 1, 0 .. YCount - 1] of TTransformNode;

procedure Update(Container: TUIContainer);
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
        Sin(I / 2 + Scene.Time) *
        Cos(J / 2 + Scene.Time);
      Transform[I, J].FdTranslation.Changed;
    end;
end;

function CreateVrmlGraph: TX3DRootNode;
var
  Shape: TShapeNode;
  Mat: TMaterialNode;
  I, J: Integer;
begin
  Result := TX3DRootNode.Create;

  Mat := TMaterialNode.Create;
  Mat.FdDiffuseColor.Value := Vector3Single(1, 1, 0);

  Shape := TShapeNode.Create;
  Shape.FdAppearance.Value := TAppearanceNode.Create;
  Shape.Appearance.FdMaterial.Value := Mat;
  Shape.FdGeometry.Value := TBoxNode.Create;

  for I := 0 to XCount - 1 do
    for J := 0 to YCount - 1 do
    begin
      Transform[I, J] := TTransformNode.Create;
      Transform[I, J].FdTranslation.Value := Vector3Single(I * 2, J * 2, 0);
      Transform[I, J].FdChildren.Add(Shape);

      Result.FdChildren.Add(Transform[I, J]);
    end;
end;

begin
  Window := TCastleWindow.Create(Application);

  Parameters.CheckHigh(0);
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  { We use a lot of boxes, so make their rendering fastest. }
  DefaultTriangulationDivisions := 0;

  Scene := TCastleScene.Create(nil);
  try
    Scene.Load(CreateVrmlGraph, true);

    {$ifdef LOG}
    InitializeLog('1.0');
    Scene.LogChanges := true;
    {$endif}

    { add Scene to SceneManager }
    Window.SceneManager.MainScene := Scene;
    Window.SceneManager.Items.Add(Scene);

    { init SceneManager.Camera }
    Window.SceneManager.Camera := TExamineCamera.Create(Window);
    (Window.SceneManager.Camera as TExamineCamera).Init(Scene.BoundingBox, 0.1);
    { set more interesting view by default }
    (Window.SceneManager.Camera as TExamineCamera).Rotations := QuatFromAxisAngle(
      Normalized(Vector3Single(1, 1, 0)), Pi/4);

    Window.OnUpdate := @Update;
    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.OpenAndRun;
  finally Scene.Free end;
end.
