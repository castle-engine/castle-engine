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

{ More involved version of animate_3d_model_by_code.lpr:
  constructs 3D model (VRML/X3D graph) by code and then animates it by code
  (showing a little more interesting animation, sin*cos displayed in 3D). }

program animate_3d_model_by_code_2;

uses CastleVectors, X3DNodes, CastleWindow, CastleLog,
  CastleUtils, SysUtils, CastleGLUtils, CastleScene, CastleCameras,
  CastleFilesUtils, CastleQuaternions, CastleParameters,
  CastleStringUtils, CastleKeysMouse, CastleApplicationProperties,
  CastleViewport, CastleTimeUtils;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;

const
  XCount = 15;
  YCount = 15;

var
  Time: TFloatTime;
  Transform: array [0 .. XCount - 1, 0 .. YCount - 1] of TTransformNode;

procedure Update(Container: TCastleContainer);
var
  I, J: Integer;
  T: TVector3;
begin
  Time += Container.Fps.SecondsPassed;

  for I := 0 to XCount - 1 do
    for J := 0 to YCount - 1 do
    begin
      T := Transform[I, J].Translation;
      T[2] := 2 *
        Sin(I / 2 + Time) *
        Cos(J / 2 + Time);
      Transform[I, J].Translation := T;
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
  Mat.DiffuseColor := Vector3(1, 1, 0);

  Shape := TShapeNode.Create;
  Shape.Appearance := TAppearanceNode.Create;
  Shape.Appearance.Material := Mat;
  Shape.Geometry := TBoxNode.Create;

  for I := 0 to XCount - 1 do
    for J := 0 to YCount - 1 do
    begin
      Transform[I, J] := TTransformNode.Create;
      Transform[I, J].Translation := Vector3(I * 2, J * 2, 0);
      Transform[I, J].AddChildren(Shape);

      Result.AddChildren(Transform[I, J]);
    end;
end;

begin
  InitializeLog;
  Application.ParseStandardParameters;
  Parameters.CheckHigh(0); // no command-line options specific to this program are allowed

  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Window.Controls.InsertFront(Viewport);

  { We use a lot of boxes, so make their rendering fastest. }
  DefaultTriangulationDivisions := 0;

  Scene := TCastleScene.Create(Application);
  Scene.Load(CreateVrmlGraph, true);

  { init Viewport with our Scene }
  Viewport.Items.MainScene := Scene;
  Viewport.Items.Add(Scene);

  { init Viewport.Camera }
  Viewport.Camera.SetView(
    Vector3(45.92, 12.68, 18.15), // position
    Vector3(-0.83,  0.09, -0.55), // direction
    Vector3(-0.06,  0.96,  0.26)  // up
  );

  { init Viewport.Navigation }
  Viewport.Navigation := TCastleExamineNavigation.Create(Application);

  Window.OnUpdate := @Update;
  Window.SetDemoOptions(keyF11, CharEscape, true);
  Application.Run;
end.
