{
  Copyright 2003-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of fog culling. When rendering with fog turned "on" (the default),
  we do not render objects outside of the fog visibility radius.
  This can make a hude speedup if you have dense fog and exterior-like level
  (where frustum culing leaves too many shapes visible).

  This always loads models/fog_culling_final.x3dv X3D file.
  Be sure to run it with proper current directory (examples/3d_rendering_processing/).
  It's a crafted scene, with some green and dense fog and a lot of
  spheres scattered around. Fog culling will work best on it.

  Handles keys:
    'f' turns fog culling on/off
    F5 makes a screenshot
}

program fog_culling;

uses SysUtils, VectorMath, GL, GLU, CastleWindow, CastleStringUtils,
  CastleClassUtils, CastleUtils, Classes, CastleWarnings,
  CastleGLUtils, X3DNodes, CastleSceneCore, CastleScene,
  CastleProgress, ProgressConsole, CastleFilesUtils, Base3D,
  CastleSceneManager, CastleParameters, RenderingCameraUnit, CastleKeysMouse;

var
  Window: TCastleWindowCustom;
  Scene: TCastleScene;

type
  TMySceneManager = class(TCastleSceneManager)
  private
    function TestFogVisibility(Shape: TGLShape): boolean;
  protected
    procedure Render3D(const Params: TRenderParams); override;
    procedure RenderFromViewEverything; override;
  end;

var
  SceneManager: TMySceneManager;

function TMySceneManager.TestFogVisibility(Shape: TGLShape): boolean;
begin
  { Test for collision between two spheres.
    1st is the bounding sphere of Shape.
    2nd is the sphere around current camera position,
      with the radius taken from fog scaled visibilityRadius.
    If there is no collision than we don't have to render given Shape. }
  Result := PointsDistanceSqr(Shape.BoundingSphereCenter, Camera.GetPosition) <=
      Sqr(Scene.FogNode.FdVisibilityRange.Value * Scene.FogNode.TransformScale +
        Sqrt(Shape.BoundingSphereRadiusSqr));
end;

var
  FogCulling: boolean = true;

procedure TMySceneManager.Render3D(const Params: TRenderParams);
begin
  if FogCulling then
    Scene.Render(@TestFogVisibility, RenderingCamera.Frustum, Params) else
    inherited;
end;

procedure TMySceneManager.RenderFromViewEverything;
begin
  inherited;
  Writeln(Format('Rendered Shapes: %d / %d (fog culling: %s)',
    [ Statistics.ShapesRendered, Statistics.ShapesVisible,
      BoolToStr[FogCulling] ]));
end;

procedure Open(Window: TCastleWindowBase);
begin
  { We use quite large triangles for fog_culling level demo wall.
    This means that fog must be correctly rendered,
    with perspective correction hint, otherwise ugly artifacts
    will be visible. }
  glHint(GL_FOG_HINT, GL_NICEST);
end;

procedure Press(Window: TCastleWindowBase; const Event: TInputPressRelease);
var
  FogNode: TFogNode;
begin
  if Event.IsKey(K_F) then
  begin
    FogCulling := not FogCulling;

    { Also, turn on/off actual fog on the model (if any).
      We do it by changing Fog.VisibilityRange (0 means no fog). }
    FogNode := Scene.FogStack.Top as TFogNode;
    if FogNode <> nil then
      if FogCulling then
        FogNode.FdVisibilityRange.Send(30) else
        FogNode.FdVisibilityRange.Send(0);

    Window.PostRedisplay;
  end;
end;

begin
  Parameters.CheckHigh(0);

  Window := TCastleWindowCustom.Create(Application);

  SceneManager := TMySceneManager.Create(Application);
  Window.Controls.Add(SceneManager);

  Scene := TCastleScene.Create(Application);
  OnWarning := @OnWarningWrite;
  Scene.Load('models' + PathDelim + 'fog_culling_final.x3dv');
  SceneManager.MainScene := Scene;
  SceneManager.Items.Add(Scene);

  Writeln(Scene.Info(true, true, false));

  { build octrees }
  Progress.UserInterface := ProgressConsoleInterface;
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building Shape octree';
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Window.OnOpen := @Open;
  Window.OnPress := @Press;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
