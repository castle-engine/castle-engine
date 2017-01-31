{
  Copyright 2003-2017 Michalis Kamburelis.

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

  This always loads data/fog_culling_final.x3dv X3D file.
  Be sure to run it with proper current directory (examples/3d_rendering_processing/).
  It's a crafted scene, with some green and dense fog and a lot of
  spheres scattered around. Fog culling will work best on it.

  Handles keys:
    'f' turns fog culling on/off
    F5 makes a screenshot
}

program fog_culling;

uses SysUtils, CastleVectors, CastleWindow, CastleStringUtils,
  CastleClassUtils, CastleUtils, Classes, CastleLog,
  CastleGLUtils, X3DNodes, CastleSceneCore, CastleScene, CastleShapes,
  CastleProgress, CastleProgressConsole, CastleFilesUtils, Castle3D,
  CastleSceneManager, CastleParameters, CastleRenderingCamera, CastleKeysMouse,
  CastleApplicationProperties;

var
  Window: TCastleWindowCustom;
  Scene: TCastleScene;

type
  TMySceneManager = class(TCastleSceneManager)
  private
    function TestFogVisibility(Shape: TShape): boolean;
  protected
    procedure Render3D(const Params: TRenderParams); override;
    procedure RenderFromViewEverything; override;
  end;

var
  SceneManager: TMySceneManager;

function TMySceneManager.TestFogVisibility(Shape: TShape): boolean;
begin
  { Test for collision between two spheres.
    1st is the bounding sphere of Shape.
    2nd is the sphere around current camera position,
      with the radius taken from fog scaled visibilityRadius.
    If there is no collision than we don't have to render given Shape. }
  Result := PointsDistanceSqr(Shape.BoundingSphereCenter, Camera.Position) <=
      Sqr(Scene.FogStack.Top.FdVisibilityRange.Value * 
          Scene.FogStack.Top.TransformScale +
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
      BoolToStr(FogCulling, true) ]));
end;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
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

    Window.Invalidate;
  end;
end;

begin
  Parameters.CheckHigh(0);

  Window := TCastleWindowCustom.Create(Application);

  SceneManager := TMySceneManager.Create(Application);
  Window.Controls.InsertFront(SceneManager);

  Scene := TCastleScene.Create(Application);
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);
  Scene.Load(ApplicationData('fog_culling_final.x3dv'));
  SceneManager.MainScene := Scene;
  SceneManager.Items.Add(Scene);

  { build octrees }
  Progress.UserInterface := ProgressConsoleInterface;
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building Shape octree';
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Window.OnPress := @Press;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
