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

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, CastleVectors, CastleWindow, CastleStringUtils,
  CastleClassUtils, CastleUtils, Classes, CastleLog,
  CastleGLUtils, X3DNodes, CastleSceneCore, CastleScene, CastleShapes,
  CastleProgress, CastleProgressConsole, CastleFilesUtils,
  CastleSceneManager, CastleParameters, CastleRenderingCamera, CastleKeysMouse,
  CastleApplicationProperties, CastleControls, CastleColors;

var
  Window: TCastleWindowCustom;
  Scene: TCastleScene;

type
  TMySceneManager = class(TCastleSceneManager)
  private
    function TestFogVisibility(Shape: TShape): boolean;
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
      Sqr(Scene.FogStack.Top.VisibilityRange *
          Scene.FogStack.Top.TransformScale +
        Sqrt(Shape.BoundingSphereRadiusSqr));
end;

var
  FogCulling: boolean;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
var
  FogNode: TFogNode;
begin
  if Event.IsKey(K_F) then
  begin
    FogCulling := not FogCulling;

    if FogCulling then
      Scene.InternalVisibilityTest := @SceneManager.TestFogVisibility
    else
      Scene.InternalVisibilityTest := nil;

    { Also, turn on/off actual fog on the model (if any).
      We do it by changing Fog.VisibilityRange (0 means no fog). }
    FogNode := Scene.FogStack.Top as TFogNode;
    if FogNode <> nil then
      if FogCulling then
        FogNode.VisibilityRange := 30
      else
        FogNode.VisibilityRange := 0;

    Window.Invalidate;
  end;
end;

procedure Render(Container: TUIContainer);
begin
  UIFont.Outline := 1;
  UIFont.OutlineColor := Black;
  UIFont.PrintStrings(10, 10, Yellow,
    [ Format('Rendered Shapes: %d / %d',
       [SceneManager.Statistics.ShapesRendered,
        SceneManager.Statistics.ShapesVisible]),
      Format('Fog culling: %s (toggle using the "F" key)',
       [BoolToStr(FogCulling, true)])
    ], false, 0);
end;

begin
  Parameters.CheckHigh(0);

  Window := TCastleWindowCustom.Create(Application);

  SceneManager := TMySceneManager.Create(Application);
  Window.Controls.InsertFront(SceneManager);

  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('fog_culling_final.x3dv'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  SceneManager.MainScene := Scene;
  SceneManager.Items.Add(Scene);

  // fake pressing "F" to turn on FogCulling correctly
  Press(nil, InputKey(TVector2.Zero, K_F, #0));

  Window.OnPress := @Press;
  Window.OnRender := @Render;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
