{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of TCastleScene.DistanceCulling, especially useful when you have fog.
  We can avoid rendering objects that are too far to be visible (covered by fog).
  It's a useful optimization when you have a dense fog,
  and a background that has the same color as the fog.

  This loads data/fog_culling_final.x3dv X3D file.

  Press [Ctrl + F] to toggle DistanceCulling (and fog display) on/off.

  Press [Ctrl + C] to toggle per-shape frustum culling on/off,
  ths also makes a difference in this demo.
}

program fog_culling;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, CastleVectors, CastleWindow, CastleStringUtils,
  CastleClassUtils, CastleUtils, Classes, CastleLog,
  CastleGLUtils, X3DNodes, CastleSceneCore, CastleScene, CastleShapes,
  CastleProgress, CastleProgressConsole, CastleFilesUtils,
  CastleViewport, CastleParameters, CastleKeysMouse,
  CastleApplicationProperties, CastleControls, CastleColors;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
  FogCulling: boolean;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
var
  FogNode: TFogNode;
begin
  if Event.IsKey(CtrlF) then
  begin
    FogCulling := not FogCulling;
    FogNode := Scene.FogStack.Top;
    if FogCulling then
    begin
      FogNode.VisibilityRange := 30;
      Scene.DistanceCulling := FogNode.VisibilityRange * FogNode.TransformScale;
    end else
    begin
      // setting VisibilityRange to 0 turns off fog display
      FogNode.VisibilityRange := 0;
      Scene.DistanceCulling := 0;
    end;
  end;

  if Event.IsKey(CtrlC) then
    Scene.ShapeFrustumCulling := not Scene.ShapeFrustumCulling;
end;

procedure Render(Container: TUIContainer);
begin
  UIFont.Outline := 1;
  UIFont.OutlineColor := Black;
  UIFont.PrintStrings(10, 10, Yellow,
    [ Format('Rendered Shapes: %d / %d', [
        Viewport.Statistics.ShapesRendered,
        Viewport.Statistics.ShapesVisible
      ]),
      Format('Fog culling: %s (toggle by Ctrl+F)', [
        BoolToStr(FogCulling, true)
      ]),
      Format('Frustum culling of each shape: %s (toggle by Ctrl+C)', [
        BoolToStr(Scene.ShapeFrustumCulling, true)
      ])
    ], false, 0);
end;

begin
  Parameters.CheckHigh(0);

  Window := TCastleWindowBase.Create(Application);

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/fog_culling_final.x3dv');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  // fake pressing "F" to turn on FogCulling correctly
  Press(nil, InputKey(TVector2.Zero, keyNone, CtrlF));

  Window.OnPress := @Press;
  Window.OnRender := @Render;
  Window.SetDemoOptions(keyF11, CharEscape, true);
  Window.OpenAndRun;
end.
