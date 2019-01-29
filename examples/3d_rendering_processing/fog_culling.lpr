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
  and you have a background that has the same color as the fog.

  This loads data/fog_culling_final.x3dv X3D file.

  Press [F] key to turn DistanceCulling (and fog display) on/off.
}

program fog_culling;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, CastleVectors, CastleWindow, CastleStringUtils,
  CastleClassUtils, CastleUtils, Classes, CastleLog,
  CastleGLUtils, X3DNodes, CastleSceneCore, CastleScene, CastleShapes,
  CastleProgress, CastleProgressConsole, CastleFilesUtils,
  CastleSceneManager, CastleParameters, CastleKeysMouse,
  CastleApplicationProperties, CastleControls, CastleColors;

var
  Window: TCastleWindow;
  Scene: TCastleScene;
  FogCulling: boolean;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
var
  FogNode: TFogNode;
begin
  if Event.IsKey(K_F) then
  begin
    FogCulling := not FogCulling;
    FogNode := Scene.FogStack.Top;
    if FogCulling then
    begin
      FogNode.VisibilityRange := 30;
      Scene.DistanceCulling := FogNode.VisibilityRange * FogNode.TransformScale
    end else
    begin
      // setting VisibilityRange to 0 turns off fog display
      FogNode.VisibilityRange := 0;
      Scene.DistanceCulling := 0;
    end;
  end;
end;

procedure Render(Container: TUIContainer);
begin
  UIFont.Outline := 1;
  UIFont.OutlineColor := Black;
  UIFont.PrintStrings(10, 10, Yellow,
    [ Format('Rendered Shapes: %d / %d',
       [Window.SceneManager.Statistics.ShapesRendered,
        Window.SceneManager.Statistics.ShapesVisible]),
      Format('Fog culling: %s (toggle using the "F" key)',
       [BoolToStr(FogCulling, true)])
    ], false, 0);
end;

begin
  Parameters.CheckHigh(0);

  Window := TCastleWindow.Create(Application);

  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('fog_culling_final.x3dv'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.SceneManager.MainScene := Scene;
  Window.SceneManager.Items.Add(Scene);

  // fake pressing "F" to turn on FogCulling correctly
  Press(nil, InputKey(TVector2.Zero, K_F, ''));

  Window.OnPress := @Press;
  Window.OnRender := @Render;
  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
