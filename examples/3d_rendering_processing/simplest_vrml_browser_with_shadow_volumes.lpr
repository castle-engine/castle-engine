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

{ simplest_vrml_browser extended to render shadow volumes.

  See
  http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows
  for documentation how to prepare your model to have shadow volumes.

  Besides setting ShadowVolumesPossible and ShadowVolumes to @true,
  we also initialize window by Window.OpenOptionalMultiSamplingAndStencil.
  The latter allows us to smoothly fallback to rendering without shadows
  on GPUs that don't support stencil buffer (really really old graphic cards). }

program simplest_vrml_browser_with_shadow_volumes;

{$apptype CONSOLE}

uses CastleUtils, CastleWindow, ProgressUnit, ProgressConsole,
  CastleSceneCore, CastleWarnings, CastleParameters;

var
  Window: TCastleWindow;

procedure StencilOff(Window: TCastleWindowBase; const FailureMessage: string);
begin
  (Window as TCastleWindow).ShadowVolumesPossible := false;
  Writeln('Stencil buffer not available, shadows could not be initialized');
end;

begin
  Parameters.CheckHigh(1);

  OnWarning := @OnWarningWrite;
  Progress.UserInterface := ProgressConsoleInterface;

  Window := TCastleWindow.Create(Application);

  Window.ShadowVolumesPossible := true;
  Window.ShadowVolumes := true;

  Window.Load(Parameters[1]);
  Writeln(Window.MainScene.Info(true, true, false));
  Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
  Window.MainScene.ProcessEvents := true;

  Window.OpenOptionalMultiSamplingAndStencil(nil, @StencilOff);
  Application.Run;
end.
