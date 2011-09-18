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

{ simplest_vrml_browser extended to render shadow volumes.

  See
  http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadows
  for documentation how to prepare your model to have shadow volumes.

  Besides setting ShadowVolumesPossible and ShadowVolumes to @true,
  we also initialize window by Window.OpenOptionalMultiSamplingAndStencil.
  The latter allows us to smoothly fallback to rendering without shadows
  on GPUs that don't support stencil buffer (really really old graphic cards). }

program simplest_vrml_browser_with_shadow_volumes;

{$apptype CONSOLE}

uses KambiUtils, GLWindow, GLWindowVRMLBrowser, ProgressUnit, ProgressConsole,
  VRMLScene, KambiWarnings, KambiParameters;

var
  BrowserWindow: TGLWindowVRMLBrowser;

procedure StencilOff(Window: TGLWindow; const FailureMessage: string);
begin
  BrowserWindow.ShadowVolumesPossible := false;
  Writeln('Stencil buffer not available, shadows could not be initialized');
end;

begin
  Parameters.CheckHigh(1);

  OnWarning := @OnWarningWrite;
  Progress.UserInterface := ProgressConsoleInterface;

  BrowserWindow := TGLWindowVRMLBrowser.Create(Application);

  BrowserWindow.ShadowVolumesPossible := true;
  BrowserWindow.ShadowVolumes := true;

  BrowserWindow.Load(Parameters[1]);
  Writeln(BrowserWindow.Scene.Info(true, true, false));
  BrowserWindow.Scene.Spatial := [ssRendering, ssDynamicCollisions];
  BrowserWindow.Scene.ProcessEvents := true;

  BrowserWindow.OpenOptionalMultiSamplingAndStencil(nil, @StencilOff);
  Application.Run;
end.
