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

{ Demo how to load and render 3D model, with some additional tricks.
  This is an extended version of view_3d_model_simple.lpr,
  so look there first. }
program view_3d_model_advanced;

{$apptype CONSOLE}

uses SysUtils, CastleUtils, CastleWindow, ProgressUnit, ProgressConsole,
  CastleSceneCore, CastleWarnings, CastleParameters, CastleScene, X3DLoad,
  CastleControls;

var
  Window: TCastleWindow;
  Scene: TCastleScene;
  OpenButton: TCastleButton;
  FileName: string = 'models' + PathDelim + 'bridge_final.x3dv';

type
  { Dummy class, just to keep OpenButtonClick method. }
  THelper = class
    class procedure OpenButtonClick(Sender: TObject);
  end;

class procedure THelper.OpenButtonClick(Sender: TObject);
var
  S: string;
begin
  { We could now manually reload the Scene and reset the camera.
    But actually TCastleWindow has already a comfortable shortcut
    for reloading scene (inside Window.SceneManager.MainScene,
    which is equal to our Scene) and reloading camera.
    This releases previous SceneManager.MainScene and SceneManager.Camera,
    reloads SceneManager.MainScene from given file and
    creates new camera suitable for new scene. }
  S := ExtractFilePath(FileName);
  if Window.FileDialog('Open 3D model', S, true, LoadVRML_FileFilters) then
  begin
    FileName := S;
    Window.Load(FileName);

    { previous Scene value is destroyed now, use new loaded value }
    Scene := Window.SceneManager.MainScene;

    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;
  end;
end;

procedure StencilOff(Window: TCastleWindowBase; const FailureMessage: string);
begin
  (Window as TCastleWindow).ShadowVolumesPossible := false;
  Writeln('Stencil buffer not available, shadow volumes could not be initialized');
end;

begin
  { You can specify initial 3D model filename by command-line parameter. }
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    FileName := Parameters[1];

  { Output warnings and progress bars on a console. }
  OnWarning := @OnWarningWrite;
  Progress.UserInterface := ProgressConsoleInterface;

  Window := TCastleWindow.Create(Application);

  { Enable rendering models using shadow volumes. This requires some special
    code, as OpenGL must be prepared in a special way to enable it,
    and some GPUs (really ancient ones) may simply not support it.
    See http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows
    for documentation how to prepare your model to have shadow volumes,
    and for links to demo models using shadow volumes.

    Besides setting ShadowVolumesPossible and ShadowVolumes to @true,
    we also initialize window by Window.OpenOptionalMultiSamplingAndStencil.
    The latter allows us to smoothly fallback to rendering without shadows
    on (really really old) GPUs that don't support stencil buffer. }
  Window.ShadowVolumesPossible := true;
  Window.ShadowVolumes := true;

  { load a Scene and add it to Window.SceneManager, just like view_3d_model_simple }
  Scene := TCastleScene.Create(Application);
  Scene.Load(FileName);

  { set titles for progress bars, otherwise progress bars are not used.
    This should be done before setting Scene.Spatial, since setting it may
    already do some progress bars. }
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building shape octree';

  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  { Output some information about the loaded scene }
  Writeln(Scene.Info(true, true, false));

  { add an "Open" button to the window controls }
  OpenButton := TCastleButton.Create(Application);
  OpenButton.Caption := 'Open';
  OpenButton.OnClick := @THelper(nil).OpenButtonClick;
  OpenButton.Left := 10;
  OpenButton.Bottom := 10;
  Window.Controls.Insert(0, OpenButton);

  Window.OpenOptionalMultiSamplingAndStencil(nil, @StencilOff);
  Application.Run;
end.
