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

{ You can enable the scripts inside VRML/X3D 3D model to call a particular
  ObjectPascal function.

  This is one way to enable your 3D models to do something
  that is usually outside the scope (and security) of a normal VRML/X3D browser.
  This way you can enable VRML/X3D models to interact with
  some database, or desktop task, or... well, do anything
  that native ObjectPascal code can do.
  This approach gives the whole control of when the given operation
  is executed to the author of VRML/X3D 3D model. The ObjectPascal code
  only implements and registers methods, that may be then called at any time
  while the VRML/X3D is loaded.

  Inside the VRML/X3D model, this is declared by "compiled:" protocol
  inside a Script node. See data/compiled_script_tests.x3dv.
  See https://castle-engine.io/x3d_extensions.php#section_ext_script_compiled
  for more information. }

program call_pascal_code_from_3d_model_script;

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

uses CastleUtils, CastleProgress, CastleProgressConsole, CastleLog, CastleScene,
  CastleSceneCore, X3DFields, X3DTime, SysUtils, CastleParameters, CastleStringUtils,
  CastleWindow, CastleKeysMouse, CastleApplicationProperties, CastleViewport;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Scene: TCastleScene;

type
  THelperObj = class
    class procedure ScriptTouchInitialize(Value: TX3DField; const Time: TX3DTime);
    class procedure ScriptTouch(Value: TX3DField; const Time: TX3DTime);
  end;

class procedure THelperObj.ScriptTouchInitialize(Value: TX3DField; const Time: TX3DTime);
begin
  Writeln(Format('Script is initialized (absolute time: %f, time since load: %f)', [
    Time.Seconds,
    Time.Seconds - Scene.TimeAtLoad
  ]));
end;

class procedure THelperObj.ScriptTouch(Value: TX3DField; const Time: TX3DTime);
begin
  Writeln(Format('Touch! (absolute time: %f, time since load: %f)', [
    Time.Seconds,
    Time.Seconds - Scene.TimeAtLoad
  ]));
end;

var
  { May also be given on command-line. }
  URL: string = 'data/compiled_script_tests.x3dv';
begin
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    URL := Parameters[1];

  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);
  Progress.UserInterface := ProgressConsoleInterface;

  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Viewport.AutoNavigation := true;
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Load(URL);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  { initialize events procesing }
  Scene.RegisterCompiledScript('touch_initialize',  @THelperObj(nil).ScriptTouchInitialize);
  Scene.RegisterCompiledScript('touch', @THelperObj(nil).ScriptTouch);

  { initialize ProcessEvents *after* attaching ScriptTouchInitialize, to receive it }
  Scene.ProcessEvents := true;

  Window.SetDemoOptions(keyF11, CharEscape, true);
  Application.Run;
end.
