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
  See http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_script_compiled
  for more information. }

program call_pascal_code_from_3d_model_script;

{$apptype CONSOLE}

uses CastleUtils, CastleProgress, CastleProgressConsole, CastleLog,
  CastleSceneCore, X3DFields, X3DTime, SysUtils, CastleParameters, CastleStringUtils,
  CastleWindow, CastleKeysMouse, CastleApplicationProperties;

var
  Window: TCastleWindow;

type
  THelperObj = class
    class procedure ScriptTouchInitialize(Value: TX3DField; const Time: TX3DTime);
    class procedure ScriptTouch(Value: TX3DField; const Time: TX3DTime);
  end;

class procedure THelperObj.ScriptTouchInitialize(Value: TX3DField; const Time: TX3DTime);
begin
  Writeln(Format('Script is initialized (timestamp: %f)', [Time.Seconds]));
end;

class procedure THelperObj.ScriptTouch(Value: TX3DField; const Time: TX3DTime);
begin
  Writeln(Format('Touch! (timestamp: %f)', [Time.Seconds]));
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

  Window := TCastleWindow.Create(nil);
  try
    Window.Load(URL);

    { initialize events procesing }
    Window.MainScene.RegisterCompiledScript('touch_initialize',
      @THelperObj(nil).ScriptTouchInitialize);
    Window.MainScene.RegisterCompiledScript('touch',
      @THelperObj(nil).ScriptTouch);

    Window.MainScene.Spatial := [ssRendering, ssDynamicCollisions];
    Window.MainScene.ProcessEvents := true;

    Window.SetDemoOptions(K_F11, CharEscape, true);
    Window.OpenAndRun;
  finally Window.Free end;
end.
