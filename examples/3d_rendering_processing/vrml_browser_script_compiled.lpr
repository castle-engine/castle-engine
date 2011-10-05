{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A simple extension of simplest_vrml_browser.lpr,
  registers handles for compiled: Script protocol. }

program vrml_browser_script_compiled;

{$apptype CONSOLE}

uses CastleUtils, ProgressUnit, ProgressConsole, CastleWarnings,
  CastleSceneCore, X3DFields, X3DTime, SysUtils, CastleParameters, CastleStringUtils,
  CastleWindow;

var
  Window: TCastleWindow;

type
  THelperObj = class
    class procedure ScriptTouchInitialize(Value: TVRMLField; const Time: TX3DTime);
    class procedure ScriptTouch(Value: TVRMLField; const Time: TX3DTime);
  end;

{ THelperObj.Script* methods below are only to demonstrate using
  "compiled:" Script protocol, see
  [http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_script_compiled]. }
class procedure THelperObj.ScriptTouchInitialize(Value: TVRMLField; const Time: TX3DTime);
begin
  Writeln(Format('Script is initialized (timestamp: %f)', [Time.Seconds]));
end;

class procedure THelperObj.ScriptTouch(Value: TVRMLField; const Time: TX3DTime);
begin
  Writeln(Format('Touch! (timestamp: %f)', [Time.Seconds]));
end;

var
  { May also be given on command-line. }
  FileName: string = 'models' + PathDelim + 'compiled_script_tests.x3dv';
begin
  Parameters.CheckHighAtMost(1);
  if Parameters.High = 1 then
    FileName := Parameters[1];

  OnWarning := @OnWarningWrite;
  Progress.UserInterface := ProgressConsoleInterface;

  Window := TCastleWindow.Create(nil);
  try
    Window.Load(FileName);

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
