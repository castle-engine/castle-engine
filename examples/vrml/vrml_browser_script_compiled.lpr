{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A simple extension of simplest_vrml_browser.lpr,
  registers handles for compiled: Script protocol. }

program vrml_browser_script_compiled;

{$apptype CONSOLE}

uses KambiUtils, GLWindowVRMLBrowser, ProgressUnit, ProgressConsole, KambiWarnings,
  VRMLScene, VRMLFields, VRMLTime, SysUtils, KambiParameters;

var
  BrowserWindow: TGLWindowVRMLBrowser;

type
  THelperObj = class
    class procedure ScriptTouchInitialize(Value: TVRMLField; const Time: TVRMLTime);
    class procedure ScriptTouch(Value: TVRMLField; const Time: TVRMLTime);
  end;

{ THelperObj.Script* methods below are only to demonstrate using
  "compiled:" Script protocol, see
  [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_script_compiled]. }
class procedure THelperObj.ScriptTouchInitialize(Value: TVRMLField; const Time: TVRMLTime);
begin
  Writeln(Format('Script is initialized (timestamp: %f)', [Time.Seconds]));
end;

class procedure THelperObj.ScriptTouch(Value: TVRMLField; const Time: TVRMLTime);
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

  BrowserWindow := TGLWindowVRMLBrowser.Create(nil);
  try
    BrowserWindow.Load(FileName);

    { initialize events procesing }
    BrowserWindow.Scene.RegisterCompiledScript('touch_initialize',
      @THelperObj(nil).ScriptTouchInitialize);
    BrowserWindow.Scene.RegisterCompiledScript('touch',
      @THelperObj(nil).ScriptTouch);

    BrowserWindow.Scene.Spatial := [ssRendering, ssDynamicCollisions];
    BrowserWindow.Scene.ProcessEvents := true;

    BrowserWindow.OpenAndRun;
  finally BrowserWindow.Free end;
end.
