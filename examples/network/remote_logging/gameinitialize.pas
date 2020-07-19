{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils, Math, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif} // support HTTPS
  CastleWindow, CastleLog, CastleApplicationProperties, CastleKeysMouse,
  GameLogHandler;

var
  Window: TCastleWindowBase;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  WritelnLog('Pressed: ' + Event.ToString);
end;

var
  LogHandler: TLogHandler;
initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'remote_logging';

  LogHandler := TLogHandler.Create(Application);
  ApplicationProperties.OnLog.Add(@LogHandler.LogCallback);

  InitializeLog;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Window.OnPress := @WindowPress;
  Application.MainWindow := Window;
end.
