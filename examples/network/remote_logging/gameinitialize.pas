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

uses SysUtils, Math, URIParser, Classes, FpHttpClient,
  CastleWindow, CastleLog, CastleApplicationProperties, CastleKeysMouse;

var
  Window: TCastleWindowBase;
  InsideLogCallback: Boolean;
  ProcessId: Cardinal;

type
  TEventsHandler = class
    class procedure LogCallback(const Message: String);
  end;

class procedure TEventsHandler.LogCallback(const Message: String);

  { Send, using HTTP post, one parameter. }
  procedure HttpPost(const URL: String; const ParameterKey, ParameterValue: String);
  var
    HttpClient: TFpHttpClient;
    FormData: TStringList;
    Response: String;
  begin
    HttpClient := TFpHttpClient.Create(nil);
    try
      FormData := TStringList.Create;
      try
        FormData.Values[ParameterKey] := ParameterValue;

        { TODO: This waits until the HTTP POST returns,
          which may slow down your application noticeably,
          since each WritelnLog call is now a network request that must be completed. }

        Response := HttpClient.FormPost(URL, FormData);
        Writeln(ErrOutput, Format('Posted log to "%s" with response: %s', [
          URL,
          Response
        ]));
      finally FreeAndNil(FormData) end;
    finally FreeAndNil(HttpClient) end;
  end;

var
  SendMessage: String;
begin
  { Use InsideLogCallback to prevent from infinite recursion:
    Download call below could also do log calls. }
  if InsideLogCallback then Exit;
  InsideLogCallback := true;
  try
    // We use TrimRight to strip traling newline
    SendMessage := ApplicationName + '[' + IntToStr(ProcessId) + '] ' + TrimRight(Message);
    HttpPost('http://example.com/cge_logger.php', 'message', SendMessage);
  finally InsideLogCallback := false end;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  WritelnLog('Pressed: ' + Event.ToString);
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'remote_logging';
  ApplicationProperties.OnLog.Add(@TEventsHandler(nil).LogCallback);

  { This "process id" is not used for any OS process management.
    It's only a unique process id, hopefully unique across all current
    application instances on all systems.
    So we can just choose it using Random, no need to use Unix "pid" or
    equivalent WinAPI function for this. }
  ProcessId := Random(1000);

  InitializeLog;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Window.OnPress := @WindowPress;
  Application.MainWindow := Window;
end.
