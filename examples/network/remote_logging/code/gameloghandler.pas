{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Log handler sending logs to server using asynchronous HTTP POST (TLogHandler). }
unit GameLogHandler;

interface

uses SysUtils, Math, Classes,
  CastleWindow, CastleLog, CastleApplicationProperties, CastleKeysMouse,
  CastleDownload, CastleClassUtils, CastleNotifications;

type
  { Log handler sending logs to server using asynchronous HTTP POST. }
  TLogHandler = class(TComponent)
  strict private
    InsideLogCallback: Boolean;
    ProcessId: Cardinal;
    procedure HttpPostFinish(const Sender: TCastleDownload; var FreeSender: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LogCallback(const Message: String);
  end;

var
  { Set by UI state. }
  LogNotifications: TCastleNotifications;

implementation

uses CastleUtils, CastleStringUtils;

constructor TLogHandler.Create(AOwner: TComponent);
begin
  inherited;
  { This "process id" is not used for any OS process management.
    It's only a unique process id, hopefully unique across all current
    application instances on all systems.
    So we can just choose it using Random, no need to use Unix "pid" or
    equivalent WinAPI function for this. }
  ProcessId := Random(1000);
end;

procedure TLogHandler.LogCallback(const Message: String);

  { Send, using HTTP post, one parameter. }
  procedure HttpPost(const Url: String; const ParameterKey, ParameterValue: String);
  var
    Request: TCastleDownload;
  begin
    Request := TCastleDownload.Create(Application);
    Request.Url := Url;
    Request.HttpPostData.Values[ParameterKey] := ParameterValue;
    Request.HttpMethod := hmPost;
    Request.OnFinish := {$ifdef FPC}@{$endif} HttpPostFinish;
    Request.Start;
  end;

var
  SendMessage: String;
begin
  { Use InsideLogCallback to prevent from infinite recursion,
    in case anything inside would also cause WritelnLog. }
  if InsideLogCallback then Exit;

  { Do not send messages about network communication, as they would cause infinite recursion too. }
  if IsPrefix('Network:', Message) then
    Exit;

  InsideLogCallback := true;
  try
    try
      // We use TrimRight to strip traling newline
      SendMessage := ApplicationName + '[' + IntToStr(ProcessId) + '] ' + TrimRight(Message);
      HttpPost('https://castle-engine.io/cge_logger.php', 'message', SendMessage);
    except
      { Catch exception, like
          EInOutError: Could not initialize OpenSSL library
        (easily possible on Linux with FPC < 3.3.1 due to libssl incompatibility).
        Display it as nice warning, instead of crashing application. }
      on E: Exception do
        WritelnWarning('Sending log remotely failed: ' + ExceptMessage(E));
    end;
  finally InsideLogCallback := false end;
end;

procedure TLogHandler.HttpPostFinish(const Sender: TCastleDownload; var FreeSender: Boolean);
begin
  { TCastleDownload does not guarantee that TCastleDownload.Contents
    are set (not nil) when Status is dsError.
    It depends on the protocol.
    But we know that we use this only with http / https protocols, in which
    case TCastleDownload.Contents are always set when finished, even on error. }
  Assert(Sender.Contents <> nil);

  if LogNotifications <> nil then
  begin
    case Sender.Status of
      dsSuccess:
        LogNotifications.Show(Format('SUCCESS: Posted log to "%s".' + NL +
          'Server response (%d): %s', [
          Sender.Url,
          Sender.HttpResponseCode,
          StreamToString(Sender.Contents)
        ]));
      dsError:
        LogNotifications.Show(Format('ERROR: Cannot post log to "%s".' + NL +
          'Error message: %s' + NL +
          'Server response (%d): %s', [
          Sender.Url,
          Sender.ErrorMessage,
          Sender.HttpResponseCode,
          StreamToString(Sender.Contents)
        ]));
      else
        raise EInternalError.Create('No other status is possible when request finished');
    end;
  end;
  FreeSender := true;
end;

end.
