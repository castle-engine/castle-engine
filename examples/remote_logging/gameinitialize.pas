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

uses SysUtils, Math, URIParser, Classes,
  CastleWindow, CastleLog, CastleApplicationProperties, CastleDownload,
  CastleKeysMouse;

var
  Window: TCastleWindowBase;
  InsideLogCallback: Boolean;

type
  TEventsHandler = class
    class procedure LogCallback(Sender: TObject; const Message: String);
  end;

class procedure TEventsHandler.LogCallback(Sender: TObject; const Message: String);

  { Escape any special URL characters.
    Following Escape from URIParser. }
  function EscapeUriParameter(const S: String): String;
  const
    ALPHA = ['A'..'Z', 'a'..'z'];
    DIGIT = ['0'..'9'];
    Allowed {Unreserved} = ALPHA + DIGIT + ['-', '.', '_', '~'];
  var
    i, L: Integer;
    P: PChar;
  begin
    L := Length(s);
    for i := 1 to Length(s) do
      if not (s[i] in Allowed) then Inc(L,2);
    if L = Length(s) then
    begin
      Result := s;
      Exit;
    end;

    SetLength(Result, L);
    P := @Result[1];
    for i := 1 to Length(s) do
    begin
      if not (s[i] in Allowed) then
      begin
        P^ := '%'; Inc(P);
        StrFmt(P, '%.2x', [ord(s[i])]); Inc(P);
      end
      else
        P^ := s[i];
      Inc(P);
    end;
  end;

const
  { Some limit is necessary, otherwise server will answer HTTP 414
    ( https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/414 ).
    TODO: Using http POST would solve it. }
  MaxMessageLen = 1024;
var
  S: TStream;
  SendMessage: String;
begin
  { Use InsideLogCallback to prevent from infinite recursion:
    Download call below could also do log calls. }
  if InsideLogCallback then Exit;
  InsideLogCallback := true;
  try
    // calculate SendMessage, apply MaxMessageLen
    if Length(Message) > MaxMessageLen then
      SendMessage := Copy(Message, 1, MaxMessageLen - 3) + '...'
    else
      SendMessage := Message;
    SendMessage := Trim(SendMessage);

    { TODO: This does synchronous waiting, until the Download() call returns,
      which will slow down your application noticeably,
      since each WritelnLog call is now a network request that must be completed. }
    S := Download('http://michalis.ii.uni.wroc.pl/~michalis/cge_logger.php?message=' +
      EscapeUriParameter(SendMessage));
    FreeAndNil(S);
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
  EnableNetwork := true;

  InitializeLog;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Window.OnPress := @WindowPress;
  Application.MainWindow := Window;
end.
