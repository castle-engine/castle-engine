{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{$endif}

{$ifdef read_implementation}

{ Handle http, https URLs using Android service. }

{$ifdef ANDROID}

type
  TAndroidDownloadService = class
    Finished, FinishedSuccess: boolean;
    TemporaryFileName, ErrorMessage: string;
    function HandleDownloadMessages(const Received: TCastleStringList): boolean;
    function Wait(const URL: string): TStream;
  end;

function TAndroidDownloadService.HandleDownloadMessages(const Received: TCastleStringList): boolean;
begin
  if (Received.Count = 2) and (Received[0] = 'download-error') then
  begin
    Finished := true;
    FinishedSuccess := false;
    ErrorMessage := Received[1];
    Result := true;
  end;
  if (Received.Count = 2) and (Received[0] = 'download-finished') then
  begin
    Finished := true;
    FinishedSuccess := true;
    TemporaryFileName := Received[1];
    Result := true;
  end;
end;

function TAndroidDownloadService.Wait(const URL: string): TStream;
begin
  Finished := false;
  Messaging.OnReceive.Add({$ifdef CASTLE_OBJFPC}@{$endif} HandleDownloadMessages);
  Messaging.Send(['download-url', URL]);
  try
    { TODO:
      - This waiting should not be necessary anymore, with TCastleDownload.
      - TCastleDownload should report it's progress by updating DownloadedBytes, TotalBytes
    }
    repeat
      ApplicationProperties._Update; // process CastleMessages
      Sleep(200);
    until Finished;
  finally
    Messaging.OnReceive.Remove({$ifdef CASTLE_OBJFPC}@{$endif} HandleDownloadMessages);
  end;
  if FinishedSuccess then
    Result := TFileStream.Create(TemporaryFileName, fmOpenRead)
  else
    raise Exception.Create(ErrorMessage);
end;

{ TUrlHttpAndroid ------------------------------------------------------------ }

type
  TUrlHttpAndroid = class
    class function Read(const Url: string; out MimeType: string): TStream;
  end;

class function TUrlHttpAndroid.Read(const Url: string; out MimeType: string): TStream;
var
  DownloadService: TAndroidDownloadService;
begin
  if not EnableNetwork then
    raise EDownloadError.Create('Downloading network resources (from "http" or "https" protocols) is not enabled');

  CheckFileAccessSafe(URL);
  WritelnLog('Network', 'Download service started for "%s"', [URIDisplay(URL)]);
  DownloadService := TAndroidDownloadService.Create;
  try
    Result := DownloadService.Wait(URL);
    MimeType := URIMimeType(URL);
  finally
    FreeAndNil(DownloadService);
  end;
end;

{$endif ANDROID}

{$endif}
