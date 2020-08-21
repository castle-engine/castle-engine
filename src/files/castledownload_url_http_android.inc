{%MainUnit castledownload.pas}
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

{ Handle http, https URLs using Android service. }

{$ifdef read_implementation}

{$ifdef ANDROID}

{ TUrlHttpAndroid ------------------------------------------------------------ }

type
  { TUrlAsynchronousReader descendant that implements http(s) downloading on Android. }
  THttpAndroidReader = class(TUrlAsynchronousReader)
  strict private
    class var
      { Every THttpAndroidReader instance uses a different id, to avoid mixing messages
        (and temporary files) when multiple downloads are running. }
      NextDownloadId: Integer;
    var
      DownloadId: Integer;
      WaitingForPermission: Boolean;
    function HandleDownloadMessages(const Received: TCastleStringList;
      const ReceivedStream: TMemoryStream): boolean;
  public
    procedure Start; override;
    destructor Destroy; override;
  end;

procedure THttpAndroidReader.Start;
begin
  inherited;

  DownloadId := NextDownloadId;
  Inc(NextDownloadId);

  CheckFileAccessSafe(Url);
  WritelnLog('Network', 'Download service started for "%s", id %d', [
    URIDisplay(Url),
    DownloadId
  ]);

  MimeType := URIMimeType(Url);

  Messaging.OnReceive.Add({$ifdef CASTLE_OBJFPC}@{$endif} HandleDownloadMessages);

  // first we need to know we have permission to read temporary files
  WaitingForPermission := true;
  Messaging.Send(['permission-request', 'android.permission.WRITE_EXTERNAL_STORAGE']);
end;

function THttpAndroidReader.HandleDownloadMessages(const Received: TCastleStringList;
  const ReceivedStream: TMemoryStream): boolean;
begin
  if WaitingForPermission then
  begin
    if (Received.Count = 2) and
       (Received[0] = 'permission-granted') and
       (Received[1] = 'android.permission.WRITE_EXTERNAL_STORAGE') then
    begin
      WaitingForPermission := false;
      Messaging.Send(['download-url', IntToStr(DownloadId), Url]);
      Contents := TMemoryStream.Create;
      Result := true;
    end;

    if (Received.Count = 2) and
       (Received[0] = 'permission-cancelled') and
       (Received[1] = 'android.permission.WRITE_EXTERNAL_STORAGE') then
    begin
      WaitingForPermission := false;
      Status := dsError;
      ErrorMessage := 'User denied necessary permissions to download URLs';
      Result := true;
    end;
  end else
  begin
    if (Received.Count = 3) and
       (Received[0] = 'download-error') and
       (Received[1] = IntToStr(DownloadId)) then
    begin
      Status := dsError;
      ErrorMessage := Received[2];
      Result := true;
    end;

    if (Received.Count = 2) and
       (Received[0] = 'download-progress') and
       (Received[1] = IntToStr(DownloadId)) then
    begin
      // append ReceivedStream to Contents
      ReceivedStream.SaveToStream(Contents);
      DownloadedBytes := Contents.Size;
      Result := true;
    end;

    if (Received.Count = 2) and
       (Received[0] = 'download-success') and
       (Received[1] = IntToStr(DownloadId)) then
    begin
      Status := dsSuccess;
      HttpResponseCode := 200;
      DownloadedBytes := Contents.Size;
      TotalBytes := Contents.Size;
      Result := true;
    end;
  end;

  { TODO:
    - report TotalBytes
    - handle
      HttpResponseCode,
      HttpResponseHeaders,
      HttpMethod,
      HttpPostData,
      HttpHeadersKeys,
      HttpHeadersValues
  }
end;

destructor THttpAndroidReader.Destroy;
begin
  { TODO: The underlying downloading (on Java side) is not interrupted.
    It will continue downloading under the hood, and we will just ignore the messages. }
  Messaging.OnReceive.Remove({$ifdef CASTLE_OBJFPC}@{$endif} HandleDownloadMessages);
  inherited;
end;

{$endif ANDROID}

{$endif read_implementation}
