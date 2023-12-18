{%MainUnit castledownload.pas}
{
  Copyright 2013-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handle http, https URLs using Android service. }

{$ifdef ANDROID}

{$ifdef read_implementation_uses}
CastleAndroidInternalAssetStream, CastleMessaging,
{$endif read_implementation_uses}

{$ifdef read_implementation}

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
    function HandleDownloadMessages(const Received: TCastleStringList;
      const ReceivedStream: TMemoryStream): boolean;
  public
    procedure Start; override;
    destructor Destroy; override;
  end;

procedure THttpAndroidReader.Start;

  function HttpMethodName: String;
  begin
    case HttpMethod of
      hmGet: Result := 'GET';
      hmPost: Result := 'POST';
      hmPut: Result := 'PUT';
      hmDelete: Result := 'DELETE';
      hmOptions: Result := 'OPTIONS';
      hmHead: Result := 'HEAD';
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('HttpMethod=?');
      {$endif}
    end;
  end;

  function HttpPostDataStr: String;
  begin
    if HttpPostData <> nil then
      Result := HttpPostData.Text
    else
      Result := '';
  end;

begin
  inherited;

  DownloadId := NextDownloadId;
  Inc(NextDownloadId);

  CheckFileAccessSafe(Url);
  WritelnLog('Network', 'Download service started for "%s", id %d', [
    UriDisplay(Url),
    DownloadId
  ]);

  MimeType := UriMimeType(Url);

  Messaging.OnReceive.Add({$ifdef FPC}@{$endif} HandleDownloadMessages);
  Messaging.Send(['download-url', IntToStr(DownloadId), Url, HttpMethodName, HttpPostDataStr]);
  Contents := TMemoryStream.Create;

  HttpResponseHeaders := TStringList.Create;
  HttpResponseHeaders.NameValueSeparator := ':';
end;

function THttpAndroidReader.HandleDownloadMessages(const Received: TCastleStringList;
  const ReceivedStream: TMemoryStream): boolean;

  procedure HandleHttpResponseHeaders(const Received: TCastleStringList);
  var
    I: Integer;
    Val, MimeTypeFromResponse: String;
  begin
    { append to HttpResponseHeaders }
    for I := 2 to Received.Count - 1 do
      HttpResponseHeaders.Append(Received[I]);

    { set TotalBytes from Content-Length response }
    Val := HttpResponseHeaders.Values['Content-Length'];
    if Val <> '' then
      TotalBytes := StrToInt64Def(Val, -1);

    { set MimeType (overrides MIME from URL extension) from Content-Type response }
    Val := HttpResponseHeaders.Values['Content-Type'];
    if Val <> '' then
    begin
      MimeTypeFromResponse := ContentTypeToMimeType(Val);
      if MimeTypeFromResponse <> '' then
        MimeType := MimeTypeFromResponse;
    end;
  end;

begin
  if (Received.Count = 3) and
     (Received[0] = 'download-error') and
     (Received[1] = IntToStr(DownloadId)) then
  begin
    Status := dsError;
    ErrorMessage := Received[2];
    Result := true;
  end;

  if (Received.Count >= 2) and
     (Received[0] = 'download-http-response-headers') and
     (Received[1] = IntToStr(DownloadId)) then
  begin
    HandleHttpResponseHeaders(Received);
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
    Contents.Position := 0; // rewind
    DownloadedBytes := Contents.Size;
    TotalBytes := Contents.Size;
    Result := true;
  end;

  if (Received.Count = 4) and
     (Received[0] = 'download-response-code') and
     (Received[1] = IntToStr(DownloadId)) then
  begin
    HttpResponseCode := StrToInt(Received[2]);
    // WritelnLog('Network', 'HTTP response for "%s" is %d: %s', [
    //   UriDisplay(Url),
    //   HttpResponseCode,
    //   Received[3]
    // ]);
    Result := true;
  end;

  { TODO:
    - HttpHeadersKeys, HttpHeadersValues not handled
      use URLConnection API (setRequestProperty), https://docs.oracle.com/javase/7/docs/api/java/net/URLConnection.html
  }
end;

destructor THttpAndroidReader.Destroy;
begin
  if Status = dsDownloading then
    Messaging.Send(['download-interrupt', IntToStr(DownloadId)]);
  Messaging.OnReceive.Remove({$ifdef FPC}@{$endif} HandleDownloadMessages);
  inherited;
end;

{$endif read_implementation}

{$endif ANDROID}

