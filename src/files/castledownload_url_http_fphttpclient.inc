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

{ Handle http, https URLs using FpHttpClient. }

{$ifdef HAS_FP_HTTP_CLIENT}

{ TProgressMemoryStream ------------------------------------------------------ }

type
  { TMemoryStream descendant that shows a progress bar when writing to it. }
  TProgressMemoryStream = class(TMemoryStream)
  private
    // TODO: ParentDownload: TCastleDownload;
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

function TProgressMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited;
  // TODO
  // if ParentDownload <> nil then
  //   ParentDownload.DownloadedBytes := Max(Size, 0);
end;

{ TCastleHTTPClient ---------------------------------------------------------- }

type
  { HTTP client. In addition to TFPHTTPClient, it handles a progress bar
    initialization and finalization. }
  TCastleHTTPClient = class(TFPHTTPClient)
  private
    ProgressStream: TProgressMemoryStream;
  protected
    function ReadResponseHeaders: Integer; override;
    procedure DisconnectFromServer; override;
  public
    destructor Destroy; override;
  end;

function TCastleHTTPClient.ReadResponseHeaders: Integer;

  { Read Content-Length from ResponseHeaders.
    Returns -1 if unknown or invalid. }
  function CheckContentLength: Integer;
  { Code copied from TFPHTTPClient.CheckContentLength (it is private there).
    The license is the same as our engine, so copying is Ok. }
  const CL = 'content-length:';
  var
    S: String;
    I: Integer;
  begin
    Result := -1;
    I := 0;
    while (Result = -1) and (I < ResponseHeaders.Count) do
    begin
      S := Trim(LowerCase(ResponseHeaders[i]));
      if Copy(S, 1, Length(Cl)) = Cl then
      begin
        System.Delete(S, 1, Length(CL));
        Result := StrToIntDef(Trim(S), -1);
      end;
      Inc(I);
    end;
  end;

begin
  Result := inherited;
  // TODO
  // ParentDownload.TotalBytes := CheckContentLength;
end;

procedure TCastleHTTPClient.DisconnectFromServer;
begin
  inherited;
end;

destructor TCastleHTTPClient.Destroy;
begin
  inherited;
end;

{ NetworkDownload ------------------------------------------------------------ }

{ Just like Download, but
  - Assumes that the URL is from the network (this prevents network URLs
    redirecting to local URLs),
  - Limits the number of redirects to given value.
  - Guarantees that result is TMemoryStream. Never handles gzip decompression. }
function NetworkDownload(const URL: string;
  const MaxRedirects: Cardinal; out MimeType: string): TProgressMemoryStream;

  { Extract MimeType from HTTP Content-Type.
    Returns empty string if Content-Type is empty (undefined). }
  function ContentTypeToMimeType(const ContentType: string): string;
  var
    P: Integer;
  begin
    P := Pos(';', ContentType);
    if P <> 0 then
      Result := Trim(Copy(ContentType, 1, P - 1)) else
      Result := Trim(ContentType);
  end;

  { Workaround http://bugs.freepascal.org/view.php?id=24332 for FPC <= 2.6.2:
    sometimes we need to add final slash to URL, otherwise FpHttpClient
    will cause Access Violation. }
  function FixURL(const URL: string): string;
  var
    URI: TURI;
  begin
    URI := ParseUri(URL);
    { TFPCustomHTTPClient.GetServerURL crashes when URI.Path is empty }
    if URI.Path = '' then
    begin
      URI.Path := '/';
      Result := EncodeUri(URI);
    end else
      Result := URL;
  end;

var
  Client: TCastleHTTPClient;
  RedirectLocation: string;
  MimeTypeFromContentHeader: boolean;
begin
  Result := TProgressMemoryStream.Create;
  try
    Client := TCastleHTTPClient.Create(nil);
    try
      Client.ProgressStream := Result;
      { do not simply use Client.Get(URL, Result), as it cannot handle redirects }
      Client.HTTPMethod('GET', FixURL(URL), Result, [200,
        { redirect status codes, see http://en.wikipedia.org/wiki/HTTP_302 }
        301, 302, 303, 307]);
      // Writeln(Client.ResponseHeaders.Text);
      Client.ResponseHeaders.NameValueSeparator := ':';
      if Client.ResponseStatusCode <> 200 then
      begin
        FreeAndNil(Result);
        RedirectLocation := Trim(Client.ResponseHeaders.Values['Location']);
        if RedirectLocation = '' then
          raise EDownloadError.Create('HTTP redirect location is not set');
        if MaxRedirects = 0 then
          raise EDownloadError.Create('Cannot download resource, maximum number of redirects reached. Possible redirect loop');
        WritelnLog('Network', 'Following HTTP redirect (code %d) to "%s"',
          [Client.ResponseStatusCode, RedirectLocation]);
        Exit(NetworkDownload(RedirectLocation, MaxRedirects - 1, MimeType));
      end;
      MimeType := ContentTypeToMimeType(Client.ResponseHeaders.Values['Content-Type']);
      MimeTypeFromContentHeader := MimeType <> '';
      if not MimeTypeFromContentHeader then
        MimeType := URIMimeType(URL);
      WritelnLog('Network', 'Successfully downloaded "%s", MIME type "%s", MIME type was specified by server: %s',
        [URL, MimeType, SysUtils.BoolToStr(MimeTypeFromContentHeader, true)]);
    finally FreeAndNil(Client) end;
    Result.Position := 0; { rewind for easy reading }
  except
    FreeAndNil(Result); raise;
  end;
end;

{ TUrlHttpFpHttpClient ------------------------------------------------------- }

type
  TUrlHttpFpHttpClient = class
    class function Read(const Url: string; out MimeType: string): TStream;
  end;

class function TUrlHttpFpHttpClient.Read(const Url: string; out MimeType: string): TStream;
const
  MaxRedirects = 32;
begin
  if not EnableNetwork then
    raise EDownloadError.Create('Downloading network resources (from "http" or "https" protocols) is not enabled');

  {$ifdef VER3_0}
  if URIProtocol(Url) = 'https' then
    { Testcase: FPC 3.0.4, Linux/x86_64:
      TFPCustomHTTPClient fails with Access Violation on https URLs.
      TODO: Test on Windows/x86_64. }
    raise EDownloadError.Create('Downloading using "https" protocol does not work when the application is compiled with FPC 3.0.x. Use newer FPC (and add OpenSSLSockets unit to the uses clause).');
  {$endif}

  CheckFileAccessSafe(Url);
  WritelnLog('Network', 'Downloading "%s"', [URIDisplay(Url)]);
  Result := NetworkDownload(Url, MaxRedirects, MimeType);
end;

{ THttpFpHttpClientReader --------------------------------------------------- }

type
  THttpFpHttpClientReader = class(TUrlAsynchronousReader)
    constructor Create(const Url: String); override;
    procedure Update; override;
  end;

constructor THttpFpHttpClientReader.Create(const Url: String);
begin
  inherited;
  // TODO
end;

procedure THttpFpHttpClientReader.Update;
begin
  inherited;
  // TODO
end;

{$endif HAS_FP_HTTP_CLIENT}

{$endif}
