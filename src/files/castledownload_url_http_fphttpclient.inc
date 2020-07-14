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
    procedure RedirectLog(Sender: TObject; const ASrc: String; var ADest: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TCastleHTTPClient.Create(AOwner: TComponent);
begin
  inherited;
  OnRedirect := @RedirectLog;
end;

procedure TCastleHTTPClient.RedirectLog(Sender: TObject; const ASrc: String; var ADest: String);
begin
  WritelnLog('Network', 'Following HTTP redirect to "%s"', [ADest]);
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

{ Download using FpHttpClient. Also updates MimeType. }
function NetworkDownload(const URL: string; out MimeType: string): TProgressMemoryStream;

  { Extract MimeType from HTTP Content-Type.
    Returns empty string if Content-Type is empty (undefined). }
  function ContentTypeToMimeType(const ContentType: string): string;
  var
    P: Integer;
  begin
    P := Pos(';', ContentType);
    if P <> 0 then
      Result := Trim(Copy(ContentType, 1, P - 1))
    else
      Result := Trim(ContentType);
  end;

var
  Client: TCastleHTTPClient;
  MimeTypeFromContentHeader: boolean;
begin
  Result := TProgressMemoryStream.Create;
  try
    Client := TCastleHTTPClient.Create(nil);
    try
      Client.ProgressStream := Result;
      Client.AllowRedirect := true;
      { do not simply use Client.Get(URL, Result), as it cannot handle redirects }
      Client.Get(Url, Result);

      // Determine MimeType
      // TODO: move to TCastleHTTPClient.ReadResponseHeaders, to determine earlier
      // Writeln(Client.ResponseHeaders.Text);
      Client.ResponseHeaders.NameValueSeparator := ':';
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
  Result := NetworkDownload(Url, MimeType);
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
