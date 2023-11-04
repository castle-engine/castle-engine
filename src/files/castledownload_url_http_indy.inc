{%MainUnit castledownload.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$if defined(DELPHI) and not defined(CASTLE_DELPHI_NET_HTTP_CLIENT)}

{ Handle downloading http, https URLs using TIdHttp (Indy component).

  See https://docwiki.embarcadero.com/CodeExamples/Sydney/en/HTTP_Get_(Delphi) .
  See https://github.com/michaliskambi/delphi-http-download for my tests.

  We use this only with Delphi now, though in principle we could make it work
  with FPC too, as it has Indy too. But we don't need it for FPC, where we use
  FpHttpClient.

  Since Delphi includes Indy, we don't need to distribute Indy with CGE,
  and users don't need to worry about getting Indy.
}

{$ifdef read_implementation_uses}
SyncObjs,
IdHttp, IdSSL, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdCTypes,
IdHeaderList, IdTCPConnection, IdComponent,
{$endif}

{$ifdef read_implementation}

type
  { Thread that downloads using TIdHttp. }
  TIndyReaderThread = class(TThread)
  strict private
    IdHttp: TIdHttp;
    MyIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    { Use with special IO handler to do SSL_set_tlsext_host_name,
      otherwise accessing sites behind Cloudflare will fail,
      see https://stackoverflow.com/questions/29875664/eidosslunderlyingcryptoerror-exception }
    procedure StatusInfoEx(ASender: TObject; const AsslSocket: PSSL;
      const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
    procedure Redirect(Sender: TObject;
      var Dest: string; var NumRedirect: Integer; var Handled: boolean; var VMethod: TIdHTTPMethod);
    procedure HeadersAvailable(Sender: TObject;
      AHeaders: TIdHeaderList; var VContinue: Boolean);
    procedure Work(ASender: TObject;
      AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    { Set these before Execute }
    HttpPostData, HttpHeadersKeys, HttpHeadersValues: TStrings; //< TODO handle
    Url: String;
    HttpMethod: THttpMethod; //< TODO handle
    SynchronizationCS: TCriticalSection;

    { Updated even during downloading, but always using SynchronizationCS. }
    DownloadedBytes, TotalBytes: Int64;
    MimeType: String;
    FinalUrl: String;

    { Once we're finished (Running = false) then

      - Success informs about whether we have dsSuccess or dsError.
      - HttpResponseCode is the HTTP answer.
      - Contents contain the response (regardless of Success value).
        If you take it, then set it to @nil, to avoid freeing it here.
      - When Success = @false, ErrorMessage is the error message.
    }
    Success: Boolean;
    Contents: TMemoryStream;
    ErrorMessage: String;
    HttpResponseCode: Integer;
    //HttpResponseHeaders: TStrings; //< TODO handle

    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TUrlAsynchronousReader descendant that implements http(s) downloading. }
  TIndyReader = class(TUrlAsynchronousReader)
  strict private
    Thread: TIndyReaderThread;
    SynchronizationCS: TCriticalSection;
  public
    procedure Start; override;
    destructor Destroy; override;
    procedure Update; override;
  end;

  { Define TDelphiHttpReader used in castledownload_register.inc.
    It is an alias to a different class, depending on whether
    CASTLE_DELPHI_NET_HTTP_CLIENT was defined. }
  TDelphiHttpReader = TIndyReader;

{ TIndyReaderThread --------------------------------------------------- }

constructor TIndyReaderThread.Create;
begin
  inherited Create(true); // create suspended
  Contents := TMemoryStream.Create;
  TotalBytes := -1;
end;

procedure TIndyReaderThread.Execute;
begin
  IdHttp := TIdHTTP.Create(nil);
  try
    IdHttp.HandleRedirects := true;

    { We need to pass User-Agent to avoid Cloudflare answering with 403 Forbidden.
      See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/User-Agent
      about User-Agent. }
    IdHttp.Request.UserAgent :=  'Mozilla/5.0 (compatible; CastleGameEngine/1.0; https://castle-engine.io/manual_network.php)';

    { Set special IO handler to do SSL_set_tlsext_host_name,
      otherwise accessing sites behind Cloudflare will fail,
      see https://stackoverflow.com/questions/29875664/eidosslunderlyingcryptoerror-exception }
    MyIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(IdHttp);
    MyIOHandler.OnStatusInfoEx := StatusInfoEx;
    MyIOHandler.SSLOptions.Method := sslvSSLv23;
    MyIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_1, sslvTLSv1];
    IdHttp.IOHandler := MyIOHandler;

    IdHttp.OnRedirect := Redirect;
    IdHttp.OnHeadersAvailable := HeadersAvailable;
    IdHttp.OnWork := Work;

    IdHttp.Get(Url, Contents);

    HttpResponseCode := IdHttp.ResponseCode;

    Contents.Position := 0; { rewind for easy reading }
    if HttpResponseCode <> 200 then
    begin
      Success := false;
      ErrorMessage := Format('HTTP request completed with status %d (not 200, so not success)', [
        HttpResponseCode
      ]);
    end else
    begin
      Success := true;
    end;
  except
    on E: TObject do
    begin
      Success := false;
      ErrorMessage := Format('Exception when downloading "%s": ', [URIDisplay(Url)]) + ExceptMessage(E);
    end;
  end;
end;

destructor TIndyReaderThread.Destroy;
begin
  // We have to free MyIOHandler early, to avoid later exceptions
  FreeAndNil(MyIOHandler);
  FreeAndNil(IdHttp);
  FreeAndNil(Contents);
  inherited;
end;

procedure TIndyReaderThread.StatusInfoEx(ASender: TObject; const AsslSocket: PSSL;
  const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
begin
  SSL_set_tlsext_host_name(AsslSocket, IdHttp.Request.Host);
end;

procedure TIndyReaderThread.Redirect(Sender: TObject;
  var Dest: string; var NumRedirect: Integer; var Handled: boolean; var VMethod: TIdHTTPMethod);
begin
  // TODO: WritelnLog is not safe from thread:
  WritelnLog('Network', 'Following HTTP redirect to "%s"', [Dest]);

  SynchronizationCS.Acquire;
  try
    FinalUrl := Dest;
  finally SynchronizationCS.Release end;
end;

procedure TIndyReaderThread.HeadersAvailable(Sender: TObject;
  AHeaders: TIdHeaderList; var VContinue: Boolean);
var
  ContentType, ContentLength: String;
  ContentLengthInt: Int64;
begin
  ContentType := AHeaders.Values['Content-Type'];
  if ContentType <> '' then
  begin
    SynchronizationCS.Acquire;
    try
      MimeType := ContentTypeToMimeType(ContentType);
    finally SynchronizationCS.Release end;
  end;

  ContentLength := AHeaders.Values['Content-Length'];
  if TryStrToInt64(ContentLength, ContentLengthInt) then
  begin
    SynchronizationCS.Acquire;
    try
      TotalBytes :=ContentLengthInt;
    finally SynchronizationCS.Release end;
  end;
end;

procedure TIndyReaderThread.Work(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  SynchronizationCS.Acquire;
  try
    DownloadedBytes := AWorkCount;
  finally SynchronizationCS.Release end;

  // honor thread Terminate call
  if Terminated then
    Abort;
end;

{ TIndyReader --------------------------------------------------- }

procedure TIndyReader.Start;

  procedure InitializeThread(const Thread: TIndyReaderThread);
  begin
    Thread.Url := Url;
    Thread.SynchronizationCS := SynchronizationCS;
    Thread.HttpMethod := HttpMethod;
    Thread.HttpPostData := HttpPostData;
    Thread.HttpHeadersKeys := HttpHeadersKeys;
    Thread.HttpHeadersValues := HttpHeadersValues;

    // set initial values, thread may update them later
    Thread.FinalUrl := Url; // will be updated on each redirect
    Thread.MimeType := URIMimeType(Url); // will be updated from HTTP headers
  end;

begin
  inherited;
  WritelnLog('Network', 'Downloading "%s"', [URIDisplay(Url)]);
  SynchronizationCS := TCriticalSection.Create;
  Thread := TIndyReaderThread.Create;
  InitializeThread(Thread);
  Thread.Start;
end;

destructor TIndyReader.Destroy;
begin
  if Thread <> nil then
  begin
    Thread.Terminate;

    WritelnLog('Network', 'Interrupting download in-progress of "%s"', [URIDisplay(Url)]);
    Thread.WaitFor;
    FreeAndNil(Thread);

    // TODO: This can cause short delay when interrupting download.
  end;

  FreeAndNil(SynchronizationCS);

  inherited;
end;

procedure TIndyReader.Update;

  procedure SynchronizeFromFinishedThread;
  begin
    DownloadedBytes := Thread.Contents.Size;
    TotalBytes := Thread.Contents.Size;
    MimeType := Thread.MimeType;
    Contents := Thread.Contents;
    Thread.Contents := nil; // do not free our Contents inside TIndyReaderThread destructor anymore
    HttpResponseCode := Thread.HttpResponseCode;
    // TODO
    //HttpResponseHeaders := Thread.HttpResponseHeaders;
    //Thread.HttpResponseHeaders := nil; // do not free HttpResponseHeaders inside TIndyReaderThread destructor
    FinalUrl := Thread.FinalUrl;
    if Thread.Success then
    begin
      Status := dsSuccess;
      WritelnLog('Network', 'Successfully downloaded "%s", MIME type "%s", HTTP response code %d', [
        Url,
        MimeType,
        HttpResponseCode
      ]);
    end else
    begin
      Status := dsError;
      ErrorMessage := Thread.ErrorMessage;
      WritelnLog('Network', 'Failed to download "%s", HTTP response code %d', [
        Url,
        HttpResponseCode
      ]);
    end;
  end;

begin
  inherited;

  if not Thread.Finished then
  begin
    SynchronizationCS.Acquire;
    try
      DownloadedBytes := Thread.DownloadedBytes;
      TotalBytes := Thread.TotalBytes;
      MimeType := Thread.MimeType;
    finally SynchronizationCS.Release end;
  end else
  begin
    SynchronizeFromFinishedThread;
  end;
end;

{$endif read_implementation}

{$endif defined(DELPHI) and not defined(CASTLE_DELPHI_NET_HTTP_CLIENT)}
