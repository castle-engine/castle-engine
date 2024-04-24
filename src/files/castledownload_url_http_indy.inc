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

  Most things work:

  - Asynchronous operation (TCastleDownload connecting and downloading doesn't
    block the main thread),
  - various HTTP methods (GET, POST with post data...),
  - getting MIME type from server response,
  - updating progress (total bytes, downloaded bytes),
  - aborting download in the middle is smooth.

  - Both http and https work smothly on Windows.

    For https, note that you have to use  OpenSSL DLLs.
    You can get the 2 necessary DLLs from

    - win64: https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/external_libraries/x86_64-win64/openssl

    - win32: https://github.com/castle-engine/castle-engine/tree/master/tools/build-tool/data/external_libraries/i386-win32/openssl

    We advise to compile at least once using CGE editor that will place
    the DLLs automatically alongside your EXE.

  - The code also work on Delphi/Linux.

    http downloading works (test e.g. http://neverssl.com/ ).

    https unfortunately doesn't work, i.e. it will answer "SSL library not found".
    It seems Indy requires really old OpenSSL library version (1.0 ?),
    not available in latest Ubuntu. And it's not even 1.1,
    so https://gist.github.com/joulgs/c8a85bb462f48ffc2044dd878ecaa786 will not help.
    The PR to support newer OpenSSL was never merged https://github.com/IndySockets/Indy/pull/299 .

  TODOs:

  - How to have https download working from Delphi/Linux?

  - Support HttpHeadersKeys, HttpHeadersValues, HttpResponseHeaders
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
    HttpPostData, HttpHeadersKeys, HttpHeadersValues: TStrings; //< TODO HttpHeadersKeys, HttpHeadersValues
    Url: String;
    HttpMethod: THttpMethod;
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

    { Use SSL with proper options in case of https protocol.
      Note that using TIdSSLIOHandlerSocketOpenSSL to unencrypted http
      also works, but then we rely on OpenSSL library (DDL, SO) being present
      unnecessarily. }
    if URIProtocol(Url) = 'https' then
    begin
      { Set special IO handler to do SSL_set_tlsext_host_name,
        otherwise accessing sites behind Cloudflare will fail,
        see https://stackoverflow.com/questions/29875664/eidosslunderlyingcryptoerror-exception }
      MyIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(IdHttp);
      MyIOHandler.OnStatusInfoEx := StatusInfoEx;
      MyIOHandler.SSLOptions.Method := sslvSSLv23;
      MyIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_1, sslvTLSv1];
      IdHttp.IOHandler := MyIOHandler;
    end;

    IdHttp.OnRedirect := Redirect;
    IdHttp.OnHeadersAvailable := HeadersAvailable;
    IdHttp.OnWork := Work;

    case HttpMethod of
      hmGet: IdHttp.Get(Url, Contents);
      { Note that HttpPostData may be nil, that's OK,
        TIdCustomHTTP.Post is ready for this case. }
      hmPost: IdHttp.Post(Url, HttpPostData, Contents);
      // TODO: We should pass Source stream to put
      hmPut: IdHttp.Put(Url, nil, Contents);
      hmDelete: IdHttp.Delete(Url, Contents);
      hmOptions: IdHttp.Options(Url, Contents);
      hmHead: IdHttp.Head(Url);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('HttpMethod=?');
      {$endif}
    end;

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
      ErrorMessage := Format('Exception when downloading "%s": ', [UriDisplay(Url)]) + ExceptMessage(E);
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
    Thread.MimeType := UriMimeType(Url); // will be updated from HTTP headers
  end;

begin
  inherited;
  if not CheckNetworkAccess then
    Exit;

  WritelnLog('Network', 'Downloading "%s"', [UriDisplay(Url)]);
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

    { Check Thread.Finished only to avoid writing unnecessarily alarming
      "interrupting download in-progress ..." message,
      testcase: examples/network/remote_logging/ . }
    if not Thread.Finished then
    begin
      WritelnLog('Network', 'Interrupting download in-progress of "%s"', [UriDisplay(Url)]);
      Thread.WaitFor;
    end;
    FreeAndNil(Thread);

    { Note: Thread.WaitFor can cause short delay when interrupting download.
      Seems unnoticeable for now, so let it be.
      See castledownload_url_http_fphttpclient.inc for working but hacky
      way to avoid this. }
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
