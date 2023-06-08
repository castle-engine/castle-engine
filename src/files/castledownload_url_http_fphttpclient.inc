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

{$ifdef read_interface}

{$endif}

{$ifdef read_implementation}

{ Handle http, https URLs using FpHttpClient. }

{$ifdef HAS_FP_HTTP_CLIENT}

type
  TFpHttpClientThread = class;
  TFpHttpClientJob = class;

  { Customized HTTP client. In addition to TFpHttpClient this:

    - Updates ParentJob.DownloadedBytes
    - Updates ParentJob.TotalBytes
    - Updates ParentJob.MimeType
    - Updates ParentJob.FinalUrl
    - Terminates the client, if ParentThread is terminated
      (ParentThread can also be = nil, for non-threaded operation)

    This is used only within thread, in TFpHttpClientThread. }
  TCastleFpHttpClient = class(TFpHttpClient)
  protected
    function ReadResponseHeaders: Integer; override;
    procedure DisconnectFromServer; override;
    procedure RedirectLog(Sender: TObject; const ASrc: String; var ADest: String);
    procedure DataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
  public
    ParentJob: TFpHttpClientJob;
    ParentThread: TFpHttpClientThread;
    SynchronizationCS: TCriticalSection;
    Contents: TMemoryStream;
    MimeTypeFromUrl: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Job that downloads using FpHttpClient.
    Can be put in thread (like TFpHttpClientThread) or used without thread. }
  TFpHttpClientJob = class
  public
    { Set these before Execute }
    HttpPostData, HttpHeadersKeys, HttpHeadersValues: TStrings;
    Url: String;
    HttpMethod: THttpMethod;
    SynchronizationCS: TCriticalSection;
    ParentThread: TFpHttpClientThread; //< leave nil for non-threaded operation

    { Updated even during downloading. }
    DownloadedBytes, TotalBytes: Int64;
    MimeType: String;

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
    HttpResponseHeaders: TStrings;
    FinalUrl: String;

    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

  { Thread that downloads using FpHttpClient. }
  TFpHttpClientThread = class(TThread)
    Job: TFpHttpClientJob;
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TUrlAsynchronousReader descendant that implements http(s) downloading. }
  TFpHttpClientReader = class(TUrlAsynchronousReader)
  strict private
    Thread: TFpHttpClientThread;
    SynchronizationCS: TCriticalSection;
    { Synchronize one last time from Job, when it finished work in Execute. }
    procedure SynchronizeFromFinishedJob(const Job: TFpHttpClientJob);
  public
    procedure Start; override;
    destructor Destroy; override;
    procedure Update; override;
  end;

{ TCastleFpHttpClient ---------------------------------------------------------- }

constructor TCastleFpHttpClient.Create(AOwner: TComponent);
begin
  inherited;
  OnRedirect := @RedirectLog;
  OnDataReceived := @DataReceived;
end;

procedure TCastleFpHttpClient.RedirectLog(Sender: TObject; const ASrc: String; var ADest: String);
begin
  // TODO: WritelnLog is not safe from thread:
  WritelnLog('Network', 'Following HTTP redirect to "%s"', [ADest]);

  SynchronizationCS.Acquire;
  try
    ParentJob.FinalUrl := ADest;
  finally SynchronizationCS.Release end;
end;

procedure TCastleFpHttpClient.DataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  SynchronizationCS.Acquire;
  try
    ParentJob.DownloadedBytes := Max(Contents.Size, 0);

    if ContentLength = 0 then
      { FpHttpClient leaves ContentLength as 0 in case of chunked HTTP response.
        We want to treat it as -1 (unknown) then.
        Testcase: examples/network/asynchronous_download/ with
        https://castle-engine.io/modern_pascal_introduction.html (FPC 3.2.0 on Linux/x86_64). }
      ParentJob.TotalBytes := -1
    else
      { Otherwise ContentLength is exactly what we need for TotalBytes.
        Note: FpHttpClient may set ContentLength to -1 if unknown
        (not specified in headers), which is exactly what we want. }
      ParentJob.TotalBytes := ContentLength;
  finally SynchronizationCS.Release end;

  {$ifndef VER3_0_2}
  if (ParentThread <> nil) and ParentThread.Terminated then
    Terminate;
  {$endif}
end;

function TCastleFpHttpClient.ReadResponseHeaders: Integer;
var
  MimeType: String;
begin
  Result := inherited;

  { Determine ParentJob.MimeType.
    Do it only for http status = 200, otherwise we would detect MIME type
    for intermediate content send with redirection. }
  if Result = 200 then
  begin
    ResponseHeaders.NameValueSeparator := ':';
    MimeType := ContentTypeToMimeType(ResponseHeaders.Values['Content-Type']);
    if MimeType = '' then
      MimeType := MimeTypeFromUrl;
    SynchronizationCS.Acquire;
    try
      ParentJob.MimeType := MimeType;
    finally SynchronizationCS.Release end;
  end;
end;

procedure TCastleFpHttpClient.DisconnectFromServer;
begin
  inherited;
end;

destructor TCastleFpHttpClient.Destroy;
begin
  inherited;
end;

{ TFpHttpClientJob -------------------------------------------------------- }

constructor TFpHttpClientJob.Create;
begin
  inherited;
  Contents := TMemoryStream.Create;
  TotalBytes := -1;
end;

destructor TFpHttpClientJob.Destroy;
begin
  FreeAndNil(Contents);
  FreeAndNil(HttpResponseHeaders);
  inherited;
end;

procedure TFpHttpClientJob.Execute;
var
  Client: TCastleFpHttpClient;
  I: Integer;
begin
  try
    Client := TCastleFpHttpClient.Create(nil);
    try
      Client.ParentJob := Self;
      Client.ParentThread := ParentThread;
      Client.SynchronizationCS := SynchronizationCS;
      Client.Contents := Contents;
      Client.AllowRedirect := true;
      Client.MimeTypeFromUrl := URIMimeType(Url);
      // Client is used only once, so KeepConnection not necessary
      {$ifndef VER3_0_2}
      Client.KeepConnection := false;
      {$endif}
      // add additional headers
      if HttpHeadersKeys <> nil then
      begin
        Assert((HttpHeadersKeys <> nil) = (HttpHeadersValues <> nil));
        Assert(HttpHeadersKeys.Count = HttpHeadersValues.Count);
        for I := 0 to HttpHeadersKeys.Count - 1 do
          Client.AddHeader(HttpHeadersKeys[I], HttpHeadersValues[I]);
      end;

      // will be updated by Client when redirect occurs
      FinalUrl := Url;

      try
        case HttpMethod of
          hmGet: Client.Get(Url, Contents);
          hmPost:
            if HttpPostData <> nil then
              Client.FormPost(Url, HttpPostData, Contents)
            else
              Client.Post(Url, Contents);
          hmPut: Client.Put(Url, Contents);
          hmDelete: Client.Delete(Url, Contents);
          hmOptions: Client.Options(Url, Contents);
          hmHead: //Client.Head(Url);
            Client.HTTPMethod('HEAD', Url, nil, [200]);
          {$ifndef COMPILER_CASE_ANALYSIS}
          else raise EInternalError.Create('HttpMethod=?');
          {$endif}
        end;
      finally
        HttpResponseCode := Client.ResponseStatusCode;
        HttpResponseHeaders := TStringList.Create;
        HttpResponseHeaders.NameValueSeparator := ':';
        HttpResponseHeaders.Assign(Client.ResponseHeaders);
      end;

      { In case of GET, this is done automatically by FpHttpClient.
        But for e.g. POST, it accepts any response code as OK (I'm not sure why).
        So we check it manually here. }
      if HttpResponseCode <> 200 then
        raise EHTTPClient.CreateFmt('Unexpected response status code: %d', [HttpResponseCode]);
    finally FreeAndNil(Client) end;

    Contents.Position := 0; { rewind for easy reading }

    Success := true;
  except
    on E: TObject do
    begin
      Success := false;
      ErrorMessage := Format('Error when downloading "%s": ', [URIDisplay(Url)]) + ExceptMessage(E);

      {$if defined(VER3_2) and defined(DARWIN) and not defined(CASTLE_IOS)}
      if (E is ESocketError) and (URIProtocol(Url) = 'https') then
      begin
        ErrorMessage += NL +
          'Warning: Https downloading with FPC 3.2.0 on macOS is broken for many hosts.' + NL +
          'Upgrade to FPC 3.3.1.' + NL +
          'See https://bugs.freepascal.org/view.php?id=37747 , https://github.com/castle-engine/castle-engine/issues/184 .';
      end;
      {$endif}
    end;
  end;
end;

{ TFpHttpClientThread -------------------------------------------------------- }

constructor TFpHttpClientThread.Create;
begin
  inherited Create(true); // create suspended
  Job := TFpHttpClientJob.Create;
  Job.ParentThread := Self;
end;

destructor TFpHttpClientThread.Destroy;
begin
  FreeAndNil(Job);
  inherited;
end;

procedure TFpHttpClientThread.Execute;
begin
  Job.Execute;
end;

{ TFpHttpClientReader --------------------------------------------------- }

procedure TFpHttpClientReader.Start;

  procedure InitializeJob(const Job: TFpHttpClientJob);
  begin
    Job.Url := Url;
    Job.SynchronizationCS := SynchronizationCS;
    Job.HttpMethod := HttpMethod;
    Job.HttpPostData := HttpPostData;
    Job.HttpHeadersKeys := HttpHeadersKeys;
    Job.HttpHeadersValues := HttpHeadersValues;
  end;

var
  SynchronousJob: TFpHttpClientJob;
begin
  inherited;

  { The main purpose of this is to disable Download from blocking.
    Additionally, it also disables TCastleDownload from "downgrading" to blocking,
    when OS doesn't support threads. }
  if ForceSynchronous and (not EnableBlockingDownloads) then
  begin
    Status := dsError;
    ErrorMessage := 'Downloading network resources (from "http" or "https"  protocols) is not enabled. Set global EnableBlockingDownloads:=true to enable.';
    Exit;
  end;

  if URIProtocol(Url) = 'https' then
  begin
    {$ifdef VER3_0}
    { Testcase: FPC 3.0.4, Linux/x86_64:
      TFPCustomHTTPClient fails with Access Violation on https URLs.
      TODO: Test on Windows/x86_64. }
    Status := dsError;
    ErrorMessage := 'Downloading using "https" protocol does not work when the application is compiled with FPC 3.0.x. Use newer FPC (and add OpenSSLSockets unit to the uses clause).';
    Exit;
    {$endif}

    {$ifndef VER3_0} // only for FPC >= 3.2.0
    { The first download using https initializes OpenSSL library.
      This is done by TFPCustomHTTPClient accessing TSSLSocketHandler.GetDefaultHandler,
      and using OpenSSLSockets unit initializes that handler to do MaybeInitSSLInterface
      at handler creation.
      However this cannot be done from thread: multiple threads trying to do
      MaybeInitSSLInterface at the same time, means that only 1 succeeds,
      others raise "Could not initialize OpenSSL library".
      Testcase: examples/network/asynchronous_download/ with 3 downloads over https
      (FPC 3.2.0 on Linux/x86_64).

      Solution is simply to initialize it from main thread here. }
    TSSLSocketHandler.GetDefaultHandler.Free;
    {$endif}
  end;

  CheckFileAccessSafe(Url);
  WritelnLog('Network', 'Downloading "%s"', [URIDisplay(Url)]);

  SynchronizationCS := TCriticalSection.Create;

  if ForceSynchronous then
  begin
    { This is useful when OS doesn't support
      threads, or they not linked in (e.g. no CThreads on Unix).
      In this case we can at least download synchronously, which sometimes is enough. }
    SynchronousJob := TFpHttpClientJob.Create;
    try
      InitializeJob(SynchronousJob);
      SynchronousJob.Execute;
      SynchronizeFromFinishedJob(SynchronousJob);
    finally FreeAndNil(SynchronousJob) end;
  end else
  begin
    Thread := TFpHttpClientThread.Create;
    InitializeJob(Thread.Job);
    Thread.Start;
  end;
end;

destructor TFpHttpClientReader.Destroy;
begin
  if Thread <> nil then
  begin
    Thread.Terminate;

    {.$define CASTLE_SAFE_DOWNLOAD_INTERRUPTION}
    {$ifdef CASTLE_SAFE_DOWNLOAD_INTERRUPTION}
    WritelnLog('Network', 'Interrupting download in-progress of "%s"', [URIDisplay(Url)]);
    Thread.WaitFor;
    FreeAndNil(Thread);

    {$else}
    if not Thread.Finished then
    begin
      { TODO: This will leak memory if Running changed from true to false between these 2 lines.
        Then thread instance will not be freed, as ThreadProc (running Execute,
        and potentially freeing thread afterwards) has already finished
        before we set FreeOnTerminate.

        We cannot fix it by doing "Thread.FreeOnTerminate := true" earlier,
        because then we have a race, and "Thread" reference may no longer be used.

        A potential fix is in CASTLE_SAFE_DOWNLOAD_INTERRUPTION but this:
        1. would cause short delay when interrupting downloads
        2. doesn't work to prevent visible mem leak in examples/network/asynchronous_download/
           (but that leak seems unrelated? it occurs even if nothing was interrupted?)

        TODO: This is actually dangerous if the interruption will happen
        before thread Execute processed HttpPostData and HttpHeadersKeys/Values,
        as they may become invalid when owning TCastleDownload it freed.
      }
      WritelnLog('Network', 'Interrupting download in-progress of "%s"', [URIDisplay(Url)]);
      Thread.FreeOnTerminate := true;
      Thread := nil;
      SynchronizationCS := nil; // do not free it here, let Thread use it
    end else
      FreeAndNil(Thread);
    {$endif}
  end;

  FreeAndNil(SynchronizationCS);

  inherited;
end;

procedure TFpHttpClientReader.SynchronizeFromFinishedJob(const Job: TFpHttpClientJob);
begin
  { Looks like FpHttpClient doesn't call DoDataRead on the final data chunk,
    and thus Job.DownloadedBytes is a bit less than Job.TotalBytes at the end.
    The actual stream has correct size, so just use this. }
  DownloadedBytes := Job.Contents.Size; // Job.DownloadedBytes;
  TotalBytes := Job.TotalBytes;
  MimeType := Job.MimeType;
  Contents := Job.Contents;
  Job.Contents := nil; // do not free our Contents inside TFpHttpClientJob destructor anymore
  HttpResponseCode := Job.HttpResponseCode;
  HttpResponseHeaders := Job.HttpResponseHeaders;
  Job.HttpResponseHeaders := nil; // do not free HttpResponseHeaders inside TFpHttpClientJob destructor
  FinalUrl := Job.FinalUrl;
  if Job.Success then
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
    ErrorMessage := Job.ErrorMessage;
    WritelnLog('Network', 'Failed to download "%s", HTTP response code %d', [
      Url,
      HttpResponseCode
    ]);
  end;
end;

procedure TFpHttpClientReader.Update;
begin
  inherited;

  if Thread <> nil then // Thread may be nil here if not supported on this platform
  begin
    if not Thread.Finished then
    begin
      SynchronizationCS.Acquire;
      try
        DownloadedBytes := Thread.Job.DownloadedBytes;
        TotalBytes := Thread.Job.TotalBytes;
        MimeType := Thread.Job.MimeType;
      finally SynchronizationCS.Release end;
    end else
    begin
      SynchronizeFromFinishedJob(Thread.Job);
    end;
  end;
end;

{$endif HAS_FP_HTTP_CLIENT}

{$endif}
