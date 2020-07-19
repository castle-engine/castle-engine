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

  { Customized HTTP client. In addition to TFpHttpClient this:

    - Updates ParentThread.DownloadedBytes
    - Updates ParentThread.TotalBytes
    - Updates ParentThread.MimeType
    - Terminates the client, if thread is terminated

    This is used only within thread, in TFpHttpClientThread. }
  TCastleFpHttpClient = class(TFpHttpClient)
  protected
    function ReadResponseHeaders: Integer; override;
    procedure DisconnectFromServer; override;
    procedure RedirectLog(Sender: TObject; const ASrc: String; var ADest: String);
    procedure DataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
  public
    ParentThread: TFpHttpClientThread;
    SynchronizationCS: TCriticalSection;
    Stream: TMemoryStream;
    MimeTypeFromUrl: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Thread that downloads using FpHttpClient. }
  TFpHttpClientThread = class(TThread)
  public
    { Set these before Execute }
    Url: String;
    HttpMethod: THttpMethod;
    SynchronizationCS: TCriticalSection;

    { Updated even during downloading. }
    DownloadedBytes, TotalBytes: Int64;
    MimeType: String;

    { Once we're finished (Running = false) then StatusSuccess informs about
      whether we have dsSuccess or dsError.
      When @true, use Stream (set if to @nil Stream, to avoid freeing it here).
      When @false, ErrorMessage is the error message. }
    Success: Boolean;
    Stream: TMemoryStream;
    ErrorMessage: String;
    constructor Create(const EnableThreading: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TUrlAsynchronousReader descendant that implements http downloading. }
  TFpHttpClientReader = class(TUrlAsynchronousReader)
  strict private
    Thread: TFpHttpClientThread;
    SynchronizationCS: TCriticalSection;
    procedure SynchronizeFromFinishedThread;
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
  // TODO: Not safe from thread:
  WritelnLog('Network', 'Following HTTP redirect to "%s"', [ADest]);
end;

procedure TCastleFpHttpClient.DataReceived(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  SynchronizationCS.Acquire;
  try
    ParentThread.DownloadedBytes := Max(Stream.Size, 0);

    if ContentLength = 0 then
      { FpHttpClient leaves ContentLength as 0 in case of chunked HTTP response.
        We want to treat it as -1 (unknown) then.
        Testcase: examples/network/asynchronous_download/ with
        https://castle-engine.io/modern_pascal_introduction.html (FPC 3.2.0 on Linux/x86_64). }
      ParentThread.TotalBytes := -1
    else
      { Otherwise ContentLength is exactly what we need for TotalBytes.
        Note: FpHttpClient may set ContentLength to -1 if unknown
        (not specified in headers), which is exactly what we want. }
      ParentThread.TotalBytes := ContentLength;
  finally SynchronizationCS.Release end;

  {$ifndef VER3_0_2}
  if ParentThread.Terminated then
    Terminate;
  {$endif}
end;

function TCastleFpHttpClient.ReadResponseHeaders: Integer;

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
  MimeType: String;
begin
  Result := inherited;

  { Determine ParentThread.MimeType.
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
      ParentThread.MimeType := MimeType;
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

{ TFpHttpClientThread -------------------------------------------------------- }

constructor TFpHttpClientThread.Create(const EnableThreading: Boolean);
begin
  { Using EnableThreading = false is a hack used when ForceSynchronous = true.
    This is useful to instantiate TFpHttpClientThread when OS doesn't support
    threads, or they not linked in (e.g. no CThreads on Unix).
    In this case we can at least download synchronously, which sometimes is enough. }
  if EnableThreading then
    inherited Create(true);
  Stream := TMemoryStream.Create;
  TotalBytes := -1;
end;

destructor TFpHttpClientThread.Destroy;
begin
  FreeAndNil(Stream);
  inherited;
end;

procedure TFpHttpClientThread.Execute;
var
  Client: TCastleFpHttpClient;
begin
  try
    Client := TCastleFpHttpClient.Create(nil);
    try
      Client.ParentThread := Self;
      Client.SynchronizationCS := SynchronizationCS;
      Client.Stream := Stream;
      Client.AllowRedirect := true;
      Client.MimeTypeFromUrl := URIMimeType(Url);
      Client.KeepConnection := false; // used only once, so KeepConnection not necessary
      case HttpMethod of
        hmGet: Client.Get(Url, Stream);
        hmPost: Client.Post(Url, Stream);
        hmPut: Client.Put(Url, Stream);
        hmDelete: Client.Delete(Url, Stream);
        hmOptions: Client.Options(Url, Stream);
        hmHead: //Client.Head(Url);
          Client.HTTPMethod('HEAD', Url, nil, [200]);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('HttpMethod=?');
        {$endif}
      end;
    finally FreeAndNil(Client) end;

    Stream.Position := 0; { rewind for easy reading }

    Success := true;
  except
    on E: TObject do
    begin
      Success := false;
      ErrorMessage := Format('Error when downloading "%s": ', [URIDisplay(Url)]) + ExceptMessage(E);
    end;
  end;
end;

{ TFpHttpClientReader --------------------------------------------------- }

procedure TFpHttpClientReader.Start;
begin
  inherited;

  { The main purpose of this is to disable Download from blocking.
    Additionally, it also disables TCastleDownload from "downgrading" to blocking,
    when OS doesn't support threads. }
  if ForceSynchronous and (not EnableBlockingDownloads) then
  begin
    Status := dsError;
    ErrorMessage := 'Downloading network resources (from "http" or "https" protocols) is not enabled';
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

  Thread := TFpHttpClientThread.Create(not ForceSynchronous);
  Thread.Url := Url;
  Thread.SynchronizationCS := SynchronizationCS;
  Thread.HttpMethod := HttpMethod;
  if ForceSynchronous then
  begin
    try
      Thread.Execute; // execute without any threads, immediately free afterwards
      SynchronizeFromFinishedThread;
    finally FreeAndNil(Thread) end;
  end else
    Thread.Start;
end;

destructor TFpHttpClientReader.Destroy;
begin
  if Thread <> nil then
  begin
    Thread.Terminate;

    if not Thread.Finished then
    begin
      { TODO: This will leak memory if Running changed from true to false between these 2 lines.
        Then thread instance will not be freed, as ThreadProc (running Execute,
        and potentially freeing thread afterwards) has already finished
        before we set FreeOnTerminate.

        We cannot fix it by doing "Thread.FreeOnTerminate := true" earlier,
        because then we have a race, and "Thread" reference may no longer be used.

        A potential fix is to do just:

          Thread.Terminate;
          Thread.WaitFor;
          FreeAndNil(Thread);

        but this:
        1. would cause short delay when interrupting downloads
        2. doesn't work to prevent visible mem leak in examples/network/asynchronous_download/
           (but that leak seems unrelated? it occurs even if nothing was interrupted?)
      }
      WritelnLog('Interrupting download in-progress of "%s"', [URIDisplay(Url)]);
      Thread.FreeOnTerminate := true;
      Thread := nil;
      SynchronizationCS := nil; // do not free it here, let Thread use it
    end else
      FreeAndNil(Thread);
  end;

  FreeAndNil(SynchronizationCS);

  inherited;
end;

procedure TFpHttpClientReader.SynchronizeFromFinishedThread;
begin
  // synchronize one last time from Thread, when it finished work in Execute
  DownloadedBytes := Thread.DownloadedBytes;
  TotalBytes := Thread.TotalBytes;
  MimeType := Thread.MimeType;
  if Thread.Success then
  begin
    Status := dsSuccess;
    MimeType := Thread.MimeType;
    Stream := Thread.Stream;
    Thread.Stream := nil; // do not free our Stream inside TFpHttpClientThread destructor anymore
    WritelnLog('Network', 'Successfully downloaded "%s", MIME type "%s"', [
      Url,
      MimeType
    ]);
  end else
  begin
    Status := dsError;
    ErrorMessage := Thread.ErrorMessage;
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
        DownloadedBytes := Thread.DownloadedBytes;
        TotalBytes := Thread.TotalBytes;
        MimeType := Thread.MimeType;
      finally SynchronizationCS.Release end;
    end else
    begin
      SynchronizeFromFinishedThread;
    end;
  end;
end;

{$endif HAS_FP_HTTP_CLIENT}

{$endif}
