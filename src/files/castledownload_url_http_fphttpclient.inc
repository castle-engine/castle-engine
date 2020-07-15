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
    Stream: TMemoryStream;
    MimeTypeFromUrl: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Thread that downloads using FpHttpClient. }
  TFpHttpClientThread = class(TThread)
  public
    Url: String; //< set before Execute

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
    procedure SynchronizeFromFinishedThread;
  public
    constructor Create(const AUrl: String; ForceSynchronous: Boolean); override;
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
  ParentThread.DownloadedBytes := Max(Stream.Size, 0);

  if ContentLength = 0 then
    { FpHttpClient leaves ContentLength as 0 in case of chunked HTTP response.
      We want to treat it as -1 (unknown) then.
      Testcase: examples/network/asynchronous_download/ with
      https://castle-engine.io/modern_pascal_introduction.html . }
    ParentThread.TotalBytes := -1
  else
    { Otherwise ContentLength is exactly what we need for TotalBytes.
      Note: FpHttpClient may set ContentLength to -1 if unknown
      (not specified in headers), which is exactly what we want. }
    ParentThread.TotalBytes := ContentLength;
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
    ParentThread.MimeType := MimeType;
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
      Client.Stream := Stream;
      Client.AllowRedirect := true;
      Client.MimeTypeFromUrl := URIMimeType(Url);
      Client.Get(Url, Stream);
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

constructor TFpHttpClientReader.Create(const AUrl: String; ForceSynchronous: Boolean);
begin
  inherited;

  if not EnableNetwork then
  begin
    Status := dsError;
    ErrorMessage := 'Downloading network resources (from "http" or "https" protocols) is not enabled';
    Exit;
  end;

  {$ifdef VER3_0}
  if URIProtocol(Url) = 'https' then
  begin
    { Testcase: FPC 3.0.4, Linux/x86_64:
      TFPCustomHTTPClient fails with Access Violation on https URLs.
      TODO: Test on Windows/x86_64. }
    Status := dsError;
    ErrorMessage := 'Downloading using "https" protocol does not work when the application is compiled with FPC 3.0.x. Use newer FPC (and add OpenSSLSockets unit to the uses clause).';
    Exit;
  end;
  {$endif}

  CheckFileAccessSafe(Url);
  WritelnLog('Network', 'Downloading "%s"', [URIDisplay(Url)]);

  { ForceSynchronous allows to make synchronous Download work,
    and TCastleDownload "downgrade" to working synchronously (instead of just failing) when
    - binary has no thread support linked (e.g. no CThreads linked on Unix).
    - OS has no thread support. }
  {$ifndef CASTLE_SUPPORTS_THREADING}
  ForceSynchronous := true;
  {$endif}

  Thread := TFpHttpClientThread.Create(not ForceSynchronous);
  Thread.Url := Url;
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
        Then thread instance will not be freed, as "OnTerminate" will not occur
        (unless TThread is smart and accounts for it?). }
      WritelnLog('Interrupting download in-progress of "%s"', [URIDisplay(Url)]);
      Thread.FreeOnTerminate := true;
      Thread := nil;
    end else
      FreeAndNil(Thread);
  end;

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
      // TODO synchronize safely
      DownloadedBytes := Thread.DownloadedBytes;
      TotalBytes := Thread.TotalBytes;
      MimeType := Thread.MimeType;
    end else
    begin
      SynchronizeFromFinishedThread;
    end;
  end;
end;

{$endif HAS_FP_HTTP_CLIENT}

{$endif}
