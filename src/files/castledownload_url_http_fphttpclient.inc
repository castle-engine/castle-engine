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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { Thread that downloads using FpHttpClient. }
  TFpHttpClientThread = class(TThread)
  public
    Url: String; //< set before Execute

    DownloadedBytes, TotalBytes: Int64;

    { Once we're finished (Running = false) then StatusSuccess informs about
      whether we have dsSuccess or dsError.
      When @true, use Stream and MimeType.
      (set to @nil Stream, to avoid freeing it here).
      When @false, StatusErrorMessage is the error message. }
    Success: Boolean;
    Stream: TMemoryStream;
    MimeType: String;
    ErrorMessage: String;
    constructor Create(const AUrl: String);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TUrlAsynchronousReader descendant that implements http downloading. }
  TFpHttpClientReader = class(TUrlAsynchronousReader)
  strict private
    Thread: TFpHttpClientThread;
  public
    constructor Create(const AUrl: String); override;
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
  { FpHttpClient sets this to -1 if unknown, which is exactly what we want. }
  ParentThread.TotalBytes := ContentLength;
end;

function TCastleFpHttpClient.ReadResponseHeaders: Integer;
begin
  Result := inherited;
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

constructor TFpHttpClientThread.Create(const AUrl: String);
begin
  Url := AUrl;
  Stream := TMemoryStream.Create;
  inherited Create(false);
end;

destructor TFpHttpClientThread.Destroy;
begin
  FreeAndNil(Stream);
  inherited;
end;

procedure TFpHttpClientThread.Execute;

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
  Client: TCastleFpHttpClient;
begin
  try
    Client := TCastleFpHttpClient.Create(nil);
    try
      Client.ParentThread := Self;
      Client.Stream := Stream;
      Client.AllowRedirect := true;

      Client.Get(Url, Stream);

      // Determine MimeType
      // TODO: move to TCastleFpHttpClient.ReadResponseHeaders, to determine earlier
      // Writeln(Client.ResponseHeaders.Text);
      Client.ResponseHeaders.NameValueSeparator := ':';
      MimeType := ContentTypeToMimeType(Client.ResponseHeaders.Values['Content-Type']);
      if MimeType = '' then
        MimeType := URIMimeType(URL);
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

constructor TFpHttpClientReader.Create(const AUrl: String);
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

  {$ifdef CASTLE_SUPPORTS_THREADING}
  Thread := TFpHttpClientThread.Create(Url);
  {$else}
  Status := dsError;
  ErrorMessage := 'Downloading from "http" / "https" on platforms without threads not possible now';
  {$endif}
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
    end else
    begin
      // synchronize one last time from thread
      DownloadedBytes := Thread.DownloadedBytes;
      TotalBytes := Thread.TotalBytes;
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
  end;
end;

{$endif HAS_FP_HTTP_CLIENT}

{$endif}
