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

var
  { Log (through CastleLog) all loading, that is: all calls to @link(Download).
    This allows to easily check e.g. whether the engine is not loading something
    during the game (which usually badly affects the performance). }
  LogAllLoading: boolean = false;

const
  DefaultEnableNetwork = false;

var
  { Can @link(Download) actually use the network.
    As all the downloading is blocking for now, this is initially @false.
    If you want to really use the network, change it to @true. }
  EnableNetwork: boolean = DefaultEnableNetwork;

type
  EDownloadError = class(Exception);
  ESaveError = class(Exception);

  { Options for the @link(Download) function. }
  TStreamOption = (
    { Force result to be a TCustomMemoryStream,
      with contents fully loaded to the memory,
      and freely seekable (you can move back and forth within).
      Without this option, @link(Download) may return other streams,
      for example TFileStream (that may not have good buffering, depending on OS)
      or TBase64DecodingStream (that may not allow seeking).

      Using TCustomMemoryStream means that reading is fast and comfortable,
      but eats memory and doesn't allow to simultaneously read and process
      the contents (the file must be fully loaded, e.g. downloaded from
      the Internet, and ungzipped, before this function returns).
      So use this option only for files that aren't too big.

      For larger files, you usually don't want to use this option,
      instead wrap result in TBufferedReadStream. }
    soForceMemoryStream,

    { Filter the contents through gzip decompression. }
    soGzip
  );
  TStreamOptions = set of TStreamOption;

{ Return a stream to read given URL.
  Returned stream is suitable only for reading, and the initial position
  is always at the beginning.
  Overloaded version also returns a MIME type (or '' if unknown).

  Any errors are reported by raising exceptions.

  A local file URL is always supported,
  without using any networking library. URL without any protocol is always
  treated like a local filename (absolute or relative to current dir),
  so this function can be a drop-in replacement for normal file reading.
  The MIME type for local files is guessed based on their extension.

  A data URI scheme (http://en.wikipedia.org/wiki/Data_URI_scheme)
  is also always supported.
  The MIME type for such content is specified explicitly in URI.
  TODO: right now, soGzip is ignored for data URIs, we never filter them
  through gunzip.

  It also automatically supports protocols to embed script contents:
  ecmascript, javascript (see VRML and X3D specifications),
  castlescript, kambiscript (see https://castle-engine.io/castle_script.php),
  compiled (https://castle-engine.io/x3d_extensions.php#section_ext_script_compiled).
  The MIME type for these is implied by the protocol (like "application/javascript"
  for ecmascript/javascript), and the returned stream simply contains
  script code.

  Set EnableNetwork to @true to support network protocols.
  Right now this means http and (in FPC >= 3.2.0) https, handled by FpHttpClient.
  The MIME type for such content is usually reported by the http server
  (but if the server doesn't report MIME type, we still try to guess it,
  looking at URL using URIMimeType).

  On Android, you should use the "read_external_storage"
  service to be able to read storage files (e.g. from SD card).
  This means files accessed by the 'file' protocol.
  See https://github.com/castle-engine/castle-engine/wiki/Android-Services .

  Note that this is a @italic(synchronous downloader).
  Which means that if you use a network URL (like http://...) then your
  application will wait until the data arrives from the Internet.
  There may be a timeout of the request (so your application will not hang
  indefinitely), but still your code (or user) have no way to cancel or watch
  the progress of this operation.
  This is why network support is disabled by default (see @link(EnableNetwork)).
  This is sometimes acceptable (e.g. if you're
  waiting during the "loading" process, and the data just @italic(has)
  to be downloaded in order to continue), and it's really easy to use
  (you just download data exactly the same way like you open a local file).

  You can use @bold(asynchronous downloading) through the @link(TCastleDownload)
  class instead.

  @raises(EDownloadError In case of problems loading from this URL.)
  @raises(EFOpenError If case opening the underlying file fails,
    raised in case of file:// URLs.)
  @raises(EStreamError If case reading the stream fails,
    raised in case of file:// URLs.)

  @raises(Exception Various TStream classes (used internally by this
    function) may raise various exceptions in case the stream cannot be created
    for reading.
    Right now, we simply let these exceptions to "pass through" from this function
    (instead of catching and re-raising).
    So, to be really safe, be ready that this function may raise @italic(any)
    Exception class.)
}
function Download(const URL: string; const Options: TStreamOptions = []): TStream; overload;
function Download(const URL: string; const Options: TStreamOptions;
  out MimeType: string): TStream; overload;

{$endif}

{$ifdef read_implementation}

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

{$ifdef HAS_FP_HTTP_CLIENT}

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

{$endif HAS_FP_HTTP_CLIENT}

{ Global functions to read --------------------------------------------------- }

{$ifdef HAS_FP_HTTP_CLIENT}

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

{$endif HAS_FP_HTTP_CLIENT}

{$ifdef ANDROID}
type
  TAndroidDownloadService = class
    Finished, FinishedSuccess: boolean;
    TemporaryFileName, ErrorMessage: string;
    function HandleDownloadMessages(const Received: TCastleStringList): boolean;
    function Wait(const URL: string): TStream;
  end;

function TAndroidDownloadService.HandleDownloadMessages(const Received: TCastleStringList): boolean;
begin
  if (Received.Count = 2) and (Received[0] = 'download-error') then
  begin
    Finished := true;
    FinishedSuccess := false;
    ErrorMessage := Received[1];
    Result := true;
  end;
  if (Received.Count = 2) and (Received[0] = 'download-finished') then
  begin
    Finished := true;
    FinishedSuccess := true;
    TemporaryFileName := Received[1];
    Result := true;
  end;
end;

function TAndroidDownloadService.Wait(const URL: string): TStream;
begin
  Finished := false;
  Messaging.OnReceive.Add({$ifdef CASTLE_OBJFPC}@{$endif} HandleDownloadMessages);
  Messaging.Send(['download-url', URL]);
  try
    { TODO: introduce download-progress messages,
       use them to make Progress.Init/Step/Fini calls,
       allowing to show user progress bar during this loop. }
    repeat
      ApplicationProperties._Update; // process CastleMessages
      Sleep(200)
    until Finished;
  finally
    Messaging.OnReceive.Remove({$ifdef CASTLE_OBJFPC}@{$endif} HandleDownloadMessages);
  end;
  if FinishedSuccess then
    Result := TFileStream.Create(TemporaryFileName, fmOpenRead)
  else
    raise Exception.Create(ErrorMessage);
end;
{$endif ANDROID}

{ Load FileName to TMemoryStream. }
function CreateMemoryStream(const FileName: string): TMemoryStream; overload;
begin
  Result := TMemoryStream.Create;
  try
    Result.LoadFromFile(FileName);
    Result.Position := 0; { rewind for easy reading }
  except
    FreeAndNil(Result); raise;
  end;
end;

{ Load Stream to TMemoryStream.
  Sets given Stream to @nil (it is freed by this function). }
function CreateMemoryStream(var Stream: TStream): TMemoryStream; overload;
begin
  Result := TMemoryStream.Create;
  try
    Result.LoadFromStream(Stream);
    FreeAndNil(Stream);
    Result.Position := 0; { rewind for easy reading }
  except
    FreeAndNil(Result); raise;
  end;
end;

{ Decompress gzipped FileName.
  When ForceMemoryStream, always returns TMemoryStream.
  Sets given Stream to @nil (it is owned by us now, possibly it is freed by this function). }
function ReadGzipped(var Stream: TStream; const ForceMemoryStream: boolean): TStream;
var
  NewResult: TMemoryStream;
begin
  Result := TGZFileStream.Create(Stream, false);
  try
    Stream := nil; // Stream is owned by Result now

    if ForceMemoryStream then
    begin
      { TODO: our engine never uses both soGzip and soForceMemoryStream
        for now, so below code path is untested. }
      NewResult := TMemoryStream.Create;
      ReadGrowingStream(Result, NewResult, true);
      FreeAndNil(Result);
      Result := NewResult;
    end;
  except
    FreeAndNil(Result); raise;
  end;
end;

procedure CheckFileAccessSafe(const URL: string);
begin
  if not ApplicationProperties._FileAccessSafe then
    WritelnWarning('Opening file "%s" before the Application.OnInitialize was called. ' +
      'This is not reliable on mobile platforms (Android, iOS). ' +
      'This usually happens if you open a file from the "initialization" section of a unit. ' +
      'You should do it in Application.OnInitialize instead.',
      [URL]);
end;

function Download(const URL: string; const Options: TStreamOptions;
  out MimeType: string): TStream;
var
  P, FileName: string;
  {$ifdef HAS_FP_HTTP_CLIENT}
  NetworkResult: TMemoryStream;
  {$endif}
  FileStream: TFileStream;
  DataURI: TDataURI;
  {$ifdef ANDROID}
  AssetStream: TReadAssetStream;
  DownloadService: TAndroidDownloadService;
  {$endif}
  RegisteredProtocol: TRegisteredProtocol;
  UnderlyingStream: TStream;
const
  MaxRedirects = 32;
begin
{ TODO:

  D := TDownload.Create;
  try
    D.OwnsContents := false;
    D.URL := URL;
    D.Options := Options;
    D.Start;
    D.WaitForFinish;
    Assert(D.Status in [dsError, dsSuccess]);
    if D.Status = dsError then
    begin
      Assert(D.Contents = nil);
      raise EDownloadError.Create(D.ErrorMessage);
    end else
    begin
      Assert(D.Status = dsSuccess);
      Result := D.Contents;
      MimeType := D.MimeType;
    end;
  finally FreeAndNil(D) end;
}

  P := URIProtocol(URL);

  // do not log castle-data:/ protocol, as this just causes recursive call to Download
  if LogAllLoading and (P <> 'castle-data') then
    WritelnLog('Loading', 'Loading "%s"', [URIDisplay(URL)]);

  RegisteredProtocol := RegisteredProtocols.Find(P);

  {$ifdef ANDROID}
  if (P = 'http') or (P = 'https') then
  begin
    if not EnableNetwork then
      raise EDownloadError.Create('Downloading network resources (from "http" or "https" protocols) is not enabled');

    CheckFileAccessSafe(URL);
    WritelnLog('Network', 'Download service started for "%s"', [URIDisplay(URL)]);
    DownloadService := TAndroidDownloadService.Create;
    try
      Result := DownloadService.Wait(URL);
      MimeType := URIMimeType(URL);
    finally
      FreeAndNil(DownloadService);
    end;
  end else
  {$endif ANDROID}

  {$ifdef HAS_FP_HTTP_CLIENT}
  { network protocols: get data into a new TMemoryStream using FpHttpClient }
  if (P = 'http') or (P = 'https') then
  begin
    if not EnableNetwork then
      raise EDownloadError.Create('Downloading network resources (from "http" or "https" protocols) is not enabled');

    {$ifdef VER3_0}
    if P = 'https' then
      { Testcase: FPC 3.0.4, Linux/x86_64:
        TFPCustomHTTPClient fails with Access Violation on https URLs.
        TODO: Test on Windows/x86_64. }
      raise EDownloadError.Create('Downloading using "https" protocol does not work when the application is compiled with FPC 3.0.x. Use newer FPC (and add OpenSSLSockets unit to the uses clause).');
    {$endif}

    CheckFileAccessSafe(URL);
    WritelnLog('Network', 'Downloading "%s"', [URIDisplay(URL)]);
    NetworkResult := NetworkDownload(URL, MaxRedirects, MimeType);
    try
      if soGzip in Options then
        Result := ReadGzipped(TStream(NetworkResult),
          soForceMemoryStream in Options) else
        Result := NetworkResult;
    except
      FreeAndNil(NetworkResult); raise;
    end;
  end else
  {$endif HAS_FP_HTTP_CLIENT}

  { local filenames are directly handled, without the need for any downloader }
  if (P = '') or (P = 'file') then
  begin
    CheckFileAccessSafe(URL);

    FileName := URIToFilenameSafe(URL);
    if FileName = '' then
      raise EDownloadError.CreateFmt('Cannot convert URL "%s" to filename', [URL]);

    if soGzip in Options then
    begin
      FileStream := TFileStream.Create(FileName, fmOpenRead);
      Result := ReadGzipped(TStream(FileStream), soForceMemoryStream in Options);
    end else
    if soForceMemoryStream in Options then
      Result := CreateMemoryStream(FileName) else
      Result := TFileStream.Create(FileName, fmOpenRead);
    MimeType := URIMimeType(URL);
  end else

  { castle-android-assets: to access Android assets }
  if (P = 'assets') or (P = 'castle-android-assets') then
  begin
    CheckFileAccessSafe(URL);
    {$ifdef ANDROID}
    AssetStream := TReadAssetStream.Create(URIToAssetPath(URL));
    try
      if soGzip in Options then
        Result := ReadGzipped(TStream(AssetStream), soForceMemoryStream in Options) else
      if soForceMemoryStream in Options then
        Result := CreateMemoryStream(TStream(AssetStream)) else
        Result := AssetStream;
    except
      FreeAndNil(AssetStream); raise;
    end;
    MimeType := URIMimeType(URL);
    {$else}
    raise EDownloadError.CreateFmt('Cannot download an asset URL, because we are not compiled for Android: %s',
      [URL]);
    {$endif}
  end else

  { castle-data: to access application data, https://castle-engine.io/manual_data_directory.php }
  if P = 'castle-data' then
  begin
    Result := Download(ResolveCastleDataURL(URL), Options, MimeType);
  end else

  { data: URI scheme }
  if P = 'data' then
  begin
    DataURI := TDataURI.Create;
    try
      DataURI.URI := URL;
      DataURI.ForceMemoryStream := soForceMemoryStream in Options;
      if not DataURI.Valid then
        raise EDownloadError.Create('Invalid data: URI scheme');
      Result := DataURI.ExtractStream;
      MimeType := DataURI.MimeType;
      Assert(Result <> nil, 'DataURI.ExtractStream must be non-nil when DataURI.Valid is true');
    finally FreeAndNil(DataURI) end;
  end else

  if (P = 'ecmascript') or
     (P = 'javascript') then
  begin
    { This ignores soGzip in Options, as it's not used by anything. }
    MimeType := 'application/javascript';
    Result := MemoryStreamLoadFromString(URIDeleteProtocol(URL));
  end else

  if (P = 'castlescript') or
     (P = 'kambiscript') then
  begin
    { This ignores soGzip in Options, as it's not used by anything. }
    MimeType := 'text/x-castlescript';
    Result := MemoryStreamLoadFromString(URIDeleteProtocol(URL));
  end else

  if P = 'compiled' then
  begin
    { This ignores soGzip in Options, as it's not used by anything. }
    MimeType := 'text/x-castle-compiled';
    Result := MemoryStreamLoadFromString(URIDeleteProtocol(URL));
  end else

  { Check RegisteredProtocol at the end, since above we handle some protocols.
    TODO: Move all protocol reading to appropriate callbacks registered
    FRegisteredProtocols.Add. }
  if RegisteredProtocol <> nil then
  begin
    if Assigned(RegisteredProtocol.ReadEvent) then
    begin
      UnderlyingStream := RegisteredProtocol.ReadEvent(URL, MimeType);
      // unpack gzip if requested
      if soGzip in Options then
        Result := ReadGzipped(UnderlyingStream, soForceMemoryStream in Options)
      else
      if soForceMemoryStream in Options then
        Result := CreateMemoryStream(UnderlyingStream)
      else
        Result := UnderlyingStream;
    end else
      raise EDownloadError.CreateFmt('Cannot read URLs with protocol "%s"', [P]);
  end else

  begin
    raise EDownloadError.CreateFmt('Downloading from protocol "%s" is not supported', [P]);
  end;
end;

function Download(const URL: string; const Options: TStreamOptions): TStream;
var
  MimeType: string;
begin
  Result := Download(URL, Options, MimeType { ignored });
end;

{$endif}
