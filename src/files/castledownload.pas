{
  Copyright 2013-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Read and write stream contents from URLs. }
unit CastleDownload;

{$I castleconf.inc}

interface

uses SysUtils, Classes {$ifdef HAS_FP_HTTP_CLIENT}, FpHttpClient {$endif},
  CastleVectors;

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
    { Force result to be a TMemoryStream,
      with contents fully loaded to the memory,
      and freely seekable (you can move back and forth within).
      Without this option, @link(Download) may return other streams,
      for example TFileStream (that may not have good buffering, depending on OS)
      or TBase64DecodingStream (that may not allow seeking).

      Using TMemoryStream means that reading is fast and comfortable,
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

  { Options for the @link(URLSaveStream) function. }
  TSaveStreamOption = (
    { Filter the contents through gzip compression. }
    ssoGzip
  );
  TSaveStreamOptions = set of TSaveStreamOption;

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

  Set EnableNetwork to @true to have also support for network protocols.
  Right now this means only http, handled by FpHttpClient.
  The MIME type for such content is usually reported by the http server
  (but if the server doesn't report MIME type, we still try to guess it,
  looking at URL using URIMimeType).

  On Android, URLs that indicate assets (files packaged inside .apk)
  are also supported, as @code(assets:/my_file.png).

  Note if you use a network URL (like http://...) and you read from this
  stream that you make a @bold(synchronous downloader).
  Which means that your application will wait until the data arrives
  from the Internet. This is why http:// support is disabled here
  by default (see @link(EnableNetwork)).
  This is sometimes acceptable (e.g. if you're
  waiting during the "loading" process, and the data just @italic(has)
  to be downloaded in order to continue), and it's really easy to use
  (you just download data exactly the same way like you open a local file).
  But often you may want to instead use @bold(asynchronous downloading)
  -- we plan to implement it by the class TDownload (see comments
  in CastleDownload code), but it's not ready yet!

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
function Download(const URL: string; const Options: TStreamOptions = []): TStream;
function Download(const URL: string; const Options: TStreamOptions;
  out MimeType: string): TStream;

(* TODO: API for asynchronous downloader is below, not implemented yet.

type
  TDownloadStatus = (dsWaiting, dsError, dsSuccess);

  { Download an URL asynchronously, without blocking the application.
    You can register a callback @link(OnFinished) or watch
    when the @link(Status) property changes to dsError or dsSuccess
    to detect when this finished downloading.

    The download starts when you construct the instance of this class.
    When it ends, the @link(OnFinished) is called and @link(Status) changes.
    You can also always just free an instance of this class, this will
    break the download immediately.

    Do not worry whether this uses threads (or not) internally.
    All the methods and properties of this class should be accessed
    from the main thread, the same thread you use for all Castle Game Engine
    functions. And the OnFinished is called in the main thread,
    so you can handle it without worrying about threading.
  }
  TDownload = class
    { Start the download process. This constructor starts downloading,
      and immediately returns.

      Note that this constructor never calls OnFinished or changes @link(Status)
      immediately (this would be quite uncomfortable,
      as you didn't have a chance to set OnFinished yet...).
      Even if you provide a URL that is a file:// or data URI
      (which can be read / decoded immediately). }
    constructor Create(const AURL: string);

    property URL: string read;

    { Event called when we finish downloading. }
    property OnFinished: TNotifyEvent read write;

    { Whether we finished the download or not, and if we finished
      -- whether we finished with an error or a success. }
    property Status: TDownloadStatus read ;

    { If the @link(Status) is dsError, this contains a detailed error message. }
    property ErrorMessage: string read ;

    { If the @link(Status) is dsSuccess, this contains the downloaded contents.
      This stream is owned by default (if ContentOwned) by this TDownload instance,
      so it will become invalid when the TDownload instance is freed. }
    property Content: TStream read;

    { Is the @link(Content) owned by this @link(TDownload) instance.
      Set this to @false to be able to free this TDownload instance
      (maybe even by setting @link(FreeOnFinish) to @true),
      and still keep the stream reference.
      It is your responsibility then to keep and free the @link(Content)
      stream whenever you want. }
    // Is this property really much useful? Possibly resign from this.
    property ContentOwned: boolean read;

    { How many bytes were downloaded.
      Together with @link(TotalBytes), you can use it e.g. to show a progress bar
      when downloading. }
    property DownloadedBytes: Int64;

    { How many bytes are expected to be downloaded, in total.
      -1 if unknown.
      Depending on the server answer, this may be known fairly quickly after
      starting the download, or if may not be known at all (until we finish
      the download).
      It's guaranteed that this is know (not -1) when @link(Status) = dsSuccess,
      in all other cases always be prepared that this may be equal -1. }
    property TotalBytes: Int64 read;

    { Automatically free the instance of this class when the download is finished
      (right after @link(OnFinished) is called). This is useful if you don't
      want to be bothered with managing the memory usage of this TDownload
      instance.

      Just like with TThread.FreeOnTerminate, be extremely careful after you
      have set this property to true: the instance of TDownload
      may become invalid at any moment (even right after when you have set
      the FreeOnFinish to @true, in case the contents were already downloaded).
      It is always freed in the main CGE thread (to avoid risk when iterating
      over @link(Downloads) list).

      You should always register the OnFinished callback before setting
      this to @true, to be notified when the download finishes.
      The OnFinished callback will be your last chance to get the downloaded
      data (unless you set FreeOnFinish := @false inside OnFinished
      implementation, which is allowed). }
    // Maybe resign from this property, it is dangerous -- is it really much useful?
    property FreeOnFinish: boolean read write default false;
  end;

  TDownloadList = specialize TObjectList<TDownload>;

{ List of currently existing TDownload instances.
  You can use this e.g. to implement a GUI to show ongoing downloads.
  Remember that some downloads on this list may be
function Downloads: TDownloadList;
*)

{ Create a stream to save a given URL, for example create a TFileStream
  to save a file for a @code(file) URL. In other words, perform @italic(upload).
  Right now, this only works for @code(file) URLs, and the only advantage
  it has over manually creating TFileStream is that this accepts URLs.

  @raises(ESaveError In case of problems saving this URL.)

  @raises(Exception Various TStream instances (used internally by this
    function) may raise exceptions in case the stream cannot be created
    for saving.
    Right now, we simply let these exceptions to "pass through" from this function
    (instead of catching and re-raising).
    So be ready that this function may raise @italic(any) Exception class.)
}
function URLSaveStream(const URL: string; const Options: TSaveStreamOptions = []): TStream;

var
  { Log (through CastleLog) all loading, that is: all calls to @link(Download).
    This allows to easily check e.g. whether the engine is not loading something
    during the game (which usually badly affects the performance). }
  LogAllLoading: boolean = false;

{ Open a proper stream to read a file, fast (with buffering) and with seeking.
  This gives you a stream most comfortable for reading (buffering means
  that you can read small, comfortable pieces of it; seeking means
  you can jump freely to various file positions, back and forward).

  On different OSes or even compilers this may require a little different
  stream, so it's safest to just use this function. For example,
  traditional Classes.TFileStream doesn't do buffering. Although under Linux,
  the buffering of file handles is done at kernel level (so everything
  works fast), on Windows the slowdown is noticeable.
  This function will always create
  proper stream descendant, eventually wrapping some standard stream
  in a buffered stream with full seeking capability.

  @deprecated Instead of this, use CastleDownload.Download with
  LocalFileInMemory. }
function CreateReadFileStream(const URL: string): TStream; deprecated;

{ Save the contents of given Stream to an URL. }
procedure StreamSaveToFile(Stream: TStream; const URL: string);

{ ---------------------------------------------------------------------------- }
{ @section(Text reading) }

type
  { Common class for reading or writing a stream like a text file. }
  TTextReaderWriter = class
  private
    FOwnsStream: boolean;
    FStream: TStream;
  public
    { Open a stream. If AOwnsStream then in destructor we will free
      given AStream object. }
    constructor Create(AStream: TStream; AOwnsStream: boolean); overload;
    destructor Destroy; override;
  end;

  { Read any stream like a text file. You can read an arbitrary TStream
    instance, or you can read an URL. Reading from URL supports all kinds
    of URL protocols supportted by @link(Download),
    including @code(file), @code(http) and Android @code(assets)
    (see https://castle-engine.io/tutorial_network.php ).

    Includes comfortable @link(Readln) routine to read line by line
    (lines may be terminated in any OS convention).
    Includes comfortable @link(Read) to read next non-whitespace
    characters, @link(ReadInteger) to read next integer and such.

    Do not use the underlying stream once you started reading it with
    this class. We will move the position within this stream ourselves. }
  TTextReader = class(TTextReaderWriter)
  private
    ReadBuf: string;
    { Try to read more data from underlying stream and add it to ReadBuf.
      Returns if we succeded, @false means that the stream ends.
      When it returns @true, you can be sure that Length(ReadBuf) increased. }
    function IncreaseReadBuf: boolean;
  public
    { Download and open a file. }
    constructor Create(const URL: string); overload;

    { Read next line from Stream. Returned string does not contain
      any end-of-line characters. }
    function Readln: string;

    { Read the next string of non-whitespace characters.
      This skips any whitespace (including newlines) we currently see,
      then reads all non-whitespace characters as far as it can.
      It does not consume any whitespace characters after the string.

      Returns empty string if and only if the stream ended.
      Otherwise, returns the read non-whitespace characters. }
    function Read: string;

    { Read the next Integer from stream. Reads next string of non-whitespace
      characters, like @link(Read), and then converts it to Integer.

       @raises(EConvertError If the next non-whitespace string
         cannot be converted to Integer. This includes situations
         when stream ended (@link(Read) would return empty string in this
         case).)  }
    function ReadInteger: Integer;

    { Read the next Single value from stream.
      Reads next string of non-whitespace
      characters, like @link(Read), and then converts it to Single.

       @raises(EConvertError If the next non-whitespace string
         cannot be converted to Single. This includes situations
         when stream ended (@link(Read) would return empty string in this
         case).)  }
    function ReadSingle: Single;

    { Read the next vector from a stream, simply reading 3 Single values
      in sequence.

       @raises(EConvertError If one of the components cannot be converted
         to Single, or when stream ended prematurely.) }
    function ReadVector3: TVector3;
    function ReadVector3Single: TVector3; deprecated 'use ReadVector3';

    function Eof: boolean;
  end;

  { Write to a stream like to a text file.

    You can write to an arbitrary TStream instance,
    or you can write to an URL. Writing to an URL supports all
    URL protocols supportted by @link(URLSaveStream), which for now
    doesn't include much: only @code(file) protocol. But it will produce
    a nice exception message in case of unsupprted URL protocol. }
  TTextWriter = class(TTextReaderWriter)
  public
    constructor Create(const URL: string); overload;
    procedure Write(const S: string); overload;
    procedure Write(const S: string; const Args: array of const); overload;
    procedure Writeln(const S: string = ''); overload;
    procedure Writeln(const S: string; const Args: array of const); overload;
  end;

  TStringsHelper = class helper for TStrings
    { Load the contents from URL.
      Uses Castle Game Engine @link(Download) to get the contents,
      then uses standard LoadFromStream to load them. }
    procedure LoadFromURL(const URL: string);

    { Save the contents to URL.
      Uses standard SaveToStream combined with
      Castle Game Engine @link(URLSaveStream) to save the contents. }
    procedure SaveToURL(const URL: string);
  end;

implementation

uses URIParser, Math,
  CastleURIUtils, CastleUtils, CastleLog, CastleInternalZStream,
  CastleClassUtils, CastleDataURI, CastleProgress, CastleStringUtils,
  CastleApplicationProperties
  {$ifdef ANDROID}, CastleAndroidInternalAssetStream, CastleMessaging {$endif};

{ TProgressMemoryStream ------------------------------------------------------ }

type
  { TMemoryStream descendant that shows a progress bar when writing to it. }
  TProgressMemoryStream = class(TMemoryStream)
  private
    UseProgress: boolean;
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

function TProgressMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited;
  if UseProgress then Progress.Position := Max(Size, 0);
end;

{ TCastleHTTPClient ---------------------------------------------------------- }

{$ifdef HAS_FP_HTTP_CLIENT}

type
  { HTTP client. In addition to TFPHTTPClient, it handles a progress bar
    initialization and finalization. }
  TCastleHTTPClient = class(TFPHTTPClient)
  private
    UseProgress: boolean;
    ProgressStream: TProgressMemoryStream;
    ProgressTitle: string;
    procedure FinishProgress;
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
  Const CL ='content-length:';
  Var
    S : String;
    I : integer;
  begin
    Result:=-1;
    I:=0;
    While (Result=-1) and (I<ResponseHeaders.Count) do
      begin
      S:=Trim(LowerCase(ResponseHeaders[i]));
      If (Copy(S,1,Length(Cl))=Cl) then
        begin
        System.Delete(S,1,Length(CL));
        Result:=StrToIntDef(Trim(S),-1);
        end;
      Inc(I);
      end;
  end;

begin
  Result := inherited;

  UseProgress := not Progress.Active;
  ProgressStream.UseProgress := UseProgress;

  { Initialize progress.
    We always add 1 step done right after downloading, this way
    we at least show *something* even when Content-Length is unknown. }
  if UseProgress then
    Progress.Init(Max(CheckContentLength, 0) + 2, ProgressTitle, true);
end;

procedure TCastleHTTPClient.FinishProgress;
begin
  if UseProgress then
  begin
    UseProgress := false;
    { Make sure that after TCastleHTTPClient is destroyed,
      ProgressStream behaves like a regular TMemoryStream.
      That is because ProgressStream may be returned to the called
      of Download function, that can do potentially anything with it. }
    if ProgressStream <> nil then
      ProgressStream.UseProgress := false;
    Progress.Fini;
  end;
end;

procedure TCastleHTTPClient.DisconnectFromServer;
begin
  if UseProgress then Progress.Step;
  inherited;
  FinishProgress;
end;

destructor TCastleHTTPClient.Destroy;
begin
  { Usually DisconnectFromServer should cause FinishProgress.
    But in case of some exception, make sure to finalize progress here. }
  FinishProgress;
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
      Client.ProgressTitle := 'Downloading ' + URL;
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
  Messaging.OnReceive.Add(@HandleDownloadMessages);
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
    Messaging.OnReceive.Remove(@HandleDownloadMessages);
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

{ Load (and free and nil) Stream to TMemoryStream. }
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
  When ForceMemoryStream, always returns TMemoryStream. }
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
    WritelnWarning('Opening file "%s" before the Application.OnInitialize was called. This is not reliable on mobile platforms (Android, iOS). This usually happens if you open a file from the "initialization" section of a unit. You should do it in Application.OnInitialize instead.',
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
const
  MaxRedirects = 32;
begin
  P := URIProtocol(URL);

  if LogAllLoading and Log then
    WritelnLog('Loading', 'Loading "%s"', [URIDisplay(URL)]);

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

function CreateReadFileStream(const URL: string): TStream;
begin
  Result := Download(URL, [soForceMemoryStream]);
end;

{ Global functions to save --------------------------------------------------- }

function URLSaveStream(const URL: string; const Options: TSaveStreamOptions): TStream;
var
  P, FileName: string;
begin
  P := URIProtocol(URL);
  if P = '' then
    FileName := URL else
  if P = 'file' then
  begin
    FileName := URIToFilenameSafe(URL);
    if FileName = '' then
      raise ESaveError.CreateFmt('Cannot convert URL to a filename: "%s"', [URL]);
  end else
    raise ESaveError.CreateFmt('Saving of URL with protocol "%s" not possible', [P]);
  if ssoGzip in Options then
  begin
    Result := TGZFileStream.Create(TFileStream.Create(FileName, fmCreate), true);
  end else
    Result := TFileStream.Create(FileName, fmCreate);
end;

procedure StreamSaveToFile(Stream: TStream; const URL: string);
const
  BufSize = 100000;
var
  S : TStream;
  Buffer: Pointer;
  ReadCount: Integer;
begin
  { optimized implementation for TMemoryStream }
  if Stream is TMemoryStream then
  begin
    TMemoryStream(Stream).SaveToFile(URIToFilenameSafe(URL));
    Exit;
  end;

  Buffer := GetMem(BufSize);
  try
    S := URLSaveStream(URL);
    try
      repeat
        ReadCount := Stream.Read(Buffer^, BufSize);
        if ReadCount = 0 then
          Break else
          S.WriteBuffer(Buffer^, ReadCount);
      until false;
    finally
      S.free;
    end;
  finally FreeMem(Buffer) end;
end;

{ TTextReaderWriter ---------------------------------------------------------- }

constructor TTextReaderWriter.Create(AStream: TStream; AOwnsStream: boolean);
begin
  inherited Create;
  FStream := Astream;
  FOwnsStream := AOwnsStream;
end;

destructor TTextReaderWriter.Destroy;
begin
  if FOwnsStream then FStream.Free;
  inherited;
end;

{ TTextReader ---------------------------------------------------------------- }

constructor TTextReader.Create(const URL: string);
begin
  inherited Create(Download(URL), true);
end;

function TTextReader.IncreaseReadBuf: boolean;
const
  BufferIncrease = 100;
var
  ReadCnt: Integer;
begin
  SetLength(ReadBuf, Length(ReadBuf) + BufferIncrease);
  ReadCnt := FStream.Read(ReadBuf[Length(ReadBuf) - BufferIncrease + 1], BufferIncrease);
  SetLength(ReadBuf, Length(ReadBuf) - BufferIncrease + ReadCnt);
  Result := ReadCnt <> 0;
end;

function TTextReader.Readln: string;
var
  I, DeleteCount: integer;
begin
  I := 1;

  { Note that ReadBuf may contain data that we
    already read from stream at some time but did not returned it to
    user of this class
    (because we realized we have read too much). }

  repeat
    if (I > Length(ReadBuf)) and not IncreaseReadBuf then
    begin
      Result := ReadBuf;
      ReadBuf := '';
      Exit;
    end;

    if ReadBuf[I] in [#10, #13] then
    begin
      Result := Copy(ReadBuf, 1, I - 1);
      DeleteCount := I;

      { If this is followed by 2nd newline character, we want to consume it.
        To do this, we may have to increase ReadBuf. }
      if ( (I < Length(ReadBuf)) or IncreaseReadBuf ) and
         { check we have #13 followed by #10 or #10 followed by #13.
           Be careful to *not* eat #10 followed by #10, as that would
           make us silently consume empty lines in files with Unix line ending. }
         ( ( (ReadBuf[I] = #10) and (ReadBuf[I + 1] = #13) ) or
           ( (ReadBuf[I] = #13) and (ReadBuf[I + 1] = #10) ) ) then
        Inc(DeleteCount);

      Delete(ReadBuf, 1, DeleteCount);
      Exit;
    end;

    Inc(I);
  until false;
end;

function TTextReader.Read: string;
var
  Start, I: integer;
begin
  I := 1;

  repeat
    if (I > Length(ReadBuf)) and not IncreaseReadBuf then
      Exit('');
    if not (ReadBuf[I] in WhiteSpaces) then
      Break;
    Inc(I);
  until false;

  Start := I;

  repeat
    if (I > Length(ReadBuf)) and not IncreaseReadBuf then
      Break;
    if ReadBuf[I] in WhiteSpaces then
      Break;
    Inc(I);
  until false;

  Dec(I); { we know we're 1 position too far now }
  Assert(I > 0);
  Result := CopyPos(ReadBuf, Start, I);
  Delete(ReadBuf, 1, I);
end;

function TTextReader.ReadInteger: Integer;
begin
  Result := StrToInt(Read);
end;

function TTextReader.ReadSingle: Single;
begin
  Result := StrToFloat(Read);
end;

function TTextReader.ReadVector3: TVector3;
begin
  Result[0] := ReadSingle;
  Result[1] := ReadSingle;
  Result[2] := ReadSingle;
end;

function TTextReader.ReadVector3Single: TVector3;
begin
  Result := ReadVector3;
end;

function TTextReader.Eof: boolean;
begin
  Result := (ReadBuf = '') and not IncreaseReadBuf;
end;

{ TTextWriter ---------------------------------------------------------------- }

constructor TTextWriter.Create(const URL: string);
begin
  inherited Create(URLSaveStream(URL), true);
end;

procedure TTextWriter.Write(const S: string);
begin
  WriteStr(FStream, S);
end;

procedure TTextWriter.Writeln(const S: string);
begin
  WritelnStr(FStream, S);
end;

procedure TTextWriter.Write(const S: string; const Args: array of const);
begin
  WriteStr(FStream, Format(S, Args));
end;

procedure TTextWriter.Writeln(const S: string; const Args: array of const);
begin
  WritelnStr(FStream, Format(S, Args));
end;

{ TStringsHelper ---------------------------------------------------------- }

procedure TStringsHelper.LoadFromURL(const URL: string);
var
  Stream: TStream;
begin
  Stream := Download(URL);
  try
    LoadFromStream(Stream);
  finally FreeAndNil(Stream) end;
end;

procedure TStringsHelper.SaveToURL(const URL: string);
var
  Stream: TStream;
begin
  Stream := URLSaveStream(URL);
  try
    SaveToStream(Stream);
  finally FreeAndNil(Stream) end;
end;

end.
