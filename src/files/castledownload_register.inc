{%MainUnit castledownload.pas}
{
  Copyright 2013-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { See @link(TCastleDownload.Status). }
  TDownloadStatus = (dsNotStarted, dsDownloading, dsError, dsSuccess);

  { See @link(TCastleDownload.HttpMethod). }
  THttpMethod = (hmGet, hmPost, hmPut, hmDelete, hmOptions, hmHead);

  { Implement this class, and pass to RegisterUrlProtocol,
    to read protocols asynchronously (such that TCastleDownload can read
    them asynchronously).

    @bold(There is no point in using this class
    unless you want to handle custom URL protocols).
    If you just want to download asynchronously from standard URLs,
    like file or http, then use TCastleDownload class. }
  TUrlAsynchronousReader = class
  protected
    { Call this from @link(Start) overrides in descendants that access the network.
      Call this after they called "inherited Start" which may change ForceSynchronous.

      This returns @false if

      - It seems that we do synchronous download
        (maybe because we are called from Download(),
        or maybe we don't have thread support).

      - And EnableBlockingDownloads is @false.

      IOW, this makes sure we honor EnableBlockingDownloads.

      When it sets @false, it also sets Status and ErrorMessage.
      The caller @link(Start) should just Exit if this returns @false. }
    function CheckNetworkAccess: Boolean;
  public
    { Fields set before Start is called. Should not be modified by this instance after Start.
      @groupBegin }
    Url: String;
    ForceSynchronous: Boolean;
    HttpMethod: THttpMethod;
    HttpPostData, HttpHeadersKeys, HttpHeadersValues: TStrings;
    HttpRequestBody: TMemoryStream;
    { @groupEnd }

    { Fields that should be updated during downloading.
      They will set TCastleDownload properties of the same name.
      @groupBegin }
    Status: TDownloadStatus;
    DownloadedBytes, TotalBytes: Int64;
    Contents: TStream; //< Not owned by this instance, TCastleDownload takes care of freeing this.
    ErrorMessage: String; //< When Status = dsError, download failed with this error.
    MimeType: String; //< May be updated at any point, even when dsDownloading, or only when dsSuccess.
    HttpResponseCode: Integer; // In case of HTTP(S) requests, HTTP response code.
    HttpResponseHeaders: TStrings; // In case of HTTP(S) requests, this must contain HTTP response headers.
    FinalUrl: String;
    { @groupEnd }

    { Start downloading, called when @link(TCastleDownload.Start) is called.

      @italic(Note for descendant implementors):
      Exceptions raised inside this will make an actual exception from
      @link(TCastleDownload.Start).
      In case of normal download failure, e.g. file (or HTTP resource) doesn't exist
      (so it is not some bug in code), this method should set Status to dsError
      and not raise an exception. }
    procedure Start; virtual;

    { Called continuously to push the download further.

      @italic(Note for descendant implementors):
      Exceptions raised inside this will make an actual exception (from
      undefined place, for now ApplicationProperties._Update, called by TCastleWindow and TCastleControl).
      In case of normal download failure, e.g. file (or HTTP resource) doesn't exist
      (so it is not some bug in code), this method should set Status to dsError
      and not raise an exception. }
    procedure Update; virtual;
  end;

  TUrlAsynchronousReaderClass = class of TUrlAsynchronousReader;

  { Event called when @link(Download) function wants to download URL with this protocol.
    Use with @link(RegisterUrlProtocol) and @link(TRegisteredProtocol.ReadEvent). }
  TUrlReadEvent = function (
    const Url: String; out MimeType: string): TStream of object;

  { Event called when @link(URLSaveStream) function wants to save URL with this protocol.
    Use with @link(RegisterUrlProtocol) and @link(TRegisteredProtocol.WriteEvent). }
  TUrlWriteEvent = function(const Url: String): TStream of object;

  { Event called when @link(UriExists) function wants to know whether given URL
    exists, and if it is a directory or file.

    When this is not assigned, @link(UriExists) answers ueUnknown for all URLs,
    which just means "we don't know, you can try to load the URL and see
    if it fails with an exception".

    Use with @link(RegisterUrlProtocol) and @link(TRegisteredProtocol.ExistsEvent). }
  TUrlExistsEvent = function(const Url: String): TUriExists of object;

  { Event called with @link(FindFiles) is used to search within a directory
    with the registered URL protocol.

    Should call FileEvent for all files and directories whose name matches
    Mask within directory UrlPath. To every call, pass regular
    @link(TFoundFileMethod) parameters:

    @unorderedList(
      @item(FileInfo: TFileInfo, with all fields filled.)

      @item(StopSearch: Boolean.
        Initially @false. Stop enumerating if caller answers @true.)
    )

    @param(UrlPath Absolute URL path where to search.
      E.g. this is a directory like @code('file:///home/user/dir/').)
    @param(Mask The mask to search for, with wildcards ? and *.
      E.g. '*.png' or '*.jpg'.) }
  TUrlFindFilesEvent = procedure(const UrlPath, Mask: String;
    const FileEvent: TFoundFileMethod) of object;

  EProtocolAlreadyRegistered = class(Exception);

  { Details of the registered URL protocol.
    @italic(Do not create instances of this class directly),
    only use @link(RegisterUrlProtocol) that returns an instance of this. }
  TRegisteredProtocol = class
  strict private
    FReadEvent: TUrlReadEvent;
    FWriteEvent: TUrlWriteEvent;
    FAsynchronousReader: TUrlAsynchronousReaderClass;
    FExistsEvent: TUrlExistsEvent;
    FFindFilesEvent: TUrlFindFilesEvent;
    function GetDetectMimeTypeFromExtension: Boolean;
    procedure SetDetectMimeTypeFromExtension(const Value: Boolean);
  private
    type
      { Event called when @link(UriMimeType) function wants to get MIME type from given URL.
        Use with @link(RegisterUrlProtocol). }
      TUrlMimeTypeEvent = function(const Url: String; out Gzipped: Boolean): String of object;
    var
      FProtocol: String;
      FMimeTypeEvent: TUrlMimeTypeEvent;
  public
    { Registered URL protocol, always lowercase.
      Example: @code('https') to handle URLs like @code('https://castle-engine.io').
      Read-only, set by @link(RegisterUrlProtocol). }
    property Protocol: String read FProtocol;

    { Optional event to read contents of URLs with given protocol.
      This event returns TStream from which the called can read.

      Used for synchronous reading by @link(Download).
      See https://castle-engine.io/url for more information how our
      resource loading works. }
    property ReadEvent: TUrlReadEvent read FReadEvent write FReadEvent;

    { Optional event to write contents of URLs with given protocol.
      This event returns TStream to which the caller can write.

      Used for writing by @link(URLSaveStream).
      See https://castle-engine.io/url for more information how our
      resource saving works. }
    property WriteEvent: TUrlWriteEvent read FWriteEvent write FWriteEvent;

    { Optional class reference to read contents of URLs with given
      protocol asynchronously.

      Protocols that want to support asynchronous reading,
      so that you can easily download the URL contents while performing
      other operations (like displaying some animation) should provide this.

      See https://castle-engine.io/url for more information how our
      resource loading works. }
    property AsynchronousReader: TUrlAsynchronousReaderClass
      read FAsynchronousReader write FAsynchronousReader;

    { Can a reasonable MIME type be inferred from the URL extension.
      This determines whether the routine @link(UriMimeType) returns
      useful MIME type for this protocol.

      The readers (in @link(ReadEvent) and @link(AsynchronousReader)) may
      also use @link(UriMimeType), thus be affected by this property,
      but they can also use more elaborate ways to determine the downloaded
      MIME type. For example, reading resources from HTTP(S) allows to (usually)
      get MIME type from the HTTP(S) server. Using @link(UriMimeType), to determine
      MIME type from extension, is only a fallback in this case. }
    property DetectMimeTypeFromExtension: Boolean read
      GetDetectMimeTypeFromExtension write SetDetectMimeTypeFromExtension
      default false;

    { Optional event to check whether given URL exists (as a directory or file). }
    property ExistsEvent: TUrlExistsEvent read FExistsEvent write FExistsEvent;

    { Optional event to enumerate files in a directory with given protocol.
      This is used by @link(FindFiles). }
    property FindFilesEvent: TUrlFindFilesEvent read FFindFilesEvent write FFindFilesEvent;
  end;

{ Register how we can load and/or save the URLs with given protocol.

  All the parameters except the first, Protocol,
  are optional. You can leave these parameters at default values,
  and in effect the corresponding fields of TRegisteredProtocol will be set
  to @nil.
  You can later configure protocol events by assigning properties of
  the returned TRegisteredProtocol.

  @param ReadEvent See @link(TRegisteredProtocol.ReadEvent).

  @param WriteEvent See @link(TRegisteredProtocol.WriteEvent).

  @param AsynchronousReader See @link(TRegisteredProtocol.AsynchronousReader).

  @returns(Instance of TRegisteredProtocol. You can configure handling of given
    protocol further by assigning properties of this instance.
    Do not free this instance yourself.
    It will be automatically freed when you call @link(UnregisterUrlProtocol).)

  @raises(EProtocolAlreadyRegistered If the protocol handlers are already registered.) }
function RegisterUrlProtocol(const Protocol: String;
  const ReadEvent: TUrlReadEvent = nil;
  const WriteEvent: TUrlWriteEvent = nil;
  const AsynchronousReader: TUrlAsynchronousReaderClass = nil;
  const DetectMimeTypeFromExtension: Boolean = true): TRegisteredProtocol;

{ Is given protocol name registered with @link(RegisterUrlProtocol).
  The Protocol searching is case-insensitive.
  This is a shortcut for FindRegisteredUrlProtocol(Protocol) <> nil. }
function RegisteredUrlProtocol(const Protocol: String): Boolean;

{ If the given protocol is registered with @link(RegisterUrlProtocol),
  returns the TRegisteredProtocol instance that describes how to handle this
  protocol. The Protocol searching is case-insensitive.
  Returns @nil if not found. }
function FindRegisteredUrlProtocol(const Protocol: String): TRegisteredProtocol;

{ Unregister protocol, reverting the @link(RegisterUrlProtocol). }
procedure UnregisterUrlProtocol(const Protocol: String);

{$endif}

{$ifdef read_implementation}

procedure TUrlAsynchronousReader.Start;
begin
  Status := dsDownloading;
  TotalBytes := -1;

  { ForceSynchronous allows to make synchronous Download work even when
    - binary has no thread support linked (e.g. no CThreads linked on Unix).
    - OS has no thread support.
    Moreover TCastleDownload can "downgrade" to working synchronously
    (instead of just failing) when OS never supports threads.

    Outside code may set ForceSynchronous, but we can override it to true. }
  {$ifndef CASTLE_SUPPORTS_THREADING}
  ForceSynchronous := true;
  {$endif}
end;

function TUrlAsynchronousReader.CheckNetworkAccess: Boolean;
begin
  Result := true;

  { The main purpose of this is to disable Download from blocking.
    Additionally, it also disables TCastleDownload from "downgrading" to blocking,
    when OS doesn't support threads. }
  if ForceSynchronous and (not EnableBlockingDownloads) then
  begin
    Status := dsError;
    ErrorMessage := 'Downloading network resources (from "http" or "https"  protocols) is not enabled. Set global EnableBlockingDownloads:=true to enable.';
    Exit(false);
  end;
end;

procedure TUrlAsynchronousReader.Update;
begin
end;

{ registering URL protocols -------------------------------------------------- }

{ TRegisteredProtocol -------------------------------------------------------- }

function TRegisteredProtocol.GetDetectMimeTypeFromExtension: Boolean;
var
  DetectMimeTypeEvent: TUrlMimeTypeEvent;
begin
  DetectMimeTypeEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType;
  Result := SameMethods(TMethod(FMimeTypeEvent), TMethod(DetectMimeTypeEvent));
end;

procedure TRegisteredProtocol.SetDetectMimeTypeFromExtension(const Value: Boolean);
begin
  if Value then
    FMimeTypeEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.MimeType
  else
    FMimeTypeEvent := nil;
end;

{ TRegisteredProtocols ------------------------------------------------------- }

type
  TRegisteredProtocols = class({$ifdef FPC}specialize{$endif} TObjectList<TRegisteredProtocol>)
    { Find TRegisteredProtocol with given Protocol, case-insensitive.
      Returns TRegisteredProtocol, or @nil if not found. }
    function Find(Protocol: String): TRegisteredProtocol;

    { Add new instance of TRegisteredProtocol, return it.
      The resulting TRegisteredProtocol has already TRegisteredProtocol.Protocol
      set, other fields are at their default (@nil) values. }
    function Add(const Protocol: String): TRegisteredProtocol; reintroduce;
  end;

function TRegisteredProtocols.Find(Protocol: String): TRegisteredProtocol;
begin
  // to compare ignoring case, entries on TRegisteredProtocol.Protocol are already lower-cased
  Protocol := LowerCase(Protocol);

  for Result in Self do
    if Result.Protocol = Protocol then
      Exit;
  Result := nil;
end;

function TRegisteredProtocols.Add(const Protocol: String): TRegisteredProtocol;
begin
  if Find(Protocol) <> nil then
    raise EProtocolAlreadyRegistered.CreateFmt('URL protocol "%s" is already registered', [Protocol]);
  // We allow Protocol = '', as it is used internally
  if (Protocol <> '') and
     (not URIValidProtocol(Protocol)) then
    raise Exception.CreateFmt('URL protocol name "%s" is invalid (see URIValidProtocol for the requirements on protocol name)', [Protocol]);
  Result := TRegisteredProtocol.Create;
  // lowercase Protocol, to later compare in TRegisteredProtocols.Find ignoring case
  Result.FProtocol := LowerCase(Protocol);
  inherited Add(Result);
end;

var
  FRegisteredProtocols: TRegisteredProtocols;

function RegisteredProtocols: TRegisteredProtocols;
var
  P: TRegisteredProtocol;
begin
  { initialize FRegisteredProtocols on-demand }
  if FRegisteredProtocols = nil then
  begin
    FRegisteredProtocols := TRegisteredProtocols.Create(true);

    // register default protocols handled in this unit

    {$if defined(ANDROID)}
    // http(s) using Android service
    P := FRegisteredProtocols.Add('http');
    P.AsynchronousReader := THttpAndroidReader;
    P.DetectMimeTypeFromExtension := true;

    P := FRegisteredProtocols.Add('https');
    P.AsynchronousReader := THttpAndroidReader;
    P.DetectMimeTypeFromExtension := true;

    {$elseif defined(HAS_FP_HTTP_CLIENT)}
    // http(s) using FPC FpHttpClient
    P := FRegisteredProtocols.Add('http');
    P.AsynchronousReader := TFpHttpClientReader;
    P.DetectMimeTypeFromExtension := true;

    P := FRegisteredProtocols.Add('https');
    P.AsynchronousReader := TFpHttpClientReader;
    P.DetectMimeTypeFromExtension := true;

    {$elseif defined(DELPHI)}
    // http(s) using Delphi System.Net.HttpClient or Indy
    P := FRegisteredProtocols.Add('http');
    P.AsynchronousReader := TDelphiHttpReader;
    P.DetectMimeTypeFromExtension := true;

    P := FRegisteredProtocols.Add('https');
    P.AsynchronousReader := TDelphiHttpReader;
    P.DetectMimeTypeFromExtension := true;

    {$else}
    // Otherwise, we have no implementation to get http(s) contents,
    // but at least we can determine MIME type from extension.
    // Makes TTestUriUtils.TestMimeTypeHttpQuery pass on web.
    P := FRegisteredProtocols.Add('http');
    P.DetectMimeTypeFromExtension := true;
    P := FRegisteredProtocols.Add('https');
    P.DetectMimeTypeFromExtension := true;

    {$endif}

    // handle regular file://, also do this when no protocol is specified
    // (which can be handled right now by registering Protocol = '').
    P := FRegisteredProtocols.Add('');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Read;
    P.WriteEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Write;
    P.ExistsEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Exists;
    P.DetectMimeTypeFromExtension := true;

    P := FRegisteredProtocols.Add('file');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Read;
    P.WriteEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Write;
    P.ExistsEvent := {$ifdef FPC}@{$endif} TUrlFile{$ifdef FPC}(nil){$endif}.Exists;
    P.DetectMimeTypeFromExtension := true;

    {$if defined(ANDROID)}
    // handle Android-specific assets
    P := FRegisteredProtocols.Add('assets');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlCastleAndroidAssets{$ifdef FPC}(nil){$endif}.Read;
    P.DetectMimeTypeFromExtension := true;

    P := FRegisteredProtocols.Add('castle-android-assets');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlCastleAndroidAssets{$ifdef FPC}(nil){$endif}.Read;
    P.DetectMimeTypeFromExtension := true;
    {$endif}

    // handle castle-config.
    // No need for reader/writer now, as our downloading routines use ResolveCastleConfigUrl,
    // so they will handle this protocol.
    P := FRegisteredProtocols.Add('castle-config');
    P.ExistsEvent := {$ifdef FPC}@{$endif} TUrlCastleConfig{$ifdef FPC}(nil){$endif}.Exists;
    P.DetectMimeTypeFromExtension := true;

    // handle castle-data.
    // No need for reader/writer now, as our downloading routines use ResolveCastleDataUrl,
    // so they will handle this protocol.
    P := FRegisteredProtocols.Add('castle-data');
    P.ExistsEvent := {$ifdef FPC}@{$endif} TUrlCastleData{$ifdef FPC}(nil){$endif}.Exists;
    P.DetectMimeTypeFromExtension := true;

    P := FRegisteredProtocols.Add('data');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlData{$ifdef FPC}(nil){$endif}.Read;
    P.ExistsEvent := {$ifdef FPC}@{$endif} TUrlData{$ifdef FPC}(nil){$endif}.Exists;
    // set internal FMimeTypeEvent, circumventing DetectMimeTypeFromExtension
    P.FMimeTypeEvent := {$ifdef FPC}@{$endif} TUrlData{$ifdef FPC}(nil){$endif}.MimeType;

    P := FRegisteredProtocols.Add('ecmascript');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlEcmaScript{$ifdef FPC}(nil){$endif}.Read;
    P.FMimeTypeEvent := {$ifdef FPC}@{$endif} TUrlEcmaScript{$ifdef FPC}(nil){$endif}.MimeType;

    P := FRegisteredProtocols.Add('javascript');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlEcmaScript{$ifdef FPC}(nil){$endif}.Read;
    P.FMimeTypeEvent := {$ifdef FPC}@{$endif} TUrlEcmaScript{$ifdef FPC}(nil){$endif}.MimeType;

    P := FRegisteredProtocols.Add('castlescript');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlCastleScript{$ifdef FPC}(nil){$endif}.Read;
    P.FMimeTypeEvent := {$ifdef FPC}@{$endif} TUrlCastleScript{$ifdef FPC}(nil){$endif}.MimeType;

    P := FRegisteredProtocols.Add('kambiscript');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlCastleScript{$ifdef FPC}(nil){$endif}.Read;
    P.FMimeTypeEvent := {$ifdef FPC}@{$endif} TUrlCastleScript{$ifdef FPC}(nil){$endif}.MimeType;

    P := FRegisteredProtocols.Add('compiled');
    P.ReadEvent := {$ifdef FPC}@{$endif} TUrlCompiled{$ifdef FPC}(nil){$endif}.Read;
    P.FMimeTypeEvent := {$ifdef FPC}@{$endif} TUrlCompiled{$ifdef FPC}(nil){$endif}.MimeType;
  end;
  Result := FRegisteredProtocols;
end;

function RegisterUrlProtocol(const Protocol: String;
  const ReadEvent: TUrlReadEvent;
  const WriteEvent: TUrlWriteEvent;
  const AsynchronousReader: TUrlAsynchronousReaderClass;
  const DetectMimeTypeFromExtension: Boolean): TRegisteredProtocol;
begin
  Result := RegisteredProtocols.Add(Protocol);
  Result.ReadEvent := ReadEvent;
  Result.WriteEvent := WriteEvent;
  Result.AsynchronousReader := AsynchronousReader;
  Result.DetectMimeTypeFromExtension := DetectMimeTypeFromExtension;
end;

function FindRegisteredUrlProtocol(const Protocol: String): TRegisteredProtocol;
begin
  Result := RegisteredProtocols.Find(Protocol);
end;

function RegisteredUrlProtocol(const Protocol: String): Boolean;
begin
  Result := FindRegisteredUrlProtocol(Protocol) <> nil;
end;

procedure UnregisterUrlProtocol(const Protocol: String);
var
  P: TRegisteredProtocol;
begin
  P := RegisteredProtocols.Find(Protocol);
  if P <> nil then
    RegisteredProtocols.Remove(P);
end;

{$endif}
