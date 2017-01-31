{
  Copyright 2013-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Download URLs. }
unit CastleDownload;

{$I castleconf.inc}

interface

uses SysUtils, Classes {$ifdef HAS_FP_HTTP_CLIENT}, FpHttpClient {$endif};

const
  DefaultEnableNetwork = false;

var
  { Can @link(Download) actually use the network.
    As all the downloading is blocking for now, this is initially @false.
    If you want to really use the network, change it to @true. }
  EnableNetwork: boolean = DefaultEnableNetwork;

type
  EDownloadError = class(Exception);

  { Options for the @link(Download) and URLSaveStream functions. }
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

    { Filter the contents through gzip decompression (for @link(Download))
      or compression (for @link(URLSaveStream)). }
    soGzip
  );
  TStreamOptions = set of TStreamOption;

{ Return a stream to read given URL.
  Returned stream is suitable only for reading, and the initial position
  is always at the beginning.
  Overloaded version also returns a MIME type (or '' if unknown).

  All errors are reported by raising exceptions.

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
  castlescript, kambiscript (see http://castle-engine.sourceforge.net/castle_script.php),
  compiled (http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_script_compiled).
  The MIME type for these is implied by the protocol (like "application/javascript"
  for ecmascript/javascript), and the returned stream simply contains
  script code.

  Set EnableNetwork to @true to have also support for network protocols.
  Right now this means only http, handled by FpHttpClient.
  The MIME type for such content is usually reported by the http server
  (but if the server doesn't report MIME type, we still try to guess it,
  looking at URL using URIMimeType).

  On Android, URLs that indicate assets (files packaged inside .apk)
  are also supported, as @code(assets:/my_file.png). }
function Download(const URL: string; const Options: TStreamOptions = []): TStream;
function Download(const URL: string; const Options: TStreamOptions;
  out MimeType: string): TStream;

{ Create a stream to save a given URL, for example create a TFileStream
  to save a file for a @code(file) URL. In other words, perform @italic(upload).
  Right now, this only works for @code(file) URLs, and the only advantage
  it has over manually creating TFileStream is that this accepts URLs. }
function URLSaveStream(const URL: string; const Options: TStreamOptions = []): TStream;

var
  { Log (through CastleLog) all loading, that is: all calls to @link(Download).
    This allows to easily check e.g. whether the engine is not loading something
    during the game (which usually badly affects the performance). }
  LogAllLoading: boolean = false;

implementation

uses URIParser, CastleURIUtils, CastleUtils, CastleLog, CastleZStream,
  CastleClassUtils, CastleDataURI, CastleProgress
  {$ifdef ANDROID}, CastleAndroidInternalAssetStream {$endif};

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

{ Global functions ----------------------------------------------------------- }

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

{ Load FileName to TMemoryStream. }
function CreateMemoryStream(const FileName: string): TMemoryStream;
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
function CreateMemoryStream(var Stream: TStream): TMemoryStream;
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

function Download(const URL: string; const Options: TStreamOptions;
  out MimeType: string): TStream;
var
  P, FileName, S: string;
  {$ifdef HAS_FP_HTTP_CLIENT}
  NetworkResult: TMemoryStream;
  {$endif}
  FileStream: TFileStream;
  DataURI: TDataURI;
  {$ifdef ANDROID}
  AssetStream: TReadAssetStream;
  {$endif}
const
  MaxRedirects = 32;
begin
  P := URIProtocol(URL);

  if LogAllLoading and Log then
    WritelnLog('Loading', 'Loading "%s"', [URIDisplay(URL)]);

  {$ifdef HAS_FP_HTTP_CLIENT}
  { network protocols: get data into a new TMemoryStream using FpHttpClient }
  if EnableNetwork and (P = 'http') then
  begin
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

  { assets: to access Android assets }
  if P = 'assets' then
  begin
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
    if P = 'http' then
      S := 'Downloading from "http" is not enabled' else
      S := Format('Downloading from protocol "%s" is not supported', [P]);
    raise EDownloadError.Create(S);
  end;
end;

function Download(const URL: string; const Options: TStreamOptions): TStream;
var
  MimeType: string;
begin
  Result := Download(URL, Options, MimeType { ignored });
end;

function URLSaveStream(const URL: string; const Options: TStreamOptions): TStream;
{ TODO: for now, this ignores soForceMemoryStream flag in Options. }
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
      raise Exception.CreateFmt('Cannot convert URL to a filename: "%s"', [URL]);
  end else
    raise Exception.CreateFmt('Saving of URL with protocol "%s" not possible', [P]);
  if soGzip in Options then
  begin
    Result := TGZFileStream.Create(TFileStream.Create(FileName, fmCreate), true);
  end else
    Result := TFileStream.Create(FileName, fmCreate);
end;

end.
