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
  are also supported, as @code(assets:/my_file.png).

  @raises(EDownloadError In case of problems saving this URL.)

  @raises(Exception Various TStream instances (used internally by this
    function) may raise exceptions in case the stream cannot be created
    for reading.
    Right now, we simply let these exceptions to "pass through" from this function
    (instead of catching and re-raising).
    So be ready that this function may raise @italic(any) Exception class.
    In case of local files (file:// URLs), the typical exception class
    is EFOpenError.)
}
function Download(const URL: string; const Options: TStreamOptions = []): TStream;
function Download(const URL: string; const Options: TStreamOptions;
  out MimeType: string): TStream;

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
    (see http://castle-engine.sourceforge.net/tutorial_network.php ).

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
    procedure Write(const S: string);
    procedure Write(const S: string; const Args: array of const);
    procedure Writeln(const S: string = '');
    procedure Writeln(const S: string; const Args: array of const);
  end;

implementation

uses URIParser, CastleURIUtils, CastleUtils, CastleLog, CastleInternalZStream,
  CastleClassUtils, CastleDataURI, CastleProgress, CastleStringUtils
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

end.
