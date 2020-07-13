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

function Download(const URL: string; const Options: TStreamOptions;
  out MimeType: string): TStream;

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

var
  P: String;
  RegisteredProtocol: TRegisteredProtocol;
  UnderlyingStream: TStream;
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

  { Handle early castle-data:, protocol to access application data,
    https://castle-engine.io/manual_data_directory.php .
    Note that we don't even log it if LogAllLoading,
    as it will cause another recursive Download() call. }
  if P = 'castle-data' then
  begin
    Result := Download(ResolveCastleDataURL(URL), Options, MimeType);
    Exit;
  end;

  if LogAllLoading then
    WritelnLog('Loading', 'Loading "%s"', [URIDisplay(URL)]);

  RegisteredProtocol := RegisteredProtocols.Find(P);
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
    raise EDownloadError.CreateFmt('Downloading from protocol "%s" is not supported', [P]);
end;

function Download(const URL: string; const Options: TStreamOptions): TStream;
var
  MimeType: string;
begin
  Result := Download(URL, Options, MimeType { ignored });
end;

{$endif}
