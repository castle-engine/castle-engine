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

var
  { Log (through CastleLog) all loading, that is: all calls to @link(Download).
    This allows to easily check e.g. whether the engine is not loading something
    during the game (which usually badly affects the performance). }
  LogAllLoading: boolean = false;

var
  { Does @link(Download) (synchronous downloading routine) support http and https protocol.
    This is by default disabled, as it means that a call to @link(Download) may block
    your application, for arbitrarily long time, and you have no way to interrupt it.
    It is recommended to use asynchronous downloading, using @link(TCastleDownload),
    to download resources over the network. }
  EnableBlockingDownloads: boolean = false;

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

  All the supported URL protocols are documented in our manual:
  https://castle-engine.io/manual_network.php . They include:

  @unorderedList(
    @item(Local @code(file) URLs.
      Moreover URL without any protocol is always
      treated like a local filename (absolute or relative to current dir),
      so this function can be a drop-in replacement for normal file reading.
      The MIME type for local files is guessed based on their extension.
    )

    @item(Data URI scheme (http://en.wikipedia.org/wiki/Data_URI_scheme).)

    @item(Protocols to embed script contents:
      ecmascript, javascript (see X3D specifications),
      castlescript, kambiscript (see https://castle-engine.io/castle_script.php),
      compiled (https://castle-engine.io/x3d_extensions.php#section_ext_script_compiled).
    )

    @item(
      Set EnableBlockingDownloads to @true to support http and https protocols.
      The MIME type for such content is usually reported by the http server
      (but if the server doesn't report MIME type, we still try to determine it,
      looking at URL using UriMimeType).
    )
  )

  This routine makes a @italic(synchronous (blocking) downloading).
  Which means that if you use a network URL (like http://...) then your
  application will wait until the data arrives from the Internet.
  There may be a timeout of the request (so your application will not hang
  indefinitely), but still your code (or user) have no way to cancel or watch
  the progress of this operation.
  This is why http/https support is disabled by default (see @link(EnableBlockingDownloads)).
  This is sometimes acceptable (e.g. if you're
  waiting during the "loading" process, and the data just @italic(has)
  to be downloaded in order to continue), and it's really easy to use
  (you just download data exactly the same way like you open a local file).

  You can use @bold(asynchronous downloading) through the @link(TCastleDownload)
  class instead.

  @raises(EDownloadError In case of problems loading from this URL.

    Any exceptions inside internal loading routines,
    like EFOpenError or EStreamError, are internally caught and changed
    to EDownloadError.)

  @groupBegin
}
function Download(const Url: String; const Options: TStreamOptions = []): TStream; overload;
function Download(const Url: String; const Options: TStreamOptions;
  out MimeType: string): TStream; overload;
{ @groupEnd }

function GetEnableNetwork: Boolean; deprecated 'use EnableBlockingDownloads';
procedure SetEnableNetwork(const Value: Boolean); deprecated 'use EnableBlockingDownloads';
{$ifdef FPC}
property EnableNetwork: Boolean read GetEnableNetwork write SetEnableNetwork;
{$endif}

{$endif}

{$ifdef read_implementation}

function Download(const Url: String; const Options: TStreamOptions;
  out MimeType: string): TStream;
var
  D: TCastleDownload;
begin
  D := TCastleDownload.Create(nil);
  try
    D.OwnsContents := false;
    D.Url := Url;
    D.Options := Options;
    D.ForceSynchronous := true;
    D.Start;
    D.WaitForFinish;
    Assert(D.Status in [dsError, dsSuccess]);
    if D.Status = dsError then
    begin
      { D.Contents may be set non-nil even in case of error, testcase:
        castle-model-viewer https://x3dgraphics.com/examples/X3dForAdvancedModeling/GltfSampleModels/AlphaBlendModeTest_not_existent.x3d
        So make sure to free them.
      }
      D.OwnsContents := true;
      raise EDownloadError.Create(D.ErrorMessage);
    end else
    begin
      Assert(D.Status = dsSuccess);
      Result := D.Contents;
      MimeType := D.MimeType;
    end;
  finally FreeAndNil(D) end;
end;

function Download(const Url: String; const Options: TStreamOptions): TStream;
var
  MimeType: string;
begin
  Result := Download(Url, Options, MimeType { ignored });
end;

function GetEnableNetwork: Boolean;
begin
  Result := EnableBlockingDownloads;
end;

procedure SetEnableNetwork(const Value: Boolean);
begin
  EnableBlockingDownloads := Value;
end;

{$endif}
