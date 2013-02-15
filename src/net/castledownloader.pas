{
  Copyright 2013 Michalis Kamburelis and FPC team.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Download URLs. }
unit CastleDownloader;

interface

uses Classes, PkgDownload, PkgWget;

const
  DefaultDownloader = 'wget';
  (*
  {$if defined(unix) or defined(windows)}
    DefaultDownloader = 'lnet';
  {$else}
    DefaultDownloader = 'base';
  {$endif}
  *)
  { TODO: as the downloading is blocking, we will probably want to set
    default downloader = 'base', to fail for network streams.
    And leave including specific network unit to a particular application. }

{ Return a stream to read given URL.
  Returned stream is suitable only for reading, and the initial position
  is always at the beginning.

  All errors are reported by raising exceptions.

  Uses PkgDownload (the same downloader as used by fppkg)
  which makes the downloading library pluggable.
  Various options are already provided in FPC sources:

  @unorderedList(
    @item(lnet (PkgLNet in ~/sources/fpc/trunk/utils/fppkg/ ,
      depends on LNet Pascal library that is already available
      inside ~/sources/fpc/trunk/utils/fppkg/lnet),)

    @item(synapse (PkgSynapse in ~/sources/fpc/trunk/utils/fppkg/examples/pkgsynapse.pp ,
      depeneds on Synapse Pascal library that you have to download yourself),)

    @item(libcurl (PkgLibCurl in ~/sources/fpc/trunk/utils/fppkg/examples/pkglibcurl.pp ,
      depends that libcurl dynamic library is installed on user system),)

    @item(wget (PkgWget ~/sources/fpc/trunk/packages/fppkg/src/pkgwget.pp ,
      depends that wget command-line utility is installed  on user system).)
  )

  A local file URL (or the URL without any protocol) is always supported,
  without using any networking library.
}
function DownloadURL(const URL: string;
  const Downloader: string = DefaultDownloader): TStream;

implementation

uses SysUtils, URIParser, PkgGlobals, PkgMessages;

function DownloadURL(const URL: string;
  const Downloader: string = DefaultDownloader): TStream;
var
  URI: TURI;
  P, FileName: string;
  DownloaderClass: TBaseDownloaderClass;
  D: TBaseDownloader;
begin
  URI := ParseURI(URL);
  P := URI.Protocol;

  { network protocols create TBaseDownloaderClass that gets data into
    a new TMemoryStream }
  if (CompareText(P, 'ftp') = 0) or
     (CompareText(P, 'http') = 0) then
  begin
    Result := TMemoryStream.Create;
    DownloaderClass := GetDownloader(Downloader);
    D := DownloaderClass.Create(nil);
    try
      { We would like to call D.FTPDownload or D.HTTPDownload directly,
        to avoid duplicating URI-parsing inside D.Download,
        but we can't: FTPDownload and HTTPDownload are protected. }
      D.Download(URL, Result);
    finally FreeAndNil(D) end;
    Result.Position := 0; { rewind for easy reading }
  end else

  { local filenames are directly handled, without the need for any downloader }
  if (P = '') or (CompareText(P, 'file') = 0) then
  begin
    if not URIToFilename(URL, FileName) then
      Error('Cannot convert URL "%s" to filename', [URL]);

    { TODO: TFileStream is not buffered (on some platforms).
      Make an option to just load this to TMemoryStream
      (like our CreateReadFileStream).
      Make an option to wrap in a buffered stream,
      like our VRML/X3D reader does.

      And make a version that always returns buffered stream,
      to avoid wrapping buffered stream in a buffered stream for VRML/X3D
      reader.

      The point is that for local filenames, no additional overhead
      (no additional streams in-between) should be created. }

    Result := TFileStream.Create(FileName, fmOpenRead);
  end else

    Error(SErrUnknownProtocol,[P]);
end;

initialization
  { PkgGlobals registers it's own handlers for OnGetVendorName
    and OnGetApplicationName. Workaround this by resetting them. }
  OnGetVendorName := nil;
  OnGetApplicationName := nil;

  { TODO: change LogHandler to CastleLog }
  // LogHandler := @LogCmd;
end.
