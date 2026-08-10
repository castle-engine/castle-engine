{%MainUnit castledownload.pas}
{
  Copyright 2013-2026 Michalis Kamburelis.

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

{ Handle file URLs (protocol "file" or ""). }

{ TUrlFile ------------------------------------------------------------------- }

type
  TUrlFile = class
    class function Read(const Url: String; out MimeType: string): TStream;
    class function Write(const Url: String): TStream;
    { Determine MIME type from URL extension. }
    class function MimeType(const URI: string; out Gzipped: Boolean): String;
    class function Exists(const Url: String): TUriExists;
  end;

class function TUrlFile.Read(const Url: String; out MimeType: string): TStream;
var
  FileName: String;
begin
  CheckFileAccessSafe(URL);

  FileName := UriToFilenameSafe(URL);
  if FileName = '' then
    raise EDownloadError.CreateFmt('Cannot convert URL "%s" to filename', [URL]);

  Result := TFileStream.Create(FileName, fmOpenRead
    { Allow other applications to read the same file while we have it open.
      This is useful in particular when running multiple processes of the same
      application from the same data, to allow them all to work
      simultaneously, without locking files from each other.

      Testcase: "Add Random Asset" from 2 clients of
      https://github.com/castle-engine/castle-and-mormot-collaborative-editing/
      running on the same system. }
    or fmShareDenyWrite
  );
  MimeType := UriMimeType(URL);
end;

class function TUrlFile.Write(const Url: String): TStream;
var
  FileName, Dir: String;
begin
  FileName := UriToFilenameSafe(URL);
  if FileName = '' then
    raise ESaveError.CreateFmt('Cannot convert URL to a filename: "%s"', [URL]);

  { We need to create subdirectory for the file,
    testcase: https://github.com/michaliskambi/test_config_subdir  }
  Dir := ExtractFilePath(FileName);
  { Ignore Dir = ''. As delphi_tests show, it is possible to get here Url
    that is relative, so also FileName that is relative, like 'aaa.castle-transform',
    and then Dir is ''. It's OK to ignore it -- it means the target directory
    is current directory, no need to create anything. }
  if (Dir <> '') and (not ForceDirectories(Dir)) then
    raise Exception.CreateFmt('Cannot create directory "%s" for filename "%s"', [
      Dir,
      FileName
    ]);

  Result := TFileStream.Create(FileName, fmCreate);
end;

class function TUrlFile.MimeType(const URI: string; out Gzipped: Boolean): String;
var
  U: TURI;
  Ext, URIName, UriPathDocument, PreviousExt: String;
begin
  Result := '';
  Gzipped := false;

  U := ParseURI(URI);

  { Deconstruct first URL properly using ParseURI.
    This allows to strip the HTTP query part "?xxx", see
    https://github.com/castle-engine/castle-engine/issues/547 .

    Note that ParseURI also strips anchor, like #xxx .
    Otherwise 'xxx.json#skinname' would not be detected as Spine JSON.
    So there's no need for URIDeleteAnchor lower, UriPathDocument is already
    without anchor.
    The anchor here is stripped, so all loaders

    - should strip anchors, e.g. both LoadImageAsNode and LoadImage must strip anchors.
    - can use anchors for extra info, like Spine, sprite sheets and images
      already do.
  }
  UriPathDocument := U.Path + U.Document;

  { We're consciously using here ExtractFileExt on URIs,
    although they should be used for filenames.
    Note that this unit does not define public functions like ExtractURIExt
    or ExtractURIDoubleExt: *everything* should operate on MIME types instead. }
  Ext := LowerCase(ExtractFileExt(UriPathDocument));

  URIName := LowerCase(ExtractURIName(UriPathDocument));

  // special case detected looking at whole name
  if URIName = '.gitignore' then
  begin
    Result := 'text/plain';
    Exit;
  end;

  { First, check for gzipped files, to recognize files with 2 extensions
    like '.x3d.gz'. }
  if Ext = '.gz' then
  begin
    { Logic to extract previous extension just like ExtractFileDoubleExt. }
    PreviousExt := LowerCase(ExtractFileExt(ChangeFileExt(UriPathDocument, '')));
    if (PreviousExt <> '') and
       UriMimeExtensions.TryGetValue(PreviousExt, Result) then
    begin
      Gzipped := true;
      Exit;
    end;
  end;

  { Otherwise, use just Ext with UriMimeExtensions map. }
  if UriMimeExtensions.TryGetValue(Ext, Result) then
  begin
    { Hardcoded list of extensions that denote gzip-compressed files }
    Gzipped := ArrayContainsString(Ext, [
      // commonly gzipped VRML or X3D
      '.wrz',
      '.x3dvz',
      '.x3dz',
      // just gzipped arbitrary content (not caught by earlier check)
      '.gz'
    ]);
    Exit;
  end;

  { Otherwise nothing (for now) - we failed to detect MIME type from URL,
    we will return Result='' and Gzipped=false.
    Maybe in the future use FpMimeTypes unit from FPC? }
end;

class function TUrlFile.Exists(const Url: String): TUriExists;
var
  FileName: String;
begin
  FileName := UriToFilenameSafe(URL);
  if FileName = '' then
  begin
    { Uncertain what to return in this case.
      It likely (but not for sure) means someone used UriExists('').
      '' means "current directory" for some file API,
      but UriExists('') seems like an error, not asking for current directory.
      So make a warning and return ueUnknown. }
    WritelnWarning('UriExists value for empty URL is not defined');
    Exit(ueUnknown);
  end;

  { FileExists in older FPC behaves inconsistently for directories.
    On non-Windows, returns true.
    On Windows, returns false.
    See http://www.freepascal.org/docs-html/rtl/sysutils/fileexists.html
    http://free-pascal-general.1045716.n5.nabble.com/FileExists-inconsistency-td2813433.html
    So check DirectoryExists first, if @true then assume it's a directory
    (regardless of FileExists result). }
  if DirectoryExists(FileName) then
    Exit(ueDirectory)
  else
  if FileExists(FileName) then
    Exit(ueFile)
  else
    Exit(ueNotExists);
end;

{$endif}
