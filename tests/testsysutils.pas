{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestSysUtils;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestSysUtils = class(TTestCase)
    procedure TestDirectoryFileExists;
  end;

implementation

uses CastleFilesUtils, CastleURIUtils;

procedure TTestSysUtils.TestDirectoryFileExists;
begin
  { window/gtk/castlewindow_gtk.inc uses FileExists and DirectoryExists
    to detect file/dir }

  { FileExists doesn't differ dirs and non-dirs... On non-Windows.
    On Windows, it does, unfortunately
    (although I would like to see it consistent one day...).
    See http://www.freepascal.org/docs-html/rtl/sysutils/fileexists.html
    http://free-pascal-general.1045716.n5.nabble.com/FileExists-inconsistency-td2813433.html }
  {$ifdef MSWINDOWS}
  AssertFalse(FileExists(URIToFilenameSafe(ApplicationData(''))));
  AssertFalse(FileExists(URIToFilenameSafe(ApplicationData('images/'))));
  {$else}
  AssertTrue(FileExists(URIToFilenameSafe(ApplicationData(''))));
  AssertTrue(FileExists(URIToFilenameSafe(ApplicationData('images/'))));
  {$endif}
  AssertTrue(FileExists(URIToFilenameSafe(ApplicationData('test.xml'))));
  AssertTrue(not FileExists(URIToFilenameSafe(ApplicationData('test-not-existing.xml'))));

  { DirectoryExists differs dirs and non-dirs }
  AssertTrue(DirectoryExists(URIToFilenameSafe(ApplicationData(''))));
  AssertTrue(DirectoryExists(URIToFilenameSafe(ApplicationData('images/'))));
  AssertTrue(not DirectoryExists(URIToFilenameSafe(ApplicationData('test.xml'))));
  AssertTrue(not DirectoryExists(URIToFilenameSafe(ApplicationData('test-not-existing.xml'))));
end;

initialization
  RegisterTest(TTestSysUtils);
end.
