{
  Copyright 2015-2015 Michalis Kamburelis.

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

  { FileExists doesn't differ dirs and non-dirs }
  AssertTrue(FileExists(URIToFilenameSafe(ApplicationData(''))));
  AssertTrue(FileExists(URIToFilenameSafe(ApplicationData('test.xml'))));
  AssertTrue(not FileExists(URIToFilenameSafe(ApplicationData('test-not-existing.xml'))));

  { DirectoryExists differs dirs and non-dirs }
  AssertTrue(DirectoryExists(URIToFilenameSafe(ApplicationData(''))));
  AssertTrue(not DirectoryExists(URIToFilenameSafe(ApplicationData('test.xml'))));
  AssertTrue(not DirectoryExists(URIToFilenameSafe(ApplicationData('test-not-existing.xml'))));
end;

initialization
  RegisterTest(TTestSysUtils);
end.
