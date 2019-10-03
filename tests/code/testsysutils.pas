{
  Copyright 2015-2018 Michalis Kamburelis.

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
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry;

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
  AssertFalse(FileExists(URIToFilenameSafe('castle-data:/')));
  AssertFalse(FileExists(URIToFilenameSafe('castle-data:/images/')));
  {$else}
  AssertTrue(FileExists(URIToFilenameSafe('castle-data:/')));
  AssertTrue(FileExists(URIToFilenameSafe('castle-data:/images/')));
  {$endif}
  AssertTrue(FileExists(URIToFilenameSafe('castle-data:/test.xml')));
  AssertTrue(not FileExists(URIToFilenameSafe('castle-data:/test-not-existing.xml')));

  { DirectoryExists differs dirs and non-dirs }
  AssertTrue(DirectoryExists(URIToFilenameSafe('castle-data:/')));
  AssertTrue(DirectoryExists(URIToFilenameSafe('castle-data:/images/')));
  AssertTrue(not DirectoryExists(URIToFilenameSafe('castle-data:/test.xml')));
  AssertTrue(not DirectoryExists(URIToFilenameSafe('castle-data:/test-not-existing.xml')));
end;

initialization
  RegisterTest(TTestSysUtils);
end.
