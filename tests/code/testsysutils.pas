// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestSysUtils" -*-
{
  Copyright 2015-2021 Michalis Kamburelis.

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
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestSysUtils = class(TCastleTestCase)
    procedure TestDirectoryFileExists;
  end;

implementation

uses CastleFilesUtils, CastleURIUtils;

procedure TTestSysUtils.TestDirectoryFileExists;
begin
  { Before FPC 3.2.0:

    FPC FileExists on Unix answers true for both regular files and directories.
    Unlike FPC FileExists on Windows, that answers true only for regular files.
    See http://www.freepascal.org/docs-html/rtl/sysutils/fileexists.html
    http://free-pascal-general.1045716.n5.nabble.com/FileExists-inconsistency-td2813433.html

    IMHO that's quite bad.
    - It's inconsistent across two platforms.
    - And Unix behavior is unexpected.

        FPC docs for FileExists say that this is deliberate on Unix,
        because "on Unix files are directories".
        But this seems like an after-thought explanation of a broken behavior
        in FPC docs.
        Indeed on Unix many things are "some kind of files"
        (including directories, sockets, pipes) when looking at C API and kernel API,
        but that's an internal detail for most people.
        In normal conversations, in normal API descriptions,
        when someone talks about "files", it's natural
        (also to Unix users and developers) to understand
        that you mean "regular files; not directories, network sockets, pipes...".

    It was fixed in
    https://github.com/graemeg/freepascal/commit/6fbfe3fc4c1dc23908b14a68fe54ae04d53ded73 ,
    thankfully.
  }
  {$ifdef MSWINDOWS}
  AssertFalse(FileExists(URIToFilenameSafe('castle-data:/')));
  AssertFalse(FileExists(URIToFilenameSafe('castle-data:/images/')));
  {$else}
    {$if not defined(VER3_3)} // For FPC 3.3.1, the behavior depends on exact revision...
      {$if defined(VER3_0) or defined(VER3_1)}
      AssertTrue(FileExists(URIToFilenameSafe('castle-data:/')));
      AssertTrue(FileExists(URIToFilenameSafe('castle-data:/images/')));
      {$else}
      AssertFalse(FileExists(URIToFilenameSafe('castle-data:/')));
      AssertFalse(FileExists(URIToFilenameSafe('castle-data:/images/')));
      {$endif}
    {$endif}
  {$endif}
  AssertTrue(FileExists(URIToFilenameSafe('castle-data:/test.xml')));
  AssertTrue(not FileExists(URIToFilenameSafe('castle-data:/test-not-existing.xml')));

  { Our RegularFileExists does not detect directory as "file". }
  AssertFalse(RegularFileExists(URIToFilenameSafe('castle-data:/')));
  AssertFalse(RegularFileExists(URIToFilenameSafe('castle-data:/images/')));
  AssertTrue(RegularFileExists(URIToFilenameSafe('castle-data:/test.xml')));
  AssertTrue(not RegularFileExists(URIToFilenameSafe('castle-data:/test-not-existing.xml')));

  { DirectoryExists detects directories, not regular files. }
  AssertTrue(DirectoryExists(URIToFilenameSafe('castle-data:/')));
  AssertTrue(DirectoryExists(URIToFilenameSafe('castle-data:/images/')));
  AssertTrue(not DirectoryExists(URIToFilenameSafe('castle-data:/test.xml')));
  AssertTrue(not DirectoryExists(URIToFilenameSafe('castle-data:/test-not-existing.xml')));
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestSysUtils);
{$endif}
end.
