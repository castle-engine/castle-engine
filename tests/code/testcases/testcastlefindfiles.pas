// -*- compile-command: "./test_single_testcase.sh TTestCastleFindFiles" -*-
{
  Copyright 2021-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleFindFiles unit. }
unit TestCastleFindFiles;

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleFindFiles = class(TCastleTestCase)
  published
    procedure TestNilHandler;
    procedure TestFindFirstCaseSensitive;
  end;

implementation

uses CastleFindFiles, CastleURIUtils;

procedure TTestCastleFindFiles.TestNilHandler;
begin
  AssertEquals(4, FindFiles('castle-data:/designs_to_count_files/', '*.castle-user-interface', false, nil, [ffRecursive]));
end;

procedure TTestCastleFindFiles.TestFindFirstCaseSensitive;
var
  FileInfo: TFileInfo;
  DataLocalSystem: String;
begin
  AssertTrue(FindFirstFile('castle-data:/designs_to_count_files/', '*.castle-user-interface', false, [], FileInfo));
  AssertTrue(FindFirstFileIgnoreCase('castle-data:/designs_to_count_files/', '*.castle-user-interface', false, [], FileInfo));

  // depends on OS, and whether castle-data contents are defined in XML
  //AssertTrue(FindFirstFile('castle-data:/designs_to_count_files/', '*.CASTLE-user-interface', false, [], FileInfo));

  // guaranteed to be true in all situations
  AssertTrue(FindFirstFileIgnoreCase('castle-data:/designs_to_count_files/', '*.CASTLE-user-interface', false, [], FileInfo));

  DataLocalSystem := URIToFilenameSafe(ResolveCastleDataUrl('castle-data:/'));

  if DataLocalSystem <> '' then
  begin
    //Writeln(DataLocalSystem);
    AssertTrue(FindFirstFile(DataLocalSystem + 'designs_to_count_files/', '*.castle-user-interface', false, [], FileInfo));
    AssertTrue(FindFirstFileIgnoreCase(DataLocalSystem + 'designs_to_count_files/', '*.castle-user-interface', false, [], FileInfo));

    // depends on OS, and whether castle-data contents are defined in XML
    //AssertTrue(FindFirstFile(DataLocalSystem + 'designs_to_count_files/', '*.CASTLE-user-interface'));

    // guaranteed to be true in all situations
    AssertTrue(FindFirstFileIgnoreCase(DataLocalSystem + 'designs_to_count_files/', '*.CASTLE-user-interface', false, [], FileInfo));
  end;
end;

initialization
  RegisterTest(TTestCastleFindFiles);
end.
