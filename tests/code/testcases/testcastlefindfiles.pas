// -*- compile-command: "./test_single_testcase.sh TTestCastleFindFiles" -*-
{
  Copyright 2021-2025 Michalis Kamburelis.

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

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCastleFindFiles = class(TCastleTestCase)
  published
    procedure TestNilHandler;
    procedure TestFindFirstCaseSensitive;
    { Test that searching inside castle-config:/ returns URLs that
      start with castle-config:/ . }
    procedure TestFindFilesCastleConfig;
  end;

implementation

uses CastleFindFiles, CastleUriUtils, CastleFilesUtils;

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

  DataLocalSystem := UriToFilenameSafe(ResolveCastleDataUrl('castle-data:/'));

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

procedure TTestCastleFindFiles.TestFindFilesCastleConfig;
var
  FoundList: TFileInfoList;
  TestDirUrl, TestDir: String;
begin
  if not CanUseCastleConfig then
  begin
    AbortTest;
    Exit;
  end;

  TestDirUrl := 'castle-config:/test-find-files-' + IntToStr(Random(10000)) + '/';
  StringToFile(TestDirUrl + 'test1.txt', 'Test string.');
  StringToFile(TestDirUrl + 'test2.txt', 'Test string 2.');
  StringToFile(TestDirUrl + 'subdir/test3.txt', 'Test string 3.');
  FoundList := FindFilesList(TestDirUrl, '*.txt', false, [ffRecursive]);

  try
    AssertEquals(3, FoundList.Count);
    FoundList.SortUrls; // make order defined
    AssertEquals(TestDirUrl + 'subdir/test3.txt', FoundList[0].Url);
    AssertEquals(TestDirUrl + 'test1.txt', FoundList[1].Url);
    AssertEquals(TestDirUrl + 'test2.txt', FoundList[2].Url);
  finally
    FreeAndNil(FoundList);
  end;

  TestDir := UriToFilenameSafe(TestDirUrl);
  Writeln(Format('Make tests in %s which maps to %s on disk', [
    TestDirUrl,
    TestDir
  ]));
  RemoveNonEmptyDir(TestDir, true);
end;

initialization
  RegisterTest(TTestCastleFindFiles);
end.
