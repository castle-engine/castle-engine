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
    procedure TestSearchSpacesPercentEncoding;
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
  if TestDir <> '' then
  begin
    Writeln(Format('Made tests in %s which maps to %s on disk', [
      TestDirUrl,
      TestDir
    ]));
    RemoveNonEmptyDir(TestDir, true);
  end else
    Writeln(Format('Made tests in %s which does not map to normal filesystem, assuming temp storage on web, not cleaning up', [
      TestDirUrl
    ]));
end;

procedure TTestCastleFindFiles.TestSearchSpacesPercentEncoding;
var
  List: TFileInfoList;
begin
  List := FindFilesList('castle-data:/',
    { Mask for FindFilesList is @italic(not) percent-encoded, it is used as-is. }
    'file name with spaces.txt', false, []);
  try
    AssertEquals(1, List.Count);
    AssertTrue(0 <> Pos('%20', List[0].Url));
    AssertEquals(
      { It's undefined whether we get castle-data:/ or resolved file:/...,
        so do ResolveCastleDataUrl to make test always correct. }
      ResolveCastleDataUrl('castle-data:/file%20name%20with%20spaces.txt'),
      ResolveCastleDataUrl(List[0].Url));
  finally
    FreeAndNil(List);
  end;

  List := FindFilesList('castle-data:/',
    { This will look for files with % in names, not for files with spaces,
      so it should not find anything. }
    'file%20name%20with%20spaces.txt', false, []);
  try
    AssertEquals(0, List.Count);
  finally
    FreeAndNil(List);
  end;

  List := FindFilesList('castle-data:/file%20name%20with%20spaces.txt', false, []);
  try
    AssertEquals(1, List.Count);
    AssertEquals(
      ResolveCastleDataUrl('castle-data:/file%20name%20with%20spaces.txt'),
      ResolveCastleDataUrl(List[0].Url));
  finally
    FreeAndNil(List);
  end;

  { This is undefined -- it's an invalid URL with literal spaces.
    Always encode spaces as %20 in URLs. }
  // List := FindFilesList('castle-data:/file name with spaces.txt', false, []);
  // try
  //   AssertEquals(0, List.Count);
  // finally
  //   FreeAndNil(List);
  // end;
end;

initialization
  RegisterTest(TTestCastleFindFiles);
end.
