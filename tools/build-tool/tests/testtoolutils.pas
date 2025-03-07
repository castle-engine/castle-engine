{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatic tests of functionality in ToolUtils. }
unit TestToolUtils;

interface

uses
  Classes, SysUtils, CastleTester,
  ToolUtils;

type
  TTestToolUtils = class(TCastleTestCase)
  published
    procedure TestZip;
  end;

implementation

uses CastleFilesUtils, CastleZip, CastleUriUtils, CastleClassUtils,
  ToolCommonUtils;

procedure TTestToolUtils.TestZip;

  function ZipFileToString(const Zip: TCastleZip; const PathInZip: String): String;
  var
    Stream: TStream;
    StringStream: TStringStream;
  begin
    Stream := Zip.Read(PathInZip);
    try
      StringStream := TStringStream.Create('');
      try
        ReadGrowingStream(Stream, StringStream, true);
        Result := StringStream.DataString;
      finally FreeAndNil(StringStream) end;
    finally FreeAndNil(Stream) end;
  end;

var
  TempDir, ZipFileName: String;
  Zip: TCastleZip;
begin
  TempDir := CreateTemporaryDir;
  try
    StringToFile(
      UriToFilenameSafe(CombinePaths(TempDir, 'zip_contents/subdir/file1.txt')), 'file1 contents');
    StringToFile(
      UriToFilenameSafe(CombinePaths(TempDir, 'zip_contents/file2.txt')), 'file2 contents');

    // create zip with SingleTopLevelDirectory = true
    ZipFileName := CombinePaths(TempDir, 'test.zip');
    Writeln('ZipFileName: ', ZipFileName);
    ZipDirectoryTool(ZipFileName, CombinePaths(TempDir, 'zip_contents'), true);

    // test zip contents are valid
    Zip := TCastleZip.Create;
    try
      Zip.Open(FilenameToUriSafe(ZipFileName));
      Writeln('Zip contents: ', Zip.Files.Text);
      AssertEquals(2, Zip.Files.Count);
      AssertTrue(Zip.Files.IndexOf('zip_contents/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('zip_contents/subdir/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('zip_contents/file2.txt') <> -1);
      AssertTrue(Zip.Files.IndexOf('zip_contents/subdir/file1.txt') <> -1);
      AssertEquals('file1 contents', ZipFileToString(Zip, 'zip_contents/subdir/file1.txt'));
      AssertEquals('file2 contents', ZipFileToString(Zip, 'zip_contents/file2.txt'));
    finally FreeAndNil(Zip) end;

    // create zip with SingleTopLevelDirectory = false
    ZipFileName := CombinePaths(TempDir, 'test2.zip');
    ZipDirectoryTool(ZipFileName, CombinePaths(TempDir, 'zip_contents'), false);

    // test zip contents are valid
    Zip := TCastleZip.Create;
    try
      Zip.Open(FilenameToUriSafe(ZipFileName));
      Writeln('Zip contents: ', Zip.Files.Text);
      AssertEquals(2, Zip.Files.Count);
      AssertTrue(Zip.Files.IndexOf('subdir/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('file2.txt') <> -1);
      AssertTrue(Zip.Files.IndexOf('subdir/file1.txt') <> -1);
      AssertEquals('file1 contents', ZipFileToString(Zip, 'subdir/file1.txt'));
      AssertEquals('file2 contents', ZipFileToString(Zip, 'file2.txt'));
    finally FreeAndNil(Zip) end;
  finally
    RemoveNonEmptyDir(TempDir);
  end;
end;

initialization
  RegisterTest(TTestToolUtils);
end.
