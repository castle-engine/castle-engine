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

{ Test CastleZip unit. }
unit TestCastleZip;

{ For these tests, we need to download files from the Internet.
  We also enable blocking downloads.
  This means that you need to be online, and tests may fail
  if servers are offline. }
{.$define CASTLE_ONLINE_TESTS}

interface

uses
  Classes, SysUtils,
  CastleTester;

type
  TTestCastleZip = class(TCastleTestCase)
  private
    procedure AssertStreamsEqual(const S1, S2: TStream);
  published
    procedure TestZipRead;
    procedure TestZipWrite;
    procedure TestZipDirectory;
    procedure TestZipWriteProtocol;
  end;

implementation

uses
  {$if defined(CASTLE_ONLINE_TESTS) and defined(FPC)}
  OpenSslSockets,
  {$endif}
  // for TPath.GetTempPath
  {$ifndef FPC} IOUtils, {$endif}
  CastleZip, CastleUriUtils, CastleClassUtils, CastleDownload,
  CastleUtils, CastleFilesUtils, CastleLog;

// Delphi 10.2 cannot read filenames with UTF-8 in our testcase
{$ifdef FPC}
  {$define CASTLE_FULL_ZIP_UNICODE_SUPPORT}
{$else}
  {$if CompilerVersion >= 36} // Delphi >= 12.0
    {$define CASTLE_FULL_ZIP_UNICODE_SUPPORT}
  {$endif}
{$endif}

{ Return URL of temporary directory. }
function CreateTemporaryDirUrl: String;
var
  Base: String;
begin
  Base := {$ifdef FPC} GetTempDir {$else} TPath.GetTempPath {$endif};
  Result := InclPathDelim(
    InclPathDelim(Base) + 'TTestCastleZip-' + IntToStr(Random(1000000)));
  CheckForceDirectories(Result);
  Result := FilenameToUriSafe(Result);
  Writeln('Temporary directory: ', Result);
end;

{ Get contents of given file in ZIP as simple String. }
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

{ TTestCastleZip ------------------------------------------------------------ }

procedure TTestCastleZip.AssertStreamsEqual(const S1, S2: TStream);
var
  S1Memory, S2Memory: TMemoryStream;
begin
  // Note: This does not "rewind" S1 or S2,
  // thus checking that they are at the beginning,
  // once opened by Download or TCastleZip.Read.
  S1Memory := TMemoryStream.Create;
  try
    ReadGrowingStream(S1, S1Memory, true);
    S2Memory := TMemoryStream.Create;
    try
      ReadGrowingStream(S2, S2Memory, true);
      AssertEquals(S1Memory.Size, S2Memory.Size);
      AssertTrue(CompareMem(S1Memory.Memory, S2Memory.Memory, S1Memory.Size));
    finally FreeAndNil(S2Memory) end;
  finally FreeAndNil(S1Memory) end;
end;

procedure TTestCastleZip.TestZipRead;
var
  Zip: TCastleZip;

  procedure CompareZip(const PathInZip: String);
  var
    StreamZip: TStream;
    StreamOutsideZip: TStream;
    DataUrl: String;
  begin
    // use InternalUriEscape to encode characters like spaces and Polish inside URL
    DataUrl := 'castle-data:/zip/' + InternalUriEscape(PathInZip);
    try
      AssertTrue(Zip.Files.IndexOf(PathInZip) <> -1);
      StreamZip := Zip.Read(PathInZip);
      try
        StreamOutsideZip := Download(DataUrl);
        try
          AssertStreamsEqual(StreamZip, StreamOutsideZip);
        finally FreeAndNil(StreamOutsideZip) end;
      finally FreeAndNil(StreamZip) end;
    except
      on E: EAssertionFailedError do
      begin
        E.Message := E.Message + NL +
          '  PathInZip: ' + PathInZip + NL +
          '  DataUrl: ' + DataUrl;
        raise;
      end;
    end;
  end;

  procedure CheckZipContents;
  begin
    Writeln('ZIP contents: ', Zip.Files.Text);
    AssertEquals(4, Zip.Files.Count);

    // test that subdir/ is not listed in Files
    AssertTrue(Zip.Files.IndexOf('subdir/') = -1);

    {$ifdef CASTLE_FULL_ZIP_UNICODE_SUPPORT}
    CompareZip('test filename żółć.txt');
    {$endif}
    CompareZip('test.txt');
    CompareZip('test_texture.png');
    // test also that / is treated as directory separator in ZIP
    {$ifdef CASTLE_FULL_ZIP_UNICODE_SUPPORT}
    CompareZip('subdir/test filename żółć in subdir.txt');
    {$endif}
  end;

var
  ZipUrl: String;
begin
  // Use InternalUriEscape to encode characters like spaces and Polish inside URL.
  // We deliberately use "żółć" and Polish in the filename, to test that it works.
  ZipUrl := 'castle-data:/zip/' + InternalUriEscape('packed żółć.zip');

  // make sure file-checking routines handle ZipUrl OK
  AssertTrue(UriExists(ZipUrl) = ueFile);
  AssertTrue(FileExists(UriToFilenameSafe(ZipUrl)));

  // test opening URL with TCastleZip.Open
  Zip := TCastleZip.Create;
  try
    Zip.Open(ZipUrl);
    CheckZipContents;
  finally FreeAndNil(Zip) end;

  // test opening TStream with TCastleZip.Open
  Zip := TCastleZip.Create;
  try
    Zip.Open(Download(ZipUrl), true);
    CheckZipContents;
  finally FreeAndNil(Zip) end;

  // repeat above, but without recreating Zip instance
  Zip := TCastleZip.Create;
  try
    Zip.Open(ZipUrl);
    CheckZipContents;
    Zip.Close; // necessary, and valid - we need to release TFileStream to access Download(ZipUrl)

    Zip.Open(Download(ZipUrl), true);
    CheckZipContents;

    Zip.OpenEmpty;
    AssertEquals(0, Zip.Files.Count);
  finally FreeAndNil(Zip) end;

  // open https URL with ZIP
  {$if defined(CASTLE_ONLINE_TESTS)}
  EnableBlockingDownloads := true;
  Zip := TCastleZip.Create;
  try
    Zip.Open('https://github.com/castle-engine/castle-engine/raw/refs/heads/master/tests/data/zip/packed%20%C5%BC%C3%B3%C5%82%C4%87.zip');
    CheckZipContents;
  finally FreeAndNil(Zip) end;
  {$endif}
end;

procedure TTestCastleZip.TestZipWrite;
var
  Zip: TCastleZip;
  TempDir, File1Url, ZipUrl: String;
  File2Stream, File3Stream: TStringStream;
begin
  TempDir := CreateTemporaryDirUrl;
  try
    File1Url := CombineUri(TempDir, InternalUriEscape(
      'zip_contents/subdir/file1 with spaces and Polish chars żółć.txt'));
    StringToFile(File1Url, 'file1 contents');

    ZipUrl := CombineUri(TempDir, 'test.zip');

    // write zip
    Zip := TCastleZip.Create;
    try
      Zip.OpenEmpty;
      Zip.Write('subdir/file1 with spaces and Polish chars żółć.txt', File1Url);
      File2Stream := TStringStream.Create('file2 contents');
      Zip.Write('file2.txt', File2Stream, true);
      File3Stream := TStringStream.Create('file3 contents');
      try
        // test OwnsStream = false
        Zip.Write('file3.txt', File3Stream, false);
      finally FreeAndNil(File3Stream) end;
      Zip.Save(ZipUrl);
      AssertTrue(Zip.IsOpen);
    finally FreeAndNil(Zip) end;

    // test zip contents are valid
    Zip := TCastleZip.Create;
    try
      Zip.Open(ZipUrl);
      Writeln('Zip contents: ', Zip.Files.Text);
      AssertEquals(3, Zip.Files.Count);
      AssertTrue(Zip.Files.IndexOf('subdir/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('file2.txt') <> -1);
      AssertTrue(Zip.Files.IndexOf('subdir/file1 with spaces and Polish chars żółć.txt') <> -1);
      AssertEquals('file1 contents', ZipFileToString(Zip, 'subdir/file1 with spaces and Polish chars żółć.txt'));
      AssertEquals('file2 contents', ZipFileToString(Zip, 'file2.txt'));
      AssertEquals('file3 contents', ZipFileToString(Zip, 'file3.txt'));
    finally FreeAndNil(Zip) end;
  finally
    RemoveNonEmptyDir(UriToFilenameSafe(TempDir));
  end;
end;

procedure TTestCastleZip.TestZipWriteProtocol;
var
  Zip: TCastleZip;
  TempDir, ZipUrl: String;
  WriteStream, WriteStreamUnfinished: TStream;
begin
  TempDir := CreateTemporaryDirUrl;
  try
    ZipUrl := CombineUri(TempDir, 'test.zip');

    // write zip
    Zip := TCastleZip.Create;
    try
      Zip.OpenEmpty;
      Zip.RegisterUrlProtocol('TestZipWriteProtocol');
      WriteStream := UrlSaveStream('TestZipWriteProtocol:subdir/' + InternalUriEscape('file1 with spaces and Polish chars żółć.txt'));
      try
        WriteStr(WriteStream, 'file1 contents');
      finally FreeAndNil(WriteStream) end;
      WriteStreamUnfinished := UrlSaveStream('TestZipWriteProtocol:subdir/unfinished.txt');
      // simpler alternative to UrlSaveStream
      StringToFile('TestZipWriteProtocol:file2.txt', 'file2 contents');
      Zip.Save(ZipUrl);
      AssertTrue(Zip.IsOpen);
    finally FreeAndNil(Zip) end;

    { Above we made deliberate error:
      WriteStreamUnfinished was not freed before Zip,
      so it didn't write anything into ZIP.
      Test that this is handled correctly, no crashes, no memory leaks. }
    FreeAndNil(WriteStreamUnfinished);

    // test zip contents are valid
    Zip := TCastleZip.Create;
    try
      Zip.Open(ZipUrl);
      Writeln('Zip contents: ', Zip.Files.Text);
      AssertEquals(2, Zip.Files.Count);
      AssertTrue(Zip.Files.IndexOf('subdir/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('file2.txt') <> -1);
      AssertTrue(Zip.Files.IndexOf('subdir/file1 with spaces and Polish chars żółć.txt') <> -1);
      AssertEquals('file1 contents', ZipFileToString(Zip, 'subdir/file1 with spaces and Polish chars żółć.txt'));
      AssertEquals('file2 contents', ZipFileToString(Zip, 'file2.txt'));
    finally FreeAndNil(Zip) end;
  finally
    RemoveNonEmptyDir(UriToFilenameSafe(TempDir));
  end;
end;

procedure TTestCastleZip.TestZipDirectory;
var
  TempDir, ZipUrl: String;
  Zip: TCastleZip;
begin
  TempDir := CreateTemporaryDirUrl;
  try
    StringToFile(
      CombineUri(TempDir,
        InternalUriEscape('zip_contents/subdir/file1 with spaces and Polish chars żółć.txt')), 'file1 contents');
    StringToFile(
      CombineUri(TempDir, 'zip_contents/file2.txt'), 'file2 contents');

    // create zip with SingleTopLevelDirectory = true
    ZipUrl := CombineUri(TempDir, 'test.zip');
    Writeln('ZipUrl: ', ZipUrl);
    ZipDirectory(ZipUrl, CombineUri(TempDir, 'zip_contents'), true);

    // test zip contents are valid
    Zip := TCastleZip.Create;
    try
      Zip.Open(ZipUrl);
      Writeln('Zip contents: ', Zip.Files.Text);
      AssertEquals(2, Zip.Files.Count);
      AssertTrue(Zip.Files.IndexOf('zip_contents/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('zip_contents/subdir/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('zip_contents/file2.txt') <> -1);
      AssertTrue(Zip.Files.IndexOf('zip_contents/subdir/file1 with spaces and Polish chars żółć.txt') <> -1);
      AssertEquals('file1 contents', ZipFileToString(Zip, 'zip_contents/subdir/file1 with spaces and Polish chars żółć.txt'));
      AssertEquals('file2 contents', ZipFileToString(Zip, 'zip_contents/file2.txt'));
    finally FreeAndNil(Zip) end;

    // create zip with SingleTopLevelDirectory = false
    ZipUrl := CombineUri(TempDir, 'test2.zip');
    Writeln('ZipUrl: ', ZipUrl);
    ZipDirectory(ZipUrl, CombineUri(TempDir, 'zip_contents'), false);

    // test zip contents are valid
    Zip := TCastleZip.Create;
    try
      Zip.Open(ZipUrl);
      Writeln('Zip contents: ', Zip.Files.Text);
      AssertEquals(2, Zip.Files.Count);
      AssertTrue(Zip.Files.IndexOf('subdir/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('file2.txt') <> -1);
      AssertTrue(Zip.Files.IndexOf('subdir/file1 with spaces and Polish chars żółć.txt') <> -1);
      AssertEquals('file1 contents', ZipFileToString(Zip, 'subdir/file1 with spaces and Polish chars żółć.txt'));
      AssertEquals('file2 contents', ZipFileToString(Zip, 'file2.txt'));
    finally FreeAndNil(Zip) end;

    // create zip with SingleTopLevelDirectory = false
    // and place ZipUrl inside the packed directory
    ZipUrl := CombineUri(TempDir, 'zip_contents/test-inside.zip');
    StringToFile(ZipUrl, 'dummy contents, to test that ZipDirectory ignores this file');
    Writeln('ZipUrl: ', ZipUrl);
    ZipDirectory(ZipUrl, CombineUri(TempDir, 'zip_contents'), false);

    // test zip contents are valid
    Zip := TCastleZip.Create;
    try
      Zip.Open(ZipUrl);
      Writeln('Zip contents: ', Zip.Files.Text);
      AssertEquals(2, Zip.Files.Count);
      AssertTrue(Zip.Files.IndexOf('subdir/') = -1); // dir not listed
      AssertTrue(Zip.Files.IndexOf('file2.txt') <> -1);
      AssertTrue(Zip.Files.IndexOf('subdir/file1 with spaces and Polish chars żółć.txt') <> -1);
      AssertEquals('file1 contents', ZipFileToString(Zip, 'subdir/file1 with spaces and Polish chars żółć.txt'));
      AssertEquals('file2 contents', ZipFileToString(Zip, 'file2.txt'));
    finally FreeAndNil(Zip) end;
  finally
    RemoveNonEmptyDir(UriToFilenameSafe(TempDir));
  end;
end;

initialization
  RegisterTest(TTestCastleZip);
end.
