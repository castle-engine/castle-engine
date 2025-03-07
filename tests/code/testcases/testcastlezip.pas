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
  end;

implementation

uses
  {$if defined(CASTLE_ONLINE_TESTS) and defined(FPC)}
  OpenSslSockets,
  {$endif}
  CastleZip, CastleUriUtils, CastleClassUtils, CastleDownload,
  CastleUtils;

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
      AssertTrue(Zip.Entries.IndexOf(PathInZip) <> -1);
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
    Writeln('ZIP contents: ', Zip.Entries.Text);
    AssertEquals(5, Zip.Entries.Count);

    // test that subdir/ is listed in Entries, and with trailing slash
    AssertTrue(Zip.Entries.IndexOf('subdir/') <> -1);

    CompareZip('test filename żółć.txt');
    CompareZip('test.txt');
    CompareZip('test_texture.png');
    // test also that / is treated as directory separator in ZIP
    CompareZip('subdir/test filename żółć in subdir.txt');
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
    AssertEquals(0, Zip.Entries.Count);
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

initialization
  RegisterTest(TTestCastleZip);
end.
