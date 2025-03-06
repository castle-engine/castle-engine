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

uses CastleZip, CastleUriUtils, CastleClassUtils, CastleDownload,
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
    DataUrl := 'castle-data:/zip/' + InternalUriEscape(PathInZip);
    try
      AssertTrue(Zip.FileList.IndexOf(PathInZip) <> -1);
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

begin
  Zip := TCastleZip.Create;
  try
    Zip.Open('castle-data:/zip/packed%20żółć.zip');
    // Writeln('ZIP contents: ', Zip.FileList.Text);
    AssertEquals(3, Zip.FileList.Count);
    CompareZip('test filename żółć.txt');
    CompareZip('test.txt');
    CompareZip('test_texture.png');
  finally FreeAndNil(Zip) end;
end;

initialization
  RegisterTest(TTestCastleZip);
end.
