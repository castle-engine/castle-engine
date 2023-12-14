{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalDataCompression unit. }
unit TestCastleInternalDataCompression;

interface

uses Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit,
  TestUtils, TestRegistry, CastleTestCase, {$else}CastleTester, {$endif}
  CastleUtils, CastleClassUtils;

type
  TTestCastleInternalDataCompression = class(TCastleTestCase)
  published
    procedure TestChannelsSplitCombine;
    procedure TestRleCompression;
  end;

implementation

uses CastleInternalDataCompression, CastleLog, CastleStringUtils;

{$I test_font_image_to_compress.inc}

{ Like CompareMem, but slower,
  and when the memory is different, log the difference:
  position and the 2 different bytes. }
function CompareMemDebug(const P1, P2: Pointer; const Size: Int64): Boolean;
var
  I: Int64;
  P1B, P2B: PByte;
begin
  Result := true;
  P1B := P1;
  P2B := P2;
  for I := 0 to Size - 1 do
    if P1B^ <> P2B^ then
    begin
      WritelnLog('Difference at %d: %d <> %d', [I, P1B^, P2B^]);
      Exit(false);
      Inc(P1B);
      Inc(P2B);
    end;
end;

{ TTestCastleInternalDataCompression ----------------------------------------- }

procedure TTestCastleInternalDataCompression.TestChannelsSplitCombine;
begin
end;

procedure TTestCastleInternalDataCompression.TestRleCompression;
var
  Initial, Compressed, Decompressed: TMemoryStream;
begin
  Initial := TMemoryStream.Create;
  try
    Initial.Size := SizeOf(FontImagePixels);
    Move(FontImagePixels[0], Initial.Memory^, Initial.Size);

    Compressed := TMemoryStream.Create;
    try
      RleCompress(Initial.Memory, Initial.Size, Compressed);

      WritelnLog('Compressed %d (%s) to %d (%s), ratio %f', [
        Initial.Size, SizeToStr(Initial.Size),
        Compressed.Size, SizeToStr(Compressed.Size),
        Compressed.Size / Initial.Size
      ]);

      Compressed.Position := 0;

      Decompressed := TMemoryStream.Create;
      try
        RleDecompress(Compressed.Memory, Compressed.Size, Decompressed);
        AssertEquals(Initial.Size, Decompressed.Size);
        AssertTrue(CompareMemDebug(Decompressed.Memory, Initial.Memory, Initial.Size));
        (*
        TODO: CompareByte and CompareMem seem to be wrong here!

        They detect the memory differs, while it is really the same,
        shown by CompareMemDebug and looking at dump of it.
        Observed with FPC 3.2.2 on Linux/x86_64.
        Test on other compilers, OS, architectures and submit as FPC bug.
        *)

        WritelnLog('CompareByte (should be 0) %d', [
          CompareByte(Decompressed.Memory^, Initial.Memory^, Initial.Size)
        ]);
        WritelnLog('CompareMem (should be true) %s', [
          BoolToStr(CompareMem(Decompressed.Memory, Initial.Memory, Initial.Size), true)
        ]);

        (*
        Once/where this is fixed, use AssertTrue(CompareMem(...))
        instead of CompareMemDebug. Use slower CompareMemDebug only
        when CompareMem answers false, to get precise information what failed.
        *)
      finally FreeAndNil(Decompressed) end;
    finally FreeAndNil(Compressed) end;
  finally FreeAndNil(Initial) end;
end;

initialization
  RegisterTest(TTestCastleInternalDataCompression);
end.
