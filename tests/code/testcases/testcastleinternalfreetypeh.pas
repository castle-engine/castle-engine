{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalFreeTypeH. }
unit TestCastleInternalFreeTypeH;

{$I ../../../src/common_includes/castleconf.inc}

interface

uses CastleTester;

type
  TTestCastleInternalFreeTypeH = class(TCastleTestCase)
  published
    procedure TestTypeSizes;
    procedure TestLibraryVersion;
  end;

implementation

uses CastleInternalFreeTypeH;

procedure TTestCastleInternalFreeTypeH.TestTypeSizes;
var
  B: FT_Bitmap;
begin
  {$ifdef CPU64}
  AssertEquals(40, SizeOf(FT_Bitmap));
  {$else}
  AssertEquals(24, SizeOf(FT_Bitmap));
  {$endif}
  // This compiles with FPC, but not Delphi
  //AssertEquals(2, SizeOf(FT_Bitmap.num_grays));
  // This compiles with both
  AssertEquals(2, SizeOf(B.num_grays));

  { Note: on 16-bit CPUs, this would need a bit adjustment
    (and checking FreeType actual sizes using C testcase).
    See CTypes definitions of CLong etc. in this case.
    Doesn't matter now, we don't support any 16-bit CPU. }
  {$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}
  AssertEquals(8, SizeOf(FT_Long));
  AssertEquals(8, SizeOf(FT_ULong));
  AssertEquals(8, SizeOf(FT_Pos));
  {$else}
  AssertEquals(4, SizeOf(FT_Long));
  AssertEquals(4, SizeOf(FT_ULong));
  AssertEquals(4, SizeOf(FT_Pos));
  {$endif}
end;

procedure TTestCastleInternalFreeTypeH.TestLibraryVersion;
var
  Lib: PFT_Library;
  Major, Minor, Patch: Integer;
begin
  if not FreeTypeLibraryInitialized then
  begin
    AbortTest;
    Exit;
  end;

  AssertEquals(0, FT_Init_FreeType(Lib));
  try
    FT_Library_Version(Lib, Major, Minor, Patch);
    // Testcase may need adjustment in the future, when FreeType 3 is released
    AssertEquals(2, Major);
    AssertTrue(Minor >= 0);
    AssertTrue(Patch >= 0);
  finally
    AssertEquals(0, FT_Done_FreeType(Lib));
  end;

  // Like FreeType, report 0.0.0 for nil library
  FT_Library_Version(nil, Major, Minor, Patch);
  AssertEquals(0, Major);
  AssertEquals(0, Minor);
  AssertEquals(0, Patch);
end;

initialization
  RegisterTest(TTestCastleInternalFreeTypeH);
end.
