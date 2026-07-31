{ Note: This file must have UTF-8 BOM! This makes sure FPC and Delphi
  interpret CorrectPolishUtf8 literal to contain UTF-8.
}

{
  Copyright 2017-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test some Pascal compiler (FPC, Delphi) stuff.
  These tests are independent from CGE. }
unit TestCompiler;

{ Needed to define EXTENDED_EQUALS_DOUBLE on some platforms/compilers. }
{$I ../../../src/common_includes/castleconf.inc}

interface

uses
  Classes, SysUtils, CastleTester;

type
  TTestCompiler = class(TCastleTestCase)
  strict private
    procedure CheckAnsiPolish(const AnsiPolish: AnsiString);
    procedure CheckUtf8Polish(const Utf8Polish: Utf8String; const AnsiPolish: AnsiString);
    procedure CheckBackToAnsi(const AnsiPolish, BackToAnsi: AnsiString);
  published
    procedure TestIs;
    procedure TestSinglePrecision;
    procedure TestCTypesSizes;
    procedure TestSizes;
    procedure TestPackedOpenArray;
    procedure TestAnsiStringUtf8Conversion_Ansi1250;
    procedure TestAnsiStringUtf8Conversion_AnsiDefault;
  end;

implementation

uses CTypes,
  CastleUtils, CastleVectors, CastleLog;

type
  TFruit = class
  end;

  TApple = class(TFruit)
  end;

  TWerewolf = class
  end;

procedure TTestCompiler.TestIs;
var
  O: TObject;
begin
  O := TWerewolf.Create;
  try
    AssertTrue(O is TWerewolf);
    AssertFalse(O is TFruit);
    AssertFalse(O is TApple);
  finally FreeAndNil(O) end;

  O := TApple.Create;
  try
    AssertFalse(O is TWerewolf);
    AssertTrue(O is TFruit);
    AssertTrue(O is TApple);
  finally FreeAndNil(O) end;

  O := nil;
  try
    AssertFalse(O is TWerewolf);
    AssertFalse(O is TFruit);
    AssertFalse(O is TApple);
  finally FreeAndNil(O) end;
end;

procedure TTestCompiler.TestSinglePrecision;
var
  I: Integer;
begin
  for I := -32000 to 32000 do
    AssertEquals(I, Round(Single(I)));
end;

procedure TTestCompiler.TestCTypesSizes;
begin
  AssertEquals(1, SizeOf(CInt8));
  AssertEquals(1, SizeOf(CUInt8));

  AssertEquals(2, SizeOf(CInt16));
  AssertEquals(2, SizeOf(CUInt16));

  AssertEquals(4, SizeOf(CInt32));
  AssertEquals(4, SizeOf(CUInt32));

  AssertEquals(8, SizeOf(CInt64));
  AssertEquals(8, SizeOf(CUInt64));
end;

procedure TTestCompiler.TestSizes;
begin
  AssertEquals(1, SizeOf(Byte));
  AssertEquals(1, SizeOf(ShortInt));

  AssertEquals(2, SizeOf(Word));
  AssertEquals(2, SizeOf(SmallInt));

  AssertEquals(4, SizeOf(Int32));
  AssertEquals(4, SizeOf(UInt32));

  AssertEquals(8, SizeOf(Int64));
  AssertEquals(8, SizeOf(UInt64));
  AssertEquals(8, SizeOf(QWord));

  { Both in FPC and Delphi, Integer/Cardinal remained 4-byte (even though
    in old days the Integer/Cardinal were documented as potentially
    platform-dependent size).
    See (Delphi): https://docwiki.embarcadero.com/RADStudio/Sydney/en/Simple_Types_(Delphi) }
  AssertEquals(4, SizeOf(Integer));
  AssertEquals(4, SizeOf(Cardinal));

  AssertEquals(4, SizeOf(Single));
  AssertEquals(8, SizeOf(Double));

  AssertEquals(
    {$if defined(EXTENDED_EQUALS_DOUBLE)} 8
    {$elseif defined(EXTENDED_EQUALS_LONG_DOUBLE)} 16
    {$else} 10
    {$endif}, SizeOf(Extended));
end;

type
  { TCastleRenderUnlitMesh implementation assumes that "array of UInt16",
    "array of TVector4" parameters are packed arrays.
    But in Delphi we cannot declare them as such.
    So let's check ar runtime they are packed. }
  TCastleRenderUnlitMeshTest = class
  public
    procedure SetIndexes(const Indexes: array of UInt16);
    procedure SetVertexes(const Vertexes: array of TVector4;
      const UsageDynamic: Boolean);
  end;

procedure TCastleRenderUnlitMeshTest.SetIndexes(const Indexes: array of UInt16);
var
  I: Integer;
begin
  {$I norqcheckbegin.inc}
  for I := 0 to 100 do
    Check(PtrUInt(@Indexes[I + 1]) - PtrUInt(@Indexes[I]) = SizeOf(UInt16));
  {$I norqcheckend.inc}
end;

procedure TCastleRenderUnlitMeshTest.SetVertexes(const Vertexes: array of TVector4;
  const UsageDynamic: Boolean);
var
  I: Integer;
begin
  Check(High(Vertexes) = 3);
  Check(TVector4.PerfectlyEquals(Vertexes[0], Vector4(1, 1.2, 1.3, 1.4)));
  {$I norqcheckbegin.inc}
  for I := 0 to 100 do
    Check(PtrUInt(@Vertexes[I + 1]) - PtrUInt(@Vertexes[I]) = SizeOf(TVector4));
  {$I norqcheckend.inc}
end;

procedure TTestCompiler.TestPackedOpenArray;
var
  Mesh: TCastleRenderUnlitMeshTest;
begin
  Mesh := TCastleRenderUnlitMeshTest.Create;
  try
    Mesh.SetIndexes([1, 2, 3, 4]);
    Mesh.SetVertexes([
      Vector4(1, 1.2, 1.3, 1.4),
      Vector4(2, 2.2, 2.3, 2.4),
      Vector4(3, 3.2, 3.3, 3.4),
      Vector4(4, 4.2, 4.3, 4.4)
    ], true);
  finally FreeAndNil(Mesh) end;
end;

{ Common utilities for TestAnsiStringUtf8Conversion_* }

const
  { This means 'snake moth foal SNAKE MOTH FOAL' in Polish.
    It happens to use a few special characters from the Polish alphabet.
    Most Polish words can be written using only ASCII characters,
    so we deliberately choose a few words that use special Polish letters,
    to test UTF-8 <-> ANSI conversions.

    Note: This must be declared as Utf8String, otherwise
    neither FPC nor Delphi give us useful information to compare.
    If ": Utf8String" is omitted, then FPC and Delphi both report
    Length(CorrectPolishUtf8) = 29, likely indicating:
    - FPC: converted it to AnsiString with ANSI encoding
    - Delphi: converted it to UnicodeString with UTF-16 encoding

    IOW, in neither case do we get UTF-8 by default.
  }
  CorrectPolishUtf8: Utf8String = 'wąż ćma źrebię WĄŻ ĆMA ŹREBIĘ';

  { We define contents on AnsiString this way, as assignment like

    AnsiPolish :=
      'w' +
      #$B9 + // ą
      #$BF + // ż
      ...

    results in wrong characters being assigned.
    The interpretation may depend on current system codepage,
    and be overridden by the current file being UTF-8 BOM.
    Avoid all this: we want to literally specify the bytes in AnsiString,
    so we use array of Byte,
    which we will later copy to AnsiString. }
  AnsiPolishContents: array [1..29] of Byte = (
    Ord('w'),
    $B9, // ą
    $BF, // ż
    Ord(' '),
    $E6, // ć
    Ord('m'),
    Ord('a'),
    Ord(' '),
    $9F, // ź
    Ord('r'),
    Ord('e'),
    Ord('b'),
    Ord('i'),
    $EA, // ę
    Ord(' '),

    Ord('W'),
    $A5, // Ą
    $AF, // Ż
    Ord(' '),
    $C6, // Ć
    Ord('M'),
    Ord('A'),
    Ord(' '),
    $8F, // Ź
    Ord('R'),
    Ord('E'),
    Ord('B'),
    Ord('I'),
    $CA // Ę
  );

{ AnsiPolish looks good. }
procedure TTestCompiler.CheckAnsiPolish(const AnsiPolish: AnsiString);
begin
  AssertEquals(1250, StringCodePage(AnsiPolish));
  AssertEquals($B9, Ord(AnsiPolish[2]));
end;

{ Utf8Polish looks good. }
procedure TTestCompiler.CheckUtf8Polish(const Utf8Polish: Utf8String; const AnsiPolish: AnsiString);
var
  I: Integer;
begin
  AssertEquals(CP_UTF8, StringCodePage(Utf8Polish));

  // UTF-8 uses more bytes than 1-byte for some chars
  WritelnLog('CheckUtf8Polish: AnsiPolish length = %d, Utf8Polish length = %d', [
    Length(AnsiPolish),
    Length(Utf8Polish)
  ]);
  AssertTrue(Length(Utf8Polish) > Length(AnsiPolish));

  { Analyze that the bytes in Utf8Polish are indeed UTF-8 encoding of the Polish letters. }
  AssertEquals(CorrectPolishUtf8, Utf8Polish);
  AssertEquals(Length(CorrectPolishUtf8), Length(Utf8Polish));
  for I := 1 to Length(CorrectPolishUtf8) do
    AssertEquals(Ord(CorrectPolishUtf8[I]), Ord(Utf8Polish[I]));
end;

{ BackToAnsi looks good and equal to AnsiPolish. }
procedure TTestCompiler.CheckBackToAnsi(const AnsiPolish, BackToAnsi: AnsiString);
var
  I: Integer;
begin
  AssertEquals(1250, StringCodePage(BackToAnsi));
  AssertEquals($B9, Ord(BackToAnsi[2]));

  AssertEquals(AnsiPolish, BackToAnsi);
  AssertEquals(Length(AnsiPolish), Length(BackToAnsi));
  for I := 1 to Length(AnsiPolish) do
    AssertEquals(Ord(AnsiPolish[I]), Ord(BackToAnsi[I]));
end;

procedure TTestCompiler.TestAnsiStringUtf8Conversion_Ansi1250;

{ Test that, regardless of CASTLE_DONT_CHANGE_STRING_ENCODING,
  Utf8String will contain UTF-8 encoded data even if regular AnsiString
  has platform-specific encoding (like Windows-1250 on Polish Windows).

  This test uses explicit declaration "type AnsiString(1250)" and thus
  should work on any system, regardless of current system codepage. }

type
  TAnsiStringPolish = type AnsiString(1250); // Windows-1250, Polish codepage
var
  AnsiPolish, BackToAnsi: TAnsiStringPolish;
  Utf8Polish: System.UTF8String;
begin
  (*This check should not be necessary, as we declare TAnsiStringPolish
    with codepage 1250, so the test should work regardless of current
    system codepage.

  if DefaultSystemCodePage <> 1250 then
  begin
    WritelnLog('DefaultSystemCodePage = %d, not Polish Windows, skipping test', [DefaultSystemCodePage]);
    AbortTest;
    Exit;
  end;
  WritelnLog('DefaultSystemCodePage = 1250, Polish Windows, proceeding with test');
  *)

  (*No hacking of SetMultiByteConversionCodePage should be necessary for this
    test, as we declare TAnsiStringPolish with codepage 1250.

  {$ifndef CASTLE_DONT_CHANGE_STRING_ENCODING}
  // revert work done by CastleUtils initialization
  SetMultiByteConversionCodePage(1250);
  {$endif}
  *)

  {$if (not defined(CASTLE_DONT_CHANGE_STRING_ENCODING)) and defined(FPC)}
  { If SetMultiByteConversionCodePage(CP_UTF8) was done in CastleUtils
    initialization:
    - FPC 3.2.2 will assign UTF-8 codepage to AnsiPolish in this case.
    - Delphi will keep it as 1250.
    Abort this test on FPC + not defined CASTLE_DONT_CHANGE_STRING_ENCODING.
  }
  AbortTest;
  Exit;
  {$endif}

  { This test only makes sense if current system default is 1250. }

  { Manually set bytes following Polish Windows codepage 1250,
    to be sure we are testing what we want.
    See https://en.wikipedia.org/wiki/Windows-1250 }
  SetLength(AnsiPolish, Length(AnsiPolishContents));
  Move(AnsiPolishContents[1], AnsiPolish[1], Length(AnsiPolishContents));
  {$ifdef FPC}
  if StringCodePage(AnsiPolish) = 0 then
  begin
    WritelnLog('StringCodePage(AnsiPolish) = 0, possible with FPC on Linux without locale configured (like in typical Docker or CI environments), skipping test');
    AbortTest;
    Exit;
  end;
  {$endif}
  CheckAnsiPolish(AnsiPolish);

  { Both explicit (with AnsiToUtf8 or Utf8ToAnsi) or implicit conversions
    will work with both FPC and Delphi,
    as we declared TAnsiStringPolish with codepage 1250. }

  Utf8Polish := AnsiToUtf8(AnsiPolish);
  CheckUtf8Polish(Utf8Polish, AnsiPolish);

  Utf8Polish := AnsiPolish;
  CheckUtf8Polish(Utf8Polish, AnsiPolish);

  BackToAnsi := Utf8ToAnsi(Utf8Polish);
  CheckBackToAnsi(AnsiPolish, BackToAnsi);

  BackToAnsi := Utf8Polish;
  CheckBackToAnsi(AnsiPolish, BackToAnsi);
end;

procedure TTestCompiler.TestAnsiStringUtf8Conversion_AnsiDefault;

{ Test that, regardless of CASTLE_DONT_CHANGE_STRING_ENCODING,
  Utf8String will contain UTF-8 encoded data even if regular AnsiString
  has platform-specific encoding (like Windows-1250 on Polish Windows).

  This test uses declaration "AnsiString" and will only make sense
  if the current system codepage is 1250 (Polish Windows),
  as we hardcode some test values to Polish Windows codepage 1250.
  It will be skipped on other systems. }

var
  AnsiPolish, BackToAnsi: AnsiString;
  Utf8Polish: System.UTF8String;
begin
  { This test only makes sense if current system default is 1250.
    Note: When not defined CASTLE_DONT_CHANGE_STRING_ENCODING,
    this will be aborted, as DefaultSystemCodePage is then 65001 (UTF-8)
    and not 1250, for both FPC and Delphi. }
  if DefaultSystemCodePage <> 1250 then
  begin
    WritelnLog('DefaultSystemCodePage = %d, not Polish Windows (or CASTLE_DONT_CHANGE_STRING_ENCODING not defined, so system is UTF-8), skipping test', [DefaultSystemCodePage]);
    AbortTest;
    Exit;
  end;
  WritelnLog('DefaultSystemCodePage = 1250, Polish Windows (with CASTLE_DONT_CHANGE_STRING_ENCODING defined), proceeding with test');

  { Manually set bytes following Polish Windows codepage 1250,
    to be sure we are testing what we want.
    See https://en.wikipedia.org/wiki/Windows-1250 }
  SetLength(AnsiPolish, Length(AnsiPolishContents));
  Move(AnsiPolishContents[1], AnsiPolish[1], Length(AnsiPolishContents));
  CheckAnsiPolish(AnsiPolish);

  { Explicit conversion makes this work with FPC and Delphi.
    Delphi would work also with implicit conversion "Utf8Polish := AnsiPolish",
    as this is Polish Windows, but FPC 3.2.2 would not (it will assign codebase
    1250 to Utf8String, which is not correct).}
  Utf8Polish := AnsiToUtf8(AnsiPolish);
  CheckUtf8Polish(Utf8Polish, AnsiPolish);

  {$ifndef FPC}
  Utf8Polish := AnsiPolish;
  CheckUtf8Polish(Utf8Polish, AnsiPolish);
  {$endif}

  BackToAnsi := Utf8ToAnsi(Utf8Polish);
  CheckBackToAnsi(AnsiPolish, BackToAnsi);

  {$ifndef FPC}
  BackToAnsi := Utf8Polish;
  CheckBackToAnsi(AnsiPolish, BackToAnsi);
  {$endif}
end;

initialization
  RegisterTest(TTestCompiler);
end.
