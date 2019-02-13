{
  Copyright 2015-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test class containing useful assertion methods for base
  Castle Game Engine types.
  You can use this instead of TTestCase in your test applications.  }
unit CastleTestCase;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, fpcunit, testutils, CastleVectors, CastleBoxes,
  CastleImages, CastleRectangles;

type
  TCastleTestCase = class(TTestCase)
  public
    procedure AssertMatrixEquals(const Expected, Actual: TMatrix4;
      const Epsilon: Single; AErrorAddrs: Pointer = nil);

    procedure AssertVectorEquals(const Expected, Actual: TVector2Byte; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector3Byte; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector4Byte; AErrorAddrs: Pointer = nil);

    procedure AssertVectorEquals(const Expected, Actual: TVector2; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector3; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector4; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector2; const Epsilon: Single; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector3; const Epsilon: Single; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector4; const Epsilon: Single; AErrorAddrs: Pointer = nil);

    procedure AssertSameValue(const Expected, Actual: Single; AErrorAddrs: Pointer = nil);
    procedure AssertSameValue(const Expected, Actual: Single; const Epsilon: Single; AErrorAddrs: Pointer = nil);

    { TODO: Need to have different names to avoid FPC errors "duplicate ASM label",
      see https://bugs.freepascal.org/view.php?id=32188 }
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double; const Epsilon: Single; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double; const Epsilon: Single; AErrorAddrs: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double; const Epsilon: Single; AErrorAddrs: Pointer = nil);
    procedure AssertSameValue(const Expected, Actual: Double; AErrorAddrs: Pointer = nil);
    procedure AssertSameValue(const Expected, Actual: Double; const Epsilon: Double; AErrorAddrs: Pointer = nil);

    procedure AssertBoxesEqual(const Expected, Actual: TBox3D; AErrorAddrs: Pointer = nil);
    procedure AssertBoxesEqual(const Expected, Actual: TBox3D; const Epsilon: Double; AErrorAddrs: Pointer = nil);
    procedure AssertFilenamesEqual(const Expected, Actual: string; AErrorAddrs: Pointer = nil);
    procedure AssertImagesEqual(const Expected, Actual: TRGBAlphaImage; AErrorAddrs: Pointer = nil);
    procedure AssertRectsEqual(const Expected, Actual: TRectangle; AErrorAddrs: Pointer = nil);
    procedure AssertRectsEqual(const Expected, Actual: TFloatRectangle; AErrorAddrs: Pointer = nil);
  end;

implementation

uses Math,
  CastleUtils;

procedure TCastleTestCase.AssertMatrixEquals(
  const Expected, Actual: TMatrix4; const Epsilon: Single; AErrorAddrs: Pointer);
var
  DifferenceEpsilon: Single;
  I, J: TMatrix4.TIndex;
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TMatrix4.Equals(Expected, Actual, Epsilon) then
  begin
    DifferenceEpsilon := 0;
    for I in TMatrix4.TIndex do
      for J in TMatrix4.TIndex do
        MaxVar(DifferenceEpsilon, Abs(Expected[I, J] - Actual[I, J]));

    Fail(Format('Matrices (TMatrix4) are not equal:' + LineEnding +
      '  Expected:' + LineEnding +
      '%s' + LineEnding +
      '  Actual:' + LineEnding +
      '%s' + LineEnding +
      '  The epsilon to ignore the difference would need to be >= %.10f, but is %.10f',
      [Expected.ToRawString('    '),
       Actual.ToRawString('    '),
       DifferenceEpsilon,
       Epsilon
      ]), AErrorAddrs);
  end;
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2Byte; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector2Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector2Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3Byte; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector3Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector3Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4Byte; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector4Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector4Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2; const Epsilon: Single; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector2.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3; const Epsilon: Single; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector3.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4; const Epsilon: Single; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector4.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Single; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertSameValue(Expected, Actual, SingleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Single;
  const Epsilon: Single; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Single) are not equal: expected: %g, actual: %g',
      [Expected, Actual]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector2Double; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector3Double; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector4Double; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector2Double; const Epsilon: Single; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector2Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector3Double; const Epsilon: Single; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector3Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector4Double; const Epsilon: Single; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not TVector4Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Double; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertSameValue(Expected, Actual, DoubleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Double;
  const Epsilon: Double; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Double) are not equal: expected: %g, actual: %g',
      [Expected, Actual]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertBoxesEqual(Expected, Actual, SingleEpsilon, AErrorAddrs);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D;
  const Epsilon: Double; AErrorAddrs: Pointer);
var
  I: Integer;
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if Expected.IsEmpty and Actual.IsEmpty then
    Exit; // OK

  if Expected.IsEmpty then
    Fail(Format('Expected empty box, actual box is NOT empty (%s)',
      [Actual.ToRawString]));

  if Actual.IsEmpty then
    Fail(Format('Expected NOT empty box (%s), actual box is empty',
      [Expected.ToRawString]), AErrorAddrs);

  for I := 0 to 2 do
    if (not SameValue(Expected.Data[0][I], Actual.Data[0][I], Epsilon)) or
       (not SameValue(Expected.Data[1][I], Actual.Data[1][I], Epsilon)) then
      Fail(Format('Boxes are not equal: expected: %s, actual: %s',
        [Expected.ToRawString, Actual.ToRawString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertFilenamesEqual(const Expected, Actual: string; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  AssertTrue(ComparisonMsg(Expected, Actual),
    AnsiCompareFileName(Expected, Actual) = 0, AErrorAddrs);
end;

procedure TCastleTestCase.AssertImagesEqual(const Expected, Actual: TRGBAlphaImage; AErrorAddrs: Pointer);
var
  ExpectedPtr, ActualPtr: PVector4Byte;
  I: Integer;
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  // Overloaded version with AErrorAddrs is missing for fpcunit AssertEquals
  AssertEquals(Expected.Width, Actual.Width{, AErrorAddrs});
  AssertEquals(Expected.Height, Actual.Height{, AErrorAddrs});
  AssertEquals(Expected.Depth, Actual.Depth{, AErrorAddrs});
  ExpectedPtr := Expected.Pixels;
  ActualPtr := Actual.Pixels;
  for I := 1 to Actual.Width * Actual.Height * Actual.Depth do
  begin
    AssertVectorEquals(ExpectedPtr^, ActualPtr^, AErrorAddrs);
    Inc(ExpectedPtr);
    Inc(ActualPtr);
  end;
end;

procedure TCastleTestCase.AssertRectsEqual(const Expected, Actual: TRectangle; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]), AErrorAddrs);
end;

procedure TCastleTestCase.AssertRectsEqual(const Expected, Actual: TFloatRectangle; AErrorAddrs: Pointer);
begin
  if AErrorAddrs = nil then
    AErrorAddrs := CallerAddr;

  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]), AErrorAddrs);
end;

end.
