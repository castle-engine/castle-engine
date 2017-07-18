{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base test class containing useful assertion methods for base
  Castle Game Engine types. }
unit CastleBaseTestCase;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, fpcunit, testutils, CastleVectors, CastleBoxes,
  CastleImages;

type
  TCastleBaseTestCase = class(TTestCase)
  public
    procedure AssertMatrixEquals(const Expected, Actual: TMatrix4;
      const Epsilon: Single);
    procedure AssertVectorEquals(const Expected, Actual: TVector2Byte);
    procedure AssertVectorEquals(const Expected, Actual: TVector3Byte);
    procedure AssertVectorEquals(const Expected, Actual: TVector4Byte);

    procedure AssertVectorEquals(const Expected, Actual: TVector2);
    procedure AssertVectorEquals(const Expected, Actual: TVector3);
    procedure AssertVectorEquals(const Expected, Actual: TVector4);
    procedure AssertVectorEquals(const Expected, Actual: TVector2; const Epsilon: Single);
    procedure AssertVectorEquals(const Expected, Actual: TVector3; const Epsilon: Single);
    procedure AssertVectorEquals(const Expected, Actual: TVector4; const Epsilon: Single);

    procedure AssertSameValue(const Expected, Actual: Single);
    procedure AssertSameValue(const Expected, Actual: Single; const Epsilon: Single);

    // TODO: Need to have different names to avoid FPC errors "duplicate ASM label"
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double; const Epsilon: Single);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double; const Epsilon: Single);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double; const Epsilon: Single);
    procedure AssertSameValue(const Expected, Actual: Double);
    procedure AssertSameValue(const Expected, Actual: Double; const Epsilon: Double);

    procedure AssertBoxesEqual(const Expected, Actual: TBox3D; const Epsilon: Double);
    procedure AssertFilenamesEqual(const Expected, Actual: string);
    procedure AssertImagesEqual(const Expected, Actual: TRGBAlphaImage);
  end;

implementation

uses Math;

procedure TCastleBaseTestCase.AssertMatrixEquals(
  const Expected, Actual: TMatrix4; const Epsilon: Single);
begin
  if not TMatrix4.Equals(Expected, Actual, Epsilon) then
    Fail('Matrices (TMatrix4) are not equal:' + LineEnding +
      '  Expected:' + LineEnding +
      Expected.ToRawString('    ') + LineEnding +
      '  Actual:' + LineEnding +
      Actual.ToRawString('    '));
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2Byte);
begin
  if not TVector2Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector2Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]));
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3Byte);
begin
  if not TVector3Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector3Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]));
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4Byte);
begin
  if not TVector4Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector4Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]));
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2);
begin
  AssertVectorEquals(Expected, Actual, SingleEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3);
begin
  AssertVectorEquals(Expected, Actual, SingleEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4);
begin
  AssertVectorEquals(Expected, Actual, SingleEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2; const Epsilon: Single);
begin
  if not TVector2.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3; const Epsilon: Single);
begin
  if not TVector3.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure TCastleBaseTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4; const Epsilon: Single);
begin
  if not TVector4.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure TCastleBaseTestCase.AssertSameValue(const Expected, Actual: Single);
begin
  AssertSameValue(Expected, Actual, SingleEpsilon);
end;

procedure TCastleBaseTestCase.AssertSameValue(const Expected, Actual: Single;
  const Epsilon: Single);
begin
  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Single) are not equal: expected: %g, actual: %g',
      [Expected, Actual]));
end;

procedure TCastleBaseTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector2Double);
begin
  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector3Double);
begin
  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector4Double);
begin
  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector2Double; const Epsilon: Single);
begin
  if not TVector2Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure TCastleBaseTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector3Double; const Epsilon: Single);
begin
  if not TVector3Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure TCastleBaseTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector4Double; const Epsilon: Single);
begin
  if not TVector4Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure TCastleBaseTestCase.AssertSameValue(const Expected, Actual: Double);
begin
  AssertSameValue(Expected, Actual, DoubleEpsilon);
end;

procedure TCastleBaseTestCase.AssertSameValue(const Expected, Actual: Double;
  const Epsilon: Double);
begin
  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Double) are not equal: expected: %g, actual: %g',
      [Expected, Actual]));
end;

procedure TCastleBaseTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D;
  const Epsilon: Double);
var
  I: Integer;
begin
  for I := 0 to 2 do
    if (not SameValue(Expected.Data[0][I], Actual.Data[0][I], Epsilon)) or
       (not SameValue(Expected.Data[1][I], Actual.Data[1][I], Epsilon)) then
      Fail(Format('Boxes are not equal: expected: %s, actual: %s',
        [Expected.ToRawString, Actual.ToRawString]));
end;

procedure TCastleBaseTestCase.AssertFilenamesEqual(const Expected, Actual: string);
begin
  AssertTrue(ComparisonMsg(Expected, Actual), AnsiCompareFileName(Expected, Actual) = 0);
end;

procedure TCastleBaseTestCase.AssertImagesEqual(const Expected, Actual: TRGBAlphaImage);
var
  ExpectedPtr, ActualPtr: PVector4Byte;
  I: Integer;
begin
  AssertEquals(Expected.Width, Actual.Width);
  AssertEquals(Expected.Height, Actual.Height);
  AssertEquals(Expected.Depth, Actual.Depth);
  ExpectedPtr := Expected.AlphaPixels;
  ActualPtr := Actual.AlphaPixels;
  for I := 1 to Actual.Width * Actual.Height * Actual.Depth do
  begin
    AssertVectorEquals(ExpectedPtr^, ActualPtr^);
    Inc(ExpectedPtr);
    Inc(ActualPtr);
  end;
end;

end.
