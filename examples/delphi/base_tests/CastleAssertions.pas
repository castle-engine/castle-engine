{
  Copyright 2015-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Useful assertions for CGE types.
  Adjusted from castle-engine/tests/code/common/castletestcase.pas
  (which is only for FPC now). }
unit CastleAssertions;

interface

uses CastleVectors, CastleRectangles, CastleBoxes;

procedure AssertMatrixEquals(const Expected, Actual: TMatrix4;
  const Epsilon: Single);

procedure AssertVectorEquals(const Expected, Actual: TVector2Byte); overload;
procedure AssertVectorEquals(const Expected, Actual: TVector3Byte); overload;
procedure AssertVectorEquals(const Expected, Actual: TVector4Byte); overload;

procedure AssertVectorEquals(const Expected, Actual: TVector2); overload;
procedure AssertVectorEquals(const Expected, Actual: TVector3); overload;
procedure AssertVectorEquals(const Expected, Actual: TVector4); overload;
procedure AssertVectorEquals(const Expected, Actual: TVector2; const Epsilon: Single); overload;
procedure AssertVectorEquals(const Expected, Actual: TVector3; const Epsilon: Single); overload;
procedure AssertVectorEquals(const Expected, Actual: TVector4; const Epsilon: Single); overload;

procedure AssertSameValue(const Expected, Actual: Single); overload;
procedure AssertSameValue(const Expected, Actual: Single; const Epsilon: Single); overload;

{ TODO: Need to have different names to avoid FPC errors "duplicate ASM label",
  see https://bugs.freepascal.org/view.php?id=32188 }
procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double); overload;
procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double); overload;
procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double); overload;
procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double; const Epsilon: Single); overload;
procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double; const Epsilon: Single); overload;
procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double; const Epsilon: Single); overload;
procedure AssertSameValue(const Expected, Actual: Double); overload;
procedure AssertSameValue(const Expected, Actual: Double; const Epsilon: Double); overload;

procedure AssertBoxesEqual(const Expected, Actual: TBox3D); overload;
procedure AssertBoxesEqual(const Expected, Actual: TBox3D; const Epsilon: Double); overload;
procedure AssertFilenamesEqual(const Expected, Actual: string); overload;
//procedure AssertImagesEqual(const Expected, Actual: TRGBAlphaImage); overload;
procedure AssertRectsEqual(const Expected, Actual: TRectangle); overload;
procedure AssertRectsEqual(const Expected, Actual: TFloatRectangle); overload;

implementation

uses Math, SysUtils,
  CastleUtils;

procedure Fail(const Message: String);
begin
  raise Exception.Create(Message);
end;

procedure AssertMatrixEquals(
  const Expected, Actual: TMatrix4; const Epsilon: Single);
var
  DifferenceEpsilon: Single;
  I, J: TMatrix4.TIndex;
begin
  if not TMatrix4.Equals(Expected, Actual, Epsilon) then
  begin
    DifferenceEpsilon := 0;
    for I := 0 to 3 do
      for J := 0 to 3 do
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
      ]));
  end;
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector2Byte);
begin
  if not TVector2Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector2Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]));
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector3Byte);
begin
  if not TVector3Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector3Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]));
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector4Byte);
begin
  if not TVector4Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector4Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]));
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector2);
begin
  AssertVectorEquals(Expected, Actual, SingleEpsilon);
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector3);
begin
  AssertVectorEquals(Expected, Actual, SingleEpsilon);
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector4);
begin
  AssertVectorEquals(Expected, Actual, SingleEpsilon);
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector2; const Epsilon: Single);
begin
  if not TVector2.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector3; const Epsilon: Single);
begin
  if not TVector3.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure AssertVectorEquals(
  const Expected, Actual: TVector4; const Epsilon: Single);
begin
  if not TVector4.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure AssertSameValue(const Expected, Actual: Single);
begin
  AssertSameValue(Expected, Actual, SingleEpsilon);
end;

procedure AssertSameValue(const Expected, Actual: Single;
  const Epsilon: Single);
begin
  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Single) are not equal: expected: %g, actual: %g',
      [Expected, Actual]));
end;

procedure AssertVectorEqualsDouble(
  const Expected, Actual: TVector2Double);
begin
  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon);
end;

procedure AssertVectorEqualsDouble(
  const Expected, Actual: TVector3Double);
begin
  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon);
end;

procedure AssertVectorEqualsDouble(
  const Expected, Actual: TVector4Double);
begin
  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon);
end;

procedure AssertVectorEqualsDouble(
  const Expected, Actual: TVector2Double; const Epsilon: Single);
begin
  if not TVector2Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure AssertVectorEqualsDouble(
  const Expected, Actual: TVector3Double; const Epsilon: Single);
begin
  if not TVector3Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure AssertVectorEqualsDouble(
  const Expected, Actual: TVector4Double; const Epsilon: Single);
begin
  if not TVector4Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]));
end;

procedure AssertSameValue(const Expected, Actual: Double);
begin
  AssertSameValue(Expected, Actual, DoubleEpsilon);
end;

procedure AssertSameValue(const Expected, Actual: Double;
  const Epsilon: Double);
begin
  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Double) are not equal: expected: %g, actual: %g',
      [Expected, Actual]));
end;

procedure AssertBoxesEqual(const Expected, Actual: TBox3D);
begin
  AssertBoxesEqual(Expected, Actual, SingleEpsilon);
end;

procedure AssertBoxesEqual(const Expected, Actual: TBox3D;
  const Epsilon: Double);
var
  I: Integer;
begin
  if Expected.IsEmpty and Actual.IsEmpty then
    Exit; // OK

  if Expected.IsEmpty then
    Fail(Format('Expected empty box, actual box is NOT empty (%s)',
      [Actual.ToRawString]));

  if Actual.IsEmpty then
    Fail(Format('Expected NOT empty box (%s), actual box is empty',
      [Expected.ToRawString]));

  for I := 0 to 2 do
    if (not SameValue(Expected.Data[0][I], Actual.Data[0][I], Epsilon)) or
       (not SameValue(Expected.Data[1][I], Actual.Data[1][I], Epsilon)) then
      Fail(Format('Boxes are not equal: expected: %s, actual: %s',
        [Expected.ToRawString, Actual.ToRawString]));
end;

procedure AssertFilenamesEqual(const Expected, Actual: string);
begin
  if not SameFileName(Expected, Actual) then
    Fail(Format('Expected filename "%s", got "%s"', [Expected, Actual]));
end;

(*
procedure AssertImagesEqual(const Expected, Actual: TRGBAlphaImage);
var
  ExpectedPtr, ActualPtr: PVector4Byte;
  I: Integer;
begin
  // Overloaded version with AErrorAddrs is missing for fpcunit AssertEquals
  AssertEquals(Expected.Width, Actual.Width{});
  AssertEquals(Expected.Height, Actual.Height{});
  AssertEquals(Expected.Depth, Actual.Depth{});
  ExpectedPtr := Expected.Pixels;
  ActualPtr := Actual.Pixels;
  for I := 1 to Actual.Width * Actual.Height * Actual.Depth do
  begin
    AssertVectorEquals(ExpectedPtr^, ActualPtr^);
    Inc(ExpectedPtr);
    Inc(ActualPtr);
  end;
end;
*)

procedure AssertRectsEqual(const Expected, Actual: TRectangle);
begin
  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]));
end;

procedure AssertRectsEqual(const Expected, Actual: TFloatRectangle);
begin
  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]));
end;

end.
