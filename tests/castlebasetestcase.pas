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

interface

uses
  Classes, SysUtils, fpcunit, testutils, CastleVectors, CastleBoxes;

type
  TCastleBaseTestCase = class(TTestCase)
  public
    procedure AssertMatricesEqual(const Expected, Actual: TMatrix4Single;
      const EqualityEpsilon: Single);
    procedure AssertVectorsEqual(const Expected, Actual: TVector2Byte);
    procedure AssertVectorsEqual(const Expected, Actual: TVector3Byte);
    procedure AssertVectorsEqual(const Expected, Actual: TVector4Byte);
    procedure AssertVectorsEqual(const Expected, Actual: TVector3Single);
    procedure AssertVectorsEqual(const Expected, Actual: TVector3Double);
    procedure AssertVectorsEqual(const Expected, Actual: TVector4Single);
    procedure AssertVectorsEqual(const Expected, Actual: TVector4Double);
    procedure AssertVectorsEqual(const Expected, Actual: TVector3Single; const EqualityEpsilon: Single);
    procedure AssertVectorsEqual(const Expected, Actual: TVector3Double; const EqualityEpsilon: Single);
    procedure AssertVectorsEqual(const Expected, Actual: TVector4Single; const EqualityEpsilon: Single);
    procedure AssertVectorsEqual(const Expected, Actual: TVector4Double; const EqualityEpsilon: Single);
    procedure AssertFloatsEqual(const Expected, Actual: Single);
    procedure AssertFloatsEqual(const Expected, Actual: Double);
    procedure AssertFloatsEqual(const Expected, Actual: Single; const EqualityEpsilon: Single);
    procedure AssertFloatsEqual(const Expected, Actual: Double; const EqualityEpsilon: Double);
    procedure AssertBoxesEqual(const Expected, Actual: TBox3D; const EqualityEpsilon: Double);
    procedure AssertFilenamesEqual(const Expected, Actual: string);
  end;

implementation

procedure TCastleBaseTestCase.AssertMatricesEqual(
  const Expected, Actual: TMatrix4Single; const EqualityEpsilon: Single);
begin
  if not MatricesEqual(Expected, Actual, EqualityEpsilon) then
    Fail('Matrices (TMatrix4Single) are not equal:' + LineEnding +
      '  Expected:' + LineEnding +
      MatrixToRawStr(Expected, '    ') + LineEnding +
      '  Actual:' + LineEnding +
      MatrixToRawStr(Actual, '    '));
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector2Byte);
begin
  if not VectorsPerfectlyEqual(Expected, Actual) then
    Fail(Format('Vectors (TVector2Byte) are not equal: expected: %s, actual: %s',
      [VectorToRawStr(Expected), VectorToRawStr(Actual)]));
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector3Byte);
begin
  if not VectorsPerfectlyEqual(Expected, Actual) then
    Fail(Format('Vectors (TVector3Byte) are not equal: expected: %s, actual: %s',
      [VectorToRawStr(Expected), VectorToRawStr(Actual)]));
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector4Byte);
begin
  if not VectorsPerfectlyEqual(Expected, Actual) then
    Fail(Format('Vectors (TVector4Byte) are not equal: expected: %s, actual: %s',
      [VectorToRawStr(Expected), VectorToRawStr(Actual)]));
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector3Single);
begin
  AssertVectorsEqual(Expected, Actual, SingleEqualityEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector3Double);
begin
  AssertVectorsEqual(Expected, Actual, DoubleEqualityEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector4Single);
begin
  AssertVectorsEqual(Expected, Actual, SingleEqualityEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector4Double);
begin
  AssertVectorsEqual(Expected, Actual, DoubleEqualityEpsilon);
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector3Single; const EqualityEpsilon: Single);
begin
  if not VectorsEqual(Expected, Actual, EqualityEpsilon) then
    Fail(Format('Vectors (TVector3Single) are not equal: expected: %s, actual: %s',
      [VectorToRawStr(Expected), VectorToRawStr(Actual)]));
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector3Double; const EqualityEpsilon: Single);
begin
  if not VectorsEqual(Expected, Actual, EqualityEpsilon) then
    Fail(Format('Vectors (TVector3Double) are not equal: expected: %s, actual: %s',
      [VectorToRawStr(Expected), VectorToRawStr(Actual)]));
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector4Single; const EqualityEpsilon: Single);
begin
  if not VectorsEqual(Expected, Actual, EqualityEpsilon) then
    Fail(Format('Vectors (TVector4Single) are not equal: expected: %s, actual: %s',
      [VectorToRawStr(Expected), VectorToRawStr(Actual)]));
end;

procedure TCastleBaseTestCase.AssertVectorsEqual(
  const Expected, Actual: TVector4Double; const EqualityEpsilon: Single);
begin
  if not VectorsEqual(Expected, Actual, EqualityEpsilon) then
    Fail(Format('Vectors (TVector4Double) are not equal: expected: %s, actual: %s',
      [VectorToRawStr(Expected), VectorToRawStr(Actual)]));
end;

procedure TCastleBaseTestCase.AssertFloatsEqual(const Expected, Actual: Single);
begin
  AssertFloatsEqual(Expected, Actual, SingleEqualityEpsilon);
end;

procedure TCastleBaseTestCase.AssertFloatsEqual(const Expected, Actual: Double);
begin
  AssertFloatsEqual(Expected, Actual, DoubleEqualityEpsilon);
end;

procedure TCastleBaseTestCase.AssertFloatsEqual(const Expected, Actual: Single;
  const EqualityEpsilon: Single);
begin
  if not FloatsEqual(Expected, Actual, EqualityEpsilon) then
    Fail(Format('Floats (Single) are not equal: expected: %g, actual: %g',
      [Expected, Actual]));
end;

procedure TCastleBaseTestCase.AssertFloatsEqual(const Expected, Actual: Double;
  const EqualityEpsilon: Double);
begin
  if not FloatsEqual(Expected, Actual, EqualityEpsilon) then
    Fail(Format('Floats (Double) are not equal: expected: %g, actual: %g',
      [Expected, Actual]));
end;

procedure TCastleBaseTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D;
  const EqualityEpsilon: Double);
var
  I: Integer;
begin
  for I := 0 to 2 do
    if (not FloatsEqual(Expected.Data[0][I], Actual.Data[0][I], EqualityEpsilon)) or
       (not FloatsEqual(Expected.Data[1][I], Actual.Data[1][I], EqualityEpsilon)) then
      Fail(Format('Boxes are not equal: expected: %s, actual: %s',
        [Expected.ToRawStr, Actual.ToRawStr]));
end;

procedure TCastleBaseTestCase.AssertFilenamesEqual(const Expected, Actual: string);
begin
  AssertTrue(ComparisonMsg(Expected, Actual), AnsiCompareFileName(Expected, Actual) = 0);
end;

end.
