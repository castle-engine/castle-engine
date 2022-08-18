{
  Copyright 2015-2022 Michalis Kamburelis.

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
  Classes, SysUtils, FpcUnit, TestUtils,
  CastleVectors, CastleBoxes, CastleImages, CastleRectangles, CastleFrustum;

type
  TCastleTestCase = class(TTestCase)
  public
    procedure AssertMatrixEquals(const Expected, Actual: TMatrix4;
      const Epsilon: Single; AddrOfError: Pointer = nil);
    procedure AssertMatrixEquals(const Expected, Actual: TMatrix4;
      AddrOfError: Pointer = nil);

    procedure AssertVectorEquals(const Expected, Actual: TVector2Byte; AddrOfError: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector3Byte; AddrOfError: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector4Byte; AddrOfError: Pointer = nil);

    procedure AssertVectorEquals(const Expected, Actual: TVector2; AddrOfError: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector3; AddrOfError: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector4; AddrOfError: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector2; const Epsilon: Single; AddrOfError: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector3; const Epsilon: Single; AddrOfError: Pointer = nil);
    procedure AssertVectorEquals(const Expected, Actual: TVector4; const Epsilon: Single; AddrOfError: Pointer = nil);

    { Check that 3D planes (defined by equation Ax+By+Cz+D=0) are equal.
      The vectors must be a component-wise multiplication of each other. }
    procedure AssertPlaneEquals(const Expected, Actual: TVector4; const Epsilon: Single; AddrOfError: Pointer = nil);
    procedure AssertPlaneEquals(const Expected, Actual: TVector4; AddrOfError: Pointer = nil);

    procedure AssertSameValue(const Expected, Actual: Single; AddrOfError: Pointer = nil);
    procedure AssertSameValue(const Expected, Actual: Single; const Epsilon: Single; AddrOfError: Pointer = nil);

    { TODO: Need to have different names to avoid FPC errors "duplicate ASM label",
      see https://bugs.freepascal.org/view.php?id=32188 }
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double; AddrOfError: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double; AddrOfError: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double; AddrOfError: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector2Double; const Epsilon: Single; AddrOfError: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector3Double; const Epsilon: Single; AddrOfError: Pointer = nil);
    procedure AssertVectorEqualsDouble(const Expected, Actual: TVector4Double; const Epsilon: Single; AddrOfError: Pointer = nil);
    procedure AssertSameValue(const Expected, Actual: Double; AddrOfError: Pointer = nil);
    procedure AssertSameValue(const Expected, Actual: Double; const Epsilon: Double; AddrOfError: Pointer = nil);

    procedure AssertBoxesEqual(const Expected, Actual: TBox3D; AddrOfError: Pointer = nil);
    procedure AssertBoxesEqual(const Expected, Actual: TBox3D; const Epsilon: Double; AddrOfError: Pointer = nil);
    procedure AssertBoxesEqual(const Msg: String; const Expected, Actual: TBox3D; AddrOfError: Pointer = nil);
    procedure AssertBoxesEqual(const Msg: String; const Expected, Actual: TBox3D; const Epsilon: Double; AddrOfError: Pointer = nil);
    procedure AssertFilenamesEqual(const Expected, Actual: string; AddrOfError: Pointer = nil);
    procedure AssertImagesEqual(const Expected, Actual: TRGBAlphaImage; AddrOfError: Pointer = nil);
    procedure AssertRectsEqual(const Expected, Actual: TRectangle; AddrOfError: Pointer = nil);
    procedure AssertRectsEqual(const Expected, Actual: TFloatRectangle; AddrOfError: Pointer = nil);

    procedure AssertFrustumEquals(const Expected, Actual: TFrustum; const Epsilon: Single; AddrOfError: Pointer = nil);
    procedure AssertFrustumEquals(const Expected, Actual: TFrustum; AddrOfError: Pointer = nil);

    procedure OnWarningRaiseException(const Category, S: string);
  end;

implementation

uses Math,
  CastleUtils;

procedure TCastleTestCase.AssertMatrixEquals(
  const Expected, Actual: TMatrix4; const Epsilon: Single; AddrOfError: Pointer);
var
  DifferenceEpsilon: Single;
  I, J: TMatrix4.TIndex;
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

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
      ]), AddrOfError);
  end;
end;

procedure TCastleTestCase.AssertMatrixEquals(
  const Expected, Actual: TMatrix4; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertMatrixEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2Byte; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector2Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector2Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3Byte; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector3Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector3Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4Byte; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector4Byte.Equals(Expected, Actual) then
    Fail(Format('Vectors (TVector4Byte) are not equal: expected: %s, actual: %s',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertVectorEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector2; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector2.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector3; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector3.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEquals(
  const Expected, Actual: TVector4; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector4.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertPlaneEquals(const Expected, Actual: TVector4; const Epsilon: Single; AddrOfError: Pointer = nil);
var
  MaxE, MaxA: Integer;
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  MaxE := MaxAbsVectorCoord(Expected);
  MaxA := MaxAbsVectorCoord(Actual);

  if MaxE <> MaxA then
    Fail(Format('Planes (TVector4) are not equal, their maximum component index differs. Expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);

  if IsZero(Expected[MaxE], Epsilon) and
     IsZero(  Actual[MaxA], Epsilon) then
  begin
    if not (Expected.IsZero(Epsilon) and Actual.IsZero(Epsilon)) then
      Fail(Format('Planes (TVector4) are not equal, they should be both zero since maximum component is zero. Expected: %s, actual: %s',
        [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
  end else
  if IsZero(Expected[MaxE], Epsilon) or
     IsZero(  Actual[MaxA], Epsilon) then
  begin
    Fail(Format('Planes (TVector4) are not equal, one of them has zero maximum component, the other not. Expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
  end else
  begin
    if not TVector4.Equals(
      Expected,
      Actual * (Expected[MaxE] / Actual[MaxA]),
      Epsilon
    ) then
      Fail(Format('Planes (TVector4) are not equal, they are not multiplied version of each other. Expected: %s, actual: %s. After trying to bring them closer, actual is %s', [
        Expected.ToRawString,
        Actual.ToRawString,
        (Actual * (Expected[MaxE] / Actual[MaxA])).ToRawString
      ]), AddrOfError);
  end;
end;

procedure TCastleTestCase.AssertPlaneEquals(const Expected, Actual: TVector4; AddrOfError: Pointer = nil);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;
  AssertPlaneEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertSameValue(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Single;
  const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Single) are not equal: expected: %g, actual: %g',
      [Expected, Actual]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector2Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector3Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector4Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertVectorEqualsDouble(Expected, Actual, DoubleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector2Double; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector2Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector2Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector3Double; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector3Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector3Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertVectorEqualsDouble(
  const Expected, Actual: TVector4Double; const Epsilon: Single; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not TVector4Double.Equals(Expected, Actual, Epsilon) then
    Fail(Format('Vectors (TVector4Double) are not equal: expected: %s, actual: %s',
      [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertSameValue(Expected, Actual, DoubleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertSameValue(const Expected, Actual: Double;
  const Epsilon: Double; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not SameValue(Expected, Actual, Epsilon) then
    Fail(Format('Floats (Double) are not equal: expected: %g, actual: %g',
      [Expected, Actual]), AddrOfError);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Msg: String; const Expected, Actual: TBox3D; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertBoxesEqual(Msg, Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Msg: String; const Expected, Actual: TBox3D;
  const Epsilon: Double; AddrOfError: Pointer);
var
  I: Integer;
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if Expected.IsEmpty and Actual.IsEmpty then
    Exit; // OK

  if Expected.IsEmpty then
    Fail(Format('Expected empty box, actual box is NOT empty (%s). ' + Msg,
      [Actual.ToRawString]));

  if Actual.IsEmpty then
    Fail(Format('Expected NOT empty box (%s), actual box is empty. ' + Msg,
      [Expected.ToRawString]), AddrOfError);

  for I := 0 to 2 do
    if (not SameValue(Expected.Data[0][I], Actual.Data[0][I], Epsilon)) or
       (not SameValue(Expected.Data[1][I], Actual.Data[1][I], Epsilon)) then
      Fail(Format('Boxes are not equal: expected: %s, actual: %s. ' + Msg,
        [Expected.ToRawString, Actual.ToRawString]), AddrOfError);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D; AddrOfError: Pointer = nil);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertBoxesEqual('', Expected, Actual, AddrOfError);
end;

procedure TCastleTestCase.AssertBoxesEqual(const Expected, Actual: TBox3D; const Epsilon: Double; AddrOfError: Pointer = nil);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertBoxesEqual('', Expected, Actual, Epsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertFilenamesEqual(const Expected, Actual: string; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  AssertTrue(ComparisonMsg(Expected, Actual),
    AnsiCompareFileName(Expected, Actual) = 0, AddrOfError);
end;

procedure TCastleTestCase.AssertImagesEqual(const Expected, Actual: TRGBAlphaImage; AddrOfError: Pointer);
var
  ExpectedPtr, ActualPtr: PVector4Byte;
  I: Integer;
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  // Overloaded version with AddrOfError is missing for fpcunit AssertEquals
  AssertEquals(Expected.Width, Actual.Width{, AddrOfError});
  AssertEquals(Expected.Height, Actual.Height{, AddrOfError});
  AssertEquals(Expected.Depth, Actual.Depth{, AddrOfError});
  ExpectedPtr := Expected.Pixels;
  ActualPtr := Actual.Pixels;
  for I := 1 to Actual.Width * Actual.Height * Actual.Depth do
  begin
    AssertVectorEquals(ExpectedPtr^, ActualPtr^, AddrOfError);
    Inc(ExpectedPtr);
    Inc(ActualPtr);
  end;
end;

procedure TCastleTestCase.AssertRectsEqual(const Expected, Actual: TRectangle; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertRectsEqual(const Expected, Actual: TFloatRectangle; AddrOfError: Pointer);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  if not Expected.Equals(Actual) then
    Fail(Format('Expected rect (%s) does not equal actual (%s)',
      [Expected.ToString, Actual.ToString]), AddrOfError);
end;

procedure TCastleTestCase.AssertFrustumEquals(const Expected, Actual: TFrustum; AddrOfError: Pointer = nil);
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;
  AssertFrustumEquals(Expected, Actual, SingleEpsilon, AddrOfError);
end;

procedure TCastleTestCase.AssertFrustumEquals(const Expected, Actual: TFrustum; const Epsilon: Single; AddrOfError: Pointer = nil);
var
  I: TFrustumPlane;
begin
  if AddrOfError = nil then
    AddrOfError := CallerAddr;

  try
    AssertEquals(Expected.ZFarInfinity, Actual.ZFarInfinity);

    if Expected.ZFarInfinity then
    begin
      for I := Low(I) to Pred(High(I)) do
        AssertPlaneEquals(Expected.Planes[I], Actual.Planes[I], Epsilon);
    end else
    begin
      for I := Low(I) to High(I) do
        AssertPlaneEquals(Expected.Planes[I], Actual.Planes[I], Epsilon);
    end;
  except
    on E: Exception do
    begin
      Fail(Format('Expected frustum (%s) does not equal actual (%s). The underlying difference exception: %s', [
        Expected.ToString('  '),
        Actual.ToString('  '),
        E.Message
      ]), AddrOfError);
    end;
  end;
end;

procedure TCastleTestCase.OnWarningRaiseException(const Category, S: string);
begin
  raise Exception.CreateFmt(ClassName + ': received a warning, and any warning here is an error: %s: %s', [Category, S]);
end;

end.
