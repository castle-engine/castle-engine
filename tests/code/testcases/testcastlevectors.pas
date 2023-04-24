// -*- compile-command: "./test_single_testcase.sh TTestCastleVectors" -*-
{
  Copyright 2004-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleVectors. }
unit TestCastleVectors;

{ $define VECTOR_MATH_SPEED_TESTS}

interface

uses
  Classes, SysUtils{$ifndef CASTLE_TESTER}, FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}, CastleTester{$endif}, CastleVectors;

type
  TTestCastleVectors = class(TCastleTestCase)
  published
    procedure TestPlaneOdcCollision;
    procedure TestCollisions;
    procedure TestArea;
    procedure TestPerpParallel;
    procedure TestPlanesIntersection;
    procedure TestOther;
    procedure TestVectorStr;
    procedure TestMatrixInverse;
    procedure TestMultMatrixTranslation;
    procedure TestMultMatricesTranslation;
    procedure TestIndexedPolygonNormalArea;
    procedure TestSphereRayIntersection;
    procedure TestMatrixMultiplication;
    procedure TestMatrixTranspose;
    procedure TestVector3FromStr;
    procedure TestVector4FromStr;
    procedure TestPlaneTransform;
    procedure TestTransformToFromCoordsMatrix;
    procedure Test2D;
    procedure TestApproximateScale;
    procedure TestXYZ;
    procedure TestPlaneMove;
    procedure TestPlaneMoveRandom;
    procedure TestTryInverseHarder;
    procedure TestMaxAbsVectorCoord;
    procedure TestMinAbsVectorCoord;
    procedure TestPointOnLineClosestToLine;
    procedure TestRotatePointAroundAxis;
    procedure TestMakeVectorOrthogonal;
    procedure TestEpsilonInModelViewToNormalMatrix;
    procedure TestInit;
    procedure TestRotationZeroAxis;
    procedure TestVectorsList;
  end;

function RandomVector: TVector3;
function RandomMatrix: TMatrix4;
function RandomNonProjectionMatrix: TMatrix4;

implementation

uses Math,
  CastleUtils, CastleStringUtils, CastleTimeUtils, CastleTriangles;

const
  {$ifdef VECTOR_MATH_SPEED_TESTS}
  SPEED_TEST_1_CYCLES = 1000000;
  SPEED_TEST_2_CYCLES = 10000000;
  SPEED_TEST_3_CYCLES = 1000;
  {$else}
  { speed testy przeprowadzaja przy okazji normalne testy ktore
    zawsze bedziesz chcial wykonac. Wiec jesli VECTOR_MATH_SPEED_TESTS
    nie jest zdefiniowane to po prostu SPEED_TEST_x_CYCLES beda
    mniejsze (ale nie zerowe) i nie bedzie Writelnow wynikow. }
  SPEED_TEST_1_CYCLES = 1000;
  SPEED_TEST_2_CYCLES = 1000;
  SPEED_TEST_3_CYCLES = 1000;
  {$endif}

procedure TTestCastleVectors.TestPlaneOdcCollision;
{ test below caught once fpc 1.0.10 bugs in inlines }
var Intersection: TVector3;
    T: Single;
begin
 T := TVector3.DotProduct(Vector3(0, 0, 1), Vector3(0, 0, 6));
 AssertSameValue(6, T);

 AssertTrue(TryPlaneLineIntersection(T,
   Vector4(0, 0, 1, 1),
   Vector3(2, 2, -3),
   Vector3(0, 0, 6) ));
 AssertSameValue(1/3, T, 0.0000001);

 AssertTrue(TryPlaneSegmentDirIntersection(Intersection, T,
   Vector4(0, 0, 1, 1),
   Vector3(2, 2, -3),
   Vector3(0, 0, 6) ));
 AssertVectorEquals(Vector3(2, 2, -1), Intersection);
 AssertSameValue(1/3, T, 0.0000001);
end;

procedure WritelnSpeedTest(const s: string);
begin
 {$ifdef VECTOR_MATH_SPEED_TESTS}
 Writeln(s);
 {$endif}
end;

procedure TTestCastleVectors.TestCollisions;
const
  TriConst: TTriangle3 = (Data: (
    (X: 2; Y: 2; Z: -1),
    (X: 3; Y: 3; Z: -1),
    (X: 3; Y: 2; Z: -1) ));
  TriPlaneConst: TVector4 = (X: 0; Y: 0; Z: 1; W: 1);
  Pos1Const: TVector3 = (X: 2; Y: 2; Z: -3);
  Pos2Const: TVector3 = (X: 2; Y: 2; Z: 3);
  Coll: TVector3 = (X: 2; Y: 2; Z: -1);
var
  Intersection, Intersection2: TVector3;
begin
 { czy dziala TryTriangleSegmentCollision na tym naszym skonstruowanym
   przykladzie ? }
 AssertTrue( TryTriangleSegmentCollision(Intersection, TriConst, TriPlaneConst,
   Pos1Const, Pos2Const) and TVector3.Equals(Intersection, Coll));
 { czy dziala tak samo gdy sam musi sobie wyliczyc place (test TTriangle3.Plane) ? }
 AssertTrue( TryTriangleSegmentCollision(Intersection2, TriConst, TriConst.Plane,
   Pos1Const, Pos2Const) and TVector3.Equals(Intersection2, Coll));
end;

procedure TTestCastleVectors.TestArea;
const
  Tri: TTriangle3 = (Data: (
    (X: 0; Y: 0; Z: 0),
    (X: 10; Y: 0; Z: 0),
    (X: 0; Y: 25; Z: 0) ));
  CCWPoly: array [0..4] of TVector2 = (
    (X: 5; Y: 4),
    (X: 2; Y: 3),
    (X: 4; Y: 3),
    (X: 2; Y: 1),
    (X: 6; Y: 2) );
  CWPoly: array [0..4] of TVector2 = (
    (X: 6; Y: 2),
    (X: 2; Y: 1),
    (X: 4; Y: 3),
    (X: 2; Y: 3),
    (X: 5; Y: 4) );
begin
 AssertTrue(Tri.Area = 10*25/2);

 AssertTrue(Polygon2dArea(CCWPoly) = 5.5);
 AssertTrue(Polygon2dArea(CWPoly) = 5.5);
 AssertTrue(IsPolygon2dCCW(CCWPoly) > 0);
 AssertTrue(IsPolygon2dCCW(CWPoly) < 0);
end;

procedure TTestCastleVectors.TestPerpParallel;
var v: TVector3;
    i: integer;
begin
 for i := 1 to 10 do
 try
  v := RandomVector;
  AssertTrue( VectorsPerp(AnyOrthogonalVector(v), v) );
  { I has to comment it out -- it fails too often due to floating point
    inaccuracy. }
  { AssertTrue( VectorsParallel(v * (Random * 10)), v) ); }
  AssertTrue( VectorsPerp(TVector3.Zero, v) );
  AssertTrue( VectorsParallel(TVector3.Zero, v) );
 except
  Writeln('and failed : v = ',v.ToString,
    ' anyPerp = ',AnyOrthogonalVector(v).ToString);
  raise;
 end;

 AssertTrue( VectorsPerp(TVector3.Zero, TVector3.Zero) );
 AssertTrue( VectorsParallel(TVector3.Zero, TVector3.Zero) );

 AssertTrue( VectorsPerp(TVector3.One[0], TVector3.One[1]) );
 AssertTrue( VectorsPerp(TVector3.One[0], TVector3.One[2]) );
 AssertTrue( VectorsPerp(TVector3.One[1], TVector3.One[2]) );
 AssertTrue( not VectorsPerp(TVector3.One[0], TVector3.One[0]) );

 AssertTrue( not VectorsParallel(TVector3.One[0], TVector3.One[1]) );
 AssertTrue( not VectorsParallel(TVector3.One[0], TVector3.One[2]) );
 AssertTrue( not VectorsParallel(TVector3.One[1], TVector3.One[2]) );
 AssertTrue( VectorsParallel(TVector3.One[0], TVector3.One[0]) );
end;

procedure TTestCastleVectors.TestPlanesIntersection;
const
  P1: TVector4 = (X: -1.9935636520385742; Y: -0.00000009909226151; Z: 0.25691652297973633; W: -30.014257431030273);
  P2: TVector4 = (X: -1.2131816148757935; Y: 1.90326225890658E-008; Z: -1.5900282859802246; W: 1.5900282859802246);
var
  Line0, LineVector: TVector3;
begin
 TwoPlanesIntersectionLine(P1, P2, Line0, LineVector);
 { Writeln(Line0.ToRawString, ' ', LineVector.ToRawString); }
end;

procedure TTestCastleVectors.TestOther;
var
  I1, I2, RayOrigin, RayDirection: TVector3;
  Plane: TVector4;
// PlaneDir: TVector3 absolute Plane;
  PlaneConstCoord: integer;
  PlaneConstVal: Single;
  b1, b2: boolean;
// t1, t2: Double;

  function RandomVector3: TVector3;
  begin
    Result := Vector3(
      Random*1000 -500.0,
      Random*1000 -500.0,
      Random*1000 -500.0
    );
  end;

const VConst: TVector3 = (X: 1.0; Y: 2.0; Z: 3.0);

var
  i: integer;
  V: TVector3;
  Time0, Time1, Time2: Double;
  StartTime: TProcessTimerResult;
begin
 { ------------------------------------------------------------
   testuj TrySimplePlaneRayIntersection przy uzyciu TryPlaneRayIntersection }
 for i := 1 to 100000 do
 begin
  RayOrigin := RandomVector3;
  RayDirection := RandomVector3;

  PlaneConstCoord := Random(3);
  PlaneConstVal := Random*1000 - 500;
  FillChar(Plane, SizeOf(Plane), 0);
  Plane.Data[PlaneConstCoord] := -1;
  Plane.Data[3] := PlaneConstVal;

  { czasami uczyn promien rownoleglym do [Simple]Plane (zeby zobaczyc
    czy sobie z tym radzi) }
  if Random(10) = 1 then
  begin
   RayDirection.Data[PlaneConstCoord] := 0;
   b1 := TrySimplePlaneRayIntersection(I1, PlaneConstCoord, PlaneConstVal, RayOrigin, RayDirection);
   b2 := TryPlaneRayIntersection(I2, Plane, RayOrigin, RayDirection);
   Check( (not b1) and (not b2) ,'intersect with parallel plane');
  end else
  begin
   { nie wykonuj testu jesli wylosowalimy niepoprawne dane }
   if not TVector3.Equals(RayDirection, Vector3(0, 0, 0)) then
   begin
    b1 := TrySimplePlaneRayIntersection(I1, PlaneConstCoord, PlaneConstVal, RayOrigin, RayDirection);
    b2 := TryPlaneRayIntersection(I2, Plane, RayOrigin, RayDirection);
    AssertEquals(b1, b2);
    if b1 then
    begin
{     if not TVector3.Equals(I1, I2) or
        not SameValue(I1[PlaneConstCoord], PlaneConstVal) or
	not SameValue(I2[PlaneConstCoord], PlaneConstVal) then
     begin
      t1:=(PlaneConstVal-RayOrigin[PlaneConstCoord]) / RayDirection[PlaneConstCoord];
      t2 := -(plane[0]*RayOrigin[0] + plane[1]*RayOrigin[1] + plane[2]*RayOrigin[2] + plane[3])/
          TVector3.DotProduct(PlaneDir, RayDirection);
      Writeln('I1 = ',I1.ToString, ' I2 = ',I2.ToString, nl,
        'PlaneConst Coord = ',PlaneConstCoord, ' Value = ',PlaneConstVal, nl,
	'Plane = ',Plane.ToString, nl,
	'RayOrigin = ',RayOrigin.ToString, ' RayDirection = ',RayDirection.ToString, nl,
	t1:1:2, nl,
	t2:1:2, nl,
	(RayOrigin + RayDirection * t1).ToString, nl,
	(RayOrigin + RayDirection * t2).ToString
      );
     end; }
     AssertSameValue(PlaneConstVal, I1[PlaneConstCoord]);
     AssertSameValue(PlaneConstVal, I2[PlaneConstCoord]);
     AssertVectorEquals(I1, I2);
    end;
   end;
  end;
 end;

 { testuj szybkosc TrySimplePlaneRayIntersection w porownaniu z
   TryPlaneRayIntersection }
 WritelnSpeedTest('SPEED TEST 1 ----------------------------------------------');

 StartTime := ProcessTimer;
 for i := 1 to SPEED_TEST_1_CYCLES do ;
 Time0 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('Empty loop = %f',[Time0]));

 StartTime := ProcessTimer;
 for i := 1 to SPEED_TEST_1_CYCLES do
  TrySimplePlaneRayIntersection(I1, PlaneConstCoord, PlaneConstVal, RayOrigin, RayDirection);
 Time1 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('TrySimplePlaneRayIntersection = %f',[Time1]));

 StartTime := ProcessTimer;
 for i := 1 to SPEED_TEST_1_CYCLES do
  TryPlaneRayIntersection(I1, Plane, RayOrigin, RayDirection);
 Time2 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('TryPlaneRayIntersection = %f',[Time2]));

 {$ifdef VECTOR_MATH_SPEED_TESTS}
 { nie uzywam tutaj WritelnSpeedTest. Jesli VECTOR_MATH_SPEED_TESTS
   not defined to stale SPEED_TEST_x_CYCLES sa tak male ze nie moge
   wykonac dzielenia przez Time1-Time0 bo Time1-Time0 = 0. }
 Writeln(Format('SimplePlane is faster than Plane by %f', [(Time2-Time0)/(Time1-Time0)]));
 {$endif}

 WritelnSpeedTest('SPEED TEST 2 ----------------------------------------------');

 StartTime := ProcessTimer;
 for i := 1 to SPEED_TEST_2_CYCLES do ;
 Time0 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('Empty loop = %f',[Time0]));

 StartTime := ProcessTimer;
 for i := 1 to SPEED_TEST_2_CYCLES do
 begin
  V := VConst;
  V := V * Pi;
 end;
 Time1 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('Using assignment + VectorScaleVar = %f',[Time1]));

 StartTime := ProcessTimer;
 for i := 1 to SPEED_TEST_2_CYCLES do
 begin
  V := VConst * Pi;
 end;
 Time2 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('Using VectorScale = %f',[Time2]));

 {$ifdef VECTOR_MATH_SPEED_TESTS}
 { nie uzywam tutaj WritelnSpeedTest. Jesli VECTOR_MATH_SPEED_TESTS
   not defined to stale SPEED_TEST_x_CYCLES sa tak male ze nie moge
   wykonac dzielenia przez Time1-Time0 bo Time1-Time0 = 0. }
 Writeln(Format('Assignment+Var is faster by %f', [(Time2-Time0)/(Time1-Time0)]));
 {$endif}
end;

procedure TTestCastleVectors.TestVectorStr;

  procedure OneTestVectorFromStr;
  var v, v2: TVector3;
      s: string;
  begin
   v := RandomVector;
   s := v.ToRawString;
   v2 := Vector3FromStr(s);
   AssertVectorEquals(v2, v, 0.001); // larger epsilon for ppc64
  end;

  procedure OneTestByDeformat;
  var v, v2: TVector3;
      s: string;
  begin
   v := RandomVector;
   s := v.ToRawString;
   DeFormat(s, '%.single. %.single. %.single.', [@v2.X, @v2.Y, @v2.Z]);
   AssertVectorEquals(v2, v, 0.001); // larger epsilon for ppc64
  end;

const
  CYCLES = SPEED_TEST_3_CYCLES;
var
  Time0, Time1, Time2: Double;
  i: integer;
  StartTime: TProcessTimerResult;
begin
 WritelnSpeedTest('SPEED TEST VectorFromStr ------------------------------------------');
 StartTime := ProcessTimer;
 for i := 1 to CYCLES do ;
 Time0 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('Empty loop = %f',[Time0]));

 StartTime := ProcessTimer;
 for i := 1 to CYCLES do OneTestVectorFromStr;
 Time1 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('VectorFromStr = %f',[Time1]));

 StartTime := ProcessTimer;
 for i := 1 to CYCLES do OneTestByDeFormat;
 Time2 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('DeFormat = %f',[Time2]));

 {$ifdef VECTOR_MATH_SPEED_TESTS}
 { nie uzywam tutaj WritelnSpeedTest. Jesli VECTOR_MATH_SPEED_TESTS
   not defined to stale SPEED_TEST_x_CYCLES sa tak male ze nie moge
   wykonac dzielenia przez Time1-Time0 bo Time1-Time0 = 0. }
 Writeln(Format('VectorFromStr is faster by %f', [(Time2-Time0)/(Time1-Time0)]));
 {$endif}
end;

procedure TTestCastleVectors.TestMatrixInverse;
var
  M: TMatrix4;
begin
  M := ScalingMatrix(Vector3(2, 2, 2));

{ Tests:
  Writeln(M.ToString('  '));
  Writeln(ScalingMatrix(Vector3(0.5, 0.5, 0.5)).ToString('  '));
  Writeln(MatrixInverse(M, MatrixDeterminant(M)).ToString('  '));
}

  AssertMatrixEquals(
    ScalingMatrix(Vector3(0.5, 0.5, 0.5)),
    M.Inverse(M.Determinant),
    0.01);

  M := TranslationMatrix(Vector3(2, 2, 2));
  AssertMatrixEquals(
    TranslationMatrix(Vector3(-2, -2, -2)),
    M.Inverse(M.Determinant),
    0.01);
end;

procedure TTestCastleVectors.TestMultMatrixTranslation;
var
  M, NewM: TMatrix4;
  I: Integer;
  V: TVector3;
begin
  for I := 1 to 100 do
  begin
    M := RandomMatrix;
    V := RandomVector;
    NewM := M * TranslationMatrix(V);
    MultMatrixTranslation(M, V);
    AssertMatrixEquals(M, NewM, 0.001);
  end;
end;

procedure TTestCastleVectors.TestMultMatricesTranslation;
var
  M, NewM, MInverse, NewMInverse: TMatrix4;
  I: Integer;
  V: TVector3;
begin
  for I := 1 to 100 do
  begin
    M := RandomMatrix;
    if not M.TryInverse(MInverse) then
      MInverse := TMatrix4.Identity;

    V := RandomVector;
    NewM := M * TranslationMatrix(V);
    NewMInverse := TranslationMatrix(-V) * MInverse;
    MultMatricesTranslation(M, MInverse, V);
    AssertMatrixEquals(M, NewM, 0.001);
    AssertMatrixEquals(MInverse, NewMInverse, 0.001);
  end;
end;

procedure TTestCastleVectors.TestIndexedPolygonNormalArea;
const
  Poly: array [0..4] of TVector3 = (
    (X: 5; Y: 4; Z: 0),
    (X: 4; Y: 4; Z: 0),
    (X: 2; Y: 3; Z: 0),
    (X: 2; Y: 1; Z: 0),
    (X: 6; Y: 2; Z: 0) );
  CCWPolyIndex: array [0..6] of LongInt = (0, 1, 5, 2, 3, 4, 999);
  CWPolyIndex: array [0..6] of LongInt = (666, 4, 105, 3, 2, 1, 0);
begin
  AssertVectorEquals(
    Vector3(0, 0, 1),
    IndexedConvexPolygonNormal(@CCWPolyIndex, High(CCWPolyIndex) + 1,
      @Poly, High(Poly) + 1, TVector3.Zero));

  AssertVectorEquals(
    Vector3(0, 0, -1),
    IndexedConvexPolygonNormal(@CWPolyIndex, High(CWPolyIndex) + 1,
      @Poly, High(Poly) + 1, TVector3.Zero));

  AssertSameValue(8,
    IndexedConvexPolygonArea(@CCWPolyIndex, High(CCWPolyIndex) + 1,
      @Poly, High(Poly) + 1));

  AssertSameValue(8,
    IndexedConvexPolygonArea(@CWPolyIndex , High(CWPolyIndex) + 1,
      @Poly, High(Poly) + 1));
end;

procedure TTestCastleVectors.TestSphereRayIntersection;
var
  Res: boolean;
  I: TVector3;
begin
  Res := TrySphereRayIntersection(I, Vector3(3, 0, 0), 10,
    Vector3(0, 0, 0), Vector3(1, 0, 0));
  AssertTrue(Res);
  AssertVectorEquals(Vector3(13, 0, 0), I);

  Res := TrySphereRayIntersection(I, Vector3(3, 0, 0), 10,
    Vector3(0, 0, 0), Vector3(-1, 0, 0));
  AssertTrue(Res);
  AssertVectorEquals(Vector3(-7, 0, 0), I);

  Res := TrySphereRayIntersection(I, Vector3(3, 0, 0), 10,
    Vector3(20, 0, 0), Vector3(1, 0, 0));
  AssertFalse(Res);

  Res := TrySphereRayIntersection(I, Vector3(3, 0, 0), 10,
    Vector3(20, 0, 0), Vector3(-1, 0, 0));
  AssertTrue(Res);
  AssertVectorEquals(Vector3(13, 0, 0), I);
end;

{ global utils --------------------------------------------------------------- }

function RandomVector: TVector3;
begin
  Result := Vector3(
    Random*1000,
    Random*1000,
    Random*1000
  );
end;

function RandomMatrix: TMatrix4;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.Data[I, J] := 50 - Random * 100;
end;

function RandomNonProjectionMatrix: TMatrix4;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 2 do
      Result.Data[I, J] := 50 - Random * 100;

  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 0;
  Result.Data[3, 3] := 1;
end;

procedure TTestCastleVectors.TestMatrixMultiplication;
var
  M1, M2, M3, Result1, Result2: TMatrix4;
begin
  M1.Columns[0] := Vector4(1, 0, 0, 0);
  M1.Columns[1] := Vector4(0, 1, 0, 0);
  M1.Columns[2] := Vector4(0, 0, 1, 0);
  M1.Columns[3] := Vector4(-0.31, 1.26, -0.03, 1);

  M2.Columns[0] := Vector4( 0.58,  0.75, 0.31, 0.00);
  M2.Columns[1] := Vector4(-0.81,  0.52, 0.26, 0.00);
  M2.Columns[2] := Vector4( 0.03, -0.40, 0.92, 0.00);
  M2.Columns[3] := Vector4( 0.00,  0.00, 0.00, 1.00);

  M3.Columns[0] := Vector4(1.00, 0.00, 0.00,  0.31);
  M3.Columns[1] := Vector4(0.00, 1.00, 0.00, -1.26);
  M3.Columns[2] := Vector4(0.00, 0.00, 1.00,  0.03);
  M3.Columns[3] := Vector4(0.00, 0.00, 0.00,  1.00);

  Result1 := M1 * M2;
  Result2 := M1 * M2;
  AssertMatrixEquals(Result1, Result2, 0.1);

  Result2 := M1 * M2 * M3;

  Result1 := M1 * M2;
  Result1 := Result1 * M3;
  AssertMatrixEquals(Result1, Result2, 0.1);

  Result1 := M1 * M2 * M3;
  AssertMatrixEquals(Result1, Result2, 0.1);
end;

procedure TTestCastleVectors.TestMatrixTranspose;
var
  M1, M2: TMatrix3;
begin
  M1.Columns[0] := Vector3(1, 2, 3);
  M1.Columns[1] := Vector3(4, 5, 6);
  M1.Columns[2] := Vector3(7, 8, 9);

  M2.Columns[0] := Vector3(1, 4, 7);
  M2.Columns[1] := Vector3(2, 5, 8);
  M2.Columns[2] := Vector3(3, 6, 9);

  M1 := M1.Transpose;
  AssertTrue(TMatrix3.PerfectlyEquals(M1, M2));
end;

procedure TTestCastleVectors.TestVector3FromStr;
var
  V: TVector3;
begin
  try
    V := Vector3FromStr('1 2 abc');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3FromStr('1 2 3 4');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3FromStr('1 2');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3FromStr('');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  V := Vector3FromStr('  11       22 ' + NL + ' 33    ');
  AssertSameValue(11, V[0]);
  AssertSameValue(22, V[1]);
  AssertSameValue(33, V[2]);
end;

procedure TTestCastleVectors.TestVector4FromStr;
var
  V: TVector4;
begin
  try
    V := Vector4FromStr('1 2 3 abc');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4FromStr('1 2 3 4 5');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4FromStr('1 2 3');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4FromStr('');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  V := Vector4FromStr('  11       22 ' + NL + ' 33    44');
  AssertSameValue(11, V[0]);
  AssertSameValue(22, V[1]);
  AssertSameValue(33, V[2]);
  AssertSameValue(44, V[3]);
end;

procedure TTestCastleVectors.TestPlaneTransform;

  function PointLiesOnPlane(const Point: TVector3; const Plane: TVector4): boolean;
  var
    PlaneDir: TVector3 absolute Plane;
  begin
    // Writeln('point ', Point.ToString, ' gives ',
    //   (TVector3.DotProduct(Point, PlaneDir) + Plane[3]):1:2);
    Result := IsZero(TVector3.DotProduct(Point, PlaneDir) + Plane[3], 0.001);
  end;

  procedure DoTest(const Plane: TVector4; const Matrix: TMatrix4;
    const PointsYes: array of TVector3;
    const PointsNo: array of TVector3);
  var
    I: Integer;
    NewPlane: TVector4;
  begin
    NewPlane := PlaneTransform(Plane, Matrix);
    // Writeln('New plane ', NewPlane.ToString);
    for I := 0 to High(PointsYes) do
      AssertTrue(PointLiesOnPlane(PointsYes[I], NewPlane));
    for I := 0 to High(PointsNo) do
      AssertTrue(not PointLiesOnPlane(PointsNo[I], NewPlane));
  end;

begin
  { x = 0 plane }
  DoTest(Vector4(1, 0, 0, 0),
    TMatrix4.Identity,
    [ Vector3(0,  10,  10),
      Vector3(0, -10,  10),
      Vector3(0,  10, -10),
      Vector3(0, -10, -10) ],
    [ Vector3( 10,  10, 123),
      Vector3(-10,  10, 2),
      Vector3( 10, -10, -3),
      Vector3(1, 0, 0) ]);

  { rotate x = 0 plane to make z = 0 }
  DoTest(Vector4(1, 0, 0, 0),
    RotationMatrixDeg(90, 0, 1, 0),
    [ Vector3( 10,  10, 0),
      Vector3(-10,  10, 0),
      Vector3( 10, -10, 0),
      Vector3(-10, -10, 0) ],
    [ Vector3( 10,  10, 123),
      Vector3(-10,  10, 2),
      Vector3( 10, -10, -3),
      Vector3(0, 0, 1) ]);

  { rotate and move x = 0 plane to make z = 10 }
  DoTest(Vector4(1, 0, 0, 0),
    TranslationMatrix(Single(0), 0, 10) * RotationMatrixDeg(90, 0, 1, 0),
    [ Vector3( 10,  10, 10),
      Vector3(-10,  10, 10),
      Vector3( 10, -10, 10),
      Vector3(-10, -10, 10) ],
    [ Vector3( 10,  10, 0),
      Vector3(-10,  10, 0),
      Vector3( 10, -10, 0),
      Vector3(-10, -10, 0),
      Vector3( 10,  10, 123),
      Vector3(-10,  10, 2),
      Vector3( 10, -10, -3),
      Vector3(0, 0, 1) ]);

  { rotate and move and scale x = 0 plane to make z = 100 }
  DoTest(Vector4(1, 0, 0, 0),
    ScalingMatrix(Vector3(10, 10, 10)) *
    TranslationMatrix(Single(0), 0, 10) *
    RotationMatrixDeg(90, 0, 1, 0),
    [ Vector3( 10,  10, 100),
      Vector3(-10,  10, 100),
      Vector3( 10, -10, 100),
      Vector3(-10, -10, 100) ],
    [ Vector3( 10,  10, 10),
      Vector3(-10,  10, 10),
      Vector3( 10, -10, 0),
      Vector3(-10, -10, 0),
      Vector3( 10,  10, 123),
      Vector3(-10,  10, 2),
      Vector3( 10, -10, -3),
      Vector3(0, 0, 1) ]);
end;

procedure TTestCastleVectors.TestTransformToFromCoordsMatrix;
var
  M, MInverse: TMatrix4;
  NewOrigin, NewX, NewY, NewZ: TVector3;
begin
  NewOrigin := RandomVector;
  repeat NewX := RandomVector.Normalize until not NewX.IsZero;
  NewY := AnyOrthogonalVector(NewX).Normalize;
  NewZ := TVector3.CrossProduct(NewX, NewY);

  M        := TransformToCoordsMatrix  (NewOrigin, NewX, NewY, NewZ);
  MInverse := TransformFromCoordsMatrix(NewOrigin, NewX, NewY, NewZ);

  try
    AssertMatrixEquals(TMatrix4.Identity, M * MInverse, 0.01);
    AssertMatrixEquals(TMatrix4.Identity, MInverse * M, 0.01);
  except
    Writeln('Failed for origin=', NewOrigin.ToRawString,
      ' newX=', NewX.ToRawString);
    raise;
  end;
end;

procedure TTestCastleVectors.Test2D;
const
  P1: TVector3 = (X: 1; Y: 2; Z: 3);
  P2: TVector3 = (X: 2; Y: 5; Z: 13);
begin
  AssertSameValue(Sqr(1) + Sqr(3) + Sqr(10), PointsDistanceSqr(P1, P2), 0.01);
  AssertSameValue(Sqr(3) + Sqr(10), PointsDistance2DSqr(P1, P2, 0), 0.01);
  AssertSameValue(Sqr(1) + Sqr(10), PointsDistance2DSqr(P1, P2, 1), 0.01);
  AssertSameValue(Sqr(1) + Sqr(3), PointsDistance2DSqr(P1, P2, 2), 0.01);
  try
    PointsDistance2DSqr(P1, P2, 3);
    Fail('Above PointsDistance2DSqr with IgnoreIndex = 3 should raise exception');
  except end;
end;

procedure TTestCastleVectors.TestApproximateScale;
const
  Epsilon = 0.0001;
begin
  AssertSameValue(2, Approximate3DScale(2, 2, 2), Epsilon);
  AssertSameValue(-2, Approximate3DScale(-2, -2, -2), Epsilon);
  AssertSameValue(1, Approximate3DScale(1, 1, 1), Epsilon);
  AssertSameValue(-1, Approximate3DScale(-1, -1, -1), Epsilon);
  AssertSameValue(7/3, Approximate3DScale(1, 3, 3), Epsilon);
  AssertSameValue(-7/3, Approximate3DScale(-1, -3, -3), Epsilon);
  AssertSameValue(1, Approximate3DScale(-1, 1, 1), Epsilon);

  AssertSameValue(2, Approximate2DScale(2, 2), Epsilon);
  AssertSameValue(-2, Approximate2DScale(-2, -2), Epsilon);
  AssertSameValue(1, Approximate2DScale(-1, 1), Epsilon);
end;

procedure TTestCastleVectors.TestXYZ;
const
  V2Const: TVector2 = (X: 1; Y: 2);
  V3Const: TVector3 = (X: 1; Y: 2; Z: 3);
  V4Const: TVector4 = (X: 1; Y: 2; Z: 3; W: 4);
var
  V2: TVector2;
  V3: TVector3;
  V4: TVector4;
begin
  V2 := V2Const;
  V3 := V3Const;
  V4 := V4Const;

  AssertEquals(1, V2.X);
  V2.X := 33;
  AssertEquals(33, V2.X);
  AssertEquals(2, V2.Y);

  AssertEquals(1, V3.X);
  AssertEquals(2, V3.Y);
  AssertEquals(3, V3.Z);
  V3.Z := 44;
  AssertEquals(1, V3.X);
  AssertEquals(2, V3.Y);
  AssertEquals(44, V3.Z);

  AssertEquals(1, V4.X);
  AssertEquals(2, V4.Y);
  AssertEquals(3, V4.Z);
  AssertEquals(4, V4.W);
end;

procedure TTestCastleVectors.TestPlaneMove;
var
  Plane: TVector4;
begin
  Plane := Vector4(1, 0, 0, 10); // x = -10
  AssertVectorEquals(Vector4(1, 0, 0, 9), PlaneMove(Plane, Vector3(1, 2, 3)));

  Plane := Vector4(1, 0, 0, 10); // x = -10
  PlaneMoveVar(Plane, Vector3(1, 2, 3));
  AssertVectorEquals(Vector4(1, 0, 0, 9), Plane);

  Plane := Vector4(0, 1, 0, 10); // y = -10
  AssertVectorEquals(Vector4(0, 1, 0, 8), PlaneMove(Plane, Vector3(1, 2, 3)));

  Plane := Vector4(0, 1, 0, 10); // y = -10
  PlaneMoveVar(Plane, Vector3(1, 2, 3));
  AssertVectorEquals(Vector4(0, 1, 0, 8), Plane);

  Plane := Vector4(0, 1, 0, 8); // y = -10
  AssertVectorEquals(Vector4(0, 1, 0, 10), PlaneAntiMove(Plane, Vector3(1, 2, 3)));
end;

procedure TTestCastleVectors.TestPlaneMoveRandom;
var
  I: Integer;
  Plane: TVector4;
  Move, PlaneDir: TVector3;
begin
  for I := 1 to 100 do
  begin
    repeat
      PlaneDir := RandomVector;
    until not PlaneDir.IsZero;
    Plane := Vector4(PlaneDir, Random * 100);
    Move := RandomVector;
    // "PlaneAntiMove + PlaneMove" should zero each other out
    AssertVectorEquals(Plane, PlaneAntiMove(PlaneMove(Plane, Move), Move), 1.0);
  end;
end;

procedure TTestCastleVectors.TestTryInverseHarder;
var
  M, M2: TMatrix4;
begin
  // used in gate_backround in castle-game demo

  M[0, 0] := -0.001710;
  M[1, 0] := -0.004698;
  M[2, 0] :=  0.000000;
  M[3, 0] :=  0.000000;

  M[0, 1] :=  0.004698;
  M[1, 1] := -0.001710;
  M[2, 1] :=  0.000000;
  M[3, 1] :=  0.000000;

  M[0, 2] := 0.000000;
  M[1, 2] := 0.000000;
  M[2, 2] := 0.005000;
  M[3, 2] := 0.000000;

  M[0, 3] := -79.753189;
  M[1, 3] := -70.291077;
  M[2, 3] :=   0.182218;
  M[3, 3] :=   1.000000;

  AssertFalse(M.TryInverse(M2));
  AssertTrue(TryInverseHarder(M, M2));
end;

procedure TTestCastleVectors.TestMaxAbsVectorCoord;
begin
  AssertEquals(0, MaxVectorCoord(Vector2(1,  -10)));
  AssertEquals(1, MaxVectorCoord(Vector2(1,  10)));

  AssertEquals(2, MaxVectorCoord(Vector3(1, 2, 3)));
  AssertEquals(0, MaxVectorCoord(Vector3(10, 2, 3)));
  AssertEquals(0, MaxVectorCoord(Vector3(1, 1, 1)));
  AssertEquals(1, MaxVectorCoord(Vector3(1, 2, 2)));
  AssertEquals(1, MaxVectorCoord(Vector3(1, 2, -3)));

  AssertEquals(3, MaxVectorCoord(Vector4(1, 2, 3, 10)));
  AssertEquals(0, MaxVectorCoord(Vector4(-1, -2, -3, -10)));

  AssertEquals(1, MaxAbsVectorCoord(Vector2(1,  -10)));
  AssertEquals(1, MaxAbsVectorCoord(Vector2(1,  10)));

  AssertEquals(2, MaxAbsVectorCoord(Vector3(1,  10, -20)));
  AssertEquals(1, MaxAbsVectorCoord(Vector3(1,  -20, 10)));

  AssertEquals(2, MaxAbsVectorCoord(Vector3(1, 2, 3)));
  AssertEquals(0, MaxAbsVectorCoord(Vector3(10, 2, 3)));
  AssertEquals(0, MaxAbsVectorCoord(Vector3(1, 1, 1)));
  AssertEquals(1, MaxAbsVectorCoord(Vector3(1, 2, 2)));
  AssertEquals(2, MaxAbsVectorCoord(Vector3(1, 2, -3)));

  AssertEquals(3, MaxAbsVectorCoord(Vector4(1, 2, 3, 10)));
  AssertEquals(3, MaxAbsVectorCoord(Vector4(-1, -2, -3, -10)));
end;

procedure TTestCastleVectors.TestMinAbsVectorCoord;
begin
  AssertEquals(0, MinAbsVectorCoord(Vector2(1, -10)));
  AssertEquals(0, MinAbsVectorCoord(Vector2(1,  10)));
  AssertEquals(1, MinAbsVectorCoord(Vector2(-10, -1)));
  AssertEquals(1, MinAbsVectorCoord(Vector2( 10,  1)));

  AssertEquals(0, MinAbsVectorCoord(Vector3(1, 2, 3)));
  AssertEquals(1, MinAbsVectorCoord(Vector3(33, 2, 3)));
  AssertEquals(2, MinAbsVectorCoord(Vector3(3, 2, 1)));

  AssertEquals(0, MinAbsVectorCoord(Vector4(1, 2, 3, 10)));
  AssertEquals(0, MinAbsVectorCoord(Vector4(-1, -2, -3, -10)));
end;

procedure TTestCastleVectors.TestPointOnLineClosestToLine;
var
  I: TVector3;
begin
  // lines parallel
  AssertFalse(PointOnLineClosestToLine(I,
    Vector3(0, 0, 0), Vector3(1, 1, 1),
    Vector3(10, 1, 1), Vector3(1, 1, 1)
  ));

  AssertTrue(PointOnLineClosestToLine(I,
    Vector3(0, 0, 0), Vector3(1, 1, 1),
    Vector3(0, 0, 0), Vector3(-1, 1, 1)
  ));
  AssertVectorEquals(Vector3(0, 0, 0), I);

  AssertTrue(PointOnLineClosestToLine(I,
    Vector3(1, 2, 3), Vector3(1, 1, 1),
    Vector3(1, 2, 3), Vector3(-1, 1, 1)
  ));
  AssertVectorEquals(Vector3(1, 2, 3), I);

  AssertTrue(PointOnLineClosestToLine(I,
    Vector3(0, 0, 0), Vector3(1, 0, 0),
    Vector3(110, 10, 10), Vector3(1, 1, 1)
  ));
  AssertVectorEquals(Vector3(100, 0, 0), I);

  AssertTrue(PointOnLineClosestToLine(I,
    Vector3(0, 0, 0), Vector3(1, 0, 0),
    Vector3(110, 10, 10), Vector3(1, 1, 0)
  ));
  AssertVectorEquals(Vector3(100, 0, 0), I);

  AssertTrue(PointOnLineClosestToLine(I,
    Vector3(0.00, 0.00, 0.00),
    Vector3(0.00, 1.00, 0.00),
    Vector3(-6.58, 1.97, -5.73),
    Vector3(0.74, -0.16, 0.65)
  ));
  AssertVectorEquals(Vector3(0, 0.5, 0), I, 0.1);
end;

procedure TTestCastleVectors.TestRotatePointAroundAxis;
begin
  AssertVectorEquals(
    RotatePointAroundAxis90(Vector3(1, 2, 3), Vector3(4, 5, 6)),
    RotatePointAroundAxisRad(Pi / 2, Vector3(1, 2, 3), Vector3(4, 5, 6)));
end;

procedure TTestCastleVectors.TestMakeVectorOrthogonal;
var
  V, NewV: TVector3;
begin
  V := Vector3(Sqrt(2) / 2, Sqrt(2) / 2, 0);
  NewV := MakeVectorOrthogonal(V, Vector3(0, 1, 0));
  AssertVectorEquals(Vector3(1, 0, 0), NewV);
end;

procedure TTestCastleVectors.TestEpsilonInModelViewToNormalMatrix;
var
  Mv1, Mv2: TMatrix4;
  Norml1Approx: TMatrix3;
begin
{
Mv1 and Mv2 are 2 TMatrix4 values which are close, but not precisely equal.
If ModelViewToNormalMatrix would use IsZero(D), then it would yield
drastically different results for them, due to one matrix resulting
in determinant close enough to Single epsilon, and other not.

------------------------------------------------------------------------------
Exact test input, generated from testcase on https://github.com/castle-engine/view3dscene/issues/35,
with mesh renderer code:

    if Arrays.Count = 629 then
    begin
      Writeln('Arrays.Count ', Arrays.Count);
      Writeln('ModelView ', Shape.ModelView.ToString(' '));
      Writeln('ModelViewRaw ', Shape.ModelView.ToRawString(' '));
      Writeln('NormalMatrix ', ModelViewToNormalMatrix(Shape.ModelView).ToString(' '));
      Writeln('NormalMatrixRaw ', ModelViewToNormalMatrix(Shape.ModelView).ToRawString(' '));
    end;

---------------------------------------------------------------------------
ModelView  0.05 0.04 0.00 0.12
 0.00 -0.01 -0.02 -0.03
 -0.02 0.09 0.00 -0.41
 0.00 0.00 0.00 1.00

ModelViewRaw  0.04658060148358345 0.035663504153490067 -0.0014015145134180784 0.12454544752836227
 -0.0025431409012526274 -0.0062981243245303631 -0.019934356212615967 -0.025968225672841072
 -0.017993897199630737 0.093211852014064789 -0.0008106923196464777 -0.40757930278778076
 0 0 0 1

NormalMatrix (ModelViewToNormalMatrix determinant 0.000100) 18.63 3.57 -3.50
 -1.02 -0.63 -49.84
 -7.20 9.32 -2.03

NormalMatrixRaw (ModelViewToNormalMatrix determinant 0.000100) 18.632238388061523 3.5663502216339111 -3.503786563873291
 -1.0172562599182129 -0.62981235980987549 -49.835891723632813
 -7.1975584030151367 9.3211841583251953 -2.0267307758331299

(ModelViewToNormalMatrix determinant 0.000100)(ModelViewToNormalMatrix determinant 1.000000)Arrays.Count 629

ModelView  0.05 0.04 0.00 0.12
 0.00 -0.01 -0.02 -0.03
 -0.02 0.09 0.00 -0.41
 0.00 0.00 0.00 1.00

ModelViewRaw  0.046580597758293152 0.03566349670290947 -0.0014015162596479058 0.12454545497894287
 -0.0025431474205106497 -0.0062981159426271915 -0.019934354349970818 -0.025968223810195923
 -0.017993893474340439 0.093211852014064789 -0.00081068999134004116 -0.40757903456687927
 0 0 0 1

NormalMatrix (ModelViewToNormalMatrix determinant 0.000100) 0.05 0.04 0.00
 0.00 -0.01 -0.02
 -0.02 0.09 0.00
------------------------------------------------------------------------------
}

  Mv1.Columns[0] := Vector4(
    0.04658060148358345,
   -0.0025431409012526274,
   -0.017993897199630737,
    0
  );
  Mv1.Columns[1] := Vector4(
    0.035663504153490067,
   -0.0062981243245303631,
    0.093211852014064789,
    0
  );
  Mv1.Columns[2] := Vector4(
   -0.0014015145134180784,
   -0.019934356212615967,
   -0.0008106923196464777,
    0
  );
  Mv1.Columns[3] := Vector4(
    0.12454544752836227,
   -0.025968225672841072,
   -0.40757930278778076,
    1
  );

  Mv2.Columns[0] := Vector4(
    0.046580597758293152,
   -0.0025431474205106497,
   -0.017993893474340439,
    0
  );
  Mv2.Columns[1] := Vector4(
    0.03566349670290947,
   -0.0062981159426271915,
    0.093211852014064789,
    0
  );
  Mv2.Columns[2] := Vector4(
   -0.0014015162596479058,
   -0.019934354349970818,
   -0.00081068999134004116,
    0
  );
  Mv2.Columns[3] := Vector4(
    0.12454545497894287,
   -0.025968223810195923,
   -0.40757903456687927,
    1
  );

  AssertTrue(TMatrix4.Equals(Mv1, Mv2));
  // Writeln(ModelViewToNormalMatrix(Mv1).ToString(' '));
  // Writeln(ModelViewToNormalMatrix(Mv2).ToString(' '));

  Norml1Approx.Columns[0] := Vector3(
    18.63,
    -1.02,
    -7.20
  );
  Norml1Approx.Columns[1] := Vector3(
    3.57,
    -0.63,
    9.32
  );
  Norml1Approx.Columns[2] := Vector3(
    -3.50,
    -49.84,
    -2.03
  );
  AssertTrue(TMatrix3.Equals(ModelViewToNormalMatrix(Mv1), Norml1Approx, 0.01));
  AssertTrue(TMatrix3.Equals(ModelViewToNormalMatrix(Mv2), Norml1Approx, 0.01));
end;

procedure TTestCastleVectors.TestInit;
var
  V2: TVector2;
  V3: TVector3;
  V4: TVector4;
begin
  V4.Init(1, 2, 3, 4);
  AssertVectorEquals(Vector4(1, 2, 3, 4), V4);

  V3.Init(11, 22, 33);
  AssertVectorEquals(Vector3(11, 22, 33), V3);

  V2.Init(111, 222);
  AssertVectorEquals(Vector2(111, 222), V2);
end;

procedure TTestCastleVectors.TestRotationZeroAxis;
var
  T: TTransformation;
begin
  T.Init;
  T.Multiply(
    Vector4(0, 0, 0, Pi/2),
    Vector3(1, 1, 1),
    Vector3(0, 0, 0)
  );
  AssertMatrixEquals(T.Transform, TMatrix4.Identity);
  AssertMatrixEquals(T.InverseTransform, TMatrix4.Identity);
  AssertSameValue(1, T.Scale);
  AssertTrue(T.UniformScale);
end;

procedure TTestCastleVectors.TestVectorsList;

{ This test is similar to TTestGenericsCollections.TestVectorsList,
  but now using TVector2 instead of TMyVector. }

var
  List: TVector2List;
  R1, R2, R: TVector2;
begin
  List := TVector2List.Create;
  try
    R1.X := 11;
    R1.Y := 22;
    List.Add(R1);

    R2.X := 33;
    R2.Y := 44;
    List.Add(R2);

    R2.X := 33;
    R2.Y := 44;
    List.Add(R2);

    AssertEquals(3, List.Count);
    AssertEquals(11, List[0].X);
    AssertEquals(22, List[0].Y);
    AssertEquals(33, List[1].X);
    AssertEquals(44, List[1].Y);
    AssertEquals(33, List[2].X);
    AssertEquals(44, List[2].Y);

    List.Delete(2);

    AssertEquals(2, List.Count);
    AssertEquals(11, List[0].X);
    AssertEquals(22, List[0].Y);
    AssertEquals(33, List[1].X);
    AssertEquals(44, List[1].Y);

    AssertEquals(0, List.IndexOf(R1));
    AssertEquals(1, List.IndexOf(R2));

    // change R1 and R2, to make sure it doesn't matter for tests
    R1.X := 111111;
    R1.Y := 222222;
    R2.X := 333333;
    R2.Y := 444444;
    AssertEquals(-1, List.IndexOf(R1));
    AssertEquals(-1, List.IndexOf(R2));

    R.X := 11;
    R.Y := 22;
    AssertEquals(0, List.IndexOf(R));

    R.X := 33;
    R.Y := 44;
    AssertEquals(1, List.IndexOf(R));

    R.X := 11;
    R.Y := 22;
    List.Remove(R);
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0].X);
    AssertEquals(44, List[0].Y);

    R.X := 666;
    R.Y := 22;
    List.Remove(R); // does nothing, no such item
    AssertEquals(1, List.Count);
    AssertEquals(33, List[0].X);
    AssertEquals(44, List[0].Y);
  finally FreeAndNil(List) end;
end;

initialization
  RegisterTest(TTestCastleVectors);
end.
