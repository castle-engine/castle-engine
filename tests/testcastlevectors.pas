{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleVectors;

{ $define VECTOR_MATH_SPEED_TESTS}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleVectors,
  CastleBaseTestCase;

type
  TTestCastleVectors = class(TCastleBaseTestCase)
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
  end;

function RandomVector: TVector3Single;
function RandomMatrix: TMatrix4Single;
function RandomNonProjectionMatrix: TMatrix4Single;

implementation

uses CastleUtils, CastleStringUtils, CastleTimeUtils, CastleTriangles;

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
var Intersection: TVector3Single;
    T: Single;
begin
 T := VectorDotProduct(Vector3Single(0, 0, 1), Vector3Single(0, 0, 6));
 AssertFloatsEqual(6, T);

 AssertTrue(TryPlaneLineIntersection(T,
   Vector4Single(0, 0, 1, 1),
   Vector3Single(2, 2, -3),
   Vector3Single(0, 0, 6) ));
 AssertFloatsEqual(1/3, T, 0.0000001);

 AssertTrue(TryPlaneSegmentDirIntersection(Intersection, T,
   Vector4Single(0, 0, 1, 1),
   Vector3Single(2, 2, -3),
   Vector3Single(0, 0, 6) ));
 AssertVectorsEqual(Vector3Single(2, 2, -1), Intersection);
 AssertFloatsEqual(1/3, T, 0.0000001);
end;

procedure WritelnSpeedTest(const s: string);
begin
 {$ifdef VECTOR_MATH_SPEED_TESTS}
 Writeln(s);
 {$endif}
end;

procedure TTestCastleVectors.TestCollisions;
const TriConst: TTriangle3Single = ((2, 2, -1), (3, 3, -1), (3, 2, -1));
      TriPlaneConst: TVector4Single = (0, 0, 1, 1);
      Pos1Const: TVector3Single = (2, 2, -3);
      Pos2Const: TVector3Single = (2, 2, 3);
      Coll: TVector3Single = (2, 2, -1);
var Intersection, Intersection2: TVector3Single;
begin
 { czy dziala TryTriangleSegmentCollision na tym naszym skonstruowanym
   przykladzie ? }
 AssertTrue( TryTriangleSegmentCollision(Intersection, TriConst, TriPlaneConst,
   Pos1Const, Pos2Const) and VectorsEqual(Intersection, Coll));
 { czy dziala tak samo gdy sam musi sobie wyliczyc TriPlane (test TrianglePlane) ? }
 AssertTrue( TryTriangleSegmentCollision(Intersection2, TriConst, TrianglePlane(TriConst),
   Pos1Const, Pos2Const) and VectorsEqual(Intersection2, Coll));
end;

procedure TTestCastleVectors.TestArea;
const
  Tri: TTriangle3Single = ((0, 0, 0), (10, 0, 0), (0, 25, 0));
  CCWPoly: array[0..4]of TVector2Single = ((5, 4), (2, 3), (4, 3), (2, 1), (6, 2));
  CWPoly: array[0..4]of TVector2Single = ((6, 2), (2, 1), (4, 3), (2, 3), (5, 4));
begin
 AssertTrue(TriangleArea(Tri) = 10*25/2);

 AssertTrue(Polygon2dArea(CCWPoly) = 5.5);
 AssertTrue(Polygon2dArea(CWPoly) = 5.5);
 AssertTrue(IsPolygon2dCCW(CCWPoly) > 0);
 AssertTrue(IsPolygon2dCCW(CWPoly) < 0);
end;

procedure TTestCastleVectors.TestPerpParallel;
var v: TVector3Single;
    i: integer;
begin
 for i := 1 to 10 do
 try
  v := RandomVector;
  AssertTrue( VectorsPerp(AnyOrthogonalVector(v), v) );
  { I has to comment it out -- it fails too often due to floating point
    inaccuracy. }
  { AssertTrue( VectorsParallel(VectorScale(v, Random*10), v) ); }
  AssertTrue( VectorsPerp(ZeroVector3Single, v) );
  AssertTrue( VectorsParallel(ZeroVector3Single, v) );
 except
  Writeln('and failed : v = ',VectorToNiceStr(v),
    ' anyPerp = ',VectorToNiceStr(AnyOrthogonalVector(v)));
  raise;
 end;

 AssertTrue( VectorsPerp(ZeroVector3Single, ZeroVector3Single) );
 AssertTrue( VectorsParallel(ZeroVector3Single, ZeroVector3Single) );

 AssertTrue( VectorsPerp(UnitVector3Single[0], UnitVector3Single[1]) );
 AssertTrue( VectorsPerp(UnitVector3Single[0], UnitVector3Single[2]) );
 AssertTrue( VectorsPerp(UnitVector3Single[1], UnitVector3Single[2]) );
 AssertTrue( not VectorsPerp(UnitVector3Single[0], UnitVector3Single[0]) );

 AssertTrue( not VectorsParallel(UnitVector3Single[0], UnitVector3Single[1]) );
 AssertTrue( not VectorsParallel(UnitVector3Single[0], UnitVector3Single[2]) );
 AssertTrue( not VectorsParallel(UnitVector3Single[1], UnitVector3Single[2]) );
 AssertTrue( VectorsParallel(UnitVector3Single[0], UnitVector3Single[0]) );
end;

procedure TTestCastleVectors.TestPlanesIntersection;
const
  P1: TVector4Single = (-1.9935636520385742, -0.00000009909226151, 0.25691652297973633, -30.014257431030273);
  P2: TVector4Single = (-1.2131816148757935, 1.90326225890658E-008, -1.5900282859802246, 1.5900282859802246);
var Line0, LineVector: TVector3Single;
begin
 TwoPlanesIntersectionLine(P1, P2, Line0, LineVector);
 { Writeln(VectorToRawStr(Line0), ' ', VectorToRawStr(LineVector)); }
end;

procedure TTestCastleVectors.TestOther;
var I1, I2, RayOrigin, RayDirection: TVector3Double;
    Plane: TVector4Double;
//    PlaneDir: TVector3Double absolute Plane;
    PlaneConstCoord: integer;
    PlaneConstVal: Double;
    b1, b2: boolean;
//    t1, t2: Double;

  function RandomVector3Double: TVector3Double;
  begin
   result[0] := Random*1000 -500.0;
   result[1] := Random*1000 -500.0;
   result[2] := Random*1000 -500.0;
  end;

const VConst: TVector3Single = (1.0, 2.0, 3.0);

var
  i: integer;
  V: TVector3Single;
  Time0, Time1, Time2: Double;
  StartTime: TProcessTimerResult;
begin
 { ------------------------------------------------------------
   testuj TrySimplePlaneRayIntersection przy uzyciu TryPlaneRayIntersection }
 for i := 1 to 100000 do
 begin
  RayOrigin := RandomVector3Double;
  RayDirection := RandomVector3Double;

  PlaneConstCoord := Random(3);
  PlaneConstVal := Random*1000 - 500;
  FillChar(Plane, SizeOf(Plane), 0);
  Plane[PlaneConstCoord] := -1;
  Plane[3] := PlaneConstVal;

  { czasami uczyn promien rownoleglym do [Simple]Plane (zeby zobaczyc
    czy sobie z tym radzi) }
  if Random(10) = 1 then
  begin
   RayDirection[PlaneConstCoord] := 0;
   b1 := TrySimplePlaneRayIntersection(I1, PlaneConstCoord, PlaneConstVal, RayOrigin, RayDirection);
   b2 := TryPlaneRayIntersection(I2, Plane, RayOrigin, RayDirection);
   Check( (not b1) and (not b2) ,'intersect with parallel plane');
  end else
  begin
   { nie wykonuj testu jesli wylosowalimy niepoprawne dane }
   if not VectorsEqual(RayDirection, Vector3Double(0, 0, 0)) then
   begin
    b1 := TrySimplePlaneRayIntersection(I1, PlaneConstCoord, PlaneConstVal, RayOrigin, RayDirection);
    b2 := TryPlaneRayIntersection(I2, Plane, RayOrigin, RayDirection);
    AssertEquals(b1, b2);
    if b1 then
    begin
{     if not VectorsEqual(I1, I2) or
        not FloatsEqual(I1[PlaneConstCoord], PlaneConstVal) or
	not FloatsEqual(I2[PlaneConstCoord], PlaneConstVal) then
     begin
      t1:=(PlaneConstVal-RayOrigin[PlaneConstCoord]) / RayDirection[PlaneConstCoord];
      t2 := -(plane[0]*RayOrigin[0] + plane[1]*RayOrigin[1] + plane[2]*RayOrigin[2] + plane[3])/
          VectorDotProduct(PlaneDir, RayDirection);
      Writeln('I1 = ',VectorToNiceStr(I1), ' I2 = ',VectorToNiceStr(I2), nl,
        'PlaneConst Coord = ',PlaneConstCoord, ' Value = ',PlaneConstVal, nl,
	'Plane = ',VectorToNiceStr(Plane), nl,
	'RayOrigin = ',VectorToNiceStr(RayOrigin), ' RayDirection = ',VectorToNiceStr(RayDirection), nl,
	FloatToNiceStr(t1), nl,
	FloatToNiceStr(t2), nl,
	VectorToNiceStr(VectorAdd(RayOrigin, VectorScale(RayDirection, t1))), nl,
	VectorToNiceStr(VectorAdd(RayOrigin, VectorScale(RayDirection, t2)))
      );
     end; }
     AssertFloatsEqual(PlaneConstVal, I1[PlaneConstCoord]);
     AssertFloatsEqual(PlaneConstVal, I2[PlaneConstCoord]);
     AssertVectorsEqual(I1, I2);
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
  VectorScaleVar(V, Pi);
 end;
 Time1 := ProcessTimerSeconds(ProcessTimer, StartTime);
 WritelnSpeedTest(Format('Using assignment + VectorScaleVar = %f',[Time1]));

 StartTime := ProcessTimer;
 for i := 1 to SPEED_TEST_2_CYCLES do
 begin
  V := VectorScale(VConst, Pi);
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
  var v, v2: TVector3Single;
      s: string;
  begin
   v := RandomVector;
   s := VectorToRawStr(v);
   v2 := Vector3SingleFromStr(s);
   AssertVectorsEqual(v2, v);
  end;

  procedure OneTestByDeformat;
  var v, v2: TVector3Single;
      s: string;
  begin
   v := RandomVector;
   s := VectorToRawStr(v);
   DeFormat(s, '%.single. %.single. %.single.', [@v2[0], @v2[1], @v2[2]]);
   AssertVectorsEqual(v2, v);
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
  M: TMatrix4Single;
begin
  M := ScalingMatrix(Vector3Single(2, 2, 2));

{ Tests:
  Writeln(MatrixToNiceStr(M, '  '));
  Writeln(MatrixToNiceStr(ScalingMatrix(Vector3Single(0.5, 0.5, 0.5)), '  '));
  Writeln(MatrixToNiceStr(MatrixInverse(M, MatrixDeterminant(M)), '  '));
}

  AssertMatricesEqual(
    ScalingMatrix(Vector3Single(0.5, 0.5, 0.5)),
    MatrixInverse(M, MatrixDeterminant(M)),
    0.01);

  M := TranslationMatrix(Vector3Single(2, 2, 2));
  AssertMatricesEqual(
    TranslationMatrix(Vector3Single(-2, -2, -2)),
    MatrixInverse(M, MatrixDeterminant(M)),
    0.01);
end;

procedure TTestCastleVectors.TestMultMatrixTranslation;
var
  M, NewM: TMatrix4Single;
  I: Integer;
  V: TVector3Single;
begin
  for I := 1 to 100 do
  begin
    M := RandomMatrix;
    V := RandomVector;
    NewM := MatrixMult(M, TranslationMatrix(V));
    MultMatrixTranslation(M, V);
    AssertMatricesEqual(M, NewM, 0.001);
  end;
end;

procedure TTestCastleVectors.TestMultMatricesTranslation;
var
  M, NewM, MInverse, NewMInverse: TMatrix4Single;
  I: Integer;
  V: TVector3Single;
begin
  for I := 1 to 100 do
  begin
    M := RandomMatrix;
    if not TryMatrixInverse(M, MInverse) then
      MInverse := IdentityMatrix4Single;

    V := RandomVector;
    NewM := MatrixMult(M, TranslationMatrix(V));
    NewMInverse := MatrixMult(TranslationMatrix(VectorNegate(V)), MInverse);
    MultMatricesTranslation(M, MInverse, V);
    AssertMatricesEqual(M, NewM, 0.001);
    AssertMatricesEqual(MInverse, NewMInverse, 0.001);
  end;
end;

procedure TTestCastleVectors.TestIndexedPolygonNormalArea;
const
  Poly: array [0..4] of TVector3Single = ((5, 4, 0), (4, 4, 0), (2, 3, 0), (2, 1, 0), (6, 2, 0));
  CCWPolyIndex: array [0..6] of LongInt = (0, 1, 5, 2, 3, 4, 999);
  CWPolyIndex: array [0..6] of LongInt = (666, 4, 105, 3, 2, 1, 0);
begin
  AssertVectorsEqual(
    Vector3Single(0, 0, 1),
    IndexedConvexPolygonNormal(@CCWPolyIndex, High(CCWPolyIndex) + 1,
      @Poly, High(Poly) + 1, ZeroVector3Single));

  AssertVectorsEqual(
    Vector3Single(0, 0, -1),
    IndexedConvexPolygonNormal(@CWPolyIndex, High(CWPolyIndex) + 1,
      @Poly, High(Poly) + 1, ZeroVector3Single));

  AssertFloatsEqual(8,
    IndexedConvexPolygonArea(@CCWPolyIndex, High(CCWPolyIndex) + 1,
      @Poly, High(Poly) + 1));

  AssertFloatsEqual(8,
    IndexedConvexPolygonArea(@CWPolyIndex , High(CWPolyIndex) + 1,
      @Poly, High(Poly) + 1));
end;

procedure TTestCastleVectors.TestSphereRayIntersection;
var
  Res: boolean;
  I: TVector3Single;
begin
  Res := TrySphereRayIntersection(I, Vector3Single(3, 0, 0), 10,
    Vector3Single(0, 0, 0), Vector3Single(1, 0, 0));
  AssertTrue(Res);
  AssertVectorsEqual(Vector3Single(13, 0, 0), I);

  Res := TrySphereRayIntersection(I, Vector3Single(3, 0, 0), 10,
    Vector3Single(0, 0, 0), Vector3Single(-1, 0, 0));
  AssertTrue(Res);
  AssertVectorsEqual(Vector3Single(-7, 0, 0), I);

  Res := TrySphereRayIntersection(I, Vector3Single(3, 0, 0), 10,
    Vector3Single(20, 0, 0), Vector3Single(1, 0, 0));
  AssertFalse(Res);

  Res := TrySphereRayIntersection(I, Vector3Single(3, 0, 0), 10,
    Vector3Single(20, 0, 0), Vector3Single(-1, 0, 0));
  AssertTrue(Res);
  AssertVectorsEqual(Vector3Single(13, 0, 0), I);
end;

{ global utils --------------------------------------------------------------- }

function RandomVector: TVector3Single;
begin
  result[0] := Random*1000;
  result[1] := Random*1000;
  result[2] := Random*1000;
end;

function RandomMatrix: TMatrix4Single;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result[I][J] := 50 - Random * 100;
end;

function RandomNonProjectionMatrix: TMatrix4Single;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 2 do
      Result[I][J] := 50 - Random * 100;

  Result[0][3] := 0;
  Result[1][3] := 0;
  Result[2][3] := 0;
  Result[3][3] := 1;
end;

procedure TTestCastleVectors.TestMatrixMultiplication;
var
  M1, M2, M3, Result1, Result2: TMatrix4Single;
begin
  M1[0] := Vector4Single(1, 0, 0, 0);
  M1[1] := Vector4Single(0, 1, 0, 0);
  M1[2] := Vector4Single(0, 0, 1, 0);
  M1[3] := Vector4Single(-0.31, 1.26, -0.03, 1);

  M2[0] := Vector4Single( 0.58,  0.75, 0.31, 0.00);
  M2[1] := Vector4Single(-0.81,  0.52, 0.26, 0.00);
  M2[2] := Vector4Single( 0.03, -0.40, 0.92, 0.00);
  M2[3] := Vector4Single( 0.00,  0.00, 0.00, 1.00);

  M3[0] := Vector4Single(1.00, 0.00, 0.00,  0.31);
  M3[1] := Vector4Single(0.00, 1.00, 0.00, -1.26);
  M3[2] := Vector4Single(0.00, 0.00, 1.00,  0.03);
  M3[3] := Vector4Single(0.00, 0.00, 0.00,  1.00);

  Result1 := M1 * M2;
  Result2 := MatrixMult(M1, M2);
  AssertMatricesEqual(Result1, Result2, 0.1);

  Result2 := MatrixMult(MatrixMult(M1, M2), M3);

  Result1 := M1 * M2;
  Result1 := Result1 * M3;
  AssertMatricesEqual(Result1, Result2, 0.1);

  Result1 := M1 * M2 * M3;
  AssertMatricesEqual(Result1, Result2, 0.1);
end;

procedure TTestCastleVectors.TestMatrixTranspose;
var
  M1, M2: TMatrix3Single;
begin
  M1[0] := Vector3Single(1, 2, 3);
  M1[1] := Vector3Single(4, 5, 6);
  M1[2] := Vector3Single(7, 8, 9);

  M2[0] := Vector3Single(1, 4, 7);
  M2[1] := Vector3Single(2, 5, 8);
  M2[2] := Vector3Single(3, 6, 9);

  MatrixTransposeVar(M1);
  AssertTrue(MatricesPerfectlyEqual(M1, M2));
end;

procedure TTestCastleVectors.TestVector3FromStr;
var
  V: TVector3Single;
begin
  try
    V := Vector3SingleFromStr('1 2 abc');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3SingleFromStr('1 2 3 4');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3SingleFromStr('1 2');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3SingleFromStr('');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  V := Vector3SingleFromStr('  11       22 ' + NL + ' 33    ');
  AssertFloatsEqual(11, V[0]);
  AssertFloatsEqual(22, V[1]);
  AssertFloatsEqual(33, V[2]);
end;

procedure TTestCastleVectors.TestVector4FromStr;
var
  V: TVector4Single;
begin
  try
    V := Vector4SingleFromStr('1 2 3 abc');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4SingleFromStr('1 2 3 4 5');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4SingleFromStr('1 2 3');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4SingleFromStr('');
    Fail('Above should fail with EConvertError');
  except on EConvertError do ; end;

  V := Vector4SingleFromStr('  11       22 ' + NL + ' 33    44');
  AssertFloatsEqual(11, V[0]);
  AssertFloatsEqual(22, V[1]);
  AssertFloatsEqual(33, V[2]);
  AssertFloatsEqual(44, V[3]);
end;

procedure TTestCastleVectors.TestPlaneTransform;

  function PointLiesOnPlane(const Point: TVector3Single; const Plane: TVector4Single): boolean;
  var
    PlaneDir: TVector3Single absolute Plane;
  begin
    // Writeln('point ', VectorToNiceStr(Point), ' gives ',
    //   FloatToNiceStr(VectorDotProduct(Point, PlaneDir) + Plane[3]));
    Result := Zero(VectorDotProduct(Point, PlaneDir) + Plane[3], 0.001);
  end;

  procedure DoTest(const Plane: TVector4Single; const Matrix: TMatrix4Single;
    const PointsYes: array of TVector3Single;
    const PointsNo: array of TVector3Single);
  var
    I: Integer;
    NewPlane: TVector4Single;
  begin
    NewPlane := PlaneTransform(Plane, Matrix);
    // Writeln('New plane ', VectorToNiceStr(NewPlane));
    for I := 0 to High(PointsYes) do
      AssertTrue(PointLiesOnPlane(PointsYes[I], NewPlane));
    for I := 0 to High(PointsNo) do
      AssertTrue(not PointLiesOnPlane(PointsNo[I], NewPlane));
  end;

begin
  { x = 0 plane }
  DoTest(Vector4Single(1, 0, 0, 0),
    IdentityMatrix4Single,
    [ Vector3Single(0,  10,  10),
      Vector3Single(0, -10,  10),
      Vector3Single(0,  10, -10),
      Vector3Single(0, -10, -10) ],
    [ Vector3Single( 10,  10, 123),
      Vector3Single(-10,  10, 2),
      Vector3Single( 10, -10, -3),
      Vector3Single(1, 0, 0) ]);

  { rotate x = 0 plane to make z = 0 }
  DoTest(Vector4Single(1, 0, 0, 0),
    RotationMatrixDeg(90, 0, 1, 0),
    [ Vector3Single( 10,  10, 0),
      Vector3Single(-10,  10, 0),
      Vector3Single( 10, -10, 0),
      Vector3Single(-10, -10, 0) ],
    [ Vector3Single( 10,  10, 123),
      Vector3Single(-10,  10, 2),
      Vector3Single( 10, -10, -3),
      Vector3Single(0, 0, 1) ]);

  { rotate and move x = 0 plane to make z = 10 }
  DoTest(Vector4Single(1, 0, 0, 0),
    TranslationMatrix(Single(0), 0, 10) * RotationMatrixDeg(90, 0, 1, 0),
    [ Vector3Single( 10,  10, 10),
      Vector3Single(-10,  10, 10),
      Vector3Single( 10, -10, 10),
      Vector3Single(-10, -10, 10) ],
    [ Vector3Single( 10,  10, 0),
      Vector3Single(-10,  10, 0),
      Vector3Single( 10, -10, 0),
      Vector3Single(-10, -10, 0),
      Vector3Single( 10,  10, 123),
      Vector3Single(-10,  10, 2),
      Vector3Single( 10, -10, -3),
      Vector3Single(0, 0, 1) ]);

  { rotate and move and scale x = 0 plane to make z = 100 }
  DoTest(Vector4Single(1, 0, 0, 0),
    ScalingMatrix(Vector3Single(10, 10, 10)) *
    TranslationMatrix(Single(0), 0, 10) *
    RotationMatrixDeg(90, 0, 1, 0),
    [ Vector3Single( 10,  10, 100),
      Vector3Single(-10,  10, 100),
      Vector3Single( 10, -10, 100),
      Vector3Single(-10, -10, 100) ],
    [ Vector3Single( 10,  10, 10),
      Vector3Single(-10,  10, 10),
      Vector3Single( 10, -10, 0),
      Vector3Single(-10, -10, 0),
      Vector3Single( 10,  10, 123),
      Vector3Single(-10,  10, 2),
      Vector3Single( 10, -10, -3),
      Vector3Single(0, 0, 1) ]);
end;

procedure TTestCastleVectors.TestTransformToFromCoordsMatrix;
var
  M, MInverse: TMatrix4Single;
  NewOrigin, NewX, NewY, NewZ: TVector3Single;
begin
  NewOrigin := RandomVector;
  repeat NewX := Normalized(RandomVector) until not ZeroVector(NewX);
  NewY := Normalized(AnyOrthogonalVector(NewX));
  NewZ := VectorProduct(NewX, NewY);

  M        := TransformToCoordsMatrix  (NewOrigin, NewX, NewY, NewZ);
  MInverse := TransformFromCoordsMatrix(NewOrigin, NewX, NewY, NewZ);

  try
    AssertMatricesEqual(IdentityMatrix4Single, M * MInverse, 0.01);
    AssertMatricesEqual(IdentityMatrix4Single, MInverse * M, 0.01);
  except
    Writeln('Failed for origin=', VectorToRawStr(NewOrigin),
      ' newX=', VectorToRawStr(NewX));
    raise;
  end;
end;

procedure TTestCastleVectors.Test2D;
const
  P1: TVector3Single = (1, 2, 3);
  P2: TVector3Single = (2, 5, 13);
begin
  AssertFloatsEqual(Sqr(1) + Sqr(3) + Sqr(10), PointsDistanceSqr(P1, P2), 0.01);
  AssertFloatsEqual(Sqr(3) + Sqr(10), PointsDistance2DSqr(P1, P2, 0), 0.01);
  AssertFloatsEqual(Sqr(1) + Sqr(10), PointsDistance2DSqr(P1, P2, 1), 0.01);
  AssertFloatsEqual(Sqr(1) + Sqr(3), PointsDistance2DSqr(P1, P2, 2), 0.01);
  try
    PointsDistance2DSqr(P1, P2, 3);
    Fail('Above PointsDistance2DSqr with IgnoreIndex = 3 should raise exception');
  except end;
end;

procedure TTestCastleVectors.TestApproximateScale;
const
  EqualityEpsilon = 0.0001;
begin
  AssertFloatsEqual(2, Approximate3DScale(2, 2, 2), EqualityEpsilon);
  AssertFloatsEqual(-2, Approximate3DScale(-2, -2, -2), EqualityEpsilon);
  AssertFloatsEqual(1, Approximate3DScale(1, 1, 1), EqualityEpsilon);
  AssertFloatsEqual(-1, Approximate3DScale(-1, -1, -1), EqualityEpsilon);
  AssertFloatsEqual(7/3, Approximate3DScale(1, 3, 3), EqualityEpsilon);
  AssertFloatsEqual(-7/3, Approximate3DScale(-1, -3, -3), EqualityEpsilon);
  AssertFloatsEqual(1, Approximate3DScale(-1, 1, 1), EqualityEpsilon);
end;

initialization
 RegisterTest(TTestCastleVectors);
end.
