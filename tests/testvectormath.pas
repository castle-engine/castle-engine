{
  Copyright 2004-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestVectorMath;

{ $define VECTOR_MATH_SPEED_TESTS}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, VectorMath;

type
  TTestVectorMath = class(TTestCase)
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
  end;

function RandomVector: TVector3Single;
function RandomMatrix: TMatrix4Single;
function RandomNonProjectionMatrix: TMatrix4Single;

implementation

uses KambiUtils, KambiStringUtils, KambiTimeUtils;

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

procedure TTestVectorMath.TestPlaneOdcCollision;
{ test below caught once fpc 1.0.10 bugs in inlines - so VectorMathInlines.inc
  was disabled }
var Intersection: TVector3Single;
    T: Single;
begin
 T := VectorDotProduct(Vector3Single(0, 0, 1), Vector3Single(0, 0, 6));
 if not FloatsEqual(T, 6) then
  raise Exception.CreateFmt('failed 1 : T = %g',[T]);

 Assert(TryPlaneLineIntersection(T,
   Vector4Single(0, 0, 1, 1),
   Vector3Single(2, 2, -3),
   Vector3Single(0, 0, 6) ));
 if not FloatsEqual(T, 1/3, 0.0000001) then
  raise Exception.CreateFmt('failed 2 : T = %g',[T]);

 Assert(TryPlaneSegmentDirIntersection(Intersection, T,
   Vector4Single(0, 0, 1, 1),
   Vector3Single(2, 2, -3),
   Vector3Single(0, 0, 6) ));
 Assert(VectorsEqual(Intersection, Vector3Single(2, 2, -1)));
 Assert(FloatsEqual(T, 1/3, 0.0000001));
end;

procedure WritelnSpeedTest(const s: string);
begin
 {$ifdef VECTOR_MATH_SPEED_TESTS}
 Writeln(s);
 {$endif}
end;

procedure TTestVectorMath.TestCollisions;
const TriConst: TTriangle3Single = ((2, 2, -1), (3, 3, -1), (3, 2, -1));
      TriPlaneConst: TVector4Single = (0, 0, 1, 1);
      Pos1Const: TVector3Single = (2, 2, -3);
      Pos2Const: TVector3Single = (2, 2, 3);
      Coll: TVector3Single = (2, 2, -1);
var Intersection, Intersection2: TVector3Single;
begin
 { czy dziala TryTriangleSegmentCollision na tym naszym skonstruowanym
   przykladzie ? }
 Assert( TryTriangleSegmentCollision(Intersection, TriConst, TriPlaneConst,
   Pos1Const, Pos2Const) and VectorsEqual(Intersection, Coll));
 { czy dziala tak samo gdy sam musi sobie wyliczyc TriPlane (test TrianglePlane) ? }
 Assert( TryTriangleSegmentCollision(Intersection2, TriConst, TrianglePlane(TriConst),
   Pos1Const, Pos2Const) and VectorsEqual(Intersection2, Coll));
end;

procedure TTestVectorMath.TestArea;
const
  Tri: TTriangle3Single = ((0, 0, 0), (10, 0, 0), (0, 25, 0));
  CCWPoly: array[0..4]of TVector2Single = ((5, 4), (2, 3), (4, 3), (2, 1), (6, 2));
  CWPoly: array[0..4]of TVector2Single = ((6, 2), (2, 1), (4, 3), (2, 3), (5, 4));
begin
 Assert(TriangleArea(Tri) = 10*25/2);

 Assert(Polygon2dArea(CCWPoly) = 5.5);
 Assert(Polygon2dArea(CWPoly) = 5.5);
 Assert(IsPolygon2dCCW(CCWPoly) > 0);
 Assert(IsPolygon2dCCW(CWPoly) < 0);
end;

procedure TTestVectorMath.TestPerpParallel;
var v: TVector3Single;
    i: integer;
begin
 for i := 1 to 10 do
 try
  v := RandomVector;
  Assert( VectorsPerp(AnyOrthogonalVector(v), v) );
  { I has to comment it out -- it fails too often due to floating point
    inaccuracy. }
  { Assert( VectorsParallel(VectorScale(v, Random*10), v) ); }
  Assert( VectorsPerp(ZeroVector3Single, v) );
  Assert( VectorsParallel(ZeroVector3Single, v) );
 except
  Writeln('and failed : v = ',VectorToNiceStr(v),
    ' anyPerp = ',VectorToNiceStr(AnyOrthogonalVector(v)));
  raise;
 end;

 Assert( VectorsPerp(ZeroVector3Single, ZeroVector3Single) );
 Assert( VectorsParallel(ZeroVector3Single, ZeroVector3Single) );

 Assert( VectorsPerp(UnitVector3Single[0], UnitVector3Single[1]) );
 Assert( VectorsPerp(UnitVector3Single[0], UnitVector3Single[2]) );
 Assert( VectorsPerp(UnitVector3Single[1], UnitVector3Single[2]) );
 Assert( not VectorsPerp(UnitVector3Single[0], UnitVector3Single[0]) );

 Assert( not VectorsParallel(UnitVector3Single[0], UnitVector3Single[1]) );
 Assert( not VectorsParallel(UnitVector3Single[0], UnitVector3Single[2]) );
 Assert( not VectorsParallel(UnitVector3Single[1], UnitVector3Single[2]) );
 Assert( VectorsParallel(UnitVector3Single[0], UnitVector3Single[0]) );
end;

procedure TTestVectorMath.TestPlanesIntersection;
const
  P1: TVector4Single = (-1.9935636520385742, -0.00000009909226151, 0.25691652297973633, -30.014257431030273);
  P2: TVector4Single = (-1.2131816148757935, 1.90326225890658E-008, -1.5900282859802246, 1.5900282859802246);
var Line0, LineVector: TVector3Single;
begin
 TwoPlanesIntersectionLine(P1, P2, Line0, LineVector);
 { Writeln(VectorToRawStr(Line0), ' ', VectorToRawStr(LineVector)); }
end;

procedure TTestVectorMath.TestOther;
var I1, I2, Ray0, RayVector: TVector3Double;
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

var i: integer;
    V: TVector3Single;
    Time0, Time1, Time2: Double;
begin
 { ------------------------------------------------------------
   testuj TrySimplePlaneRayIntersection przy uzyciu TryPlaneRayIntersection }
 for i := 1 to 100000 do
 begin
  Ray0 := RandomVector3Double;
  RayVector := RandomVector3Double;

  PlaneConstCoord := Random(3);
  PlaneConstVal := Random*1000 - 500;
  FillChar(Plane, SizeOf(Plane), 0);
  Plane[PlaneConstCoord] := -1;
  Plane[3] := PlaneConstVal;

  { czasami uczyn promien rownoleglym do [Simple]Plane (zeby zobaczyc
    czy sobie z tym radzi) }
  if Random(10) = 1 then
  begin
   RayVector[PlaneConstCoord] := 0;
   b1 := TrySimplePlaneRayIntersection(I1, PlaneConstCoord, PlaneConstVal, Ray0, RayVector);
   b2 := TryPlaneRayIntersection(I2, Plane, Ray0, RayVector);
   Check( (not b1) and (not b2) ,'intersect with parallel plane');
  end else
  begin
   { nie wykonuj testu jesli wylosowalimy niepoprawne dane }
   if not VectorsEqual(RayVector, Vector3Double(0, 0, 0)) then
   begin
    b1 := TrySimplePlaneRayIntersection(I1, PlaneConstCoord, PlaneConstVal, Ray0, RayVector);
    b2 := TryPlaneRayIntersection(I2, Plane, Ray0, RayVector);
    Assert( b1 = b2 , 'b1 <> b2');
    if b1 then
    begin
{     if not VectorsEqual(I1, I2) or
        not FloatsEqual(I1[PlaneConstCoord], PlaneConstVal) or
	not FloatsEqual(I2[PlaneConstCoord], PlaneConstVal) then
     begin
      t1:=(PlaneConstVal-Ray0[PlaneConstCoord]) / RayVector[PlaneConstCoord];
      t2 := -(plane[0]*Ray0[0] + plane[1]*Ray0[1] + plane[2]*Ray0[2] + plane[3])/
          VectorDotProduct(PlaneDir, RayVector);
      Writeln('I1 = ',VectorToNiceStr(I1), ' I2 = ',VectorToNiceStr(I2), nl,
        'PlaneConst Coord = ',PlaneConstCoord, ' Value = ',PlaneConstVal, nl,
	'Plane = ',VectorToNiceStr(Plane), nl,
	'Ray0 = ',VectorToNiceStr(Ray0), ' RayVector = ',VectorToNiceStr(RayVector), nl,
	FloatToNiceStr(t1), nl,
	FloatToNiceStr(t2), nl,
	VectorToNiceStr(VectorAdd(Ray0, VectorScale(RayVector, t1))), nl,
	VectorToNiceStr(VectorAdd(Ray0, VectorScale(RayVector, t2)))
      );
     end; }
     Assert( FloatsEqual(I1[PlaneConstCoord], PlaneConstVal), 'I1 not ok');
     Assert( FloatsEqual(I2[PlaneConstCoord], PlaneConstVal), 'I2 not ok');
     Assert( VectorsEqual(I1, I2) ,'I1 <> I2');
    end;
   end;
  end;
 end;

 { testuj szybkosc TrySimplePlaneRayIntersection w porownaniu z
   TryPlaneRayIntersection }
 WritelnSpeedTest('SPEED TEST 1 ----------------------------------------------');

 ProcessTimerBegin;
 for i := 1 to SPEED_TEST_1_CYCLES do ;
 Time0 := ProcessTimerEnd;
 WritelnSpeedTest(Format('Empty loop = %f',[Time0]));

 ProcessTimerBegin;
 for i := 1 to SPEED_TEST_1_CYCLES do
  TrySimplePlaneRayIntersection(I1, PlaneConstCoord, PlaneConstVal, Ray0, RayVector);
 Time1 := ProcessTimerEnd;
 WritelnSpeedTest(Format('TrySimplePlaneRayIntersection = %f',[Time1]));

 ProcessTimerBegin;
 for i := 1 to SPEED_TEST_1_CYCLES do
  TryPlaneRayIntersection(I1, Plane, Ray0, RayVector);
 Time2 := ProcessTimerEnd;
 WritelnSpeedTest(Format('TryPlaneRayIntersection = %f',[Time2]));

 {$ifdef VECTOR_MATH_SPEED_TESTS}
 { nie uzywam tutaj WritelnSpeedTest. Jesli VECTOR_MATH_SPEED_TESTS
   not defined to stale SPEED_TEST_x_CYCLES sa tak male ze nie moge
   wykonac dzielenia przez Time1-Time0 bo Time1-Time0 = 0. }
 Writeln(Format('SimplePlane is faster than Plane by %f', [(Time2-Time0)/(Time1-Time0)]));
 {$endif}

 WritelnSpeedTest('SPEED TEST 2 ----------------------------------------------');

 ProcessTimerBegin;
 for i := 1 to SPEED_TEST_2_CYCLES do ;
 Time0 := ProcessTimerEnd;
 WritelnSpeedTest(Format('Empty loop = %f',[Time0]));

 ProcessTimerBegin;
 for i := 1 to SPEED_TEST_2_CYCLES do
 begin
  V := VConst;
  VectorScaleTo1st(V, Pi);
 end;
 Time1 := ProcessTimerEnd;
 WritelnSpeedTest(Format('Using assignment + VectorScaleTo1st = %f',[Time1]));

 ProcessTimerBegin;
 for i := 1 to SPEED_TEST_2_CYCLES do
 begin
  V := VectorScale(VConst, Pi);
 end;
 Time2 := ProcessTimerEnd;
 WritelnSpeedTest(Format('Using VectorScale = %f',[Time2]));

 {$ifdef VECTOR_MATH_SPEED_TESTS}
 { nie uzywam tutaj WritelnSpeedTest. Jesli VECTOR_MATH_SPEED_TESTS
   not defined to stale SPEED_TEST_x_CYCLES sa tak male ze nie moge
   wykonac dzielenia przez Time1-Time0 bo Time1-Time0 = 0. }
 Writeln(Format('Assignment+To1st is faster by %f', [(Time2-Time0)/(Time1-Time0)]));
 {$endif}
end;

procedure TTestVectorMath.TestVectorStr;

  procedure OneTestVectorFromStr;
  var v, v2: TVector3Single;
      s: string;
  begin
   v := RandomVector;
   s := VectorToRawStr(v);
   v2 := Vector3SingleFromStr(s);
   Assert(VectorsEqual(v2, v));
  end;

  procedure OneTestByDeformat;
  var v, v2: TVector3Single;
      s: string;
  begin
   v := RandomVector;
   s := VectorToRawStr(v);
   DeFormat(s, '%.single. %.single. %.single.', [@v2[0], @v2[1], @v2[2]]);
   Assert(VectorsEqual(v2, v));
  end;

const CYCLES = SPEED_TEST_3_CYCLES;
var Time0, Time1, Time2: Double;
    i: integer;
begin
 WritelnSpeedTest('SPEED TEST VectorFromStr ------------------------------------------');
 ProcessTimerBegin;
 for i := 1 to CYCLES do ;
 Time0 := ProcessTimerEnd; WritelnSpeedTest(Format('Empty loop = %f',[Time0]));

 ProcessTimerBegin;
 for i := 1 to CYCLES do OneTestVectorFromStr;
 Time1 := ProcessTimerEnd; WritelnSpeedTest(Format('VectorFromStr = %f',[Time1]));

 ProcessTimerBegin;
 for i := 1 to CYCLES do OneTestByDeFormat;
 Time2 := ProcessTimerEnd; WritelnSpeedTest(Format('DeFormat = %f',[Time2]));

 {$ifdef VECTOR_MATH_SPEED_TESTS}
 { nie uzywam tutaj WritelnSpeedTest. Jesli VECTOR_MATH_SPEED_TESTS
   not defined to stale SPEED_TEST_x_CYCLES sa tak male ze nie moge
   wykonac dzielenia przez Time1-Time0 bo Time1-Time0 = 0. }
 Writeln(Format('VectorFromStr is faster by %f', [(Time2-Time0)/(Time1-Time0)]));
 {$endif}
end;

procedure TTestVectorMath.TestMatrixInverse;
var
  M: TMatrix4Single;
begin
  M := ScalingMatrix(Vector3Single(2, 2, 2));

{ Tests:
  Writeln(MatrixToNiceStr(M, '  '));
  Writeln(MatrixToNiceStr(ScalingMatrix(Vector3Single(0.5, 0.5, 0.5)), '  '));
  Writeln(MatrixToNiceStr(MatrixInverse(M, MatrixDeterminant(M)), '  '));
}

  Assert(MatricesEqual(
    MatrixInverse(M, MatrixDeterminant(M)),
    ScalingMatrix(Vector3Single(0.5, 0.5, 0.5)), 0.01));

  M := TranslationMatrix(Vector3Single(2, 2, 2));
  Assert(MatricesEqual(
    MatrixInverse(M, MatrixDeterminant(M)),
    TranslationMatrix(Vector3Single(-2, -2, -2)), 0.01));
end;

procedure TTestVectorMath.TestMultMatrixTranslation;
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
    Assert(MatricesEqual(M, NewM, 0.001));
  end;
end;

procedure TTestVectorMath.TestMultMatricesTranslation;
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
    Assert(MatricesEqual(M, NewM, 0.001));
    Assert(MatricesEqual(MInverse, NewMInverse, 0.001));
  end;
end;

procedure TTestVectorMath.TestIndexedPolygonNormalArea;
const
  Poly: array [0..4] of TVector3Single = ((5, 4, 0), (4, 4, 0), (2, 3, 0), (2, 1, 0), (6, 2, 0));
  CCWPolyIndex: array [0..6] of LongInt = (0, 1, 5, 2, 3, 4, 999);
  CWPolyIndex: array [0..6] of LongInt = (666, 4, 105, 3, 2, 1, 0);
begin
  Assert(VectorsEqual(
    IndexedConvexPolygonNormal(@CCWPolyIndex, High(CCWPolyIndex) + 1,
      @Poly, High(Poly) + 1, ZeroVector3Single),
    Vector3Single(0, 0, 1)));

  Assert(VectorsEqual(
    IndexedConvexPolygonNormal(@CWPolyIndex, High(CWPolyIndex) + 1,
      @Poly, High(Poly) + 1, ZeroVector3Single),
    Vector3Single(0, 0, -1)));

  Assert(FloatsEqual(
    IndexedConvexPolygonArea(@CCWPolyIndex, High(CCWPolyIndex) + 1,
      @Poly, High(Poly) + 1), 8));

  Assert(FloatsEqual(
    IndexedConvexPolygonArea(@CWPolyIndex , High(CWPolyIndex) + 1,
      @Poly, High(Poly) + 1), 8));
end;

procedure TTestVectorMath.TestSphereRayIntersection;
var
  Res: boolean;
  I: TVector3Single;
begin
  Res := TrySphereRayIntersection(I, Vector3Single(3, 0, 0), 10,
    Vector3Single(0, 0, 0), Vector3Single(1, 0, 0));
  Assert(Res);
  Assert(VectorsEqual(I, Vector3Single(13, 0, 0)));

  Res := TrySphereRayIntersection(I, Vector3Single(3, 0, 0), 10,
    Vector3Single(0, 0, 0), Vector3Single(-1, 0, 0));
  Assert(Res);
  Assert(VectorsEqual(I, Vector3Single(-7, 0, 0)));

  Res := TrySphereRayIntersection(I, Vector3Single(3, 0, 0), 10,
    Vector3Single(20, 0, 0), Vector3Single(1, 0, 0));
  Assert(not Res);

  Res := TrySphereRayIntersection(I, Vector3Single(3, 0, 0), 10,
    Vector3Single(20, 0, 0), Vector3Single(-1, 0, 0));
  Assert(Res);
  Assert(VectorsEqual(I, Vector3Single(13, 0, 0)));
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

procedure TTestVectorMath.TestMatrixMultiplication;
var
  M1, M2, M3, Result1, Result2: TMatrix4Single;
begin
  M1[0] := Vector4Single(1, 0, 0, 0);
  M1[1] := Vector4Single(0, 1, 0, 0);
  M1[2] := Vector4Single(0, 0, 1, 0);
  M1[2] := Vector4Single(-0.31, 1.26, -0.03, 1);

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
  Assert(MatricesEqual(Result1, Result2, 0.1));

  Result2 := MatrixMult(MatrixMult(M1, M2), M3);

  Result1 := M1 * M2;
  Result1 := Result1 * M3;
  Assert(MatricesEqual(Result1, Result2, 0.1));

  Result1 := M1 * M2 * M3;
  Assert(MatricesEqual(Result1, Result2, 0.1));
end;

procedure TTestVectorMath.TestMatrixTranspose;
var
  M1, M2: TMatrix3Single;
begin
  M1[0] := Vector3Single(1, 2, 3);
  M1[1] := Vector3Single(4, 5, 6);
  M1[2] := Vector3Single(7, 8, 9);

  M2[0] := Vector3Single(1, 4, 7);
  M2[1] := Vector3Single(2, 5, 8);
  M2[2] := Vector3Single(3, 6, 9);

  MatrixTransposeTo1st(M1);
  Assert(MatricesPerfectlyEqual(M1, M2));
end;

procedure TTestVectorMath.TestVector3FromStr;
var
  V: TVector3Single;
begin
  try
    V := Vector3SingleFromStr('1 2 abc');
    Assert(false, 'Should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3SingleFromStr('1 2 3 4');
    Assert(false, 'Should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3SingleFromStr('1 2');
    Assert(false, 'Should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector3SingleFromStr('');
    Assert(false, 'Should fail with EConvertError');
  except on EConvertError do ; end;

  V := Vector3SingleFromStr('  11       22 ' + NL + ' 33    ');
  Assert(FloatsEqual(V[0], 11));
  Assert(FloatsEqual(V[1], 22));
  Assert(FloatsEqual(V[2], 33));
end;

procedure TTestVectorMath.TestVector4FromStr;
var
  V: TVector4Single;
begin
  try
    V := Vector4SingleFromStr('1 2 3 abc');
    Assert(false, 'Should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4SingleFromStr('1 2 3 4 5');
    Assert(false, 'Should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4SingleFromStr('1 2 3');
    Assert(false, 'Should fail with EConvertError');
  except on EConvertError do ; end;

  try
    V := Vector4SingleFromStr('');
    Assert(false, 'Should fail with EConvertError');
  except on EConvertError do ; end;

  V := Vector4SingleFromStr('  11       22 ' + NL + ' 33    44');
  Assert(FloatsEqual(V[0], 11));
  Assert(FloatsEqual(V[1], 22));
  Assert(FloatsEqual(V[2], 33));
  Assert(FloatsEqual(V[3], 44));
end;

initialization
 RegisterTest(TTestVectorMath);
end.
