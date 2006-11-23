{
  Copyright 2004-2005 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit TestVectorMath;

{ $define VECTOR_MATH_SPEED_TESTS}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVectorMath = class(TTestCase)
  published
    procedure TestPlaneOdcCollision;
    procedure TestCollisions;
    procedure TestArea;
    procedure TestPerpParallel;
    procedure TestPlanesIntersection;
    procedure TestFrustum;
    procedure TestOther;
    procedure TestVectorStr;
  end;

implementation

uses VectorMath, KambiUtils, Boxes3d, KambiStringUtils;

{$I vectormathinlines.inc}

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
{ test below catched once fpc 1.0.10 bugs in inlines - so VectorMathInlines.inc
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
 if not FloatsEqual(T, 1/3) then
  raise Exception.CreateFmt('failed 2 : T = %g',[T]);

 Assert(TryPlaneSegmentDirIntersection(Intersection, T,
   Vector4Single(0, 0, 1, 1),
   Vector3Single(2, 2, -3),
   Vector3Single(0, 0, 6) ));
 Assert(VectorsEqual(Intersection, Vector3Single(2, 2, -1)));
 Assert(FloatsEqual(T, 1/3));
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

  function RandomVector: TVector3Single;
  begin
   result[0] := Random;
   result[1] := Random;
   result[2] := Random;
  end;

var v: TVector3Single;
    i: integer;
begin
 for i := 1 to 10 do
 try
  v := RandomVector;
  Assert( VectorsPerp(AnyPerpVector(v), v) );
  { I has to comment it out -- it fails too often due to floating point
    inaccuracy. }
  { Assert( VectorsParallel(VectorScale(v, Random*10), v) ); }
  Assert( VectorsPerp(ZeroVector3Single, v) );
  Assert( VectorsParallel(ZeroVector3Single, v) );
 except
  Writeln('and failed : v = ',VectorToNiceStr(v),
    ' anyPerp = ',VectorToNiceStr(AnyPerpVector(v)));
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

procedure TTestVectorMath.TestFrustum;

  procedure AssertFrustumSphereCollisionPossible(const Frustum: TFrustum;
    const SphereCenter: TVector3Single; const SphereRadiusSqt: Single;
    const GoodResult: TFrustumCollisionPossible);
  begin
   Assert( FrustumSphereCollisionPossible(Frustum, SphereCenter,
     SphereRadiusSqt) = GoodResult);

   Assert( FrustumSphereCollisionPossibleSimple(Frustum, SphereCenter,
       SphereRadiusSqt) = (GoodResult <> fcNoCollision) );
  end;

  procedure AssertFrustumBox3dCollisionPossible(const Frustum: TFrustum;
    const Box3d: TBox3d; const GoodResult: TFrustumCollisionPossible);
  begin
   Assert( FrustumBox3dCollisionPossible(Frustum, Box3d) = GoodResult);

   Assert( FrustumBox3dCollisionPossibleSimple(Frustum, Box3d) =
     (GoodResult <> fcNoCollision) );
  end;

var Frustum: TFrustum;
begin
 { Calculate testing frustum }
 CalculateFrustum(Frustum,
   PerspectiveProjMatrixDeg(60, 1, 10, 100),
   LookDirMatrix(
     Vector3Single(10, 10, 10) { eye position },
     Vector3Single(1, 0, 0) { look direction },
     vector3Single(0, 0, 1) { up vector } ));

 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(0, 0, 0), 81,
   fcNoCollision);
 { This is between camera pos and near plane }
 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(0, 0, 0), 200,
   fcNoCollision);
 { This should collide with frustum, as it crosses near plane }
 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(0, 0, 0), 420,
   fcSomeCollisionPossible);
 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(50, 10, 10), 1,
   fcInsideFrustum);
 { This sphere intersects near plane }
 AssertFrustumSphereCollisionPossible(Frustum, Vector3Single(20, 10, 10), 1,
   fcSomeCollisionPossible);

 AssertFrustumBox3dCollisionPossible(Frustum,
   Box3d(Vector3Single(-1, -1, -1), Vector3Single(9, 9, 9)),
   fcNoCollision);
 AssertFrustumBox3dCollisionPossible(Frustum,
   Box3d(Vector3Single(50, 10, 10), Vector3Single(51, 11, 11)),
   fcInsideFrustum);
 AssertFrustumBox3dCollisionPossible(Frustum,
   Box3d(Vector3Single(19, 10, 10), Vector3Single(21, 11, 11)),
   fcSomeCollisionPossible);
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

  function RandomVector: TVector3Single;
  begin
   result[0] := Random*1000;
   result[1] := Random*1000;
   result[2] := Random*1000;
  end;

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

initialization
 RegisterTest(TTestVectorMath);
end.
