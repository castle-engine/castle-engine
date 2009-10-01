{
  Copyright 2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit TestBoxes3d;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestBoxes3d = class(TTestCase)
  published
    procedure TestIsCenteredBox3dPlaneCollision;
    procedure TestBox3dPlaneCollision;
    procedure TestIsBox3dTriangleCollision;
    procedure TestIsBox3dTriangleCollisionEpsilons;
    procedure TestBox3dTransform;
    procedure TestBox3dMaximumPlane;
    procedure TestBox3dMinimumPlane;
  end;

implementation

uses VectorMath, KambiUtils, Boxes3d, KambiStringUtils, KambiTimeUtils,
  TestVectorMath;

procedure TTestBoxes3d.TestIsCenteredBox3dPlaneCollision;
begin
  { box 10, 1, 1 with a plane that crosses 0,0,0 point always collides }
  Assert(IsCenteredBox3dPlaneCollision(
    Vector3Single(10, 1, 1), Vector4Single(0, 0, 1, 0)));
  Assert(IsCenteredBox3dPlaneCollision(
    Vector3Single(10, 1, 1), Vector4Single(0, 1, 0, 0)));
  Assert(IsCenteredBox3dPlaneCollision(
    Vector3Single(10, 1, 1), Vector4Single(1, 0, 0, 0)));
  Assert(IsCenteredBox3dPlaneCollision(
    Vector3Single(10, 1, 1), Vector4Single(123, 456, 789, 0)));

  Assert(not IsCenteredBox3dPlaneCollision(
    Vector3Single(10, 1, 1), Vector4Single(0, 0, -1, 5)));
  Assert(not IsCenteredBox3dPlaneCollision(
    Vector3Single(10, 1, 1), Vector4Single(0, -1, 0, 5)));
  Assert(IsCenteredBox3dPlaneCollision(
    Vector3Single(10, 1, 1), Vector4Single(-1, 0, 0, 5)));
end;

procedure TTestBoxes3d.TestBox3dPlaneCollision;

  procedure AssertBox3dPlaneCollision(const Box: TBox3d;
    const Plane: TVector4Single; CollisionResult: TPlaneCollision);
  begin
    Assert(Box3dPlaneCollision(Box, Plane) = CollisionResult);
    { Check by the way Box3dPlaneCollisionInside, Box3dPlaneCollisionOutside }
    Assert(Box3dPlaneCollisionInside(Box, Plane) = (CollisionResult = pcInside));
    Assert(Box3dPlaneCollisionOutside(Box, Plane) = (CollisionResult = pcOutside));
  end;

var
  Box: TBox3d;
  Plane: TVector4Single;
begin
  Box[0] := Vector3Single(-10, -1, -1);
  Box[1] := Vector3Single( 10,  1,  1);

  { box 10, 1, 1 with a plane that crosses 0,0,0 point always collides }
  AssertBox3dPlaneCollision(
    Box, Vector4Single(0, 0, 1, 0), pcIntersecting);
  AssertBox3dPlaneCollision(
    Box, Vector4Single(0, 1, 0, 0), pcIntersecting);
  AssertBox3dPlaneCollision(
    Box, Vector4Single(1, 0, 0, 0), pcIntersecting);
  AssertBox3dPlaneCollision(
    Box, Vector4Single(123, 456, 789, 0), pcIntersecting);

  { test inside/outside difference }
  AssertBox3dPlaneCollision(
    Box3d(Vector3Single(-1, -1, 10),
          Vector3Single( 1,  1, 20)),
    Vector4Single(0, 0, 1, 0), pcOutside);
  AssertBox3dPlaneCollision(
    Box3d(Vector3Single(-1, -1, -20),
          Vector3Single( 1,  1, -10)),
    Vector4Single(0, 0, 1, 0), pcInside);

  { basic test for pcNone }
  AssertBox3dPlaneCollision(EmptyBox3d,
    Vector4Single(0, 0, 1, 0), pcNone);

  AssertBox3dPlaneCollision(
    Box, Vector4Single(0, 0, -1, 5), pcOutside);
  AssertBox3dPlaneCollision(
    Box, Vector4Single(0, -1, 0, 5), pcOutside);
  AssertBox3dPlaneCollision(
    Box, Vector4Single(-1, 0, 0, 5), pcIntersecting);

  Box := Box3dTranslate(Box, Vector3Single(0, 1000, 0));

  AssertBox3dPlaneCollision(
    Box, Vector4Single(0, 0, 1, 0), pcIntersecting);
  AssertBox3dPlaneCollision(
    Box, Vector4Single(0, 1, 0, 0), pcOutside);
  AssertBox3dPlaneCollision(
    Box, Vector4Single(1, 0, 0, 0), pcIntersecting);

  Plane[0] := 0;
  Plane[1] := 0;
  Plane[2] := 1;
  Plane[3] := 1.980401039E+00;
  Box[0][0] :=  2.837333679E-01;
  Box[0][1] := -9.844776917E+01;
  Box[0][2] := -1.980401039E+00;
  Box[1][0] :=  1.283623352E+02;
  Box[1][1] :=  3.240192413E+00;
  Box[1][2] :=  3.100979996E+01;
  AssertBox3dPlaneCollision(Box, Plane, pcIntersecting);
end;

procedure TTestBoxes3d.TestIsBox3dTriangleCollision;

  procedure RandomTrianglesTest(
    const XRandomness, YRandomness, ZRandomness: Integer);
  var
    Triangle: TTriangle3Single;
    Box: TBox3d;
    V0, V1, V2: TVector3Single;
    I: Integer;
  begin
    { random triangle completely within the box }
    Box[0] := Vector3Single(10, 10, 10);
    Box[1] := Vector3Single(20, 20, 20);
    for I := 1 to 50 do
    begin
      repeat
        Triangle[0] := Vector3Single(12 + Random(8) * XRandomness, 12 + Random(8) * YRandomness, 12 + Random(8) * ZRandomness);
        Triangle[1] := Vector3Single(12 + Random(8) * XRandomness, 12 + Random(8) * YRandomness, 12 + Random(8) * ZRandomness);
        Triangle[2] := Vector3Single(12 + Random(8) * XRandomness, 12 + Random(8) * YRandomness, 12 + Random(8) * ZRandomness);
      until IsValidTriangle(Triangle);
      Assert(IsBox3dTriangleCollision(Box, Triangle));
    end;

    { random triangle completely outside the box (but chosen to collide) }
    Box[0] := Vector3Single(10, 10, 10);
    Box[1] := Vector3Single(20, 20, 20);
    for I := 1 to 50 do
    begin
      repeat
        V0 := Vector3Single(10 + Random(10) * XRandomness, 10 + Random(10) * YRandomness, 10 + Random(10) * ZRandomness);
        V1 := Vector3Single(10 + Random(10) * XRandomness, 10 + Random(10) * YRandomness, 10 + Random(10) * ZRandomness);
        V2 := VectorNegate(VectorAdd(V0, V1));
        Triangle[0] := VectorAdd(Vector3Single(15, 15, 15), V0);
        Triangle[1] := VectorAdd(Vector3Single(15, 15, 15), V1);
        Triangle[2] := VectorAdd(Vector3Single(15, 15, 15), V2);
      until IsValidTriangle(Triangle);

      Assert(IsBox3dTriangleCollision(Box, Triangle));

      VectorAddTo1st(Triangle[0], Vector3Single(100, 100, 100));
      VectorAddTo1st(Triangle[1], Vector3Single(100, 100, 100));
      VectorAddTo1st(Triangle[2], Vector3Single(100, 100, 100));
      Assert(not IsBox3dTriangleCollision(Box, Triangle));
    end;

    { random triangle with 1 point inside the box, other 2 outside }
    Box[0] := Vector3Single(10, 10, 10);
    Box[1] := Vector3Single(20, 20, 20);
    for I := 1 to 50 do
    begin
      repeat
        V0 := Vector3Single(10 + Random(10) * XRandomness, 10 + Random(10) * YRandomness, 10 + Random(10) * ZRandomness);
        V1 := Vector3Single(10 + Random(10) * XRandomness, 10 + Random(10) * YRandomness, 10 + Random(10) * ZRandomness);
        Triangle[0] := VectorAdd(Vector3Single(15, 15, 15), V0);
        Triangle[1] := VectorAdd(Vector3Single(15, 15, 15), V1);
        Triangle[2] := Vector3Single(15, 15, 15);
      until IsValidTriangle(Triangle);

      Assert(IsBox3dTriangleCollision(Box, Triangle));

      VectorAddTo1st(Triangle[0], Vector3Single(100, 100, 100));
      VectorAddTo1st(Triangle[1], Vector3Single(100, 100, 100));
      VectorAddTo1st(Triangle[2], Vector3Single(100, 100, 100));
      Assert(not IsBox3dTriangleCollision(Box, Triangle));
    end;
  end;

var
  Triangle: TTriangle3Single;
  Box: TBox3d;
begin
  Triangle[0] := Vector3Single(0, 0, 0);
  Triangle[1] := Vector3Single(10, 0, 0);
  Triangle[2] := Vector3Single(10, 10, 0);
  Box[0] := Vector3Single(-10, -1, -1);
  Box[1] := Vector3Single( 10,  1,  1);

  Assert(IsBox3dTriangleCollision(Box, Triangle));

  Box := Box3dTranslate(Box, Vector3Single(0, 0, 0.5));
  Assert(IsBox3dTriangleCollision(Box, Triangle));

  Box := Box3dTranslate(Box, Vector3Single(0, 0, 1.0));
  Assert(not IsBox3dTriangleCollision(Box, Triangle));

  RandomTrianglesTest(1, 1, 1);

  { make one coord locked, and perform the same tests as above }
  RandomTrianglesTest(0, 1, 1);
  RandomTrianglesTest(1, 0, 1);
  RandomTrianglesTest(1, 1, 0);
end;

procedure TTestBoxes3d.TestIsBox3dTriangleCollisionEpsilons;
var
  EqualityEpsilon: Single;

  { Modified version of IsCenteredBox3dPlaneCollision and IsBox3dTriangleCollision
    that use EqualityEpsilon variable here. Also, use Single precision calculations. }

  function IsCenteredBox3dPlaneCollision(
    const BoxHalfSize: TVector3Single;
    const Plane: TVector4Single): boolean;
  { Implementation of this is based on
    [http://jgt.akpeters.com/papers/AkenineMoller01/tribox.html]
    planeBoxOverlap routine, by Tomas Akenine-Möller,
    mentioned in his paper [http://jgt.akpeters.com/papers/AkenineMoller01/]
    about "Fast 3D Triangle-Box Overlap Testing", downloadable from
    [http://www.cs.lth.se/home/Tomas_Akenine_Moller/pubs/tribox.pdf].

    The idea: we need to test plane equation with only two points
    (instead of eight points, as in naive version). Think about the plane
    normal vector; imagine 8 box points projected on this vector; now
    we can find 2 box points, one that has minimal value when projected
    on normal vector, and one that has maximum value. Now you need to test
    is the plane between these two points. }
  var
    I: Integer;
    VMin, VMax: TVector3Single;
  begin
    for I := 0 to 2 do
      if Plane[I] > 0 then
      begin
        VMin[I] := -BoxHalfSize[I];
        VMax[I] :=  BoxHalfSize[I];
      end else
      begin
        VMin[I] :=  BoxHalfSize[I];
        VMax[I] := -BoxHalfSize[I];
      end;

    { If VMin is above the plane (plane equation is > 0), then VMax
      is also above, no need to test anything else. }
    if Plane[0] * VMin[0] +
       Plane[1] * VMin[1] +
       Plane[2] * VMin[2] +
       Plane[3] > EqualityEpsilon then
      Exit(false);

    { So VMin is <= plane. So if VMax is >= 0, then there's a collision. }
    Result :=  Plane[0] * VMax[0] +
               Plane[1] * VMax[1] +
               Plane[2] * VMax[2] +
               Plane[3] >= EqualityEpsilon;
  end;

  function IsBox3dTriangleCollision(
    const Box: TBox3d;
    const Triangle: TTriangle3Single): boolean;

  { Implementation of this is based on
    [http://jgt.akpeters.com/papers/AkenineMoller01/tribox.html],
    by Tomas Akenine-Möller, described
    in his paper [http://jgt.akpeters.com/papers/AkenineMoller01/]
    "Fast 3D Triangle-Box Overlap Testing", downloadable from
    [http://www.cs.lth.se/home/Tomas_Akenine_Moller/pubs/tribox.pdf].

    Use separating axis theorem to test overlap between triangle and box
    need to test for overlap in these directions:
    1) the (x,y,z)-directions
    2) normal of the triangle
    3) crossproduct(edge from tri, (x,y,z)-direction)
       this gives 3x3=9 more tests
  }
  var
    TriangleMoved: TTriangle3Single;
    BoxHalfSize: TVector3Single;

    { ======================== X-tests ======================== }
    function AXISTEST_X01(const a, b, fa, fb: Single): boolean;
    var
      p0, p2, rad, min, max: Single;
    begin
      p0 := a * TriangleMoved[0][1] - b * TriangleMoved[0][2];
      p2 := a * TriangleMoved[2][1] - b * TriangleMoved[2][2];
      if p0<p2 then begin min := p0; max := p2; end else
                    begin min := p2; max := p0; end;
      rad := fa * BoxHalfSize[1] + fb * BoxHalfSize[2];
      Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
    end;

    function AXISTEST_X2(const a, b, fa, fb: Single): boolean;
    var
      p0, p1, rad, min, max: Single;
    begin
      p0 := a * TriangleMoved[0][1] - b * TriangleMoved[0][2];
      p1 := a * TriangleMoved[1][1] - b * TriangleMoved[1][2];
      if p0<p1 then begin min := p0; max := p1; end else
                    begin min := p1; max := p0; end;
      rad := fa * BoxHalfSize[1] + fb * BoxHalfSize[2];
      Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
    end;

    { ======================== Y-tests ======================== }
    function AXISTEST_Y02(const a, b, fa, fb: Single): boolean;
    var
      p0, p2, rad, min, max: Single;
    begin
      p0 := -a * TriangleMoved[0][0] + b * TriangleMoved[0][2];
      p2 := -a * TriangleMoved[2][0] + b * TriangleMoved[2][2];
      if p0<p2 then begin min := p0; max := p2; end else
                    begin min := p2; max := p0; end;
      rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[2];
      Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
    end;

    function AXISTEST_Y1(const a, b, fa, fb: Single): boolean;
    var
      p0, p1, rad, min, max: Single;
    begin
      p0 := -a * TriangleMoved[0][0] + b * TriangleMoved[0][2];
      p1 := -a * TriangleMoved[1][0] + b * TriangleMoved[1][2];
      if p0<p1 then begin min := p0; max := p1; end else
                    begin min := p1; max := p0; end;
      rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[2];
      Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
    end;

    { ======================== Z-tests ======================== }
    function AXISTEST_Z12(const a, b, fa, fb: Single): boolean;
    var
      p1, p2, rad, min, max: Single;
    begin
      p1 := a * TriangleMoved[1][0] - b * TriangleMoved[1][1];
      p2 := a * TriangleMoved[2][0] - b * TriangleMoved[2][1];
      if p2<p1 then begin min := p2; max := p1; end else
                    begin min := p1; max := p2; end;
      rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[1];
      Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
    end;

    function AXISTEST_Z0(const a, b, fa, fb: Single): boolean;
    var
      p0, p1, rad, min, max: Single;
    begin
      p0 := a * TriangleMoved[0][0] - b * TriangleMoved[0][1];
      p1 := a * TriangleMoved[1][0] - b * TriangleMoved[1][1];
      if p0<p1 then begin min := p0; max := p1; end else
                    begin min := p1; max := p0; end;
      rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[1];
      Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
    end;

    procedure FindMinMax(const x0, x1, x2: Single; out min, max: Single);
    begin
      min := x0;
      max := x0;
      if   x1 < min then min := x1 else
        if x1 > max then max := x1;
      if   x2 < min then min := x2 else
        if x2 > max then max := x2;
    end;

  var
    BoxCenter: TVector3Single;
    I: Integer;
    TriangleEdges: array [0..2] of TVector3Single;
    EdgeAbs: TVector3Single;
    min, max: Single;
    Plane: TVector4Single;
    PlaneDir: TVector3Single absolute Plane;
  begin
    if IsEmptyBox3d(Box) then
      Exit(false);

    { calculate BoxCenter and BoxHalfSize }
    for I := 0 to 2 do
    begin
      BoxCenter[I] := (Box[0, I] + Box[1, I]) / 2;
      BoxHalfSize[I] := (Box[1, I] - Box[0, I]) / 2;
    end;

    { calculate TriangleMoved (Triangle shifted by -BoxCenter,
      so that we can treat the BoxHalfSize as centered around origin) }
    TriangleMoved[0] := VectorSubtract(Triangle[0], BoxCenter);
    TriangleMoved[1] := VectorSubtract(Triangle[1], BoxCenter);
    TriangleMoved[2] := VectorSubtract(Triangle[2], BoxCenter);

    { calculate TriangleMoved edges }
    TriangleEdges[0] := VectorSubtract(TriangleMoved[1], TriangleMoved[0]);
    TriangleEdges[1] := VectorSubtract(TriangleMoved[2], TriangleMoved[1]);
    TriangleEdges[2] := VectorSubtract(TriangleMoved[0], TriangleMoved[2]);

    { tests 3) }
    EdgeAbs[0] := Abs(TriangleEdges[0][0]);
    EdgeAbs[1] := Abs(TriangleEdges[0][1]);
    EdgeAbs[2] := Abs(TriangleEdges[0][2]);
    if AXISTEST_X01(TriangleEdges[0][2], TriangleEdges[0][1], EdgeAbs[2], EdgeAbs[1]) then Exit(false);
    if AXISTEST_Y02(TriangleEdges[0][2], TriangleEdges[0][0], EdgeAbs[2], EdgeAbs[0]) then Exit(false);
    if AXISTEST_Z12(TriangleEdges[0][1], TriangleEdges[0][0], EdgeAbs[1], EdgeAbs[0]) then Exit(false);

    EdgeAbs[0] := Abs(TriangleEdges[1][0]);
    EdgeAbs[1] := Abs(TriangleEdges[1][1]);
    EdgeAbs[2] := Abs(TriangleEdges[1][2]);
    if AXISTEST_X01(TriangleEdges[1][2], TriangleEdges[1][1], EdgeAbs[2], EdgeAbs[1]) then Exit(false);
    if AXISTEST_Y02(TriangleEdges[1][2], TriangleEdges[1][0], EdgeAbs[2], EdgeAbs[0]) then Exit(false);
    if AXISTEST_Z0 (TriangleEdges[1][1], TriangleEdges[1][0], EdgeAbs[1], EdgeAbs[0]) then Exit(false);

    EdgeAbs[0] := Abs(TriangleEdges[2][0]);
    EdgeAbs[1] := Abs(TriangleEdges[2][1]);
    EdgeAbs[2] := Abs(TriangleEdges[2][2]);
    if AXISTEST_X2 (TriangleEdges[2][2], TriangleEdges[2][1], EdgeAbs[2], EdgeAbs[1]) then Exit(false);
    if AXISTEST_Y1 (TriangleEdges[2][2], TriangleEdges[2][0], EdgeAbs[2], EdgeAbs[0]) then Exit(false);
    if AXISTEST_Z12(TriangleEdges[2][1], TriangleEdges[2][0], EdgeAbs[1], EdgeAbs[0]) then Exit(false);

    { tests 1)
      first test overlap in the (x,y,z)-directions
      find min, max of the triangle each direction, and test for overlap in
      that direction -- this is equivalent to testing a minimal AABB around
      the triangle against the AABB }

    { test in X-direction }
    FindMinMax(TriangleMoved[0][0], TriangleMoved[1][0], TriangleMoved[2][0], min, max);
    if (min >  boxhalfsize[0] + EqualityEpsilon) or
       (max < -boxhalfsize[0] - EqualityEpsilon) then Exit(false);

    { test in Y-direction }
    FindMinMax(TriangleMoved[0][1], TriangleMoved[1][1], TriangleMoved[2][1], min, max);
    if (min >  boxhalfsize[1] + EqualityEpsilon) or
       (max < -boxhalfsize[1] - EqualityEpsilon) then Exit(false);

    { test in Z-direction }
    FindMinMax(TriangleMoved[0][2], TriangleMoved[1][2], TriangleMoved[2][2], min, max);
    if (min >  boxhalfsize[2] + EqualityEpsilon) or
       (max < -boxhalfsize[2] - EqualityEpsilon) then Exit(false);

    { tests 2)
      test if the box intersects the plane of the triangle
      compute plane equation of triangle: normal*x+d=0 }
    PlaneDir := VectorProduct(TriangleEdges[0], TriangleEdges[1]);
    Plane[3] := -VectorDotProduct(PlaneDir, TriangleMoved[0]);
    if not IsCenteredBox3dPlaneCollision(BoxHalfSize, Plane) then
      Exit(false);

    Result := true; { box and triangle overlaps }
  end;

var
  Triangle: TTriangle3Single;
  Box: TBox3d;

  procedure DoTest(const TestName: string; CorrectResult: boolean);
  begin
    { These writelns were used to experimentally check that 1e-4 is still too small,
      and 1e-3 is enough for these tests... That's assuming that we have
      IsBox3dTriangleCollision implementation based on Single type. }
    {Write(TestName, ': ');
    EqualityEpsilon := 1e-3;
    Write(IsBox3dTriangleCollision(Box, Triangle), ' ');
    EqualityEpsilon := 1e-6;
    Write(IsBox3dTriangleCollision(Box, Triangle), ' ');
    Write(CorrectResult, ' ');
    Write(Boxes3d.IsBox3dTriangleCollision(Box, Triangle), ' ');
    Writeln;}
    Assert(Boxes3d.IsBox3dTriangleCollision(Box, Triangle) = CorrectResult);
  end;

const
  A = 1.980401039123535;
var
  OldBox3dPlaneCollisionEqualityEpsilon: Double;
begin
  EqualityEpsilon := 1e-5;

  Box[0][0] := -7.721179485321045;
  Box[0][1] := -3.115305423736572;
  Box[0][2] := 26.886024475097656;
  Box[1][0] := 0.283733367919922;
  Box[1][1] := 3.240192413330078;
  Box[1][2] := 28.947912216186523;
  Triangle[0][0] := -7.759810924530029;
  Triangle[0][1] := 6.43093835606123E-006;
  Triangle[0][2] := 28.172618865966797;
  Triangle[1][0] := -7.610710620880127;
  Triangle[1][1] := -1.513858914375305;
  Triangle[1][2] := 31.17262077331543;
  Triangle[2][0] := -7.759810924530029;
  Triangle[2][1] := 6.43093835606123E-006;
  Triangle[2][2] := 31.17262077331543;
  (* DoTest('1', false
    { Not sure what the result should be... ? But it sure depends on the epsilon used in
      IsBox3dTriangleCollision. Test on Double values shows that this should be false.
    }); *)

  Box[0][0] := 0.283733367919922;
  Box[0][1] := -98.447769165039062;
  Box[0][2] := -A;
  Box[1][0] :=  128.36233520507812;
  Box[1][1] := 3.240192413330078;
  Box[1][2] := 31.009799957275391;
  Triangle[0][0] := 25.288267135620117;
  Triangle[0][1] := 8.671939849853516;
  Triangle[0][2] := -A;
  Triangle[1][0] := 16.125827789306641;
  Triangle[1][1] := -21.297039031982422;
  Triangle[1][2] := -A;
  Triangle[2][0] := 19.586576461791992;
  Triangle[2][1] := -26.554182052612305;
  Triangle[2][2] := -A;

  {$ifdef CPU64}
  { Looks like for this test, even larger Box3dPlaneCollisionEqualityEpsilon
    is needed under x86_64 (tested on Linux with fpc 2.2.4 and trunk on 2009-08-21). }
  OldBox3dPlaneCollisionEqualityEpsilon := Box3dPlaneCollisionEqualityEpsilon;
  Box3dPlaneCollisionEqualityEpsilon := 1e-3;
  {$endif}

  DoTest('2', true);

  {$ifdef CPU64}
  Box3dPlaneCollisionEqualityEpsilon := OldBox3dPlaneCollisionEqualityEpsilon;
  {$endif}

  Box[0][0] := 0.283733367919922;
  Box[0][1] := -47.603790283203125;
  Box[0][2] := -A;
  Box[1][0] := 64.323036193847656;
  Box[1][1] := 3.240192413330078;
  Box[1][2] := 14.514699935913086;
  { Triangle as before }
  DoTest('3', true);

  Box[0][0] := 0.283733367919922;
  Box[0][1] := -47.603790283203125;
  Box[0][2] := -A;
  Box[1][0] := 32.303382873535156;
  Box[1][1] := -22.181798934936523;
  Box[1][2] := 6.267149448394775;
  { Triangle as before }
  DoTest('4', true);

  Box[0][0] := 16.293558120727539;
  Box[0][1] := 3.240192413330078;
  Box[0][2] := -A;
  Box[1][0] := 24.298469543457031;
  Box[1][1] := 9.59568977355957;
  Box[1][2] := 0.081486582756042;
  Triangle[0][0] := 25.288267135620117;
  Triangle[0][1] := 8.671939849853516;
  Triangle[0][2] := -A;
  Triangle[1][0] := -0.731123030185699;
  Triangle[1][1] := 32.452774047851562;
  Triangle[1][2] := -A;
  Triangle[2][0] := -0.382562607526779;
  Triangle[2][1] := 26.646867752075195;
  Triangle[2][2] := -A;
  DoTest('5', true);

  Box[0][0] := -17.727319717407227;
  Box[0][1] := 4.829066753387451;
  Box[0][2] := 5.751677513122559;
  Box[1][0] := -15.726092338562012;
  Box[1][1] := 6.417941093444824;
  Box[1][2] := 6.267149448394775;
  Triangle[0][0] := -6.18981409072876;
  Triangle[0][1] := 2.234785079956055;
  Triangle[0][2] := 29.618535995483398;
  Triangle[1][0] := -20.651203155517578;
  Triangle[1][1] := 5.486495018005371;
  Triangle[1][2] := -0.132393002510071;
  Triangle[2][0] := -6.149000644683838;
  Triangle[2][1] := 2.083860397338867;
  Triangle[2][2] := 29.618535995483398;
  (* DoTest('6', false
    { Not sure what the result should be... ? But it sure depends on the epsilon used in
      IsBox3dTriangleCollision. Test on Double values shows that this should be false.
      }); *)
end;

procedure TTestBoxes3d.TestBox3dTransform;
{ Test Box3dTransform for correctness and speed.
  Compare with Slower implementation, that should be slower
  (on non-projection matrices) but give the same results. }

  function Slower(const Box: TBox3d; const Matrix: TMatrix4Single): TBox3d;
  var
    BoxPoints: array [0..7] of TVector3Single;
    i: integer;
  begin
    if IsEmptyBox3d(Box) then
      Exit(EmptyBox3d);

    Box3dGetAllPoints(@boxpoints, Box);
    for i := 0 to 7 do boxpoints[i] := MatrixMultPoint(Matrix, boxpoints[i]);

    { Non-optimized version:
        Result := CalculateBoundingBox(@boxpoints, 8, 0);

      But it turns out that the code below, that does essentially the same
      thing as CalculateBoundingBox implementation, works noticeably faster.
      This is noticeable on "The Castle" with many creatures: then a considerable
      time is spend inside TCreature.BoundingBox, that must calculate
      transformed bounding boxes.
    }

    Result[0] := BoxPoints[0];
    Result[1] := BoxPoints[0];
    for I := 1 to High(BoxPoints) do
    begin
      if BoxPoints[I, 0] < Result[0, 0] then Result[0, 0] := BoxPoints[I, 0];
      if BoxPoints[I, 1] < Result[0, 1] then Result[0, 1] := BoxPoints[I, 1];
      if BoxPoints[I, 2] < Result[0, 2] then Result[0, 2] := BoxPoints[I, 2];
      if BoxPoints[I, 0] > Result[1, 0] then Result[1, 0] := BoxPoints[I, 0];
      if BoxPoints[I, 1] > Result[1, 1] then Result[1, 1] := BoxPoints[I, 1];
      if BoxPoints[I, 2] > Result[1, 2] then Result[1, 2] := BoxPoints[I, 2];
    end;
  end;

  function RandomBox: TBox3d;
  var
    I: Integer;
  begin
    for I := 0 to 2 do
    begin
      Result[0][I] := 50 - Random * 100;
      Result[1][I] := 50 - Random * 100;
      OrderUp(Result[0][I], Result[1][I]);
    end;
  end;

  procedure AssertBoxesEqual(const Box1, Box2: TBox3d);
  var
    I: Integer;
  begin
    try
      for I := 0 to 2 do
      begin
        Assert(FloatsEqual(Box1[0][I], Box2[0][I], 0.01));
        Assert(FloatsEqual(Box1[1][I], Box2[1][I], 0.01));
      end;
    except
      writeln('AssertBoxesEqual failed: ',
        Box3dToNiceStr(Box1), ' ', Box3dToNiceStr(Box2));
      raise;
    end;
  end;

var
  Box: TBox3d;
  I: Integer;
  Matrix: TMatrix4Single;
begin
  for I := 0 to 1000 do
  begin
    Box := RandomBox;
    Matrix := RandomMatrix;
    AssertBoxesEqual(Slower(Box, Matrix), Box3dTransform(Box, Matrix));
  end;

  for I := 0 to 1000 do
  begin
    Box := RandomBox;
    Matrix := RandomNonProjectionMatrix;
    AssertBoxesEqual(Slower(Box, Matrix), Box3dTransform(Box, Matrix));
  end;

  { $define BOX3D_TRANSFORM_SPEED_TEST}
  {$ifdef BOX3D_TRANSFORM_SPEED_TEST}
  Writeln('On possibly projection matrix:');

  Box := RandomBox;
  Matrix := RandomMatrix;

  ProcessTimerBegin;
  for I := 0 to 1000000 do Slower(Box, Matrix);
  Writeln(Format('Slower: %f', [ProcessTimerEnd]));

  ProcessTimerBegin;
  for I := 0 to 1000000 do Box3dTransform(Box, Matrix);
  Writeln(Format('Box3dTransform: %f', [ProcessTimerEnd]));

  Writeln('On non-projection matrix:');

  Box := RandomBox;
  Matrix := RandomNonProjectionMatrix;

  ProcessTimerBegin;
  for I := 0 to 1000000 do Slower(Box, Matrix);
  Writeln(Format('Slower: %f', [ProcessTimerEnd]));

  ProcessTimerBegin;
  for I := 0 to 1000000 do Box3dTransform(Box, Matrix);
  Writeln(Format('Box3dTransform: %f', [ProcessTimerEnd]));
  {$endif BOX3D_TRANSFORM_SPEED_TEST}
end;

procedure TTestBoxes3d.TestBox3dMaximumPlane;
begin
  try
    Box3dMaximumPlane(EmptyBox3d, Vector3Single(1, 1, 1));
  except
    on E: EBox3dEmpty do { Ok };
  end;

  Assert(VectorsEqual(Box3dMaximumPlane(Box3d(
    Vector3Single(2, 3, 4),
    Vector3Single(50, 60, 70)), Vector3Single(-1, 0, 0)),
    Vector4Single(-1, 0, 0, 2)));

  Assert(VectorsEqual(Box3dMaximumPlane(Box3d(
    Vector3Single(2, 3, 4),
    Vector3Single(50, 60, 70)), Vector3Single(0, 0, -1)),
    Vector4Single(0, 0, -1, 4)));

  Assert(VectorsEqual(Box3dMaximumPlane(Box3d(
    Vector3Single(2, 3, 4),
    Vector3Single(50, 60, 70)), Vector3Single(1, 1, 1)),
    Vector4Single(1, 1, 1,
      { 50 + 60 + 70 + Result[3] = 0 }
      - 50 - 60 - 70
    )));
end;

procedure TTestBoxes3d.TestBox3dMinimumPlane;
begin
  try
    Box3dMinimumPlane(EmptyBox3d, Vector3Single(1, 1, 1));
  except
    on E: EBox3dEmpty do { Ok };
  end;

  Assert(VectorsEqual(Box3dMinimumPlane(Box3d(
    Vector3Single(2, 3, 4),
    Vector3Single(50, 60, 70)), Vector3Single(1, 0, 0)),
    Vector4Single(1, 0, 0, -2)));

  Assert(VectorsEqual(Box3dMinimumPlane(Box3d(
    Vector3Single(2, 3, 4),
    Vector3Single(50, 60, 70)), Vector3Single(0, 0, 1)),
    Vector4Single(0, 0, 1, -4)));

  Assert(VectorsEqual(Box3dMinimumPlane(Box3d(
    Vector3Single(2, 3, 4),
    Vector3Single(50, 60, 70)), Vector3Single(1, 1, 1)),
    Vector4Single(1, 1, 1,
      { 2 + 3 + 4 + Result[3] = 0 }
      - 2 - 3 - 4
    )));
end;

initialization
  RegisterTest(TTestBoxes3d);
end.
