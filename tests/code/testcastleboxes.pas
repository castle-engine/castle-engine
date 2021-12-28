// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestCastleBoxes" -*-
{
  Copyright 2007-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleBoxes unit. }
unit TestCastleBoxes;

interface

uses
  Classes, SysUtils{$ifndef CASTLE_TESTER}, FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}, CastleTester{$endif};

type
  TTestCastleBoxes = class(TCastleTestCase)
  published
    procedure TestIsCenteredBox3DPlaneCollision;
    procedure TestBox3DPlaneCollision;
    procedure TestIsBox3DTriangleCollision;
    procedure TestIsBox3DTriangleCollisionEpsilons;
    procedure TestBox3DTransform;
    procedure TestBox3DMaximumPlane;
    procedure TestBox3DMinimumPlane;
    procedure TestBox3DPointDistance;
    procedure Test2D;
  end;

implementation

uses CastleVectors, CastleUtils, CastleBoxes, CastleStringUtils, CastleTimeUtils,
  TestCastleVectors, CastleTriangles;

procedure TTestCastleBoxes.TestIsCenteredBox3DPlaneCollision;
begin
  { box 10, 1, 1 with a plane that crosses 0,0,0 point always collides }
  AssertTrue(IsCenteredBox3DPlaneCollision(
    Vector3(10, 1, 1), Vector4(0, 0, 1, 0)));
  AssertTrue(IsCenteredBox3DPlaneCollision(
    Vector3(10, 1, 1), Vector4(0, 1, 0, 0)));
  AssertTrue(IsCenteredBox3DPlaneCollision(
    Vector3(10, 1, 1), Vector4(1, 0, 0, 0)));
  AssertTrue(IsCenteredBox3DPlaneCollision(
    Vector3(10, 1, 1), Vector4(123, 456, 789, 0)));

  AssertTrue(not IsCenteredBox3DPlaneCollision(
    Vector3(10, 1, 1), Vector4(0, 0, -1, 5)));
  AssertTrue(not IsCenteredBox3DPlaneCollision(
    Vector3(10, 1, 1), Vector4(0, -1, 0, 5)));
  AssertTrue(IsCenteredBox3DPlaneCollision(
    Vector3(10, 1, 1), Vector4(-1, 0, 0, 5)));
end;

procedure TTestCastleBoxes.TestBox3DPlaneCollision;

  procedure AssertBox3DPlaneCollision(const Box: TBox3D;
    const Plane: TVector4; CollisionResult: TPlaneCollision);
  begin
    AssertTrue(Box.PlaneCollision(Plane) = CollisionResult);
    { Check by the way Box3DPlaneCollisionInside, Box3DPlaneCollisionOutside }
    AssertTrue(Box.PlaneCollisionInside(Plane) = (CollisionResult = pcInside));
    AssertTrue(Box.PlaneCollisionOutside(Plane) = (CollisionResult = pcOutside));
  end;

var
  Box: TBox3D;
  Plane: TVector4;
begin
  Box.Data[0] := Vector3(-10, -1, -1);
  Box.Data[1] := Vector3( 10,  1,  1);

  { box 10, 1, 1 with a plane that crosses 0,0,0 point always collides }
  AssertBox3DPlaneCollision(
    Box, Vector4(0, 0, 1, 0), pcIntersecting);
  AssertBox3DPlaneCollision(
    Box, Vector4(0, 1, 0, 0), pcIntersecting);
  AssertBox3DPlaneCollision(
    Box, Vector4(1, 0, 0, 0), pcIntersecting);
  AssertBox3DPlaneCollision(
    Box, Vector4(123, 456, 789, 0), pcIntersecting);

  { test inside/outside difference }
  AssertBox3DPlaneCollision(
    Box3D(Vector3(-1, -1, 10),
          Vector3( 1,  1, 20)),
    Vector4(0, 0, 1, 0), pcOutside);
  AssertBox3DPlaneCollision(
    Box3D(Vector3(-1, -1, -20),
          Vector3( 1,  1, -10)),
    Vector4(0, 0, 1, 0), pcInside);

  { basic test for pcNone }
  AssertBox3DPlaneCollision(TBox3D.Empty,
    Vector4(0, 0, 1, 0), pcNone);

  AssertBox3DPlaneCollision(
    Box, Vector4(0, 0, -1, 5), pcOutside);
  AssertBox3DPlaneCollision(
    Box, Vector4(0, -1, 0, 5), pcOutside);
  AssertBox3DPlaneCollision(
    Box, Vector4(-1, 0, 0, 5), pcIntersecting);

  Box := Box.Translate(Vector3(0, 1000, 0));

  AssertBox3DPlaneCollision(
    Box, Vector4(0, 0, 1, 0), pcIntersecting);
  AssertBox3DPlaneCollision(
    Box, Vector4(0, 1, 0, 0), pcOutside);
  AssertBox3DPlaneCollision(
    Box, Vector4(1, 0, 0, 0), pcIntersecting);

  Plane[0] := 0;
  Plane[1] := 0;
  Plane[2] := 1;
  Plane[3] := 1.980401039E+00;
  Box.Data[0].Data[0] :=  2.837333679E-01;
  Box.Data[0].Data[1] := -9.844776917E+01;
  Box.Data[0].Data[2] := -1.980401039E+00;
  Box.Data[1].Data[0] :=  1.283623352E+02;
  Box.Data[1].Data[1] :=  3.240192413E+00;
  Box.Data[1].Data[2] :=  3.100979996E+01;
  AssertBox3DPlaneCollision(Box, Plane, pcIntersecting);
end;

procedure TTestCastleBoxes.TestIsBox3DTriangleCollision;

  procedure RandomTrianglesTest(
    const XRandomness, YRandomness, ZRandomness: Integer);
  var
    Triangle: TTriangle3;
    Box: TBox3D;
    V0, V1, V2: TVector3;
    I: Integer;
  begin
    { random triangle completely within the box }
    Box.Data[0] := Vector3(10, 10, 10);
    Box.Data[1] := Vector3(20, 20, 20);
    for I := 1 to 50 do
    begin
      repeat
        Triangle.Data[0] := Vector3(12 + Random(8) * XRandomness, 12 + Random(8) * YRandomness, 12 + Random(8) * ZRandomness);
        Triangle.Data[1] := Vector3(12 + Random(8) * XRandomness, 12 + Random(8) * YRandomness, 12 + Random(8) * ZRandomness);
        Triangle.Data[2] := Vector3(12 + Random(8) * XRandomness, 12 + Random(8) * YRandomness, 12 + Random(8) * ZRandomness);
      until Triangle.IsValid;
      AssertTrue(Box.IsTriangleCollision(Triangle));
    end;

    { random triangle completely outside the box (but chosen to collide) }
    Box.Data[0] := Vector3(10, 10, 10);
    Box.Data[1] := Vector3(20, 20, 20);
    for I := 1 to 50 do
    begin
      repeat
        V0 := Vector3(10 + Random(10) * XRandomness, 10 + Random(10) * YRandomness, 10 + Random(10) * ZRandomness);
        V1 := Vector3(10 + Random(10) * XRandomness, 10 + Random(10) * YRandomness, 10 + Random(10) * ZRandomness);
        V2 := -(V0 + V1);
        Triangle.Data[0] := Vector3(15, 15, 15) + V0;
        Triangle.Data[1] := Vector3(15, 15, 15) + V1;
        Triangle.Data[2] := Vector3(15, 15, 15) + V2;
      until Triangle.IsValid;

      AssertTrue(Box.IsTriangleCollision(Triangle));

      Triangle.Data[0] := Triangle.Data[0] + Vector3(100, 100, 100);
      Triangle.Data[1] := Triangle.Data[1] + Vector3(100, 100, 100);
      Triangle.Data[2] := Triangle.Data[2] + Vector3(100, 100, 100);
      AssertTrue(not Box.IsTriangleCollision(Triangle));
    end;

    { random triangle with 1 point inside the box, other 2 outside }
    Box.Data[0] := Vector3(10, 10, 10);
    Box.Data[1] := Vector3(20, 20, 20);
    for I := 1 to 50 do
    begin
      repeat
        V0 := Vector3(10 + Random(10) * XRandomness, 10 + Random(10) * YRandomness, 10 + Random(10) * ZRandomness);
        V1 := Vector3(10 + Random(10) * XRandomness, 10 + Random(10) * YRandomness, 10 + Random(10) * ZRandomness);
        Triangle.Data[0] := Vector3(15, 15, 15) + V0;
        Triangle.Data[1] := Vector3(15, 15, 15) + V1;
        Triangle.Data[2] := Vector3(15, 15, 15);
      until Triangle.IsValid;

      AssertTrue(Box.IsTriangleCollision(Triangle));

      Triangle.Data[0] := Triangle.Data[0] + Vector3(100, 100, 100);
      Triangle.Data[1] := Triangle.Data[1] + Vector3(100, 100, 100);
      Triangle.Data[2] := Triangle.Data[2] + Vector3(100, 100, 100);
      AssertTrue(not Box.IsTriangleCollision(Triangle));
    end;
  end;

var
  Triangle: TTriangle3;
  Box: TBox3D;
begin
  Triangle.Data[0] := Vector3(0, 0, 0);
  Triangle.Data[1] := Vector3(10, 0, 0);
  Triangle.Data[2] := Vector3(10, 10, 0);
  Box.Data[0] := Vector3(-10, -1, -1);
  Box.Data[1] := Vector3( 10,  1,  1);

  AssertTrue(Box.IsTriangleCollision(Triangle));

  Box := Box.Translate(Vector3(0, 0, 0.5));
  AssertTrue(Box.IsTriangleCollision(Triangle));

  Box := Box.Translate(Vector3(0, 0, 1.0));
  AssertTrue(not Box.IsTriangleCollision(Triangle));

  RandomTrianglesTest(1, 1, 1);

  { make one coord locked, and perform the same tests as above }
  RandomTrianglesTest(0, 1, 1);
  RandomTrianglesTest(1, 0, 1);
  RandomTrianglesTest(1, 1, 0);
end;

procedure TTestCastleBoxes.TestIsBox3DTriangleCollisionEpsilons;
var
  Epsilon: Single;

  { Modified version of IsCenteredBox3DPlaneCollision and IsBox3DTriangleCollision
    that use Epsilon variable here. Also, use Single precision calculations. }

  function IsCenteredBox3DPlaneCollision(
    const BoxHalfSize: TVector3;
    const Plane: TVector4): boolean;
  { Implementation of this is based on
    [http://jgt.akpeters.com/papers/AkenineMoller01/tribox.html]
    planeBoxOverlap routine, by Tomas Akenine-Moller,
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
    VMin, VMax: TVector3;
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
       Plane[3] > Epsilon then
      Exit(false);

    { So VMin is <= plane. So if VMax is >= 0, then there's a collision. }
    Result :=  Plane[0] * VMax[0] +
               Plane[1] * VMax[1] +
               Plane[2] * VMax[2] +
               Plane[3] >= Epsilon;
  end;

  function IsBox3DTriangleCollision(
    const Box: TBox3D;
    const Triangle: TTriangle3): boolean;

  { Implementation of this is based on
    [http://jgt.akpeters.com/papers/AkenineMoller01/tribox.html],
    by Tomas Akenine-Moller, described
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
    TriangleMoved: TTriangle3;
    BoxHalfSize: TVector3;

    { ======================== X-tests ======================== }
    function AXISTEST_X01(const a, b, fa, fb: Single): boolean;
    var
      p0, p2, rad, min, max: Single;
    begin
      p0 := a * TriangleMoved.Data[0].Data[1] - b * TriangleMoved.Data[0].Data[2];
      p2 := a * TriangleMoved.Data[2].Data[1] - b * TriangleMoved.Data[2].Data[2];
      if p0<p2 then begin min := p0; max := p2; end else
                    begin min := p2; max := p0; end;
      rad := fa * BoxHalfSize[1] + fb * BoxHalfSize[2];
      Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
    end;

    function AXISTEST_X2(const a, b, fa, fb: Single): boolean;
    var
      p0, p1, rad, min, max: Single;
    begin
      p0 := a * TriangleMoved.Data[0].Data[1] - b * TriangleMoved.Data[0].Data[2];
      p1 := a * TriangleMoved.Data[1].Data[1] - b * TriangleMoved.Data[1].Data[2];
      if p0<p1 then begin min := p0; max := p1; end else
                    begin min := p1; max := p0; end;
      rad := fa * BoxHalfSize[1] + fb * BoxHalfSize[2];
      Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
    end;

    { ======================== Y-tests ======================== }
    function AXISTEST_Y02(const a, b, fa, fb: Single): boolean;
    var
      p0, p2, rad, min, max: Single;
    begin
      p0 := -a * TriangleMoved.Data[0].Data[0] + b * TriangleMoved.Data[0].Data[2];
      p2 := -a * TriangleMoved.Data[2].Data[0] + b * TriangleMoved.Data[2].Data[2];
      if p0<p2 then begin min := p0; max := p2; end else
                    begin min := p2; max := p0; end;
      rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[2];
      Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
    end;

    function AXISTEST_Y1(const a, b, fa, fb: Single): boolean;
    var
      p0, p1, rad, min, max: Single;
    begin
      p0 := -a * TriangleMoved.Data[0].Data[0] + b * TriangleMoved.Data[0].Data[2];
      p1 := -a * TriangleMoved.Data[1].Data[0] + b * TriangleMoved.Data[1].Data[2];
      if p0<p1 then begin min := p0; max := p1; end else
                    begin min := p1; max := p0; end;
      rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[2];
      Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
    end;

    { ======================== Z-tests ======================== }
    function AXISTEST_Z12(const a, b, fa, fb: Single): boolean;
    var
      p1, p2, rad, min, max: Single;
    begin
      p1 := a * TriangleMoved.Data[1].Data[0] - b * TriangleMoved.Data[1].Data[1];
      p2 := a * TriangleMoved.Data[2].Data[0] - b * TriangleMoved.Data[2].Data[1];
      if p2<p1 then begin min := p2; max := p1; end else
                    begin min := p1; max := p2; end;
      rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[1];
      Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
    end;

    function AXISTEST_Z0(const a, b, fa, fb: Single): boolean;
    var
      p0, p1, rad, min, max: Single;
    begin
      p0 := a * TriangleMoved.Data[0].Data[0] - b * TriangleMoved.Data[0].Data[1];
      p1 := a * TriangleMoved.Data[1].Data[0] - b * TriangleMoved.Data[1].Data[1];
      if p0<p1 then begin min := p0; max := p1; end else
                    begin min := p1; max := p0; end;
      rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[1];
      Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
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
    BoxCenter: TVector3;
    I: Integer;
    TriangleEdges: array [0..2] of TVector3;
    EdgeAbs: TVector3;
    min, max: Single;
    Plane: TVector4;
    PlaneDir: TVector3 absolute Plane;
  begin
    if Box.IsEmpty then
      Exit(false);

    { calculate BoxCenter and BoxHalfSize }
    for I := 0 to 2 do
    begin
      BoxCenter[I] := (Box.Data[0].Data[I] + Box.Data[1].Data[I]) / 2;
      BoxHalfSize[I] := (Box.Data[1].Data[I] - Box.Data[0].Data[I]) / 2;
    end;

    { calculate TriangleMoved (Triangle shifted by -BoxCenter,
      so that we can treat the BoxHalfSize as centered around origin) }
    TriangleMoved.Data[0] := Triangle.Data[0] - BoxCenter;
    TriangleMoved.Data[1] := Triangle.Data[1] - BoxCenter;
    TriangleMoved.Data[2] := Triangle.Data[2] - BoxCenter;

    { calculate TriangleMoved edges }
    TriangleEdges[0] := TriangleMoved.Data[1] - TriangleMoved.Data[0];
    TriangleEdges[1] := TriangleMoved.Data[2] - TriangleMoved.Data[1];
    TriangleEdges[2] := TriangleMoved.Data[0] - TriangleMoved.Data[2];

    { tests 3) }
    EdgeAbs[0] := Abs(TriangleEdges[0].Data[0]);
    EdgeAbs[1] := Abs(TriangleEdges[0].Data[1]);
    EdgeAbs[2] := Abs(TriangleEdges[0].Data[2]);
    if AXISTEST_X01(TriangleEdges[0].Data[2], TriangleEdges[0].Data[1], EdgeAbs[2], EdgeAbs[1]) then Exit(false);
    if AXISTEST_Y02(TriangleEdges[0].Data[2], TriangleEdges[0].Data[0], EdgeAbs[2], EdgeAbs[0]) then Exit(false);
    if AXISTEST_Z12(TriangleEdges[0].Data[1], TriangleEdges[0].Data[0], EdgeAbs[1], EdgeAbs[0]) then Exit(false);

    EdgeAbs[0] := Abs(TriangleEdges[1].Data[0]);
    EdgeAbs[1] := Abs(TriangleEdges[1].Data[1]);
    EdgeAbs[2] := Abs(TriangleEdges[1].Data[2]);
    if AXISTEST_X01(TriangleEdges[1].Data[2], TriangleEdges[1].Data[1], EdgeAbs[2], EdgeAbs[1]) then Exit(false);
    if AXISTEST_Y02(TriangleEdges[1].Data[2], TriangleEdges[1].Data[0], EdgeAbs[2], EdgeAbs[0]) then Exit(false);
    if AXISTEST_Z0 (TriangleEdges[1].Data[1], TriangleEdges[1].Data[0], EdgeAbs[1], EdgeAbs[0]) then Exit(false);

    EdgeAbs[0] := Abs(TriangleEdges[2].Data[0]);
    EdgeAbs[1] := Abs(TriangleEdges[2].Data[1]);
    EdgeAbs[2] := Abs(TriangleEdges[2].Data[2]);
    if AXISTEST_X2 (TriangleEdges[2].Data[2], TriangleEdges[2].Data[1], EdgeAbs[2], EdgeAbs[1]) then Exit(false);
    if AXISTEST_Y1 (TriangleEdges[2].Data[2], TriangleEdges[2].Data[0], EdgeAbs[2], EdgeAbs[0]) then Exit(false);
    if AXISTEST_Z12(TriangleEdges[2].Data[1], TriangleEdges[2].Data[0], EdgeAbs[1], EdgeAbs[0]) then Exit(false);

    { tests 1)
      first test overlap in the (x,y,z)-directions
      find min, max of the triangle each direction, and test for overlap in
      that direction -- this is equivalent to testing a minimal AABB around
      the triangle against the AABB }

    { test in X-direction }
    FindMinMax(TriangleMoved.Data[0].Data[0], TriangleMoved.Data[1].Data[0], TriangleMoved.Data[2].Data[0], min, max);
    if (min >  boxhalfsize[0] + Epsilon) or
       (max < -boxhalfsize[0] - Epsilon) then Exit(false);

    { test in Y-direction }
    FindMinMax(TriangleMoved.Data[0].Data[1], TriangleMoved.Data[1].Data[1], TriangleMoved.Data[2].Data[1], min, max);
    if (min >  boxhalfsize[1] + Epsilon) or
       (max < -boxhalfsize[1] - Epsilon) then Exit(false);

    { test in Z-direction }
    FindMinMax(TriangleMoved.Data[0].Data[2], TriangleMoved.Data[1].Data[2], TriangleMoved.Data[2].Data[2], min, max);
    if (min >  boxhalfsize[2] + Epsilon) or
       (max < -boxhalfsize[2] - Epsilon) then Exit(false);

    { tests 2)
      test if the box intersects the plane of the triangle
      compute plane equation of triangle: normal*x+d=0 }
    PlaneDir := TVector3.CrossProduct(TriangleEdges[0], TriangleEdges[1]);
    Plane[3] := -TVector3.DotProduct(PlaneDir, TriangleMoved.Data[0]);
    if not IsCenteredBox3DPlaneCollision(BoxHalfSize, Plane) then
      Exit(false);

    Result := true; { box and triangle overlaps }
  end;

var
  Triangle: TTriangle3;
  Box: TBox3D;

  procedure DoTest(const TestName: string; CorrectResult: boolean);
  begin
    try
      { These writelns were used to experimentally check that 1e-4 is still too small,
        and 1e-3 is enough for these tests... That's assuming that we have
        IsBox3DTriangleCollision implementation based on Single type. }
      {
      Epsilon := 1e-3;
      Write(IsBox3DTriangleCollision(Box, Triangle), ' ');
      Epsilon := 1e-6;
      Write(IsBox3DTriangleCollision(Box, Triangle), ' ');
      Write(CorrectResult, ' ');
      Write(IsBox3DTriangleCollision(Box, Triangle), ' ');
      Writeln;}
      AssertTrue(IsBox3DTriangleCollision(Box, Triangle) = CorrectResult);
      AssertTrue(Box.IsTriangleCollision(Triangle) = CorrectResult);
    except
      on E: EAssertionFailedError do
      begin
        E.Message := E.Message + ' (test name: ' + TestName + ')';
        raise;
      end;
    end;
  end;

const
  A = 1.980401039123535;
var
  OldEpsilon: Single;
begin
  Epsilon := 1e-5;

  Box.Data[0].Data[0] := -7.721179485321045;
  Box.Data[0].Data[1] := -3.115305423736572;
  Box.Data[0].Data[2] := 26.886024475097656;
  Box.Data[1].Data[0] := 0.283733367919922;
  Box.Data[1].Data[1] := 3.240192413330078;
  Box.Data[1].Data[2] := 28.947912216186523;
  Triangle.Data[0].Data[0] := -7.759810924530029;
  Triangle.Data[0].Data[1] := 6.43093835606123E-006;
  Triangle.Data[0].Data[2] := 28.172618865966797;
  Triangle.Data[1].Data[0] := -7.610710620880127;
  Triangle.Data[1].Data[1] := -1.513858914375305;
  Triangle.Data[1].Data[2] := 31.17262077331543;
  Triangle.Data[2].Data[0] := -7.759810924530029;
  Triangle.Data[2].Data[1] := 6.43093835606123E-006;
  Triangle.Data[2].Data[2] := 31.17262077331543;
  (* DoTest('1', false
    { TODO:
      Not sure what the result should be... ? But it sure depends on the epsilon used in
      IsBox3DTriangleCollision. Test on Double values shows that this should be false.
    }); *)

  Box.Data[0].Data[0] := 0.283733367919922;
  Box.Data[0].Data[1] := -98.447769165039062;
  Box.Data[0].Data[2] := -A;
  Box.Data[1].Data[0] :=  128.36233520507812;
  Box.Data[1].Data[1] := 3.240192413330078;
  Box.Data[1].Data[2] := 31.009799957275391;
  Triangle.Data[0].Data[0] := 25.288267135620117;
  Triangle.Data[0].Data[1] := 8.671939849853516;
  Triangle.Data[0].Data[2] := -A;
  Triangle.Data[1].Data[0] := 16.125827789306641;
  Triangle.Data[1].Data[1] := -21.297039031982422;
  Triangle.Data[1].Data[2] := -A;
  Triangle.Data[2].Data[0] := 19.586576461791992;
  Triangle.Data[2].Data[1] := -26.554182052612305;
  Triangle.Data[2].Data[2] := -A;

  { Looks like for this test, even larger Epsilon is needed.
    At least under x86_64 (tested on Linux with fpc 2.2.4 and trunk on 2009-08-21,
    tested again with FPC 3.1.1 from 2015-11).
    And probably on armel / armhf too. }
  OldEpsilon := Epsilon;
  Epsilon := 1e-3;

  // TODO: This doesn't work with Box.IsTriangleCollision based on Double.
  // Is this test even correct, maybe it should be false, if the more precise
  // calculation (although with stricter Epsilon) fails?
  // We need to calculate this in some independent way, to be sure...
  //DoTest('2', true);
  try
    AssertTrue(IsBox3DTriangleCollision(Box, Triangle) = true);
  except
    on E: EAssertionFailedError do
    begin
      E.Message := E.Message + ' (test name: 2)';
      raise;
    end;
  end;

  Epsilon := OldEpsilon;

  Box.Data[0].Data[0] := 0.283733367919922;
  Box.Data[0].Data[1] := -47.603790283203125;
  Box.Data[0].Data[2] := -A;
  Box.Data[1].Data[0] := 64.323036193847656;
  Box.Data[1].Data[1] := 3.240192413330078;
  Box.Data[1].Data[2] := 14.514699935913086;
  { Triangle as before }
  DoTest('3', true);

  Box.Data[0].Data[0] := 0.283733367919922;
  Box.Data[0].Data[1] := -47.603790283203125;
  Box.Data[0].Data[2] := -A;
  Box.Data[1].Data[0] := 32.303382873535156;
  Box.Data[1].Data[1] := -22.181798934936523;
  Box.Data[1].Data[2] := 6.267149448394775;
  { Triangle as before }
  DoTest('4', true);

  Box.Data[0].Data[0] := 16.293558120727539;
  Box.Data[0].Data[1] := 3.240192413330078;
  Box.Data[0].Data[2] := -A;
  Box.Data[1].Data[0] := 24.298469543457031;
  Box.Data[1].Data[1] := 9.59568977355957;
  Box.Data[1].Data[2] := 0.081486582756042;
  Triangle.Data[0].Data[0] := 25.288267135620117;
  Triangle.Data[0].Data[1] := 8.671939849853516;
  Triangle.Data[0].Data[2] := -A;
  Triangle.Data[1].Data[0] := -0.731123030185699;
  Triangle.Data[1].Data[1] := 32.452774047851562;
  Triangle.Data[1].Data[2] := -A;
  Triangle.Data[2].Data[0] := -0.382562607526779;
  Triangle.Data[2].Data[1] := 26.646867752075195;
  Triangle.Data[2].Data[2] := -A;
  DoTest('5', true);

  Box.Data[0].Data[0] := -17.727319717407227;
  Box.Data[0].Data[1] := 4.829066753387451;
  Box.Data[0].Data[2] := 5.751677513122559;
  Box.Data[1].Data[0] := -15.726092338562012;
  Box.Data[1].Data[1] := 6.417941093444824;
  Box.Data[1].Data[2] := 6.267149448394775;
  Triangle.Data[0].Data[0] := -6.18981409072876;
  Triangle.Data[0].Data[1] := 2.234785079956055;
  Triangle.Data[0].Data[2] := 29.618535995483398;
  Triangle.Data[1].Data[0] := -20.651203155517578;
  Triangle.Data[1].Data[1] := 5.486495018005371;
  Triangle.Data[1].Data[2] := -0.132393002510071;
  Triangle.Data[2].Data[0] := -6.149000644683838;
  Triangle.Data[2].Data[1] := 2.083860397338867;
  Triangle.Data[2].Data[2] := 29.618535995483398;
  (* DoTest('6', false
    { TODO:
      Not sure what the result should be... ? But it sure depends on the epsilon used in
      IsBox3DTriangleCollision. Test on Double values shows that this should be false.
      }); *)
end;

procedure TTestCastleBoxes.TestBox3DTransform;
{ Test Box3DTransform for correctness and speed.
  Compare with Slower implementation, that should be slower
  (on non-projection matrices) but give the same results. }

  function Slower(const Box: TBox3D; const Matrix: TMatrix4): TBox3D;
  var
    BoxPoints: TBoxCorners;
    i: integer;
  begin
    if Box.IsEmpty then
      Exit(TBox3D.Empty);

    Box.Corners(BoxPoints);
    for i := 0 to 7 do BoxPoints[i] := Matrix.MultPoint(BoxPoints[i]);

    { Non-optimized version:
        Result := CalculateBoundingBox(@BoxPoints, 8, 0);

      But it turns out that the code below, that does essentially the same
      thing as CalculateBoundingBox implementation, works noticeably faster.
      This is noticeable on "The Castle" with many creatures: then a considerable
      time is spend inside TCreature.BoundingBox, that must calculate
      transformed bounding boxes.
    }

    Result.Data[0] := BoxPoints[0];
    Result.Data[1] := BoxPoints[0];
    for I := 1 to High(BoxPoints) do
    begin
      if BoxPoints[I].Data[0] < Result.Data[0].Data[0] then Result.Data[0].Data[0] := BoxPoints[I].Data[0];
      if BoxPoints[I].Data[1] < Result.Data[0].Data[1] then Result.Data[0].Data[1] := BoxPoints[I].Data[1];
      if BoxPoints[I].Data[2] < Result.Data[0].Data[2] then Result.Data[0].Data[2] := BoxPoints[I].Data[2];
      if BoxPoints[I].Data[0] > Result.Data[1].Data[0] then Result.Data[1].Data[0] := BoxPoints[I].Data[0];
      if BoxPoints[I].Data[1] > Result.Data[1].Data[1] then Result.Data[1].Data[1] := BoxPoints[I].Data[1];
      if BoxPoints[I].Data[2] > Result.Data[1].Data[2] then Result.Data[1].Data[2] := BoxPoints[I].Data[2];
    end;
  end;

  function RandomBox: TBox3D;
  var
    I: Integer;
  begin
    for I := 0 to 2 do
    begin
      Result.Data[0].Data[I] := 50 - Random * 100;
      Result.Data[1].Data[I] := 50 - Random * 100;
      OrderUp(Result.Data[0].Data[I], Result.Data[1].Data[I]);
    end;
  end;

var
  Box: TBox3D;
  I: Integer;
  Matrix: TMatrix4;
begin
  for I := 0 to 1000 do
  begin
    Box := RandomBox;
    Matrix := RandomMatrix;
    AssertBoxesEqual(Slower(Box, Matrix), Box.Transform(Matrix), 0.01);
  end;

  for I := 0 to 1000 do
  begin
    Box := RandomBox;
    Matrix := RandomNonProjectionMatrix;
    AssertBoxesEqual(Slower(Box, Matrix), Box.Transform(Matrix), 0.01);
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
  for I := 0 to 1000000 do Box3DTransform(Box, Matrix);
  Writeln(Format('Box3DTransform: %f', [ProcessTimerEnd]));

  Writeln('On non-projection matrix:');

  Box := RandomBox;
  Matrix := RandomNonProjectionMatrix;

  ProcessTimerBegin;
  for I := 0 to 1000000 do Slower(Box, Matrix);
  Writeln(Format('Slower: %f', [ProcessTimerEnd]));

  ProcessTimerBegin;
  for I := 0 to 1000000 do Box3DTransform(Box, Matrix);
  Writeln(Format('Box3DTransform: %f', [ProcessTimerEnd]));
  {$endif BOX3D_TRANSFORM_SPEED_TEST}
end;

procedure TTestCastleBoxes.TestBox3DMaximumPlane;
begin
  try
    TBox3D.Empty.MaximumPlane(Vector3(1, 1, 1));
  except
    on E: EBox3DEmpty do { Ok };
  end;

  AssertVectorEquals(Vector4(-1, 0, 0, 2), Box3D(
    Vector3(2, 3, 4),
    Vector3(50, 60, 70)).MaximumPlane(Vector3(-1, 0, 0)));

  AssertVectorEquals(Vector4(0, 0, -1, 4), Box3D(
    Vector3(2, 3, 4),
    Vector3(50, 60, 70)).MaximumPlane(Vector3(0, 0, -1)));

  AssertVectorEquals(Vector4(1, 1, 1,
      { 50 + 60 + 70 + Result.Data[3] = 0 }
      - 50 - 60 - 70
    ), Box3D(
    Vector3(2, 3, 4),
    Vector3(50, 60, 70)).MaximumPlane(Vector3(1, 1, 1)));
end;

procedure TTestCastleBoxes.TestBox3DMinimumPlane;
begin
  try
    TBox3D.Empty.MinimumPlane(Vector3(1, 1, 1));
  except
    on E: EBox3DEmpty do { Ok };
  end;

  AssertVectorEquals(Vector4(1, 0, 0, -2), Box3D(
    Vector3(2, 3, 4),
    Vector3(50, 60, 70)).MinimumPlane(Vector3(1, 0, 0)));

  AssertVectorEquals(Vector4(0, 0, 1, -4), Box3D(
    Vector3(2, 3, 4),
    Vector3(50, 60, 70)).MinimumPlane(Vector3(0, 0, 1)));

  AssertVectorEquals(Vector4(1, 1, 1,
      { 2 + 3 + 4 + Result.Data[3] = 0 }
      - 2 - 3 - 4
    ), Box3D(
    Vector3(2, 3, 4),
    Vector3(50, 60, 70)).MinimumPlane(Vector3(1, 1, 1)));
end;

procedure TTestCastleBoxes.TestBox3DPointDistance;
const
  Box: TBox3D = (Data: (
    (Data: (1, 2, 3)),
    (Data: (4, 5, 6))
  ));
  Epsilon = 0.0001;
begin
  { check point inside box case }
  AssertSameValue(0, Box.PointDistance(Vector3(1, 2, 3)), 0);
  AssertSameValue(0, Box.PointDistance(Vector3(3, 4, 5)), 0);
  { check point <-> box side case }
  AssertSameValue(4, Box.PointDistance(Vector3(3, 4, 10)));
  AssertSameValue(3, Box.PointDistance(Vector3(3, 4, 0)));
  { check point <-> box edge case }
  AssertSameValue(Sqrt( Sqr(10-6) + Sqr(10-5) ),
    Box.PointDistance(Vector3(3, 10, 10)),
    Epsilon);
  AssertSameValue(Sqrt( Sqr(0-2)  + Sqr(0-3)  ),
    Box.PointDistance(Vector3(3, 0, 0)),
    Epsilon);
  { check point <-> box corner case }
  AssertSameValue(Sqrt( Sqr(10-6) + Sqr(10-5) + Sqr(10-4) ),
    Box.PointDistance(Vector3(10, 10, 10)),
    Epsilon);
  AssertSameValue(Sqrt( Sqr(0-2)  + Sqr(0-3)  + Sqr(0-1)  ),
    Box.PointDistance(Vector3(0, 0, 0)),
    Epsilon);
end;

procedure TTestCastleBoxes.Test2D;
const
  Box: TBox3D = (Data: (
    (Data: (1, 2, 3)),
    (Data: (4, 5, 6))
  ));
  Box2: TBox3D = (Data: (
    (Data: (1, 2, 3)),
    (Data: (2, 5, 13))
  ));
begin
  AssertTrue(Box.Contains2D(Vector3(2, 3, 10), 2));
  AssertTrue(not Box.Contains2D(Vector3(2, 3, 10), 0));
  AssertTrue(not Box.Contains2D(Vector3(2, 3, 10), 1));
  // This is disallowed at compile-time now, as argument is T3DAxis
  // try
  //   Box.Contains2D(Vector3(2, 3, 10), 3);
  //   Fail('Above Contains2D with IgnoreIndex = 3 should raise exception');
  // except end;

  AssertSameValue(Sqrt(Sqr(5) + Sqr(6)), Box.Radius2D(0), 0.01);
  AssertSameValue(Sqrt(Sqr(4) + Sqr(6)), Box.Radius2D(1), 0.01);
  AssertSameValue(Sqrt(Sqr(4) + Sqr(5)), Box.Radius2D(2), 0.01);
  // This is disallowed at compile-time now, as argument is T3DAxis
  // try
  //   Box.Radius2D(3);
  //   Fail('Above Radius2D with IgnoreIndex = 3 should raise exception');
  // except end;

  AssertSameValue(Sqrt(Sqr(4) + Sqr(5) + Sqr(6)), Box.Radius, 0.01);

  AssertSameValue(Sqrt(Sqr(3) + Sqr(3) + Sqr(3)), Box.Diagonal, 0.01);
  AssertSameValue(Sqrt(Sqr(1) + Sqr(10) + Sqr(3)), Box2.Diagonal, 0.01);
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestCastleBoxes);
{$endif}
end.
