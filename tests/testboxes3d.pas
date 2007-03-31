{
  Copyright 2007 Michalis Kamburelis.

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

unit TestBoxes3d;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestBoxes3d = class(TTestCase)
  published
    procedure TestIsCenteredBox3dPlaneCollision;
    procedure TestIsBox3dPlaneCollision;
    procedure TestIsBox3dTriangleCollision;
  end;

implementation

uses VectorMath, KambiUtils, Boxes3d, KambiStringUtils;

procedure TTestBoxes3d.TestIsCenteredBox3dPlaneCollision;
var
  BoxHalfSize: TVector3Single;
  Plane: TVector4Single;
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

procedure TTestBoxes3d.TestIsBox3dPlaneCollision;
var
  Box: TBox3d;
begin
  Box[0] := Vector3Single(-10, -1, -1);
  Box[1] := Vector3Single( 10,  1,  1);

  { box 10, 1, 1 with a plane that crosses 0,0,0 point always collides }
  Assert(IsBox3dPlaneCollision(
    Box, Vector4Single(0, 0, 1, 0)));
  Assert(IsBox3dPlaneCollision(
    Box, Vector4Single(0, 1, 0, 0)));
  Assert(IsBox3dPlaneCollision(
    Box, Vector4Single(1, 0, 0, 0)));
  Assert(IsBox3dPlaneCollision(
    Box, Vector4Single(123, 456, 789, 0)));

  Assert(not IsBox3dPlaneCollision(
    Box, Vector4Single(0, 0, -1, 5)));
  Assert(not IsBox3dPlaneCollision(
    Box, Vector4Single(0, -1, 0, 5)));
  Assert(IsBox3dPlaneCollision(
    Box, Vector4Single(-1, 0, 0, 5)));

  Box := Box3dTranslate(Box, Vector3Single(0, 1000, 0));

  Assert(IsBox3dPlaneCollision(
    Box, Vector4Single(0, 0, 1, 0)));
  Assert(not IsBox3dPlaneCollision(
    Box, Vector4Single(0, 1, 0, 0)));
  Assert(IsBox3dPlaneCollision(
    Box, Vector4Single(1, 0, 0, 0)));
end;

procedure TTestBoxes3d.TestIsBox3dTriangleCollision;

  procedure RandomTrianglesTest(
    const XRandomness, YRandomness, ZRandomness: Cardinal);
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

initialization
  RegisterTest(TTestBoxes3d);
end.
