{
  Copyright 2003-2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Type TBox3d and many operations on it. }
unit Boxes3d;

interface

uses VectorMath, SysUtils, KambiUtils;

{$define read_interface}

type
  { Axis-aligned box. Rectangular prism with all sides parallel to basic planes
    X = 0, Y = 0 and Z = 0. This is sometimes called AABB, "axis-aligned bounding
    box".

    This is a handy type, because it's easy to implement many
    operations on it in a fast manner.

    First point always has all the smaller coords, second point has all
    the larger coords. I.e. always
@preformatted(
  Box[0, 0] <= Box[1, 0] and
  Box[0, 1] <= Box[1, 1] and
  Box[0, 2] <= Box[1, 2]
)
    The only exception is the special value EmptyBox3d. }
  TBox3d = array[0..1]of TVector3Single;
  PBox3d = ^TBox3d;

  TObjectBBox = class
  public
    function BoundingBox: TBox3d; virtual; abstract;
  end;

const
  { Special value for TBox3d meaning "bounding box doesn't exist".
    This is used when the object has no points, so bounding box
    doesn't exist. }
  EmptyBox3d: TBox3d = ((0, 0, 0), (-1, -1, -1));

{ Check is box empty.
  You can think of this function as "compare Box with EmptyBox3d".

  But actually it works a little faster, by utilizing the assumption
  that EmptyBox3d is the only allowed value that breaks
  @code(Box[0, 0] <= Box[1, 0]) rule. }
function IsEmptyBox3d(const Box: TBox3d): boolean;

function Box3d(const p0, p1: TVector3Single): TBox3d;
function Box3dOrderUp(const p0, p1: TVector3Single): TBox3d;

{ These functions calculate the middle point, average size, max size
  and particular sizes of given bounding box.
  When given Box is empty (IsEmptyBox3d), they raise some exception.

  @groupBegin }
function Box3dMiddle(const Box: TBox3d): TVector3Single;
function Box3dAvgSize(const Box: TBox3d): Single;
function Box3dMaxSize(const box: TBox3d): Single;
function Box3dMinSize(const box: TBox3d): Single;
function Box3dSizeX(const box: TBox3d): Single;
function Box3dSizeY(const box: TBox3d): Single;
function Box3dSizeZ(const box: TBox3d): Single;
{ @groupEnd }

{ This decreases Box[0, 0], Box[0, 1], Box[0, 2] by Expand
   and increases Box[1, 0], Box[1, 1], Box[1, 2] by Expand.
  So you get Box with all sizes increased by 2 * Expand.

  Box must not be empty.
  Note that Expand may be negative, but then you must be sure
  that it doesn't make Box empty. }
procedure BoxExpandTo1st(var Box: TBox3d; const Expand: Single); overload;

{ This decreases Box[0] by Expand, and increases Box[1] by Expand.
  So you get Box with all sizes increased by 2 * Expand.

  Box must not be empty.
  Note that Expand may be negative, but then you must be sure
  that it doesn't make Box empty. }
procedure BoxExpandTo1st(var box: TBox3d; const Expand: TVector3Single); overload;

function BoxExpand(var Box: TBox3d; const Expand: Single): TBox3d; overload;
function BoxExpand(var box: TBox3d; const Expand: TVector3Single): TBox3d; overload;

{ Check is the point inside the box.
  Always false if Box is empty (obviously, no point is inside an empty box).

  @groupBegin }
function Box3dPointInside(const pt: TVector3Single; const box: TBox3d): boolean; overload;
function Box3dPointInside(const pt: TVector3Double; const box: TBox3d): boolean; overload;
{ @groupEnd }

function Box3dCubeAroundPoint(const pt: TVector3Single; CubeSize: Single): TBox3d;

type
  TGetIndexFromIndexNumFunc = function(indexNum: integer): integer of object;
  TGetVertexFromIndexFunc = function(index: integer): TVector3Single of object;

{ Calculate bounding box of a set of 3D points.
  This calculates the smallest possible box enclosing all given points.
  For VertsCount = 0 this returns EmptyBox3d.

  Overloaded version with Transform parameter transforms each point
  by given matrix.

  Overloaded version with GetVertex as a function uses GetVertex to query
  for indexes from [0 .. VertsCount - 1] range.

  As usual, VertsStride = 0 means VertsStride = SizeOf(TVector3Single).

  @groupBegin }
function CalculateBoundingBox(
  Verts: PVector3Single; VertsCount: Cardinal; VertsStride: Cardinal): TBox3d; overload;
function CalculateBoundingBox(
  Verts: PVector3Single; VertsCount: Cardinal; VertsStride: Cardinal;
  const Transform: TMatrix4Single): TBox3d; overload;
function CalculateBoundingBox(
  GetVertex: TGetVertexFromIndexFunc;
  VertsCount: integer): TBox3d; overload;
{ @groupEnd }

{ Calculate bounding box of a set of indexed 3D points.

  This is much like CalculateBoundingBox, except there are two functions:
  For each number in [0 .. VertsIndicesCount - 1] range, GetVertIndex
  returns an index. If this index is >= 0 then it's used to query
  GetVertex function to get actual vertex position.

  Indexes < 0 are ignored, this is sometimes comfortable. E.g. for VRML models,
  you often have a list of indexes with -1 in between marking end of faces.

  Returns smallest box enclosing all vertexes.

  Overloaded version with Transform parameter transforms each point
  by given matrix.

  @groupBegin }
function CalculateBoundingBoxFromIndices(
  GetVertIndex: TGetIndexFromIndexNumFunc;
  VertsIndicesCount: integer;
  GetVertex: TGetVertexFromIndexFunc): TBox3d; overload;
function CalculateBoundingBoxFromIndices(
  GetVertIndex: TGetIndexFromIndexNumFunc;
  VertsIndicesCount: integer;
  GetVertex: TGetVertexFromIndexFunc;
  const Transform: TMatrix4Single): TBox3d; overload;
{ @groupEnd }

{ Sum two TBox3d values. This calculates the smallest box that encloses
  both Box1 and Box2.

  Box3dSumTo1st places the result of calculation back in Box1 argument.

  @groupBegin }
function Box3dSum(const box1, box2: TBox3d): TBox3d;
procedure Box3dSumTo1st(var box1: TBox3d; const box2: TBox3d); overload;
{ @groupEnd }

{ This enlarges Box1 so that it contains given Point. }
procedure Box3dSumTo1st(var box1: TBox3d; const Point: TVector3Single); overload;

{ Three box sizes. }
function Box3dSizes(const box: TBox3d): TVector3Single;

{ Calculates eight corners of the box, placing them in AllPoints^[0..7]. }
procedure Box3dGetAllPoints(allpoints: PVector3Single; const box: TBox3d);

{ Transforms the box by given matrix. More precisely, transforms all
  8 box corners, and makes new box enclosing these 8 points.

  Since this is axis-aligned box, rotating etc. of the box usually makes
  larger box (this is the primary disadvantage of axis-aligned boxes,
  as opposed to oriented boxes). }
function BoundingBoxTransform(const bbox: TBox3d;
  const Matrix: TMatrix4Single): TBox3d;

{ Move Box. Does nothing if Box is empty. }
function Box3dTranslate(const Box: TBox3d;
  const Translation: TVector3Single): TBox3d;

{ Move Box, by -Translation. Does nothing if Box is empty. }
function Box3dAntiTranslate(const Box: TBox3d;
  const Translation: TVector3Single): TBox3d;

function Box3dToNiceStr(const box: TBox3d): string;
function Box3dToRawStr(const box: TBox3d): string;

procedure Box3dClamp(var point: TVector3Single; const box: TBox3d); overload;
procedure Box3dClamp(var point: TVector3Double; const box: TBox3d); overload;

function TriangleBoundingBox(const T: TTriangle3Single): TBox3d;

{ TryBoxRayClosestIntersection calculates intersection between the
  ray (returns closest intersection to Ray0) and the box.

  The box is treated just like a set of 6 rectangles in 3D.
  This means that the intersection will always be placed on one of the
  box sides, even if Ray0 starts inside the box.
  See TryBoxRayEntrance for the other version.

  Returns also IntersectionDistance, which is the distance to the Intersection
  relative to RayVector (i.e. Intersection is always = Ray0 +
  IntersectionDistance * RayVector).

  @groupBegin }
function TryBoxRayClosestIntersection(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Box: TBox3d; const Ray0, RayVector: TVector3Single): boolean; overload;
function TryBoxRayClosestIntersection(
  out Intersection: TVector3Single;
  const Box: TBox3d; const Ray0, RayVector: TVector3Single): boolean; overload;
function TryBoxRayClosestIntersection(
  out IntersectionDistance: Single;
  const Box: TBox3d; const Ray0, RayVector: TVector3Single): boolean; overload;
{ @groupEnd }

{ TryBoxRayEntrance calculates intersection between the
  ray (returns closest intersection to Ray0) and the box, treating the box
  as a filled volume.

  This means that if Ray0 is inside the box, TryBoxRayEntrance simply returns
  Ray0. If Ray0 is outside of the box, the answer is the same
  as with TryBoxRayClosestIntersection. }
function TryBoxRayEntrance(out Entrance: TVector3Single;
  const Box: TBox3d; const Ray0, RayVector: TVector3Single): boolean;

function IsBox3dSegmentCollision(
  const Box: TBox3d;
  const Segment1, Segment2: TVector3Single): boolean;

{ Tests for collision between box3d centered around (0, 0, 0)
  and a plane.

  Note that you can't express empty box3d here: all BoxHalfSize items
  must be >= 0. The case when size = 0 is considered like infintely small
  box in some dimension (e.g. if all three sizes are = 0 then the box
  becomes a point).

  @groupBegin }
function IsCenteredBox3dPlaneCollision(
  const BoxHalfSize: TVector3Single;
  const Plane: TVector4Single): boolean; overload;
function IsCenteredBox3dPlaneCollision(
  const BoxHalfSize: TVector3Double;
  const Plane: TVector4Double): boolean; overload;
{ @groupEnd }

function IsBox3dPlaneCollision(const Box3d: TBox3d;
  const Plane: TVector4Single): boolean;

function IsBox3dTriangleCollision(
  const Box: TBox3d;
  const Triangle: TTriangle3Single): boolean;

{ This is equivalent to @link(FrustumSphereCollisionPossible),
  but here it takes a box instead of a sphere. }
function FrustumBox3dCollisionPossible(const Frustum: TFrustum;
  const Box: TBox3d): TFrustumCollisionPossible;

{ This is like @link(FrustumBox3dCollisionPossible)
  but it returns true when FrustumBox3dCollisionPossible
  would return fcSomeCollisionPossible or fcInsideFrustum.
  Otherwise (when FrustumBox3dCollisionPossible would return
  fcNoCollision) this returns false.

  So this returns less detailed result, but is a little faster. }
function FrustumBox3dCollisionPossibleSimple(const Frustum: TFrustum;
  const Box: TBox3d): boolean;

{ This calculates smallest possible sphere completely
  enclosing given Box.
  SphereRadiusSqr = 0 and SphereCenter is undefined if Box is empty. }
procedure Box3dBoundingSphere(const Box3d: TBox3d;
  var SphereCenter: TVector3Single; var SphereRadiusSqr: Single);

function Boxes3dCollision(const Box1, Box2: TBox3d): boolean;

{ This is just like Boxes3dCollision, but it takes into account only
  XY plane. I.e. it works like Box1 and Box2 are infinitely large in the
  Z coordinate. Or, in other words, this actually checks collision
  of 2D rectangles obtained by projecting both boxes on plane XY. }
function Boxes3dXYCollision(const Box1, Box2: TBox3d): boolean;

{ This calculates maximum Sqr(distance) of 8 box points to the (0, 0, 0)
  point. This can be useful when you want to get bounding sphere,
  centered in (0, 0, 0), around this Box. }
function Box3dSqrRadius(const Box: TBox3d): Single;

{ Just like Box3dSqrRadius, but this returns correct value
  (not it's Sqr). Speed note: you pay here one Sqrt operation. }
function Box3dRadius(const Box: TBox3d): Single;

{ This is like projecting Box on XY plane (that is, we just ignore Z
  coords of Box points here), and then calculates maximum Sqr(distance)
  in plane XY of 4 Box corners to point (0, 0). }
function Box3dXYSqrRadius(const Box: TBox3d): Single;

{ Just like Box3dXYSqrRadius, but this returns correct value
  (not it's Sqr). Speed note: you pay here one Sqrt operation. }
function Box3dXYRadius(const Box: TBox3d): Single;

{ Check for collision betweeb box and sphere, fast @italic(but not
  entirely correct).

  This considers a Box enlarged by SphereRadius in each direction.
  Then checks whether SphereRadius is inside such enlarged Box.
  So this check will incorrectly report collision while in fact
  there's no collision in the case when the Sphere is very near
  the edge of the Box.

  So this check is not 100% correct. But often this is good enough
  --- in games, if you know that the SphereRadius is going to be
  relatively small compared to the Box, this may be perfectly
  acceptable. And it's fast. }
function Box3dSphereSimpleCollision(const Box: TBox3d;
  const SphereCenter: TVector3Single; const SphereRadius: Single): boolean;

{ Check for box <-> sphere collision.
  This is a little slower than Box3dSphereSimpleCollision, although still
  damn fast, and it's a precise check. }
function Box3dSphereCollision(const Box: TBox3d;
  const SphereCenter: TVector3Single; const SphereRadius: Single): boolean;

type
  TDynArrayItem_1 = TBox3d;
  PDynArrayItem_1 = PBox3d;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TArray_Box3d = TInfiniteArray_1;
  PArray_Box3d = PInfiniteArray_1;
  TDynBox3dArray = TDynArray_1;

{$undef read_interface}

implementation

{$define read_implementation}
{$I dynarray_1.inc}

function IsEmptyBox3d(const Box: TBox3d): boolean;
begin
 result := Box[0, 0] > Box[1, 0];
end;

function Box3d(const p0, p1: TVector3Single): TBox3d;
begin
 result[0] := p0;
 result[1] := p1;
end;

function Box3dOrderUp(const p0, p1: TVector3Single): TBox3d;
begin
 OrderUp(p0[0], p1[0], result[0, 0], result[1, 0]);
 OrderUp(p0[1], p1[1], result[0, 1], result[1, 1]);
 OrderUp(p0[2], p1[2], result[0, 2], result[1, 2]);
end;

function Box3dMiddle(const Box: TBox3d): TVector3Single;
begin
 Check(not IsEmptyBox3d(Box), 'Empty box 3d - no middle point');
 {petla for i := 0 to 2 rozwinieta aby zyskac tycityci na czasie}
 result[0] := (Box[0, 0]+Box[1, 0])/2;
 result[1] := (Box[0, 1]+Box[1, 1])/2;
 result[2] := (Box[0, 2]+Box[1, 2])/2;
end;

function Box3dAvgSize(const Box: TBox3d): Single;
begin
 Check(not IsEmptyBox3d(Box), 'Empty box 3d - no average size');
 {korzystamy z faktu ze Box3d ma wierzcholki uporzadkowane
  i w zwiazku z tym w roznicach ponizej nie musimy robic abs() }
 result := ((Box[1, 0]-Box[0, 0]) +
            (Box[1, 1]-Box[0, 1]) +
            (Box[1, 2]-Box[0, 2]))/3;
end;

function Box3dMaxSize(const box: TBox3d): Single;
var sizes: TVector3Single;
begin
 Check(not IsEmptyBox3d(Box), 'Empty box 3d - no maximum size');
 sizes := Box3dSizes(box);
 result := sizes[MaxVectorCoord(sizes)];
end;

function Box3dMinSize(const box: TBox3d): Single;
begin
 Check(not IsEmptyBox3d(Box), 'Empty box 3d - no maximum size');

 Result := Min(
   Box[1, 0] - Box[0, 0],
   Box[1, 1] - Box[0, 1],
   Box[1, 2] - Box[0, 2]);

 { Another version is below (but this is slower without any benefit...)

   var sizes: TVector3Single;
     sizes := Box3dSizes(box);
     result := sizes[MaxVectorCoord(sizes)];
 }
end;

function Box3dSizeX(const box: TBox3d): Single;
begin
  Check(not IsEmptyBox3d(Box), 'Empty box 3d - no size');
  Result := Box[1, 0] - Box[0, 0];
end;

function Box3dSizeY(const box: TBox3d): Single;
begin
  Check(not IsEmptyBox3d(Box), 'Empty box 3d - no size');
  Result := Box[1, 1] - Box[0, 1];
end;

function Box3dSizeZ(const box: TBox3d): Single;
begin
  Check(not IsEmptyBox3d(Box), 'Empty box 3d - no size');
  Result := Box[1, 2] - Box[0, 2];
end;

function Box3dCubeAroundPoint(const pt: TVector3Single; cubeSize: Single): TBox3d;
begin
 result[0] := VectorSubtract(pt, Vector3Single(-cubeSize, -cubeSize, -cubeSize));
 result[1] := VectorAdd(pt, Vector3Single(cubeSize, cubeSize, cubeSize));
end;

procedure BoxExpandTo1st(var Box: TBox3d; const Expand: Single);
begin
 Box[0, 0] -= Expand;
 Box[0, 1] -= Expand;
 Box[0, 2] -= Expand;

 Box[1, 0] += Expand;
 Box[1, 1] += Expand;
 Box[1, 2] += Expand;
end;

procedure BoxExpandTo1st(var box: TBox3d; const Expand: TVector3Single);
begin
 Box[0, 0] -= Expand[0];
 Box[0, 1] -= Expand[1];
 Box[0, 2] -= Expand[2];

 Box[1, 0] += Expand[0];
 Box[1, 1] += Expand[1];
 Box[1, 2] += Expand[2];
end;

function BoxExpand(var Box: TBox3d; const Expand: Single): TBox3d;
begin
  Result[0, 0] := Box[0, 0] - Expand;
  Result[0, 1] := Box[0, 1] - Expand;
  Result[0, 2] := Box[0, 2] - Expand;

  Result[1, 0] := Box[1, 0] + Expand;
  Result[1, 1] := Box[1, 1] + Expand;
  Result[1, 2] := Box[1, 2] + Expand;
end;

function BoxExpand(var box: TBox3d; const Expand: TVector3Single): TBox3d;
begin
  Result[0, 0] := Box[0, 0] - Expand[0];
  Result[0, 1] := Box[0, 1] - Expand[1];
  Result[0, 2] := Box[0, 2] - Expand[2];

  Result[1, 0] := Box[1, 0] + Expand[0];
  Result[1, 1] := Box[1, 1] + Expand[1];
  Result[1, 2] := Box[1, 2] + Expand[2];
end;

{$define Box3dPointInside_IMPLEMENT:=
begin
 if IsEmptyBox3d(box) then exit(false);
 result := Between(pt[0], box[0, 0], box[1, 0]) and
           Between(pt[1], box[0, 1], box[1, 1]) and
           Between(pt[2], box[0, 2], box[1, 2]);
end;}

function Box3dPointInside(const pt: TVector3Single; const box: TBox3d): boolean;
Box3dPointInside_IMPLEMENT

function Box3dPointInside(const pt: TVector3Double; const box: TBox3d): boolean;
Box3dPointInside_IMPLEMENT

{ MinSingleTo1st i MaxSingleTo1st will be useful for CalculateBoundingBox }
type
  TChooseOneTo1st_Single = procedure (var A: Single; const B: Single);

procedure MaxSingleTo1st(var A: Single; const B: Single);
begin
  if A < B then A := B;
end;

procedure MinSingleTo1st(var A: Single; const B: Single);
begin
  if A > B then A := B;
end;

function CalculateBoundingBox(
  GetVertex: TGetVertexFromIndexFunc;
  VertsCount: integer): TBox3d;

{ TODO: sprawdzic - czy jest realny sens w implementowaniu tu
  algorytmu MinMax ktory znajduje min i max jednoczesnie w czasie
  3/2*n zamiast 2*n ? }

  function find_extremum(ChooseFuncTo1st: TChooseOneTo1st_Single;
    coord: integer): Single;
  var i: integer;
  begin
   result := GetVertex(0)[coord];
   for i := 1 to VertsCount-1 do
     ChooseFuncTo1st(Result, GetVertex(i)[coord]);
  end;

begin
 if VertsCount = 0 then
 begin
  result := EmptyBox3d;
  exit
 end;

 result[0, 0] := find_extremum({$ifdef FPC_OBJFPC} @ {$endif} MinSingleTo1st, 0);
 result[0, 1] := find_extremum({$ifdef FPC_OBJFPC} @ {$endif} MinSingleTo1st, 1);
 result[0, 2] := find_extremum({$ifdef FPC_OBJFPC} @ {$endif} MinSingleTo1st, 2);
 result[1, 0] := find_extremum({$ifdef FPC_OBJFPC} @ {$endif} MaxSingleTo1st, 0);
 result[1, 1] := find_extremum({$ifdef FPC_OBJFPC} @ {$endif} MaxSingleTo1st, 1);
 result[1, 2] := find_extremum({$ifdef FPC_OBJFPC} @ {$endif} MaxSingleTo1st, 2);
end;

type
  {klasa do wewnetrznego uzytku w CalculateBoundingBox}
  TBBox_Calculator = class
    Verts: PVector3Single;
    VertsStride: Cardinal; { tutaj VertsStride juz nie moze byc = 0 }
    PMatrix: PMatrix4Single;
    function GetVertexNotTransform(index: integer): TVector3Single;
    function GetVertexTransform(index: integer): TVector3Single;
  end;

  function TBBox_Calculator.GetVertexNotTransform(index: integer): TVector3Single;
  begin
   result := PVector3Single(PointerAdd(Verts, VertsStride*Cardinal(index)))^;
  end;

  function TBBox_Calculator.GetVertexTransform(index: integer): TVector3Single;
  begin
   result := MultMatrixPoint(
     PMatrix^, PVector3Single(PointerAdd(Verts, VertsStride*Cardinal(index)))^ );
  end;

function CalculateBoundingBox(
  Verts: PVector3Single; VertsCount: Cardinal; VertsStride: Cardinal): TBox3d;
var Calculator: TBBox_Calculator;
begin
 if VertsStride = 0 then VertsStride := SizeOf(TVector3Single);
 Calculator := TBBox_Calculator.Create;
 try
  Calculator.VertsStride := VertsStride;
  Calculator.Verts := Verts;
  result := CalculateBoundingBox(
    {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertexNotTransform, VertsCount);
 finally Calculator.Free end;
end;

function CalculateBoundingBox(
  Verts: PVector3Single; VertsCount: Cardinal; VertsStride: Cardinal;
  const Transform: TMatrix4Single): TBox3d;
var Calculator: TBBox_Calculator;
begin
 if VertsStride = 0 then VertsStride := SizeOf(TVector3Single);
 Calculator := TBBox_Calculator.Create;
 try
  Calculator.VertsStride := VertsStride;
  Calculator.Verts := Verts;
  Calculator.PMatrix := @Transform;
  result := CalculateBoundingBox(
    {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetVertexTransform, VertsCount);
 finally Calculator.Free end;
end;

function CalculateBoundingBoxFromIndices(
  GetVertIndex: TGetIndexFromIndexNumFunc;
  VertsIndicesCount: integer;
  GetVertex: TGetVertexFromIndexFunc): TBox3d;
var
  { pozycja pierwszego nieujemnego indexu.
    Zwracamy EmptyBox3d wtw. gdy firstIndex nie istnieje }
  FirstIndexNum: integer;

  IndexNum, Index: integer;
  ThisVertex: TVector3Single;
begin
 {seek for firstIndex}
 firstIndexNum := 0;
 while (firstIndexNum < VertsIndicesCount) and (GetVertIndex(firstIndexNum) < 0) do
  Inc(firstIndexNum);

 if firstIndexNum = VertsIndicesCount then {firstIndex not found ?}
 begin
  result := EmptyBox3d;
  exit;
 end;

 { Note that I do only one pass, getting all vertexes.

   This is important, because GetVertex may be quite expensive
   operation (in case of e.g. TVertTransform_Calculator.GetTransformed,
   this is MultMatrixPoint for every vertex). At the beginning
   I implemented this by caling 6 time find_extremum function,
   and each call to find_extremum was iterating over every vertex.
   This was obviously wrong, because this caused calling GetVertex
   6 times more often than necessary. In some cases (like preparing
   TVRMLGLAnimation in "The Castle") this can cause really significant
   slowdown. }

 ThisVertex := GetVertex(GetVertIndex(firstIndexNum));
 Result[0] := ThisVertex;
 Result[1] := ThisVertex;
 for IndexNum := FirstIndexNum+1 to VertsIndicesCount - 1 do
 begin
   Index := GetVertIndex(IndexNum);
   if Index >= 0 then
   begin
     ThisVertex := GetVertex(Index);
     if ThisVertex[0] < Result[0, 0] then Result[0, 0] := ThisVertex[0];
     if ThisVertex[1] < Result[0, 1] then Result[0, 1] := ThisVertex[1];
     if ThisVertex[2] < Result[0, 2] then Result[0, 2] := ThisVertex[2];
     if ThisVertex[0] > Result[1, 0] then Result[1, 0] := ThisVertex[0];
     if ThisVertex[1] > Result[1, 1] then Result[1, 1] := ThisVertex[1];
     if ThisVertex[2] > Result[1, 2] then Result[1, 2] := ThisVertex[2];
   end;
 end;
end;

type
  TVertTransform_Calculator = class
    PTransform: PMatrix4Single;
    GetNotTransformed: TGetVertexFromIndexFunc;
    function GetTransformed(index: integer): TVector3Single;
  end;
  function TVertTransform_Calculator.GetTransformed(index: integer): TVector3Single;
  begin
   result := MultMatrixPoint(PTransform^, GetNotTransformed(index));
  end;

function CalculateBoundingBoxFromIndices(
  GetVertIndex: TGetIndexFromIndexNumFunc;
  VertsIndicesCount: integer;
  GetVertex: TGetVertexFromIndexFunc;
  const Transform: TMatrix4Single): TBox3d;
var Calculator: TVertTransform_Calculator;
begin
 Calculator := TVertTransform_Calculator.Create;
 try
  Calculator.PTransform := @Transform;
  Calculator.GetNotTransformed := GetVertex;
  result := CalculateBoundingBoxFromIndices(
    GetVertIndex,
    VertsIndicesCount,
    {$ifdef FPC_OBJFPC} @ {$endif} Calculator.GetTransformed);
 finally Calculator.Free end;
end;

function Box3dSum(const box1, box2: TBox3d): TBox3d;
begin
  if IsEmptyBox3d(box1) then
    Result := box2 else
  if IsEmptyBox3d(box2) then
    Result := box1 else
  begin
    result[0, 0] := min(box1[0, 0], box2[0, 0]);
    result[1, 0] := max(box1[1, 0], box2[1, 0]);
    result[0, 1] := min(box1[0, 1], box2[0, 1]);
    result[1, 1] := max(box1[1, 1], box2[1, 1]);
    result[0, 2] := min(box1[0, 2], box2[0, 2]);
    result[1, 2] := max(box1[1, 2], box2[1, 2]);
  end;
end;

procedure Box3dSumTo1st(var box1: TBox3d; const box2: TBox3d);
begin
  if IsEmptyBox3d(box2) then
    Exit else
  if IsEmptyBox3d(box1) then
    box1 := box2 else
  begin
    MinTo1st(box1[0, 0], box2[0, 0]);
    MaxTo1st(box1[1, 0], box2[1, 0]);
    MinTo1st(box1[0, 1], box2[0, 1]);
    MaxTo1st(box1[1, 1], box2[1, 1]);
    MinTo1st(box1[0, 2], box2[0, 2]);
    MaxTo1st(box1[1, 2], box2[1, 2]);
  end;
end;

procedure Box3dSumTo1st(var Box1: TBox3d; const Point: TVector3Single);
begin
  if IsEmptyBox3d(Box1) then
  begin
    Box1[0] := Point;
    Box1[1] := Point;
  end else
  begin
    MinTo1st(Box1[0, 0], Point[0]);
    MaxTo1st(Box1[1, 0], Point[0]);
    MinTo1st(Box1[0, 1], Point[1]);
    MaxTo1st(Box1[1, 1], Point[1]);
    MinTo1st(Box1[0, 2], Point[2]);
    MaxTo1st(Box1[1, 2], Point[2]);
  end;
end;

function Box3dSizes(const Box: TBox3d): TVector3Single;
begin
  Check(not IsEmptyBox3d(Box), 'Empty box 3d - no size');
  Result[0] := Box[1, 0] - Box[0, 0];
  Result[1] := Box[1, 1] - Box[0, 1];
  Result[2] := Box[1, 2] - Box[0, 2];
end;

procedure Box3dGetAllPoints(allpoints: PVector3Single; const box: TBox3d);
const
  {zapisy dwojkowe liczb od 0 do 7 czyli wszystkie kombinacje 0 i 1-nek
   na 3 pozycjach.}
  kombinacje: array[0..7, 0..2]of 0..1 =
  ((0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1), (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1));
var i, j: integer;
begin
 for i := 0 to 7 do
  for j := 0 to 2 do
   PArray_Vector3Single(allpoints)^[i][j] := box[kombinacje[i, j], j];
end;

function BoundingBoxTransform(const bbox: TBox3d; const Matrix: TMatrix4Single): TBox3d;
var
  BoxPoints: array [0..7] of TVector3Single;
  i: integer;
begin
  Box3dGetAllPoints(@boxpoints, bbox);
  for i := 0 to 7 do boxpoints[i] := MultMatrixPoint(Matrix, boxpoints[i]);

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

function Box3dTranslate(const Box: TBox3d;
  const Translation: TVector3Single): TBox3d;
begin
  if not IsEmptyBox3d(Box) then
  begin
    Result[0] := VectorAdd(Box[0], Translation);
    Result[1] := VectorAdd(Box[1], Translation);
  end else
    Result := EmptyBox3d;
end;

function Box3dAntiTranslate(const Box: TBox3d;
  const Translation: TVector3Single): TBox3d;
begin
  if not IsEmptyBox3d(Box) then
  begin
    Result[0] := VectorSubtract(Box[0], Translation);
    Result[1] := VectorSubtract(Box[1], Translation);
  end else
    Result := EmptyBox3d;
end;

function Box3dToNiceStr(const box: TBox3d): string;
begin
 if IsEmptyBox3d(box) then
  result := 'EMPTY' else
  result := VectorToNiceStr(box[0])+' - '+VectorToNiceStr(box[1]);
end;

function Box3dToRawStr(const box: TBox3d): string;
begin
 if IsEmptyBox3d(box) then
  result := 'EMPTY' else
  result := VectorToRawStr(box[0])+' - '+VectorToRawStr(box[1]);
end;

{$define CLAMP_IMPLEMENTATION:=
  var i: integer;
  begin
   for i := 0 to 2 do
   begin
    if point[i] < box[0, i] then point[i] := box[0, i] else
     if point[i] > box[1, i] then point[i] := box[1, i];
   end;
  end;}

procedure Box3dClamp(var point: TVector3Single; const box: TBox3d); CLAMP_IMPLEMENTATION
procedure Box3dClamp(var point: TVector3Double; const box: TBox3d); CLAMP_IMPLEMENTATION

function TriangleBoundingBox(const T: TTriangle3Single): TBox3d;
begin
  MinMax(T[0][0], T[1][0], T[2][0], Result[0][0], Result[1][0]);
  MinMax(T[0][1], T[1][1], T[2][1], Result[0][1], Result[1][1]);
  MinMax(T[0][2], T[1][2], T[2][2], Result[0][2], Result[1][2]);
end;

function TryBoxRayClosestIntersection(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const Box: TBox3d;
  const Ray0, RayVector: TVector3Single): boolean;
var
  IntrProposed: boolean absolute result;

  procedure ProposeBoxIntr(const PlaneConstCoord: integer;
    const PlaneConstValue: Single);
  var
    NowIntersection: TVector3Single;
    NowIntersectionDistance: Single;
    c1, c2: integer;
  begin
    if TrySimplePlaneRayIntersection(NowIntersection, NowIntersectionDistance,
      PlaneConstCoord, PlaneConstValue, Ray0, RayVector) then
    begin
      RestOf3dCoords(PlaneConstCoord, c1, c2);
      if Between(NowIntersection[c1], Box[0, c1], Box[1, c1]) and
         Between(NowIntersection[c2], Box[0, c2], Box[1, c2]) then
      begin
        if (not IntrProposed) or
           (NowIntersectionDistance < IntersectionDistance) then
        begin
          IntrProposed := true;
          Intersection := NowIntersection;
          IntersectionDistance := NowIntersectionDistance;
        end;
      end;
    end;
  end;

var
  I: integer;
begin
  IntrProposed := false;
  for I := 0 to 2 do
  begin
    { wykorzystujemy ponizej fakt ze jezeli Ray0[i] < Box[0, i] to na pewno
      promien ktory przecinalby scianke Box[1, i] pudelka przecinalby najpierw
      tez inna scianke. Wiec jezeli Ray0[i] < Box[0, i] to nie musimy sprawdzac
      przeciecia z plaszczyzna Box[1, i]. }
    if Ray0[i] < Box[0, i] then
      ProposeBoxIntr(i, Box[0, i]) else
    if Ray0[i] > Box[1, i] then
      ProposeBoxIntr(i, Box[1, i]) else
    begin
      ProposeBoxIntr(i, Box[0, i]);
      ProposeBoxIntr(i, Box[1, i]);
    end;
  end;
end;

function TryBoxRayClosestIntersection(
  out Intersection: TVector3Single;
  const Box: TBox3d;
  const Ray0, RayVector: TVector3Single): boolean;
var
  IntersectionDistance: Single;
begin
  Result := TryBoxRayClosestIntersection(
    Intersection, IntersectionDistance, Box, Ray0, RayVector);
end;

function TryBoxRayClosestIntersection(
  out IntersectionDistance: Single;
  const Box: TBox3d;
  const Ray0, RayVector: TVector3Single): boolean;
var
  Intersection: TVector3Single;
begin
  Result := TryBoxRayClosestIntersection(
    Intersection, IntersectionDistance, Box, Ray0, RayVector);
end;

function TryBoxRayEntrance(out Entrance: TVector3Single;
  const Box: TBox3d; const Ray0, RayVector: TVector3Single): boolean;
begin
 if Box3dPointInside(Ray0, Box) then
 begin
  Entrance := Ray0;
  result := true;
 end else
  result := TryBoxRayClosestIntersection(Entrance, Box, Ray0, RayVector);
end;

function IsBox3dSegmentCollision(
  const Box: TBox3d;
  const Segment1, Segment2: TVector3Single): boolean;

  function IsCollisionWithBoxPlane(const PlaneConstCoord: integer;
    const PlaneConstValue: Single): boolean;
  var
    NowIntersection: TVector3Single;
    c1, c2: integer;
  begin
    if TrySimplePlaneSegmentIntersection(NowIntersection,
      PlaneConstCoord, PlaneConstValue, Segment1, Segment2) then
    begin
      RestOf3dCoords(PlaneConstCoord, c1, c2);
      Result :=
        Between(NowIntersection[c1], Box[0, c1], Box[1, c1]) and
        Between(NowIntersection[c2], Box[0, c2], Box[1, c2]);
    end else
      Result := false;
  end;

var
  I: integer;
begin
  for I := 0 to 2 do
  begin
    { wykorzystujemy ponizej fakt ze jezeli Segment1[i] < Box[0, i] to na pewno
      promien ktory przecinalby scianke Box[1, i] pudelka przecinalby najpierw
      tez inna scianke. Wiec jezeli Segment1[i] < Box[0, i] to nie musimy sprawdzac
      przeciecia z plaszczyzna Box[1, i]. }
    if Segment1[i] < Box[0, i] then
    begin
      if IsCollisionWithBoxPlane(i, Box[0, i]) then Exit(true);
    end else
    if Segment1[i] > Box[1, i] then
    begin
      if IsCollisionWithBoxPlane(i, Box[1, i]) then Exit(true);
    end else
    begin
      if IsCollisionWithBoxPlane(i, Box[0, i]) then Exit(true);
      if IsCollisionWithBoxPlane(i, Box[1, i]) then Exit(true);
    end;
  end;

  Result := false;
end;

function IsBox3dPlaneCollision(const Box3d: TBox3d;
  const Plane: TVector4Single): boolean;
var
  BoxCenter, BoxHalfSize: TVector3Single;
  PlaneMoved: TVector4Single;
  I: Integer;
begin
  if IsEmptyBox3d(Box3d) then
    Exit(false);

  { calculate BoxCenter and BoxHalfSize }
  for I := 0 to 2 do
  begin
    BoxCenter[I] := (Box3d[0, I] + Box3d[1, I]) / 2;
    BoxHalfSize[I] := (Box3d[1, I] - Box3d[0, I]) / 2;
  end;

  { calculate PlaneMoved = Plane moved by -BoxCenter }
  PlaneMoved := PlaneAntiMove(Plane, BoxCenter);

  Result := IsCenteredBox3dPlaneCollision(BoxHalfSize, PlaneMoved);
end;

{$define TGenericFloat := Single}
{$define TVector3GenericFloat := TVector3Single}
{$define TVector4GenericFloat := TVector4Single}
{$I boxes3d_generic_float.inc}

{$define TGenericFloat := Double}
{$define TVector3GenericFloat := TVector3Double}
{$define TVector4GenericFloat := TVector4Double}
{$I boxes3d_generic_float.inc}

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

const
  { The same comments about precision as for IsCenteredBox3dPlaneCollision apply also here. }
  EqualityEpsilon = 1e-5;

var
  TriangleMoved: TTriangle3Double;
  BoxHalfSize: TVector3Double;

  { ======================== X-tests ======================== }
  function AXISTEST_X01(const a, b, fa, fb: Double): boolean;
  var
    p0, p2, rad, min, max: Double;
  begin
    p0 := a * TriangleMoved[0][1] - b * TriangleMoved[0][2];
    p2 := a * TriangleMoved[2][1] - b * TriangleMoved[2][2];
    if p0<p2 then begin min := p0; max := p2; end else
                  begin min := p2; max := p0; end;
    rad := fa * BoxHalfSize[1] + fb * BoxHalfSize[2];
    Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
  end;

  function AXISTEST_X2(const a, b, fa, fb: Double): boolean;
  var
    p0, p1, rad, min, max: Double;
  begin
    p0 := a * TriangleMoved[0][1] - b * TriangleMoved[0][2];
    p1 := a * TriangleMoved[1][1] - b * TriangleMoved[1][2];
    if p0<p1 then begin min := p0; max := p1; end else
                  begin min := p1; max := p0; end;
    rad := fa * BoxHalfSize[1] + fb * BoxHalfSize[2];
    Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
  end;

  { ======================== Y-tests ======================== }
  function AXISTEST_Y02(const a, b, fa, fb: Double): boolean;
  var
    p0, p2, rad, min, max: Double;
  begin
    p0 := -a * TriangleMoved[0][0] + b * TriangleMoved[0][2];
    p2 := -a * TriangleMoved[2][0] + b * TriangleMoved[2][2];
    if p0<p2 then begin min := p0; max := p2; end else
                  begin min := p2; max := p0; end;
    rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[2];
    Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
  end;

  function AXISTEST_Y1(const a, b, fa, fb: Double): boolean;
  var
    p0, p1, rad, min, max: Double;
  begin
    p0 := -a * TriangleMoved[0][0] + b * TriangleMoved[0][2];
    p1 := -a * TriangleMoved[1][0] + b * TriangleMoved[1][2];
    if p0<p1 then begin min := p0; max := p1; end else
                  begin min := p1; max := p0; end;
    rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[2];
    Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
  end;

  { ======================== Z-tests ======================== }
  function AXISTEST_Z12(const a, b, fa, fb: Double): boolean;
  var
    p1, p2, rad, min, max: Double;
  begin
    p1 := a * TriangleMoved[1][0] - b * TriangleMoved[1][1];
    p2 := a * TriangleMoved[2][0] - b * TriangleMoved[2][1];
    if p2<p1 then begin min := p2; max := p1; end else
                  begin min := p1; max := p2; end;
    rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[1];
    Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
  end;

  function AXISTEST_Z0(const a, b, fa, fb: Double): boolean;
  var
    p0, p1, rad, min, max: Double;
  begin
    p0 := a * TriangleMoved[0][0] - b * TriangleMoved[0][1];
    p1 := a * TriangleMoved[1][0] - b * TriangleMoved[1][1];
    if p0<p1 then begin min := p0; max := p1; end else
                  begin min := p1; max := p0; end;
    rad := fa * BoxHalfSize[0] + fb * BoxHalfSize[1];
    Result := (min > rad + EqualityEpsilon) or (max < -rad - EqualityEpsilon);
  end;

var
  BoxCenter: TVector3Double;
  I: Integer;
  TriangleEdges: array [0..2] of TVector3Double;
  EdgeAbs: TVector3Double;
  min, max: Double;
  Plane: TVector4Double;
  PlaneDir: TVector3Double absolute Plane;
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
  TriangleMoved[0] := VectorSubtract(Vector3Double(Triangle[0]), BoxCenter);
  TriangleMoved[1] := VectorSubtract(Vector3Double(Triangle[1]), BoxCenter);
  TriangleMoved[2] := VectorSubtract(Vector3Double(Triangle[2]), BoxCenter);

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
  MinMax(TriangleMoved[0][0], TriangleMoved[1][0], TriangleMoved[2][0], min, max);
  if (min >  boxhalfsize[0] + EqualityEpsilon) or
     (max < -boxhalfsize[0] - EqualityEpsilon) then Exit(false);

  { test in Y-direction }
  MinMax(TriangleMoved[0][1], TriangleMoved[1][1], TriangleMoved[2][1], min, max);
  if (min >  boxhalfsize[1] + EqualityEpsilon) or
     (max < -boxhalfsize[1] - EqualityEpsilon) then Exit(false);

  { test in Z-direction }
  MinMax(TriangleMoved[0][2], TriangleMoved[1][2], TriangleMoved[2][2], min, max);
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

function FrustumBox3dCollisionPossible(const Frustum: TFrustum;
  const Box: TBox3d): TFrustumCollisionPossible;

{ Note: I tried to optimize this function,
  since it's crucial for TOctree.EnumerateCollidingOctreeItems,
  and this is crucial for TVRMLFlatSceneGL.RenderFrustumOctree,
  and this is crucial for overall speed of rendering. }

var
  fp: TFrustumPlane;
  FrustumMultiplyBox: TBox3d;

  function CheckOutsideCorner(const XIndex, YIndex, ZIndex: Cardinal): boolean;
  begin
   Result :=
     { Frustum[fp][0] * Box[XIndex][0] +
       Frustum[fp][1] * Box[YIndex][1] +
       Frustum[fp][2] * Box[ZIndex][2] +
       optimized version : }
     FrustumMultiplyBox[XIndex][0] +
     FrustumMultiplyBox[YIndex][1] +
     FrustumMultiplyBox[ZIndex][2] +
     Frustum[fp][3] < 0;
  end;

var
  InsidePlanesCount: Cardinal;
  LastPlane: TFrustumPlane;
begin
  InsidePlanesCount := 0;

  LastPlane := High(FP);
  Assert(LastPlane = fpFar);

  { If the frustum has far plane in infinity, then ignore this plane.
    Inc InsidePlanesCount, since the box is inside this infinite plane. }
  if (Frustum[fpFar][0] = 0) and
     (Frustum[fpFar][1] = 0) and
     (Frustum[fpFar][2] = 0) then
  begin
    LastPlane := Pred(LastPlane);
    Inc(InsidePlanesCount);
  end;

  { The logic goes like this:
      if box is on the "outside" of *any* of 6 planes, result is NoCollision
      if box is on the "inside" of *all* 6 planes, result is InsideFrustum
      else SomeCollisionPossible. }

  for fp := Low(fp) to LastPlane do
  begin
   { This way I need 6 multiplications instead of 8*3=24
     (in case I would have to execute CheckOutsideCorner 8 times) }
   FrustumMultiplyBox[0][0] := Frustum[fp][0] * Box[0][0];
   FrustumMultiplyBox[0][1] := Frustum[fp][1] * Box[0][1];
   FrustumMultiplyBox[0][2] := Frustum[fp][2] * Box[0][2];
   FrustumMultiplyBox[1][0] := Frustum[fp][0] * Box[1][0];
   FrustumMultiplyBox[1][1] := Frustum[fp][1] * Box[1][1];
   FrustumMultiplyBox[1][2] := Frustum[fp][2] * Box[1][2];

   { I'm splitting code below to two possilibilities.
     This way I can calculate 7 remaining CheckOutsideCorner
     calls using code  like
       "... and ... and ..."
     or
       "... or ... or ..."
     , and this means that short-circuit boolean evaluation
     may usually reduce number of needed CheckOutsideCorner calls
     (i.e. I will not need to actually call CheckOutsideCorner 8 times
     per frustum plane). }

   if CheckOutsideCorner(0, 0, 0) then
   begin
    if CheckOutsideCorner(0, 0, 1) and
       CheckOutsideCorner(0, 1, 0) and
       CheckOutsideCorner(0, 1, 1) and
       CheckOutsideCorner(1, 0, 0) and
       CheckOutsideCorner(1, 0, 1) and
       CheckOutsideCorner(1, 1, 0) and
       CheckOutsideCorner(1, 1, 1) then
     { All 8 corners outside }
     Exit(fcNoCollision);
   end else
   begin
    if not (
       CheckOutsideCorner(0, 0, 1) or
       CheckOutsideCorner(0, 1, 0) or
       CheckOutsideCorner(0, 1, 1) or
       CheckOutsideCorner(1, 0, 0) or
       CheckOutsideCorner(1, 0, 1) or
       CheckOutsideCorner(1, 1, 0) or
       CheckOutsideCorner(1, 1, 1) ) then
     { All 8 corners inside }
     Inc(InsidePlanesCount);
   end;
  end;

  if InsidePlanesCount = 6 then
    Result := fcInsideFrustum else
    Result := fcSomeCollisionPossible;
end;

function FrustumBox3dCollisionPossibleSimple(const Frustum: TFrustum;
  const Box: TBox3d): boolean;

{ Implementation is obviously based on
  FrustumBox3dCollisionPossible above, see there for more comments. }

var
  fp: TFrustumPlane;
  FrustumMultiplyBox: TBox3d;

  function CheckOutsideCorner(const XIndex, YIndex, ZIndex: Cardinal): boolean;
  begin
   Result :=
     { Frustum[fp][0] * Box[XIndex][0] +
       Frustum[fp][1] * Box[YIndex][1] +
       Frustum[fp][2] * Box[ZIndex][2] +
       optimized version : }
     FrustumMultiplyBox[XIndex][0] +
     FrustumMultiplyBox[YIndex][1] +
     FrustumMultiplyBox[ZIndex][2] +
     Frustum[fp][3] < 0;
  end;

var
  LastPlane: TFrustumPlane;
begin
  LastPlane := High(FP);
  Assert(LastPlane = fpFar);

  { If the frustum has far plane in infinity, then ignore this plane. }
  if (Frustum[fpFar][0] = 0) and
     (Frustum[fpFar][1] = 0) and
     (Frustum[fpFar][2] = 0) then
    LastPlane := Pred(LastPlane);

  for fp := Low(fp) to LastPlane do
  begin
    { This way I need 6 multiplications instead of 8*3=24 }
    FrustumMultiplyBox[0][0] := Frustum[fp][0] * Box[0][0];
    FrustumMultiplyBox[0][1] := Frustum[fp][1] * Box[0][1];
    FrustumMultiplyBox[0][2] := Frustum[fp][2] * Box[0][2];
    FrustumMultiplyBox[1][0] := Frustum[fp][0] * Box[1][0];
    FrustumMultiplyBox[1][1] := Frustum[fp][1] * Box[1][1];
    FrustumMultiplyBox[1][2] := Frustum[fp][2] * Box[1][2];

    if CheckOutsideCorner(0, 0, 0) and
       CheckOutsideCorner(0, 0, 1) and
       CheckOutsideCorner(0, 1, 0) and
       CheckOutsideCorner(0, 1, 1) and
       CheckOutsideCorner(1, 0, 0) and
       CheckOutsideCorner(1, 0, 1) and
       CheckOutsideCorner(1, 1, 0) and
       CheckOutsideCorner(1, 1, 1) then
      Exit(false);
  end;

  Result := true;
end;

procedure Box3dBoundingSphere(const Box3d: TBox3d;
  var SphereCenter: TVector3Single; var SphereRadiusSqr: Single);
begin
 if IsEmptyBox3d(Box3d) then
 begin
  SphereRadiusSqr := 0;
 end else
 begin
  SphereCenter := Box3dMiddle(Box3d);
  SphereRadiusSqr := PointsDistanceSqr(SphereCenter, Box3d[0]);
 end;
end;

function Boxes3dCollision(const Box1, Box2: TBox3d): boolean;
begin
  Result :=
    (not IsEmptyBox3d(Box1)) and
    (not IsEmptyBox3d(Box2)) and
    (not ((Box1[1, 0] < Box2[0, 0]) or (Box2[1, 0] < Box1[0, 0]))) and
    (not ((Box1[1, 1] < Box2[0, 1]) or (Box2[1, 1] < Box1[0, 1]))) and
    (not ((Box1[1, 2] < Box2[0, 2]) or (Box2[1, 2] < Box1[0, 2])));
end;

function Boxes3dXYCollision(const Box1, Box2: TBox3d): boolean;
begin
  Result :=
    (not IsEmptyBox3d(Box1)) and
    (not IsEmptyBox3d(Box2)) and
    (not ((Box1[1, 0] < Box2[0, 0]) or (Box2[1, 0] < Box1[0, 0]))) and
    (not ((Box1[1, 1] < Box2[0, 1]) or (Box2[1, 1] < Box1[0, 1])));
end;

function Box3dSqrRadius(const Box: TBox3d): Single;
begin
  Result := Max(
    Max(Max(VectorLenSqr(Vector3Single(Box[0, 0], Box[0, 1], Box[0, 2])),
            VectorLenSqr(Vector3Single(Box[1, 0], Box[0, 1], Box[0, 2]))),
        Max(VectorLenSqr(Vector3Single(Box[1, 0], Box[1, 1], Box[0, 2])),
            VectorLenSqr(Vector3Single(Box[0, 0], Box[1, 1], Box[0, 2])))),
    Max(Max(VectorLenSqr(Vector3Single(Box[0, 0], Box[0, 1], Box[1, 2])),
            VectorLenSqr(Vector3Single(Box[1, 0], Box[0, 1], Box[1, 2]))),
        Max(VectorLenSqr(Vector3Single(Box[1, 0], Box[1, 1], Box[1, 2])),
            VectorLenSqr(Vector3Single(Box[0, 0], Box[1, 1], Box[1, 2])))));
end;

function Box3dRadius(const Box: TBox3d): Single;
begin
  Result := Sqrt(Box3dSqrRadius(Box));
end;

function Box3dXYSqrRadius(const Box: TBox3d): Single;
begin
  Result := Max(
    Max(VectorLenSqr(Vector2Single(Box[0, 0], Box[0, 1])),
        VectorLenSqr(Vector2Single(Box[1, 0], Box[0, 1]))),
    Max(VectorLenSqr(Vector2Single(Box[1, 0], Box[1, 1])),
        VectorLenSqr(Vector2Single(Box[0, 0], Box[1, 1]))));
end;

function Box3dXYRadius(const Box: TBox3d): Single;
begin
  Result := Sqrt(Box3dXYSqrRadius(Box));
end;

function Box3dSphereSimpleCollision(const Box: TBox3d;
  const SphereCenter: TVector3Single; const SphereRadius: Single): boolean;
begin
  Result := (not IsEmptyBox3d(Box)) and
    (SphereCenter[0] >= Box[0][0] - SphereRadius) and
    (SphereCenter[0] <= Box[1][0] + SphereRadius) and
    (SphereCenter[1] >= Box[0][1] - SphereRadius) and
    (SphereCenter[1] <= Box[1][1] + SphereRadius) and
    (SphereCenter[2] >= Box[0][2] - SphereRadius) and
    (SphereCenter[2] <= Box[1][2] + SphereRadius);
end;

function Box3dSphereCollision(const Box: TBox3d;
  const SphereCenter: TVector3Single; const SphereRadius: Single): boolean;
{ This great and simple algorithm  was invented by Arvo, I read about
  it in "Real-Time Rendering" by Moller and Haines.
  The idea is beatifully simple: we can easily find point on the Box
  that is closest to SphereCenter: on each of X, Y, Z axis,
  1. SphereCenter[I] is within Box, so distance on this axis is 0
  2. SphereCenter[I] is not within Box, so the closest point is taken
     from appropriate box corner
  Then just compare distance between these points and radius.

  Implementation below is low-optimized: we actually calculate
  distance, d, as we go (we don't keep explicitly our "closest point",
  although we think about calculating it). And loop over three planes
  is unfolded to be sure. }
var
  D: Single;
begin
  if IsEmptyBox3d(Box) then Exit(false);

  D := 0;

  { Uses:
    4 up to 7 comparisons,
    6 additions,
    4 multiplications.

    Ok, that's damn fast, but still a little slower than
    Box3dSphereSimpleCollision (that has 1 up to 6 comparisons and additions). }

  if SphereCenter[0] < Box[0][0] then D += Sqr(SphereCenter[0] - Box[0][0]) else
  if SphereCenter[0] > Box[1][0] then D += Sqr(SphereCenter[0] - Box[1][0]);

  if SphereCenter[1] < Box[0][1] then D += Sqr(SphereCenter[1] - Box[0][1]) else
  if SphereCenter[1] > Box[1][1] then D += Sqr(SphereCenter[1] - Box[1][1]);

  if SphereCenter[2] < Box[0][2] then D += Sqr(SphereCenter[2] - Box[0][2]) else
  if SphereCenter[2] > Box[1][2] then D += Sqr(SphereCenter[2] - Box[1][2]);

  Result := D <= Sqr(SphereRadius);
end;

end.
