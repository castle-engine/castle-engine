{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Axis-aligned 3D boxes (TBox3D). }
unit CastleBoxes;

{$I castleconf.inc}

interface

uses CastleVectors, SysUtils, CastleUtils, CastleGenericLists, CastleTriangles,
  CastleRectangles;

type
  EBox3DEmpty = class(Exception);

  TGetIndexFromIndexNumFunc = function (indexNum: integer): integer of object;

  { State of collision between a plane and some other object.

    pcNone occurs only when the "other object" is empty
    (TBox3D.IsEmpty, in case of box).
    Other values mean that the other object is not empty.

    pcOutside means that the whole object is on the side of the plane pointed
    by plane direction (normal) vector. More formally, every point P
    inserted into the plane equation will yield (P*PlaneNormal + PlaneD) > 0.

    pcInside is the reverse of pcOutside:
    the other object is on the side of plane
    pointed by inverted plane normal. Every point inserted into plane
    equation will yield < 0.

    pcIntersecting is, well, the remaining case. It means that there's
    for sure some point P of other object that, when inserted into
    plane equation, will yield = 0. }
  TPlaneCollision = (pcIntersecting, pcOutside, pcInside, pcNone);

  TBoxCorners = array [0..7] of TVector3Single;

  { Various ways to sort the 3D objects, in particular useful to correctly
    render the partially-transparent objects.
    @seealso TSceneRenderingAttributes.BlendingSort }
  TBlendingSort = (
    { Do not sort.
      Using this for @link(TSceneRenderingAttributes.BlendingSort Scene.Attributes.BlendingSort)
      is fastest, but will cause artifacts if multiple
      partially-transparent objects may be visible on top of each other. }
    bsNone,

    { Sort objects by their Z coordinate.
      Using this for @link(TSceneRenderingAttributes.BlendingSort Scene.Attributes.BlendingSort)
      is very useful for 2D worlds, with flat 2D objects
      that have zero (or near-zero) size in the Z axis,
      and they are moved in the Z axis to specify which is on top for another.

      More precisely, we take the minimum bounding box Z coordinate
      of two objects. (We don't bother calculating the middle Z coordinate,
      as we assume that the bounding box is infinitely small along the Z axis.)
      The one with @italic(larger) Z coordinate is considered to be
      @italic(closer), this is consistent with the right-handed coordinate system.

      Note that the actual camera position doesn't matter for this sorting.
      So the 2D object will look OK, @italic(even if viewed from an angle,
      even if viewed from the other side). }
    bs2D,

    { Sort objects by the (3D) distance to the camera.
      Using this for @link(TSceneRenderingAttributes.BlendingSort Scene.Attributes.BlendingSort)
      is the best sorting method for 3D
      scenes with many partially-transparent objects.

      The distance is measured from the middle
      of the bounding box to the camera posotion. }
    bs3D);

  { Axis-aligned box. Rectangular prism with all sides parallel to basic planes
    X = 0, Y = 0 and Z = 0. This is sometimes called AABB, "axis-aligned bounding
    box". Many geometric operations are fast and easy on this type.

    The actual box dimensions are stored inside the @link(Data) field, as two 3D points.
    First point has always all the smaller coords, second point has all
    the larger coords. I.e. always

    @preformatted(
      Data[0, 0] <= Data[1, 0] and
      Data[0, 1] <= Data[1, 1] and
      Data[0, 2] <= Data[1, 2]
    )

    The only exception is the special value EmptyBox3D.

    Note that the box may still have all sizes equal 0. Consider a 3D model with
    only a single 3D point --- it's not empty, but all the sizes must be 0.

    This is an old-style object (withut any virtual
    methods). This way there's no need for using constructors / destructors
    to manage this, you can simply declare TBox3D type and copy / pass around
    this box to other procedures. }
  TBox3D = object
    Data: array [0..1] of TVector3Single;

    const
      { Special TBox3D value meaning "bounding box is empty".
        This is different than just bounding box with zero sizes,
        as bounding box with zero sizes still has some position.
        Empty bounding box doesn't contain any portion of 3D space. }
      Empty: TBox3D = (Data: ((0, 0, 0), (-1, -1, -1)));

    { Check is box empty.
      You can think of this function as "compare Box with EmptyBox3D".

      But actually it works a little faster, by utilizing the assumption
      that EmptyBox3D is the only allowed value that breaks
      @code(Data[0, 0] <= Data[1, 0]) rule. }
    function IsEmpty: boolean;

    { Check is box empty or has all the sizes equal 0. }
    function IsEmptyOrZero: boolean;

    procedure CheckNonEmpty;

    { Center of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function Middle: TVector3Single; deprecated 'use Center';

    { Center of the box.
      Name consistent with e.g. @link(TAbstractX3DGroupingNode.BboxCenter).
      @raises(EBox3DEmpty If the Box is empty.) }
    function Center: TVector3Single;

    { Average size of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function AverageSize: Single;

    { Largest size of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function MaxSize: Single;

    { Smallest size of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function MinSize: Single;

    { Size in X (width) of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function SizeX: Single;

    { Size in Y (height) of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function SizeY: Single;

    { Size in Z (depth) of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function SizeZ: Single;

    { Average size of TBox3D, or EmptyBoxSize if box is empty.
      @param(AllowZero Decides what to do when box is not empty but the result
        would be zero, which means that the box is infinitely thin in all axes.
        If @true, then result is just 0, otherwise it's EmptyBoxSize.) }
    function AverageSize(const AllowZero: boolean;
      const EmptyBoxSize: Single): Single; overload;

    { Largest size of TBox3D, or EmptyBoxSize if box is empty.
      @param(AllowZero Decides what to do when box is not empty but the result
        would be zero, which means that the box is infinitely thin in all axes.
        If @true, then result is just 0, otherwise it's EmptyBoxSize.) }
    function MaxSize(const AllowZero: boolean;
      const EmptyBoxSize: Single): Single; overload;

    { Area of the six TBox3D sides, EmptyBoxArea if box is empty.
      @param(AllowZero Decides what to do when box is not empty but the result
        would be zero, which means that the box is infinitely thin in all axes.
        If @true, then result is just 0, otherwise it's EmptyBoxSize.) }
    function Area(const AllowZero: boolean;
      const EmptyBoxArea: Single): Single;

    { This decreases Data[0, 0], Data[0, 1], Data[0, 2] by AExpand
       and increases Data[1, 0], Data[1, 1], Data[1, 2] by AExpand.
      So you get Box with all sizes increased by 2 * AExpand.

      Box must not be empty.
      Note that AExpand may be negative, but then you must be sure
      that it doesn't make Box empty. }
    procedure ExpandMe(const AExpand: Single); overload;

    { This decreases Data[0] by AExpand, and increases Data[1] by AExpand.
      So you get Box with all sizes increased by 2 * AExpand.

      Box must not be empty.
      Note that AExpand may be negative, but then you must be sure
      that it doesn't make Box empty. }
    procedure ExpandMe(const AExpand: TVector3Single); overload;

    function Expand(const AExpand: Single): TBox3D; overload;
    function Expand(const AExpand: TVector3Single): TBox3D; overload;

    { Check is the point inside the box.
      Always false if Box is empty (obviously, no point is inside an empty box).

      @groupBegin }
    function PointInside(const Point: TVector3Single): boolean; overload;
    function PointInside(const Point: TVector3Double): boolean; overload;
    { @groupEnd }

    { Is the 2D point inside the 2D projection of the box, ignores the Z coord of box. }
    function PointInside2D(const Point: TVector2Single): boolean;

    { Is the 2D point inside the 2D projection of the box.
      2D projection (of point and box) is obtained by rejecting
      the IgnoreIndex coordinate (must be 0, 1 or 2). }
    function PointInside2D(const Point: TVector3Single; const IgnoreIndex: Integer): boolean;

    { Sum two TBox3D values. This calculates the smallest box that encloses
      both Box1 and Box2. You can also use + operator. }
    procedure Add(const box2: TBox3D); overload;

    { Make box larger, if necessary, to contain given Point. }
    procedure Add(const Point: TVector3Single); overload;

    { Three box sizes. }
    function Sizes: TVector3Single; deprecated 'use Size';

    { Three box sizes. Name consistent with TBoxNode.Size.
      @raises(EBox3DEmpty If the Box is empty.) }
    function Size: TVector3Single;

    { Calculate eight corners of the box.}
    procedure Corners(var AllPoints: TBoxCorners);
    procedure GetAllPoints(AllPoints: PVector3Single); deprecated 'use Corners';

    { Transform the Box by given matrix.
      Since this is still an axis-aligned box, rotating etc. of the box
      usually makes larger box.

      Note that this is very optimized for Matrix with no projection
      (where last row of the last matrix = [0, 0, 0, 1]). It still works
      for all matrices (eventually fallbacks to simple "transform 8 corners and get
      box enclosing them" method).

      @raises(ETransformedResultInvalid When the Matrix will
      transform some point to a direction (vector with 4th component
      equal zero). In this case we just cannot interpret the result as a 3D point,
      so we also cannot interpret the final result as a box.) }
    function Transform(const Matrix: TMatrix4Single): TBox3D;

    { Move Box. Does nothing if Box is empty. }
    function Translate(const Translation: TVector3Single): TBox3D;

    { Move Box, by -Translation. Does nothing if Box is empty. }
    function AntiTranslate(const Translation: TVector3Single): TBox3D;

    function ToNiceStr: string;
    function ToRawStr: string;

    procedure ClampVar(var point: TVector3Single); overload;
    procedure ClampVar(var point: TVector3Double); overload;

    { TryBoxRayClosestIntersection calculates intersection between the
      ray (returns closest intersection to RayOrigin) and the box.

      The box is treated just like a set of 6 rectangles in 3D.
      This means that the intersection will always be placed on one of the
      box sides, even if RayOrigin starts inside the box.
      See TryBoxRayEntrance for the other version.

      Returns also IntersectionDistance, which is the distance to the Intersection
      relative to RayDirection (i.e. Intersection is always = RayOrigin +
      IntersectionDistance * RayDirection).

      @groupBegin }
    function TryRayClosestIntersection(
      out Intersection: TVector3Single;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single): boolean; overload;
    function TryRayClosestIntersection(
      out Intersection: TVector3Single;
      const RayOrigin, RayDirection: TVector3Single): boolean; overload;
    function TryRayClosestIntersection(
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3Single): boolean; overload;
    { @groupEnd }

    { Intersection between the ray (returns closest intersection to RayOrigin)
      and the box, treating the box as a filled volume.

      If RayOrigin is inside the box, TryBoxRayEntrance simply returns
      RayOrigin. If RayOrigin is outside of the box, the answer is the same
      as with TryBoxRayClosestIntersection.

      @groupBegin }
    function TryRayEntrance(
      out Entrance: TVector3Single; out EntranceDistance: Single;
      const RayOrigin, RayDirection: TVector3Single): boolean; overload;
    function TryRayEntrance(
      out Entrance: TVector3Single;
      const RayOrigin, RayDirection: TVector3Single): boolean; overload;
    { @groupEnd }

    function SegmentCollision(
      const Segment1, Segment2: TVector3Single): boolean;

    { Deprecated name for SegmentCollision. @deprecated @exclude }
    function IsSegmentCollision(
      const Segment1, Segment2: TVector3Single): boolean; deprecated;

    { Collision between axis-aligned box (TBox3D) and 3D plane.
      Returns detailed result as TPlaneCollision. }
    function PlaneCollision(const Plane: TVector4Single): TPlaneCollision;

    { Check is axis-aligned box (TBox3D) fully inside/outside the plane.

      Inside/outside are defined as for TPlaneCollision:
      Outside is where plane direction (normal) points.
      Inside is where the @italic(inverted) plane direction (normal) points.

      They work exactly like Box3DPlaneCollision, except they returns @true
      when box is inside/outside (when Box3DPlaneCollision returned pcInside/pcOutside),
      and @false otherwise.

      For example Box3DPlaneCollisionInside doesn't differentiate between case
      when box is empty, of partially intersects the plane, and is on the outside.
      But it works (very slightly) faster.

      @groupBegin }
    function PlaneCollisionInside(const Plane: TVector4Single): boolean;
    function PlaneCollisionOutside(const Plane: TVector4Single): boolean;
    { @groupEnd }

    function IsTriangleCollision(
      const Triangle: TTriangle3Single): boolean;

    { Smallest possible sphere completely enclosing given Box.
      When Box is empty we return SphereRadiusSqr = 0 and undefined SphereCenter. }
    procedure BoundingSphere(
      var SphereCenter: TVector3Single; var SphereRadiusSqr: Single);

    function Collision(const Box2: TBox3D): boolean;

    { Radius of the minimal sphere that contains this box.
      Sphere center is assumed to be in (0, 0, 0).
      0 if box is empty. }
    function Radius: Single;

    { Radius of the minimal circle that contains the 2D projection of this box.
      2D box projection is obtained by rejecting the IgnoreIndex coordinate
      (must be 0, 1 or 2).
      Circle center is assumed to be in (0, 0).
      0 if box is empty. }
    function Radius2D(const IgnoreIndex: Integer): Single;

    { Check for collision between box and sphere, fast @italic(but not
      entirely correct).

      This considers a Box enlarged by SphereRadius in each direction.
      Then checks whether SphereCenter is inside such enlarged Box.
      So this check will incorrectly report collision while in fact
      there's no collision in the case when the sphere center is near
      the corner of the Box.

      So this check is not 100% correct. But often this is good enough
      --- in games, if you know that the SphereRadius is going to be
      relatively small compared to the Box, this may be perfectly
      acceptable. And it's fast. }
    function SphereSimpleCollision(
      const SphereCenter: TVector3Single; const SphereRadius: Single): boolean;

    { Check box vs sphere collision. }
    function SphereCollision(
      const SphereCenter: TVector3Single; const SphereRadius: Single): boolean;

    { Check box vs sphere collision in 2D (ignores Z coordinates of box). }
    function SphereCollision2D(
      const SphereCenter: TVector2Single; const SphereRadius: Single): boolean;

    { Calculate a plane in 3D space with direction = given Direction, moved
      maximally in Direction and still intersecting the given Box.

      For example, if Direction = -Z = (0, 0, -1), then this will return
      the bottom plane of this box. For Direction = (1, 1, 1), this will return
      a plane intersecting the Data[1] (maximum) point, with slope = (1, 1, 1).
      The resulting plane always intersects at least one of the 8 corners of the box.

      @raises(EBox3DEmpty If the Box is empty.) }
    function MaximumPlane(const Direction: TVector3Single): TVector4Single;

    { Calculate a plane in 3D space with direction = given Direction, moved
      such that it touches the Box but takes minimum volume of this box.

      For example, if Direction = +Z = (0, 0, 1), then this will return
      the bottom plane of this box. For Direction = (1, 1, 1), this will return
      a plane intersecting the Data[0] (minimum) point, with slope = (1, 1, 1).
      The resulting plane always intersects at least one of the 8 corners of the box.

      @raises(EBox3DEmpty If the Box is empty.) }
    function MinimumPlane(const Direction: TVector3Single): TVector4Single;

    { Farthest corner of the box in the given Direction.
      @raises(EBox3DEmpty If the Box is empty.) }
    function MaximumCorner(const Direction: TVector3Single): TVector3Single;

    { Corner of the box such that the rest of the box lies in the given
      Direction from this corner.
      @raises(EBox3DEmpty If the Box is empty.) }
    function MinimumCorner(const Direction: TVector3Single): TVector3Single;

    { Calculate the distances between a given 3D point and a box.
      MinDistance is the distance to the closest point of the box,
      MaxDistance is the distance to the farthest point of the box.

      Note that always MinDistance <= MaxDistance.
      Note that both distances are always >= 0.

      When the point is inside the box, it works correct too: minimum distance
      is zero in this case.

      @raises EBox3DEmpty When used with an empty box.

      TODO: calculation of MinDistance is not perfect now. We assume that
      the closest/farthest point of the box is one of the 8 box corners.
      Which may not be true in case of the closest point, because it may
      lie in the middle of some box face (imagine a sphere with increasing
      radius reaching from a point to a box). So our minimum may be a *little*
      too large. }
    procedure PointDistances(const P: TVector3Single;
      out MinDistance, MaxDistance: Single);

    { Calculate the distances along a direction to a box.
      The idea is that you have a 3D plane orthogonal to direction Dir
      and passing through Point. You can move this plane,
      but you have to keep it's direction constant.
      MinDistance is the minimal distance along the Dir that you can
      move this plane, such that it touches the box.
      MaxDistance is the maximum such distance.

      Note that always MinDistance <= MaxDistance.
      Note that one distance (MinDistance) or both distances may be negative.

      As a practical example: imagine a DirectionalLight (light rays are
      parallel) that has a location. Now MinDistance and MaxDistance give
      ranges of depth where the Box is, as seen from the light source.

      @raises EBox3DEmpty When used with an empty box. }
    procedure DirectionDistances(
      const Point, Dir: TVector3Single;
      out MinDistance, MaxDistance: Single);

    { Shortest distance between the box and a point.
      Always zero when the point is inside the box.

      @raises EBox3DEmpty When used with an empty box. }
    function PointDistance(const Point: TVector3Single): Single;

    { Maximum distance between the box and a point.
      Returns EmptyBoxDistance when box is empty. }
    function PointMaxDistance(const Point: TVector3Single;
      const EmptyBoxDistance: Single): Single;

    function Equal(const Box2: TBox3D): boolean;
    function Equal(const Box2: TBox3D; const EqualityEpsilon: Single): boolean;

    { Diagonal of the box, zero if empty. }
    function Diagonal: Single;

    function RectangleXY: TFloatRectangle;
    function RectangleXZ: TFloatRectangle;

    { Project box along a given direction to a 2D rectangle.
      @bold(Assumes that Dir, Side and Up vectors are already
      orthogonal and normalized.) }
    function OrthoProject(const Pos, Dir, Side, Up: TVector3Single): TFloatRectangle;

    { Compare two bounding boxes based
      on their distance to the SortPosition point,
      suitable for depth sorting in 3D.
      Follows the algorithm documented at @link(TBlendingSort.bs3D).
      Returns -1 if A < B, 1 if A > B, 0 if A = B.

      Using this with a typical sorting function will result
      in boxes back-to-front ordering, which means that the farthest
      box will be first. }
    class function CompareBackToFront3D(
      const A, B: TBox3D; const SortPosition: TVector3Single): Integer; static;

    { Compare two bounding boxes based
      on their Z coordinates, suitable for depth sorting in 2D.
      Follows the algorithm documented at @link(TBlendingSort.bs3D).
      Returns -1 if A < B, 1 if A > B, 0 if A = B.

      Using this with a typical sorting function will result
      in boxes back-to-front ordering, which means that the farthest
      box will be first. }
    class function CompareBackToFront2D(
      const A, B: TBox3D): Integer; static;
  end;

  TBox3DBool = array [boolean] of TVector3Single;
  PBox3D = ^TBox3D;

const
  { Special TBox3D value meaning "bounding box is empty".
    This is different than just bounding box with zero sizes,
    as bounding box with zero sizes still has some position.
    Empty bounding box doesn't contain any portion of 3D space. }
  EmptyBox3D: TBox3D = (Data: ((0, 0, 0), (-1, -1, -1)));

type
  TBox3DList = specialize TGenericStructList<TBox3D>;

{ Construct TBox3D value from a minimum and maximum 3D point. }
function Box3D(const p0, p1: TVector3Single): TBox3D;

{ Construct TBox3D value from a center and size.
  When any Size component is < 0, we return an empty box (@link(TBox3D.Empty)).
  This is consistent with X3D bboxCenter/Size definition e.g. at X3D Group node,
  see http://www.web3d.org/documents/specifications/19775-1/V3.2/Part01/components/group.html#Group
  @groupBegin }
function Box3DAroundPoint(const Pt: TVector3Single; Size: Single): TBox3D;
function Box3DAroundPoint(const Pt: TVector3Single; Size: TVector3Single): TBox3D;
{ @groupEnd }

{ Calculate bounding box of a set of 3D points.
  This calculates the smallest possible box enclosing all given points.
  For VertsCount = 0 this returns EmptyBox3D.

  Overloaded version with Transform parameter transforms each point
  by given matrix.

  Overloaded version with GetVertex as a function uses GetVertex to query
  for indexes from [0 .. VertsCount - 1] range.

  As usual, VertsStride = 0 means VertsStride = SizeOf(TVector3Single).

  @groupBegin }
function CalculateBoundingBox(
  Verts: PVector3Single; VertsCount: Cardinal; VertsStride: Cardinal): TBox3D; overload;
function CalculateBoundingBox(
  Verts: PVector3Single; VertsCount: Cardinal; VertsStride: Cardinal;
  const Transform: TMatrix4Single): TBox3D; overload;
function CalculateBoundingBox(Verts: TVector3SingleList): TBox3D; overload;
function CalculateBoundingBox(Verts: TVector3SingleList;
  const Transform: TMatrix4Single): TBox3D; overload;
function CalculateBoundingBox(
  GetVertex: TGetVertexFromIndexFunc;
  VertsCount: integer): TBox3D; overload;
{ @groupEnd }

{ Calculate bounding box of a set of indexed 3D points.

  This is much like CalculateBoundingBox, except there are two functions:
  For each number in [0 .. VertsIndicesCount - 1] range, GetVertIndex
  returns an index. If this index is >= 0 then it's used to query
  GetVertex function to get actual vertex position.

  Indexes < 0 are ignored, this is sometimes comfortable. E.g. for X3D models,
  you often have a list of indexes with -1 in between marking end of faces.

  Returns smallest box enclosing all vertexes.

  Overloaded version with Transform parameter transforms each point
  by given matrix.

  @groupBegin }
function CalculateBoundingBoxFromIndices(
  GetVertIndex: TGetIndexFromIndexNumFunc;
  VertsIndicesCount: integer;
  GetVertex: TGetVertexFromIndexFunc): TBox3D; overload;
function CalculateBoundingBoxFromIndices(
  GetVertIndex: TGetIndexFromIndexNumFunc;
  VertsIndicesCount: integer;
  GetVertex: TGetVertexFromIndexFunc;
  const Transform: TMatrix4Single): TBox3D; overload;
{ @groupEnd }

function TriangleBoundingBox(const T: TTriangle3Single): TBox3D;

var
  { Special equality epsilon used by IsCenteredBox3DPlaneCollision.
    For implementation reasons, they always
    use Double precision (even when called with arguments with Single precision),
    and still have to use epsilon slightly larger than usual
    CastleVectors.DoubleEqualityEpsilon. }
  Box3DPlaneCollisionEqualityEpsilon: Double = 1e-5;

{ Tests for collision between box3d centered around (0, 0, 0)
  and a plane.

  Note that you can't express empty box3d here: all BoxHalfSize items
  must be >= 0. The case when size = 0 is considered like infintely small
  box in some dimension (e.g. if all three sizes are = 0 then the box
  becomes a point).

  @groupBegin }
function IsCenteredBox3DPlaneCollision(
  const BoxHalfSize: TVector3Single;
  const Plane: TVector4Single): boolean; overload;
function IsCenteredBox3DPlaneCollision(
  const BoxHalfSize: TVector3Double;
  const Plane: TVector4Double): boolean; overload;
{ @groupEnd }

{ Smallest possible box enclosing a sphere with Center, Radius. }
function BoundingBox3DFromSphere(const Center: TVector3Single;
  const Radius: Single): TBox3D;

operator+ (const Box1, Box2: TBox3D): TBox3D;
operator+ (const B: TBox3D; const V: TVector3Single): TBox3D; deprecated 'use TBox3D.Translate. Operator is ambiguous (do we add a point, or translate?)';
operator+ (const V: TVector3Single; const B: TBox3D): TBox3D; deprecated 'use TBox3D.Translate. Operator is ambiguous (do we add a point, or translate?)';

implementation

{ TBox3D --------------------------------------------------------------------- }

function TBox3D.IsEmpty: boolean;
begin
  Result := Data[0, 0] > Data[1, 0];
end;

function TBox3D.IsEmptyOrZero: boolean;
begin
  Result := (Data[0, 0] > Data[1, 0]) or
    ( (Data[0, 0] = Data[1, 0]) and
      (Data[0, 1] = Data[1, 1]) and
      (Data[0, 2] = Data[1, 2])
    );
end;

procedure TBox3D.CheckNonEmpty;
begin
  if IsEmpty then
    raise EBox3DEmpty.Create('Empty box 3d: no middle point, no sizes etc.');
end;

function TBox3D.Center: TVector3Single;
begin
  CheckNonEmpty;
  Result[0] := (Data[0, 0] + Data[1, 0])/2;
  Result[1] := (Data[0, 1] + Data[1, 1])/2;
  Result[2] := (Data[0, 2] + Data[1, 2])/2;
end;

function TBox3D.Middle: TVector3Single;
begin
  Result := Center;
end;

function TBox3D.AverageSize: Single;
begin
  CheckNonEmpty;
  Result := (
    (Data[1, 0]-Data[0, 0]) +
    (Data[1, 1]-Data[0, 1]) +
    (Data[1, 2]-Data[0, 2])) / 3;
end;

function TBox3D.AverageSize(const AllowZero: boolean;
  const EmptyBoxSize: Single): Single;
begin
  if IsEmpty then
    Result := EmptyBoxSize else
  begin
    Result := ((Data[1, 0]-Data[0, 0]) +
               (Data[1, 1]-Data[0, 1]) +
               (Data[1, 2]-Data[0, 2]))/3;
    if (not AllowZero) and (Result = 0) then
      Result := EmptyBoxSize;
  end;
end;

function TBox3D.MaxSize: Single;
begin
  CheckNonEmpty;
  Result := Max(
     Data[1, 0] - Data[0, 0],
     Data[1, 1] - Data[0, 1],
     Data[1, 2] - Data[0, 2]);
end;

function TBox3D.MaxSize(const AllowZero: boolean;
  const EmptyBoxSize: Single): Single;
begin
  if IsEmpty then
    Result := EmptyBoxSize else
  begin
    Result := Max(
      Data[1, 0] - Data[0, 0],
      Data[1, 1] - Data[0, 1],
      Data[1, 2] - Data[0, 2]);
    if (not AllowZero) and (Result = 0) then
      Result := EmptyBoxSize;
  end;
end;

function TBox3D.Area(const AllowZero: boolean;
  const EmptyBoxArea: Single): Single;
var
  A, B, C: Single;
begin
  if IsEmpty then
    Result := EmptyBoxArea else
  begin
    A := Data[1, 0] - Data[0, 0];
    B := Data[1, 1] - Data[0, 1];
    C := Data[1, 2] - Data[0, 2];
    Result := 2*A*B + 2*B*C + 2*A*C;
    if (not AllowZero) and (Result = 0) then
      Result := EmptyBoxArea;
  end;
end;

function TBox3D.MinSize: Single;
begin
 CheckNonEmpty;

 Result := Min(
   Data[1, 0] - Data[0, 0],
   Data[1, 1] - Data[0, 1],
   Data[1, 2] - Data[0, 2]);

 { Another version is below (but this is slower without any benefit...)

   var sizes: TVector3Single;
     sizes := Box3DSizes(box);
     result := sizes[MaxVectorCoord(sizes)];
 }
end;

function TBox3D.SizeX: Single;
begin
  CheckNonEmpty;
  Result := Data[1, 0] - Data[0, 0];
end;

function TBox3D.SizeY: Single;
begin
  CheckNonEmpty;
  Result := Data[1, 1] - Data[0, 1];
end;

function TBox3D.SizeZ: Single;
begin
  CheckNonEmpty;
  Result := Data[1, 2] - Data[0, 2];
end;

procedure TBox3D.ExpandMe(const AExpand: Single);
begin
 Data[0, 0] -= AExpand;
 Data[0, 1] -= AExpand;
 Data[0, 2] -= AExpand;

 Data[1, 0] += AExpand;
 Data[1, 1] += AExpand;
 Data[1, 2] += AExpand;
end;

procedure TBox3D.ExpandMe(const AExpand: TVector3Single);
begin
 Data[0, 0] -= AExpand[0];
 Data[0, 1] -= AExpand[1];
 Data[0, 2] -= AExpand[2];

 Data[1, 0] += AExpand[0];
 Data[1, 1] += AExpand[1];
 Data[1, 2] += AExpand[2];
end;

function TBox3D.Expand(const AExpand: Single): TBox3D;
begin
  Result.Data[0, 0] := Data[0, 0] - AExpand;
  Result.Data[0, 1] := Data[0, 1] - AExpand;
  Result.Data[0, 2] := Data[0, 2] - AExpand;

  Result.Data[1, 0] := Data[1, 0] + AExpand;
  Result.Data[1, 1] := Data[1, 1] + AExpand;
  Result.Data[1, 2] := Data[1, 2] + AExpand;
end;

function TBox3D.Expand(const AExpand: TVector3Single): TBox3D;
begin
  Result.Data[0, 0] := Data[0, 0] - AExpand[0];
  Result.Data[0, 1] := Data[0, 1] - AExpand[1];
  Result.Data[0, 2] := Data[0, 2] - AExpand[2];

  Result.Data[1, 0] := Data[1, 0] + AExpand[0];
  Result.Data[1, 1] := Data[1, 1] + AExpand[1];
  Result.Data[1, 2] := Data[1, 2] + AExpand[2];
end;

function TBox3D.PointInside(const Point: TVector3Single): boolean;
begin
  if IsEmpty then Exit(false);
  Result :=
    (Data[0, 0] <= Point[0]) and (Point[0] <=  Data[1, 0]) and
    (Data[0, 1] <= Point[1]) and (Point[1] <=  Data[1, 1]) and
    (Data[0, 2] <= Point[2]) and (Point[2] <=  Data[1, 2]);
end;

function TBox3D.PointInside(const Point: TVector3Double): boolean;
begin
  if IsEmpty then Exit(false);
  Result :=
    (Data[0, 0] <= Point[0]) and (Point[0] <=  Data[1, 0]) and
    (Data[0, 1] <= Point[1]) and (Point[1] <=  Data[1, 1]) and
    (Data[0, 2] <= Point[2]) and (Point[2] <=  Data[1, 2]);
end;

{ Separated from PointInside2D, to not slowdown it by implicit
  try/finally section because we use string. }
procedure PointInside2D_InvalidIgnoreIndex;
begin
  raise EInternalError.Create('Invalid IgnoreIndex for TBox3D.PointInside2D');
end;

function TBox3D.PointInside2D(const Point: TVector2Single): boolean;
begin
  if IsEmpty then Exit(false);
  Result :=
    (Data[0, 0] <= Point[0]) and (Point[0] <=  Data[1, 0]) and
    (Data[0, 1] <= Point[1]) and (Point[1] <=  Data[1, 1]);
end;

function TBox3D.PointInside2D(const Point: TVector3Single;
  const IgnoreIndex: Integer): boolean;
begin
  if IsEmpty then Exit(false);
  case IgnoreIndex of
    0: Result :=
         (Data[0, 1] <= Point[1]) and (Point[1] <=  Data[1, 1]) and
         (Data[0, 2] <= Point[2]) and (Point[2] <=  Data[1, 2]);
    1: Result :=
         (Data[0, 2] <= Point[2]) and (Point[2] <=  Data[1, 2]) and
         (Data[0, 0] <= Point[0]) and (Point[0] <=  Data[1, 0]);
    2: Result :=
         (Data[0, 0] <= Point[0]) and (Point[0] <=  Data[1, 0]) and
         (Data[0, 1] <= Point[1]) and (Point[1] <=  Data[1, 1]);
    else PointInside2D_InvalidIgnoreIndex;
  end;
end;

procedure TBox3D.Add(const box2: TBox3D);
begin
  if Box2.IsEmpty then
    Exit else
  if IsEmpty then
    Data := Box2.Data else
  begin
    MinVar(Data[0, 0], box2.Data[0, 0]);
    MaxVar(Data[1, 0], box2.Data[1, 0]);
    MinVar(Data[0, 1], box2.Data[0, 1]);
    MaxVar(Data[1, 1], box2.Data[1, 1]);
    MinVar(Data[0, 2], box2.Data[0, 2]);
    MaxVar(Data[1, 2], box2.Data[1, 2]);
  end;
end;

procedure TBox3D.Add(const Point: TVector3Single);
begin
  if IsEmpty then
  begin
    Data[0] := Point;
    Data[1] := Point;
  end else
  begin
    MinVar(Data[0, 0], Point[0]);
    MaxVar(Data[1, 0], Point[0]);
    MinVar(Data[0, 1], Point[1]);
    MaxVar(Data[1, 1], Point[1]);
    MinVar(Data[0, 2], Point[2]);
    MaxVar(Data[1, 2], Point[2]);
  end;
end;

function TBox3D.Size: TVector3Single;
begin
  CheckNonEmpty;
  Result[0] := Data[1, 0] - Data[0, 0];
  Result[1] := Data[1, 1] - Data[0, 1];
  Result[2] := Data[1, 2] - Data[0, 2];
end;

function TBox3D.Sizes: TVector3Single;
begin
  Result := Size;
end;

procedure TBox3D.GetAllPoints(AllPoints: PVector3Single);
begin
  AllPoints[0] := Vector3Single(Data[0][0], Data[0][1], Data[0][2]);
  AllPoints[1] := Vector3Single(Data[0][0], Data[0][1], Data[1][2]);
  AllPoints[2] := Vector3Single(Data[0][0], Data[1][1], Data[0][2]);
  AllPoints[3] := Vector3Single(Data[0][0], Data[1][1], Data[1][2]);

  AllPoints[4] := Vector3Single(Data[1][0], Data[0][1], Data[0][2]);
  AllPoints[5] := Vector3Single(Data[1][0], Data[0][1], Data[1][2]);
  AllPoints[6] := Vector3Single(Data[1][0], Data[1][1], Data[0][2]);
  AllPoints[7] := Vector3Single(Data[1][0], Data[1][1], Data[1][2]);
end;

procedure TBox3D.Corners(var AllPoints: TBoxCorners);
begin
  {$warnings off} // using deprecated knowingly
  GetAllPoints(@AllPoints);
  {$warnings on}
end;

function TBox3D.Transform(
  const Matrix: TMatrix4Single): TBox3D;

  function Slower(const Matrix: TMatrix4Single): TBox3D;
  var
    BoxPoints: TBoxCorners;
    i: integer;
  begin
    Corners(BoxPoints);
    for i := 0 to 7 do BoxPoints[i] := MatrixMultPoint(Matrix, BoxPoints[i]);

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
      if BoxPoints[I, 0] < Result.Data[0, 0] then Result.Data[0, 0] := BoxPoints[I, 0];
      if BoxPoints[I, 1] < Result.Data[0, 1] then Result.Data[0, 1] := BoxPoints[I, 1];
      if BoxPoints[I, 2] < Result.Data[0, 2] then Result.Data[0, 2] := BoxPoints[I, 2];
      if BoxPoints[I, 0] > Result.Data[1, 0] then Result.Data[1, 0] := BoxPoints[I, 0];
      if BoxPoints[I, 1] > Result.Data[1, 1] then Result.Data[1, 1] := BoxPoints[I, 1];
      if BoxPoints[I, 2] > Result.Data[1, 2] then Result.Data[1, 2] := BoxPoints[I, 2];
    end;
  end;

  function Faster(const Matrix: TMatrix4Single): TBox3D;
  { Reasoning why this works Ok: look at Slower approach, and imagine
    how each of the 8 points is multiplied by the same matrix.
    Each of the 8 points is

    ( Data[0][0] or Data[1][0],
      Data[0][1] or Data[1][1],
      Data[0][2] or Data[1][2],
      1 )

    To calculate X components of 8 resulting points, you multiply 8 original
    points by the same Matrix row. Since we're only interested in the minimum
    and maximum X component, we can actually just take

      Result[0][0] := ( min( Matrix[0, 0] * Data[0][0], Matrix[0, 0] * Data[1][0] ),
                        min( Matrix[1, 0] * Data[0][1], Matrix[1, 0] * Data[1][1] ),
                        min( Matrix[2, 0] * Data[0][2], Matrix[2, 0] * Data[1][2] ),
                        Matrix[3, 0] )

    Result[0][1] is the same, but with max instead of min.
    This way we fully calculated X components.

    Idea from http://www.soe.ucsc.edu/~pang/160/f98/Gems/Gems/TransBox.c,
    see also http://www.gamedev.net/community/forums/topic.asp?topic_id=349370.

    This is 2-3 times faster than Slower (compiled with -dRELEASE, like
    0.44 to 0.13 =~ 3.3 times faster). See
    testcastleboxes.pas for speed (and correctness) test. }
  var
    I, J: Integer;
    A, B: Single;
  begin
    { Initially, both Result corners are copies of Matrix[3][0..2]
      (the "translate" numbers of Matrix) }
    Move(Matrix[3], Result.Data[0], SizeOf(Result.Data[0]));
    Move(Matrix[3], Result.Data[1], SizeOf(Result.Data[1]));

    for I := 0 to 2 do
    begin
      { Calculate Result[0][I], Result[1][I] }
      for J := 0 to 2 do
      begin
        A := Matrix[J][I] * Data[0][J];
        B := Matrix[J][I] * Data[1][J];
        if A < B then
        begin
          Result.Data[0][I] += A;
          Result.Data[1][I] += B;
        end else
        begin
          Result.Data[0][I] += B;
          Result.Data[1][I] += A;
        end;
      end;
    end;
  end;

begin
  if IsEmpty then
    Exit(EmptyBox3D);

  if (Matrix[0][3] = 0) and
     (Matrix[1][3] = 0) and
     (Matrix[2][3] = 0) and
     (Matrix[3][3] = 1) then
    Result := Faster(Matrix) else
    Result := Slower(Matrix);
end;

function TBox3D.Translate(
  const Translation: TVector3Single): TBox3D;
begin
  if not IsEmpty then
  begin
    Result.Data[0] := VectorAdd(Data[0], Translation);
    Result.Data[1] := VectorAdd(Data[1], Translation);
  end else
    Result := EmptyBox3D;
end;

function TBox3D.AntiTranslate(
  const Translation: TVector3Single): TBox3D;
begin
  if not IsEmpty then
  begin
    Result.Data[0] := VectorSubtract(Data[0], Translation);
    Result.Data[1] := VectorSubtract(Data[1], Translation);
  end else
    Result := EmptyBox3D;
end;

function TBox3D.ToNiceStr: string;
begin
 if IsEmpty then
  result := 'EMPTY' else
  result := VectorToNiceStr(Data[0])+' - '+VectorToNiceStr(Data[1]);
end;

function TBox3D.ToRawStr: string;
begin
 if IsEmpty then
  result := 'EMPTY' else
  result := '(' + VectorToRawStr(Data[0]) + ') - (' + VectorToRawStr(Data[1]) + ')';
end;

{$define CLAMP_IMPLEMENTATION:=
  var i: integer;
  begin
   for i := 0 to 2 do
   begin
    if point[i] < Data[0, i] then point[i] := Data[0, i] else
     if point[i] > Data[1, i] then point[i] := Data[1, i];
   end;
  end;}

procedure TBox3D.ClampVar(var point: TVector3Single); CLAMP_IMPLEMENTATION
procedure TBox3D.ClampVar(var point: TVector3Double); CLAMP_IMPLEMENTATION

function TBox3D.TryRayClosestIntersection(
  out Intersection: TVector3Single;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3Single): boolean;
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
      PlaneConstCoord, PlaneConstValue, RayOrigin, RayDirection) then
    begin
      RestOf3dCoords(PlaneConstCoord, c1, c2);
      if Between(NowIntersection[c1], Data[0, c1], Data[1, c1]) and
         Between(NowIntersection[c2], Data[0, c2], Data[1, c2]) then
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
    { wykorzystujemy ponizej fakt ze jezeli RayOrigin[i] < Data[0, i] to na pewno
      promien ktory przecinalby scianke Data[1, i] pudelka przecinalby najpierw
      tez inna scianke. Wiec jezeli RayOrigin[i] < Data[0, i] to nie musimy sprawdzac
      przeciecia z plaszczyzna Data[1, i]. }
    if RayOrigin[i] < Data[0, i] then
      ProposeBoxIntr(i, Data[0, i]) else
    if RayOrigin[i] > Data[1, i] then
      ProposeBoxIntr(i, Data[1, i]) else
    begin
      ProposeBoxIntr(i, Data[0, i]);
      ProposeBoxIntr(i, Data[1, i]);
    end;
  end;
end;

function TBox3D.TryRayClosestIntersection(
  out Intersection: TVector3Single;
  const RayOrigin, RayDirection: TVector3Single): boolean;
var
  IntersectionDistance: Single;
begin
  Result := TryRayClosestIntersection(
    Intersection, IntersectionDistance, RayOrigin, RayDirection);
end;

function TBox3D.TryRayClosestIntersection(
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3Single): boolean;
var
  Intersection: TVector3Single;
begin
  Result := TryRayClosestIntersection(
    Intersection, IntersectionDistance, RayOrigin, RayDirection);
end;

function TBox3D.TryRayEntrance(
  out Entrance: TVector3Single; out EntranceDistance: Single;
  const RayOrigin, RayDirection: TVector3Single): boolean;
begin
  if PointInside(RayOrigin) then
  begin
    Entrance := RayOrigin;
    EntranceDistance := 0;
    result := true;
  end else
    result := TryRayClosestIntersection(Entrance, EntranceDistance, RayOrigin, RayDirection);
end;

function TBox3D.TryRayEntrance(
  out Entrance: TVector3Single;
  const RayOrigin, RayDirection: TVector3Single): boolean;
begin
  if PointInside(RayOrigin) then
  begin
    Entrance := RayOrigin;
    result := true;
  end else
    result := TryRayClosestIntersection(Entrance, RayOrigin, RayDirection);
end;

function TBox3D.IsSegmentCollision(
  const Segment1, Segment2: TVector3Single): boolean;
begin
  Result := SegmentCollision(Segment1, Segment2);
end;

function TBox3D.SegmentCollision(
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
        Between(NowIntersection[c1], Data[0, c1], Data[1, c1]) and
        Between(NowIntersection[c2], Data[0, c2], Data[1, c2]);
    end else
      Result := false;
  end;

var
  I: integer;
begin
  for I := 0 to 2 do
  begin
    { wykorzystujemy ponizej fakt ze jezeli Segment1[i] < Data[0, i] to na pewno
      promien ktory przecinalby scianke Data[1, i] pudelka przecinalby najpierw
      tez inna scianke. Wiec jezeli Segment1[i] < Data[0, i] to nie musimy sprawdzac
      przeciecia z plaszczyzna Data[1, i]. }
    if Segment1[i] < Data[0, i] then
    begin
      if IsCollisionWithBoxPlane(i, Data[0, i]) then Exit(true);
    end else
    if Segment1[i] > Data[1, i] then
    begin
      if IsCollisionWithBoxPlane(i, Data[1, i]) then Exit(true);
    end else
    begin
      if IsCollisionWithBoxPlane(i, Data[0, i]) then Exit(true);
      if IsCollisionWithBoxPlane(i, Data[1, i]) then Exit(true);
    end;
  end;

  Result := false;
end;

function TBox3D.PlaneCollision(
  const Plane: TVector4Single): TPlaneCollision;
{ This generalizes the idea from IsCenteredBox3DPlaneCollision
  in castleboxes_generic_float.inc.
  It's also explained in
  Akenine-Moller, Haines "Real-Time Rendering" (2nd ed), 13.9 (page 586)
}
var
  I: Integer;
  VMin, VMax: TVector3Single;
  B: boolean;
  BoxBool: TBox3DBool absolute Data;
begin
  if IsEmpty then
    Exit(pcNone);

  for I := 0 to 2 do
  begin
    { Normal code:
    if Plane[I] >= 0 then
    begin
      VMin[I] := Data[0][I];
      VMax[I] := Data[1][I];
    end else
    begin
      VMin[I] := Data[1][I];
      VMax[I] := Data[0][I];
    end;
    }

    { Code optimized to avoid "if", instead doing table lookup by BoxBool }
    B := Plane[I] >= 0;
    VMin[I] := BoxBool[not B][I];
    VMax[I] := BoxBool[B][I];
  end;

  if Plane[0] * VMin[0] +
     Plane[1] * VMin[1] +
     Plane[2] * VMin[2] +
     Plane[3] > 0 then
    Exit(pcOutside);

  if Plane[0] * VMax[0] +
     Plane[1] * VMax[1] +
     Plane[2] * VMax[2] +
     Plane[3] < 0 then
    Exit(pcInside);

  Result := pcIntersecting;
end;

function TBox3D.PlaneCollisionInside(
  const Plane: TVector4Single): boolean;
{ Based on Box3DPlaneCollision, except now we need only VMax point.

  Actually, we don't even store VMax. Instead, we calculate to
  PlaneResult the equation

    Plane[0] * VMax[0] +
    Plane[1] * VMax[1] +
    Plane[2] * VMax[2] +
    Plane[3]
}
var
  BoxBool: TBox3DBool absolute Data;
begin
  if IsEmpty then
    Exit(false);

  Result :=
    BoxBool[Plane[0] >= 0][0] * Plane[0] +
    BoxBool[Plane[1] >= 0][1] * Plane[1] +
    BoxBool[Plane[2] >= 0][2] * Plane[2] +
    Plane[3] < 0;
end;

function TBox3D.PlaneCollisionOutside(
  const Plane: TVector4Single): boolean;
var
  BoxBool: TBox3DBool absolute Data;
begin
  if IsEmpty then
    Exit(false);

  Result :=
    BoxBool[Plane[0] < 0][0] * Plane[0] +
    BoxBool[Plane[1] < 0][1] * Plane[1] +
    BoxBool[Plane[2] < 0][2] * Plane[2] +
    Plane[3] > 0;
end;

function TBox3D.IsTriangleCollision(const Triangle: TTriangle3Single): boolean;

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

{ The same comments about precision as for IsCenteredBox3DPlaneCollision apply also here. }
{$define EqualityEpsilon := Box3DPlaneCollisionEqualityEpsilon}

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
  if IsEmpty then
    Exit(false);

  { calculate BoxCenter and BoxHalfSize }
  for I := 0 to 2 do
  begin
    BoxCenter[I] := (Data[0, I] + Data[1, I]) / 2;
    BoxHalfSize[I] := (Data[1, I] - Data[0, I]) / 2;
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
  if not IsCenteredBox3DPlaneCollision(BoxHalfSize, Plane) then
    Exit(false);

  Result := true; { box and triangle overlaps }
end;

{$undef EqualityEpsilon}

procedure TBox3D.BoundingSphere(
  var SphereCenter: TVector3Single; var SphereRadiusSqr: Single);
begin
 if IsEmpty then
 begin
  SphereRadiusSqr := 0;
 end else
 begin
  SphereCenter := Center;
  SphereRadiusSqr := PointsDistanceSqr(SphereCenter, Data[0]);
 end;
end;

function TBox3D.Collision(const Box2: TBox3D): boolean;
begin
  Result :=
    (not IsEmpty) and
    (not Box2.IsEmpty) and
    (not ((Data[1, 0] < Box2.Data[0, 0]) or (Box2.Data[1, 0] < Data[0, 0]))) and
    (not ((Data[1, 1] < Box2.Data[0, 1]) or (Box2.Data[1, 1] < Data[0, 1]))) and
    (not ((Data[1, 2] < Box2.Data[0, 2]) or (Box2.Data[1, 2] < Data[0, 2])));
end;

function TBox3D.Radius: Single;
begin
  if IsEmpty then
    Result := 0 else
    Result := Sqrt(Max(
      Max(Max(VectorLenSqr(Vector3Single(Data[0, 0], Data[0, 1], Data[0, 2])),
              VectorLenSqr(Vector3Single(Data[1, 0], Data[0, 1], Data[0, 2]))),
          Max(VectorLenSqr(Vector3Single(Data[1, 0], Data[1, 1], Data[0, 2])),
              VectorLenSqr(Vector3Single(Data[0, 0], Data[1, 1], Data[0, 2])))),
      Max(Max(VectorLenSqr(Vector3Single(Data[0, 0], Data[0, 1], Data[1, 2])),
              VectorLenSqr(Vector3Single(Data[1, 0], Data[0, 1], Data[1, 2]))),
          Max(VectorLenSqr(Vector3Single(Data[1, 0], Data[1, 1], Data[1, 2])),
              VectorLenSqr(Vector3Single(Data[0, 0], Data[1, 1], Data[1, 2]))))));
end;

{ Separated from Radius2D, to not slowdown it by implicit
  try/finally section because we use string. }
procedure Radius2D_InvalidIgnoreIndex;
begin
  raise EInternalError.Create('Invalid IgnoreIndex for TBox3D.Radius2D');
end;

function TBox3D.Radius2D(const IgnoreIndex: Integer): Single;
begin
  if IsEmpty then
    Result := 0 else
  begin
    case IgnoreIndex of
      0: Result := Max(
           Max(VectorLenSqr(Vector2Single(Data[0, 1], Data[0, 2])),
               VectorLenSqr(Vector2Single(Data[1, 1], Data[0, 2]))),
           Max(VectorLenSqr(Vector2Single(Data[1, 1], Data[1, 2])),
               VectorLenSqr(Vector2Single(Data[0, 1], Data[1, 2]))));
      1: Result := Max(
           Max(VectorLenSqr(Vector2Single(Data[0, 2], Data[0, 0])),
               VectorLenSqr(Vector2Single(Data[1, 2], Data[0, 0]))),
           Max(VectorLenSqr(Vector2Single(Data[1, 2], Data[1, 0])),
               VectorLenSqr(Vector2Single(Data[0, 2], Data[1, 0]))));
      2: Result := Max(
           Max(VectorLenSqr(Vector2Single(Data[0, 0], Data[0, 1])),
               VectorLenSqr(Vector2Single(Data[1, 0], Data[0, 1]))),
           Max(VectorLenSqr(Vector2Single(Data[1, 0], Data[1, 1])),
               VectorLenSqr(Vector2Single(Data[0, 0], Data[1, 1]))));
      else Radius2D_InvalidIgnoreIndex;
    end;

    Result := Sqrt(Result);
  end;
end;

function TBox3D.SphereSimpleCollision(
  const SphereCenter: TVector3Single; const SphereRadius: Single): boolean;
begin
  Result := (not IsEmpty) and
    (SphereCenter[0] >= Data[0][0] - SphereRadius) and
    (SphereCenter[0] <= Data[1][0] + SphereRadius) and
    (SphereCenter[1] >= Data[0][1] - SphereRadius) and
    (SphereCenter[1] <= Data[1][1] + SphereRadius) and
    (SphereCenter[2] >= Data[0][2] - SphereRadius) and
    (SphereCenter[2] <= Data[1][2] + SphereRadius);
end;

function TBox3D.SphereCollision(
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
  if IsEmpty then Exit(false);

  D := 0;

  { Uses:
    4 up to 7 comparisons,
    6 additions,
    4 multiplications.

    Ok, that's damn fast, but still a little slower than
    TBox3D.SphereSimpleCollision (that has 1 up to 6 comparisons and additions). }

  if SphereCenter[0] < Data[0][0] then D += Sqr(SphereCenter[0] - Data[0][0]) else
  if SphereCenter[0] > Data[1][0] then D += Sqr(SphereCenter[0] - Data[1][0]);

  if SphereCenter[1] < Data[0][1] then D += Sqr(SphereCenter[1] - Data[0][1]) else
  if SphereCenter[1] > Data[1][1] then D += Sqr(SphereCenter[1] - Data[1][1]);

  if SphereCenter[2] < Data[0][2] then D += Sqr(SphereCenter[2] - Data[0][2]) else
  if SphereCenter[2] > Data[1][2] then D += Sqr(SphereCenter[2] - Data[1][2]);

  Result := D <= Sqr(SphereRadius);
end;

function TBox3D.SphereCollision2D(
  const SphereCenter: TVector2Single; const SphereRadius: Single): boolean;
var
  D: Single;
begin
  if IsEmpty then Exit(false);

  D := 0;

  if SphereCenter[0] < Data[0][0] then D += Sqr(SphereCenter[0] - Data[0][0]) else
  if SphereCenter[0] > Data[1][0] then D += Sqr(SphereCenter[0] - Data[1][0]);

  if SphereCenter[1] < Data[0][1] then D += Sqr(SphereCenter[1] - Data[0][1]) else
  if SphereCenter[1] > Data[1][1] then D += Sqr(SphereCenter[1] - Data[1][1]);

  Result := D <= Sqr(SphereRadius);
end;

function TBox3D.MaximumPlane(
  const Direction: TVector3Single): TVector4Single;
var
  BoxBool: TBox3DBool absolute Data;
  ResultDir: TVector3Single absolute Result;
begin
  CheckNonEmpty;

  { first 3 plane components are just copied from Direction }
  ResultDir := Direction;

(*
  { calculate box corner that intersects resulting plane:
    just choose appropriate coords using Direction. }
  P[0] := BoxBool[Direction[0] >= 0][0];
  P[1] := BoxBool[Direction[1] >= 0][1];
  P[2] := BoxBool[Direction[2] >= 0][2];

  { calculate 4th plane component.
    Plane must intersect P, so
      P[0] * Result[0] + .... + Result[3] = 0
  }
  Result[3] := - (P[0] * Result[0] +
                  P[1] * Result[1] +
                  P[2] * Result[2]);
*)

  { optimized version, just do this in one go: }
  Result[3] := - (BoxBool[Direction[0] >= 0][0] * Result[0] +
                  BoxBool[Direction[1] >= 0][1] * Result[1] +
                  BoxBool[Direction[2] >= 0][2] * Result[2]);
end;

function TBox3D.MinimumPlane(const Direction: TVector3Single): TVector4Single;
var
  BoxBool: TBox3DBool absolute Data;
  ResultDir: TVector3Single absolute Result;
begin
  CheckNonEmpty;

  { first 3 plane components are just copied from Direction }
  ResultDir := Direction;

  { optimized version, just do this in one go: }
  Result[3] := - (BoxBool[Direction[0] < 0][0] * Result[0] +
                  BoxBool[Direction[1] < 0][1] * Result[1] +
                  BoxBool[Direction[2] < 0][2] * Result[2]);
end;

function TBox3D.MaximumCorner(const Direction: TVector3Single): TVector3Single;
var
  BoxBool: TBox3DBool absolute Data;
begin
  CheckNonEmpty;
  Result[0] := BoxBool[Direction[0] >= 0][0];
  Result[1] := BoxBool[Direction[1] >= 0][1];
  Result[2] := BoxBool[Direction[2] >= 0][2];
end;

function TBox3D.MinimumCorner(const Direction: TVector3Single): TVector3Single;
var
  BoxBool: TBox3DBool absolute Data;
begin
  CheckNonEmpty;
  Result[0] := BoxBool[Direction[0] < 0][0];
  Result[1] := BoxBool[Direction[1] < 0][1];
  Result[2] := BoxBool[Direction[2] < 0][2];
end;

procedure TBox3D.PointDistances(const P: TVector3Single;
  out MinDistance, MaxDistance: Single);
var
  Dist0, Dist1: Single;
  I: Integer;
begin
  CheckNonEmpty;

  MinDistance := 0;
  MaxDistance := 0;

  { For each coordinate (0, 1, 2), find which side of the box is closest.
    Effectively, we find the closest of the 8 box corners.
    The opposite corner is the farthest.
    We want to calculate distance to this point, so we do it by the way. }
  for I := 0 to 2 do
  begin
    Dist0 := Sqr(P[I] - Data[0][I]);
    Dist1 := Sqr(P[I] - Data[1][I]);
    if Dist0 < Dist1 then
    begin
      MinDistance += Dist0;
      MaxDistance += Dist1;
    end else
    begin
      MinDistance += Dist1;
      MaxDistance += Dist0;
    end;
  end;

  if PointInside(P) then
    MinDistance := 0;

  { Because of floating point inaccuracy, MinDistance may be larger
    by epsilon than MaxDistance? Fix it to be sure. }
  { For now: just assert it: }
  Assert(MinDistance <= MaxDistance);
end;

procedure TBox3D.DirectionDistances(
  const Point, Dir: TVector3Single;
  out MinDistance, MaxDistance: Single);
var
  B: TBox3DBool absolute Data;
  XMin, YMin, ZMin: boolean;
  MinPoint, MaxPoint: TVector3Single;
  Coord: Integer;
begin
  CheckNonEmpty;

  XMin := Dir[0] < 0;
  YMin := Dir[1] < 0;
  ZMin := Dir[2] < 0;

  MinPoint := PointOnLineClosestToPoint(Point, Dir,
    Vector3Single(B[XMin][0], B[YMin][1], B[ZMin][2]));
  MaxPoint := PointOnLineClosestToPoint(Point, Dir,
    Vector3Single(B[not XMin][0], B[not YMin][1], B[not ZMin][2]));

  MinDistance := PointsDistance(Point, MinPoint);
  MaxDistance := PointsDistance(Point, MaxPoint);

  { choose one of the 3 coordinates where Dir is largest, for best
    numerical stability. We need to compare now and see which
    distances should be negated. }
  Coord := MaxAbsVectorCoord(Dir);

  if Dir[Coord] > 0 then
  begin
    { So the distances to points that are *larger* on Coord are positive.
      Others should be negative. }
    if MinPoint[Coord] < Point[Coord] then
      MinDistance := -MinDistance;
    if MaxPoint[Coord] < Point[Coord] then
      MaxDistance := -MaxDistance;
  end else
  begin
    if MinPoint[Coord] > Point[Coord] then
      MinDistance := -MinDistance;
    if MaxPoint[Coord] > Point[Coord] then
      MaxDistance := -MaxDistance;
  end;

  { Because of floating point inaccuracy, MinDistance may be larger
    by epsilon than MaxDistance? Fix it to be sure. }
  { For now: just assert it: }
  Assert(MinDistance <= MaxDistance);
end;

function TBox3D.PointDistance(const Point: TVector3Single): Single;
var
  I: Integer;
begin
  CheckNonEmpty;

  { There are 4 cases:
    0. point is in no box range - calculate distance to closest corner
    1. point is 1 box range - calculate distance to closest edge
    2. point is 2 box ranges - calculate distance to closest side
    3. point is 3 box ranges - so point is inside, distance = 0

    First naive implementation was detecting these cases by calculating
    InsideRangeCount, InsideRange and such.
    But actually you can calculate all cases at once. }

  Result := 0;
  for I := 0 to 2 do
  begin
    if Point[I] < Data[0][I] then
      Result += Sqr(Point[I] - Data[0][I]) else
    if Point[I] > Data[1][I] then
      Result += Sqr(Point[I] - Data[1][I]);
  end;

  Result := Sqrt(Result);
end;

function TBox3D.PointMaxDistance(const Point: TVector3Single;
  const EmptyBoxDistance: Single): Single;
var
  B: TBox3DBool absolute Data;
begin
  if IsEmpty then
    Result := EmptyBoxDistance else
    Result := Sqrt(
      Sqr(Point[0] - B[Point[0] < (Data[0][0] + Data[1][0]) / 2][0]) +
      Sqr(Point[1] - B[Point[1] < (Data[0][1] + Data[1][1]) / 2][1]) +
      Sqr(Point[2] - B[Point[2] < (Data[0][2] + Data[1][2]) / 2][2])
    );
end;

function TBox3D.Equal(const Box2: TBox3D): boolean;
begin
  if IsEmpty then
    Result := Box2.IsEmpty else
    Result := (not Box2.IsEmpty) and
      VectorsEqual(Data[0], Box2.Data[0]) and
      VectorsEqual(Data[1], Box2.Data[1]);
end;

function TBox3D.Equal(const Box2: TBox3D; const EqualityEpsilon: Single): boolean;
begin
  if IsEmpty then
    Result := Box2.IsEmpty else
    Result := (not Box2.IsEmpty) and
      VectorsEqual(Data[0], Box2.Data[0], EqualityEpsilon) and
      VectorsEqual(Data[1], Box2.Data[1], EqualityEpsilon);
end;

function TBox3D.Diagonal: Single;
begin
  if IsEmpty then
    Result := 0 else
    Result := Sqrt(Sqr(Data[1][0] - Data[0][0]) +
                   Sqr(Data[1][1] - Data[0][1]) +
                   Sqr(Data[1][2] - Data[0][2]));
end;

function TBox3D.RectangleXY: TFloatRectangle;
begin
  if IsEmpty then
    Exit(TFloatRectangle.Empty) else
  begin
    Result.Left   := Data[0][0];
    Result.Bottom := Data[0][1];
    Result.Width  := Data[1][0] - Data[0][0];
    Result.Height := Data[1][1] - Data[0][1];
  end;
end;

function TBox3D.RectangleXZ: TFloatRectangle;
begin
  if IsEmpty then
    Exit(TFloatRectangle.Empty) else
  begin
    Result.Left   := Data[0][0];
    Result.Bottom := Data[0][2];
    Result.Width  := Data[1][0] - Data[0][0];
    Result.Height := Data[1][2] - Data[0][2];
  end;
end;

function TBox3D.OrthoProject(const Pos, Dir, Side, Up: TVector3Single): TFloatRectangle;

  function ProjectPoint(const P: TVector3Single): TVector2Single;
  var
    PDiff: TVector3Single;
  begin
    PDiff := P - Pos;
    Result[0] := VectorDotProduct(PDiff, Side);
    Result[1] := VectorDotProduct(PDiff, Up);
  end;

var
  C: TBoxCorners;
  I: Integer;
begin
  Corners(C);
  Result := FloatRectangle(ProjectPoint(C[0]), 0, 0);
  for I := 1 to 7 do
    Result := Result.Add(ProjectPoint(C[I]));
end;

class function TBox3D.CompareBackToFront3D(
  const A, B: TBox3D; const SortPosition: TVector3Single): Integer; static;
begin
  { We always treat empty box as closer than non-empty.
    And two empty boxes are always equal.

    Remember that code below must make sure that Result = 0
    for equal elements (Sort may depend on this). So A > B only when:
    - A empty, and B non-empty
    - both non-empty, and A closer }

  if (not A.IsEmpty) and
    ( B.IsEmpty or
      ( PointsDistanceSqr(A.Center, SortPosition) >
        PointsDistanceSqr(B.Center, SortPosition))) then
    Result := -1 else
  if (not B.IsEmpty) and
    ( A.IsEmpty or
      ( PointsDistanceSqr(B.Center, SortPosition) >
        PointsDistanceSqr(A.Center, SortPosition))) then
    Result :=  1 else
    Result :=  0;
end;

class function TBox3D.CompareBackToFront2D(
  const A, B: TBox3D): Integer; static;
begin
  { Note that we ignore SortPosition, we do not look at distance between
    SortPosition and A, we merely look at A.
    This way looking at 2D Spine scene from the other side is also Ok.

    For speed, we don't look at bounding box Middle, only at it's min point.
    The assumption here is that shape is 2D, so
      BoundingBox.Data[0][2] = BoundingBox.Data[1][2] = BoundingBox.Center[2] . }

  if (not A.IsEmpty) and
    ( B.IsEmpty or
      ( A.Data[0][2] < B.Data[0][2] )) then
    Result := -1 else
  if (not B.IsEmpty) and
    ( A.IsEmpty or
      ( B.Data[0][2] < A.Data[0][2] )) then
    Result :=  1 else
    Result :=  0;
end;

{ Routines ------------------------------------------------------------------- }

{$define TGenericFloat := Single}
{$define TVector3GenericFloat := TVector3Single}
{$define TVector4GenericFloat := TVector4Single}
{$I castleboxes_generic_float.inc}

{$define TGenericFloat := Double}
{$define TVector3GenericFloat := TVector3Double}
{$define TVector4GenericFloat := TVector4Double}
{$I castleboxes_generic_float.inc}

function Box3D(const p0, p1: TVector3Single): TBox3D;
begin
  result.Data[0] := p0;
  result.Data[1] := p1;
end;

function Box3DAroundPoint(const Pt: TVector3Single; Size: Single): TBox3D;
begin
  if Size < 0 then
    Exit(TBox3D.Empty);

  Size /= 2;
  Result.Data[0][0] := Pt[0] - Size;
  Result.Data[0][1] := Pt[1] - Size;
  Result.Data[0][2] := Pt[2] - Size;
  Result.Data[1][0] := Pt[0] + Size;
  Result.Data[1][1] := Pt[1] + Size;
  Result.Data[1][2] := Pt[2] + Size;
end;

function Box3DAroundPoint(const Pt: TVector3Single; Size: TVector3Single): TBox3D;
begin
  if (Size[0] < 0) or
     (Size[1] < 0) or
     (Size[2] < 0) then
    Exit(TBox3D.Empty);

  Size /= 2;
  Result.Data[0][0] := Pt[0] - Size[0];
  Result.Data[0][1] := Pt[1] - Size[1];
  Result.Data[0][2] := Pt[2] - Size[2];
  Result.Data[1][0] := Pt[0] + Size[0];
  Result.Data[1][1] := Pt[1] + Size[1];
  Result.Data[1][2] := Pt[2] + Size[2];
end;

function CalculateBoundingBox(
  GetVertex: TGetVertexFromIndexFunc;
  VertsCount: integer): TBox3D;
var
  I: Integer;
  V: TVector3Single;
begin
  if VertsCount = 0 then
    Result := EmptyBox3D else
  begin
    Result.Data[0] := GetVertex(0);
    Result.Data[1] := Result.Data[0];
    for I := 1 to VertsCount - 1 do
    begin
      V := GetVertex(I);
      MinVar(Result.Data[0][0], V[0]);
      MinVar(Result.Data[0][1], V[1]);
      MinVar(Result.Data[0][2], V[2]);

      MaxVar(Result.Data[1][0], V[0]);
      MaxVar(Result.Data[1][1], V[1]);
      MaxVar(Result.Data[1][2], V[2]);
    end;
  end;
end;

type
  { Internal helper for CalculateBoundingBox }
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
   result := MatrixMultPoint(
     PMatrix^, PVector3Single(PointerAdd(Verts, VertsStride*Cardinal(index)))^ );
  end;

function CalculateBoundingBox(
  Verts: PVector3Single; VertsCount: Cardinal; VertsStride: Cardinal): TBox3D;
var
  Calculator: TBBox_Calculator;
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
  const Transform: TMatrix4Single): TBox3D;
var
  Calculator: TBBox_Calculator;
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

function CalculateBoundingBox(Verts: TVector3SingleList): TBox3D;
begin
  Result := CalculateBoundingBox(PVector3Single(Verts.List), Verts.Count, 0);
end;

function CalculateBoundingBox(Verts: TVector3SingleList;
  const Transform: TMatrix4Single): TBox3D;
begin
  Result := CalculateBoundingBox(PVector3Single(Verts.List), Verts.Count, 0,
    Transform);
end;

function CalculateBoundingBoxFromIndices(
  GetVertIndex: TGetIndexFromIndexNumFunc;
  VertsIndicesCount: integer;
  GetVertex: TGetVertexFromIndexFunc): TBox3D;
var
  { pozycja pierwszego nieujemnego indexu.
    Zwracamy EmptyBox3D wtw. gdy firstIndex nie istnieje }
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
    result := EmptyBox3D;
    exit;
  end;

  { Note that I do only one pass, getting all vertexes.

    This is important, because GetVertex may be quite expensive
    operation (in case of e.g. TVertTransform_Calculator.GetTransformed,
    this is MatrixMultPoint for every vertex). At the beginning
    I implemented this by caling 6 time find_extremum function,
    and each call to find_extremum was iterating over every vertex.
    This was obviously wrong, because this caused calling GetVertex
    6 times more often than necessary. In some cases (like preparing
    animations in "The Castle") this can cause really significant
    slowdown. }

  ThisVertex := GetVertex(GetVertIndex(firstIndexNum));
  Result.Data[0] := ThisVertex;
  Result.Data[1] := ThisVertex;
  for IndexNum := FirstIndexNum+1 to VertsIndicesCount - 1 do
  begin
    Index := GetVertIndex(IndexNum);
    if Index >= 0 then
    begin
      ThisVertex := GetVertex(Index);
      if ThisVertex[0] < Result.Data[0, 0] then Result.Data[0, 0] := ThisVertex[0];
      if ThisVertex[1] < Result.Data[0, 1] then Result.Data[0, 1] := ThisVertex[1];
      if ThisVertex[2] < Result.Data[0, 2] then Result.Data[0, 2] := ThisVertex[2];
      if ThisVertex[0] > Result.Data[1, 0] then Result.Data[1, 0] := ThisVertex[0];
      if ThisVertex[1] > Result.Data[1, 1] then Result.Data[1, 1] := ThisVertex[1];
      if ThisVertex[2] > Result.Data[1, 2] then Result.Data[1, 2] := ThisVertex[2];
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
    result := MatrixMultPoint(PTransform^, GetNotTransformed(index));
  end;

function CalculateBoundingBoxFromIndices(
  GetVertIndex: TGetIndexFromIndexNumFunc;
  VertsIndicesCount: integer;
  GetVertex: TGetVertexFromIndexFunc;
  const Transform: TMatrix4Single): TBox3D;
var
  Calculator: TVertTransform_Calculator;
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

function TriangleBoundingBox(const T: TTriangle3Single): TBox3D;
begin
  MinMax(T[0][0], T[1][0], T[2][0], Result.Data[0][0], Result.Data[1][0]);
  MinMax(T[0][1], T[1][1], T[2][1], Result.Data[0][1], Result.Data[1][1]);
  MinMax(T[0][2], T[1][2], T[2][2], Result.Data[0][2], Result.Data[1][2]);
end;

function BoundingBox3DFromSphere(const Center: TVector3Single;
  const Radius: Single): TBox3D;
begin
  Result.Data[0] := Center;
  Result.Data[0][0] -= Radius;
  Result.Data[0][1] -= Radius;
  Result.Data[0][2] -= Radius;

  Result.Data[1] := Center;
  Result.Data[1][0] += Radius;
  Result.Data[1][1] += Radius;
  Result.Data[1][2] += Radius;
end;

operator+ (const Box1, Box2: TBox3D): TBox3D;
begin
  if Box1.IsEmpty then
    Result := Box2 else
  if Box2.IsEmpty then
    Result := Box1 else
  begin
    Result.Data[0, 0] := Min(Box1.Data[0, 0], Box2.Data[0, 0]);
    Result.Data[1, 0] := Max(Box1.Data[1, 0], Box2.Data[1, 0]);
    Result.Data[0, 1] := Min(Box1.Data[0, 1], Box2.Data[0, 1]);
    Result.Data[1, 1] := Max(Box1.Data[1, 1], Box2.Data[1, 1]);
    Result.Data[0, 2] := Min(Box1.Data[0, 2], Box2.Data[0, 2]);
    Result.Data[1, 2] := Max(Box1.Data[1, 2], Box2.Data[1, 2]);
  end;
end;

operator+ (const B: TBox3D; const V: TVector3Single): TBox3D;
begin
  Result := B.Translate(V);
end;

operator+ (const V: TVector3Single; const B: TBox3D): TBox3D;
begin
  Result := B.Translate(V);
end;

end.
