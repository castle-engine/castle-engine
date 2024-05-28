{
  Copyright 2003-2023 Michalis Kamburelis.

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

uses SysUtils, Generics.Collections,
  CastleVectors, CastleUtils, CastleTriangles, CastleRectangles;

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

  TBoxCorners = array [0..7] of TVector3;

  { Axis-aligned box. Rectangular prism with all sides parallel to basic planes
    X = 0, Y = 0 and Z = 0. This is sometimes called AABB, "axis-aligned bounding
    box". Many geometric operations are fast and easy on this type.

    The actual box dimensions are stored inside the @link(Data) field, as two 3D points.
    First point has always all the smaller coords, second point has all
    the larger coords. I.e. always

    @preformatted(
      Data[0].X <= Data[1].X and
      Data[0].Y <= Data[1].Y and
      Data[0].Z <= Data[1].Z
    )

    The only exception is the special value TBox3D.Empty.

    Note that the box may still have all sizes equal 0. Consider a 3D model with
    only a single 3D point --- it's not empty, but all the sizes must be 0. }
  TBox3D = record
  strict private
    function GetMin: TVector3;
    procedure SetMin(const Value: TVector3);
    function GetMax: TVector3;
    procedure SetMax(const Value: TVector3);
  public
    Data: array [0..1] of TVector3;

    { Special TBox3D value meaning "bounding box is empty".
      This is different than just bounding box with zero sizes,
      as bounding box with zero sizes still has some position.
      Empty bounding box doesn't contain any portion of 3D space. }
    class function Empty: TBox3D; static;

    { Check is box empty.
      You can think of this function as "compare Box with TBox3D.Empty".

      But actually it works a little faster, by utilizing the assumption
      that TBox3D.Empty is the only allowed value that breaks
      @code(Data[0].X <= Data[1].X) rule. }
    function IsEmpty: boolean;

    { The minimum 3D position within this box. Use only if not @link(IsEmpty),
      in which case this is just a shortcut for @code(Data[0]). }
    property Min: TVector3 read GetMin write SetMin;

    { The maximum 3D position within this box. Use only if not @link(IsEmpty),
      in which case this is just a shortcut for @code(Data[1]). }
    property Max: TVector3 read GetMax write SetMax;

    { Check is box empty or has all the sizes equal 0. }
    function IsEmptyOrZero: boolean;

    procedure CheckNonEmpty;

    { Center of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function Middle: TVector3; deprecated 'use Center';

    { Center of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function Center: TVector3;

    { Average size of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function AverageSize: Single; overload;

    { Largest size of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function MaxSize: Single; overload;

    { Smallest size of the box.
      @raises(EBox3DEmpty If the Box is empty.) }
    function MinSize: Single; overload;

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

    { Average size of TBox3D, or EmptyBoxSize if box is empty.
      2D box projection is obtained by rejecting the IgnoreIndex coordinate
      (must be 0, 1 or 2).

      @param(AllowZero Decides what to do when box is not empty but the result
        would be zero, which means that the box is infinitely thin in all axes.
        If @true, then result is just 0, otherwise it's EmptyBoxSize.) }
    function AverageSize2D(const AllowZero: boolean;
      const EmptyBoxSize: Single; const IgnoreIndex: T3DAxis): Single; overload;

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

    { Decrease "minimum corner" by (AExpand, AExpand, AExpand) vector,
      and increase "maximum corner" by the same vector.
      So you get Box with all sizes increased by 2 * AExpand.

      Box must not be empty.
      Note that AExpand may be negative, but then you must be sure
      that it doesn't make Box empty. }
    procedure ExpandMe(const AExpand: Single); overload;

    { Decrease "minimum corner" by AExpand vector,
      and increase "maximum corner" by the same vector.
      So you get Box with all sizes increased by 2 * AExpand.

      Box must not be empty.
      Note that AExpand may be negative, but then you must be sure
      that it doesn't make Box empty. }
    procedure ExpandMe(const AExpand: TVector3); overload;

    function Grow(const AExpand: Single): TBox3D; overload;
    function Grow(const AExpand: TVector3): TBox3D; overload;

    function Expand(const AExpand: Single): TBox3D; overload; deprecated 'use Grow, consistent with TRectangle.Grow';
    function Expand(const AExpand: TVector3): TBox3D; overload; deprecated 'use Grow, consistent with TRectangle.Grow';

    { Check is the point inside the box.
      Always false if Box is empty (obviously, no point is inside an empty box).

      @groupBegin }
    function Contains(const Point: TVector3): boolean; overload;

    { Causes FPC errors about "duplicate ASM label",
      see https://bugs.freepascal.org/view.php?id=32188 .
      Also, it's not really very useful. }
    //function Contains(const Point: TVector3Double): boolean; overload;

    { }
    function PointInside(const Point: TVector3): boolean; overload; deprecated 'use Contains method, which is consistent with TRectangle';

    { Causes FPC errors about "duplicate ASM label",
      see https://bugs.freepascal.org/view.php?id=32188 .
      Also, it's not really very useful. }
    //function PointInside(const Point: TVector3Double): boolean; overload; deprecated 'use Contains method, which is consistent with TRectangle';
    { @groupEnd }

    { Is the 2D point inside the 2D projection of the box, ignores the Z coord of box. }
    function Contains2D(const Point: TVector2): boolean; overload;
    function PointInside2D(const Point: TVector2): boolean; overload; deprecated 'use Contains2d method';

    { Is the 2D point inside the 2D projection of the box.
      2D projection (of point and box) is obtained by rejecting
      the IgnoreIndex coordinate (must be 0, 1 or 2). }
    function Contains2D(const Point: TVector3; const IgnoreIndex: T3DAxis): boolean; overload;
    function PointInside2D(const Point: TVector3; const IgnoreIndex: T3DAxis): boolean; overload; deprecated 'use Contains2D method';

    { Add another box to our box.
      This calculates the smallest box that encloses both the current box,
      and Box2. Doing @code(MyBox.Include(AnotherBox)) is equivalent to doing
      @code(MyBox := MyBox + AnotherBox). }
    procedure Include(const box2: TBox3D); overload;

    { Make box larger, if necessary, to contain given Point. }
    procedure Include(const Point: TVector3); overload;
    procedure Include(const Points: TVector3List); overload;

    { Make a box that contains given points. }
    class function FromPoints(const Points: TVector3List): TBox3D; static;

    { Three box sizes. }
    function Sizes: TVector3; deprecated 'use Size';

    { Three box sizes. Name consistent with TBoxNode.Size.
      @raises(EBox3DEmpty If the Box is empty.) }
    function Size: TVector3;

    { Calculate eight corners of the box.}
    procedure Corners(out AllPoints: TBoxCorners);
    procedure GetAllPoints(AllPoints: PVector3Array); deprecated 'use Corners';

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
    function Transform(const Matrix: TMatrix4): TBox3D;

    { Move Box. Does nothing if Box is empty. }
    function Translate(const Translation: TVector3): TBox3D;

    { Move Box, by -Translation. Does nothing if Box is empty. }
    function AntiTranslate(const Translation: TVector3): TBox3D;

    function ToNiceStr: string; deprecated 'use ToString';
    function ToRawStr: string; deprecated 'use ToRawString';

    function ToString: string;
    function ToRawString: string;

    procedure ClampVar(var point: TVector3); overload;

    { Causes FPC errors about "duplicate ASM label",
      see https://bugs.freepascal.org/view.php?id=32188 .
      Also, it's not really very useful. }
    // procedure ClampVar(var point: TVector3Double); overload;

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
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3): boolean; overload;
    function TryRayClosestIntersection(
      out Intersection: TVector3;
      const RayOrigin, RayDirection: TVector3): boolean; overload;
    function TryRayClosestIntersection(
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3): boolean; overload;
    { @groupEnd }

    { Intersection between the ray (returns closest intersection to RayOrigin)
      and the box, treating the box as a filled volume.

      If RayOrigin is inside the box, TryBoxRayEntrance simply returns
      RayOrigin. If RayOrigin is outside of the box, the answer is the same
      as with TryBoxRayClosestIntersection.

      @groupBegin }
    function TryRayEntrance(
      out Entrance: TVector3; out EntranceDistance: Single;
      const RayOrigin, RayDirection: TVector3): boolean; overload;
    function TryRayEntrance(
      out Entrance: TVector3;
      const RayOrigin, RayDirection: TVector3): boolean; overload;
    { @groupEnd }

    function SegmentCollision(
      const Segment1, Segment2: TVector3): boolean;

    { Deprecated name for SegmentCollision. @deprecated @exclude }
    function IsSegmentCollision(
      const Segment1, Segment2: TVector3): boolean; deprecated;

    { Collision between axis-aligned box (TBox3D) and 3D plane.
      Returns detailed result as TPlaneCollision. }
    function PlaneCollision(const Plane: TVector4): TPlaneCollision;

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
    function PlaneCollisionInside(const Plane: TVector4): boolean;
    function PlaneCollisionOutside(const Plane: TVector4): boolean;
    { @groupEnd }

    function IsTriangleCollision(
      const Triangle: TTriangle3): boolean;

    { Smallest possible sphere completely enclosing the current box.
      When this is empty (@link(IsEmpty)) we return
      SphereRadiusSqr = 0 and an undefined SphereCenter.

      @param(SphereCenter The calculated sphere center.)
      @param(SphereRadiusSqr The calculated sphere radius, squared.

        Use @code(Sqrt) to get the actual value.
        Often the square of the radius is enough, and this way you can avoid
        calculating expensive @code(Sqrt), and thus gain some speed.

        But please remember the general guideline: "do not optimize when there's no need".
        Sometimes it is simpler to just use @code(Sqrt) to get the actual radius,
        and there's no measurable difference in performance.
        So don't worry and do @code(SphereRadius := Sqrt(SphereRadiusSqr)).)
    }
    procedure BoundingSphere(
      var SphereCenter: TVector3; var SphereRadiusSqr: Single);

    { Does it have any common part with another box.
      Better use @link(Collides),
      which has a name consistent with @link(TFloatRectangle.Collides),
      this method will be deprecated some day and later removed. }
    function Collision(const Box2: TBox3D): boolean;

    { Does it have any common part with another box. }
    function Collides(const Box2: TBox3D): boolean;

    { Radius of the minimal sphere that contains this box.
      Sphere center is assumed to be in (0, 0, 0).
      0 if box is empty. }
    function Radius: Single;

    { Radius of the minimal circle that contains the 2D projection of this box.
      2D box projection is obtained by rejecting the IgnoreIndex coordinate
      (must be 0, 1 or 2).
      Circle center is assumed to be in (0, 0).
      0 if box is empty. }
    function Radius2D(const IgnoreIndex: T3DAxis): Single;

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
      const SphereCenter: TVector3; const SphereRadius: Single): boolean;

    { Check box vs sphere collision. }
    function SphereCollision(
      const SphereCenter: TVector3; const SphereRadius: Single): boolean;

    { Check box vs sphere collision in 2D (ignores Z coordinates of box). }
    function SphereCollision2D(
      const SphereCenter: TVector2; const SphereRadius: Single): boolean;

    { Calculate a plane in 3D space with direction = given Direction, moved
      maximally in Direction and still intersecting the given Box.

      For example, if Direction = -Z = (0, 0, -1), then this will return
      the bottom plane of this box. For Direction = (1, 1, 1), this will return
      a plane intersecting the Data[1] (maximum) point, with slope = (1, 1, 1).
      The resulting plane always intersects at least one of the 8 corners of the box.

      @raises(EBox3DEmpty If the Box is empty.) }
    function MaximumPlane(const Direction: TVector3): TVector4;

    { Calculate a plane in 3D space with direction = given Direction, moved
      such that it touches the Box but takes minimum volume of this box.

      For example, if Direction = +Z = (0, 0, 1), then this will return
      the bottom plane of this box. For Direction = (1, 1, 1), this will return
      a plane intersecting the Data[0] (minimum) point, with slope = (1, 1, 1).
      The resulting plane always intersects at least one of the 8 corners of the box.

      @raises(EBox3DEmpty If the Box is empty.) }
    function MinimumPlane(const Direction: TVector3): TVector4;

    { Farthest corner of the box in the given Direction.
      @raises(EBox3DEmpty If the Box is empty.) }
    function MaximumCorner(const Direction: TVector3): TVector3;

    { Corner of the box such that the rest of the box lies in the given
      Direction from this corner.
      @raises(EBox3DEmpty If the Box is empty.) }
    function MinimumCorner(const Direction: TVector3): TVector3;

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
    procedure PointDistances(const P: TVector3;
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
      const Point, Dir: TVector3;
      out MinDistance, MaxDistance: Single);

    (* MaxDistanceAlongDirection not used, so not defined for now.
    { Maximum distance from Point to one of box corners, along the given direction.

      Like DirectionDistances, but only returns MaxDistance,
      and is faster. }
    function MaxDistanceAlongDirection(const Point, Dir: TVector3): Single;
    *)

    { Shortest distance between the box and a point.
      Always zero when the point is inside the box.

      @raises EBox3DEmpty When used with an empty box. }
    function PointDistance(const Point: TVector3): Single;

    { Shortest distance between the box and a point, squared.
      Always zero when the point is inside the box.

      @raises EBox3DEmpty When used with an empty box. }
    function PointDistanceSqr(const Point: TVector3): Single;

    (*
    { Maximum distance between the box and a point.
      Returns EmptyBoxDistance when box is empty. }
    function PointMaxDistance(const Point: TVector3;
      const EmptyBoxDistance: Single): Single;

    { Maximum distance between the box and a point, squared.
      Returns EmptyBoxDistance when box is empty. }
    function PointMaxDistanceSqr(const Point: TVector3;
      const EmptyBoxDistance: Single): Single;
    *)

    function Equal(const Box2: TBox3D): boolean; overload;
    function Equal(const Box2: TBox3D; const Epsilon: Single): boolean; overload;

    { Diagonal of the box, zero if empty. }
    function Diagonal: Single;

    function RectangleXY: TFloatRectangle;
    function RectangleXZ: TFloatRectangle;

    { Project box along a given direction to a 2D rectangle.
      @bold(Assumes that Dir, Side and Up vectors are already
      orthogonal and normalized.) }
    function OrthoProject(const Pos, Dir, Side, Up: TVector3): TFloatRectangle;

    { Compare two bounding boxes based
      on their Z coordinates, suitable for depth sorting in 2D.
      Follows the algorithm documented at @link(sort2D).
      Returns -1 if A < B, 1 if A > B, 0 if A = B.

      Using this with a typical sorting function will result
      in boxes back-to-front ordering, which means that the farthest
      box will be first. }
    class function CompareBackToFront2D(
      const A, B: TBox3D): Integer; static;
      deprecated 'rely on TCastleViewport.BlendingSort to sort shapes';

    class operator {$ifdef FPC}+{$else}Add{$endif} (const Box1, Box2: TBox3D): TBox3D;
    class operator {$ifdef FPC}+{$else}Add{$endif} (const B: TBox3D; const V: TVector3): TBox3D; deprecated 'use TBox3D.Translate. Operator is ambiguous (do we add a point, or translate?)';
    class operator {$ifdef FPC}+{$else}Add{$endif} (const V: TVector3; const B: TBox3D): TBox3D; deprecated 'use TBox3D.Translate. Operator is ambiguous (do we add a point, or translate?)';

    { Convert from center and size vector. Empty box has size (-1,-1,-1). }
    class function FromCenterSize(const ACenter, ASize: TVector3): TBox3D; static;
    { Convert to center and size vector. Empty box has size (-1,-1,-1). }
    procedure ToCenterSize(out ACenter, ASize: TVector3);
  end;

  TBox3DBool = array [boolean] of TVector3;
  PBox3D = ^TBox3D;

const
  { Special TBox3D value meaning "bounding box is empty".
    This is different than just bounding box with zero sizes,
    as bounding box with zero sizes still has some position.
    Empty bounding box doesn't contain any portion of 3D space. }
  EmptyBox3D: TBox3D = (Data: ((X: 0; Y: 0; Z: 0), (X: -1; Y: -1; Z: -1))) deprecated 'use TBox3D.Empty';

type
  TBox3DList = {$ifdef FPC}specialize{$endif} TStructList<TBox3D>;

{ Construct TBox3D value from a minimum and maximum 3D point. }
function Box3D(const p0, p1: TVector3): TBox3D;

{ Construct TBox3D value from a center and size.
  When any Size component is < 0, we return an empty box (@link(TBox3D.Empty)).
  This is consistent with X3D bboxCenter/Size definition e.g. at X3D Group node,
  see http://www.web3d.org/documents/specifications/19775-1/V3.2/Part01/components/group.html#Group
  @groupBegin }
function Box3DAroundPoint(const Pt: TVector3; Size: Single): TBox3D; overload;
function Box3DAroundPoint(const Pt: TVector3; Size: TVector3): TBox3D; overload;
{ @groupEnd }

{ Calculate bounding box of a set of 3D points.
  This calculates the smallest possible box enclosing all given points.
  For VertsCount = 0 this returns TBox3D.Empty.

  Overloaded version with Transform parameter transforms each point
  by given matrix.

  Overloaded version with GetVertex as a function uses GetVertex to query
  for indexes from [0 .. VertsCount - 1] range.

  As usual, VertsStride = 0 means VertsStride = SizeOf(TVector3).

  @groupBegin }
function CalculateBoundingBox(
  Verts: PVector3; VertsCount: Cardinal; VertsStride: Cardinal): TBox3D; overload;
function CalculateBoundingBox(
  Verts: PVector3; VertsCount: Cardinal; VertsStride: Cardinal;
  const Transform: TMatrix4): TBox3D; overload;
function CalculateBoundingBox(Verts: TVector3List): TBox3D; overload;
function CalculateBoundingBox(Verts: TVector3List;
  const Transform: TMatrix4): TBox3D; overload;
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
  const GetVertIndex: TGetIndexFromIndexNumFunc;
  const VertsIndicesCount: integer;
  const GetVertex: TGetVertexFromIndexFunc): TBox3D; overload;
function CalculateBoundingBoxFromIndices(
  const GetVertIndex: TGetIndexFromIndexNumFunc;
  const VertsIndicesCount: integer;
  const GetVertex: TGetVertexFromIndexFunc;
  const Transform: TMatrix4): TBox3D; overload;
{ @groupEnd }

function TriangleBoundingBox(const T: TTriangle3): TBox3D;

{ Tests for collision between box3d centered around (0, 0, 0)
  and a plane.

  Note that you can't express empty box3d here: all BoxHalfSize items
  must be >= 0. The case when size = 0 is considered like infintely small
  box in some dimension (e.g. if all three sizes are = 0 then the box
  becomes a point).  }
function IsCenteredBox3DPlaneCollision(
  const BoxHalfSize: TVector3;
  const Plane: TVector4): boolean;

{ Smallest possible box enclosing a sphere with Center, Radius. }
function BoundingBox3DFromSphere(const Center: TVector3;
  const Radius: Single): TBox3D;

type
  TBox3DEvent = function: TBox3D of object;

implementation

uses Math;

// Internal IsCenteredBox3DPlaneCollision alternative with Double-precision.
function IsCenteredBox3DPlaneCollisionDouble(
  const BoxHalfSize: TVector3Double;
  const Plane: TVector4Double): boolean; forward;

{ Special procedures for raising errors, to make GetMin and friends ultra-fast,
  so that they don't need an implicit try-finally section because they have a string. }

procedure RaiseGetMin;
begin
  raise EBox3DEmpty.Create('Empty box 3D (Box.IsEmpty), cannot get minimum point');
end;

procedure RaiseSetMin;
begin
  raise EBox3DEmpty.Create('Empty box 3D (Box.IsEmpty), cannot set minimum point. Set the whole box to non-empty using the global Box3D() function');
end;

procedure RaiseGetMax;
begin
  raise EBox3DEmpty.Create('Empty box 3D (Box.IsEmpty), cannot get maximum point');
end;

procedure RaiseSetMax;
begin
  raise EBox3DEmpty.Create('Empty box 3D (Box.IsEmpty), cannot set maximum point. Set the whole box to non-empty using the global Box3D() function');
end;

procedure RaiseRadius2DInvalidIgnoreIndex;
begin
  raise EInternalError.Create('Invalid IgnoreIndex for TBox3D.Radius2D');
end;

{ TBox3D --------------------------------------------------------------------- }

function TBox3D.GetMin: TVector3;
begin
  if IsEmpty then RaiseGetMin;
  Result := Data[0];
end;

procedure TBox3D.SetMin(const Value: TVector3);
begin
  if IsEmpty then RaiseSetMin;
  Data[0] := Value;
end;

function TBox3D.GetMax: TVector3;
begin
  if IsEmpty then RaiseGetMax;
  Result := Data[1];
end;

procedure TBox3D.SetMax(const Value: TVector3);
begin
  if IsEmpty then RaiseSetMax;
  Data[1] := Value;
end;

class function TBox3D.Empty: TBox3D;
const
  R: TBox3D = (Data: ((X: 0; Y: 0; Z: 0), (X: -1; Y: -1; Z: -1)));
begin
  Result := R;
end;

function TBox3D.IsEmpty: boolean;
begin
  Result := Data[0].X > Data[1].X;
end;

function TBox3D.IsEmptyOrZero: boolean;
begin
  Result := (Data[0].X > Data[1].X) or
    ( (Data[0].X = Data[1].X) and
      (Data[0].Y = Data[1].Y) and
      (Data[0].Z = Data[1].Z)
    );
end;

procedure TBox3D.CheckNonEmpty;
begin
  if IsEmpty then
    raise EBox3DEmpty.Create('Empty box 3d: no middle point, no sizes etc.');
end;

function TBox3D.Center: TVector3;
begin
  CheckNonEmpty;
  Result.X := (Data[0].X + Data[1].X) / 2;
  Result.Y := (Data[0].Y + Data[1].Y) / 2;
  Result.Z := (Data[0].Z + Data[1].Z) / 2;
end;

function TBox3D.Middle: TVector3;
begin
  Result := Center;
end;

function TBox3D.AverageSize: Single;
begin
  CheckNonEmpty;
  Result := (
    (Data[1].X - Data[0].X) +
    (Data[1].Y - Data[0].Y) +
    (Data[1].Z - Data[0].Z)) / 3;
end;

function TBox3D.AverageSize(const AllowZero: boolean;
  const EmptyBoxSize: Single): Single;
begin
  if IsEmpty then
    Result := EmptyBoxSize else
  begin
    Result := ((Data[1].X - Data[0].X) +
               (Data[1].Y - Data[0].Y) +
               (Data[1].Z - Data[0].Z)) / 3;
    if (not AllowZero) and (Result = 0) then
      Result := EmptyBoxSize;
  end;
end;

function TBox3D.AverageSize2D(const AllowZero: boolean;
  const EmptyBoxSize: Single; const IgnoreIndex: T3DAxis): Single;
begin
  if IsEmpty then
    Result := EmptyBoxSize else
  begin
    case IgnoreIndex of
      0: Result := ((Data[1].Y - Data[0].Y) +
                    (Data[1].Z - Data[0].Z)) / 2;
      1: Result := ((Data[1].X - Data[0].X) +
                    (Data[1].Z - Data[0].Z)) / 2;
      2: Result := ((Data[1].X - Data[0].X) +
                    (Data[1].Y - Data[0].Y)) / 2;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create(20221209);
      {$endif}
    end;
    if (not AllowZero) and (Result = 0) then
      Result := EmptyBoxSize;
  end;
end;

function TBox3D.MaxSize: Single;
begin
  CheckNonEmpty;
  Result := MaxValue([
    Data[1].X - Data[0].X,
    Data[1].Y - Data[0].Y,
    Data[1].Z - Data[0].Z
  ]);
end;

function TBox3D.MaxSize(const AllowZero: boolean;
  const EmptyBoxSize: Single): Single;
begin
  if IsEmpty then
    Result := EmptyBoxSize else
  begin
    Result := MaxValue([
      Data[1].X - Data[0].X,
      Data[1].Y - Data[0].Y,
      Data[1].Z - Data[0].Z
    ]);
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
    A := Data[1].X - Data[0].X;
    B := Data[1].Y - Data[0].Y;
    C := Data[1].Z - Data[0].Z;
    Result := 2*A*B + 2*B*C + 2*A*C;
    if (not AllowZero) and (Result = 0) then
      Result := EmptyBoxArea;
  end;
end;

function TBox3D.MinSize: Single;
begin
  CheckNonEmpty;

  Result := MinValue([
    Data[1].X - Data[0].X,
    Data[1].Y - Data[0].Y,
    Data[1].Z - Data[0].Z
  ]);

  { Another version is below (but this is slower without any benefit...)

    var sizes: TVector3;
      sizes := Box3DSizes(box);
      result := sizes[MaxVectorCoord(sizes)];
  }
end;

function TBox3D.SizeX: Single;
begin
  CheckNonEmpty;
  Result := Data[1].X - Data[0].X;
end;

function TBox3D.SizeY: Single;
begin
  CheckNonEmpty;
  Result := Data[1].Y - Data[0].Y;
end;

function TBox3D.SizeZ: Single;
begin
  CheckNonEmpty;
  Result := Data[1].Z - Data[0].Z;
end;

procedure TBox3D.ExpandMe(const AExpand: Single);
begin
 Data[0].X := Data[0].X - AExpand;
 Data[0].Y := Data[0].Y - AExpand;
 Data[0].Z := Data[0].Z - AExpand;

 Data[1].X := Data[1].X + AExpand;
 Data[1].Y := Data[1].Y + AExpand;
 Data[1].Z := Data[1].Z + AExpand;
end;

procedure TBox3D.ExpandMe(const AExpand: TVector3);
begin
 Data[0].X := Data[0].X - AExpand.X;
 Data[0].Y := Data[0].Y - AExpand.Y;
 Data[0].Z := Data[0].Z - AExpand.Z;

 Data[1].X := Data[1].X + AExpand.X;
 Data[1].Y := Data[1].Y + AExpand.Y;
 Data[1].Z := Data[1].Z + AExpand.Z;
end;

function TBox3D.Grow(const AExpand: Single): TBox3D;
begin
  if IsEmpty then Exit(Empty);

  Result.Data[0].X := Data[0].X - AExpand;
  Result.Data[0].Y := Data[0].Y - AExpand;
  Result.Data[0].Z := Data[0].Z - AExpand;

  Result.Data[1].X := Data[1].X + AExpand;
  Result.Data[1].Y := Data[1].Y + AExpand;
  Result.Data[1].Z := Data[1].Z + AExpand;
end;

function TBox3D.Grow(const AExpand: TVector3): TBox3D;
begin
  if IsEmpty then Exit(Empty);

  Result.Data[0].X := Data[0].X - AExpand.X;
  Result.Data[0].Y := Data[0].Y - AExpand.Y;
  Result.Data[0].Z := Data[0].Z - AExpand.Z;

  Result.Data[1].X := Data[1].X + AExpand.X;
  Result.Data[1].Y := Data[1].Y + AExpand.Y;
  Result.Data[1].Z := Data[1].Z + AExpand.Z;
end;

function TBox3D.Expand(const AExpand: Single): TBox3D;
begin
  Result := Grow(AExpand);
end;

function TBox3D.Expand(const AExpand: TVector3): TBox3D;
begin
  Result := Grow(AExpand);
end;

function TBox3D.Contains(const Point: TVector3): boolean;
begin
  if IsEmpty then Exit(false);
  Result :=
    (Data[0].X <= Point.X) and (Point.X <=  Data[1].X) and
    (Data[0].Y <= Point.Y) and (Point.Y <=  Data[1].Y) and
    (Data[0].Z <= Point.Z) and (Point.Z <=  Data[1].Z);
end;

{ Causes FPC error:
  Error: Asm: Duplicate label CASTLEBOXES/home/michalis/bin/castle-engineTBOX3D_$__$$_CONTAINS$TGENERICVECTOR3$$BOOLEAN
  See https://bugs.freepascal.org/view.php?id=32188

function TBox3D.Contains(const Point: TVector3Double): boolean;
begin
  if IsEmpty then Exit(false);
  Result :=
    (Data[0].X <= Point.X) and (Point.X <=  Data[1].X) and
    (Data[0].Y <= Point.Y) and (Point.Y <=  Data[1].Y) and
    (Data[0].Z <= Point.Z) and (Point.Z <=  Data[1].Z);
end;
}

function TBox3D.PointInside(const Point: TVector3): boolean;
begin
  Result := Contains(Point);
end;

{ Causes FPC error:
  Error: Asm: Duplicate label CASTLEBOXES/home/michalis/bin/castle-engineTBOX3D_$__$$_POINTINSIDE$TGENERICVECTOR3$$BOOLEAN
  See https://bugs.freepascal.org/view.php?id=32188

function TBox3D.PointInside(const Point: TVector3Double): boolean;
begin
  Result := Contains(Point);
end;
}

{ Separated from Contains2D, to not slowdown it by implicit
  try/finally section because we use string. }
procedure Contains2D_InvalidIgnoreIndex;
begin
  raise EInternalError.Create('Invalid IgnoreIndex for TBox3D.Contains2D');
end;

function TBox3D.Contains2D(const Point: TVector2): boolean;
begin
  if IsEmpty then Exit(false);
  Result :=
    (Data[0].X <= Point.X) and (Point.X <=  Data[1].X) and
    (Data[0].Y <= Point.Y) and (Point.Y <=  Data[1].Y);
end;

function TBox3D.Contains2D(const Point: TVector3;
  const IgnoreIndex: T3DAxis): boolean;
begin
  if IsEmpty then Exit(false);
  case IgnoreIndex of
    0: Result :=
         (Data[0].Y <= Point.Y) and (Point.Y <=  Data[1].Y) and
         (Data[0].Z <= Point.Z) and (Point.Z <=  Data[1].Z);
    1: Result :=
         (Data[0].Z <= Point.Z) and (Point.Z <=  Data[1].Z) and
         (Data[0].X <= Point.X) and (Point.X <=  Data[1].X);
    2: Result :=
         (Data[0].X <= Point.X) and (Point.X <=  Data[1].X) and
         (Data[0].Y <= Point.Y) and (Point.Y <=  Data[1].Y);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else
      begin
        Contains2D_InvalidIgnoreIndex;
        Result :=  false; // just silence Delphi warning
      end;
    {$endif}
  end;
end;

function TBox3D.PointInside2D(const Point: TVector2): boolean;
begin
  Result := Contains2D(Point);
end;

function TBox3D.PointInside2D(const Point: TVector3;
  const IgnoreIndex: T3DAxis): boolean;
begin
  Result := Contains2D(Point, IgnoreIndex);
end;

procedure TBox3D.Include(const box2: TBox3D);
begin
  if Box2.IsEmpty then
    Exit else
  if IsEmpty then
    Data := Box2.Data else
  begin
    MinVar(Data[0].X, box2.Data[0].X);
    MaxVar(Data[1].X, box2.Data[1].X);
    MinVar(Data[0].Y, box2.Data[0].Y);
    MaxVar(Data[1].Y, box2.Data[1].Y);
    MinVar(Data[0].Z, box2.Data[0].Z);
    MaxVar(Data[1].Z, box2.Data[1].Z);
  end;
end;

procedure TBox3D.Include(const Point: TVector3);
begin
  if IsEmpty then
  begin
    Data[0] := Point;
    Data[1] := Point;
  end else
  begin
    MinVar(Data[0].X, Point.X);
    MaxVar(Data[1].X, Point.X);
    MinVar(Data[0].Y, Point.Y);
    MaxVar(Data[1].Y, Point.Y);
    MinVar(Data[0].Z, Point.Z);
    MaxVar(Data[1].Z, Point.Z);
  end;
end;

procedure TBox3D.Include(const Points: TVector3List);
var
  V: TVector3;
  I, StartIndex: Integer;
begin
  if IsEmpty then
  begin
    Data[0] := Points.L[0];
    Data[1] := Points.L[0];
    StartIndex := 1;
  end else
    StartIndex := 0;

  for I := StartIndex to Points.Count - 1 do
  begin
    V := Points.L[I];
    MinVar(Data[0].X, V.X);
    MaxVar(Data[1].X, V.X);
    MinVar(Data[0].Y, V.Y);
    MaxVar(Data[1].Y, V.Y);
    MinVar(Data[0].Z, V.Z);
    MaxVar(Data[1].Z, V.Z);
  end;
end;

class function TBox3D.FromPoints(const Points: TVector3List): TBox3D;
var
  V: TVector3;
  I: Integer;
begin
  if Points.Count = 0 then
    Result := TBox3D.Empty
  else
  begin
    Result.Data[0] := Points.L[0];
    Result.Data[1] := Points.L[0];
    for I := 1 to Points.Count - 1 do
    begin
      { Note: On Delphi, we *have to* use L[...] below and depend on $pointermath on,
        instead of using List^[...].
        That's because on Delphi, List^[...] may have too small (declared) upper size
        due to Delphi not supporting SizeOf(T) in generics.
        See https://github.com/castle-engine/castle-engine/issues/474 . }
      V := Points.L[I];
      MinVar(Result.Data[0].X, V.X);
      MaxVar(Result.Data[1].X, V.X);
      MinVar(Result.Data[0].Y, V.Y);
      MaxVar(Result.Data[1].Y, V.Y);
      MinVar(Result.Data[0].Z, V.Z);
      MaxVar(Result.Data[1].Z, V.Z);
    end;
  end;
end;

function TBox3D.Size: TVector3;
begin
  CheckNonEmpty;
  Result.X := Data[1].X - Data[0].X;
  Result.Y := Data[1].Y - Data[0].Y;
  Result.Z := Data[1].Z - Data[0].Z;
end;

function TBox3D.Sizes: TVector3;
begin
  Result := Size;
end;

procedure TBox3D.GetAllPoints(AllPoints: PVector3Array);
begin
  AllPoints^[0] := Vector3(Data[0].X, Data[0].Y, Data[0].Z);
  AllPoints^[1] := Vector3(Data[0].X, Data[0].Y, Data[1].Z);
  AllPoints^[2] := Vector3(Data[0].X, Data[1].Y, Data[0].Z);
  AllPoints^[3] := Vector3(Data[0].X, Data[1].Y, Data[1].Z);

  AllPoints^[4] := Vector3(Data[1].X, Data[0].Y, Data[0].Z);
  AllPoints^[5] := Vector3(Data[1].X, Data[0].Y, Data[1].Z);
  AllPoints^[6] := Vector3(Data[1].X, Data[1].Y, Data[0].Z);
  AllPoints^[7] := Vector3(Data[1].X, Data[1].Y, Data[1].Z);
end;

procedure TBox3D.Corners(out AllPoints: TBoxCorners);
begin
  {$warnings off} // using deprecated knowingly
  GetAllPoints(@AllPoints);
  {$warnings on}
end;

function TBox3D.Transform(
  const Matrix: TMatrix4): TBox3D;

  function Slower(const Matrix: TMatrix4): TBox3D;
  var
    BoxPoints: TBoxCorners;
    i: integer;
  begin
    Corners(BoxPoints);
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
      if BoxPoints[I].X < Result.Data[0].X then Result.Data[0].X := BoxPoints[I].X;
      if BoxPoints[I].Y < Result.Data[0].Y then Result.Data[0].Y := BoxPoints[I].Y;
      if BoxPoints[I].Z < Result.Data[0].Z then Result.Data[0].Z := BoxPoints[I].Z;

      if BoxPoints[I].X > Result.Data[1].X then Result.Data[1].X := BoxPoints[I].X;
      if BoxPoints[I].Y > Result.Data[1].Y then Result.Data[1].Y := BoxPoints[I].Y;
      if BoxPoints[I].Z > Result.Data[1].Z then Result.Data[1].Z := BoxPoints[I].Z;
    end;
  end;

  function Faster(const Matrix: TMatrix4): TBox3D;
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
    Move(Matrix.Data[3], Result.Data[0], SizeOf(Result.Data[0]));
    Move(Matrix.Data[3], Result.Data[1], SizeOf(Result.Data[1]));

    for I := 0 to 2 do
    begin
      { Calculate Result[0].Data[I], Result[1].Data[I] }
      for J := 0 to 2 do
      begin
        A := Matrix.Data[J, I] * Data[0].Data[J];
        B := Matrix.Data[J, I] * Data[1].Data[J];
        if A < B then
        begin
          Result.Data[0].Data[I] := Result.Data[0].Data[I] + A;
          Result.Data[1].Data[I] := Result.Data[1].Data[I] + B;
        end else
        begin
          Result.Data[0].Data[I] := Result.Data[0].Data[I] + B;
          Result.Data[1].Data[I] := Result.Data[1].Data[I] + A;
        end;
      end;
    end;
  end;

begin
  if IsEmpty then
    Exit(Empty);

  if (Matrix.Data[0, 3] = 0) and
     (Matrix.Data[1, 3] = 0) and
     (Matrix.Data[2, 3] = 0) and
     (Matrix.Data[3, 3] = 1) then
    Result := Faster(Matrix)
  else
    Result := Slower(Matrix);
end;

function TBox3D.Translate(
  const Translation: TVector3): TBox3D;
begin
  if not IsEmpty then
  begin
    Result.Data[0] := Data[0] + Translation;
    Result.Data[1] := Data[1] + Translation;
  end else
    Result := Empty;
end;

function TBox3D.AntiTranslate(
  const Translation: TVector3): TBox3D;
begin
  if not IsEmpty then
  begin
    Result.Data[0] := Data[0] - Translation;
    Result.Data[1] := Data[1] - Translation;
  end else
    Result := Empty;
end;

function TBox3D.ToString: string;
begin
  if IsEmpty then
    Result := 'EMPTY'
  else
    Result := '(Min: ' + Data[0].ToString + ') - (Max: ' + Data[1].ToString + ')';
end;

function TBox3D.ToRawString: string;
begin
  if IsEmpty then
    Result := 'EMPTY'
  else
    Result := '(Min: ' + Data[0].ToRawString + ') - (Max: ' + Data[1].ToRawString + ')';
end;

function TBox3D.ToNiceStr: string;
begin
  Result := ToString;
end;

function TBox3D.ToRawStr: string;
begin
  Result := ToRawString;
end;

procedure TBox3D.ClampVar(var point: TVector3);
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    if Point.Data[I] < Data[0][I] then
      Point.Data[I] := Data[0][I]
    else
    if Point.Data[I] > Data[1][I] then
      Point.Data[I] := Data[1][I];
  end;
end;

{ Causes FPC errors:
  Error: Asm: Duplicate label CASTLEBOXES/home/michalis/bin/castle-engineTBOX3D_$__$$_CLAMPVAR$TGENERICVECTOR3
  See https://bugs.freepascal.org/view.php?id=32188

procedure TBox3D.ClampVar(var point: TVector3Double);
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    if Point.Data[I] < Data[0].Data[I] then
      Point.Data[I] := Data[0].Data[I]
    else
    if Point.Data[I] > Data[1].Data[I] then
      Point.Data[I] := Data[1].Data[I];
  end;
end;
}

function TBox3D.TryRayClosestIntersection(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  IntrProposed: boolean absolute result;

  procedure ProposeBoxIntr(const PlaneConstCoord: T3DAxis;
    const PlaneConstValue: Single);
  var
    NowIntersection: TVector3;
    NowIntersectionDistance: Single;
    c1, c2: T3DAxis;
  begin
    if TrySimplePlaneRayIntersection(NowIntersection, NowIntersectionDistance,
      PlaneConstCoord, PlaneConstValue, RayOrigin, RayDirection) then
    begin
      RestOf3DCoords(PlaneConstCoord, c1, c2);
      if Between(NowIntersection[c1], Data[0][c1], Data[1][c1]) and
         Between(NowIntersection[c2], Data[0][c2], Data[1][c2]) then
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
    { wykorzystujemy ponizej fakt ze jezeli RayOrigin[i] < Data[0][i] to na pewno
      promien ktory przecinalby scianke Data[1][i] pudelka przecinalby najpierw
      tez inna scianke. Wiec jezeli RayOrigin[i] < Data[0][i] to nie musimy sprawdzac
      przeciecia z plaszczyzna Data[1][i]. }
    if RayOrigin[i] < Data[0][i] then
      ProposeBoxIntr(i, Data[0][i]) else
    if RayOrigin[i] > Data[1][i] then
      ProposeBoxIntr(i, Data[1][i]) else
    begin
      ProposeBoxIntr(i, Data[0][i]);
      ProposeBoxIntr(i, Data[1][i]);
    end;
  end;
end;

function TBox3D.TryRayClosestIntersection(
  out Intersection: TVector3;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  IntersectionDistance: Single;
begin
  Result := TryRayClosestIntersection(
    Intersection, IntersectionDistance, RayOrigin, RayDirection);
end;

function TBox3D.TryRayClosestIntersection(
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  Intersection: TVector3;
begin
  Result := TryRayClosestIntersection(
    Intersection, IntersectionDistance, RayOrigin, RayDirection);
end;

function TBox3D.TryRayEntrance(
  out Entrance: TVector3; out EntranceDistance: Single;
  const RayOrigin, RayDirection: TVector3): boolean;
begin
  if Contains(RayOrigin) then
  begin
    Entrance := RayOrigin;
    EntranceDistance := 0;
    result := true;
  end else
    result := TryRayClosestIntersection(Entrance, EntranceDistance, RayOrigin, RayDirection);
end;

function TBox3D.TryRayEntrance(
  out Entrance: TVector3;
  const RayOrigin, RayDirection: TVector3): boolean;
begin
  if Contains(RayOrigin) then
  begin
    Entrance := RayOrigin;
    result := true;
  end else
    result := TryRayClosestIntersection(Entrance, RayOrigin, RayDirection);
end;

function TBox3D.IsSegmentCollision(
  const Segment1, Segment2: TVector3): boolean;
begin
  Result := SegmentCollision(Segment1, Segment2);
end;

function TBox3D.SegmentCollision(
  const Segment1, Segment2: TVector3): boolean;

  function IsCollisionWithBoxPlane(const PlaneConstCoord: T3DAxis;
    const PlaneConstValue: Single): boolean;
  var
    NowIntersection: TVector3;
    c1, c2: T3DAxis;
  begin
    if TrySimplePlaneSegmentIntersection(NowIntersection,
      PlaneConstCoord, PlaneConstValue, Segment1, Segment2) then
    begin
      RestOf3DCoords(PlaneConstCoord, c1, c2);
      Result :=
        Between(NowIntersection[c1], Data[0][c1], Data[1][c1]) and
        Between(NowIntersection[c2], Data[0][c2], Data[1][c2]);
    end else
      Result := false;
  end;

var
  I: integer;
begin
  for I := 0 to 2 do
  begin
    { wykorzystujemy ponizej fakt ze jezeli Segment1[i] < Data[0][i] to na pewno
      promien ktory przecinalby scianke Data[1][i] pudelka przecinalby najpierw
      tez inna scianke. Wiec jezeli Segment1[i] < Data[0][i] to nie musimy sprawdzac
      przeciecia z plaszczyzna Data[1][i]. }
    if Segment1[i] < Data[0][i] then
    begin
      if IsCollisionWithBoxPlane(i, Data[0][i]) then Exit(true);
    end else
    if Segment1[i] > Data[1][i] then
    begin
      if IsCollisionWithBoxPlane(i, Data[1][i]) then Exit(true);
    end else
    begin
      if IsCollisionWithBoxPlane(i, Data[0][i]) then Exit(true);
      if IsCollisionWithBoxPlane(i, Data[1][i]) then Exit(true);
    end;
  end;

  Result := false;
end;

function TBox3D.PlaneCollision(
  const Plane: TVector4): TPlaneCollision;
{ This generalizes the idea from IsCenteredBox3DPlaneCollision
  in castleboxes_generic_float.inc.
  It's also explained in
  Akenine-Moller, Haines "Real-Time Rendering" (2nd ed), 13.9 (page 586)
}
var
  I: Integer;
  VMin, VMax: TVector3;
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
    VMin.Data[I] := BoxBool[not B][I];
    VMax.Data[I] := BoxBool[B][I];
  end;

  if Plane.X * VMin.X +
     Plane.Y * VMin.Y +
     Plane.Z * VMin.Z +
     Plane.W > 0 then
    Exit(pcOutside);

  if Plane.X * VMax.X +
     Plane.Y * VMax.Y +
     Plane.Z * VMax.Z +
     Plane.W < 0 then
    Exit(pcInside);

  Result := pcIntersecting;
end;

function TBox3D.PlaneCollisionInside(
  const Plane: TVector4): boolean;
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
    BoxBool[Plane.X >= 0].X * Plane.X +
    BoxBool[Plane.Y >= 0].Y * Plane.Y +
    BoxBool[Plane.Z >= 0].Z * Plane.Z +
    Plane.W < 0;
end;

function TBox3D.PlaneCollisionOutside(
  const Plane: TVector4): boolean;
var
  BoxBool: TBox3DBool absolute Data;
begin
  if IsEmpty then
    Exit(false);

  Result :=
    BoxBool[Plane.X < 0].X * Plane.X +
    BoxBool[Plane.Y < 0].Y * Plane.Y +
    BoxBool[Plane.Z < 0].Z * Plane.Z +
    Plane.W > 0;
end;

function TBox3D.IsTriangleCollision(const Triangle: TTriangle3): boolean;

{ Implementation based on
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
  { Special equality epsilon used by IsCenteredBox3DPlaneCollision.
    For implementation reasons, they always
    use Double precision (even when called with arguments with Single precision),
    and still have to use epsilon slightly larger than usual
    CastleVectors.DoubleEpsilon. }
  Epsilon: Double = 1e-5;

{ It's better to make these calculations using Double precision. }
{$define IsTriangleCollision_DoublePrecision}

type
  TVector3 = {$ifdef IsTriangleCollision_DoublePrecision}
    CastleVectors.TVector3Double {$else}
    CastleVectors.TVector3 {$endif};
  TVector4 = {$ifdef IsTriangleCollision_DoublePrecision}
    CastleVectors.TVector4Double {$else}
    CastleVectors.TVector4 {$endif};
  TScalar = {$ifdef IsTriangleCollision_DoublePrecision}
    Double {$else}
    Single {$endif};
  TTriangle3 = array [0..2] of TVector3;

var
  TriangleMoved: TTriangle3;
  BoxHalfSize: TVector3;

  { ======================== X-tests ======================== }
  function AXISTEST_X01(const a, b, fa, fb: TScalar): boolean;
  var
    p0, p2, rad, min, max: TScalar;
  begin
    p0 := a * TriangleMoved[0].Y - b * TriangleMoved[0].Z;
    p2 := a * TriangleMoved[2].Y - b * TriangleMoved[2].Z;
    if p0<p2 then begin min := p0; max := p2; end else
                  begin min := p2; max := p0; end;
    rad := fa * BoxHalfSize.Y + fb * BoxHalfSize.Z;
    Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
  end;

  function AXISTEST_X2(const a, b, fa, fb: TScalar): boolean;
  var
    p0, p1, rad, min, max: TScalar;
  begin
    p0 := a * TriangleMoved[0].Y - b * TriangleMoved[0].Z;
    p1 := a * TriangleMoved[1].Y - b * TriangleMoved[1].Z;
    if p0<p1 then begin min := p0; max := p1; end else
                  begin min := p1; max := p0; end;
    rad := fa * BoxHalfSize.Y + fb * BoxHalfSize.Z;
    Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
  end;

  { ======================== Y-tests ======================== }
  function AXISTEST_Y02(const a, b, fa, fb: TScalar): boolean;
  var
    p0, p2, rad, min, max: TScalar;
  begin
    p0 := -a * TriangleMoved[0].X + b * TriangleMoved[0].Z;
    p2 := -a * TriangleMoved[2].X + b * TriangleMoved[2].Z;
    if p0<p2 then begin min := p0; max := p2; end else
                  begin min := p2; max := p0; end;
    rad := fa * BoxHalfSize.X + fb * BoxHalfSize.Z;
    Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
  end;

  function AXISTEST_Y1(const a, b, fa, fb: TScalar): boolean;
  var
    p0, p1, rad, min, max: TScalar;
  begin
    p0 := -a * TriangleMoved[0].X + b * TriangleMoved[0].Z;
    p1 := -a * TriangleMoved[1].X + b * TriangleMoved[1].Z;
    if p0<p1 then begin min := p0; max := p1; end else
                  begin min := p1; max := p0; end;
    rad := fa * BoxHalfSize.X + fb * BoxHalfSize.Z;
    Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
  end;

  { ======================== Z-tests ======================== }
  function AXISTEST_Z12(const a, b, fa, fb: TScalar): boolean;
  var
    p1, p2, rad, min, max: TScalar;
  begin
    p1 := a * TriangleMoved[1].X - b * TriangleMoved[1].Y;
    p2 := a * TriangleMoved[2].X - b * TriangleMoved[2].Y;
    if p2<p1 then begin min := p2; max := p1; end else
                  begin min := p1; max := p2; end;
    rad := fa * BoxHalfSize.X + fb * BoxHalfSize.Y;
    Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
  end;

  function AXISTEST_Z0(const a, b, fa, fb: TScalar): boolean;
  var
    p0, p1, rad, min, max: TScalar;
  begin
    p0 := a * TriangleMoved[0].X - b * TriangleMoved[0].Y;
    p1 := a * TriangleMoved[1].X - b * TriangleMoved[1].Y;
    if p0<p1 then begin min := p0; max := p1; end else
                  begin min := p1; max := p0; end;
    rad := fa * BoxHalfSize.X + fb * BoxHalfSize.Y;
    Result := (min > rad + Epsilon) or (max < -rad - Epsilon);
  end;

var
  BoxCenter: TVector3;
  I: Integer;
  TriangleEdges: TTriangle3;
  EdgeAbs: TVector3;
  TriMin, TriMax: TScalar;
  Plane: TVector4;
  PlaneDir: TVector3 absolute Plane;
begin
  if IsEmpty then
    Exit(false);

  { calculate BoxCenter and BoxHalfSize }
  for I := 0 to 2 do
  begin
    BoxCenter.Data[I] := (Data[0][I] + Data[1][I]) / 2;
    BoxHalfSize.Data[I] := (Data[1][I] - Data[0][I]) / 2;
  end;

  { calculate TriangleMoved (Triangle shifted by -BoxCenter,
    so that we can treat the BoxHalfSize as centered around origin) }
  TriangleMoved[0] := {$ifdef IsTriangleCollision_DoublePrecision}Vector3Double{$endif}(Triangle.Data[0]) - BoxCenter;
  TriangleMoved[1] := {$ifdef IsTriangleCollision_DoublePrecision}Vector3Double{$endif}(Triangle.Data[1]) - BoxCenter;
  TriangleMoved[2] := {$ifdef IsTriangleCollision_DoublePrecision}Vector3Double{$endif}(Triangle.Data[2]) - BoxCenter;

  { calculate TriangleMoved edges }
  TriangleEdges[0] := TriangleMoved[1] - TriangleMoved[0];
  TriangleEdges[1] := TriangleMoved[2] - TriangleMoved[1];
  TriangleEdges[2] := TriangleMoved[0] - TriangleMoved[2];

  { tests 3) }
  EdgeAbs.X := Abs(TriangleEdges[0].X);
  EdgeAbs.Y := Abs(TriangleEdges[0].Y);
  EdgeAbs.Z := Abs(TriangleEdges[0].Z);
  if AXISTEST_X01(TriangleEdges[0].Z, TriangleEdges[0].Y, EdgeAbs.Z, EdgeAbs.Y) then Exit(false);
  if AXISTEST_Y02(TriangleEdges[0].Z, TriangleEdges[0].X, EdgeAbs.Z, EdgeAbs.X) then Exit(false);
  if AXISTEST_Z12(TriangleEdges[0].Y, TriangleEdges[0].X, EdgeAbs.Y, EdgeAbs.X) then Exit(false);

  EdgeAbs.X := Abs(TriangleEdges[1].X);
  EdgeAbs.Y := Abs(TriangleEdges[1].Y);
  EdgeAbs.Z := Abs(TriangleEdges[1].Z);
  if AXISTEST_X01(TriangleEdges[1].Z, TriangleEdges[1].Y, EdgeAbs.Z, EdgeAbs.Y) then Exit(false);
  if AXISTEST_Y02(TriangleEdges[1].Z, TriangleEdges[1].X, EdgeAbs.Z, EdgeAbs.X) then Exit(false);
  if AXISTEST_Z0 (TriangleEdges[1].Y, TriangleEdges[1].X, EdgeAbs.Y, EdgeAbs.X) then Exit(false);

  EdgeAbs.X := Abs(TriangleEdges[2].X);
  EdgeAbs.Y := Abs(TriangleEdges[2].Y);
  EdgeAbs.Z := Abs(TriangleEdges[2].Z);
  if AXISTEST_X2 (TriangleEdges[2].Z, TriangleEdges[2].Y, EdgeAbs.Z, EdgeAbs.Y) then Exit(false);
  if AXISTEST_Y1 (TriangleEdges[2].Z, TriangleEdges[2].X, EdgeAbs.Z, EdgeAbs.X) then Exit(false);
  if AXISTEST_Z12(TriangleEdges[2].Y, TriangleEdges[2].X, EdgeAbs.Y, EdgeAbs.X) then Exit(false);

  { tests 1)
    first test overlap in the (x,y,z)-directions
    find TriMin, TriMax of the triangle each direction, and test for overlap in
    that direction -- this is equivalent to testing a minimal AABB around
    the triangle against the AABB }

  { test in X-direction }
  MinMax(TriangleMoved[0].X, TriangleMoved[1].X, TriangleMoved[2].X, TriMin, TriMax);
  if (TriMin >  boxhalfsize.X + Epsilon) or
     (TriMax < -boxhalfsize.X - Epsilon) then Exit(false);

  { test in Y-direction }
  MinMax(TriangleMoved[0].Y, TriangleMoved[1].Y, TriangleMoved[2].Y, TriMin, TriMax);
  if (TriMin >  boxhalfsize.Y + Epsilon) or
     (TriMax < -boxhalfsize.Y - Epsilon) then Exit(false);

  { test in Z-direction }
  MinMax(TriangleMoved[0].Z, TriangleMoved[1].Z, TriangleMoved[2].Z, TriMin, TriMax);
  if (TriMin >  boxhalfsize.Z + Epsilon) or
     (TriMax < -boxhalfsize.Z - Epsilon) then Exit(false);

  { tests 2)
    test if the box intersects the plane of the triangle
    compute plane equation of triangle: normal*x+d=0 }
  PlaneDir := TVector3.CrossProduct(TriangleEdges[0], TriangleEdges[1]);
  Plane.W := -TVector3.DotProduct(PlaneDir, TriangleMoved[0]);
  if not {$ifdef IsTriangleCollision_DoublePrecision}
         IsCenteredBox3DPlaneCollisionDouble{$else}
         IsCenteredBox3DPlaneCollision{$endif}
         (BoxHalfSize, Plane) then
    Exit(false);

  Result := true; { box and triangle overlaps }
end;

procedure TBox3D.BoundingSphere(
  var SphereCenter: TVector3; var SphereRadiusSqr: Single);
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
  Result := Collides(Box2);
end;

function TBox3D.Collides(const Box2: TBox3D): boolean;
begin
  Result :=
    (not IsEmpty) and
    (not Box2.IsEmpty) and
    (not ((Data[1].X < Box2.Data[0].X) or (Box2.Data[1].X < Data[0].X))) and
    (not ((Data[1].Y < Box2.Data[0].Y) or (Box2.Data[1].Y < Data[0].Y))) and
    (not ((Data[1].Z < Box2.Data[0].Z) or (Box2.Data[1].Z < Data[0].Z)));
end;

function TBox3D.Radius: Single;
begin
  if IsEmpty then
    Result := 0
  else
    Result := Sqrt(MaxValue([
      Vector3(Data[0].X, Data[0].Y, Data[0].Z).LengthSqr,
      Vector3(Data[1].X, Data[0].Y, Data[0].Z).LengthSqr,
      Vector3(Data[1].X, Data[1].Y, Data[0].Z).LengthSqr,
      Vector3(Data[0].X, Data[1].Y, Data[0].Z).LengthSqr,
      Vector3(Data[0].X, Data[0].Y, Data[1].Z).LengthSqr,
      Vector3(Data[1].X, Data[0].Y, Data[1].Z).LengthSqr,
      Vector3(Data[1].X, Data[1].Y, Data[1].Z).LengthSqr,
      Vector3(Data[0].X, Data[1].Y, Data[1].Z).LengthSqr
    ]));
end;

function TBox3D.Radius2D(const IgnoreIndex: T3DAxis): Single;
begin
  if IsEmpty then
    Result := 0 else
  begin
    case IgnoreIndex of
      0: Result := MaxValue([
           Vector2(Data[0].Y, Data[0].Z).LengthSqr,
           Vector2(Data[1].Y, Data[0].Z).LengthSqr,
           Vector2(Data[1].Y, Data[1].Z).LengthSqr,
           Vector2(Data[0].Y, Data[1].Z).LengthSqr
         ]);
      1: Result := MaxValue([
           Vector2(Data[0].Z, Data[0].X).LengthSqr,
           Vector2(Data[1].Z, Data[0].X).LengthSqr,
           Vector2(Data[1].Z, Data[1].X).LengthSqr,
           Vector2(Data[0].Z, Data[1].X).LengthSqr
         ]);
      2: Result := MaxValue([
           Vector2(Data[0].X, Data[0].Y).LengthSqr,
           Vector2(Data[1].X, Data[0].Y).LengthSqr,
           Vector2(Data[1].X, Data[1].Y).LengthSqr,
           Vector2(Data[0].X, Data[1].Y).LengthSqr
         ]);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else
        begin
          RaiseRadius2DInvalidIgnoreIndex;
          Result :=  0; // just silence Delphi warning
        end;
      {$endif}
    end;

    Result := Sqrt(Result);
  end;
end;

function TBox3D.SphereSimpleCollision(
  const SphereCenter: TVector3; const SphereRadius: Single): boolean;
begin
  Result := (not IsEmpty) and
    (SphereCenter.X >= Data[0].X - SphereRadius) and
    (SphereCenter.X <= Data[1].X + SphereRadius) and
    (SphereCenter.Y >= Data[0].Y - SphereRadius) and
    (SphereCenter.Y <= Data[1].Y + SphereRadius) and
    (SphereCenter.Z >= Data[0].Z - SphereRadius) and
    (SphereCenter.Z <= Data[1].Z + SphereRadius);
end;

function TBox3D.SphereCollision(
  const SphereCenter: TVector3; const SphereRadius: Single): boolean;
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

  if SphereCenter.X < Data[0].X then D := D + (Sqr(SphereCenter.X - Data[0].X)) else
  if SphereCenter.X > Data[1].X then D := D + (Sqr(SphereCenter.X - Data[1].X));

  if SphereCenter.Y < Data[0].Y then D := D + (Sqr(SphereCenter.Y - Data[0].Y)) else
  if SphereCenter.Y > Data[1].Y then D := D + (Sqr(SphereCenter.Y - Data[1].Y));

  if SphereCenter.Z < Data[0].Z then D := D + (Sqr(SphereCenter.Z - Data[0].Z)) else
  if SphereCenter.Z > Data[1].Z then D := D + (Sqr(SphereCenter.Z - Data[1].Z));

  Result := D <= Sqr(SphereRadius);
end;

function TBox3D.SphereCollision2D(
  const SphereCenter: TVector2; const SphereRadius: Single): boolean;
var
  D: Single;
begin
  if IsEmpty then Exit(false);

  D := 0;

  if SphereCenter.X < Data[0].X then D := D + (Sqr(SphereCenter.X - Data[0].X)) else
  if SphereCenter.X > Data[1].X then D := D + (Sqr(SphereCenter.X - Data[1].X));

  if SphereCenter.Y < Data[0].Y then D := D + (Sqr(SphereCenter.Y - Data[0].Y)) else
  if SphereCenter.Y > Data[1].Y then D := D + (Sqr(SphereCenter.Y - Data[1].Y));

  Result := D <= Sqr(SphereRadius);
end;

function TBox3D.MaximumPlane(
  const Direction: TVector3): TVector4;
var
  BoxBool: TBox3DBool absolute Data;
  ResultDir: TVector3 absolute Result;
begin
  CheckNonEmpty;

  { first 3 plane components are just copied from Direction }
  ResultDir := Direction;

(*
  { calculate box corner that intersects resulting plane:
    just choose appropriate coords using Direction. }
  P[0] := BoxBool[Direction[0] >= 0].X;
  P[1] := BoxBool[Direction[1] >= 0].Y;
  P[2] := BoxBool[Direction[2] >= 0].Z;

  { calculate 4th plane component.
    Plane must intersect P, so
      P[0] * Result[0] + .... + Result[3] = 0
  }
  Result[3] := - (P[0] * Result[0] +
                  P[1] * Result[1] +
                  P[2] * Result[2]);
*)

  { optimized version, just do this in one go: }
  Result.W := - (
    BoxBool[Direction.X >= 0].X * Result.X +
    BoxBool[Direction.Y >= 0].Y * Result.Y +
    BoxBool[Direction.Z >= 0].Z * Result.Z);
end;

function TBox3D.MinimumPlane(const Direction: TVector3): TVector4;
var
  BoxBool: TBox3DBool absolute Data;
  ResultDir: TVector3 absolute Result;
begin
  CheckNonEmpty;

  { first 3 plane components are just copied from Direction }
  ResultDir := Direction;

  { optimized version, just do this in one go: }
  Result.W := - (
    BoxBool[Direction.X < 0].X * Result.X +
    BoxBool[Direction.Y < 0].Y * Result.Y +
    BoxBool[Direction.Z < 0].Z * Result.Z);
end;

function TBox3D.MaximumCorner(const Direction: TVector3): TVector3;
var
  BoxBool: TBox3DBool absolute Data;
begin
  CheckNonEmpty;
  Result.X := BoxBool[Direction.X >= 0].X;
  Result.Y := BoxBool[Direction.Y >= 0].Y;
  Result.Z := BoxBool[Direction.Z >= 0].Z;
end;

function TBox3D.MinimumCorner(const Direction: TVector3): TVector3;
var
  BoxBool: TBox3DBool absolute Data;
begin
  CheckNonEmpty;
  Result.X := BoxBool[Direction.X < 0].X;
  Result.Y := BoxBool[Direction.Y < 0].Y;
  Result.Z := BoxBool[Direction.Z < 0].Z;
end;

procedure TBox3D.PointDistances(const P: TVector3;
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
      MinDistance := MinDistance + Dist0;
      MaxDistance := MaxDistance + Dist1;
    end else
    begin
      MinDistance := MinDistance + Dist1;
      MaxDistance := MaxDistance + Dist0;
    end;
  end;

  if Contains(P) then
    MinDistance := 0;

  MinDistance := Sqrt(MinDistance);
  MaxDistance := Sqrt(MaxDistance);

  { Because of floating point inaccuracy, MinDistance may be larger
    by epsilon than MaxDistance? Fix it to be sure. }
  { For now: just assert it: }
  Assert(MinDistance <= MaxDistance);
end;

procedure TBox3D.DirectionDistances(
  const Point, Dir: TVector3;
  out MinDistance, MaxDistance: Single);
var
  B: TBox3DBool absolute Data;
  XMin, YMin, ZMin: boolean;
  MinPoint, MaxPoint: TVector3;
  Coord: Integer;
begin
  CheckNonEmpty;

  XMin := Dir.X < 0;
  YMin := Dir.Y < 0;
  ZMin := Dir.Z < 0;

  MinPoint := PointOnLineClosestToPoint(Point, Dir,
    Vector3(B[XMin].X, B[YMin].Y, B[ZMin].Z));
  MaxPoint := PointOnLineClosestToPoint(Point, Dir,
    Vector3(B[not XMin].X, B[not YMin].Y, B[not ZMin].Z));

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

(* MaxDistanceAlongDirection not used, so not defined for now.
function TBox3D.MaxDistanceAlongDirection(const Point, Dir: TVector3): Single;
var
  B: TBox3DBool absolute Data;
  XMin, YMin, ZMin: boolean;
  MaxPoint: TVector3;
  Coord: Integer;
begin
  CheckNonEmpty;

  XMin := Dir.X < 0;
  YMin := Dir.Y < 0;
  ZMin := Dir.Z < 0;

  MaxPoint := PointOnLineClosestToPoint(Point, Dir,
    Vector3(B[not XMin].X, B[not YMin].Y, B[not ZMin].Z));

  Result := PointsDistance(Point, MaxPoint);

  { choose one of the 3 coordinates where Dir is largest, for best
    numerical stability. We need to compare now and see which
    distances should be negated. }
  Coord := MaxAbsVectorCoord(Dir);

  if Dir[Coord] > 0 then
  begin
    { So the distances to points that are *larger* on Coord are positive.
      Others should be negative. }
    if MaxPoint[Coord] < Point[Coord] then
      Result := -Result;
  end else
  begin
    if MaxPoint[Coord] > Point[Coord] then
      Result := -Result;
  end;
end;
*)

function TBox3D.PointDistance(const Point: TVector3): Single;
begin
  Result := Sqrt(PointDistanceSqr(Point));
end;

function TBox3D.PointDistanceSqr(const Point: TVector3): Single;
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
      Result := Result + (Sqr(Point[I] - Data[0][I])) else
    if Point[I] > Data[1][I] then
      Result := Result + (Sqr(Point[I] - Data[1][I]));
  end;
end;

(*
function TBox3D.PointMaxDistance(const Point: TVector3;
  const EmptyBoxDistance: Single): Single;
var
  B: TBox3DBool absolute Data;
begin
  if IsEmpty then
    Result := EmptyBoxDistance
  else
    Result := Sqrt(
      Sqr(Point.X - B[Point.X < (Data[0].X + Data[1].X) / 2].X) +
      Sqr(Point.Y - B[Point.Y < (Data[0].Y + Data[1].Y) / 2].Y) +
      Sqr(Point.Z - B[Point.Z < (Data[0].Z + Data[1].Z) / 2].Z)
    );
end;

function TBox3D.PointMaxDistanceSqr(const Point: TVector3;
  const EmptyBoxDistance: Single): Single;
var
  B: TBox3DBool absolute Data;
begin
  if IsEmpty then
    Result := EmptyBoxDistance
  else
    Result :=
      Sqr(Point.X - B[Point.X < (Data[0].X + Data[1].X) / 2].X) +
      Sqr(Point.Y - B[Point.Y < (Data[0].Y + Data[1].Y) / 2].Y) +
      Sqr(Point.Z - B[Point.Z < (Data[0].Z + Data[1].Z) / 2].Z)
    ;
end;
*)

function TBox3D.Equal(const Box2: TBox3D): boolean;
begin
  if IsEmpty then
    Result := Box2.IsEmpty else
    Result := (not Box2.IsEmpty) and
      TVector3.Equals(Data[0], Box2.Data[0]) and
      TVector3.Equals(Data[1], Box2.Data[1]);
end;

function TBox3D.Equal(const Box2: TBox3D; const Epsilon: Single): boolean;
begin
  if IsEmpty then
    Result := Box2.IsEmpty else
    Result := (not Box2.IsEmpty) and
      TVector3.Equals(Data[0], Box2.Data[0], Epsilon) and
      TVector3.Equals(Data[1], Box2.Data[1], Epsilon);
end;

function TBox3D.Diagonal: Single;
begin
  if IsEmpty then
    Result := 0 else
    Result := Sqrt(Sqr(Data[1].X - Data[0].X) +
                   Sqr(Data[1].Y - Data[0].Y) +
                   Sqr(Data[1].Z - Data[0].Z));
end;

function TBox3D.RectangleXY: TFloatRectangle;
begin
  if IsEmpty then
    Exit(TFloatRectangle.Empty) else
  begin
    Result.Left   := Data[0].X;
    Result.Bottom := Data[0].Y;
    Result.Width  := Data[1].X - Data[0].X;
    Result.Height := Data[1].Y - Data[0].Y;
  end;
end;

function TBox3D.RectangleXZ: TFloatRectangle;
begin
  if IsEmpty then
    Exit(TFloatRectangle.Empty) else
  begin
    Result.Left   := Data[0].X;
    Result.Bottom := Data[0].Z;
    Result.Width  := Data[1].X - Data[0].X;
    Result.Height := Data[1].Z - Data[0].Z;
  end;
end;

function TBox3D.OrthoProject(const Pos, Dir, Side, Up: TVector3): TFloatRectangle;

  function ProjectPoint(const P: TVector3): TVector2;
  var
    PDiff: TVector3;
  begin
    PDiff := P - Pos;
    Result.Data[0] := TVector3.DotProduct(PDiff, Side);
    Result.Data[1] := TVector3.DotProduct(PDiff, Up);
  end;

var
  C: TBoxCorners;
  I: Integer;
begin
  Corners(C);
  Result := FloatRectangle(ProjectPoint(C[0]), 0, 0);
  for I := 1 to 7 do
    Result := Result.Include(ProjectPoint(C[I]));
end;

class function TBox3D.CompareBackToFront2D(
  const A, B: TBox3D): Integer;
begin
  { Note that we ignore camera position/direction for 2D comparison.
    We merely look at Z coordinates.
    This way looking at 2D Spine scene from the other side is also Ok.

    For speed, we don't look at bounding box Middle, only at it's min point.
    The assumption here is that shape is 2D, so
      BoundingBox.Data[0].Z ~=
      BoundingBox.Data[1].Z ~=
      BoundingBox.Center.Z .
  }

  if (not A.IsEmpty) and
    ( B.IsEmpty or
      ( A.Data[0].Z < B.Data[0].Z )) then
    Result := -1 else
  if (not B.IsEmpty) and
    ( A.IsEmpty or
      ( B.Data[0].Z < A.Data[0].Z )) then
    Result :=  1 else
    Result :=  0;
end;

class operator TBox3D.{$ifdef FPC}+{$else}Add{$endif} (const Box1, Box2: TBox3D): TBox3D;
begin
  if Box1.IsEmpty then
    Result := Box2 else
  if Box2.IsEmpty then
    Result := Box1 else
  begin
    Result.Data[0].X := Math.Min(Box1.Data[0].X, Box2.Data[0].X);
    Result.Data[1].X := Math.Max(Box1.Data[1].X, Box2.Data[1].X);
    Result.Data[0].Y := Math.Min(Box1.Data[0].Y, Box2.Data[0].Y);
    Result.Data[1].Y := Math.Max(Box1.Data[1].Y, Box2.Data[1].Y);
    Result.Data[0].Z := Math.Min(Box1.Data[0].Z, Box2.Data[0].Z);
    Result.Data[1].Z := Math.Max(Box1.Data[1].Z, Box2.Data[1].Z);
  end;
end;

class operator TBox3D.{$ifdef FPC}+{$else}Add{$endif} (const B: TBox3D; const V: TVector3): TBox3D;
begin
  Result := B.Translate(V);
end;

class operator TBox3D.{$ifdef FPC}+{$else}Add{$endif} (const V: TVector3; const B: TBox3D): TBox3D;
begin
  Result := B.Translate(V);
end;

class function TBox3D.FromCenterSize(const ACenter, ASize: TVector3): TBox3D;
begin
  Result := Box3DAroundPoint(ACenter, ASize);
end;

procedure TBox3D.ToCenterSize(out ACenter, ASize: TVector3);
begin
  if IsEmpty then
  begin
    ACenter := TVector3.Zero;
    ASize := Vector3(-1, -1, -1);
  end else
  begin
    ACenter := Center;
    ASize := Size;
  end;
end;

{ Routines ------------------------------------------------------------------- }

function IsCenteredBox3DPlaneCollisionDouble(
  const BoxHalfSize: TVector3Double;
  const Plane: TVector4Double): boolean;

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

{ Tests (see TTestCastleBoxes.TestIsBox3DTriangleCollisionEpsilonsSingle)
  show that this calculation should really be done on at least Double precision.
  The values for these tests were taken from debugging behavior on
  castle.wrl test VRML model, so yes, these errors produce real errors
  (they make some valid triangles not appear at all in the octree, so collision detection
  and picking fail on these areas).
  Otherwise floating point errors will force you to define really large Epsilon:
  when trying to set this experimentally, I had to set Epsilon = 1e-3 (even
  Epsilon = 1e-4 was still too small epsilon !).

  With Double, I can use Epsilon below. OK, that's stil a large epsilon...
  You can test on VRML models like malfunction/trunk/vrmls/wawoz.wrl:
  castle-model-viewer (when compiled with -dDEBUG) with fail loading
  (Assert(AddedSomewhere) will trigger) with too small epsilon. }

const
  { Special equality epsilon used by IsCenteredBox3DPlaneCollision.
    For implementation reasons, it always uses Double precision,
    and it still has to use epsilon slightly larger than usual
    CastleVectors.DoubleEpsilon. }
  Epsilon: Double = 1e-5;
var
  I: Integer;
  VMin, VMax: TVector3Double;
begin
  for I := 0 to 2 do
    if Plane[I] > 0 then
    begin
      VMin.Data[I] := -BoxHalfSize[I];
      VMax.Data[I] :=  BoxHalfSize[I];
    end else
    begin
      VMin.Data[I] :=  BoxHalfSize[I];
      VMax.Data[I] := -BoxHalfSize[I];
    end;

  { If VMin is above the plane (plane equation is > 0), then VMax
    is also above, no need to test anything else. }
  if Plane.X * VMin.X +
     Plane.Y * VMin.Y +
     Plane.Z * VMin.Z +
     Plane.W > Epsilon then
    Exit(false);

  { So VMin is <= plane. So if VMax is >= 0, then there's a collision. }
  Result :=  Plane.X * VMax.X +
             Plane.Y * VMax.Y +
             Plane.Z * VMax.Z +
             Plane.W >= -Epsilon;
end;

function IsCenteredBox3DPlaneCollision(
  const BoxHalfSize: TVector3;
  const Plane: TVector4): boolean;
begin
  // redirect to Double-precision version
  Result := IsCenteredBox3DPlaneCollisionDouble(
    Vector3Double(BoxHalfSize),
    Vector4Double(Plane));
end;

function Box3D(const p0, p1: TVector3): TBox3D;
begin
  result.Data[0] := p0;
  result.Data[1] := p1;
end;

function Box3DAroundPoint(const Pt: TVector3; Size: Single): TBox3D;
begin
  if Size < 0 then
    Exit(TBox3D.Empty);

  Size := Size / 2;
  Result.Data[0].X := Pt.X - Size;
  Result.Data[0].Y := Pt.Y - Size;
  Result.Data[0].Z := Pt.Z - Size;
  Result.Data[1].X := Pt.X + Size;
  Result.Data[1].Y := Pt.Y + Size;
  Result.Data[1].Z := Pt.Z + Size;
end;

function Box3DAroundPoint(const Pt: TVector3; Size: TVector3): TBox3D;
begin
  if (Size.X < 0) or
     (Size.Y < 0) or
     (Size.Z < 0) then
    Exit(TBox3D.Empty);

  Size := Size / 2;
  Result.Data[0].X := Pt.X - Size.X;
  Result.Data[0].Y := Pt.Y - Size.Y;
  Result.Data[0].Z := Pt.Z - Size.Z;
  Result.Data[1].X := Pt.X + Size.X;
  Result.Data[1].Y := Pt.Y + Size.Y;
  Result.Data[1].Z := Pt.Z + Size.Z;
end;

function CalculateBoundingBox(
  GetVertex: TGetVertexFromIndexFunc;
  VertsCount: integer): TBox3D;
var
  I: Integer;
  V: TVector3;
begin
  if VertsCount = 0 then
    Result := TBox3D.Empty else
  begin
    Result.Data[0] := GetVertex(0);
    Result.Data[1] := Result.Data[0];
    for I := 1 to VertsCount - 1 do
    begin
      V := GetVertex(I);
      MinVar(Result.Data[0].X, V.X);
      MinVar(Result.Data[0].Y, V.Y);
      MinVar(Result.Data[0].Z, V.Z);

      MaxVar(Result.Data[1].X, V.X);
      MaxVar(Result.Data[1].Y, V.Y);
      MaxVar(Result.Data[1].Z, V.Z);
    end;
  end;
end;

type
  { Internal helper for CalculateBoundingBox }
  TBBox_Calculator = class
    Verts: PVector3;
    VertsStride: Cardinal; { tutaj VertsStride juz nie moze byc = 0 }
    PMatrix: PMatrix4;
    function GetVertexNotTransform(index: integer): TVector3;
    function GetVertexTransform(index: integer): TVector3;
  end;

  function TBBox_Calculator.GetVertexNotTransform(index: integer): TVector3;
  begin
   result := PVector3(PointerAdd(Verts, VertsStride*Cardinal(index)))^;
  end;

  function TBBox_Calculator.GetVertexTransform(index: integer): TVector3;
  begin
   result := PMatrix^.MultPoint(PVector3(PointerAdd(Verts, VertsStride*Cardinal(index)))^);
  end;

function CalculateBoundingBox(
  Verts: PVector3; VertsCount: Cardinal; VertsStride: Cardinal): TBox3D;
var
  Calculator: TBBox_Calculator;
begin
  if VertsStride = 0 then VertsStride := SizeOf(TVector3);
  Calculator := TBBox_Calculator.Create;
  try
    Calculator.VertsStride := VertsStride;
    Calculator.Verts := Verts;
    result := CalculateBoundingBox(
      {$ifdef FPC} @ {$endif} Calculator.GetVertexNotTransform, VertsCount);
  finally Calculator.Free end;
end;

function CalculateBoundingBox(
  Verts: PVector3; VertsCount: Cardinal; VertsStride: Cardinal;
  const Transform: TMatrix4): TBox3D;
var
  Calculator: TBBox_Calculator;
begin
  if VertsStride = 0 then VertsStride := SizeOf(TVector3);
  Calculator := TBBox_Calculator.Create;
  try
    Calculator.VertsStride := VertsStride;
    Calculator.Verts := Verts;
    Calculator.PMatrix := @Transform;
    result := CalculateBoundingBox(
      {$ifdef FPC} @ {$endif} Calculator.GetVertexTransform, VertsCount);
  finally Calculator.Free end;
end;

function CalculateBoundingBox(Verts: TVector3List): TBox3D;
begin
  Result := CalculateBoundingBox(PVector3(Verts.L), Verts.Count, 0);
end;

function CalculateBoundingBox(Verts: TVector3List;
  const Transform: TMatrix4): TBox3D;
begin
  Result := CalculateBoundingBox(PVector3(Verts.L), Verts.Count, 0,
    Transform);
end;

function CalculateBoundingBoxFromIndices(
  const GetVertIndex: TGetIndexFromIndexNumFunc;
  const VertsIndicesCount: integer;
  const GetVertex: TGetVertexFromIndexFunc): TBox3D;
var
  { pozycja pierwszego nieujemnego indexu.
    Zwracamy TBox3D.Empty wtw. gdy firstIndex nie istnieje }
  FirstIndexNum: integer;

  IndexNum, Index: integer;
  ThisVertex: TVector3;
begin
  {seek for firstIndex}
  firstIndexNum := 0;
  while (firstIndexNum < VertsIndicesCount) and (GetVertIndex(firstIndexNum) < 0) do
    Inc(firstIndexNum);

  if firstIndexNum = VertsIndicesCount then {firstIndex not found ?}
  begin
    result := TBox3D.Empty;
    exit;
  end;

  { Note that I do only one pass, getting all vertexes.

    This is important, because GetVertex may be quite expensive
    operation (in case of e.g. TVertTransform_Calculator.GetTransformed,
    this is Matrix.MultPoint for every vertex). At the beginning
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
      if ThisVertex.X < Result.Data[0].X then Result.Data[0].X := ThisVertex.X;
      if ThisVertex.Y < Result.Data[0].Y then Result.Data[0].Y := ThisVertex.Y;
      if ThisVertex.Z < Result.Data[0].Z then Result.Data[0].Z := ThisVertex.Z;
      if ThisVertex.X > Result.Data[1].X then Result.Data[1].X := ThisVertex.X;
      if ThisVertex.Y > Result.Data[1].Y then Result.Data[1].Y := ThisVertex.Y;
      if ThisVertex.Z > Result.Data[1].Z then Result.Data[1].Z := ThisVertex.Z;
    end;
  end;
end;

type
  TVertTransform_Calculator = class
    PTransform: PMatrix4;
    GetNotTransformed: TGetVertexFromIndexFunc;
    function GetTransformed(index: integer): TVector3;
  end;
  function TVertTransform_Calculator.GetTransformed(index: integer): TVector3;
  begin
    result := PTransform^.MultPoint(GetNotTransformed(index));
  end;

function CalculateBoundingBoxFromIndices(
  const GetVertIndex: TGetIndexFromIndexNumFunc;
  const VertsIndicesCount: integer;
  const GetVertex: TGetVertexFromIndexFunc;
  const Transform: TMatrix4): TBox3D;
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
      {$ifdef FPC} @ {$endif} Calculator.GetTransformed);
  finally Calculator.Free end;
end;

function TriangleBoundingBox(const T: TTriangle3): TBox3D;
begin
  MinMax(T.Data[0].X, T.Data[1].X, T.Data[2].X, Result.Data[0].X, Result.Data[1].X);
  MinMax(T.Data[0].Y, T.Data[1].Y, T.Data[2].Y, Result.Data[0].Y, Result.Data[1].Y);
  MinMax(T.Data[0].Z, T.Data[1].Z, T.Data[2].Z, Result.Data[0].Z, Result.Data[1].Z);
end;

function BoundingBox3DFromSphere(const Center: TVector3;
  const Radius: Single): TBox3D;
begin
  Result.Data[0] := Center;
  Result.Data[0].X := Result.Data[0].X - Radius;
  Result.Data[0].Y := Result.Data[0].Y - Radius;
  Result.Data[0].Z := Result.Data[0].Z - Radius;

  Result.Data[1] := Center;
  Result.Data[1].X := Result.Data[1].X + Radius;
  Result.Data[1].Y := Result.Data[1].Y + Radius;
  Result.Data[1].Z := Result.Data[1].Z + Radius;
end;

end.
