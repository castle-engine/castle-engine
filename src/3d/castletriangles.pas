{
  Copyright 2003-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Triangles. In 2D, 3D and 4D space. }
unit CastleTriangles;

{$I castleconf.inc}
{$I octreeconf.inc}

interface

uses Generics.Collections,
  CastleUtils, CastleVectors;

type
  { Triangle in 2D space. }
  TTriangle2 = record
    Data: packed array [0..2] of TVector2;

    { Multiline triangle description. }
    function ToString: string;
  end;

  PTriangle2 = ^TTriangle2;

  { Triangle in 3D space.

    Triangle's three points must not be collinear,
    i.e. routines in this unit generally don't accept "degenerated" triangles
    that are not really triangles. So 3D triangle must unambiguously
    define some plane in the 3D space. The only function in this unit
    that is able to handle "degenerated" triangles is @link(TTriangle3.IsValid),
    which is exactly used to check whether the triangle is degenerated. }
  TTriangle3 = record
  public
    type
      TIndex = 0..2;
  strict private
    function GetItems(const Index: TIndex): TVector3;
    procedure SetItems(const Index: TIndex; const Value: TVector3);
  public
    Data: packed array [TIndex] of TVector3;

    { Access the points of the triangle.
      This is the same as accessing @link(Data) field,
      it is also the default record property so you can just write @code(MyTriangle[0])
      instead of @code(MyTriangle.Items[0]) or @code(MyTriangle.Data[0]). }
    property Items [const Index: TIndex]: TVector3 read GetItems write SetItems; default;

    { Multiline triangle description. }
    function ToString: string;

    { Check does the triangle define a correct plane in 3D space.
      That is, check does the triangle not degenerate to a point or line segment
      (which can happen when some points are at the same position, or are colinear). }
    function IsValid: boolean;

    { Like a normal vector of a triangle (see @link(Normal)), but not necessarily normalized. }
    function Direction: TVector3;

    { Normal vector of a triangle. Returns vector pointing our from CCW triangle
      side (for right-handed coordinate system), and orthogonal to triangle plane.
      For degenerated triangles (when @link(IsValid) would return @false),
      we return zero vector. }
    function Normal: TVector3;

    { Plane of the triangle. Note that this has many possible solutions
      (plane representation as equation @code(Ax + By + Cz + D = 0)
      is not unambiguous), this just returns some solution deterministically.

      It's guaranteed that the direction of this plane (i.e. first 3 items
      of returned vector) will be in the same direction as calcualted by
      @link(Direction), which means that it points outward from CCW side of
      the triangle (assuming right-handed coord system).

      For NormalizedPlane, this direction is also normalized
      (makes a vector with length 1). This way NormalizedPlane calculates
      also @link(Normal).

      For three points that do not define a plane, a plane with first three
      components = 0 is returned. In fact, the 4th component will be zero too
      in this case (for now), but don't depend on it.
      @groupBegin }
    function Plane: TVector4;
    function NormalizedPlane: TVector4;

    { Transform triangle by 4x4 matrix. This simply transforms each triangle point.

      @raises(ETransformedResultInvalid Raised when matrix
      will transform some point to a direction (vector with 4th component
      equal zero). In this case we just cannot interpret the result as a 3D point.) }
    function Transform(const M: TMatrix4): TTriangle3;

    { Surface area of 3D triangle.
      This works for degenerated (equal to line segment or even single point)
      triangles too: returns 0 for them.

      @groupBegin }
    function Area: Single;
    function AreaSqr: Single;
    { @groupEnd }

    { Random triangle point, chosen with a constant density for triangle area. }
    function RandomPoint: TVector3;

    { For a given Point lying on a given Triangle, calculate it's barycentric
      coordinates.

      The resulting Barycentric coordinates can be used for linearly
      interpolating values along the triangle, as they satisfy the equation:

      @preformatted(
        Result.Data[0] * Triangle.Data[0] +
        Result.Data[1] * Triangle.Data[1] +
        Result.Data[2] * Triangle.Data[2] = Point
      )

      See also [http://en.wikipedia.org/wiki/Barycentric_coordinate_system_%28mathematics%29] }
    function Barycentric(const Point: TVector3): TVector3;
  end;

  TTriangle3List = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TTriangle3>;

  PTriangle3 = ^TTriangle3;

  { Triangle in 4D (or 3D homogeneous) space. }
  TTriangle4 = record
    Data: packed array [0..2] of TVector4;

    { Multiline triangle description. }
    function ToString: string;
  end;

  PTriangle4 = ^TTriangle4;

  TTriangle2Single = TTriangle2 deprecated 'use TTriangle2';
  TTriangle3Single = TTriangle3 deprecated 'use TTriangle3';
  TTriangle4Single = TTriangle4 deprecated 'use TTriangle4';

  PTriangle2Single = PTriangle2 deprecated 'use PTriangle2';
  PTriangle3Single = PTriangle3 deprecated 'use PTriangle3';
  PTriangle4Single = PTriangle4 deprecated 'use PTriangle4';

function Triangle3(const p0, p1, p2: TVector3): TTriangle3;

{ Normal vector of a triangle defined as three indexes intro vertex array.
  VerticesStride is the shift between vertex values in the array,
  VerticesStride = 0 behaves like VerticesStride = SizeOf(TVector3). }
function IndexedTriangleNormal(const Indexes: TVector3Cardinal;
  VerticesArray: PVector3; VerticesStride: integer): TVector3;

type
  { Triangle expressed in particular coordinate system, for TTriangle. }
  TTriangleGeometry = record
    Triangle: TTriangle3;

    { Area of the triangle. }
    {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
    function Area: Single;
    {$else}
    Area: Single;
    {$endif}

    {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
    function Plane: TVector4;
    function Normal: TVector3;
    {$else}
    case Integer of
      0: ({ Triangle normal, a 3D plane containing our Triangle, with normalized
            direction vector. }
          Plane: TVector4;);
      1: (Normal: TVector3;);
    {$endif}
  end;

{ TFaceIndex ----------------------------------------------------------------- }

type
  { Describe a range of indexes where the face (polygon and such) is located.

    When a triangle is part of a face defined by the coordIndex field
    (like in IndexedFaceSet) then this describes where
    in this coordIndex this face is located. This is useful for
    editing / removing a face corresponding to a given triangle.

    Otherwise, both IndexBegin and IndexEnd are -1. }
  TFaceIndex = object
    IndexBegin, IndexEnd: Integer;
  end;

  TFaceIndexesList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TFaceIndex>;

const
  UnknownFaceIndex: TFaceIndex = (IndexBegin: -1; IndexEnd: -1);

{ TTriangle ------------------------------------------------------------------ }

type
  TMailboxTag = Int64;

  { Triangle in 3D.
    This object should always be initialized by @link(Init),
    and updated only by it's methods (never modify fields of
    this object directly). }
  TTriangle = record
  public
    { Geometry of this item.
      We need two geometry descriptions:

      @unorderedList(

        @item(Local is based on initial Triangle, given when constructing
          this TTriangle. It's constant for this TTriangle. It's used
          by octree collision routines, that is things like
          TBaseTrianglesOctree.SphereCollision, TBaseTrianglesOctree.RayCollision
          and such expect parameters in the same coord space.

          This may be local coord space of this shape (this is used
          by TShape.OctreeTriangles) or world coord space
          (this is used by TCastleSceneCore.OctreeTriangles).)

        @item(World is the geometry of Local transformed to be in world
          coordinates. Initially, World is just a copy of Local.

          If Local already contains world-space geometry, then World
          can just remain constant, and so is always Local copy.

          If Local contains local shape-space geometry, then World
          will have to be updated by TTriangle.UpdateWorld whenever some octree item's
          geometry will be needed in world coords. This will have to be
          done e.g. by TBaseTrianglesOctree.XxxCollision for each returned item.)
      ) }
    Local, World: TTriangleGeometry;

    { Shape containing this triangle.
      This is always an instance of TShape class, but due
      to unit dependencies it cannot be declared as such here.
      Use X3DTriangles unit to have a "record helper" method that returns
      a Shape as TShape instance. }
    InternalShape: TObject;

    {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
    { Tag of an object (like a ray or a line segment)
      for which we have saved an
      intersection result. Intersection result is in
      MailboxIsIntersection, MailboxIntersection, MailboxIntersectionDistance.

      To make things correct, we obviously assume that every segment
      and ray have different tags. Also, tag -1 is reserved.
      In practice, we simply initialize MailboxSavedTag to -1,
      and each new segment/ray get consecutive tags starting from 0.

      @italic(History): a naive implementation at the beginning
      was not using tags, instead I had MailboxState (empty, ray or segment)
      and I was storing ray/line vectors (2 TVector3 values).
      This had much larger size (6 * SizeOf(Single) + SizeOf(enum) = 28 bytes)
      than tag, which is important (3D models have easily thousands of
      TTriangle). And it took longer to compare and assign,
      so it was working much slower.

      @groupBegin }
    MailboxSavedTag: TMailboxTag;
    MailboxIsIntersection: boolean;
    MailboxIntersection: TVector3;
    MailboxIntersectionDistance: Single;
    { @groupEnd }
    {$endif}

    {$ifndef CONSERVE_TRIANGLE_MEMORY}
    { Normal vectors, at each triangle vertex. }
    Normal: TTriangle3;

    { Texture coordinates, for each triangle point.

      Each texture coordinate is a 4D vector, since we may have 3D textures
      referenced by 4D (homogeneous) coordinates. For normal 2D textures,
      you can simply take the first 2 components of the vector,
      and ignore the remaining 2 components. The 3th component is always
      0 if was not specified (if model had only 2D texture coords).
      The 4th component is always 1 if was not specified
      (if model had only 2D or 3D texture coords).

      In case of multi-texturing, this describes coordinates
      of the first texture unit.
      In case no texture is defined, this is undefined. }
    TexCoord: TTriangle4;

    { The indexes of this face, for editing / removing it.
      See TFaceIndex. }
    Face: TFaceIndex;
    {$else}

    { The indexes of this face, for editing / removing it.
      See TFaceIndex. }
    function Face: TFaceIndex;
    {$endif not CONSERVE_TRIANGLE_MEMORY}

    { Initialize new triangle.
      Given ATriangle must satisfy ATriangle.IsValid. }
    procedure Init(AShape: TObject;
      const ATriangle: TTriangle3;
      const ANormal: TTriangle3; const ATexCoord: TTriangle4;
      const AFace: TFaceIndex);

    { Check collisions between TTriangle and ray/segment.

      Always use these routines to check for collisions,
      to use mailboxes if possible. Mailboxes are used only if this was
      compiled with TRIANGLE_OCTREE_USE_MAILBOX defined.

      Increments TriangleCollisionTestsCounter if actual test was done
      (that is, if we couldn't use mailbox to get the result quickier).

      @groupBegin }
    function SegmentDirCollision(
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const Segment0, SegmentVector: TVector3;
      const SegmentTag: TMailboxTag): boolean;

    function RayCollision(
      out Intersection: TVector3;
      out IntersectionDistance: Single;
      const RayOrigin, RayDirection: TVector3;
      const RayTag: TMailboxTag): boolean;
    { @groupEnd }

    {$ifndef CONSERVE_TRIANGLE_MEMORY}

    { For a given position (in world coordinates), return the texture
      coordinate at this point. It is an interpolated texture coordinate
      from our per-vertex texture coordinates in @link(TexCoord) field.

      This assumes that Position actually lies within the triangle.

      The ITexCoord2D returns the same, but cut to the first 2 texture
      coordinate components. Usable for normal 2D textures.
      @groupBegin }
    function ITexCoord(const Point: TVector3): TVector4;
    function ITexCoord2D(const Point: TVector3): TVector2;
    { @groupEnd }

    { For a given position (in world coordinates), return the smooth
      normal vector at this point. It is an interpolated normal
      from our per-vertex normals in the @link(Normal) field,
      thus is supports also the case when you have smooth shading
      (normals change throughout the triangle).

      Like the @link(Normal) field, the returned vector is
      a normal vector in the local coordinates.
      Use @link(TTriangleHelper.INormalWorldSpace) to get a normal vector in scene
      coordinates.

      This assumes that Position actally lies within the triangle. }
    function INormal(const Point: TVector3): TVector3;

    { Like INormal, but not necessarily normalized. }
    function INormalCore(const Point: TVector3): TVector3;
    {$endif}
  end;
  PTriangle = ^TTriangle;

  TTriangleList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TTriangle>;

  { Return for given Triangle do we want to ignore collisions with it.
    For now, Sender is always TTriangleOctree. }
  TTriangleIgnoreFunc = function (const Sender: TObject;
    const Triangle: PTriangle): boolean of object;

  T3DTriangleGeometry = TTriangleGeometry deprecated 'use TTriangleGeometry';
  T3DTriangle = TTriangle deprecated 'use TTriangle';
  P3DTriangle = PTriangle deprecated 'use PTriangle';
  T3DTriangleIgnoreFunc = TTriangleIgnoreFunc deprecated 'use TTriangleIgnoreFunc';

var
  { Counter of collision tests done by TTriangle when the actual collision
    calculation had to be done.
    This counts all calls to TTriangle.SegmentDirCollision and
    TTriangle.RayCollision when the result had to be actually geometrically
    calculated (result was not in the cache aka "mailbox").

    It is especially useful to look at this after using some spatial
    data structure, like an octree. The goal of tree structures is to
    minimize this number.

    It is a global variable, because that's the most comfortable way to use
    it. Triangles are usually wrapped in an octree (like TTriangleOctree),
    or even in an octree of octrees (like TShapeOctree).
    Tracking collisions using the global variable is most comfortable,
    instead of spending time on propagating this (purely debugging) information
    through the octree structures. }
  TriangleCollisionTestsCounter: Cardinal;

{ polygons ------------------------------------------------------------------- }

{ Calculates normalized normal vector for polygon composed from
  indexed vertices. Polygon is defines as vertices
  Verts^[Indices[0]], Verts^[Indices[1]] ... Verts^[Indices[IndicesCount-1]].
  Returns normal pointing from CCW.

  It's secured against invalid indexes on Indices list (that's the only
  reason why it takes VertsCount parameter, after all): they are ignored.

  If the polygon is degenerated, that is it doesn't determine a plane in
  3D space (this includes, but is not limited, to cases when there are
  less than 3 valid points, like when IndicesCount < 3)
  then it returns ResultForIncorrectPoly.

  @groupBegin }
function IndexedConvexPolygonNormal(
  Indices: PLongintArray; IndicesCount: integer;
  Verts: PVector3Array; const VertsCount: Integer;
  const ResultForIncorrectPoly: TVector3): TVector3; overload;
function IndexedConvexPolygonNormal(
  const Indices: PLongintArray; const IndicesCount: integer;
  const Verts: PVector3Array; const VertsCount: Integer;
  const VertsStride: PtrUInt;
  const ResultForIncorrectPoly: TVector3): TVector3; overload;
{ @groupEnd }

{ Surface area of indexed convex polygon.
  Polygon is defines as vertices
  Verts^[Indices[0]], Verts^[Indices[1]] ... Verts^[Indices[IndicesCount-1]].

  It's secured against invalid indexes on Indices list (that's the only
  reason why it takes VertsCount parameter, after all): they are ignored.

  @groupBegin }
function IndexedConvexPolygonArea(
  const Indices: PLongintArray; const IndicesCount: integer;
  const Verts: PVector3Array; const VertsCount: Integer): Single; overload;
function IndexedConvexPolygonArea(
  const Indices: PLongintArray; const IndicesCount: integer;
  const Verts: PVector3Array; const VertsCount: Integer;
  const VertsStride: PtrUInt): Single; overload;
{ @groupEnd }

{ Are the polygon points ordered CCW (counter-clockwise). When viewed
  with typical camera settings, that is +Y goes up and +X goes right.

  Polygon doesn't have to be convex. Polygon doesn't have to have all triangles
  valid, that is it's OK if some polygon triangles degenerate into points
  or line segments.

  Returns something > 0 if polygon is CCW, or < 0 when it's not.
  Returns zero when polygon has area 0.
  @groupBegin }
function IsPolygon2dCCW(Verts: PVector2Array; const VertsCount: Integer): Single; overload;
function IsPolygon2dCCW(const Verts: array of TVector2): Single; overload;
{ @groupEnd }

{ Calculate polygon area.

  Polygon doesn't have to be convex. Polygon doesn't have to have all triangles
  valid, that is it's OK if some polygon triangles degenerate into points
  or line segments.

  @groupBegin }
function Polygon2dArea(Verts: PVector2Array; const VertsCount: Integer): Single; overload;
function Polygon2dArea(const Verts: array of TVector2): Single; overload;
{ @groupEnd }

{ Assuming a point lies on a triangle plane,
  check does it lie inside a triangle.
  Give first 3 components of triangle plane as TriDir.
  @groupBegin }
function IsPointOnTrianglePlaneWithinTriangle(const P: TVector3;
  const Tri: TTriangle3; const TriDir: TVector3): boolean; overload;
{ @groupEnd }

{ Check does point lie inside a triangle, in 2D.
  @groupBegin }
function IsPointWithinTriangle2D(const P: TVector2;
  const Tri: TTriangle2): boolean; overload;
function IsPointWithinTriangle2D(const P: TVector2;
  const Tri: TTriangle3): boolean; overload;
{ @groupEnd }

{ Check triangle with line segment collision.
  You can pass the triangle plane along with a triangle,
  this will speed calculation.
  @groupBegin }
function IsTriangleSegmentCollision(const Tri: TTriangle3;
  const TriPlane: TVector4;
  const Pos1, Pos2: TVector3): boolean; overload;
function IsTriangleSegmentCollision(const Tri: TTriangle3;
  const Pos1, Pos2: TVector3): boolean; overload;
{ @groupEnd }

function IsTriangleSphereCollision(const Tri: TTriangle3;
  const TriPlane: TVector4;
  const SphereCenter: TVector3; SphereRadius: Single): boolean; overload;
function IsTriangleSphereCollision(const Tri: TTriangle3;
  const SphereCenter: TVector3; SphereRadius: Single): boolean; overload;

{ Test collision between triangle and sphere in 2D.
  If you use overloaded version with TTriangle3, the Z coordinate
  of the triangle corners is simply ignored, so everything is projected
  on the Z=0 plane.
  @groupBegin }
function IsTriangleSphereCollision2D(const Tri: TTriangle2;
  const SphereCenter: TVector2; SphereRadius: Single): boolean; overload;
function IsTriangleSphereCollision2D(const Tri: TTriangle3;
  const SphereCenter: TVector2; SphereRadius: Single): boolean; overload;
{ @groupEnd }

{ Calculate triangle with line segment collision.
  You can pass the triangle plane along with a triangle,
  this will speed calculation.

  When there's no intersection, returns @false and doesn't modify Intersection
  or T.
  @groupBegin }
function TryTriangleSegmentCollision(var Intersection: TVector3;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const Pos1, Pos2: TVector3): boolean; overload;

function TryTriangleSegmentDirCollision(var Intersection: TVector3;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const Segment0, SegmentVector: TVector3): boolean; overload;
function TryTriangleSegmentDirCollision(var Intersection: TVector3; var T: Single;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const Segment0, SegmentVector: TVector3): boolean; overload;
{ @groupEnd }

{ Calculate triangle with ray collision.
  You can pass the triangle plane along with a triangle,
  this will speed calculation.

  When there's no intersection, returns @false and doesn't modify Intersection
  or T.
  @groupBegin }
function TryTriangleRayCollision(var Intersection: TVector3;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
function TryTriangleRayCollision(var Intersection: TVector3; var T: Single;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
{ @groupEnd }

function TriangleDirection(const p0, p1, p2: TVector3): TVector3; overload;
function TriangleDir(const p0, p1, p2: TVector3): TVector3; overload; deprecated 'use TriangleDirection';
function TriangleNormal(const p0, p1, p2: TVector3): TVector3; overload;
function TrianglePlane(const p0, p1, p2: TVector3): TVector4; overload;

function TriangleDir(const T: TTriangle3): TVector3; overload; deprecated 'use Triangle.Direction';
function TriangleNormal(const T: TTriangle3): TVector3; overload; deprecated 'use Triangle.Normal';
function TrianglePlane(const T: TTriangle3): TVector4; overload; deprecated 'use Triangle.Plane';
function TriangleTransform(const T: TTriangle3; const M: TMatrix4): TTriangle3; deprecated 'use Triangle.Transform';
function TriangleNormPlane(const T: TTriangle3): TVector4; deprecated 'use Triangle.NormalizedPlane';
function TriangleArea(const T: TTriangle3): Single; deprecated 'use Triangle.Area';
function Barycentric(const T: TTriangle3; const Point: TVector3): TVector3; deprecated 'use Triangle.Barycentric';
function TriangleToNiceStr(const T: TTriangle3): string; deprecated 'use T.ToString';

implementation

uses Math;

function Triangle3(const p0, p1, p2: TVector3): TTriangle3;
begin
  Result.Data[0] := p0;
  Result.Data[1] := p1;
  Result.Data[2] := p2;
end;

function IndexedTriangleNormal(const Indexes: TVector3Cardinal;
  VerticesArray: PVector3; VerticesStride: integer): TVector3;
var
  Tri: TTriangle3;
  i: integer;
begin
  if VerticesStride = 0 then VerticesStride := SizeOf(TVector3);
  for i := 0 to 2 do
    Tri.Data[i] := PVector3(PointerAdd(VerticesArray, VerticesStride*Integer(Indexes.Data[i])))^;
  Result := Tri.Normal;
end;

function IndexedConvexPolygonNormal(
  Indices: PLongintArray; IndicesCount: integer;
  Verts: PVector3Array; const VertsCount: Integer;
  const ResultForIncorrectPoly: TVector3): TVector3;
begin
  Result := IndexedConvexPolygonNormal(
    Indices, IndicesCount,
    Verts, VertsCount, SizeOf(TVector3),
    ResultForIncorrectPoly);
end;

function IndexedConvexPolygonNormal(
  const Indices: PLongintArray; const IndicesCount: integer;
  const Verts: PVector3Array; const VertsCount: Integer;
  const VertsStride: PtrUInt;
  const ResultForIncorrectPoly: TVector3): TVector3;

  { Like Verts^[Indices[I]] but takes into account VertsStride. }
  function VertsIndices(const I: Integer): PVector3;
  begin
    Result := PVector3(PtrUInt(Verts) + PtrUInt(Indices^[I]) * VertsStride);
  end;

var
  Tri: TTriangle3;
  I: Integer;
begin
  { We calculate normal vector as an average of normal vectors of
    polygon's triangles. Not taking into account invalid Indices
    (pointing beyond the VertsCount range) and degenerated triangles.

    This isn't the fastest method possible, but it's safest.
    It works Ok even if the polygon isn't precisely planar, or has
    some degenerate triangles. }

  Result := TVector3.Zero;

  I := 0;

  while (I < IndicesCount) and (Indices^[I] >= VertsCount) do Inc(I);
  { This secures us against polygons with no valid Indices[].
    (including case when IndicesCount = 0). }
  if I >= IndicesCount then
    Exit(ResultForIncorrectPoly);
  Tri.Data[0] := VertsIndices(I)^;

  repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
  if I >= IndicesCount then
    Exit(ResultForIncorrectPoly);
  Tri.Data[1] := VertsIndices(I)^;

  repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
  if I >= IndicesCount then
    Exit(ResultForIncorrectPoly);
  Tri.Data[2] := VertsIndices(I)^;

  if Tri.IsValid then
    Result := Result + Tri.Normal;

  repeat
    { find next valid point, which makes another triangle of polygon }

    repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
    if I >= IndicesCount then
      Break;
    Tri.Data[1] := Tri.Data[2];
    Tri.Data[2] := VertsIndices(I)^;

    if Tri.IsValid then
      Result := Result + Tri.Normal;
  until false;

  { All triangle normals are summed up now. (Each triangle normal was also
    normalized, to have equal contribution to the Result.)
    Normalize Result now, if we had any valid triangle. }
  if Result.IsZero then
    Result := ResultForIncorrectPoly
  else
    Result.NormalizeMe;
end;

function IndexedConvexPolygonArea(
  const Indices: PLongintArray; const IndicesCount: integer;
  const Verts: PVector3Array; const VertsCount: Integer): Single;
begin
  Result := IndexedConvexPolygonArea(
    Indices, IndicesCount,
    Verts, VertsCount, SizeOf(TVector3));
end;

function IndexedConvexPolygonArea(
  const Indices: PLongintArray; const IndicesCount: integer;
  const Verts: PVector3Array; const VertsCount: Integer;
  const VertsStride: PtrUInt): Single;

  { Like Verts^[Indices[I]] but takes into account VertsStride. }
  function VertsIndices(const I: Integer): PVector3;
  begin
    Result := PVector3(PtrUInt(Verts) + PtrUInt(Indices^[I]) * VertsStride);
  end;

var
  Tri: TTriangle3;
  i: integer;
begin
  { We calculate area as a sum of areas of
    polygon's triangles. Not taking into account invalid Indices
    (pointing beyond the VertsCount range). }

  Result := 0;

  I := 0;

  while (I < IndicesCount) and (Indices^[I] >= VertsCount) do Inc(I);
  { This secures us against polygons with no valid Indices[].
    (including case when IndicesCount = 0). }
  if I >= IndicesCount then
    Exit;
  Tri.Data[0] := VertsIndices(I)^;

  repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
  if I >= IndicesCount then
    Exit;
  Tri.Data[1] := VertsIndices(I)^;

  repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
  if I >= IndicesCount then
    Exit;
  Tri.Data[2] := VertsIndices(I)^;

  Result := Result + Tri.Area;

  repeat
    { find next valid point, which makes another triangle of polygon }

    repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
    if I >= IndicesCount then
      Break;
    Tri.Data[1] := Tri.Data[2];
    Tri.Data[2] := VertsIndices(I)^;

    Result := Result + Tri.Area;
  until false;
end;

function IsPolygon2dCCW(Verts: PVector2Array; const VertsCount: Integer): Single;
{ licz pole polygonu CCW.

  Implementacja na podstawie "Graphic Gems II", gem I.1
  W Graphic Gems pisza ze to jest formula na polygon CCW (na plaszczyznie
  kartezjanskiej, z +X w prawo i +Y w gore) i nie podaja tego Abs() na koncu.
  Widac jednak ze jesli podamy zamiast wielokata CCW ten sam wielokat ale
  z wierzcholkami w odwrotnej kolejnosci to procedura policzy dokladnie to samo
  ale skosy dodatnie zostana teraz policzone jako ujemne a ujemne jako dodatnie.
  Czyli dostaniemy ujemne pole.

  Mozna wiec wykorzystac powyzszy fakt aby testowac czy polygon jest CCW :
  brac liczona tu wartosc i jesli >0 to CCW, <0 to CW
  (jesli =0 to nie wiadomo no i polygony o polu = 0 rzeczywiscie nie maja
  jednoznacznej orientacji). Moznaby pomyslec ze mozna znalezc prostsze
  testy na to czy polygon jest CCW - mozna przeciez testowac tylko wyciety
  z polygonu trojkat. Ale uwaga - wtedy trzebaby uwazac i koniecznie
  wybrac z polygonu niezdegenerowany trojkat (o niezerowym polu),
  no chyba ze caly polygon mialby zerowe pole. Tak jak jest nie trzeba
  sie tym przejmowac i jest prosto.

  W ten sposob ponizsza procedura jednoczesnie liczy pole polygonu
  (Polygon2dArea jest zaimplementowane jako proste Abs() z wyniku tej
  funkcji. }
var
  i: Integer;
begin
  Result := 0.0;
  if VertsCount = 0 then Exit;

  { licze i = 0..VertsCount-2, potem osobno przypadek gdy i = VertsCount-1.
    Moglbym ujac je razem, dajac zamiast "Verts^[i+1, 1]"
    "Verts^[(i+1)mod VertsCount, 1]" ale szkoda byloby dawac tu "mod" na potrzebe
    tylko jednego przypadku. Tak jest optymalniej czasowo. }
  for i := 0 to VertsCount-2 do
    Result := Result +
              Verts^[i].Data[0] * Verts^[i+1].Data[1] -
              Verts^[i].Data[1] * Verts^[i+1].Data[0];
  Result := Result +
            Verts^[VertsCount-1].Data[0] * Verts^[0].Data[1] -
            Verts^[VertsCount-1].Data[1] * Verts^[0].Data[0];
  Result := Result / 2;
end;

function IsPolygon2dCCW(const Verts: array of TVector2): Single;
begin
  Result := IsPolygon2dCCW(@Verts, High(Verts)+1);
end;

function Polygon2dArea(Verts: PVector2Array; const VertsCount: Integer): Single;
begin
  Result := Abs(IsPolygon2dCCW(Verts, VertsCount));
end;

function Polygon2dArea(const Verts: array of TVector2): Single;
{ We depend on the (internal) fact that IsPolygonCCW
  returns an area in case of CCW, or -area in case of CW polygon. }
begin
  Result := Polygon2dArea(@Verts, High(Verts) + 1);
end;

{ TTriangleGeometry -------------------------------------------------------- }

{$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
function TTriangleGeometry.Area: Single;
begin
  Result := Triangle.Area;
end;

function TTriangleGeometry.Plane: TVector4;
begin
  Result := Triangle.NormalizedPlane;
end;

function TTriangleGeometry.Normal: TVector3;
begin
  Result := Triangle.Normal;
end;
{$endif}

{ TTriangle  --------------------------------------------------------------- }

procedure TTriangle.Init(AShape: TObject;
  const ATriangle: TTriangle3;
  const ANormal: TTriangle3; const ATexCoord: TTriangle4;
  const AFace: TFaceIndex);
begin
  Local.Triangle := ATriangle;
  {$ifndef CONSERVE_TRIANGLE_MEMORY_MORE}
  Local.Plane := ATriangle.NormalizedPlane;
  Local.Area := ATriangle.Area;
  {$endif}

  World := Local;

  InternalShape := AShape;

  {$ifndef CONSERVE_TRIANGLE_MEMORY}
  Normal := ANormal;
  TexCoord := ATexCoord;
  Face := AFace;
  {$endif not CONSERVE_TRIANGLE_MEMORY}

  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
  MailboxSavedTag := -1;
  {$endif}
end;

function TTriangle.SegmentDirCollision(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const Segment0, SegmentVector: TVector3;
  const SegmentTag: TMailboxTag): boolean;
begin
  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
  if MailboxSavedTag = SegmentTag then
  begin
    result := MailboxIsIntersection;
    if result then
    begin
      Intersection         := MailboxIntersection;
      IntersectionDistance := MailboxIntersectionDistance;
    end;
  end else
  begin
  {$endif}

    Result := TryTriangleSegmentDirCollision(
      Intersection, IntersectionDistance,
      Local.Triangle, Local.Plane,
      Segment0, SegmentVector);
    Inc(TriangleCollisionTestsCounter);

  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
    { save result to mailbox }
    MailboxSavedTag := SegmentTag;
    MailboxIsIntersection := result;
    if result then
    begin
      MailboxIntersection         := Intersection;
      MailboxIntersectionDistance := IntersectionDistance;
    end;
  end;
  {$endif}
end;

function TTriangle.RayCollision(
  out Intersection: TVector3;
  out IntersectionDistance: Single;
  const RayOrigin, RayDirection: TVector3;
  const RayTag: TMailboxTag): boolean;
begin
  { uwzgledniam tu fakt ze czesto bedzie wypuszczanych wiele promieni
    z jednego RayOrigin ale z roznym RayDirection (np. w raytracerze). Wiec lepiej
    najpierw porownywac przechowywane w skrzynce RayDirection (niz RayOrigin)
    zeby moc szybciej stwierdzic niezgodnosc. }
  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
  if MailboxSavedTag = RayTag then
  begin
    result := MailboxIsIntersection;
    if result then
    begin
      Intersection         := MailboxIntersection;
      IntersectionDistance := MailboxIntersectionDistance;
    end;
  end else
  begin
  {$endif}

    result := TryTriangleRayCollision(
      Intersection, IntersectionDistance,
      Local.Triangle, Local.Plane,
      RayOrigin, RayDirection);
    Inc(TriangleCollisionTestsCounter);

  {$ifdef TRIANGLE_OCTREE_USE_MAILBOX}
    { zapisz wyniki do mailboxa }
    MailboxSavedTag := RayTag;
    MailboxIsIntersection := result;
    if result then
    begin
      MailboxIntersection         := Intersection;
      MailboxIntersectionDistance := IntersectionDistance;
    end;
  end;
  {$endif}
end;

{$ifndef CONSERVE_TRIANGLE_MEMORY}
function TTriangle.ITexCoord(const Point: TVector3): TVector4;
var
  B: TVector3;
begin
  B := World.Triangle.Barycentric(Point);
  Result := TexCoord.Data[0] * B[0] +
            TexCoord.Data[1] * B[1] +
            TexCoord.Data[2] * B[2];
end;

function TTriangle.ITexCoord2D(const Point: TVector3): TVector2;
var
  V: TVector4;
begin
  V := ITexCoord(Point);
  Move(V, Result, SizeOf(TVector2));
end;

function TTriangle.INormalCore(const Point: TVector3): TVector3;
var
  B: TVector3;
begin
  B := World.Triangle.Barycentric(Point);
  Result := Normal.Data[0] * B[0] +
            Normal.Data[1] * B[1] +
            Normal.Data[2] * B[2];
end;

function TTriangle.INormal(const Point: TVector3): TVector3;
begin
  Result := INormalCore(Point).Normalize;
end;

{$else}
function TTriangle.Face: TFaceIndex;
begin
  Result := UnknownFaceIndex;
end;
{$endif not CONSERVE_TRIANGLE_MEMORY}

{ TTriangle2 ----------------------------------------------------------------- }

function TTriangle2.ToString: string;
begin
  Result :=
    Data[0].ToString + NL +
    Data[1].ToString + NL +
    Data[2].ToString + NL;
end;

{ TTriangle3 ----------------------------------------------------------------- }

function TTriangle3.IsValid: boolean;
begin
  (* We want to check is Tri a "non-degenerated" triangle,
     i.e. does not determine a plane in 3D.
     So all points must be different, and also must not be colinear.

     We can check this by checking
       TVector3.CrossProduct(
         (Data[2] - Data[1]),
         (Data[0] - Data[1])).Length > 0.

     This actually exactly corresponds to saying "this triangle has non-zero area".
     It also measn that TrianglePlane is non-zero, since it uses the same
     TVector3.CrossProduct.
     You can calculate this using TTriangle3.Direction, since TTriangle3.Direction calculates
     exactly this TVector3.CrossProduct.
  *)

  { This is incorrect:
    It would detect as invalid too much (the tests/data/model_manifold.wrl
    would fail then, not all edges detected manifold). }
  // Result := not IsZero(Direction.LengthSqr);

  { This is incorrect:
    It would detect as invalid still too much (the
    examples/research_special_rendering_methods/radiance_transfer/data/chinchilla.wrl.gz
    would be black then, and not pickable) }
  // Result := Direction.LengthSqr > Sqr(SingleEpsilon);

  { This used to be used in Castle Game Engine 6.2, and seems to work OK.
    But it feels wrong: we can make a valid, arbitrarily small triangle,
    and thus any epsilon is wrong. }
  // Result := Direction.LengthSqr > Sqr(1e-7);

  { This is safer, but slower.
    Makes 2 normalizations and this way it can use a larger epsilon.
    But it is still not perfect, as TTestCastleTriangles.TestIsValid shows.
    This detects as invalid

      T[0] := Vector3(5, 0.01, 0);
      T[1] := Vector3(0, 0, 0);
      T[2] := Vector3(10, 0, 0);
      AssertTrue('23', T.IsValid);

    ... and only changing 0.01 to 0.1 helps to make it valid.
  }
  // Result := not IsZero(TVector3.CrossProduct(
  //   (Data[2] - Data[1]).Normalize,
  //   (Data[0] - Data[1]).Normalize
  // ).LengthSqr);

  { This is simple, fast and correct according to tests. }
  Result := Direction.LengthSqr > 0;
end;

function TTriangle3.Direction: TVector3;
begin
  Result := TVector3.CrossProduct(
    Data[2] - Data[1],
    Data[0] - Data[1]);
end;

function TTriangle3.Normal: TVector3;
begin
  Result := Direction.Normalize;
end;

function TTriangle3.Plane: TVector4;
var
  ResultDir: TVector3 absolute Result;
begin
  ResultDir := Direction;
  (* Punkt Data[0] musi lezec na plane Result. Wiec musi zachodzic

     ResulrDir[0] * Data[0, 0] +
     ResulrDir[1] * Data[0, 1] +
     ResulrDir[2] * Data[0, 2]
       + Result[3] = 0

     Stad widac jak wyznaczyc Result[3]. *)
  Result.Data[3] :=
    -ResultDir.Data[0] * Data[0].Data[0]
    -ResultDir.Data[1] * Data[0].Data[1]
    -ResultDir.Data[2] * Data[0].Data[2];
end;

function TTriangle3.NormalizedPlane: TVector4;
var
  ResultNormal: TVector3 absolute Result;
begin
  (* dzialamy tak samo jak TrianglePlane tyle ze teraz uzywamy TriangleNormal
     zamiast TriangleNormalNotNorm *)
  ResultNormal := Normal;
  Result.Data[3] :=
    -ResultNormal.Data[0] * Data[0].Data[0]
    -ResultNormal.Data[1] * Data[0].Data[1]
    -ResultNormal.Data[2] * Data[0].Data[2];
end;

function TTriangle3.Transform(const M: TMatrix4): TTriangle3;
begin
  Result.Data[0] := M.MultPoint(Data[0]);
  Result.Data[1] := M.MultPoint(Data[1]);
  Result.Data[2] := M.MultPoint(Data[2]);
end;

function TTriangle3.Area: Single;
begin
  Result := TVector3.CrossProduct(
    Data[1] - Data[0],
    Data[2] - Data[0]).Length / 2;
end;

function TTriangle3.AreaSqr: Single;
begin
  Result := TVector3.CrossProduct(
    Data[1] - Data[0],
    Data[2] - Data[0]).LengthSqr / 4;
end;

function TTriangle3.ToString: string;
begin
  Result :=
    Data[0].ToString + NL +
    Data[1].ToString + NL +
    Data[2].ToString + NL;
end;

function TTriangle3.RandomPoint: TVector3;
var
  r1Sqrt, r2: Single;
begin
  { Based on "Global Illumination Compendium" }
  r1Sqrt := Sqrt(Random);
  r2 := Random;
  Result :=
    (Data[0] * (1 - r1Sqrt) ) +
    (Data[1] * ((1 - r2) * r1Sqrt) ) +
    (Data[2] * (r2 * r1Sqrt) );
end;

function TTriangle3.Barycentric(const Point: TVector3): TVector3;

  { TODO: a tiny bit of CastleBoxes unit used here, to prevent any dependency
    from CastleVectors to CastleBoxes. }
  type
    TBox3D = record
      Data: array [0..1] of TVector3;
    end;

  function Box3DSizes(const Box: TBox3D): TVector3;
  begin
    Result.Data[0] := Box.Data[1].Data[0] - Box.Data[0].Data[0];
    Result.Data[1] := Box.Data[1].Data[1] - Box.Data[0].Data[1];
    Result.Data[2] := Box.Data[1].Data[2] - Box.Data[0].Data[2];
  end;

  function TriangleBoundingBox: TBox3D;
  begin
    MinMax(Data[0].Data[0], Data[1].Data[0], Data[2].Data[0], Result.Data[0].Data[0], Result.Data[1].Data[0]);
    MinMax(Data[0].Data[1], Data[1].Data[1], Data[2].Data[1], Result.Data[0].Data[1], Result.Data[1].Data[1]);
    MinMax(Data[0].Data[2], Data[1].Data[2], Data[2].Data[2], Result.Data[0].Data[2], Result.Data[1].Data[2]);
  end;

var
  C1, C2: Integer;
  Det: Single;
begin
  { Map triangle and point into 2D, where the solution is simpler.
    Calculate C1 and C2 --- two largest coordinates of
    triangle axis-aligned bounding box. }
  RestOf3DCoords(MinVectorCoord(Box3DSizes(TriangleBoundingBox)), C1, C2);

  { Now calculate coordinates on 2D, following equations at wikipedia }
  Det :=
    (Data[1].Data[C2] - Data[2].Data[C2]) * (Data[0].Data[C1] - Data[2].Data[C1]) +
    (Data[0].Data[C2] - Data[2].Data[C2]) * (Data[2].Data[C1] - Data[1].Data[C1]);
  Result.Data[0] := (
    (Data[1].Data[C2] - Data[2].Data[C2]) * (  Point.Data[C1] - Data[2].Data[C1]) +
    (  Point.Data[C2] - Data[2].Data[C2]) * (Data[2].Data[C1] - Data[1].Data[C1])
    ) / Det;
  Result.Data[1] := (
    (  Point.Data[C2] - Data[2].Data[C2]) * (Data[0].Data[C1] - Data[2].Data[C1]) +
    (Data[2].Data[C2] - Data[0].Data[C2]) * (  Point.Data[C1] - Data[2].Data[C1])
    ) / Det;
  Result.Data[2] := 1 - Result.Data[0] - Result.Data[1];
end;

function TTriangle3.GetItems(const Index: TIndex): TVector3;
begin
  Result := Data[Index];
end;

procedure TTriangle3.SetItems(const Index: TIndex; const Value: TVector3);
begin
  Data[Index] := Value;
end;

{ TTriangle4 ----------------------------------------------------------------- }

function TTriangle4.ToString: string;
begin
  Result :=
    Data[0].ToString + NL +
    Data[1].ToString + NL +
    Data[2].ToString + NL;
end;

{ others --------------------------------------------------------------------- }

function IsPointOnTrianglePlaneWithinTriangle(const P: TVector3;
  const Tri: TTriangle3; const TriDir: TVector3): boolean;

{ We tried many approaches for this:
  - Check do three angles:
    between vectors (t[0]-p) and (t[1]-p),
    between vectors (t[1]-p) and (t[2]-p),
    between vectors (t[2]-p) and (t[0]-p)
    sum to full 360 stopni.
  - Cast triangle on the most suitable 2D plane, and check there.

  The current algorithm is very slightly faster than the above. It's based on
  http://geometryalgorithms.com/Archive/algorithm_0105/algorithm_0105.htm
  (still accessible through
  http://web.archive.org/web/20081018162011/http://www.geometryalgorithms.com/Archive/algorithm_0105/algorithm_0105.htm
  ).

  Idea:
  - Every point on the plane of our triangle may be expressed as s,t, such that
    point = tri[0] + s*u + t*v, where u = tri[1]-tri[0], v = tri[2]-tri[0].
    This way 2 triangle edges determine the 2D coordinates axes,
    analogous to normal OX and OY axes on a 2D plane.
    (We only handle non-degenerate triangles is, so we can assume that
    all triangle points are different and u is not parallel to v.)

  - Point is within the triangle iff s >= 0 and t >= 0 and s+t <= 1.
    (Some reason: note that point = tri[0]*(1-s-t) + tri[1]*s + tri[2]*t,
    so s,t are just 2 barycentric coordinates of our point.)

  - It remains to find s,t.
    Let w = point - tri[0], so w = s*u + t*v.
    Let x^ (for x = direction on a plane) mean TVector3.CrossProduct(x, PlaneDir),
    so a direction orthogonal to x and still on the plane.
    Note some dot product properties:

      (a + b).c = a.c + b.c
      (x * a).c = x * (a.c)
      where a, b, c are some vectors, x is scalar, * is scalar multiplication.

    Now make a dot product of both sides of "w = ..." equation with v^,
    and use the dot product properties mentioned above:

      w.v^ = s*u.v^ + t*v.v^

    v.v^ = 0, because v and v^ are orthogonal.
    So we can calculate s as

      s := w.v^ / (u.v^)

    Analogously, we can calculate v.

  - With some optimizations, this can be further simplified,
    but we found out that the simplified version is actually slightly slower.
}

{ $define IsPointOnTrianglePlaneWithinTriangle_Simplified}
{$ifdef IsPointOnTrianglePlaneWithinTriangle_Simplified}

var
  S, T, One, UU, UV, VV, WV, WU, Denominator: Single;
  W, U, V: TVector3;
begin
  U := Tri.Data[1] - Tri.Data[0];
  V := Tri.Data[2] - Tri.Data[0];
  UV := TVector3.DotProduct(U, V);
  UU := U.LengthSqr; { = TVector3.DotProduct(U, U) }
  VV := V.LengthSqr; { = TVector3.DotProduct(V, V) }
  Denominator := Sqr(UV) - UU * VV;

  W := P - Tri.Data[0];
  WV := TVector3.DotProduct(W, V);
  WU := TVector3.DotProduct(W, U);

  One := 1 + SingleEpsilon;

  S := (UV * WV - VV * WU) / Denominator;
  if (S < -SingleEpsilon) or
    { As far as only correctness is concerned, check for S > One isn't needed
      here since we will check S+T <= One later anyway.
      But for the speed, it's better to make here a quick check
      "S > One" and in many cases avoid the long calculation of T.
      See ~/3dmodels/rayhunter-demos/raporty/2006-11-12/README:
      speed of this procedure has a significant impact
      on the ray-tracer speed, so it's really a justified optimization. }
     (S > One) then
    Exit(false);

  T := (UV * WU - UU * WV) / Denominator;
  if T < -SingleEpsilon then
    Exit(false);

  Result := S + T <= One;
end;

{$else}

var
  S, T: Single;
  W, U, V, Ortho: TVector3;
  One: Single;
begin
  U := Tri.Data[1] - Tri.Data[0];
  V := Tri.Data[2] - Tri.Data[0];
  W := P - Tri.Data[0];

  One := 1 + SingleEpsilon;

  Ortho := TVector3.CrossProduct(V, TriDir);
  S := TVector3.DotProduct(W, Ortho) / TVector3.DotProduct(U, Ortho);
  if (S < -SingleEpsilon) or
    { As far as only correctness is concerned, check for S > One isn't needed
      here since we will check S+T <= One later anyway.
      But for the speed, it's better to make here a quick check
      "S > One" and in many cases avoid the long calculation of T.
      See ~/3dmodels/rayhunter-demos/raporty/2006-11-12/README:
      speed of this procedure has a significant impact
      on the ray-tracer speed, so it's really a justified optimization. }
     (S > One) then
    Exit(false);

  Ortho := TVector3.CrossProduct(U, TriDir);
  T := TVector3.DotProduct(W, Ortho) / TVector3.DotProduct(V, Ortho);
  if T < -SingleEpsilon then
    Exit(false);

  Result := S + T <= One;
end;

{$endif IsPointOnTrianglePlaneWithinTriangle_Simplified}

//function IsPointOnTrianglePlaneWithinTriangle(const P: TVector3;
//  const Tri: TTriangle3): boolean;
//begin
//  Result := IsPointOnTrianglePlaneWithinTriangle(P, Tri, TriangleDirection(Tri));
//end;

function IsPointWithinTriangle2D(const P: TVector2;
  const Tri: TTriangle2): boolean;
var
  Area, S, T, One: Single;
begin
  { see http://stackoverflow.com/questions/2049582/how-to-determine-a-point-in-a-2d-triangle }
  Area := 1 / 2 * (
    - Tri.Data[1].Data[1]*Tri.Data[2].Data[0]
    + Tri.Data[0].Data[1]*(-Tri.Data[1].Data[0] + Tri.Data[2].Data[0])
    + Tri.Data[0].Data[0]*(Tri.Data[1].Data[1] - Tri.Data[2].Data[1])
    + Tri.Data[1].Data[0]*Tri.Data[2].Data[1]);

  S := 1/(2*Area)*(
      Tri.Data[0].Data[1]*Tri.Data[2].Data[0]
    - Tri.Data[0].Data[0]*Tri.Data[2].Data[1]
    + (Tri.Data[2].Data[1] - Tri.Data[0].Data[1]) * P.Data[0]
    + (Tri.Data[0].Data[0] - Tri.Data[2].Data[0]) * P.Data[1]);

  One := 1 + SingleEpsilon;
  if (S < -SingleEpsilon) or
    { Like in 3D: checking this here is an optimization. }
     (S > One) then
    Exit(false);

  T := 1/(2*Area)*(
      Tri.Data[0].Data[0]*Tri.Data[1].Data[1]
    - Tri.Data[0].Data[1]*Tri.Data[1].Data[0]
    + (Tri.Data[0].Data[1] - Tri.Data[1].Data[1]) * P.Data[0]
    + (Tri.Data[1].Data[0] - Tri.Data[0].Data[0]) * P.Data[1]);

  { We could check at the end just this:
      Result := (S > 0) and (T > 0) and (1 - S - T > 0);
    Our more optimized version tries to exit early, and also applies
    SingleEpsilon. }

  if T < -SingleEpsilon then
    Exit(false);
  Result := S + T <= One;
end;

function IsPointWithinTriangle2D(const P: TVector2;
  const Tri: TTriangle3): boolean;
var
  Tri2D: TTriangle2;
begin
  { project Tri on 2D }
  Tri2D.Data[0].Data[0] := Tri.Data[0].Data[0];
  Tri2D.Data[0].Data[1] := Tri.Data[0].Data[1];

  Tri2D.Data[1].Data[0] := Tri.Data[1].Data[0];
  Tri2D.Data[1].Data[1] := Tri.Data[1].Data[1];

  Tri2D.Data[2].Data[0] := Tri.Data[2].Data[0];
  Tri2D.Data[2].Data[1] := Tri.Data[2].Data[1];

  Result := IsPointWithinTriangle2D(P, Tri2D);
end;

function IsTriangleSegmentCollision(const Tri: TTriangle3;
  const TriPlane: TVector4; const Pos1, Pos2: TVector3): boolean;
var
  LineVector, MaybeIntersection: TVector3;
  TriDir: TVector3 absolute TriPlane;
begin
  LineVector := Pos2 - Pos1;
  Result := TryPlaneLineIntersection(MaybeIntersection, TriPlane, Pos1, LineVector) and
            IsPointOnSegmentLineWithinSegment(MaybeIntersection, Pos1, Pos2) and
            IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
end;

function IsTriangleSegmentCollision(const Tri: TTriangle3; const Pos1, Pos2: TVector3): boolean;
begin
  Result := IsTriangleSegmentCollision(Tri, Tri.Plane, Pos1, Pos2);
end;

function TryTriangleSegmentCollision(var Intersection: TVector3;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const Pos1, Pos2: TVector3): boolean;
begin
  Result := TryTriangleSegmentDirCollision(Intersection, Tri, TriPlane,
    Pos1, Pos2 - Pos1);
end;

function TryTriangleSegmentDirCollision(var Intersection: TVector3; var T: Single;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeIntersection: TVector3;
  MaybeT: Single;
  TriDir: TVector3 absolute TriPlane;
begin
  Result := TryPlaneSegmentDirIntersection(MaybeIntersection, MaybeT, TriPlane, Segment0, SegmentVector) and
          IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
  if Result then
  begin
    Intersection := MaybeIntersection;
    T := MaybeT;
  end;
end;

function TryTriangleSegmentDirCollision(var Intersection: TVector3;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeIntersection: TVector3;
  MaybeT: Single;
  TriDir: TVector3 absolute TriPlane;
begin
  Result := TryPlaneSegmentDirIntersection(MaybeIntersection, MaybeT, TriPlane, Segment0, SegmentVector) and
          IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
  if Result then
    Intersection := MaybeIntersection;
end;

function IsTriangleSphereCollision(const Tri: TTriangle3;
  const TriPlane: TVector4;
  const SphereCenter: TVector3; SphereRadius: Single): boolean;
(*$define HAS_PRECALC_PLANE*)
(*$I castletriangles_istrianglespherecollision.inc*)
(*$undef HAS_PRECALC_PLANE*)

function IsTriangleSphereCollision(const Tri: TTriangle3;
  const SphereCenter: TVector3; SphereRadius: Single): boolean;
(*$I castletriangles_istrianglespherecollision.inc*)

function IsTriangleSphereCollision2D(const Tri: TTriangle2;
  const SphereCenter: TVector2; SphereRadius: Single): boolean;
var
  Intersection: TVector2;
  SphereRadiusSqr: Single;
  I, NextI: integer;
begin
  SphereRadiusSqr := Sqr(SphereRadius);

  (* Is SphereCenter within the radius of one of triangle corners.
     Note that this case is necessary, it is not fully catched by
     "SphereCenter close to one of the triangles' edges" lower. *)
  if (PointsDistanceSqr(Tri.Data[0], SphereCenter) <= SphereRadiusSqr) or
     (PointsDistanceSqr(Tri.Data[1], SphereCenter) <= SphereRadiusSqr) or
     (PointsDistanceSqr(Tri.Data[2], SphereCenter) <= SphereRadiusSqr) then
    Exit(true);

  if IsPointWithinTriangle2D(SphereCenter, Tri) then
    Exit(true);

  (* Is SphereCenter close to one of the triangles' edges. *)
  for I := 0 to 2 do
  begin
    NextI := (I + 1) mod 3;
    Intersection := PointOnLineClosestToPoint(Tri.Data[I],
      Tri.Data[NextI] - Tri.Data[I], SphereCenter);
    if IsPointOnSegmentLineWithinSegment(Intersection, Tri.Data[I], Tri.Data[NextI]) and
       (PointsDistanceSqr(SphereCenter, Intersection) <= SphereRadiusSqr) then
      Exit(true);
  end;

  Result := false;
end;

function IsTriangleSphereCollision2D(const Tri: TTriangle3;
  const SphereCenter: TVector2; SphereRadius: Single): boolean;
var
  Tri2D: TTriangle2;
begin
  { project Tri on 2D }
  Tri2D.Data[0].Data[0] := Tri.Data[0].Data[0];
  Tri2D.Data[0].Data[1] := Tri.Data[0].Data[1];

  Tri2D.Data[1].Data[0] := Tri.Data[1].Data[0];
  Tri2D.Data[1].Data[1] := Tri.Data[1].Data[1];

  Tri2D.Data[2].Data[0] := Tri.Data[2].Data[0];
  Tri2D.Data[2].Data[1] := Tri.Data[2].Data[1];

  Result := IsTriangleSphereCollision2D(Tri2D, SphereCenter, SphereRadius);
end;

function TryTriangleRayCollision(var Intersection: TVector3; var T: Single;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeIntersection: TVector3;
  MaybeT: Single;
  TriDir: TVector3 absolute TriPlane;
begin
  Result := TryPlaneRayIntersection(MaybeIntersection, MaybeT, TriPlane, RayOrigin, RayDirection) and
          IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
  if Result then
  begin
    Intersection := MaybeIntersection;
    T := MaybeT;
  end;
end;

function TryTriangleRayCollision(var Intersection: TVector3;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeIntersection: TVector3;
  MaybeT: Single;
  TriDir: TVector3 absolute TriPlane;
begin
  Result := TryPlaneRayIntersection(MaybeIntersection, MaybeT, TriPlane, RayOrigin, RayDirection) and
          IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
  if Result then
    Intersection := MaybeIntersection;
end;

function TriangleDirection(const p0, p1, p2: TVector3): TVector3;
begin
  Result := TVector3.CrossProduct(
    p2 - p1,
    p0 - p1);
end;

function TriangleDir(const p0, p1, p2: TVector3): TVector3;
begin
  Result := TVector3.CrossProduct(
    p2 - p1,
    p0 - p1);
end;

function TriangleNormal(const p0, p1, p2: TVector3): TVector3;
begin
  Result := TriangleDirection(p0, p1, p2).Normalize;
end;

function TrianglePlane(const p0, p1, p2: TVector3): TVector4;
var
  ResultDir: TVector3 absolute Result;
begin
  ResultDir := TriangleDirection(p0, p1, p2);
  Result.Data[3] :=
    -ResultDir.Data[0] * p0.Data[0]
    -ResultDir.Data[1] * p0.Data[1]
    -ResultDir.Data[2] * p0.Data[2];
end;

function TriangleDir(const T: TTriangle3): TVector3;
begin
  Result := T.Direction;
end;

function TriangleNormal(const T: TTriangle3): TVector3;
begin
  Result := T.Normal;
end;

function TrianglePlane(const T: TTriangle3): TVector4;
begin
  Result := T.Plane;
end;

function TriangleTransform(const T: TTriangle3;const M: TMatrix4): TTriangle3;
begin
  Result := T.Transform(M);
end;

function TriangleNormPlane(const T: TTriangle3): TVector4;
begin
  Result := T.NormalizedPlane;
end;

function TriangleArea(const T: TTriangle3): Single;
begin
  Result := T.Area;
end;

function Barycentric(const T: TTriangle3; const Point: TVector3): TVector3;
begin
  Result := T.Barycentric(Point);
end;

function TriangleToNiceStr(const T: TTriangle3): string; deprecated 'use T.ToString';
begin
  Result := T.ToString;
end;

end.
