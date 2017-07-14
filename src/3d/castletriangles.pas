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

{ Triangles.

  @italic(Triangle) is a @code(TTriangle<point-type>) type.
  Where @code(<point-type>) is such suffix that vector type
  @code(TVector<point-type>) exists. For example, we have
  TVector3Single type that represents a point in 3D space,
  so you can use TTriangle3Single to represent triangle in 3D space.
  There are also 2D triangles like TTriangle2Single and TTriangle2Double.

  Triangle's three points must not be collinear,
  i.e. routines in this unit generally don't accept "degenerated" triangles
  that are not really triangles. So 3D triangle must unambiguously
  define some plane in the 3D space. The only function in this unit
  that is able to handle "degenerated" triangles is IsValidTriangle,
  which is exactly used to check whether the triangle is degenerated.

  Since every valid triangle unambiguously determines some plane in the
  3D space, it also determines it's normal vector. In this unit,
  when dealing with normal vectors, I use two names:
  @unorderedList(
    @itemSpacing Compact
    @item(@italic(@noAutoLink(TriangleNormal))
      means that this is the normalized (i.e. scaled to length 1.0)
      normal vector.)
    @item(@italic(@noAutoLink(TriangleDir))
      means that this is not necessarily normalized normal vector.)
  )
}
unit CastleTriangles;

{$I castleconf.inc}
{$I octreeconf.inc}

interface

uses Generics.Collections,
  CastleUtils, CastleVectors;

type
  TTriangle2Single = packed array[0..2]of TVector2Single;     PTriangle2Single = ^TTriangle2Single;
  TTriangle2Double = packed array[0..2]of TVector2Double;     PTriangle2Double = ^TTriangle2Double;
  TTriangle2Extended = packed array[0..2]of TVector2Extended; PTriangle2Extended = ^TTriangle2Extended;

  TTriangle3Single = packed array[0..2]of TVector3Single;     PTriangle3Single = ^TTriangle3Single;
  TTriangle3Double = packed array[0..2]of TVector3Double;     PTriangle3Double = ^TTriangle3Double;
  TTriangle3Extended = packed array[0..2]of TVector3Extended; PTriangle3Extended = ^TTriangle3Extended;

  TTriangle4Single = packed array[0..2]of TVector4Single;     PTriangle4Single = ^TTriangle4Single;

function Triangle3Single(const T: TTriangle3Double): TTriangle3Single; overload;
function Triangle3Single(const p0, p1, p2: TVector3Single): TTriangle3Single; overload;
function Triangle3Double(const T: TTriangle3Single): TTriangle3Double; overload;
function Triangle3Double(const p0, p1, p2: TVector3Double): TTriangle3Double; overload;

{ Normal vector of a triangle defined as three indexes intro vertex array.
  VerticesStride is the shift between vertex values in the array,
  VerticesStride = 0 behaves like VerticesStride = SizeOf(TVector3Single). }
function IndexedTriangleNormal(const Indexes: TVector3Cardinal;
  VerticesArray: PVector3Single; VerticesStride: integer): TVector3Single;

{ Random triangle point, chosen with a constant density for triangle area. }
function SampleTrianglePoint(const Tri: TTriangle3Single): TVector3Single;

{ For a given Point lying on a given Triangle, calculate it's barycentric
  coordinates.

  The resulting Barycentric coordinates can be used for linearly
  interpolating values along the triangle, as they satisfy the equation:

  @preformatted(
    Result[0] * Triangle[0] +
    Result[1] * Triangle[1] +
    Result[2] * Triangle[2] = Point
  )

  See also [http://en.wikipedia.org/wiki/Barycentric_coordinate_system_%28mathematics%29] }
function Barycentric(const Triangle: TTriangle3Single;
  const Point: TVector3Single): TVector3Single;

type
  { Triangle expressed in particular coordinate system, for T3DTriangle. }
  T3DTriangleGeometry = record
    Triangle: TTriangle3Single;

    { Area of the triangle. }
    {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
    function Area: Single;
    {$else}
    Area: Single;
    {$endif}

    {$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
    function Plane: TVector4Single;
    function Normal: TVector3Single;
    {$else}
    case Integer of
      0: ({ Triangle normal, a 3D plane containing our Triangle, with normalized
            direction vector. }
          Plane: TVector4Single;);
      1: (Normal: TVector3Single;);
    {$endif}
  end;

  { 3D triangle.

    This object should always be initialized by @link(Init),
    and updated only by it's methods (never modify fields of
    this object directly).

    I use old-style Pascal "object" to define this,
    since this makes it a little more efficient. This doesn't need
    any virtual methods or such, so (at least for now) it's easier
    and more memory-efficient to keep this as an old-style object.
    And memory efficiency is somewhat important here, since large
    scenes may easily have milions of triangles, and each triangle
    results in one TTriangle (descendant of T3DTriangle) instance. }
  T3DTriangle = object
  public
    { Initialize new triangle. Given ATriangle must satisfy IsValidTriangle. }
    constructor Init(const ATriangle: TTriangle3Single);

  public
    { Geometry of this item.
      We need two geometry descriptions:

      @unorderedList(

        @item(Local is based on initial Triangle, given when constructing
          this T3DTriangle. It's constant for this T3DTriangle. It's used
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
    Local, World: T3DTriangleGeometry;
  end;
  P3DTriangle = ^T3DTriangle;

  { Return for given Triangle do we want to ignore collisions with it.
    For now, Sender is always TTriangleOctree. }
  T3DTriangleIgnoreFunc = function (
    const Sender: TObject;
    const Triangle: P3DTriangle): boolean of object;

{ polygons ------------------------------------------------------------------- }

{ Calculates normalized normal vector for polygon composed from
  indexed vertices. Polygon is defines as vertices
  Verts[Indices[0]], Verts[Indices[1]] ... Verts[Indices[IndicesCount-1]].
  Returns normal pointing from CCW.

  It's secured against invalid indexes on Indices list (that's the only
  reason why it takes VertsCount parameter, after all): they are ignored.

  If the polygon is degenerated, that is it doesn't determine a plane in
  3D space (this includes, but is not limited, to cases when there are
  less than 3 valid points, like when IndicesCount < 3)
  then it returns ResultForIncorrectPoly.

  @groupBegin }
function IndexedConvexPolygonNormal(
  Indices: PArray_Longint; IndicesCount: integer;
  Verts: PVector3Single; const VertsCount: Integer;
  const ResultForIncorrectPoly: TVector3Single): TVector3Single; overload;
function IndexedConvexPolygonNormal(
  Indices: PArray_Longint; IndicesCount: integer;
  Verts: PVector3Single; const VertsCount: Integer; const VertsStride: PtrUInt;
  const ResultForIncorrectPoly: TVector3Single): TVector3Single; overload;
{ @groupEnd }

{ Surface area of indexed convex polygon.
  Polygon is defines as vertices
  Verts[Indices[0]], Verts[Indices[1]] ... Verts[Indices[IndicesCount-1]].

  It's secured against invalid indexes on Indices list (that's the only
  reason why it takes VertsCount parameter, after all): they are ignored.

  @groupBegin }
function IndexedConvexPolygonArea(
  Indices: PArray_Longint; IndicesCount: integer;
  Verts: PArray_Vector3Single; const VertsCount: Integer): Single; overload;
function IndexedConvexPolygonArea(
  Indices: PArray_Longint; IndicesCount: integer;
  Verts: PVector3Single; const VertsCount: Integer; const VertsStride: PtrUInt): Single; overload;
{ @groupEnd }

{ Are the polygon points ordered CCW (counter-clockwise). When viewed
  with typical camera settings, that is +Y goes up and +X goes right.

  Polygon doesn't have to be convex. Polygon doesn't have to have all triangles
  valid, that is it's OK if some polygon triangles degenerate into points
  or line segments.

  Returns something > 0 if polygon is CCW, or < 0 when it's not.
  Returns zero when polygon has area 0.
  @groupBegin }
function IsPolygon2dCCW(Verts: PArray_Vector2Single; const VertsCount: Integer): Single; overload;
function IsPolygon2dCCW(const Verts: array of TVector2Single): Single; overload;
{ @groupEnd }

{ Calculate polygon area.

  Polygon doesn't have to be convex. Polygon doesn't have to have all triangles
  valid, that is it's OK if some polygon triangles degenerate into points
  or line segments.

  @groupBegin }
function Polygon2dArea(Verts: PArray_Vector2Single; const VertsCount: Integer): Single; overload;
function Polygon2dArea(const Verts: array of TVector2Single): Single; overload;
{ @groupEnd }

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

  TFaceIndexesList = specialize TStructList<TFaceIndex>;

const
  UnknownFaceIndex: TFaceIndex = (IndexBegin: -1; IndexEnd: -1);

{ includes ------------------------------------------------------------------- }

{$define read_interface}

{$define TScalar := Single}
{$define TVector2 := TVector2Single}
{$define TVector3 := TVector3Single}
{$define TVector4 := TVector4Single}
{$define PVector2 := PVector2Single}
{$define PVector3 := PVector3Single}
{$define PVector4 := PVector4Single}
{$define TTriangle2 := TTriangle2Single}
{$define TTriangle3 := TTriangle3Single}
{$define TMatrix2 := TMatrix2Single}
{$define TMatrix3 := TMatrix3Single}
{$define TMatrix4 := TMatrix4Single}
{$define ScalarEqualityEpsilon := SingleEqualityEpsilon}
{$define UnitVector3 := UnitVector3Single}
{$define ZeroVector3 := ZeroVector3Single}
{$define IdentityMatrix4 := IdentityMatrix4Single}
{$define TMatrix2_ := TMatrix2_Single}
{$define TMatrix3_ := TMatrix3_Single}
{$define TMatrix4_ := TMatrix4_Single}
{$define TVector2_ := TVector2_Single}
{$define TVector3_ := TVector3_Single}
{$define TVector4_ := TVector4_Single}
{$I castletriangles_generic_float.inc}

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}
{$define TScalar := Double}
{$define TVector2 := TVector2Double}
{$define TVector3 := TVector3Double}
{$define TVector4 := TVector4Double}
{$define PVector2 := PVector2Double}
{$define PVector3 := PVector3Double}
{$define PVector4 := PVector4Double}
{$define TTriangle2 := TTriangle2Double}
{$define TTriangle3 := TTriangle3Double}
{$define TMatrix2 := TMatrix2Double}
{$define TMatrix3 := TMatrix3Double}
{$define TMatrix4 := TMatrix4Double}
{$define ScalarEqualityEpsilon := DoubleEqualityEpsilon}
{$define UnitVector3 := UnitVector3Double}
{$define ZeroVector3 := ZeroVector3Double}
{$define IdentityMatrix4 := IdentityMatrix4Double}
{$define TMatrix2_ := TMatrix2_Double}
{$define TMatrix3_ := TMatrix3_Double}
{$define TMatrix4_ := TMatrix4_Double}
{$define TVector2_ := TVector2_Double}
{$define TVector3_ := TVector3_Double}
{$define TVector4_ := TVector4_Double}
{$I castletriangles_generic_float.inc}
{$endif CASTLE_HAS_DOUBLE_PRECISION}

{$undef read_interface}

implementation

function Triangle3Single(const T: TTriangle3Double): TTriangle3Single;
begin
  result[0] := Vector3Single(T[0]);
  result[1] := Vector3Single(T[1]);
  result[2] := Vector3Single(T[2]);
end;

function Triangle3Single(const p0, p1, p2: TVector3Single): TTriangle3Single;
begin
  result[0] := p0;
  result[1] := p1;
  result[2] := p2;
end;

function Triangle3Double(const T: TTriangle3Single): TTriangle3Double;
begin
  result[0] := Vector3Double(T[0]);
  result[1] := Vector3Double(T[1]);
  result[2] := Vector3Double(T[2]);
end;

function Triangle3Double(const p0, p1, p2: TVector3Double): TTriangle3Double;
begin
  result[0] := p0;
  result[1] := p1;
  result[2] := p2;
end;

function IndexedTriangleNormal(const Indexes: TVector3Cardinal;
  VerticesArray: PVector3Single; VerticesStride: integer): TVector3Single;
var Tri: TTriangle3Single;
    i: integer;
begin
 if VerticesStride = 0 then VerticesStride := SizeOf(TVector3Single);
 for i := 0 to 2 do
  Tri[i] := PVector3Single(PointerAdd(VerticesArray, VerticesStride*Integer(Indexes[i])))^;
 result := TriangleNormal(Tri);
end;

function IndexedConvexPolygonNormal(
  Indices: PArray_Longint; IndicesCount: integer;
  Verts: PVector3Single; const VertsCount: Integer;
  const ResultForIncorrectPoly: TVector3Single): TVector3Single;
begin
  Result := IndexedConvexPolygonNormal(
    Indices, IndicesCount,
    Verts, VertsCount, SizeOf(TVector3Single),
    ResultForIncorrectPoly);
end;

function IndexedConvexPolygonNormal(
  Indices: PArray_Longint; IndicesCount: integer;
  Verts: PVector3Single; const VertsCount: Integer; const VertsStride: PtrUInt;
  const ResultForIncorrectPoly: TVector3Single): TVector3Single;
var Tri: TTriangle3Single;
    i: integer;
begin
  { We calculate normal vector as an average of normal vectors of
    polygon's triangles. Not taking into account invalid Indices
    (pointing beyond the VertsCount range) and degenerated triangles.

    This isn't the fastest method possible, but it's safest.
    It works Ok even if the polygon isn't precisely planar, or has
    some degenerate triangles. }

  Result := ZeroVector3Single;

  I := 0;

  { Verts_Indices_I = Verts[Indices[I]], but takes into account
    that Verts is an array with VertsStride. }
  {$define Verts_Indices_I :=
    PVector3Single(PtrUInt(Verts) + PtrUInt(Indices^[I]) * VertsStride)^}

  while (I < IndicesCount) and (Indices^[I] >= VertsCount) do Inc(I);
  { This secures us against polygons with no valid Indices[].
    (including case when IndicesCount = 0). }
  if I >= IndicesCount then
    Exit(ResultForIncorrectPoly);
  Tri[0] := Verts_Indices_I;

  repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
  if I >= IndicesCount then
    Exit(ResultForIncorrectPoly);
  Tri[1] := Verts_Indices_I;

  repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
  if I >= IndicesCount then
    Exit(ResultForIncorrectPoly);
  Tri[2] := Verts_Indices_I;

  if IsValidTriangle(Tri) then
    Result := Result + TriangleNormal(Tri);

  repeat
    { find next valid point, which makes another triangle of polygon }

    repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
    if I >= IndicesCount then
      Break;
    Tri[1] := Tri[2];
    Tri[2] := Verts_Indices_I;

    if IsValidTriangle(Tri) then
      Result := Result + TriangleNormal(Tri);
  until false;

  { All triangle normals are summed up now. (Each triangle normal was also
    normalized, to have equal contribution to the result.)
    Normalize Result now, if we had any valid triangle. }
  if ZeroVector(Result) then
    Result := ResultForIncorrectPoly else
    NormalizeVar(Result);
end;

function IndexedConvexPolygonArea(
  Indices: PArray_Longint; IndicesCount: integer;
  Verts: PArray_Vector3Single; const VertsCount: Integer): Single;
begin
  Result := IndexedConvexPolygonArea(
    Indices, IndicesCount,
    PVector3Single(Verts), VertsCount, SizeOf(TVector3Single));
end;

function IndexedConvexPolygonArea(
  Indices: PArray_Longint; IndicesCount: integer;
  Verts: PVector3Single; const VertsCount: Integer; const VertsStride: PtrUInt): Single;
var
  Tri: TTriangle3Single;
  i: integer;
begin
  { We calculate area as a sum of areas of
    polygon's triangles. Not taking into account invalid Indices
    (pointing beyond the VertsCount range). }

  Result := 0;

  I := 0;

  { Verts_Indices_I = Verts[Indices[I]], but takes into account
    that Verts is an array with VertsStride. }
  {$define Verts_Indices_I :=
    PVector3Single(PtrUInt(Verts) + PtrUInt(Indices^[I]) * VertsStride)^}

  while (I < IndicesCount) and (Indices^[I] >= VertsCount) do Inc(I);
  { This secures us against polygons with no valid Indices[].
    (including case when IndicesCount = 0). }
  if I >= IndicesCount then
    Exit;
  Tri[0] := Verts_Indices_I;

  repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
  if I >= IndicesCount then
    Exit;
  Tri[1] := Verts_Indices_I;

  repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
  if I >= IndicesCount then
    Exit;
  Tri[2] := Verts_Indices_I;

  Result += TriangleArea(Tri);

  repeat
    { find next valid point, which makes another triangle of polygon }

    repeat Inc(I) until (I >= IndicesCount) or (Indices^[I] < VertsCount);
    if I >= IndicesCount then
      Break;
    Tri[1] := Tri[2];
    Tri[2] := Verts_Indices_I;

    Result += TriangleArea(Tri);
  until false;
end;

function IsPolygon2dCCW(Verts: PArray_Vector2Single; const VertsCount: Integer): Single;
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
  result := 0.0;
  if VertsCount = 0 then Exit;

  { licze i = 0..VertsCount-2, potem osobno przypadek gdy i = VertsCount-1.
    Moglbym ujac je razem, dajac zamiast "Verts[i+1, 1]"
    "Verts[(i+1)mod VertsCount, 1]" ale szkoda byloby dawac tu "mod" na potrzebe
    tylko jednego przypadku. Tak jest optymalniej czasowo. }
  for i := 0 to VertsCount-2 do
    result += Verts^[i, 0] * Verts^[i+1, 1] -
              Verts^[i, 1] * Verts^[i+1, 0];
  result += Verts^[VertsCount-1, 0] * Verts^[0, 1] -
            Verts^[VertsCount-1, 1] * Verts^[0, 0];

  result /= 2;
end;

function IsPolygon2dCCW(const Verts: array of TVector2Single): Single;
begin
  result := IsPolygon2dCCW(@Verts, High(Verts)+1);
end;

function Polygon2dArea(Verts: PArray_Vector2Single; const VertsCount: Integer): Single;
{ opieramy sie tutaj na WEWNETRZNEJ IMPLEMENTACJI funkcji IsPolygonCCW:
  mianowicie wiemy ze, przynajmniej teraz, funkcja ta zwraca pole
  polygonu CCW lub -pole polygonu CW. }
begin
  result := Abs(IsPolygon2dCCW(Verts, VertsCount));
end;

function Polygon2dArea(const Verts: array of TVector2Single): Single;
begin
  result := Polygon2dArea(@Verts, High(Verts) + 1);
end;

function SampleTrianglePoint(const Tri: TTriangle3Single): TVector3Single;
var
  r1Sqrt, r2: Single;
begin
  { Based on "Global Illumination Compendium" }
  r1Sqrt := Sqrt(Random);
  r2 := Random;
  Result :=
    (Tri[0] * (1 - r1Sqrt) ) +
    (Tri[1] * ((1 - r2) * r1Sqrt) ) +
    (Tri[2] * (r2 * r1Sqrt) );
end;

function Barycentric(const Triangle: TTriangle3Single;
  const Point: TVector3Single): TVector3Single;

  { TODO: a tiny bit of CastleBoxes unit used here, to prevent any dependency
    from CastleVectors to CastleBoxes. }
  type
    TBox3D     = array [0..1] of TVector3Single;

  function Box3DSizes(const Box: TBox3D): TVector3Single;
  begin
    Result[0] := Box[1, 0] - Box[0, 0];
    Result[1] := Box[1, 1] - Box[0, 1];
    Result[2] := Box[1, 2] - Box[0, 2];
  end;

  function TriangleBoundingBox(const T: TTriangle3Single): TBox3D;
  begin
    MinMax(T[0][0], T[1][0], T[2][0], Result[0][0], Result[1][0]);
    MinMax(T[0][1], T[1][1], T[2][1], Result[0][1], Result[1][1]);
    MinMax(T[0][2], T[1][2], T[2][2], Result[0][2], Result[1][2]);
  end;

var
  C1, C2: Integer;
  Det: Single;
begin
  { Map triangle and point into 2D, where the solution is simpler.
    Calculate C1 and C2 --- two largest coordinates of
    triangle axis-aligned bounding box. }
  RestOf3DCoords(MinVectorCoord(Box3DSizes(TriangleBoundingBox(Triangle))), C1, C2);

  { Now calculate coordinates on 2D, following equations at wikipedia }
  Det :=
    (Triangle[1][C2] - Triangle[2][C2]) * (Triangle[0][C1] - Triangle[2][C1]) +
    (Triangle[0][C2] - Triangle[2][C2]) * (Triangle[2][C1] - Triangle[1][C1]);
  Result[0] := (
    (Triangle[1][C2] - Triangle[2][C2]) * (      Point[C1] - Triangle[2][C1]) +
    (      Point[C2] - Triangle[2][C2]) * (Triangle[2][C1] - Triangle[1][C1])
    ) / Det;
  Result[1] := (
    (      Point[C2] - Triangle[2][C2]) * (Triangle[0][C1] - Triangle[2][C1]) +
    (Triangle[2][C2] - Triangle[0][C2]) * (      Point[C1] - Triangle[2][C1])
    ) / Det;
  Result[2] := 1 - Result[0] - Result[1];
end;

{ T3DTriangleGeometry -------------------------------------------------------- }

{$ifdef CONSERVE_TRIANGLE_MEMORY_MORE}
function T3DTriangleGeometry.Area: Single;
begin
  Result := TriangleArea(Triangle);
end;

function T3DTriangleGeometry.Plane: TVector4Single;
begin
  Result := TriangleNormPlane(Triangle);
end;

function T3DTriangleGeometry.Normal: TVector3Single;
begin
  Result := TriangleNormal(Triangle);
end;
{$endif}

{ T3DTriangle  --------------------------------------------------------------- }

constructor T3DTriangle.Init(const ATriangle: TTriangle3Single);
begin
  Local.Triangle := ATriangle;
  {$ifndef CONSERVE_TRIANGLE_MEMORY_MORE}
  Local.Plane := TriangleNormPlane(ATriangle);
  Local.Area := TriangleArea(ATriangle);
  {$endif}

  World := Local;
end;

{ includes ------------------------------------------------------------------- }

{$define read_implementation}

{$define TScalar := Single}
{$define TVector2 := TVector2Single}
{$define TVector3 := TVector3Single}
{$define TVector4 := TVector4Single}
{$define PVector2 := PVector2Single}
{$define PVector3 := PVector3Single}
{$define PVector4 := PVector4Single}
{$define TTriangle2 := TTriangle2Single}
{$define TTriangle3 := TTriangle3Single}
{$define TMatrix2 := TMatrix2Single}
{$define TMatrix3 := TMatrix3Single}
{$define TMatrix4 := TMatrix4Single}
{$define ScalarEqualityEpsilon := SingleEqualityEpsilon}
{$define UnitVector3 := UnitVector3Single}
{$define ZeroVector3 := ZeroVector3Single}
{$define IdentityMatrix4 := IdentityMatrix4Single}
{$define TMatrix2_ := TMatrix2_Single}
{$define TMatrix3_ := TMatrix3_Single}
{$define TMatrix4_ := TMatrix4_Single}
{$define TVector2_ := TVector2_Single}
{$define TVector3_ := TVector3_Single}
{$define TVector4_ := TVector4_Single}
{$I castletriangles_generic_float.inc}

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}
{$define TScalar := Double}
{$define TVector2 := TVector2Double}
{$define TVector3 := TVector3Double}
{$define TVector4 := TVector4Double}
{$define PVector2 := PVector2Double}
{$define PVector3 := PVector3Double}
{$define PVector4 := PVector4Double}
{$define TTriangle2 := TTriangle2Double}
{$define TTriangle3 := TTriangle3Double}
{$define TMatrix2 := TMatrix2Double}
{$define TMatrix3 := TMatrix3Double}
{$define TMatrix4 := TMatrix4Double}
{$define ScalarEqualityEpsilon := DoubleEqualityEpsilon}
{$define UnitVector3 := UnitVector3Double}
{$define ZeroVector3 := ZeroVector3Double}
{$define IdentityMatrix4 := IdentityMatrix4Double}
{$define TMatrix2_ := TMatrix2_Double}
{$define TMatrix3_ := TMatrix3_Double}
{$define TMatrix4_ := TMatrix4_Double}
{$define TVector2_ := TVector2_Double}
{$define TVector3_ := TVector3_Double}
{$define TVector4_ := TVector4_Double}
{$I castletriangles_generic_float.inc}
{$endif CASTLE_HAS_DOUBLE_PRECISION}

end.
