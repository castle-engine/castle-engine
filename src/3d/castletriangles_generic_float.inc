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

{ Interface and implementation of CastleTriangles for a one floating-point precision,
  like Single or Double.

  Before including this file, define types like TScalar and TVector3 to something like
  - Single and TVector3Single, or
  - Double and TVector3Double.

  This file will define (and implement) triangles functions using these types. }

{$ifdef read_interface}

{ Check does the triangle define a correct plane in 3D space.
  That is, check does the triangle not degenerate to a point or line segment
  (which can happen when some points are at the same position, or are colinear).
  @groupBegin }
function IsValidTriangle(const Tri: TTriangle3): boolean; overload;
{ @groupEnd }

{ Normal vector of a triangle. Returns vector pointing our from CCW triangle
  side (for right-handed coordinate system), and orthogonal to triangle plane.
  The version "Dir" (TriangleDir) doesn't normalize the result
  (it may not have length equal 1).

  For degenerated triangles (when IsValidTriangle would return false),
  we return zero vector.
  @groupBegin }
function TriangleDir(const Tri: TTriangle3): TVector3; overload;
function TriangleDir(const p0, p1, p2: TVector3): TVector3; overload;
function TriangleNormal(const Tri: TTriangle3): TVector3; overload;
function TriangleNormal(const p0, p1, p2: TVector3): TVector3; overload;
{ @groupEnd }

{ Plane of the triangle. Note that this has many possible solutions
  (plane representation as equation @code(Ax + By + Cz + D = 0)
  is not unambiguous), this just returns some solution deterministically.

  It's guaranteed that the direction of this plane (i.e. first 3 items
  of returned vector) will be in the same direction as calcualted by
  TriangleDir, which means that it points outward from CCW side of
  the triangle (assuming right-handed coord system).

  For TriangleNormPlane, this direction is also normalized
  (makes a vector with length 1). This way TrianglePlane calculates
  also TriangleNormal.

  For three points that do not define a plane, a plane with first three
  components = 0 is returned. In fact, the 4th component will be zero too
  in this case (for now), but don't depend on it.
  @groupBegin }
function TrianglePlane(const Tri: TTriangle3): TVector4; overload;
function TrianglePlane(const p0, p1, p2: TVector3): TVector4; overload;
function TriangleNormPlane(const Tri: TTriangle3): TVector4; overload;
{ @groupEnd }

{ Transform triangle by 4x4 matrix. This simply transforms each triangle point.

  @raises(ETransformedResultInvalid Raised when matrix
  will transform some point to a direction (vector with 4th component
  equal zero). In this case we just cannot interpret the result as a 3D point.)

  @groupBegin }
function TriangleTransform(const Tri: TTriangle3; const M: TMatrix4): TTriangle3; overload;
{ @groupEnd }

{ Surface area of 3D triangle.
  This works for degenerated (equal to line segment or even single point)
  triangles too: returns 0 for them.

  @groupBegin }
function TriangleArea(const Tri: TTriangle3): TScalar; overload;
function TriangleAreaSqr(const Tri: TTriangle3): TScalar; overload;
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
  const SphereCenter: TVector3; SphereRadius: TScalar): boolean; overload;
function IsTriangleSphereCollision(const Tri: TTriangle3;
  const SphereCenter: TVector3; SphereRadius: TScalar): boolean; overload;

{ Test collision between triangle and sphere in 2D.
  If you use overloaded version with TTriangle3, the Z coordinate
  of the triangle corners is simply ignored, so everything is projected
  on the Z=0 plane.
  @groupBegin }
function IsTriangleSphereCollision2D(const Tri: TTriangle2;
  const SphereCenter: TVector2; SphereRadius: TScalar): boolean; overload;
function IsTriangleSphereCollision2D(const Tri: TTriangle3;
  const SphereCenter: TVector2; SphereRadius: TScalar): boolean; overload;
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
function TryTriangleSegmentDirCollision(var Intersection: TVector3; var T: TScalar;
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
function TryTriangleRayCollision(var Intersection: TVector3; var T: TScalar;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
{ @groupEnd }

{ Converting to string.
  @groupBegin }
function TriangleToNiceStr(const t: TTriangle2): string; overload;
function TriangleToNiceStr(const t: TTriangle3): string; overload;
function TriangleToRawStr(const t: TTriangle3): string; overload;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

function IsValidTriangle(const Tri: TTriangle3): boolean;
begin
  (* We want to check is Tri a "non-degenerated" triangle,
     i.e. does not determine a plane in 3D.
     So all points must be different, and also must not be colinear.

     We can check this by checking
       VectorProduct(
         (Tri[2] - Tri[1]),
         (Tri[0] - Tri[1])).Length > 0.

     This actually exactly corresponds to saying "this triangle has non-zero area".
     It also measn that TrianglePlane is non-zero, since it uses the same
     VectorProduct.
     You can calculate this using TriangleDir(), since TriangleDir calculates
     exactly this VectorProduct.
  *)
  Result := VectorLenSqr(TriangleDir(Tri)) > Sqr(ScalarEqualityEpsilon);
end;

function TriangleDir(const Tri: TTriangle3): TVector3;
begin
  Result := VectorProduct(
    Tri[2] - Tri[1],
    Tri[0] - Tri[1]);
end;

function TriangleDir(const p0, p1, p2: TVector3): TVector3;
begin
  Result := VectorProduct(
    p2 - p1,
    p0 - p1);
end;

function TriangleNormal(const Tri: TTriangle3): TVector3;
begin
  Result := Normalized( TriangleDir(Tri) );
end;

function TriangleNormal(const p0, p1, p2: TVector3): TVector3;
begin
  Result := Normalized( TriangleDir(p0, p1, p2) );
end;

function TrianglePlane(const Tri: TTriangle3): TVector4;
var
  ResultDir: TVector3 absolute Result;
begin
  ResultDir := TriangleDir(Tri);
  (* Punkt Tri[0] musi lezec na plane Result. Wiec musi zachodzic
     ResulrDir[0]*Tri[0, 0] + ResulrDir[1]*Tri[0, 1] + ResulrDir[2]*Tri[0, 2]
       + Result[3] = 0.
     Stad widac jak wyznaczyc Result[3]. *)
  Result[3] := -ResultDir[0]*Tri[0, 0]
               -ResultDir[1]*Tri[0, 1]
               -ResultDir[2]*Tri[0, 2];
end;

function TrianglePlane(const p0, p1, p2: TVector3): TVector4;
var
  ResultDir: TVector3 absolute Result;
begin
  ResultDir := TriangleDir(p0, p1, p2);
  Result[3] := -ResultDir[0]*p0[0]
               -ResultDir[1]*p0[1]
               -ResultDir[2]*p0[2];
end;

function TriangleNormPlane(const Tri: TTriangle3): TVector4;
var
  ResultNormal: TVector3 absolute Result;
begin
  (* dzialamy tak samo jak TrianglePlane tyle ze teraz uzywamy TriangleNormal
     zamiast TriangleNormalNotNorm *)
  ResultNormal := TriangleNormal(Tri);
  Result[3] := -ResultNormal[0]*Tri[0, 0] -ResultNormal[1]*Tri[0, 1]
    -ResultNormal[2]*Tri[0, 2];
end;

function TriangleTransform(const Tri: TTriangle3; const M: TMatrix4): TTriangle3;
begin
  Result[0] := MatrixMultPoint(M, Tri[0]);
  Result[1] := MatrixMultPoint(M, Tri[1]);
  Result[2] := MatrixMultPoint(M, Tri[2]);
end;

function TriangleArea(const Tri: TTriangle3): TScalar;
begin
  Result := VectorLen(VectorProduct(
    Tri[1] - Tri[0],
    Tri[2] - Tri[0])) / 2;
end;

function TriangleAreaSqr(const Tri: TTriangle3): TScalar;
begin
  Result := VectorLenSqr(
    VectorProduct(Tri[1] - Tri[0],
                  Tri[2] - Tri[0])) / 4;
end;

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
    Let x^ (for x = direction on a plane) mean VectorProduct(x, PlaneDir),
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
  S, T, One, UU, UV, VV, WV, WU, Denominator: TScalar;
  W, U, V: TVector3;
begin
  U := Tri[1] - Tri[0];
  V := Tri[2] - Tri[0];
  UV := VectorDotProduct(U, V);
  UU := VectorLenSqr(U); { = VectorDotProduct(U, U) }
  VV := VectorLenSqr(V); { = VectorDotProduct(V, V) }
  Denominator := Sqr(UV) - UU * VV;

  W := P - Tri[0];
  WV := VectorDotProduct(W, V);
  WU := VectorDotProduct(W, U);

  One := 1 + ScalarEqualityEpsilon;

  S := (UV * WV - VV * WU) / Denominator;
  if (S < -ScalarEqualityEpsilon) or
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
  if T < -ScalarEqualityEpsilon then
    Exit(false);

  Result := S + T <= One;
end;

{$else}

var
  S, T: TScalar;
  W, U, V, Ortho: TVector3;
  One: TScalar;
begin
  U := Tri[1] - Tri[0];
  V := Tri[2] - Tri[0];
  W := P - Tri[0];

  One := 1 + ScalarEqualityEpsilon;

  Ortho := VectorProduct(V, TriDir);
  S := VectorDotProduct(W, Ortho) / VectorDotProduct(U, Ortho);
  if (S < -ScalarEqualityEpsilon) or
    { As far as only correctness is concerned, check for S > One isn't needed
      here since we will check S+T <= One later anyway.
      But for the speed, it's better to make here a quick check
      "S > One" and in many cases avoid the long calculation of T.
      See ~/3dmodels/rayhunter-demos/raporty/2006-11-12/README:
      speed of this procedure has a significant impact
      on the ray-tracer speed, so it's really a justified optimization. }
     (S > One) then
    Exit(false);

  Ortho := VectorProduct(U, TriDir);
  T := VectorDotProduct(W, Ortho) / VectorDotProduct(V, Ortho);
  if T < -ScalarEqualityEpsilon then
    Exit(false);

  Result := S + T <= One;
end;

{$endif IsPointOnTrianglePlaneWithinTriangle_Simplified}

//function IsPointOnTrianglePlaneWithinTriangle(const P: TVector3;
//  const Tri: TTriangle3): boolean;
//begin
//  Result := IsPointOnTrianglePlaneWithinTriangle(P, Tri, TriangleDir(Tri));
//end;

function IsPointWithinTriangle2D(const P: TVector2;
  const Tri: TTriangle2): boolean;
var
  Area, S, T, One: TScalar;
begin
  { see http://stackoverflow.com/questions/2049582/how-to-determine-a-point-in-a-2d-triangle }
  Area := 1 / 2 * (
    - Tri[1][1]*Tri[2][0]
    + Tri[0][1]*(-Tri[1][0] + Tri[2][0])
    + Tri[0][0]*(Tri[1][1] - Tri[2][1])
    + Tri[1][0]*Tri[2][1]);

  S := 1/(2*Area)*(
      Tri[0][1]*Tri[2][0]
    - Tri[0][0]*Tri[2][1]
    + (Tri[2][1] - Tri[0][1]) * P[0]
    + (Tri[0][0] - Tri[2][0]) * P[1]);

  One := 1 + ScalarEqualityEpsilon;
  if (S < -ScalarEqualityEpsilon) or
    { Like in 3D: checking this here is an optimization. }
     (S > One) then
    Exit(false);

  T := 1/(2*Area)*(
      Tri[0][0]*Tri[1][1]
    - Tri[0][1]*Tri[1][0]
    + (Tri[0][1] - Tri[1][1]) * P[0]
    + (Tri[1][0] - Tri[0][0]) * P[1]);

  { We could check at the end just this:
      Result := (S > 0) and (T > 0) and (1 - S - T > 0);
    Our more optimized version tries to exit early, and also applies
    ScalarEqualityEpsilon. }

  if T < -ScalarEqualityEpsilon then
    Exit(false);
  Result := S + T <= One;
end;

function IsPointWithinTriangle2D(const P: TVector2;
  const Tri: TTriangle3): boolean;
var
  Tri2D: TTriangle2;
begin
  { project Tri on 2D }
  Tri2D[0][0] := Tri[0][0];
  Tri2D[0][1] := Tri[0][1];

  Tri2D[1][0] := Tri[1][0];
  Tri2D[1][1] := Tri[1][1];

  Tri2D[2][0] := Tri[2][0];
  Tri2D[2][1] := Tri[2][1];

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
  Result := IsTriangleSegmentCollision(Tri, TrianglePlane(Tri), Pos1, Pos2);
end;

function TryTriangleSegmentCollision(var Intersection: TVector3;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const Pos1, Pos2: TVector3): boolean;
begin
  Result := TryTriangleSegmentDirCollision(Intersection, Tri, TriPlane,
    Pos1, Pos2 - Pos1);
end;

function TryTriangleSegmentDirCollision(var Intersection: TVector3; var T: TScalar;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeIntersection: TVector3;
  MaybeT: TScalar;
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
  MaybeT: TScalar;
  TriDir: TVector3 absolute TriPlane;
begin
  Result := TryPlaneSegmentDirIntersection(MaybeIntersection, MaybeT, TriPlane, Segment0, SegmentVector) and
          IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
  if Result then
    Intersection := MaybeIntersection;
end;

function IsTriangleSphereCollision(const Tri: TTriangle3;
  const TriPlane: TVector4;
  const SphereCenter: TVector3; SphereRadius: TScalar): boolean;
(*$define HAS_PRECALC_PLANE*)
(*$I castletriangles_istrianglespherecollision.inc*)
(*$undef HAS_PRECALC_PLANE*)

function IsTriangleSphereCollision(const Tri: TTriangle3;
  const SphereCenter: TVector3; SphereRadius: TScalar): boolean;
(*$I castletriangles_istrianglespherecollision.inc*)

function IsTriangleSphereCollision2D(const Tri: TTriangle2;
  const SphereCenter: TVector2; SphereRadius: TScalar): boolean;
(*$I castletriangles_istrianglespherecollision2d.inc*)

function IsTriangleSphereCollision2D(const Tri: TTriangle3;
  const SphereCenter: TVector2; SphereRadius: TScalar): boolean;
var
  Tri2D: TTriangle2;
begin
  { project Tri on 2D }
  Tri2D[0][0] := Tri[0][0];
  Tri2D[0][1] := Tri[0][1];

  Tri2D[1][0] := Tri[1][0];
  Tri2D[1][1] := Tri[1][1];

  Tri2D[2][0] := Tri[2][0];
  Tri2D[2][1] := Tri[2][1];

  Result := IsTriangleSphereCollision2D(Tri2D, SphereCenter, SphereRadius);
end;

function TryTriangleRayCollision(var Intersection: TVector3; var T: TScalar;
  const Tri: TTriangle3; const TriPlane: TVector4;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeIntersection: TVector3;
  MaybeT: TScalar;
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
  MaybeT: TScalar;
  TriDir: TVector3 absolute TriPlane;
begin
  Result := TryPlaneRayIntersection(MaybeIntersection, MaybeT, TriPlane, RayOrigin, RayDirection) and
          IsPointOnTrianglePlaneWithinTriangle(MaybeIntersection, Tri, TriDir);
  if Result then
    Intersection := MaybeIntersection;
end;

{ triangles and strings ------------------------------------------------------ }

function TriangleToNiceStr(const t: TTriangle2): string;
begin
  Result := '['+VectorToNiceStr(t[0])+', '
               +VectorToNiceStr(t[1])+', '
               +VectorToNiceStr(t[2])+']';
end;

function TriangleToNiceStr(const t: TTriangle3): string;
begin
  Result := '['+VectorToNiceStr(t[0])+', '
               +VectorToNiceStr(t[1])+', '
               +VectorToNiceStr(t[2])+']';
end;

function TriangleToRawStr(const T: TTriangle3): string;
begin
  Result := '[' + VectorToRawStr(T[0]) + ', '
                + VectorToRawStr(T[1]) + ', '
                + VectorToRawStr(T[2]) + ']';
end;

{$endif read_implementation}
