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

{ Interface and implementation of CastleVectors for a one floating-point precision,
  like Single or Double.

  Before including this file, define types like TScalar and TVector3 to something like
  - Single and TVector3Single, or
  - Double and TVector3Double.

  This file will define (and implement) many vector, matrix and geometric functions
  using these types. }

{$ifdef read_interface}

{ Compare two float values, with some epsilon.
  When two float values differ by less than given epsilon, they are
  considered equal.

  @groupBegin }
function FloatsEqual(const f1, f2: TScalar): boolean; overload;
function FloatsEqual(const f1, f2, EqEpsilon: TScalar): boolean; overload;
{ @groupEnd }

{ Compare float value with zero, with some epsilon.
  This is somewhat optimized version of doing FloatsEqual(F1, 0).

  This is named Zero, not IsZero --- to not collide with IsZero function
  in Math unit (that has the same purpose, but uses different epsilons
  by default).

  @groupBegin }
function Zero(const f1: TScalar): boolean; overload;
function Zero(const f1, EqEpsilon: TScalar): boolean; overload;
{ @groupEnd }

{ Subtract two vectors.

  Versions *Var place result back into the 1st vector,
  like "-=" operator. Are @italic(very very slightly) faster.

  @groupBegin }
function VectorSubtract(const V1, V2: TVector2): TVector2; overload;
function VectorSubtract(const V1, V2: TVector3): TVector3; overload;
function VectorSubtract(const V1, V2: TVector4): TVector4; overload;
procedure VectorSubtractVar(var v1: TVector2; const v2: TVector2); overload;
procedure VectorSubtractVar(var v1: TVector3; const v2: TVector3); overload;
procedure VectorSubtractVar(var v1: TVector4; const v2: TVector4); overload;
{ @groupEnd }

{ Add two vectors.

  Versions *Var place result back into the 1st vector,
  like "+=" operator. Are @italic(very very slightly) faster.

  @groupBegin }
function VectorAdd(const V1, V2: TVector2): TVector2; overload;
function VectorAdd(const V1, V2: TVector3): TVector3; overload;
function VectorAdd(const V1, V2: TVector4): TVector4; overload;
procedure VectorAddVar(var v1: TVector2; const v2: TVector2); overload;
procedure VectorAddVar(var v1: TVector3; const v2: TVector3); overload;
procedure VectorAddVar(var v1: TVector4; const v2: TVector4); overload;
{ @groupEnd }

{ Scale vector (aka multiply by scalar).

  Versions *Var scale place result back into the 1st vector,
  like "*=" operator. Are @italic(very very slightly) faster.

  @groupBegin }
function VectorScale(const v1: TVector2; const Scalar: TScalar): TVector2; overload;
function VectorScale(const v1: TVector3; const Scalar: TScalar): TVector3; overload;
function VectorScale(const v1: TVector4; const Scalar: TScalar): TVector4; overload;
procedure VectorScaleVar(var v1: TVector2; const Scalar: TScalar); overload;
procedure VectorScaleVar(var v1: TVector3; const Scalar: TScalar); overload;
procedure VectorScaleVar(var v1: TVector4; const Scalar: TScalar); overload;
{ @groupEnd }

{ Negate vector (return -V).

  Versions *Var scale place result back into the 1st vector.
  Are @italic(very very slightly) faster.

  @groupBegin }
function VectorNegate(const v: TVector2): TVector2; overload;
function VectorNegate(const v: TVector3): TVector3; overload;
function VectorNegate(const v: TVector4): TVector4; overload;
procedure VectorNegateVar(var v: TVector2); overload;
procedure VectorNegateVar(var v: TVector3); overload;
procedure VectorNegateVar(var v: TVector4); overload;
{ @groupEnd }

{ Scale vector such that it has given length (VecLen).
  Given VecLen may be negative, then we'll additionally negate the vector.
  @groupBegin }
function VectorAdjustToLength(const v: TVector2; VecLen: TScalar): TVector2; overload;
function VectorAdjustToLength(const v: TVector3; VecLen: TScalar): TVector3; overload;
procedure VectorAdjustToLengthVar(var v: TVector2; VecLen: TScalar); overload;
procedure VectorAdjustToLengthVar(var v: TVector3; VecLen: TScalar); overload;
{ @groupEnd }

{ @groupEnd }

{ Vector length.
  @groupBegin }
function VectorLen(const v: TVector2): TScalar; overload;
function VectorLen(const v: TVector3): TScalar; overload;
function VectorLen(const v: TVector4): TScalar; overload;
{ @groupEnd }

{ Vector length squared.

  This is slightly faster than calculating actual vector length,
  as it avoids doing expensive Sqrt. In many cases, you can
  operate on such squared vector length, and thus you gain some speed.
  For example, to check if vector is longer than 10,
  check @code(VectorLenSqr(V) > 100) instead of @code(VectorLen(V) > 10).

  Also note that when you have a vector with discrete values
  (like TVector3Byte), VectorLenSqr returns a precide integer
  value, while VectorLen must return floating-point value. }
function VectorLenSqr(const v: TVector2): TScalar; overload;
function VectorLenSqr(const v: TVector3): TScalar; overload;
function VectorLenSqr(const v: TVector4): TScalar; overload;

{ Vector cross product.

  This is a vector orthogonal to both given vectors.
  Generally there are two such vectors, this function returns
  the one following right-hand rule. More precisely, V1, V2 and
  VectorProduct(V1, V2) are in the same relation as basic X, Y, Z
  axes. Reverse the order of arguments to get negated result.

  If you use this to calculate a normal vector of a triangle
  (P0, P1, P2): note that @code(VectorProduct(P1 - P0, P1 - P2))
  points out from CCW triangle side in right-handed coordinate system.

  When V1 and V2 are parallel (that is, when V1 = V2 multiplied by some scalar),
  and this includes the case when one of them is zero,
  then result is a zero vector.

  See http://en.wikipedia.org/wiki/Cross_product
  @groupBegin }
function VectorProduct(const V1, V2: TVector3): TVector3; overload;
{ @groupEnd }

{ Dot product (aka scalar product) of two vectors.

  Overloaded versions that take as one argument 3-component vector and
  as the second argument 4-component vector: they simply behave like
  the missing 4th component would be equal 1.0. This is useful when
  V1 is a 3D point and V2 is something like plane equation.

  @groupBegin }
function VectorDotProduct(const V1, V2: TVector2): TScalar; overload;
function VectorDotProduct(const V1, V2: TVector3): TScalar; overload;
function VectorDotProduct(const V1, V2: TVector4): TScalar; overload;
function VectorDotProduct(const v1: TVector3; const v2: TVector4): TScalar; overload;
{ @groupEnd }

{ Multiply two vectors component-wise.
  That is, Result[I] := V1[I] * V2[I] for each I.

  @groupBegin }
function VectorMultiplyComponents(const V1, V2: TVector3): TVector3; overload;
procedure VectorMultiplyComponentsVar(var v1: TVector3; const v2: TVector3); overload;
{ @groupEnd }

{ Change each vector component into Power(component, Exp).
  @raises(EInvalidArgument When some component is < 0 and Exp <> 0.
    Version VectorPowerComponentsVar leaves the V in undefined state
    in case of such exception.) }
function VectorPowerComponents(const v: TVector3; const Exp: TScalar): TVector3; overload;
procedure VectorPowerComponentsVar(var v: TVector3; const Exp: TScalar); overload;

{ Cosinus of angle between two vectors.

  CosAngleBetweenNormals is a little faster, but must receive
  normalized (length 1) vectors. This avoids expensive Sqrt
  inside CosAngleBetweenVectors.

  @raises EVectorInvalidOp If V1 or V2 is zero.
  @groupBegin }
function CosAngleBetweenVectors(const V1, V2: TVector3): TScalar; overload;
function CosAngleBetweenNormals(const V1, V2: TVector3): TScalar; overload;
{ @groupEnd }

{ Angle between two vectors, in radians.
  Returns always positive angle, between 0 and Pi.

  AngleRadBetweenNormals is a little faster, but must receive
  normalized (length 1) vectors. This avoids expensive Sqrt.
  See also CosAngleBetweenVectors and CosAngleBetweenNormals
  to avoid expensive ArcCos.

  @raises EVectorInvalidOp If V1 or V2 is zero.
  @groupBegin }
function AngleRadBetweenVectors(const V1, V2: TVector3): TScalar; overload;
function AngleRadBetweenNormals(const V1, V2: TVector3): TScalar; overload;
{ @groupEnd }

{ Signed angle between two vectors, in radians.
  As opposed to AngleRadBetweenNormals, this returns a signed angle,
  between -Pi and Pi. This is guaranteed to be such angle that rotating
  V1 around vector cross product (V1 x V2) will produce V2.
  As you see, the order or arguments is important (just like it's important
  for vector cross).

  Overloaded versions with Cross argument assume the rotation is done around
  given Cross vector, which @italic(must) be a cross product or it's negation
  (in other words, it must be orthogonal to both vectors).

  @raises EVectorInvalidOp If V1 or V2 is zero.
  @groupBegin }
function RotationAngleRadBetweenVectors(const V1, V2: TVector3): TScalar; overload;
function RotationAngleRadBetweenVectors(const V1, V2, Cross: TVector3): TScalar; overload;
{ @groupEnd }

{ Rotate point Point around the Axis by given Angle.
  Axis cannot be zero.

  Note that this is equivalent to constructing a rotation matrix
  and then using it, like

  @longCode(#
    M := RotationMatrixDeg(Angle, Axis);
    Result := MatrixMultPoint(M, Point);
  #)

  Except this will be a little faster. So rotations are done in the
  same direction as RotationMatrixDeg, and as OpenGL.
  @groupBegin }
function RotatePointAroundAxisDeg(Angle: TScalar; const Point: TVector3; const Axis: TVector3): TVector3; overload;
function RotatePointAroundAxisRad(Angle: TScalar; const Point: TVector3; const Axis: TVector3): TVector3; overload;
{ @groupEnd }

{ Which coordinate (0, 1, 2, and eventually 3 for 4D versions) is the largest.
  When the vector components are equal, the first one "wins", for example
  if V[0] = V[1] (and are larger than other vector component) we return 0.
  MaxAbsVectorCoord compares the absolute value of components.
  @groupBegin }
function MaxVectorCoord(const v: TVector2): integer; overload;
function MaxVectorCoord(const v: TVector3): integer; overload;
function MaxVectorCoord(const v: TVector4): integer; overload;
function MaxAbsVectorCoord(const v: TVector2): integer; overload;
function MaxAbsVectorCoord(const v: TVector3): integer; overload;
{ @groupEnd }

function VectorAbs(const V: TVector2): TVector2;
function VectorAbs(const V: TVector3): TVector3;
function VectorAbs(const V: TVector4): TVector4;

function MinVectorCoord(const v: TVector3): integer; overload;

procedure SortAbsVectorCoord(const v: TVector3; out Max, Middle, Min: Integer); overload;

{ Vector orthogonal to plane and pointing in the given direction.

  Given a plane equation (or just the first 3 components of this equation),
  we have vector orthogonal to the plane (just the first 3 components of plane
  equation). This returns either this vector, or it's negation.
  It chooses the one that points in the same 3D half-space as given Direction.

  When given Direction is paralell to Plane, returns original
  plane direction, not it's negation.

  This really simply returns the first 3 components of plane equation.
  possibly negated. So e.g. if the plane direction was normalized, result
  is normalized too.

  PlaneDirNotInDirection chooses the direction opposite to given Direction
  parameter. So it's like @code(PlaneDirInDirection(Plane, -Direction)).

  @groupBegin }
function PlaneDirInDirection(const Plane: TVector4; const Direction: TVector3): TVector3; overload;
function PlaneDirInDirection(const PlaneDir, Direction: TVector3): TVector3; overload;
function PlaneDirNotInDirection(const Plane: TVector4; const Direction: TVector3): TVector3; overload;
function PlaneDirNotInDirection(const PlaneDir, Direction: TVector3): TVector3; overload;
{ @groupEnd }

{ Endianess utility functions for vectors  ----------------------------------- }

function SwapEndian(const V: TVector2): TVector2; overload;
function SwapEndian(const V: TVector3): TVector3; overload;
function SwapEndian(const V: TVector4): TVector4; overload;

function LEtoN(const V: TVector2): TVector2; overload;
function LEtoN(const V: TVector3): TVector3; overload;
function LEtoN(const V: TVector4): TVector4; overload;

function BEtoN(const V: TVector2): TVector2; overload;
function BEtoN(const V: TVector3): TVector3; overload;
function BEtoN(const V: TVector4): TVector4; overload;

function NtoLE(const V: TVector2): TVector2; overload;
function NtoLE(const V: TVector3): TVector3; overload;
function NtoLE(const V: TVector4): TVector4; overload;

function NtoBE(const V: TVector2): TVector2; overload;
function NtoBE(const V: TVector3): TVector3; overload;
function NtoBE(const V: TVector4): TVector4; overload;

{ Simple vectors operations  ------------------------------------------------- }

{ }
procedure SwapValues(var V1, V2: TVector2); overload;
procedure SwapValues(var V1, V2: TVector3); overload;
procedure SwapValues(var V1, V2: TVector4); overload;

function VectorAverage(const V: TVector3): TScalar; overload;

{ Overload := operator to allow conversion between
  Matrix unit objects and this unit's arrays easy. }
operator := (const V: TVector2_): TVector2;
operator := (const V: TVector3_): TVector3;
operator := (const V: TVector4_): TVector4;
operator := (const V: TVector2): TVector2_;
operator := (const V: TVector3): TVector3_;
operator := (const V: TVector4): TVector4_;

{ Linear interpolation between two vector values.
  Returns (1-A) * V1 + A * V2 (well, calculated a little differently for speed).
  So A = 0 gives V1, A = 1 gives V2, and values between and around are
  interpolated.

  @groupBegin }
function Lerp(const A: TScalar; const V1, V2: TVector2): TVector2; overload;
function Lerp(const A: TScalar; const V1, V2: TVector3): TVector3; overload;
function Lerp(const A: TScalar; const V1, V2: TVector4): TVector4; overload;
function Lerp(const A: TScalar; const M1, M2: TMatrix3): TMatrix3; overload;
function Lerp(const A: TScalar; const M1, M2: TMatrix4): TMatrix4; overload;
{ @groupEnd }

function Vector_Init_Lerp(const A: TScalar; const V1, V2: TVector3_): TVector3_; overload;
function Vector_Init_Lerp(const A: TScalar; const V1, V2: TVector4_): TVector4_; overload;

procedure NormalizeVar(var v: TVector2); overload;
procedure NormalizeVar(var v: TVector3); overload;

function Normalized(const v: TVector2): TVector2; overload;
function Normalized(const v: TVector3): TVector3; overload;

function Vector_Get_Normalized(const V: TVector3_): TVector3_; overload;

procedure Vector_Normalize(var V: TVector3_); overload;

{ This normalizes Plane by scaling all @italic(four) coordinates of Plane
  so that length of plane vector (taken from 1st @italic(three) coordinates)
  is one.

  Also, contrary to normal NormalizeVar on 3-component vectors,
  this will fail with some awful error (like floating point overflow)
  in case length of plane vector is zero. That's because we know
  that plane vector @italic(must) be always non-zero. }
procedure NormalizePlaneVar(var v: TVector4); overload;

function ZeroVector(const v: TVector2): boolean; overload;
function ZeroVector(const v: TVector3): boolean; overload;
function ZeroVector(const v: TVector4): boolean; overload;

function ZeroVector(const v: TVector2; const EqualityEpsilon: TScalar): boolean; overload;
function ZeroVector(const v: TVector3; const EqualityEpsilon: TScalar): boolean; overload;
function ZeroVector(const v: TVector4; const EqualityEpsilon: TScalar): boolean; overload;

function PerfectlyZeroVector(const v: TVector2): boolean; overload;
function PerfectlyZeroVector(const v: TVector3): boolean; overload;
function PerfectlyZeroVector(const v: TVector4): boolean; overload;

{ Intersection of two 3D planes.
  @raises EPlanesParallel If planes are parallel.
  @groupBegin }
procedure TwoPlanesIntersectionLine(const Plane0, Plane1: TVector4;
  out Line0, LineVector: TVector3); overload;
{ @groupEnd }

{ Intersection of two 2D lines.
  2D lines are expressed here as a vector of three values (A,B,C),
  such that Ax+By+C=0 is true for points on the line.
  @raises ELinesParallel if lines parallel
  @groupBegin }
function Lines2DIntersection(const Line0, Line1: TVector3): TVector2; overload;
{ @groupEnd }

{ Intersection of three 3D planes, results in a single 3D point.
  If the intersection is not a single 3D point, result is undefined,
  so don't try to use this.
  @groupBegin }
function ThreePlanesIntersectionPoint(
  const Plane0, Plane1, Plane2: TVector4): TVector3; overload;
{ @groupEnd }

{ Move a plane by a specifed vector.
  The first three plane numbers (plane normal vector) don't change
  (so, in particular, if you used the plane to define the half-space,
  the half-space gets moved as it should).

  PlaneAntiMove work like PlaneMove, but they translate by negated Move
  So it's like PlaneAntiMove(Plane, V) := PlaneMove(Plane, -V),
  but (very slightly) faster.

  This works Ok with invalid planes (1st three components = 0),
  that is after the move the plane remains invalid (1st three components
  remain = 0).

  @groupBegin }
function PlaneMove(const Plane: TVector4;
  const Move: TVector3): TVector4; overload;

procedure PlaneMoveVar(var Plane: TVector4; const Move: TVector3); overload;

function PlaneAntiMove(const Plane: TVector4;
  const Move: TVector3): TVector4; overload;
{ @groupEnd }

{ Check if both directions indicate the same side of given 3D plane.
  If one direction is parallel to the plane, also returns @true.
  You can specify only the first 3 components of plane equation (PlaneDir),
  since the 4th component would be ignored anyway.
  @groupBegin }
function VectorsSamePlaneDirections(const V1, V2: TVector3; const Plane: TVector4): boolean; overload;
function VectorsSamePlaneDirections(const V1, V2: TVector3; const PlaneDir: TVector3): boolean; overload;
{ @groupEnd }

{ Check if both points are on the same side of given 3D plane.
  If one of the points is exactly on the plane, also returns @true.
  @groupBegin }
function PointsSamePlaneSides(const p1, p2: TVector3; const Plane: TVector4): boolean; overload;
{ @groupEnd }

function PointsDistance(const V1, V2: TVector2): TScalar; overload;
function PointsDistance(const V1, V2: TVector3): TScalar; overload;
function PointsDistanceSqr(const V1, V2: TVector2): TScalar; overload;
function PointsDistanceSqr(const V1, V2: TVector3): TScalar; overload;

{ Distance between points projected on the 2D plane.
  Projection is done by rejecting IgnoreIndex coordinate (must be 0, 1 or 2).
  @groupBegin }
function PointsDistance2DSqr(const V1, V2: TVector3; const IgnoreIndex: Integer): TScalar; overload;
{ @groupEnd }

{ Compare two vectors, with epsilon to tolerate slightly different floats.
  Uses SingleEqualityEpsilon or DoubleEqualityEpsilon (depending on the parameters),
  just like FloatsEqual.

  Note that the case when EqualityEpsilon (or TScalarEqualityEpsilon
  or DoubleEqualityEpsilon) is exactly 0 is optimized here,
  just like VectorsPerfectlyEqual.

  @seealso VectorsPerfectlyEqual

  @groupBegin }
function VectorsEqual(const V1, V2: TVector2): boolean; overload;
function VectorsEqual(const V1, V2: TVector2; const EqualityEpsilon: TScalar): boolean; overload;
function VectorsEqual(const V1, V2: TVector3): boolean; overload;
function VectorsEqual(const V1, V2: TVector3; const EqualityEpsilon: TScalar): boolean; overload;
function VectorsEqual(const V1, V2: TVector4): boolean; overload;
function VectorsEqual(const V1, V2: TVector4; const EqualityEpsilon: TScalar): boolean; overload;
{ @groupEnd }

{ Compare two vectors using perfect comparison, that is using the "=" operator
  to compare floats.
  @seealso VectorsEqual
  @groupBegin }
function VectorsPerfectlyEqual(const V1, V2: TVector2): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector3): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector4): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
{ @groupEnd }

function MatricesEqual(const M1, M2: TMatrix3; const EqualityEpsilon: TScalar): boolean; overload;
function MatricesEqual(const M1, M2: TMatrix4; const EqualityEpsilon: TScalar): boolean; overload;

function MatricesPerfectlyEqual(const M1, M2: TMatrix3): boolean; overload;
function MatricesPerfectlyEqual(const M1, M2: TMatrix4): boolean; overload;

function VectorsPerp(const V1, V2: TVector3): boolean; overload;

{ Are the two vectors parallel (one is a scaled version of another).
  In particular, if one of the vectors is zero, then this is @true.
  @groupBegin }
function VectorsParallel(const V1, V2: TVector3): boolean; overload;
{ @groupEnd }

{ Adjust the V1 vector to force given angle between V1 and V2.
  Vector V1 will be adjusted, such that it has the same length
  and the 3D plane defined by V1, V2 and (0, 0, 0) is the same.

  When vectors are parallel (this includes the case when one of them is zero),
  we set V1 to ResultWhenParallel.

  We make it such that V1 rotated around axis VectorProduct(V1, V2) by given
  angle will result in V2. Note that this means that
  @code(MakeVectorsAngleRadOnTheirPlane(V1, V2, Angle, ...))
  results in the same (not reversed) relation between vectors as
  @code(MakeVectorsAngleRadOnTheirPlane(V2, V1, Angle, ...)).
  That's because you change the arguments order, but also VectorProduct
  sign changes.
  @groupBegin }
procedure MakeVectorsAngleRadOnTheirPlane(var v1: TVector3;
  const v2: TVector3; const AngleRad: TScalar;
  const ResultWhenParallel: TVector3); overload;
{ @groupEnd }

{ Adjust the V1 vector to force V1 and V2 to be orthogonal.
  When vectors are parallel, we set V1 to be AnyOrthogonalVector(V2). }
procedure MakeVectorsOrthoOnTheirPlane(var v1: TVector3;
  const v2: TVector3); overload;

{ Return, deterministically, some vector orthogonal to V.
  When V is non-zero, then the result is non-zero.
  @groupBegin }
function AnyOrthogonalVector(const v: TVector2): TVector2; overload;
function AnyOrthogonalVector(const v: TVector3): TVector3; overload;
{ @groupEnd }

function IsLineParallelToPlane(const lineVector: TVector3; const plane: TVector4): boolean; overload;

function IsLineParallelToSimplePlane(const lineVector: TVector3;
  const PlaneConstCoord: integer): boolean; overload;

{ Assuming that Vector1 and Vector2 are parallel,
  check do they point in the same direction.

  This assumes that both vectors are non-zero.
  If one of the vectors is zero, the result is undefined --- false or true.
  (but the function will surely not raise some floating point error etc.) }
function AreParallelVectorsSameDirection(
  const Vector1, Vector2: TVector3): boolean; overload;

{ Orthogonally project a point on a plane, that is find a closest
  point to Point lying on a Plane.
  @groupBegin }
function PointOnPlaneClosestToPoint(const plane: TVector4; const point: TVector3): TVector3; overload;
{ @groupEnd }

function PointToPlaneDistanceSqr(const Point: TVector3;
  const Plane: TVector4): TScalar; overload;

{ Distance from a point to a plane (with already normalized direction).

  Note: distance of the plane from origin point (0,0,0) may be simply
  obtained by Abs(Plane[3]) when Plane is Normalized.
  @groupBegin }
function PointToNormalizedPlaneDistance(const Point: TVector3;
  const Plane: TVector4): TScalar; overload;
{ @groupEnd }

{ Distance from a point to a plane.

  Note that calculating this costs you one Sqrt
  (contrary to PointToPlaneDistanceSqr or
  PointToNormalizedPlaneDistance).

  @groupBegin }
function PointToPlaneDistance(const Point: TVector3;
  const Plane: TVector4): TScalar; overload;
{ @groupEnd }

function PointToSimplePlaneDistance(const point: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar): TScalar; overload;

function PointOnLineClosestToPoint(const line0, lineVector, point: TVector2): TVector2; overload;
function PointOnLineClosestToPoint(const line0, lineVector, point: TVector3): TVector3; overload;

function PointToLineDistanceSqr(const point, line0, lineVector: TVector3): TScalar; overload;

{ Plane and line intersection.

  Returns @false and doesn't modify Intersection or T when
  the line is parallel to the plane (this includes the case when
  the line @italic(lies on a plane), so theoretically the whole
  line is an intersection).

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = Line0 + LineVector * T).
  @groupBegin }
function TryPlaneLineIntersection(out intersection: TVector3;
  const plane: TVector4; const line0, lineVector: TVector3): boolean; overload;
function TryPlaneLineIntersection(out t: TScalar;
  const plane: TVector4; const line0, lineVector: TVector3): boolean; overload;
{ @groupEnd }

{ Plane and ray intersection.

  Returns @false and doesn't modify Intersection or T when
  the ray is parallel to the plane (this includes the case when
  the ray @italic(lies on a plane). Also returns @false when the ray would
  have to point in the opposite direction to hit the plane.

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = RayOrigin + RayDirection * T).
  @groupBegin }
function TrySimplePlaneRayIntersection(out Intersection: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
function TrySimplePlaneRayIntersection(out Intersection: TVector3; out T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
function TrySimplePlaneRayIntersection(out T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean; overload;

function TryPlaneRayIntersection(out Intersection: TVector3;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean; overload;
function TryPlaneRayIntersection(out Intersection: TVector3; out T: TScalar;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean; overload;
{ @groupEnd }

{ Plane and line segment intersection.

  Returns @false and doesn't modify Intersection or T when
  the segment is parallel to the plane (this includes the case when
  the segment @italic(lies on a plane). Also returns @false when the segment
  would have to be longer to hit the plane.

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = RayOrigin + RayDirection * T).
  @groupBegin }
function TrySimplePlaneSegmentDirIntersection(var Intersection: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Segment0, SegmentVector: TVector3): boolean; overload;
function TrySimplePlaneSegmentDirIntersection(var Intersection: TVector3; var T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Segment0, SegmentVector: TVector3): boolean; overload;
function TrySimplePlaneSegmentDirIntersection(var T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Segment0, SegmentVector: TVector3): boolean; overload;

function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Pos1, Pos2: TVector3): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3; out T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Pos1, Pos2: TVector3): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out T: TScalar;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar;
  const Pos1, Pos2: TVector3): boolean; overload;

function TryPlaneSegmentDirIntersection(out Intersection: TVector3;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean; overload;
function TryPlaneSegmentDirIntersection(out Intersection: TVector3; out T: TScalar;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean; overload;
{ @groupEnd }

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector2): boolean; overload;
function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector3): boolean; overload;

{ Line passing through two @italic(different) points.
  When the points are equal, undefined.
  @groupBegin }
function LineOfTwoDifferentPoints2d(const p1, p2: TVector2): TVector3; overload;
{ @groupEnd }

function PointToSegmentDistanceSqr(const point, pos1, pos2: TVector3): TScalar; overload;

{ Transform plane by a matrix.

  @raises(ETransformedResultInvalid Raised when matrix
  will transform some point to a direction, or direction to point,
  in homogeneous coordinates.)

  @groupBegin }
function PlaneTransform(const Plane: TVector4; const Matrix: TMatrix4): TVector4;
{ @groupEnd }

function IsTunnelSphereCollision(const Tunnel1, Tunnel2: TVector3;
  const TunnelRadius: TScalar; const SphereCenter: TVector3;
  const SphereRadius: TScalar): boolean; overload;

function IsSpheresCollision(const Sphere1Center: TVector3; const Sphere1Radius: TScalar;
  const Sphere2Center: TVector3; const Sphere2Radius: TScalar): boolean; overload;

function IsSegmentSphereCollision(const pos1, pos2, SphereCenter: TVector3;
  const SphereRadius: TScalar): boolean; overload;

function TrySphereRayIntersection(out Intersection: TVector3;
  const SphereCenter: TVector3; const SphereRadius: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean; overload;

{ Intersection between an (infinitely tall) cylinder and a ray.
  @groupBegin }
function TryCylinderRayIntersection(out Intersection: TVector3;
  const CylinderAxisOrigin, CylinderAxis: TVector3;
  const CylinderRadius: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
{ @groupEnd }

{ Convert to string using a nice (human-readable) format.
  These functions (named "ToNiceStr") use by default a nice (readable by humans)
  float representation, which is by default just a decimal representation
  with 2 digits after the dot, like "123.45".

  This representation is actually configurable using global FloatNiceFormat.

  These functions (named "ToNiceStr") also sometimes add some decoration (like
  "[ ]" characters around matrix rows) to make the result look nice
  and readable.

  Overloaded versions that take a LineIndent parameter (may) output a multiline-string.
  In such case, the last line is @italic(never) terminated with newline
  character(s).

  @groupBegin }
function FloatToNiceStr(f: TScalar): string; overload;
function VectorToNiceStr(const v: array of TScalar): string; overload;
function MatrixToNiceStr(const v: TMatrix4; const LineIndent: string): string; overload;
{ @groupEnd }

{ Convert to string using the most precise (not always easily readable by humans) format.
  These functions (called "ToRawStr") format floats using the most precise format,
  often using the exponential (scientific) notation if needed.

  They are suitable for storing the floating-point value in a file,
  with a best precision possible.

  Also, these functions (called "ToRawStr") do not add any decoration when outputting
  vectors / matrices. They simply spit a sequence of floating-point values
  separated by spaces.

  @groupBegin }
function FloatToRawStr(f: TScalar): string; overload;
function VectorToRawStr(const v: array of TScalar): string; overload;
function MatrixToRawStr(const v: TMatrix4; const LineIndent: string): string; overload;
{ @groupEnd }

{ Matrix operations ---------------------------------------------------------- }

{ }
function MatrixAdd(const m1, m2: TMatrix3): TMatrix3; overload;
function MatrixAdd(const m1, m2: TMatrix4): TMatrix4; overload;

procedure MatrixAddVar(var m1: TMatrix3; const m2: TMatrix3); overload;
procedure MatrixAddVar(var m1: TMatrix4; const m2: TMatrix4); overload;

function MatrixSubtract(const m1, m2: TMatrix3): TMatrix3; overload;
function MatrixSubtract(const m1, m2: TMatrix4): TMatrix4; overload;

procedure MatrixSubtractVar(var m1: TMatrix3; const m2: TMatrix3); overload;
procedure MatrixSubtractVar(var m1: TMatrix4; const m2: TMatrix4); overload;

function MatrixNegate(const m1: TMatrix3): TMatrix3; overload;
function MatrixNegate(const m1: TMatrix4): TMatrix4; overload;

function MatrixMultScalar(const m: TMatrix3; const s: TScalar): TMatrix3; overload;
function MatrixMultScalar(const m: TMatrix4; const s: TScalar): TMatrix4; overload;

{ Transform a 3D or 2D point with 4x4 matrix.

  This works by temporarily converting point to 4-component vector
  (4th component is 1). After multiplying matrix * vector we divide
  by 4th component. So this works Ok for all matrices,
  even with last row different than identity (0, 0, 0, 1).
  E.g. this works for projection matrices too.

  @raises(ETransformedResultInvalid This is raised when matrix
  will transform point to a direction (vector with 4th component
  equal zero). In this case we just cannot interpret the result as a point.)

  @groupBegin }
function MatrixMultPoint(const m: TMatrix4; const pt: TVector3): TVector3; overload;
function MatrixMultPoint(const m: TMatrix4; const pt: TVector2): TVector2; overload;
{ @groupEnd }

{ Transform a 3D or 2D direction with 4x4 matrix.

  This works by temporarily converting direction to 4-component vector
  (4th component is 0). After multiplying matrix * vector we check
  is the 4th component still 0 (eventually raising ETransformedResultInvalid).

  @raises(ETransformedResultInvalid This is raised when matrix
  will transform direction to a point (vector with 4th component
  nonzero). In this case we just cannot interpret the result as a direction.)

  @groupBegin }
function MatrixMultDirection(const m: TMatrix4; const Dir: TVector3): TVector3; overload;
function MatrixMultDirection(const m: TMatrix4; const Dir: TVector2): TVector2; overload;
{ @groupEnd }

function MatrixMultVector(const m: TMatrix2; const v: TVector2): TVector2; overload;
function MatrixMultVector(const m: TMatrix3; const v: TVector3): TVector3; overload;
function MatrixMultVector(const m: TMatrix4; const v: TVector4): TVector4; overload;

operator * (const m: Tmatrix2; const v: Tvector2): Tvector2;
operator * (const m: Tmatrix3; const v: Tvector3): Tvector3;
operator * (const m: Tmatrix4; const v: Tvector4): Tvector4;

function MatrixMult(const m1, m2: TMatrix2): TMatrix2; overload;
function MatrixMult(const m1, m2: TMatrix3): TMatrix3; overload;
function MatrixMult(const m1, m2: TMatrix4): TMatrix4; overload;

operator * (const m1, m2: TMatrix2): TMatrix2;
operator * (const m1, m2: TMatrix3): TMatrix3;
operator * (const m1, m2: TMatrix4): TMatrix4;

function MatrixRow(const m: TMatrix2; const Row: Integer): TVector2; overload;
function MatrixRow(const m: TMatrix3; const Row: Integer): TVector3; overload;
function MatrixRow(const m: TMatrix4; const Row: Integer): TVector4; overload;

function MatrixDeterminant(const M: TMatrix2): TScalar; overload;
function MatrixDeterminant(const M: TMatrix3): TScalar; overload;
function MatrixDeterminant(const M: TMatrix4): TScalar; overload;

{ Inverse the matrix.

  They do division by Determinant internally, so will raise exception
  from this float division if the matrix is not reversible.

  @groupBegin }
function MatrixInverse(const M: TMatrix2; const Determinant: TScalar): TMatrix2; overload;
function MatrixInverse(const M: TMatrix3; const Determinant: TScalar): TMatrix3; overload;
function MatrixInverse(const M: TMatrix4; const Determinant: TScalar): TMatrix4; overload;
{ @groupEnd }

{ Transpose the matrix.
  @groupBegin }
procedure MatrixTransposeVar(var M: TMatrix3); overload;

function MatrixTranspose(const M: TMatrix3): TMatrix3; overload;
function MatrixTranspose(const M: TMatrix4): TMatrix4; overload;
{ @groupEnd }

{ Functions to create common 4x4 matrices used in 3D graphics.

  These functions generate the same matrices that are made by corresponding
  OpenGL (gl or glu) functions. So rotations will be generated in the same
  fashion, etc. For exact specification of what matrices they create see
  OpenGL specification for routines glTranslate, glScale, glRotate.

  Functions named Matrices below generate both normal and inverted matrices.
  For example, function RotationMatrices returns two matrices that you
  could calculate separately by

  @longCode(#
          Matrix: = RotationMatrix( Angle, Axis);
  InvertedMatrix: = RotationMatrix(-Angle, Axis);
  #)

  This is useful sometimes, and generating them both at the same time
  allows for some speedup (for example, calling RotationMatrix twice will
  calculate sincos of Angle twice).

  Note that inverse of scaling matrix will not exist if the
  ScaleFactor has one of the components zero !
  Depending on InvertedMatrixIdentityIfNotExists, this will
  (if @false) raise division by zero exception or (if @true) cause
  the matrix to be set to identity.

  Note that rotation matrix (both normal and inverse) is always defined,
  for Axis = zero both normal and inverse matrices are set to identity.

  @groupBegin }
function TranslationMatrix(const X, Y, Z: TScalar): TMatrix4Single; overload;
function TranslationMatrix(const Transl: TVector3): TMatrix4Single; overload;

procedure TranslationMatrices(const X, Y, Z: TScalar; out Matrix, InvertedMatrix: TMatrix4Single); overload;
procedure TranslationMatrices(const Transl: TVector3; out Matrix, InvertedMatrix: TMatrix4Single); overload;

{ Multiply matrix M by translation matrix.

  This is equivalent to M := MatrixMult(M, TranslationMatrix(Transl)),
  but it works much faster since TranslationMatrix is a very simple matrix
  and multiplication by it may be much optimized.

  An additional speedup comes from the fact that the result is placed
  back in M (so on places where M doesn't change (and there's a lot
  of them for multiplication with translation matrix) there's no useless
  copying).

  MultMatricesTranslation is analogous to calculating both
  TranslationMatrix(Transl) and it's inverse, and then

  @longCode(#
    M := MatrixMult(M, translation);
    MInvert := MatrixMult(inverted translation, MInvert);
  #)

  The idea is that if M represented some translation, and MInvert it's
  inverse, then after MultMatricesTranslation this will still hold.

  @groupBegin }
procedure MultMatrixTranslation(var M: TMatrix4; const Transl: TVector3); overload;
procedure MultMatricesTranslation(var M, MInvert: TMatrix4; const Transl: TVector3); overload;
{ @groupEnd }

{ Transform coordinates to / from a coordinate system.
  Stuff multiplied by this matrix is supplied in other coordinate system.

  The "new" coordinate system (you specify it explicitly for
  TransformToCoordsMatrix) is the coordinate system in which your 3D stuff
  is defined. That is, when you supply the points (that will later be
  multiplied by TransformToCoordsMatrix) you think in the "new" coordinate
  system. The "old" coordinate system
  (you specify it explicitly for TransformFromCoordsMatrix)
  is the coordinate system of stuff @italic(after)
  it's multiplied by this matrix.

  This may get confusing, so to be more precise:

  @unorderedList(

    @item(
      TransformToCoordsMatrix says how the new coords system looks
      from the point of view of the old coords system.
      A stuff lying at (0, 0, 0) in new coord system will be seen
      at NewOrigin after the transformation (in the old coordinate system).
      Similarly, direction (0, 1, 0) will be seen as NewY after
      the transformation.)

    @item(
      TransformFromCoordsMatrix is the inverse: how the old system
      is seen from the new one. If before the transformation you are
      at OldOrigin, then after the transformation you are at (0, 0, 0).
      This is natural way to implement LookAtMatrix, LookDirMatrix.)
  )

  The lengths of directions (New or Old X, Y, Z vectors) are meaningful.
  These vectors correspond to unit vectors (1, 0, 0), (0, 1, 0) and (0, 0, 1)
  in the other coordinate system. Supplying here non-normalized vectors
  will result in scaling.

  You can use the "NoScale" versions to have the vectors automatically
  normalized, thus you waste a little time (on normalizing) but you
  avoid the scaling.

  Overloaded versions without OldOrigin / NewOrigin parameters
  work like the old/new origin is zero. IOW, the origin of the coordinate
  system doesn't change in this case.

  @groupBegin }
function TransformToCoordsMatrix(const
  NewX, NewY, NewZ: TVector3): TMatrix4Single; overload;
function TransformToCoordsMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4Single; overload;
function TransformToCoordsNoScaleMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4Single; overload;

function TransformFromCoordsMatrix(const
  OldX, OldY, OldZ: TVector3): TMatrix4Single; overload;
function TransformFromCoordsMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4Single; overload;
function TransformFromCoordsNoScaleMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4Single; overload;
{ @groupEnd }

{ Calculate matrix to convert to given coordinate system
  (like TransformToCoordsMatrix) and it's inverse
  (like TransformFromCoordsMatrix).

  @groupBegin }
procedure TransformCoordsMatrices(const NewX, NewY, NewZ: TVector3;
  out ToCoords, FromCoords: TMatrix4Single); overload;
{ @groupEnd }

{ Transform vector into new coordinate space.

  Equivalent to @code(MatrixMultPoint(TransformToCoordsMatrix(ZeroVector3Xxx,
  NewX, NewY, NewZ))). So the origin of new coordinate system is at the same
  place. You should pass NewX, NewY, NewZ vectors normalized if you want to
  preserve vector length.
  @groupBegin }
function TransformToCoords(const V, NewX, NewY, NewZ: TVector3): TVector3;
{ @groupEnd }

{ Camera matrix to look at the specified point (or along the specified direction).
  Work according to right-handed coordinate system.

  When applied to the scene, they transform it, such that a camera standing
  at (0, 0, 0) (with dir (0, 0, -1) and up vector (0, 1, 0)),
  was seeing the same view as if it was standing at Eye
  (with given Dir and Up vectors).

  For LookAtMatrix, looking direction is implicitly given as @code(Center - Eye).
  Just like gluLookAt.

  @unorderedList(
    @item(For the overloaded LookDirMatrix version with Side parameter,
      we assume that Dir, Side and Up are already normalized
      and orthogonal to each other.)

    @item(For the overloaded version without the Side parameter,
      Dir and Up do not have to normalized.
      We'll normalize them if needed, so their lengths do not affect the result
      (just as the distance between Center and Eye points for LookAtMatrix).

      Also, Dir and Up do not have to be perfectly orthogonal
      (we will eventually adjust Up internally to make it orthogonal to Up).

      You still must make sure that Dir and Up are not parallel.)
  )

  @groupBegin }
function LookAtMatrix(const Eye, Center, Up: TVector3): TMatrix4Single; overload;
function LookDirMatrix(const Eye, Dir, Up: TVector3): TMatrix4Single; overload;
function LookDirMatrix(const Eye, Dir, Side, Up: TVector3): TMatrix4Single; overload;
{ @groupEnd }

{ Calculate LookDirMatrix (or it's inverse), fast.

  Has some assumptions that make it run fast:
  @unorderedList(
    @item(It assumes camera position is zero.)
    @item(It assumes that Dir and Up are already normalized and orthogonal.)
  )

  @groupBegin
}
function FastLookDirMatrix(const Direction, Up: TVector3): TMatrix4Single;
function InverseFastLookDirMatrix(const Direction, Up: TVector3): TMatrix4Single;
{ @groupEnd }

{ Convert ModelView matrix to a Normal matrix, just like 3D graphic libraries do.
  See e.g. http://www.lighthouse3d.com/tutorials/glsl-tutorial/the-normal-matrix/
  for explanation why this is necessary, and how it's done. }
function ModelViewToNormalMatrix(const M: TMatrix4): TMatrix3;

{ Multiply vector by a transposition of the same vector.
  For 3d vectors, this results in a 3x3 matrix.
  To put this inside a 4x4 matrix,
  we fill the last row and column just like for an identity matrix.

  This is useful for calculating rotation matrix. }
function VectorMultTransposedSameVector(const v: TVector3): TMatrix4;

function ScalingMatrix(const ScaleFactor: TVector3): TMatrix4;

procedure ScalingMatrices(const ScaleFactor: TVector3;
  InvertedMatrixIdentityIfNotExists: boolean;
  out Matrix, InvertedMatrix: TMatrix4);

function RotationMatrixRad(const AngleRad: TScalar; const Axis: TVector3): TMatrix4; overload;
function RotationMatrixDeg(const AngleDeg: TScalar; const Axis: TVector3): TMatrix4; overload;
function RotationMatrixRad(const AngleRad: TScalar; const AxisX, AxisY, AxisZ: TScalar): TMatrix4; overload;
function RotationMatrixDeg(const AngleDeg: TScalar; const AxisX, AxisY, AxisZ: TScalar): TMatrix4; overload;

procedure RotationMatricesRad(const AngleRad: TScalar; const Axis: TVector3;
  out Matrix, InvertedMatrix: TMatrix4);
procedure RotationMatricesRad(const AxisAngle: TVector4;
  out Matrix, InvertedMatrix: TMatrix4);
{ @groupEnd }

function MatrixDet4x4(const mat: TMatrix4): TScalar;
function MatrixDet3x3(const a1, a2, a3, b1, b2, b3, c1, c2, c3: TScalar): TScalar;
function MatrixDet2x2(const a, b, c, d: TScalar): TScalar;

{$endif read_interface}

{$ifdef read_implementation}

function FloatsEqual(const f1, f2: TScalar): boolean;
begin
  if ScalarEqualityEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < ScalarEqualityEpsilon;
end;

function FloatsEqual(const f1, f2, EqEpsilon: TScalar): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < EqEpsilon
end;

function Zero(const f1: TScalar  ): boolean;
begin
  if ScalarEqualityEpsilon = 0 then
    Result := f1 = 0 else
    Result := Abs(f1)<  ScalarEqualityEpsilon
end;

function Zero(const f1, EqEpsilon: TScalar): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = 0 else
    result := Abs(f1) < EqEpsilon
end;

{$define VECTOR_OP_FUNCS:=
function VectorOpFuncName(const V1, V2: TVector2): TVector2;
begin
  Result[0] := V1[0] VectorOp V2[0];
  Result[1] := V1[1] VectorOp V2[1];
end;

function VectorOpFuncName(const V1, V2: TVector3): TVector3;
begin
  Result[0] := V1[0] VectorOp V2[0];
  Result[1] := V1[1] VectorOp V2[1];
  Result[2] := V1[2] VectorOp V2[2];
end;

function VectorOpFuncName(const V1, V2: TVector4): TVector4;
begin
  Result[0] := V1[0] VectorOp V2[0];
  Result[1] := V1[1] VectorOp V2[1];
  Result[2] := V1[2] VectorOp V2[2];
  Result[3] := V1[3] VectorOp V2[3];
end;

procedure VectorOpVarFuncName(var v1: TVector2; const v2: TVector2);
begin
  V1[0] VectorOpVar V2[0];
  V1[1] VectorOpVar V2[1];
end;

procedure VectorOpVarFuncName(var V1: TVector3; const V2: TVector3);
begin
  V1[0] VectorOpVar V2[0];
  V1[1] VectorOpVar V2[1];
  V1[2] VectorOpVar V2[2];
end;

procedure VectorOpVarFuncName(var v1: TVector4; const v2: TVector4);
begin
  V1[0] VectorOpVar V2[0];
  V1[1] VectorOpVar V2[1];
  V1[2] VectorOpVar V2[2];
  V1[3] VectorOpVar V2[3];
end;
}

  {$define VectorOpFuncName := VectorSubtract}
  {$define VectorOpVarFuncName := VectorSubtractVar}
  {$define VectorOp := -}
  {$define VectorOpVar := -=}
  VECTOR_OP_FUNCS

  {$define VectorOpFuncName := VectorAdd}
  {$define VectorOpVarFuncName := VectorAddVar}
  {$define VectorOp:= +}
  {$define VectorOpVar:= +=}
  VECTOR_OP_FUNCS

{$undef VectorOpFuncName}
{$undef VectorOpVarFuncName}
{$undef VectorOp}
{$undef VectorOpVar}
{$undef VECTOR_OP_FUNCS}

function VectorScale(const v1: TVector2; const Scalar: TScalar): TVector2;
begin
  Result[0] := V1[0] * Scalar;
  Result[1] := V1[1] * Scalar;
end;

function VectorScale(const v1: TVector3; const Scalar: TScalar): TVector3;
begin
  Result[0] := V1[0] * Scalar;
  Result[1] := V1[1] * Scalar;
  Result[2] := V1[2] * Scalar;
end;

function VectorScale(const v1: TVector4; const Scalar: TScalar): TVector4;
begin
  Result[0] := V1[0] * Scalar;
  Result[1] := V1[1] * Scalar;
  Result[2] := V1[2] * Scalar;
  Result[3] := V1[3] * Scalar;
end;

procedure VectorScaleVar(var v1: TVector2; const Scalar: TScalar);
begin
  V1[0] *= Scalar;
  V1[1] *= Scalar;
end;

procedure VectorScaleVar(var v1: TVector3; const Scalar: TScalar);
begin
  V1[0] *= Scalar;
  V1[1] *= Scalar;
  V1[2] *= Scalar;
end;

procedure VectorScaleVar(var v1: TVector4; const Scalar: TScalar);
begin
  V1[0] *= Scalar;
  V1[1] *= Scalar;
  V1[2] *= Scalar;
  V1[3] *= Scalar;
end;

function VectorNegate(const v: TVector2): TVector2;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
end;

function VectorNegate(const v: TVector3): TVector3;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
  Result[2] := -v[2];
end;

function VectorNegate(const v: TVector4): TVector4;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
  Result[2] := -v[2];
  Result[3] := -v[3];
end;

procedure VectorNegateVar(var v: TVector2);
begin
  v[0] := -v[0];
  v[1] := -v[1];
end;

procedure VectorNegateVar(var v: TVector3);
begin
  v[0] := -v[0];
  v[1] := -v[1];
  v[2] := -v[2];
end;

procedure VectorNegateVar(var v: TVector4);
begin
  v[0] := -v[0];
  v[1] := -v[1];
  v[2] := -v[2];
  v[3] := -v[3];
end;

function VectorProduct(const V1, V2: TVector3): TVector3;
begin
  Result[0] := V1[1]*V2[2] - V1[2]*V2[1];
  Result[1] := V1[2]*V2[0] - V1[0]*V2[2];
  Result[2] := V1[0]*V2[1] - V1[1]*V2[0];
end;

function VectorAbs(const V: TVector2): TVector2;
begin
  Result[0] := Abs(V[0]);
  Result[1] := Abs(V[1]);
end;

function VectorAbs(const V: TVector3): TVector3;
begin
  Result[0] := Abs(V[0]);
  Result[1] := Abs(V[1]);
  Result[2] := Abs(V[2]);
end;

function VectorAbs(const V: TVector4): TVector4;
begin
  Result[0] := Abs(V[0]);
  Result[1] := Abs(V[1]);
  Result[2] := Abs(V[2]);
  Result[3] := Abs(V[3]);
end;

function VectorDotProduct(const V1, V2: TVector2): TScalar;
begin
  result := V1[0]*V2[0]+ V1[1]*V2[1];
end;

function VectorDotProduct(const V1, V2: TVector3): TScalar;
begin
  result := V1[0]*V2[0]+ V1[1]*V2[1]+ V1[2]*V2[2];
end;

function VectorDotProduct(const V1, V2: TVector4): TScalar;
begin
  result := V1[0]*V2[0]+ V1[1]*V2[1]+ V1[2]*V2[2]+ V1[3]*V2[3];
end;

function VectorDotProduct(const V1: TVector3; const V2: TVector4): TScalar;
begin
  result := V1[0]*V2[0]+ V1[1]*V2[1]+ V1[2]*V2[2]+       V2[3];
end;

function VectorMultiplyComponents(const V1, V2: TVector3): TVector3;
begin
  Result[0] := V1[0] * V2[0];
  Result[1] := V1[1] * V2[1];
  Result[2] := V1[2] * V2[2];
end;

procedure VectorMultiplyComponentsVar(var V1: TVector3; const V2: TVector3);
begin
  V1[0] *= V2[0];
  V1[1] *= V2[1];
  V1[2] *= V2[2];
end;

function SwapEndian(const V: TVector2): TVector2;
begin
  Result[0] := SwapEndian(V[0]);
  Result[1] := SwapEndian(V[1]);
end;

function SwapEndian(const V: TVector3): TVector3;
begin
  Result[0] := SwapEndian(V[0]);
  Result[1] := SwapEndian(V[1]);
  Result[2] := SwapEndian(V[2]);
end;

function SwapEndian(const V: TVector4): TVector4;
begin
  Result[0] := SwapEndian(V[0]);
  Result[1] := SwapEndian(V[1]);
  Result[2] := SwapEndian(V[2]);
  Result[3] := SwapEndian(V[3]);
end;

function NtoLE(const V: TVector2): TVector2;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function NtoLE(const V: TVector3): TVector3;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function NtoLE(const V: TVector4): TVector4;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function NtoBE(const V: TVector2): TVector2;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function NtoBE(const V: TVector3): TVector3;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function NtoBE(const V: TVector4): TVector4;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function LEtoN(const V: TVector2): TVector2;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function LEtoN(const V: TVector3): TVector3;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function LEtoN(const V: TVector4): TVector4;
begin
{$ifdef ENDIAN_BIG}
  Result := SwapEndian(V);
{$else ENDIAN_BIG}
  Result := V;
{$endif ENDIAN_BIG}
end;

function BEtoN(const V: TVector2): TVector2;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function BEtoN(const V: TVector3): TVector3;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

function BEtoN(const V: TVector4): TVector4;
begin
{$ifdef ENDIAN_BIG}
  Result := V;
{$else ENDIAN_BIG}
  Result := SwapEndian(V);
{$endif ENDIAN_BIG}
end;

procedure SwapValues(var V1, V2: TVector2);
var
  Tmp: TVector2;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

procedure SwapValues(var V1, V2: TVector3);
var
  Tmp: TVector3;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

procedure SwapValues(var V1, V2: TVector4);
var
  Tmp: TVector4;
begin
  Tmp := V1;
  V1 := V2;
  V2 := Tmp;
end;

function VectorAverage(const V: TVector3): TScalar;
begin
  Result := (V[0] + V[1] + V[2]) / 3;
end;

operator := (const V: TVector2_): TVector2;
begin
  Result := V.Data;
end;

operator := (const V: TVector3_): TVector3;
begin
  Result := V.Data;
end;

operator := (const V: TVector4_): TVector4;
begin
  Result := V.Data;
end;

operator := (const V: TVector2): TVector2_;
begin
  Result.Data := V;
end;

operator := (const V: TVector3): TVector3_;
begin
  Result.Data := V;
end;

operator := (const V: TVector4): TVector4_;
begin
  Result.Data := V;
end;

function Vector_Init_Lerp(const A: TScalar; const V1, V2: TVector3_): TVector3_;
begin
  Result.Data[0] := V1.Data[0] + A * (V2.Data[0] - V1.Data[0]);
  Result.Data[1] := V1.Data[1] + A * (V2.Data[1] - V1.Data[1]);
  Result.Data[2] := V1.Data[2] + A * (V2.Data[2] - V1.Data[2]);
end;

function Vector_Init_Lerp(const A: TScalar; const V1, V2: TVector4_): TVector4_;
begin
  Result.Data[0] := V1.Data[0] + A * (V2.Data[0] - V1.Data[0]);
  Result.Data[1] := V1.Data[1] + A * (V2.Data[1] - V1.Data[1]);
  Result.Data[2] := V1.Data[2] + A * (V2.Data[2] - V1.Data[2]);
  Result.Data[3] := V1.Data[3] + A * (V2.Data[3] - V1.Data[3]);
end;

procedure NormalizeVar(var V: TVector2);
var
  len: TScalar;
begin
  len := Sqrt( Sqr(V[0]) + Sqr(V[1]) );
  if len = 0.0 then exit;
  V[0] /= len;
  V[1] /= len;
end;

procedure NormalizeVar(var V: TVector3);
var
  len: TScalar;
begin
  len := Sqrt( Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]) );
  if len = 0.0 then exit;
  V[0] /= len;
  V[1] /= len;
  V[2] /= len;
end;

procedure NormalizePlaneVar(var v: TVector4);
var
  len: TScalar;
begin
  len := Sqrt( Sqr(V[0]) + Sqr(V[1]) + Sqr(V[2]) );
  V[0] /= len;
  V[1] /= len;
  V[2] /= len;
  V[3] /= len;
end;

function ZeroVector(const V: TVector2): boolean;
begin
  result := (Abs(V[0]) < ScalarEqualityEpsilon) and
            (Abs(V[1]) < ScalarEqualityEpsilon);
end;

function ZeroVector(const V: TVector3): boolean;
begin
  result := (Abs(V[0]) < ScalarEqualityEpsilon) and
            (Abs(V[1]) < ScalarEqualityEpsilon) and
            (Abs(V[2]) < ScalarEqualityEpsilon);
end;

function ZeroVector(const V: TVector4): boolean;
begin
  result := (Abs(V[0]) < ScalarEqualityEpsilon) and
            (Abs(V[1]) < ScalarEqualityEpsilon) and
            (Abs(V[2]) < ScalarEqualityEpsilon) and
            (Abs(V[3]) < ScalarEqualityEpsilon);
end;

function ZeroVector(const V: TVector2; const EqualityEpsilon: TScalar): boolean;
begin
  result := (Abs(V[0]) < EqualityEpsilon) and
            (Abs(V[1]) < EqualityEpsilon);
end;

function ZeroVector(const V: TVector3; const EqualityEpsilon: TScalar): boolean;
begin
  result := (Abs(V[0]) < EqualityEpsilon) and
            (Abs(V[1]) < EqualityEpsilon) and
            (Abs(V[2]) < EqualityEpsilon);
end;

function ZeroVector(const V: TVector4; const EqualityEpsilon: TScalar): boolean;
begin
  result := (Abs(V[0]) < EqualityEpsilon) and
            (Abs(V[1]) < EqualityEpsilon) and
            (Abs(V[2]) < EqualityEpsilon) and
            (Abs(V[3]) < EqualityEpsilon);
end;

function PerfectlyZeroVector(const V: TVector2): boolean;
begin
  Result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function PerfectlyZeroVector(const V: TVector3): boolean;
begin
  Result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function PerfectlyZeroVector(const V: TVector4): boolean;
begin
  Result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function VectorAdjustToLength(const v: TVector2; VecLen: TScalar): TVector2;
begin
  result := VectorScale(v, VecLen/VectorLen(v));
end;

function VectorAdjustToLength(const v: TVector3; VecLen: TScalar): TVector3;
begin
  result := VectorScale(v, VecLen/VectorLen(v));
end;

procedure VectorAdjustToLengthVar(var v: TVector2; VecLen: TScalar);
begin
  VectorScaleVar(v, VecLen/VectorLen(v));
end;

procedure VectorAdjustToLengthVar(var v: TVector3; VecLen: TScalar);
begin
  VectorScaleVar(v, VecLen/VectorLen(v));
end;

function VectorLen(const v: TVector2): TScalar;
begin
  Result := Sqrt(VectorLenSqr(v));
end;

function VectorLenSqr(const v: TVector2): TScalar;
begin
  Result := Sqr(v[0]) + Sqr(v[1]);
end;

function VectorLen(const v: TVector3): TScalar;
begin
  Result := Sqrt(VectorLenSqr(v));
end;

function VectorLenSqr(const v: TVector3): TScalar;
begin
  Result := Sqr(v[0]) + Sqr(v[1]) + Sqr(v[2]);
end;

function VectorLen(const v: TVector4): TScalar;
begin
  Result := Sqrt(VectorLenSqr(v));
end;

function VectorLenSqr(const v: TVector4): TScalar;
begin
  Result := Sqr(v[0]) + Sqr(v[1]) + Sqr(v[2]) + Sqr(v[3]);
end;

function VectorPowerComponents(const v: TVector3; const Exp: TScalar): TVector3;
begin
  Result[0] := Power(v[0], Exp);
  Result[1] := Power(v[1], Exp);
  Result[2] := Power(v[2], Exp);
end;

procedure VectorPowerComponentsVar(var v: TVector3; const Exp: TScalar);
begin
  v[0] := Power(v[0], Exp);
  v[1] := Power(v[1], Exp);
  v[2] := Power(v[2], Exp);
end;

function CosAngleBetweenVectors(const V1, V2: TVector3): TScalar;
var
  LensSquared: Float;
begin
  (* jak widac, wykrecam sie jednym pierwiastkowaniem pierwiastkujac
     VectorLenSqr(v1) i VectorLenSqr(v2) jednoczesnie. *)

  LensSquared := VectorLenSqr(v1) * VectorLenSqr(v2);
  if LensSquared < ScalarEqualityEpsilon then
    raise EVectorInvalidOp.Create(
      'Cannot calculate angle between vectors, at least one of the vectors is zero');

  (* musimy robic tu Clamp do (-1, 1) bo praktyka pokazala ze czasami na skutek
     bledow obliczen zmiennoprzec. wynik tej funkcji jest maciupinke poza
     zakresem. A Cosinum musi byc w zakresie -1..1, w szczegolnosci
     ArcCos() dla czegos choc troche poza zakresem wywala paskudny EInvalidArgument *)
  result := Clamped(
    VectorDotProduct(V1, V2) / Sqrt(LensSquared), -1.0, 1.0);
end;

function AngleRadBetweenVectors(const V1, V2: TVector3): TScalar;
begin
  result := ArcCos(CosAngleBetweenVectors(V1, V2));
end;

function CosAngleBetweenNormals(const V1, V2: TVector3): TScalar;
begin
  result := Clamped(VectorDotProduct(V1, V2), -1.0, 1.0);
end;

function AngleRadBetweenNormals(const V1, V2: TVector3): TScalar;
begin
  result := ArcCos(CosAngleBetweenNormals(V1, V2));
end;

function RotationAngleRadBetweenVectors(const V1, V2, Cross: TVector3): TScalar;
begin
  Result := AngleRadBetweenVectors(V1, V2);
  if PointsDistanceSqr(RotatePointAroundAxisRad( Result, V1, Cross), V2) >
     PointsDistanceSqr(RotatePointAroundAxisRad(-Result, V1, Cross), V2) then
    Result := -Result;

  { Note that an assertion here that

      PointsDistance(RotatePointAroundAxisRad(Result, V1, Cross), V2)

    is zero would *not* be correct: V1 and V2 may have different
    lengths, and then really neither Result nor -Result will get
    V1 to rotate exactly to V2. However, the algorithm is still correct:
    The valid Result (AngleRadBetweenVectors or -AngleRadBetweenVectors)
    will result in shorter distance (difference between V1 and V2 lengths),
    the invalid result would for sure make longer distance. }

{ Commented out by default because this assertion is costly.
  But it should be valid, you can uncomment it for test!

  Assert(FloatsEqual(
    PointsDistance(RotatePointAroundAxisRad(Result, V1, Cross), V2),
    Abs(VectorLen(V1) - VectorLen(V2)), 0.01));
}
end;

function RotationAngleRadBetweenVectors(const V1, V2: TVector3): TScalar;
begin
  Result := RotationAngleRadBetweenVectors(V1, V2, VectorProduct(V1, V2));
end;

function Normalized(const v: TVector3): TVector3;
begin
  Result := v;
  NormalizeVar(result);
end;

function Normalized(const v: TVector2): TVector2;
begin
  Result := v;
  NormalizeVar(result);
end;

function Vector_Get_Normalized(const V: TVector3_): TVector3_;
begin
  Result.Data := V.Data;
  NormalizeVar(Result.Data);
end;

procedure Vector_Normalize(var V: TVector3_);
begin
  NormalizeVar(V.Data);
end;

function RotatePointAroundAxisDeg(Angle: TScalar; const Point: TVector3;
  const Axis: TVector3): TVector3;
begin
  result := RotatePointAroundAxisRad(DegToRad(Angle), Point, Axis);
end;

function RotatePointAroundAxisRad(Angle: TScalar; const Point: TVector3;
  const Axis: TVector3): TVector3;
var
  x, y,z, l: TScalar;
  sinAngle, cosAngle: Float;
begin
  SinCos(Angle, sinAngle, cosAngle);
  l := VectorLen(Axis);

  { normalize and decompose Axis vector }
  x := Axis[0]/l;
  y := Axis[1]/l;
  z := Axis[2]/l;

  Result[0] := (cosAngle + (1 - cosAngle) * x * x)     * Point[0]
            + ((1 - cosAngle) * x * y - z * sinAngle)  * Point[1]
            + ((1 - cosAngle) * x * z + y * sinAngle)  * Point[2];

  Result[1] := ((1 - cosAngle) * x * y + z * sinAngle)  * Point[0]
            + (cosAngle + (1 - cosAngle) * y * y)       * Point[1]
            + ((1 - cosAngle) * y * z - x * sinAngle)   * Point[2];

  Result[2] := ((1 - cosAngle) * x * z - y * sinAngle)  * Point[0]
            + ((1 - cosAngle) * y * z + x * sinAngle)   * Point[1]
            + (cosAngle + (1 - cosAngle) * z * z)       * Point[2];
end;

function MaxVectorCoord(const v: TVector2): integer;
begin
  result := 0;
  if v[1] > v[result] then result := 1;
end;

function MaxVectorCoord(const v: TVector3): integer;
begin
  result := 0;
  { order of comparisons is important. We start from 0, then 1 and 2,
    and change only when differ (>, not just >=). This way we
    guarantee that when values are equal, lower coordinate wins. }
  if v[1] > v[result] then result := 1;
  if v[2] > v[result] then result := 2;
end;

function MinVectorCoord(const v: TVector3): integer;
begin
  result := 0;
  if v[1] < v[result] then result := 1;
  if v[2] < v[result] then result := 2;
end;

function MaxVectorCoord(const v: TVector4): integer;
begin
  result := 0;
  if v[1] > v[result] then result := 1;
  if v[2] > v[result] then result := 2;
  if v[3] > v[result] then result := 3;
end;

function MaxAbsVectorCoord(const v: TVector2): integer;
begin
  result := 0;
  if Abs(v[1]) > Abs(v[result]) then result := 1;
end;

function MaxAbsVectorCoord(const v: TVector3): integer;
begin
  result := 0;
  if Abs(v[1]) > Abs(v[result]) then result := 1;
  if Abs(v[2]) > Abs(v[result]) then result := 2;
end;

procedure SortAbsVectorCoord(const v: TVector3;
  out Max, Middle, Min: Integer);
begin
  Max := 0;
  if Abs(V[1]) > Abs(V[Max]) then Max := 1;
  if Abs(V[2]) > Abs(V[Max]) then Max := 2;
  case Max of
    0: if Abs(V[1]) >= Abs(V[2]) then begin Middle := 1; Min := 2; end else begin Middle := 2; Min := 1; end;
    1: if Abs(V[0]) >= Abs(V[2]) then begin Middle := 0; Min := 2; end else begin Middle := 2; Min := 0; end;
    else {2: }
       if Abs(V[0]) >= Abs(V[1]) then begin Middle := 0; Min := 1; end else begin Middle := 1; Min := 0; end;
  end;
end;

function PlaneDirInDirection(const Plane: TVector4; const Direction: TVector3): TVector3;
var
  PlaneDir: TVector3 absolute Plane;
begin
  result := PlaneDirInDirection(PlaneDir, Direction);
end;

function PlaneDirInDirection(const PlaneDir, Direction: TVector3): TVector3;
begin
  (* "Normalny" sposob aby sprawdzic czy dwa wektory wskazuja z tej samej
    plaszczyzny to porownac
          VectorDotProduct(V1, PlaneDir) > 0
          VectorDotProduct(V2, PlaneDir) > 0
    czyli tak jakby obciac czwarta wspolrzedna plaszczyzny (zeby plaszczyzna
    przechodzila przez (0, 0,0)) i sprawdzic czy dwa punkty leza po tej samej
    stronie plaszczyzny

    (jezeli jeden z wektorow V1 lub V2 jest rownolegly do plaszczyzny,
     tzn. VectorDotProduct(V*, PlaneDir) = 0 to przyjmujemy ze drugi
     moze byc w dowolna strone, wiec nawet sie
     nie przejmujemy co bedzie gdy zajdzie rownosc w ktorejs z powyzszych
     nierownosci).

    Ale mozna to uproscic gdy V1 = PlaneDir. Wiemy ze
      VectorDotProduct(PlaneDir, PlaneDir) > 0
    bo to przeciez suma trzech kwadratow. Wiec wystarczy sprawdzic czy
      VectorDotProduct(Direction, PlaneDir) > 0
    - jesli nie to trzeba odwrocic Normal. *)
  if VectorDotProduct(Direction, PlaneDir) < 0 then
    result := VectorNegate(PlaneDir) else
    result := PlaneDir;
end;

function PlaneDirNotInDirection(const Plane: TVector4; const Direction: TVector3): TVector3;
var
  PlaneDir: TVector3 absolute Plane;
begin
  result := PlaneDirNotInDirection(PlaneDir, Direction);
end;

procedure TwoPlanesIntersectionLine(const Plane0, Plane1: TVector4;
  out Line0, LineVector: TVector3);
var
  Plane0Dir: TVector3 absolute Plane0;
  Plane1Dir: TVector3 absolute Plane1;
  NonZeroIndex, Index1, Index2: Integer;
  PlaneWithNonZeroIndex1: PVector4;
  PlaneMultiply, Sum_Index2, Sum_3: TScalar;
begin
  LineVector := VectorProduct(Plane0Dir, Plane1Dir);

  NonZeroIndex := MaxAbsVectorCoord(LineVector);
  if Zero(LineVector[NonZeroIndex]) then
    raise EPlanesParallel.Create(
      'Unable to calculate intersection line of two planes ' +
      VectorToRawStr(Plane0) + ' and ' + VectorToRawStr(Plane1) + ' because ' +
      'planes are parallel');

  { Since LineVector[NonZeroIndex] <> 0, we know that we can find exactly
    one point on this line by assuming that Point[NonZeroIndex] = 0. }
  Line0[NonZeroIndex] := 0;
  RestOf3dCoords(NonZeroIndex, Index1, Index2);

  { Now we must solve
      Plane0[Index1] * Line0[Index1] + Plane0[Index2] * Line0[Index2] + Plane0[3] = 0
      Plane1[Index1] * Line0[Index1] + Plane1[Index2] * Line0[Index2] + Plane1[3] = 0
    We want to sum these two equations to eliminate Line0[Index1]:
      0                                 + Sum_Index2        * Line0[Index2] + Sum_3        = 0
  }
  if not Zero(Plane0[Index1]) then
  begin
    PlaneWithNonZeroIndex1 := @Plane0;
    PlaneMultiply := - Plane1[Index1] / Plane0[Index1];
    Sum_Index2 := Plane0[Index2] * PlaneMultiply + Plane1[Index2];
    Sum_3      := Plane0[3]      * PlaneMultiply + Plane1[3];
  end else
  if not Zero(Plane1[Index1]) then
  begin
    PlaneWithNonZeroIndex1 := @Plane1;
    PlaneMultiply := - Plane0[Index1] / Plane1[Index1];
    Sum_Index2 := Plane0[Index2] + Plane1[Index2] * PlaneMultiply;
    Sum_3      := Plane0[3]      + Plane1[3]      * PlaneMultiply;
  end else
  begin
    { If Plane0[Index1] = Plane1[Index1] = 0, this is simple.
        Sum_Index2 := Plane0[Index2] + Plane1[Index2];
        Sum_3      := Plane0[3]      + Plane1[3]     ;
        PlaneWithNonZeroIndex1 := ???;
      But it's useless, because then I will not be able to calculate
      Line0[Index1] (after obtaining Line0[Index2]).
      TODO -- some proof that this cannot happen for correct input ? }
    raise Exception.Create('Cannot calculate intersection line of two planes');
  end;

  { Now we know that
      Sum_Index2 * Line0[Index2] + Sum_3 = 0
    Sum_Index2 must be <> 0, since we know that Line0[Index2] must be uniquely
    determined ? Right ? TODO -- I'm not sure, how to prove this simply ?
  }
  Line0[Index2] := - Sum_3 / Sum_Index2;

  { Note we have
      PlaneWithNonZeroIndex1^[Index1] * Line0[Index1] +
      PlaneWithNonZeroIndex1^[Index2] * Line0[Index2] +
      PlaneWithNonZeroIndex1^[3] = 0
    All is known except Line0[Index1],
    PlaneWithNonZeroIndex1^[Index1] is for sure <> 0. }
  Line0[Index1] := -
    (PlaneWithNonZeroIndex1^[Index2] * Line0[Index2] +
     PlaneWithNonZeroIndex1^[3]) /
    PlaneWithNonZeroIndex1^[Index1];
end;

function Lines2DIntersection(const Line0, Line1: TVector3): TVector2;
var
  Ratio, Divide: TScalar;
begin
  { Only one from Line0[0], Line0[1] may be zero.
    Take larger one for numerical stability. }
  if Abs(Line0[0]) > Abs(Line0[1]) then
  begin
    Ratio := Line1[0] / Line0[0];

    { we have equations
        Line0[0] * x + Line0[1] * y + Line0[2] = 0
        Line1[0] * x + Line1[1] * y + Line1[2] = 0
      Multiply first equation by Ratio and subtract to 2nd one:
        y * (Line0[1] * Ratio - Line1[1]) + Line0[2] * Ratio - Line1[2] = 0 }
    Divide := Line0[1] * Ratio - Line1[1];
    if Divide = 0 then
      raise ELinesParallel.Create('Lines are parallel, Lines2DIntersection not possible');
    Result[1] := - (Line0[2] * Ratio - Line1[2]) / Divide;
    Result[0] := - (Line0[1] * Result[1] + Line0[2]) / Line0[0];
  end else
  begin
    Ratio := Line1[1] / Line0[1];

    { we have equations
        Line0[0] * x + Line0[1] * y + Line0[2] = 0
        Line1[0] * x + Line1[1] * y + Line1[2] = 0
      Multiply first equation by Ratio and subtract to 2nd one:
        x * (Line0[0] * Ratio - Line1[0]) + Line0[2] * Ratio - Line1[2] = 0 }
    Divide := Line0[0] * Ratio - Line1[0];
    if Divide = 0 then
      raise ELinesParallel.Create('Lines are parallel, Lines2DIntersection not possible');
    Result[0] := - (Line0[2] * Ratio - Line1[2]) / Divide;
    Result[1] := - (Line0[0] * Result[0] + Line0[2]) / Line0[1];
  end;

  { tests: (checking should write zeros)
  Writeln('intersection 2d: ', VectorToNiceStr(Line0), ' ',
    VectorToNiceStr(Line1), ' gives ', VectorToNiceStr(Result), ' checking: ',
    FloatToNiceStr(Line0[0] * Result[0] + Line0[1] * Result[1] + Line0[2]), ' ',
    FloatToNiceStr(Line1[0] * Result[0] + Line1[1] * Result[1] + Line1[2])); }
end;

function ThreePlanesIntersectionPoint(
  const Plane0, Plane1, Plane2: TVector4): TVector3;
var
  Line0, LineVector: TVector3;
begin
  TwoPlanesIntersectionLine(Plane0, Plane1, Line0, LineVector);
  if not TryPlaneLineIntersection(Result, Plane2, Line0, LineVector) then
    raise Exception.Create('Cannot calculate intersection point of three planes :' +
      'intersection line of first two planes is parallel to the 3rd plane');
end;

function PlaneMove(const Plane: TVector4;
  const Move: TVector3): TVector4;
begin
  { Given a plane Ax + By + Cz + D = 0.
    We want to find a new plane, moved by Move.
    Actually, we want to find only new D, since we know that (A, B, C)
    is a normal vector of the plane, so it doesn't change.

    Math says: old plane equation is OK for point (x, y, z) if and only if
    new plane equation is OK for (x, y, z) + Move. Therefore
      Ax + By + Cz + D = 0 iff
      A * (x + Move[0]) + B * (y + Move[1]) + C * (z + Move[2]) + NewD = 0
    The 2nd equation can be rewritten as
      Ax + By + Cz + NewD + A * Move[0] + B * Move[1] + C * Move[2] = 0
    so
      NewD = D - (A * Move[0] + B * Move[1] + C * Move[2]);
  }

  Result := Plane;
  Result[3] -= Plane[0] * Move[0] +
               Plane[1] * Move[1] +
               Plane[2] * Move[2];
end;

procedure PlaneMoveVar(var Plane: TVector4; const Move: TVector3);
begin
  Plane[3] -= Plane[0] * Move[0] +
              Plane[1] * Move[1] +
              Plane[2] * Move[2];
end;

function PlaneAntiMove(const Plane: TVector4;
  const Move: TVector3): TVector4;
begin
  { Like PlaneMove, but Move vector is negated.
    So we just do "Result[3] +=" instead of "Result[3] -=". }

  Result := Plane;
  Result[3] += Plane[0] * Move[0] +
               Plane[1] * Move[1] +
               Plane[2] * Move[2];
end;

{$define VectorsSamePlaneDirections_Implement:=
var
  v1dot, v2dot: TScalar;
begin
  v1dot := VectorDotProduct(v1, PlaneDir);
  v2dot := VectorDotProduct(v2, PlaneDir);
  result := Zero(v1dot) or Zero(v2dot) or ((v1dot > 0) = (v2dot > 0));
end;}

  function VectorsSamePlaneDirections(const V1, V2: TVector3;
    const Plane: TVector4): boolean;
  var PlaneDir: TVector3 absolute Plane;
  VectorsSamePlaneDirections_Implement

  function VectorsSamePlaneDirections(const V1, V2: TVector3;
    const PlaneDir: TVector3): boolean;
  VectorsSamePlaneDirections_Implement

{$undef VectorsSamePlaneDirections_Implement}

function PointsSamePlaneSides(const p1, p2: TVector3; const Plane: TVector4): boolean;
var
  p1Side, p2Side: TScalar;
begin
  p1Side := p1[0]*Plane[0] + p1[1]*Plane[1] + p1[2]*Plane[2] + Plane[3];
  p2Side := p2[0]*Plane[0] + p2[1]*Plane[1] + p2[2]*Plane[2] + Plane[3];
  result := Zero(p1Side) or Zero(p2Side) or ((p1Side > 0) = (p2Side > 0));
end;

function PlaneDirNotInDirection(const PlaneDir, Direction: TVector3): TVector3;
begin
  if VectorDotProduct(Direction, PlaneDir) > 0 then
    result := VectorNegate(PlaneDir) else
    result := PlaneDir;
end;

function PointsDistance(const V1, V2: TVector2): TScalar;
begin
  { Result := Sqrt(PointsDistanceSqr(V1, V2));, expanded for speed }
  result := Sqrt( Sqr(V2[0]-V1[0]) + Sqr(V2[1]-V1[1]) );
end;

function PointsDistance(const V1, V2: TVector3): TScalar;
begin
  { Result := Sqrt(PointsDistanceSqr(V1, V2));, expanded for speed }
  result := Sqrt( Sqr(V2[0]-V1[0]) + Sqr(V2[1]-V1[1]) + Sqr(V2[2]-V1[2]) );
end;

function PointsDistanceSqr(const V1, V2: TVector3): TScalar;
begin
  { Result := VectorLenSqr(VectorSubtract(v2, v1));, expanded for speed }
  result := Sqr(V2[0]-V1[0]) + Sqr(V2[1]-V1[1]) + Sqr(V2[2]-V1[2]);
end;

function PointsDistanceSqr(const V1, V2: TVector2): TScalar;
begin
  { Result := VectorLenSqr(VectorSubtract(v2, v1));, expanded for speed }
  result := Sqr(V2[0]-V1[0]) + Sqr(V2[1]-V1[1]);
end;

function PointsDistance2DSqr(const V1, V2: TVector3; const IgnoreIndex: Integer): TScalar;
begin
  case IgnoreIndex of
    0: Result := Sqr(V2[1] - V1[1]) + Sqr(V2[2] - V1[2]);
    1: Result := Sqr(V2[2] - V1[2]) + Sqr(V2[0] - V1[0]);
    2: Result := Sqr(V2[0] - V1[0]) + Sqr(V2[1] - V1[1]);
    else PointsDistance2DSqr_InvalidIgnoreIndex;
  end;
end;

function VectorsEqual(const V1, V2: TVector2): boolean;
begin
  if ScalarEqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) else
    Result := (Abs(V1[0]-V2[0]) < ScalarEqualityEpsilon) and
              (Abs(V1[1]-V2[1]) < ScalarEqualityEpsilon);
end;

function VectorsEqual(const V1, V2: TVector2;
  const EqualityEpsilon: TScalar): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) else
    Result := (Abs(V1[0]-V2[0]) < EqualityEpsilon) and
              (Abs(V1[1]-V2[1]) < EqualityEpsilon);
end;

function VectorsEqual(const V1, V2: TVector3): boolean;
begin
  if ScalarEqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) and
              (V1[2] = V2[2]) else
    Result := FloatsEqual(V1[0], V2[0]) and
              FloatsEqual(V1[1], V2[1]) and
              FloatsEqual(V1[2], V2[2]);
end;

function VectorsEqual(const V1, V2: TVector3;
  const EqualityEpsilon: TScalar): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) and
              (V1[2] = V2[2]) else
    Result := (Abs(V1[0]-V2[0]) < EqualityEpsilon) and
              (Abs(V1[1]-V2[1]) < EqualityEpsilon) and
              (Abs(V1[2]-V2[2]) < EqualityEpsilon);
end;

function VectorsEqual(const V1, V2: TVector4): boolean;
begin
  if ScalarEqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) and
              (V1[2] = V2[2]) and
              (V1[3] = V2[3]) else
    Result := FloatsEqual(V1[0], V2[0]) and
              FloatsEqual(V1[1], V2[1]) and
              FloatsEqual(V1[2], V2[2]) and
              FloatsEqual(V1[3], V2[3]);
end;

function VectorsEqual(const V1, V2: TVector4;
  const EqualityEpsilon: TScalar): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := (V1[0] = V2[0]) and
              (V1[1] = V2[1]) and
              (V1[2] = V2[2]) and
              (V1[3] = V2[3]) else
    Result := (Abs(V1[0]-V2[0]) < EqualityEpsilon) and
              (Abs(V1[1]-V2[1]) < EqualityEpsilon) and
              (Abs(V1[2]-V2[2]) < EqualityEpsilon) and
              (Abs(V1[3]-V2[3]) < EqualityEpsilon);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector2): boolean;
{$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector3): boolean;
{$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector4): boolean;
{$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]) and
            (V1[3] = V2[3]);
end;

function MatricesEqual(const M1, M2: TMatrix3;
  const EqualityEpsilon: TScalar): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := CompareMem(@M1, @M2, SizeOf(M1)) else
    Result :=
      (Abs(M1[0, 0] - M2[0, 0]) < EqualityEpsilon) and
      (Abs(M1[0, 1] - M2[0, 1]) < EqualityEpsilon) and
      (Abs(M1[0, 2] - M2[0, 2]) < EqualityEpsilon) and

      (Abs(M1[1, 0] - M2[1, 0]) < EqualityEpsilon) and
      (Abs(M1[1, 1] - M2[1, 1]) < EqualityEpsilon) and
      (Abs(M1[1, 2] - M2[1, 2]) < EqualityEpsilon) and

      (Abs(M1[2, 0] - M2[2, 0]) < EqualityEpsilon) and
      (Abs(M1[2, 1] - M2[2, 1]) < EqualityEpsilon) and
      (Abs(M1[2, 2] - M2[2, 2]) < EqualityEpsilon);
end;

function MatricesEqual(const M1, M2: TMatrix4;
  const EqualityEpsilon: TScalar): boolean;
begin
  if EqualityEpsilon = 0 then
    Result := CompareMem(@M1, @M2, SizeOf(M1)) else
    Result :=
      (Abs(M1[0, 0] - M2[0, 0]) < EqualityEpsilon) and
      (Abs(M1[0, 1] - M2[0, 1]) < EqualityEpsilon) and
      (Abs(M1[0, 2] - M2[0, 2]) < EqualityEpsilon) and
      (Abs(M1[0, 3] - M2[0, 3]) < EqualityEpsilon) and

      (Abs(M1[1, 0] - M2[1, 0]) < EqualityEpsilon) and
      (Abs(M1[1, 1] - M2[1, 1]) < EqualityEpsilon) and
      (Abs(M1[1, 2] - M2[1, 2]) < EqualityEpsilon) and
      (Abs(M1[1, 3] - M2[1, 3]) < EqualityEpsilon) and

      (Abs(M1[2, 0] - M2[2, 0]) < EqualityEpsilon) and
      (Abs(M1[2, 1] - M2[2, 1]) < EqualityEpsilon) and
      (Abs(M1[2, 2] - M2[2, 2]) < EqualityEpsilon) and
      (Abs(M1[2, 3] - M2[2, 3]) < EqualityEpsilon) and

      (Abs(M1[3, 0] - M2[3, 0]) < EqualityEpsilon) and
      (Abs(M1[3, 1] - M2[3, 1]) < EqualityEpsilon) and
      (Abs(M1[3, 2] - M2[3, 2]) < EqualityEpsilon) and
      (Abs(M1[3, 3] - M2[3, 3]) < EqualityEpsilon);
end;

function Lerp(const A: TScalar; const M1, M2: TMatrix3): TMatrix3;
begin
  Result[0, 0] := M1[0, 0] + A * (M2[0, 0] - M1[0, 0]);
  Result[0, 1] := M1[0, 1] + A * (M2[0, 1] - M1[0, 1]);
  Result[0, 2] := M1[0, 2] + A * (M2[0, 2] - M1[0, 2]);

  Result[1, 0] := M1[1, 0] + A * (M2[1, 0] - M1[1, 0]);
  Result[1, 1] := M1[1, 1] + A * (M2[1, 1] - M1[1, 1]);
  Result[1, 2] := M1[1, 2] + A * (M2[1, 2] - M1[1, 2]);

  Result[2, 0] := M1[2, 0] + A * (M2[2, 0] - M1[2, 0]);
  Result[2, 1] := M1[2, 1] + A * (M2[2, 1] - M1[2, 1]);
  Result[2, 2] := M1[2, 2] + A * (M2[2, 2] - M1[2, 2]);
end;

function Lerp(const A: TScalar; const M1, M2: TMatrix4): TMatrix4;
begin
  Result[0, 0] := M1[0, 0] + A * (M2[0, 0] - M1[0, 0]);
  Result[0, 1] := M1[0, 1] + A * (M2[0, 1] - M1[0, 1]);
  Result[0, 2] := M1[0, 2] + A * (M2[0, 2] - M1[0, 2]);
  Result[0, 3] := M1[0, 3] + A * (M2[0, 3] - M1[0, 3]);

  Result[1, 0] := M1[1, 0] + A * (M2[1, 0] - M1[1, 0]);
  Result[1, 1] := M1[1, 1] + A * (M2[1, 1] - M1[1, 1]);
  Result[1, 2] := M1[1, 2] + A * (M2[1, 2] - M1[1, 2]);
  Result[1, 3] := M1[1, 3] + A * (M2[1, 3] - M1[1, 3]);

  Result[2, 0] := M1[2, 0] + A * (M2[2, 0] - M1[2, 0]);
  Result[2, 1] := M1[2, 1] + A * (M2[2, 1] - M1[2, 1]);
  Result[2, 2] := M1[2, 2] + A * (M2[2, 2] - M1[2, 2]);
  Result[2, 3] := M1[2, 3] + A * (M2[2, 3] - M1[2, 3]);

  Result[3, 0] := M1[3, 0] + A * (M2[3, 0] - M1[3, 0]);
  Result[3, 1] := M1[3, 1] + A * (M2[3, 1] - M1[3, 1]);
  Result[3, 2] := M1[3, 2] + A * (M2[3, 2] - M1[3, 2]);
  Result[3, 3] := M1[3, 3] + A * (M2[3, 3] - M1[3, 3]);
end;

function Lerp(const A: TScalar; const V1, V2: TVector2): TVector2;
begin
  Result[0] := V1[0] + a*(V2[0]-V1[0]);
  Result[1] := V1[1] + a*(V2[1]-V1[1]);
end;

function Lerp(const A: TScalar; const V1, V2: TVector3): TVector3;
begin
  Result[0] := V1[0] + a*(V2[0]-V1[0]);
  Result[1] := V1[1] + a*(V2[1]-V1[1]);
  Result[2] := V1[2] + a*(V2[2]-V1[2]);
end;

function Lerp(const A: TScalar; const V1, V2: TVector4): TVector4;
begin
  Result[0] := V1[0] + a*(V2[0]-V1[0]);
  Result[1] := V1[1] + a*(V2[1]-V1[1]);
  Result[2] := V1[2] + a*(V2[2]-V1[2]);
  Result[3] := V1[3] + a*(V2[3]-V1[3]);
end;

function MatricesPerfectlyEqual(const M1, M2: TMatrix3): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

function MatricesPerfectlyEqual(const M1, M2: TMatrix4): boolean;
begin
  Result := CompareMem(@M1, @M2, SizeOf(M1));
end;

function VectorsPerp(const V1, V2: TVector3): boolean;
begin
  (* prosto : result := CosAngleBetweenVectors(V1, V2) = 0.
     Ale mozna zobaczyc jak liczymy CosAngleBetweenVectors - to jest
     VectorDotProduct / cos-tam. Wynik jest = 0 <=> VectorDotProduct = 0. *)
  result := Zero(VectorDotProduct(V1, V2), ScalarEqualityEpsilon*2);
end;

function VectorsParallel(const V1, V2: TVector3): boolean;
var
  mc, c1, c2: Integer;
  Scale: TScalar;
begin
  mc := MaxAbsVectorCoord(v1);
  if Zero(V1[mc]) then Exit(true);

  Scale := V2[mc] / V1[mc];
  RestOf3dCoords(mc, c1, c2);
  result := FloatsEqual(V1[c1] * Scale, V2[c1]) and
            FloatsEqual(V1[c2] * Scale, V2[c2]);
end;

procedure MakeVectorsAngleRadOnTheirPlane(var v1: TVector3;
  const v2: TVector3; const AngleRad: TScalar; const ResultWhenParallel: TVector3);
var
  rotAxis: TVector3;
  v1len: TScalar;
begin
  v1len := VectorLen(v1);
  rotAxis := VectorProduct(V1, V2);
  if ZeroVector(rotAxis) then
    V1 := ResultWhenParallel else
    V1 := VectorAdjustToLength(
      RotatePointAroundAxisRad(-AngleRad, v2, rotAxis), v1len);
end;

procedure MakeVectorsOrthoOnTheirPlane(var v1: TVector3; const v2: TVector3);
begin
  { TODO: can we speed this up ?
    For Pi/2, the RotatePointAroundAxisRad can probably be speed up. }
  MakeVectorsAngleRadOnTheirPlane(V1, V2, Pi / 2, AnyOrthogonalVector(V2));
end;

function AnyOrthogonalVector(const v: TVector3): TVector3;
begin
  { This uses a simple trick to make an orthogonal vector:
    if you take @code(Result := (V[1], -V[0], 0)) then the dot product
    between the Result and V is zero, so they are orthogonal.
    There's also a small check needed to use a similar but different version
    when the only non-zero component of V is V[2]. }

  if Zero(v[0]) and Zero(v[1]) then
  begin
    Result[0] := 0;
    Result[1] := v[2];
    Result[2] := -v[1];
  end else
  begin
    Result[0] := v[1];
    Result[1] := -v[0];
    Result[2] := 0;
  end;
end;

function AnyOrthogonalVector(const v: TVector2): TVector2;
begin
  Result[0] :=  V[1];
  Result[1] := -V[0];
end;

function IsLineParallelToPlane(const lineVector: TVector3; const plane: TVector4): boolean;
var
  PlaneDir: TVector3 absolute plane;
begin
  result := VectorsPerp(lineVector, PlaneDir);
end;

function IsLineParallelToSimplePlane(const lineVector: TVector3;
  const PlaneConstCoord: integer): boolean;
begin
  result := Zero(lineVector[PlaneConstCoord]);
end;

function AreParallelVectorsSameDirection(
  const Vector1, Vector2: TVector3): boolean;
var
  Coord: Integer;
begin
  { Assuming that Vector1 is non-zero, MaxAbsVectorCoord(Vector1)
    must be non-zero. }
  Coord := MaxAbsVectorCoord(Vector1);

  Result := (Vector1[Coord] > 0) = (Vector2[Coord] > 0);
end;

function PointOnPlaneClosestToPoint(const plane: TVector4; const point: TVector3): TVector3;
var
  d: TScalar;
  PlaneDir: TVector3 absolute plane;
begin
  (*licz punkt Pr - punkt na plaszczyznie plane bedacy rzutem prostopadlym
    punktu pos na ta plaszczyzne. Pr = pos + d * PlaneDir.
    plane[0]*Pr[0] + plane[1]*Pr[1] + plane[2]*Pr[2] + plane[3] = 0,
    mamy wiec
    plane[0]*(pos[0] + d*plane[0])+
    plane[1]*(pos[1] + d*plane[1])+
    plane[2]*(pos[2] + d*plane[2])+ plane[3] = 0
    Przeksztalcajac otrzymujemy rownanie na d.*)
  d := -(plane[0]*point[0] + plane[1]*point[1] + plane[2]*point[2] + plane[3])/
      VectorLenSqr(PlaneDir);
  result := VectorAdd(point, VectorScale(PlaneDir, d));
end;

function PointToPlaneDistanceSqr(const Point: TVector3;
  const Plane: TVector4): TScalar;
begin
  Result :=
    Sqr(Plane[0] * Point[0] +
        Plane[1] * Point[1] +
        Plane[2] * Point[2] +
        Plane[3]) /
    (Sqr(Plane[0]) + Sqr(Plane[1]) + Sqr(Plane[2]));
end;

function PointToNormalizedPlaneDistance(const Point: TVector3;
  const Plane: TVector4): TScalar;
begin
  Result :=
    Abs(Plane[0] * Point[0] +
        Plane[1] * Point[1] +
        Plane[2] * Point[2] +
        Plane[3]);
end;

function PointToPlaneDistance(const Point: TVector3;
  const Plane: TVector4): TScalar;
begin
  Result :=
    Abs(Plane[0] * Point[0] +
        Plane[1] * Point[1] +
        Plane[2] * Point[2] +
        Plane[3]) /
    Sqrt(Sqr(Plane[0]) + Sqr(Plane[1]) + Sqr(Plane[2]));
end;

function PointToSimplePlaneDistance(const point: TVector3;
  const PlaneConstCoord: integer; const PlaneConstValue: TScalar): TScalar;
begin
  result := Abs(point[PlaneConstCoord]-PlaneConstValue);
end;

function PointOnLineClosestToPoint(
  const line0, lineVector, point: TVector3): TVector3;
var
  d: TScalar;
begin
  (*
   wiemy ze wektory result-point i lineVector (albo result-line0) sa prostopadle.
   (Result[0]-point[0])*(lineVector[0]) +
   (Result[1]-point[1])*(lineVector[1]) +
   (Result[2]-point[2])*(lineVector[2]) = 0 czyli
   Result[0]*lineVector[0] +
   Result[1]*lineVector[1] +
   Result[2]*lineVector[2] = point[0]*lineVector[0] +
                             point[1]*lineVector[1] +
                             point[2]*lineVector[2]
   Wiemy ze result wyraza sie jako line0 + lineVector*d
   result = line0+lineVector*d
   czyli
   Result[0] = line0[0] + lineVector[0]*d
   Result[1] = line0[1] + lineVector[1]*d
   Result[2] = line0[2] + lineVector[2]*d
   a wiec 4 rownania, 4 niewiadome i juz wiemy ze jestesmy w domu.
   Podstawiamy :
   (line0[0] + lineVector[0]*d)*lineVector[0]+
   (line0[1] + lineVector[1]*d)*lineVector[1]+
   (line0[2] + lineVector[2]*d)*lineVector[2] = point[0]*lineVector[0] +
                             point[1]*lineVector[1] +
                             point[2]*lineVector[2]
   d*(Sqr(lineVector[0])+ Sqr(lineVector[1])+ Sqr(lineVector[2]) ) =
   d*VectorLenSqr(lineVector) =
   lineVector[0]*(point[0]-line0[0]) +
   lineVector[1]*(point[1]-line0[1]) +
   lineVector[2]*(point[2]-line0[2]);
   i stad mamy d. *)
  d := (lineVector[0] * (point[0]-line0[0]) +
        lineVector[1] * (point[1]-line0[1]) +
        lineVector[2] * (point[2]-line0[2]) ) / VectorLenSqr(lineVector);
  result := VectorAdd(line0, VectorScale(lineVector, d));
end;

function PointOnLineClosestToPoint(
  const line0, lineVector, point: TVector2): TVector2;
var
  d: TScalar;
begin
  d := (lineVector[0] * (point[0]-line0[0]) +
        lineVector[1] * (point[1]-line0[1]) ) / VectorLenSqr(lineVector);
  result := VectorAdd(line0, VectorScale(lineVector, d));
end;

function PointToLineDistanceSqr(const point, line0, lineVector: TVector3): TScalar;
begin
  result := PointsDistanceSqr(point, PointOnLineClosestToPoint(line0, lineVector, point));
end;

function TryPlaneLineIntersection(out t: TScalar;
  const plane: TVector4; const line0, lineVector: TVector3): boolean;
var
  PlaneDir: TVector3 absolute plane;
  Dot: TScalar;
begin
  Dot := VectorDotProduct(LineVector, PlaneDir);
  if not Zero(Dot) then
  begin
    result := true;
    t := -(plane[0]*line0[0] + plane[1]*line0[1] + plane[2]*line0[2] + plane[3])/Dot;
  end else
    result := false;
end;

function TryPlaneLineIntersection(out intersection: TVector3;
  const plane: TVector4; const line0, lineVector: TVector3): boolean;
var
  t: TScalar;
begin
  result := TryPlaneLineIntersection(t, Plane, Line0, LineVector);
  if result then Intersection := VectorAdd(Line0, VectorScale(LineVector, t));
end;

function TryPlaneRayIntersection(out Intersection: TVector3;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeT: TScalar;
begin
  result := TryPlaneLineIntersection(MaybeT, Plane, RayOrigin, RayDirection) and (MaybeT >= 0);
  if result then Intersection := VectorAdd(RayOrigin, VectorScale(RayDirection, MaybeT));
end;

function TryPlaneRayIntersection(
  out Intersection: TVector3; out T: TScalar;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeT: TScalar;
begin
  result := TryPlaneLineIntersection(MaybeT, Plane, RayOrigin, RayDirection) and (MaybeT >= 0);
  if result then
  begin
    // Intersection := VectorAdd(RayOrigin, VectorScale(RayDirection, MaybeT));
    // powyzsza instrukcja zapisana ponizej w 3 linijkach dziala nieco szybciej:
    Intersection := RayDirection;
    VectorScaleVar(Intersection, MaybeT);
    VectorAddVar(Intersection, RayOrigin);

    t := MaybeT;
  end;
end;

function TryPlaneSegmentDirIntersection(out Intersection: TVector3;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeT: TScalar;
begin
  result := TryPlaneLineIntersection(MaybeT, Plane, Segment0, SegmentVector) and
    (MaybeT >= 0) and (MaybeT <= 1);
  if result then Intersection := VectorAdd(Segment0, VectorScale(SegmentVector, MaybeT));
end;

function TryPlaneSegmentDirIntersection(
  out Intersection: TVector3; out T: TScalar;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeT: TScalar;
begin
  result := TryPlaneLineIntersection(MaybeT, Plane, Segment0, SegmentVector) and
    (MaybeT >= 0) and (MaybeT <= 1);
  if result then
  begin
    // Intersection := VectorAdd(Segment0, VectorScale(SegmentVector, MaybeT));
    // powyzsza instrukcja zapisana ponizej w 3 linijkach dziala nieco szybciej:
    Intersection := SegmentVector;
    VectorScaleVar(Intersection, MaybeT);
    VectorAddVar(Intersection, Segment0);

    t := MaybeT;
  end;
end;

(*$I castlevectors_trysimpleplanexxxintersection.inc*)

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector3): boolean;
var
  vecSizes: TVector3;
  c, i: integer;
begin
  (*rzutujemy 3 zadane punkty na ta wspolrzedna na ktorej mamy najwieksza swobode*)
  for i := 0 to High(vecSizes) do
    vecSizes[i] := Abs(pos1[i]-pos2[i]);
  c := MaxVectorCoord(vecSizes);
  result := ((pos1[c] <= intersection[c]) and (intersection[c] <= pos2[c])) or
            ((pos1[c] >= intersection[c]) and (intersection[c] >= pos2[c]));
end;

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector2): boolean;
var
  vecSizes: TVector2;
  c, i: integer;
begin
  (*rzutujemy 3 zadane punkty na ta wspolrzedna na ktorej mamy najwieksza swobode*)
  for i := 0 to High(vecSizes) do
    vecSizes[i] := Abs(pos1[i]-pos2[i]);
  c := MaxVectorCoord(vecSizes);
  result := ((pos1[c] <= intersection[c]) and (intersection[c] <= pos2[c])) or
            ((pos1[c] >= intersection[c]) and (intersection[c] >= pos2[c]));
end;

function LineOfTwoDifferentPoints2d(const p1, p2: TVector2): TVector3;
var
  lineVector: TVector2;
  cGood, cOther: integer;
begin
  (* chcemy zeby Vector2f(result) i p2-p1(=lineVector) byly prostopadle czyli ich
     iloczyn skalarny = 0 czyli Result[0]*lineVector[0] +
     Result[1]*lineVector[1] = 0. Niech cGood to wspolrzedna
     lineVector rozna od 0, cOther to ta druga.
     Niech Result[cOther] = -1 i zobaczmy ze wtedy mozemy skonstruowac
     Result[cGood] = lineVector[cOther] / lineVector[cGood]. *)
  lineVector := VectorSubtract(p2, p1);
  if Abs(lineVector[0]) > Abs(lineVector[1]) then
    begin cGood := 0; cOther := 1 end else
    begin cOther := 0; cGood := 1 end;
  Result[cOther] := -1;
  Result[cGood] := lineVector[cOther] / lineVector[cGood];

  (* Result[0]*p1[0] + Result[1]*p1[1] + Result[2] = 0 wiec widac jak obliczyc
     teraz Result[2] *)
  Result[2] := -Result[0]*p1[0] -Result[1]*p1[1];
end;

function IsSpheresCollision(const Sphere1Center: TVector3; const Sphere1Radius: TScalar;
  const Sphere2Center: TVector3; const Sphere2Radius: TScalar): boolean;
begin
  result := PointsDistanceSqr(Sphere1Center, Sphere2Center)<=
    Sqr(Sphere1Radius+Sphere2Radius);
end;

function PointToSegmentDistanceSqr(const point, pos1, pos2: TVector3): TScalar;
var
  Closest: TVector3;
begin
  Closest := PointOnLineClosestToPoint(pos1, VectorSubtract(pos2, pos1), point);
  if IsPointOnSegmentLineWithinSegment(Closest, pos1, pos2) then
    result := PointsDistanceSqr(Closest, point) else
    result := CastleUtils.min(PointsDistanceSqr(pos1, point),
                              PointsDistanceSqr(pos2, point));
end;

function PlaneTransform(const Plane: TVector4; const Matrix: TMatrix4): TVector4;
var
  MaxCoord: Integer;
  PlaneDir: TVector3 absolute Plane;
  NewPlaneDir: TVector3 absolute Result;
  PlanePoint, NewPlanePoint: TVector3;
begin
  { calculate point that for sure lies on a plane.
    For this, we need a plane direction coordinate that isn't zero
    --- we know that such coordinate exists, since plane direction cannot be zero.
    For maximum numeric stability, choose largest coordinate. }
  MaxCoord := MaxAbsVectorCoord(PlaneDir);
  PlanePoint := ZeroVector3;
  PlanePoint[MaxCoord] := -Plane[3] / Plane[MaxCoord];

  NewPlanePoint := MatrixMultPoint(Matrix, PlanePoint);
  NewPlaneDir := MatrixMultDirection(Matrix, PlaneDir);
  Result[3] := -VectorDotProduct(NewPlanePoint, NewPlaneDir);
end;

function IsTunnelSphereCollision(const Tunnel1, Tunnel2: TVector3;
  const TunnelRadius: TScalar; const SphereCenter: TVector3;
  const SphereRadius: TScalar): boolean;
begin
  result := PointToSegmentDistanceSqr(SphereCenter, Tunnel1, Tunnel2)<=
    Sqr(SphereRadius+TunnelRadius);
end;

function IsSegmentSphereCollision(const pos1, pos2, SphereCenter: TVector3;
  const SphereRadius: TScalar): boolean;
var
  SphereRadiusSqr: TScalar;
  Intersect: TVector3;
begin
  SphereRadiusSqr := Sqr(SphereRadius);
  result:= (PointsDistanceSqr(pos1, SphereCenter) <= SphereRadiusSqr) or
           (PointsDistanceSqr(pos2, SphereCenter) <= SphereRadiusSqr);
  if not result then
  begin
    Intersect := PointOnLineClosestToPoint(pos1, VectorSubtract(pos2, pos1), SphereCenter);
    result := IsPointOnSegmentLineWithinSegment(Intersect, pos1, pos2) and
      (PointsDistanceSqr(Intersect, SphereCenter) <= SphereRadiusSqr);
  end;
end;

{ Solve intersection routine with a ray that resolved into a quadratic
  equation. The solution is such T >= 0 that
    A * T^2 + B * T + C = 0 }
function TryRayIntersectionQuadraticEquation(out T: TScalar;
  const A, B, C: TScalar): boolean;
var
  Delta, T1, T2: TScalar;
begin
  Delta := Sqr(B) - 4 * A * C;

  if Delta < 0 then
    Result := false else
  if Delta = 0 then
  begin
    T := -B / (2 * A);
    Result := T >= 0;
  end else
  begin
    Delta := Sqrt(Delta);

    { There are two solutions, choose closest to RayOrigin (smallest)
      but >= 0 (the one < 0 does not fall on ray). }
    T1 := (-B - Delta) / (2 * A);
    T2 := (-B + Delta) / (2 * A);
    OrderUp(T1, T2);
    if T1 >= 0 then
    begin
      T := T1;
      Result := true;
    end else
    if T2 >= 0 then
    begin
      T := T2;
      Result := true;
    end else
      Result := false;
  end;
end;

function TrySphereRayIntersection(out Intersection: TVector3;
  const SphereCenter: TVector3; const SphereRadius: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  T, A, B, C: TScalar;
  RayOriginMinusCenter: TVector3;
begin
  { Intersection = RayOrigin + RayDirection * T,
    Also Distance(Intersection, SphereCenter) = SphereRadius,
    so Distance(RayOrigin + RayDirection * T, SphereCenter) = SphereRadius.

    Expand this, and use to calculate T: we get a quadratic equation for T.
    A * T^2 + B * T + C = 0. }
  RayOriginMinusCenter := RayOrigin - SphereCenter;
  A := Sqr(RayDirection[0]) +
       Sqr(RayDirection[1]) +
       Sqr(RayDirection[2]);
  B := 2 * RayDirection[0] * RayOriginMinusCenter[0] +
       2 * RayDirection[1] * RayOriginMinusCenter[1] +
       2 * RayDirection[2] * RayOriginMinusCenter[2];
  C := Sqr(RayOriginMinusCenter[0]) +
       Sqr(RayOriginMinusCenter[1]) +
       Sqr(RayOriginMinusCenter[2]) - Sqr(SphereRadius);

  Result := TryRayIntersectionQuadraticEquation(T, A, B, C);
  if Result then
    Intersection := RayOrigin + RayDirection * T;
end;

function TryCylinderRayIntersection(out Intersection: TVector3;
  const CylinderAxisOrigin, CylinderAxis: TVector3;
  const CylinderRadius: TScalar;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  T, AA, BB, CC: TScalar;
  X, Y, B: TVector3;
begin
  { We know Intersection = RayOrigin + RayDirection * T.
    For cylinder, normalize CylinderAxis and then

      VectorLen( VectorProduct(Intersection - CylinderAxisOrigin, CylinderAxis))
      = CylinderRadius

    (For why, see http://en.wikipedia.org/wiki/Cross_product:
    length of VectorProduct is the area of parallelogram between it's vectors.
    This is equal to area of CylinderRadius * length CylinderAxis in this case.)
    Got the idea from oliii post on
    http://www.gamedev.net/community/forums/topic.asp?topic_id=467789

    Insert ray equation into cylinder equation, and solve for T. }

  X := RayOrigin - CylinderAxisOrigin;
  Y := RayDirection;
  B := Normalized(CylinderAxis);

  { Now let A = X + Y * T, then VectorLen(A x B)^2 = CylinderRadius^2.
    Solve for T. Expanding this by hand would be *real* pain,
    so I used open-source maxima (http://maxima.sourceforge.net/):

    display2d:false$
    a0: x0 + y0 * t;
    a1: x1 + y1 * t;
    a2: x2 + y2 * t;
    rsqr: (a0 * b1 - b0 * a1)^2 + (a1 * b2 - a2 * b1)^2 + (a2 * b0 + a0 * b2)^2;
    expand (rsqr);

    At this point I just took the maxima output, and grouped by
    hand 30 sum items to get AA, BB, CC such that
    AA * T^2 + BB * T + CC = 0. (I could probably let maxima do this also,
    but was too lazy to read the docs :)

    And CC gets additional "- CylinderRadius^2".
  }

  AA := Sqr(B[1])*Sqr(Y[2]) + Sqr(B[0])*Sqr(Y[2]) - 2*B[1]*B[2]*Y[1]*Y[2]
    + 2*B[0]*B[2]*Y[0]*Y[2] + Sqr(B[2])*Sqr(Y[1]) + Sqr(B[0])*Sqr(Y[1])
    - 2*B[0]*B[1]*Y[0]*Y[1] + Sqr(B[2])*Sqr(Y[0]) + Sqr(B[1])*Sqr(Y[0]);

  BB := 2*Sqr(B[1])*X[2]*Y[2] + 2*Sqr(B[0])*X[2]*Y[2] - 2*B[1]*B[2]*X[1]*Y[2]
      + 2*B[0]*B[2]*X[0]*Y[2] - 2*B[1]*B[2]*X[2]*Y[1] + 2*Sqr(B[2])*X[1]*Y[1]
      + 2*Sqr(B[0])*X[1]*Y[1] - 2*B[0]*B[1]*X[0]*Y[1] + 2*B[0]*B[2]*X[2]*Y[0]
      - 2*B[0]*B[1]*X[1]*Y[0] + 2*Sqr(B[2])*X[0]*Y[0] + 2*Sqr(B[1])*X[0]*Y[0];

  CC := Sqr(B[1])*Sqr(X[2]) + Sqr(B[0])*Sqr(X[2]) - 2*B[1]*B[2]*X[1]*X[2]
    + 2*B[0]*B[2]*X[0]*X[2] + Sqr(B[2])*Sqr(X[1]) + Sqr(B[0])*Sqr(X[1])
    - 2*B[0]*B[1]*X[0]*X[1] + Sqr(B[2])*Sqr(X[0]) + Sqr(B[1])*Sqr(X[0])
    - Sqr(CylinderRadius);

  Result := TryRayIntersectionQuadraticEquation(T, AA, BB, CC);
  if Result then
    Intersection := RayOrigin + RayDirection * T;
end;

(* converting to string -------------------------------------------------------- *)

function FloatToNiceStr(f: TScalar): string;
begin
  result := Format('%'+FloatNiceFormat, [f]);
end;

function VectorToNiceStr(const v: array of TScalar): string;
var
  i: integer;
begin
  result := '(';
  for i := 0 to High(v)-1 do result := result +FloatToNiceStr(v[i]) +', ';
  if High(v) >= 0 then result := result +FloatToNiceStr(v[High(v)]) +')';
end;

function MatrixToNiceStr(const v: TMatrix4; const LineIndent: string): string;
begin
  result := Format('%s[ %4s %4s %4s %4s ]'+nl+
                   '%s| %4s %4s %4s %4s |'+nl+
                   '%s| %4s %4s %4s %4s |'+nl+
                   '%s[ %4s %4s %4s %4s ]',
   [LineIndent, FloatToNiceStr(v[0, 0]), FloatToNiceStr(v[1, 0]), FloatToNiceStr(v[2, 0]), FloatToNiceStr(v[3, 0]),
    LineIndent, FloatToNiceStr(v[0, 1]), FloatToNiceStr(v[1, 1]), FloatToNiceStr(v[2, 1]), FloatToNiceStr(v[3, 1]),
    LineIndent, FloatToNiceStr(v[0, 2]), FloatToNiceStr(v[1, 2]), FloatToNiceStr(v[2, 2]), FloatToNiceStr(v[3, 2]),
    LineIndent, FloatToNiceStr(v[0, 3]), FloatToNiceStr(v[1, 3]), FloatToNiceStr(v[2, 3]), FloatToNiceStr(v[3, 3]) ]);
end;

function MatrixToRawStr(const v: TMatrix4; const LineIndent: string): string;
begin
  result := Format('%s[ %4s %4s %4s %4s ]'+nl+
                   '%s| %4s %4s %4s %4s |'+nl+
                   '%s| %4s %4s %4s %4s |'+nl+
                   '%s[ %4s %4s %4s %4s ]',
   [LineIndent, FloatToRawStr(v[0, 0]), FloatToRawStr(v[1, 0]), FloatToRawStr(v[2, 0]), FloatToRawStr(v[3, 0]),
    LineIndent, FloatToRawStr(v[0, 1]), FloatToRawStr(v[1, 1]), FloatToRawStr(v[2, 1]), FloatToRawStr(v[3, 1]),
    LineIndent, FloatToRawStr(v[0, 2]), FloatToRawStr(v[1, 2]), FloatToRawStr(v[2, 2]), FloatToRawStr(v[3, 2]),
    LineIndent, FloatToRawStr(v[0, 3]), FloatToRawStr(v[1, 3]), FloatToRawStr(v[2, 3]), FloatToRawStr(v[3, 3]) ]);
end;

function FloatToRawStr(f: TScalar): string;
begin
  result := Format('%g', [f]);
end;

function VectorToRawStr(const v: array of TScalar): string;
var i: integer;
begin
  result := '';
  for i := 0 to High(v)-1 do result += FloatToRawStr(v[i]) +' ';
  if High(v) >= 0 then result += FloatToRawStr(v[High(v)]);
end;

{ simple matrix math --------------------------------------------------------- }

function MatrixAdd(const m1, m2: TMatrix3): TMatrix3;
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := m1[i, j] + m2[i, j];
end;

procedure MatrixAddVar(var m1: TMatrix3; const m2: TMatrix3);
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      m1[i, j] += m2[i, j];
end;

function MatrixAdd(const m1, m2: TMatrix4): TMatrix4;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := m1[i, j] + m2[i, j];
end;

procedure MatrixAddVar(var m1: TMatrix4; const m2: TMatrix4);
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      m1[i, j] += m2[i, j];
end;

function MatrixSubtract(const m1, m2: TMatrix3): TMatrix3;
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := m1[i, j] - m2[i, j];
end;

procedure MatrixSubtractVar(var m1: TMatrix3; const m2: TMatrix3);
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      m1[i, j] -= m2[i, j];
end;

function MatrixSubtract(const m1, m2: TMatrix4): TMatrix4;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := m1[i, j] - m2[i, j];
end;

procedure MatrixSubtractVar(var m1: TMatrix4; const m2: TMatrix4);
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      m1[i, j] -= m2[i, j];
end;

function MatrixNegate(const m1: TMatrix3): TMatrix3;
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := - m1[i, j];
end;

function MatrixNegate(const m1: TMatrix4): TMatrix4;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := - m1[i, j];
end;

function MatrixMultScalar(const m: TMatrix3; const s: TScalar): TMatrix3;
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := m[i, j]*s;
end;

function MatrixMultScalar(const m: TMatrix4; const s: TScalar): TMatrix4;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] := m[i, j]*s;
end;

function MatrixMultPoint(const m: TMatrix4;
  const pt: TVector3): TVector3;
var
  Divisor: TScalar;
begin
  { Simple implementation:
  Result := Vector3SinglePoint(MatrixMultVector(m, Vector4Single(pt))); }

  Result[0] := M[0, 0] * Pt[0] + M[1, 0] * Pt[1] + M[2, 0] * Pt[2] + M[3, 0];
  Result[1] := M[0, 1] * Pt[0] + M[1, 1] * Pt[1] + M[2, 1] * Pt[2] + M[3, 1];
  Result[2] := M[0, 2] * Pt[0] + M[1, 2] * Pt[1] + M[2, 2] * Pt[2] + M[3, 2];

  { It looks strange, but the check below usually pays off.

    Tests: 17563680 calls of this proc within Creatures.PrepareRender
    inside "The Castle", gprof says that time without this check
    is 12.01 secs and with this checks it's 8.25.

    Why ? Because in 99% of situations, the conditions "(M[0, 3] = 0) and ..."
    is true. Because that's how all usual matrices in 3D graphics
    (translation, rotation, scaling) look like.
    So usually I pay 4 comparisons (exact comparisons, not things like
    FloatsEqual) and I avoid 3 multiplications, 4 additions and
    3 divisions. }

  if not (
    (M[0, 3] = 0) and
    (M[1, 3] = 0) and
    (M[2, 3] = 0) and
    (M[3, 3] = 1)) then
  begin
    Divisor := M[0, 3] * Pt[0] + M[1, 3] * Pt[1] + M[2, 3] * Pt[2] + M[3, 3];
    if Zero(Divisor) then
      raise ETransformedResultInvalid.Create('3D point transformed by 4x4 matrix to a direction');

    Result[0] /= Divisor;
    Result[1] /= Divisor;
    Result[2] /= Divisor;
  end;
end;

function MatrixMultPoint(const m: TMatrix4; const pt: TVector2): TVector2;
var
  Res3D: TVector3;
begin
  Res3D := MatrixMultPoint(M, Vector3(Pt, 0));
  Result[0] := Res3D[0];
  Result[1] := Res3D[1];
end;

function MatrixMultDirection(const m: TMatrix4;
  const Dir: TVector3): TVector3;
var
  Divisor: TScalar;
begin
  Result[0] := M[0, 0] * Dir[0] + M[1, 0] * Dir[1] + M[2, 0] * Dir[2];
  Result[1] := M[0, 1] * Dir[0] + M[1, 1] * Dir[1] + M[2, 1] * Dir[2];
  Result[2] := M[0, 2] * Dir[0] + M[1, 2] * Dir[1] + M[2, 2] * Dir[2];

  if not (
    (M[0, 3] = 0) and
    (M[1, 3] = 0) and
    (M[2, 3] = 0) ) then
  begin
    Divisor := M[0, 3] * Dir[0] + M[1, 3] * Dir[1] + M[2, 3] * Dir[2];
    if not Zero(Divisor) then
      raise ETransformedResultInvalid.Create(Format(
        '3D direction transformed by 4x4 matrix to a point, with divisor = %f (%g), with matrix:',
        [Divisor, Divisor]) + NL + MatrixToRawStr(M, '  '));
  end;
end;

function MatrixMultDirection(const m: TMatrix4; const Dir: TVector2): TVector2;
var
  Res3D: TVector3;
begin
  Res3D := MatrixMultDirection(M, Vector3(Dir, 0));
  Result[0] := Res3D[0];
  Result[1] := Res3D[1];
end;

function MatrixMultVector(const m: TMatrix4; const v: TVector4): TVector4;
{var i, j: integer;}
begin
  {
  for i := 0 to 3 do
  begin
   Result[i] := 0;
   for j := 0 to 3 do Result[i] := Result[i] + m[j, i]*v[j];
  end;

  Code expanded for the sake of speed:}

  Result[0] := M[0, 0] * V[0] + M[1, 0] * V[1] + M[2, 0] * V[2] + M[3, 0] * V[3];
  Result[1] := M[0, 1] * V[0] + M[1, 1] * V[1] + M[2, 1] * V[2] + M[3, 1] * V[3];
  Result[2] := M[0, 2] * V[0] + M[1, 2] * V[1] + M[2, 2] * V[2] + M[3, 2] * V[3];
  Result[3] := M[0, 3] * V[0] + M[1, 3] * V[1] + M[2, 3] * V[2] + M[3, 3] * V[3];
end;

function MatrixMultVector(const m: TMatrix3; const v: TVector3): TVector3;
begin
  Result[0] := M[0, 0] * V[0] + M[1, 0] * V[1] + M[2, 0] * V[2];
  Result[1] := M[0, 1] * V[0] + M[1, 1] * V[1] + M[2, 1] * V[2];
  Result[2] := M[0, 2] * V[0] + M[1, 2] * V[1] + M[2, 2] * V[2];
end;

function MatrixMultVector(const m: TMatrix2; const v: TVector2): TVector2;
begin
  Result[0] := M[0, 0] * V[0] + M[1, 0] * V[1];
  Result[1] := M[0, 1] * V[0] + M[1, 1] * V[1];
end;

operator * (const m: Tmatrix2; const v: Tvector2): Tvector2;
begin
  Result := MatrixMultVector(M, V);
end;

operator * (const m: Tmatrix3; const v: Tvector3): Tvector3;
begin
  Result := MatrixMultVector(M, V);
end;

operator * (const m: Tmatrix4; const v: Tvector4): Tvector4;
begin
  Result := MatrixMultVector(M, V);
end;

function MatrixMult(const m1, m2: TMatrix4): TMatrix4;
{var i, j, k: integer;}
begin
(*
  FillChar(result, SizeOf(result), 0);
  for i := 0 to 3 do { i = wiersze, j = kolumny }
    for j := 0 to 3 do
      for k := 0 to 3 do
        Result[j, i] += m1[k, i]*m2[j, k];
*)

  { This is code above expanded for speed sake
    (code generated by console.testy/genMultMatrix) }
  Result[0, 0] := m1[0, 0] * m2[0, 0] + m1[1, 0] * m2[0, 1] + m1[2, 0] * m2[0, 2] + m1[3, 0] * m2[0, 3];
  Result[1, 0] := m1[0, 0] * m2[1, 0] + m1[1, 0] * m2[1, 1] + m1[2, 0] * m2[1, 2] + m1[3, 0] * m2[1, 3];
  Result[2, 0] := m1[0, 0] * m2[2, 0] + m1[1, 0] * m2[2, 1] + m1[2, 0] * m2[2, 2] + m1[3, 0] * m2[2, 3];
  Result[3, 0] := m1[0, 0] * m2[3, 0] + m1[1, 0] * m2[3, 1] + m1[2, 0] * m2[3, 2] + m1[3, 0] * m2[3, 3];
  Result[0, 1] := m1[0, 1] * m2[0, 0] + m1[1, 1] * m2[0, 1] + m1[2, 1] * m2[0, 2] + m1[3, 1] * m2[0, 3];
  Result[1, 1] := m1[0, 1] * m2[1, 0] + m1[1, 1] * m2[1, 1] + m1[2, 1] * m2[1, 2] + m1[3, 1] * m2[1, 3];
  Result[2, 1] := m1[0, 1] * m2[2, 0] + m1[1, 1] * m2[2, 1] + m1[2, 1] * m2[2, 2] + m1[3, 1] * m2[2, 3];
  Result[3, 1] := m1[0, 1] * m2[3, 0] + m1[1, 1] * m2[3, 1] + m1[2, 1] * m2[3, 2] + m1[3, 1] * m2[3, 3];
  Result[0, 2] := m1[0, 2] * m2[0, 0] + m1[1, 2] * m2[0, 1] + m1[2, 2] * m2[0, 2] + m1[3, 2] * m2[0, 3];
  Result[1, 2] := m1[0, 2] * m2[1, 0] + m1[1, 2] * m2[1, 1] + m1[2, 2] * m2[1, 2] + m1[3, 2] * m2[1, 3];
  Result[2, 2] := m1[0, 2] * m2[2, 0] + m1[1, 2] * m2[2, 1] + m1[2, 2] * m2[2, 2] + m1[3, 2] * m2[2, 3];
  Result[3, 2] := m1[0, 2] * m2[3, 0] + m1[1, 2] * m2[3, 1] + m1[2, 2] * m2[3, 2] + m1[3, 2] * m2[3, 3];
  Result[0, 3] := m1[0, 3] * m2[0, 0] + m1[1, 3] * m2[0, 1] + m1[2, 3] * m2[0, 2] + m1[3, 3] * m2[0, 3];
  Result[1, 3] := m1[0, 3] * m2[1, 0] + m1[1, 3] * m2[1, 1] + m1[2, 3] * m2[1, 2] + m1[3, 3] * m2[1, 3];
  Result[2, 3] := m1[0, 3] * m2[2, 0] + m1[1, 3] * m2[2, 1] + m1[2, 3] * m2[2, 2] + m1[3, 3] * m2[2, 3];
  Result[3, 3] := m1[0, 3] * m2[3, 0] + m1[1, 3] * m2[3, 1] + m1[2, 3] * m2[3, 2] + m1[3, 3] * m2[3, 3];
end;

function MatrixMult(const m1, m2: TMatrix3): TMatrix3;
begin
  Result[0, 0] := m1[0, 0] * m2[0, 0] + m1[1, 0] * m2[0, 1] + m1[2, 0] * m2[0, 2];
  Result[1, 0] := m1[0, 0] * m2[1, 0] + m1[1, 0] * m2[1, 1] + m1[2, 0] * m2[1, 2];
  Result[2, 0] := m1[0, 0] * m2[2, 0] + m1[1, 0] * m2[2, 1] + m1[2, 0] * m2[2, 2];
  Result[0, 1] := m1[0, 1] * m2[0, 0] + m1[1, 1] * m2[0, 1] + m1[2, 1] * m2[0, 2];
  Result[1, 1] := m1[0, 1] * m2[1, 0] + m1[1, 1] * m2[1, 1] + m1[2, 1] * m2[1, 2];
  Result[2, 1] := m1[0, 1] * m2[2, 0] + m1[1, 1] * m2[2, 1] + m1[2, 1] * m2[2, 2];
  Result[0, 2] := m1[0, 2] * m2[0, 0] + m1[1, 2] * m2[0, 1] + m1[2, 2] * m2[0, 2];
  Result[1, 2] := m1[0, 2] * m2[1, 0] + m1[1, 2] * m2[1, 1] + m1[2, 2] * m2[1, 2];
  Result[2, 2] := m1[0, 2] * m2[2, 0] + m1[1, 2] * m2[2, 1] + m1[2, 2] * m2[2, 2];
end;

function MatrixMult(const m1, m2: TMatrix2): TMatrix2;
begin
  Result[0, 0] := m1[0, 0] * m2[0, 0] + m1[1, 0] * m2[0, 1];
  Result[1, 0] := m1[0, 0] * m2[1, 0] + m1[1, 0] * m2[1, 1];
  Result[0, 1] := m1[0, 1] * m2[0, 0] + m1[1, 1] * m2[0, 1];
  Result[1, 1] := m1[0, 1] * m2[1, 0] + m1[1, 1] * m2[1, 1];
end;

operator * (const m1, m2: TMatrix2): TMatrix2;
begin
  Result := MatrixMult(M1, M2);
end;

operator * (const m1, m2: TMatrix3): TMatrix3;
begin
  Result := MatrixMult(M1, M2);
end;

operator * (const m1, m2: TMatrix4): TMatrix4;
begin
  Result := MatrixMult(M1, M2);
end;

{ matrix transforms for 3D graphics  ----------------------------------------- }

function TranslationMatrix(const Transl: TVector3): TMatrix4Single;
begin
  result := IdentityMatrix4Single;
  Result[3, 0] := Transl[0];
  Result[3, 1] := Transl[1];
  Result[3, 2] := Transl[2];
end;

function TranslationMatrix(const X, Y, Z: TScalar): TMatrix4Single;
begin
  result := IdentityMatrix4Single;
  Result[3, 0] := X;
  Result[3, 1] := Y;
  Result[3, 2] := Z;
end;

procedure TranslationMatrices(const X, Y, Z: TScalar;
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  Matrix := IdentityMatrix4Single;
  Matrix[3, 0] := X;
  Matrix[3, 1] := Y;
  Matrix[3, 2] := Z;

  InvertedMatrix := IdentityMatrix4Single;
  InvertedMatrix[3, 0] := -X;
  InvertedMatrix[3, 1] := -Y;
  InvertedMatrix[3, 2] := -Z;
end;

procedure TranslationMatrices(const Transl: TVector3;
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  Matrix := IdentityMatrix4Single;
  Matrix[3, 0] := Transl[0];
  Matrix[3, 1] := Transl[1];
  Matrix[3, 2] := Transl[2];

  InvertedMatrix := IdentityMatrix4Single;
  InvertedMatrix[3, 0] := -Transl[0];
  InvertedMatrix[3, 1] := -Transl[1];
  InvertedMatrix[3, 2] := -Transl[2];
end;

procedure MultMatrixTranslation(var M: TMatrix4;
  const Transl: TVector3);
var
  NewColumn: TVector4;
begin
  NewColumn := M[3];
  NewColumn[0] += M[0, 0] * Transl[0] + M[1, 0] * Transl[1] + M[2, 0] * Transl[2];
  NewColumn[1] += M[0, 1] * Transl[0] + M[1, 1] * Transl[1] + M[2, 1] * Transl[2];
  NewColumn[2] += M[0, 2] * Transl[0] + M[1, 2] * Transl[1] + M[2, 2] * Transl[2];
  NewColumn[3] += M[0, 3] * Transl[0] + M[1, 3] * Transl[1] + M[2, 3] * Transl[2];
  M[3] := NewColumn;
end;

procedure MultMatricesTranslation(var M, MInvert: TMatrix4;
  const Transl: TVector3);
var
  NewColumn: TVector4;
  { OldLastRow may use the same space as NewColumn }
  OldLastRow: TVector4 absolute NewColumn;
begin
  NewColumn := M[3];
  NewColumn[0] += M[0, 0] * Transl[0] + M[1, 0] * Transl[1] + M[2, 0] * Transl[2];
  NewColumn[1] += M[0, 1] * Transl[0] + M[1, 1] * Transl[1] + M[2, 1] * Transl[2];
  NewColumn[2] += M[0, 2] * Transl[0] + M[1, 2] * Transl[1] + M[2, 2] * Transl[2];
  NewColumn[3] += M[0, 3] * Transl[0] + M[1, 3] * Transl[1] + M[2, 3] * Transl[2];
  M[3] := NewColumn;

  OldLastRow[0] := MInvert[0, 3];
  OldLastRow[1] := MInvert[1, 3];
  OldLastRow[2] := MInvert[2, 3];
  OldLastRow[3] := MInvert[3, 3];

  MInvert[0, 0] -= Transl[0] * OldLastRow[0];
  MInvert[1, 0] -= Transl[0] * OldLastRow[1];
  MInvert[2, 0] -= Transl[0] * OldLastRow[2];
  MInvert[3, 0] -= Transl[0] * OldLastRow[3];
  MInvert[0, 1] -= Transl[1] * OldLastRow[0];
  MInvert[1, 1] -= Transl[1] * OldLastRow[1];
  MInvert[2, 1] -= Transl[1] * OldLastRow[2];
  MInvert[3, 1] -= Transl[1] * OldLastRow[3];
  MInvert[0, 2] -= Transl[2] * OldLastRow[0];
  MInvert[1, 2] -= Transl[2] * OldLastRow[1];
  MInvert[2, 2] -= Transl[2] * OldLastRow[2];
  MInvert[3, 2] -= Transl[2] * OldLastRow[3];
end;

function TransformToCoords(const V, NewX, NewY, NewZ: TVector3): TVector3;
begin
  Result[0] := V[0] * NewX[0] + V[1] * NewY[0] + V[2] * NewZ[0];
  Result[1] := V[0] * NewX[1] + V[1] * NewY[1] + V[2] * NewZ[1];
  Result[2] := V[0] * NewX[2] + V[1] * NewY[2] + V[2] * NewZ[2];
end;

function TransformToCoordsMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4Single;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Result[0, i] := NewX[i];
    Result[1, i] := NewY[i];
    Result[2, i] := NewZ[i];
    Result[3, i] := NewOrigin[i];
  end;
  { bottom row }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function TransformToCoordsMatrix(const NewX, NewY, NewZ: TVector3): TMatrix4Single;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Result[0, i] := NewX[i];
    Result[1, i] := NewY[i];
    Result[2, i] := NewZ[i];
  end;
  { right column }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;
  { bottom row }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function TransformToCoordsNoScaleMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4Single;
begin
  result := TransformToCoordsMatrix(NewOrigin,
    Normalized(NewX), Normalized(NewY), Normalized(NewZ));
end;

function TransformFromCoordsMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4Single;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    { Difference between TrasformToCoords and TransformFromCoords:
      up-left 3x3 matrix is applied in a transposed manner,
      compared with TrasformToCoords. }
    Result[i, 0] := OldX[i];
    Result[i, 1] := OldY[i];
    Result[i, 2] := OldZ[i];
  end;

  { Another difference between TrasformToCoords and TransformFromCoords:
    - OldOrigin must be negated here
    - OldOrigin must have directions applied
    See e.g. Global Illumination Compendium by Philip Dutre, section (15). }
  Result[3, 0] := -VectorDotProduct(OldOrigin, OldX);
  Result[3, 1] := -VectorDotProduct(OldOrigin, OldY);
  Result[3, 2] := -VectorDotProduct(OldOrigin, OldZ);

  { bottom row }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function TransformFromCoordsMatrix(const OldX, OldY, OldZ: TVector3): TMatrix4Single;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    { Difference between TrasformToCoords and TransformFromCoords:
      up-left 3x3 matrix is applied in a transposed manner,
      compared with TrasformToCoords. }
    Result[i, 0] := OldX[i];
    Result[i, 1] := OldY[i];
    Result[i, 2] := OldZ[i];
  end;

  { right column }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;
  { bottom row }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function TransformFromCoordsNoScaleMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4Single;
begin
  result := TransformFromCoordsMatrix(OldOrigin,
    Normalized(OldX), Normalized(OldY), Normalized(OldZ));
end;

procedure TransformCoordsMatrices(const NewX, NewY, NewZ: TVector3;
  out ToCoords, FromCoords: TMatrix4Single);
begin
  ToCoords := TransformToCoordsMatrix(NewX, NewY, NewZ);
  FromCoords := TransformFromCoordsMatrix(NewX, NewY, NewZ);
end;

function LookAtMatrix(const Eye, Center, Up: TVector3): TMatrix4Single;
begin
  result := LookDirMatrix(Eye, VectorSubtract(Center, Eye), Up);
end;

function LookDirMatrix(const Eye, Dir, Up: TVector3): TMatrix4Single;
var
  GoodDir, GoodUp, Side: TVector3;
begin
  Side := Normalized(VectorProduct(Dir, Up));
  GoodDir := Normalized(Dir);

  { Recalculate GoodUp from Side and GoodDir. This way:
    1. We make sure GoodUp is orthogonal to Side and GoodDir.
       The Up was already orthogonal to Side,
       but it was not necessarily orthogonal to GoodDir.
    2. We make sure GoodUp is normalized (vector product of normalized
       vectors is also normalized).
    This is done looking at gluLookAt implementation in
    SGI Sample OpenGL Implementation. }
  GoodUp := VectorProduct(Side, GoodDir);

  Result := LookDirMatrix(Eye, GoodDir, Side, GoodUp);
end;

function LookDirMatrix(const Eye, Dir, Side, Up: TVector3): TMatrix4Single;
begin
  { VectorNegate on Dir, since in right-handed coordinate system
    you look in the -Z direction. }
  Result := TransformFromCoordsMatrix(Eye, Side, Up, VectorNegate(Dir));
end;

function FastLookDirMatrix(const Direction, Up: TVector3): TMatrix4Single;
var
  Side: TVector3;
  i: integer;
begin
  Side := VectorProduct(Direction, Up);

  { Make TransformToCoordsMatrix with origin zero now. }

  for i := 0 to 2 do
  begin
    Result[i, 0] := Side[i];
    Result[i, 1] := Up[i];
    Result[i, 2] := -Direction[i]; { negate Direction, since it goes to -Z }
  end;

  { bottom row and right column }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;

  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;

  Result[3, 3] := 1;
end;

function InverseFastLookDirMatrix(const Direction, Up: TVector3): TMatrix4Single;
var
  Side: TVector3;
  i: integer;
begin
  Side := VectorProduct(Direction, Up);

  { Inverse of LookDirMatrix is now to make
    TransformToCoordsMatrix with origin zero. }

  for i := 0 to 2 do
  begin
    Result[0, i] := Side[i];
    Result[1, i] := Up[i];
    Result[2, i] := -Direction[i]; { negate Direction, since it goes to -Z }
  end;

  { bottom row and right column }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;

  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;

  Result[3, 3] := 1;
end;

{ We're happily using FPC's Matrix unit to trivially implement
  matrix determinant and inverse. }

function MatrixDeterminant(const M: TMatrix2): TScalar;
var
  MM: TMatrix2_;
begin
  { Note that generally data should be transposed between
    TMatrix2_ and TMatrix2. But in this case, it's not needed,
    as the determinant of the transposition is exactly the same. }

  MM.Data := M;
  Result := MM.Determinant;
end;

function MatrixDeterminant(const M: TMatrix3): TScalar;
var
  MM: TMatrix3_;
begin
  MM.Data := M;
  Result := MM.Determinant;
end;

function MatrixDeterminant(const M: TMatrix4): TScalar;
var
  MM: TMatrix4_;
begin
  MM.Data := M;
  Result := MM.Determinant;
end;

function MatrixInverse(const M: TMatrix2; const Determinant: TScalar): TMatrix2;
var
  MM: TMatrix2_;
begin
  { Note that generally data should be transposed between
    TMatrix2_ and TMatrix2. But in this case, it's not needed,
    as the transpose of the inverse is the inverse of the transpose.
    Which means that
      Result = transpose(inverse(transpose(m))
             = transpose(transpose(inverse(m))) = just inverse(m)) }

  MM.Data := M;
  Result := MM.Inverse(Determinant).Data;
end;

function MatrixInverse(const M: TMatrix3; const Determinant: TScalar): TMatrix3;
var
  MM: TMatrix3_;
begin
  MM.Data := M;
  Result := MM.Inverse(Determinant).Data;
end;

function MatrixInverse(const M: TMatrix4; const Determinant: TScalar): TMatrix4;
var
  MM: TMatrix4_;
begin
  MM.Data := M;
  Result := MM.Inverse(Determinant).Data;
end;

function MatrixRow(const m: TMatrix2; const Row: Integer): TVector2;
begin
  Result[0] := M[0][Row];
  Result[1] := M[1][Row];
end;

function MatrixRow(const m: TMatrix3; const Row: Integer): TVector3;
begin
  Result[0] := M[0][Row];
  Result[1] := M[1][Row];
  Result[2] := M[2][Row];
end;

function MatrixRow(const m: TMatrix4; const Row: Integer): TVector4;
begin
  Result[0] := M[0][Row];
  Result[1] := M[1][Row];
  Result[2] := M[2][Row];
  Result[3] := M[3][Row];
end;

procedure MatrixTransposeVar(var M: TMatrix3);
var
  Tmp: TScalar;
begin
  Tmp := M[0, 1]; M[0, 1] := M[1, 0]; M[1, 0] := Tmp;
  Tmp := M[0, 2]; M[0, 2] := M[2, 0]; M[2, 0] := Tmp;
  Tmp := M[1, 2]; M[1, 2] := M[2, 1]; M[2, 1] := Tmp;
end;

function MatrixTranspose(const M: TMatrix3): TMatrix3;
begin
  Result[0, 0] := M[0, 0];
  Result[0, 1] := M[1, 0];
  Result[0, 2] := M[2, 0];

  Result[1, 0] := M[0, 1];
  Result[1, 1] := M[1, 1];
  Result[1, 2] := M[2, 1];

  Result[2, 0] := M[0, 2];
  Result[2, 1] := M[1, 2];
  Result[2, 2] := M[2, 2];
end;

function MatrixTranspose(const M: TMatrix4): TMatrix4;
begin
  Result[0, 0] := M[0, 0];
  Result[0, 1] := M[1, 0];
  Result[0, 2] := M[2, 0];
  Result[0, 3] := M[3, 0];

  Result[1, 0] := M[0, 1];
  Result[1, 1] := M[1, 1];
  Result[1, 2] := M[2, 1];
  Result[1, 3] := M[3, 1];

  Result[2, 0] := M[0, 2];
  Result[2, 1] := M[1, 2];
  Result[2, 2] := M[2, 2];
  Result[2, 3] := M[3, 2];

  Result[3, 0] := M[0, 3];
  Result[3, 1] := M[1, 3];
  Result[3, 2] := M[2, 3];
  Result[3, 3] := M[3, 3];
end;

function ModelViewToNormalMatrix(const M: TMatrix4): TMatrix3;
var
  D: TScalar;
  M3: TMatrix3;
begin
  Move(M[0], M3[0], SizeOf(TVector3));
  Move(M[1], M3[1], SizeOf(TVector3));
  Move(M[2], M3[2], SizeOf(TVector3));
  D := MatrixDeterminant(M3);
  if Zero(D) then
    { modelview not invertible, nothing good to do but we have to continue
      without errors, since this can happen with arbitrary 3D scenes. }
    Result := M3 else
  begin
    Result := MatrixInverse(M3, D);
    MatrixTransposeVar(Result);
  end;
end;

function VectorMultTransposedSameVector(const v: TVector3): TMatrix4;
begin
  (* Naive version:

  for i := 0 to 2 do { i = column, j = row }
    for j := 0 to 2 do
      result[i, j] := v[i]*v[j];

  Expanded and optimized version below. *)

  result[0, 0] := sqr(v[0]);
  result[1, 1] := sqr(v[1]);
  result[2, 2] := sqr(v[2]);

  result[0, 1] := v[0]*v[1]; result[1, 0] := result[0, 1];
  result[0, 2] := v[0]*v[2]; result[2, 0] := result[0, 2];
  result[1, 2] := v[1]*v[2]; result[2, 1] := result[1, 2];

  { Fill the last row and column like an identity matrix }
  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;

  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;

  Result[3, 3] := 1;
end;

function ScalingMatrix(const ScaleFactor: TVector3): TMatrix4;
begin
  result := IdentityMatrix4;
  result[0, 0] := ScaleFactor[0];
  result[1, 1] := ScaleFactor[1];
  result[2, 2] := ScaleFactor[2];
end;

procedure ScalingMatrices(const ScaleFactor: TVector3;
  InvertedMatrixIdentityIfNotExists: boolean;
  out Matrix, InvertedMatrix: TMatrix4);
begin
  Matrix := IdentityMatrix4;
  Matrix[0, 0] := ScaleFactor[0];
  Matrix[1, 1] := ScaleFactor[1];
  Matrix[2, 2] := ScaleFactor[2];

  InvertedMatrix := IdentityMatrix4;
  if not
    (InvertedMatrixIdentityIfNotExists and
      ( Zero(ScaleFactor[0]) or
        Zero(ScaleFactor[1]) or
        Zero(ScaleFactor[2]) )) then
  begin
    InvertedMatrix[0, 0] := 1 / ScaleFactor[0];
    InvertedMatrix[1, 1] := 1 / ScaleFactor[1];
    InvertedMatrix[2, 2] := 1 / ScaleFactor[2];
  end;
end;

function RotationMatrixRad(const AngleRad: TScalar;
  const Axis: TVector3): TMatrix4;
var
  NormAxis: TVector3;
  AngleSin, AngleCos: Float;
  S, C: TScalar;
begin
  NormAxis := Normalized(Axis);

  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to TScalar once }
  S := AngleSin;
  C := AngleCos;

  Result := VectorMultTransposedSameVector(NormAxis);

  { We do not touch the last column and row of Result in the following code,
    treating Result like a 3x3 matrix. The last column and row are already Ok. }

  { Expanded Result := Result + (IdentityMatrix3 - Result) * AngleCos; }
  Result[0, 0] += (1 - Result[0, 0]) * C;
  Result[1, 0] +=    - Result[1, 0]  * C;
  Result[2, 0] +=    - Result[2, 0]  * C;

  Result[0, 1] +=    - Result[0, 1]  * C;
  Result[1, 1] += (1 - Result[1, 1]) * C;
  Result[2, 1] +=    - Result[2, 1]  * C;

  Result[0, 2] +=    - Result[0, 2]  * C;
  Result[1, 2] +=    - Result[1, 2]  * C;
  Result[2, 2] += (1 - Result[2, 2]) * C;

  NormAxis[0] *= S;
  NormAxis[1] *= S;
  NormAxis[2] *= S;

  { Add M3 (from OpenGL matrix equations) }
  Result[1, 0] += -NormAxis[2];
  Result[2, 0] +=  NormAxis[1];

  Result[0, 1] +=  NormAxis[2];
  Result[2, 1] += -NormAxis[0];

  Result[0, 2] += -NormAxis[1];
  Result[1, 2] +=  NormAxis[0];
end;

procedure RotationMatricesRad(const AxisAngle: TVector4;
  out Matrix, InvertedMatrix: TMatrix4);
var
  Axis: TVector3 absolute AxisAngle;
begin
  RotationMatricesRad(AxisAngle[3], Axis, Matrix, InvertedMatrix);
end;

procedure RotationMatricesRad(const AngleRad: TScalar;
  const Axis: TVector3;
  out Matrix, InvertedMatrix: TMatrix4);
var
  NormAxis: TVector3;
  V: TScalar;
  AngleSin, AngleCos: Float;
  S, C: TScalar;
begin
  NormAxis := Normalized(Axis);

  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to TScalar once }
  S := AngleSin;
  C := AngleCos;

  Matrix := VectorMultTransposedSameVector(NormAxis);

  { We do not touch the last column and row of Matrix in the following code,
    treating Matrix like a 3x3 matrix. The last column and row are already Ok. }

  { Expanded Matrix := Matrix + (IdentityMatrix3 - Matrix) * AngleCos; }
  Matrix[0, 0] += (1 - Matrix[0, 0]) * C;
  Matrix[1, 0] +=    - Matrix[1, 0]  * C;
  Matrix[2, 0] +=    - Matrix[2, 0]  * C;

  Matrix[0, 1] +=    - Matrix[0, 1]  * C;
  Matrix[1, 1] += (1 - Matrix[1, 1]) * C;
  Matrix[2, 1] +=    - Matrix[2, 1]  * C;

  Matrix[0, 2] +=    - Matrix[0, 2]  * C;
  Matrix[1, 2] +=    - Matrix[1, 2]  * C;
  Matrix[2, 2] += (1 - Matrix[2, 2]) * C;

  { Up to this point, calculated Matrix is also good for InvertedMatrix }
  InvertedMatrix := Matrix;

  NormAxis[0] *= S;
  NormAxis[1] *= S;
  NormAxis[2] *= S;

  { Now add M3 to Matrix, and subtract M3 from InvertedMatrix.
    That's because for the inverted rotation, AngleSin is negated,
    so the M3 should be subtracted. }
  V := -NormAxis[2]; Matrix[1, 0] += V; InvertedMatrix[1, 0] -= V;
  V :=  NormAxis[1]; Matrix[2, 0] += V; InvertedMatrix[2, 0] -= V;

  V :=  NormAxis[2]; Matrix[0, 1] += V; InvertedMatrix[0, 1] -= V;
  V := -NormAxis[0]; Matrix[2, 1] += V; InvertedMatrix[2, 1] -= V;

  V := -NormAxis[1]; Matrix[0, 2] += V; InvertedMatrix[0, 2] -= V;
  V :=  NormAxis[0]; Matrix[1, 2] += V; InvertedMatrix[1, 2] -= V;
end;

function RotationMatrixDeg(const AngleDeg: TScalar; const Axis: TVector3): TMatrix4;
begin
  result := RotationMatrixRad(DegToRad(AngleDeg), Axis);
end;

function RotationMatrixDeg(const AngleDeg: TScalar;
  const AxisX, AxisY, AxisZ: TScalar): TMatrix4;
begin
  result := RotationMatrixRad(DegToRad(AngleDeg), Vector3(AxisX, AxisY, AxisZ));
end;

function RotationMatrixRad(const AngleRad: TScalar;
  const AxisX, AxisY, AxisZ: TScalar): TMatrix4;
begin
  result := RotationMatrixRad(AngleRad, Vector3(AxisX, AxisY, AxisZ));
end;

{ kod dla MatrixDet* przerobiony z vect.c z mgflib }

function MatrixDet4x4(const mat: TMatrix4): TScalar;
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: TScalar;
begin
  a1 := mat[0][0]; b1 := mat[0][1];
  c1 := mat[0][2]; d1 := mat[0][3];

  a2 := mat[1][0]; b2 := mat[1][1];
  c2 := mat[1][2]; d2 := mat[1][3];

  a3 := mat[2][0]; b3 := mat[2][1];
  c3 := mat[2][2]; d3 := mat[2][3];

  a4 := mat[3][0]; b4 := mat[3][1];
  c4 := mat[3][2]; d4 := mat[3][3];

  result := a1 * MatrixDet3x3 (b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * MatrixDet3x3 (a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * MatrixDet3x3 (a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * MatrixDet3x3 (a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;


function MatrixDet3x3(const a1, a2, a3, b1, b2, b3, c1, c2, c3: TScalar): TScalar;
begin
  result := a1 * MatrixDet2x2 (b2, b3, c2, c3)
          - b1 * MatrixDet2x2 (a2, a3, c2, c3)
          + c1 * MatrixDet2x2 (a2, a3, b2, b3);
end;

function MatrixDet2x2(const a, b, c, d: TScalar): TScalar;
begin
  result := a * d - b * c;
end;

{$endif read_implementation}
