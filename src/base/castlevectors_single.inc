{%MainUnit castlevectors.pas}
{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleVectors routines (global functions, procedures)
  for Single precision of vectors and matrices. }

{$ifdef read_interface}

type
  TGetVertexFromIndexFunc = function (Index: integer): TVector3 of object;

{ Cosinus of angle between two vectors.

  CosAngleBetweenNormals is a little faster, but must receive
  normalized (length 1) vectors. This avoids expensive Sqrt
  inside CosAngleBetweenVectors.

  @raises EVectorInvalidOp If V1 or V2 is zero.
  @groupBegin }
function CosAngleBetweenVectors(const V1, V2: TVector3): Single; overload;
function CosAngleBetweenNormals(const V1, V2: TVector3): Single; overload;
{ @groupEnd }

{ Angle between two vectors, in radians.
  Returns always positive angle, between 0 and Pi.

  AngleRadBetweenNormals is a little faster, but must receive
  normalized (length 1) vectors. This avoids expensive Sqrt.
  See also CosAngleBetweenVectors and CosAngleBetweenNormals
  to avoid expensive ArcCos.

  @raises EVectorInvalidOp If V1 or V2 is zero.
  @groupBegin }
function AngleRadBetweenVectors(const V1, V2: TVector3): Single; overload;
function AngleRadBetweenNormals(const V1, V2: TVector3): Single; overload;
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
function RotationAngleRadBetweenVectors(const V1, V2: TVector3): Single; overload;
function RotationAngleRadBetweenVectors(const V1, V2, Cross: TVector3): Single; overload;
{ @groupEnd }

{ Rotate Point around the Axis by given Angle.
  Axis does not have to be normalized (it will be normalized internally).

  Axis can be an exact zero only if Angle is also an exact zero.
  This special case is supported to make it easier to initialize stuff.
  This way by default a vector and an angle can be left filled with zeros
  (like @link(TCastleTransform.Rotation)), and this function will accept
  them (in effect e.g. @link(TCastleTransform.Direction) will be valid).
  In all other cases, axis must be a non-zero vector.

  Note that this is equivalent to constructing a rotation matrix
  and then using it, like

  @longCode(#
    M := RotationMatrixRad(Angle, Axis);
    Result := M.MultPoint(Point);
  #)

  Except this will be a little faster.

  Rotations are done in the same direction as RotationMatrixRad, and as OpenGL.

  @groupBegin }
function RotatePointAroundAxisRad(const Angle: Single; const Point: TVector3; const Axis: TVector3): TVector3; overload;
function RotatePointAroundAxisDeg(const Angle: Single; const Point: TVector3; const Axis: TVector3): TVector3; overload;
  deprecated 'use radians for everything throughout CGE';
{ @groupEnd }

{ Rotate Point around the Axis by 90 degrees.
  This is specialized and optimized version of RotatePointAroundAxisRad. }
function RotatePointAroundAxis90(const Point: TVector3; const Axis: TVector3): TVector3;

{ Rotate Point around the Axis by -90 degrees.
  This is specialized and optimized version of RotatePointAroundAxisRad. }
function RotatePointAroundAxisMinus90(const Point: TVector3; const Axis: TVector3): TVector3;

{ Rotate Point around the given axis by a given angle.
  Axis is specified in the first 3 components of AxisAngle,
  rotation angle (in radians) is specified in the last component of AxisAngle. }
function RotatePointAroundAxis(const AxisAngle: TVector4; const Point: TVector3): TVector3;

{ Which coordinate (0, 1, 2, and eventually 3 for 4D versions) is the largest.
  When the vector components are equal, the first one "wins", for example
  if V[0] = V[1] (and are larger than other vector component) we return 0.
  MaxAbsVectorCoord compares the absolute value of components.
  @groupBegin }
function MaxVectorCoord(const V: TVector2): Integer; overload;
function MaxVectorCoord(const V: TVector3): T3DAxis; overload;
function MaxVectorCoord(const V: TVector4): Integer; overload;
function MaxAbsVectorCoord(const V: TVector2): Integer; overload;
function MaxAbsVectorCoord(const V: TVector3): T3DAxis; overload;
function MaxAbsVectorCoord(const V: TVector4): Integer; overload;
{ @groupEnd }

function MinAbsVectorCoord(const V: TVector2): Integer; overload;
function MinAbsVectorCoord(const V: TVector3): T3DAxis; overload;
function MinAbsVectorCoord(const V: TVector4): Integer; overload;

function MinVectorCoord(const V: TVector3): T3DAxis; overload;

procedure SortAbsVectorCoord(const V: TVector3; out Max, Middle, Min: T3DAxis); overload;

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

{ Endianess swapping for vectors. }
function SwapEndian(const V: TVector2): TVector2; overload;
function SwapEndian(const V: TVector3): TVector3; overload;
function SwapEndian(const V: TVector4): TVector4; overload;

{ Endianess conversion for vectors (little endian to native). }
function LEtoN(const V: TVector2): TVector2; overload;
function LEtoN(const V: TVector3): TVector3; overload;
function LEtoN(const V: TVector4): TVector4; overload;

{ Endianess conversion for vectors (big endian to native). }
function BEtoN(const V: TVector2): TVector2; overload;
function BEtoN(const V: TVector3): TVector3; overload;
function BEtoN(const V: TVector4): TVector4; overload;

{ Endianess conversion for vectors (native to little endian). }
function NtoLE(const V: TVector2): TVector2; overload;
function NtoLE(const V: TVector3): TVector3; overload;
function NtoLE(const V: TVector4): TVector4; overload;

{ Endianess conversion for vectors (native to big endian). }
function NtoBE(const V: TVector2): TVector2; overload;
function NtoBE(const V: TVector3): TVector3; overload;
function NtoBE(const V: TVector4): TVector4; overload;

{ Replace contents of 2 variables. }
procedure SwapValues(var V1, V2: TVector2); overload;
procedure SwapValues(var V1, V2: TVector3); overload;
procedure SwapValues(var V1, V2: TVector4); overload;

{ This normalizes Plane by scaling all @italic(four) coordinates of Plane
  so that length of plane vector (taken from 1st @italic(three) coordinates)
  is one.

  Also, contrary to normal NormalizeVar on 3-component vectors,
  this will fail with some awful error (like floating point overflow)
  in case length of plane vector is zero. That's because we know
  that plane vector @italic(must) be always non-zero. }
procedure NormalizePlaneVar(var V: TVector4); overload;

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

function PointsDistance(const V1, V2: TVector2): Single; overload;
function PointsDistance(const V1, V2: TVector3): Single; overload;
function PointsDistanceSqr(const V1, V2: TVector2): Single; overload;
function PointsDistanceSqr(const V1, V2: TVector3): Single; overload;

{ Distance between points projected on the 2D plane.
  Projection is done by rejecting IgnoreIndex coordinate (must be 0, 1 or 2).
  @groupBegin }
function PointsDistance2DSqr(const V1, V2: TVector3; const IgnoreIndex: Integer): Single; overload;
{ @groupEnd }

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

  We make it such that V1 rotated around axis TVector3.CrossProduct(V1, V2) by given
  angle will result in V2. Note that this means that
  @code(MakeVectorsAngleRadOnTheirPlane(V1, V2, Angle, ...))
  results in the same (not reversed) relation between vectors as
  @code(MakeVectorsAngleRadOnTheirPlane(V2, V1, Angle, ...)).
  That's because you change the arguments order, but also TVector3.CrossProduct
  sign changes.
  @groupBegin }
procedure MakeVectorsAngleRadOnTheirPlane(var v1: TVector3;
  const v2: TVector3; const AngleRad: Single;
  const ResultWhenParallel: TVector3); overload;
{ @groupEnd }

{ Adjust the V1 vector to force V1 and V2 to be orthogonal,
  but keep the 3D plane determined by V1 and V2 unchanged.
  When vectors are parallel, we set V1 to be AnyOrthogonalVector(V2). }
procedure MakeVectorsOrthoOnTheirPlane(var v1: TVector3; const v2: TVector3); overload;

{ Adjust and return the V1 vector to force V1 and V2 to be orthogonal,
  but keep the 3D plane determined by V1 and V2 unchanged.
  When vectors are parallel, we return AnyOrthogonalVector(V2).

  This is a functional version of MakeVectorsOrthoOnTheirPlane. }
function MakeVectorOrthogonal(const V1, V2: TVector3): TVector3;

{ Return, deterministically, some vector orthogonal to V.
  When V is non-zero, then the result is non-zero.
  @groupBegin }
function AnyOrthogonalVector(const V: TVector2): TVector2; overload;
function AnyOrthogonalVector(const V: TVector3): TVector3; overload;
{ @groupEnd }

function IsLineParallelToPlane(const lineVector: TVector3; const plane: TVector4): boolean; overload;

function IsLineParallelToSimplePlane(const lineVector: TVector3;
  const PlaneConstCoord: T3DAxis): boolean; overload;

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
  const Plane: TVector4): Single; overload;

{ Distance from a point to a plane (with already normalized direction).

  Note: distance of the plane from origin point (0,0,0) may be simply
  obtained by Abs(Plane[3]) when Plane is Normalized.
  @groupBegin }
function PointToNormalizedPlaneDistance(const Point: TVector3;
  const Plane: TVector4): Single; overload;
{ @groupEnd }

{ Distance from a point to a plane.

  Note that calculating this costs you one Sqrt
  (contrary to PointToPlaneDistanceSqr or
  PointToNormalizedPlaneDistance).

  @groupBegin }
function PointToPlaneDistance(const Point: TVector3;
  const Plane: TVector4): Single; overload;
{ @groupEnd }

function PointToSimplePlaneDistance(const point: TVector3;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single): Single; overload;

function PointOnLineClosestToPoint(const line0, lineVector, point: TVector2): TVector2; overload;
function PointOnLineClosestToPoint(const line0, lineVector, point: TVector3): TVector3; overload;

function PointToLineDistanceSqr(const point, line0, lineVector: TVector3): Single; overload;

{ Plane and line intersection.

  Returns @false and doesn't modify Intersection or T when
  the line is parallel to the plane (this includes the case when
  the line @italic(lies on a plane), so theoretically the whole
  line is an intersection).

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = Line0 + LineVector * T).
  T is always within [0,1] range.
  @groupBegin }
function TryPlaneLineIntersection(out intersection: TVector3;
  const plane: TVector4; const line0, lineVector: TVector3): boolean; overload;
function TryPlaneLineIntersection(out t: Single;
  const plane: TVector4; const line0, lineVector: TVector3): boolean; overload;
{ @groupEnd }

{ Plane and ray intersection.

  Returns @false and doesn't modify Intersection or T when
  the ray is parallel to the plane (this includes the case when
  the ray @italic(lies on a plane). Also returns @false when the ray would
  have to point in the opposite direction to hit the plane.

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = RayOrigin + RayDirection * T).
  T is always >= 0.
  @groupBegin }
function TrySimplePlaneRayIntersection(out Intersection: TVector3;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
function TrySimplePlaneRayIntersection(out Intersection: TVector3; out T: Single;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
function TrySimplePlaneRayIntersection(out T: Single;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3): boolean; overload;

function TryPlaneRayIntersection(out Intersection: TVector3;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean; overload;
function TryPlaneRayIntersection(out Intersection: TVector3; out T: Single;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean; overload;
{ @groupEnd }

{ Plane and line segment intersection.

  Returns @false and doesn't modify Intersection or T when
  the segment is parallel to the plane (this includes the case when
  the segment @italic(lies on a plane). Also returns @false when the segment
  would have to be longer to hit the plane.

  Otherwise, returns @true, and calculates 3D intersection point,
  or calculates T such that @code(3D intersection = Segment0 + SegmentVector * T).
  T is always in range [0,1].
  @groupBegin }
function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3; out T: Single;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3): boolean; overload;
function TrySimplePlaneSegmentIntersection(
  out T: Single;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3): boolean; overload;

function TryPlaneSegmentDirIntersection(out Intersection: TVector3;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean; overload;
function TryPlaneSegmentDirIntersection(out Intersection: TVector3; out T: Single;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean; overload;
{ @groupEnd }

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector2): boolean; overload;
function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector3): boolean; overload;

{ Coefficients of the 2D line equation (Ax + By + C = 0),
  given 2 @italic(different) points lying on the line.
  When the points are equal, this is undefined. }
function Line2DFrom2Points(const P1, P2: TVector2): TVector3;
function LineOfTwoDifferentPoints2D(const P1, P2: TVector2): TVector3; deprecated 'use Line2DFrom2Points';

{ Coefficients of the 2D line equation (Ax + By + C = 0),
  given 1 point lying on the line, and non-zero line direction.
  When LineVector is zero, this is undefined. }
function Line2DFromOriginVector(const Line0, LineVector: TVector2): TVector3;

function PointToSegmentDistanceSqr(const point, pos1, pos2: TVector3): Single; overload;

{ Transform plane by a matrix.

  @raises(ETransformedResultInvalid Raised when matrix
  will transform some point to a direction, or direction to point,
  in homogeneous coordinates.)

  @groupBegin }
function PlaneTransform(const Plane: TVector4; const Matrix: TMatrix4): TVector4;
{ @groupEnd }

function IsTunnelSphereCollision(const Tunnel1, Tunnel2: TVector3;
  const TunnelRadius: Single; const SphereCenter: TVector3;
  const SphereRadius: Single): boolean; overload;

function IsSpheresCollision(const Sphere1Center: TVector3; const Sphere1Radius: Single;
  const Sphere2Center: TVector3; const Sphere2Radius: Single): boolean; overload;

function IsSegmentSphereCollision(const pos1, pos2, SphereCenter: TVector3;
  const SphereRadius: Single): boolean; overload;

function TrySphereRayIntersection(out Intersection: TVector3;
  const SphereCenter: TVector3; const SphereRadius: Single;
  const RayOrigin, RayDirection: TVector3): boolean; overload;

{ Intersection between an (infinitely tall) cylinder and a ray.
  @groupBegin }
function TryCylinderRayIntersection(out Intersection: TVector3;
  const CylinderAxisOrigin, CylinderAxis: TVector3;
  const CylinderRadius: Single;
  const RayOrigin, RayDirection: TVector3): boolean; overload;
{ @groupEnd }

{ Given two 3D lines (both given as an some point + direction in 3D),
  calculate closest point on 1st line to the 2nd line.
  Returns @false if lines are parallel, thus is not possible to determine any single
  closest point. }
function PointOnLineClosestToLine(
  out Intersection: TVector3;
  const Line1Origin, Line1Vector, Line2Origin, Line2Vector: TVector3): Boolean;

{ Matrix operations ---------------------------------------------------------- }

{ Functions to create common 4x4 matrices used in 3D graphics.

  These functions generate the same matrices as old OpenGL fixed-function was using.
  They work in right-handed coordinate system, just like X3D, glTF and OpenGL.

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
function TranslationMatrix(const X, Y, Z: Single): TMatrix4; overload;
function TranslationMatrix(const Transl: TVector3): TMatrix4; overload;

procedure TranslationMatrices(const X, Y, Z: Single; out Matrix, InvertedMatrix: TMatrix4); overload;
procedure TranslationMatrices(const Transl: TVector3; out Matrix, InvertedMatrix: TMatrix4); overload;

{ Multiply matrix M by translation matrix.

  This is equivalent to M := M * TranslationMatrix(Transl),
  but it works much faster since TranslationMatrix is a very simple matrix
  and multiplication by it may be much optimized.

  An additional speedup comes from the fact that the result is placed
  back in M (so on places where M doesn't change (and there's a lot
  of them for multiplication with translation matrix) there's no useless
  copying).

  MultMatricesTranslation is analogous to calculating both
  TranslationMatrix(Transl) and it's inverse, and then

  @longCode(#
    M := M * translation;
    MInvert := inverted-translation * MInvert;
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
  NewX, NewY, NewZ: TVector3): TMatrix4; overload;
function TransformToCoordsMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4; overload;
function TransformToCoordsNoScaleMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4; overload;

function TransformFromCoordsMatrix(const
  OldX, OldY, OldZ: TVector3): TMatrix4; overload;
function TransformFromCoordsMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4; overload;
function TransformFromCoordsNoScaleMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4; overload;
{ @groupEnd }

{ Calculate matrix to convert to given coordinate system
  (like TransformToCoordsMatrix) and it's inverse
  (like TransformFromCoordsMatrix).

  @groupBegin }
procedure TransformCoordsMatrices(const NewX, NewY, NewZ: TVector3;
  out ToCoords, FromCoords: TMatrix4); overload;
{ @groupEnd }

{ Transform vector into new coordinate space.

  Equivalent to @code(TransformToCoordsMatrix(TVector3.Zero,
  NewX, NewY, NewZ).MultPoint(V)). So the origin of new coordinate system is at the same
  place. You should pass NewX, NewY, NewZ vectors normalized if you want to
  preserve vector length.
  @groupBegin }
function TransformToCoords(const V, NewX, NewY, NewZ: TVector3): TVector3;
{ @groupEnd }

type
  { Express a 3D transformation as position, direction and up 3D vectors.
    Useful especially for cameras. }
  TViewVectors = record
    Translation, Direction, Up: TVector3;
    function Matrix: TMatrix4;
    function RotationMatrix: TMatrix4;
  end;

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
function LookAtMatrix(const Eye, Center, Up: TVector3): TMatrix4; overload;
function LookDirMatrix(const Eye, Dir, Up: TVector3): TMatrix4; overload;
function LookDirMatrix(const Eye, Dir, Side, Up: TVector3): TMatrix4; overload;
{ @groupEnd }

{ Calculate LookDirMatrix (or it's inverse), fast.

  Has some assumptions that make it run fast:
  @unorderedList(
    @item(It assumes camera position is zero.)
    @item(It assumes that Dir and Up are already normalized and orthogonal.)
  )

  @groupBegin
}
function FastLookDirMatrix(const Direction, Up: TVector3): TMatrix4;
function InverseFastLookDirMatrix(const Direction, Up: TVector3): TMatrix4;
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
function VectorMultTransposedSameVector(const V: TVector3): TMatrix4;

function ScalingMatrix(const ScaleFactor: TVector3): TMatrix4;

procedure ScalingMatrices(const ScaleFactor: TVector3;
  InvertedMatrixIdentityIfNotExists: boolean;
  out Matrix, InvertedMatrix: TMatrix4);

function RotationMatrix(const AxisAngle: TVector4): TMatrix4; overload;

function RotationMatrixRad(const AngleRad: Single; const Axis: TVector3): TMatrix4; overload;
function RotationMatrixDeg(const AngleDeg: Single; const Axis: TVector3): TMatrix4; overload; deprecated 'use radians for everything throughout CGE';
function RotationMatrixRad(const AngleRad: Single; const AxisX, AxisY, AxisZ: Single): TMatrix4; overload;
function RotationMatrixDeg(const AngleDeg: Single; const AxisX, AxisY, AxisZ: Single): TMatrix4; overload; deprecated 'use radians for everything throughout CGE';

procedure RotationMatricesRad(const AngleRad: Single; const Axis: TVector3;
  out Matrix, InvertedMatrix: TMatrix4); overload;
procedure RotationMatricesRad(const AxisAngle: TVector4;
  out Matrix, InvertedMatrix: TMatrix4); overload;
{ @groupEnd }

{ Negate a rotation expressed as axis-angle (3 components for axis, 1 for angle).
  This simply negates the 4th vector component. }
function RotationNegate(const Rotation: TVector4): TVector4;

{ Rotate point in 2D, in a counter-clockwise fashion.
  AngleRad is in radians. }
function RotatePoint2D(const Point: TVector2; const AngleRad: Single): TVector2;

{ When you really, really must approximate a 3D or 2D scale by a single float.
  E.g. if your algorithm cannot handle non-uniform 3D or 2D scale,
  you have to approximate 3D or 2D scale by a single float.

  This is similar to an average of X, Y (and Z, in 3D), but it additionally
  secures in case some of X, Y, Z are negative and some are positive
  (like scale (-1, 1), common in 2D to flip horizontally).

  The overloaded version with matrix argument
  extracts scale from matrix, just like MatrixDecompose.

  @groupBegin }
function Approximate3DScale(const X, Y, Z: Single): Single; overload;
function Approximate3DScale(const V: TVector3): Single; overload;
function Approximate3DScale(const M: TMatrix4): Single; overload;

function Approximate2DScale(const X, Y: Single): Single; overload;
function Approximate2DScale(const V: TVector2): Single; overload;
{ @groupEnd }

{ Smoothly change Source to Target.

  The speed decreases as Source is closer to Target,
  so the animation ends smoothly.

  Note: Initially the argument Speed was represented by 2 values:
    MaxChangeSpeed, MaxDistance: Single
  The intended meaning of MaxChangeSpeed was the speed
  with which Source changes to Target,
  so that MaxChangeSpeed = 1.0 means that it changes in 1 second,
  MaxChangeSpeed = 0.5 means that it changes in 2 seconds,
  MaxChangeSpeed = 2 means that it changes in 0.5 seconds.

  Later: now it's just Speed.
  You can just passs Speed = MaxChangeSpeed / MaxDistance to get the
  same logic as above.
}
function SmoothTowards(const Source, Target, SecondsPassed,
  Speed: Single): Single; overload;
function SmoothTowards(const Source, Target: TVector2;
  const SecondsPassed, Speed: Single): TVector2; overload;
function SmoothTowards(const Source, Target: TVector3;
  const SecondsPassed, Speed: Single): TVector3; overload;

const
  NoScale: TVector3 = (X: 1; Y: 1; Z: 1);

{$endif read_interface}

{$ifdef read_implementation}

function SwapEndian(const V: TVector2): TVector2;
begin
  Result.X := SwapEndian(V.X);
  Result.Y := SwapEndian(V.Y);
end;

function SwapEndian(const V: TVector3): TVector3;
begin
  Result.X := SwapEndian(V.X);
  Result.Y := SwapEndian(V.Y);
  Result.Z := SwapEndian(V.Z);
end;

function SwapEndian(const V: TVector4): TVector4;
begin
  Result.X := SwapEndian(V.X);
  Result.Y := SwapEndian(V.Y);
  Result.Z := SwapEndian(V.Z);
  Result.W := SwapEndian(V.W);
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

procedure NormalizePlaneVar(var V: TVector4);
var
  Len: Single;
begin
  Len := Sqrt( Sqr(V.X) + Sqr(V.Y) + Sqr(V.Z) );
  Len := 1 / Len;
  V.X := V.X * Len;
  V.Y := V.Y * Len;
  V.Z := V.Z * Len;
  V.W := V.W * Len;
end;

function CosAngleBetweenVectors(const V1, V2: TVector3): Single;
var
  LensSquared: Float;
begin
  (* jak widac, wykrecam sie jednym pierwiastkowaniem pierwiastkujac
     v1.LengthSqr i v2.LengthSqr jednoczesnie. *)

  LensSquared := v1.LengthSqr * v2.LengthSqr;
  if IsZero(LensSquared) then
    raise EVectorInvalidOp.Create(
      'Cannot calculate angle between vectors, at least one of the vectors is zero');

  (* musimy robic tu Clamp do (-1, 1) bo praktyka pokazala ze czasami na skutek
     bledow obliczen zmiennoprzec. wynik tej funkcji jest maciupinke poza
     zakresem. A Cosinum musi byc w zakresie -1..1, w szczegolnosci
     ArcCos() dla czegos choc troche poza zakresem wywala paskudny EInvalidArgument *)
  Result := Clamped(
    TVector3.DotProduct(V1, V2) / Sqrt(LensSquared), -1.0, 1.0);
end;

function AngleRadBetweenVectors(const V1, V2: TVector3): Single;
begin
  Result := ArcCos(CosAngleBetweenVectors(V1, V2));
end;

function CosAngleBetweenNormals(const V1, V2: TVector3): Single;
begin
  Result := Clamped(TVector3.DotProduct(V1, V2), -1.0, 1.0);
end;

function AngleRadBetweenNormals(const V1, V2: TVector3): Single;
begin
  Result := ArcCos(CosAngleBetweenNormals(V1, V2));
end;

function RotationAngleRadBetweenVectors(const V1, V2, Cross: TVector3): Single;
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
    the invalid Result would for sure make longer distance. }

{ Commented out by default because this assertion is costly.
  But it should be valid, you can uncomment it for test!

  Assert(SameValue(
    PointsDistance(RotatePointAroundAxisRad(Result, V1, Cross), V2),
    Abs(V1.Length - V2.Length), 0.01));
}
end;

function RotationAngleRadBetweenVectors(const V1, V2: TVector3): Single;
begin
  Result := RotationAngleRadBetweenVectors(V1, V2, TVector3.CrossProduct(V1, V2));
end;

function RotatePointAroundAxisDeg(const Angle: Single; const Point: TVector3;
  const Axis: TVector3): TVector3;
begin
  Result := RotatePointAroundAxisRad(DegToRad(Angle), Point, Axis);
end;

procedure WarningZeroVectorNonZeroAngle;
begin
  WritelnWarning('RotatePointAroundAxisRad called with rotation Axis = zero but non-zero rotation Angle, this is invalid');
end;

function RotatePointAroundAxisRad(const Angle: Single; const Point: TVector3;
  const Axis: TVector3): TVector3;
var
  X, Y, Z, L: Single;
  SinAngle, CosAngle: Float;
begin
  SinCos(Angle, SinAngle, CosAngle);

  { normalize and decompose Axis vector }
  L := Axis.Length;
  if L <> 0 then
  begin
    L := 1 / L;
    X := Axis.X * L;
    Y := Axis.Y * L;
    Z := Axis.Z * L;
  end else
  begin
    { In case vector has zero length (so it's just zero),
      the Angle must be zero, and we can exit early. }
    if Angle <> 0 then
      WarningZeroVectorNonZeroAngle;
    Exit(Point);
  end;

  Result.X := (CosAngle + (1 - CosAngle) * x * x)     * Point.X
            + ((1 - CosAngle) * x * y - z * SinAngle)  * Point.Y
            + ((1 - CosAngle) * x * z + y * SinAngle)  * Point.Z;

  Result.Y := ((1 - CosAngle) * x * y + z * SinAngle)  * Point.X
            + (CosAngle + (1 - CosAngle) * y * y)       * Point.Y
            + ((1 - CosAngle) * y * z - x * SinAngle)   * Point.Z;

  Result.Z := ((1 - CosAngle) * x * z - y * SinAngle)  * Point.X
            + ((1 - CosAngle) * y * z + x * SinAngle)   * Point.Y
            + (CosAngle + (1 - CosAngle) * z * z)       * Point.Z;
end;

function RotatePointAroundAxis90(const Point: TVector3; const Axis: TVector3): TVector3;
var
  X, Y, Z, L: Single;
begin
  { Optimized version of RotatePointAroundAxisRad for Angle = Pi / 2,
    so SinAngle = 1, CosAngle = 0. }

  { normalize and decompose Axis vector }
  L := Axis.Length;
  if L <> 0 then
  begin
    L := 1 / L;
    X := Axis.X * L;
    Y := Axis.Y * L;
    Z := Axis.Z * L;
  end else
  begin
    WarningZeroVectorNonZeroAngle;
    Exit(Point);
  end;

  Result.X := (    x * x) * Point.X
            + (x * y - z) * Point.Y
            + (x * z + y) * Point.Z;

  Result.Y := (x * y + z) * Point.X
            + (    y * y) * Point.Y
            + (y * z - x) * Point.Z;

  Result.Z := (x * z - y) * Point.X
            + (y * z + x) * Point.Y
            + (    z * z) * Point.Z;
end;

function RotatePointAroundAxisMinus90(const Point: TVector3; const Axis: TVector3): TVector3;
var
  X, Y, Z, L: Single;
begin
  { Optimized version of RotatePointAroundAxisRad for Angle = -Pi / 2,
    so SinAngle = -1, CosAngle = 0. }

  { normalize and decompose Axis vector }
  L := Axis.Length;
  if L <> 0 then
  begin
    L := 1 / L;
    X := Axis.X * L;
    Y := Axis.Y * L;
    Z := Axis.Z * L;
  end else
  begin
    WarningZeroVectorNonZeroAngle;
    Exit(Point);
  end;

  Result.X := (    x * x) * Point.X
            + (x * y + z) * Point.Y
            + (x * z - y) * Point.Z;

  Result.Y := (x * y - z) * Point.X
            + (    y * y) * Point.Y
            + (y * z + x) * Point.Z;

  Result.Z := (x * z + y) * Point.X
            + (y * z - x) * Point.Y
            + (    z * z) * Point.Z;
end;

function RotatePointAroundAxis(const AxisAngle: TVector4; const Point: TVector3): TVector3;
var
  Axis: TVector3 absolute AxisAngle;
begin
  Result := RotatePointAroundAxisRad(AxisAngle.W, Point, Axis);
end;

function MaxVectorCoord(const V: TVector2): Integer;
begin
  Result := 0;
  if V.Y > V[Result] then Result := 1;
end;

function MaxVectorCoord(const V: TVector3): T3DAxis;
begin
  Result := 0;
  { order of comparisons is important. We start from 0, then 1 and 2,
    and change only when differ (>, not just >=). This way we
    guarantee that when values are equal, lower coordinate wins. }
  if V.Y > V[Result] then Result := 1;
  if V.Z > V[Result] then Result := 2;
end;

function MinVectorCoord(const V: TVector3): T3DAxis;
begin
  Result := 0;
  if V.Y < V[Result] then Result := 1;
  if V.Z < V[Result] then Result := 2;
end;

function MaxVectorCoord(const V: TVector4): Integer;
begin
  Result := 0;
  if V.Y > V[Result] then Result := 1;
  if V.Z > V[Result] then Result := 2;
  if V.W > V[Result] then Result := 3;
end;

function MaxAbsVectorCoord(const V: TVector2): Integer;
begin
  Result := 0;
  if Abs(V.Y) > Abs(V[Result]) then Result := 1;
end;

function MaxAbsVectorCoord(const V: TVector3): T3DAxis;
var
  Max, New: Single;
begin
  Result := 0;
  Max := Abs(V.X);
  New := Abs(V.Y); if New > Max then begin Result := 1; Max := New; end;
  New := Abs(V.Z); if New > Max then begin Result := 2; { Max := New; not needed anymore } end;
end;

function MaxAbsVectorCoord(const V: TVector4): Integer;
var
  Max, New: Single;
begin
  Result := 0;
  Max := Abs(V.X);
  New := Abs(V.Y); if New > Max then begin Result := 1; Max := New; end;
  New := Abs(V.Z); if New > Max then begin Result := 2; Max := New; end;
  New := Abs(V.W); if New > Max then begin Result := 3; { Max := New; not needed anymore } end;
end;

function MinAbsVectorCoord(const V: TVector2): Integer;
begin
  Result := 0;
  if Abs(V.Y) < Abs(V[Result]) then Result := 1;
end;

function MinAbsVectorCoord(const V: TVector3): T3DAxis;
var
  Min, New: Single;
begin
  Result := 0;
  Min := Abs(V.X);
  New := Abs(V.Y); if New < Min then begin Result := 1; Min := New; end;
  New := Abs(V.Z); if New < Min then begin Result := 2; { Min := New; not needed anymore } end;
end;

function MinAbsVectorCoord(const V: TVector4): Integer;
var
  Min, New: Single;
begin
  Result := 0;
  Min := Abs(V.X);
  New := Abs(V.Y); if New < Min then begin Result := 1; Min := New; end;
  New := Abs(V.Z); if New < Min then begin Result := 2; Min := New; end;
  New := Abs(V.W); if New < Min then begin Result := 3; { Min := New; not needed anymore } end;
end;

procedure SortAbsVectorCoord(const V: TVector3;
  out Max, Middle, Min: T3DAxis);
begin
  Max := MaxAbsVectorCoord(V);
  case Max of
    0: if Abs(V.Y) >= Abs(V.Z) then begin Middle := 1; Min := 2; end else begin Middle := 2; Min := 1; end;
    1: if Abs(V.X) >= Abs(V.Z) then begin Middle := 0; Min := 2; end else begin Middle := 2; Min := 0; end;
    else {2: }
       if Abs(V.X) >= Abs(V.Y) then begin Middle := 0; Min := 1; end else begin Middle := 1; Min := 0; end;
  end;
end;

function PlaneDirInDirection(const Plane: TVector4; const Direction: TVector3): TVector3;
var
  PlaneDir: TVector3 absolute Plane;
begin
  Result := PlaneDirInDirection(PlaneDir, Direction);
end;

function PlaneDirInDirection(const PlaneDir, Direction: TVector3): TVector3;
begin
  (* "Normalny" sposob aby sprawdzic czy dwa wektory wskazuja z tej samej
    plaszczyzny to porownac
          TVector3.DotProduct(V1, PlaneDir) > 0
          TVector3.DotProduct(V2, PlaneDir) > 0
    czyli tak jakby obciac czwarta wspolrzedna plaszczyzny (zeby plaszczyzna
    przechodzila przez (0, 0,0)) i sprawdzic czy dwa punkty leza po tej samej
    stronie plaszczyzny

    (jezeli jeden z wektorow V1 lub V2 jest rownolegly do plaszczyzny,
     tzn. TVector3.DotProduct(V*, PlaneDir) = 0 to przyjmujemy ze drugi
     moze byc w dowolna strone, wiec nawet sie
     nie przejmujemy co bedzie gdy zajdzie rownosc w ktorejs z powyzszych
     nierownosci).

    Ale mozna to uproscic gdy V1 = PlaneDir. Wiemy ze
      TVector3.DotProduct(PlaneDir, PlaneDir) > 0
    bo to przeciez suma trzech kwadratow. Wiec wystarczy sprawdzic czy
      TVector3.DotProduct(Direction, PlaneDir) > 0
    - jesli nie to trzeba odwrocic Normal. *)
  if TVector3.DotProduct(Direction, PlaneDir) < 0 then
    Result := -PlaneDir
  else
    Result := PlaneDir;
end;

function PlaneDirNotInDirection(const Plane: TVector4; const Direction: TVector3): TVector3;
var
  PlaneDir: TVector3 absolute Plane;
begin
  Result := PlaneDirNotInDirection(PlaneDir, Direction);
end;

procedure TwoPlanesIntersectionLine(const Plane0, Plane1: TVector4;
  out Line0, LineVector: TVector3);
var
  Plane0Dir: TVector3 absolute Plane0;
  Plane1Dir: TVector3 absolute Plane1;
  NonZeroIndex, Index1, Index2: T3DAxis;
  PlaneWithNonZeroIndex1: PVector4;
  PlaneMultiply, Sum_Index2, Sum_3: Single;
begin
  LineVector := TVector3.CrossProduct(Plane0Dir, Plane1Dir);

  NonZeroIndex := MaxAbsVectorCoord(LineVector);
  if IsZero(LineVector[NonZeroIndex]) then
    raise EPlanesParallel.Create(
      'Unable to calculate intersection line of two planes ' +
      Plane0.ToRawString + ' and ' + Plane1.ToRawString + ' because ' +
      'planes are parallel');

  { Since LineVector[NonZeroIndex] <> 0, we know that we can find exactly
    one point on this line by assuming that Point[NonZeroIndex] = 0. }
  Line0.Data[NonZeroIndex] := 0;
  RestOf3DCoords(NonZeroIndex, Index1, Index2);

  { Now we must solve
      Plane0[Index1] * Line0[Index1] + Plane0[Index2] * Line0[Index2] + Plane0.W = 0
      Plane1[Index1] * Line0[Index1] + Plane1[Index2] * Line0[Index2] + Plane1.W = 0
    We want to sum these two equations to eliminate Line0[Index1]:
                                         + Sum_Index2 *  Line0[Index2] + Sum_3   = 0
  }
  if not IsZero(Plane0[Index1]) then
  begin
    PlaneWithNonZeroIndex1 := @Plane0;
    PlaneMultiply := - Plane1[Index1] / Plane0[Index1];
    Sum_Index2 := Plane0[Index2] * PlaneMultiply + Plane1[Index2];
    Sum_3      := Plane0.W      * PlaneMultiply + Plane1.W;
  end else
  if not IsZero(Plane1[Index1]) then
  begin
    PlaneWithNonZeroIndex1 := @Plane1;
    PlaneMultiply := - Plane0[Index1] / Plane1[Index1];
    Sum_Index2 := Plane0[Index2] + Plane1[Index2] * PlaneMultiply;
    Sum_3      := Plane0.W      + Plane1.W      * PlaneMultiply;
  end else
  begin
    { If Plane0[Index1] = Plane1[Index1] = 0, this is simple.
        Sum_Index2 := Plane0[Index2] + Plane1[Index2];
        Sum_3      := Plane0.W      + Plane1.W     ;
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
  Line0.Data[Index2] := - Sum_3 / Sum_Index2;

  { Note we have
      PlaneWithNonZeroIndex1^[Index1] * Line0[Index1] +
      PlaneWithNonZeroIndex1^[Index2] * Line0[Index2] +
      PlaneWithNonZeroIndex1^.W = 0
    All is known except Line0[Index1],
    PlaneWithNonZeroIndex1^[Index1] is for sure <> 0. }
  Line0.Data[Index1] := -
    (PlaneWithNonZeroIndex1^[Index2] * Line0[Index2] +
     PlaneWithNonZeroIndex1^.W) /
    PlaneWithNonZeroIndex1^[Index1];
end;

function Lines2DIntersection(const Line0, Line1: TVector3): TVector2;
var
  Ratio, Divide: Single;
begin
  { Only one from Line0.X, Line0.Y may be zero.
    Take larger one for numerical stability. }
  if Abs(Line0.X) > Abs(Line0.Y) then
  begin
    Ratio := Line1.X / Line0.X;

    { we have equations
        Line0.X * x + Line0.Y * y + Line0.Z = 0
        Line1.X * x + Line1.Y * y + Line1.Z = 0
      Multiply first equation by Ratio and subtract to 2nd one:
        y * (Line0.Y * Ratio - Line1.Y) + Line0.Z * Ratio - Line1.Z = 0 }
    Divide := Line0.Y * Ratio - Line1.Y;
    if Divide = 0 then
      raise ELinesParallel.Create('Lines are parallel, Lines2DIntersection not possible');
    Result.Y := - (Line0.Z * Ratio - Line1.Z) / Divide;
    Result.X := - (Line0.Y * Result.Y + Line0.Z) / Line0.X;
  end else
  begin
    Ratio := Line1.Y / Line0.Y;

    { we have equations
        Line0.X * x + Line0.Y * y + Line0.Z = 0
        Line1.X * x + Line1.Y * y + Line1.Z = 0
      Multiply first equation by Ratio and subtract to 2nd one:
        x * (Line0.X * Ratio - Line1.X) + Line0.Z * Ratio - Line1.Z = 0 }
    Divide := Line0.X * Ratio - Line1.X;
    if Divide = 0 then
      raise ELinesParallel.Create('Lines are parallel, Lines2DIntersection not possible');
    Result.X := - (Line0.Z * Ratio - Line1.Z) / Divide;
    Result.Y := - (Line0.X * Result.X + Line0.Z) / Line0.Y;
  end;

  { tests: (checking should write zeros)
  Writeln('intersection 2d: ', Line0.ToString, ' ',
    Line1.ToString, ' gives ', Result.ToString, ' checking: ',
    FloatToNiceStr(Line0.X * Result.X + Line0.Y * Result.Y + Line0.Z), ' ',
    FloatToNiceStr(Line1.X * Result.X + Line1.Y * Result.Y + Line1.Z)); }
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
      A * (x + Move.X) + B * (y + Move.Y) + C * (z + Move.Z) + NewD = 0
    The 2nd equation can be rewritten as
      Ax + By + Cz + NewD + A * Move.X + B * Move.Y + C * Move.Z = 0
    so
      NewD = D - (A * Move.X + B * Move.Y + C * Move.Z);
  }

  Result := Plane;
  Result.W := Result.W - (
    Plane.X * Move.X +
    Plane.Y * Move.Y +
    Plane.Z * Move.Z);
end;

procedure PlaneMoveVar(var Plane: TVector4; const Move: TVector3);
begin
  Plane.W := Plane.W - (
    Plane.X * Move.X +
    Plane.Y * Move.Y +
    Plane.Z * Move.Z);
end;

function PlaneAntiMove(const Plane: TVector4;
  const Move: TVector3): TVector4;
begin
  { Like PlaneMove, but Move vector is negated.
    So we just do "Result.W +=" instead of "Result.W -=". }

  Result := Plane;
  Result.W := Result.W + (
    { Use parenthesis to group this expression, this way it's calculated
      in a similar order to PlaneMove, which increses it's numerical
      stability. Confirmed by TTestCastleVectors.TestPlaneMoveRandom,
      if you have these parenthesis you can have smaller epsilon to compare). }
    Plane.X * Move.X +
    Plane.Y * Move.Y +
    Plane.Z * Move.Z);
end;

function VectorsSamePlaneDirections(const V1, V2: TVector3;
  const Plane: TVector4): boolean;
var
  PlaneDir: TVector3 absolute Plane;
  V1Dot, V2Dot: Single;
begin
  V1Dot := TVector3.DotProduct(V1, PlaneDir);
  V2Dot := TVector3.DotProduct(V2, PlaneDir);
  Result := IsZero(V1Dot) or IsZero(V2Dot) or ((V1Dot > 0) = (V2Dot > 0));
end;

function VectorsSamePlaneDirections(const V1, V2: TVector3;
  const PlaneDir: TVector3): boolean;
var
  V1Dot, V2Dot: Single;
begin
  V1Dot := TVector3.DotProduct(V1, PlaneDir);
  V2Dot := TVector3.DotProduct(V2, PlaneDir);
  Result := IsZero(V1Dot) or IsZero(V2Dot) or ((V1Dot > 0) = (V2Dot > 0));
end;

function PointsSamePlaneSides(const p1, p2: TVector3; const Plane: TVector4): boolean;
var
  p1Side, p2Side: Single;
begin
  p1Side := p1.X*Plane.X + p1.Y*Plane.Y + p1.Z*Plane.Z + Plane.W;
  p2Side := p2.X*Plane.X + p2.Y*Plane.Y + p2.Z*Plane.Z + Plane.W;
  Result := IsZero(p1Side) or IsZero(p2Side) or ((p1Side > 0) = (p2Side > 0));
end;

function PlaneDirNotInDirection(const PlaneDir, Direction: TVector3): TVector3;
begin
  if TVector3.DotProduct(Direction, PlaneDir) > 0 then
    Result := -PlaneDir
  else
    Result := PlaneDir;
end;

function PointsDistance(const V1, V2: TVector2): Single;
begin
  { Result := Sqrt(PointsDistanceSqr(V1, V2));, expanded for speed }
  Result := Sqrt( Sqr(V2.X-V1.X) + Sqr(V2.Y-V1.Y) );
end;

function PointsDistance(const V1, V2: TVector3): Single;
begin
  { Result := Sqrt(PointsDistanceSqr(V1, V2));, expanded for speed }
  Result := Sqrt( Sqr(V2.X-V1.X) + Sqr(V2.Y-V1.Y) + Sqr(V2.Z-V1.Z) );
end;

function PointsDistanceSqr(const V1, V2: TVector3): Single;
begin
  { Result := (v2 - v1).LengthSqr;, expanded for speed }
  Result := Sqr(V2.X-V1.X) + Sqr(V2.Y-V1.Y) + Sqr(V2.Z-V1.Z);
end;

function PointsDistanceSqr(const V1, V2: TVector2): Single;
begin
  { Result := (v2 - v1).LengthSqr;, expanded for speed }
  Result := Sqr(V2.X-V1.X) + Sqr(V2.Y-V1.Y);
end;

function PointsDistance2DSqr(const V1, V2: TVector3; const IgnoreIndex: Integer): Single;
begin
{$IFNDEF FPC}
  Result := 0; // Make the Delphi Compiler Happy
{$ENDIF}
  case IgnoreIndex of
    0: Result := Sqr(V2.Y - V1.Y) + Sqr(V2.Z - V1.Z);
    1: Result := Sqr(V2.Z - V1.Z) + Sqr(V2.X - V1.X);
    2: Result := Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y);
    else PointsDistance2DSqr_InvalidIgnoreIndex;
  end;
end;

function VectorsPerp(const V1, V2: TVector3): boolean;
begin
  (* Naive implementation:

       Result := CosAngleBetweenVectors(V1, V2) = 0;

     But if you look how CosAngleBetweenVectors is calculated,
     it can be optimized. *)
  Result := IsZero(TVector3.DotProduct(V1, V2));
end;

function VectorsParallel(const V1, V2: TVector3): boolean;
var
  mc, c1, c2: T3DAxis;
  Scale: Single;
begin
  mc := MaxAbsVectorCoord(v1);
  if IsZero(V1[mc]) then Exit(true);

  Scale := V2[mc] / V1[mc];
  RestOf3DCoords(mc, c1, c2);
  Result := SameValue(V1[c1] * Scale, V2[c1]) and
            SameValue(V1[c2] * Scale, V2[c2]);
end;

procedure MakeVectorsAngleRadOnTheirPlane(var v1: TVector3;
  const v2: TVector3; const AngleRad: Single; const ResultWhenParallel: TVector3);
var
  rotAxis: TVector3;
  v1len: Single;
begin
  v1len := v1.Length;
  rotAxis := TVector3.CrossProduct(V1, V2);
  if rotAxis.IsZero then
    V1 := ResultWhenParallel
  else
    V1 := RotatePointAroundAxisRad(-AngleRad, v2, rotAxis).AdjustToLength(v1len);
end;

procedure MakeVectorsAngle90OnTheirPlane(var v1: TVector3;
  const v2: TVector3; const ResultWhenParallel: TVector3);
var
  rotAxis: TVector3;
  v1len: Single;
begin
  v1len := v1.Length;
  rotAxis := TVector3.CrossProduct(V1, V2);
  if rotAxis.IsZero then
    V1 := ResultWhenParallel
  else
    V1 := RotatePointAroundAxisMinus90(v2, rotAxis).AdjustToLength(v1len);
end;

procedure MakeVectorsOrthoOnTheirPlane(var v1: TVector3; const v2: TVector3);
begin
  MakeVectorsAngle90OnTheirPlane(V1, V2, AnyOrthogonalVector(V2));
  // MakeVectorsAngleRadOnTheirPlane(V1, V2, Pi / 2, AnyOrthogonalVector(V2));
end;

function MakeVectorOrthogonal(const V1, V2: TVector3): TVector3;
begin
  Result := V1;
  MakeVectorsOrthoOnTheirPlane(Result, V2);
end;

function AnyOrthogonalVector(const V: TVector3): TVector3;
begin
  { This uses a simple trick to make an orthogonal vector:
    if you take @code(Result := (V.Y, -V.X, 0)) then the dot product
    between the Result and V is zero, so they are orthogonal.
    There's also a small check needed to use a similar but different version
    when the only non-zero component of V is V.Z. }

  if IsZero(V.X) and IsZero(V.Y) then
  begin
    Result.X := 0;
    Result.Y := V.Z;
    Result.Z := -V.Y;
  end else
  begin
    Result.X := V.Y;
    Result.Y := -V.X;
    Result.Z := 0;
  end;
end;

function AnyOrthogonalVector(const V: TVector2): TVector2;
begin
  Result.X :=  V.Y;
  Result.Y := -V.X;
end;

function IsLineParallelToPlane(const lineVector: TVector3; const plane: TVector4): boolean;
var
  PlaneDir: TVector3 absolute plane;
begin
  Result := VectorsPerp(lineVector, PlaneDir);
end;

function IsLineParallelToSimplePlane(const lineVector: TVector3;
  const PlaneConstCoord: T3DAxis): boolean;
begin
  Result := IsZero(lineVector[PlaneConstCoord]);
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

function PointOnPlaneClosestToPoint(const Plane: TVector4; const Point: TVector3): TVector3;
var
  D: Single;
  PlaneDir: TVector3 absolute Plane;
begin
  { We know that

      Result = Point + D * PlaneDir
      dot(PlaneDir, Result) + Plane.W = 0

    So

      dot(PlaneDir, Point + D * PlaneDir) + Plane.W = 0

    We can expand the dot definition for 3D now, and get an equation
    to calculate D.
  }

  D := -(
    Plane.X * Point.X +
    Plane.Y * Point.Y +
    Plane.Z * Point.Z +
    Plane.W ) /
    PlaneDir.LengthSqr;
  Result := Point + PlaneDir * D;
end;

function PointToPlaneDistanceSqr(const Point: TVector3;
  const Plane: TVector4): Single;
begin
  Result :=
    Sqr(Plane.X * Point.X +
        Plane.Y * Point.Y +
        Plane.Z * Point.Z +
        Plane.W) /
    (Sqr(Plane.X) + Sqr(Plane.Y) + Sqr(Plane.Z));
end;

function PointToNormalizedPlaneDistance(const Point: TVector3;
  const Plane: TVector4): Single;
begin
  Result :=
    Abs(Plane.X * Point.X +
        Plane.Y * Point.Y +
        Plane.Z * Point.Z +
        Plane.W);
end;

function PointToPlaneDistance(const Point: TVector3;
  const Plane: TVector4): Single;
begin
  Result :=
    Abs(Plane.X * Point.X +
        Plane.Y * Point.Y +
        Plane.Z * Point.Z +
        Plane.W) /
    Sqrt(Sqr(Plane.X) + Sqr(Plane.Y) + Sqr(Plane.Z));
end;

function PointToSimplePlaneDistance(const point: TVector3;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single): Single;
begin
  Result := Abs(point[PlaneConstCoord]-PlaneConstValue);
end;

function PointOnLineClosestToPoint(
  const line0, lineVector, point: TVector3): TVector3;
var
  d: Single;
begin
  (*
   We know that (Result-Point) and LineVector (or Result - Line0) are orthogonal.

   (Result[0]-point[0])*(lineVector[0]) +
   (Result[1]-point[1])*(lineVector[1]) +
   (Result[2]-point[2])*(lineVector[2]) = 0 czyli
   Result[0]*lineVector[0] +
   Result[1]*lineVector[1] +
   Result[2]*lineVector[2] = point[0]*lineVector[0] +
                             point[1]*lineVector[1] +
                             point[2]*lineVector[2]

   We know Result can be expresssed as

   Result = line0 + lineVector * d

   So

   Result[0] = line0[0] + lineVector[0]*d
   Result[1] = line0[1] + lineVector[1]*d
   Result[2] = line0[2] + lineVector[2]*d

   So 4 equations, 4 unknowns, solving:

   (line0[0] + lineVector[0]*d)*lineVector[0]+
   (line0[1] + lineVector[1]*d)*lineVector[1]+
   (line0[2] + lineVector[2]*d)*lineVector[2] = point[0]*lineVector[0] +
                             point[1]*lineVector[1] +
                             point[2]*lineVector[2]
   d*(Sqr(lineVector[0])+ Sqr(lineVector[1])+ Sqr(lineVector[2]) ) =
   d*lineVector.LengthSqr =
   lineVector[0]*(point[0]-line0[0]) +
   lineVector[1]*(point[1]-line0[1]) +
   lineVector[2]*(point[2]-line0[2]);

   And we can calculate d now. *)
  d := (lineVector.X * (point.X-line0.X) +
        lineVector.Y * (point.Y-line0.Y) +
        lineVector.Z * (point.Z-line0.Z) ) / lineVector.LengthSqr;
  Result := line0 + lineVector * d;
end;

function PointOnLineClosestToPoint(
  const line0, lineVector, point: TVector2): TVector2;
var
  d: Single;
begin
  d := (lineVector.X * (point.X-line0.X) +
        lineVector.Y * (point.Y-line0.Y) ) / lineVector.LengthSqr;
  Result := line0 + lineVector * d;
end;

function PointToLineDistanceSqr(const point, line0, lineVector: TVector3): Single;
begin
  Result := PointsDistanceSqr(point, PointOnLineClosestToPoint(line0, lineVector, point));
end;

function TryPlaneLineIntersection(out t: Single;
  const plane: TVector4; const line0, lineVector: TVector3): boolean;
var
  PlaneDir: TVector3 absolute plane;
  Dot: Single;
begin
  Dot := TVector3.DotProduct(LineVector, PlaneDir);
  if not IsZero(Dot) then
  begin
    Result := true;
    t := -(plane.X*line0.X + plane.Y*line0.Y + plane.Z*line0.Z + plane.W)/Dot;
  end else
    Result := false;
end;

function TryPlaneLineIntersection(out intersection: TVector3;
  const plane: TVector4; const line0, lineVector: TVector3): boolean;
var
  t: Single;
begin
  Result := TryPlaneLineIntersection(t, Plane, Line0, LineVector);
  if Result then Intersection := Line0 + LineVector * t;
end;

function TryPlaneRayIntersection(out Intersection: TVector3;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeT: Single;
begin
  Result := TryPlaneLineIntersection(MaybeT, Plane, RayOrigin, RayDirection) and (MaybeT >= 0);
  if Result then Intersection := RayOrigin + RayDirection * MaybeT;
end;

function TryPlaneRayIntersection(
  out Intersection: TVector3; out T: Single;
  const Plane: TVector4; const RayOrigin, RayDirection: TVector3): boolean;
var
  MaybeT: Single;
begin
  Result := TryPlaneLineIntersection(MaybeT, Plane, RayOrigin, RayDirection) and (MaybeT >= 0);
  if Result then
  begin
    // Intersection := RayOrigin + RayDirection * MaybeT;
    // powyzsza instrukcja zapisana ponizej w 3 linijkach dziala nieco szybciej:
    Intersection := RayDirection;
    Intersection := Intersection * MaybeT;
    Intersection := Intersection + RayOrigin;

    t := MaybeT;
  end;
end;

function TryPlaneSegmentDirIntersection(out Intersection: TVector3;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeT: Single;
begin
  Result := TryPlaneLineIntersection(MaybeT, Plane, Segment0, SegmentVector) and
    (MaybeT >= 0) and (MaybeT <= 1);
  if Result then Intersection := Segment0 + SegmentVector * MaybeT;
end;

function TryPlaneSegmentDirIntersection(
  out Intersection: TVector3; out T: Single;
  const Plane: TVector4; const Segment0, SegmentVector: TVector3): boolean;
var
  MaybeT: Single;
begin
  Result := TryPlaneLineIntersection(MaybeT, Plane, Segment0, SegmentVector) and
    (MaybeT >= 0) and (MaybeT <= 1);
  if Result then
  begin
    // Intersection := Segment0 + SegmentVector * MaybeT;
    // powyzsza instrukcja zapisana ponizej w 3 linijkach dziala nieco szybciej:
    Intersection := SegmentVector;
    Intersection := Intersection * MaybeT;
    Intersection := Intersection + Segment0;

    t := MaybeT;
  end;
end;

{ TrySimplePlaneRayIntersection ---------------------------------------------- }

function TrySimplePlaneRayIntersection(out Intersection: TVector3;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  T: Single;
begin
  Result := TrySimplePlaneRayIntersection(T, PlaneConstCoord, PlaneConstValue, RayOrigin, RayDirection);
  if Result then
    Intersection := RayOrigin + RayDirection * T;
end;

function TrySimplePlaneRayIntersection(
  out Intersection: TVector3; out T: Single;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3): boolean;
begin
  Result := TrySimplePlaneRayIntersection(T, PlaneConstCoord, PlaneConstValue, RayOrigin, RayDirection);
  if Result then
    Intersection := RayOrigin + RayDirection * T;
end;

function TrySimplePlaneRayIntersection(out T: Single;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const RayOrigin, RayDirection: TVector3): boolean;
begin
  Result := not IsLineParallelToSimplePlane(RayDirection, PlaneConstCoord);
  if Result then
  begin
    T := (PlaneConstValue-RayOrigin[PlaneConstCoord]) / RayDirection[PlaneConstCoord];
    Result := T >= 0;
  end;
end;

{ TrySimplePlaneSegmentIntersection ------------------------------------------ }

function TrySimplePlaneSegmentIntersection(out Intersection: TVector3;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3): boolean;
var
  T: Single;
begin
  Result := TrySimplePlaneSegmentIntersection(T, PlaneConstCoord, PlaneConstValue, Pos1, Pos2);
  if Result then
    Intersection := Pos1 + (Pos2 - Pos1) * T;
end;

function TrySimplePlaneSegmentIntersection(
  out Intersection: TVector3; out T: Single;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3): boolean;
begin
  Result := TrySimplePlaneSegmentIntersection(T, PlaneConstCoord, PlaneConstValue, Pos1, Pos2);
  if Result then
    Intersection := Pos1 + (Pos2 - Pos1) * T;
end;

function TrySimplePlaneSegmentIntersection(out T: Single;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  const Pos1, Pos2: TVector3): boolean;
var
  SegmentVector: TVector3;
begin
  SegmentVector := Pos2 - Pos1;

  Result := not IsLineParallelToSimplePlane(SegmentVector, PlaneConstCoord);
  if Result then
  begin
    T := (PlaneConstValue-Pos1[PlaneConstCoord]) / SegmentVector[PlaneConstCoord];
    Result := (T >= 0) and (T <= 1);
  end;
end;

{ others --------------------------------------------------------------------- }

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector3): boolean;
var
  c: T3DAxis;
begin
  { project points on the best axis-aligned plane }
  c := MaxAbsVectorCoord(Pos1 - Pos2);
  Result := ((pos1[c] <= intersection[c]) and (intersection[c] <= pos2[c])) or
            ((pos1[c] >= intersection[c]) and (intersection[c] >= pos2[c]));
end;

function IsPointOnSegmentLineWithinSegment(const intersection, pos1, pos2: TVector2): boolean;
var
  c: T3DAxis;
begin
  { project points on the best axis-aligned plane }
  c := MaxAbsVectorCoord(Pos1 - Pos2);
  Result := ((pos1[c] <= intersection[c]) and (intersection[c] <= pos2[c])) or
            ((pos1[c] >= intersection[c]) and (intersection[c] >= pos2[c]));
end;

function Line2DFrom2Points(const P1, P2: TVector2): TVector3;
begin
  Result := Line2DFromOriginVector(P1, P2 - P1);
end;

function LineOfTwoDifferentPoints2D(const P1, P2: TVector2): TVector3;
begin
  Result := Line2DFromOriginVector(P1, P2 - P1);
end;

function Line2DFromOriginVector(const Line0, LineVector: TVector2): TVector3;
var
  AxisGood, AxisOther: Integer;
begin
  { Set AxisGood to LineVector coordinate that is definitely non-zero,
    let AxisOther be the other one. }
  AxisGood := MaxAbsVectorCoord(LineVector);
  AxisOther := 1 - AxisGood;

  { We want to make Result.XY and LineVector orthogonal (this is how 2D line equation Ax+By+C works).

    So their dot product = 0.

    Let Result[AxisOther] = -1 and then we can construct
    Result[AxisGood] = LineVector[AxisOther] / LineVector[AxisGood].
  }
  Result.Data[AxisOther] := -1;
  Result.Data[AxisGood] := LineVector.Data[AxisOther] / LineVector[AxisGood];
  Result.Z := - (Result.X * Line0.X + Result.Y * Line0.Y);
end;

function IsSpheresCollision(const Sphere1Center: TVector3; const Sphere1Radius: Single;
  const Sphere2Center: TVector3; const Sphere2Radius: Single): boolean;
begin
  Result := PointsDistanceSqr(Sphere1Center, Sphere2Center)<=
    Sqr(Sphere1Radius+Sphere2Radius);
end;

function PointToSegmentDistanceSqr(const point, pos1, pos2: TVector3): Single;
var
  Closest: TVector3;
begin
  Closest := PointOnLineClosestToPoint(pos1, pos2 - pos1, point);
  if IsPointOnSegmentLineWithinSegment(Closest, pos1, pos2) then
    Result := PointsDistanceSqr(Closest, point) else
    Result := Min(PointsDistanceSqr(pos1, point),
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
  PlanePoint := TVector3.Zero;
  PlanePoint.Data[MaxCoord] := -Plane.W / Plane[MaxCoord];

  NewPlanePoint := Matrix.MultPoint(PlanePoint);
  NewPlaneDir := Matrix.MultDirection(PlaneDir);
  Result.W := -TVector3.DotProduct(NewPlanePoint, NewPlaneDir);
end;

function IsTunnelSphereCollision(const Tunnel1, Tunnel2: TVector3;
  const TunnelRadius: Single; const SphereCenter: TVector3;
  const SphereRadius: Single): boolean;
begin
  Result := PointToSegmentDistanceSqr(SphereCenter, Tunnel1, Tunnel2)<=
    Sqr(SphereRadius+TunnelRadius);
end;

function IsSegmentSphereCollision(const pos1, pos2, SphereCenter: TVector3;
  const SphereRadius: Single): boolean;
var
  SphereRadiusSqr: Single;
  Intersect: TVector3;
begin
  SphereRadiusSqr := Sqr(SphereRadius);
  Result:= (PointsDistanceSqr(pos1, SphereCenter) <= SphereRadiusSqr) or
           (PointsDistanceSqr(pos2, SphereCenter) <= SphereRadiusSqr);
  if not Result then
  begin
    Intersect := PointOnLineClosestToPoint(pos1, pos2 - pos1, SphereCenter);
    Result := IsPointOnSegmentLineWithinSegment(Intersect, pos1, pos2) and
      (PointsDistanceSqr(Intersect, SphereCenter) <= SphereRadiusSqr);
  end;
end;

{ Solve intersection routine with a ray that resolved into a quadratic
  equation. The solution is such T >= 0 that
    A * T^2 + B * T + C = 0 }
function TryRayIntersectionQuadraticEquation(out T: Single;
  const A, B, C: Single): boolean;
var
  Delta, T1, T2: Single;
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
  const SphereCenter: TVector3; const SphereRadius: Single;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  T, A, B, C: Single;
  RayOriginMinusCenter: TVector3;
begin
  { Intersection = RayOrigin + RayDirection * T,
    Also Distance(Intersection, SphereCenter) = SphereRadius,
    so Distance(RayOrigin + RayDirection * T, SphereCenter) = SphereRadius.

    Expand this, and use to calculate T: we get a quadratic equation for T.
    A * T^2 + B * T + C = 0. }
  RayOriginMinusCenter := RayOrigin - SphereCenter;
  A := Sqr(RayDirection.X) +
       Sqr(RayDirection.Y) +
       Sqr(RayDirection.Z);
  B := 2 * RayDirection.X * RayOriginMinusCenter.X +
       2 * RayDirection.Y * RayOriginMinusCenter.Y +
       2 * RayDirection.Z * RayOriginMinusCenter.Z;
  C := Sqr(RayOriginMinusCenter.X) +
       Sqr(RayOriginMinusCenter.Y) +
       Sqr(RayOriginMinusCenter.Z) - Sqr(SphereRadius);

  Result := TryRayIntersectionQuadraticEquation(T, A, B, C);
  if Result then
    Intersection := RayOrigin + RayDirection * T;
end;

function TryCylinderRayIntersection(out Intersection: TVector3;
  const CylinderAxisOrigin, CylinderAxis: TVector3;
  const CylinderRadius: Single;
  const RayOrigin, RayDirection: TVector3): boolean;
var
  T, AA, BB, CC: Single;
  X, Y, B: TVector3;
begin
  { We know Intersection = RayOrigin + RayDirection * T.
    For cylinder, normalize CylinderAxis and then

      TVector3.CrossProduct(Intersection - CylinderAxisOrigin, CylinderAxis).Length
      = CylinderRadius

    (For why, see http://en.wikipedia.org/wiki/Cross_product:
    length of TVector3.CrossProduct is the area of parallelogram between it's vectors.
    This is equal to area of CylinderRadius * length CylinderAxis in this case.)
    Got the idea from oliii post on
    http://www.gamedev.net/community/forums/topic.asp?topic_id=467789

    Insert ray equation into cylinder equation, and solve for T. }

  X := RayOrigin - CylinderAxisOrigin;
  Y := RayDirection;
  B := CylinderAxis.Normalize;

  { Now let A = X + Y * T, then A x B.Length^2 = CylinderRadius^2.
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

  AA := Sqr(B.Y)*Sqr(Y.Z) + Sqr(B.X)*Sqr(Y.Z) - 2*B.Y*B.Z*Y.Y*Y.Z
    + 2*B.X*B.Z*Y.X*Y.Z + Sqr(B.Z)*Sqr(Y.Y) + Sqr(B.X)*Sqr(Y.Y)
    - 2*B.X*B.Y*Y.X*Y.Y + Sqr(B.Z)*Sqr(Y.X) + Sqr(B.Y)*Sqr(Y.X);

  BB := 2*Sqr(B.Y)*X.Z*Y.Z + 2*Sqr(B.X)*X.Z*Y.Z - 2*B.Y*B.Z*X.Y*Y.Z
      + 2*B.X*B.Z*X.X*Y.Z - 2*B.Y*B.Z*X.Z*Y.Y + 2*Sqr(B.Z)*X.Y*Y.Y
      + 2*Sqr(B.X)*X.Y*Y.Y - 2*B.X*B.Y*X.X*Y.Y + 2*B.X*B.Z*X.Z*Y.X
      - 2*B.X*B.Y*X.Y*Y.X + 2*Sqr(B.Z)*X.X*Y.X + 2*Sqr(B.Y)*X.X*Y.X;

  CC := Sqr(B.Y)*Sqr(X.Z) + Sqr(B.X)*Sqr(X.Z) - 2*B.Y*B.Z*X.Y*X.Z
    + 2*B.X*B.Z*X.X*X.Z + Sqr(B.Z)*Sqr(X.Y) + Sqr(B.X)*Sqr(X.Y)
    - 2*B.X*B.Y*X.X*X.Y + Sqr(B.Z)*Sqr(X.X) + Sqr(B.Y)*Sqr(X.X)
    - Sqr(CylinderRadius);

  Result := TryRayIntersectionQuadraticEquation(T, AA, BB, CC);
  if Result then
    Intersection := RayOrigin + RayDirection * T;
end;

function PointOnLineClosestToLine(
  out Intersection: TVector3;
  const Line1Origin, Line1Vector, Line2Origin, Line2Vector: TVector3): Boolean;
var
  Line0, LineVector, Ray0, RayVector: TVector3;
  N: TVector3;
  {Dist, }LineF: Single;
  Ax: Integer;
  ProjectAxisX, ProjectAxisY: TVector3;
  ProjectedIntersection, ProjectLine0, ProjectLineVector, ProjectRay0, ProjectRayVector: TVector2;
begin
  Line0 := Line1Origin;
  LineVector := Line1Vector;
  Ray0 := Line2Origin;
  RayVector := Line2Vector;

  { See calculate distance between 2 lines in 3D:
    https://math.stackexchange.com/questions/2213165/find-shortest-distance-between-lines-in-3d
    Observation: there must be N,
    orthogonal to LineVector and RayVector,
    that determines the vector between 2 closest points on 2 lines. }
  N := TVector3.CrossProduct(LineVector, RayVector);
  if N.IsZero then
    Exit(false); // ray parallel to axis, no sensible answer

  // N := N.Normalize; // it is not necessary to normalize for anything below

  // Dist := TVector3.DotProduct(N, Line0 - Ray0){ / N.Length}; // not needed in this approach

  { Project our 2 lines on 2D }
  ProjectAxisX := AnyOrthogonalVector(N);
  ProjectAxisY := TVector3.CrossProduct(N, ProjectAxisX);

  ProjectLine0 := Vector2(
    TVector3.DotProduct(ProjectAxisX, Line0),
    TVector3.DotProduct(ProjectAxisY, Line0)
  );
  ProjectLineVector := Vector2(
    TVector3.DotProduct(ProjectAxisX, LineVector),
    TVector3.DotProduct(ProjectAxisY, LineVector)
  );
  ProjectRay0 := Vector2(
    TVector3.DotProduct(ProjectAxisX, Ray0),
    TVector3.DotProduct(ProjectAxisY, Ray0)
  );
  ProjectRayVector := Vector2(
    TVector3.DotProduct(ProjectAxisX, RayVector),
    TVector3.DotProduct(ProjectAxisY, RayVector)
  );

  ProjectedIntersection := Lines2DIntersection(
    Line2DFromOriginVector(ProjectLine0, ProjectLineVector),
    Line2DFromOriginVector(ProjectRay0, ProjectRayVector)
  );

  Ax := MaxAbsVectorCoord(ProjectLineVector); // choose non-zero coordinate
  LineF := (ProjectedIntersection[Ax] - ProjectLine0[Ax]) / ProjectLineVector[Ax];

  Result := true;
  Intersection := Line0 + LineF * LineVector;
end;

{ matrix transforms for 3D graphics  ----------------------------------------- }

function TranslationMatrix(const Transl: TVector3): TMatrix4;
begin
  Result := TMatrix4.Identity;
  Result.Data[3, 0] := Transl.X;
  Result.Data[3, 1] := Transl.Y;
  Result.Data[3, 2] := Transl.Z;
end;

function TranslationMatrix(const X, Y, Z: Single): TMatrix4;
begin
  Result := TMatrix4.Identity;
  Result.Data[3, 0] := X;
  Result.Data[3, 1] := Y;
  Result.Data[3, 2] := Z;
end;

procedure TranslationMatrices(const X, Y, Z: Single;
  out Matrix, InvertedMatrix: TMatrix4);
begin
  Matrix := TMatrix4.Identity;
  Matrix.Data[3, 0] := X;
  Matrix.Data[3, 1] := Y;
  Matrix.Data[3, 2] := Z;

  InvertedMatrix := TMatrix4.Identity;
  InvertedMatrix.Data[3, 0] := -X;
  InvertedMatrix.Data[3, 1] := -Y;
  InvertedMatrix.Data[3, 2] := -Z;
end;

procedure TranslationMatrices(const Transl: TVector3;
  out Matrix, InvertedMatrix: TMatrix4);
begin
  Matrix := TMatrix4.Identity;
  Matrix.Data[3, 0] := Transl.X;
  Matrix.Data[3, 1] := Transl.Y;
  Matrix.Data[3, 2] := Transl.Z;

  InvertedMatrix := TMatrix4.Identity;
  InvertedMatrix.Data[3, 0] := -Transl.X;
  InvertedMatrix.Data[3, 1] := -Transl.Y;
  InvertedMatrix.Data[3, 2] := -Transl.Z;
end;

procedure MultMatrixTranslation(var M: TMatrix4;
  const Transl: TVector3);
var
  NewColumn: TVector4;
begin
  NewColumn := M.Columns[3];
  NewColumn.X := NewColumn.X + (M.Data[0, 0] * Transl.X + M.Data[1, 0] * Transl.Y + M.Data[2, 0] * Transl.Z);
  NewColumn.Y := NewColumn.Y + (M.Data[0, 1] * Transl.X + M.Data[1, 1] * Transl.Y + M.Data[2, 1] * Transl.Z);
  NewColumn.Z := NewColumn.Z + (M.Data[0, 2] * Transl.X + M.Data[1, 2] * Transl.Y + M.Data[2, 2] * Transl.Z);
  NewColumn.W := NewColumn.W + (M.Data[0, 3] * Transl.X + M.Data[1, 3] * Transl.Y + M.Data[2, 3] * Transl.Z);
  M.Columns[3] := NewColumn;
end;

procedure MultMatricesTranslation(var M, MInvert: TMatrix4;
  const Transl: TVector3);
var
  NewColumn: TVector4;
  { OldLastRow may use the same space as NewColumn }
  OldLastRow: TVector4 absolute NewColumn;
begin
  NewColumn := M.Columns[3];
  NewColumn.X := NewColumn.X + (M.Data[0, 0] * Transl.X + M.Data[1, 0] * Transl.Y + M.Data[2, 0] * Transl.Z);
  NewColumn.Y := NewColumn.Y + (M.Data[0, 1] * Transl.X + M.Data[1, 1] * Transl.Y + M.Data[2, 1] * Transl.Z);
  NewColumn.Z := NewColumn.Z + (M.Data[0, 2] * Transl.X + M.Data[1, 2] * Transl.Y + M.Data[2, 2] * Transl.Z);
  NewColumn.W := NewColumn.W + (M.Data[0, 3] * Transl.X + M.Data[1, 3] * Transl.Y + M.Data[2, 3] * Transl.Z);
  M.Columns[3] := NewColumn;

  OldLastRow.X := MInvert.Data[0, 3];
  OldLastRow.Y := MInvert.Data[1, 3];
  OldLastRow.Z := MInvert.Data[2, 3];
  OldLastRow.W := MInvert.Data[3, 3];

  MInvert.Data[0, 0] := MInvert.Data[0, 0] - Transl.X * OldLastRow.X;
  MInvert.Data[1, 0] := MInvert.Data[1, 0] - Transl.X * OldLastRow.Y;
  MInvert.Data[2, 0] := MInvert.Data[2, 0] - Transl.X * OldLastRow.Z;
  MInvert.Data[3, 0] := MInvert.Data[3, 0] - Transl.X * OldLastRow.W;
  MInvert.Data[0, 1] := MInvert.Data[0, 1] - Transl.Y * OldLastRow.X;
  MInvert.Data[1, 1] := MInvert.Data[1, 1] - Transl.Y * OldLastRow.Y;
  MInvert.Data[2, 1] := MInvert.Data[2, 1] - Transl.Y * OldLastRow.Z;
  MInvert.Data[3, 1] := MInvert.Data[3, 1] - Transl.Y * OldLastRow.W;
  MInvert.Data[0, 2] := MInvert.Data[0, 2] - Transl.Z * OldLastRow.X;
  MInvert.Data[1, 2] := MInvert.Data[1, 2] - Transl.Z * OldLastRow.Y;
  MInvert.Data[2, 2] := MInvert.Data[2, 2] - Transl.Z * OldLastRow.Z;
  MInvert.Data[3, 2] := MInvert.Data[3, 2] - Transl.Z * OldLastRow.W;
end;

function TransformToCoords(const V, NewX, NewY, NewZ: TVector3): TVector3;
begin
  Result.X := V.X * NewX.X + V.Y * NewY.X + V.Z * NewZ.X;
  Result.Y := V.X * NewX.Y + V.Y * NewY.Y + V.Z * NewZ.Y;
  Result.Z := V.X * NewX.Z + V.Y * NewY.Z + V.Z * NewZ.Z;
end;

function TransformToCoordsMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Result.Data[0, i] := NewX[i];
    Result.Data[1, i] := NewY[i];
    Result.Data[2, i] := NewZ[i];
    Result.Data[3, i] := NewOrigin[i];
  end;
  { bottom row }
  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 0;
  Result.Data[3, 3] := 1;
end;

function TransformToCoordsMatrix(const NewX, NewY, NewZ: TVector3): TMatrix4;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Result.Data[0, i] := NewX[i];
    Result.Data[1, i] := NewY[i];
    Result.Data[2, i] := NewZ[i];
  end;
  { right column }
  Result.Data[3, 0] := 0;
  Result.Data[3, 1] := 0;
  Result.Data[3, 2] := 0;
  { bottom row }
  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 0;
  Result.Data[3, 3] := 1;
end;

function TransformToCoordsNoScaleMatrix(const NewOrigin,
  NewX, NewY, NewZ: TVector3): TMatrix4;
begin
  Result := TransformToCoordsMatrix(NewOrigin,
    NewX.Normalize, NewY.Normalize, NewZ.Normalize);
end;

function TransformFromCoordsMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    { Difference between TrasformToCoords and TransformFromCoords:
      up-left 3x3 matrix is applied in a transposed manner,
      compared with TrasformToCoords. }
    Result.Data[i, 0] := OldX[i];
    Result.Data[i, 1] := OldY[i];
    Result.Data[i, 2] := OldZ[i];
  end;

  { Another difference between TrasformToCoords and TransformFromCoords:
    - OldOrigin must be negated here
    - OldOrigin must have directions applied
    See e.g. Global Illumination Compendium by Philip Dutre, section (15). }
  Result.Data[3, 0] := -TVector3.DotProduct(OldOrigin, OldX);
  Result.Data[3, 1] := -TVector3.DotProduct(OldOrigin, OldY);
  Result.Data[3, 2] := -TVector3.DotProduct(OldOrigin, OldZ);

  { bottom row }
  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 0;
  Result.Data[3, 3] := 1;
end;

function TransformFromCoordsMatrix(const OldX, OldY, OldZ: TVector3): TMatrix4;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    { Difference between TrasformToCoords and TransformFromCoords:
      up-left 3x3 matrix is applied in a transposed manner,
      compared with TrasformToCoords. }
    Result.Data[i, 0] := OldX[i];
    Result.Data[i, 1] := OldY[i];
    Result.Data[i, 2] := OldZ[i];
  end;

  { right column }
  Result.Data[3, 0] := 0;
  Result.Data[3, 1] := 0;
  Result.Data[3, 2] := 0;
  { bottom row }
  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 0;
  Result.Data[3, 3] := 1;
end;

function TransformFromCoordsNoScaleMatrix(const OldOrigin,
  OldX, OldY, OldZ: TVector3): TMatrix4;
begin
  Result := TransformFromCoordsMatrix(OldOrigin,
    OldX.Normalize, OldY.Normalize, OldZ.Normalize);
end;

procedure TransformCoordsMatrices(const NewX, NewY, NewZ: TVector3;
  out ToCoords, FromCoords: TMatrix4);
begin
  ToCoords := TransformToCoordsMatrix(NewX, NewY, NewZ);
  FromCoords := TransformFromCoordsMatrix(NewX, NewY, NewZ);
end;

function TViewVectors.Matrix: TMatrix4;
begin
  Result := LookDirMatrix(Translation, Direction, Up);
end;

function TViewVectors.RotationMatrix: TMatrix4;
begin
  { TODO: We don't use FastLookDirMatrix for now,
    because we aren't sure are Direction / Up normalized
    and orthogonal in all cases.
    We could check / fix it in the future (make Direction / Up normalized
    and orthogonal) and then use faster FastLookDirMatrix. }

  Result := LookDirMatrix(TVector3.Zero, Direction, Up);
end;

function LookAtMatrix(const Eye, Center, Up: TVector3): TMatrix4;
begin
  Result := LookDirMatrix(Eye, Center - Eye, Up);
end;

function LookDirMatrix(const Eye, Dir, Up: TVector3): TMatrix4;
var
  GoodDir, GoodUp, Side: TVector3;
begin
  Side := TVector3.CrossProduct(Dir, Up).Normalize;
  GoodDir := Dir.Normalize;

  { Recalculate GoodUp from Side and GoodDir. This way:
    1. We make sure GoodUp is orthogonal to Side and GoodDir.
       The Up was already orthogonal to Side,
       but it was not necessarily orthogonal to GoodDir.
    2. We make sure GoodUp is normalized (vector product of normalized
       vectors is also normalized).
    This is done looking at gluLookAt implementation in
    SGI Sample OpenGL Implementation. }
  GoodUp := TVector3.CrossProduct(Side, GoodDir);

  Result := LookDirMatrix(Eye, GoodDir, Side, GoodUp);
end;

function LookDirMatrix(const Eye, Dir, Side, Up: TVector3): TMatrix4;
begin
  { VectorNegate on Dir, since in right-handed coordinate system
    you look in the -Z direction. }
  Result := TransformFromCoordsMatrix(Eye, Side, Up, -Dir);
end;

function FastLookDirMatrix(const Direction, Up: TVector3): TMatrix4;
var
  Side: TVector3;
  i: integer;
begin
  Side := TVector3.CrossProduct(Direction, Up);

  { Make TransformToCoordsMatrix with origin zero now. }

  for i := 0 to 2 do
  begin
    Result.Data[i, 0] := Side[i];
    Result.Data[i, 1] := Up[i];
    Result.Data[i, 2] := -Direction[i]; { negate Direction, since it goes to -Z }
  end;

  { bottom row and right column }
  Result.Data[3, 0] := 0;
  Result.Data[3, 1] := 0;
  Result.Data[3, 2] := 0;

  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 0;

  Result.Data[3, 3] := 1;
end;

function InverseFastLookDirMatrix(const Direction, Up: TVector3): TMatrix4;
var
  Side: TVector3;
  i: integer;
begin
  Side := TVector3.CrossProduct(Direction, Up);

  { Inverse of LookDirMatrix is now to make
    TransformToCoordsMatrix with origin zero. }

  for i := 0 to 2 do
  begin
    Result.Data[0, i] := Side[i];
    Result.Data[1, i] := Up[i];
    Result.Data[2, i] := -Direction[i]; { negate Direction, since it goes to -Z }
  end;

  { bottom row and right column }
  Result.Data[3, 0] := 0;
  Result.Data[3, 1] := 0;
  Result.Data[3, 2] := 0;

  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 0;

  Result.Data[3, 3] := 1;
end;

function ModelViewToNormalMatrix(const M: TMatrix4): TMatrix3;
var
  D: Single;
  M3: TMatrix3;
begin
  Move(M.Data[0], M3.Data[0], SizeOf(TVector3));
  Move(M.Data[1], M3.Data[1], SizeOf(TVector3));
  Move(M.Data[2], M3.Data[2], SizeOf(TVector3));
  D := M3.Determinant;
  { Comparing "IsZero(D)" would not be OK,
    see TTestCastleVectors.TestEpsilonInModelViewToNormalMatrix ,
    https://github.com/castle-engine/view3dscene/issues/35 }
  //if IsZero(D) then
  if D = 0 then
    { modelview not invertible, nothing good to do but we have to continue
      without errors, since this can happen with arbitrary 3D scenes. }
    Result := M3
  else
    Result := M3.Inverse(D).Transpose;
end;

function VectorMultTransposedSameVector(const V: TVector3): TMatrix4;
begin
  (* Naive version:

  for i := 0 to 2 do { i = column, j = row }
    for j := 0 to 2 do
      Result.Data[i, j] := V.Data[i]*V.Data[j];

  Expanded and optimized version below. *)

  {$ifdef FPC}
  Assert(SizeOf(Result) mod SizeOf(DWord) = 0);
  FillDWord(Result, SizeOf(Result) div SizeOf(DWord), 0);
  {$else}
  FillChar(Result, SizeOf(Result), 0);
  {$endif}

  Result.Data[0, 0] := Sqr(V.X);
  Result.Data[1, 1] := Sqr(V.Y);
  Result.Data[2, 2] := Sqr(V.Z);

  Result.Data[0, 1] := V.X * V.Y; Result.Data[1, 0] := Result.Data[0, 1];
  Result.Data[0, 2] := V.X * V.Z; Result.Data[2, 0] := Result.Data[0, 2];
  Result.Data[1, 2] := V.Y * V.Z; Result.Data[2, 1] := Result.Data[1, 2];

  { Fill the last row and column like an identity matrix.
    Note that FillDWord above already took care of zeros. }
  Result.Data[3, 3] := 1;
end;

function ScalingMatrix(const ScaleFactor: TVector3): TMatrix4;
begin
  Result := TMatrix4.Identity;
  Result.Data[0, 0] := ScaleFactor.X;
  Result.Data[1, 1] := ScaleFactor.Y;
  Result.Data[2, 2] := ScaleFactor.Z;
end;

procedure ScalingMatrices(const ScaleFactor: TVector3;
  InvertedMatrixIdentityIfNotExists: boolean;
  out Matrix, InvertedMatrix: TMatrix4);
begin
  Matrix := TMatrix4.Identity;
  Matrix.Data[0, 0] := ScaleFactor.X;
  Matrix.Data[1, 1] := ScaleFactor.Y;
  Matrix.Data[2, 2] := ScaleFactor.Z;

  InvertedMatrix := TMatrix4.Identity;
  if not
    (InvertedMatrixIdentityIfNotExists and
      ( IsZero(ScaleFactor.X) or
        IsZero(ScaleFactor.Y) or
        IsZero(ScaleFactor.Z) )) then
  begin
    InvertedMatrix.Data[0, 0] := 1 / ScaleFactor.X;
    InvertedMatrix.Data[1, 1] := 1 / ScaleFactor.Y;
    InvertedMatrix.Data[2, 2] := 1 / ScaleFactor.Z;
  end;
end;

function RotationMatrix(const AxisAngle: TVector4): TMatrix4;
var
  Axis: TVector3 absolute AxisAngle;
begin
  Result := RotationMatrixRad(AxisAngle.W, Axis);
end;

function RotationMatrixRad(const AngleRad: Single;
  const Axis: TVector3): TMatrix4;
var
  NormAxis: TVector3;
  AngleSin, AngleCos: Float;
  AxisLen, S, C: Single;
begin
  // NormAxis := Axis.Normalize;
  { Instead of calling Axis.Normalize, we do normalization here.
    This way we can react nicer (for zero additional cost) to Axis being
    exactly zero (which is an easy mistake to make e.g. when editing Rotation
    in CGE editor).
    We'll return an identity matrix instead of invalid matrix
    that would likely make transformed model invisible.
    We still don't officially tolerate such Axis value. }
  AxisLen := Axis.Length;
  if AxisLen = 0 then
    Exit(TMatrix4.Identity);
  AxisLen := 1 / AxisLen;
  NormAxis.X := Axis.X * AxisLen;
  NormAxis.Y := Axis.Y * AxisLen;
  NormAxis.Z := Axis.Z * AxisLen;

  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to Single once }
  S := AngleSin;
  C := AngleCos;

  Result := VectorMultTransposedSameVector(NormAxis);

  { We do not touch the last column and row of Result in the following code,
    treating Result like a 3x3 matrix. The last column and row are already Ok. }

  { Expanded Result := Result + (IdentityMatrix3 - Result) * AngleCos; }
  Result.Data[0, 0] := Result.Data[0, 0] + (1 - Result.Data[0, 0]) * C;
  Result.Data[1, 0] := Result.Data[1, 0] +    - Result.Data[1, 0]  * C;
  Result.Data[2, 0] := Result.Data[2, 0] +    - Result.Data[2, 0]  * C;

  Result.Data[0, 1] := Result.Data[0, 1] +    - Result.Data[0, 1]  * C;
  Result.Data[1, 1] := Result.Data[1, 1] + (1 - Result.Data[1, 1]) * C;
  Result.Data[2, 1] := Result.Data[2, 1] +    - Result.Data[2, 1]  * C;

  Result.Data[0, 2] := Result.Data[0, 2] +    - Result.Data[0, 2]  * C;
  Result.Data[1, 2] := Result.Data[1, 2] +    - Result.Data[1, 2]  * C;
  Result.Data[2, 2] := Result.Data[2, 2] + (1 - Result.Data[2, 2]) * C;

  NormAxis.X := NormAxis.X * S;
  NormAxis.Y := NormAxis.Y * S;
  NormAxis.Z := NormAxis.Z * S;

  { Add M3 (from OpenGL matrix equations) }
  Result.Data[1, 0] := Result.Data[1, 0] + -NormAxis.Z;
  Result.Data[2, 0] := Result.Data[2, 0] +  NormAxis.Y;

  Result.Data[0, 1] := Result.Data[0, 1] +  NormAxis.Z;
  Result.Data[2, 1] := Result.Data[2, 1] + -NormAxis.X;

  Result.Data[0, 2] := Result.Data[0, 2] + -NormAxis.Y;
  Result.Data[1, 2] := Result.Data[1, 2] +  NormAxis.X;
end;

procedure RotationMatricesRad(const AxisAngle: TVector4;
  out Matrix, InvertedMatrix: TMatrix4);
var
  Axis: TVector3 absolute AxisAngle;
begin
  RotationMatricesRad(AxisAngle.W, Axis, Matrix, InvertedMatrix);
end;

procedure RotationMatricesRad(const AngleRad: Single;
  const Axis: TVector3;
  out Matrix, InvertedMatrix: TMatrix4);
var
  NormAxis: TVector3;
  V: Single;
  AngleSin, AngleCos: Float;
  AxisLen, S, C: Single;
begin
  // NormAxis := Axis.Normalize;
  { Instead of calling Axis.Normalize, we do normalization here.
    This way we can react nicer (for zero additional cost) to Axis being
    exactly zero (which is an easy mistake to make e.g. when editing Rotation
    in CGE editor).
    We'll return an identity matrix instead of invalid matrix
    that would likely make transformed model invisible.
    We still don't officially tolerate such Axis value. }
  AxisLen := Axis.Length;
  if AxisLen = 0 then
  begin
    Matrix := TMatrix4.Identity;
    InvertedMatrix := TMatrix4.Identity;
    Exit;
  end;
  AxisLen := 1 / AxisLen;
  NormAxis.X := Axis.X * AxisLen;
  NormAxis.Y := Axis.Y * AxisLen;
  NormAxis.Z := Axis.Z * AxisLen;

  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to Single once }
  S := AngleSin;
  C := AngleCos;

  Matrix := VectorMultTransposedSameVector(NormAxis);

  { We do not touch the last column and row of Matrix in the following code,
    treating Matrix like a 3x3 matrix. The last column and row are already Ok. }

  { Expanded Matrix := Matrix + (IdentityMatrix3 - Matrix) * AngleCos; }
  Matrix.Data[0, 0] := Matrix.Data[0, 0] + (1 - Matrix.Data[0, 0]) * C;
  Matrix.Data[1, 0] := Matrix.Data[1, 0] +    - Matrix.Data[1, 0]  * C;
  Matrix.Data[2, 0] := Matrix.Data[2, 0] +    - Matrix.Data[2, 0]  * C;

  Matrix.Data[0, 1] := Matrix.Data[0, 1] +    - Matrix.Data[0, 1]  * C;
  Matrix.Data[1, 1] := Matrix.Data[1, 1] + (1 - Matrix.Data[1, 1]) * C;
  Matrix.Data[2, 1] := Matrix.Data[2, 1] +    - Matrix.Data[2, 1]  * C;

  Matrix.Data[0, 2] := Matrix.Data[0, 2] +    - Matrix.Data[0, 2]  * C;
  Matrix.Data[1, 2] := Matrix.Data[1, 2] +    - Matrix.Data[1, 2]  * C;
  Matrix.Data[2, 2] := Matrix.Data[2, 2] + (1 - Matrix.Data[2, 2]) * C;

  { Up to this point, calculated Matrix is also good for InvertedMatrix }
  InvertedMatrix := Matrix;

  NormAxis.X := NormAxis.X * S;
  NormAxis.Y := NormAxis.Y * S;
  NormAxis.Z := NormAxis.Z * S;

  { Now add M3 to Matrix, and subtract M3 from InvertedMatrix.
    That's because for the inverted rotation, AngleSin is negated,
    so the M3 should be subtracted. }
  V := -NormAxis.Z; Matrix.Data[1, 0] := Matrix.Data[1, 0] + V; InvertedMatrix.Data[1, 0] := InvertedMatrix.Data[1, 0] - V;
  V :=  NormAxis.Y; Matrix.Data[2, 0] := Matrix.Data[2, 0] + V; InvertedMatrix.Data[2, 0] := InvertedMatrix.Data[2, 0] - V;

  V :=  NormAxis.Z; Matrix.Data[0, 1] := Matrix.Data[0, 1] + V; InvertedMatrix.Data[0, 1] := InvertedMatrix.Data[0, 1] - V;
  V := -NormAxis.X; Matrix.Data[2, 1] := Matrix.Data[2, 1] + V; InvertedMatrix.Data[2, 1] := InvertedMatrix.Data[2, 1] - V;

  V := -NormAxis.Y; Matrix.Data[0, 2] := Matrix.Data[0, 2] + V; InvertedMatrix.Data[0, 2] := InvertedMatrix.Data[0, 2] - V;
  V :=  NormAxis.X; Matrix.Data[1, 2] := Matrix.Data[1, 2] + V; InvertedMatrix.Data[1, 2] := InvertedMatrix.Data[1, 2] - V;
end;

function RotationMatrixDeg(const AngleDeg: Single; const Axis: TVector3): TMatrix4;
begin
  Result := RotationMatrixRad(DegToRad(AngleDeg), Axis);
end;

function RotationMatrixDeg(const AngleDeg: Single;
  const AxisX, AxisY, AxisZ: Single): TMatrix4;
begin
  Result := RotationMatrixRad(DegToRad(AngleDeg), Vector3(AxisX, AxisY, AxisZ));
end;

function RotationMatrixRad(const AngleRad: Single;
  const AxisX, AxisY, AxisZ: Single): TMatrix4;
begin
  Result := RotationMatrixRad(AngleRad, Vector3(AxisX, AxisY, AxisZ));
end;

function RotatePoint2D(const Point: TVector2; const AngleRad: Single): TVector2;
var
  AngleSin, AngleCos: Float;
  S, C: Single;
begin
  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to Single once }
  S := AngleSin;
  C := AngleCos;
  Result.X := Point.X * C - Point.Y * S;
  Result.Y := Point.X * S + Point.Y * C;
end;

function RotationNegate(const Rotation: TVector4): TVector4;
begin
  Result := Rotation;
  Result.W := -Result.W;
end;

function Approximate3DScale(const X, Y, Z: Single): Single;
begin
  if (X * Y < 0) or
     (X * Z < 0) or
     (Y * X < 0) then
    { If some values have opposite signs, it's better to make
      an average of absolute values. This way a scale like (-1, 1, 1),
      that flips X but preserves size, results in 1 (not 1/3).
      Bug reproduce: escape with mirrored map parts. }
    Result := (Abs(X) + Abs(Y) + Abs(Z)) / 3
  else
    Result := (    X  +     Y  +     Z ) / 3;
end;

function Approximate3DScale(const V: TVector3): Single;
begin
  Result := Approximate3DScale(V.X, V.Y, V.Z);
end;

function Approximate3DScale(const M: TMatrix4): Single;
var
  Scale: TVector3;
begin
  { See MatrixDecompose }
  Scale.X := Vector3(M.Data[0, 0], M.Data[0, 1], M.Data[0, 2]).Length;
  Scale.Y := Vector3(M.Data[1, 0], M.Data[1, 1], M.Data[1, 2]).Length;
  Scale.Z := Vector3(M.Data[2, 0], M.Data[2, 1], M.Data[2, 2]).Length;
  Result := Approximate3DScale(Scale);
end;

function Approximate2DScale(const X, Y: Single): Single;
begin
  if (X * Y < 0) then
    { If X and Y have opposite signs, it's better to make
      an average of absolute values. This way a scale like (-1, 1),
      that flips X but preserves Y, results in 1 (not 0). }
    Result := (Abs(X) + Abs(Y)) / 2
  else
    Result := (    X  +     Y ) / 2;
end;

function Approximate2DScale(const V: TVector2): Single;
begin
  Result := Approximate2DScale(V.X, V.Y);
end;

function SmoothTowards(const Source, Target, SecondsPassed,
  Speed: Single): Single;
begin
  Result := Source +
    Min(1, SecondsPassed * Speed) * (Target - Source);
end;

function SmoothTowards(const Source, Target: TVector2;
  const SecondsPassed, Speed: Single): TVector2;
begin
  Result := Source +
    Min(1, SecondsPassed * Speed) * (Target - Source);
end;

function SmoothTowards(const Source, Target: TVector3;
  const SecondsPassed, Speed: Single): TVector3;
begin
  Result := Source +
    Min(1, SecondsPassed * Speed) * (Target - Source);
end;

{$endif read_implementation}
