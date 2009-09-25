{
  Copyright 2003-2008 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ Quaternion operations, in particular using unit quaternions to express
  3D rotations. }
unit Quaternions;

interface

uses VectorMath;

type
  TQuaternion = packed record
    case Integer of
      0: (Vector: TVector3Single;
          Real: Single);
      1: ({ Alternative, sometimes comfortable, view on quaternion as one
            4-element vector. }
          Vector4: TVector4Single);
  end;

const
  QuatIdentityRot: TQuaternion = (Vector: (0, 0, 0); Real: 1);

{ Calculate unit quaternion representing rotation around Axis
  (must be normalized) by AngleRad angle (in radians).

  Actually, non-normalized Axis may also be used, which will then
  result in non-normalized quaternion that doesn't represent rotation...
  so it's mostly useless for us. }
function QuatFromAxisAngle(const Axis: TVector3Single;
  const AngleRad: Single): TQuaternion;

{ Calculate axis (will be normalized) and angle (will be in radians)
  of rotation encoded in unit quaternion Q.
  This is the reverse of QuatFromAxisAngle. }
procedure QuatToAxisAngle(const Q: TQuaternion;
  out Axis: TVector3Single; out AngleRad: Single);

{ Calculate matrix doing rotation described by unit quaternion. }
function QuatToRotationMatrix(const Q: TQuaternion): TMatrix4Single;

{ Multiply two quaternions.

  Geometric interpretation: If these are unit quaternions representing
  rotations, multiplying them calculates one rotation that has the same
  effect as rotating by Q2 and then by Q1.

  Normal of result is equal to norm of Q1 * norm of Q2 (in particular,
  multiplying unit quaternions (used for rotations) yields another unit
  quaternion for sure). }
function QuatMultiply(const Q1, Q2: TQuaternion): TQuaternion;

{ Quaternion conjugation. This is just a fancy name for negating Q.Vector.
  @groupBegin }
function QuatConjugate(const Q: TQuaternion): TQuaternion;
procedure QuatConjugateTo1st(var Q: TQuaternion);
{ @groupEnd }

{ Rotate by unit quaternion.

  You can pass here TVector4Single, which is then understood to be a 3D
  position in homogenous coordinates.
  @groupBegin }
function QuatRotate(const Q: TQuaternion;
  const Point: TVector4Single): TVector4Single; overload;
function QuatRotate(const Q: TQuaternion;
  const Point: TVector3Single): TVector3Single; overload;
{ @groupEnd }

procedure QuatNormalizeTo1st(var Q: TQuaternion);

{ Perform normalization, only if the quaternion is detected to be
  "significantly unnormalized". It checks if the quaternion needs
  normalization using fast VectorLenSqr, that is quaternion length
  is not needed for the check (sqrt not needed). Only if it's significantly
  different that 1.0, sqrt is done and quaternion is normalized.

  This may be useful if you fear of eventual
  errors because of floating-point error cumulations, e.g. when you
  repeatedly multiply one quaternion by another, and yet another, and yet
  another etc. Calling this will trigger normalization from time to time
  (although will tolerate very small, epsilon-like, differences that are
  normal). Thus it prevents the quaternion from getting "too unnormalized".

  Generally, this is not needed, as quaternions are nicely numerically
  stable (which means that quaternion "very slightly unnormalized" will
  only generate "very slightly wrong" results, so it's not that bad).
  And no, I didn't actually observe the need for this in my programs.
  But you can see it actually called when you use TMatrixExaminer and
  deliberately cause spinning by very very large value (e.g. run
  view3dscene and press and hold right key, this will cause model
  spinning very fast, which causes quat multiplication every frame).
  So possibly this would trigger incorrect quaternions at some point.

  Anyway, this remains mostly a paranoid correctness measure.  }
procedure QuatLazyNormalizeTo1st(var Q: TQuaternion);

{ Interpolate between two rotations, along the shortest path on the unit sphere,
  with constant speed.

  The overloaded version that works with TVector4Single takes
  a rotation (not a quaternion) expressed as an axis
  (first 3 elements) and angle (in radians, 4th element).
  Axis does not have to be normalized (we'll normalize it).
  This is nice e.g. to interpolate VRML/X3D rotations.

  @groupBegin }
function SLerp(const A: Single; const Q1, Q2: TQuaternion): TQuaternion;
function SLerp(const A: Single; const Rot1, Rot2: TVector4Single): TVector4Single;
{ @groupEnd }

{ Interpolate between two rotations, along the shortest path on the unit sphere.

  This is faster than SLerp, but does not make the interpolation
  with constant speed. Often it's not a noticeable / important problem.
  See http://number-none.com/product/Understanding%20Slerp,%20Then%20Not%20Using%20It/

  The overloaded version that works with TVector4Single takes
  a rotation (not a quaternion) expressed as an axis
  (first 3 elements) and angle (in radians, 4th element).
  Axis does not have to be normalized (we'll normalize it).
  This is nice e.g. to interpolate VRML/X3D rotations.

  @groupBegin }
function NLerp(const A: Single; const Q1, Q2: TQuaternion): TQuaternion;
function NLerp(const A: Single; const Rot1, Rot2: TVector4Single): TVector4Single;
{ @groupEnd }

implementation

uses Math;

function QuatFromAxisAngle(const Axis: TVector3Single;
  const AngleRad: Single): TQuaternion;
var
  SinHalfAngle, CosHalfAngle: Float;
begin
  { The quaternion requires half angles. }
  SinCos(AngleRad / 2, SinHalfAngle, CosHalfAngle);

  Result.Vector := VectorScale(Axis, SinHalfAngle);
  Result.Real := CosHalfAngle;
end;

procedure QuatToAxisAngle(const Q: TQuaternion;
  out Axis: TVector3Single;
  out AngleRad: Single);
{ Q is a normalized quaternion, so
    Q.Vector = Sin(AngleRad / 2) * Axis
    Q.Real = Cos(AngleRad / 2)
}
var
  HalfAngle, SinHalfAngle: Single;
begin
  HalfAngle := ArcCos(Q.Real);
  SinHalfAngle := Sin(HalfAngle);
  AngleRad := HalfAngle * 2;
  if Zero(SinHalfAngle) then
  begin
    { Then Q.Vector must be zero also... How could this happen?
      SinHalfAngle = 0 means that HalfAngle = Pi * K (e.g. 0).
      So Angle = 2 * Pi * K so there's no rotation actually happening...

      Which means that any Axis is Ok (but return anything normalized,
      to keep assertion that returned Axis is normalized). }
    Axis := Vector3Single(0, 0, 1);
  end else
    Axis := VectorScale(Q.Vector, 1 / SinHalfAngle);
end;

function QuatMultiply(const Q1, Q2: TQuaternion): TQuaternion;
begin
  Result.Vector := VectorProduct(Q1.Vector, Q2.Vector);
  VectorAddTo1st(Result.Vector, VectorScale(Q1.Vector, Q2.Real));
  VectorAddTo1st(Result.Vector, VectorScale(Q2.Vector, Q1.Real));

  Result.Real := Q1.Real * Q2.Real - VectorDotProduct(Q1.Vector, Q2.Vector);
end;

function QuatConjugate(const Q: TQuaternion): TQuaternion;
begin
  Result.Vector := VectorNegate(Q.Vector);
  Result.Real := Q.Real;
end;

procedure QuatConjugateTo1st(var Q: TQuaternion);
begin
  VectorNegateTo1st(Q.Vector);
end;

function QuatRotate(const Q: TQuaternion;
  const Point: TVector4Single): TVector4Single; overload;
begin
  Result := QuatMultiply(Q, TQuaternion(Point)).Vector4;
  Result := QuatMultiply(TQuaternion(Result), QuatConjugate(Q)).Vector4;
end;

function QuatRotate(const Q: TQuaternion;
  const Point: TVector3Single): TVector3Single; overload;
var
  P4: TVector4Single;
begin
  P4 := Vector4Single(Point, 0);
  P4 := QuatRotate(Q, P4);
  Result := Vector3SingleCut(P4);
end;

function QuatToRotationMatrix(const X, Y, Z, W: Single): TMatrix4Single;
var
  XX, YY, ZZ: Single;
begin
  XX := Sqr(X);
  YY := Sqr(Y);
  ZZ := Sqr(Z);

  { row 0 }
  Result[0, 0] := 1 - 2 * (YY + ZZ);
  Result[1, 0] := 2 * ( X * Y - W * Z );
  Result[2, 0] := 2 * ( X * Z + W * Y );
  Result[3, 0] := 0;

  { row 1 }
  Result[0, 1] := 2 * ( X * Y + W * Z );
  Result[1, 1] := 1 - 2 * (XX + ZZ);
  Result[2, 1] := 2 * ( Y * Z - W * X );
  Result[3, 1] := 0;

  { row 2 }
  Result[0, 2] := 2 * ( X * Z - W * Y );
  Result[1, 2] := 2 * ( Y * Z + W * X );
  Result[2, 2] := 1 - 2 * (XX + YY);
  Result[3, 2] := 0;

  { row 3 - like in identity matrix,
    only 3x3 matrix is interesting in rotations. }
  Result[0, 3] := 0;
  Result[1, 3] := 0;
  Result[2, 3] := 0;
  Result[3, 3] := 1;
end;

function QuatToRotationMatrix(const Q: TQuaternion): TMatrix4Single;
begin
  Result := QuatToRotationMatrix(Q.Vector[0], Q.Vector[1], Q.Vector[2], Q.Real);
end;

procedure QuatNormalizeTo1st(var Q: TQuaternion);
var
  Len: Single;
begin
  Len := VectorLen(Q.Vector4);
  if Len <> 0 then
  begin
    Len := 1/Len;
    Q.Vector[0] *= Len;
    Q.Vector[1] *= Len;
    Q.Vector[2] *= Len;
    Q.Real *= Len;
  end;
end;

procedure QuatLazyNormalizeTo1st(var Q: TQuaternion);
var
  Len: Single;
begin
  Len := VectorLenSqr(Q.Vector4);
  if (Len - 1) > 0.001 then
  begin
    { tests: Writeln('quat lazily normed'); }
    Len := Sqrt(Len);
    if Len <> 0 then
    begin
      Len := 1/Len;
      Q.Vector[0] *= Len;
      Q.Vector[1] *= Len;
      Q.Vector[2] *= Len;
      Q.Real *= Len;
    end;
  end;
end;

{ Like QuatFromAxisAngle, except Axis is always normalized here,
  and Axis+Angle are packed within a vector. }
function QuatFromAxisAngle_UnnormalizedPacked(Rot: TVector4Single): TQuaternion;
var
  Axis: TVector3Single absolute Rot;
begin
  NormalizeTo1st(Axis);
  Result := QuatFromAxisAngle(Axis, Rot[3]);
end;

{ Like QuatToAxisAngle, except Axis+Angle are packed within a vector.
  Axis will be normalized only if quaternion was also normalized
  (which is usually the case, if working with rotation quaternions). }
function QuatToAxisAngle_Packed(const Q: TQuaternion): TVector4Single;
var
  Axis: TVector3Single absolute Result;
begin
  QuatToAxisAngle(Q, Axis, Result[3]);
end;

{ For SLerp and NLerp implementations, see
  http://www.3dkingdoms.com/weekly/weekly.php?a=36
  http://www.3dkingdoms.com/weekly/quat.h
  http://number-none.com/product/Understanding%20Slerp,%20Then%20Not%20Using%20It/
  http://en.wikipedia.org/wiki/Slerp
}

function SLerp(const A: Single; const Q1, Q2: TQuaternion): TQuaternion;
var
  W1, W2: Single;
  CosTheta, Theta: Float;
  SinTheta: Single;
begin
  CosTheta := VectorDotProduct(Q1.Vector4, Q2.Vector4);
  Theta := ArcCos(CosTheta);
  SinTheta := Sin(Theta);
  if SinTheta > 0.001 then
  begin
    W1 := Sin( (1-A) * Theta ) / SinTheta;
    W2 := Sin(    A  * Theta ) / SinTheta;
  end else
  begin
    { Theta ~= 0, so both rotations equal (or opposite, in which case
      result in undefined anyway). }
    W1 := 1 - A;
    W2 := A;
  end;
  Result.Vector4 := VectorAdd(VectorScale(Q1.Vector4, W1), VectorScale(Q2.Vector4, W2));
end;

function SLerp(const A: Single; const Rot1, Rot2: TVector4Single): TVector4Single;
begin
  Result := QuatToAxisAngle_Packed(SLerp(A,
    QuatFromAxisAngle_UnnormalizedPacked(Rot1),
    QuatFromAxisAngle_UnnormalizedPacked(Rot2)));
end;

function NLerp(const A: Single; const Q1, Q2: TQuaternion): TQuaternion;
begin
  Result.Vector4 := Lerp(A, Q1.Vector4, Q2.Vector4);
  QuatNormalizeTo1st(Result);
end;

function NLerp(const A: Single; const Rot1, Rot2: TVector4Single): TVector4Single;
begin
  Result := QuatToAxisAngle_Packed(NLerp(A,
    QuatFromAxisAngle_UnnormalizedPacked(Rot1),
    QuatFromAxisAngle_UnnormalizedPacked(Rot2)));
end;

end.
