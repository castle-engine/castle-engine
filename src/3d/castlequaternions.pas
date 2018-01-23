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

{ Quaternions (in particular using them to express 3D rotations).
  @noAutoLinkHere }
unit CastleQuaternions;

{$I castleconf.inc}

interface

uses CastleVectors;

type
  { Quaternions are useful to represent rotations in 3D
    that can be easily applied and combined with other rotations.
    Note that, while this structure (and some, but not all, of it's methods)
    can handle @italic(any) quaternion, you are usually interested
    @italic(only in @bold(unit quaternions), that can represent a 3D rotation). }
  TQuaternion = record
    Data: packed record
      case Integer of
        0: (Vector: TVector3;
            Real: Single);
        1: ({ Alternative, sometimes comfortable, view on quaternion as one
              4-element vector. }
            Vector4: TVector4);
    end;

    const
      { Quaternion representing @italic("no rotation").
        Note: this is @italic(not) a quaternion filled with zeros
        (the @code(Data.Real) component is 1.0), instead this is a unit quaternion
        that correctly represents @italic("rotation by zero degrees/radians"). }
      ZeroRotation: TQuaternion = (Data: (Vector: (Data: (0, 0, 0)); Real: 1));

    { Calculate axis (will be normalized) and angle (will be in radians)
      of rotation encoded in unit quaternion Q.
      This is the reverse of QuatFromAxisAngle. }
    procedure ToAxisAngle(out Axis: TVector3; out AngleRad: Single); overload;

    { Convert quaternion to a rotation axis and angle encoded in 4D vector.
      Axis is normalized if quaternion was also normalized
      (which is true if working with rotation quaternions).
      Angle is in radians. }
    function ToAxisAngle: TVector4; overload;

    { Calculate matrix doing rotation described by unit quaternion. }
    function ToRotationMatrix: TMatrix4;

    { Rotate a point, treating this quaternion as a representation of 3D rotation.
      For this operation to make sense in 3D, this must be a "unit" quaternion
      (which is created by a function like @link(QuatFromAxisAngle)).

      You can pass here TVector4, which is then understood to be a 3D
      position in homogeneous coordinates.
      @groupBegin }
    function Rotate(const Point: TVector4): TVector4; overload;
    function Rotate(const Point: TVector3): TVector3; overload;
    { @groupEnd }

    { Quaternion conjugation. This is just a fancy name for negating @code(Data.Vector).
      @groupBegin }
    function Conjugate: TQuaternion;
    procedure ConjugateMe;
    { @groupEnd }

    { Make the quaternion normalized. }
    procedure NormalizeMe;
    procedure Normalize; deprecated 'use NormalizeMe (consistent with TVector3.NormalizeMe and TQuaternion.ConjugateMe)';

    { Perform normalization but only if the quaternion is detected to be
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
    procedure LazyNormalizeMe;
    procedure LazyNormalize; deprecated 'use LazyNormalizeMe';

    { Multiply two quaternions.

      Geometric interpretation: If these are unit quaternions representing
      rotations, multiplying them calculates one rotation that has the same
      effect as rotating by Q2 and then by Q1.

      Normal of result is equal to norm of Q1 * norm of Q2 (in particular,
      multiplying unit quaternions (used for rotations) yields another unit
      quaternion for sure). }
    class operator {$ifdef FPC}*{$else}Multiply{$endif} (const Q1, Q2: TQuaternion): TQuaternion;
  end;

const
  QuatIdentityRot: TQuaternion = (Data: (Vector: (Data: (0, 0, 0)); Real: 1))
    deprecated 'use TQuaternion.ZeroRotation';

{ Calculate unit quaternion representing rotation around Axis
  by AngleRad angle (in radians).

  Axis must be normalized, or you have to pass NormalizeAxis = true
  (then we'll normalize it ourselves inside). Otherwise you will
  get non-normalized quaternion that doesn't represent rotation,
  and is usually useless for us. }
function QuatFromAxisAngle(const Axis: TVector3;
  const AngleRad: Single; const NormalizeAxis: boolean = false): TQuaternion; overload;
function QuatFromAxisAngle(const AxisAngle: TVector4;
  const NormalizeAxis: boolean = false): TQuaternion; overload;

{ Interpolate between two rotations, along the shortest path on the unit sphere,
  with constant speed.

  The overloaded version that works with TVector4 takes
  a rotation (not a quaternion) expressed as an axis
  (first 3 elements) and angle (in radians, 4th element).
  Axis does not have to be normalized (we'll normalize it).
  This is nice e.g. to interpolate VRML/X3D rotations.

  @groupBegin }
function SLerp(const A: Single; const Q1, Q2: TQuaternion): TQuaternion; overload;
function SLerp(const A: Single; const Rot1, Rot2: TVector4): TVector4; overload;
{ @groupEnd }

{ Interpolate between two rotations, along the straightest path on the unit sphere.

  This is faster than SLerp, but does not guarantee the interpolated
  result travels with constant speed.
  Often it's not a noticeable / important problem (see
  http://number-none.com/product/Understanding%20Slerp,%20Then%20Not%20Using%20It/)

  When ForceShortestPath = @false, this doesn't guarantee choosing
  the shortest path. Although it goes through the @italic(straightest) path,
  there are two such paths, it may go through the shorter or longer one.
  Use ForceShortestPath = @true if you want to interpolate through the
  shortest.

  The overloaded version that works with TVector4 takes
  a rotation (not a quaternion) expressed as an axis
  (first 3 elements) and angle (in radians, 4th element).
  Axis does not have to be normalized (we'll normalize it).
  This is nice e.g. to interpolate VRML/X3D rotations.

  @groupBegin }
function NLerp(const A: Single; const Q1, Q2: TQuaternion;
  const ForceShortestPath: boolean = true): TQuaternion; overload;
function NLerp(const A: Single; const Rot1, Rot2: TVector4;
  const ForceShortestPath: boolean = true): TVector4; overload;
{ @groupEnd }

implementation

uses Math, CastleUtils;

{ TQuaternion ---------------------------------------------------------------- }

procedure TQuaternion.ToAxisAngle(out Axis: TVector3;
  out AngleRad: Single);
{ Data is a normalized quaternion, so
    Data.Vector = Sin(AngleRad / 2) * Axis
    Data.Real = Cos(AngleRad / 2)
}
var
  HalfAngle, SinHalfAngle: Single;
begin
  { Use Clamped to secure against cosinus being slightly outside [-1,1], like in SLerp. }
  HalfAngle := ArcCos(Clamped(Data.Real, -1, 1));

  SinHalfAngle := Sin(HalfAngle);
  AngleRad := HalfAngle * 2;
  if IsZero(SinHalfAngle) then
  begin
    { Then Data.Vector must be zero also... How could this happen?
      SinHalfAngle = 0 means that HalfAngle = Pi * K (e.g. 0).
      So Angle = 2 * Pi * K so there's no rotation actually happening...

      Which means that any Axis is Ok (but return anything normalized,
      to keep assertion that returned Axis is normalized). }
    Axis := Vector3(0, 0, 1);
  end else
    Axis := Data.Vector * (1 / SinHalfAngle);
end;

function TQuaternion.ToAxisAngle: TVector4;
var
  Axis: TVector3 absolute Result;
begin
  ToAxisAngle(Axis, Result.Data[3]);
end;

function TQuaternion.Conjugate: TQuaternion;
begin
  Result.Data.Vector := -Data.Vector;
  Result.Data.Real := Data.Real;
end;

procedure TQuaternion.ConjugateMe;
begin
  Data.Vector := -Data.Vector;
end;

function TQuaternion.Rotate(const Point: TVector4): TVector4;
begin
  Result := (Self * TQuaternion(Point)).Data.Vector4;
  Result := (TQuaternion(Result) * Conjugate).Data.Vector4;
end;

function TQuaternion.Rotate(const Point: TVector3): TVector3;
var
  P4: TVector4;
begin
  P4 := Vector4(Point, 0);
  P4 := Rotate(P4);
  Result := P4.XYZ;
end;

function QuatToRotationMatrix(const X, Y, Z, W: Single): TMatrix4;
var
  XX, YY, ZZ: Single;
begin
  XX := Sqr(X);
  YY := Sqr(Y);
  ZZ := Sqr(Z);

  { row 0 }
  Result.Data[0, 0] := 1 - 2 * (YY + ZZ);
  Result.Data[1, 0] := 2 * ( X * Y - W * Z );
  Result.Data[2, 0] := 2 * ( X * Z + W * Y );
  Result.Data[3, 0] := 0;

  { row 1 }
  Result.Data[0, 1] := 2 * ( X * Y + W * Z );
  Result.Data[1, 1] := 1 - 2 * (XX + ZZ);
  Result.Data[2, 1] := 2 * ( Y * Z - W * X );
  Result.Data[3, 1] := 0;

  { row 2 }
  Result.Data[0, 2] := 2 * ( X * Z - W * Y );
  Result.Data[1, 2] := 2 * ( Y * Z + W * X );
  Result.Data[2, 2] := 1 - 2 * (XX + YY);
  Result.Data[3, 2] := 0;

  { row 3 - like in identity matrix,
    only 3x3 matrix is interesting in rotations. }
  Result.Data[0, 3] := 0;
  Result.Data[1, 3] := 0;
  Result.Data[2, 3] := 0;
  Result.Data[3, 3] := 1;
end;

function TQuaternion.ToRotationMatrix: TMatrix4;
begin
  Result := QuatToRotationMatrix(
    Data.Vector.Data[0],
    Data.Vector.Data[1],
    Data.Vector.Data[2],
    Data.Real);
end;

procedure TQuaternion.Normalize;
begin
  NormalizeMe;
end;

procedure TQuaternion.NormalizeMe;
var
  Len: Single;
begin
  Len := Data.Vector4.Length;
  if Len <> 0 then
  begin
    Len := 1/Len;
    Data.Vector.Data[0] := Data.Vector.Data[0] * Len;
    Data.Vector.Data[1] := Data.Vector.Data[1] * Len;
    Data.Vector.Data[2] := Data.Vector.Data[2] * Len;
    Data.Real := Data.Real * Len;
  end;
end;

procedure TQuaternion.LazyNormalize;
begin
  LazyNormalizeMe;
end;

procedure TQuaternion.LazyNormalizeMe;
var
  Len: Single;
begin
  Len := Data.Vector4.LengthSqr;
  if (Len - 1) > 0.001 then
  begin
    { tests: Writeln('quat lazily normed'); }
    Len := Sqrt(Len);
    if Len <> 0 then
    begin
      Len := 1/Len;
      Data.Vector.Data[0] := Data.Vector.Data[0] * Len;
      Data.Vector.Data[1] := Data.Vector.Data[1] * Len;
      Data.Vector.Data[2] := Data.Vector.Data[2] * Len;
      Data.Real := Data.Real * Len;
    end;
  end;
end;

class operator TQuaternion.{$ifdef FPC}*{$else}Multiply{$endif} (const Q1, Q2: TQuaternion): TQuaternion;
begin
  Result.Data.Vector :=
    TVector3.CrossProduct(Q1.Data.Vector, Q2.Data.Vector) +
    (Q1.Data.Vector * Q2.Data.Real) +
    (Q2.Data.Vector * Q1.Data.Real);

  Result.Data.Real := Q1.Data.Real * Q2.Data.Real -
    TVector3.DotProduct(Q1.Data.Vector, Q2.Data.Vector);
end;

{ routines ------------------------------------------------------------------- }

function QuatFromAxisAngle(const Axis: TVector3;
  const AngleRad: Single; const NormalizeAxis: boolean): TQuaternion;
var
  SinHalfAngle, CosHalfAngle: Float;
begin
  { The quaternion requires half angles. }
  SinCos(AngleRad / 2, SinHalfAngle, CosHalfAngle);

  if NormalizeAxis then
  begin
    { protect from zero-length axis
      (testcase: code/tovrmlx3d demo-models/castle-anim-frames/simple/cube_opening.castle-anim-frames ) }
    if Axis.IsZero then
      Exit(TQuaternion.ZeroRotation);
    SinHalfAngle := SinHalfAngle / Axis.Length;
  end;

  Result.Data.Vector := Axis * SinHalfAngle;
  Result.Data.Real := CosHalfAngle;
end;

function QuatFromAxisAngle(const AxisAngle: TVector4;
  const NormalizeAxis: boolean): TQuaternion;
var
  Axis: TVector3 absolute AxisAngle;
begin
  Result := QuatFromAxisAngle(Axis, AxisAngle.Data[3], NormalizeAxis);
end;

{ For SLerp and NLerp implementations, see
  http://www.3dkingdoms.com/weekly/weekly.php?a=36
  http://www.3dkingdoms.com/weekly/quat.h
  http://number-none.com/product/Understanding%20Slerp,%20Then%20Not%20Using%20It/
  http://en.wikipedia.org/wiki/Slerp
}

function SLerp(const A: Single; const Q1, Q2: TQuaternion): TQuaternion;
var
  W1, W2, NegateOneQuaternion: Single;
  CosTheta, Theta: Float;
  SinTheta: Single;
begin
  CosTheta := TVector4.DotProduct(Q1.Data.Vector4, Q2.Data.Vector4);

  { Following wikipedia:
    Long paths can be prevented by negating one end if the dot product,
    CosTheta, is negative. See also
    http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/slerp/index.htm
    http://www.shatters.net/forum/viewtopic.php?f=4&t=10955&p=86203

    We do it by NegateOneQuaternion, either 1 (not negate) or -1 (negate).
    Later W1 will be multiplied by this.
    This way the actual negation of Q1 happens when it's multiplied by W1,
    so at ~zero cost. }
  if CosTheta < 0 then
  begin
    NegateOneQuaternion := -1;
    CosTheta := -CosTheta;
  end else
    NegateOneQuaternion := 1;

  { Sometimes CosTheta may get slightly > 1, and then ArcCos fails with
    EInvalidArgument. Testcase: demo_models/x3d/orientation_cos_1.x3d
    with view3dscene. }
  MinVar(CosTheta, 1);

  Theta := ArcCos(CosTheta);
  SinTheta := Sin(Theta);
  if SinTheta > 0.001 then
  begin
    W1 := NegateOneQuaternion * Sin( (1-A) * Theta ) / SinTheta;
    W2 :=                       Sin(    A  * Theta ) / SinTheta;
  end else
  begin
    { Theta ~= 0, so both rotations equal (or opposite, in which case
      result in undefined anyway). }
    W1 := 1 - A;
    W2 := A;
  end;
  Result.Data.Vector4 := (Q1.Data.Vector4 * W1) + (Q2.Data.Vector4 * W2);
end;

function SLerp(const A: Single; const Rot1, Rot2: TVector4): TVector4;
begin
  Result := SLerp(A,
    QuatFromAxisAngle(Rot1, true),
    QuatFromAxisAngle(Rot2, true)).ToAxisAngle;
end;

function NLerp(const A: Single; const Q1, Q2: TQuaternion;
  const ForceShortestPath: boolean): TQuaternion;
begin
  if ForceShortestPath and (TVector4.DotProduct(Q1.Data.Vector4, Q2.Data.Vector4) < 0) then
  begin
    { negate one quaternion }
    Result.Data.Vector4 := Lerp(A, -Q1.Data.Vector4, Q2.Data.Vector4);
  end else
    Result.Data.Vector4 := Lerp(A, Q1.Data.Vector4, Q2.Data.Vector4);

  Result.NormalizeMe;
end;

function NLerp(const A: Single; const Rot1, Rot2: TVector4;
  const ForceShortestPath: boolean): TVector4;
begin
  Result := NLerp(A,
    QuatFromAxisAngle(Rot1, true),
    QuatFromAxisAngle(Rot2, true),
    ForceShortestPath).ToAxisAngle;
end;

end.
