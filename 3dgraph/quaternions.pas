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

{ Multiply two quaternions.

  Geometric interpretation: If these are unit quaternions representing
  rotations, multiplying them calculates one rotation that has the same
  effect as rotating by Q2 and then by Q1. }
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
  if IsZero(SinHalfAngle) then
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
  P4 := Vector4Single(Point, 1);
  P4 := QuatRotate(Q, P4);
  Result := Vector3SingleCut(P4);
end;

end.
