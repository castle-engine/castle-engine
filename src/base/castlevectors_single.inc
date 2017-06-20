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

{ Interface and implementation of CastleVectors
  for vectors and matrices with Single precision. }

{$ifdef read_interface}

{ Negate a rotation expressed as axis-angle (3 components for axis, 1 for angle).
  This simply negates the 4th vector component. }
function RotationNegate(const Rotation: TVector4Single): TVector4Single;

{ Rotate point in 2D, in a counter-clockwise fashion.
  AngleRad is in radians. }
function RotatePoint2D(const Point: TVector2Single; const AngleRad: Single): TVector2Single;

{ When you really, really must approximate a 3D scale by a single float.
  If your algorithm cannot handle non-uniform 3D scale,
  you have to approximate 3D scale by a single float.

  This is similar to an average of X, Y, Z, but it additionally
  secures in case some of X, Y, Z are negative and some are positive
  (like scale (-1, 1), common in 2D to flip horizontally).

  @groupBegin }
function Approximate3DScale(const X, Y, Z: Single): Single;
function Approximate3DScale(const V: TVector3Single): Single;
{ @groupEnd }

{ Normalize the first 3 vector components. For zero vectors, does nothing. }
procedure NormalizeVar3Singlev(vv: PVector3Single);

{$endif read_interface}

{$ifdef read_implementation}

function RotatePoint2D(const Point: TVector2Single; const AngleRad: Single): TVector2Single;
var
  AngleSin, AngleCos: Float;
  S, C: Single;
begin
  SinCos(AngleRad, AngleSin, AngleCos);
  { convert Float to Single once }
  S := AngleSin;
  C := AngleCos;
  Result[0] := Point[0] * C - Point[1] * S;
  Result[1] := Point[0] * S + Point[1] * C;
end;

function RotationNegate(const Rotation: TVector4Single): TVector4Single;
begin
  Result := Rotation;
  Result[3] := -Result[3];
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
    Result := (Abs(X) + Abs(Y) + Abs(Z)) / 3 else
    Result := (    X  +     Y  +     Z ) / 3;
end;

function Approximate3DScale(const V: TVector3Single): Single;
begin
  Result := Approximate3DScale(V[0], V[1], V[2]);
end;

procedure NormalizeVar3Singlev(vv: PVector3Single);
var
  Len: Single;
begin
  Len := Sqrt(
    Sqr(vv^[0]) +
    Sqr(vv^[1]) +
    Sqr(vv^[2]));
  if Len = 0 then exit;
  vv^[0] := vv^[0] / Len;
  vv^[1] := vv^[1] / Len;
  vv^[2] := vv^[2] / Len;
end;

{$endif read_implementation}
