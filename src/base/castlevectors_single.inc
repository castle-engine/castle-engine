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

{$endif read_implementation}
