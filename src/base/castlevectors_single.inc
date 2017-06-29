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

{ Inverse the matrix, trying harder (but possibly slower).

  Basically, they internally calculate determinant and then calculate
  inverse using this determinant. Return @false if the determinant is zero.

  The main feature is that Single precision versions actually internally
  calculate everything (determinant and inverse) in Double precision.
  This gives better accuracy, and safety from matrices with very very small
  (but not zero) determinants.

  This is quite important for many matrices. For example, a 4x4 matrix
  with scaling = 1/200 (which can be easily found in practice,
  see e.g. castle/data/levels/gate/gate_processed.wrl) already
  has determinant = 1/8 000 000, which will not pass Zero test
  (with SingleEqualityEpsilon). But it's possible to calculate it
  (even on Single precision, although safer in Double precision).

  @groupBegin }
function TryMatrixInverse(const M: TMatrix2Single; out MInverse: TMatrix2Single): boolean; overload;
function TryMatrixInverse(const M: TMatrix3Single; out MInverse: TMatrix3Single): boolean; overload;
function TryMatrixInverse(const M: TMatrix4Single; out MInverse: TMatrix4Single): boolean; overload;
{ @groupEnd }

{ Convert string to vector. Each component is simply parsed by StrToFloat,
  and components must be separated by whitespace (see @link(WhiteSpaces) constant).
  @raises(EConvertError In case of problems during conversion (invalid float
    or unexpected string end or expected but missed string end).)
  @groupBegin }
function Vector2SingleFromStr(const S: string): TVector2Single;
function Vector3SingleFromStr(const S: string): TVector3Single;
function Vector4SingleFromStr(const S: string): TVector4Single;
{ @groupEnd }

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

{$ifdef CASTLE_HAS_DOUBLE_PRECISION}

{ When CASTLE_HAS_DOUBLE_PRECISION is not defined, use slower implementations
  of TryMatrixInverse, that convert to Double precision. }

function TryMatrixInverse(const M: TMatrix2Single; out MInverse: TMatrix2Single): boolean;
var
  D: Double;
  MD, MDInverse: TMatrix2Double;
begin
  MD := Matrix2Double(M);
  D := MatrixDeterminant(MD);
  Result := not Zero(D);
  if Result then
  begin
    MDInverse := MatrixInverse(MD, D);
    MInverse := Matrix2Single(MDInverse);
  end;
end;

function TryMatrixInverse(const M: TMatrix3Single; out MInverse: TMatrix3Single): boolean;
var
  D: Double;
  MD, MDInverse: TMatrix3Double;
begin
  MD := Matrix3Double(M);
  D := MatrixDeterminant(MD);
  Result := not Zero(D);
  if Result then
  begin
    MDInverse := MatrixInverse(MD, D);
    MInverse := Matrix3Single(MDInverse);
  end;
end;

function TryMatrixInverse(const M: TMatrix4Single; out MInverse: TMatrix4Single): boolean;
var
  D: Double;
  MD, MDInverse: TMatrix4Double;
begin
  MD := Matrix4Double(M);
  D := MatrixDeterminant(MD);
  Result := not Zero(D);
  if Result then
  begin
    MDInverse := MatrixInverse(MD, D);
    MInverse := Matrix4Single(MDInverse);
  end;
end;

{$else}

{ When CASTLE_HAS_DOUBLE_PRECISION is not defined, use simpler implementations
  of TryMatrixInverse, that stay within Single precision. }

function TryMatrixInverse(const M: TMatrix2Single; out MInverse: TMatrix2Single): boolean;
var
  D: Single;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function TryMatrixInverse(const M: TMatrix3Single; out MInverse: TMatrix3Single): boolean;
var
  D: Single;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;

function TryMatrixInverse(const M: TMatrix4Single; out MInverse: TMatrix4Single): boolean;
var
  D: Single;
begin
  D := MatrixDeterminant(M);
  Result := not Zero(D);
  if Result then
    MInverse := MatrixInverse(M, D);
end;
{$endif CASTLE_HAS_DOUBLE_PRECISION}

function Vector2SingleFromStr(const S: string): TVector2Single;
var
  SPosition: Integer;
begin
  SPosition := 1;
  Result[0] := StrToFloat(NextToken(S, SPosition));
  Result[1] := StrToFloat(NextToken(S, SPosition));
  if NextToken(S, SPosition) <> '' then
    raise EConvertError.Create('Expected end of data when reading vector from string');
end;

function Vector3SingleFromStr(const s: string): TVector3Single;
{$I castlevectors_vector3fromstr.inc}

function Vector4SingleFromStr(const S: string): TVector4Single;
var
  SPosition: Integer;
begin
  SPosition := 1;
  Result[0] := StrToFloat(NextToken(S, SPosition));
  Result[1] := StrToFloat(NextToken(S, SPosition));
  Result[2] := StrToFloat(NextToken(S, SPosition));
  Result[3] := StrToFloat(NextToken(S, SPosition));
  if NextToken(s, SPosition) <> '' then
    raise EConvertError.Create('Expected end of data when reading vector from string');
end;

{$endif read_implementation}
