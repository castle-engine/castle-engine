{%MainUnit castlevectors.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Deprecated definitions (types and functions), existing only to keep
  backward-compatibility with older versions of Castle Game Engine. }

{$ifdef read_interface}

const
  { Vector with all components zero. }
  ZeroVector2Single: TVector2 = (X: 0; Y: 0) deprecated 'use TVector2.Zero';
  ZeroVector3Single: TVector3 = (X: 0; Y: 0; Z: 0) deprecated 'use TVector3.Zero';
  ZeroVector4Single: TVector4 = (X: 0; Y: 0; Z: 0; W: 0) deprecated 'use TVector4.Zero';

  ZeroVector2Double: TVector2Double = (X: 0; Y: 0) deprecated 'use TVector2Double.Zero';
  ZeroVector3Double: TVector3Double = (X: 0; Y: 0; Z: 0) deprecated 'use TVector3Double.Zero';
  ZeroVector4Double: TVector4Double = (X: 0; Y: 0; Z: 0; W: 0) deprecated 'use TVector4Double.Zero';

  ZeroVector2Byte: TVector2Byte = (X: 0; Y: 0) deprecated 'use TVector2Byte.Zero';
  ZeroVector3Byte: TVector3Byte = (X: 0; Y: 0; Z: 0) deprecated 'use TVector3Byte.Zero';
  ZeroVector4Byte: TVector4Byte = (X: 0; Y: 0; Z: 0; W: 0) deprecated 'use TVector4Byte.Zero';

  ZeroVector2Integer: TVector2Integer = (X: 0; Y: 0) deprecated 'use TVector2Integer.Zero';
  ZeroVector3Integer: TVector3Integer = (X: 0; Y: 0; Z: 0) deprecated 'use TVector3Integer.Zero';
  ZeroVector4Integer: TVector4Integer = (X: 0; Y: 0; Z: 0; W: 0) deprecated 'use TVector4Integer.Zero';

  ZeroVector2Cardinal: TVector2Cardinal = (X: 0; Y: 0) deprecated 'use TVector2Cardinal.Zero';
  ZeroVector3Cardinal: TVector3Cardinal = (X: 0; Y: 0; Z: 0) deprecated 'use TVector3Cardinal.Zero';
  ZeroVector4Cardinal: TVector4Cardinal = (X: 0; Y: 0; Z: 0; W: 0) deprecated 'use TVector4Cardinal.Zero';

  ZeroMatrix2Single: TMatrix2 = (Data: ((0, 0), (0, 0))) deprecated 'use TMatrix2.Zero';
  ZeroMatrix3Single: TMatrix3 = (Data: ((0, 0, 0), (0, 0, 0), (0, 0, 0))) deprecated 'use TMatrix3.Zero';
  ZeroMatrix4Single: TMatrix4 = (Data: ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0))) deprecated 'use TMatrix4.Zero';

  ZeroMatrix2Double: TMatrix2Double = (Data: ((0, 0), (0, 0))) deprecated 'use TMatrix2Double.Zero';
  ZeroMatrix3Double: TMatrix3Double = (Data: ((0, 0, 0), (0, 0, 0), (0, 0, 0))) deprecated 'use TMatrix3Double.Zero';
  ZeroMatrix4Double: TMatrix4Double = (Data: ((0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0))) deprecated 'use TMatrix4Double.Zero';

  IdentityMatrix2Single: TMatrix2 = (Data: ((1, 0), (0, 1))) deprecated 'use TMatrix2.Identity';
  IdentityMatrix3Single: TMatrix3 = (Data: ((1, 0, 0), (0, 1, 0), (0, 0, 1))) deprecated 'use TMatrix3.Identity';
  IdentityMatrix4Single: TMatrix4 = (Data: ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1))) deprecated 'use TMatrix4.Identity';

  IdentityMatrix2Double: TMatrix2Double = (Data: ((1, 0), (0, 1))) deprecated 'use TMatrix2Double.Identity';
  IdentityMatrix3Double: TMatrix3Double = (Data: ((1, 0, 0), (0, 1, 0), (0, 0, 1))) deprecated 'use TMatrix3Double.Identity';
  IdentityMatrix4Double: TMatrix4Double = (Data: ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1))) deprecated 'use TMatrix4Double.Identity';

  UnitVector2Single: array [TVector2.TIndex] of TVector2 = (
    (X: 1; Y: 0),
    (X: 0; Y: 1)
  ) deprecated 'use TVector2.One';
  UnitVector3Single: array [TVector3.TIndex] of TVector3 = (
    (X: 1; Y: 0; Z: 0),
    (X: 0; Y: 1; Z: 0),
    (X: 0; Y: 0; Z: 1)
  ) deprecated 'use TVector3.One';
  UnitVector4Single: array [TVector4.TIndex] of TVector4 = (
    (X: 1; Y: 0; Z: 0; W: 0),
    (X: 0; Y: 1; Z: 0; W: 0),
    (X: 0; Y: 0; Z: 1; W: 0),
    (X: 0; Y: 0; Z: 0; W: 1)
  ) deprecated 'use TVector4.One';

type
  TVector2Single = TVector2 deprecated;
  TVector3Single = TVector3 deprecated;
  TVector4Single = TVector4 deprecated;

  TMatrix2Single = TMatrix2 deprecated;
  TMatrix3Single = TMatrix3 deprecated;
  TMatrix4Single = TMatrix4 deprecated;

  TVector2SingleList = TVector2List deprecated;
  TVector3SingleList = TVector3List deprecated;
  TVector4SingleList = TVector4List deprecated;

  TMatrix3SingleList = TMatrix3List deprecated;
  TMatrix4SingleList = TMatrix4List deprecated;

  PVector2Single = PVector2 deprecated;
  PVector3Single = PVector3 deprecated;
  PVector4Single = PVector4 deprecated;

  PMatrix2Single = PMatrix2 deprecated;
  PMatrix3Single = PMatrix3 deprecated;
  PMatrix4Single = PMatrix4 deprecated;

{ Construct an initialized TVector2 value. }
function Vector2Single(const X, Y: Single): TVector2; overload; deprecated 'use Vector2';
function Vector2Single(const V: TVector2Double): TVector2; overload; deprecated 'use Vector2';

{ Construct an initialized TVector3 value. }
function Vector3Single(const X, Y, Z: Single): TVector3; overload; deprecated 'use Vector3';
function Vector3Single(const V: TVector2; const Z: Single): TVector3; overload; deprecated 'use Vector3';
function Vector3Single(const V: TVector3Double): TVector3; overload; deprecated 'use Vector3';

{ Construct an initialized TVector4 value. }
function Vector4Single(const X, Y, Z, W: Single): TVector4; overload; deprecated 'use Vector4';
function Vector4Single(const V: TVector3; const W: Single): TVector4; overload; deprecated 'use Vector4';
function Vector4Single(const V: TVector4Double): TVector4; overload; deprecated 'use Vector4';

{ Various compatibility functions (on Single) }

function FloatsEqual(const A, B: Single): boolean; overload; deprecated 'use Math.SameValue';
function FloatsEqual(const A, B: Single; const Epsilon: Single): boolean; overload; deprecated 'use Math.SameValue';
function FloatToNiceStr(const A: Single): string; overload; deprecated 'use Format(''%f'', [Value])';
function FloatToRawStr(const A: Single): string; overload; deprecated 'use Format(''%g'', [Value])';
function Zero(const A: Single): boolean; overload; deprecated 'use Math.IsZero';

{ Various compatibility functions (on vectors and matrices) }

function VectorAdd(const V1, V2: TVector2): TVector2; overload; deprecated 'use V1 + V2';
function VectorAdd(const V1, V2: TVector3): TVector3; overload; deprecated 'use V1 + V2';
function VectorAdd(const V1, V2: TVector4): TVector4; overload; deprecated 'use V1 + V2';
function VectorSubtract(const V1, V2: TVector2): TVector2; overload; deprecated 'use V1 - V2';
function VectorSubtract(const V1, V2: TVector3): TVector3; overload; deprecated 'use V1 - V2';
function VectorSubtract(const V1, V2: TVector4): TVector4; overload; deprecated 'use V1 - V2';
function VectorMultiplyComponents(const V1, V2: TVector2): TVector2; overload; deprecated 'use V1 * V2';
function VectorMultiplyComponents(const V1, V2: TVector3): TVector3; overload; deprecated 'use V1 * V2';
function VectorMultiplyComponents(const V1, V2: TVector4): TVector4; overload; deprecated 'use V1 * V2';
function VectorProduct(const V1, V2: TVector3): TVector3; overload; deprecated 'use TVector3.CrossProduct';
function Normalized(const V: TVector2): TVector2; overload; deprecated 'use V.Normalize method';
function Normalized(const V: TVector3): TVector3; overload; deprecated 'use V.Normalize method';
procedure NormalizeVar(var V: TVector2); overload; deprecated 'use V := V.Normalize';
procedure NormalizeVar(var V: TVector3); overload; deprecated 'use V := V.Normalize';
function MatrixMult(const M1, M2: TMatrix4): TMatrix4; overload; deprecated 'use * operator to multiply matrices';
function MatrixMultPoint(const M: TMatrix4; const P: TVector2): TVector2; overload; deprecated 'use M.MultPoint method';
function MatrixMultPoint(const M: TMatrix4; const P: TVector3): TVector3; overload; deprecated 'use M.MultPoint method';
function MatrixMultDirection(const M: TMatrix4; const P: TVector2): TVector2; overload; deprecated 'use M.MultDirection method';
function MatrixMultDirection(const M: TMatrix4; const P: TVector3): TVector3; overload; deprecated 'use M.MultDirection method';
function VectorLenSqr(const V: TVector2): Single; overload; deprecated 'use V.LengthSqr';
function VectorLenSqr(const V: TVector3): Single; overload; deprecated 'use V.LengthSqr';
function VectorLenSqr(const V: TVector4): Single; overload; deprecated 'use V.LengthSqr';
function VectorLen(const V: TVector2): Single; overload; deprecated 'use V.Length';
function VectorLen(const V: TVector3): Single; overload; deprecated 'use V.Length';
function VectorLen(const V: TVector4): Single; overload; deprecated 'use V.Length';
function VectorAverage(const V: TVector3): Single; overload; deprecated 'use V.Average';
function VectorToRawStr(const V: TVector2): string; overload; deprecated 'use V.ToRawString';
function VectorToRawStr(const V: TVector3): string; overload; deprecated 'use V.ToRawString';
function VectorToRawStr(const V: TVector4): string; overload; deprecated 'use V.ToRawString';
function VectorToNiceStr(const V: TVector2): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector3): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector4): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector2Integer): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector3Integer): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector4Integer): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector2Cardinal): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector3Cardinal): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector4Cardinal): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector2Byte): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector3Byte): string; overload; deprecated 'use V.ToString';
function VectorToNiceStr(const V: TVector4Byte): string; overload; deprecated 'use V.ToString';
function ZeroVector(const V: TVector2): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector3): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector4): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector2Byte): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector3Byte): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector4Byte): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector2Integer): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector3Integer): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector4Integer): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector2Cardinal): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector3Cardinal): boolean; overload; deprecated 'use V.IsZero';
function ZeroVector(const V: TVector4Cardinal): boolean; overload; deprecated 'use V.IsZero';
function PerfectlyZeroVector(const V: TVector2): boolean; overload; deprecated 'use V.IsPerfectlyZero';
function PerfectlyZeroVector(const V: TVector3): boolean; overload; deprecated 'use V.IsPerfectlyZero';
function PerfectlyZeroVector(const V: TVector4): boolean; overload; deprecated 'use V.IsPerfectlyZero';
function VectorAdjustToLength(const V: TVector2; const NewLength: Single): TVector2; overload; deprecated 'use V.AdjustToLength';
function VectorAdjustToLength(const V: TVector3; const NewLength: Single): TVector3; overload; deprecated 'use V.AdjustToLength';
function VectorAdjustToLength(const V: TVector4; const NewLength: Single): TVector4; overload; deprecated 'use V.AdjustToLength';
function Vector2SingleFromStr(const S: string): TVector2; overload; deprecated 'use Vector2FromStr';
function Vector3SingleFromStr(const s: string): TVector3; overload; deprecated 'use Vector3FromStr';
function Vector4SingleFromStr(const S: string): TVector4; overload; deprecated 'use Vector4FromStr';
function VectorDotProduct(const V1, V2: TVector2): Single; overload; deprecated 'use TVector2.DotProduct(V1, V2)';
function VectorDotProduct(const V1, V2: TVector3): Single; overload; deprecated 'use TVector3.DotProduct(V1, V2)';
function VectorDotProduct(const V1, V2: TVector4): Single; overload; deprecated 'use TVector4.DotProduct(V1, V2)';
function VectorsEqual(const V1, V2: TVector2): boolean; overload; deprecated 'use TVector2.Equals(V1, V2)';
function VectorsEqual(const V1, V2: TVector3): boolean; overload; deprecated 'use TVector3.Equals(V1, V2)';
function VectorsEqual(const V1, V2: TVector4): boolean; overload; deprecated 'use TVector4.Equals(V1, V2)';
function VectorsEqual(const V1, V2: TVector2; const Epsilon: Single): boolean; overload; deprecated 'use TVector2.Equals(V1, V2, Epsilon)';
function VectorsEqual(const V1, V2: TVector3; const Epsilon: Single): boolean; overload; deprecated 'use TVector3.Equals(V1, V2, Epsilon)';
function VectorsEqual(const V1, V2: TVector4; const Epsilon: Single): boolean; overload; deprecated 'use TVector4.Equals(V1, V2, Epsilon)';
function VectorsPerfectlyEqual(const V1, V2: TVector2): boolean; overload; deprecated 'use TVector2.PerfectlyEquals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector3): boolean; overload; deprecated 'use TVector3.PerfectlyEquals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector4): boolean; overload; deprecated 'use TVector4.PerfectlyEquals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector2Byte): boolean; overload; deprecated 'use TVector2Byte.Equals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector3Byte): boolean; overload; deprecated 'use TVector3Byte.Equals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector4Byte): boolean; overload; deprecated 'use TVector4Byte.Equals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector2Integer): boolean; overload; deprecated 'use TVector2Integer.Equals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector3Integer): boolean; overload; deprecated 'use TVector3Integer.Equals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector4Integer): boolean; overload; deprecated 'use TVector4Integer.Equals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector2Cardinal): boolean; overload; deprecated 'use TVector2Cardinal.Equals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector3Cardinal): boolean; overload; deprecated 'use TVector3Cardinal.Equals(V1, V2)';
function VectorsPerfectlyEqual(const V1, V2: TVector4Cardinal): boolean; overload; deprecated 'use TVector4Cardinal.Equals(V1, V2)';
function TryMatrixInverse(const M: TMatrix2; out MInverse: TMatrix2): boolean; overload; overload; deprecated 'use TMatrix2.TryInverse';
function TryMatrixInverse(const M: TMatrix3; out MInverse: TMatrix3): boolean; overload; overload; deprecated 'use TMatrix3.TryInverse';
function TryMatrixInverse(const M: TMatrix4; out MInverse: TMatrix4): boolean; overload; overload; deprecated 'use TMatrix4.TryInverse';
function MatrixToRawStr(const M: TMatrix2; const LineIndent: string): string; overload; overload; deprecated 'use TMatrix2.ToRawString';
function MatrixToRawStr(const M: TMatrix3; const LineIndent: string): string; overload; overload; deprecated 'use TMatrix3.ToRawString';
function MatrixToRawStr(const M: TMatrix4; const LineIndent: string): string; overload; overload; deprecated 'use TMatrix4.ToRawString';
function MatrixToNiceStr(const M: TMatrix2; const LineIndent: string): string; overload; overload; deprecated 'use TMatrix2.ToString';
function MatrixToNiceStr(const M: TMatrix3; const LineIndent: string): string; overload; overload; deprecated 'use TMatrix3.ToString';
function MatrixToNiceStr(const M: TMatrix4; const LineIndent: string): string; overload; overload; deprecated 'use TMatrix4.ToString';
function MatricesPerfectlyEqual(const V1, V2: TMatrix2): boolean; overload; deprecated 'use TMatrix2.PerfectlyEquals(V1, V2)';
function MatricesPerfectlyEqual(const V1, V2: TMatrix3): boolean; overload; deprecated 'use TMatrix3.PerfectlyEquals(V1, V2)';
function MatricesPerfectlyEqual(const V1, V2: TMatrix4): boolean; overload; deprecated 'use TMatrix4.PerfectlyEquals(V1, V2)';
function MatrixRow(const M: TMatrix2; const Row: Integer): TVector2; overload; overload; deprecated 'use TMatrix2.Rows[Row]';
function MatrixRow(const M: TMatrix3; const Row: Integer): TVector3; overload; overload; deprecated 'use TMatrix3.Rows[Row]';
function MatrixRow(const M: TMatrix4; const Row: Integer): TVector4; overload; overload; deprecated 'use TMatrix4.Rows[Row]';
function Vector2SingleCut(const V: TVector3): TVector2; overload; deprecated 'use V.XY';
function Vector3SingleCut(const V: TVector4): TVector3; overload; deprecated 'use V.XYZ';
function Matrix3Single(const V: TMatrix3Double): TMatrix3; overload; deprecated 'use Matrix3';
function Matrix4Single(const V: TMatrix4Double): TMatrix4; overload; deprecated 'use Matrix4';

{$endif read_interface}

{$ifdef read_implementation}

{ Construct 2D float vectors ------------------------------------------------------------ }

function Vector2Single(const X, Y: Single): TVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector2Single(const V: TVector2Double): TVector2;
begin
  Result.X := V.X;
  Result.Y := V.Y;
end;

{ Construct 3D float vectors ------------------------------------------------------------ }

function Vector3Single(const X, Y, Z: Single): TVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3Single(const V: TVector2; const Z: Single): TVector3;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := Z;
end;

function Vector3Single(const V: TVector3Double): TVector3;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
end;

{ Construct 4D float vectors ------------------------------------------------------------ }

function Vector4Single(const X, Y, Z, W: Single): TVector4;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4Single(const V: TVector3; const W: Single): TVector4;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
  Result.W := W;
end;

function Vector4Single(const V: TVector4Double): TVector4;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
  Result.W := V.W;
end;

{ float compatibility functions ---------------------------------------------- }

function FloatsEqual(const A, B: Single): boolean;
begin
  Result := SameValue(A, B);
end;

function FloatsEqual(const A, B, Epsilon: Single): boolean;
begin
  Result := SameValue(A, B, Epsilon);
end;

function FloatToNiceStr(const A: Single): string;
begin
  Result := Format('%f', [A]);
end;

function FloatToRawStr(const A: Single): string;
begin
  Result := Format('%g', [A]);
end;

function Zero(const A: Single): boolean;
begin
  Result := IsZero(A);
end;

{ various compatibility functions -------------------------------------------- }

function VectorAdd(const V1, V2: TVector2): TVector2;
begin
  Result := V1 + V2;
end;

function VectorAdd(const V1, V2: TVector3): TVector3;
begin
  Result := V1 + V2;
end;

function VectorAdd(const V1, V2: TVector4): TVector4;
begin
  Result := V1 + V2;
end;

function VectorSubtract(const V1, V2: TVector2): TVector2;
begin
  Result := V1 - V2;
end;

function VectorSubtract(const V1, V2: TVector3): TVector3;
begin
  Result := V1 - V2;
end;

function VectorSubtract(const V1, V2: TVector4): TVector4;
begin
  Result := V1 - V2;
end;

function VectorMultiplyComponents(const V1, V2: TVector2): TVector2;
begin
  Result := V1 * V2;
end;

function VectorMultiplyComponents(const V1, V2: TVector3): TVector3;
begin
  Result := V1 * V2;
end;

function VectorMultiplyComponents(const V1, V2: TVector4): TVector4;
begin
  Result := V1 * V2;
end;

function VectorProduct(const V1, V2: TVector3): TVector3;
begin
  Result := TVector3.CrossProduct(V1, V2);
end;

function Normalized(const V: TVector2): TVector2;
begin
  Result := V.Normalize;
end;

function Normalized(const V: TVector3): TVector3;
begin
  Result := V.Normalize;
end;

procedure NormalizeVar(var V: TVector2);
begin
  V := V.Normalize;
end;

procedure NormalizeVar(var V: TVector3);
begin
  V := V.Normalize;
end;

function MatrixMult(const M1, M2: TMatrix4): TMatrix4;
begin
  Result := M1 * M2;
end;

function MatrixMultPoint(const M: TMatrix4; const P: TVector2): TVector2;
begin
  Result := M.MultPoint(P);
end;

function MatrixMultPoint(const M: TMatrix4; const P: TVector3): TVector3;
begin
  Result := M.MultPoint(P);
end;

function MatrixMultDirection(const M: TMatrix4; const P: TVector2): TVector2;
begin
  Result := M.MultDirection(P);
end;

function MatrixMultDirection(const M: TMatrix4; const P: TVector3): TVector3;
begin
  Result := M.MultDirection(P);
end;

function VectorLenSqr(const V: TVector2): Single;
begin
  Result := V.LengthSqr;
end;

function VectorLenSqr(const V: TVector3): Single;
begin
  Result := V.LengthSqr;
end;

function VectorLenSqr(const V: TVector4): Single;
begin
  Result := V.LengthSqr;
end;

function VectorLen(const V: TVector2): Single;
begin
  Result := V.Length;
end;

function VectorLen(const V: TVector3): Single;
begin
  Result := V.Length;
end;

function VectorLen(const V: TVector4): Single;
begin
  Result := V.Length;
end;

function VectorAverage(const V: TVector3): Single;
begin
  Result := V.Average;
end;

function VectorToRawStr(const V: TVector2): string;
begin
  Result := V.ToRawString;
end;

function VectorToRawStr(const V: TVector3): string;
begin
  Result := V.ToRawString;
end;

function VectorToRawStr(const V: TVector4): string;
begin
  Result := V.ToRawString;
end;

function VectorToNiceStr(const V: TVector2): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector3): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector4): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector2Integer): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector3Integer): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector4Integer): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector2Cardinal): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector3Cardinal): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector4Cardinal): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector2Byte): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector3Byte): string;
begin
  Result := V.ToString;
end;

function VectorToNiceStr(const V: TVector4Byte): string;
begin
  Result := V.ToString;
end;

function ZeroVector(const V: TVector2): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector3): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector4): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector2Byte): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector3Byte): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector4Byte): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector2Integer): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector3Integer): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector4Integer): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector2Cardinal): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector3Cardinal): boolean;
begin
  Result := V.IsZero;
end;

function ZeroVector(const V: TVector4Cardinal): boolean;
begin
  Result := V.IsZero;
end;

function PerfectlyZeroVector(const V: TVector2): boolean;
begin
  Result := V.IsPerfectlyZero;
end;

function PerfectlyZeroVector(const V: TVector3): boolean;
begin
  Result := V.IsPerfectlyZero;
end;

function PerfectlyZeroVector(const V: TVector4): boolean;
begin
  Result := V.IsPerfectlyZero;
end;

function VectorAdjustToLength(const V: TVector2; const NewLength: Single): TVector2;
begin
  Result := V.AdjustToLength(NewLength);
end;

function VectorAdjustToLength(const V: TVector3; const NewLength: Single): TVector3;
begin
  Result := V.AdjustToLength(NewLength);
end;

function VectorAdjustToLength(const V: TVector4; const NewLength: Single): TVector4;
begin
  Result := V.AdjustToLength(NewLength);
end;

function Vector2SingleFromStr(const S: string): TVector2;
begin
  Result := Vector2FromStr(S);
end;

function Vector3SingleFromStr(const s: string): TVector3;
begin
  Result := Vector3FromStr(S);
end;

function Vector4SingleFromStr(const S: string): TVector4;
begin
  Result := Vector4FromStr(S);
end;

function VectorDotProduct(const V1, V2: TVector2): Single;
begin
  Result := TVector2.DotProduct(V1, V2);
end;

function VectorDotProduct(const V1, V2: TVector3): Single;
begin
  Result := TVector3.DotProduct(V1, V2);
end;

function VectorDotProduct(const V1, V2: TVector4): Single;
begin
  Result := TVector4.DotProduct(V1, V2);
end;

function VectorsEqual(const V1, V2: TVector2): boolean;
begin
  Result := TVector2.Equals(V1, V2);
end;

function VectorsEqual(const V1, V2: TVector3): boolean;
begin
  Result := TVector3.Equals(V1, V2);
end;

function VectorsEqual(const V1, V2: TVector4): boolean;
begin
  Result := TVector4.Equals(V1, V2);
end;

function VectorsEqual(const V1, V2: TVector2; const Epsilon: Single): boolean;
begin
  Result := TVector2.Equals(V1, V2, Epsilon);
end;

function VectorsEqual(const V1, V2: TVector3; const Epsilon: Single): boolean;
begin
  Result := TVector3.Equals(V1, V2, Epsilon);
end;

function VectorsEqual(const V1, V2: TVector4; const Epsilon: Single): boolean;
begin
  Result := TVector4.Equals(V1, V2, Epsilon);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector2): boolean;
begin
  Result := TVector2.PerfectlyEquals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector3): boolean;
begin
  Result := TVector3.PerfectlyEquals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector4): boolean;
begin
  Result := TVector4.PerfectlyEquals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector2Byte): boolean;
begin
  Result := TVector2Byte.Equals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector3Byte): boolean;
begin
  Result := TVector3Byte.Equals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector4Byte): boolean;
begin
  Result := TVector4Byte.Equals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector2Integer): boolean;
begin
  Result := TVector2Integer.Equals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector3Integer): boolean;
begin
  Result := TVector3Integer.Equals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector4Integer): boolean;
begin
  Result := TVector4Integer.Equals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector2Cardinal): boolean;
begin
  Result := TVector2Cardinal.Equals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector3Cardinal): boolean;
begin
  Result := TVector3Cardinal.Equals(V1, V2);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector4Cardinal): boolean;
begin
  Result := TVector4Cardinal.Equals(V1, V2);
end;

function TryMatrixInverse(const M: TMatrix2; out MInverse: TMatrix2): boolean;
begin
  Result := M.TryInverse(MInverse);
end;

function TryMatrixInverse(const M: TMatrix3; out MInverse: TMatrix3): boolean;
begin
  Result := M.TryInverse(MInverse);
end;

function TryMatrixInverse(const M: TMatrix4; out MInverse: TMatrix4): boolean;
begin
  Result := M.TryInverse(MInverse);
end;

function MatrixToRawStr(const M: TMatrix2; const LineIndent: string): string;
begin
  Result := M.ToRawString(LineIndent);
end;

function MatrixToRawStr(const M: TMatrix3; const LineIndent: string): string;
begin
  Result := M.ToRawString(LineIndent);
end;

function MatrixToRawStr(const M: TMatrix4; const LineIndent: string): string;
begin
  Result := M.ToRawString(LineIndent);
end;

function MatrixToNiceStr(const M: TMatrix2; const LineIndent: string): string;
begin
  Result := M.ToString(LineIndent);
end;

function MatrixToNiceStr(const M: TMatrix3; const LineIndent: string): string;
begin
  Result := M.ToString(LineIndent);
end;

function MatrixToNiceStr(const M: TMatrix4; const LineIndent: string): string;
begin
  Result := M.ToString(LineIndent);
end;

function MatricesPerfectlyEqual(const V1, V2: TMatrix2): boolean;
begin
  Result := TMatrix2.PerfectlyEquals(V1, V2);
end;

function MatricesPerfectlyEqual(const V1, V2: TMatrix3): boolean;
begin
  Result := TMatrix3.PerfectlyEquals(V1, V2);
end;

function MatricesPerfectlyEqual(const V1, V2: TMatrix4): boolean;
begin
  Result := TMatrix4.PerfectlyEquals(V1, V2);
end;

function MatrixRow(const M: TMatrix2; const Row: Integer): TVector2;
begin
  Result := M.Rows[Row];
end;

function MatrixRow(const M: TMatrix3; const Row: Integer): TVector3;
begin
  Result := M.Rows[Row];
end;

function MatrixRow(const M: TMatrix4; const Row: Integer): TVector4;
begin
  Result := M.Rows[Row];
end;

function Vector2SingleCut(const V: TVector3): TVector2;
begin
  Result := V.XY;
end;

function Vector3SingleCut(const V: TVector4): TVector3;
begin
  Result := V.XYZ;
end;

function Matrix3Single(const V: TMatrix3Double): TMatrix3;
begin
  Result := Matrix3(V);
end;

function Matrix4Single(const V: TMatrix4Double): TMatrix4;
begin
  Result := Matrix4(V);
end;

{$endif read_implementation}
