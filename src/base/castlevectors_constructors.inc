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

{ Interface and implementation of CastleVectors constructors
  (functions that simply return appropriate types). }

{$ifdef read_interface}

{ }
function Vector2Cardinal(const x, y: Cardinal): TVector2Cardinal;
function Vector2Integer(const x, y: Integer): TVector2Integer;
function Vector2LongInt(const x, y: Longint): TVector2LongInt;
function Vector2SmallInt(const x, y: SmallInt): TVector2SmallInt;

function Vector2Single(const x, y: Single): TVector2Single; overload;
function Vector2Single(const V: TVector2Double): TVector2Single; overload;

function Vector2Double(const x, y: Double): TVector2Double;

function Vector3Single(const x, y: Single; const z: Single = 0.0): TVector3Single; overload;
function Vector3Single(const v3: TVector3Double): TVector3Single; overload;
function Vector3Single(const v3: TVector3Byte): TVector3Single; overload;
function Vector3Single(const v2: TVector2Single; const z: Single = 0.0): TVector3Single; overload;

function Vector3Longint(const p0, p1, p2: Longint): TVector3Longint;

function Vector3Double(const x, y: Double; const z: Double = 0.0): TVector3Double; overload;
function Vector3Double(const v: TVector3Single): TVector3Double; overload;
function Vector3Double(const v2: TVector2Double; const z: Double = 0.0): TVector3Double; overload;

function Vector4Single(const x, y: Single;
  const z: Single = 0; const w: Single = 1): TVector4Single; overload;
function Vector4Single(const v3: TVector3Single;
  const w: Single = 1): TVector4Single; overload;
function Vector4Single(const v2: TVector2Single;
  const z: Single = 0; const w: Single = 1): TVector4Single; overload;
function Vector4Single(const ub: TVector4Byte): TVector4Single; overload;
function Vector4Single(const V3: TVector3Byte; const W: Byte): TVector4Single; overload;
function Vector4Single(const v: TVector4Double): TVector4Single; overload;

function Vector4Double(const x, y, z ,w: Double): TVector4Double; overload;
function Vector4Double(const v: TVector4Single): TVector4Double; overload;

function Vector2Byte(x, y: Byte): TVector2Byte; overload;

function Vector3Byte(x, y, z: Byte): TVector3Byte; overload;

{ Convert float vectors into byte vectors.
  Each float component is converted such that float 0.0 (or less) results in
  0 byte, 1.0 (or more) results in byte 255 (note: not 256).
  Values between 0.0 and 1.0 are appropriately (linearly) converted
  into the byte range.
  @groupBegin }
function Vector3Byte(const v: TVector3Single): TVector3Byte; overload;
function Vector3Byte(const v: TVector3Double): TVector3Byte; overload;
function Vector4Byte(const f4: TVector4Single): TVector4Byte; overload;
{ @groupEnd }

function Vector4Byte(const x, y, z: Byte; const w: Byte = 255): TVector4Byte; overload;
function Vector4Byte(const f3: TVector3Byte; w: Byte): TVector4Byte; overload;

function Vector4Integer(const X, Y, Z, W: Integer): TVector4Integer;

{ Convert a point in homogeneous coordinates into normal 3D point.
  In other words, convert 4D @code((x, y, z, w)) into
  @code((x/w, y/w, z/w)). Make sure the 4th vector component <> 0. }
function Vector3SinglePoint(const v: TVector4Single): TVector3Single;

{ Convert 3D vector into 2D by simply discarding (ignoring) the last component. }
function Vector2SingleCut(const v: TVector3Single): TVector2Single;

{ Convert 4D vector into 3D by simply discarding (ignoring) the 4th vector
  component. }
function Vector3SingleCut(const v: TVector4Single): TVector3Single;
function Vector3ByteCut(const v: TVector4Byte): TVector3Byte;

{ Construct and normalize 3D vector value. }
function Normal3Single(const x, y: Single; const z: Single = 0.0): TVector3Single; overload;

{ Convert between single and double precision matrices.
  @groupBegin }
function Matrix2Double(const M: TMatrix2Single): TMatrix2Double;
function Matrix2Single(const M: TMatrix2Double): TMatrix2Single;
function Matrix3Double(const M: TMatrix3Single): TMatrix3Double;
function Matrix3Single(const M: TMatrix3Double): TMatrix3Single;
function Matrix4Double(const M: TMatrix4Single): TMatrix4Double;
function Matrix4Single(const M: TMatrix4Double): TMatrix4Single;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

function Vector2Integer(const x, y: Integer): TVector2Integer;
begin
  result[0] := x; result[1] := y;
end;

function Vector2LongInt(const x, y: Longint): TVector2LongInt;
begin
  result[0] := x; result[1] := y;
end;

function Vector2SmallInt(const x, y: SmallInt): TVector2SmallInt;
begin
  result[0] := x; result[1] := y;
end;

function Vector2Cardinal(const x, y: Cardinal): TVector2Cardinal;
begin
  result[0] := x; result[1] := y;
end;

function Vector2Single(const x, y: Single): TVector2Single;
begin
  result[0] := x; result[1] := y;
end;

function Vector2Single(const V: TVector2Double): TVector2Single;
begin
  Result[0] := V[0];
  Result[1] := V[1];
end;

function Vector2Double(const x, y: Double): TVector2Double;
begin
  result[0] := x; result[1] := y;
end;

function Vector4Single(const x, y: Single; const z: Single{=0}; const w: Single{=1}): TVector4Single;
begin
  result[0] := x; result[1] := y; result[2] := z; result[3] := w;
end;

function Vector4Single(const v3: TVector3Single; const w: Single{=1}): TVector4Single;
begin
  move(v3, result, SizeOf(TVector3Single));
  result[3] := w;
end;

function Vector4Single(const v2: TVector2Single;
  const z: Single = 0; const w: Single = 1): TVector4Single;
begin
  Move(V2, Result, SizeOf(TVector2Single));
  Result[2] := Z;
  Result[3] := W;
end;

function Vector4Single(const ub: TVector4Byte): TVector4Single;
begin
  result[0] := ub[0]/255;
  result[1] := ub[1]/255;
  result[2] := ub[2]/255;
  result[3] := ub[3]/255;
end;

function Vector4Single(const V3: TVector3Byte; const W: Byte): TVector4Single;
begin
  result[0] := V3[0] / 255;
  result[1] := V3[1] / 255;
  result[2] := V3[2] / 255;
  result[3] := W;
end;

function Vector4Single(const v: TVector4Double): TVector4Single;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := v[3];
end;

function Vector4Double(const x, y, z, w: Double): TVector4Double;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
  result[3] := w;
end;

function Vector4Double(const v: TVector4Single): TVector4Double;
begin
  result[0] := v[0];
  result[1] := v[1];
  result[2] := v[2];
  result[3] := v[3];
end;

function Vector3Single(const x, y: Single; const z: Single{=0.0}): TVector3Single;
begin
  result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3Double(const x, y: Double; const z: Double{=0.0}): TVector3Double;
begin
  result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3Single(const v3: TVector3Double): TVector3Single;
begin
  result[0] := v3[0]; result[1] := v3[1]; result[2] := v3[2];
end;

function Vector3Single(const v3: TVector3Byte): TVector3Single;
begin
  result[0] := v3[0]/255;
  result[1] := v3[1]/255;
  result[2] := v3[2]/255;
end;

function Vector3Single(const v2: TVector2Single; const z: Single): TVector3Single;
begin
  move(v2, result, SizeOf(v2));
  result[2] := z;
end;

function Vector3Double(const v: TVector3Single): TVector3Double;
begin
  result[0] := v[0]; result[1] := v[1]; result[2] := v[2];
end;

function Vector3Double(const v2: TVector2Double; const z: Double): TVector3Double;
begin
  move(v2, result, SizeOf(v2));
  result[2] := z;
end;

function Vector2Byte(x, y: Byte): TVector2Byte;
begin
  result[0] := x; result[1] := y;
end;

function Vector3Byte(x, y, z: Byte): TVector3Byte;
begin
  result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3Byte(const v: TVector3Single): TVector3Byte;
begin
  result[0] := Clamped(Round(v[0] * 255), Low(Byte), High(Byte));
  result[1] := Clamped(Round(v[1] * 255), Low(Byte), High(Byte));
  result[2] := Clamped(Round(v[2] * 255), Low(Byte), High(Byte));
end;

function Vector3Byte(const v: TVector3Double): TVector3Byte;
begin
  result[0] := Clamped(Round(v[0] * 255), Low(Byte), High(Byte));
  result[1] := Clamped(Round(v[1] * 255), Low(Byte), High(Byte));
  result[2] := Clamped(Round(v[2] * 255), Low(Byte), High(Byte));
end;

function Vector3Longint(const p0, p1, p2: Longint): TVector3Longint;
begin
  result[0] := p0;
  result[1] := p1;
  result[2] := p2;
end;

function Vector4Byte(const x, y, z, w: Byte): TVector4Byte;
begin
  result[0] := x; result[1] := y; result[2] := z; result[3] := w;
end;

function Vector4Byte(const f4: TVector4Single): TVector4Byte;
begin
  result[0] := Round(f4[0] * 255);
  result[1] := Round(f4[1] * 255);
  result[2] := Round(f4[2] * 255);
  result[3] := Round(f4[3] * 255);
end;

function Vector4Byte(const f3: TVector3Byte; w: Byte): TVector4Byte;
begin
  result[0] := f3[0];
  result[1] := f3[1];
  result[2] := f3[2];
  result[3] := w;
end;

function Vector4Integer(const X, Y, Z, W: Integer): TVector4Integer;
begin
  Result[0] := X;
  Result[1] := Y;
  Result[2] := Z;
  Result[3] := W;
end;

function Vector3SinglePoint(const v: TVector4Single): TVector3Single;
begin
  result[0] := v[0]/v[3];
  result[1] := v[1]/v[3];
  result[2] := v[2]/v[3];
end;

function Vector2SingleCut(const v: TVector3Single): TVector2Single;
begin
  Move(v, Result, SizeOf(Result));
end;

function Vector3SingleCut(const v: TVector4Single): TVector3Single;
begin
  Move(v, Result, SizeOf(Result));
end;

function Vector3ByteCut(const v: TVector4Byte): TVector3Byte;
begin
  Move(v, Result, SizeOf(Result));
end;

function Normal3Single(const x, y: Single; const z: Single{=0}): TVector3Single;
begin
  result[0] := x; result[1] := y; result[2] := z;
  NormalizeVar3Singlev(@result);
end;

function Matrix2Double(const M: TMatrix2Single): TMatrix2Double;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
end;

function Matrix2Single(const M: TMatrix2Double): TMatrix2Single;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
end;

function Matrix3Double(const M: TMatrix3Single): TMatrix3Double;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];
  Result[0][2] := M[0][2];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
  Result[1][2] := M[1][2];

  Result[2][0] := M[2][0];
  Result[2][1] := M[2][1];
  Result[2][2] := M[2][2];
end;

function Matrix3Single(const M: TMatrix3Double): TMatrix3Single;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];
  Result[0][2] := M[0][2];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
  Result[1][2] := M[1][2];

  Result[2][0] := M[2][0];
  Result[2][1] := M[2][1];
  Result[2][2] := M[2][2];
end;

function Matrix4Double(const M: TMatrix4Single): TMatrix4Double;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];
  Result[0][2] := M[0][2];
  Result[0][3] := M[0][3];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
  Result[1][2] := M[1][2];
  Result[1][3] := M[1][3];

  Result[2][0] := M[2][0];
  Result[2][1] := M[2][1];
  Result[2][2] := M[2][2];
  Result[2][3] := M[2][3];

  Result[3][0] := M[3][0];
  Result[3][1] := M[3][1];
  Result[3][2] := M[3][2];
  Result[3][3] := M[3][3];
end;

function Matrix4Single(const M: TMatrix4Double): TMatrix4Single;
begin
  Result[0][0] := M[0][0];
  Result[0][1] := M[0][1];
  Result[0][2] := M[0][2];
  Result[0][3] := M[0][3];

  Result[1][0] := M[1][0];
  Result[1][1] := M[1][1];
  Result[1][2] := M[1][2];
  Result[1][3] := M[1][3];

  Result[2][0] := M[2][0];
  Result[2][1] := M[2][1];
  Result[2][2] := M[2][2];
  Result[2][3] := M[2][3];

  Result[3][0] := M[3][0];
  Result[3][1] := M[3][1];
  Result[3][2] := M[3][2];
  Result[3][3] := M[3][3];
end;

{$endif read_implementation}
