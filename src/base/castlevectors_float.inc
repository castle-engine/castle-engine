{%MainUnit castlevectors.pas}
{
  Copyright 2017-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Define Single- and Double-precision vector and matrix types
  (and their constructors) using advanced records. }

{$ifdef read_interface}

type
  { Vector of 2 floating-point values (Single precision). }
  TVector2 = CastleVectorsInternalSingle.TGenericVector2;  PVector2 = ^TVector2;
  { Vector of 2 floating-point values (Double precision). }
  TVector2Double = CastleVectorsInternalDouble.TGenericVector2; PVector2Double = ^TVector2Double;

  { Vector of 3 floating-point values (Single precision). }
  TVector3 = CastleVectorsInternalSingle.TGenericVector3;  PVector3 = ^TVector3;
  { Vector of 3 floating-point values (Double precision). }
  TVector3Double = CastleVectorsInternalDouble.TGenericVector3; PVector3Double = ^TVector3Double;

  { Vector of 4 floating-point values (Single precision). }
  TVector4 = CastleVectorsInternalSingle.TGenericVector4;  PVector4 = ^TVector4;
  { Vector of 4 floating-point values (Double precision). }
  TVector4Double = CastleVectorsInternalDouble.TGenericVector4; PVector4Double = ^TVector4Double;

  { 2x2 matrix of floating-point values (Single precision). }
  TMatrix2 = CastleVectorsInternalSingle.TGenericMatrix2;  PMatrix2 = ^TMatrix2;
  { 2x2 matrix of floating-point values (Double precision). }
  TMatrix2Double = CastleVectorsInternalDouble.TGenericMatrix2; PMatrix2Double = ^TMatrix2Double;

  { 3x3 matrix of floating-point values (Single precision). }
  TMatrix3 = CastleVectorsInternalSingle.TGenericMatrix3;  PMatrix3 = ^TMatrix3;
  { 3x3 matrix of floating-point values (Double precision). }
  TMatrix3Double = CastleVectorsInternalDouble.TGenericMatrix3; PMatrix3Double = ^TMatrix3Double;

  { 4x4 matrix of floating-point values (Single precision). }
  TMatrix4 = CastleVectorsInternalSingle.TGenericMatrix4;  PMatrix4 = ^TMatrix4;
  { 4x4 matrix of floating-point values (Double precision). }
  TMatrix4Double = CastleVectorsInternalDouble.TGenericMatrix4; PMatrix4Double = ^TMatrix4Double;

  TVector2Array = packed array [0..MaxInt div SizeOf(TVector2) - 1] of TVector2;
  PVector2Array = ^TVector2Array;
  TVector3Array = packed array [0..MaxInt div SizeOf(TVector3) - 1] of TVector3;
  PVector3Array = ^TVector3Array;
  TVector4Array = packed array [0..MaxInt div SizeOf(TVector4) - 1] of TVector4;
  PVector4Array = ^TVector4Array;

{ Construct an initialized TVector2 value. }
function Vector2(const X, Y: Single): TVector2; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector2(const V: TVector2Double): TVector2; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

{ Construct an initialized TVector2Double value. }
function Vector2Double(const X, Y: Double): TVector2Double; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

{ Construct an initialized TVector3 value. }
function Vector3(const X, Y, Z: Single): TVector3; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector3(const V: TVector2; const Z: Single): TVector3; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector3(const V: TVector3Double): TVector3; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

{ Construct an initialized TVector3Double value. }
function Vector3Double(const X, Y, Z: Double): TVector3Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector3Double(const V: TVector3): TVector3Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

{ Construct an initialized TVector4 value. }
function Vector4(const X, Y, Z, W: Single): TVector4; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector4(const V: TVector3; const W: Single): TVector4; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector4(const V: TVector2; const Z, W: Single): TVector4; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector4(const V: TVector4Double): TVector4; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

{ Construct an initialized TVector4Double value. }
function Vector4Double(const X, Y, Z, W: Double): TVector4Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector4Double(const V: TVector4): TVector4Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Vector4Double(const V: TVector3Double; const W: Double): TVector4Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

{ Convert between Double-precision and Single-precision matrix. }
function Matrix3(const V: TMatrix3Double): TMatrix3;
function Matrix4(const V: TMatrix4Double): TMatrix4;
function Matrix3Double(const V: TMatrix3): TMatrix3Double;
function Matrix4Double(const V: TMatrix4): TMatrix4Double;

{ Convert string to vector. Each component is simply parsed by StrToFloatDot,
  and components must be separated by whitespace (see @link(WhiteSpaces) constant).
  This is a reverse of
  @link(CastleVectorsInternalSingle.TGenericVector2.ToString TVector2.ToString),
  @link(CastleVectorsInternalSingle.TGenericVector3.ToString TVector3.ToString),
  @link(CastleVectorsInternalSingle.TGenericVector4.ToString TVector4.ToString) methods.

  @raises(EConvertError In case of problems during conversion (invalid float
    or unexpected string end or expected but missed string end).)

  @groupBegin }
function Vector2FromStr(const S: string): TVector2;
function Vector3FromStr(const S: string): TVector3;
function Vector4FromStr(const S: string): TVector4;
{ @groupEnd }

{ Linear interpolation between two vector values.
  Returns @code((1-A) * V1 + A * V2).

  So:

  @unorderedList(
    @itemSpacing Compact
    @item A = 0 gives V1,
    @item A = 1 gives V2,
    @item values between are interpolated,
    @item values outside are extrapolated.
  )

  @groupBegin }
function Lerp(const A: Single; const V1, V2: TVector2): TVector2; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Single; const V1, V2: TVector3): TVector3; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Single; const V1, V2: TVector4): TVector4; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Single; const M1, M2: TMatrix2): TMatrix2; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Single; const M1, M2: TMatrix3): TMatrix3; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Single; const M1, M2: TMatrix4): TMatrix4; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}

function Lerp(const A: Double; const V1, V2: TVector2Double): TVector2Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Double; const V1, V2: TVector3Double): TVector3Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Double; const V1, V2: TVector4Double): TVector4Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Double; const M1, M2: TMatrix2Double): TMatrix2Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Double; const M1, M2: TMatrix3Double): TMatrix3Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
function Lerp(const A: Double; const M1, M2: TMatrix4Double): TMatrix4Double; overload; {$ifdef INLINE_GENERIC_VECTORS}inline;{$endif}
{ @groupEnd }

{ Try to inverse single-precision matrix using double-precision, if necessary. }
function TryInverseHarder(const M: TMatrix4; out MInverse: TMatrix4): boolean;

{$endif read_interface}

{$ifdef read_implementation}

{ Construct 2D vectors ------------------------------------------------------------ }

function Vector2(const X, Y: Single): TVector2;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector2(const V: TVector2Double): TVector2;
begin
  Result.X := V.X;
  Result.Y := V.Y;
end;

function Vector2Double(const X, Y: Double): TVector2Double;
begin
  Result.X := X;
  Result.Y := Y;
end;

{ Construct 3D vectors ------------------------------------------------------------ }

function Vector3(const X, Y, Z: Single): TVector3;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3(const V: TVector2; const Z: Single): TVector3;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := Z;
end;

function Vector3(const V: TVector3Double): TVector3;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
end;

function Vector3Double(const X, Y, Z: Double): TVector3Double;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3Double(const V: TVector3): TVector3Double;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
end;

{ Construct 4D vectors ------------------------------------------------------------ }

function Vector4(const X, Y, Z, W: Single): TVector4;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4(const V: TVector3; const W: Single): TVector4;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
  Result.W := W;
end;

function Vector4(const V: TVector2; const Z, W: Single): TVector4;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4(const V: TVector4Double): TVector4;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
  Result.W := V.W;
end;

function Vector4Double(const X, Y, Z, W: Double): TVector4Double;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4Double(const V: TVector4): TVector4Double;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
  Result.W := V.W;
end;

function Vector4Double(const V: TVector3Double; const W: Double): TVector4Double;
begin
  Result.X := V.X;
  Result.Y := V.Y;
  Result.Z := V.Z;
  Result.W := W;
end;

{ Convert matrix ------------------------------------------------------------- }

function Matrix3(const V: TMatrix3Double): TMatrix3;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result.Data[I, J] := V.Data[I, J];
end;

function Matrix4(const V: TMatrix4Double): TMatrix4;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.Data[I, J] := V.Data[I, J];
end;

function Matrix3Double(const V: TMatrix3): TMatrix3Double;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result.Data[I, J] := V.Data[I, J];
end;

function Matrix4Double(const V: TMatrix4): TMatrix4Double;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.Data[I, J] := V.Data[I, J];
end;

{ Convert from string -------------------------------------------------------- }

function Vector2FromStr(const S: string): TVector2;
var
  SPosition: Integer;
begin
  SPosition := 1;
  Result.X := StrToFloatDot(NextToken(S, SPosition));
  Result.Y := StrToFloatDot(NextToken(S, SPosition));
  if NextToken(S, SPosition) <> '' then
    raise EConvertError.Create('Expected end of data when reading vector from string');
end;

function Vector3FromStr(const S: string): TVector3;
var
  SPosition: Integer;
begin
  SPosition := 1;
  Result.X := StrToFloatDot(NextToken(S, SPosition));
  Result.Y := StrToFloatDot(NextToken(S, SPosition));
  Result.Z := StrToFloatDot(NextToken(S, SPosition));
  if NextToken(S, SPosition) <> '' then
    raise EConvertError.Create('Expected end of data when reading vector from string');

  { This could also be implemented by DeFormat, but this is faster
    (which is important for e.g. reading 3D data where this function may be used
    often) }
end;

function Vector4FromStr(const S: string): TVector4;
var
  SPosition: Integer;
begin
  SPosition := 1;
  Result.X := StrToFloatDot(NextToken(S, SPosition));
  Result.Y := StrToFloatDot(NextToken(S, SPosition));
  Result.Z := StrToFloatDot(NextToken(S, SPosition));
  Result.W := StrToFloatDot(NextToken(S, SPosition));
  if NextToken(S, SPosition) <> '' then
    raise EConvertError.Create('Expected end of data when reading vector from string');
end;

{ Lerp ----------------------------------------------------------------------- }

function Lerp(const A: Single; const V1, V2: TVector2): TVector2;
begin
  Result := TVector2.Lerp(A, V1, V2);
end;

function Lerp(const A: Single; const V1, V2: TVector3): TVector3;
begin
  Result := TVector3.Lerp(A, V1, V2);
end;

function Lerp(const A: Single; const V1, V2: TVector4): TVector4;
begin
  Result := TVector4.Lerp(A, V1, V2);
end;

function Lerp(const A: Single; const M1, M2: TMatrix2): TMatrix2;
begin
  Result := TMatrix2.Lerp(A, M1, M2);
end;

function Lerp(const A: Single; const M1, M2: TMatrix3): TMatrix3;
begin
  Result := TMatrix3.Lerp(A, M1, M2);
end;

function Lerp(const A: Single; const M1, M2: TMatrix4): TMatrix4;
begin
  Result := TMatrix4.Lerp(A, M1, M2);
end;

function Lerp(const A: Double; const V1, V2: TVector2Double): TVector2Double;
begin
  Result := TVector2Double.Lerp(A, V1, V2);
end;

function Lerp(const A: Double; const V1, V2: TVector3Double): TVector3Double;
begin
  Result := TVector3Double.Lerp(A, V1, V2);
end;

function Lerp(const A: Double; const V1, V2: TVector4Double): TVector4Double;
begin
  Result := TVector4Double.Lerp(A, V1, V2);
end;

function Lerp(const A: Double; const M1, M2: TMatrix2Double): TMatrix2Double;
begin
  Result := TMatrix2Double.Lerp(A, M1, M2);
end;

function Lerp(const A: Double; const M1, M2: TMatrix3Double): TMatrix3Double;
begin
  Result := TMatrix3Double.Lerp(A, M1, M2);
end;

function Lerp(const A: Double; const M1, M2: TMatrix4Double): TMatrix4Double;
begin
  Result := TMatrix4Double.Lerp(A, M1, M2);
end;

{ TryInverseHarder ----------------------------------------------------------- }

function TryInverseHarder(const M: TMatrix4; out MInverse: TMatrix4): boolean;
var
  MInverseDouble: TMatrix4Double;
begin
  Result := M.TryInverse(MInverse);
  if not Result then
  begin
    Result := Matrix4Double(M).TryInverse(MInverseDouble);
    if Result then
    begin
      // WritelnLogMultiline('TryInverseHarder', 'Failed to inverse matrix in Single-precision, but managed to inverse in Double-precision.' + NL +
      //   M.ToRawString);
      MInverse := Matrix4(MInverseDouble);
    end;
  end;
end;

{$endif read_implementation}
