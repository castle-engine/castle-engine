{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Define (new) vectors API using advanced records. }

{$ifdef read_interface}

type
  { Vector of 2 floating-point values (Single precision). }
  TVector2 = CastleVectorsInternalSingle.TGenericVector2;
  { Vector of 2 floating-point values (Double precision). }
  TVector2d = CastleVectorsInternalDouble.TGenericVector2;

  { Vector of 3 floating-point values (Single precision). }
  TVector3 = CastleVectorsInternalSingle.TGenericVector3;
  { Vector of 3 floating-point values (Double precision). }
  TVector3d = CastleVectorsInternalDouble.TGenericVector3;

  { Vector of 4 floating-point values (Single precision). }
  TVector4 = CastleVectorsInternalSingle.TGenericVector4;
  { Vector of 4 floating-point values (Double precision). }
  TVector4d = CastleVectorsInternalDouble.TGenericVector4;

  { 3x3 matrix of floating-point values (Single precision). }
  TMatrix3 = CastleVectorsInternalSingle.TGenericMatrix3;
  { 3x3 matrix of floating-point values (Double precision). }
  TMatrix3d = CastleVectorsInternalDouble.TGenericMatrix3;

  { 4x4 matrix of floating-point values (Single precision). }
  TMatrix4 = CastleVectorsInternalSingle.TGenericMatrix4;
  { 4x4 matrix of floating-point values (Double precision). }
  TMatrix4d = CastleVectorsInternalDouble.TGenericMatrix4;

{ Construct an initialized TVector2 value. }
function Vector2(const X, Y: Single): TVector2;
{ Construct an initialized TVector2d value. }
function Vector2d(const X, Y: Double): TVector2d;

{ Construct an initialized TVector3 value. }
function Vector3(const X, Y, Z: Single): TVector3;
{ Construct an initialized TVector3d value. }
function Vector3d(const X, Y, Z: Double): TVector3d;

{ Construct an initialized TVector4 value. }
function Vector4(const X, Y, Z, W: Single): TVector4;
{ Construct an initialized TVector4d value. }
function Vector4d(const X, Y, Z, W: Double): TVector4d;

{$ifdef FPC}
{ Convert between record-based types (like TVector3) and
  array-based types (like TVector3Single). }

operator := (const V: TVector2Single): TVector2;
operator := (const V: TVector2): TVector2Single;

operator := (const V: TVector2Double): TVector2d;
operator := (const V: TVector2d): TVector2Double;

operator := (const V: TVector3Single): TVector3;
operator := (const V: TVector3): TVector3Single;

operator := (const V: TVector3Double): TVector3d;
operator := (const V: TVector3d): TVector3Double;

operator := (const V: TVector4Single): TVector4;
operator := (const V: TVector4): TVector4Single;

operator := (const V: TVector4Double): TVector4d;
operator := (const V: TVector4d): TVector4Double;
{$endif}

{$endif read_interface}

{$ifdef read_implementation}

{ Construct 2D vectors ------------------------------------------------------------ }

function Vector2(const X, Y: Single): TVector2;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
end;

function Vector2d(const X, Y: Double): TVector2d;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
end;

{ Convert 2D vectors ------------------------------------------------------------ }

operator := (const V: TVector2Single): TVector2;
begin
  Result.Data[0] := V[0];
  Result.Data[1] := V[1];
end;

operator := (const V: TVector2): TVector2Single;
begin
  Result[0] := V.Data[0];
  Result[1] := V.Data[1];
end;

operator := (const V: TVector2Double): TVector2d;
begin
  Result.Data[0] := V[0];
  Result.Data[1] := V[1];
end;

operator := (const V: TVector2d): TVector2Double;
begin
  Result[0] := V.Data[0];
  Result[1] := V.Data[1];
end;

{ Construct 3D vectors ------------------------------------------------------------ }

function Vector3(const X, Y, Z: Single): TVector3;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
end;

function Vector3d(const X, Y, Z: Double): TVector3d;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
end;

{ Convert 3D vectors ------------------------------------------------------------ }

operator := (const V: TVector3Single): TVector3;
begin
  Result.Data[0] := V[0];
  Result.Data[1] := V[1];
  Result.Data[2] := V[2];
end;

operator := (const V: TVector3): TVector3Single;
begin
  Result[0] := V.Data[0];
  Result[1] := V.Data[1];
  Result[2] := V.Data[2];
end;

operator := (const V: TVector3Double): TVector3d;
begin
  Result.Data[0] := V[0];
  Result.Data[1] := V[1];
  Result.Data[2] := V[2];
end;

operator := (const V: TVector3d): TVector3Double;
begin
  Result[0] := V.Data[0];
  Result[1] := V.Data[1];
  Result[2] := V.Data[2];
end;

{ Construct 4D vectors ------------------------------------------------------------ }

function Vector4(const X, Y, Z, W: Single): TVector4;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
  Result.Data[3] := W;
end;

function Vector4d(const X, Y, Z, W: Double): TVector4d;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
  Result.Data[3] := W;
end;

{ Convert 4D vectors ------------------------------------------------------------ }

operator := (const V: TVector4Single): TVector4;
begin
  Result.Data[0] := V[0];
  Result.Data[1] := V[1];
  Result.Data[2] := V[2];
  Result.Data[3] := V[3];
end;

operator := (const V: TVector4): TVector4Single;
begin
  Result[0] := V.Data[0];
  Result[1] := V.Data[1];
  Result[2] := V.Data[2];
  Result[3] := V.Data[3];
end;

operator := (const V: TVector4Double): TVector4d;
begin
  Result.Data[0] := V[0];
  Result.Data[1] := V[1];
  Result.Data[2] := V[2];
  Result.Data[3] := V[3];
end;

operator := (const V: TVector4d): TVector4Double;
begin
  Result[0] := V.Data[0];
  Result[1] := V.Data[1];
  Result[2] := V.Data[2];
  Result[3] := V.Data[3];
end;

{$endif read_implementation}
