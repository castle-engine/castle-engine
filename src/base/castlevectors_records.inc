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
  { Vector of 3 floating-point values (Single precision). }
  TVector3 = CastleVectorsInternalSingle.TGenericVector3;
  { Vector of 3 floating-point values (Double precision). }
  TVector3d = CastleVectorsInternalDouble.TGenericVector3;

{ Construct an initialized TVector3 value. }
function Vector3(const X: Single = 0; const Y: Single = 0; const Z: Single = 0): TVector3;

{ Construct an initialized TVector3d value. }
function Vector3d(const X: Double = 0; const Y: Double = 0; const Z: Double = 0): TVector3d;

{$ifdef FPC}
{ Convert between record-based types (like TVector3) and
  array-based types (like TVector3Single). }

operator := (const V: TVector3Single): TVector3;
operator := (const V: TVector3): TVector3Single;

operator := (const V: TVector3Double): TVector3d;
operator := (const V: TVector3d): TVector3Double;
{$endif}

{$endif read_interface}

{$ifdef read_implementation}

{ global routines ------------------------------------------------------------ }

function Vector3(const X: Single = 0; const Y: Single = 0; const Z: Single = 0): TVector3;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
end;

function Vector3d(const X: Double = 0; const Y: Double = 0; const Z: Double = 0): TVector3d;
begin
  Result.Data[0] := X;
  Result.Data[1] := Y;
  Result.Data[2] := Z;
end;

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

{$endif read_implementation}
