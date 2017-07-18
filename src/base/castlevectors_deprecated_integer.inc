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

{ Interface and implementation of CastleVectors for Integer vectors. }

{$ifdef read_interface}

function Lerp(const A: Single; const V1, V2: TVector2Integer): TVector2Single; overload;

function ZeroVector(const v: TVector4Integer): boolean; overload;

function VectorToNiceStr(const v: array of Integer): string; overload;

function VectorToRawStr(const v: array of Integer): string; overload;

{$endif read_interface}

{$ifdef read_implementation}

function Lerp(const A: Single; const V1, V2: TVector2Integer): TVector2Single;
begin
  result[0] := V1[0] + a*(V2[0]-V1[0]);
  result[1] := V1[1] + a*(V2[1]-V1[1]);
end;

function ZeroVector(const v: TVector4Integer): boolean;
begin
  result := IsMemCharFilled(v, SizeOf(v), #0);
end;

function VectorToNiceStr(const v: array of Integer): string; overload;
var
  i: Integer;
begin
  result := '(';
  for i := 0 to High(v)-1 do result := result +IntToStr(v[i]) +', ';
  if High(v) >= 0 then result := result +IntToStr(v[High(v)]) +')';
end;

function VectorToRawStr(const v: array of Integer): string;
var i: integer;
begin
  result := '';
  for i := 0 to High(v)-1 do result += IntToStr(v[i]) +' ';
  if High(v) >= 0 then result += IntToStr(v[High(v)]);
end;

{$endif read_implementation}
