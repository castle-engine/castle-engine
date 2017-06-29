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

{ Interface and implementation of CastleVectors for Byte vectors. }

{$ifdef read_interface}

function Lerp(const A: Single; const V1, V2: TVector2Byte): TVector2Byte; overload;
function Lerp(const A: Single; const V1, V2: TVector3Byte): TVector3Byte; overload;
function Lerp(const A: Single; const V1, V2: TVector4Byte): TVector4Byte; overload;

function VectorLen(const v: TVector3Byte): Single; overload;
function VectorLenSqr(const v: TVector3Byte): Integer; overload;

function VectorToNiceStr(const v: array of Byte): string; overload;

function VectorToRawStr(const v: array of Byte): string; overload;

function VectorsPerfectlyEqual(const V1, V2: TVector2Byte  ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector3Byte  ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}
function VectorsPerfectlyEqual(const V1, V2: TVector4Byte  ): boolean; overload; {$ifdef SUPPORTS_INLINE} inline; {$endif}

{ Normalize the first 3 vector components. For zero vectors, does nothing. }
procedure NormalizeVar3Bytev(vv: PVector3Byte);

{$endif read_interface}

{$ifdef read_implementation}

function Lerp(const A: Single; const V1, V2: TVector2Byte): TVector2Byte;
begin
  Result[0] := Clamped(Round(V1[0] + A * (V2[0] - V1[0])), 0, High(Byte));
  Result[1] := Clamped(Round(V1[1] + A * (V2[1] - V1[1])), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector3Byte): TVector3Byte;
begin
  Result[0] := Clamped(Round(V1[0] + A * (V2[0] - V1[0])), 0, High(Byte));
  Result[1] := Clamped(Round(V1[1] + A * (V2[1] - V1[1])), 0, High(Byte));
  Result[2] := Clamped(Round(V1[2] + A * (V2[2] - V1[2])), 0, High(Byte));
end;

function Lerp(const A: Single; const V1, V2: TVector4Byte): TVector4Byte;
begin
  Result[0] := Clamped(Round(V1[0] + A * (V2[0] - V1[0])), 0, High(Byte));
  Result[1] := Clamped(Round(V1[1] + A * (V2[1] - V1[1])), 0, High(Byte));
  Result[2] := Clamped(Round(V1[2] + A * (V2[2] - V1[2])), 0, High(Byte));
  Result[3] := Clamped(Round(V1[3] + A * (V2[3] - V1[3])), 0, High(Byte));
end;

function VectorLen(const v: TVector3Byte): Single;
begin
  result := Sqrt(VectorLenSqr(v))
end;

function VectorLenSqr(const v: TVector3Byte): Integer;
begin
  result := Sqr(Integer(v[0])) + Sqr(Integer(v[1])) + Sqr(Integer(v[2]));
end;

function VectorToNiceStr(const v: array of Byte): string; overload;
var
  i: Integer;
begin
  result := '(';
  for i := 0 to High(v)-1 do result := result +IntToStr(v[i]) +', ';
  if High(v) >= 0 then result := result +IntToStr(v[High(v)]) +')';
end;

function VectorToRawStr(const v: array of Byte): string;
var i: integer;
begin
  result := '';
  for i := 0 to High(v)-1 do result += IntToStr(v[i]) +' ';
  if High(v) >= 0 then result += IntToStr(v[High(v)]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector2Byte): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector3Byte): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]);
end;

function VectorsPerfectlyEqual(const V1, V2: TVector4Byte): boolean;
begin
  Result := (V1[0] = V2[0]) and
            (V1[1] = V2[1]) and
            (V1[2] = V2[2]) and
            (V1[3] = V2[3]);
end;

procedure NormalizeVar3Bytev(vv: PVector3Byte);
var
  Len: integer;
begin
  Len := Round( Sqrt(
    Sqr(Integer(vv^[0])) +
    Sqr(Integer(vv^[1])) +
    Sqr(Integer(vv^[2]))) );
  if Len = 0 then exit;
  vv^[0] := vv^[0] div Len;
  vv^[1] := vv^[1] div Len;
  vv^[2] := vv^[2] div Len;
end;

{$endif read_implementation}
