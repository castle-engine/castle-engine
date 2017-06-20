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
  for vectors and matrices with Extended precision. }

{$ifdef read_interface}

{$ifndef EXTENDED_EQUALS_DOUBLE}
function FloatsEqual(const f1, f2: Extended): boolean; overload;
function FloatsEqual(const f1, f2, EqEpsilon: Extended): boolean; overload;

function Zero(const f1: Extended): boolean; overload;
function Zero(const f1, EqEpsilon: Extended): boolean; overload;
{$endif not EXTENDED_EQUALS_DOUBLE}

{$endif read_interface}

{$ifdef read_implementation}

{$ifndef EXTENDED_EQUALS_DOUBLE}
function FloatsEqual(const f1, f2: Extended): boolean;
begin
  if ExtendedEqualityEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < ExtendedEqualityEpsilon
end;

function FloatsEqual(const f1, f2, EqEpsilon: Extended): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = f2 else
    Result := Abs(f1-f2) < EqEpsilon
end;

function Zero(const f1: Extended): boolean;
begin
  if ExtendedEqualityEpsilon = 0 then
    Result := f1 = 0 else
    Result := Abs(f1) < ExtendedEqualityEpsilon
end;

function Zero(const f1, EqEpsilon: Extended): boolean;
begin
  if EqEpsilon = 0 then
    Result := f1 = 0 else
    Result := Abs(f1) < EqEpsilon
end;
{$endif not EXTENDED_EQUALS_DOUBLE}

{$endif read_implementation}
