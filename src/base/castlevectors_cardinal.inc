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

{ Interface and implementation of CastleVectors for Cardinal vectors. }

{$ifdef read_interface}

function ZeroVector(const v: TVector4Cardinal): boolean; overload;

{$endif read_interface}

{$ifdef read_implementation}

function ZeroVector(const v: TVector4Cardinal): boolean;
begin
  result := IsMemCharFilled(v, SizeOf(v), #0);
end;

{$endif read_implementation}
