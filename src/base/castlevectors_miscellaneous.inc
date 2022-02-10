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

{ Miscellaneous stuff for CastleVectors. }

{$ifdef read_interface}

type
  { Vector of 4 Pointers. }
  TVector4Pointer = packed record
    X, Y, Z, W: Pointer;
  end;

  EPlanesParallel = class(Exception);
  ELinesParallel = class(Exception);

const
  { Epsilon used by default when compating Single (Single-precision float values).
    Compatible with Math unit value, used by standard routines like Math.SameValue
    and Math.IsZero. }
  SingleEpsilon = 1E-4;

  { Epsilon used by default when compating Double (Double-precision float values).
    Compatible with Math unit value, used by standard routines like Math.SameValue
    and Math.IsZero. }
  DoubleEpsilon = 1E-12;

  SingleEqualityEpsilon = SingleEpsilon deprecated 'use SingleEpsilon';
  DoubleEqualityEpsilon = DoubleEpsilon deprecated 'use DoubleEpsilon';
  ExtendedEqualityEpsilon = 1E-16 deprecated 'use only Math functions, like SameValue and IsZero, to operate on Extended type; CastleVectors unit does not deal with Extended type anymore';

{$endif read_interface}

{$ifdef read_implementation}

{ Separated from PointsDistance2DSqr, to not slow it down by implicit
  try/finally section because we use string. }
procedure PointsDistance2DSqr_InvalidIgnoreIndex;
begin
  raise EInternalError.Create('Invalid IgnoreIndex for PointsDistance2DSqr');
end;

{$endif read_implementation}
