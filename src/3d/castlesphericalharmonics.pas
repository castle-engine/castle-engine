{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spherical harmonic basis functions. }
unit CastleSphericalHarmonics;

{$I castleconf.inc}

interface

uses CastleVectors, CastleUtils, Math, CastleCubeMaps;

const
  { How many basis can SHBasis calculate. LM for SHBasis must be within
    0 .. SHBasesCount - 1. }
  MaxSHBasis = 25;

  { The first SH basis function is actually constant.
    This is sometimes useful. }
  SHBasis0 = 1 / (2 * Sqrt(Pi));

{ Calculate spherical harmonic basis function for given arguments.

  LM indicates (L, M) that specify which SH basis to use.
  LM determines a pair (L, M) in the natural order: for each L,
  take for each M from -L to L.
  That is, LM = (0, 1, 2, 3, ...) indicate
  @preformatted(
      (L, M) =
    ( (0, 0),
      (1, -1), (1, 0), (1, 1),
      (2, -2), (2, -1), (2, 0), (2, 1), (2, 2),
      ... )
  ) ) }
function SHBasis(const LM: Cardinal; const PhiTheta: TVector2Single): Float;

procedure LMDecode(const LM: Cardinal; out L: Cardinal; out M: Integer);

var
  (*For each SHBasis function (first index of the array is LM of this function),
    a precalculated results of basic spherical harmonic functions.
    Multiplied by solid angle of this cube map pixel, since this is what
    you usually need.

    For each side of the cube, and for each pixel on this side (pixels are
    arranged same as in TGrayscaleImage, that is row-by-row from
    lower to higher, from left to right) this gives the result of SHBasis
    for this direction. Multiplied by solid angle.

    You have to initialize this (once, like at the beginning of your
    program) by InitializeSHBasisMap.

    This is useful for calculating sh basis vector from given cube map:
    since you can just project any function on any basis, so if you have
    a particular cube map you can project it on each SH basis function.
    See SHVectorFromCubeMap implementation for code how to use SHBasisMap
    for this, and in simple cases you can just call SHVectorFromCubeMap. *)
  SHBasisMap: array [0..MaxSHBasis - 1] of TCubeMapFloat;

procedure InitializeSHBasisMap;

type
  TSHVectorSingle = array [0..MaxSHBasis - 1] of Single;
  PSHVectorSingle = ^TSHVectorSingle;

{ Calculate SH basis coefficients that approximate function in Map.
  This uses SHBasisMap, so be sure to initialize it first. }
procedure SHVectorFromCubeMap(var SHVector: array of Single;
  const Map: TCubeMapByte);

implementation

uses CastleSphereSampling;

function SHBasis(const LM: Cardinal; const PhiTheta: TVector2Single): Float;

{ Taken from http://www.sjbrown.co.uk/2004/10/16/spherical-harmonic-basis/ }

var
  SinPhi, CosPhi, SinTheta, CosTheta: Float;

  function Sin2Theta: Float;
  begin
    Result := Sqr(SinTheta);
  end;

  function Sin3Theta: Float;
  begin
    Result := Sqr(SinTheta)*SinTheta;
  end;

  function Sin4Theta: Float;
  begin
    Result := Sqr(Sqr(SinTheta));
  end;

  function Cos2Theta: Float;
  begin
    Result := Sqr(CosTheta);
  end;

  function Cos3Theta: Float;
  begin
    Result := Sqr(CosTheta)*CosTheta;
  end;

  function Cos4Theta: Float;
  begin
    Result := Sqr(Sqr(CosTheta));
  end;

  function Sin2Phi: Float;
  begin
    Result := Sqr(SinPhi);
  end;

  function Sin3Phi: Float;
  begin
    Result := Sqr(SinPhi)*SinPhi;
  end;

  function Sin4Phi: Float;
  begin
    Result := Sqr(Sqr(SinPhi));
  end;

  function Cos2Phi: Float;
  begin
    Result := Sqr(CosPhi);
  end;

  function Cos3Phi: Float;
  begin
    Result := Sqr(CosPhi)*CosPhi;
  end;

  function Cos4Phi: Float;
  begin
    Result := Sqr(Sqr(CosPhi));
  end;

begin
  SinCos(PhiTheta[0], SinPhi, CosPhi);
  SinCos(PhiTheta[1], SinTheta, CosTheta);

  { Fear not, this case should be converted to lookup table by FPC. }

  case LM of
    0: Result := SHBasis0;

    1: Result := Sqrt(3) / (2 * Sqrt(Pi)) * CosPhi * SinTheta;
    2: Result := Sqrt(3) / (2 * Sqrt(Pi)) * CosTheta;
    3: Result := Sqrt(3) / (2 * Sqrt(Pi)) * SinPhi * SinTheta;

    4: Result := Sqrt(15) / (2 * Sqrt(Pi)) * CosPhi * SinPhi * Sin2Theta;
    5: Result := Sqrt(15) / (2 * Sqrt(Pi)) * SinPhi * CosTheta * SinTheta;
    6: Result := Sqrt(15) / (4 * Sqrt(Pi)) * (3 * Cos2Theta - 1);
    7: Result := Sqrt(15) / (2 * Sqrt(Pi)) * CosPhi * CosTheta * SinTheta;
    8: Result := Sqrt(15) / (4 * Sqrt(Pi)) * (Cos2Phi - Sin2Phi) * Sin2Theta;

    9 : Result := Sqrt(35)  / (4 * Sqrt(2*Pi) ) * (3 * Cos2Phi - Sin2Phi) * SinPhi * Sin3Theta;
    10: Result := Sqrt(105) / (2 * Sqrt(Pi  ) ) * CosPhi * SinPhi * CosTheta * Sin2Theta;
    11: Result := Sqrt(21)  / (4 * Sqrt(2*Pi) ) * (5 * Cos2Theta - 1) * SinPhi * SinTheta;
    12: Result := Sqrt(7)   / (4 * Sqrt(Pi  ) ) * (5 * Cos3Theta - 3 * CosTheta);
    13: Result := Sqrt(21)  / (4 * Sqrt(2*Pi) ) * (5 * Cos2Theta - 1) * CosPhi * SinTheta;
    14: Result := Sqrt(105) / (4 * Sqrt(Pi  ) ) * (Cos2Phi -     Sin2Phi) * CosTheta * Sin2Theta;
    15: Result := Sqrt(35)  / (4 * Sqrt(2*Pi) ) * (Cos2Phi - 3 * Sin2Phi) * CosPhi * Sin3Theta;

    16: Result := 3 * Sqrt(35) / (4  * Sqrt(Pi  ) ) * (    Cos2Phi - Sin2Phi) * CosPhi * SinPhi * Sin4Theta;
    17: Result := 3 * Sqrt(35) / (4  * Sqrt(2*Pi) ) * (3 * Cos2Phi - Sin2Phi) * SinPhi * CosTheta * Sin3Theta;
    18: Result := 3 * Sqrt(5)  / (4  * Sqrt(Pi  ) ) * (7 * Cos2Theta - 1) * CosPhi * SinPhi * Sin2Theta;
    19: Result := 3 * Sqrt(5)  / (4  * Sqrt(2*Pi) ) * (7 * Cos2Theta - 3) * SinPhi * CosTheta * SinTheta;
    20: Result := 3            / (16 * Sqrt(Pi  ) ) * (3 - 30 * Cos2Theta + 35 * Cos4Theta);
    21: Result := 3 * Sqrt(5)  / (4  * Sqrt(2*Pi) ) * (7 * Cos2Theta - 3) * CosPhi * CosTheta * SinTheta;
    22: Result := 3 * Sqrt(5)  / (8  * Sqrt(Pi  ) ) * (7 * Cos2Theta - 1) * (Cos2Phi - Sin2Phi) * Sin2Theta;
    23: Result := 3 * Sqrt(35) / (4  * Sqrt(2*Pi) ) * (Cos2Phi - 3 * Sin2Phi) * CosPhi * CosTheta * Sin3Theta;
    24: Result := 3 * Sqrt(35) / (16 * Sqrt(Pi  ) ) * (Sin4Phi - 6 * Cos2Phi * Sin2Phi + Cos4Phi) * Sin4Theta;

    else raise EInternalError.Create('SHBasis LM out');
  end
end;

procedure LMDecode(const LM: Cardinal; out L: Cardinal; out M: Integer);
var
  ReachedLM: Integer;
begin
  L := 0;
  ReachedLM := -1;
  repeat
    if ReachedLM + Integer(2*L+1) >= Integer(LM) then
    begin
      M := LM - ReachedLM - L - 1;
      Break;
    end;
    ReachedLM += Integer(2*L+1);
    Inc(L);
  until false;
end;

var
  SHBasisMapInitialized: boolean = false;

procedure InitializeSHBasisMap;
var
  LM: Cardinal;
  Side: TCubeMapSide;
  Pixel: Cardinal;
begin
  if SHBasisMapInitialized then Exit;

  for LM := 0 to MaxSHBasis - 1 do
    for Side := Low(Side) to High(Side) do
      for Pixel := 0 to Sqr(CubeMapSize) - 1 do
      begin
        SHBasisMap[LM][Side][Pixel] :=
          SHBasis(LM, XYZToPhiTheta(CubeMapDirection(Side, Pixel))) *
          CubeMapSolidAngle(Side, Pixel);
      end;

  SHBasisMapInitialized := true;
end;

procedure SHVectorFromCubeMap(var SHVector: array of Single;
  const Map: TCubeMapByte);
var
  LM: Cardinal;
  Side: TCubeMapSide;
  Pixel: Cardinal;
begin
  Assert(SHBasisMapInitialized);

  for LM := 0 to High(SHVector) do
  begin
    SHVector[LM] := 0;
    for Side := Low(Side) to High(Side) do
      for Pixel := 0 to Sqr(CubeMapSize) - 1 do
        SHVector[LM] += (Map[Side, Pixel]/255) * SHBasisMap[LM, Side, Pixel];

    { SHVector[LM] is now calculated for all sphere points.
      We want this to be integral over a sphere, so normalize now.

      Since SHBasisMap contains result of SH function * solid angle of pixel
      (on cube map, pixels have different solid angles),
      so below we divide by 4*Pi (sphere area, sum of solid angles for every
      pixel). }

    SHVector[LM] /= 4 * Pi;
  end;
end;

end.
