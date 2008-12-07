{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Utilities for cube environment maps. }
unit CubeEnvMap;

interface

uses VectorMath, Math;

type
  TEnvMapSide = (emsRight, emsBack, emsLeft, emsFront, emsTop, emsBottom);
  TEnvMapInfo = record
    Dir, Up, Side: TVector3Single;
    ScreenX, ScreenY: Integer;
  end;

const
  EnvMapSize = 16;

  EnvMapInfo: array [TEnvMapSide] of TEnvMapInfo =
  ( (Dir: ( 1,  0, 0); Up: (0, 0, 1); Side: ( 0, -1, 0); ScreenX: 0             ; ScreenY: 0),
    (Dir: ( 0, -1, 0); Up: (0, 0, 1); Side: (-1,  0, 0); ScreenX:     EnvMapSize; ScreenY: 0),
    (Dir: (-1,  0, 0); Up: (0, 0, 1); Side: ( 0,  1, 0); ScreenX: 2 * EnvMapSize; ScreenY: 0),
    (Dir: ( 0,  1, 0); Up: (0, 0, 1); Side: ( 1,  0, 0); ScreenX: 3 * EnvMapSize; ScreenY: 0),
    (Dir: (0, 0,  1); Up: (-1, 0, 0); Side: (0, -1, 0); ScreenX: 0; ScreenY: + EnvMapSize),
    (Dir: (0, 0, -1); Up: ( 1, 0, 0); Side: (0, -1, 0); ScreenX: 0; ScreenY: - EnvMapSize)
  );

{ Direction corresponding to given environment map side and pixel number.
  That is, assuming that the middle of cube is placed at (0, 0, 0),
  each pixel of the cube map corresponds to some direction.

  Pixel number must be between 0 and EnvMapSize*2-1.
  It's assumed that pixels
  are in the same order as they are in OpenGL images, that is row-by-row,
  from the lowest row to highest, in each row from left to right.

  Returned vector is @italic(not normalized). }
function EnvMapDirection(const Side: TEnvMapSide;
  const Pixel: Cardinal): TVector3Single;

{ Return environment map side and pixel that is the closest to
  given direction Dir. This is the reverse of EnvMapDirection function.

  Given here Dir need not be normalized, although must not be zero. }
procedure DirectionToEnvMap(const Dir: TVector3Single;
  out Side: TEnvMapSide; out Pixel: Cardinal);

type
  TEnvMapSide4 = array [0..3] of TEnvMapSide;
  TCardinal4 = array [0..3] of Cardinal;

{ Return 4 environment map indexes (side and pixel, along with ratio)
  that are closest to given direction Dir.
  All ratios will sum to 1.

  This is like DirectionToEnvMap, except this returns 4 values.
  It allows you to do bilinear interpolation between cube map items. }
procedure Direction4ToEnvMap(const Dir: TVector3Single;
  out Side: TEnvMapSide4;
  out Pixel: TVector4Cardinal;
  out Ratio: TVector4Single);

type
  { Cube environment map, with each item being a Float. }
  TEnvMapFloat = array [TEnvMapSide, 0..Sqr(EnvMapSize) - 1] of Float;
  PEnvMapFloat = ^TEnvMapFloat;

  { Cube environment map, with each item being in 0..1 range, encoded as a Byte.
    This assumes that every item is actually a float in 0..1 range,
    encoded as Byte. }
  TEnvMapByte = array [TEnvMapSide, 0..Sqr(EnvMapSize) - 1] of Byte;
  PEnvMapByte = ^TEnvMapByte;

{ Calculate solid angle of given pixel on the cube map. }
function EnvMapSolidAngle(const Side: TEnvMapSide;
  const Pixel: Cardinal): Float;

implementation

uses SysUtils, KambiUtils;

{ Note: EnvMapSolidAngle assumes that implementation of this actually
  returns the position of the middle of the pixel. That is, it assumes
  you don't normalize the returned direction. }
function EnvMapDirection(const Side: TEnvMapSide;
  const Pixel: Cardinal): TVector3Single;
var
  PixelX, PixelY: Cardinal;
begin
  PixelX := Pixel mod EnvMapSize;
  PixelY := Pixel div EnvMapSize;
  { Result = exactly EnvMapInfo[Side].Dir when
    PixelX/Y = EnvMapSize/2 (pixel is on the middle of the image). }
  Result := EnvMapInfo[Side].Dir;
  VectorAddTo1st(Result,
    VectorScale(EnvMapInfo[Side].Side, -1 + 2 * PixelX/EnvMapSize

    { We want the generated direction to be exactly in the middle of
      environment map pixel.

      Reasons? For perfection, and to be synchronized with captured
      images (from OpenGL, and such), and to be synchronized with
      DirectionToEnvMap, and to cover the environment map most fairly.

      So we want to have results for min PixelX (0) and max PixelX
      (EnvMapSize - 1) have the same distance from middle (absolute value).
      For "-1 + 2 * PixelX/EnvMapSize", we have min = -1,
      max = -1 + 2 * (1 - 1/EnvMapSize) = -1 + 2 - 2/EnvMapSize
      = 1 - 2/EnvMapSize.

      So if we just add 1/EnvMapSize, we'll have
      min = -1 + 1/EnvMapSize,
      max =  1 - 1/EnvMapSize,
      so all will be perfect. }
      + 1/EnvMapSize
    ));
  VectorAddTo1st(Result,
    VectorScale(EnvMapInfo[Side].Up  , -1 + 2 * PixelY/EnvMapSize
      + 1/EnvMapSize));
end;

procedure DirectionToEnvMap(const Dir: TVector3Single;
  out Side: TEnvMapSide; out Pixel: Cardinal);
var
  SidePlane: TVector4Single;
  SidePlaneDir: TVector3Single absolute SidePlane;
  SideCoord: Integer;
  SideIntersect: TVector3Single;
  PixelX, PixelY: Integer;
begin
  SideCoord := MaxAbsVectorCoord(Dir);
  case SideCoord of
    0: if Dir[0] >= 0 then Side := emsRight else Side := emsLeft;
    1: if Dir[1] >= 0 then Side := emsFront else Side := emsBack;
    2: if Dir[2] >= 0 then Side := emsTop   else Side := emsBottom;
  end;

  SidePlaneDir := EnvMapInfo[Side].Dir;
  SidePlane[3] := -1;

  if not TryPlaneRayIntersection(SideIntersect,
    SidePlane, ZeroVector3Single, Dir) then
    raise Exception.CreateFmt('DirectionToEnvMap: direction (%s) doesn''t hit it''s env map side (%d)',
      [VectorToRawStr(Dir), Side]);

  { We're not interested in this coord, this is either 1 or -1.
    Having this non-zero would break VectorDotProduct (projecting to Side/Up)
    in following code. }
  SideIntersect[SideCoord] := 0;

  PixelX := Round(MapRange(
    VectorDotProduct(SideIntersect, EnvMapInfo[Side].Side),
    { 1/EnvMapSize here, to take into account that the perfect ray
      goes exactly through the middle pixel of env map pixel.
      See EnvMapDirection reasoning. }
    -1 + 1/EnvMapSize,
     1 - 1/EnvMapSize,
    0, EnvMapSize - 1));

  PixelY := Round(MapRange(
    VectorDotProduct(SideIntersect, EnvMapInfo[Side].Up),
    -1 + 1/EnvMapSize,
     1 - 1/EnvMapSize,
    0, EnvMapSize - 1));

  { clamp, just to be safe }
  Clamp(PixelX, 0, EnvMapSize - 1);
  Clamp(PixelY, 0, EnvMapSize - 1);

  Pixel := PixelY * EnvMapSize + PixelX;
end;

procedure Direction4ToEnvMap(const Dir: TVector3Single;
  out Side: TEnvMapSide4;
  out Pixel: TVector4Cardinal;
  out Ratio: TVector4Single);
var
  SidePlane: TVector4Single;
  SidePlaneDir: TVector3Single absolute SidePlane;
  SideCoord: Integer;
  SideIntersect: TVector3Single;
  PixelFX, PixelFY, PixelXFrac, PixelYFrac: Single;
  PixelX, PixelY: array [0..3] of Cardinal;
  I: Cardinal;
  PixelXTrunc, PixelYTrunc: Integer;
begin
  SideCoord := MaxAbsVectorCoord(Dir);
  case SideCoord of
    0: if Dir[0] >= 0 then Side[0] := emsRight else Side[0] := emsLeft;
    1: if Dir[1] >= 0 then Side[0] := emsFront else Side[0] := emsBack;
    2: if Dir[2] >= 0 then Side[0] := emsTop   else Side[0] := emsBottom;
  end;

  { TODO: for now, all four sides are always equal.
    This means that bilinear interpolation using this will look
    wrong (have aliasing) on the edges of cube map. }
  Side[1] := Side[0];
  Side[2] := Side[0];
  Side[3] := Side[0];

  SidePlaneDir := EnvMapInfo[Side[0]].Dir;
  SidePlane[3] := -1;

  if not TryPlaneRayIntersection(SideIntersect,
    SidePlane, ZeroVector3Single, Dir) then
    raise Exception.CreateFmt('DirectionToEnvMap: direction (%s) doesn''t hit it''s env map side (%d)',
      [VectorToRawStr(Dir), Side[0]]);

  { We're not interested in this coord, this is either 1 or -1.
    Having this non-zero would break VectorDotProduct (projecting to Side/Up)
    in following code. }
  SideIntersect[SideCoord] := 0;

  PixelFX := MapRange(
    VectorDotProduct(SideIntersect, EnvMapInfo[Side[0]].Side),
    { 1/EnvMapSize here, to take into account that the perfect ray
      goes exactly through the middle pixel of env map pixel.
      See EnvMapDirection reasoning. }
    -1 + 1/EnvMapSize,
     1 - 1/EnvMapSize,
    0, EnvMapSize - 1);

  PixelFY := MapRange(
    VectorDotProduct(SideIntersect, EnvMapInfo[Side[0]].Up),
    -1 + 1/EnvMapSize,
     1 - 1/EnvMapSize,
    0, EnvMapSize - 1);

  PixelXTrunc := Trunc(PixelFX);
  PixelYTrunc := Trunc(PixelFY);
  PixelXFrac := Frac(PixelFX);
  PixelYFrac := Frac(PixelFY);

  PixelX[0] := PixelXTrunc;
  PixelY[0] := PixelYTrunc;
  Ratio[0] := (1-PixelXFrac) * (1-PixelYFrac);

  PixelX[1] := PixelXTrunc+1;
  PixelY[1] := PixelYTrunc;
  Ratio[1] := PixelXFrac * (1-PixelYFrac);

  PixelX[2] := PixelXTrunc;
  PixelY[2] := PixelYTrunc+1;
  Ratio[2] := (1-PixelXFrac) * PixelYFrac;

  PixelX[3] := PixelXTrunc+1;
  PixelY[3] := PixelYTrunc+1;
  Ratio[3] := PixelXFrac * PixelYFrac;

  { test: Writeln((Ratio[0] + Ratio[1] + Ratio[2] + Ratio[3]):1:10); }

  { clamp, just to be safe }
  for I := 0 to 3 do
  begin
    Clamp(PixelX[I], 0, EnvMapSize - 1);
    Clamp(PixelY[I], 0, EnvMapSize - 1);
    Pixel[I] := PixelY[I] * EnvMapSize + PixelX[I];
  end;
end;

function EnvMapSolidAngle(const Side: TEnvMapSide;
  const Pixel: Cardinal): Float;

  { An approximation of solid angle valid for small angles is:

    cos(angle between vector from zero through middle of the polygon
              and normal vector of polygon)
    * polygon area
    / Sqr(distance from zero to middle of the polygon)

    "middle of the polygon" = just Dir (we depend here on EnvMapDirection
    implementation --- this is Ok, we're in the same unit).

    The cos(...) = vector dot product between normalized(dir) and normal of
    this side.

    The area is always Sqr(2/EnvMapSize) (since cube map = cube of size 2,
    each side has EnvMapSize * EnvMapSize pixels. }

var
  Dir: TVector3Single;
  DirLength: Single;
begin
  Dir := EnvMapDirection(Side, Pixel);
  DirLength := VectorLen(Dir);

  { normalize Dir. Since we already have DirLength,
    we can just call VectorScale. }
  VectorScaleTo1st(Dir, 1/DirLength);

  Result := VectorDotProduct(Dir, EnvMapInfo[Side].Dir) *
    ( 4 / Sqr(EnvMapSize) ) /
    Sqr(DirLength);
end;

end.
