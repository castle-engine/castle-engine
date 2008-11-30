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

uses VectorMath;

type
  TEnvMapSide = 0..5;
  TEnvMapInfo = record
    Dir, Up, Side: TVector3Single;
    ScreenX, ScreenY: Integer;
  end;

const
  EnvMapSize = 16;

  EnvMapInfo: array [TEnvMapSide] of TEnvMapInfo =
  ( (Dir: ( 1,  0, 0); Up: (0, 0, 1); Side: ( 0, -1, 0); ScreenX: 10                 ; ScreenY: 100),
    (Dir: ( 0, -1, 0); Up: (0, 0, 1); Side: (-1,  0, 0); ScreenX: 10 +     EnvMapSize; ScreenY: 100),
    (Dir: (-1,  0, 0); Up: (0, 0, 1); Side: ( 0,  1, 0); ScreenX: 10 + 2 * EnvMapSize; ScreenY: 100),
    (Dir: ( 0,  1, 0); Up: (0, 0, 1); Side: ( 1,  0, 0); ScreenX: 10 + 3 * EnvMapSize; ScreenY: 100),
    (Dir: (0, 0,  1); Up: (-1, 0, 0); Side: (0, -1, 0); ScreenX: 10; ScreenY: 100 + EnvMapSize),
    (Dir: (0, 0, -1); Up: ( 1, 0, 0); Side: (0, -1, 0); ScreenX: 10; ScreenY: 100 - EnvMapSize)
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

implementation

uses SysUtils, KambiUtils;

function EnvMapDirection(const Side: TEnvMapSide;
  const Pixel: Cardinal): TVector3Single;
var
  PixelX, PixelY: Cardinal;
begin
  PixelX := Pixel mod EnvMapSize;
  PixelY := Pixel div EnvMapSize;
  { Result = exactly EnvMapInfo[Side].Dir when
    PixelX/Y = EnvMapSize/2 (pixel is on the middle of the image). }
  Result :=  EnvMapInfo[Side].Dir;
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
    0: if Dir[0] >= 0 then Side := 0 else Side := 2;
    1: if Dir[1] >= 0 then Side := 3 else Side := 1;
    2: if Dir[2] >= 0 then Side := 4 else Side := 5;
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

end.
