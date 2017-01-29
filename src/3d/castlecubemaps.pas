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

{ Utilities for cube (environment) maps. }
unit CastleCubeMaps;

{$I castleconf.inc}

interface

uses Math,
  CastleVectors, CastleCompositeImage;

type
  TCubeMapSide = CastleCompositeImage.TCubeMapSide;

  TCubeMapInfo = record
    Dir, Up, Side: TVector3Single;
    ScreenX, ScreenY: Integer;
  end;

const
  CubeMapSize = 16;

  { Information about cube map faces.

    Names and orientation of faces match precisely OpenGL naming and
    orientation (see http://www.opengl.org/registry/specs/ARB/texture_cube_map.txt),
    so it's straighforward to use this for OpenGL cube maps. }
  CubeMapInfo: array [TCubeMapSide] of TCubeMapInfo =
  ( (Dir: ( 1,  0, 0); Up: (0, -1, 0); Side: ( 0, 0,-1); ScreenX: 3; ScreenY: 0),
    (Dir: (-1,  0, 0); Up: (0, -1, 0); Side: ( 0, 0, 1); ScreenX: 1; ScreenY: 0),

    (Dir: ( 0,  1, 0); Up: (0, 0,  1); Side: ( 1, 0, 0); ScreenX: 2; ScreenY: -1),
    (Dir: ( 0, -1, 0); Up: (0, 0, -1); Side: ( 1, 0, 0); ScreenX: 2; ScreenY: +1),

    (Dir: ( 0, 0,  1); Up: (0, -1, 0); Side: ( 1, 0, 0); ScreenX: 2; ScreenY: 0),
    (Dir: ( 0, 0, -1); Up: (0, -1, 0); Side: (-1, 0, 0); ScreenX: 0; ScreenY: 0)
  );

{ Direction corresponding to given cube map side and pixel number.
  That is, assuming that the middle of cube is placed at (0, 0, 0),
  each pixel of the cube map corresponds to some direction.

  Pixel number must be between 0 and CubeMapSize*2-1.
  It's assumed that pixels
  are in the same order as they are in OpenGL images, that is row-by-row,
  from the lowest row to highest, in each row from left to right.

  Returned vector is @italic(not normalized). }
function CubeMapDirection(const Side: TCubeMapSide;
  const Pixel: Cardinal): TVector3Single;

{ Return cube map side and pixel that is the closest to
  given direction Dir. This is the reverse of CubeMapDirection function.

  Given here Dir need not be normalized, although must not be zero. }
procedure DirectionToCubeMap(const Dir: TVector3Single;
  out Side: TCubeMapSide; out Pixel: Cardinal);

type
  TCubeMapSide4 = array [0..3] of TCubeMapSide;

{ Return 4 cube map indexes (side and pixel, along with ratio)
  that are closest to given direction Dir.
  All ratios will sum to 1.

  This is like DirectionToCubeMap, except this returns 4 values.
  It allows you to do bilinear interpolation between cube map items. }
procedure Direction4ToCubeMap(const Dir: TVector3Single;
  out Side: TCubeMapSide4;
  out Pixel: TVector4Cardinal;
  out Ratio: TVector4Single);

type
  { Cube map, with each item being a Float. }
  TCubeMapFloat = array [TCubeMapSide, 0..Sqr(CubeMapSize) - 1] of Float;
  PCubeMapFloat = ^TCubeMapFloat;

  { Cube map, with each item being in 0..1 range, encoded as a Byte.
    This assumes that every item is actually a float in 0..1 range,
    encoded as Byte. }
  TCubeMapByte = array [TCubeMapSide, 0..Sqr(CubeMapSize) - 1] of Byte;
  PCubeMapByte = ^TCubeMapByte;

{ Calculate solid angle of given pixel on the cube map. }
function CubeMapSolidAngle(const Side: TCubeMapSide;
  const Pixel: Cardinal): Single;

implementation

uses SysUtils, CastleUtils;

{ Note: CubeMapSolidAngle assumes that implementation of this actually
  returns the position of the middle of the pixel. That is, it assumes
  you don't normalize the returned direction. }
function CubeMapDirection(const Side: TCubeMapSide;
  const Pixel: Cardinal): TVector3Single;
var
  PixelX, PixelY: Cardinal;
begin
  PixelX := Pixel mod CubeMapSize;
  PixelY := Pixel div CubeMapSize;
  { Result = exactly CubeMapInfo[Side].Dir when
    PixelX/Y = CubeMapSize/2 (pixel is on the middle of the image). }
  Result := CubeMapInfo[Side].Dir;
  VectorAddVar(Result,
    VectorScale(CubeMapInfo[Side].Side, -1 + 2 * PixelX/CubeMapSize

    { We want the generated direction to be exactly in the middle of
      cube map pixel.

      Reasons? For perfection, and to be synchronized with captured
      images (from OpenGL, and such), and to be synchronized with
      DirectionToCubeMap, and to cover the cube map most fairly.

      So we want to have results for min PixelX (0) and max PixelX
      (CubeMapSize - 1) have the same distance from middle (absolute value).
      For "-1 + 2 * PixelX/CubeMapSize", we have min = -1,
      max = -1 + 2 * (1 - 1/CubeMapSize) = -1 + 2 - 2/CubeMapSize
      = 1 - 2/CubeMapSize.

      So if we just add 1/CubeMapSize, we'll have
      min = -1 + 1/CubeMapSize,
      max =  1 - 1/CubeMapSize,
      so all will be perfect. }
      + 1/CubeMapSize
    ));
  VectorAddVar(Result,
    VectorScale(CubeMapInfo[Side].Up  , -1 + 2 * PixelY/CubeMapSize
      + 1/CubeMapSize));
end;

procedure DirectionToCubeMap(const Dir: TVector3Single;
  out Side: TCubeMapSide; out Pixel: Cardinal);
var
  SidePlane: TVector4Single;
  SidePlaneDir: TVector3Single absolute SidePlane;
  SideCoord: Integer;
  SideIntersect: TVector3Single;
  PixelX, PixelY: Integer;
begin
  SideCoord := MaxAbsVectorCoord(Dir);
  case SideCoord of
    0: if Dir[0] >= 0 then Side := csPositiveX else Side := csNegativeX;
    1: if Dir[1] >= 0 then Side := csPositiveY else Side := csNegativeY;
    2: if Dir[2] >= 0 then Side := csPositiveZ else Side := csNegativeZ;
  end;

  SidePlaneDir := CubeMapInfo[Side].Dir;
  SidePlane[3] := -1;

  if not TryPlaneRayIntersection(SideIntersect,
    SidePlane, ZeroVector3Single, Dir) then
    raise Exception.CreateFmt('DirectionToCubeMap: direction (%s) doesn''t hit it''s cube map side (%d)',
      [VectorToRawStr(Dir), Side]);

  { We're not interested in this coord, this is either 1 or -1.
    Having this non-zero would break VectorDotProduct (projecting to Side/Up)
    in following code. }
  SideIntersect[SideCoord] := 0;

  PixelX := Round(MapRange(
    VectorDotProduct(SideIntersect, CubeMapInfo[Side].Side),
    { 1/CubeMapSize here, to take into account that the perfect ray
      goes exactly through the middle pixel of cube map pixel.
      See CubeMapDirection reasoning. }
    -1 + 1/CubeMapSize,
     1 - 1/CubeMapSize,
    0, CubeMapSize - 1));

  PixelY := Round(MapRange(
    VectorDotProduct(SideIntersect, CubeMapInfo[Side].Up),
    -1 + 1/CubeMapSize,
     1 - 1/CubeMapSize,
    0, CubeMapSize - 1));

  { clamp, just to be safe }
  ClampVar(PixelX, 0, CubeMapSize - 1);
  ClampVar(PixelY, 0, CubeMapSize - 1);

  Pixel := PixelY * CubeMapSize + PixelX;
end;

procedure Direction4ToCubeMap(const Dir: TVector3Single;
  out Side: TCubeMapSide4;
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
    0: if Dir[0] >= 0 then Side[0] := csPositiveX else Side[0] := csNegativeX;
    1: if Dir[1] >= 0 then Side[0] := csPositiveY else Side[0] := csNegativeY;
    2: if Dir[2] >= 0 then Side[0] := csPositiveZ else Side[0] := csNegativeZ;
  end;

  { TODO: for now, all four sides are always equal.
    This means that bilinear interpolation using this will look
    wrong (have aliasing) on the edges of cube map. }
  Side[1] := Side[0];
  Side[2] := Side[0];
  Side[3] := Side[0];

  SidePlaneDir := CubeMapInfo[Side[0]].Dir;
  SidePlane[3] := -1;

  if not TryPlaneRayIntersection(SideIntersect,
    SidePlane, ZeroVector3Single, Dir) then
    raise Exception.CreateFmt('DirectionToCubeMap: direction (%s) doesn''t hit it''s cube map side (%d)',
      [VectorToRawStr(Dir), Side[0]]);

  { We're not interested in this coord, this is either 1 or -1.
    Having this non-zero would break VectorDotProduct (projecting to Side/Up)
    in following code. }
  SideIntersect[SideCoord] := 0;

  PixelFX := MapRange(
    VectorDotProduct(SideIntersect, CubeMapInfo[Side[0]].Side),
    { 1/CubeMapSize here, to take into account that the perfect ray
      goes exactly through the middle pixel of cube map pixel.
      See CubeMapDirection reasoning. }
    -1 + 1/CubeMapSize,
     1 - 1/CubeMapSize,
    0, CubeMapSize - 1);

  PixelFY := MapRange(
    VectorDotProduct(SideIntersect, CubeMapInfo[Side[0]].Up),
    -1 + 1/CubeMapSize,
     1 - 1/CubeMapSize,
    0, CubeMapSize - 1);

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
    ClampVar(PixelX[I], 0, CubeMapSize - 1);
    ClampVar(PixelY[I], 0, CubeMapSize - 1);
    Pixel[I] := PixelY[I] * CubeMapSize + PixelX[I];
  end;
end;

function CubeMapSolidAngle(const Side: TCubeMapSide;
  const Pixel: Cardinal): Single;

{ An approximation of solid angle valid for small angles is:

  cos(angle between vector from zero through middle of the polygon
            and normal vector of polygon)
  * polygon area
  / Sqr(distance from zero to middle of the polygon)

  "middle of the polygon" = just Dir (we depend here on CubeMapDirection
  implementation --- this is Ok, we're in the same unit).

  The cos(...) = vector dot product between normalized(dir) and normal of
  this side.

  The area is always Sqr(2/CubeMapSize) (since cube map = cube of size 2,
  each side has CubeMapSize * CubeMapSize pixels. }

{ A note on accuracy: I tried to return here Float, and use Float values
  in the middle, but testing with
  castle_game_engine/tests/testcubemap.pas
  shows that it doesn't matter. Single is enough, Double or Extended
  doesn't improve it. }

var
  Dir: TVector3Single;
  DirLength: Single;
begin
  Dir := CubeMapDirection(Side, Pixel);
  DirLength := VectorLen(Dir);

  { normalize Dir. Since we already have DirLength,
    we can just call VectorScale. }
  VectorScaleVar(Dir, 1/DirLength);

  Result := VectorDotProduct(Dir, CubeMapInfo[Side].Dir) *
    ( 4 / Sqr(CubeMapSize) ) /
    Sqr(DirLength);
end;

end.
