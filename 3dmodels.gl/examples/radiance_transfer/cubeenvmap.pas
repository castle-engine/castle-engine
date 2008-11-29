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

implementation

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
    VectorScale(EnvMapInfo[Side].Side, -1 + 2 * PixelX/EnvMapSize));
  VectorAddTo1st(Result,
    VectorScale(EnvMapInfo[Side].Up  , -1 + 2 * PixelY/EnvMapSize));
end;

end.