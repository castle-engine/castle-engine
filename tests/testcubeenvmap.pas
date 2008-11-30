{
  Copyright 2008 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit TestCubeEnvMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestCubeEnvMap = class(TTestCase)
  published
    procedure TestReverse;
  end;

implementation

uses VectorMath, CubeEnvMap;

procedure TTestCubeEnvMap.TestReverse;
var
  Side, NewSide: TEnvMapSide;
  Pixel, NewPixel: Cardinal;
  Dir: TVector3Single;
begin
  for Side := Low(Side) to High(Side) do
    for Pixel := 0 to Sqr(EnvMapSize) - 1 do
    begin
      Dir := EnvMapDirection(Side, Pixel);
      DirectionToEnvMap(Dir, NewSide, NewPixel);
      Assert(NewSide = Side);
      Assert(NewPixel = Pixel);
    end;
end;

initialization
  RegisterTest(TTestCubeEnvMap);
end.
