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
    procedure TestEnvMapSolidAngle;
  end;

implementation

uses VectorMath, CubeEnvMap, Math;

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

procedure TTestCubeEnvMap.TestEnvMapSolidAngle;
var
  Side: TEnvMapSide;
  Pixel: Cardinal;
  SphereArea, SphereArea2: Float;
begin
  SphereArea := 0;
  SphereArea2 := 0;

  for Side := Low(Side) to High(Side) do
    for Pixel := 0 to Sqr(EnvMapSize) - 1 do
    begin
      SphereArea += EnvMapSolidAngle(Side, Pixel);
      SphereArea2 += (4 * Pi)/(6*Sqr(EnvMapSize));
    end;

  { I use SphereArea2 only to test the accuracy of addition:
    remember that even if EnvMapSolidAngle would be perfect,
    adding "6*Sqr(EnvMapSize)" values together always has some precision
    error accumulated.

    Although in practice, it turned out that SphereArea2 is identical
    (at least on first 30 decimal places) to 4*Pi. So Float accuracy
    is nice. }

  {
  Writeln(SphereArea:1:30);
  Writeln(SphereArea2:1:30);
  Writeln((4 * Pi):1:30);
  }

  { EnvMapSolidAngle is a gross approximation now, so we allow quite large
    error. }

  Assert(FloatsEqual(SphereArea, 4 * Pi, 0.02));
end;

initialization
  RegisterTest(TTestCubeEnvMap);
end.
