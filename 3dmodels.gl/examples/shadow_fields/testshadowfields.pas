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

  ----------------------------------------------------------------------------
}

{ Test of ShadowFields unit.

  See kambi_vrml_game_engine/tests/ for a program that will
  actually pick up and execute this test. }
unit TestShadowFields;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestShadowFields = class(TTestCase)
  published
    procedure Test1;
  end;

implementation

uses VectorMath, ShadowFields, KambiUtils, CubeEnvMap;

procedure TTestShadowFields.Test1;
var
  SF: TShadowField;
  FileName: string;
  Map: PEnvMap;
begin
  FileName := InclPathDelim(GetTempDir) + 'testing.shadow_field';

  { Save and load shadow field with some non-zero values.
    Tests that save, load work.
    Also test that EnvMapFromPoint works, and returns various env maps. }

  SF := TShadowField.Create;
  try
    FillChar(SF.EnvMaps, SizeOf(SF.EnvMaps), 0);
    SF.FirstSphereRadius := 2;
    SF.LastSphereRadius := 10;
    SF.SpheresMiddle := Vector3Single(50, 0, 0);

    Map := SF.EnvMapFromPoint(Vector3Single(100, 100, 100));
    Assert(Map = nil);
    Map := SF.EnvMapFromPoint(Vector3Single(50, 0, 0));
    Assert(Map = nil);
    Map := SF.EnvMapFromPoint(Vector3Single(0, 0, 0));
    Assert(Map = nil);

    Map := SF.EnvMapFromPoint(Vector3Single(55, 0, 0));
    Assert(Map <> nil);
    Map := SF.EnvMapFromPoint(Vector3Single(51, 0, 0));
    Map^[emsRight, 10] := 55;
    Map^[emsLeft , 20] := 66;

    Map := SF.EnvMapFromPoint(Vector3Single(55, 0, 0));
    Map^[emsRight, 10] := 11;
    Map^[emsLeft , 20] := 22;

    Map := SF.EnvMapFromPoint(Vector3Single(60, 0, 0));
    Map^[emsRight, 10] := 99;
    Map^[emsLeft , 20] := 88;

    SF.SaveToFile(FileName);
  finally FreeAndNil(SF) end;

  SF := TShadowField.Create;
  try
    SF.LoadFromFile(FileName);
    Assert(SF.FirstSphereRadius = 2);
    Assert(SF.LastSphereRadius = 10);

    Map := SF.EnvMapFromPoint(Vector3Single(100, 100, 100));
    Assert(Map = nil);
    Map := SF.EnvMapFromPoint(Vector3Single(50, 0, 0));
    Assert(Map = nil);
    Map := SF.EnvMapFromPoint(Vector3Single(0, 0, 0));
    Assert(Map = nil);

    Map := SF.EnvMapFromPoint(Vector3Single(55, 0, 0));
    Assert(Map <> nil);

    Map := SF.EnvMapFromPoint(Vector3Single(51, 0, 0));
    Assert(Map^[emsRight, 10] = 55);
    Assert(Map^[emsLeft , 20] = 66);

    Map := SF.EnvMapFromPoint(Vector3Single(55, 0, 0));
    Assert(Map^[emsRight, 10] = 11);
    Assert(Map^[emsLeft , 20] = 22);

    Map := SF.EnvMapFromPoint(Vector3Single(60, 0, 0));
    Assert(Map^[emsRight, 10] = 99);
    Assert(Map^[emsLeft , 20] = 88);

    { Also, test PointFromEnvMap }
    Assert(
      VectorLen(SF.PointFromEnvMap(0, emsRight, 0)) <
      VectorLen(SF.PointFromEnvMap(1, emsRight, 0)));

    Assert(
      VectorLen(SF.PointFromEnvMap(1, emsRight, 0)) <
      VectorLen(SF.PointFromEnvMap(2, emsRight, 0)));

    Assert(
      VectorLen(SF.PointFromEnvMap(10, emsRight, 0)) <
      VectorLen(SF.PointFromEnvMap(11, emsRight, 0)));

    { Test PointFromEnvMap further: since SpheresMiddle is in +x,
      so looking at emsLeft sides with larger radius gets us closer to 0. }
    Assert(
      VectorLen(SF.PointFromEnvMap(0, emsLeft, 0)) >
      VectorLen(SF.PointFromEnvMap(1, emsLeft, 0)));

    Assert(
      VectorLen(SF.PointFromEnvMap(1, emsLeft, 0)) >
      VectorLen(SF.PointFromEnvMap(2, emsLeft, 0)));

    Assert(
      VectorLen(SF.PointFromEnvMap(10, emsLeft, 0)) >
      VectorLen(SF.PointFromEnvMap(11, emsLeft, 0)));
  finally FreeAndNil(SF) end;
end;

initialization
  RegisterTest(TTestShadowFields);
end.
