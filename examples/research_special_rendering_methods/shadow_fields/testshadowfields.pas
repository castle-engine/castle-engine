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

{ Test of ShadowFields unit.

  See castle_game_engine/tests/ for a program that will
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

uses CastleVectors, ShadowFields, CastleUtils, CastleFilesUtils,
  CastleCompositeImage, CastleCubeMaps;

procedure TTestShadowFields.Test1;
var
  SF: TShadowField;
  FileName: string;
  Map: PCubeMapByte;
begin
  FileName := GetTempFileNameCheck;

  { Save and load shadow field with some non-zero values.
    Tests that save, load work.
    Also test that CubeMapFromPoint works, and returns various env maps. }

  SF := TShadowField.Create;
  try
    FillChar(SF.EnvMaps, SizeOf(SF.EnvMaps), 0);
    SF.FirstSphereRadius := 2;
    SF.LastSphereRadius := 10;
    SF.SpheresMiddle := Vector3Single(50, 0, 0);

    Map := SF.EnvMapFromPoint(Vector3Single(100, 100, 100), 1);
    Assert(Map = nil);
    Map := SF.EnvMapFromPoint(Vector3Single(50, 0, 0), 1);
    Assert(Map = nil);
    Map := SF.EnvMapFromPoint(Vector3Single(0, 0, 0), 1);
    Assert(Map = nil);

    Map := SF.EnvMapFromPoint(Vector3Single(55, 0, 0), 1);
    Assert(Map <> nil);
    Map := SF.EnvMapFromPoint(Vector3Single(51, 0, 0), 1);
    Map^[csPositiveX, 10] := 55;
    Map^[csNegativeX, 20] := 66;

    Map := SF.EnvMapFromPoint(Vector3Single(55, 0, 0), 1);
    Map^[csPositiveX, 10] := 11;
    Map^[csNegativeX, 20] := 22;

    Map := SF.EnvMapFromPoint(Vector3Single(60, 0, 0), 1);
    Map^[csPositiveX, 10] := 99;
    Map^[csNegativeX, 20] := 88;

    SF.SaveToFile(FileName);
  finally FreeAndNil(SF) end;

  SF := TShadowField.Create;
  try
    SF.LoadFromFile(FileName);
    Assert(SF.FirstSphereRadius = 2);
    Assert(SF.LastSphereRadius = 10);

    Map := SF.EnvMapFromPoint(Vector3Single(100, 100, 100), 1);
    Assert(Map = nil);
    Map := SF.EnvMapFromPoint(Vector3Single(50, 0, 0), 1);
    Assert(Map = nil);
    Map := SF.EnvMapFromPoint(Vector3Single(0, 0, 0), 1);
    Assert(Map = nil);

    Map := SF.EnvMapFromPoint(Vector3Single(55, 0, 0), 1);
    Assert(Map <> nil);

    Map := SF.EnvMapFromPoint(Vector3Single(51, 0, 0), 1);
    Assert(Map^[csPositiveX, 10] = 55);
    Assert(Map^[csNegativeX, 20] = 66);

    Map := SF.EnvMapFromPoint(Vector3Single(55, 0, 0), 1);
    Assert(Map^[csPositiveX, 10] = 11);
    Assert(Map^[csNegativeX, 20] = 22);

    Map := SF.EnvMapFromPoint(Vector3Single(60, 0, 0), 1);
    Assert(Map^[csPositiveX, 10] = 99);
    Assert(Map^[csNegativeX, 20] = 88);

    { Also, test PointFromIndex }
    Assert(
      VectorLen(SF.PointFromIndex(0, csPositiveX, 0)) <
      VectorLen(SF.PointFromIndex(1, csPositiveX, 0)));

    Assert(
      VectorLen(SF.PointFromIndex(1, csPositiveX, 0)) <
      VectorLen(SF.PointFromIndex(2, csPositiveX, 0)));

    Assert(
      VectorLen(SF.PointFromIndex(10, csPositiveX, 0)) <
      VectorLen(SF.PointFromIndex(11, csPositiveX, 0)));

    { Test PointFromIndex further: since SpheresMiddle is in +x,
      so looking at csNegativeX sides with larger radius gets us closer to 0. }
    Assert(
      VectorLen(SF.PointFromIndex(0, csNegativeX, 0)) >
      VectorLen(SF.PointFromIndex(1, csNegativeX, 0)));

    Assert(
      VectorLen(SF.PointFromIndex(1, csNegativeX, 0)) >
      VectorLen(SF.PointFromIndex(2, csNegativeX, 0)));

    Assert(
      VectorLen(SF.PointFromIndex(10, csNegativeX, 0)) >
      VectorLen(SF.PointFromIndex(11, csNegativeX, 0)));
  finally FreeAndNil(SF) end;

  CheckDeleteFile(FileName, true);
end;

initialization
  RegisterTest(TTestShadowFields);
end.
