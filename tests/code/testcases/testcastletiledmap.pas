// -*- compile-command: "./test_single_testcase.sh TTestCastleTiledMap" -*-
{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleTiledMap unit. }
unit TestCastleTiledMap;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleTiledMap = class(TCastleTestCase)
  published
    procedure TestLoading;
  end;

implementation

uses CastleTiledMap;

procedure TTestCastleTiledMap.TestLoading;
var
  Map: TCastleTiledMapControl;
begin
  Map := TCastleTiledMapControl.Create(nil);
  try
    Map.Url := 'castle-data:/tiled-maps/desert.tmx';
    Map.Url := 'castle-data:/tiled-maps/desert_with_objects.tmx';
    Map.Url := 'castle-data:/tiled-maps/hexagonal-mini.tmx';
    Map.Url := 'castle-data:/tiled-maps/isometric_grass_and_water.tmx';
    Map.Url := 'castle-data:/tiled-maps/orthogonal-outside.tmx';
    Map.Url := 'castle-data:/tiled-maps/perspective_walls.tmx';
    Map.Url := 'castle-data:/tiled-maps/sewers.tmx';
    Map.Url := 'castle-data:/tiled-maps2/map-hexagonal.tmx';
    Map.Url := 'castle-data:/tiled-maps2/map-isometric-staggered.tmx';
    Map.Url := 'castle-data:/tiled-maps2/map-isometric-test-non-square.tmx';
    Map.Url := 'castle-data:/tiled-maps2/map-isometric.tmx';
    Map.Url := 'castle-data:/tiled-maps2/map-orthogonal.tmx';
  finally FreeAndNil(Map) end;
end;

initialization
  RegisterTest(TTestCastleTiledMap);
end.
