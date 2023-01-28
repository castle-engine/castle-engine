// -*- compile-command: "./test_single_testcase.sh TTestCastleTiledMap" -*-
{
  Copyright 2021-2023 Michalis Kamburelis.

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
    procedure TestLoadingUi;
    procedure TestLoadingTransform;
  end;

implementation

uses CastleTiledMap;

const
  TestMaps: array [0..12] of String = (
    'castle-data:/tiled-maps/desert.tmx',
    'castle-data:/tiled-maps/desert_with_objects.tmx',
    'castle-data:/tiled-maps/hexagonal-mini.tmx',
    'castle-data:/tiled-maps/isometric_grass_and_water.tmx',
    'castle-data:/tiled-maps/orthogonal-outside.tmx',
    'castle-data:/tiled-maps/perspective_walls.tmx',
    'castle-data:/tiled-maps/sewers.tmx',
    // See https://github.com/castle-engine/castle-engine/pull/438
    'castle-data:/tiled-maps/desert_with_empty_objects_layer.tmx',
    'castle-data:/tiled-maps2/map-hexagonal.tmx',
    'castle-data:/tiled-maps2/map-isometric-staggered.tmx',
    'castle-data:/tiled-maps2/map-isometric-test-non-square.tmx',
    'castle-data:/tiled-maps2/map-isometric.tmx',
    'castle-data:/tiled-maps2/map-orthogonal.tmx'
  );

procedure TTestCastleTiledMap.TestLoadingUi;
var
  Map: TCastleTiledMapControl;
  MapUrl: String;
begin
  Map := TCastleTiledMapControl.Create(nil);
  try
    for MapUrl in TestMaps do
      Map.Url := MapUrl;
  finally FreeAndNil(Map) end;
end;

procedure TTestCastleTiledMap.TestLoadingTransform;
var
  Map: TCastleTiledMap;
  MapUrl: String;
begin
  Map := TCastleTiledMap.Create(nil);
  try
    for MapUrl in TestMaps do
      Map.Url := MapUrl;
  finally FreeAndNil(Map) end;
end;

initialization
  RegisterTest(TTestCastleTiledMap);
end.
