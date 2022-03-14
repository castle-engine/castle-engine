// -*- compile-command: "./test_single_testcase.sh TTestCastleResources" -*-
{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleResources unit. }
unit TestCastleResources;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleResources = class(TCastleTestCase)
  published
    procedure TestResourceFromCode;
  end;

implementation

uses CastleResources, CastleCreatures, CastleLevels, CastleViewport, CastleVectors;

procedure TTestCastleResources.TestResourceFromCode;
var
  Res: TStillCreatureResource;
  MyCreature: TCreature;
  Viewport: TCastleViewport;
  Level: TLevel;
begin
  // set all variables to nil, to allow easy "finally" clause
  Res := nil;
  Viewport := nil;
  Level := nil;
  MyCreature := nil;

  try
    Res := TStillCreatureResource.Create(nil);
    Res.Name := 'KnightCreatedFromCodeTest';
    Res.ModelURL := 'castle-data:/knight_resource/knight.gltf';
    Res.Animations.FindName('idle').AnimationName := 'Idle';

    AssertEquals(1.0, Res.ScaleMin);
    AssertEquals(1.0, Res.ScaleMax);

    // create creature from this resource

    Resources.Clear;
    Resources.Add(Res);

    Viewport := TCastleViewport.Create(nil);

    Levels.Clear;
    Levels.LoadFromFiles('castle-data:/game/level_without_loading_image');

    Level := TLevel.Create(nil);
    Level.Viewport := Viewport;
    Level.Load('level_without_loading_image');

    Resources.Prepare(Viewport.PrepareParams, 'resources');

    MyCreature := Res.CreateCreature(Level,
      { Translation } Vector3(0, 0, 0),
      { Direction } Vector3(0, 0, 1));
  finally
    FreeAndNil(MyCreature);
    FreeAndNil(Level);
    FreeAndNil(Viewport);

    Resources.Clear;
    // FreeAndNil(Res); // Resources owns it
  end;
end;

initialization
  RegisterTest(TTestCastleResources);
end.
