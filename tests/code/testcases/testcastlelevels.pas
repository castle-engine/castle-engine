// -*- compile-command: "./test_single_testcase.sh TTestCastleLevels" -*-
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

{ Test CastleLevels. }
unit TestCastleLevels;

interface

uses Classes, SysUtils, CastleTester;

type
  TTestCastleLevels = class(TCastleTestCase)
  published
    { Test that TLevel.Load works even when GL context is not ready yet }
    procedure TestLoadLevelWithoutGLContext;
    { Test setting/destroying Player after TLevel.Load }
    procedure TestSetDestroyPlayerAfterLevelLoad;
  end;

implementation

uses CastleApplicationProperties, CastleViewport, CastleLevels, CastlePlayer;

procedure TTestCastleLevels.TestLoadLevelWithoutGLContext;
var
  Viewport: TCastleViewport;
  Level: TLevel;
begin
  { This test should pass both with and without OpenGL context. }
  //AssertFalse(ApplicationProperties.IsGLContextOpen);

  Viewport := TCastleViewport.Create(nil);

  Levels.Clear;
  Levels.LoadFromFiles('castle-data:/game/level_without_loading_image');

  Level := TLevel.Create(nil);
  Level.Viewport := Viewport;
  Level.Load('level_without_loading_image');

  Level.Free;
  Viewport.Free;
end;

procedure TTestCastleLevels.TestSetDestroyPlayerAfterLevelLoad;
var
  Level: TLevel;
  Viewport: TCastleViewport;
  Player: TPlayer;
begin
  Player := TPlayer.Create(nil);
  Viewport := TCastleViewport.Create(nil);

  Levels.Clear;
  Levels.LoadFromFiles('castle-data:/game/level_without_loading_image');

  Level := TLevel.Create(nil);
  Level.Viewport := Viewport;
  Level.Load('level_without_loading_image');

  Level.Player := Player;
  FreeAndNil(Player);

  Player := TPlayer.Create(nil);
  Level.Player := Player;

  //Level.Load('level_without_loading_image');
  FreeAndNil(Player);
  Player := TPlayer.Create(nil);
  Level.Player := Player;
  Level.Free;
  Player.Free;
  Viewport.Free;
end;

initialization
  RegisterTest(TTestCastleLevels);
end.
