{
  Copyright 2020-2020 Michalis Kamburelis.

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

uses Classes, SysUtils, FpcUnit, TestUtils, TestRegistry;

type
  TTestCastleLevels = class(TTestCase)
  published
    { Test that TLevel.Load works even when GL context is not ready yet }
    procedure TestLoadLevelWithoutGLContext;
  end;

implementation

uses CastleApplicationProperties, CastleViewport, CastleLevels;

procedure TTestCastleLevels.TestLoadLevelWithoutGLContext;
var
  Viewport: TCastleViewport;
  Level: TLevel;
begin
  AssertFalse(ApplicationProperties.IsGLContextOpen);

  Viewport := TCastleViewport.Create(nil);

  Level := TLevel.Create(nil);
  Level.Viewport := Viewport;
  Level.Load('level_without_loading_image');

  Level.Free;
  Viewport.Free;
end;

initialization
  RegisterTest(TTestCastleLevels);
end.
