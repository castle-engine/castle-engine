// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestScene" -*-
{
  Copyright 2004-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleScene unit. }
unit TestCastleScene;

interface

uses
  Classes, SysUtils, FpcUnit, TestUtils, TestRegistry;

type
  TTestScene = class(TTestCase)
  published
    procedure TestScene;
  end;

implementation

uses X3DNodes, CastleSceneCore, CastleScene, CastleBoxes, CastleVectors,
  CastleInternalRays, CastleProjection;

procedure TTestScene.TestScene;

  procedure EmptySceneAsserts(EmptyScene: TCastleScene);
  var
    CamProjection: TProjectionType;
    CamPos, CamDir, CamUp, GravityUp: TVector3;
  begin
    AssertTrue(EmptyScene.VerticesCount(false) = 0);
    AssertTrue(EmptyScene.VerticesCount(true) = 0);
    AssertTrue(EmptyScene.TrianglesCount(false) = 0);
    AssertTrue(EmptyScene.TrianglesCount(true) = 0);

    AssertTrue(EmptyScene.BoundingBox.IsEmpty);

    AssertTrue(EmptyScene.ShapesActiveCount = 0);

    AssertTrue(EmptyScene.GetViewpoint(CamProjection, CamPos, CamDir, CamUp, GravityUp) = nil);

    AssertTrue(EmptyScene.FogStack.Top = nil);

    AssertTrue(EmptyScene.InternalBackgroundRenderer = nil);
  end;

var EmptyScene: TCastleScene;
begin
 EmptyScene := TCastleScene.Create(nil);
 try
  EmptySceneAsserts(EmptyScene);
  EmptyScene.ChangedAll;
  EmptySceneAsserts(EmptyScene);
 finally FreeAndNil(EmptyScene) end;

 EmptyScene := TCastleScene.Create(nil);
 try
  EmptyScene.Load(TX3DRootNode.Create, true);
  EmptySceneAsserts(EmptyScene);
  EmptyScene.ChangedAll;
  EmptySceneAsserts(EmptyScene);
 finally FreeAndNil(EmptyScene) end;
end;

initialization
 RegisterTest(TTestScene);
end.
