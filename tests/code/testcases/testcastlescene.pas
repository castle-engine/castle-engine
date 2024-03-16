// -*- compile-command: "./test_single_testcase.sh TTestScene" -*-
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
  Classes, SysUtils,
  CastleTester;

type
  TTestScene = class(TCastleTestCase)
  published
    procedure TestScene;
    procedure TestSpatialUpgrade;
  end;

implementation

uses X3DNodes, CastleSceneCore, CastleScene, CastleBoxes, CastleVectors,
  CastleInternalRays, CastleProjection, CastleComponentSerialize, CastleUIControls;

procedure TTestScene.TestScene;

  procedure EmptySceneAsserts(EmptyScene: TCastleScene);
  var
    CamProjection: TProjectionType;
    CamPos, CamDir, CamUp, GravityUp: TVector3;
  begin
    AssertTrue(EmptyScene.VerticesCount = 0);
    AssertTrue(EmptyScene.TrianglesCount = 0);

    AssertTrue(EmptyScene.BoundingBox.IsEmpty);

    AssertTrue(EmptyScene.ShapesActiveCount = 0);

    AssertTrue(EmptyScene.GetViewpoint(CamProjection, CamPos, CamDir, CamUp, GravityUp) = nil);

    AssertTrue(EmptyScene.FogStack.Top = nil);

    AssertTrue(EmptyScene.InternalBackgroundRenderer = nil);
  end;

var
  EmptyScene: TCastleScene;
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

procedure TTestScene.TestSpatialUpgrade;
var
  SceneSpatialNone, SceneSpatialPreciseCollisions,
    SceneSpatialOnlyRendering, SceneSpatialOnlyDynamicCollisions,
    SceneSpatialOnlyVisibleTriangles, SceneSpatialOnlyStaticCollisions: TCastleScene;
  UiOwner: TComponent;
begin
  UiOwner := TComponent.Create(nil);
  try
    UserInterfaceLoad('castle-data:/designs/test_spatial_upgrade.castle-user-interface', UiOwner);

    SceneSpatialNone := UiOwner.FindRequiredComponent('SceneSpatialNone') as TCastleScene;
    SceneSpatialPreciseCollisions := UiOwner.FindRequiredComponent('SceneSpatialPreciseCollisions') as TCastleScene;
    SceneSpatialOnlyRendering := UiOwner.FindRequiredComponent('SceneSpatialOnlyRendering') as TCastleScene;
    SceneSpatialOnlyDynamicCollisions := UiOwner.FindRequiredComponent('SceneSpatialOnlyDynamicCollisions') as TCastleScene;
    SceneSpatialOnlyVisibleTriangles := UiOwner.FindRequiredComponent('SceneSpatialOnlyVisibleTriangles') as TCastleScene;
    SceneSpatialOnlyStaticCollisions := UiOwner.FindRequiredComponent('SceneSpatialOnlyStaticCollisions') as TCastleScene;

    AssertTrue(SceneSpatialNone.Spatial = []);
    AssertTrue(SceneSpatialPreciseCollisions.Spatial = [ssRendering, ssDynamicCollisions]);
    AssertTrue(SceneSpatialOnlyRendering.Spatial = [ssRendering]);
    AssertTrue(SceneSpatialOnlyDynamicCollisions.Spatial = [ssDynamicCollisions]);
    AssertTrue(SceneSpatialOnlyVisibleTriangles.Spatial = [ssVisibleTriangles]);
    AssertTrue(SceneSpatialOnlyStaticCollisions.Spatial = [ssStaticCollisions]);

    AssertFalse(SceneSpatialNone.PreciseCollisions);
    AssertTrue(SceneSpatialPreciseCollisions.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyRendering.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyDynamicCollisions.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyVisibleTriangles.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyStaticCollisions.PreciseCollisions);
  finally FreeAndNil(UiOwner) end;
end;

initialization
  RegisterTest(TTestScene);
end.
