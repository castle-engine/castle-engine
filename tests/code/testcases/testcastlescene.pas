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
    procedure TestImageTransformImageWidthHeight;
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

    AssertFalse(SceneSpatialNone.PreciseCollisions);
    AssertTrue(SceneSpatialPreciseCollisions.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyRendering.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyDynamicCollisions.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyVisibleTriangles.PreciseCollisions);
    AssertTrue(SceneSpatialOnlyStaticCollisions.PreciseCollisions);
  finally FreeAndNil(UiOwner) end;
end;

procedure TTestScene.TestImageTransformImageWidthHeight;
var
  ImageTransform: TCastleImageTransform;
begin
  ImageTransform := TCastleImageTransform.Create(nil);
  try
    AssertEquals(0, ImageTransform.ImageWidth);
    AssertEquals(0, ImageTransform.ImageHeight);

    ImageTransform.Url := 'castle-data:/test_texture.png';
    AssertEquals(256, ImageTransform.ImageWidth);
    AssertEquals(256, ImageTransform.ImageHeight);

    ImageTransform.Size := Vector2(100, 200);
    ImageTransform.Scale := Vector3(0.25, 0.333333, 1);

    // ImageWidth and ImageHeight should not change because of Size and Scale
    AssertEquals(256, ImageTransform.ImageWidth);
    AssertEquals(256, ImageTransform.ImageHeight);

    // LocalBoundingBox should reflect Size
    AssertSameValue(100, ImageTransform.LocalBoundingBox.SizeX);
    AssertSameValue(200, ImageTransform.LocalBoundingBox.SizeY);

    // BoundingBox should reflect Size scaled by Scale
    AssertSameValue(100 * 0.25, ImageTransform.BoundingBox.SizeX, 0.01);
    AssertSameValue(200 * 0.333333, ImageTransform.BoundingBox.SizeY, 0.01);

    ImageTransform.Url := '';

    // make sure ImageWidth and ImageHeight are reset to 0
    AssertEquals(0, ImageTransform.ImageWidth);
    AssertEquals(0, ImageTransform.ImageHeight);

    { The box is, for now, not empty, but has zero size. Though we don't promise
      exact behavior of this in API. }
    AssertFalse(ImageTransform.LocalBoundingBox.IsEmpty);
    AssertSameValue(0, ImageTransform.LocalBoundingBox.SizeX);
    AssertSameValue(0, ImageTransform.LocalBoundingBox.SizeY);
    AssertFalse(ImageTransform.BoundingBox.IsEmpty);
    AssertSameValue(0, ImageTransform.BoundingBox.SizeX);
    AssertSameValue(0, ImageTransform.BoundingBox.SizeY);
  finally FreeAndNil(ImageTransform) end;
end;

initialization
  RegisterTest(TTestScene);
end.
