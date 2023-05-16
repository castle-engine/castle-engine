// -*- compile-command: "./test_single_testcase.sh TTestCastleInternalGLShadowVolumes" -*-
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleInternalGLShadowVolumes;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleInternalGLShadowVolumes = class(TCastleTestCase)
  published
    procedure TestCasterShadowPossiblyVisible;
  end;

implementation

uses CastleTransform, CastleFrustum, CastleInternalGLShadowVolumes, CastleProjection,
  CastleVectors, CastleViewport, CastleBoxes;

procedure TTestCastleInternalGLShadowVolumes.TestCasterShadowPossiblyVisible;
var
  Viewport: TCastleViewport;
  Camera: TCastleCamera;
  Projection: TProjection;
  Frustum: TFrustum;
  SvRenderer: TGLShadowVolumeRenderer;
begin
  Viewport := TCastleViewport.Create(nil);

  Camera := Viewport.Camera;
  { stand at (10, 0, 0), looking in -Z, up in +Y }
  Camera.SetView(
    Vector3(10, 0, 0),
    Vector3(0, 0, -1),
    Vector3(0, 1, 0));

  FillChar(Projection, SizeOf(Projection), #0);
  Projection.ProjectionType := ptPerspective;
  Projection.PerspectiveAnglesRad := Vector2(Pi / 3, Pi / 3);
  Projection.ProjectionNear := 1;
  Projection.ProjectionFar := ZFarInfinity;

  Frustum.Init(Projection.Matrix(1), Camera.Matrix);
  AssertTrue(Frustum.ZFarInfinity);

  SvRenderer := TGLShadowVolumeRenderer.Create;
  try
    // point light test
    SvRenderer.InitFrustumAndLight(Frustum, Vector4(-10, 0, 0, 1));
    AssertTrue(SvRenderer.GetCasterShadowPossiblyVisible(
      Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)).Transform(TranslationMatrix(0, 0, 0))
    ));
    AssertTrue(SvRenderer.GetCasterShadowPossiblyVisible(
      Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)).Transform(TranslationMatrix(-10, 0, 0))
    ));
    AssertTrue(SvRenderer.GetCasterShadowPossiblyVisible(
      Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)).Transform(TranslationMatrix(10, 0, 0))
    ));
    // too far to the right to cast shadows
    AssertFalse(SvRenderer.GetCasterShadowPossiblyVisible(
      Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)).Transform(TranslationMatrix(20, 0, 0))
    ));

    // directional light test
    SvRenderer.InitFrustumAndLight(Frustum, Vector4(1, 0, 0, 0));
    AssertTrue(SvRenderer.GetCasterShadowPossiblyVisible(
      Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)).Transform(TranslationMatrix(0, 0, 0))
    ));
    AssertTrue(SvRenderer.GetCasterShadowPossiblyVisible(
      Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)).Transform(TranslationMatrix(-10, 0, 0))
    ));
    AssertTrue(SvRenderer.GetCasterShadowPossiblyVisible(
      Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)).Transform(TranslationMatrix(10, 0, 0))
    ));
    // too far to the right to cast shadows
    AssertFalse(SvRenderer.GetCasterShadowPossiblyVisible(
      Box3D(Vector3(-1, -1, -1), Vector3(1, 1, 1)).Transform(TranslationMatrix(20, 0, 0))
    ));
  finally
    FreeAndNil(SvRenderer);
    FreeAndNil(Viewport);
  end;
end;

initialization
  RegisterTest(TTestCastleInternalGLShadowVolumes);
end.
