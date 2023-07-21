// -*- compile-command: "./test_single_testcase.sh TTestCastleViewport" -*-
{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleViewport unit. }
unit TestCastleViewport;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif};

type
  TTestCastleViewport = class(TCastleTestCase)
    procedure TestReadingOldDesigns;
    procedure TestReadingOldDesigns2DSceneManager;
    procedure TestCameraNonDesign;
    procedure TestAutoCameraIgnoresGizmos;
    procedure TestAutoProjection;
    procedure TestEffectiveBlendingSort;
  end;

implementation

uses CastleComponentSerialize, CastleUIControls, CastleViewport, CastleCameras,
  Castle2DSceneManager, CastleProjection, CastleUtils, CastleTransform, CastleVectors,
  CastleScene, CastleBoxes, CastleRenderOptions;

procedure TTestCastleViewport.TestReadingOldDesigns;

  { Check camera was read and inserted into viewport items. }
  procedure AssertCameraUpgraded(const V: TCastleViewport; const CameraName: String);
  begin
    AssertTrue(V.Camera <> nil);
    AssertTrue(V.Camera.Name = CameraName);
    AssertTrue(V.Items.Count > 0);
    AssertTrue(V.Items[V.Items.Count - 1] = V.Camera);
  end;

var
  RootOwner: TComponent;
  //Root: TCastleUserInterface;
begin
  RootOwner := TComponent.Create(nil);
  {Root := }UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/multiple_viewports.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('ViewportTransparent') as TCastleViewport, 'Camera');
    // when reading at runtime, we don't rename it to Camera1 etc. unless we have to, at CastleComponentSerialize
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('ViewportExamine') as TCastleViewport, 'Camera1');
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('ViewportScreenEffect') as TCastleViewport, 'Camera2');
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('ViewportNormal') as TCastleViewport, 'Camera3');
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  {Root := }UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_2d.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('MainViewport') as TCastleViewport, 'Camera');
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  {Root := }UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_3d.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('MainViewport') as TCastleViewport, 'Camera');
    AssertTrue((RootOwner.FindRequiredComponent('WalkNavigation') as TCastleNavigation).Parent =
      RootOwner.FindRequiredComponent('MainViewport'));
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  {Root := }UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_3d_viewer.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('Viewport') as TCastleViewport, 'Camera');
    AssertTrue((RootOwner.FindRequiredComponent('ExamineNavigation1') as TCastleNavigation).Parent =
      RootOwner.FindRequiredComponent('Viewport'));
  finally FreeAndNil(RootOwner) end;
end;

procedure TTestCastleViewport.TestReadingOldDesigns2DSceneManager;

  { Check camera will be inserted into TCastle2DSceneManager items. }
  procedure AssertCameraUpgraded2DSceneManager(const V: TCastle2DSceneManager);
  var
    DummyHandleInput: Boolean;
  begin
    { TCastle2DSceneManager may not include camera as a subcomponent,
      it will be added later during Update. }
    AssertTrue(V.Camera = nil);

    DummyHandleInput := true;
    V.Update(1/30, DummyHandleInput);

    AssertTrue(V.Camera <> nil);
    AssertTrue(V.Items.Count > 0);
    AssertTrue(V.Items[V.Items.Count - 1] = V.Camera);
    AssertTrue(V.Camera.Name = ''); // created by SetupCamera, nameless, to not conflict with existing components
    AssertTrue(V.Camera.ProjectionType = ptOrthographic);
  end;

var
  RootOwner: TComponent;
  //Root: TCastleUserInterface;
begin
  RootOwner := TComponent.Create(nil);
  {Root := }UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/old_scaling_tests_2dscenemanager_without_camera.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded2DSceneManager(RootOwner.FindRequiredComponent('SceneManager1') as TCastle2DSceneManager);
  finally FreeAndNil(RootOwner) end;
end;

procedure TTestCastleViewport.TestCameraNonDesign;
var
  V: TCastleViewport;
  SavedApplicationMode: TCastleApplicationMode;
begin
  // fake design-mode (CGE editor) for test
  SavedApplicationMode := InternalCastleApplicationMode;
  InternalCastleApplicationMode := appDesign;
  try
    V := TCastleViewport.InternalCreateNonDesign(nil);
    try
      // V.Camera will be auto-created, but it will not have a bounding box or any children (so no gizmo)
      AssertTrue(V.Camera <> nil);
      AssertTrue(V.Camera.BoundingBox.IsEmpty);
      AssertTrue(V.Camera.Count = 0);
      AssertTrue(V.Items.BoundingBox.IsEmpty);
    finally FreeAndNil(V) end;
  finally InternalCastleApplicationMode := SavedApplicationMode end;
end;

procedure TTestCastleViewport.TestAutoCameraIgnoresGizmos;
var
  V: TCastleViewport;
  C: TCastleCamera;
  L: TCastlePointLight;
  SavedApplicationMode: TCastleApplicationMode;
begin
  // fake design-mode (CGE editor) for test
  SavedApplicationMode := InternalCastleApplicationMode;
  // TODO: should be done globally at some place where tests start
  InternalCastleDesignData := 'castle-data:/editor-data/';
  InternalCastleApplicationMode := appDesign;
  try
    V := TCastleViewport.Create(nil);
    try
      V.Items.Add(TCastleBox.Create(V));

      { Bbox of C gizmo should not affect camera determined by V.AssignDefaultCamera }
      C := TCastleCamera.Create(V);
      C.Translation := Vector3(10, 10, 10);
      V.Camera := C;
      V.Items.Add(C);

      { Bbox of L gizmo should not affect camera determined by V.AssignDefaultCamera }
      L := TCastlePointLight.Create(V);
      L.Translation := Vector3(100, 100, 100);
      V.Items.Add(L);

      { Bbox of L gizmo should not affect camera determined by V.AssignDefaultCamera }
      L := TCastlePointLight.Create(V);
      L.Translation := Vector3(1000, 1000, 1000);
      V.Items.Add(L);

      { Bbox will be larger because of C and L, this is normal,
        they have bboxes to allow picking. }
      AssertBoxesEqual(Box3D(
        Vector3(-1.00, -1.00, -1.00),
        Vector3(1000.50, 1000.50, 1000.50)
      ), V.Items.BoundingBox);

      V.AssignDefaultCamera;

      AssertVectorEquals(Vector3(0.00, 0.00, 5.00), C.Translation);
    finally FreeAndNil(V) end;
  finally InternalCastleApplicationMode := SavedApplicationMode end;
end;

type
  TMyViewport = class(TCastleViewport)
    function Proj: TProjection;
  end;

function TMyViewport.Proj: TProjection;
begin
  Result := CalculateProjection;
end;

procedure TTestCastleViewport.TestAutoProjection;
var
  V: TMyViewport;
  B1, B2: TCastleBox;
  ProjectionNear: Single;
begin
  V := TMyViewport.Create(nil);
  try
    ProjectionNear := V.Proj.ProjectionNear;
    AssertEquals(DefaultCameraRadius * RadiusToProjectionNear, ProjectionNear);

    B1 := TCastleBox.Create(V);
    B1.Size := Vector3(10, 10, 10);
    V.Items.Add(B1);
    ProjectionNear := V.Proj.ProjectionNear;
    AssertEquals(DefaultCameraRadius * RadiusToProjectionNear, ProjectionNear);

    { At the very least, adding and enlarging B2 should not make ProjectionNear
      larger, as it would be surprising for user.
      Actually, now it's simpler: we just use constant
      DefaultCameraRadius * RadiusToProjectionNear always. }

    B2 := TCastleBox.Create(V);
    B2.Size := Vector3(10, 10, 10);
    V.Items.Add(B2);
    ProjectionNear := V.Proj.ProjectionNear;
    AssertEquals(DefaultCameraRadius * RadiusToProjectionNear, ProjectionNear);

    B2.Translation := Vector3(1000, 1000, 1000);
    ProjectionNear := V.Proj.ProjectionNear;
    AssertEquals(DefaultCameraRadius * RadiusToProjectionNear, ProjectionNear);
  finally FreeAndNil(V) end;
end;

procedure TTestCastleViewport.TestEffectiveBlendingSort;

  { Copy-pasted from CastleViewport.pas, to test it. }
  function EffectiveBlendingSort(const V: TCastleViewport): TShapeSortNoAuto;
  begin
    if V.BlendingSort = sortAuto then
    begin
      if (V.Camera <> nil) and
        (V.Camera.ProjectionType = ptOrthographic) and
        (TVector3.Equals(V.Camera.Direction, DefaultCameraDirection)) then
        Result := sort2D
      else
        Result := sort3D;
    end else
      Result := V.BlendingSort;
  end;

var
  V: TCastleViewport;
begin
  V := TCastleViewport.Create(nil);
  try
    AssertTrue(V.BlendingSort = sortAuto);
    AssertTrue(sort3D = EffectiveBlendingSort(V));

    V.Setup2D;

    // camera after Setup2D should cause sort2D detected, relied on e.g. by Escape, Unholy
    AssertTrue(V.BlendingSort = sortAuto);
    AssertTrue(sort2D = EffectiveBlendingSort(V));

    V.BlendingSort := sort3DGround;
    AssertTrue(sort3DGround = EffectiveBlendingSort(V));
  finally FreeAndNil(V) end;
end;

initialization
  RegisterTest(TTestCastleViewport);
end.
