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
  end;

implementation

uses CastleComponentSerialize, CastleUIControls, CastleViewport, CastleCameras;

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
  Root: TCastleUserInterface;
begin
  RootOwner := TComponent.Create(nil);
  Root := UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/multiple_viewports.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('ViewportTransparent') as TCastleViewport, 'Camera');
    // when reading at runtime, we don't rename it to Camera1 etc. unless we have to, at CastleComponentSerialize
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('ViewportExamine') as TCastleViewport, 'Camera1');
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('ViewportScreenEffect') as TCastleViewport, 'Camera2');
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('ViewportNormal') as TCastleViewport, 'Camera3');
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  Root := UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_2d.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('MainViewport') as TCastleViewport, 'Camera');
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  Root := UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_3d.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('MainViewport') as TCastleViewport, 'Camera');
    AssertTrue((RootOwner.FindRequiredComponent('WalkNavigation') as TCastleNavigation).Parent =
      RootOwner.FindRequiredComponent('MainViewport'));
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  Root := UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_3d_viewer.castle-user-interface', RootOwner);
  try
    AssertCameraUpgraded(RootOwner.FindRequiredComponent('Viewport') as TCastleViewport, 'Camera');
    AssertTrue((RootOwner.FindRequiredComponent('ExamineNavigation1') as TCastleNavigation).Parent =
      RootOwner.FindRequiredComponent('Viewport'));
  finally FreeAndNil(RootOwner) end;
end;

initialization
  RegisterTest(TTestCastleViewport);
end.
