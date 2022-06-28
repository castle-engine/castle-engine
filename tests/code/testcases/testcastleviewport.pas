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

uses CastleComponentSerialize, CastleUIControls;

procedure TTestCastleViewport.TestReadingOldDesigns;
var
  RootOwner: TComponent;
  Root: TCastleUserInterface;
begin
  RootOwner := TComponent.Create(nil);
  Root := UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/multiple_viewports.castle-user-interface', RootOwner);
  try
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  Root := UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_2d.castle-user-interface', RootOwner);
  try
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  Root := UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_3d.castle-user-interface', RootOwner);
  try
  finally FreeAndNil(RootOwner) end;

  RootOwner := TComponent.Create(nil);
  Root := UserInterfaceLoad('castle-data:/designs_before_camera_as_transform/template_3d_viewer.castle-user-interface', RootOwner);
  try
  finally FreeAndNil(RootOwner) end;
end;

initialization
  RegisterTest(TTestCastleViewport);
end.
