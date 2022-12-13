// -*- compile-command: "./test_single_testcase.sh TTestCastleDesignComponents" -*-
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

{ Test TCastleDesign and TCastleTransformDesign classes. }
unit TestCastleDesignComponents;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif}, CastleClassUtils;

type
  TTestCastleDesignComponents = class(TCastleTestCase)
  strict private
    FreeNotificationHappened: Boolean;
    procedure FreeNotificationFlag(const Sender: TFreeNotificationObserver);
  published
    procedure TestUiFreesAllWhenUrlsChanged;
    procedure TestTransformFreesAllWhenUrlsChanged;
  end;

implementation

uses CastleTransform, CastleUIControls, CastleControls, CastleVectors, CastleScene;

procedure TTestCastleDesignComponents.FreeNotificationFlag(const Sender: TFreeNotificationObserver);
begin
  FreeNotificationHappened := true;
end;

procedure TTestCastleDesignComponents.TestUiFreesAllWhenUrlsChanged;
var
  Own: TComponent;
  UiDes: TCastleDesign;
  Img: TCastleImageControl;
  ImgObserver: TFreeNotificationObserver;
begin
  Own := TComponent.Create(nil);
  try
    UiDes := TCastleDesign.Create(Own);
    UiDes.Url := 'castle-data:/designs/test_free_children_of_ui.castle-user-interface';
    AssertTrue(Own.FindComponent('ImageControl1') = nil);

    Img := UiDes.DesignedComponent('ImageControl1') as TCastleImageControl;
    AssertTrue(Img <> nil);
    // test deprecated methods that provide backward compat
    AssertTrue(Img = UiDes.FindComponent('ImageControl1'));
    AssertTrue(Img = UiDes.FindRequiredComponent('ImageControl1'));

    AssertVectorEquals(Vector4(1, 1, 1, 1), Img.Color);

    ImgObserver := TFreeNotificationObserver.Create(Own);
    ImgObserver.Observed := Img;
    ImgObserver.OnFreeNotification := {$ifdef FPC}@{$endif} FreeNotificationFlag;
    FreeNotificationHappened := false;

    UiDes.Url := 'castle-data:/designs/test_free_children_of_ui_2.castle-user-interface';

    { Changing Url should have freed all the previous loaded contents. }
    AssertTrue(FreeNotificationHappened);
    AssertTrue(ImgObserver.Observed = nil);

    { new design has ImageControl1 with different color }
    Img := UiDes.DesignedComponent('ImageControl1') as TCastleImageControl;
    AssertTrue(Img <> nil);
    // test deprecated methods that provide backward compat
    AssertTrue(Img = UiDes.FindComponent('ImageControl1'));
    AssertTrue(Img = UiDes.FindRequiredComponent('ImageControl1'));

    AssertVectorEquals(Vector4(0, 0, 1, 1), Img.Color);
  finally FreeAndNil(Own) end;
end;

procedure TTestCastleDesignComponents.TestTransformFreesAllWhenUrlsChanged;
var
  Own: TComponent;
  TrDes: TCastleTransformDesign;
  Cone: TCastleCone;
  ConeObserver: TFreeNotificationObserver;
begin
  Own := TComponent.Create(nil);
  try
    TrDes := TCastleTransformDesign.Create(Own);
    TrDes.Url := 'castle-data:/designs/test_free_children_of_transform.castle-transform';
    AssertTrue(Own.FindComponent('Cone1') = nil);

    Cone := TrDes.DesignedComponent('Cone1') as TCastleCone;
    AssertTrue(Cone <> nil);
    // test deprecated methods that provide backward compat
    AssertTrue(Cone = TrDes.FindComponent('Cone1'));
    AssertTrue(Cone = TrDes.FindRequiredComponent('Cone1'));

    AssertVectorEquals(Vector4(1, 1, 1, 1), Cone.Color);

    ConeObserver := TFreeNotificationObserver.Create(Own);
    ConeObserver.Observed := Cone;
    ConeObserver.OnFreeNotification := {$ifdef FPC}@{$endif} FreeNotificationFlag;
    FreeNotificationHappened := false;

    TrDes.Url := 'castle-data:/designs/test_free_children_of_transform_2.castle-transform';

    { Changing Url should have freed all the previous loaded contents. }
    AssertTrue(FreeNotificationHappened);
    AssertTrue(ConeObserver.Observed = nil);

    { new design has Cone1 with different color }
    Cone := TrDes.DesignedComponent('Cone1') as TCastleCone;
    AssertTrue(Cone <> nil);
    // test deprecated methods that provide backward compat
    AssertTrue(Cone = TrDes.FindComponent('Cone1'));
    AssertTrue(Cone = TrDes.FindRequiredComponent('Cone1'));

    AssertVectorEquals(Vector4(0, 0, 1, 1), Cone.Color);
  finally FreeAndNil(Own) end;
end;

initialization
  RegisterTest(TTestCastleDesignComponents);
end.
