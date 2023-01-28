// -*- compile-command: "./test_single_testcase.sh TTestCastleUIControls" -*-

{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test of CastleUIControls unit. }
unit TestCastleUIControls;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase,{$else}CastleTester,{$endif} CastleUIControls;

type
  TTestCastleUIControls = class(TCastleTestCase)
  published
    procedure TestRectEffective;
    procedure TestRecursiveSize;
    procedure TestForIn;
  end;

implementation

uses CastleRectangles, CastleVectors, CastleUtils;

procedure TTestCastleUIControls.TestRectEffective;
var
  N: TCastleUserInterface;
  F: TCastleUserInterface;
begin
  N := TCastleUserInterface.Create(nil);
  try
    AssertEquals(100, N.EffectiveWidth);
    AssertEquals(100, N.EffectiveHeight);

    N.Width := 0;
    N.Height := 0;
    AssertEquals(0, N.EffectiveWidth);
    AssertEquals(0, N.EffectiveHeight);
  finally FreeAndNil(N); end;

  F := TCastleUserInterface.Create(nil);
  try
    F.Width := 300;
    F.Height := 400;
    AssertEquals(300, F.EffectiveWidth);
    AssertEquals(400, F.EffectiveHeight);
  finally FreeAndNil(F); end;
end;

type
  TParentAdjustsToChildren = class(TCastleUserInterface)
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  end;

procedure TParentAdjustsToChildren.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  I: Integer;
  C: TCastleUserInterface;
begin
  for I := 0 to ControlsCount - 1 do
  begin
    C := Controls[I];
    MaxVar(PreferredWidth, C.EffectiveWidth * UIScale);
    MaxVar(PreferredHeight, C.EffectiveHeight * UIScale);
  end;
end;

procedure TTestCastleUIControls.TestRecursiveSize;
var
  UiParent: TParentAdjustsToChildren;
  UiChild: TCastleUserInterface;
begin
  UiParent := TParentAdjustsToChildren.Create(nil);
  UiChild := TCastleUserInterface.Create(nil);
  try
    UiParent.InsertFront(UiChild);

    // check that calculating rects doesn't cause infinite loop
    UiParent.RenderRect;
    UiChild.RenderRect;

    UiChild.FullSize := true;

    // check that calculating rects doesn't cause infinite loop
    UiParent.RenderRect;
    UiChild.RenderRect;
  finally
    FreeAndNil(UiParent);
    FreeAndNil(UiChild);
  end;
end;

procedure TTestCastleUIControls.TestForIn;
var
  Owner: TComponent;
  T, T1, C: TCastleUserInterface;
  Y: Single;
begin
  Owner := TComponent.Create(nil);
  try
    T := TCastleUserInterface.Create(Owner);
    T.AnchorDelta := Vector2(0, 0);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(1, 0);
    T.InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(2, 0);
    T.InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(3, 0);
    T.InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(1, 1);
    T.Controls[0].InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(1, 2);
    T.Controls[0].InsertFront(T1);

    T1 := TCastleUserInterface.Create(Owner);
    T1.AnchorDelta := Vector2(1, 3);
    T.Controls[0].InsertFront(T1);

    Y := 1;
    for C in T do
    begin
      AssertVectorEquals(C.AnchorDelta, Vector2(Y, 0));
      Y := Y + 1;
    end;
    AssertSameValue(Y, 4);
  finally FreeAndNil(Owner) end;
end;

initialization
  RegisterTest(TTestCastleUIControls);
end.
