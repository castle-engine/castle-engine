// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestWindow" -*-
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleWindow. }
unit TestCastleWindow;

interface

uses FpcUnit, TestUtils, TestRegistry, CastleTestCase;

type
  TTestWindow = class(TCastleTestCase)
  published
    procedure Test1;
    procedure TestNotifications;
    procedure TestMenu;
    { Test TCastleUserInterface.AutoSizeToChildren.
      In this unit, as it requires TCastleWindow (UI container) to make sense. }
    procedure TestAutoSizeToChildren;
    { Test TUIContainer.Focus.
      In this unit, as it requires TCastleWindow (UI container) to make sense. }
    procedure TestFocus;
  end;

implementation

uses SysUtils, Classes,
  CastleWindow, CastleControls, CastleStringUtils, CastleKeysMouse,
  CastleUIControls, CastleRectangles, CastleOnScreenMenu, CastleComponentSerialize,
  CastleInspectorControl, CastleCameras, CastleSceneManager, CastleVectors;

procedure TTestWindow.Test1;
var
  Window: TCastleWindowBase;
begin
  Window := TCastleWindowBase.Create(nil);
  try
    Window.Open;
    Window.Close;
  finally FreeAndNil(Window) end;
end;

procedure TTestWindow.TestNotifications;
var
  Window: TCastleWindowBase;
  C: TCastleButton;
begin
  Window := TCastleWindowBase.Create(nil);
  try
    C := TCastleButton.Create(Window);
    FreeAndNil(C);
  finally FreeAndNil(Window) end;
end;

procedure TTestWindow.TestMenu;
var
  M: TMenuItem;
begin
  M := TMenuItem.Create('blah', 123, 'a');
  AssertTrue(M.KeyMatches(keyA, 'a', []));
  { Below may be improved in the future, for now our KeyMatches is probably too forgiving.
    Below combination is not even usually possible, with mkCtrl you would get CtrlA
    character usually. }
  AssertTrue(M.KeyMatches(keyA, 'a', [mkCtrl]));
  FreeAndNil(M);

  M := TMenuItem.Create('blah', 123, keyF11);
  AssertTrue(M.KeyMatches(keyF11, #0, []));
  AssertTrue(M.KeyMatches(keyF11, '', [])); // string '' and #0 should be treated equal, for backward compat
  { below may be improved in the future, for now our KeyMatches is probably too forgiving }
  AssertTrue(M.KeyMatches(keyF11, #0, [mkCtrl]));
  AssertTrue(M.KeyMatches(keyF11, '', [mkCtrl])); // string '' and #0 should be treated equal, for backward compat
  FreeAndNil(M);

  M := TMenuItem.Create('blah', 123, keyF11);
  M.Modifiers := [mkCtrl];
  AssertTrue(not M.KeyMatches(keyF11, #0, []));
  AssertTrue(M.KeyMatches(keyF11, #0, [mkCtrl]));
  FreeAndNil(M);
end;

procedure TTestWindow.TestAutoSizeToChildren;
var
  Window: TCastleWindowBase;
  Parent, Child1, Child2: TCastleUserInterface;
begin
  Window := nil;
  Parent := nil;
  Child1 := nil;
  Child2 := nil;
  try
    Window := TCastleWindowBase.Create(nil);
    Window.Width := 500;
    Window.Height := 500;
    Window.ResizeAllowed := raNotAllowed;
    Window.Open;

    Parent := TCastleUserInterface.Create(nil);
    Window.Controls.InsertFront(Parent);

    Child1 := TCastleUserInterface.Create(nil);
    Child2 := TCastleUserInterface.Create(nil);

    AssertRectsEqual(FloatRectangle(0, 0, 100, 100), Parent.RenderRect);

    Parent.Anchor(hpLeft, 50);
    Parent.Anchor(vpTop, -60);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 100, 100, 100), Parent.RenderRect);

    Parent.AutoSizeToChildren := true;
    AssertRectsEqual(TFloatRectangle.Empty, Parent.RenderRect);

    Parent.InsertFront(Child1);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 100, 100, 100), Parent.RenderRect);

    Child1.Width := 40;
    Child1.Height := 30;
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 30, 40, 30), Parent.RenderRect);

    Child2.Width := 200;
    Child2.Height := 100;
    Child2.Anchor(vpTop, -10);
    Parent.InsertFront(Child2);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 110, 200, 110), Parent.RenderRect);

    Child1.Width := 10;
    Child1.Height := 10;
    Child1.Anchor(hpLeft, 200);
    Child2.Width := 10;
    Child2.Height := 10;
    Child2.Anchor(vpBottom, 400);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 410, 210, 410), Parent.RenderRect);

    Child1.Width := 80;
    Child1.Height := 30;
    Child1.Anchor(hpLeft, 30);
    Child1.Anchor(vpBottom, 40);
    Child2.Width := 80;
    Child2.Height := 30;
    Child2.Anchor(hpLeft, 40);
    Child2.Anchor(vpBottom, 60);
    AssertRectsEqual(FloatRectangle(50, 500 - 60 - 90, 120, 90), Parent.RenderRect);
  finally
    FreeAndNil(Window);
    FreeAndNil(Parent);
    FreeAndNil(Child1);
    FreeAndNil(Child2);
  end;
end;

procedure TTestWindow.TestFocus;
var
  Window: TCastleWindowBase;
  ManualButton, Button2: TCastleButton;
  OnScreenMenu1: TCastleOnScreenMenu;
  SceneManager1: TCastleSceneManager;

  { Based on examples/lazarus/model_3d_with_2d_controls/ }

  procedure AddUserInterfaceDesigned;
  var
    UiRoot: TCastleUserInterface;
    UiOwner: TComponent;
  begin
    UiOwner := TComponent.Create(Window);
    UiRoot := UserInterfaceLoad('castle-data:/focus_test/main.castle-user-interface', UiOwner);
    Window.Controls.InsertFront(UiRoot);
    Button2 := UiOwner.FindRequiredComponent('Button2') as TCastleButton;
    SceneManager1 := UiOwner.FindRequiredComponent('SceneManager1') as TCastleSceneManager;
    SceneManager1.AssignDefaultNavigation; // adds TCastleWalkNavigation
  end;

  procedure AddUserInterfaceFromCode;
  begin
    ManualButton := TCastleButton.Create(Window);
    ManualButton.Caption := 'Button added from code';
    ManualButton.Anchor(hpRight, -10);
    ManualButton.Anchor(vpBottom, 120);
    Window.Controls.InsertFront(ManualButton);

    OnScreenMenu1 := TCastleOnScreenMenu.Create(Window);
    OnScreenMenu1.Bottom := 140;
    OnScreenMenu1.Left := 400;
    OnScreenMenu1.Add('one');
    OnScreenMenu1.Add('two');
    OnScreenMenu1.Add('three');
    Window.Controls.InsertFront(OnScreenMenu1);

    Window.Controls.InsertFront(TCastleInspectorControl.Create(Window));
  end;

  procedure MoveMouse(const Pos: TVector2);
  // var
  //   C: TCastleUserInterface;
  begin
    Window.InternalFakeMotion(InputMotion(Window.MousePosition, Pos, [], 0));
    Window.Container.UpdateFocusAndMouseCursor;
    (* Useful to test current Focus value:
    Writeln('Focus now ', Window.Container.Focus.Count);
    for C in Window.Container.Focus do
      Writeln('  ', C.Name, ':', C.ClassName);
    *)
  end;

begin
  Window := TCastleWindowBase.Create(nil);
  try
    Window.Width := 800;
    Window.Height := 800;
    Window.ResizeAllowed := raNotAllowed;
    Window.Open;

    AddUserInterfaceDesigned;
    AddUserInterfaceFromCode;

    MoveMouse(FloatRectangle(Window.Rect).Middle);
    AssertEquals(6, Window.Container.Focus.Count);
    AssertTrue(Window.Container.Focus[0].Name = 'Group1');
    AssertTrue(Window.Container.Focus[1].Name = 'SceneManager1');
    AssertTrue(Window.Container.Focus[2] is TCastleWalkNavigation); // internal in SceneManager1
    AssertTrue(Window.Container.Focus[3] is TCastleInspectorControl);
    AssertTrue(Window.Container.Focus[4] is TCastleRectangleControl); // internal in TCastleInspectorControl
    AssertTrue(Window.Container.Focus[5] is TCastleLabel); // internal in TCastleInspectorControl

    MoveMouse(ManualButton.RenderRect.Middle);
    AssertEquals(6, Window.Container.Focus.Count);
    AssertTrue(Window.Container.Focus[0].Name = 'Group1');
    AssertTrue(Window.Container.Focus[1].Name = 'SceneManager1');
    AssertTrue(Window.Container.Focus[2] is TCastleWalkNavigation); // internal in SceneManager1
    AssertTrue(Window.Container.Focus[3] = ManualButton);
    AssertTrue(Window.Container.Focus[4] is TCastleInspectorControl);
    AssertTrue(Window.Container.Focus[5] is TCastleRectangleControl); // internal in TCastleInspectorControl

    MoveMouse(Button2.RenderRect.Middle);
    AssertEquals(6, Window.Container.Focus.Count);
    AssertTrue(Window.Container.Focus[0].Name = 'Group1');
    AssertTrue(Window.Container.Focus[1].Name = 'SceneManager1');
    AssertTrue(Window.Container.Focus[2] is TCastleWalkNavigation); // internal in SceneManager1
    AssertTrue(Window.Container.Focus[3] = Button2);
    AssertTrue(Window.Container.Focus[4] is TCastleInspectorControl);
    AssertTrue(Window.Container.Focus[5] is TCastleRectangleControl); // internal in TCastleInspectorControl
  finally FreeAndNil(Window) end;
end;

initialization
  RegisterTest(TTestWindow);
end.
