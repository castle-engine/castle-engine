// -*- compile-command: "./compile_console.sh && ./test_castle_game_engine --suite=TTestWindow" -*-
{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleWindow;

interface

uses fpcunit, testutils, testregistry, CastleTestCase;

type
  TTestWindow = class(TCastleTestCase)
  published
    procedure Test1;
    procedure TestNotifications;
    procedure TestMenu;
    { Test TCastleUserInterface.AutoSizeToChildren.
      Placed here, as it requires TCastleWindow (UI container) to make sense. }
    procedure TestAutoSizeToChildren;
  end;

implementation

uses SysUtils,
  CastleWindow, CastleControls, CastleStringUtils, CastleKeysMouse,
  CastleUIControls, CastleRectangles;

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
  AssertTrue(M.KeyMatches(K_A, 'a', []));
  { Below may be improved in the future, for now our KeyMatches is probably too forgiving.
    Below combination is not even usually possible, with mkCtrl you would get CtrlA
    character usually. }
  AssertTrue(M.KeyMatches(K_A, 'a', [mkCtrl]));
  FreeAndNil(M);

  M := TMenuItem.Create('blah', 123, K_F11);
  AssertTrue(M.KeyMatches(K_F11, #0, []));
  AssertTrue(M.KeyMatches(K_F11, '', [])); // string '' and #0 should be treated equal, for backward compat
  { below may be improved in the future, for now our KeyMatches is probably too forgiving }
  AssertTrue(M.KeyMatches(K_F11, #0, [mkCtrl]));
  AssertTrue(M.KeyMatches(K_F11, '', [mkCtrl])); // string '' and #0 should be treated equal, for backward compat
  FreeAndNil(M);

  M := TMenuItem.Create('blah', 123, K_F11);
  M.Modifiers := [mkCtrl];
  AssertTrue(not M.KeyMatches(K_F11, #0, []));
  AssertTrue(M.KeyMatches(K_F11, #0, [mkCtrl]));
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

initialization
  RegisterTest(TTestWindow);
end.
