{
  Copyright 2010-2017 Michalis Kamburelis.

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

uses fpcunit, testutils, testregistry;

type
  TTestWindow = class(TTestCase)
  published
    procedure Test1;
    procedure TestNotifications;
    procedure TestMenu;
  end;

implementation

uses SysUtils, CastleWindow, CastleControls, CastleStringUtils, CastleKeysMouse;

procedure TTestWindow.Test1;
var
  Window: TCastleWindowCustom;
begin
  Window := TCastleWindowCustom.Create(nil);
  try
    Window.Open;
    Window.Close;
  finally FreeAndNil(Window) end;
end;

procedure TTestWindow.TestNotifications;
var
  Window: TCastleWindowCustom;
  C: TCastleButton;
begin
  Window := TCastleWindowCustom.Create(nil);
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
  { below may be improved in the future, for now our KeyMatches is probably too forgiving }
  AssertTrue(M.KeyMatches(K_F11, #0, [mkCtrl]));
  FreeAndNil(M);

  M := TMenuItem.Create('blah', 123, K_F11);
  M.Modifiers := [mkCtrl];
  AssertTrue(not M.KeyMatches(K_F11, #0, []));
  AssertTrue(M.KeyMatches(K_F11, #0, [mkCtrl]));
  FreeAndNil(M);
end;

initialization
  RegisterTest(TTestWindow);
end.
