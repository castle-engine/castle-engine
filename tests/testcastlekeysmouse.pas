{
  Copyright 2012-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestCastleKeysMouse;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestKeysMouse = class(TTestCase)
  published
    procedure TestKey;
    procedure TestKeyToStrAndBack;
    procedure TestCharToNiceStr;
  end;

implementation

uses CastleKeysMouse, CastleStringUtils;

procedure TTestKeysMouse.TestKey;
begin
  AssertTrue(Ord(K_Reserved_28) = 28);
  AssertTrue(Ord(K_Reserved_139) = 139);
  AssertTrue(Ord(K_Reserved_186) = 186);
  AssertTrue(Ord(K_Reserved_191) = 191);

  AssertTrue(Ord(K_None) = 0);
  AssertTrue(Ord(K_BackSpace) = Ord(CharBackSpace));
  AssertTrue(Ord(K_Tab) = Ord(CharTab));
  AssertTrue(Ord(K_Enter) = Ord(CharEnter));
  AssertTrue(Ord(K_Escape) = Ord(CharEscape));
  AssertTrue(Ord(K_Space) = Ord(' '));

  AssertTrue(Ord(K_0) = Ord('0'));
  AssertTrue(Ord(K_1) = Ord(K_0) + 1);
  AssertTrue(Ord(K_2) = Ord(K_0) + 2);
  AssertTrue(Ord(K_3) = Ord(K_0) + 3);
  AssertTrue(Ord(K_4) = Ord(K_0) + 4);
  AssertTrue(Ord(K_5) = Ord(K_0) + 5);
  AssertTrue(Ord(K_6) = Ord(K_0) + 6);
  AssertTrue(Ord(K_7) = Ord(K_0) + 7);
  AssertTrue(Ord(K_8) = Ord(K_0) + 8);
  AssertTrue(Ord(K_9) = Ord(K_0) + 9);

  AssertTrue(Ord(K_A) = Ord('A'));
  AssertTrue(Ord(K_Z) = Ord('Z'));

  AssertTrue(Ord(K_Numpad_1) = Ord(K_Numpad_0) + 1);
  AssertTrue(Ord(K_Numpad_2) = Ord(K_Numpad_0) + 2);
  AssertTrue(Ord(K_Numpad_3) = Ord(K_Numpad_0) + 3);
  AssertTrue(Ord(K_Numpad_4) = Ord(K_Numpad_0) + 4);
  AssertTrue(Ord(K_Numpad_5) = Ord(K_Numpad_0) + 5);
  AssertTrue(Ord(K_Numpad_6) = Ord(K_Numpad_0) + 6);
  AssertTrue(Ord(K_Numpad_7) = Ord(K_Numpad_0) + 7);
  AssertTrue(Ord(K_Numpad_8) = Ord(K_Numpad_0) + 8);
  AssertTrue(Ord(K_Numpad_9) = Ord(K_Numpad_0) + 9);

  AssertTrue(Ord(K_F2) = Ord(K_F1) + 1);
  AssertTrue(Ord(K_F3) = Ord(K_F1) + 2);
  AssertTrue(Ord(K_F4) = Ord(K_F1) + 3);
  AssertTrue(Ord(K_F5) = Ord(K_F1) + 4);
  AssertTrue(Ord(K_F6) = Ord(K_F1) + 5);
  AssertTrue(Ord(K_F7) = Ord(K_F1) + 6);
  AssertTrue(Ord(K_F8) = Ord(K_F1) + 7);
  AssertTrue(Ord(K_F9) = Ord(K_F1) + 8);
  AssertTrue(Ord(K_F10) = Ord(K_F1) + 9);
  AssertTrue(Ord(K_F11) = Ord(K_F1) + 10);
  AssertTrue(Ord(K_F12) = Ord(K_F1) + 11);
end;

procedure TTestKeysMouse.TestKeyToStrAndBack;
var
  K: TKey;
begin
  for K := Low(K) to High(K) do
  begin
    AssertTrue(StrToKey(KeyToStr(K), K_Reserved_28) = K);
    { no whitespace around }
    AssertTrue(Trim(KeyToStr(K)) =  KeyToStr(K));
  end;
end;

procedure TTestKeysMouse.TestCharToNiceStr;
begin
  AssertTrue(CharToNiceStr('a') = 'A');
  AssertTrue(CharToNiceStr('A') = 'Shift+A');
  AssertTrue(CharToNiceStr(CtrlA) = 'Ctrl+A');
  AssertTrue(CharToNiceStr(CtrlA, [mkCtrl]) = 'Ctrl+A');
  AssertTrue(CharToNiceStr(CtrlA, [mkCtrl, mkShift]) = 'Shift+Ctrl+A');
  AssertTrue(CharToNiceStr(CtrlA, [mkCtrl], false, true) = 'Command+A');
  AssertTrue(CharToNiceStr(CtrlA, [mkCtrl, mkShift], false, true) = 'Shift+Command+A');
  AssertTrue(KeyToStr(K_F11, []) = 'F11');
  AssertTrue(KeyToStr(K_F11, [mkCtrl]) = 'Ctrl+F11');
  AssertTrue(KeyToStr(K_F11, [mkCtrl], true) = 'Command+F11');
end;

initialization
  RegisterTest(TTestKeysMouse);
end.
