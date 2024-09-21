// -*- compile-command: "./test_single_testcase.sh TTestKeysMouse" -*-
{
  Copyright 2012-2021 Michalis Kamburelis.

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
  Classes, SysUtils, CastleTester;

type
  TTestKeysMouse = class(TCastleTestCase)
  published
    procedure TestKey;
    procedure TestKeyToStrAndBack;
    procedure TestCharToNiceStr;
    procedure TestKeyStringToNiceStr;
  end;

implementation

uses CastleKeysMouse, CastleStringUtils;

procedure TTestKeysMouse.TestKey;
begin
  AssertTrue(Ord(keyReserved_28) = 28);
  AssertTrue(Ord(keyReserved_139) = 139);
  AssertTrue(Ord(keyReserved_186) = 186);
  AssertTrue(Ord(keyReserved_191) = 191);

  AssertTrue(Ord(keyNone) = 0);
  AssertTrue(Ord(keyBackSpace) = Ord(CharBackSpace));
  AssertTrue(Ord(keyTab) = Ord(CharTab));
  AssertTrue(Ord(keyEnter) = Ord(CharEnter));
  AssertTrue(Ord(keyEscape) = Ord(CharEscape));
  AssertTrue(Ord(keySpace) = Ord(' '));

  AssertTrue(Ord(key0) = Ord('0'));
  AssertTrue(Ord(key1) = Ord(key0) + 1);
  AssertTrue(Ord(key2) = Ord(key0) + 2);
  AssertTrue(Ord(key3) = Ord(key0) + 3);
  AssertTrue(Ord(key4) = Ord(key0) + 4);
  AssertTrue(Ord(key5) = Ord(key0) + 5);
  AssertTrue(Ord(key6) = Ord(key0) + 6);
  AssertTrue(Ord(key7) = Ord(key0) + 7);
  AssertTrue(Ord(key8) = Ord(key0) + 8);
  AssertTrue(Ord(key9) = Ord(key0) + 9);

  AssertTrue(Ord(keyA) = Ord('A'));
  AssertTrue(Ord(keyZ) = Ord('Z'));

  AssertTrue(Ord(keyNumpad1) = Ord(keyNumpad0) + 1);
  AssertTrue(Ord(keyNumpad2) = Ord(keyNumpad0) + 2);
  AssertTrue(Ord(keyNumpad3) = Ord(keyNumpad0) + 3);
  AssertTrue(Ord(keyNumpad4) = Ord(keyNumpad0) + 4);
  AssertTrue(Ord(keyNumpad5) = Ord(keyNumpad0) + 5);
  AssertTrue(Ord(keyNumpad6) = Ord(keyNumpad0) + 6);
  AssertTrue(Ord(keyNumpad7) = Ord(keyNumpad0) + 7);
  AssertTrue(Ord(keyNumpad8) = Ord(keyNumpad0) + 8);
  AssertTrue(Ord(keyNumpad9) = Ord(keyNumpad0) + 9);

  AssertTrue(Ord(keyF2) = Ord(keyF1) + 1);
  AssertTrue(Ord(keyF3) = Ord(keyF1) + 2);
  AssertTrue(Ord(keyF4) = Ord(keyF1) + 3);
  AssertTrue(Ord(keyF5) = Ord(keyF1) + 4);
  AssertTrue(Ord(keyF6) = Ord(keyF1) + 5);
  AssertTrue(Ord(keyF7) = Ord(keyF1) + 6);
  AssertTrue(Ord(keyF8) = Ord(keyF1) + 7);
  AssertTrue(Ord(keyF9) = Ord(keyF1) + 8);
  AssertTrue(Ord(keyF10) = Ord(keyF1) + 9);
  AssertTrue(Ord(keyF11) = Ord(keyF1) + 10);
  AssertTrue(Ord(keyF12) = Ord(keyF1) + 11);
end;

procedure TTestKeysMouse.TestKeyToStrAndBack;
var
  K: TKey;
begin
  for K := Low(K) to High(K) do
  begin
    AssertTrue(StrToKey(KeyToStr(K), keyReserved_28) = K);
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
  AssertTrue(KeyToStr(keyF11, []) = 'F11');
  AssertTrue(KeyToStr(keyF11, [mkCtrl]) = 'Ctrl+F11');
  AssertTrue(KeyToStr(keyF11, [mkCtrl], true) = 'Command+F11');
end;

procedure TTestKeysMouse.TestKeyStringToNiceStr;
begin
  AssertTrue(KeyStringToNiceStr('') = 'none');
  AssertTrue(KeyStringToNiceStr('aa') = 'aa'); // UTF-8 char, untouched by KeyStringToNiceStr
  AssertTrue(KeyStringToNiceStr('a') = 'A');
  AssertTrue(KeyStringToNiceStr('A') = 'Shift+A');
  AssertTrue(KeyStringToNiceStr(CtrlA) = 'Ctrl+A');
  AssertTrue(KeyStringToNiceStr(CtrlA, [mkCtrl]) = 'Ctrl+A');
  AssertTrue(KeyStringToNiceStr(CtrlA, [mkCtrl, mkShift]) = 'Shift+Ctrl+A');
  AssertTrue(KeyStringToNiceStr(CtrlA, [mkCtrl], false, true) = 'Command+A');
  AssertTrue(KeyStringToNiceStr(CtrlA, [mkCtrl, mkShift], false, true) = 'Shift+Command+A');
  AssertTrue(KeyToStr(keyF11, []) = 'F11');
  AssertTrue(KeyToStr(keyF11, [mkCtrl]) = 'Ctrl+F11');
  AssertTrue(KeyToStr(keyF11, [mkCtrl], true) = 'Command+F11');
end;

initialization
  RegisterTest(TTestKeysMouse);
end.
