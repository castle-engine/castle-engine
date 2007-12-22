{
  Copyright 2001-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Key constants used in GLWindow unit.

  Some properties of K_Xxx constants that are guaranteed:

  K_None is a very special key value, see it's description for more.
  It's guaranteed that K_None constant is always equal to zero.

  Constants K_A .. K_Z are guaranteed to be always equal to
  TKey('A') .. TKey('Z') and constants K_0 .. K_9 are
  guaranteed to be always equal to TKey('0') .. TKey('9').
  Also K_F1 .. K_F12 (function keys) are guaranteed to be always nicely ordered
  (i.e. K_F2 = K_F1 + 1, K_F3 = K_F2 + 1 and so on).
  Also K_Escape, K_BackSpace, K_Tab, K_Enter are guaranteed to be always equal
  to CharEscape, CharBackSpace, CharTab, CharEnter (well, typecasted to
  TKey type).

  No other thing is guaranteed, so you shouldn't assume too much about
  constants K_Xxx when writing your code.
  Also, you should try to not assume that TKey size is 1 byte.
  Also, try to be prepared that maybe one day TKey type
  will be changed to be an enumerated type (not a simple Byte).
}

unit Keys;

{ Design notes:
  Although this unit is made for cooperation with GLWindow unit,
  it @italic(doesn't depend on GLWindow unit).

  Advantage: we can keep this unit in @code(3dgraph) group,
  i.e. units that don't depend on OpenGL. This means that also
  unit MatrixNavigation (that must use unit Keys) can stay in this group.
  This is good, because it makes important unit MatrixNavigation more
  generally-usable.

  Disadvantage: because this unit doesn't depend on GLWindow unit,
  it doesn't know what implementation of GLWindow unit
  (Xlib, glut, gtk, WinAPI -- see GLWINDOW_xxx defines in GLWindow unit) is used.
  It also can't use platform-specific units like Xlib, or WinAPI.
  On one hand, this is good (because this unit is simple and portable),
  on the other hand, we can't implement in this unit any conversion
  from WinAPI / XLib / Gtk key codes -> to K_Xxx constants.
  Such conversion has to be done in GLWindow unit implementation.
}

interface

uses KambiUtils, KambiStringUtils;

type
  { }
  TKey = Byte;

  TKeysBooleans = array[TKey]of Boolean;
  PKeysBooleans = ^TKeysBooleans;
  TKeysBytes = array[TKey]of Byte;
  PKeysBytes = ^TKeysBytes;

const
  { K_None is a very special value of type TKey. It means "no key",
    and is generally useful in similar situations when "nil" value
    is useful for Pointer types: to indicate some special "invalid"
    or "nonexisting" value. E.g. instead of
      @longCode# Key: TKey; ValidKey: boolean; #
    you can use just
      @longCode# Key: TKey; #
    and say that "Key = K_None means that key is not a valid key".

    It's guaranteed that K_None constant is always equal to zero.
    It's not a nice programming practice, but you can depend on it. }
  K_None = 0;

  K_PrintScreen = 1;
  K_CapsLock = 2;
  K_ScrollLock = 3;
  K_NumLock = 4;
  K_Pause = 5;
  K_Apostrophe = 6;
  K_Semicolon = 7;
  K_BackSpace = Ord(CharBackSpace); //< = 8
  K_Tab = Ord(CharTab); //< = 9
  K_Slash = 10;
  K_BackQuote = 11;
  K_Minus = 12;
  K_Enter = Ord(CharEnter); //< = 13
  K_Equal = 14;
  K_BackSlash = 15;
  K_Shift = 16;
  K_Ctrl = 17;
  K_Alt = 18;

  K_Escape = Ord(CharEscape); //< = 27
  K_Space = Ord(' '); //< 32
  K_PageUp = 33;
  K_PageDown = 34;
  K_End = 35;
  K_Home = 36;
  K_Left = 37;
  K_Up = 38;
  K_Right = 39;
  K_Down = 40;
  K_Insert = 45;
  K_Delete = 46;

  K_0 = Ord('0'); //< = 48
  K_1 = Ord('1');
  K_2 = Ord('2');
  K_3 = Ord('3');
  K_4 = Ord('4');
  K_5 = Ord('5');
  K_6 = Ord('6');
  K_7 = Ord('7');
  K_8 = Ord('8');
  K_9 = Ord('9'); //< = 57

  K_A = Ord('A'); //< = 65
  K_B = Ord('B');
  K_C = Ord('C');
  K_D = Ord('D');
  K_E = Ord('E');
  K_F = Ord('F');
  K_G = Ord('G');
  K_H = Ord('H');
  K_I = Ord('I');
  K_J = Ord('J');
  K_K = Ord('K');
  K_L = Ord('L');
  K_M = Ord('M');
  K_N = Ord('N');
  K_O = Ord('O');
  K_P = Ord('P');
  K_Q = Ord('Q');
  K_R = Ord('R');
  K_S = Ord('S');
  K_T = Ord('T');
  K_U = Ord('U');
  K_V = Ord('V');
  K_W = Ord('W');
  K_X = Ord('X');
  K_Y = Ord('Y');
  K_Z = Ord('Z'); //< = 90

  K_LeftBracket = 91;
  K_RightBracket = 93;

  K_Numpad_Plus  = 107;
  K_Numpad_Minus = 109;

  K_F1 = 112;
  K_F2 = 113;
  K_F3 = 114;
  K_F4 = 115;
  K_F5 = 116;
  K_F6 = 117;
  K_F7 = 118;
  K_F8 = 119;
  K_F9 = 120;
  K_F10 = 121;
  K_F11 = 122;
  K_F12 = 123;

  K_Numpad_0 = 140;
  K_Numpad_1 = 141;
  K_Numpad_2 = 142;
  K_Numpad_3 = 143;
  K_Numpad_4 = 144;
  K_Numpad_5 = 145;
  K_Numpad_6 = 146;
  K_Numpad_7 = 147;
  K_Numpad_8 = 148;
  K_Numpad_9 = 149;
  K_Numpad_End = 150;
  K_Numpad_Down = 151;
  K_Numpad_PageDown = 152;
  K_Numpad_Left = 153;
  K_Numpad_Begin = 154;
  K_Numpad_Right = 155;
  K_Numpad_Home = 156;
  K_Numpad_Up = 157;
  K_Numpad_PageUp = 158;
  K_Numpad_Insert = 159;
  K_Numpad_Delete = 160;
  K_Numpad_Enter = 161;
  K_Numpad_Multiply = 162;
  K_Numpad_Divide = 163;

  K_Comma = 188;
  K_Period = 190;

function KeyToStr(key: TKey): string;

{ ---------------------------------------------------------------------------- }
{ @section(Key modifiers) }

type
  { Modifier keys are keys that, when pressed, modify the meaning of
    other keys. Of course, this is actually just a convention.
    The actual interpretation is left up to the final program
    -- there you have to decide when and how modifiers affect the
    meaning of other keys. }
  TModifierKey = (mkCtrl, mkShift, mkAlt);
  TModifierKeys = set of TModifierKey;

const
  ModifierKeyToKey: array[TModifierKey]of TKey = (K_Ctrl, K_Shift, K_Alt);

{ @abstract(This "packs" values like KeysDown[K_Ctrl], KeysDown[K_Shift] etc.
  -- KeysDown for all TModifierKey.)

  Version with "PKeysBooleans" parameter returns [] (empty set)
  when KeysDown = nil. This may be useful sometimes.

  @groupBegin }
function ModifiersDown(const KeysDown: TKeysBooleans): TModifierKeys; overload;
function ModifiersDown(KeysDown: PKeysBooleans): TModifierKeys; overload;
{ @groupEnd }

function ModifierKeysToNiceStr(const MK: TModifierKeys): string;

implementation

uses SysUtils;

function KeyToStr(key: TKey): string;
begin
 case key of
  K_None: result := 'None';
  K_PrintScreen: Result := 'Print Screen';
  K_CapsLock: Result := 'Caps Lock';
  K_ScrollLock: Result := 'Scroll Lock';
  K_NumLock: Result := 'Num Lock';
  K_Pause: Result := 'Pause';
  K_BackSpace: result := 'BackSpace';
  K_Tab: result := 'Tab';
  K_Enter: result := 'Enter';
  K_Shift: result := 'Shift';
  K_Ctrl: Result := 'Ctrl';
  K_Alt: Result := 'Alt';
  K_Escape: Result := 'Escape';
  K_Space: Result := 'Space';
  K_PageUp: Result := 'Page Up';
  K_PageDown: Result := 'Page Down';
  K_End: Result := 'End';
  K_Home: Result := 'Home';
  K_Left: Result := 'Left';
  K_Up: Result := 'Up';
  K_Right: Result := 'Right';
  K_Down: Result := 'Down';
  K_Insert: Result := 'Insert';
  K_Delete: Result := 'Delete';
  K_LeftBracket: Result := '[';
  K_RightBracket: Result := ']';
  K_Numpad_Plus: Result := 'Numpad Plus';
  K_Numpad_Minus: Result := 'Numpad Minus';
  K_Comma: Result := 'Comma';
  K_Period: Result := 'Period';
  K_Numpad_0: Result := 'Numpad 0';
  K_Numpad_1: Result := 'Numpad 1';
  K_Numpad_2: Result := 'Numpad 2';
  K_Numpad_3: Result := 'Numpad 3';
  K_Numpad_4: Result := 'Numpad 4';
  K_Numpad_5: Result := 'Numpad 5';
  K_Numpad_6: Result := 'Numpad 6';
  K_Numpad_7: Result := 'Numpad 7';
  K_Numpad_8: Result := 'Numpad 8';
  K_Numpad_9: Result := 'Numpad 9';
  K_Numpad_End: Result := 'Numpad End';
  K_Numpad_Down: Result := 'Numpad Down';
  K_Numpad_PageDown: Result := 'Numpad Page Down';
  K_Numpad_Left: Result := 'Numpad Left';
  K_Numpad_Begin: Result := 'Numpad Begin';
  K_Numpad_Right: Result := 'Numpad Right';
  K_Numpad_Home: Result := 'Numpad Home';
  K_Numpad_Up: Result := 'Numpad Up';
  K_Numpad_PageUp: Result := 'Numpad Page Up';
  K_Numpad_Insert: Result := 'Numpad Insert';
  K_Numpad_Delete: Result := 'Numpad Delete';
  K_Numpad_Enter: Result := 'Numpad Enter';
  K_Numpad_Multiply: Result := 'Numpad Multiply';
  K_Numpad_Divide: Result := 'Numpad Divide';
  K_Apostrophe: Result := 'Apostrophe';
  K_Semicolon: Result := 'Semicolon';
  K_Slash: Result := 'Slash';
  K_BackQuote: Result := 'Backquote';
  K_Minus: Result := 'Minus';
  K_Equal: Result := 'Equal';
  K_BackSlash: Result := 'Backslash';

  K_0 .. K_9: Result := IntToStr(key-K_0);
  K_A .. K_Z: Result := Chr( Ord('A')+key-K_A );
  K_F1 .. K_F12: Result := 'F'+IntToStr(key-K_F1+1);

  else result := '<unknown key>';
 end;
end;

function ModifiersDown(const KeysDown: TKeysBooleans): TModifierKeys;
var mk: TModifierKey;
begin
 result:=[];
 for mk := Low(TModifierKey) to High(TModifierKey) do
  if KeysDown[ModifierKeyToKey[mk]] then Include(result, mk);
end;

function ModifiersDown(KeysDown: PKeysBooleans): TModifierKeys;
begin
 if KeysDown <> nil then result := ModifiersDown(KeysDown^) else result:=[];
end;

function ModifierKeysToNiceStr(const MK: TModifierKeys): string;
var k: TModifierKey;
begin
 Result := '[';
 for k := Low(k) to High(k) do
  if k in MK then
   Result += KeyToStr(ModifierKeyToKey[k]) + ',';
 { we know that Length(result) >= 1 (because Result starts with '[')
   so it's safe to check Result[Length(result)]. }
 if Result[Length(result)] = ',' then SetLength(result, Length(result)-1);
 Result += ']';
end;

end.
