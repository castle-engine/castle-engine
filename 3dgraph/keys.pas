{
  Copyright 2001-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dgraph Pascal units".

  "Kambi's 3dgraph Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dgraph Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dgraph Pascal units"; if not, write to the Free Software
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

  @noAutoLinkHere
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

uses KambiUtils;

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

  K_BackSpace = Ord(CharBackSpace);
  K_Tab = Ord(CharTab);
  K_Enter = Ord(CharEnter);

  K_Shift = 16;
  K_Ctrl = 17;
  K_Alt = 18;

  K_Escape = Ord(CharEscape);
  K_Space = Ord(' ');
  K_PgUp = 33;
  K_PgDown = 34;
  K_End = 35;
  K_Home = 36;
  K_Left = 37;
  K_Up = 38;
  K_Right = 39;
  K_Down = 40;
  K_Insert = 45;
  K_Delete = 46;

  K_Plus  = 107;
  K_Minus = 109;

  K_0 = Ord('0');
  K_1 = Ord('1');
  K_2 = Ord('2');
  K_3 = Ord('3');
  K_4 = Ord('4');
  K_5 = Ord('5');
  K_6 = Ord('6');
  K_7 = Ord('7');
  K_8 = Ord('8');
  K_9 = Ord('9');

  K_A = Ord('A');
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
  K_Z = Ord('Z');

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
  K_None : result := 'None';
  K_BackSpace : result := 'BackSpace';
  K_Tab : result := 'Tab';
  K_Enter : result := 'Enter';
  K_Shift : result := 'Shift';
  K_Ctrl : result := 'Ctrl';
  K_Alt : result := 'Alt';
  K_Escape : result := 'Escape';
  K_Space : result := 'Space';
  K_PgUp : result := 'PgUp';
  K_PgDown : result := 'PgDown';
  K_End : result := 'End';
  K_Home : result := 'Home';
  K_Left : result := 'Left';
  K_Up : result := 'Up';
  K_Right : result := 'Right';
  K_Down : result := 'Down';
  K_Insert : result := 'Insert';
  K_Delete : result := 'Delete';
  K_Plus : result := 'Plus';
  K_Minus : result := 'Minus';
  K_Comma : result := 'Comma';
  K_Period : result := 'Period';

  K_0 .. K_9 : result := IntToStr(key-K_0);
  K_A .. K_Z : result := Chr( Ord('A')+key-K_A );
  K_F1 .. K_F12 : result := 'F'+IntToStr(key-K_F1+1);

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
