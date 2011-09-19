{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Key and mouse types and constants.

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

unit KeysMouse;

{ This was started as a spin-off from GLWindow unit.
  Was extracted from GLWindow unit to a separate unit,
  that @italic(doesn't depend on GLWindow unit).

  Advantage: we can keep this unit independent from OpenGL.
  This means that also Cameras unit (that must use unit KeysMouse)
  can stay independent from OpenGL. Which is good, makes
  Cameras unit more generally-usable.

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

  TCharactersBooleans = array [Char] of Boolean;
  PCharactersBooleans = ^TCharactersBooleans;

  TMouseButton = (mbLeft, mbMiddle, mbRight);
  TMouseButtons = set of TMouseButton;

  { Look of the mouse cursor.
    Used for various properties:
    TUIControl.Cursor, T3D.Cursor, TGLWindow.Cursor.

    mcDefault, mcNone, mcCustom have somewhat special meanings.
    The rest are some cursor images will well-defined meanings for the user,
    their exact look may depend on current window manager theme etc.  }
  TMouseCursor = (
    { Leave cursor as default, decided by a window manager. }
    mcDefault,
    { Make cursor invisible. }
    mcNone,
    { Use a custom cursor image in TGLWindow.CustomCursor.

      In normal circumstances, this should not be used for
      TUIControl.Cursor, T3D.Cursor and others, as they have no way
      to set TGLWindow.CustomCursor. }
    mcCustom,
    { Standard arrow, indicates, well, that user can point / click something. }
    mcStandard,
    { Indicates the program is busy and user should wait. }
    mcWait,
    { Text cursor, indicates that there's text under the cursor,
      which usually means that it can be selected,
      or that user can click to set focus to the text area. }
    mcText,
    { Indicates something active is under cursor, usually for links. }
    mcHand);

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
  K_Plus = 19;

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

  MouseButtonStr: array [TMouseButton] of string = ('left', 'middle', 'right');

function KeyToStr(key: TKey): string;

type
  { Modifier keys are keys that, when pressed, modify the meaning of
    other keys. Of course, this is actually just a convention.
    The actual interpretation is left up to the final program
    -- there you have to decide when and how modifiers affect the
    meaning of other keys. }
  TModifierKey = (mkCtrl, mkShift, mkAlt);
  TModifierKeys = set of TModifierKey;

  { Tracking the "pressed" state of keys. Allows you to query is key (TKey)
    pressed, and is some character (Char type) pressed. }
  TKeysPressed = class
  private
    { Characters are updated on the basis that given
      TKey corresponds to character. PressedKeyToCharacter and
      PressedCharacterToKey arrays
      store 1-1 mapping between pressed keys and pressed characters.
      So at any given point, we consider that given character corresponds
      to only one key. So Characters may get fooled by user in some
      complicated cases, that's acceptable (since unavoidable,
      see Characters comments). If a new key corresponding to already
      pressed character is pressed, this new key replaces previous key
      in the mapping.

      PressedKeyToCharacter reverse each other, that is
        PressedCharacterToKey[PressedKeyToCharacter[Key]] = Key and
        PressedKeyToCharacter[PressedCharacterToKey[C]] = C
      for all keys and characters, assuming that
      PressedCharacterToKey[C] <> K_None and
      PressedKeyToCharacter[Key] <> #0 (which indicate that no character
      is pressed / no character is pressed corresponding to this key).

      Storing correspondence means that if each KeyDown is paired by
      KeyUp, then each pressed Character will also be released.
      (Since each pressed Character *always* has a corresponding key
      that activated it.)

      PressedXxx arrays may seem complicated, but their programming
      is trivial and they allow me to update Characters array
      quickly and reliably. }
    PressedKeyToCharacter: array [TKey] of Char;
    PressedCharacterToKey: array [Char] of TKey;

    function GetItems(const Key: TKey): boolean;
  public
    { Check is a key (TKey) pressed.

      This array is read-only from outside of this class!
      Always Keys[K_None] = false. }
    Keys: TKeysBooleans;

    { Check is a character pressed.

      This array is read-only from outside of this class!
      Always Characters[#0] = false.

      Note that since a given character may be generated by various
      key combinations, this doesn't work as reliably as @link(Keys) array.
      For example, consider pressing a, then shift, then releasing a
      --- for a short time character 'A', and not 'a', should be pressed.

      Although we do our best (have some mapping tables to track characters),
      and in practice this works Ok. But still checking for keys
      on @link(Keys) array, when possible, is advised. }
    Characters: TCharactersBooleans;

    { Check is a key (TKey) pressed.

      Returns the same values as are in the @link(Keys) table.
      Although this is more comfortable: it's a default property of this class,
      so you can write simply @code(KeysPressed[K_X]) instead of
      @code(KeysPressed.Keys[K_X]). }
    property Items [Key: TKey]: boolean read GetItems; default;

    { Check which modifier keys are pressed.
      The result it based on current Keys[K_Ctrl], Keys[K_Shift] etc. values. }
    function Modifiers: TModifierKeys;

    { Call when key is pressed.
      Pass TKey, and corresponding character (Char).

      Pass Key = K_None if this is not
      representable as TKey, pass CharKey = #0 if this is not representable
      as char. But never pass both Key = K_None and CharKey = #0
      (this would have no meaning). }
    procedure KeyDown(const Key: TKey; const CharKey: char);

    { Call when key is released.
      Never pass Key = K_None here.

      It returns which character was released as a consequence of this
      key release. }
    procedure KeyUp(const Key: TKey; out CharKey: char);

    { Mark all keys as released. That is, this sets all @link(Keys) and
      @link(Characters) items to @false. Also resets internal arrays
      used to track @link(Characters) from KeyDown and KeyUp. }
    procedure Clear;
  end;

const
  ModifierKeyToKey: array[TModifierKey]of TKey = (K_Ctrl, K_Shift, K_Alt);

{ @abstract(This "packs" values like KeysDown[K_Ctrl], KeysDown[K_Shift] etc.
  -- KeysDown for all TModifierKey.)

  Version with TKeysPressed parameter returns [] (empty set)
  when argument is @nil. This may be useful sometimes.

  @groupBegin }
function ModifiersDown(const KeysDown: TKeysBooleans): TModifierKeys; overload;
function ModifiersDown(Pressed: TKeysPressed): TModifierKeys; overload;
{ @groupEnd }

function ModifierKeysToNiceStr(const MK: TModifierKeys): string;

type
  TMouseWheelDirection = (mwNone, mwUp, mwDown, mwLeft, mwRight);

const
  MouseWheelDirectionStr: array [TMouseWheelDirection] of string =
  ('none', 'up', 'down', 'left', 'right');

{ Determine simple mouse wheel direction from a Scroll and Vertical
  parameters received from TGLWindow.OnMouseWheel.
  Assumes that Scroll <> 0, like TGLWindow.OnMouseWheel guarantees. }
function MouseWheelDirection(const Scroll: Single; const Vertical: boolean): TMouseWheelDirection;

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
  K_Plus : Result := 'Plus';
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

function ModifiersDown(Pressed: TKeysPressed): TModifierKeys;
begin
 if Pressed <> nil then result := ModifiersDown(Pressed.Keys) else result:=[];
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

function MouseWheelDirection(const Scroll: Single; const Vertical: boolean): TMouseWheelDirection;
begin
  if Scroll > 0 then
  begin
    if Vertical then Result := mwUp else Result := mwLeft;
  end else
    if Vertical then Result := mwDown else Result := mwRight;
end;

{ TKeysPressed --------------------------------------------------------------- }

function TKeysPressed.GetItems(const Key: TKey): boolean;
begin
  Result := Keys[Key];
end;

function TKeysPressed.Modifiers: TModifierKeys;
begin
  Result := ModifiersDown(Keys);
end;

procedure TKeysPressed.KeyDown(const Key: TKey; const CharKey: char);
begin
  if Key <> K_None then
    Keys[Key] := true;

  if (Key <> K_None) and
     (CharKey <> #0) and
     (PressedKeyToCharacter[Key] = #0) then
  begin
    { update Characters and PressedXxx mapping arrays }
    if PressedCharacterToKey[CharKey] = K_None then
    begin
      Assert(not Characters[CharKey]);
      Characters[CharKey] := true;
    end else
    begin
      { some key already recorded as generating this character }
      Assert(Characters[CharKey]);
      Assert(PressedKeyToCharacter[PressedCharacterToKey[CharKey]] = CharKey);

      PressedKeyToCharacter[PressedCharacterToKey[CharKey]] := #0;
      PressedCharacterToKey[CharKey] := K_None;
    end;

    PressedKeyToCharacter[Key] := CharKey;
    PressedCharacterToKey[CharKey] := Key;
  end;

end;

procedure TKeysPressed.KeyUp(const Key: TKey; out CharKey: char);
begin
  CharKey := PressedKeyToCharacter[Key];
  if CharKey <> #0 then
  begin
    { update Characters and PressedXxx mapping arrays }
    Assert(Characters[CharKey]);
    Characters[CharKey] := false;
    PressedCharacterToKey[CharKey] := K_None;
    PressedKeyToCharacter[Key] := #0;
  end;

  Keys[key] := false;
end;

procedure TKeysPressed.Clear;
begin
  FillChar(Keys, SizeOf(Keys), 0);
  FillChar(Characters, SizeOf(Characters), 0);
  FillChar(PressedKeyToCharacter, SizeOf(PressedKeyToCharacter), 0);
  FillChar(PressedCharacterToKey, SizeOf(PressedCharacterToKey), 0);
end;

end.
