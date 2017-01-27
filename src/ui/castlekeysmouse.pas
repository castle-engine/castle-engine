{
  Copyright 2001-2017 Michalis Kamburelis, Tomasz Wojtyś.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Types and constants to handle keys and mouse.
  They are used throughout our engine, both by CastleControl (Lazarus component)
  and by non-Lazarus CastleWindow. }
unit CastleKeysMouse;

{$I castleconf.inc}

interface

uses CastleUtils, CastleStringUtils, CastleVectors, CastleXMLConfig;

type
  { Keys on keyboard.
    Do not ever use values K_Reserved_Xxx (they are declared here only to avoid
    using assignments, which would prevent FPC from allowing TKey to index arrays).

    Some properties of K_Xxx constants that are guaranteed:

    @unorderedList(
      @item(K_None means "no key". It's guaranteed that it's always equal to zero.)

      @item(Letters (constants K_A .. K_Z) are guaranteed to be always equal to
        TKey('A') .. TKey('Z') and digits (constants K_0 .. K_9) are
        guaranteed to be always equal to TKey('0') .. TKey('9').
        That is, their ordinal values are equal to their ASCII codes,
        and they are always ordered.

        Also K_F1 .. K_F12 (function keys) are guaranteed to be always nicely ordered
        (i.e. K_F2 = K_F1 + 1, K_F3 = K_F2 + 1 and so on).

        Also K_Escape, K_BackSpace, K_Tab, K_Enter are guaranteed to be always equal
        to CharEscape, CharBackSpace, CharTab, CharEnter (well, typecasted to
        TKey type).)
    ) }
  TKey = (
    K_None,
    K_PrintScreen,
    K_CapsLock,
    K_ScrollLock,
    K_NumLock,
    K_Pause,
    K_Apostrophe,
    K_Semicolon,
    K_BackSpace, //< = Ord(CharBackSpace) = 8
    K_Tab, //< = Ord(CharTab) = 9
    K_Slash,
    K_BackQuote,
    K_Minus,
    K_Enter, //< = Ord(CharEnter) = 13
    K_Equal,
    K_BackSlash,
    K_Shift,
    K_Ctrl,
    K_Alt,
    K_Plus,
    K_Reserved_20,
    K_Reserved_21,
    K_Reserved_22,
    K_Reserved_23,
    K_Reserved_24,
    K_Reserved_25,
    K_Reserved_26,
    K_Escape, //< = Ord(CharEscape) = 27
    K_Reserved_28,
    K_Reserved_29,
    K_Reserved_30,
    K_Reserved_31,
    K_Space, //< = Ord(' ') = 32
    K_PageUp,
    K_PageDown,
    K_End,
    K_Home,
    K_Left,
    K_Up,
    K_Right,
    K_Down,
    K_Reserved_41,
    K_Reserved_42,
    K_Reserved_43,
    K_Reserved_44,
    K_Insert,
    K_Delete,
    K_Reserved_47,
    K_0, //< = Ord('0') = 48
    K_1, //< = Ord('1')
    K_2, //< = Ord('2')
    K_3, //< = Ord('3')
    K_4, //< = Ord('4')
    K_5, //< = Ord('5')
    K_6, //< = Ord('6')
    K_7, //< = Ord('7')
    K_8, //< = Ord('8')
    K_9, //< = Ord('9') = 57
    K_Reserved_58,
    K_Reserved_59,
    K_Reserved_60,
    K_Reserved_61,
    K_Reserved_62,
    K_Reserved_63,
    K_Reserved_64,
    K_A, //< = Ord('A') = 65
    K_B, //< = Ord('B')
    K_C, //< = Ord('C')
    K_D, //< = Ord('D')
    K_E, //< = Ord('E')
    K_F, //< = Ord('F')
    K_G, //< = Ord('G')
    K_H, //< = Ord('H')
    K_I, //< = Ord('I')
    K_J, //< = Ord('J')
    K_K, //< = Ord('K')
    K_L, //< = Ord('L')
    K_M, //< = Ord('M')
    K_N, //< = Ord('N')
    K_O, //< = Ord('O')
    K_P, //< = Ord('P')
    K_Q, //< = Ord('Q')
    K_R, //< = Ord('R')
    K_S, //< = Ord('S')
    K_T, //< = Ord('T')
    K_U, //< = Ord('U')
    K_V, //< = Ord('V')
    K_W, //< = Ord('W')
    K_X, //< = Ord('X')
    K_Y, //< = Ord('Y')
    K_Z, //< = Ord('Z') = 90
    K_LeftBracket,
    K_Reserved_92,
    K_RightBracket,
    K_Reserved_94,
    K_Reserved_95,
    K_Reserved_96,
    K_Reserved_97,
    K_Reserved_98,
    K_Reserved_99,
    K_Reserved_100,
    K_Reserved_101,
    K_Reserved_102,
    K_Reserved_103,
    K_Reserved_104,
    K_Reserved_105,
    K_Reserved_106,
    K_Numpad_Plus ,
    K_Reserved_108,
    K_Numpad_Minus,
    K_Reserved_110,
    K_Reserved_111,
    K_F1,
    K_F2,
    K_F3,
    K_F4,
    K_F5,
    K_F6,
    K_F7,
    K_F8,
    K_F9,
    K_F10,
    K_F11,
    K_F12,
    K_Reserved_124,
    K_Reserved_125,
    K_Reserved_126,
    K_Reserved_127,
    K_Reserved_128,
    K_Reserved_129,
    K_Reserved_130,
    K_Reserved_131,
    K_Reserved_132,
    K_Reserved_133,
    K_Reserved_134,
    K_Reserved_135,
    K_Reserved_136,
    K_Reserved_137,
    K_Reserved_138,
    K_Reserved_139,
    K_Numpad_0,
    K_Numpad_1,
    K_Numpad_2,
    K_Numpad_3,
    K_Numpad_4,
    K_Numpad_5,
    K_Numpad_6,
    K_Numpad_7,
    K_Numpad_8,
    K_Numpad_9,
    K_Numpad_End,
    K_Numpad_Down,
    K_Numpad_PageDown,
    K_Numpad_Left,
    K_Numpad_Begin,
    K_Numpad_Right,
    K_Numpad_Home,
    K_Numpad_Up,
    K_Numpad_PageUp,
    K_Numpad_Insert,
    K_Numpad_Delete,
    K_Numpad_Enter,
    K_Numpad_Multiply,
    K_Numpad_Divide,
    K_Reserved_164,
    K_Reserved_165,
    K_Reserved_166,
    K_Reserved_167,
    K_Reserved_168,
    K_Reserved_169,
    K_Reserved_170,
    K_Reserved_171,
    K_Reserved_172,
    K_Reserved_173,
    K_Reserved_174,
    K_Reserved_175,
    K_Reserved_176,
    K_Reserved_177,
    K_Reserved_178,
    K_Reserved_179,
    K_Reserved_180,
    K_Reserved_181,
    K_Reserved_182,
    K_Reserved_183,
    K_Reserved_184,
    K_Reserved_185,
    K_Reserved_186,
    K_Reserved_187,
    K_Comma,
    K_Reserved_189,
    K_Period,
    K_Reserved_191
  );

  TKeysBooleans = array [TKey] of Boolean;
  PKeysBooleans = ^TKeysBooleans;
  TKeysBytes = array [Byte] of TKey;
  PKeysBytes = ^TKeysBytes;

  TCharactersBooleans = array [Char] of Boolean;
  PCharactersBooleans = ^TCharactersBooleans;

  TMouseButton = (mbLeft, mbMiddle, mbRight, mbExtra1, mbExtra2);
  TMouseButtons = set of TMouseButton;

  { Look of the mouse cursor.
    Used for various properties:
    TUIControl.Cursor, T3D.Cursor, TCastleWindowCustom.Cursor.

    mcDefault, mcNone, mcForceNone, mcCustom have somewhat special meanings.
    The rest are some cursor images will well-defined meanings for the user,
    their exact look may depend on current window manager theme etc.  }
  TMouseCursor = (
    { Leave cursor as default, decided by a window manager. }
    mcDefault,
    { Make cursor invisible. }
    mcNone,
    { Forcefully make cursor invisible.

      If *any* UI control under the cursor
      says that the cursor is mcForceNone, it will be invisible.
      This is in contrast to mcNone, that only hides the cursor if
      the currently focused control (under the mouse cursor) sets it. }
    mcForceNone,
    { Use a custom cursor image in TCastleWindowCustom.CustomCursor.

      In normal circumstances, this should not be used for
      TUIControl.Cursor, T3D.Cursor and others, as they have no way
      to set TCastleWindowCustom.CustomCursor. }
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
  MouseButtonStr: array [TMouseButton] of string = ('left', 'middle', 'right',
                                                    'extra1', 'extra2');

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

function KeyToStr(const Key: TKey; const Modifiers: TModifierKeys = [];
  const CtrlIsCommand: boolean = false): string;

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

{ Return a nice very short description of the character.
  When Modifiers is not empty, these are the additional modifiers
  required to be pressed (although some C values, like CtrlA ... CtrlZ,
  may already indicate some modifier).

  For normal readable characters just returns them, for special
  characters returns short string like "Ctrl+C" or "Escape".

  The returned string doesn't contain any quotes around, doesn't
  contain any word merely stating "character" (for example argument 'c' just
  generates 'c', not 'character "c"').

  BackSpaceTabEnterString determines behavior on three special values:
  #8, #9, #13. These may be either described as Backspace/Tab/Enter
  (if BackSpaceTabEnterString = true)
  or as Ctrl+H, Ctrl+I, Ctrl+M (if BackSpaceTabEnterString = false). }
function CharToNiceStr(const C: char; const Modifiers: TModifierKeys = [];
  const BackSpaceTabEnterString: boolean = true;
  const CtrlIsCommand: boolean = false): string;

type
  TMouseWheelDirection = (mwNone, mwUp, mwDown, mwLeft, mwRight);

const
  MouseWheelDirectionStr: array [TMouseWheelDirection] of string =
  ('none', 'up', 'down', 'left', 'right');

{ Determine simple mouse wheel direction from a Scroll and Vertical
  parameters received from TCastleWindowCustom.OnMouseWheel.
  Assumes that Scroll <> 0, like TCastleWindowCustom.OnMouseWheel guarantees. }
function MouseWheelDirection(const Scroll: Single; const Vertical: boolean): TMouseWheelDirection;

{ Convert string value back to a key name, reversing KeyToStr.
  If string does not contain any recognized key name, return DefaultKey. }
function StrToKey(const S: string; const DefaultKey: TKey): TKey;

type
  TInputPressReleaseType = (itKey, itMouseButton, itMouseWheel);

  TFingerIndex = Cardinal;

  { Input press or release event.
    Either key press/release or mouse button press/release or
    mouse wheel action.
    This is nicely matching with TInputShortcut processing in CastleInputs,
    so it allows to easily store and check for user actions. }
  TInputPressRelease = object
    EventType: TInputPressReleaseType;

    { When EventType is itKey, this is the key pressed or released.
      Either Key <> K_None or KeyCharacter <> #0 in this case.
      When EventType <> itKey, then Key = K_None and KeyCharacter = #0.

      Both Key and KeyCharacter represent the same action. Sometimes one,
      sometimes the other is useful.

      @bold(Not all key presses can be represented as TKey value.)
      For example, pressing '(' (opening parenthesis), which is done on most
      keyboards by pressing shift + zero, does not have any TKey value.
      So it will generate event with Key = K_None, but KeyCharacter = '('.

      @bold(Likewise, not all key presses can be represented as char value.)
      For example "up arrow" (Key = K_Up) doesn't have a char code
      (it will have KeyCharacter = #0).

      KeyCharacter is influenced by some other keys state,
      like Shift or Ctrl or CapsLock or some key to input localized characters
      (all dependent on your system settings, we don't deal with it in our engine,
      we merely take what system gives us). For example, you can get "a" or "A"
      depending of Shift and CapsLock state, or CtrlA if you hold Ctrl.

      When the user holds the key pressed, we will get consecutive
      key down events. Under some OSes, you will also get consecutive
      key up events, but it's not guaranteed (on some OSes, you may
      simply get only consecutive key down). So the more precise
      definition when key down occurs is: it's a notification that
      the key is (still) pressed down.
      @groupBegin }
    Key: TKey;
    KeyCharacter: char;
    { @groupEnd }

    { When EventType is itMouseButton, this is the mouse button pressed or released.
      Always mbLeft for touch device press/release events.

      CastleWindow notes (but relevant also to other interfaces, like Lazarus
      component, although in that case it's beyond our control):
      When user presses the mouse over
      our control, mouse is automatically captured, so all further OnMotion
      following mouse release will be passed to this control (even if user moves mouse
      outside of this control), until user releases all mouse buttons.
      Note that this means that mouse positions may be outside
      of [0..Width - 1, 0..Height - 1] range. }
    MouseButton: TMouseButton;

    { When EventType is itMouseButton, this is the finger index pressed or
      released on a touch device. Always 0 for normal mouse events. }
    FingerIndex: TFingerIndex;

    { The position of the current mouse/finger on the window,
      for EventType = itMouseButton (in case of mouse press/release).

      For normal backends that simply support a single mouse device,
      this is just equivalent to TCastleWindow.MousePosition
      and TCastleControl.MousePosition, so it's not really interesting.

      For multi-touch devices, this is very useful, as it describes
      the position of the current finger (corresponding to FingerIndex).

      For other EventType values (not itMouseButton),
      this is the position of main mouse/finger.
      See TCastleWindow.MousePosition documentation for what it means,
      in particular what happens on touch devices. }
    Position: TVector2Single;

    { When EventType is itMouseWheel, this is the mouse wheel action.
      MouseWheel is mwNone if and only if EventType <> itMouseWheel.

      Positive value of Scroll means user scrolled up or left,
      negative means user scrolled down or right. It is never zero
      (as long as EventType = itMouseWheel of course).

      Scroll units are such that 1.0 should be treated like a "one operation",
      like a one click. On most normal mouses only an integer scroll will be
      possible to make. On the other hand, on touchpads it's common to be able
      to scroll by flexible amounts.

      CastleWindow backends notes:
      GTK and Xlib cannot generate Scroll values different than 1 or -1.

      @groupBegin }
    MouseWheelScroll: Single;
    MouseWheelVertical: boolean;
    function MouseWheel: TMouseWheelDirection;
    { @groupEnd }

    { Check is event type correct, and then check if event Key or KeyCharacter
      matches. Always false for AKey = K_None or AKeyCharacter = #0.
      @groupBegin }
    function IsKey(const AKey: TKey): boolean;
    function IsKey(const AKeyCharacter: char): boolean;
    { @groupEnd }
    function IsMouseButton(const AMouseButton: TMouseButton): boolean;
    function IsMouseWheel(const AMouseWheel: TMouseWheelDirection): boolean;

    { Textual description of this event. }
    function ToString: string;
    { @deprecated Deprecated name for ToString. }
    function Description: string; deprecated;
  end;

  { Motion (movement) of mouse or a finger on a touch device. }
  TInputMotion = object
    OldPosition, Position: TVector2Single;
    Pressed: TMouseButtons;
    FingerIndex: TFingerIndex;
  end;

{ Construct TInputPressRelease corresponding to given event.
  @groupBegin }
function InputKey(const Position: TVector2Single;
  const Key: TKey; const KeyCharacter: Char): TInputPressRelease;
function InputMouseButton(const Position: TVector2Single;
  const MouseButton: TMouseButton; const FingerIndex: TFingerIndex): TInputPressRelease;
function InputMouseWheel(const Position: TVector2Single;
  const Scroll: Single; const Vertical: boolean): TInputPressRelease;
{ @groupEnd }

{ Construct TInputMotion. }
function InputMotion(const OldPosition, Position: TVector2Single;
  const Pressed: TMouseButtons; const FingerIndex: TFingerIndex): TInputMotion;

type
  TCastleConfigKeysMouseHelper = class helper for TCastleConfig
    { Reading/writing key values to config file.
      Key names are expected to follow StrToKey and KeyToStr functions in CastleKeysMouse.

      @groupBegin }
    function GetKey(const APath: string;
      const ADefaultValue: TKey): TKey; overload;
    procedure SetKey(const APath: string;
      const AValue: TKey); overload;
    procedure SetDeleteKey(const APath: string;
      const AValue, ADefaultValue: TKey); overload;
    { @groupEnd }
  end;

implementation

uses SysUtils;

const
  KeyToStrTable: array [TKey] of string = (
  'None',
  'Print Screen',
  'Caps Lock',
  'Scroll Lock',
  'Num Lock',
  'Pause',
  'Apostrophe',
  'Semicolon',
  'BackSpace',
  'Tab',
  'Slash',
  'BackQuote',
  'Minus',
  'Enter',
  'Equal',
  'BackSlash',
  'Shift',
  'Ctrl',
  'Alt',
  'Plus',
  'Reserved_20',
  'Reserved_21',
  'Reserved_22',
  'Reserved_23',
  'Reserved_24',
  'Reserved_25',
  'Reserved_26',
  'Escape',
  'Reserved_28',
  'Reserved_29',
  'Reserved_30',
  'Reserved_31',
  'Space',
  'Page Up',
  'Page Down',
  'End',
  'Home',
  'Left',
  'Up',
  'Right',
  'Down',
  'Reserved_41',
  'Reserved_42',
  'Reserved_43',
  'Reserved_44',
  'Insert',
  'Delete',
  'Reserved_47',
  '0',
  '1',
  '2',
  '3',
  '4',
  '5',
  '6',
  '7',
  '8',
  '9',
  'Reserved_58',
  'Reserved_59',
  'Reserved_60',
  'Reserved_61',
  'Reserved_62',
  'Reserved_63',
  'Reserved_64',
  'A',
  'B',
  'C',
  'D',
  'E',
  'F',
  'G',
  'H',
  'I',
  'J',
  'K',
  'L',
  'M',
  'N',
  'O',
  'P',
  'Q',
  'R',
  'S',
  'T',
  'U',
  'V',
  'W',
  'X',
  'Y',
  'Z',
  '[',
  'Reserved_92',
  ']',
  'Reserved_94',
  'Reserved_95',
  'Reserved_96',
  'Reserved_97',
  'Reserved_98',
  'Reserved_99',
  'Reserved_100',
  'Reserved_101',
  'Reserved_102',
  'Reserved_103',
  'Reserved_104',
  'Reserved_105',
  'Reserved_106',
  'Numpad Plus',
  'Reserved_108',
  'Numpad Minus',
  'Reserved_110',
  'Reserved_111',
  'F1',
  'F2',
  'F3',
  'F4',
  'F5',
  'F6',
  'F7',
  'F8',
  'F9',
  'F10',
  'F11',
  'F12',
  'Reserved_124',
  'Reserved_125',
  'Reserved_126',
  'Reserved_127',
  'Reserved_128',
  'Reserved_129',
  'Reserved_130',
  'Reserved_131',
  'Reserved_132',
  'Reserved_133',
  'Reserved_134',
  'Reserved_135',
  'Reserved_136',
  'Reserved_137',
  'Reserved_138',
  'Reserved_139',
  'Numpad 0',
  'Numpad 1',
  'Numpad 2',
  'Numpad 3',
  'Numpad 4',
  'Numpad 5',
  'Numpad 6',
  'Numpad 7',
  'Numpad 8',
  'Numpad 9',
  'Numpad End',
  'Numpad Down',
  'Numpad PageDown',
  'Numpad Left',
  'Numpad Begin',
  'Numpad Right',
  'Numpad Home',
  'Numpad Up',
  'Numpad PageUp',
  'Numpad Insert',
  'Numpad Delete',
  'Numpad Enter',
  'Numpad Multiply',
  'Numpad Divide',
  'Reserved_164',
  'Reserved_165',
  'Reserved_166',
  'Reserved_167',
  'Reserved_168',
  'Reserved_169',
  'Reserved_170',
  'Reserved_171',
  'Reserved_172',
  'Reserved_173',
  'Reserved_174',
  'Reserved_175',
  'Reserved_176',
  'Reserved_177',
  'Reserved_178',
  'Reserved_179',
  'Reserved_180',
  'Reserved_181',
  'Reserved_182',
  'Reserved_183',
  'Reserved_184',
  'Reserved_185',
  'Reserved_186',
  'Reserved_187',
  'Comma',
  'Reserved_189',
  'Period',
  'Reserved_191'
  );

function KeyToStr(const Key: TKey; const Modifiers: TModifierKeys;
  const CtrlIsCommand: boolean): string;
begin
  { early exit, key K_None means "no key", Modifiers are ignored }
  if Key = K_None then Exit(KeyToStrTable[Key]);

  Result := '';

  { add modifiers description }
  if mkShift in Modifiers then
    Result += 'Shift+';
  if mkAlt in Modifiers then
    Result += 'Alt+';
  if mkCtrl in Modifiers then
  begin
    if CtrlIsCommand then
      Result += 'Command+' else
      Result += 'Ctrl+';
  end;

  Result += KeyToStrTable[Key];
end;

function StrToKey(const S: string; const DefaultKey: TKey): TKey;
begin
  for Result := Low(Result) to High(Result) do
    if KeyToStrTable[Result] = S then
      Exit;
  Result := DefaultKey;
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

function CharToNiceStr(const C: char; const Modifiers: TModifierKeys;
  const BackSpaceTabEnterString, CtrlIsCommand: boolean): string;
var
  CharactersImplicatingCtrlModifier: TSetOfChars;
begin
  { early exit, character #0 means "no key", Modifiers are ignored }
  if C = #0 then Exit('#0');

  Result := '';

  CharactersImplicatingCtrlModifier := [CtrlA .. CtrlZ];
  if BackSpaceTabEnterString then
    { do not show Tab and similar chars as Ctrl+Tab }
    CharactersImplicatingCtrlModifier -=
      [CharBackSpace, CharTab, CharEnter];

  { add modifiers description }
  if (mkShift in Modifiers) or (C in ['A'..'Z']) then
    Result += 'Shift+';
  if mkAlt in Modifiers then
    Result += 'Alt+';
  if (mkCtrl in Modifiers) or
     (C in CharactersImplicatingCtrlModifier) then
  begin
    if CtrlIsCommand then
      Result += 'Command+' else
      Result += 'Ctrl+';
  end;

  if BackSpaceTabEnterString then
  begin
    case C of
      CharBackSpace: begin Result += 'BackSpace'; Exit; end;
      CharTab      : begin Result += 'Tab'      ; Exit; end;
      CharEnter    : begin Result += 'Enter'    ; Exit; end;
    end;
  end;

  case c of
    CharEscape: Result += 'Esc';
    ' ' : Result += 'Space';
    { Show lowercase letters as uppercase, this is standard for showing menu item shortcuts.
      Uppercase letters will be prefixed with Shift+. }
    'a' .. 'z': Result += Chr(Ord(C) - Ord('a') + Ord('A'));
    CtrlA .. CtrlZ: Result += Chr(Ord(C) - Ord(CtrlA) + Ord('A')); // we already added Ctrl+ prefix
    else Result += C;
  end;
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

{ TInputPressRelease --------------------------------------------------------- }

function TInputPressRelease.MouseWheel: TMouseWheelDirection;
begin
  if EventType = itMouseWheel then
    Result := MouseWheelDirection(MouseWheelScroll, MouseWheelVertical) else
    Result := mwNone;
end;

function TInputPressRelease.IsKey(const AKey: TKey): boolean;
begin
  Result := (AKey <> K_None) and (EventType = itKey) and (Key = AKey);
end;

function TInputPressRelease.IsKey(const AKeyCharacter: char): boolean;
begin
  Result := (AKeyCharacter <> #0) and (EventType = itKey) and (KeyCharacter = AKeyCharacter);
end;

function TInputPressRelease.IsMouseButton(const AMouseButton: TMouseButton): boolean;
begin
  Result := (EventType = itMouseButton) and (MouseButton = AMouseButton);
end;

function TInputPressRelease.IsMouseWheel(const AMouseWheel: TMouseWheelDirection): boolean;
begin
  Result := (EventType = itMouseWheel) and (MouseWheel = AMouseWheel);
end;

function TInputPressRelease.ToString: string;
begin
  case EventType of
    itKey: Result := Format('key %s, character %s (code %d)',
      [ KeyToStr(Key), CharToNiceStr(KeyCharacter), Ord(KeyCharacter)]);
    itMouseButton: Result := 'mouse ' + MouseButtonStr[MouseButton];
    itMouseWheel: Result := Format('mouse wheel %s (amount %f, vertical: %s)',
      [ MouseWheelDirectionStr[MouseWheel],
        MouseWheelScroll,
        BoolToStr(MouseWheelVertical, true) ]);
    else raise EInternalError.Create('TInputPressRelease.Description: EventType?');
  end;
end;

function TInputPressRelease.Description: string;
begin
  Result := ToString;
end;

function InputKey(const Position: TVector2Single;
  const Key: TKey; const KeyCharacter: Char): TInputPressRelease;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Position := Position;
  Result.EventType := itKey;
  Result.Key := Key;
  Result.KeyCharacter := KeyCharacter;
end;

function InputMouseButton(const Position: TVector2Single;
  const MouseButton: TMouseButton; const FingerIndex: TFingerIndex): TInputPressRelease;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Position := Position;
  Result.EventType := itMouseButton;
  Result.MouseButton := MouseButton;
  Result.FingerIndex := FingerIndex;
end;

function InputMouseWheel(const Position: TVector2Single;
  const Scroll: Single; const Vertical: boolean): TInputPressRelease;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Position := Position;
  Result.EventType := itMouseWheel;
  Result.MouseWheelScroll := Scroll;
  Result.MouseWheelVertical := Vertical;
end;

function InputMotion(const OldPosition, Position: TVector2Single;
  const Pressed: TMouseButtons; const FingerIndex: TFingerIndex): TInputMotion;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.OldPosition := OldPosition;
  Result.Position := Position;
  Result.Pressed := Pressed;
  Result.FingerIndex := FingerIndex;
end;

{ TCastleConfigKeysMouseHelper ----------------------------------------------- }

function TCastleConfigKeysMouseHelper.GetKey(const APath: string;
  const ADefaultValue: TKey): TKey;
begin
  Result := StrToKey(GetValue(APath, KeyToStr(ADefaultValue)), ADefaultValue);
end;

procedure TCastleConfigKeysMouseHelper.SetKey(const APath: string;
  const AValue: TKey);
begin
  SetValue(APath, KeyToStr(AValue));
end;

procedure TCastleConfigKeysMouseHelper.SetDeleteKey(const APath: string;
  const AValue, ADefaultValue: TKey);
begin
  SetDeleteValue(APath, KeyToStr(AValue), KeyToStr(ADefaultValue));
end;

end.
