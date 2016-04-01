(*
 * Copyright (C) 2010 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
  *)

// Note: This translation does not limit itself to the NDK defined constants
// but instead all constants from android.view.KeyEvent can be added here
//
// The NDK files are located in usr/include/android/keycodes.h

{ @exclude Internal for the engine. }
unit CastleAndroidInternalKeycodes;

{$I castleconf.inc}

interface

uses ctypes;

(******************************************************************
 *
 * IMPORTANT NOTICE:
 *
 *   This file is part of Android's set of stable system headers
 *   exposed by the Android NDK (Native Development Kit).
 *
 *   Third-party source AND binary code relies on the definitions
 *   here to be FROZEN ON ALL UPCOMING PLATFORM RELEASES.
 *
 *   - DO NOT MODIFY ENUMS (EXCEPT IF YOU ADD NEW 32-BIT VALUES)
 *   - DO NOT MODIFY CONSTANTS OR FUNCTIONAL MACROS
 *   - DO NOT CHANGE THE SIGNATURE OF FUNCTIONS IN ANY WAY
 *   - DO NOT CHANGE THE LAYOUT OR SIZE OF STRUCTURES
  *)

(*
 * Key codes.
  *)

const
  // First NDK defined keycodes

  AKEYCODE_UNKNOWN = 0;
  AKEYCODE_SOFT_LEFT = 1;
  AKEYCODE_SOFT_RIGHT = 2;
  AKEYCODE_HOME = 3;
  AKEYCODE_BACK = 4;
  AKEYCODE_CALL = 5;
  AKEYCODE_ENDCALL = 6;
  AKEYCODE_0 = 7;
  AKEYCODE_1 = 8;
  AKEYCODE_2 = 9;
  AKEYCODE_3 = 10;
  AKEYCODE_4 = 11;
  AKEYCODE_5 = 12;
  AKEYCODE_6 = 13;
  AKEYCODE_7 = 14;
  AKEYCODE_8 = 15;
  AKEYCODE_9 = 16;
  AKEYCODE_STAR = 17;
  AKEYCODE_POUND = 18;
  AKEYCODE_DPAD_UP = 19;
  AKEYCODE_DPAD_DOWN = 20;
  AKEYCODE_DPAD_LEFT = 21;
  AKEYCODE_DPAD_RIGHT = 22;
  AKEYCODE_DPAD_CENTER = 23;
  AKEYCODE_VOLUME_UP = 24;
  AKEYCODE_VOLUME_DOWN = 25;
  AKEYCODE_POWER = 26;
  AKEYCODE_CAMERA = 27;
  AKEYCODE_CLEAR = 28;
  AKEYCODE_A = 29;
  AKEYCODE_B = 30;
  AKEYCODE_C = 31;
  AKEYCODE_D = 32;
  AKEYCODE_E = 33;
  AKEYCODE_F = 34;
  AKEYCODE_G = 35;
  AKEYCODE_H = 36;
  AKEYCODE_I = 37;
  AKEYCODE_J = 38;
  AKEYCODE_K = 39;
  AKEYCODE_L = 40;
  AKEYCODE_M = 41;
  AKEYCODE_N = 42;
  AKEYCODE_O = 43;
  AKEYCODE_P = 44;
  AKEYCODE_Q = 45;
  AKEYCODE_R = 46;
  AKEYCODE_S = 47;
  AKEYCODE_T = 48;
  AKEYCODE_U = 49;
  AKEYCODE_V = 50;
  AKEYCODE_W = 51;
  AKEYCODE_X = 52;
  AKEYCODE_Y = 53;
  AKEYCODE_Z = 54;
  AKEYCODE_COMMA = 55;
  AKEYCODE_PERIOD = 56;
  AKEYCODE_ALT_LEFT = 57;
  AKEYCODE_ALT_RIGHT = 58;
  AKEYCODE_SHIFT_LEFT = 59;
  AKEYCODE_SHIFT_RIGHT = 60;
  AKEYCODE_TAB = 61;
  AKEYCODE_SPACE = 62;
  AKEYCODE_SYM = 63;
  AKEYCODE_EXPLORER = 64;
  AKEYCODE_ENVELOPE = 65;
  AKEYCODE_ENTER = 66;
  AKEYCODE_DEL = 67;
  AKEYCODE_GRAVE = 68;
  AKEYCODE_MINUS = 69;
  AKEYCODE_EQUALS = 70;
  AKEYCODE_LEFT_BRACKET = 71;
  AKEYCODE_RIGHT_BRACKET = 72;
  AKEYCODE_BACKSLASH = 73;
  AKEYCODE_SEMICOLON = 74;
  AKEYCODE_APOSTROPHE = 75;
  AKEYCODE_SLASH = 76;
  AKEYCODE_AT = 77;
  AKEYCODE_NUM = 78;
  AKEYCODE_HEADSETHOOK = 79;
  AKEYCODE_FOCUS = 80;  // *Camera* focus
  AKEYCODE_PLUS = 81;
  AKEYCODE_MENU = 82;
  AKEYCODE_NOTIFICATION = 83;
  AKEYCODE_SEARCH = 84;
  AKEYCODE_MEDIA_PLAY_PAUSE = 85;
  AKEYCODE_MEDIA_STOP = 86;
  AKEYCODE_MEDIA_NEXT = 87;
  AKEYCODE_MEDIA_PREVIOUS = 88;
  AKEYCODE_MEDIA_REWIND = 89;
  AKEYCODE_MEDIA_FAST_FORWARD = 90;
  AKEYCODE_MUTE = 91;
  AKEYCODE_PAGE_UP = 92;
  AKEYCODE_PAGE_DOWN = 93;
  AKEYCODE_PICTSYMBOLS = 94;
  AKEYCODE_SWITCH_CHARSET = 95;
  AKEYCODE_BUTTON_A = 96;
  AKEYCODE_BUTTON_B = 97;
  AKEYCODE_BUTTON_C = 98;
  AKEYCODE_BUTTON_X = 99;
  AKEYCODE_BUTTON_Y = 100;
  AKEYCODE_BUTTON_Z = 101;
  AKEYCODE_BUTTON_L1 = 102;
  AKEYCODE_BUTTON_R1 = 103;
  AKEYCODE_BUTTON_L2 = 104;
  AKEYCODE_BUTTON_R2 = 105;
  AKEYCODE_BUTTON_THUMBL = 106;
  AKEYCODE_BUTTON_THUMBR = 107;
  AKEYCODE_BUTTON_START = 108;
  AKEYCODE_BUTTON_SELECT = 109;
  AKEYCODE_BUTTON_MODE = 110;

  // Now all elements from android.view.KeyEvent

  ACTION_DOWN = 0;
  ACTION_MULTIPLE = 2;
  ACTION_UP = 1;
  FLAG_CANCELED = $20;
  FLAG_CANCELED_LONG_PRESS = $100;
  FLAG_EDITOR_ACTION = $10;
  FLAG_FALLBACK = $400;
  FLAG_FROM_SYSTEM = 8;
  FLAG_KEEP_TOUCH_MODE = 4;
  FLAG_LONG_PRESS = $80;
  FLAG_SOFT_KEYBOARD = 2;
  FLAG_TRACKING = $200;
  FLAG_VIRTUAL_HARD_KEY = $40;
  FLAG_WOKE_HERE = 1;
  KEYCODE_0 = 7;
  KEYCODE_1 = 8;
  KEYCODE_2 = 9;
  KEYCODE_3 = 10;
  KEYCODE_3D_MODE = $000000ce; // 3D Mode key. Toggles the display between 2D and 3D mode.
  KEYCODE_4 = 11;
  KEYCODE_5 = 12;
  KEYCODE_6 = 13;
  KEYCODE_7 = 14;
  KEYCODE_8 = 15;
  KEYCODE_9 = 16;
  KEYCODE_A = 29;
  KEYCODE_ALT_LEFT = $00000039;
  KEYCODE_ALT_RIGHT = $0000003a;
  KEYCODE_APOSTROPHE = $0000004b;
  KEYCODE_APP_SWITCH = $000000bb;
  KEYCODE_AT = $0000004d;
  KEYCODE_AVR_INPUT = $000000b6;
  KEYCODE_AVR_POWER = $000000b5;
  KEYCODE_B = 30;
  KEYCODE_BACK = 4;
  KEYCODE_BACKSLASH = $00000049;
  KEYCODE_BOOKMARK = $000000ae;
  KEYCODE_BREAK = $00000079;
  KEYCODE_BUTTON_1 = $000000bc;
  KEYCODE_BUTTON_10 = $000000c5;
  KEYCODE_BUTTON_11 = $000000c6;
  KEYCODE_BUTTON_12 = $000000c7;
  KEYCODE_BUTTON_13 = $000000c8;
  KEYCODE_BUTTON_14 = $000000c9;
  KEYCODE_BUTTON_15 = $000000ca;
  KEYCODE_BUTTON_16 = $000000cb; // Generic Game Pad Button #16.
  KEYCODE_BUTTON_2 = $000000bd; // Generic Game Pad Button #2.
  KEYCODE_BUTTON_3 = $000000be;
  KEYCODE_BUTTON_4 = $000000bf;
  KEYCODE_BUTTON_5 = $000000c0;
  KEYCODE_BUTTON_6 = $000000c1;
  KEYCODE_BUTTON_7 = $000000c2;
  KEYCODE_BUTTON_8 = $000000c3;
  KEYCODE_BUTTON_9 = $000000c4; // Generic Game Pad Button #9.
  KEYCODE_BUTTON_A = $00000060; // A Button key. On a game controller, the A button should be either the button labeled A or the first button on the upper row of controller buttons.
  KEYCODE_BUTTON_B = $00000061;
  KEYCODE_BUTTON_C = $00000062;
  KEYCODE_BUTTON_L1 = $00000066; // L1 Button key. On a game controller, the L1 button should be either the button labeled L1 (or L) or the top left trigger button.
  KEYCODE_BUTTON_L2 = $00000068;
  KEYCODE_BUTTON_MODE = $0000006e; // Mode Button key. On a game controller, the button labeled Mode.
  KEYCODE_BUTTON_R1 = $00000067; // R1 Button key. On a game controller, the R1 button should be either the button labeled R1 (or R) or the top right trigger button.
  KEYCODE_BUTTON_R2 = $00000069; // R2 Button key. On a game controller, the R2 button should be either the button labeled R2 or the bottom right trigger button.
  KEYCODE_BUTTON_SELECT = $0000006d; // Select Button key. On a game controller, the button labeled Select.
  KEYCODE_BUTTON_START = $0000006c; // Start Button key. On a game controller, the button labeled Start.
  KEYCODE_BUTTON_THUMBL = $0000006a; // Left Thumb Button key. On a game controller, the left thumb button indicates that the left (or only) joystick is pressed.
  KEYCODE_BUTTON_THUMBR = $0000006b; // Right Thumb Button key. On a game controller, the right thumb button indicates that the right joystick is pressed.
  KEYCODE_BUTTON_X = $00000063; // X Button key. On a game controller, the X button should be either the button labeled X or the first button on the lower row of controller buttons.
  KEYCODE_BUTTON_Y = $00000064; // Y Button key. On a game controller, the Y button should be either the button labeled Y or the second button on the lower row of controller buttons.
  KEYCODE_BUTTON_Z = $00000065; // Z Button key. On a game controller, the Z button should be either the button labeled Z or the third button on the lower row of controller buttons.
  KEYCODE_C = 31; // 'C' key.
  KEYCODE_CALCULATOR = $000000d2; // Calculator special function key. Used to launch a calculator application.
  KEYCODE_CALENDAR = $000000d0; // Calendar special function key. Used to launch a calendar application.
  KEYCODE_CALL = $00000005; // Call key.
  KEYCODE_CAMERA = $0000001b; // Camera key. Used to launch a camera application or take pictures.
  KEYCODE_CAPS_LOCK = $00000073;
  KEYCODE_CAPTIONS = $000000af; // Toggle captions key. Switches the mode for closed-captioning text, for example during television shows.
  KEYCODE_CHANNEL_DOWN = $000000a7; // Channel down key. On TV remotes, decrements the television channel.
  KEYCODE_CHANNEL_UP = $000000a6; // Channel up key. On TV remotes, increments the television channel.
  KEYCODE_CLEAR = $0000001c;
  KEYCODE_COMMA = $00000037;
  KEYCODE_CONTACTS = $000000cf; // Contacts special function key. Used to launch an address book application.
  KEYCODE_CTRL_LEFT = $00000071; // Left Control modifier key.
  KEYCODE_CTRL_RIGHT = $00000072; // Right Control modifier key.
  KEYCODE_D = 32;
  KEYCODE_DEL = $00000043; // Backspace key. Deletes characters before the insertion point, unlike KEYCODE_FORWARD_DEL.
  KEYCODE_DPAD_CENTER = $00000017; // Directional Pad Center key. May also be synthesized from trackball motions.
  KEYCODE_DPAD_DOWN = $00000014; // Directional Pad Down key. May also be synthesized from trackball motions.
  KEYCODE_DPAD_LEFT = $00000015; // Directional Pad Left key. May also be synthesized from trackball motions.
  KEYCODE_DPAD_RIGHT = $00000016; // Directional Pad Right key. May also be synthesized from trackball motions.
  KEYCODE_DPAD_UP = $00000013; // Directional Pad Up key. May also be synthesized from trackball motions.
  KEYCODE_DVR = $000000ad; // DVR key. On some TV remotes, switches to a DVR mode for recorded shows.
  KEYCODE_E = 33;
  KEYCODE_ENDCALL = $00000006; // End Call key.
  KEYCODE_ENTER = $00000042; // Enter key.
  KEYCODE_ENVELOPE = $00000041; // Envelope special function key. Used to launch a mail application.
  KEYCODE_EQUALS = $00000046; // '=' key.
  KEYCODE_ESCAPE = $0000006f; // Escape key.
  KEYCODE_EXPLORER = $00000040; // Explorer special function key. Used to launch a browser application.
  KEYCODE_F = 34; // 'F' key.
  KEYCODE_F1 = $00000083;
  KEYCODE_F10 = $0000008c;
  KEYCODE_F11 = $0000008d;
  KEYCODE_F12 = $0000008e;
  KEYCODE_F2 = $00000084;
  KEYCODE_F3 = $00000085;
  KEYCODE_F4 = $00000086;
  KEYCODE_F5 = $00000087;
  KEYCODE_F6 = $00000088;
  KEYCODE_F7 = $00000089;
  KEYCODE_F8 = $0000008a;
  KEYCODE_F9 = $0000008b;
  KEYCODE_FOCUS = $00000050; // Camera Focus key. Used to focus the camera.
  KEYCODE_FORWARD = $0000007d; // Forward key. Navigates forward in the history stack. Complement of KEYCODE_BACK.
  KEYCODE_FORWARD_DEL = $00000070; // Forward Delete key. Deletes characters ahead of the insertion point, unlike KEYCODE_DEL.
  KEYCODE_FUNCTION = $00000077; // Function modifier key.
  KEYCODE_G = 35;
  KEYCODE_GRAVE = $00000044; // '`' (backtick) key.
  KEYCODE_GUIDE = $000000ac; // Guide key. On TV remotes, shows a programming guide.
  KEYCODE_H = 36;
  KEYCODE_HEADSETHOOK = $0000004f; // Headset Hook key. Used to hang up calls and stop media.
  KEYCODE_HOME = $00000003; // Home key. This key is handled by the framework and is never delivered to applications.
  KEYCODE_I = 37;
  KEYCODE_INFO = $000000a5; // Info key. Common on TV remotes to show additional information related to what is currently being viewed.
  KEYCODE_INSERT = $0000007c; // Insert key. Toggles insert / overwrite edit mode.
  KEYCODE_J = 38;
  KEYCODE_K = 39;
  KEYCODE_L = 40;
  KEYCODE_LANGUAGE_SWITCH = $000000cc; // Language Switch key. Toggles the current input language such as switching between English and Japanese on a QWERTY keyboard. On some devices, the same function may be performed by pressing Shift+Spacebar.
  KEYCODE_LEFT_BRACKET = $00000047; // '[' key.
  KEYCODE_M = 41;
  KEYCODE_MANNER_MODE = $000000cd; // Manner Mode key. Toggles silent or vibrate mode on and off to make the device behave more politely in certain settings such as on a crowded train. On some devices, the key may only operate when long-pressed.
  KEYCODE_MEDIA_CLOSE = $00000080; // Close media key. May be used to close a CD tray, for example.
  KEYCODE_MEDIA_EJECT = $00000081; // Eject media key. May be used to eject a CD tray, for example.
  KEYCODE_MEDIA_FAST_FORWARD = $0000005a; // Fast Forward media key.
  KEYCODE_MEDIA_NEXT = $00000057; // Play Next media key.
  KEYCODE_MEDIA_PAUSE = $0000007f; // Pause media key.
  KEYCODE_MEDIA_PLAY = $0000007e; // Play media key.
  KEYCODE_MEDIA_PLAY_PAUSE = $00000055; // Play/Pause media key.
  KEYCODE_MEDIA_PREVIOUS = $00000058; // Play Previous media key.
  KEYCODE_MEDIA_RECORD = $00000082; // Record media key.
  KEYCODE_MEDIA_REWIND = $00000059; // Rewind media key.
  KEYCODE_MEDIA_STOP = $00000056; // Stop media key.
  KEYCODE_MENU = $00000052; // Menu key.
  KEYCODE_META_LEFT = $00000075; // Left Meta modifier key.
  KEYCODE_META_RIGHT = $00000076; // Right Meta modifier key.
  KEYCODE_MINUS = $00000045; // '-'
  KEYCODE_MOVE_END  = $0000007b; // End Movement key. Used for scrolling or moving the cursor around to the end of a line or to the bottom of a list.
  KEYCODE_MOVE_HOME = $0000007a; // Home Movement key. Used for scrolling or moving the cursor around to the start of a line or to the top of a list.
  KEYCODE_MUSIC = $000000d1; // Music special function key. Used to launch a music player application.
  KEYCODE_MUTE = $0000005b; //Mute key. Mutes the microphone, unlike KEYCODE_VOLUME_MUTE
  KEYCODE_N = 42;
  KEYCODE_NOTIFICATION = $00000053;
  KEYCODE_NUM = $0000004e; // Number modifier key. Used to enter numeric symbols. This key is not Num Lock; it is more like KEYCODE_ALT_LEFT and is interpreted as an ALT key by MetaKeyKeyListener.
  KEYCODE_NUMPAD_0 = $00000090;
  KEYCODE_NUMPAD_1 = $00000091;
  KEYCODE_NUMPAD_2 = $00000092;
  KEYCODE_NUMPAD_3 = $00000093;
  KEYCODE_NUMPAD_4 = $00000094;
  KEYCODE_NUMPAD_5 = $00000095;
  KEYCODE_NUMPAD_6 = $00000096;
  KEYCODE_NUMPAD_7 = $00000097;
  KEYCODE_NUMPAD_8 = $00000098;
  KEYCODE_NUMPAD_9 = $00000099;
  KEYCODE_NUMPAD_ADD = $0000009d;
  KEYCODE_NUMPAD_COMMA = $0000009f;
  KEYCODE_NUMPAD_DIVIDE = $0000009a;
  KEYCODE_NUMPAD_DOT = $0000009e;
  KEYCODE_NUMPAD_ENTER = $000000a0;
  KEYCODE_NUMPAD_EQUALS = $000000a1;
  KEYCODE_NUMPAD_LEFT_PAREN = $000000a2;
  KEYCODE_NUMPAD_MULTIPLY = $0000009b;
  KEYCODE_NUMPAD_RIGHT_PAREN = $000000a3;
  KEYCODE_NUMPAD_SUBTRACT = $0000009c;
  KEYCODE_NUM_LOCK = $0000008f;
  KEYCODE_O = 43;
  KEYCODE_P = 44;
  KEYCODE_PAGE_DOWN = $0000005d;
  KEYCODE_PAGE_UP = $0000005c;
  KEYCODE_PERIOD = $00000038;
  KEYCODE_PICTSYMBOLS = $0000005e;
  KEYCODE_PLUS = $00000051; // '+' key
  KEYCODE_POUND = $00000012; // '#' key.
  KEYCODE_POWER = $0000001a;
  KEYCODE_PROG_BLUE = $000000ba;
  KEYCODE_PROG_GREEN = $000000b8;
  KEYCODE_PROG_RED = $000000b7;
  KEYCODE_PROG_YELLOW = $000000b9;
  KEYCODE_Q = 45;
  KEYCODE_R = 46;
  KEYCODE_RIGHT_BRACKET = $00000048;
  KEYCODE_S = 47;
  KEYCODE_SCROLL_LOCK = $00000074;
  KEYCODE_SEARCH = $00000054;
  KEYCODE_SEMICOLON = $0000004a;
  KEYCODE_SETTINGS = $000000b0;
  KEYCODE_SHIFT_LEFT = 59;
  KEYCODE_SHIFT_RIGHT = 60;
  KEYCODE_SLASH = $0000004c; // '/' key.
  KEYCODE_SOFT_LEFT = $00000001;
  KEYCODE_SOFT_RIGHT = $00000002;
  KEYCODE_SPACE = $0000003e;
  KEYCODE_STAR = $00000011;
  KEYCODE_STB_INPUT = $000000b4;
  KEYCODE_STB_POWER = $000000b3;
  KEYCODE_SWITCH_CHARSET = $0000005f;
  KEYCODE_SYM = $0000003f; // Symbol modifier key. Used to enter alternate symbols.
  KEYCODE_SYSRQ = $00000078; // System Request / Print Screen key.
  KEYCODE_T = 48;
  KEYCODE_TAB = $0000003d;
  KEYCODE_TV = $000000aa;
  KEYCODE_TV_INPUT = $000000b2;
  KEYCODE_TV_POWER = $000000b1;
  KEYCODE_U = 49;
  KEYCODE_UNKNOWN = 0;
  KEYCODE_V = 50;
  KEYCODE_VOLUME_DOWN = $00000019;
  KEYCODE_VOLUME_MUTE = $000000a4;
  KEYCODE_VOLUME_UP = $00000018;
  KEYCODE_W = 51;
  KEYCODE_WINDOW = $000000ab; // Window key. On TV remotes, toggles picture-in-picture mode or other windowing functions.
  KEYCODE_X = 52;
  KEYCODE_Y = 53;
  KEYCODE_Z = 54;
  KEYCODE_ZOOM_IN = $000000a8;
  KEYCODE_ZOOM_OUT = $000000a9;
  MAX_KEYCODE = $00000054; // deprecated!
  META_ALT_LEFT_ON = $00000010;
  META_ALT_MASK = $00000032;
  META_ALT_ON = $00000002;
  META_ALT_RIGHT_ON = $00000020;
  META_CAPS_LOCK_ON = $00100000;
  META_CTRL_LEFT_ON = $00002000;
  META_CTRL_MASK = $00007000;
  META_CTRL_ON = $00001000;
  META_CTRL_RIGHT_ON = $00004000;
  META_FUNCTION_ON = $00000008;
  META_META_LEFT_ON = $00020000;
  META_META_MASK = $00070000;
  META_META_ON = $00010000;
  META_META_RIGHT_ON = $00040000;
  META_NUM_LOCK_ON = $00200000;
  META_SCROLL_LOCK_ON = $00400000;
  META_SHIFT_LEFT_ON = $00000040;
  META_SHIFT_MASK = $000000c1;
  META_SHIFT_ON = $00000001;
  META_SHIFT_RIGHT_ON = $00000080;
  META_SYM_ON = 4;

implementation

end.
