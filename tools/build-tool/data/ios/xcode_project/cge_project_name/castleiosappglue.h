/*
  Copyright 2013-2017 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/


#ifndef CGE_IOSAPPGLUE_INCLUDED
#define CGE_IOSAPPGLUE_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

enum ECgeTouchCtlInterface
{
    ecgetciNone              = 0,
    ecgetciCtlWalkCtlRotate  = 1,
    ecgetciCtlWalkDragRotate = 2,
    etciCtlFlyCtlWalkDragRotate = 3,
    etciCtlPanXYDragRotate   = 4,
};

enum ECgeMouseCursor
{
    ecgecursorDefault   = 0,
    ecgecursorWait      = 1,
    ecgecursorHand      = 2,
    ecgecursorText      = 3,
    ecgecursorNone      = 4,
};

enum ECgeLibCallbackCode
{
    ecgelibNeedsDisplay          = 0,  // app should repaint the view (content changed)
    ecgelibSetMouseCursor        = 1,  // sends ECgeMouseCursor in iParam1
    ecgelibNavigationTypeChanged = 2,  // sends ECgeNavigationType in iParam1
    ecgelibSetMousePosition      = 3,  // sends x in iParam1 and y in iParam2
    ecgelibWarning               = 4,  // sends message in szParam
};

enum ECgeKey    // values for these constants have to be same as in unit CastleKeysMouse (TKey)
{
  kcge_None        = 0,
  kcge_PrintScreen = 1,
  kcge_CapsLock    = 2,
  kcge_ScrollLock  = 3,
  kcge_NumLock     = 4,
  kcge_Pause       = 5,
  kcge_Apostrophe  = 6,
  kcge_Semicolon   = 7,
  kcge_BackSpace   = 8,
  kcge_Tab         = 9,
  kcge_Slash       = 10,
  kcge_BackQuote   = 11,
  kcge_Minus       = 12,
  kcge_Enter       = 13,
  kcge_Equal       = 14,
  kcge_BackSlash   = 15,
  kcge_Shift       = 16,
  kcge_Ctrl        = 17,
  kcge_Alt         = 18,
  kcge_Plus        = 19,
  kcge_Escape      = 27,
  kcge_Space       = 32,
  kcge_PageUp      = 33,
  kcge_PageDown    = 34,
  kcge_End         = 35,
  kcge_Home        = 36,
  kcge_Left        = 37,
  kcge_Up          = 38,
  kcge_Right       = 39,
  kcge_Down        = 40,
  kcge_Insert      = 45,
  kcge_Delete      = 46,
  kcge_0           = 48,
  kcge_1           = 49,
  kcge_2           = 50,
  kcge_3           = 51,
  kcge_4           = 52,
  kcge_5           = 53,
  kcge_6           = 54,
  kcge_7           = 55,
  kcge_8           = 56,
  kcge_9           = 57,
  kcge_A           = 65,
  kcge_B           = 66,
  kcge_C           = 67,
  kcge_D           = 68,
  kcge_E           = 69,
  kcge_F           = 70,
  kcge_G           = 71,
  kcge_H           = 72,
  kcge_I           = 73,
  kcge_J           = 74,
  kcge_K           = 75,
  kcge_L           = 76,
  kcge_M           = 77,
  kcge_N           = 78,
  kcge_O           = 79,
  kcge_P           = 80,
  kcge_Q           = 81,
  kcge_R           = 82,
  kcge_S           = 83,
  kcge_T           = 84,
  kcge_U           = 85,
  kcge_V           = 86,
  kcge_W           = 87,
  kcge_X           = 88,
  kcge_Y           = 89,
  kcge_Z           = 90,
  kcge_LeftBracket = 91,
  kcge_RightBracket = 92,
  kcge_Numpad_Plus = 107,
  kcge_Numpad_Minus = 109,
  kcge_F1          = 112,
  kcge_F2          = 113,
  kcge_F3          = 114,
  kcge_F4          = 115,
  kcge_F5          = 116,
  kcge_F6          = 117,
  kcge_F7          = 118,
  kcge_F8          = 119,
  kcge_F9          = 120,
  kcge_F10         = 121,
  kcge_F11         = 122,
  kcge_F12         = 123,
  kcge_Numpad_0    = 140,
  kcge_Numpad_1    = 141,
  kcge_Numpad_2    = 142,
  kcge_Numpad_3    = 143,
  kcge_Numpad_4    = 144,
  kcge_Numpad_5    = 145,
  kcge_Numpad_6    = 146,
  kcge_Numpad_7    = 147,
  kcge_Numpad_8    = 148,
  kcge_Numpad_9    = 149,
  kcge_Numpad_End  = 150,
  kcge_Numpad_Down = 151,
  kcge_Numpad_PageDown = 152,
  kcge_Numpad_Left     = 153,
  kcge_Numpad_Begin    = 154,
  kcge_Numpad_Right    = 155,
  kcge_Numpad_Home     = 156,
  kcge_Numpad_Up       = 157,
  kcge_Numpad_PageUp   = 158,
  kcge_Numpad_Insert   = 159,
  kcge_Numpad_Delete   = 160,
  kcge_Numpad_Enter    = 161,
  kcge_Numpad_Multiply = 162,
  kcge_Numpad_Divide   = 163,
  kcge_Pad_A      = 164,
  kcge_Pad_B      = 165,
  kcge_Pad_X      = 166,
  kcge_Pad_Y      = 167,
  kcge_Pad_L      = 168,
  kcge_Pad_R      = 169,
  kcge_Pad_ZL     = 170,
  kcge_Pad_ZR     = 171,
  kcge_Pad_Plus   = 172,
  kcge_Pad_Minus  = 173,
  kcge_Pad_Left   = 174,
  kcge_Pad_Up     = 175,
  kcge_Pad_Right  = 176,
  kcge_Pad_Down   = 177,
  kcge_Comma       = 188,
  kcge_Period      = 190,
};

// __cdecl here causes warning:
// /Users/michalis/sources/cat-astrophe-games/escape_universe/trunk/castle-engine-output/ios/xcode_project/escape_universe/castleiosappglue.h:166:14: Calling convention '__cdecl' ignored for this target

typedef int (*TCgeLibraryCallback)(int /*ECgeLibCallbackCode*/eCode, int iParam1, int iParam2, const char *szParam);
typedef void (*TCgeReceiveMessageFromPascalCallback)(const char *message);

//-----------------------------------------------------------------------------

// Initialize the library, this function must be called first (required).
// Then CGEApp_Open and CGEApp_Close may be called multiple times.
extern void CGEApp_Initialize(const char *applicationConfigDirectory);
extern void CGEApp_Finalize(void);

extern int CGEApp_ContextProperties(int* redBits, int* greenBits, int* blueBits, int* alphaBits, int* depthBits, int* stencilBits, int* multiSampling);

extern void CGEApp_Open(unsigned initialWidth, unsigned initialHeight, unsigned initialStatusBarHeight, unsigned nDpi);
extern void CGEApp_Close(bool quitWhenLastWindowClosed);

extern void CGEApp_Resize(unsigned uiViewWidth, unsigned uiViewHeight, unsigned uiStatusBarHeight);       // let the library know about the viewport size changes
extern void CGEApp_Render(void);                                                  // paints the 3d scene into the context
extern void CGEApp_SetLibraryCallbackProc(TCgeLibraryCallback pProc);     // set callback function
extern void CGEApp_Update(void);                                                  // let the 3d engine perform the animations, etc

extern void CGEApp_MouseDown(int x, int y, bool bLeftBtn, int nFingerIdx);    // [0,0] is the bottom-left corner!
extern void CGEApp_Motion(int x, int y, int nFingerIdx);
extern void CGEApp_MouseUp(int x, int y, bool bLeftBtn, int nFingerIdx);

extern void CGEApp_KeyDown(int /*ECgeKey*/ eKey);
extern void CGEApp_KeyUp(int /*ECgeKey*/ eKey);

extern void CGEApp_JoystickAxis(int joystickIndex, float x, float y);
extern void CGEApp_JoystickCount(int joystickCount);

extern int CGEApp_HandleOpenUrl(const char *szUrl); // open URL, return 1 if processed

extern void CGEApp_SetReceiveMessageFromPascalCallback(TCgeReceiveMessageFromPascalCallback aCallback);
extern void CGEApp_SendMessageToPascal(const char *message);

#ifdef __cplusplus
}
#endif

#endif //CGE_IOSAPPGLUE_INCLUDED
