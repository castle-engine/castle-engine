{
  Copyright 2008-2023 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  This is another test project for our library. Instead of including library
  units directly, it uses the compiled dynamic library.

  This file is here as a wrapper, only to load the castleengine shared library
  (castleengine.dll, libcastleengine.so, libcastleengine.dylib depending on OS)
  for you.

  Usage:
  1. Copy castlelib_dynloader.pas into your project folder.

  2. Make the shared library accessible to your project.

     Windows: Copy castleengine.dll to your project folder,
     where executable file is generated. Or to any directory listed on $PATH.

     Linux: Make sure libcastleengine.so is available in a directory
     listed on $LD_LIBRARY_PATH (or in one of the predefined directories,
     like /usr/lib; see "man dlopen" for details). For local testing,
     it's usually most comfortable to set LD_LIBRARY_PATH to just contain
     .../castle_game_engine/src/deprecated_library/ directory.

  3. Include castlelib_dynloader in your source files. Library is automatically
     initialized. Just call CGE_xxx functions as usual.
}

unit castlelib_dynloader;

{$I castleconf.inc}

interface
uses
  ctypes;

const
  // open flags
  ecgeofSaveMemory = 1;
  ecgeofLog        = 2;

  // used for quering engine parameters in CGE_Set/GetVariable
  ecgevarWalkHeadBobbing = 0;   // walking effect (int, 1 = on, 0 = off)
  ecgevarEffectSSAO      = 1;   // screen space ambient occlusion (int, 1 or 0)
  ecgevarMouseLook       = 2;   // activate mouse look viewing mode, desktop interface only (int, 1 or 0)
  ecgevarCrossHair       = 3;   // show crosshair in the center of the screen (int, 1 or 0)
  ecgevarAnimationRunning = 4;  // (read-only) engine would like to progress with the animation (int, 1 or 0)
  ecgevarWalkTouchCtl    = 5;   // walking touch control (int, one of ECgeTouchCtlInterface values)
  ecgevarScenePaused     = 6;   // pause Viewport (int, 1 = on, 0 = off)
  ecgevarAutoRedisplay   = 7;   // automatically redraws the window all the time (int, 1 = on, 0 = off)
  ecgevarHeadlight       = 8;   // avatar's headlight (int, 1 = on, 0 = off)
  ecgevarOcclusionQuery  = 9;   // occlusion query, ignored when hierarchical on (int, 1 = on, 0 = off)
  ecgevarPhongShading    = 10;  // phong shading (int, 1 = on, 0 = off)

  // navigation types (ECgeNavigationType enum)
  ecgenavWalk      = 0;
  ecgenavFly       = 1;
  ecgenavExamine   = 2;
  ecgenavTurntable = 3;
  ecgenavNone      = 4;

  // touch interface modes
  tiNone              = 0;
  tiCtlWalkCtlRotate  = 1;
  tiCtlWalkDragRotate = 2;
  tiCtlFlyCtlWalkDragRotate = 3;
  tiCtlPanXYDragRotate   = 4;

  // library callback codes
  ecgelibNeedsDisplay     = 0;
  ecgelibSetMouseCursor   = 1;   // sends mouse cursor code in iParam1
  ecgelibNavigationTypeChanged = 2;  // sends ECgeNavigationType in iParam1 (see castleengine.h)
  ecgelibSetMousePosition      = 3;  // sends x in iParam1 and y in iParam2
  ecgelibWarning               = 4;  // sends message in szParam

  // mouse cursor codes
  ecgecursorDefault   = 0;
  ecgecursorWait      = 1;
  ecgecursorHand      = 2;
  ecgecursorText      = 3;
  ecgecursorNone      = 4;

  // key codes (ECgeKey)
  kcge_None        = 0;
  kcge_PrintScreen = 1;
  kcge_CapsLock    = 2;
  kcge_ScrollLock  = 3;
  kcge_NumLock     = 4;
  kcge_Pause       = 5;
  kcge_Apostrophe  = 6;
  kcge_Semicolon   = 7;
  kcge_BackSpace   = 8;
  kcge_Tab         = 9;
  kcge_Slash       = 10;
  kcge_BackQuote   = 11;
  kcge_Minus       = 12;
  kcge_Enter       = 13;
  kcge_Equal       = 14;
  kcge_BackSlash   = 15;
  kcge_Shift       = 16;
  kcge_Ctrl        = 17;
  kcge_Alt         = 18;
  kcge_Plus        = 19;
  kcge_Escape      = 27;
  kcge_Space       = 32;
  kcge_PageUp      = 33;
  kcge_PageDown    = 34;
  kcge_End         = 35;
  kcge_Home        = 36;
  kcge_Left        = 37;
  kcge_Up          = 38;
  kcge_Right       = 39;
  kcge_Down        = 40;
  kcge_Insert      = 45;
  kcge_Delete      = 46;
  kcge_0           = 48;
  kcge_1           = 49;
  kcge_2           = 50;
  kcge_3           = 51;
  kcge_4           = 52;
  kcge_5           = 53;
  kcge_6           = 54;
  kcge_7           = 55;
  kcge_8           = 56;
  kcge_9           = 57;
  kcge_A           = 65;
  kcge_B           = 66;
  kcge_C           = 67;
  kcge_D           = 68;
  kcge_E           = 69;
  kcge_F           = 70;
  kcge_G           = 71;
  kcge_H           = 72;
  kcge_I           = 73;
  kcge_J           = 74;
  kcge_K           = 75;
  kcge_L           = 76;
  kcge_M           = 77;
  kcge_N           = 78;
  kcge_O           = 79;
  kcge_P           = 80;
  kcge_Q           = 81;
  kcge_R           = 82;
  kcge_S           = 83;
  kcge_T           = 84;
  kcge_U           = 85;
  kcge_V           = 86;
  kcge_W           = 87;
  kcge_X           = 88;
  kcge_Y           = 89;
  kcge_Z           = 90;
  kcge_LeftBracket = 91;
  kcge_RightBracket = 92;
  kcge_Numpad_Plus = 107;
  kcge_Numpad_Minus = 109;
  kcge_F1          = 112;
  kcge_F2          = 113;
  kcge_F3          = 114;
  kcge_F4          = 115;
  kcge_F5          = 116;
  kcge_F6          = 117;
  kcge_F7          = 118;
  kcge_F8          = 119;
  kcge_F9          = 120;
  kcge_F10         = 121;
  kcge_F11         = 122;
  kcge_F12         = 123;
  kcge_Numpad_0    = 140;
  kcge_Numpad_1    = 141;
  kcge_Numpad_2    = 142;
  kcge_Numpad_3    = 143;
  kcge_Numpad_4    = 144;
  kcge_Numpad_5    = 145;
  kcge_Numpad_6    = 146;
  kcge_Numpad_7    = 147;
  kcge_Numpad_8    = 148;
  kcge_Numpad_9    = 149;
  kcge_Numpad_End  = 150;
  kcge_Numpad_Down = 151;
  kcge_Numpad_PageDown = 152;
  kcge_Numpad_Left     = 153;
  kcge_Numpad_Begin    = 154;
  kcge_Numpad_Right    = 155;
  kcge_Numpad_Home     = 156;
  kcge_Numpad_Up       = 157;
  kcge_Numpad_PageUp   = 158;
  kcge_Numpad_Insert   = 159;
  kcge_Numpad_Delete   = 160;
  kcge_Numpad_Enter    = 161;
  kcge_Numpad_Multiply = 162;
  kcge_Numpad_Divide   = 163;
  kcge_Pad_A      = 164;
  kcge_Pad_B      = 165;
  kcge_Pad_X      = 166;
  kcge_Pad_Y      = 167;
  kcge_Pad_L      = 168;
  kcge_Pad_R      = 169;
  kcge_Pad_ZL     = 170;
  kcge_Pad_ZR     = 171;
  kcge_Pad_Plus   = 172;
  kcge_Pad_Minus  = 173;
  kcge_Pad_Left   = 174;
  kcge_Pad_Up     = 175;
  kcge_Pad_Right  = 176;
  kcge_Pad_Down   = 177;
  kcge_Comma       = 188;
  kcge_Period      = 190;

type
  TLibraryCallbackProc = function (eCode, iParam1, iParam2: cInt32; szParam: pcchar):cInt32; cdecl;

procedure CGE_Initialize(ApplicationConfigDirectory: PCChar); cdecl; external 'castleengine';
procedure CGE_Finalize(); cdecl; external 'castleengine';
procedure CGE_Open(flags: cUInt32; InitialWidth, InitialHeight, Dpi: cUInt32); cdecl; external 'castleengine';
procedure CGE_Close(); cdecl; external 'castleengine';
procedure CGE_GetOpenGLInformation(szBuffer: pchar; nBufSize: cInt32); cdecl; external 'castleengine';
procedure CGE_Resize(uiViewWidth, uiViewHeight: cUInt32); cdecl; external 'castleengine';
procedure CGE_Render(); cdecl; external 'castleengine';
procedure CGE_SaveScreenshotToFile(szFile: pcchar); cdecl; external 'castleengine';
procedure CGE_SetLibraryCallbackProc(aProc: TLibraryCallbackProc); cdecl; external 'castleengine';
procedure CGE_Update(); cdecl; external 'castleengine';
procedure CGE_MouseDown(X, Y: cInt32; bLeftBtn: cBool; FingerIndex: CInt32); cdecl; external 'castleengine';
procedure CGE_Motion(X, Y: cInt32; FingerIndex: CInt32); cdecl; external 'castleengine';
procedure CGE_MouseUp(X, Y: cInt32; bLeftBtn: cBool; FingerIndex: CInt32; trackReleased: cBool); cdecl; external 'castleengine';
procedure CGE_MouseWheel(zDelta: cFloat; bVertical: cBool); cdecl; external 'castleengine';
procedure CGE_KeyDown(eKey: CInt32); cdecl; external 'castleengine';
procedure CGE_KeyUp(eKey: CInt32); cdecl; external 'castleengine';
procedure CGE_LoadSceneFromFile(szFile: pcchar); cdecl; external 'castleengine';
function CGE_GetViewpointsCount(): cInt32; cdecl; external 'castleengine';
procedure CGE_GetViewpointName(iViewpointIdx: cInt32; szName: pchar; nBufSize: cInt32); cdecl; external 'castleengine';
procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool); cdecl; external 'castleengine';
procedure CGE_AddViewpointFromCurrentView(szName: pcchar); cdecl; external 'castleengine';
procedure CGE_GetBoundingBox(pfXMin, pfXMax, pfYMin, pfYMax, pfZMin, pfZMax: pcfloat); cdecl; external 'castleengine';
procedure CGE_GetViewCoords(pfPosX, pfPosY, pfPosZ, pfDirX, pfDirY, pfDirZ,
                            pfUpX, pfUpY, pfUpZ, pfGravX, pfGravY, pfGravZ: pcfloat); cdecl; external 'castleengine';
procedure CGE_MoveViewToCoords(fPosX, fPosY, fPosZ, fDirX, fDirY, fDirZ,
                               fUpX, fUpY, fUpZ, fGravX, fGravY, fGravZ: cFloat;
                               bAnimated: cBool); cdecl; external 'castleengine';
function CGE_GetNavigationType(): cInt32; cdecl; external 'castleengine';
procedure CGE_SetNavigationType(NewType: cInt32); cdecl; external 'castleengine';
procedure CGE_SetTouchInterface(eMode: cInt32); cdecl; external 'castleengine';
procedure CGE_SetUserInterface(AutomaticTouchInterface: cBool); cdecl; external 'castleengine';

procedure CGE_SetVariableInt(eVar: cInt32; nValue: cInt32); cdecl; external 'castleengine';
function CGE_GetVariableInt(eVar: cInt32): cInt32; cdecl; external 'castleengine';

procedure CGE_SetNodeFieldValue(szNodeName, szFieldName: pcchar;
                                fVal1, fVal2, fVal3, fVal4: cFloat); cdecl; external 'castleengine';

procedure CGE_IncreaseSceneTime(fTimeS: cFloat); cdecl; external 'castleengine';

implementation

{$ifdef DARWIN}
  {$linklib castleengine}
{$endif}

begin
end.
