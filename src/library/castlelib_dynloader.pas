{
  Copyright 2008-2017 Jan Adamec, Michalis Kamburelis.

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
     .../castle_game_engine/src/library/ directory.

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

  // user interface modes
  ecgeuiDesktop = 0;
  ecgeuiTouch   = 1;

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

type
  TLibraryCallbackProc = function (eCode, iParam1, iParam2: cInt32; szParam: pcchar):cInt32; cdecl;

procedure CGE_Open(flags: cUInt32); cdecl; external 'castleengine';
procedure CGE_Close(); cdecl; external 'castleengine';
procedure CGE_GetOpenGLInformation(szBuffer: pchar; nBufSize: cInt32); cdecl; external 'castleengine';
procedure CGE_Resize(uiViewWidth, uiViewHeight: cUInt32); cdecl; external 'castleengine';
procedure CGE_Render(); cdecl; external 'castleengine';
procedure CGE_SaveScreenshotToFile(szFile: pcchar); cdecl; external 'castleengine';
procedure CGE_SetLibraryCallbackProc(aProc: TLibraryCallbackProc); cdecl; external 'castleengine';
procedure CGE_Update(); cdecl; external 'castleengine';
procedure CGE_MouseDown(X, Y: cInt32; bLeftBtn: cBool; FingerIndex: CInt32); cdecl; external 'castleengine';
procedure CGE_Motion(X, Y: cInt32; FingerIndex: CInt32); cdecl; external 'castleengine';
procedure CGE_MouseUp(X, Y: cInt32; bLeftBtn: cBool; FingerIndex: CInt32); cdecl; external 'castleengine';
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
procedure CGE_SetUserInterface(AutomaticTouchInterface: cBool; nDpi: cInt32); cdecl; external 'castleengine';

procedure CGE_SetVariableInt(eVar: cInt32; nValue: cInt32); cdecl; external 'castleengine';
function CGE_GetVariableInt(eVar: cInt32): cInt32; cdecl; external 'castleengine';

procedure CGE_SetNodeFieldValue(szNodeName, szFieldName: pcchar;
                                fVal1, fVal2, fVal3, fVal4: cFloat); cdecl; external 'castleengine';
                                
procedure CGE_IncreaseSceneTime(fTimeS: cFloat); cdecl; external 'castleengine';

implementation

begin
end.
