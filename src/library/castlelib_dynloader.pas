{
  Copyright 2008-2013 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  This is another test project for our library. Instead of including library
  units directly, it uses the compiled dynamic library.

  This file is here as a wrapper, only to load the CastleLib.dll for you and
  to call GetProcedureAddress for all exported functions.

  Usage:
  1. Copy castle_dynloader.pas into your project folder.

  2. Copy castleengine.dll to your project folder, where executable file is
     generated.

  3. Include castlelib_dynloader in your source files, call CGE_LoadLibrary at
     the start of your program, and then call CGE_xxx functions as usual.
}

unit castlelib_dynloader;

interface
uses
  ctypes;

const
  ecgessShift = 1;
  ecgessAlt   = 2;
  ecgessCtrl  = 4;

  // touch interface modes
  ecgetciNone              = 0;
  ecgetciCtlWalkCtlRotate  = 1;
  ecgetciCtlWalkDragRotate = 2;

  // library callback codes
  ecgelibNeedsDisplay     = 0;
  ecgelibSetMouseCursor   = 1;   // sends mouse cursor code in iParam1

  // mouse cursor codes
  ecgecursorDefault   = 0;
  ecgecursorWait      = 1;
  ecgecursorHand      = 2;
  ecgecursorText      = 3;
  ecgecursorNone      = 4;

type
  TCgeLibraryCallbackProc = function (eCode, iParam1, iParam2: cInt32):cInt32; cdecl;

procedure CGE_Init(); cdecl; external 'castleengine';
procedure CGE_Close(); cdecl; external 'castleengine';
procedure CGE_SetRenderParams(uiViewWidth, uiViewHeight: cUInt32); cdecl; external 'castleengine';
procedure CGE_Render(); cdecl; external 'castleengine';
procedure CGE_SetLibraryCallbackProc(aProc: TCgeLibraryCallbackProc); cdecl; external 'castleengine';
procedure CGE_OnIdle(); cdecl; external 'castleengine';
procedure CGE_OnMouseDown(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32); cdecl; external 'castleengine';
procedure CGE_OnMouseMove(x, y: cInt32; uiShift: cUInt32); cdecl; external 'castleengine';
procedure CGE_OnMouseUp(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32); cdecl; external 'castleengine';
procedure CGE_OnMouseWheel(zDelta: cFloat; bVertical: cBool; uiShift: cUint32); cdecl; external 'castleengine';
procedure CGE_LoadSceneFromFile(szFile: pcchar); cdecl; external 'castleengine';
function CGE_GetViewpointsCount(): cInt32; cdecl; external 'castleengine';
procedure CGE_GetViewpointName(iViewpointIdx: cInt32; szName: pchar; nBufSize: cInt32); cdecl; external 'castleengine';
procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool); cdecl; external 'castleengine';
function CGE_GetCurrentNavigationType(): cInt32; cdecl; external 'castleengine';
procedure CGE_SetNavigationType(NewType: cInt32); cdecl; external 'castleengine';
procedure CGE_UpdateTouchInterface(eMode: cInt32); cdecl; external 'castleengine';

implementation

begin
end.
