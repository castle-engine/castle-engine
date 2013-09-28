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

  2. Copy castlelib.dll to your project folder, where executable file is
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

procedure CGE_LoadLibrary();
procedure CGE_Init();
procedure CGE_Close();
procedure CGE_SetRenderParams(uiViewWidth, uiViewHeight: cUInt32);
procedure CGE_Render();
procedure CGE_SetLibraryCallbackProc(aProc: TCgeLibraryCallbackProc);
procedure CGE_OnIdle();
procedure CGE_OnMouseDown(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32);
procedure CGE_OnMouseMove(x, y: cInt32; uiShift: cUInt32);
procedure CGE_OnMouseUp(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32);
procedure CGE_OnMouseWheel(zDelta: cFloat; bVertical: cBool; uiShift: cUint32);
procedure CGE_LoadSceneFromFile(szFile: pcchar);
function CGE_GetViewpointsCount(): cInt32;
procedure CGE_GetViewpointName(iViewpointIdx: cInt32; szName: pchar; nBufSize: cInt32);
procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool);
function CGE_GetCurrentNavigationType(): cInt32;
procedure CGE_SetNavigationType(NewType: cInt32);

implementation

uses
  SysUtils, dynlibs;

type
  PFNRD_CGE_Init = procedure(); cdecl;
  PFNRD_CGE_Close = procedure(); cdecl;
  PFNRD_CGE_SetRenderParams = procedure(uiViewWidth, uiViewHeight: cUInt32); cdecl;
  PFNRD_CGE_Render = procedure(); cdecl;
  PFNRD_CGE_SetLibraryCallbackProc = procedure(pProc: TCgeLibraryCallbackProc); cdecl;
  PFNRD_CGE_OnIdle = procedure(); cdecl;

  PFNRD_CGE_OnMouseDown = procedure(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32); cdecl;
  PFNRD_CGE_OnMouseMove = procedure(x, y: cInt32; uiShift: cUInt32); cdecl;
  PFNRD_CGE_OnMouseUp = procedure(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32); cdecl;
  PFNRD_CGE_OnMouseWheel = procedure(zDelta: cFloat; bVertical: cBool; uiShift: cUint32); cdecl;

  PFNRD_CGE_LoadSceneFromFile = procedure(szFile: pcchar); cdecl;
  PFNRD_CGE_GetViewpointsCount = function():cInt32; cdecl;
  PFNRD_CGE_GetViewpointName = procedure(iViewpointIdx: cInt32; szName: pchar; nBufSize: cInt32); cdecl;
  PFNRD_CGE_MoveToViewpoint = procedure(iViewpointIdx: cInt32; bAnimated: cBool); cdecl;

  PFNRD_CGE_GetCurrentNavigationType = function():cInt32; cdecl;
  PFNRD_CGE_SetNavigationType = procedure(eNewType: cInt32); cdecl;

var
  g_hCgeDll: TLibHandle;
  pfrd_CGE_Init: PFNRD_CGE_Init;
  pfrd_CGE_Close: PFNRD_CGE_Close;
  pfrd_CGE_SetRenderParams: PFNRD_CGE_SetRenderParams;
  pfrd_CGE_Render: PFNRD_CGE_Render;
  pfrd_CGE_SetLibraryCallbackProc: PFNRD_CGE_SetLibraryCallbackProc;
  pfrd_CGE_OnIdle: PFNRD_CGE_OnIdle;
  pfrd_CGE_OnMouseDown: PFNRD_CGE_OnMouseDown;
  pfrd_CGE_OnMouseMove: PFNRD_CGE_OnMouseMove;
  pfrd_CGE_OnMouseUp: PFNRD_CGE_OnMouseUp;
  pfrd_CGE_OnMouseWheel: PFNRD_CGE_OnMouseWheel;
  pfrd_CGE_LoadSceneFromFile: PFNRD_CGE_LoadSceneFromFile;
  pfrd_CGE_GetViewpointsCount: PFNRD_CGE_GetViewpointsCount;
  pfrd_CGE_GetViewpointName: PFNRD_CGE_GetViewpointName;
  pfrd_CGE_MoveToViewpoint: PFNRD_CGE_MoveToViewpoint;
  pfrd_CGE_GetCurrentNavigationType: PFNRD_CGE_GetCurrentNavigationType;
  pfrd_CGE_SetNavigationType: PFNRD_CGE_SetNavigationType;

//-----------------------------------------------------------------------------
procedure CGE_LoadLibrary();
const
  SharedPrefix = {$ifdef UNIX} 'lib' {$else} '' {$endif};
var
  LibName: string;
begin
  LibName := SharedPrefix + 'castlelib.' + SharedSuffix;
  g_hCgeDll := LoadLibrary(LibName);
  if g_hCgeDll = 0 then
    raise Exception.CreateFmt('Castle Game Engine shared library "%s" cannot be loaded',
      [LibName]);

  pfrd_CGE_Init := PFNRD_CGE_Init(GetProcedureAddress(g_hCgeDll, 'CGE_Init'));
  pfrd_CGE_Close := PFNRD_CGE_Close(GetProcedureAddress(g_hCgeDll, 'CGE_Close'));
  pfrd_CGE_SetRenderParams := PFNRD_CGE_SetRenderParams(GetProcedureAddress(g_hCgeDll, 'CGE_SetRenderParams'));
  pfrd_CGE_Render := PFNRD_CGE_Render(GetProcedureAddress(g_hCgeDll, 'CGE_Render'));
  pfrd_CGE_SetLibraryCallbackProc := PFNRD_CGE_SetLibraryCallbackProc(GetProcedureAddress(g_hCgeDll, 'CGE_SetLibraryCallbackProc'));
  pfrd_CGE_OnIdle := PFNRD_CGE_OnIdle(GetProcedureAddress(g_hCgeDll, 'CGE_OnIdle'));
  pfrd_CGE_OnMouseDown := PFNRD_CGE_OnMouseDown(GetProcedureAddress(g_hCgeDll, 'CGE_OnMouseDown'));
  pfrd_CGE_OnMouseMove := PFNRD_CGE_OnMouseMove(GetProcedureAddress(g_hCgeDll, 'CGE_OnMouseMove'));
  pfrd_CGE_OnMouseUp := PFNRD_CGE_OnMouseUp(GetProcedureAddress(g_hCgeDll, 'CGE_OnMouseUp'));
  pfrd_CGE_OnMouseWheel :=PFNRD_CGE_OnMouseWheel(GetProcedureAddress(g_hCgeDll, 'CGE_OnMouseWheel'));
  pfrd_CGE_LoadSceneFromFile := PFNRD_CGE_LoadSceneFromFile(GetProcedureAddress(g_hCgeDll, 'CGE_LoadSceneFromFile'));
  pfrd_CGE_GetViewpointsCount := PFNRD_CGE_GetViewpointsCount(GetProcedureAddress(g_hCgeDll, 'CGE_GetViewpointsCount'));
  pfrd_CGE_GetViewpointName := PFNRD_CGE_GetViewpointName(GetProcedureAddress(g_hCgeDll, 'CGE_GetViewpointName'));
  pfrd_CGE_MoveToViewpoint := PFNRD_CGE_MoveToViewpoint(GetProcedureAddress(g_hCgeDll, 'CGE_MoveToViewpoint'));
  pfrd_CGE_GetCurrentNavigationType := PFNRD_CGE_GetCurrentNavigationType(GetProcedureAddress(g_hCgeDll, 'CGE_GetCurrentNavigationType'));
  pfrd_CGE_SetNavigationType := PFNRD_CGE_SetNavigationType(GetProcedureAddress(g_hCgeDll, 'CGE_SetNavigationType'));
end;

procedure CGE_Init();
begin
  pfrd_CGE_Init();
end;

procedure CGE_Close();
begin
  pfrd_CGE_Close();
end;

procedure CGE_SetRenderParams(uiViewWidth, uiViewHeight: cUInt32);
begin
  pfrd_CGE_SetRenderParams(uiViewWidth, uiViewHeight);
end;

procedure CGE_Render();
begin
  pfrd_CGE_Render();
end;

procedure CGE_SetLibraryCallbackProc(aProc: TCgeLibraryCallbackProc);
begin
  pfrd_CGE_SetLibraryCallbackProc(aProc);
end;

procedure CGE_OnIdle();
begin
  pfrd_CGE_OnIdle();
end;

procedure CGE_OnMouseDown(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32);
begin
  pfrd_CGE_OnMouseDown(x, y, bLeftBtn, uiShift);
end;

procedure CGE_OnMouseMove(x, y: cInt32; uiShift: cUInt32);
begin
  pfrd_CGE_OnMouseMove(x, y, uiShift);
end;

procedure CGE_OnMouseUp(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32);
begin
  pfrd_CGE_OnMouseUp(x, y, bLeftBtn, uiShift);
end;

procedure CGE_OnMouseWheel(zDelta: cFloat; bVertical: cBool; uiShift: cUint32);
begin
  pfrd_CGE_OnMouseWheel(zDelta, bVertical, uiShift);
end;

procedure CGE_LoadSceneFromFile(szFile: pcchar);
begin
  pfrd_CGE_LoadSceneFromFile(szFile);
end;

function CGE_GetViewpointsCount(): cInt32;
begin
  Result := pfrd_CGE_GetViewpointsCount();
end;

procedure CGE_GetViewpointName(iViewpointIdx: cInt32; szName: pchar; nBufSize: cInt32);
begin
  pfrd_CGE_GetViewpointName(iViewpointIdx, szName, nBufSize);
end;

procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool);
begin
  pfrd_CGE_MoveToViewpoint(iViewpointIdx, bAnimated);
end;

function CGE_GetCurrentNavigationType(): cInt32;
begin
  Result := pfrd_CGE_GetCurrentNavigationType();
end;

procedure CGE_SetNavigationType(NewType: cInt32);
begin
  pfrd_CGE_SetNavigationType(NewType);
end;

begin
end.