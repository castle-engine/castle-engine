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

procedure CGE_LoadLibrary();
procedure CGE_Init();
procedure CGE_Close();
procedure CGE_SetRenderParams(uiViewWidth, uiViewHeight: cUInt32);
procedure CGE_Render();
procedure CGE_OnIdle();
procedure CGE_OnMouseDown(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32);
procedure CGE_OnMouseMove(x, y: cInt32; uiShift: cUInt32);
procedure CGE_OnMouseUp(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32);
procedure CGE_OnMouseWheel(zDelta: cFloat);
procedure CGE_LoadSceneFromFile(szFile: pcchar);
procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool);

implementation

uses
  dynlibs;

type
  PFNRD_CGE_Init = procedure(); cdecl;
  PFNRD_CGE_Close = procedure(); cdecl;
  PFNRD_CGE_SetRenderParams = procedure(uiViewWidth, uiViewHeight: cUInt32); cdecl;
  PFNRD_CGE_Render = procedure(); cdecl;
  PFNRD_CGE_OnIdle = procedure(); cdecl;

  PFNRD_CGE_OnMouseDown = procedure(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32); cdecl;
  PFNRD_CGE_OnMouseMove = procedure(x, y: cInt32; uiShift: cUInt32); cdecl;
  PFNRD_CGE_OnMouseUp = procedure(x, y: cInt32; bLeftBtn: cBool; uiShift: cUInt32); cdecl;
  PFNRD_CGE_OnMouseWheel = procedure(zDelta: cFloat); cdecl;

  PFNRD_CGE_LoadSceneFromFile = procedure(szFile: pcchar); cdecl;
  PFNRD_CGE_MoveToViewpoint = procedure(iViewpointIdx: cInt32; bAnimated: cBool); cdecl;

var
  g_hCgeDll: TLibHandle;
  pfrd_CGE_Init: PFNRD_CGE_Init;
  pfrd_CGE_Close: PFNRD_CGE_Close;
  pfrd_CGE_SetRenderParams: PFNRD_CGE_SetRenderParams;
  pfrd_CGE_Render: PFNRD_CGE_Render;
  pfrd_CGE_OnIdle: PFNRD_CGE_OnIdle;
  pfrd_CGE_OnMouseDown: PFNRD_CGE_OnMouseDown;
  pfrd_CGE_OnMouseMove: PFNRD_CGE_OnMouseMove;
  pfrd_CGE_OnMouseUp: PFNRD_CGE_OnMouseUp;
  pfrd_CGE_OnMouseWheel: PFNRD_CGE_OnMouseWheel;
  pfrd_CGE_LoadSceneFromFile: PFNRD_CGE_LoadSceneFromFile;
  pfrd_CGE_MoveToViewpoint: PFNRD_CGE_MoveToViewpoint;

//-----------------------------------------------------------------------------
procedure CGE_LoadLibrary();
begin
  g_hCgeDll := LoadLibrary('castlelib.' + SharedSuffix);
  if g_hCgeDll = 0 then Exit;

  pfrd_CGE_Init := PFNRD_CGE_Init(GetProcedureAddress(g_hCgeDll, 'CGE_Init'));
  pfrd_CGE_Close := PFNRD_CGE_Close(GetProcedureAddress(g_hCgeDll, 'CGE_Close'));
  pfrd_CGE_SetRenderParams := PFNRD_CGE_SetRenderParams(GetProcedureAddress(g_hCgeDll, 'CGE_SetRenderParams'));
  pfrd_CGE_Render := PFNRD_CGE_Render(GetProcedureAddress(g_hCgeDll, 'CGE_Render'));
  pfrd_CGE_OnIdle := PFNRD_CGE_OnIdle(GetProcedureAddress(g_hCgeDll, 'CGE_OnIdle'));
  pfrd_CGE_OnMouseDown := PFNRD_CGE_OnMouseDown(GetProcedureAddress(g_hCgeDll, 'CGE_OnMouseDown'));
  pfrd_CGE_OnMouseMove := PFNRD_CGE_OnMouseMove(GetProcedureAddress(g_hCgeDll, 'CGE_OnMouseMove'));
  pfrd_CGE_OnMouseUp := PFNRD_CGE_OnMouseUp(GetProcedureAddress(g_hCgeDll, 'CGE_OnMouseUp'));
  pfrd_CGE_OnMouseWheel :=PFNRD_CGE_OnMouseWheel(GetProcedureAddress(g_hCgeDll, 'CGE_OnMouseWheel'));
  pfrd_CGE_LoadSceneFromFile := PFNRD_CGE_LoadSceneFromFile(GetProcedureAddress(g_hCgeDll, 'CGE_LoadSceneFromFile'));
  pfrd_CGE_MoveToViewpoint := PFNRD_CGE_MoveToViewpoint(GetProcedureAddress(g_hCgeDll, 'CGE_MoveToViewpoint'));
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

procedure CGE_OnMouseWheel(zDelta: cFloat);
begin
  pfrd_CGE_OnMouseWheel(zDelta);
end;

procedure CGE_LoadSceneFromFile(szFile: pcchar);
begin
  pfrd_CGE_LoadSceneFromFile(szFile);
end;

procedure CGE_MoveToViewpoint(iViewpointIdx: cInt32; bAnimated: cBool);
begin
  pfrd_CGE_MoveToViewpoint(iViewpointIdx, bAnimated);
end;

begin
end.
