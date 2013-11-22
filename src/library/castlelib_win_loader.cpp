/*
  Copyright 2013 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  This file is here as a wrapper, only to load the CastleLib.dll for you and
  to call GetProcAddress for all exported functions.

  Usage:
  1. Copy castle_win_loader.cpp and castlelib.h into your project folder and
     add them to your project workspace.

  2. Copy castleengine.dll to your project folder (or to Debug and Release folders)

  3. Include castlelib.h in your source files, call CGE_LoadLibrary at
     the start of your program, and then call CGE_xxx functions as usual.
*/

#include <windows.h>
#include "castleengine.h"

//-----------------------------------------------------------------------------
HMODULE g_hCgeDll = NULL;

typedef void (__cdecl *PFNRD_CGE_Init)();
typedef void (__cdecl *PFNRD_CGE_Close)();
typedef void (__cdecl *PFNRD_CGE_GetOpenGLInformation)(char *szBuffer, int nBufSize);

typedef void (__cdecl *PFNRD_CGE_SetRenderParams)(unsigned uiViewWidth, unsigned uiViewHeight);
typedef void (__cdecl *PFNRD_CGE_Render)();
typedef void (__cdecl *PFNRD_CGE_SaveScreenshotToFile)(const char *szFile);
typedef void (__cdecl *PFNRD_CGE_SetLibraryCallbackProc)(TCgeLibraryCallbackProc pProc);
typedef void (__cdecl *PFNRD_CGE_OnIdle)();

typedef void (__cdecl *PFNRD_CGE_OnMouseDown)(int x, int y, bool bLeftBtn, unsigned uiShift);
typedef void (__cdecl *PFNRD_CGE_OnMouseMove)(int x, int y, unsigned uiShift);
typedef void (__cdecl *PFNRD_CGE_OnMouseUp)(int x, int y, bool bLeftBtn, unsigned uiShift);
typedef void (__cdecl *PFNRD_CGE_OnMouseWheel)(float zDelta, bool bVertical, unsigned uiShift);

typedef void (__cdecl *PFNRD_CGE_LoadSceneFromFile)(const char *szFile);

typedef int (__cdecl *PFNRD_CGE_GetViewpointsCount)();
typedef void (__cdecl *PFNRD_CGE_GetViewpointName)(int iViewpointIdx, char *szName, int nBufSize);
typedef void (__cdecl *PFNRD_CGE_MoveToViewpoint)(int iViewpointIdx, bool bAnimated);
typedef void (__cdecl *PFNRD_CGE_GetViewCoords)(float *pfPosX, float *pfPosY, float *pfPosZ, float *pfDirX, float *pfDirY, float *pfDirZ, 
                                                float *pfUpX, float *pfUpY, float *pfUpZ, float *pfGravX, float *pfGravY, float *pfGravZ);
typedef void (__cdecl *PFNRD_CGE_MoveViewToCoords)(float fPosX, float fPosY, float fPosZ, float fDirX, float fDirY, float fDirZ, 
                                                   float fUpX, float fUpY, float fUpZ, float fGravX, float fGravY, float fGravZ);

typedef int (__cdecl *PFNRD_CGE_GetCurrentNavigationType)();
typedef void (__cdecl *PFNRD_CGE_SetNavigationType)(int eNewType);
typedef void (__cdecl *PFNRD_CGE_UpdateTouchInterface)(int eMode);
typedef void (__cdecl *PFNRD_CGE_SetUserInterfaceInfo)(int eMode, int nDpi);


PFNRD_CGE_Init pfrd_CGE_Init = NULL;
PFNRD_CGE_Close pfrd_CGE_Close = NULL;
PFNRD_CGE_GetOpenGLInformation pfrd_CGE_GetOpenGLInformation = NULL;
PFNRD_CGE_SetRenderParams pfrd_CGE_SetRenderParams = NULL;
PFNRD_CGE_Render pfrd_CGE_Render = NULL;
PFNRD_CGE_SaveScreenshotToFile pfrd_CGE_SaveScreenshotToFile = NULL;
PFNRD_CGE_SetLibraryCallbackProc pfrd_CGE_SetLibraryCallbackProc = NULL;
PFNRD_CGE_OnIdle pfrd_CGE_OnIdle = NULL;
PFNRD_CGE_OnMouseDown pfrd_CGE_OnMouseDown = NULL;
PFNRD_CGE_OnMouseMove pfrd_CGE_OnMouseMove = NULL;
PFNRD_CGE_OnMouseUp pfrd_CGE_OnMouseUp = NULL;
PFNRD_CGE_OnMouseWheel pfrd_CGE_OnMouseWheel = NULL;
PFNRD_CGE_LoadSceneFromFile pfrd_CGE_LoadSceneFromFile = NULL;
PFNRD_CGE_GetViewpointsCount pfrd_CGE_GetViewpointsCount = NULL;
PFNRD_CGE_GetViewpointName pfrd_CGE_GetViewpointName = NULL;
PFNRD_CGE_MoveToViewpoint pfrd_CGE_MoveToViewpoint = NULL;
PFNRD_CGE_GetViewCoords pfrd_CGE_GetViewCoords = NULL;
PFNRD_CGE_MoveViewToCoords pfrd_CGE_MoveViewToCoords = NULL;
PFNRD_CGE_GetCurrentNavigationType pfrd_CGE_GetCurrentNavigationType = NULL;
PFNRD_CGE_SetNavigationType pfrd_CGE_SetNavigationType = NULL;
PFNRD_CGE_UpdateTouchInterface pfrd_CGE_UpdateTouchInterface = NULL;
PFNRD_CGE_SetUserInterfaceInfo pfrd_CGE_SetUserInterfaceInfo = NULL;

//-----------------------------------------------------------------------------
void CGE_LoadLibrary()
{
	g_hCgeDll = LoadLibrary("castleengine.dll");
	if (g_hCgeDll==NULL)
		return;

	pfrd_CGE_Init = (PFNRD_CGE_Init)GetProcAddress(g_hCgeDll, "CGE_Init");
	pfrd_CGE_Close = (PFNRD_CGE_Close)GetProcAddress(g_hCgeDll, "CGE_Close");
	pfrd_CGE_GetOpenGLInformation = (PFNRD_CGE_GetOpenGLInformation)GetProcAddress(g_hCgeDll, "CGE_GetOpenGLInformation");
	pfrd_CGE_SetRenderParams = (PFNRD_CGE_SetRenderParams)GetProcAddress(g_hCgeDll, "CGE_SetRenderParams");
	pfrd_CGE_Render = (PFNRD_CGE_Render)GetProcAddress(g_hCgeDll, "CGE_Render");
	pfrd_CGE_SaveScreenshotToFile = (PFNRD_CGE_SaveScreenshotToFile)GetProcAddress(g_hCgeDll, "CGE_SaveScreenshotToFile");
	pfrd_CGE_SetLibraryCallbackProc = (PFNRD_CGE_SetLibraryCallbackProc)GetProcAddress(g_hCgeDll, "CGE_SetLibraryCallbackProc");
	pfrd_CGE_OnIdle = (PFNRD_CGE_OnIdle)GetProcAddress(g_hCgeDll, "CGE_OnIdle");
	pfrd_CGE_OnMouseDown = (PFNRD_CGE_OnMouseDown)GetProcAddress(g_hCgeDll, "CGE_OnMouseDown");
	pfrd_CGE_OnMouseMove = (PFNRD_CGE_OnMouseMove)GetProcAddress(g_hCgeDll, "CGE_OnMouseMove");
	pfrd_CGE_OnMouseUp = (PFNRD_CGE_OnMouseUp)GetProcAddress(g_hCgeDll, "CGE_OnMouseUp");
	pfrd_CGE_OnMouseWheel =(PFNRD_CGE_OnMouseWheel) GetProcAddress(g_hCgeDll, "CGE_OnMouseWheel");
	pfrd_CGE_LoadSceneFromFile = (PFNRD_CGE_LoadSceneFromFile)GetProcAddress(g_hCgeDll, "CGE_LoadSceneFromFile");
	pfrd_CGE_GetViewpointsCount = (PFNRD_CGE_GetViewpointsCount)GetProcAddress(g_hCgeDll, "CGE_GetViewpointsCount");
	pfrd_CGE_GetViewpointName = (PFNRD_CGE_GetViewpointName)GetProcAddress(g_hCgeDll, "CGE_GetViewpointName");
	pfrd_CGE_MoveToViewpoint = (PFNRD_CGE_MoveToViewpoint)GetProcAddress(g_hCgeDll, "CGE_MoveToViewpoint");
	pfrd_CGE_GetViewCoords = (PFNRD_CGE_GetViewCoords)GetProcAddress(g_hCgeDll, "CGE_GetViewCoords");
	pfrd_CGE_MoveViewToCoords = (PFNRD_CGE_MoveViewToCoords)GetProcAddress(g_hCgeDll, "CGE_MoveViewToCoords");
	pfrd_CGE_GetCurrentNavigationType = (PFNRD_CGE_GetCurrentNavigationType)GetProcAddress(g_hCgeDll, "CGE_GetCurrentNavigationType");
	pfrd_CGE_SetNavigationType = (PFNRD_CGE_SetNavigationType)GetProcAddress(g_hCgeDll, "CGE_SetNavigationType");
	pfrd_CGE_UpdateTouchInterface = (PFNRD_CGE_UpdateTouchInterface)GetProcAddress(g_hCgeDll, "CGE_UpdateTouchInterface");
	pfrd_CGE_SetUserInterfaceInfo = (PFNRD_CGE_SetUserInterfaceInfo)GetProcAddress(g_hCgeDll, "CGE_SetUserInterfaceInfo");
}

//-----------------------------------------------------------------------------
void CGE_Init()
{
	if (pfrd_CGE_Init!=NULL)
		(*pfrd_CGE_Init)();
}

//-----------------------------------------------------------------------------
void CGE_Close()
{
	if (pfrd_CGE_Close!=NULL)
		(*pfrd_CGE_Close)();
}

//-----------------------------------------------------------------------------
void CGE_GetOpenGLInformation(char *szBuffer, int nBufSize)
{
	if (pfrd_CGE_GetOpenGLInformation!=NULL)
        (*pfrd_CGE_GetOpenGLInformation)(szBuffer, nBufSize);
}

//-----------------------------------------------------------------------------
void CGE_SetRenderParams(unsigned uiViewWidth, unsigned uiViewHeight)
{
	if (pfrd_CGE_SetRenderParams!=NULL)
		(*pfrd_CGE_SetRenderParams)(uiViewWidth, uiViewHeight);
}

//-----------------------------------------------------------------------------
void CGE_Render()
{
	if (pfrd_CGE_Render!=NULL)
		(*pfrd_CGE_Render)();
}

//-----------------------------------------------------------------------------
void CGE_SaveScreenshotToFile(const char *szFile)
{
	if (pfrd_CGE_SaveScreenshotToFile!=NULL)
		(*pfrd_CGE_SaveScreenshotToFile)(szFile);
}

//-----------------------------------------------------------------------------
void CGE_SetLibraryCallbackProc(TCgeLibraryCallbackProc pProc)
{
	if (pfrd_CGE_SetLibraryCallbackProc!=NULL)
		(*pfrd_CGE_SetLibraryCallbackProc)(pProc);
}

//-----------------------------------------------------------------------------
void CGE_OnIdle()
{
	if (pfrd_CGE_OnIdle!=NULL)
		(*pfrd_CGE_OnIdle)();
}

//-----------------------------------------------------------------------------
void CGE_OnMouseDown(int x, int y, bool bLeftBtn, unsigned uiShift)
{
	if (pfrd_CGE_OnMouseDown!=NULL)
		(*pfrd_CGE_OnMouseDown)(x, y, bLeftBtn, uiShift);
}

//-----------------------------------------------------------------------------
void CGE_OnMouseMove(int x, int y, unsigned uiShift)
{
	if (pfrd_CGE_OnMouseMove!=NULL)
		(*pfrd_CGE_OnMouseMove)(x, y, uiShift);
}

//-----------------------------------------------------------------------------
void CGE_OnMouseUp(int x, int y, bool bLeftBtn, unsigned uiShift)
{
	if (pfrd_CGE_OnMouseUp!=NULL)
		(*pfrd_CGE_OnMouseUp)(x, y, bLeftBtn, uiShift);
}

//-----------------------------------------------------------------------------
void CGE_OnMouseWheel(float zDelta, bool bVertical, unsigned uiShift)
{
	if (pfrd_CGE_OnMouseWheel!=NULL)
		(*pfrd_CGE_OnMouseWheel)(zDelta, bVertical, uiShift);
}

//-----------------------------------------------------------------------------
void CGE_LoadSceneFromFile(const char *szFile)
{
	if (pfrd_CGE_LoadSceneFromFile!=NULL)
		(*pfrd_CGE_LoadSceneFromFile)(szFile);
}

//-----------------------------------------------------------------------------
int CGE_GetViewpointsCount()
{
	if (pfrd_CGE_MoveToViewpoint!=NULL)
		return (*pfrd_CGE_GetViewpointsCount)();
    else
        return 0;
}

//-----------------------------------------------------------------------------
void CGE_GetViewpointName(int iViewpointIdx, char *szName, int nBufSize)
{
	if (pfrd_CGE_GetViewpointName!=NULL)
		(*pfrd_CGE_GetViewpointName)(iViewpointIdx, szName, nBufSize);
}

//-----------------------------------------------------------------------------
void CGE_MoveToViewpoint(int iViewpointIdx, bool bAnimated)
{
	if (pfrd_CGE_MoveToViewpoint!=NULL)
		(*pfrd_CGE_MoveToViewpoint)(iViewpointIdx, bAnimated);
}

//-----------------------------------------------------------------------------
void CGE_GetViewCoords(float *pfPosX, float *pfPosY, float *pfPosZ, float *pfDirX, float *pfDirY, float *pfDirZ, 
                              float *pfUpX, float *pfUpY, float *pfUpZ, float *pfGravX, float *pfGravY, float *pfGravZ)
{
	if (pfrd_CGE_GetViewCoords!=NULL)
		(*pfrd_CGE_GetViewCoords)(pfPosX, pfPosY, pfPosZ, pfDirX, pfDirY, pfDirZ, pfUpX, pfUpY, pfUpZ, pfGravX, pfGravY, pfGravZ);
}

//-----------------------------------------------------------------------------
void CGE_MoveViewToCoords(float fPosX, float fPosY, float fPosZ, float fDirX, float fDirY, float fDirZ, 
                                 float fUpX, float fUpY, float fUpZ, float fGravX, float fGravY, float fGravZ)
{
	if (pfrd_CGE_MoveViewToCoords!=NULL)
		(*pfrd_CGE_MoveViewToCoords)(fPosX, fPosY, fPosZ, fDirX, fDirY, fDirZ, fUpX, fUpY, fUpZ, fGravX, fGravY, fGravZ);
}

//-----------------------------------------------------------------------------
int CGE_GetCurrentNavigationType()
{
	if (pfrd_CGE_GetCurrentNavigationType!=NULL)
		return (*pfrd_CGE_GetCurrentNavigationType)();
    else
        return 0;
}

//-----------------------------------------------------------------------------
void CGE_SetNavigationType(int /*ECgeNavigationType*/ eNewType)
{
	if (pfrd_CGE_SetNavigationType!=NULL)
        (*pfrd_CGE_SetNavigationType)(eNewType);
}

//-----------------------------------------------------------------------------
void CGE_UpdateTouchInterface(int /*ECgeTouchCtlInterface*/ eMode)
{
	if (pfrd_CGE_UpdateTouchInterface!=NULL)
        (*pfrd_CGE_UpdateTouchInterface)(eMode);
}

//-----------------------------------------------------------------------------
void CGE_SetUserInterfaceInfo(int /*ECgeUserInterface*/ eMode, int nDpi)
{
	if (pfrd_CGE_SetUserInterfaceInfo!=NULL)
        (*pfrd_CGE_SetUserInterfaceInfo)(eMode, nDpi);
}
