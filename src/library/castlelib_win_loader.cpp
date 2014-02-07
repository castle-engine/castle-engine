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

typedef void (__cdecl *PFNRD_CGE_Open)(unsigned uiFlags);
typedef void (__cdecl *PFNRD_CGE_Close)();
typedef void (__cdecl *PFNRD_CGE_GetOpenGLInformation)(char *szBuffer, int nBufSize);

typedef void (__cdecl *PFNRD_CGE_Resize)(unsigned uiViewWidth, unsigned uiViewHeight);
typedef void (__cdecl *PFNRD_CGE_Render)();
typedef void (__cdecl *PFNRD_CGE_SaveScreenshotToFile)(const char *szFile);
typedef void (__cdecl *PFNRD_CGE_SetLibraryCallbackProc)(TCgeLibraryCallbackProc pProc);
typedef void (__cdecl *PFNRD_CGE_Update)();

typedef void (__cdecl *PFNRD_CGE_MouseDown)(int x, int y, bool bLeftBtn);
typedef void (__cdecl *PFNRD_CGE_MouseMove)(int x, int y);
typedef void (__cdecl *PFNRD_CGE_MouseUp)(int x, int y, bool bLeftBtn);
typedef void (__cdecl *PFNRD_CGE_MouseWheel)(float zDelta, bool bVertical);

typedef void (__cdecl *PFNRD_CGE_LoadSceneFromFile)(const char *szFile);

typedef int (__cdecl *PFNRD_CGE_GetViewpointsCount)();
typedef void (__cdecl *PFNRD_CGE_GetViewpointName)(int iViewpointIdx, char *szName, int nBufSize);
typedef void (__cdecl *PFNRD_CGE_MoveToViewpoint)(int iViewpointIdx, bool bAnimated);
typedef void (__cdecl *PFNRD_CGE_AddViewpointFromCurrentView)(const char *szName);
typedef void (__cdecl *PFNRD_CGE_GetBoundingBox)(float *pfXMin, float *pfXMax, float *pfYMin, float *pfYMax, float *pfZMin, float *pfZMax);
typedef void (__cdecl *PFNRD_CGE_GetViewCoords)(float *pfPosX, float *pfPosY, float *pfPosZ, float *pfDirX, float *pfDirY, float *pfDirZ, 
                                                float *pfUpX, float *pfUpY, float *pfUpZ, float *pfGravX, float *pfGravY, float *pfGravZ);
typedef void (__cdecl *PFNRD_CGE_MoveViewToCoords)(float fPosX, float fPosY, float fPosZ, float fDirX, float fDirY, float fDirZ, 
                                                   float fUpX, float fUpY, float fUpZ, float fGravX, float fGravY, float fGravZ);

typedef int (__cdecl *PFNRD_CGE_GetNavigationType)();
typedef void (__cdecl *PFNRD_CGE_SetNavigationType)(int eNewType);
typedef void (__cdecl *PFNRD_CGE_SetTouchInterface)(int eMode);
typedef void (__cdecl *PFNRD_CGE_SetUserInterface)(bool bAutomaticTouchInterface, int nDpi);

typedef void (__cdecl *PFNRD_CGE_SetVariableInt)(int eVar, int nValue);
typedef int (__cdecl *PFNRD_CGE_GetVariableInt)(int eVar);


PFNRD_CGE_Open pfrd_CGE_Open = NULL;
PFNRD_CGE_Close pfrd_CGE_Close = NULL;
PFNRD_CGE_GetOpenGLInformation pfrd_CGE_GetOpenGLInformation = NULL;
PFNRD_CGE_Resize pfrd_CGE_Resize = NULL;
PFNRD_CGE_Render pfrd_CGE_Render = NULL;
PFNRD_CGE_SaveScreenshotToFile pfrd_CGE_SaveScreenshotToFile = NULL;
PFNRD_CGE_SetLibraryCallbackProc pfrd_CGE_SetLibraryCallbackProc = NULL;
PFNRD_CGE_Update pfrd_CGE_Update = NULL;
PFNRD_CGE_MouseDown pfrd_CGE_MouseDown = NULL;
PFNRD_CGE_MouseMove pfrd_CGE_MouseMove = NULL;
PFNRD_CGE_MouseUp pfrd_CGE_MouseUp = NULL;
PFNRD_CGE_MouseWheel pfrd_CGE_MouseWheel = NULL;
PFNRD_CGE_LoadSceneFromFile pfrd_CGE_LoadSceneFromFile = NULL;
PFNRD_CGE_GetViewpointsCount pfrd_CGE_GetViewpointsCount = NULL;
PFNRD_CGE_GetViewpointName pfrd_CGE_GetViewpointName = NULL;
PFNRD_CGE_MoveToViewpoint pfrd_CGE_MoveToViewpoint = NULL;
PFNRD_CGE_AddViewpointFromCurrentView pfrd_CGE_AddViewpointFromCurrentView = NULL;
PFNRD_CGE_GetBoundingBox pfrd_CGE_GetBoundingBox = NULL;
PFNRD_CGE_GetViewCoords pfrd_CGE_GetViewCoords = NULL;
PFNRD_CGE_MoveViewToCoords pfrd_CGE_MoveViewToCoords = NULL;
PFNRD_CGE_GetNavigationType pfrd_CGE_GetNavigationType = NULL;
PFNRD_CGE_SetNavigationType pfrd_CGE_SetNavigationType = NULL;
PFNRD_CGE_SetTouchInterface pfrd_CGE_SetTouchInterface = NULL;
PFNRD_CGE_SetUserInterface pfrd_CGE_SetUserInterface = NULL;
PFNRD_CGE_SetVariableInt pfrd_CGE_SetVariableInt = NULL;
PFNRD_CGE_GetVariableInt pfrd_CGE_GetVariableInt = NULL;

//-----------------------------------------------------------------------------
void CGE_LoadLibrary()
{
	g_hCgeDll = LoadLibrary("castleengine.dll");
	if (g_hCgeDll==NULL)
		return;

	pfrd_CGE_Open = (PFNRD_CGE_Open)GetProcAddress(g_hCgeDll, "CGE_Open");
	pfrd_CGE_Close = (PFNRD_CGE_Close)GetProcAddress(g_hCgeDll, "CGE_Close");
	pfrd_CGE_GetOpenGLInformation = (PFNRD_CGE_GetOpenGLInformation)GetProcAddress(g_hCgeDll, "CGE_GetOpenGLInformation");
	pfrd_CGE_Resize = (PFNRD_CGE_Resize)GetProcAddress(g_hCgeDll, "CGE_Resize");
	pfrd_CGE_Render = (PFNRD_CGE_Render)GetProcAddress(g_hCgeDll, "CGE_Render");
	pfrd_CGE_SaveScreenshotToFile = (PFNRD_CGE_SaveScreenshotToFile)GetProcAddress(g_hCgeDll, "CGE_SaveScreenshotToFile");
	pfrd_CGE_SetLibraryCallbackProc = (PFNRD_CGE_SetLibraryCallbackProc)GetProcAddress(g_hCgeDll, "CGE_SetLibraryCallbackProc");
	pfrd_CGE_Update = (PFNRD_CGE_Update)GetProcAddress(g_hCgeDll, "CGE_Update");
	pfrd_CGE_MouseDown = (PFNRD_CGE_MouseDown)GetProcAddress(g_hCgeDll, "CGE_MouseDown");
	pfrd_CGE_MouseMove = (PFNRD_CGE_MouseMove)GetProcAddress(g_hCgeDll, "CGE_MouseMove");
	pfrd_CGE_MouseUp = (PFNRD_CGE_MouseUp)GetProcAddress(g_hCgeDll, "CGE_MouseUp");
	pfrd_CGE_MouseWheel =(PFNRD_CGE_MouseWheel) GetProcAddress(g_hCgeDll, "CGE_MouseWheel");
	pfrd_CGE_LoadSceneFromFile = (PFNRD_CGE_LoadSceneFromFile)GetProcAddress(g_hCgeDll, "CGE_LoadSceneFromFile");
	pfrd_CGE_GetViewpointsCount = (PFNRD_CGE_GetViewpointsCount)GetProcAddress(g_hCgeDll, "CGE_GetViewpointsCount");
	pfrd_CGE_GetViewpointName = (PFNRD_CGE_GetViewpointName)GetProcAddress(g_hCgeDll, "CGE_GetViewpointName");
	pfrd_CGE_MoveToViewpoint = (PFNRD_CGE_MoveToViewpoint)GetProcAddress(g_hCgeDll, "CGE_MoveToViewpoint");
    pfrd_CGE_AddViewpointFromCurrentView = (PFNRD_CGE_AddViewpointFromCurrentView)GetProcAddress(g_hCgeDll, "CGE_AddViewpointFromCurrentView");
	pfrd_CGE_GetBoundingBox = (PFNRD_CGE_GetBoundingBox)GetProcAddress(g_hCgeDll, "CGE_GetBoundingBox");
	pfrd_CGE_GetViewCoords = (PFNRD_CGE_GetViewCoords)GetProcAddress(g_hCgeDll, "CGE_GetViewCoords");
	pfrd_CGE_MoveViewToCoords = (PFNRD_CGE_MoveViewToCoords)GetProcAddress(g_hCgeDll, "CGE_MoveViewToCoords");
	pfrd_CGE_GetNavigationType = (PFNRD_CGE_GetNavigationType)GetProcAddress(g_hCgeDll, "CGE_GetNavigationType");
	pfrd_CGE_SetNavigationType = (PFNRD_CGE_SetNavigationType)GetProcAddress(g_hCgeDll, "CGE_SetNavigationType");
	pfrd_CGE_SetTouchInterface = (PFNRD_CGE_SetTouchInterface)GetProcAddress(g_hCgeDll, "CGE_SetTouchInterface");
	pfrd_CGE_SetUserInterface = (PFNRD_CGE_SetUserInterface)GetProcAddress(g_hCgeDll, "CGE_SetUserInterface");
	pfrd_CGE_SetVariableInt = (PFNRD_CGE_SetVariableInt)GetProcAddress(g_hCgeDll, "CGE_SetVariableInt");
	pfrd_CGE_GetVariableInt = (PFNRD_CGE_GetVariableInt)GetProcAddress(g_hCgeDll, "CGE_GetVariableInt");
}

//-----------------------------------------------------------------------------
void CGE_Open(unsigned uiFlags)
{
	if (pfrd_CGE_Open!=NULL)
		(*pfrd_CGE_Open)(uiFlags);
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
void CGE_Resize(unsigned uiViewWidth, unsigned uiViewHeight)
{
	if (pfrd_CGE_Resize!=NULL)
		(*pfrd_CGE_Resize)(uiViewWidth, uiViewHeight);
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
void CGE_Update()
{
	if (pfrd_CGE_Update!=NULL)
		(*pfrd_CGE_Update)();
}

//-----------------------------------------------------------------------------
void CGE_MouseDown(int x, int y, bool bLeftBtn)
{
	if (pfrd_CGE_MouseDown!=NULL)
		(*pfrd_CGE_MouseDown)(x, y, bLeftBtn);
}

//-----------------------------------------------------------------------------
void CGE_MouseMove(int x, int y)
{
	if (pfrd_CGE_MouseMove!=NULL)
		(*pfrd_CGE_MouseMove)(x, y);
}

//-----------------------------------------------------------------------------
void CGE_MouseUp(int x, int y, bool bLeftBtn)
{
	if (pfrd_CGE_MouseUp!=NULL)
		(*pfrd_CGE_MouseUp)(x, y, bLeftBtn);
}

//-----------------------------------------------------------------------------
void CGE_MouseWheel(float zDelta, bool bVertical)
{
	if (pfrd_CGE_MouseWheel!=NULL)
		(*pfrd_CGE_MouseWheel)(zDelta, bVertical);
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
void CGE_AddViewpointFromCurrentView(const char *szName)
{
	if (pfrd_CGE_AddViewpointFromCurrentView!=NULL)
		(*pfrd_CGE_AddViewpointFromCurrentView)(szName);
}

//-----------------------------------------------------------------------------
void CGE_GetBoundingBox(float *pfXMin, float *pfXMax, float *pfYMin, float *pfYMax, float *pfZMin, float *pfZMax)
{
	if (pfrd_CGE_GetBoundingBox!=NULL)
		(*pfrd_CGE_GetBoundingBox)(pfXMin, pfXMax, pfYMin, pfYMax, pfZMin, pfZMax);
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
int CGE_GetNavigationType()
{
	if (pfrd_CGE_GetNavigationType!=NULL)
		return (*pfrd_CGE_GetNavigationType)();
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
void CGE_SetTouchInterface(int /*ECgeTouchCtlInterface*/ eMode)
{
	if (pfrd_CGE_SetTouchInterface!=NULL)
        (*pfrd_CGE_SetTouchInterface)(eMode);
}

//-----------------------------------------------------------------------------
void CGE_SetUserInterface(bool bAutomaticTouchInterface, int nDpi)
{
	if (pfrd_CGE_SetUserInterface!=NULL)
        (*pfrd_CGE_SetUserInterface)(bAutomaticTouchInterface, nDpi);
}

//-----------------------------------------------------------------------------
void CGE_SetVariableInt(int /*ECgeVariable*/ eVar, int nValue)
{
    if (pfrd_CGE_SetVariableInt!=NULL)
        (*pfrd_CGE_SetVariableInt)(eVar, nValue);
}

//-----------------------------------------------------------------------------
int CGE_GetVariableInt(int /*ECgeVariable*/ eVar)
{
    if (pfrd_CGE_GetVariableInt!=NULL)
        return (*pfrd_CGE_GetVariableInt)(eVar);
    else
        return -1;
}
