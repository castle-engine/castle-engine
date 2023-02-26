/*
  Copyright 2013-2014 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  This file is here as a wrapper, only to load the castleengine.dll for you and
  to resolve all exported functions.

  It works for multiplatform projects made in Qt (need to define QT_BUILD macro),
  or for projects made in WinAPI.

  Usage:
  1. Add castle_c_loader.cpp and castlelib.h into your project workspace. You may
     want to copy them instead in case you don't want to use the latest versions.

  2. Copy compiled dynamic library (castleengine.dll/.dylib) to your project
     folder (or to Debug and Release folders)

  3. Include castlelib.h in your source files, call CGE_LoadLibrary at
     the start of your program, and then call CGE_xxx functions as usual.
*/

#ifdef QT_BUILD
  #include <QLibrary>
#else          // suppose Windows build
  #include <windows.h>
#endif
#include "castleengine.h"

//-----------------------------------------------------------------------------
typedef void (CDECL *PFNRD_CGE_Initialize)(const char *applicationConfigDirectory);
typedef void (CDECL *PFNRD_CGE_Finalize)();
typedef void (CDECL *PFNRD_CGE_Open)(unsigned uiFlags, unsigned initialWidth, unsigned initialHeight, unsigned uiDpi);
typedef void (CDECL *PFNRD_CGE_Close)(bool quitWhenLastWindowClosed);
typedef void (CDECL *PFNRD_CGE_GetOpenGLInformation)(char *szBuffer, int nBufSize);

typedef void (CDECL *PFNRD_CGE_Resize)(unsigned uiViewWidth, unsigned uiViewHeight);
typedef void (CDECL *PFNRD_CGE_Render)();
typedef void (CDECL *PFNRD_CGE_SaveScreenshotToFile)(const char *szFile);
typedef void (CDECL *PFNRD_CGE_SetLibraryCallbackProc)(TCgeLibraryCallback pProc);
typedef void (CDECL *PFNRD_CGE_Update)();

typedef void (CDECL *PFNRD_CGE_MouseDown)(int x, int y, bool bLeftBtn, int nFingerIdx);
typedef void (CDECL *PFNRD_CGE_Motion)(int x, int y, int nFingerIdx);
typedef void (CDECL *PFNRD_CGE_MouseUp)(int x, int y, bool bLeftBtn, int nFingerIdx, bool trackReleased);
typedef void (CDECL *PFNRD_CGE_MouseWheel)(float zDelta, bool bVertical);

typedef void (CDECL *PFNRD_CGE_KeyDown)(int eKey);
typedef void (CDECL *PFNRD_CGE_KeyUp)(int eKey);

typedef void (CDECL *PFNRD_CGE_LoadSceneFromFile)(const char *szFile);

typedef int (CDECL *PFNRD_CGE_GetViewpointsCount)();
typedef void (CDECL *PFNRD_CGE_GetViewpointName)(int iViewpointIdx, char *szName, int nBufSize);
typedef void (CDECL *PFNRD_CGE_MoveToViewpoint)(int iViewpointIdx, bool bAnimated);
typedef void (CDECL *PFNRD_CGE_AddViewpointFromCurrentView)(const char *szName);
typedef void (CDECL *PFNRD_CGE_GetBoundingBox)(float *pfXMin, float *pfXMax, float *pfYMin, float *pfYMax, float *pfZMin, float *pfZMax);
typedef void (CDECL *PFNRD_CGE_GetViewCoords)(float *pfPosX, float *pfPosY, float *pfPosZ, float *pfDirX, float *pfDirY, float *pfDirZ,
                                                float *pfUpX, float *pfUpY, float *pfUpZ, float *pfGravX, float *pfGravY, float *pfGravZ);
typedef void (CDECL *PFNRD_CGE_MoveViewToCoords)(float fPosX, float fPosY, float fPosZ, float fDirX, float fDirY, float fDirZ,
                                                   float fUpX, float fUpY, float fUpZ, float fGravX, float fGravY, float fGravZ, bool bAnimated);

typedef int (CDECL *PFNRD_CGE_GetNavigationType)();
typedef void (CDECL *PFNRD_CGE_SetNavigationType)(int eNewType);
typedef void (CDECL *PFNRD_CGE_SetTouchInterface)(int eMode);
typedef void (CDECL *PFNRD_CGE_SetUserInterface)(bool bAutomaticTouchInterface);

typedef void (CDECL *PFNRD_CGE_SetVariableInt)(int eVar, int nValue);
typedef int (CDECL *PFNRD_CGE_GetVariableInt)(int eVar);
typedef void (CDECL *PFNRD_CGE_SetNodeFieldValue)(const char *szNodeName, const char *szFieldName,
                                                    float fVal1, float fVal2, float fVal3, float fVal4);

typedef void (CDECL *PFNRD_CGE_IncreaseSceneTime)(float fTimeS);

PFNRD_CGE_Initialize pfrd_CGE_Initialize = NULL;
PFNRD_CGE_Finalize pfrd_CGE_Finalize = NULL;
PFNRD_CGE_Open pfrd_CGE_Open = NULL;
PFNRD_CGE_Close pfrd_CGE_Close = NULL;
PFNRD_CGE_GetOpenGLInformation pfrd_CGE_GetOpenGLInformation = NULL;
PFNRD_CGE_Resize pfrd_CGE_Resize = NULL;
PFNRD_CGE_Render pfrd_CGE_Render = NULL;
PFNRD_CGE_SaveScreenshotToFile pfrd_CGE_SaveScreenshotToFile = NULL;
PFNRD_CGE_SetLibraryCallbackProc pfrd_CGE_SetLibraryCallbackProc = NULL;
PFNRD_CGE_Update pfrd_CGE_Update = NULL;
PFNRD_CGE_MouseDown pfrd_CGE_MouseDown = NULL;
PFNRD_CGE_Motion pfrd_CGE_Motion = NULL;
PFNRD_CGE_MouseUp pfrd_CGE_MouseUp = NULL;
PFNRD_CGE_MouseWheel pfrd_CGE_MouseWheel = NULL;
PFNRD_CGE_KeyDown pfrd_CGE_KeyDown = NULL;
PFNRD_CGE_KeyUp pfrd_CGE_KeyUp = NULL;
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
PFNRD_CGE_SetNodeFieldValue pfrd_CGE_SetNodeFieldValue = NULL;
PFNRD_CGE_IncreaseSceneTime pfrd_CGE_IncreaseSceneTime = NULL;

#ifdef QT_BUILD
//-----------------------------------------------------------------------------
QFunctionPointer cge_GetProc(QLibrary &rCgeLib, const char *symbol)
{
    return rCgeLib.resolve(symbol);
}
#else
FARPROC WINAPI cge_GetProc(HMODULE hCgeLib, const char *symbol)
{
    return GetProcAddress(hCgeLib, symbol);
}
#endif

//-----------------------------------------------------------------------------
void CGE_LoadLibrary()
{
#ifdef QT_BUILD
    QLibrary hCgeDll("castleengine");
    hCgeDll.load();
    if (!hCgeDll.isLoaded())
        return;
#else
    HMODULE hCgeDll = LoadLibrary("castleengine.dll");
    if (hCgeDll==NULL)
		return;
#endif

    pfrd_CGE_Initialize = (PFNRD_CGE_Initialize)cge_GetProc(hCgeDll, "CGE_Initialize");
    pfrd_CGE_Finalize = (PFNRD_CGE_Finalize)cge_GetProc(hCgeDll, "CGE_Finalize");
    pfrd_CGE_Open = (PFNRD_CGE_Open)cge_GetProc(hCgeDll, "CGE_Open");
    pfrd_CGE_Close = (PFNRD_CGE_Close)cge_GetProc(hCgeDll, "CGE_Close");
    pfrd_CGE_GetOpenGLInformation = (PFNRD_CGE_GetOpenGLInformation)cge_GetProc(hCgeDll, "CGE_GetOpenGLInformation");
    pfrd_CGE_Resize = (PFNRD_CGE_Resize)cge_GetProc(hCgeDll, "CGE_Resize");
    pfrd_CGE_Render = (PFNRD_CGE_Render)cge_GetProc(hCgeDll, "CGE_Render");
    pfrd_CGE_SaveScreenshotToFile = (PFNRD_CGE_SaveScreenshotToFile)cge_GetProc(hCgeDll, "CGE_SaveScreenshotToFile");
    pfrd_CGE_SetLibraryCallbackProc = (PFNRD_CGE_SetLibraryCallbackProc)cge_GetProc(hCgeDll, "CGE_SetLibraryCallbackProc");
    pfrd_CGE_Update = (PFNRD_CGE_Update)cge_GetProc(hCgeDll, "CGE_Update");
    pfrd_CGE_MouseDown = (PFNRD_CGE_MouseDown)cge_GetProc(hCgeDll, "CGE_MouseDown");
    pfrd_CGE_Motion = (PFNRD_CGE_Motion)cge_GetProc(hCgeDll, "CGE_Motion");
    pfrd_CGE_MouseUp = (PFNRD_CGE_MouseUp)cge_GetProc(hCgeDll, "CGE_MouseUp");
    pfrd_CGE_MouseWheel = (PFNRD_CGE_MouseWheel)cge_GetProc(hCgeDll, "CGE_MouseWheel");
    pfrd_CGE_KeyDown = (PFNRD_CGE_KeyDown)cge_GetProc(hCgeDll, "CGE_KeyDown");
    pfrd_CGE_KeyUp = (PFNRD_CGE_KeyUp)cge_GetProc(hCgeDll, "CGE_KeyUp");
    pfrd_CGE_LoadSceneFromFile = (PFNRD_CGE_LoadSceneFromFile)cge_GetProc(hCgeDll, "CGE_LoadSceneFromFile");
    pfrd_CGE_GetViewpointsCount = (PFNRD_CGE_GetViewpointsCount)cge_GetProc(hCgeDll, "CGE_GetViewpointsCount");
    pfrd_CGE_GetViewpointName = (PFNRD_CGE_GetViewpointName)cge_GetProc(hCgeDll, "CGE_GetViewpointName");
    pfrd_CGE_MoveToViewpoint = (PFNRD_CGE_MoveToViewpoint)cge_GetProc(hCgeDll, "CGE_MoveToViewpoint");
    pfrd_CGE_AddViewpointFromCurrentView = (PFNRD_CGE_AddViewpointFromCurrentView)cge_GetProc(hCgeDll, "CGE_AddViewpointFromCurrentView");
    pfrd_CGE_GetBoundingBox = (PFNRD_CGE_GetBoundingBox)cge_GetProc(hCgeDll, "CGE_GetBoundingBox");
    pfrd_CGE_GetViewCoords = (PFNRD_CGE_GetViewCoords)cge_GetProc(hCgeDll, "CGE_GetViewCoords");
    pfrd_CGE_MoveViewToCoords = (PFNRD_CGE_MoveViewToCoords)cge_GetProc(hCgeDll, "CGE_MoveViewToCoords");
    pfrd_CGE_GetNavigationType = (PFNRD_CGE_GetNavigationType)cge_GetProc(hCgeDll, "CGE_GetNavigationType");
    pfrd_CGE_SetNavigationType = (PFNRD_CGE_SetNavigationType)cge_GetProc(hCgeDll, "CGE_SetNavigationType");
    pfrd_CGE_SetTouchInterface = (PFNRD_CGE_SetTouchInterface)cge_GetProc(hCgeDll, "CGE_SetTouchInterface");
    pfrd_CGE_SetUserInterface = (PFNRD_CGE_SetUserInterface)cge_GetProc(hCgeDll, "CGE_SetUserInterface");
    pfrd_CGE_SetVariableInt = (PFNRD_CGE_SetVariableInt)cge_GetProc(hCgeDll, "CGE_SetVariableInt");
    pfrd_CGE_GetVariableInt = (PFNRD_CGE_GetVariableInt)cge_GetProc(hCgeDll, "CGE_GetVariableInt");
    pfrd_CGE_SetNodeFieldValue = (PFNRD_CGE_SetNodeFieldValue)cge_GetProc(hCgeDll, "CGE_SetNodeFieldValue");
    pfrd_CGE_IncreaseSceneTime = (PFNRD_CGE_IncreaseSceneTime)cge_GetProc(hCgeDll, "CGE_IncreaseSceneTime");
}

//-----------------------------------------------------------------------------
void CGE_Initialize(const char *applicationConfigDirectory)
{
	if (pfrd_CGE_Initialize!=NULL)
		(*pfrd_CGE_Initialize)(applicationConfigDirectory);
}

//-----------------------------------------------------------------------------
void CGE_Finalize()
{
	if (pfrd_CGE_Finalize!=NULL)
		(*pfrd_CGE_Finalize)();
}

//-----------------------------------------------------------------------------
void CGE_Open(unsigned uiFlags, unsigned initialWidth, unsigned initialHeight, unsigned uiDpi)
{
	if (pfrd_CGE_Open!=NULL)
		(*pfrd_CGE_Open)(uiFlags, initialWidth, initialHeight, uiDpi);
}

//-----------------------------------------------------------------------------
void CGE_Close(bool quitWhenLastWindowClosed)
{
	if (pfrd_CGE_Close!=NULL)
        (*pfrd_CGE_Close)(quitWhenLastWindowClosed);
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
void CGE_SetLibraryCallbackProc(TCgeLibraryCallback pProc)
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
void CGE_MouseDown(int x, int y, bool bLeftBtn, int nFingerIdx)
{
	if (pfrd_CGE_MouseDown!=NULL)
		(*pfrd_CGE_MouseDown)(x, y, bLeftBtn, nFingerIdx);
}

//-----------------------------------------------------------------------------
void CGE_Motion(int x, int y, int nFingerIdx)
{
	if (pfrd_CGE_Motion!=NULL)
		(*pfrd_CGE_Motion)(x, y, nFingerIdx);
}

//-----------------------------------------------------------------------------
void CGE_MouseUp(int x, int y, bool bLeftBtn, int nFingerIdx, bool trackReleased)
{
	if (pfrd_CGE_MouseUp!=NULL)
		(*pfrd_CGE_MouseUp)(x, y, bLeftBtn, nFingerIdx, trackReleased);
}

//-----------------------------------------------------------------------------
void CGE_MouseWheel(float zDelta, bool bVertical)
{
	if (pfrd_CGE_MouseWheel!=NULL)
		(*pfrd_CGE_MouseWheel)(zDelta, bVertical);
}

//-----------------------------------------------------------------------------
void CGE_KeyDown(int /*ECgeKey*/ eKey)
{
    if (pfrd_CGE_KeyDown!=NULL)
        (*pfrd_CGE_KeyDown)(eKey);
}

//-----------------------------------------------------------------------------
void CGE_KeyUp(int /*ECgeKey*/ eKey)
{
    if (pfrd_CGE_KeyUp!=NULL)
        (*pfrd_CGE_KeyUp)(eKey);
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
                          float fUpX, float fUpY, float fUpZ, float fGravX, float fGravY, float fGravZ, bool bAnimated)
{
	if (pfrd_CGE_MoveViewToCoords!=NULL)
		(*pfrd_CGE_MoveViewToCoords)(fPosX, fPosY, fPosZ, fDirX, fDirY, fDirZ, fUpX, fUpY, fUpZ, fGravX, fGravY, fGravZ, bAnimated);
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
void CGE_SetUserInterface(bool bAutomaticTouchInterface)
{
    if (pfrd_CGE_SetUserInterface!=NULL)
        (*pfrd_CGE_SetUserInterface)(bAutomaticTouchInterface);
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

//-----------------------------------------------------------------------------
void CGE_SetNodeFieldValue(const char *szNodeName, const char *szFieldName,
                           float fVal1, float fVal2, float fVal3, float fVal4)
{
    if (pfrd_CGE_SetNodeFieldValue!=NULL)
        (*pfrd_CGE_SetNodeFieldValue)(szNodeName, szFieldName, fVal1, fVal2, fVal3, fVal4);
}

//-----------------------------------------------------------------------------
void CGE_IncreaseSceneTime(float fTimeS)
{
	if (pfrd_CGE_IncreaseSceneTime!=NULL)
        (*pfrd_CGE_IncreaseSceneTime)(fTimeS);
}
