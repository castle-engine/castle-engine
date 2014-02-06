/*
  Copyright 2013 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  This include file should be used in C/C++ projects. It contains all
  functions exported from the library.

  Windows: add castlelib_win_loader.cpp file to your project and call
      CGE_LoadLibrary first to enable to Castle Game Engine funtions.

  iOS: castlelib will be statically linked to your app, so just include
      this file and you are ready to go.
*/


#ifndef CGE_LIBRARY_INCLUDED
#define CGE_LIBRARY_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

enum ECgeOpenFlag
{
    ecgeofSaveMemory = 1,
};

enum ECgeNavigationType
{
    ecgenavWalk      = 0,
    ecgenavFly       = 1,
    ecgenavExamine   = 2,
    ecgenavTurntable = 3,
    ecgenavNone      = 4,
};

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
};

typedef int (__cdecl *TCgeLibraryCallbackProc)(int /*ECgeLibCallbackCode*/eCode, int iParam1, int iParam2);


//-----------------------------------------------------------------------------
extern void CGE_LoadLibrary();	// function defined in the loader CPP file

//-----------------------------------------------------------------------------
extern void CGE_Open(unsigned uiFlags);     // init the library, this function must be called first (required). Flags is any combination of ECgeOpenFlag
extern void CGE_Close();
extern void CGE_GetOpenGLInformation(char *szBuffer, int nBufSize);        // szBuffer is filled inside the function with max size of nBufSize
extern void CGE_SetUserInterface(bool bAutomaticTouchInterface, int nDpi); // should be called at the start of the program. Touch interface controls will be updated automatically then.

extern void CGE_Resize(unsigned uiViewWidth, unsigned uiViewHeight);       // let the library know about the viewport size (required)
extern void CGE_Render();                                                  // paints the 3d scene into the context
extern void CGE_SaveScreenshotToFile(const char *szFile);
extern void CGE_SetLibraryCallbackProc(TCgeLibraryCallbackProc pProc);     // set callback function
extern void CGE_Update();                                                  // let the 3d engine perform the animations, etc

extern void CGE_MouseDown(int x, int y, bool bLeftBtn);
extern void CGE_MouseMove(int x, int y);
extern void CGE_MouseUp(int x, int y, bool bLeftBtn);
extern void CGE_MouseWheel(float zDelta, bool bVertical);

extern void CGE_LoadSceneFromFile(const char *szFile);                     // name od the file has to be utf-8 encoded

extern int CGE_GetViewpointsCount();
extern void CGE_GetViewpointName(int iViewpointIdx, char *szName, int nBufSize);    // szName is buffer of size nBufSize, and is filled with utf-8 encoded string
extern void CGE_MoveToViewpoint(int iViewpointIdx, bool bAnimated);
extern void CGE_AddViewpointFromCurrentView(const char *szName);

extern void CGE_GetViewCoords(float *pfPosX, float *pfPosY, float *pfPosZ, float *pfDirX, float *pfDirY, float *pfDirZ,
                              float *pfUpX, float *pfUpY, float *pfUpZ, float *pfGravX, float *pfGravY, float *pfGravZ);
extern void CGE_MoveViewToCoords(float fPosX, float fPosY, float fPosZ, float fDirX, float fDirY, float fDirZ,
                                 float fUpX, float fUpY, float fUpZ, float fGravX, float fGravY, float fGravZ);

extern int CGE_GetNavigationType();
extern void CGE_SetNavigationType(int /*ECgeNavigationType*/ eNewType);

extern void CGE_SetTouchInterface(int /*ECgeTouchCtlInterface*/ eMode);

extern void CGE_SetWalkHeadBobbing(bool bOn);   // activates the walking effect (default: on)
extern void CGE_SetEffectSsao(bool bOn);        // activates SSAO (screen space ambient occlusion) effect (default: off)

#ifdef __cplusplus
}
#endif

#endif //CGE_LIBRARY_INCLUDED
