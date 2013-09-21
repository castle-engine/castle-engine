/*
  Copyright 2008-2013 Jan Adamec, Michalis Kamburelis.

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

enum ECgeShiftState
{
    ecgessShift = 1,
    ecgessAlt   = 2,
    ecgessCtrl  = 4,
};

extern void CGE_LoadLibrary();	// function defined in the loader CPP file

extern void CGE_Init();
extern void CGE_Close();

extern void CGE_SetRenderParams(unsigned uiViewWidth, unsigned uiViewHeight);
extern void CGE_Render();
extern void CGE_OnIdle();

extern void CGE_OnMouseDown(int x, int y, bool bLeftBtn, unsigned uiShift);
extern void CGE_OnMouseMove(int x, int y, unsigned uiShift);
extern void CGE_OnMouseUp(int x, int y, bool bLeftBtn, unsigned uiShift);
extern void CGE_OnMouseWheel(float zDelta);

extern void CGE_LoadSceneFromFile(const char *szFile);
extern void CGE_MoveToViewpoint(int iViewpointIdx, bool bAnimated);

#endif //CGE_LIBRARY_INCLUDED