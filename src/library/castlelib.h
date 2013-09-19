#ifndef CGE_LIBRARY_INCLUDED
#define CGE_LIBRARY_INCLUDED

enum ECgeShiftState
{
    ecgessShift = 1,
    ecgessAlt   = 2,
    ecgessCtrl  = 4,
};

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