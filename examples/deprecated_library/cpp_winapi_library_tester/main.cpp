/*
  Copyright 2013-2017 Jan Adamec, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  This is a MS Windows test project for our library in src/deprecated_library/.
  It uses library API (exposed in castleengine.h), and uses a compiled dynamic
  library with the engine.

  Using the engine units directly gives you a complete object-oriented API
  in ObjectPascal to do everything :) The C library API offers only a small
  subset of engine functionality. The library is useful to access the engine
  from other programming languages.

  HOW TO RUN THIS: prior to running this project, compile and copy the shared
  library (in src/deprecated_library/) to a place where it can be loaded, it means
  copy castleengine.dll to this project folder, or anywhere on $PATH.

  You will also need other dynamic libraries (zlib1.dll, libpng.dll, ogg.dll,
  OpenAL32.dll, ...) that are shipped with Castle Game Engine.
  See https://castle-engine.io/compiling_from_source.php#section_Windows .
*/

#if !defined(WIN32_LEAN_AND_MEAN)
#define WIN32_LEAN_AND_MEAN
#endif

#include <windows.h>
#include <windowsx.h>
#include <Commdlg.h>
#include <GL/gl.h>
#include <sstream>
#include <stdexcept>

#include "../../../src/deprecated_library/castleengine.h"

#if defined(_DEBUG)
#include <crtdbg.h>
#endif

//-----------------------------------------------------------------------------
// Globals.
//-----------------------------------------------------------------------------

HWND      g_hWnd = NULL;
HINSTANCE g_hInstance;
HDC       g_hDC = NULL;
HGLRC     g_hRC = NULL;
int       g_windowWidth;
int       g_windowHeight;
bool      g_isFullScreen;
bool      g_hasFocus;

//-----------------------------------------------------------------------------
int g_nViewpointCount = 0;
int g_nCurrentViewpoint;

//-----------------------------------------------------------------------------
// Function Prototypes.
//-----------------------------------------------------------------------------

void Cleanup();
HWND CreateAppWindow(const WNDCLASSEX &wcl, const char *pszTitle);
bool Init();
void InitGL();
void ToggleFullScreen();
void OnIdle();
void ShowOpenFileDialog();
LRESULT CALLBACK WindowProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);

//-----------------------------------------------------------------------------
// Functions.
//-----------------------------------------------------------------------------

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd)
{
#if defined _DEBUG
    _CrtSetDbgFlag(_CRTDBG_LEAK_CHECK_DF | _CRTDBG_ALLOC_MEM_DF);
    _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);
    _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);
#endif

    MSG msg = {0};
    WNDCLASSEX wcl = {0};

    wcl.cbSize = sizeof(wcl);
    wcl.style = CS_OWNDC | CS_HREDRAW | CS_VREDRAW;
    wcl.lpfnWndProc = WindowProc;
    wcl.cbClsExtra = 0;
    wcl.cbWndExtra = 0;
    wcl.hInstance = g_hInstance = hInstance;
    wcl.hIcon = LoadIcon(0, IDI_APPLICATION);
    wcl.hCursor = LoadCursor(0, IDC_ARROW);
    wcl.hbrBackground = 0;
    wcl.lpszMenuName = 0;
    wcl.lpszClassName = "GL3WindowClass";
    wcl.hIconSm = 0;
    if (!RegisterClassEx(&wcl))
        return 0;

    g_hWnd = CreateAppWindow(wcl, "CastleLib Test App");
	if (g_hWnd==NULL)
		return 0;

	if (!Init())	// Init OpenGL context and our library
		return 0;

    ShowWindow(g_hWnd, nShowCmd);
    UpdateWindow(g_hWnd);

    while (true)
    {
        while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE))
        {
            if (msg.message == WM_QUIT)
                break;

            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }

        if (msg.message == WM_QUIT)
            break;

        if (g_hasFocus)
            OnIdle();
        else
            WaitMessage();
    }
    return (int)msg.wParam;
}

//-----------------------------------------------------------------------------
LRESULT CALLBACK WindowProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg)
    {
    case WM_ACTIVATE:
        switch (wParam)
        {
        default:
            break;

        case WA_ACTIVE:
        case WA_CLICKACTIVE:
            g_hasFocus = true;
            break;

        case WA_INACTIVE:
            if (g_isFullScreen)
                ShowWindow(hWnd, SW_MINIMIZE);
            g_hasFocus = false;
            break;
        }
        break;

    case WM_ERASEBKGND:
        if (IsWindowEnabled(hWnd))
        {
            wglMakeCurrent(g_hDC, g_hRC);
            CGE_Render();			// do not draw in WM_PAINT (inspired in CastleWindow)
            SwapBuffers(g_hDC);
        }
        return TRUE;

    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
        CGE_MouseDown(GET_X_LPARAM(lParam), g_windowHeight-1-GET_Y_LPARAM(lParam), msg==WM_LBUTTONDOWN, 0);
        SetCapture(hWnd);
        break;

    case WM_MOUSEMOVE:
        CGE_Motion(GET_X_LPARAM(lParam), g_windowHeight-1-GET_Y_LPARAM(lParam), 0);
        break;

    case WM_LBUTTONUP:
    case WM_RBUTTONUP:
        CGE_MouseUp(GET_X_LPARAM(lParam), g_windowHeight-1-GET_Y_LPARAM(lParam),
            msg==WM_LBUTTONUP, 0, true);
        if (GetCapture()==hWnd)
            ReleaseCapture();
        break;

    case WM_MOUSEWHEEL:
        CGE_MouseWheel(GET_WHEEL_DELTA_WPARAM(wParam), true);
        break;

    case WM_CHAR:
        switch (wParam)
        {
        case VK_ESCAPE:
            PostMessage(hWnd, WM_CLOSE, 0, 0);
            break;

        case 's':
            CGE_SaveScreenshotToFile("screenshot.jpg");
            break;

        case 'f':
            CGE_SetNavigationType(ecgenavFly);
            break;

        case 'e':
            CGE_SetNavigationType(ecgenavExamine);
            break;

        case 'o':
            {
                int nVal = CGE_GetVariableInt(ecgevarEffectSSAO);
                if (nVal < 0) nVal = 0;
                CGE_SetVariableInt(ecgevarEffectSSAO, 1-nVal);
            }
            break;

        case 'h':
            {
                int nVal = CGE_GetVariableInt(ecgevarWalkHeadBobbing);
                if (nVal < 0) nVal = 0;
                CGE_SetVariableInt(ecgevarWalkHeadBobbing, 1-nVal);
            }
            break;

        default:
            break;
        }
        break;

    case WM_KEYDOWN:
        switch (wParam)
        {
        case VK_PRIOR:      // PageUp - move to previous viewpoint
            if (g_nViewpointCount > 0)
            {
                if (g_nCurrentViewpoint > 0)
                    g_nCurrentViewpoint--;
                else
                    g_nCurrentViewpoint = g_nViewpointCount-1;
                CGE_MoveToViewpoint(g_nCurrentViewpoint, true);
            }
            break;

        case VK_NEXT:       // PageDown - move to next viewpoint
            if (g_nViewpointCount > 0)
            {
                if (g_nCurrentViewpoint < g_nViewpointCount-1)
                    g_nCurrentViewpoint++;
                else
                    g_nCurrentViewpoint = 0;
                CGE_MoveToViewpoint(g_nCurrentViewpoint, true);
            }
            break;
        default:
            break;
        }
        break;

    case WM_DESTROY:
        Cleanup();
        PostQuitMessage(0);
        return 0;

    case WM_SIZE:
        g_windowWidth = LOWORD(lParam);
        g_windowHeight = HIWORD(lParam);
        CGE_Resize(g_windowWidth, g_windowHeight);
        break;

    case WM_SYSKEYDOWN:
        if (wParam == VK_RETURN)		// Alt + Enter
            ToggleFullScreen();
        break;

    default:
        break;
    }

    return DefWindowProc(hWnd, msg, wParam, lParam);
}

//-----------------------------------------------------------------------------
void Cleanup()
{
    if (g_hDC)
    {
        if (g_hRC)
        {
            wglMakeCurrent(g_hDC, NULL);
            wglDeleteContext(g_hRC);
            g_hRC = NULL;
        }

        ReleaseDC(g_hWnd, g_hDC);
        g_hDC = NULL;
    }
}

//-----------------------------------------------------------------------------
HWND CreateAppWindow(const WNDCLASSEX &wcl, const char *pszTitle)
{
    DWORD wndExStyle = WS_EX_OVERLAPPEDWINDOW;
    DWORD wndStyle = WS_OVERLAPPEDWINDOW/* | WS_CLIPCHILDREN | WS_CLIPSIBLINGS*/;

    HWND hWnd = CreateWindowEx(wndExStyle, wcl.lpszClassName, pszTitle,
                wndStyle, 0, 0, 0, 0, 0, 0, wcl.hInstance, 0);
    if (hWnd)
    {
		// Create a window that is centered on the desktop. It's exactly 1/4 the
		// size of the desktop.
        int screenWidth = GetSystemMetrics(SM_CXSCREEN);
        int screenHeight = GetSystemMetrics(SM_CYSCREEN);
        int halfScreenWidth = screenWidth / 2;
        int halfScreenHeight = screenHeight / 2;
        int left = (screenWidth - halfScreenWidth) / 2;
        int top = (screenHeight - halfScreenHeight) / 2;
        RECT rc = {0};

        SetRect(&rc, left, top, left + halfScreenWidth, top + halfScreenHeight);
        AdjustWindowRectEx(&rc, wndStyle, FALSE, wndExStyle);
        MoveWindow(hWnd, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, TRUE);

        GetClientRect(hWnd, &rc);
        g_windowWidth = rc.right - rc.left;
        g_windowHeight = rc.bottom - rc.top;
    }

    return hWnd;
}

//-----------------------------------------------------------------------------
int __cdecl OpenGlLibraryCallback(int eCode, int iParam1, int iParam2, const char *szParam)
{
    switch (eCode)
    {
    case ecgelibNeedsDisplay:
        InvalidateRect(g_hWnd, NULL, TRUE);
        return 1;

    case ecgelibSetMouseCursor:
        {
            HCURSOR hNewCur = NULL;
            switch (iParam1)
            {
            case ecgecursorWait: hNewCur = LoadCursor(NULL, IDC_WAIT); break;
            case ecgecursorHand: hNewCur = LoadCursor(NULL, IDC_HAND); break;
            case ecgecursorText: hNewCur = LoadCursor(NULL, IDC_IBEAM); break;
            default: hNewCur = LoadCursor(NULL, IDC_ARROW);
            }
            SetCursor(hNewCur);
        }
        return 1;

    case ecgelibNavigationTypeChanged:
        // TODO: update nav. buttons
        return 1;
    }
    return 0;
}

//-----------------------------------------------------------------------------
bool Init()
{
    try
    {
        InitGL();

	char applicationConfigDirectory[1000];
	int bytes = GetModuleFileName(NULL, applicationConfigDirectory, 1000);

        CGE_LoadLibrary();
        // Note: the log output is in
        // c:/Users/<username>/AppData/Local/cpp_winapi_library_tester/cpp_winapi_library_tester.log
        // See https://castle-engine.io/manual_log.php
        CGE_Initialize(applicationConfigDirectory);
        CGE_Open(ecgeofLog, g_windowWidth, g_windowHeight, 96);
        CGE_SetLibraryCallbackProc(OpenGlLibraryCallback);
        CGE_SetUserInterface(true);
        //CGE_LoadSceneFromFile("c:\\projects\\humanoid_stand.wrl");
        ShowOpenFileDialog();
        return true;
    }
    catch (const std::exception &e)
    {
        std::ostringstream msg;

        msg << "Application initialization failed!" << std::endl << std::endl;
        msg << e.what();

        MessageBox(g_hWnd, msg.str().c_str(), "Error", MB_ICONERROR);
        return false;
    }
}

//-----------------------------------------------------------------------------
void InitGL()
{
    if (!(g_hDC = GetDC(g_hWnd)))
        throw std::runtime_error("GetDC() failed.");

    // Create and set a pixel format for the window.
    PIXELFORMATDESCRIPTOR pfd = {0};
    pfd.nSize = sizeof(pfd);
    pfd.nVersion = 1;
    pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
    pfd.iPixelType = PFD_TYPE_RGBA;
    pfd.cColorBits = 24;
    pfd.cDepthBits = 24;
    pfd.iLayerType = PFD_MAIN_PLANE;

    bool bMsInitSuccess = false;
    /*if (WGL_ARB_multisample && WGL_ARB_pixel_format && wglChoosePixelFormatARB)
    {
        float fVisualAttr[2];
        int *pVisualAttrList = LGlMsCreateOpenGLContextAttrList(DoubleBuffered, RGBA, MultiSampling);
        fVisualAttr[0] = fVisualAttr[1] = 0;
        unsigned uiReturnedFormats = 0;
        int pf;
        bMsInitSuccess = wglChoosePixelFormatARB(g_hDC, (GLint*)pVisualAttrList, fVisualAttr, 1, &pf, &uiReturnedFormats);
        free(pVisualAttrList);

        if (bMsInitSuccess && uiReturnedFormats >= 1)
            SetPixelFormat(g_hDC, pf, nil);
        else
            bMsInitSuccess = false;
    }*/

    if (!bMsInitSuccess)
    {
        int pf = ChoosePixelFormat(g_hDC, &pfd);
        if (!SetPixelFormat(g_hDC, pf, &pfd))
            throw std::runtime_error("SetPixelFormat() failed.");
    }

    g_hRC = wglCreateContext(g_hDC);

    if (!wglMakeCurrent(g_hDC, g_hRC))
        throw std::runtime_error("wglMakeCurrent() failed for OpenGL context.");

	/*
    // Display the OpenGL version string in the window title bar.
    const char *pszGLVersion = reinterpret_cast<const char *>(glGetString(GL_VERSION));
    std::ostringstream text;
    text << "GL_VERSION: " << pszGLVersion;
    SetWindowText(g_hWnd, text.str().c_str());*/
}

//-----------------------------------------------------------------------------
void ToggleFullScreen()
{
    static DWORD savedExStyle;
    static DWORD savedStyle;
    static RECT rcSaved;

    g_isFullScreen = !g_isFullScreen;

    if (g_isFullScreen)
    {
        // Moving to full screen mode.
        savedExStyle = GetWindowLong(g_hWnd, GWL_EXSTYLE);
        savedStyle = GetWindowLong(g_hWnd, GWL_STYLE);
        GetWindowRect(g_hWnd, &rcSaved);

        SetWindowLong(g_hWnd, GWL_EXSTYLE, 0);
        SetWindowLong(g_hWnd, GWL_STYLE, WS_POPUP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS);
        SetWindowPos(g_hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_FRAMECHANGED | SWP_SHOWWINDOW);

        g_windowWidth = GetSystemMetrics(SM_CXSCREEN);
        g_windowHeight = GetSystemMetrics(SM_CYSCREEN);

        SetWindowPos(g_hWnd, HWND_TOPMOST, 0, 0, g_windowWidth, g_windowHeight, SWP_SHOWWINDOW);
    }
    else
    {
        // Moving back to windowed mode.
        SetWindowLong(g_hWnd, GWL_EXSTYLE, savedExStyle);
        SetWindowLong(g_hWnd, GWL_STYLE, savedStyle);
        SetWindowPos(g_hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_FRAMECHANGED | SWP_SHOWWINDOW);

        g_windowWidth = rcSaved.right - rcSaved.left;
        g_windowHeight = rcSaved.bottom - rcSaved.top;

        SetWindowPos(g_hWnd, HWND_NOTOPMOST, rcSaved.left, rcSaved.top, g_windowWidth, g_windowHeight, SWP_SHOWWINDOW);
    }
    CGE_Resize(g_windowWidth, g_windowHeight);
}

//-----------------------------------------------------------------------------
void OnIdle()
{
    CGE_Update();
}

//-----------------------------------------------------------------------------
void ShowOpenFileDialog()
{
    char szFile[MAX_PATH];
    szFile[0] = 0;

    OPENFILENAME ofn;
    ZeroMemory(&ofn, sizeof(OPENFILENAME));
    ofn.lStructSize = sizeof(OPENFILENAME);
    ofn.hwndOwner = g_hWnd;
    ofn.lpstrFile = szFile;
    ofn.nMaxFile = MAX_PATH;
    ofn.lpstrFilter = "VRML files (*.wrl)\0*.WRL\0";
    ofn.nFilterIndex = 0;
    ofn.lpstrFileTitle = NULL;
    ofn.nMaxFileTitle = 0;
    ofn.lpstrInitialDir = NULL;
    ofn.Flags = OFN_PATHMUSTEXIST|OFN_FILEMUSTEXIST;
    if (GetOpenFileName(&ofn))
    {
        CGE_LoadSceneFromFile(szFile);

        // show viewpoints available
        std::string sViewpointList;
        int nCount = CGE_GetViewpointsCount();
        for (int i = 0; i < nCount; i++)
        {
            char sName[512];
            CGE_GetViewpointName(i, sName, 512);
            sViewpointList += sName;
            sViewpointList += "\n";
        }
        MessageBox(g_hWnd, sViewpointList.c_str(), "Viewpoints found", 0);

        /*char sGlInfo[1024];
        CGE_GetOpenGLInformation(sGlInfo, 1024);
        MessageBox(g_hWnd, sGlInfo, "GL Info", 0);*/

        InvalidateRect(g_hWnd, NULL, TRUE);
        g_nViewpointCount = nCount;
        g_nCurrentViewpoint = 0;
    }
}
