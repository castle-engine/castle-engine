#if !defined(WIN32_LEAN_AND_MEAN)
#define WIN32_LEAN_AND_MEAN
#endif

#include <windows.h>
#include <windowsx.h>
#include <Commdlg.h>
#include <GL/gl.h>
#include <sstream>
#include <stdexcept>

#include "castlelib.h"

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
    return msg.wParam;
}

unsigned WinShiftToCgeShift(unsigned wParam)
{
	unsigned res = 0;
	if (wParam & MK_CONTROL) res |= ecgessCtrl;
	if (wParam & MK_SHIFT) res |= ecgessShift;
	return res;
}

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
	        CGE_Render();			// do not draw in WM_PAINT
            SwapBuffers(g_hDC);
        }
        return TRUE;

    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
        CGE_OnMouseDown(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), msg==WM_LBUTTONDOWN, WinShiftToCgeShift(wParam));
        SetCapture(hWnd);
        break;

    case WM_MOUSEMOVE:
        CGE_OnMouseMove(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), WinShiftToCgeShift(wParam));
        break;

    case WM_LBUTTONUP:
    case WM_RBUTTONUP:
        CGE_OnMouseUp(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), msg==WM_LBUTTONUP, WinShiftToCgeShift(wParam));
        if (GetCapture()==hWnd)
            ReleaseCapture();
        break;

    case WM_MOUSEWHEEL:
        CGE_OnMouseWheel(GET_WHEEL_DELTA_WPARAM(wParam));
        break;

    case WM_CHAR:
        switch (wParam)
        {
        case VK_ESCAPE:
            PostMessage(hWnd, WM_CLOSE, 0, 0);
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
		CGE_SetRenderParams(g_windowWidth, g_windowHeight);
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

bool Init()
{
    try
    {
        InitGL();

        CGE_LoadLibrary();
        CGE_Init();
        CGE_SetRenderParams(g_windowWidth, g_windowHeight);
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

    int pf = ChoosePixelFormat(g_hDC, &pfd);
    if (!SetPixelFormat(g_hDC, pf, &pfd))
        throw std::runtime_error("SetPixelFormat() failed.");

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
	CGE_SetRenderParams(g_windowWidth, g_windowHeight);
}

void OnIdle()
{
    CGE_OnIdle();
    InvalidateRect(g_hWnd, NULL, TRUE);
}

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
        InvalidateRect(g_hWnd, NULL, TRUE);
    }
}