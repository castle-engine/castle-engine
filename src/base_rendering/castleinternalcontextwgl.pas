{
  Copyright 2013-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initializes OpenGL context using Windows WGL. }
unit CastleInternalContextWgl;

{$i castleconf.inc}

interface

{$ifndef MSWINDOWS}
{ To make it easier to organize Lazarus packages
  (to keep this unit in castle_engine_base.lpk, since it doesn't depend on TCastleWindow,
  but at the same time this unit cannot have "<AddToUsesPkgSection Value="False"/>"
  in castle_engine_base.lpk because castle_engine_window.lpk expects to use this unit
  without recompiling...) ->
  this unit compiles even for platforms that don't have wgl (non-Windows),
  the unit just doesn't define anything in this case. }
implementation
end.
{$else}

uses Windows, SysUtils, Classes,
  CastleVectors, CastleRenderOptions, CastleInternalContextBase;

type
  { OpenGL context created using Windows-specific wgl library. }
  TGLContextWGL = class(TGLContext)
  private
    class var
      WndClassName: UnicodeString;
    class procedure NeedsWndClassName;
  protected
    procedure InitializeCore(const Requirements: TGLContextRequirements); override;
    procedure FinalizeCore; override;
    procedure MakeCurrentCore; override;
    procedure SwapBuffersCore; override;
  public
    { Note about WndPtr:
      We actually ignore it in the current implementation. Only h_Dc is used.
      But we still define the field and require to set it (we assert it is <> 0),
      because

      - in all practical cases, you *have* HWND when using this,
        no significant benefit from not requiring it.
      - and it is consistent with EGL that only uses WinPtr.
      - and it may be again useful in the future -- this is actually the major
        argument. It is very likely to be useful for something in the future,
        since so much WinAPI works with HWND,
        and it would be detrimental having to readd it later,
        after removing it now. }

    // Set this before using Initialize and other methods
    WndPtr: HWND;
    h_Dc: HDC;

    // Created by Initialize, destroyed by Finalize
    h_GLRc: HGLRC;
  end;

implementation

uses {$ifdef OpenGLES} CastleGLES, {$else} CastleGL, {$endif}
  CastleUtils, CastleStringUtils, CastleGLUtils, CastleInternalGLUtils, CastleLog;

{ TGLContextWGL -------------------------------------------------------------- }

procedure TGLContextWGL.InitializeCore(const Requirements: TGLContextRequirements);

  { Both SetPixelFormat* set pixel format (required context capabilities)
    of Windows H_Dc device context. They try to set it, and eventually raise some
    exception (e.g. by CheckRequestedBufferAttributes) if it's not possible.

    SetPixelFormat_WGLChoose tries to use wglChoosePixelFormat, which is generally
    much better, more flexible, most of all: it allows to set multisampling
    parameters, which is not possible by classic ChoosePixelFormat.

    But wglChoosePixelFormat is not guaranteed to exist, so if it doesn't --- we
    fall back on classic ChoosePixelFormat (and DescribePixelFormat) by calling
    SetPixelFormat_ClassicChoose. }
  procedure SetPixelFormat_ClassicChoose;
  var
    PixelFormat: Int32;
    pfd: Tpixelformatdescriptor;
  begin
    FillChar(pfd, SizeOf(pfd), 0);
    with pfd do
    begin
      nSize := SizeOf(TPIXELFORMATDESCRIPTOR);
      nVersion := 1;
      dwFlags := PFD_DRAW_TO_WINDOW              // Format Must Support Window
        or PFD_SUPPORT_OPENGL;                  // Format Must Support OpenGL
      if Requirements.DoubleBuffer then dwFlags := dwFlags or PFD_DOUBLEBUFFER;
      iPixelType := PFD_TYPE_RGBA;              // Request An RGBA Format
      cColorBits := Requirements.ColorBits; // WinAPI accepts here 0 as "default"
      cAlphaBits := Requirements.AlphaBits;
      cDepthBits := Requirements.DepthBits;
      cStencilBits := Requirements.StencilBits;
      iLayerType := PFD_MAIN_PLANE;             // Main Drawing Layer
    end;
    PixelFormat := Windows.ChoosePixelFormat(h_Dc, {$ifndef FPC}@{$endif}pfd);
    OSCheck( PixelFormat <> 0, 'ChoosePixelFormat');

    { Check if we got required AlphaBits, DepthBits, StencilBits -
      because ChoosePixelFormat doesn't guarantee it.

      In the future, I may switch to using SetPixelFormat_WGLChoose by default.
      wglChoosePixelFormatARB makes CheckRequestedBufferAttributes not needed
      (as wglChoosePixelFormatARB already sensibly guarantees that GL context will
      satisfy appropriate limits).
      So this un-elegant code below will not be used (on modern Windowses / GPUs). }
    DescribePixelFormat(h_Dc, PixelFormat, SizeOf(pfd), pfd);
    Requirements.CheckRequestedBufferAttributes('ChoosePixelFormat',
      pfd.cStencilBits, pfd.cDepthBits, pfd.cAlphaBits,
      0 { we have to assume that ChoosePixelFormat returns context
          without multisampling abiilty });

    { Since PixelFormat is OK, set it.
      Passing pfd below is not necessary, as far as I understoon WinAPI docs.
      It's important to pass PixelFormat. }
    OSCheck( SetPixelFormat(h_Dc, PixelFormat, @pfd), 'SetPixelFormat');
  end;

  procedure SetPixelFormat_WGLChoose;
  var
    Temp_h_GLRc: HGLRC;
    Temp_h_Dc: HDC;
    Temp_h_Wnd: HWND;

    { We have to create temporary window, just to query wgl. It's useless to
      call wglGetProcAddress without any GL context active.
      It is quite weird to create a window just to query wgl extensions,
      but that's effectively what every program using wgl has to do.

      We create this temporary window with absolutely standard, minumum
      properties --- any system (supporting OpenGL at all) should
      be able to create our CreateTemporaryWindow. }

    procedure DestroyTemporaryWindow; forward;

    procedure CreateTemporaryWindow;
    var
      PixelFormat: Int32;
      pfd: Tpixelformatdescriptor;
    begin
      NeedsWndClassName;

      Temp_h_Wnd := 0;
      Temp_h_Dc := 0;
      Temp_h_GLRc := 0;

      try
        { create Temp_H_wnd }
        Temp_H_wnd := CreateWindowExW(WS_EX_APPWINDOW or WS_EX_WINDOWEDGE,
          PWideChar(WndClassName),
          PWideChar(StringToUtf16('Temporary window to query WGL extensions')),
          WS_OVERLAPPEDWINDOW or WS_CLIPSIBLINGS or WS_CLIPCHILDREN,
          0, 0, 100, 100,
          0 { no parent window }, 0 { no menu }, hInstance,
          nil { don't pass anything to WM_CREATE } );
        Check( Temp_H_Wnd <> 0, 'Creating temporary window (CreateWindowExW) to query WGL extensions failed');

        { create Temp_h_Dc }
        Temp_h_Dc := GetDC(Temp_h_Wnd);
        Check ( Temp_h_Dc <> 0, 'GetDC failed');

        { create and set PixelFormat (must support OpenGL to be able to
          later do wglCreateContext) }
        FillChar(pfd, SizeOf(pfd), 0);
        with pfd do
        begin
          nSize := SizeOf(TPIXELFORMATDESCRIPTOR);
          nVersion := 1;
          dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;
          iPixelType := PFD_TYPE_RGBA;
          iLayerType := PFD_MAIN_PLANE;
        end;
        PixelFormat := Windows.ChoosePixelFormat(Temp_h_Dc, {$ifndef FPC}@{$endif}pfd);
        OSCheck( PixelFormat <> 0, 'ChoosePixelFormat');
        OSCheck( SetPixelFormat(Temp_h_Dc, PixelFormat, @pfd), 'SetPixelFormat');

        { create and make current Temp_h_GLRc }
        Temp_h_GLRc := wglCreateContext(Temp_h_Dc);
        OSCheck( Temp_h_GLRc <> 0, 'wglCreateContext' );
        OSCheck( wglMakeCurrent(Temp_h_Dc, Temp_h_GLRc), 'wglMakeCurrent');
      except
        { make sure to finalize all partially initialized window parts }
        DestroyTemporaryWindow;
        raise;
      end;
    end;

    procedure DestroyTemporaryWindow;
    begin
      if Temp_h_GLRc <> 0 then
      begin
        wglMakeCurrent(Temp_h_Dc, 0);
        wglDeleteContext(Temp_h_GLRc);
        Temp_h_GLRc := 0;
      end;

      if Temp_h_Dc <> 0 then
      begin
        ReleaseDC(Temp_h_Wnd, Temp_h_Dc);
        Temp_h_Dc := 0;
      end;

      if Temp_h_Wnd <> 0 then
      begin
        DestroyWindow(Temp_h_Wnd);
        Temp_h_Wnd := 0;
      end;
    end;

  var
    PixelFormat: Int32;
    ReturnedFormats: UINT;
    VisualAttr: TInt32List;
    VisualAttrFloat: array [0..1] of Single;
    Success: WINBOOL;
  begin
    CreateTemporaryWindow;
    try
      { Note: CastleGL unit ReadImplementationProperties looks at wglGetCurrentDC
        (this is passed to wglGetExtensionsStringARB call).
        That's Ok, this current context is set by our CreateTemporaryWindow. }

      ReadImplementationProperties;
      ReadExtensions; // read extension entry points like wglChoosePixelFormatARB

      if WGL_ARB_extensions_string then
      begin
        if WGL_ARB_pixel_format then
        begin
          { Ok, wglChoosePixelFormatARB is available }

          VisualAttr := TInt32List.Create;
          try
            VisualAttr.AddRange([
              WGL_DRAW_TO_WINDOW_ARB, GL_TRUE,
              WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
              { If indexed mode will be implemented in CastleWindow one day, this can take
                WGL_TYPE_COLORINDEX_ARB instead of WGL_TYPE_RGBA_ARB. }
              WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB,
              WGL_ACCELERATION_ARB, WGL_FULL_ACCELERATION_ARB]);
            if Requirements.DoubleBuffer then
              VisualAttr.AddRange([WGL_DOUBLE_BUFFER_ARB, GL_TRUE]);
            VisualAttr.AddRange([
              WGL_RED_BITS_ARB, Requirements.RedBits,
              WGL_GREEN_BITS_ARB, Requirements.GreenBits,
              WGL_BLUE_BITS_ARB, Requirements.BlueBits,
              WGL_DEPTH_BITS_ARB, Requirements.DepthBits,
              WGL_STENCIL_BITS_ARB, Requirements.StencilBits,
              WGL_ALPHA_BITS_ARB, Requirements.AlphaBits
            ]);

            if Requirements.MultiSampling > 1 then
            begin
              if WGL_ARB_multisample then
              begin
                VisualAttr.AddRange([
                  WGL_SAMPLE_BUFFERS_ARB, 1,
                  WGL_SAMPLES_ARB, Requirements.MultiSampling ]);
                WritelnLog('MultiSampling', 'WGL_ARB_multisample supported, using multisampling');
              end else
                raise EGLContextNotPossible.CreateFmt('Multisampling (%d samples) ' +
                  'requested, but WGL_ARB_multisample not supported',
                  [Requirements.MultiSampling]);
            end;

            { end of VisualAttr array }
            VisualAttr.AddRange([0, 0]);

            FillChar(VisualAttrFloat, SizeOf(VisualAttrFloat), 0);

            Success := wglChoosePixelFormatARB(H_Dc, PGLint(VisualAttr.L),
              @VisualAttrFloat[0], 1, @PixelFormat, @ReturnedFormats);
          finally FreeAndNil(VisualAttr) end;

          if Success and (ReturnedFormats >= 1) then
          begin
            { We could use wglGetPixelFormatAttribivARB (equivalent to "classic"
              DescribePixelFormat) here to query context we got, and call
              CheckRequestedBufferAttributes to check whether we really got what
              we requested.

              But we don't have to: wglChoosePixelFormatARB (see
              [http://www.opengl.org/registry/specs/ARB/wgl_pixel_format.txt])
              clearly specifies which limits are "exact", which are "minimum",
              and we're actually perfectly satisfied with this.

              So, we're done now :) Just set this pixel format. }
            OSCheck( SetPixelFormat(h_Dc, PixelFormat, nil), 'SetPixelFormat');
          end else
            raise EGLContextNotPossible.CreateFmt(
              'wglChoosePixelFormatARB: pixel format with requested attributes (%s) not found',
              [ Requirements.RequestedBufferAttributes ]);
        end else
        begin
          WritelnLog('wgl', 'WGL_ARB_pixel_format not available, using classic ChoosePixelFormat');
          SetPixelFormat_ClassicChoose;
        end;
      end else
      begin
        WritelnLog('wgl', 'wglGetExtensionsStringARB not available, using classic ChoosePixelFormat');
        SetPixelFormat_ClassicChoose;
      end;
    finally DestroyTemporaryWindow end;
  end;

  procedure UseCreateContextAttribsARB;
  var
    Attribs: TInt32List;
    ShareContextGlrc: HGLRC;
  begin
    // WritelnLog('wgl', 'Initializing Windows OpenGL context using modern wglCreateContextAttribsARB, good'); // too verbose

    Attribs := TInt32List.Create;
    try
      { Select 3.2 core context.
        See https://registry.khronos.org/OpenGL/extensions/ARB/WGL_ARB_create_context.txt
        https://www.khronos.org/opengl/wiki/Creating_an_OpenGL_Context_(WGL) }
      if TGLFeatures.RequestCapabilities = rcForceModern then
      begin
        Attribs.AddRange([
          WGL_CONTEXT_MAJOR_VERSION_ARB, TGLFeatures.ModernVersionMajor,
          WGL_CONTEXT_MINOR_VERSION_ARB, TGLFeatures.ModernVersionMinor,
          WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB
          { Not used, following https://www.khronos.org/opengl/wiki/OpenGL_Context#Context_types
            forward-compatible really only makes sense on macOS.
            The "core" profile is what should be just used with new OpenGLs on sane
            (non-macOS) platforms. }
          //WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        ]);
      end;
      { Select debug context.
        Note that the implementation assumes nothing above passes WGL_CONTEXT_FLAGS_ARB. }
      if TGLFeatures.Debug then
      begin
        Attribs.AddRange([
          WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_DEBUG_BIT_ARB
        ]);
      end;
      Attribs.AddRange([0]);
      Assert(SizeOf(TGLint) = SizeOf(Int32)); // make sure Attribs match PGLint type

      if SharedContext <> nil then
        ShareContextGlrc := (SharedContext as TGLContextWGL).h_GLRc
      else
        ShareContextGlrc := 0;

      h_GLRc := wglCreateContextAttribsARB(h_Dc, ShareContextGlrc, PGLint(Attribs.L));
      OSCheck(h_GLRc <> 0, 'wglCreateContextAttribsARB');
    finally FreeAndNil(Attribs) end;
  end;

  procedure UseCreateContext;
  begin
    { create gl context and make it current }
    h_GLRc := wglCreateContext(h_Dc);
    OSCheck(h_GLRc <> 0, 'wglCreateContext');

    { We need wglShareLists.
      Testcase: see if examples/window/multi_window.lpr
      shows font in all windows. }
    if SharedContext <> nil then
      OSCheck(wglShareLists((SharedContext as TGLContextWGL).h_GLRc, h_GLRc), 'wglShareLists');
  end;

begin
  // make sure caller didn't forget to set these before calling CreateContext
  Assert(WndPtr <> 0);
  Assert(h_Dc <> 0);

  // load base (not extensions) WGL functions
  if GL_LibHandle = nil then
  	InitOpenGL;

  SetPixelFormat_WGLChoose;

  { Note that SetPixelFormat_WGLChoose setup temporary context
    and called ReadImplementationProperties
    and thus initialized WGL_ARB_xxx. }

  if (GetDeviceCaps(h_Dc, RASTERCAPS) and RC_PALETTE) <> 0 then
    raise EGLContextNotPossible.Create('This device is paletted, bad display settings');

  if WGL_ARB_create_context and
     WGL_ARB_create_context_profile then
    UseCreateContextAttribsARB
  else
    UseCreateContext;
end;

procedure TGLContextWGL.FinalizeCore;
begin
  if h_GLRc <> 0 then
  begin
    if (not wglMakeCurrent(h_Dc, 0)) then
      WritelnWarning('WinAPI', 'Deactivating current OpenGL rendering context (wglMakeCurrent(..., NULL)) failed.');
    if (not wglDeleteContext(h_GLRc)) then
      WritelnWarning('WinAPI', 'Releasing current OpenGL rendering context (wglDeleteContext) failed.');
    h_GLRc := 0;
  end;
end;

procedure TGLContextWGL.MakeCurrentCore;
begin
  Assert(h_GLRc <> 0); // this is called only when Initialized
  OSCheck( wglMakeCurrent(h_Dc, h_GLRc), 'wglMakeCurrent');
end;

procedure TGLContextWGL.SwapBuffersCore;
begin
  Assert(h_GLRc <> 0); // this is called only when Initialized
  Windows.SwapBuffers(h_Dc);
end;

{ Handler for events for our temporary window.
  Does nothing for now, we could as well pass
    WindowClass.lpfnWndProc := @DefWindowProcW;
  but maybe there will be a need to handle something here some day. }
function WndProc(hWnd: HWND; uMsg: UINT; wParm: WPARAM; lParm: LPARAM): LRESULT; stdcall;
begin
  result := DefWindowProcW(hWnd, uMsg, wParm, lParm);
end;

class procedure TGLContextWGL.NeedsWndClassName;
var
  WindowClass: TWndClassW;
begin
  if WndClassName = '' then
  begin
    { Register minimal window class, just to create temporary WinAPI window
      to query WGL extensions.
      See https://www.khronos.org/opengl/wiki/Creating_an_OpenGL_Context_(WGL) }

    WndClassName := 'TGLContextWGL';

    FillChar(WindowClass, SizeOf(WindowClass), 0);
    WindowClass.style := CS_OWNDC;
    WindowClass.lpfnWndProc := @WndProc;
    WindowClass.hInstance := hInstance;
    WindowClass.lpszClassName := PWideChar(WndClassName);
    OSCheck( RegisterClassW(WindowClass) <> 0, 'RegisterClassW');
  end;
end;

end.

{$endif}