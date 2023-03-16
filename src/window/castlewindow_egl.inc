{%MainUnit castlewindow.pas}
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

{ Use this to make a CastleWindow backend using EGL
  (initialize window with OpenGL ES context).

  Written based on
  http://www.khronos.org/registry/egl/sdk/docs/man/xhtml/eglIntro.html .
  See http://www.khronos.org/registry/egl/sdk/docs/man/xhtml/
  for an up-to-date reference of EGL.
  FPC example in packages/opengles/examples/es2example1.pas was also useful. }

{$ifdef read_interface_uses}
{$endif}

{$ifdef read_window_interface}
private
  Context: EGLContext;
  Surface: EGLSurface;
  Display: EGLDisplay;

  { GL context creation before creating native window. }
  procedure ContextCreateBegin;

  { GL context creation after creating native window. }
  procedure ContextCreateEnd(WndPtr: EGLNativeWindowType);

  { Destroy context created by ContextCreateBegin + ContextCreateEnd.
    Call ContextDestroyBegin early from CloseBackend, to destroy OpenGL context. }
  procedure ContextDestroyBegin;

  { Call this to destroy resources that should be destroyed
    *after* the rest of the native window is destroyed. }
  procedure ContextDestroyEnd;
public
  { Query the EGL context what is current size. }
  procedure QuerySize(out AWidth, AHeight: EGLint);
{$endif}

{$ifdef read_application_interface}
private
  {$ifdef CASTLE_WINDOW_XLIB}
  { Check is context creation possible. XDisplay is available now. }
  procedure ContextApplicationCheck;
  {$endif}
{$endif}

{$ifdef read_implementation}

procedure TCastleWindow.ContextCreateBegin;
begin
end;

{ EGL error codes and descriptions from
  http://www.khronos.org/registry/egl/sdk/docs/man/xhtml/eglGetError.html }
function EGLError: string;
var
  ErrorCode: EGLint;
begin
  ErrorCode := eglGetError();
  case ErrorCode of
    EGL_SUCCESS: Result := 'The last function succeeded without error.';
    EGL_NOT_INITIALIZED: Result := 'EGL is not initialized, or could not be initialized, for the specified EGL display connection.';
    EGL_BAD_ACCESS: Result := 'EGL cannot access a requested resource (for example a context is bound in another thread).';
    EGL_BAD_ALLOC: Result := 'EGL failed to allocate resources for the requested operation.';
    EGL_BAD_ATTRIBUTE: Result := 'An unrecognized attribute or attribute value was passed in the attribute list.';
    EGL_BAD_CONTEXT: Result := 'An EGLContext argument does not name a valid EGL rendering context.';
    EGL_BAD_CONFIG: Result := 'An EGLConfig argument does not name a valid EGL frame buffer configuration.';
    EGL_BAD_CURRENT_SURFACE: Result := 'The current surface of the calling thread is a window, pixel buffer or pixmap that is no longer valid.';
    EGL_BAD_DISPLAY: Result := 'An EGLDisplay argument does not name a valid EGL display connection.';
    EGL_BAD_SURFACE: Result := 'An EGLSurface argument does not name a valid surface (window, pixel buffer or pixmap) configured for GL rendering.';
    EGL_BAD_MATCH: Result := 'Arguments are inconsistent (for example, a valid context requires buffers not supplied by a valid surface).';
    EGL_BAD_PARAMETER: Result := 'One or more argument values are invalid.';
    EGL_BAD_NATIVE_PIXMAP: Result := 'A NativePixmapType argument does not refer to a valid native pixmap.';
    EGL_BAD_NATIVE_WINDOW: Result := 'A NativeWindowType argument does not refer to a valid native window.';
    EGL_CONTEXT_LOST: Result := 'A power management event has occurred. The application must destroy all contexts and reinitialise OpenGL ES state and objects to continue rendering.';
    else Result := Format('EGL error %d', [ErrorCode]);
  end;
end;

procedure TCastleWindow.ContextCreateEnd(WndPtr: EGLNativeWindowType);
var
  Config: EGLConfig;
  ShareContext: EGLContext;
  NumConfig: EGLint;
  Attribs: TInt32List;
const
  ContextAttribsv2: array [0..2] of EGLint =
  ( EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
  ContextAttribsv3: array [0..2] of EGLint =
  ( EGL_CONTEXT_CLIENT_VERSION, 3, EGL_NONE);
begin
  Display := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if Display = EGL_NO_DISPLAY then
    { This does not set eglGetError, so we don't use EGLError in message below. }
    raise EGLContextNotPossible.Create('EGL: Cannot get display');

  if eglInitialize(Display, nil, nil) = EGL_FALSE then
    raise EGLContextNotPossible.Create('EGL: Cannot initialize: ' + EGLError);

  Attribs := TInt32List.Create;
  try
    if StencilBits > 0 then
      Attribs.AddRange([EGL_STENCIL_SIZE, StencilBits]);
    if AlphaBits > 0 then
      Attribs.AddRange([EGL_ALPHA_SIZE, AlphaBits]);
    Attribs.AddRange([
      EGL_DEPTH_SIZE, DepthBits,
      EGL_RED_SIZE  , Max(1, RedBits),
      EGL_GREEN_SIZE, Max(1, GreenBits),
      EGL_BLUE_SIZE , Max(1, BlueBits),
      EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
      EGL_NONE
    ]);
    if eglChooseConfig(Display, PEGLint(Attribs.L), @Config, 1, @NumConfig) = EGL_FALSE then
      raise EGLContextNotPossible.Create('EGL: Cannot choose config: ' + EGLError);
  finally FreeAndNil(Attribs) end;

  if Application.FOpenWindows.Count <> 0 then
    ShareContext := Application.FOpenWindows[0].Context else
    ShareContext := EGL_NO_CONTEXT;
  Context := eglCreateContext(Display, Config, ShareContext, @ContextAttribsv3);
  if Context = EGL_NO_CONTEXT then
    Context := eglCreateContext(Display, Config, ShareContext, @ContextAttribsv2);
  if Context = EGL_NO_CONTEXT then
    raise EGLContextNotPossible.Create('EGL: Cannot create context: ' + EGLError);

  Surface := eglCreateWindowSurface(Display, Config, WndPtr, nil);
  if Surface = EGL_NO_SURFACE then
    raise EGLContextNotPossible.Create('EGL: Cannot create surface: ' + EGLError);
end;

procedure TCastleWindow.ContextDestroyBegin;
begin
  if Surface <> EGL_NO_SURFACE { nil } then
  begin
    if eglDestroySurface(Display, Surface) = EGL_FALSE then
      WritelnWarning('EGL', 'Cannot destroy surface: ' + EGLError);
    Surface := EGL_NO_SURFACE;
  end;

  if Display <> EGL_NO_DISPLAY { nil } then
  begin
    { Release the current context, to allow eglTerminate to release resources.
      This allows on Android to reopen OpenGL context from the application
      (like android_demo ReopenContextButton). }
    if eglMakeCurrent(Display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT) = EGL_FALSE then
      WritelnWarning('EGL', 'Cannot release current context: ' + EGLError);
    if eglTerminate(Display) = EGL_FALSE then
      WritelnWarning('EGL', 'Cannot terminate display connection: ' + EGLError);
    Display := EGL_NO_DISPLAY;
  end;
end;

procedure TCastleWindow.ContextDestroyEnd;
begin
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  Assert(not Closed);
  if eglMakeCurrent(Display, Surface, Surface, Context) = EGL_FALSE then
    WritelnWarning('EGL', 'Cannot make context current: ' + EGLError);
end;

procedure TCastleWindow.SwapBuffers;
begin
  if eglSwapBuffers(Display, Surface) = EGL_FALSE then
    WritelnWarning('EGL', 'Cannot swap buffers (this is normal if app is no longer active): ' + EGLError);
end;

procedure TCastleWindow.QuerySize(out AWidth, AHeight: EGLint);
begin
  if eglQuerySurface(Display, Surface, EGL_WIDTH, @AWidth) = EGL_FALSE then
  begin
    WritelnWarning('EGL', 'Cannot query surface width (this is normal if app is no longer active): ' + EGLError);
    AWidth := 0; // otherwise AWidth would be left undefined
  end;
  if eglQuerySurface(Display, Surface, EGL_HEIGHT, @AHeight) = EGL_FALSE then
  begin
    WritelnWarning('EGL', 'Cannot query surface height (this is normal if app is no longer active): ' + EGLError);
    AHeight := 0; // otherwise AHeight would be left undefined
  end;
end;

{$ifdef CASTLE_WINDOW_XLIB}
procedure TCastleApplication.ContextApplicationCheck;
begin
end;
{$endif}

{$endif read_implementation}
