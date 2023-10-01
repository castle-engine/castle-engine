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

{ Initializes OpenGL or OpenGLES context using cross-platform EGL. }
unit CastleInternalContextEgl;

{$i castleconf.inc}

interface

uses SysUtils, Classes,
  CastleInternalContextBase, CastleInternalEgl;

type
  { Initializes OpenGL or OpenGLES context using cross-platform EGL. }
  TGLContextEgl = class(TGLContext)
  private
    Context: EGLContext;
    Surface: EGLSurface;
    Display: EGLDisplay;
  public
    // Set this before using ContextCreate and other methods
    WndPtr: EGLNativeWindowType;

    procedure ContextCreate(const Requirements: TGLContextRequirements); override;
    procedure ContextDestroy; override;
    procedure MakeCurrent; override;
    procedure SwapBuffers; override;

    { Query context size, returns 0 0 if cannot query for some reason. }
    procedure QuerySize(out AWidth, AHeight: EGLint);
  end;

implementation

uses Math,
  CastleLog, CastleUtils;

{ EGL references:
  - http://www.khronos.org/registry/egl/sdk/docs/man/xhtml/eglIntro.html .
  - See http://www.khronos.org/registry/egl/sdk/docs/man/xhtml/
    for an up-to-date reference of EGL.
  - FPC example in packages/opengles/examples/es2example1.pas was also useful.
}

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

{ TGLContextEgl -------------------------------------------------------------- }

procedure TGLContextEgl.ContextCreate(const Requirements: TGLContextRequirements);
var
  Config: EGLConfig;
  ShareContextEgl: EGLContext;
  NumConfig: EGLint;
  Attribs: TInt32List;
const
  ContextAttribsv2: array [0..2] of EGLint =
  ( EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
  ContextAttribsv3: array [0..2] of EGLint =
  ( EGL_CONTEXT_CLIENT_VERSION, 3, EGL_NONE);
begin
  if not EglAvailable then
    raise Exception.Create('Could not load EGL library, required to initialize context');

  Display := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if Display = EGL_NO_DISPLAY then
    { This does not set eglGetError, so we don't use EGLError in message below. }
    raise EGLContextNotPossible.Create('EGL: Cannot get display');

  if eglInitialize(Display, nil, nil) = EGL_FALSE then
    raise EGLContextNotPossible.Create('EGL: Cannot initialize: ' + EGLError);

  Attribs := TInt32List.Create;
  try
    if Requirements.StencilBits > 0 then
      Attribs.AddRange([EGL_STENCIL_SIZE, Requirements.StencilBits]);
    if Requirements.AlphaBits > 0 then
      Attribs.AddRange([EGL_ALPHA_SIZE, Requirements.AlphaBits]);
    Attribs.AddRange([
      EGL_DEPTH_SIZE, Requirements.DepthBits,
      EGL_RED_SIZE  , Max(1, Requirements.RedBits),
      EGL_GREEN_SIZE, Max(1, Requirements.GreenBits),
      EGL_BLUE_SIZE , Max(1, Requirements.BlueBits),
      EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
      EGL_NONE
    ]);
    if eglChooseConfig(Display, PEGLint(Attribs.L), @Config, 1, @NumConfig) = EGL_FALSE then
      raise EGLContextNotPossible.Create('EGL: Cannot choose config: ' + EGLError);
  finally FreeAndNil(Attribs) end;

  if SharedContext <> nil then
    ShareContextEgl := (SharedContext as TGLContextEgl).Context
  else
    ShareContextEgl := EGL_NO_CONTEXT;
  Context := eglCreateContext(Display, Config, ShareContextEgl, @ContextAttribsv3);
  if Context = EGL_NO_CONTEXT then
    Context := eglCreateContext(Display, Config, ShareContextEgl, @ContextAttribsv2);
  if Context = EGL_NO_CONTEXT then
    raise EGLContextNotPossible.Create('EGL: Cannot create context: ' + EGLError);

  Surface := eglCreateWindowSurface(Display, Config, WndPtr, nil);
  if Surface = EGL_NO_SURFACE then
    raise EGLContextNotPossible.Create('EGL: Cannot create surface: ' + EGLError);
end;

procedure TGLContextEgl.ContextDestroy;
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

procedure TGLContextEgl.MakeCurrent;
begin
  if eglMakeCurrent(Display, Surface, Surface, Context) = EGL_FALSE then
    WritelnWarning('EGL', 'Cannot make context current: ' + EGLError);
end;

procedure TGLContextEgl.SwapBuffers;
begin
  if eglSwapBuffers(Display, Surface) = EGL_FALSE then
    WritelnWarning('EGL', 'Cannot swap buffers (this is normal if app is no longer active): ' + EGLError);
end;

procedure TGLContextEgl.QuerySize(out AWidth, AHeight: EGLint);
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

end.
