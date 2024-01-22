{
  Copyright 2013-2024 Michalis Kamburelis.

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
    class var
      Display: EGLDisplay;
      EGLMajor, EGLMinor: EGLint;
    var
      Context: EGLContext;
      Surface: EGLSurface;
    { Call eglCreateContext, hiding some differences between
      OpenGLES and OpenGL needs. }
    function CallEglCreateContext(Config: EGLConfig;
      ShareContextEgl: EGLContext): EGLContext;
    { Check is EGL version at least as specified. }
    function VersionAtLeast(const Major, Minor: EGLint): Boolean;

    { Initialize EGL library, Display, and version (EGLMajor, EGLMinor).
      Does nothing if already initialized.

      The Display is a class variable, so it's shared between all contexts.
      This is important, as we cannot terminate eglTerminate(Display)
      just in any FinalizeCore.
      eglTerminate(Display) it would release Display of all contexts.
      We need to call eglTerminate(Display) only when finalizing the last context. }
    procedure ClassInitialize;

    { Finalize Display.
      Does nothing if already finalized. }
    procedure ClassFinalize;
  protected
    procedure InitializeCore(const Requirements: TGLContextRequirements); override;
    procedure FinalizeCore; override;
    procedure MakeCurrentCore; override;
    procedure SwapBuffersCore; override;
  public
    // Set this before using Initialize and other methods
    WndPtr: EGLNativeWindowType;

    { Query context size, returns 0 0 if cannot query for some reason. }
    procedure QuerySize(out AWidth, AHeight: EGLint);
  end;

implementation

uses Math,
  CastleLog, CastleUtils, CastleGLUtils;

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

function TGLContextEgl.VersionAtLeast(const Major, Minor: EGLint): Boolean;
begin
  Result := (EGLMajor > Major) or
    ((EGLMajor = Major) and (EGLMinor >= Minor));
end;

{$ifdef OpenGLES}

{ Call eglCreateContext, passing attributes to initialize OpenGL ES 3,
  eventually fallback on OpenGL ES 2. }
function TGLContextEgl.CallEglCreateContext(Config: EGLConfig;
  ShareContextEgl: EGLContext): EGLContext;
const
  ContextAttribsEs2: array [0..2] of EGLint =
  ( EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
  ContextAttribsEs3: array [0..2] of EGLint =
  ( EGL_CONTEXT_CLIENT_VERSION, 3, EGL_NONE);
begin
  Result := eglCreateContext(Display, Config, ShareContextEgl, @ContextAttribsEs3);
  if Result = EGL_NO_CONTEXT then
    Context := eglCreateContext(Display, Config, ShareContextEgl, @ContextAttribsEs2);
end;

{$else}

{ Call eglCreateContext, passing attributes to initialize OpenGL.
  We may request "core" profile or debug context,
  following TGLFeatures.RequestCapabilities. }
function TGLContextEgl.CallEglCreateContext(Config: EGLConfig;
  ShareContextEgl: EGLContext): EGLContext;
var
  ContextAttribs: TInt32List;
begin
  if VersionAtLeast(1, 4) then
  begin
    if eglBindAPI(EGL_OPENGL_API) <> EGL_TRUE then
      raise EGLContextNotPossible.Create('EGL: Cannot bind OpenGL API. Error reported: ' + EGLError);
  end else
    raise EGLContextNotPossible.Create('EGL version is too old to initialize OpenGL (not ES), we need at least 1.4');

  ContextAttribs := TInt32List.Create;
  try
    if TGLFeatures.RequestCapabilities = rcForceModern then
    begin
      if VersionAtLeast(1, 5) then
        ContextAttribs.AddRange([
          EGL_CONTEXT_MAJOR_VERSION, TGLFeatures.ModernVersionMajor,
          EGL_CONTEXT_MINOR_VERSION, TGLFeatures.ModernVersionMinor,
          EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT
          { Not used, following https://www.khronos.org/opengl/wiki/OpenGL_Context#Context_types
            forward-compatible really only makes sense on macOS.
            The "core" profile is what should be just used with new OpenGLs on sane
            (non-macOS) platforms. }
          //EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE, EGL_TRUE,
        ])
      else
        WritelnWarning('OpenGL core context could not be requested, because EGL version is too old');
    end;
    if TGLFeatures.Debug then
    begin
      if VersionAtLeast(1, 5) then
        ContextAttribs.AddRange([
          EGL_CONTEXT_OPENGL_DEBUG, EGL_TRUE
        ])
      else
        WritelnWarning('OpenGL debug context could not be requested, because EGL version is too old');
    end;
    ContextAttribs.Add(EGL_NONE);
    Result := eglCreateContext(Display, Config, ShareContextEgl, PEGLint(ContextAttribs.L));
  finally FreeAndNil(ContextAttribs) end;
end;

{$endif}

procedure TGLContextEgl.ClassInitialize;
begin
  if Display = EGL_NO_DISPLAY { nil } then
  begin
    if not EglAvailable then
      raise EGLContextNotPossible.Create('Could not load EGL library, required to initialize context');

    Display := eglGetDisplay(EGL_DEFAULT_DISPLAY);
    if Display = EGL_NO_DISPLAY then
      { This does not set eglGetError, so we don't use EGLError in message below. }
      raise EGLContextNotPossible.Create('EGL: Cannot get display');

    if eglInitialize(Display, @EGLMajor, @EGLMinor) = EGL_FALSE then
      raise EGLContextNotPossible.Create('EGL: Cannot initialize: ' + EGLError);
    WritelnLog('EGL initialized (version %d.%d).', [EGLMajor, EGLMinor]);
  end;
end;

procedure TGLContextEgl.ClassFinalize;
begin
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

procedure TGLContextEgl.InitializeCore(const Requirements: TGLContextRequirements);
var
  Config: EGLConfig;
  ShareContextEgl: EGLContext;
  NumConfig: EGLint;
  ConfigAttribs: TInt32List;
begin
  ClassInitialize;

  ConfigAttribs := TInt32List.Create;
  try
    if Requirements.StencilBits > 0 then
      ConfigAttribs.AddRange([EGL_STENCIL_SIZE, Requirements.StencilBits]);
    if Requirements.AlphaBits > 0 then
      ConfigAttribs.AddRange([EGL_ALPHA_SIZE, Requirements.AlphaBits]);
    ConfigAttribs.AddRange([
      EGL_DEPTH_SIZE, Requirements.DepthBits,
      EGL_RED_SIZE  , Max(1, Requirements.RedBits),
      EGL_GREEN_SIZE, Max(1, Requirements.GreenBits),
      EGL_BLUE_SIZE , Max(1, Requirements.BlueBits),
      EGL_RENDERABLE_TYPE,
        {$ifdef OpenGLES} EGL_OPENGL_ES2_BIT, {$else} EGL_OPENGL_BIT, {$endif}
      EGL_NONE
    ]);
    if eglChooseConfig(Display, PEGLint(ConfigAttribs.L), @Config, 1, @NumConfig) = EGL_FALSE then
      raise EGLContextNotPossible.Create('EGL: Cannot choose config: ' + EGLError);
  finally FreeAndNil(ConfigAttribs) end;

  if SharedContext <> nil then
    ShareContextEgl := (SharedContext as TGLContextEgl).Context
  else
    ShareContextEgl := EGL_NO_CONTEXT;
  Context := CallEglCreateContext(Config, ShareContextEgl);
  if Context = EGL_NO_CONTEXT then
    raise EGLContextNotPossible.Create('EGL: Cannot create context: ' + EGLError);

  Surface := eglCreateWindowSurface(Display, Config, WndPtr, nil);
  if Surface = EGL_NO_SURFACE then
    raise EGLContextNotPossible.Create('EGL: Cannot create surface: ' + EGLError);
end;

procedure TGLContextEgl.FinalizeCore;
begin
  if Surface <> EGL_NO_SURFACE { nil } then
  begin
    if eglDestroySurface(Display, Surface) = EGL_FALSE then
      WritelnWarning('EGL', 'Cannot destroy surface: ' + EGLError);
    Surface := EGL_NO_SURFACE;
  end;

  if InitializedContextsCount = 1 then // we are the last context
    ClassFinalize;
end;

procedure TGLContextEgl.MakeCurrentCore;
begin
  if eglMakeCurrent(Display, Surface, Surface, Context) = EGL_FALSE then
    WritelnWarning('EGL', 'Cannot make context current: ' + EGLError);
end;

procedure TGLContextEgl.SwapBuffersCore;
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
