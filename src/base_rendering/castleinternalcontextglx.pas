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

{ Initializes OpenGL using Unix glX. }
unit CastleInternalContextGlx;

{$i castleconf.inc}

interface

{$ifndef UNIX_WITH_X}
{ To make it easier to organize Lazarus packages
  (to keep this unit in castle_base.lpk, since it doesn't depend on TCastleWindow,
  but at the same time this unit cannot have "<AddToUsesPkgSection Value="False"/>"
  in castle_base.lpk because castle_window.lpk expects to use this unit
  without recompiling...) ->
  this unit compiles even for platforms that don't have glX,
  the unit just doesn't define anything in this case. }
implementation
end.
{$else}

uses SysUtils, Classes,
  Glx, X, XUtil, Xlib,
  CastleInternalContextBase;

type
  { Initializes OpenGL using Unix glX. }
  TGLContextGlx = class(TGLContext)
  private
    Context: GLXContext;
    UseFBConfig: boolean;
    FBConfig: TGLXFBConfig;
    procedure GlxAvailableCheck;
  protected
    procedure InitializeCore(const Requirements: TGLContextRequirements); override;
    procedure FinalizeCore; override;
    procedure MakeCurrentCore; override;
    procedure SwapBuffersCore; override;
  public
    { Set this before using Initialize and other methods.

      Note: screen of a window can change at runtime.
      But this class doesn't use XScreen after context initialization. }
    XDisplay: PDisplay;
    XScreen: Integer;

    // Initialized by InitializeEarly, freed by FinalizeLate
    XVisual: PXVisualInfo;

    // Set before SwapBuffer, MakeCurrent
    // TODO: simplify, require it before Initialize
    WindowXID: TWindow;

    { Call before creating an X window (since window creation needs XVisual)
      to create XVisual. }
    procedure InitializeEarly(const Requirements: TGLContextRequirements);

    { Call after window is destroyed to safely free XVisual.}
    procedure FinalizeLate;
  end;

implementation

uses CTypes,
  CastleLog, CastleUtils, CastleGLUtils;

{ TGLContextGlx -------------------------------------------------------------- }

procedure TGLContextGlx.GlxAvailableCheck;

  function GlxConfirmedVersion: String;
  begin
    if GLX_version_1_4(XDisplay) then
      GlxConfirmedVersion := '1.4'
    else
    if GLX_version_1_3(XDisplay) then
      GlxConfirmedVersion := '1.3'
    else
    if GLX_version_1_2(XDisplay) then
      GlxConfirmedVersion := '1.2'
    else
    if GLX_version_1_1(XDisplay) then
      GlxConfirmedVersion := '1.1'
    else
    if GLX_version_1_0(XDisplay) then
      GlxConfirmedVersion := '1.0'
    else
      GlxConfirmedVersion := 'None';
  end;

var
  GlxExtensions: String;
begin
  if not GLX_version_1_0(XDisplay) then
  begin
    raise Exception.Create('glX extension (version at least 1.0) not found (necessary for OpenGL-based programs)');
  end else
  begin
    GlxExtensions := glXQueryExtensionsString(XDisplay, XScreen);
    if LogGLInformationVerbose then
    begin
      WritelnLogMultiline('GLX', 'glX extension at least 1.0 found.' +NL+
        'Versions (determined by checking both glXQueryExtension, glXQueryVersion and assigned entry points):' +NL+
        Format('  Version 1.1: %s', [BoolToStr(GLX_version_1_1(XDisplay), true)]) +NL+
        Format('  Version 1.2: %s', [BoolToStr(GLX_version_1_2(XDisplay), true)]) +NL+
        Format('  Version 1.3: %s', [BoolToStr(GLX_version_1_3(XDisplay), true)]) +NL+
        Format('  Version 1.4: %s', [BoolToStr(GLX_version_1_4(XDisplay), true)]) +NL+
        NL+
        'Important extensions (determined by checking glXQueryExtensionsString and assigned entry points):' +NL+
        Format('  GLX_ARB_multisample: %s'   , [BoolToStr(GLX_ARB_multisample(XDisplay, XScreen), true)]) + NL+
        Format('  GLX_ARB_create_context: %s', [BoolToStr(GLX_ARB_create_context(XDisplay, XScreen), true)]) +NL+
        NL+
        'All extensions (according to glXQueryExtensionsString):' +NL+
        GlxExtensions);
    end else
    begin
      WritelnLog('glX initialized (version >= %s). Multisample possible: %s, modern context creation: %s (more info: LogGLInformationVerbose:=true)', [
        GlxConfirmedVersion,
        BoolToStr(GLX_ARB_multisample(XDisplay, XScreen), true),
        BoolToStr(GLX_ARB_create_context(XDisplay, XScreen), true)
      ]);
    end;
  end;
end;

procedure TGLContextGlx.InitializeEarly(const Requirements: TGLContextRequirements);
var
  Attribs: TInt32List;
  FBConfigs: PGLXFBConfig;
  FBConfigsCount: Integer;
begin
  GlxAvailableCheck;

  { Use new glX 1.3 functions to create context, that take TGLXFBConfig?
    About modern GLX context selection, see
    http://www.opengl.org/wiki/Creating_an_OpenGL_Context and
    http://www.opengl.org/wiki/Tutorial:_OpenGL_3.0_Context_Creation_%28GLX%29 }
  UseFBConfig := GLX_version_1_3(XDisplay);

  Attribs := TInt32List.Create;
  try
    { constant Attribs part }
    if UseFBConfig then
      Attribs.AddRange([GLX_X_RENDERABLE, 1 { true }]) else
      Attribs.Add(GLX_RGBA);

    if Requirements.DoubleBuffer then
      if UseFBConfig then
        Attribs.AddRange([GLX_DOUBLEBUFFER, 1]) else
        Attribs.Add(GLX_DOUBLEBUFFER);

    Attribs.AddRange([
      { Buffer sizes below are all in bits. }
      GLX_DEPTH_SIZE, Requirements.DepthBits,
      GLX_STENCIL_SIZE, Requirements.StencilBits,
      GLX_ALPHA_SIZE, Requirements.AlphaBits
    ]);
    if Requirements.RedBits   <> 0 then Attribs.AddRange([GLX_RED_SIZE  , Requirements.RedBits  ]);
    if Requirements.GreenBits <> 0 then Attribs.AddRange([GLX_GREEN_SIZE, Requirements.GreenBits]);
    if Requirements.BlueBits  <> 0 then Attribs.AddRange([GLX_BLUE_SIZE , Requirements.BlueBits ]);

    if Requirements.MultiSampling > 1 then
    begin
      if GLX_ARB_multisample(XDisplay, XScreen) then
      begin
        Attribs.AddRange([
          GLX_SAMPLE_BUFFERS_ARB, 1,
          GLX_SAMPLES_ARB, Requirements.MultiSampling ]);
        WritelnLog('MultiSampling', 'GLX_ARB_multisample supported, using multisampling');
      end else
        raise EGLContextNotPossible.CreateFmt('Multisampling (%d samples) ' +
          'requested, but GLX_ARB_multisample not supported on this screen',
          [Requirements.MultiSampling]);
    end;

    { end of Attribs array }
    Attribs.Add(None);

    if UseFBConfig then
    begin
      FBConfigs := glXChooseFBConfig(XDisplay, XScreen,
        Attribs.L, FBConfigsCount);
      if FBConfigsCount = 0 then
        raise EGLContextNotPossible.CreateFmt('No frame buffer configurations that match the specified attributes (%s)',
          [Requirements.RequestedBufferAttributes]);
      { just choose the first FB config from the FBConfigs list.
        More involved selection possible. }
      FBConfig := FBConfigs^;
      XVisual := glXGetVisualFromFBConfig(XDisplay, FBConfig);
    end else
      XVisual := glXChooseVisual(XDisplay, XScreen, Attribs.L);
  finally FreeAndNil(Attribs) end;

  if XVisual = nil then
    raise EGLContextNotPossible.CreateFmt(
      'X visual with requested attributes (%s) not found',
      [ Requirements.RequestedBufferAttributes ]);
  { XScreen should always be XVisual.screen now }
end;

procedure TGLContextGlx.InitializeCore(const Requirements: TGLContextRequirements);
var
  ShareContextGlx: GLXContext;

  function UseCreateContextAttribsARB: GLXContext;
  var
    Attribs: TInt32List;
  begin
    { TODO:

      We should catch X errors (see XSync and XSetErrorHandler inside
      http://www.opengl.org/wiki/Tutorial:_OpenGL_3.0_Context_Creation_%28GLX%29 )
      and fallback on non-modern context,
      not using glXCreateContextAttribsARB, if it is impossible to get modern one.

      Right now, our XLibUtils catches X errors, but without XSync
      it's not known where they occur anyway. }

    Attribs := TInt32List.Create;
    try
      { Select 3.2 core context.
        See https://registry.khronos.org/OpenGL/extensions/ARB/GLX_ARB_create_context.txt }
      if TGLFeatures.RequestCapabilities = rcForceModern then
      begin
        Attribs.AddRange([
          GLX_CONTEXT_MAJOR_VERSION_ARB, TGLFeatures.ModernVersionMajor,
          GLX_CONTEXT_MINOR_VERSION_ARB, TGLFeatures.ModernVersionMinor,
          GLX_CONTEXT_PROFILE_MASK_ARB, GLX_CONTEXT_CORE_PROFILE_BIT_ARB
          { Not used, following https://www.khronos.org/opengl/wiki/OpenGL_Context#Context_types
            forward-compatible really only makes sense on macOS.
            The "core" profile is what should be just used with new OpenGLs on sane
            (non-macOS) platforms. }
          //GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        ]);
      end;
      { Select debug context.
        Note that the implementation assumes nothing above passes GLX_CONTEXT_FLAGS_ARB. }
      if TGLFeatures.Debug then
      begin
        Attribs.AddRange([
          GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_DEBUG_BIT_ARB
        ]);
      end;
      Attribs.AddRange([None]);
      Assert(SizeOf(CInt) = SizeOf(Int32)); // make sure Attribs match PCInt type
      Result := glXCreateContextAttribsARB(XDisplay, FBConfig, ShareContextGlx, true, PCInt(Attribs.L));
    finally FreeAndNil(Attribs) end;
  end;

begin
  { All OpenGL contexts should be shared }
  if SharedContext <> nil then
    ShareContextGlx := (SharedContext as TGLContextGlx).Context
  else
    ShareContextGlx := nil;

  if UseFBConfig then
  begin
    if (TGLFeatures.RequestCapabilities = rcForceModern) or
       TGLFeatures.Debug then
    begin
      if not GLX_ARB_create_context_profile(XDisplay, XScreen) then
        raise EGLContextNotPossible.Create('GLX_ARB_create_context_profile not available, cannot initialize context with "core" profile or debug bit');
      Context := UseCreateContextAttribsARB;
    end else
      Context := glXCreateNewContext(XDisplay, FBConfig, GLX_RGBA_TYPE, ShareContextGlx, true);
  end else
    Context := glXCreateContext(XDisplay, XVisual, ShareContextGlx, true);

  Check(Context <> nil, 'Could not initialize rendering context');
end;

procedure TGLContextGlx.FinalizeCore;
begin
  if Context <> nil then
  begin
    glXDestroyContext(XDisplay, Context);
    Context := nil;
  end;
end;

procedure TGLContextGlx.FinalizeLate;
begin
  if XVisual <> nil then
  begin
    XFree(XVisual);
    XVisual := nil;
  end;
end;

procedure TGLContextGlx.MakeCurrentCore;
begin
  Check( glXMakeCurrent(XDisplay, WindowXID, Context), 'glXMakeCurrent');
end;

procedure TGLContextGlx.SwapBuffersCore;
begin
  glXSwapBuffers(XDisplay, WindowXID);
end;

end.

{$endif}