{%MainUnit ../castlewindow.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Use this from castlewindow_xlib.inc to make a CastleWindow backend
  using Xlib and glX (initialize window on X, without any toolkit,
  with OpenGL context). }

{ Define this to use OpenGL 3 forward-compatible context.
  Not supported for now throughout CGE, but this define is useful to test it. }
{.$define CASTLE_CONTEXT_FORWARD_3}

{$ifdef read_interface_uses}
Glx,
{$endif}

{$ifdef read_window_interface}
private
  Context: GLXContext;
  XVisual: PXVisualInfo;

  { Used only by ContextCreateBegin and ContextCreateEnd }
  UseFBConfig: boolean;
  FBConfig: TGLXFBConfig;

  { Methods compatible with castlewindow_egl.inc, see there for docs.
    This creates XVisual in ContextCreateBegin,
    you *have* to use it (by creating a colormap and attaching it to your window)
    to actually use the context.
    @groupBegin }
  procedure ContextCreateBegin;
  procedure ContextCreateEnd(WndPtr: TWindow);
  procedure ContextDestroyBegin;
  procedure ContextDestroyEnd;
  { @groupEnd }
{$endif}

{$ifdef read_application_interface}
private
  { Methods compatible with castlewindow_egl.inc. }
  procedure ContextApplicationCheck;
{$endif}

{$ifdef read_implementation}

procedure TCastleWindow.ContextCreateBegin;
var
  Attribs: TInt32List;
  FBConfigs: PGLXFBConfig;
  FBConfigsCount: Integer;
begin
  { Use new glX 1.3 functions to create context, that take TGLXFBConfig?
    About modern GLX context selection, see
    http://www.opengl.org/wiki/Creating_an_OpenGL_Context and
    http://www.opengl.org/wiki/Tutorial:_OpenGL_3.0_Context_Creation_%28GLX%29 }
  UseFBConfig := GLX_version_1_3(Application.XDisplay);

  Attribs := TInt32List.Create;
  try
    { constant Attribs part }
    if UseFBConfig then
      Attribs.AddRange([GLX_X_RENDERABLE, 1 { true }]) else
      Attribs.Add(GLX_RGBA);

    if DoubleBuffer then
      if UseFBConfig then
        Attribs.AddRange([GLX_DOUBLEBUFFER, 1]) else
        Attribs.Add(GLX_DOUBLEBUFFER);

    Attribs.AddRange([
      { Buffer sizes below are all in bits. }
      GLX_DEPTH_SIZE, DepthBits,
      GLX_STENCIL_SIZE, StencilBits,
      GLX_ALPHA_SIZE, AlphaBits
    ]);
    if RedBits   <> 0 then Attribs.AddRange([GLX_RED_SIZE  , RedBits  ]);
    if GreenBits <> 0 then Attribs.AddRange([GLX_GREEN_SIZE, GreenBits]);
    if BlueBits  <> 0 then Attribs.AddRange([GLX_BLUE_SIZE , BlueBits ]);

    if MultiSampling > 1 then
    begin
      if GLX_ARB_multisample(Application.XDisplay, Application.XScreen) then
      begin
        Attribs.AddRange([
          GLX_SAMPLE_BUFFERS_ARB, 1,
          GLX_SAMPLES_ARB, MultiSampling ]);
        WritelnLog('MultiSampling', 'GLX_ARB_multisample supported, using multisampling');
      end else
        raise EGLContextNotPossible.CreateFmt('Multisampling (%d samples) ' +
          'requested, but GLX_ARB_multisample not supported on this screen',
          [MultiSampling]);
    end;

    { end of Attribs array }
    Attribs.Add(None);

    if UseFBConfig then
    begin
      FBConfigs := glXChooseFBConfig(Application.XDisplay, Application.XScreen,
        Attribs.L, FBConfigsCount);
      if FBConfigsCount = 0 then
        raise EGLContextNotPossible.CreateFmt('No frame buffer configurations that match the specified attributes (%s)',
          [RequestedBufferAttributes]);
      { just choose the first FB config from the FBConfigs list.
        More involved selection possible. }
      FBConfig := FBConfigs^;
      XVisual := glXGetVisualFromFBConfig(Application.XDisplay, FBConfig);
    end else
      XVisual := glXChooseVisual(Application.XDisplay, Application.XScreen, Attribs.L);
  finally FreeAndNil(Attribs) end;

  if XVisual = nil then
    raise EGLContextNotPossible.CreateFmt(
      'X visual with requested attributes (%s) not found',
      [ RequestedBufferAttributes ]);
  { Application.XScreen should always be XVisual.screen now }
end;

procedure TCastleWindow.ContextCreateEnd(WndPtr: TWindow);
var
  ShareContext: GLXContext;

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
      Result := glXCreateContextAttribsARB(Application.XDisplay, FBConfig, ShareContext, true, PCInt(Attribs.L));
    finally FreeAndNil(Attribs) end;
  end;

begin
  { All OpenGL contexts should be shared }
  if Application.OpenWindowsCount <> 0 then
    ShareContext := Application.OpenWindows[0].Context
  else
    ShareContext := nil;

  if UseFBConfig then
  begin
    if (TGLFeatures.RequestCapabilities = rcForceModern) or
       TGLFeatures.Debug then
    begin
      if not GLX_ARB_create_context_profile(Application.XDisplay, Application.XScreen) then
        raise EGLContextNotPossible.Create('GLX_ARB_create_context_profile not available, cannot OpenGL context with "core" profile or debug bit');
      Context := UseCreateContextAttribsARB;
    end else
      Context := glXCreateNewContext(Application.XDisplay, FBConfig, GLX_RGBA_TYPE, ShareContext, true);
  end else
    Context := glXCreateContext(Application.XDisplay, XVisual, ShareContext, true);

  Check(Context <> nil, 'Could not create OpenGL rendering context');
end;

procedure TCastleWindow.ContextDestroyBegin;
begin
  if Context <> nil then
  begin
    glXDestroyContext(Application.XDisplay, Context);
    Context := nil;
  end;
end;

procedure TCastleWindow.ContextDestroyEnd;
begin
  if XVisual   <> nil then
  begin
    XFree(XVisual);
    XVisual := nil;
  end;
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  Assert(not Closed);
  Check( glXMakeCurrent(Application.XDisplay, WindowXID, Context), 'glXMakeCurrent');
end;

procedure TCastleWindow.SwapBuffers;
begin
  glXSwapBuffers(Application.XDisplay, windowXID);
end;

procedure TCastleApplication.ContextApplicationCheck;
var
  GlxExtensions: string;
begin
  if not GLX_version_1_0(XDisplay) then
    raise Exception.Create('glX extension (version at least 1.0) not found (necessary for OpenGL-based programs)') else
  begin
    GlxExtensions := glXQueryExtensionsString(XDisplay, XScreen);
    WritelnLogMultiline('GLX', 'GLX extension at least 1.0 found.' +NL+
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
  end;
end;

{$endif read_implementation}
