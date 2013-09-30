{
  Copyright 2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Use this from castlewindow_xlib.inc to make a CastleWindow backend
  using Xlib and EGL (initialize window on X, without any toolkit,
  with OpenGL ES context).

  Written based on
  http://www.khronos.org/registry/egl/sdk/docs/man/xhtml/eglIntro.html .
  See http://www.khronos.org/registry/egl/sdk/docs/man/xhtml/
  for an up-to-date reference of EGL.
  FPC example in packages/opengles/examples/es2example1.pas
  was also useful. }

{$ifdef read_interface_uses}
{$endif}

{$ifdef read_window_interface}
private
  Context: EGLContext;
  Surface: EGLSurface;
  Display: EGLDisplay;
{$endif}

{$ifdef read_implementation}

procedure TCastleWindowBase.ContextCreateBegin(var Attr: TXSetWindowAttributes;
  var AttrValueMask: Cardinal);
begin
end;

procedure TCastleWindowBase.ContextCreateEnd;
var
  Config: EGLConfig;
  ShareContext: EGLContext;
  NumConfig: EGLint;
  Attribs: TLongIntList;
const
  ContextAttribs: array [0..2] of EGLint =
  ( EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
begin
  Display := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if Display = EGL_NO_DISPLAY then
    raise EGLContextNotPossible.Create('Cannot create EGL display');

  eglInitialize(Display, nil, nil);

  Attribs := TLongIntList.Create;
  try
    Attribs.AddArray([
      EGL_DEPTH_SIZE, DepthBits,
      EGL_RED_SIZE  , Max(1, RedBits),
      EGL_GREEN_SIZE, Max(1, GreenBits),
      EGL_BLUE_SIZE , Max(1, BlueBits),
      EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
      EGL_NONE
    ]);
    eglChooseConfig(Display, PEGLint(Attribs.List), @Config, 1, @NumConfig);
  finally FreeAndNil(Attribs) end;

  if Application.FOpenWindows.Count <> 0 then
    ShareContext := Application.FOpenWindows[0].Context else
    ShareContext := EGL_NO_CONTEXT;
  Context := eglCreateContext(Display, Config, ShareContext, @ContextAttribs);

  Surface := eglCreateWindowSurface(Display, Config, WindowXID, nil);
  if Surface = EGL_NO_SURFACE then
    raise EGLContextNotPossible.Create('Cannot create EGL surface');
end;

procedure TCastleWindowBase.ContextDestroy;
begin
  if Surface <> EGL_NO_SURFACE { nil } then
    eglDestroySurface(Display, Surface);

  if Display <> EGL_NO_DISPLAY { nil } then
    eglTerminate(Display);
end;

procedure TCastleWindowBase.BackendMakeCurrent;
begin
  Assert(not Closed);
  eglMakeCurrent(Display, Surface, Surface, Context);
end;

procedure TCastleWindowBase.SwapBuffers;
begin
  eglSwapBuffers(Display, Surface);
end;

procedure TCastleApplication.ContextApplicationCheck;
begin
end;

{$endif read_implementation}
