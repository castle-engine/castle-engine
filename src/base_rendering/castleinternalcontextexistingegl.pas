{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Check that we already have a working context using EGL API.
  Useful when the context is initialized by something else
  (like FireMonkey on Android), and we just want to use it. }
unit CastleInternalContextExistingEgl;

{$i castleconf.inc}

interface

uses SysUtils, Classes,
  CastleInternalContextBase, CastleInternalEgl;

type
  { Check that we already have a working context using EGL API.
    Useful when the context is initialized by something else
    (like FireMonkey on Android), and we just want to use it. }
  TGLContextExistingEgl = class(TGLContext)
  strict private
    class var
      FLoadLibrariesDone: Boolean;
    class function CheckRenderingContextAvailable: Boolean;
  protected
    procedure InitializeCore(const Requirements: TGLContextRequirements); override;
    procedure FinalizeCore; override;
    procedure MakeCurrentCore; override;
    procedure SwapBuffersCore; override;
  end;

implementation

uses Math,
  // for TCustomAndroidContext.SharedDisplay
  {$ifdef ANDROID} FMX.Context.GLES.Android, {$endif}
  CastleLog, CastleUtils, CastleGLUtils, CastleGLES;

class function TGLContextExistingEgl.CheckRenderingContextAvailable: Boolean;

  procedure LoadLibraries;
  begin
    LoadEgl;
    GLESInitialization;
  end;

var
  EglVersion: PAnsiChar;
  Display: EGLDisplay;
  Surface: EGLSurface;
  Context: EGLContext;
begin
  Result := false;

  // load libraries entry points, on-demand, once
  if not FLoadLibrariesDone then
  begin
    FLoadLibrariesDone := true;
    LoadLibraries;
  end;

  if not EglAvailable then
  begin
    WritelnWarning('EGL library not available'); // Cannot render using Castle Game Engine on Delphi/Android.');
    Exit;
  end;

  {$ifdef ANDROID}
  { Display is necessary for most EGL commands, evet to get EGL version.
    Get the same one as FMX uses (it's just initialized to
    eglGetDisplay(EGL_DEFAULT_DISPLAY) by FMX now). }
  if TCustomAndroidContext.SharedDisplay = nil then
  begin
    WritelnWarning('EGL display not yet initialized by FMX');
    Exit;
  end;
  {$endif ANDROID}

  EglVersion := eglQueryString(TCustomAndroidContext.SharedDisplay, EGL_VERSION);
  WritelnLog('EGL library available, version %s', [EglVersion]);

  Display := eglGetCurrentDisplay();
  if Display = EGL_NO_DISPLAY then
  begin
    WritelnWarning('EGL has no current display');
    Exit;
  end;

  if Display <> TCustomAndroidContext.SharedDisplay then
  begin
    WritelnWarning('EGL current display is not the same as FMX -- weird state(something else interacted with EGL, aborting)');
    Exit;
  end;

  Surface := eglGetCurrentSurface(EGL_DRAW);
  if Surface = EGL_NO_SURFACE then
  begin
    WritelnWarning('EGL has no current surface');
    Exit;
  end;

  Context := eglGetCurrentContext();
  if Context = EGL_NO_CONTEXT then
  begin
    WritelnWarning('EGL has no current context');
    Exit;
  end;

  WritelnLog('EGL current display, surface, and context are valid');
  Result := true;
end;

procedure TGLContextExistingEgl.InitializeCore(const Requirements: TGLContextRequirements);
begin
  if not CheckRenderingContextAvailable then
    raise Exception.Create('Cannot use existing EGL context: it is not available or not valid');
end;

procedure TGLContextExistingEgl.FinalizeCore;
begin
  // Nothing to do, we don't own the context.
end;

procedure TGLContextExistingEgl.MakeCurrentCore;
begin
  // No need to do anything, FMX will do this for us
  // if eglMakeCurrent(Display, Surface, Surface, Context) = EGL_FALSE then
  //   WritelnWarning('EGL', 'Cannot make context current: ' + EGLError);
end;

procedure TGLContextExistingEgl.SwapBuffersCore;
begin
  // No need to do anything, FMX will do this for us
  // if eglSwapBuffers(Display, Surface) = EGL_FALSE then
  //   WritelnWarning('EGL', 'Cannot swap buffers (this is normal if app is no longer active): ' + EGLError);
end;

end.
