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

{ Check that we already have a working rendering context. }
unit CastleInternalContextExisting;

{$i castleconf.inc}

interface

uses SysUtils, Classes,
  CastleInternalContextBase;

type
  { Check that we already have a working rendering context.
    Useful when the context is initialized by something else
    (like FireMonkey on Delphi/Android or Delphi/iOS), and we just want to use it. }
  TGLContextExisting = class(TGLContext)
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

{$if defined(ANDROID)} {$define USE_EGL} {$endif ANDROID}

uses Math,
  // for EGL API
  {$if defined(USE_EGL)} CastleInternalEgl, {$endif}
  // for TCustomAndroidContext.SharedDisplay
  {$if defined(DELPHI) and defined(ANDROID)} FMX.Context.GLES.Android, {$endif}
  // for TCustomContextIOS.SharedContext
  {$if defined(DELPHI) and defined(IOS)} FMX.Context.GLES.iOS, {$endif}
  CastleLog, CastleUtils, CastleGLUtils, CastleGLES;

class function TGLContextExisting.CheckRenderingContextAvailable: Boolean;

  procedure LoadLibraries;
  begin
    {$if defined(USE_EGL)}
    LoadEgl;
    {$endif USE_EGL}

    GLESInitialization;
  end;

{$if defined(USE_EGL)}
var
  EglVersion: PAnsiChar;
  Display: EGLDisplay;
  Surface: EGLSurface;
  Context: EGLContext;
{$endif}
begin
  Result := false;

  // load libraries entry points, on-demand, once
  if not FLoadLibrariesDone then
  begin
    FLoadLibrariesDone := true;
    LoadLibraries;
  end;

  {$if defined(DELPHI) and defined(ANDROID)}
  { Display is necessary for most EGL commands, evet to get EGL version.
    Get the same one as FMX uses (it's just initialized to
    eglGetDisplay(EGL_DEFAULT_DISPLAY) by FMX now). }
  if TCustomAndroidContext.SharedDisplay = nil then
  begin
    WritelnWarning('EGL display not yet initialized by FMX');
    Exit;
  end;
  {$endif}

  {$if defined(DELPHI) and defined(IOS)}
  if TCustomContextIOS.SharedContext = nil then
  begin
    WritelnWarning('Rendering context not yet initialized by FMX');
    Exit;
  end;
  {$endif}

  {$if defined(USE_EGL)}
  if not EglAvailable then
  begin
    WritelnWarning('EGL library not available'); // Cannot render using Castle Game Engine on Delphi/Android.');
    Exit;
  end;

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
  {$endif}

  WritelnLog('Existing rendering context created by FireMonkey looks valid, we will use it for Castle Game Engine');
  Result := true;
end;

procedure TGLContextExisting.InitializeCore(const Requirements: TGLContextRequirements);
begin
  if not CheckRenderingContextAvailable then
    raise Exception.Create('Cannot use existing rendering context: not available or not valid');
end;

procedure TGLContextExisting.FinalizeCore;
begin
  // Nothing to do, we don't own the context.
end;

procedure TGLContextExisting.MakeCurrentCore;
begin
  // No need to do anything, FMX will do this for us
end;

procedure TGLContextExisting.SwapBuffersCore;
begin
  // No need to do anything, FMX will do this for us
end;

end.
