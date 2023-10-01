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
  (initialize window with OpenGL ES context). }

{$ifdef read_interface_uses}
CastleInternalContextEgl,
{$endif}

{$ifdef read_window_interface}
private
  Context: TGLContextEgl;
  procedure ContextCreate(WndPtr: EGLNativeWindowType);
  procedure ContextDestroy;
public
  { Query the EGL context what is current size. }
  procedure QuerySize(out AWidth, AHeight: EGLint);
{$endif}

{$ifdef read_implementation}

procedure TCastleWindow.ContextCreate(WndPtr: EGLNativeWindowType);
begin
  Context := TGLContextEgl.Create;
  Context.WndPtr := WndPtr;
  { All OpenGL contexts created by CGE should be shared }
  if Application.OpenWindowsCount <> 0 then
    Context.SharedContext := Application.OpenWindows[0].Context;
  Context.ContextCreate(FRequirements);
end;

procedure TCastleWindow.ContextDestroy;
begin
  if Context <> nil then
  begin
    Context.ContextDestroy;
    FreeAndNil(Context);
  end;
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  Assert(not Closed);
  Assert(Context <> nil);
  Context.MakeCurrent;
end;

procedure TCastleWindow.SwapBuffers;
begin
  Assert(not Closed);
  Assert(Context <> nil);
  Context.SwapBuffers;
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

{$endif read_implementation}
