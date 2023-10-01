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
CastleInternalContextGlx,
{$endif}

{$ifdef read_window_interface}
private
  Context: TGLContextGlx;

  // Initialized by ContextCreateEarly, freed by ContextDestroyLate
  XVisual: PXVisualInfo;

  procedure ContextCreateEarly;
  procedure ContextCreate;
  procedure ContextDestroy;
  procedure ContextDestroyLate;
  { @groupEnd }
{$endif}

{$ifdef read_implementation}

procedure TCastleWindow.ContextCreateEarly;
begin
  Context := TGLContextGlx.Create;
  { All backends that can use glX (this means now Xlib and GTK)
    must define Application.XDisplay / XScreen. }
  Context.XDisplay := Application.XDisplay;
  Context.XScreen := Application.XScreen;
  { All OpenGL contexts created by CGE should be shared }
  if Application.OpenWindowsCount <> 0 then
    Context.SharedContext := Application.OpenWindows[0].Context;
  Context.ContextCreateEarly(FRequirements);
  XVisual := Context.XVisual;
end;

procedure TCastleWindow.ContextCreate;
begin
  Assert(Context <> nil);
  Context.ContextCreate(FRequirements);
end;

procedure TCastleWindow.ContextDestroy;
begin
  if Context <> nil then
  begin
    Context.ContextDestroy;
    // FreeAndNil(Context); // leave it for ContextDestroyLate
  end;
end;

procedure TCastleWindow.ContextDestroyLate;
begin
  if Context <> nil then
  begin
    Context.ContextDestroyLate;
    FreeAndNil(Context);
  end;
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  Assert(not Closed);
  Assert(Context <> nil);
  Context.WindowXID := WindowXID;
  Context.MakeCurrent;
end;

procedure TCastleWindow.SwapBuffers;
begin
  Assert(not Closed);
  Assert(Context <> nil);
  Context.WindowXID := WindowXID;
  Context.SwapBuffers;
end;

{$endif read_implementation}
