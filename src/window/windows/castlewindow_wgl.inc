{%MainUnit ../castlewindow.pas}
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

{ Use this from castlewindow_winapi.inc to make a CastleWindow backend
  using WinAPI and wgl. Initializes window on Windows with OpenGL context.

  At this point this is a thin wrapper that uses CastleInternalContextWgl
  to do the actual job. }

{$ifdef read_interface_uses}
CastleInternalContextWgl,
{$endif}

{$ifdef read_window_interface}
private
  Context: TGLContextWGL;

  { Methods compatible with castlewindow_egl.inc.
    @groupBegin }
  procedure ContextCreate(WndPtr: HWND);
  procedure ContextDestroy;
  { @groupEnd }
{$endif}

{$ifdef read_implementation}

procedure TCastleWindow.ContextCreate(WndPtr: HWND);
begin
  Context := TGLContextWGL.Create;
  Context.WndPtr := WndPtr;
  Context.h_Dc := h_Dc;
  Context.Initialize(FRequirements);
end;

procedure TCastleWindow.ContextDestroy;
begin
  if Context <> nil then
  begin
    Context.Finalize;
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

{$endif read_implementation}
