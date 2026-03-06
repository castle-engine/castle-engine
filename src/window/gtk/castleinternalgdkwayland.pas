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
{ Using GDK (part of GTK) together with Wayland. }
unit CastleInternalGdkWayland;

{$I castleconf.inc}

interface

uses CTypes, CastleInternalGdk3;

type
  { Records defined just to make pointers below incompatible with each other,
    to avoid mistakes. }
  Twl_surface = record end;
  Twl_display = record end;
  Twl_egl_window = record end;

  { Pointer types for Wayland types. }
  Pwl_surface = ^Twl_surface;
  Pwl_display = ^Twl_display;
  Pwl_egl_window = ^Twl_egl_window;

{ Routines from GDK Wayland backend (libgdk-3.so). }
function gdk_wayland_window_get_wl_surface(window: PGdkWindow): Pwl_surface; cdecl; external LazGdk3_library;
function gdk_wayland_display_get_wl_display(display: PGdkDisplay): Pwl_display; cdecl; external LazGdk3_library;

function WaylandEglAvailable: Boolean;

var
  { Routines from libwayland-egl.so. Call @link(WaylandEglAvailable) first. }
  wl_egl_window_create: function (surface: Pwl_surface; width, height: CInt): Pwl_egl_window; cdecl;
  wl_egl_window_destroy: procedure (egl_window: Pwl_egl_window); cdecl;
  wl_egl_window_resize: procedure (egl_window: Pwl_egl_window; width, height: CInt; dx, dy: CInt); cdecl;

implementation

uses CastleDynLib;

var
  WaylandEglLibrary: TDynLib;

function WaylandEglAvailable: Boolean;
begin
  if WaylandEglLibrary = nil then
  begin
    WaylandEglLibrary := TDynLib.Load('libwayland-egl.so.1', false);
    if WaylandEglLibrary <> nil then
    begin
      Pointer({$ifndef FPC}@{$endif} wl_egl_window_create) := WaylandEglLibrary.Symbol('wl_egl_window_create');
      Pointer({$ifndef FPC}@{$endif} wl_egl_window_destroy) := WaylandEglLibrary.Symbol('wl_egl_window_destroy');
      Pointer({$ifndef FPC}@{$endif} wl_egl_window_resize) := WaylandEglLibrary.Symbol('wl_egl_window_resize');
    end;
  end;

  Result := WaylandEglLibrary <> nil;
end;

end.
