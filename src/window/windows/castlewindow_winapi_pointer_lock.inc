{%MainUnit ../castlewindow.pas}
{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Pointer lock implementation on Windows. }

type
  { Pointer lock implementation on Windows.
    We inherit cross-platform logic of @link(TCastleDesktopPointerLock),
    adding additional robustness using Windows API.

    The caller (@link(TCastleWindow) on WinAPI) must keep
    the WindowHandle valid and call UpdateClipCursor. }
  TCastleWindowsPointerLock = class(TCastleDesktopPointerLock)
  strict private
    UpdatingClipCursor: Boolean;
  protected
    procedure ActiveChanged(const NewValue: Boolean); override;
  public
    WindowHandle: HWND;

    { If we are updating ClipCursor, do it now.
      You need to call this whenever size or position of the window
      on the screen changes. }
    procedure UpdateClipCursor;

    { Called when the window is closing (TCastleWindow.CloseBackend).
      Clear our state (WindowHandle) and release any pending WinAPI stuff. }
    procedure WindowClosing;
  end;

procedure TCastleWindowsPointerLock.ActiveChanged(const NewValue: Boolean);
begin
  inherited;

  (*The idea for this comes from Godot Engine, which does
    (see https://github.com/godotengine/godot/blob/master/platform/windows/display_server_windows.cpp ):

      ClipCursor(&clipRect);
      if (p_mode == DisplayServerEnums::MOUSE_MODE_CAPTURED) {
        ....
        SetCursorPos(pos.x, pos.y);
        SetCapture(wd.hWnd);

        _register_raw_input_devices(window_id);
      }

    We take from it 2 inspirations:
    1.Do SetCapture + ReleaseCapture to be able to grab the mouse "back into
      our window" if it tries to escape.
    2.Do ClipCursor to prevent it escaping from the window.
    3.Not done for now, but a useful future idea: we could read deltas
      by RegisterRawInputDevices, which would avoid the need to reposition
      the mouse / care whether mouse is close to screen edge, from what I
      understand.
      This would mean we can avoid depending on quite complicated
      TCastleDesktopPointerLock delta + reposition logic.

    Hints how to test this:
    - use explore_impressive_castle example
    - hack castleuicontrols_pointer_lock_desktop.inc to
      - comment out 2x lines doing Container.FInternalOverrideCursor := ...
      - comment line doing Container.MousePosition := ...
    This way, TCastleDesktopPointerLock will not hide and reposition the mouse,
    and you should nicely observe how mouse is confined to the window thanks
    to ClipCursor done here.
  *)

  if NewValue then
  begin
    SetCapture(WindowHandle);

    UpdatingClipCursor := true;
    UpdateClipCursor;
  end else
  begin
    UpdatingClipCursor := false;
    OSCheckWarning(ClipCursor(nil), 'ClipCursor(nil)');

    OSCheckWarning(ReleaseCapture(), 'ReleaseCapture');
  end;
end;

procedure TCastleWindowsPointerLock.UpdateClipCursor;
var
  ClipRect: Windows.TRect;
begin
  { Do nothing if called while we don't have pointer lock active. }
  if not UpdatingClipCursor then Exit;

  { Note: GetWindowRect is not good, because it returns position and size
    of the whole window, including title bar and borders. We want to clip
    the cursor to the client area (the OpenGL context), so we use GetClientRect
    instead. }
  //if not GetWindowRect(WindowHandle, ClipRect) then

  if not GetClientRect(WindowHandle, ClipRect) then
  begin
    WritelnWarning('WinAPI', 'GetClientRect failed in UpdateClipCursor: ' + SysErrorMessage(GetLastError));
    Exit;
  end;

  OSCheckWarning(ClientToScreen(WindowHandle, ClipRect.TopLeft),
    'ClientToScreen TopLeft in UpdateClipCursor');
  OSCheckWarning(ClientToScreen(WindowHandle, ClipRect.BottomRight),
    'ClientToScreen BottomRight in UpdateClipCursor');

  { Debug log:
  WritelnLog('WinAPI', 'UpdateClipCursor: ClipRect (in screen coordinates) = ' +
    Format('(%d, %d, %d, %d)', [
      ClipRect.Left,
      ClipRect.Top,
      ClipRect.Right,
      ClipRect.Bottom
    ]));
  }

  { Weird values are possible if we manage to minimize the window while
    pointer lock is active.
    - GetWindowRect can return values like (-32000, -32000, -31840, -31972)
    - GetClientRect can return values like (-32000, -32000, 0, 0)
    But we don't want to detect them explicitly, because on multi-monitor setups,
    negative coordinates are valid (for monitors to the left or above primary monitor).
    So use IsIconic to detect minimized window.
    In this case, we just don't clip the cursor. }
  // if (ClipRect.Left <= 0) and
  //    (ClipRect.Top <= 0) and
  //    (ClipRect.Right <= 0) and
  //    (ClipRect.Bottom <= 0) then
  if IsIconic(WindowHandle) then
  begin
    WritelnLog('WinAPI', 'Window seems to be minimized during pointer lock, not clipping cursor');
    OSCheckWarning(ClipCursor(nil), 'ClipCursor(nil) because window is minimized');
    Exit;
  end;

  OSCheckWarning(ClipCursor(@ClipRect),
    Format('ClipCursor(Rect(%d, %d, %d, %d))', [
      ClipRect.Left,
      ClipRect.Top,
      ClipRect.Right,
      ClipRect.Bottom
    ]));
end;

procedure TCastleWindowsPointerLock.WindowClosing;
begin
  if UpdatingClipCursor then
  begin
    UpdatingClipCursor := false;
    OSCheckWarning(ClipCursor(nil), 'ClipCursor(nil) because window is closing');
  end;
  WindowHandle := 0;
end;
