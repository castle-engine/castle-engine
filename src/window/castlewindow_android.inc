{
  Copyright 2004-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Manage OpenGL ES context on Android using EGL. }

{$I castlewindow_egl.inc}
{$I castlewindow_dialogs_by_messages.inc}

{$ifdef read_interface_uses}
{$endif}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_window_interface}
public
  NativeWindow: EGLNativeWindowType; //< must be set before Window.Open
  { AndroidXxx functions are called by library code. }
  procedure AndroidDraw;
  procedure AndroidResize(const AWidth, AHeight: Integer);
  procedure AndroidMouseDown(const X, Y: Integer; Btn: CastleKeysMouse.TMouseButton);
  procedure AndroidMouseUp(const X, Y: Integer; Btn: CastleKeysMouse.TMouseButton);
  procedure AndroidMouseMove(const X, Y: Integer);
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  FScreenWidth, FScreenHeight: Integer;
public
  procedure AndroidInit(const AWidth, AHeight: Integer);
{$endif read_application_interface}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_implementation}

{ TCastleWindowBase ------------------------------------------------------------------ }

procedure TCastleWindowBase.CreateBackend;
begin
end;

// TODO: update Android's app menu based on MainMenu contents?

procedure TCastleWindowBase.BackendMenuInitialize;
begin
end;

procedure TCastleWindowBase.BackendMenuFinalize;
begin
end;

procedure TCastleWindowBase.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  MenuFinalize;
  MenuInitialize;
end;

function TCastleWindowBase.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindowBase.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.OpenBackend;
begin
  ContextCreateEnd(NativeWindow);
  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindowBase.CloseBackend;
begin
  ContextDestroy;
end;

procedure TCastleWindowBase.PostRedisplay;
begin
  if not Closed then { TODO };
end;

procedure TCastleWindowBase.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then { TODO: use GetWholeCaption };
end;

procedure TCastleWindowBase.FlushRedisplay;
begin
  { TODO (redraw *now*, if PostRedisplay pending;
    it's valid (although non-optimal) to just always call DoDraw here) }
end;

procedure TCastleWindowBase.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      { TODO UpdateCursor };
  end;
end;

procedure TCastleWindowBase.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  { TODO }
  FCustomCursor := Value;
end;

function TCastleWindowBase.RedirectKeyDownToMenuClick: boolean;
begin
  Result := { TODO } true;
end;

procedure TCastleWindowBase.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  if not Closed then
    { TODO };
end;

procedure TCastleWindowBase.SetFullScreen(const Value: boolean);
begin
  FFullScreen := Value; //< does nothing on Android
end;

procedure TCastleWindowBase.AndroidDraw;
begin
  DoDraw;
end;

procedure TCastleWindowBase.AndroidResize(const AWidth, AHeight: Integer);
begin
  DoResize(AWidth, AHeight, false);
end;

procedure TCastleWindowBase.AndroidMouseDown(const X, Y: Integer; Btn: CastleKeysMouse.TMouseButton);
begin
  DoMouseDown(X, Y, Btn);
end;

procedure TCastleWindowBase.AndroidMouseUp(const X, Y: Integer; Btn: CastleKeysMouse.TMouseButton);
begin
  DoMouseUp(X, Y, Btn);
end;

procedure TCastleWindowBase.AndroidMouseMove(const X, Y: Integer);
begin
  DoMouseMove(X, Y);
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;
begin
end;

procedure TCastleApplication.DestroyBackend;
begin
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  { no message processing on Android, this makes no sense }
  Result := true;
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  { no message processing on Android, this makes no sense }
  Result := true;
end;

procedure TCastleApplication.Run;
begin
  { no message processing on Android, Run makes no sense }
end;

procedure TCastleApplication.QuitWhenNoOpenWindows;
begin
  { TODO }
end;

procedure TCastleApplication.AndroidInit(const AWidth, AHeight: Integer);
begin
  FScreenWidth := AWidth;
  FScreenHeight := AHeight;
end;

function TCastleApplication.ScreenWidth: integer;
begin
  Result := FScreenWidth;
end;

function TCastleApplication.ScreenHeight: integer;
begin
  Result := FScreenHeight;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'Android';
end;

{ TCastleClipboard ----------------------------------------------------------- }

function TCastleClipboard.GetAsText: string;
begin
  { TODO }
  Result := '';
end;

procedure TCastleClipboard.SetAsText(const Value: string);
begin
  { TODO }
end;

{$endif read_implementation}
