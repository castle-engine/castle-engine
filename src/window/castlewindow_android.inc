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

{$I castlewindow_dialogs_by_messages.inc}

{$ifdef read_interface_uses}
{$endif}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_window_interface}
public
  { AnroidXxx functions are called by Android activity from Java. }
  procedure AndroidDraw;
  procedure AndroidResize(const AWidth, AHeight: Integer);
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
  { TODO (initialize backend-specific per-window stuff; may do nothing) }
end;

procedure TCastleWindowBase.BackendMenuInitialize;
begin
  { TODO (may do nothing if you do not care about displaying menu) }
end;

procedure TCastleWindowBase.BackendMenuFinalize;
begin
  { TODO (may do nothing if you do not care about displaying menu) }
end;

procedure TCastleWindowBase.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
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
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowBase.SwapBuffers;
begin
  { TODO }
end;

procedure TCastleWindowBase.OpenBackend;
begin
  { TODO (create OpenGL context, show window with this context) }
  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindowBase.CloseBackend;
begin
  { TODO (destroy OpenGL context, hide window; counterpart to OpenBackend) }
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

procedure TCastleWindowBase.BackendMakeCurrent;
begin
  { TODO }
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
  { Some backends may provide a better way of doing this. }
  SimpleSetFullScreen(Value);
end;

procedure TCastleWindowBase.AndroidDraw;
begin
  DoDraw;
end;

procedure TCastleWindowBase.AndroidResize(const AWidth, AHeight: Integer);
begin
  DoResize(AWidth, AHeight, false);
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;
begin
  { TODO (initialize backend-specific stuff for the whole application;
    may do nothing if you don't need anything) }
end;

procedure TCastleApplication.DestroyBackend;
begin
  { TODO }
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  { TODO }
  Result := true;
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  { TODO }
  Result := true;
end;

procedure TCastleApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;

  { Implementing Run by calling ProcessMessage in a loop (like below)
    is a valid and good implementation. Make sure your ProcessMessage
    honours WaitForMessage and WaitToLimitFPS = true,
    to avoid wasting CPU on "busy waiting". }
  while ProcessMessage(true, true) do ;
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
