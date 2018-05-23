{
  Copyright 2004-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface_uses}
{$endif}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_window_interface}
{$endif read_window_interface}

{$ifdef read_application_interface}
{$endif read_application_interface}

{$ifdef read_implementation}

{ TCastleWindowCustom ------------------------------------------------------------------ }

procedure TCastleWindowCustom.CreateBackend;
begin
  { TODO (initialize backend-specific per-window stuff; may do nothing) }
end;

procedure TCastleWindowCustom.BackendMenuInitialize;
begin
  { TODO (may do nothing if you do not care about displaying menu) }
end;

procedure TCastleWindowCustom.BackendMenuFinalize;
begin
  { TODO (may do nothing if you do not care about displaying menu) }
end;

procedure TCastleWindowCustom.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

function TCastleWindowCustom.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindowCustom.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
  MenuFinalize;
  MenuInitialize;
end;

procedure TCastleWindowCustom.SwapBuffers;
begin
  { TODO }
end;

procedure TCastleWindowCustom.OpenBackend;
begin
  { TODO (create OpenGL context, show window with this context) }

  { Update position/size in case window is fullscreen.
    If the backend always creates a fullscreen window for some reason
    (e.g. typical on mobile), then you can remove the condition
    "if FullScreen then" and just do this always, regardless of FullScreen value. }
  if FullScreen then
  begin
    FLeft := 0;
    FTop := 0;
    DoResize(Application.ScreenWidth, Application.ScreenHeight, false);
  end;

  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindowCustom.CloseBackend;
begin
  { TODO (destroy OpenGL context, hide window; counterpart to OpenBackend) }
end;

procedure TCastleWindowCustom.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then { TODO: use GetWholeCaption };
end;

procedure TCastleWindowCustom.BackendMakeCurrent;
begin
  { TODO }
end;

procedure TCastleWindowCustom.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      { TODO UpdateCursor };
  end;
end;

procedure TCastleWindowCustom.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  { TODO }
  FCustomCursor := Value;
end;

function TCastleWindowCustom.RedirectKeyDownToMenuClick: boolean;
begin
  Result := { TODO } true;
end;

procedure TCastleWindowCustom.SetMousePosition(const Value: TVector2);
begin
  if not Closed then
    { TODO };
end;

procedure TCastleWindowCustom.UpdateFullScreenBackend;
begin
  { This is the simplest implementation of this method: }
  SimpleUpdateFullScreenBackend;
  { Some backends may provide a better way of doing this.
    Remove the above line then, and put better implementation here. }
end;

{ TCastleWindowCustom dialogs -------------------------------------------------- }

{ Methods below should make native-looking dialog boxes.
  Alternatively, you can include castlewindow_dialogs_by_messages.inc
  (outside of $ifdef read_implementation, usually at the beginning of
  castlewindow_xxx.inc file), to have simple dialog boxes drawn on
  OpenGL context using our CastleMessages unit. }

function TCastleWindowCustom.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  { TODO } Result := false;
end;

function TCastleWindowCustom.ColorDialog(var Color: TVector3): boolean;
begin
  { TODO } Result := false;
end;

procedure TCastleWindowCustom.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  { TODO }
end;

function TCastleWindowCustom.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  { TODO } Result := true;
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
  Result := not Terminated;

  { TODO:
    This should process *one* window system message, if it's available in the queue.
    Like a key press, mouse clicks etc.
    Call DoKeyDown, DoMouseDown and other DoXxx methods when these events occur.
    Be sure to recheck the "Terminated" state after calling any DoXxx method,
    doing something like "if Terminated then Exit(false);".

    UpdateAndRenderEverything should only be called if no message was in the queue.
  }

  if Result then
  begin
    UpdateAndRenderEverything;
    Result := not Terminated;
  end;
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  { This implementation is valid for start: }
  Result := ProcessMessage(false, false);

  { In general, ProcessAllMessages should make sure that all messages
    are handled, calling "ProcessMessage(false, false)" as long as some
    message exists in window system queue.
    Then it should call UpdateAndRenderEverything.
    See the GTK backend for example. }
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

function TCastleApplication.ScreenWidth: integer;
begin
  { TODO }
  Result := 200;
end;

function TCastleApplication.ScreenHeight: integer;
begin
  { TODO }
  Result := 200;
end;

function TCastleApplication.ScreenStatusBarScaledHeight: Cardinal;
begin
  Result := 0;
end;

function TCastleApplication.BackendName: string;
begin
  { TODO }
  Result := 'My CastleWindow Backend';
end;

{ TCastleWindow -------------------------------------------------------------- }

procedure TCastleWindow.NavigationInfoChanged;
begin
end;

{$endif read_implementation}
