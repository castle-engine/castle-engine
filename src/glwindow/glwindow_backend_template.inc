{
  Copyright 2004-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_tglwindow_private}

{$endif read_tglwindow_private}

{$ifdef read_tglwindowmanager_private}

{$endif read_tglwindowmanager_private}

{$ifdef read_implementation}

{ TGLWindow ------------------------------------------------------------------ }

procedure TGLWindow.CreateBackend;
begin
  { TODO (initialize backend-specific per-window stuff; may do nothing) }
end;

procedure TGLWindow.BackendMenuInitialize;
begin
  { TODO (may do nothing if you do not care about displaying menu) }
end;

procedure TGLWindow.BackendMenuFinalize;
begin
  { TODO (may do nothing if you do not care about displaying menu) }
end;

procedure TGLWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
end;

procedure TGLWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
end;

procedure TGLWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  { TODO (may do nothing if you do not care about displaying menu;
    may do MenuFinalize + MenuInitialize if you display menu,
    but don't care about updating it fast) }
end;

function TGLWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TGLWindow.SwapBuffers;
begin
  { TODO }
end;

procedure TGLWindow.OpenBackend;
begin
  { TODO (create OpenGL context, show window with this context) }
end;

procedure TGLWindow.CloseBackend;
begin
  { TODO (destroy OpenGL context, hide window; counterpart to OpenBackend) }
end;

procedure TGLWindow.PostRedisplay;
begin
  if not Closed then { TODO };
end;

procedure TGLWindow.SetCaption(const Value: string);
begin
  FCaption := Value;
  if not Closed then { TODO };
end;

procedure TGLWindow.BackendMakeCurrent;
begin
  { TODO }
end;

procedure TGLWindow.FlushRedisplay;
begin
  { TODO (redraw *now*, if PostRedisplay pending;
    it's valid (although non-optimal) to just always call DoDraw here) }
end;

procedure TGLWindow.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      { TODO UpdateCursor };
  end;
end;

procedure TGLWindow.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  { TODO }
  FCustomCursor := Value;
end;

function TGLWindow.RedirectKeyDownToMenuCommand: boolean;
begin
  Result := { TODO } true;
end;

procedure TGLWindow.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  if not Closed then
    { TODO };
end;

{ TGLWindow dialogs ---------------------------------------------------------- }

{ Methods below should make native-looking dialog boxes.
  Alternatively, you can include glwindow_dialogs_by_glwinmessages.inc,
  to have simple dialog boxes drawn on OpenGL context using our GLWinMessages
  unit. }
{ $I glwindow_dialogs_by_glwinmessages.inc}

function TGLWindow.FileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  { TODO }
end;

function TGLWindow.ColorDialog(var Color: TVector3Single): boolean;
begin
  { TODO }
end;

procedure TGLWindow.MessageOK(const S: string; const MessageType: TGLWindowMessageType);
begin
  { TODO }
end;

function TGLWindow.MessageYesNo(const S: string;
  const MessageType: TGLWindowMessageType): boolean;
begin
  { TODO }
end;

{ TGLApplication ---------------------------------------------------------- }

procedure TGLApplication.CreateBackend;
begin
  { TODO (initialize backend-specific stuff for the whole application;
    may do nothing if you don't need anything) }
end;

procedure TGLApplication.DestroyBackend;
begin
  { TODO }
end;

function TGLApplication.ProcessMessage(AllowSuspend: boolean): boolean;
begin
  { TODO }
end;

function TGLApplication.ProcessAllMessages: boolean;
begin
  { TODO }
end;

procedure TGLApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;

  { Implementing Run by calling ProcessMessage in a loop (like below)
    is a valid and good implementation. Make sure your ProcessMessage
    honours AllowSuspend = true, to avoid wasting CPU on "busy waiting". }
  while ProcessMessage(true) do ;
end;

procedure TGLApplication.QuitWhenNoOpenWindows;
begin
  { TODO }
end;

function TGLApplication.ScreenWidth: integer;
begin
  { TODO }
end;

function TGLApplication.ScreenHeight: integer;
begin
  { TODO }
end;

function TGLApplication.BackendName: string;
begin
  { TODO }
  Result := 'My GLWindow Backend';
end;

{$endif read_implementation}
