{
  Copyright 2004-2010 Michalis Kamburelis.

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

procedure TGLWindow.SwapBuffers;
begin
  { TODO }
end;

procedure TGLWindow.OpenBackend;
begin
  { TODO }
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

procedure TGLWindow.CloseBackend;
begin
  { TODO }
end;

procedure TGLWindow.MakeCurrent;
begin
  { TODO }
end;

procedure TGLWindow.CreateBackend;
begin
  { TODO }
end;

procedure TGLWindow.FlushRedisplay;
begin
  { TODO }
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

{ TGLWindow.*Dialog ---------------------------------------------------------- }

function TGLWindow.FileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFiltersList): boolean;
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

  { TODO }
end;

procedure TGLApplication.QuitWhenNoOpenWindows;
begin
  { TODO }
end;

procedure TGLApplication.CreateBackend;
begin
  { TODO }
end;

procedure TGLApplication.DestroyBackend;
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
