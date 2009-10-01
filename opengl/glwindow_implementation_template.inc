{
  Copyright 2004,2008 Michalis Kamburelis.

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

procedure TGLWindow.MainMenuChangedImplDepend;
begin
  { TODO }
end;

procedure TGLWindow.SwapBuffers;
begin
  { TODO }
end;

procedure TGLWindow.InitImplDepend;
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

procedure TGLWindow.CloseImplDepend;
begin
  { TODO }
end;

procedure TGLWindow.MakeCurrent;
begin
  { TODO }
end;

procedure TGLWindow.CreateImplDepend;
begin
  { TODO }
end;

procedure TGLWindow.FlushRedisplay;
begin
  { TODO }
end;

procedure TGLWindow.SetCursor(const Value: TGLWindowCursor);
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
  { TODO }
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

{ TGLWindowsManager ---------------------------------------------------------- }

{ Note: you can avoid implementing ProcessMessage and ProcessAllMessages
  if you don't define GLWINDOW_HAS_PROCESS_MESSAGE for your backend.
  But it's a good idea to try to implement them, as they allow some
  other features, like GLWinModes and GLWinMessages. }

function TGLWindowsManager.ProcessMessage(AllowSuspend: boolean): boolean;
begin
  { TODO }
end;

function TGLWindowsManager.ProcessAllMessages: boolean;
begin
  { TODO }
end;

procedure TGLWindowsManager.Loop;
begin
  if ActiveCount = 0 then Exit;

  { TODO }
end;

procedure TGLWindowsManager.QuitWhenNoWindowsActive;
begin
  { TODO }
end;

procedure TGLWindowsManager.CreateImplDependent;
begin
  { TODO }
end;

procedure TGLWindowsManager.DestroyImplDependent;
begin
  { TODO }
end;

function TGLWindowsManager.ScreenWidth: integer;
begin
  { TODO }
end;

function TGLWindowsManager.ScreenHeight: integer;
begin
  { TODO }
end;

function TGLWindowsManager.ImplementationName: string;
begin
  { TODO }
  Result := 'My Implementation';
end;

{$endif read_implementation}
