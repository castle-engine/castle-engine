{
  Copyright 2004 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_tglwindow_private}

{$endif read_tglwindow_private}

{$ifdef read_tglwindowmanager_private}

{$endif read_tglwindowmanager_private}

{$ifdef read_implementation}

{ TGLWindow ---------------------------------------- }

procedure TGLWindow.MainMenuChangedImplDepend;
begin
 TODO
end;

procedure TGLWindow.SwapBuffers;
begin
 TODO
end;

procedure TGLWindow.InitImplDepend;
begin
 TODO
end;

procedure TGLWindow.PostRedisplay;
begin
 if not Closed then TODO
end;

procedure TGLWindow.SetCaption(const Value:string);
begin
 FCaption:=Value;
 if not Closed then TODO
end;

procedure TGLWindow.CloseImplDepend;
begin
 TODO
end;

procedure TGLWindow.MakeCurrent;
begin
 TODO
end;

procedure TGLWindow.CreateImplDepend;
begin
 TODO
end;

function TGLWindow.FlushRedisplay:boolean;
begin
 TODO
end;

{ TGLWindow.*Dialog --------------------------------------------------- }

function TGLWindow.FileDialog(const Title:string; var FileName:string;
  OpenDialog:boolean):boolean;
begin
 TODO
end;

function TGLWindow.ColorDialog(var Color:TVector3Single):boolean;
begin
 TODO
end;

{ TGLWindowsManager ------------------------------------------- }

procedure TGLWindowsManager.Loop;
begin 
 TODO
end;

procedure TGLWindowsManager.QuitWhenNoWindowsActive;
begin
 TODO
end;

procedure TGLWindowsManager.CreateImplDependent;
begin
end;

procedure TGLWindowsManager.DestroyImplDependent;
begin
end;

function TGLWindowsManager.ScreenWidth:integer;
begin 
 TODO
end;

function TGLWindowsManager.ScreenHeight:integer;
begin 
 TODO
end;

function TGLWindowsManager.ImplementationName:string;
begin
 Result:=TODO;
end;

{$endif read_implementation}