{
  Copyright 2004-2006 Michalis Kamburelis.

  This file is part of "Kambi's OpenGL Pascal units".

  "Kambi's OpenGL Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's OpenGL Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's OpenGL Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Example program using GLWindow.
  Displays many OnXxx events as they happen.
  Also a demo of TimeMessages unit. }

program glWinEvents;

{$apptype GUI}

uses SysUtils, KambiUtils, KambiGLUtils, OpenGLh, TimeMessages, GLWindow,
  GLW_demo, Keys, KambiStringUtils;

var TimeMsg: TTimeMessagesManager;

procedure Init(glwin: TGLWindow);
begin
 TimeMsg := TTimeMessagesManager.Create(glw, hpMiddle, vpUp, 0);
 TimeMsg.MaxMessagesCount := 15;
 TimeMsg.MessageDuration := 20000;
 TimeMsg.Show('Init message');
end;

procedure Close(glwin: TGLWindow);
begin
 FreeAndNil(TimeMsg);
end;

procedure Resize(glwin: TGLWindow);
begin
 Resize2D(glwin);
 TimeMsg.Show(Format('Resize message : new size %d %d (pos %d, %d)',
   [glwin.Width, glwin.Height, Glwin.Left, Glwin.Top]));
end;

procedure BeforeDraw(glwin: TGLWindow);
begin
 { Part of functionality of OnDraw moved to BeforeDraw.
   In this program, glWinEvents, there is no point in doing that.
   But I wanted just to show that BeforeDraw really works. }
 glClear(GL_COLOR_BUFFER_BIT);
end;

procedure Draw(glwin: TGLWindow);
begin
 TimeMsg.Draw2d(glwin.Width, glwin.Height, glwin.Width, glwin.Height);
end;

procedure Idle(glwin: TGLWindow);
begin
 TimeMsg.Idle;
end;

procedure Timer(glwin: TGLWindow);
begin
 TimeMsg.Show(Format('Timer message. Time now %s', [FormatDateTime('tt', Time)]));
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  { Tests: }
  case C of
    'n': Glwin.Cursor := gcNone;
    'd': Glwin.Cursor := gcDefault;
    'w': Glwin.Cursor := gcWait;
    '1': Glwin.SetMousePosition(0          , 0);
    '2': Glwin.SetMousePosition(Glwin.Width, 0);
    '3': Glwin.SetMousePosition(Glwin.Width, Glwin.Height);
    '4': Glwin.SetMousePosition(0          , Glwin.Height);
    '5': Glwin.SetMousePosition(Glwin.Width div 2, Glwin.Height div 2);
  end; {}

  TimeMsg.Show(Format('KeyDown message : key %s, char %s (ord %d)',
    [KeyToStr(key), DescribeKey(c), Ord(c)]));
end;

procedure KeyUp(glwin: TGLWindow; key: TKey);
begin
 TimeMsg.Show(Format('KeyUp message : key %s', [KeyToStr(key)]));
end;

function MouseButtonToStr(btn: TMouseButton): string;
begin
 case btn of
  mbLeft: result := 'left';
  mbMiddle: result := 'middle';
  mbRight: result := 'right';
 end;
end;

procedure MouseDown(glwin: TGLWindow; btn: TMouseButton);
begin
 TimeMsg.Show(Format('Mouse Down message : %s (at %d,%d)',
   [MouseButtonToStr(btn), glwin.MouseX, glwin.MouseY]));
end;

procedure MouseUp(glwin: TGLWindow; btn: TMouseButton);
begin
 TimeMsg.Show(Format('Mouse Up message : %s (at %d,%d)',
   [MouseButtonToStr(btn), glwin.MouseX, glwin.MouseY]));
end;

procedure MouseMove(glwin: TGLWindow; newX, newY: integer);
begin
 TimeMsg.Show(Format('Mouse Move : old pos %d %d, new pos %d %d',
   [glwin.MouseX, glwin.MouseY, newX, newY]));
end;

var
  M: TMenu;
begin
 glw.ParseParameters;

 glw.OnInit := @Init;
 glw.OnClose := @Close;
 glw.OnResize := @Resize;
 glw.OnBeforeDraw := @BeforeDraw;
 glw.OnDraw := @Draw;

 glw.OnKeyDown := @KeyDown;
 glw.OnKeyUp := @KeyUp;

 glw.OnMouseDown := @MouseDown;
 glw.OnMouseMove := @MouseMove;
 glw.OnMouseUp := @MouseUp;

 glw.OnIdle := @Idle;
 glwm.TimerMilisec := 5000;
 glw.OnTimer := @Timer;

 {testing:
 Writeln('I''m trying to get window size ', glw.Width, ',', glw.Height, '...');
 }
 glw.MainMenu := TMenu.Create('Test menu');
 M := TMenu.Create('Menu 1');
   M.Append(TMenuItem.Create('Menu Item 1', 0));
   M.Append(TMenuItem.Create('Menu Item 2', 1));
   glw.MainMenu.Append(M);
 M := TMenu.Create('Menu 2');
   M.Append(TMenuItem.Create('Menu Item 3', 3));
   M.Append(TMenuItem.Create('Menu Item 4', 4));
   glw.MainMenu.Append(M);
 glw.MainMenu.Append(TMenuItem.Create('Menu Item 5', 5));

 //glw.ResizeAllowed := raNotAllowed; // just for testing

 glw.InitLoop;
end.
