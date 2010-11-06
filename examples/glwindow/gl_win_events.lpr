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

{ Example program using GLWindow.
  Displays many OnXxx events as they happen, shows Pressed and Pressed.Characters.
  So it allows you to "see" how TGLWindow events happen.

  Also a demo of Notifications unit. }

program gl_win_events;

{$apptype GUI}

uses SysUtils, KambiUtils, KambiGLUtils, GL, GLU, GLNotifications, GLWindow,
  KeysMouse, KambiStringUtils,
  OpenGLBmpFonts, BFNT_BitstreamVeraSansMono_Bold_m15_Unit,
  Classes, GLWinMessages;

var
  Glw: TGLWindowDemo;
  Notifications: TGLNotifications;
  Font: TGLBitmapFont;

procedure Init(glwin: TGLWindow);
begin
  Notifications := TGLNotifications.Create(glw, hpMiddle, vpUp, 0);
  Notifications.MaxMessages := 15;
  Notifications.MessageTimeout := 20000;
  Notifications.Show('Init message');

  Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_Bold_m15);
end;

procedure Close(glwin: TGLWindow);
begin
  FreeAndNil(Notifications);
  FreeAndNil(Font);
end;

procedure Resize(glwin: TGLWindow);
begin
  Resize2D(glwin);
  Notifications.Show(Format('Resize message : new size %d %d (pos %d, %d)',
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
var
  C: Char;
  Key: TKey;
  S: string;
const
  Margin = 20;
begin
  Notifications.Draw2D(glwin.Width, glwin.Height, glwin.Width, glwin.Height);

  glColor3f(0.5, 0.5, 0.5);

  S := '';
  for C := Low(C) to High(C) do
    if Glwin.Pressed.Characters[C] then
    begin
      if S <> '' then S += ', ';
      S += CharToNiceStr(C);
    end;
  S := 'Characters pressed: [' + S + ']';
  Font.PrintBrokenString(S, Glwin.Width - Margin * 2, Margin, 100, false, 0);

  S := '';
  for Key := Low(Key) to High(Key) do
    if Glwin.Pressed[Key] then
    begin
      if S <> '' then S += ', ';
      S += KeyToStr(Key);
    end;
  S := 'Keys pressed: [' + S + ']';
  Font.PrintBrokenString(S, Glwin.Width - Margin * 2, Margin, 200, false, 0);
end;

procedure Idle(glwin: TGLWindow);
begin
  Notifications.Idle;
  if Glwin.Pressed[K_F12] then MessageOk(Glwin, 'F12 key pressed. This is just a test that MessageOk works even from callbacks like OnIdle.', taLeft);
end;

procedure Timer(glwin: TGLWindow);
begin
  Notifications.Show(Format('Timer message. Time now %s', [FormatDateTime('tt', Time)]));
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  { Tes
  case C of
    'n': Glwin.Cursor := mcNone;
    'd': Glwin.Cursor := mcDefault;
    'w': Glwin.Cursor := mcWait;
    '1': Glwin.SetMousePosition(0          , 0);
    '2': Glwin.SetMousePosition(Glwin.Width, 0);
    '3': Glwin.SetMousePosition(Glwin.Width, Glwin.Height);
    '4': Glwin.SetMousePosition(0          , Glwin.Height);
    '5': Glwin.SetMousePosition(Glwin.Width div 2, Glwin.Height div 2);
  end; }

  Notifications.Show(Format('KeyDown message : key %s, char %s (ord %d)',
    [KeyToStr(key), CharToNiceStr(c), Ord(c)]));
end;

procedure KeyUp(glwin: TGLWindow; key: TKey; c: char);
begin
  Notifications.Show(Format('KeyUp message : key %s, char %s (ord %d)',
    [KeyToStr(key), CharToNiceStr(c), Ord(c)]));
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
  Notifications.Show(Format('Mouse Down message : %s (at %d,%d)',
    [MouseButtonToStr(btn), glwin.MouseX, glwin.MouseY]));
end;

procedure MouseUp(glwin: TGLWindow; btn: TMouseButton);
begin
  Notifications.Show(Format('Mouse Up message : %s (at %d,%d)',
    [MouseButtonToStr(btn), glwin.MouseX, glwin.MouseY]));
end;

procedure MouseMove(glwin: TGLWindow; newX, newY: integer);
begin
  Notifications.Show(Format('Mouse Move : old pos %d %d, new pos %d %d',
    [glwin.MouseX, glwin.MouseY, newX, newY]));
end;

procedure MouseWheel(glwin: TGLWindow; const Scroll: Single; const Vertical: boolean);
begin
  Notifications.Show(Format('Mouse Wheel: %f, vertical: %s', [Scroll, BoolToStr[Vertical]]));
end;

var
  M: TMenu;
begin
  Glw := TGLWindowDemo.Create(Application);

  Glw.ParseParameters;

  Glw.OnInit := @Init;
  Glw.OnClose := @Close;
  Glw.OnResize := @Resize;
  Glw.OnBeforeDraw := @BeforeDraw;
  Glw.OnDraw := @Draw;

  Glw.OnKeyDown := @KeyDown;
  Glw.OnKeyUp := @KeyUp;

  Glw.OnMouseDown := @MouseDown;
  Glw.OnMouseMove := @MouseMove;
  Glw.OnMouseUp := @MouseUp;
  Glw.OnMouseWheel := @MouseWheel;

  Glw.OnIdle := @Idle;
  Application.TimerMilisec := 5000;
  Glw.OnTimer := @Timer;

  {testing:
  Writeln('I''m trying to get window size ', Glw.Width, ',', Glw.Height, '...');
  }
  Glw.MainMenu := TMenu.Create('Test menu');
  M := TMenu.Create('Menu 1');
    M.Append(TMenuItem.Create('Menu Item 1', 0));
    M.Append(TMenuItem.Create('Menu Item 2', 1));
    Glw.MainMenu.Append(M);
  M := TMenu.Create('Menu 2');
    M.Append(TMenuItem.Create('Menu Item 3', 3));
    M.Append(TMenuItem.Create('Menu Item 4', 4));
    Glw.MainMenu.Append(M);
  Glw.MainMenu.Append(TMenuItem.Create('Menu Item 5', 5));

  //Glw.ResizeAllowed := raNotAllowed; // just for testing

  Glw.InitAndRun;
end.
