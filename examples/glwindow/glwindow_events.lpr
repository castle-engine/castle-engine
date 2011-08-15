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

{ Demo numerous TGLWindow events.
  Displays many OnXxx events as they happen,
  also shows Pressed and Pressed.Characters.
  Also a demo of GLNotifications unit. }

program glwindow_events;

{$apptype GUI}

uses SysUtils, KambiUtils, KambiGLUtils, GL, GLU, GLNotifications, GLWindow,
  KeysMouse, KambiStringUtils,
  OpenGLBmpFonts, BFNT_BitstreamVeraSansMono_Bold_m15_Unit,
  Classes, GLWinMessages;

var
  Window: TGLWindowDemo;
  Notifications: TGLNotifications;
  Font: TGLBitmapFont;

procedure Open(Window: TGLWindow);
begin
  Notifications := TGLNotifications.Create(Window, hpMiddle, vpUp, 0);
  Notifications.MaxMessages := 15;
  Notifications.MessageTimeout := 20000;
  Notifications.Show('Init message');

  Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_Bold_m15);
end;

procedure Close(Window: TGLWindow);
begin
  FreeAndNil(Notifications);
  FreeAndNil(Font);
end;

procedure Resize(Window: TGLWindow);
begin
  Resize2D(Window);
  Notifications.Show(Format('Resize message : new size %d %d (pos %d, %d)',
    [Window.Width, Window.Height, Window.Left, Window.Top]));
end;

procedure BeforeDraw(Window: TGLWindow);
begin
  { Part of functionality of OnDraw moved to BeforeDraw.
    In this program, glWinEvents, there is no point in doing that.
    But I wanted just to show that BeforeDraw really works. }
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure Draw(Window: TGLWindow);
var
  C: Char;
  Key: TKey;
  S: string;
const
  Margin = 20;
begin
  Notifications.Draw2D(Window.Width, Window.Height, Window.Width, Window.Height);

  glColor3f(0.5, 0.5, 0.5);

  S := '';
  for C := Low(C) to High(C) do
    if Window.Pressed.Characters[C] then
    begin
      if S <> '' then S += ', ';
      S += CharToNiceStr(C);
    end;
  S := 'Characters pressed: [' + S + ']';
  Font.PrintBrokenString(S, Window.Width - Margin * 2, Margin, 100, false, 0);

  S := '';
  for Key := Low(Key) to High(Key) do
    if Window.Pressed[Key] then
    begin
      if S <> '' then S += ', ';
      S += KeyToStr(Key);
    end;
  S := 'Keys pressed: [' + S + ']';
  Font.PrintBrokenString(S, Window.Width - Margin * 2, Margin, 200, false, 0);
end;

procedure Idle(Window: TGLWindow);
begin
  Notifications.Idle;
  if Window.Pressed[K_F12] then MessageOk(Window, 'F12 key pressed. This is just a test that MessageOk works even from callbacks like OnIdle.', taLeft);
end;

procedure Timer(Window: TGLWindow);
begin
  Notifications.Show(Format('Timer message. Time now %s', [FormatDateTime('tt', Time)]));
end;

procedure KeyDown(Window: TGLWindow; key: TKey; c: char);
begin
  { Tes
  case C of
    'n': Window.Cursor := mcNone;
    'd': Window.Cursor := mcDefault;
    'w': Window.Cursor := mcWait;
    '1': Window.SetMousePosition(0          , 0);
    '2': Window.SetMousePosition(Window.Width, 0);
    '3': Window.SetMousePosition(Window.Width, Window.Height);
    '4': Window.SetMousePosition(0          , Window.Height);
    '5': Window.SetMousePosition(Window.Width div 2, Window.Height div 2);
  end; }

  Notifications.Show(Format('KeyDown message : key %s, char %s (ord %d)',
    [KeyToStr(key), CharToNiceStr(c), Ord(c)]));
end;

procedure KeyUp(Window: TGLWindow; key: TKey; c: char);
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

procedure MouseDown(Window: TGLWindow; btn: TMouseButton);
begin
  Notifications.Show(Format('Mouse Down message : %s (at %d,%d)',
    [MouseButtonToStr(btn), Window.MouseX, Window.MouseY]));
end;

procedure MouseUp(Window: TGLWindow; btn: TMouseButton);
begin
  Notifications.Show(Format('Mouse Up message : %s (at %d,%d)',
    [MouseButtonToStr(btn), Window.MouseX, Window.MouseY]));
end;

procedure MouseMove(Window: TGLWindow; newX, newY: integer);
begin
  Notifications.Show(Format('Mouse Move : old pos %d %d, new pos %d %d',
    [Window.MouseX, Window.MouseY, newX, newY]));
end;

procedure MouseWheel(Window: TGLWindow; const Scroll: Single; const Vertical: boolean);
begin
  Notifications.Show(Format('Mouse Wheel: %f, vertical: %s', [Scroll, BoolToStr[Vertical]]));
end;

var
  M: TMenu;
begin
  Window := TGLWindowDemo.Create(Application);

  Window.ParseParameters;

  Window.OnOpen := @Open;
  Window.OnClose := @Close;
  Window.OnResize := @Resize;
  Window.OnBeforeDraw := @BeforeDraw;
  Window.OnDraw := @Draw;

  Window.OnKeyDown := @KeyDown;
  Window.OnKeyUp := @KeyUp;

  Window.OnMouseDown := @MouseDown;
  Window.OnMouseMove := @MouseMove;
  Window.OnMouseUp := @MouseUp;
  Window.OnMouseWheel := @MouseWheel;

  Window.OnIdle := @Idle;
  Application.TimerMilisec := 5000;
  Window.OnTimer := @Timer;

  {testing:
  Writeln('I''m trying to get window size ', Window.Width, ',', Window.Height, '...');
  }
  Window.MainMenu := TMenu.Create('Test menu');
  M := TMenu.Create('Menu 1');
    M.Append(TMenuItem.Create('Menu Item 1', 0));
    M.Append(TMenuItem.Create('Menu Item 2', 1));
    Window.MainMenu.Append(M);
  M := TMenu.Create('Menu 2');
    M.Append(TMenuItem.Create('Menu Item 3', 3));
    M.Append(TMenuItem.Create('Menu Item 4', 4));
    Window.MainMenu.Append(M);
  Window.MainMenu.Append(TMenuItem.Create('Menu Item 5', 5));

  //Window.ResizeAllowed := raNotAllowed; // just for testing

  Window.OpenAndRun;
end.
