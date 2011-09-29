{
  Copyright 2004-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo numerous TCastleWindowBase events.
  Displays many OnXxx events as they happen,
  also shows Pressed and Pressed.Characters.
  Also a demo of GLNotifications unit. }

program window_events;

{$apptype GUI}

uses SysUtils, CastleUtils, CastleGLUtils, GL, GLU, GLNotifications, CastleWindow,
  KeysMouse, CastleStringUtils,
  OpenGLBmpFonts, BFNT_BitstreamVeraSansMono_Bold_m15_Unit,
  Classes, CastleMessages;

var
  Window: TCastleWindowCustom;
  Notifications: TCastleNotifications;
  Font: TGLBitmapFont;

procedure Open(Window: TCastleWindowBase);
begin
  Notifications.Show('Open message');

  Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_Bold_m15);
end;

procedure Close(Window: TCastleWindowBase);
begin
  FreeAndNil(Font);
end;

procedure Resize(Window: TCastleWindowBase);
begin
  Resize2D(Window);
  Notifications.Show(Format('Resize message : new size %d %d (pos %d, %d)',
    [Window.Width, Window.Height, Window.Left, Window.Top]));
end;

procedure BeforeDraw(Window: TCastleWindowBase);
begin
  { Part of functionality of OnDraw moved to BeforeDraw.
    In this program, glWinEvents, there is no point in doing that.
    But I wanted just to show that BeforeDraw really works. }
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure Draw(Window: TCastleWindowBase);
var
  C: Char;
  Key: TKey;
  S: string;
const
  Margin = 20;
begin
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

procedure Idle(Window: TCastleWindowBase);
begin
  if Window.Pressed[K_F12] then MessageOk(Window, 'F12 key pressed. This is just a test that MessageOk works even from callbacks like OnIdle.', taLeft);
end;

procedure Timer(Window: TCastleWindowBase);
begin
  Notifications.Show(Format('Timer message. Time now %s', [FormatDateTime('tt', Time)]));
end;

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);
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

procedure KeyUp(Window: TCastleWindowBase; key: TKey; c: char);
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

procedure MouseDown(Window: TCastleWindowBase; btn: TMouseButton);
begin
  Notifications.Show(Format('Mouse Down message : %s (at %d,%d)',
    [MouseButtonToStr(btn), Window.MouseX, Window.MouseY]));
end;

procedure MouseUp(Window: TCastleWindowBase; btn: TMouseButton);
begin
  Notifications.Show(Format('Mouse Up message : %s (at %d,%d)',
    [MouseButtonToStr(btn), Window.MouseX, Window.MouseY]));
end;

procedure MouseMove(Window: TCastleWindowBase; newX, newY: integer);
begin
  Notifications.Show(Format('Mouse Move : old pos %d %d, new pos %d %d',
    [Window.MouseX, Window.MouseY, newX, newY]));
end;

procedure MouseWheel(Window: TCastleWindowBase; const Scroll: Single; const Vertical: boolean);
begin
  Notifications.Show(Format('Mouse Wheel: %f, vertical: %s', [Scroll, BoolToStr[Vertical]]));
end;

begin
  Window := TCastleWindowCustom.Create(Application);

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
  Window.SetDemoOptions(K_F11, CharEscape, true);

  Window.OnIdle := @Idle;
  Application.TimerMilisec := 5000;
  Window.OnTimer := @Timer;

  Notifications := TCastleNotifications.Create(Window);
  Notifications.VerticalPosition := vpUp;
  Notifications.MaxMessages := 15;
  Notifications.Timeout := 20000;
  Window.Controls.Add(Notifications);

  Window.OpenAndRun;
end.
