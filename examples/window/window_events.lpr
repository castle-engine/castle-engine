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

{ Demo numerous TCastleWindowCustom events.
  Displays many OnXxx events as they happen,
  also shows Pressed and Pressed.Characters.
  Also a demo of CastleNotifications unit. }

program window_events;

{$apptype GUI}

uses SysUtils, CastleUtils, CastleGLUtils, CastleNotifications, CastleWindow,
  CastleKeysMouse, CastleStringUtils, CastleColors, Classes, CastleMessages,
  CastleControls;

var
  Window: TCastleWindowCustom;
  Notifications: TCastleNotifications;

procedure Open(Container: TUIContainer);
begin
  Notifications.Show('Open message');
end;

procedure Close(Container: TUIContainer);
begin
end;

procedure Resize(Container: TUIContainer);
begin
  Notifications.Show(Format('Resize message : new size %d %d (pos %d, %d)',
    [Window.Width, Window.Height, Window.Left, Window.Top]));
end;

procedure BeforeRender(Container: TUIContainer);
begin
  { Part of functionality of OnRender moved to BeforeRender.
    In this program there is no point in doing that.
    But I wanted just to show that BeforeRender really works. }
  GLClear([cbColor], Black);
end;

procedure Render(Container: TUIContainer);
var
  C: Char;
  Key: TKey;
  S: string;
const
  Margin = 20;
begin
  S := '';
  for C := Low(C) to High(C) do
    if Window.Pressed.Characters[C] then
    begin
      if S <> '' then S += ', ';
      S += CharToNiceStr(C);
    end;
  S := 'Characters pressed: [' + S + ']';
  UIFont.PrintBrokenString(Margin, 100, Gray, S, Window.Width - Margin * 2, false, 0);

  S := '';
  for Key := Low(Key) to High(Key) do
    if Window.Pressed[Key] then
    begin
      if S <> '' then S += ', ';
      S += KeyToStr(Key);
    end;
  S := 'Keys pressed: [' + S + ']';
  UIFont.PrintBrokenString(Margin, 200, Gray, S, Window.Width - Margin * 2, false, 0);
end;

procedure Update(Container: TUIContainer);
begin
  if Window.Pressed[K_F12] then
    // MessageOk(Window, 'F12 key pressed. This is just a test that MessageOk works even from callbacks like OnUpdate.');
    Window.MessageOk('F12 key pressed. This is just a test that MessageOk works even from callbacks like OnUpdate.', mtInfo);
end;

procedure Timer(Container: TUIContainer);
begin
  Notifications.Show(Format('Timer message. Time now %s', [FormatDateTime('tt', Time)]));
end;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
begin
  Notifications.Show('Press message : ' + Event.Description);

  { Cursor tests: }
  case Event.KeyCharacter of
    'n': Window.Cursor := mcNone;
    'd': Window.Cursor := mcDefault;
    'w': Window.Cursor := mcWait;
    '1': Window.SetMousePosition(0           , 0);
    '2': Window.SetMousePosition(Window.Width, 0);
    '3': Window.SetMousePosition(Window.Width, Window.Height);
    '4': Window.SetMousePosition(0           , Window.Height);
    '5': Window.SetMousePosition(Window.Width div 2, Window.Height div 2);
  end;

  { Test what messages happen when switching FullScreen }
  if Event.IsKey(K_F11) {$ifdef DARWIN} and Window.Pressed[K_Ctrl] {$endif} then
    Window.FullScreen := not Window.FullScreen;
end;

procedure Release(Container: TUIContainer; const Event: TInputPressRelease);
begin
  Notifications.Show('Release message : ' + Event.Description);
end;

procedure MouseMove(Container: TUIContainer; newX, newY: integer);
begin
  Notifications.Show(Format('Mouse Move : old pos %d %d, new pos %d %d',
    [Window.MouseX, Window.MouseY, newX, newY]));
end;

begin
  Window := TCastleWindowCustom.Create(Application);

  Window.ParseParameters;

  Window.OnOpen := @Open;
  Window.OnClose := @Close;
  Window.OnResize := @Resize;
  Window.OnBeforeRender := @BeforeRender;
  Window.OnRender := @Render;
  Window.OnPress := @Press;
  Window.OnRelease := @Release;
  Window.OnMouseMove := @MouseMove;
  Window.SetDemoOptions(K_F11, CharEscape, true);

  Window.OnUpdate := @Update;
  Application.TimerMilisec := 5000;
  Window.OnTimer := @Timer;

  Notifications := TCastleNotifications.Create(Window);
  Notifications.VerticalPosition := vpUp;
  Notifications.MaxMessages := 15;
  Notifications.Timeout := 20000;
  Window.Controls.Add(Notifications);

  Window.OpenAndRun;
end.
