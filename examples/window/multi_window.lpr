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

{ Demo of
  - using multiple windows with CastleWindow unit
  - instead of registering OnXxx callbacks overriding EventXxx methods
    (more OOP approach)
  - press c to change cursor in the window that has focus, this is to demo
    that TCastleWindowBase.Cursor indeed works as appropriate, i.e. changes the cursor
    only for the given window.
}

program multi_window;

{$apptype GUI}

uses CastleWindow, SysUtils, CastleUtils, CastleGLUtils, CastleKeysMouse, CastleVectors,
  CastleStringUtils, CastleColors, CastleControls, CastleUIControls;

type
  TMyWindow = class(TCastleWindow)
  public
    Text: string;
    LightColor, DarkColor: TCastleColor;
    procedure EventRender; override;
    procedure EventResize; override;
    procedure EventPress(const Event: TInputPressRelease); override;
  end;

procedure TMyWindow.EventRender;
begin
  inherited;
  GLClear([cbColor, cbDepth], DarkColor);
  UIFont.Print(10, 10, LightColor, Text);
end;

procedure TMyWindow.EventResize;
begin
  inherited;
  glViewport(Rect);
  PerspectiveProjection(45.0, Width/Height, 0.1, 100.0);
end;

procedure TMyWindow.EventPress(const Event: TInputPressRelease);
var
  URL: string;
begin
  inherited;
  case Event.KeyCharacter of
    'c': if Cursor = High(Cursor) then Cursor := Low(Cursor) else Cursor := Succ(Cursor);
    'o': begin
           URL := '';
           { when file dialog is open, note that the other windows
             are still active as they should. }
           FileDialog('Test open file dialog', URL, true);
         end;
  end;
end;

var
  I: Integer;
  Windows: array [0..4] of TMyWindow;
begin
  for i := 0 to High(Windows) do
  begin
    Windows[I] := TMyWindow.Create(Application);

    Windows[I].Text := 'Window ' + IntToStr(I);
    Windows[I].LightColor := Vector4Single(Random*1.5, Random*1.5, Random*1.5, 1);
    Windows[I].DarkColor  := Vector4Single(Random*0.7, Random*0.7, Random*0.7, 1);
    Windows[I].RenderStyle := rs2D;

    Windows[I].Caption := 'Window ' + IntToStr(I);
    Windows[I].Width := 200;
    Windows[I].Height := 200;
    Windows[I].Left := 30 + 250 * (I mod 3);
    Windows[I].Top := 30 + 250 * (I div 3);

    Windows[I].Open;
    Windows[I].SetDemoOptions(K_F11, CharEscape, true);
  end;

  Application.Run;
end.
