{
  Copyright 2004-2023 Michalis Kamburelis.

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
    that TCastleWindow.Cursor indeed works as appropriate, i.e. changes the cursor
    only for the given window.
}

program multi_window;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses CastleWindow, SysUtils, CastleUtils, CastleGLUtils, CastleKeysMouse, CastleVectors,
  CastleStringUtils, CastleColors, CastleControls, CastleUIControls;

type
  TText = class(TCastleUserInterface)
  public
    Text: string;
    LightColor, DarkColor: TCastleColor;
    ParentWindow: TCastleWindow;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function CapturesEventsAtPosition(const Position: TVector2): boolean; override;
  end;

procedure TText.Render;
begin
  inherited;
  DrawRectangle(ParentRect, DarkColor);
  FallbackFont.Print(10, 10, LightColor, Text);
end;

function TText.Press(const Event: TInputPressRelease): boolean;
var
  URL: string;
begin
  Result := inherited;
  if Result then Exit;

  case Event.KeyCharacter of
    'c':
      begin
        if Cursor = High(Cursor) then Cursor := Low(Cursor) else Cursor := Succ(Cursor);
        Exit(true);
      end;
    'o':
      begin
        URL := '';
        { when file dialog is open, note that the other windows
          are still active as they should. }
        ParentWindow.FileDialog('Test open file dialog', URL, true);
        Exit(true);
      end;
  end;
end;

function TText.CapturesEventsAtPosition(const Position: TVector2): boolean;
begin
  Result := true; // always
end;

var
  I: Integer;
  Windows: array [0..4] of TCastleWindow;
  Text: TText;
begin
  for i := 0 to High(Windows) do
  begin
    Windows[I] := TCastleWindow.Create(Application);

    Text := TText.Create(Windows[I]);
    Text.Text := 'Window ' + IntToStr(I);
    Text.LightColor := Vector4(Random*1.5, Random*1.5, Random*1.5, 1);
    Text.DarkColor  := Vector4(Random*0.7, Random*0.7, Random*0.7, 1);
    Text.ParentWindow := Windows[I];
    Windows[I].Controls.InsertFront(Text);

    Windows[I].Caption := 'Window ' + IntToStr(I);
    Windows[I].Width := 200;
    Windows[I].Height := 200;
    Windows[I].Left := 30 + 250 * (I mod 3);
    Windows[I].Top := 30 + 250 * (I div 3);

    Windows[I].Open;
    Windows[I].SetDemoOptions(keyF11, CharEscape, true);
  end;

  Application.Run;
end.
