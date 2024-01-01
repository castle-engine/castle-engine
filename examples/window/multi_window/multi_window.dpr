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

{ Demo of using multiple open windows with TCastleWindow.

  Additional demo:
  press c to change cursor in the window that has focus, this is to demo
  that TCastleWindow.Cursor indeed works as appropriate, i.e. changes the cursor
  only for the given window.
}

program multi_window;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils, Classes,
  CastleWindow, CastleUtils, CastleGLUtils, CastleKeysMouse, CastleVectors,
  CastleStringUtils, CastleColors, CastleControls, CastleUIControls, CastleLog;

type
  TWindowView = class(TCastleUserInterface)
  private
    FStatusLabel: TCastleLabel;
  public
    WindowIndex: Integer;
    DarkColor: TCastleColor;
    ParentWindow: TCastleWindow;
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function CapturesEventsAtPosition(const Position: TVector2): boolean; override;
  end;

constructor TWindowView.Create(AOwner: TComponent);
begin
  inherited;

  FStatusLabel := TCastleLabel.Create(Self);
  FStatusLabel.FontSize := 10;
  FStatusLabel.Anchor(hpLeft, 10);
  FStatusLabel.Anchor(vpBottom, 10);
  // random light color
  FStatusLabel.Color := Vector4(
    RandomFloatRange(0.5, 1.0),
    RandomFloatRange(0.5, 1.0),
    RandomFloatRange(0.5, 1.0), 1);
  InsertFront(FStatusLabel);
end;

procedure TWindowView.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  FStatusLabel.Caption := Format('Window %d' + NL + 'Pixels width / height: %d / %d', [
    WindowIndex,
    ParentWindow.Width,
    ParentWindow.Height
  ]);
end;

procedure TWindowView.Render;
begin
  inherited;
  { TODO: use TCastleRectangleControl for drawing this.
    TODO: TWindowView should be class(TCastleView)
  }
  DrawRectangle(ParentRect, DarkColor);
end;

function TWindowView.Press(const Event: TInputPressRelease): boolean;
var
  Url: String;
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
        Url := '';
        { when file dialog is open, note that the other windows
          are still active as they should. }
        ParentWindow.FileDialog('Test open file dialog', Url, true);
        Exit(true);
      end;
  end;
end;

function TWindowView.CapturesEventsAtPosition(const Position: TVector2): boolean;
begin
  Result := true; // always
end;

var
  I: Integer;
  Windows: array [0..4] of TCastleWindow;
  WindowView: TWindowView;
begin
  InitializeLog;

  for i := 0 to High(Windows) do
  begin
    Windows[I] := TCastleWindow.Create(Application);

    WindowView := TWindowView.Create(Windows[I]);
    WindowView.WindowIndex := I;
    WindowView.DarkColor  := Vector4(Random*0.7, Random*0.7, Random*0.7, 1);
    WindowView.ParentWindow := Windows[I];
    Windows[I].Controls.InsertFront(WindowView);

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
