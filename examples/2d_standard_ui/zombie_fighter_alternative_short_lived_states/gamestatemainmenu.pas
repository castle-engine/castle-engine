{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game state with main menu. }
unit GameStateMainMenu;

interface

uses Classes, CastleControls, CastleUIState, CastleOnScreenMenu;

type
  TStateMainMenu = class(TUIState)
  strict private
    Background: TCastleRectangleControl;
    Menu: TCastleOnScreenMenu;
    procedure NewGameClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
  public
    procedure Start; override;
  end;

implementation

uses CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils,
  CastleUtils,
  GameStatePlay;

{ TStateMainMenu ------------------------------------------------------------- }

procedure TStateMainMenu.Start;
begin
  inherited;

  Background := TCastleRectangleControl.Create(FreeAtStop);
  Background.FullSize := true;
  Background.Color := LightBlue;
  InsertFront(Background);

  Menu := TCastleOnScreenMenu.Create(FreeAtStop);
  Menu.FontSize := 30;
  Menu.Add('New game', @NewGameClick);
  Menu.Add('Quit', @QuitClick);
  Menu.DrawFocusedBorder := false;
  Menu.DrawBackgroundRectangle := false;
  Menu.CaptureAllEvents := true;
  Menu.Anchor(hpMiddle);
  Menu.Anchor(vpMiddle);
  InsertFront(Menu);
end;

procedure TStateMainMenu.NewGameClick(Sender: TObject);
begin
  TUIState.Current := TStatePlay.CreateUntilStopped;
end;

procedure TStateMainMenu.QuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
