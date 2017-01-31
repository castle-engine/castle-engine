{
  Copyright 2016-2017 Michalis Kamburelis.

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
    SimpleBackground: TCastleSimpleBackground;
    Menu: TCastleOnScreenMenu;
    procedure NewGameClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils,
  CastleUtils,
  GameStatePlay;

{ TStateMainMenu ------------------------------------------------------------- }

procedure TStateMainMenu.Start;
begin
  inherited;

  SimpleBackground := TCastleSimpleBackground.Create(FreeAtStop);
  SimpleBackground.Color := Black;
  InsertFront(SimpleBackground);

  Menu := TCastleOnScreenMenu.Create(FreeAtStop);
  Menu.DrawFocusedBorder := false;
  Menu.FontSize := 100;
  Menu.Add('New game', @NewGameClick);
  Menu.Add('Quit', @QuitClick);
  Menu.Anchor(hpMiddle);
  Menu.Anchor(vpMiddle);
  InsertFront(Menu);
end;

procedure TStateMainMenu.NewGameClick(Sender: TObject);
begin
  TUIState.Current := StatePlay;
end;

procedure TStateMainMenu.QuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
