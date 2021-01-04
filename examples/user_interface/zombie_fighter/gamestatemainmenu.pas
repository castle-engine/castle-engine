{
  Copyright 2016-2021 Michalis Kamburelis.

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
    ButtonNewGame, ButtonQuit: TCastleButton;
    procedure NewGameClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleApplicationProperties,
  CastleUtils, CastleComponentSerialize,
  GameStatePlay;

{ TStateMainMenu ------------------------------------------------------------- }

procedure TStateMainMenu.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main_menu.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  ButtonNewGame := UiOwner.FindRequiredComponent('ButtonNewGame') as TCastleButton;
  ButtonQuit := UiOwner.FindRequiredComponent('ButtonQuit') as TCastleButton;

  { attach events }
  ButtonNewGame.OnClick := @NewGameClick;
  ButtonQuit.OnClick := @QuitClick;

  // on some platforms, showing "Quit" button is not standard
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
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
