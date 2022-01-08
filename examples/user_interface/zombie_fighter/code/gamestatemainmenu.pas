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

uses Classes, CastleControls, CastleUIState;

type
  TStateMainMenu = class(TUIState)
  strict private
    ButtonNewGame, ButtonQuit: TCastleButton;
    procedure ClickNewGame(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses CastleColors, CastleWindow, CastleUIControls, CastleFilesUtils, CastleApplicationProperties,
  CastleUtils, CastleComponentSerialize,
  GameStateLoading;

{ TStateMainMenu ------------------------------------------------------------- }

constructor TStateMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemainmenu.castle-user-interface';
end;

procedure TStateMainMenu.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  ButtonNewGame := DesignedComponent('ButtonNewGame') as TCastleButton;
  ButtonQuit := DesignedComponent('ButtonQuit') as TCastleButton;

  { attach events }
  ButtonNewGame.OnClick := {$ifdef FPC}@{$endif}ClickNewGame;
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif}ClickQuit;

  // on some platforms, showing "Quit" button is not standard
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
end;

procedure TStateMainMenu.ClickNewGame(Sender: TObject);
begin
  Container.View := StateLoading;
end;

procedure TStateMainMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
