{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main menu, at the start of the game. }
unit GameStateMenu;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleSoundEngine;

type
  TStateMenu = class(TUIState)
  private
    procedure ClickCredits(Sender: TObject);
    procedure ClickPlay(Sender: TObject);
    procedure ClickOptions(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonPlay: TCastleButton;
    ButtonQuit: TCastleButton;
    ButtonOptions: TCastleButton;
    ButtonCredits: TCastleButton;
  end;

var
  StateMenu: TStateMenu;

implementation

uses CastleApplicationProperties, CastleWindow,
  GameStateOptions, GameStatePlay, GameStateCredits;

constructor TStateMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemenu.castle-user-interface';
  DesignPreload := true; // make it fast to transition to this state
end;

procedure TStateMenu.Start;
begin
  inherited;

  ButtonPlay.OnClick := {$ifdef FPC}@{$endif} ClickPlay;
  ButtonOptions.OnClick := {$ifdef FPC}@{$endif} ClickOptions;
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif} ClickQuit;
  ButtonCredits.OnClick  := {$ifdef FPC}@{$endif} ClickCredits;

  // Hide "Quit" button on mobile/console platforms, where users don't expect such button
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
end;

procedure TStateMenu.ClickCredits(Sender: TObject);
begin
  TUIState.Current := StateCredits;
end;

procedure TStateMenu.ClickPlay(Sender: TObject);
begin
  TUIState.Current := StatePlay;
end;

procedure TStateMenu.ClickOptions(Sender: TObject);
begin
  StateOptions.OverGame := false;
  TUIState.Current := StateOptions;
end;

procedure TStateMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
