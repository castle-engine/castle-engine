{
  Copyright 2021-2021 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ State when game is over (player died or cancelled). }
unit GameStateGameOver;

interface

uses Classes,
  CastleUIState, CastleControls;

type
  TStateGameOver = class(TUIState)
  private
    ButtonMenu: TCastleButton;

    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;

  end;

var
  StateGameOver: TStateGameOver;

implementation

uses CastleSoundEngine, GameStateMenu;

constructor TStateGameOver.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestategameover.castle-user-interface';
end;

procedure TStateGameOver.ClickMenu(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

procedure TStateGameOver.Start;
begin
  inherited;

  ButtonMenu := DesignedComponent('ButtonMenu') as TCastleButton;
  ButtonMenu.OnClick := {$ifdef FPC}@{$endif}ClickMenu;

  { Play menu music }
  SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('menu_music');
end;

end.
