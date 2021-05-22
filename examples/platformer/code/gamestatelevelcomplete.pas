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

{ State when you win. }
unit GameStateLevelComplete;

interface

uses Classes,
  CastleUIState, CastleControls;

type
  TStateLevelComplete = class(TUIState)
  private
    ButtonMenu: TCastleButton;

    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;

  end;

var
  StateLevelComplete: TStateLevelComplete;

implementation

uses CastleSoundEngine, GameStateMenu;

constructor TStateLevelComplete.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatelevelcomplete.castle-user-interface';
end;

procedure TStateLevelComplete.ClickMenu(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

procedure TStateLevelComplete.Start;
begin
  inherited;

  ButtonMenu := DesignedComponent('ButtonMenu') as TCastleButton;
  ButtonMenu.OnClick := @ClickMenu;

  { Play menu music }
  SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('menu_music');
end;

end.
