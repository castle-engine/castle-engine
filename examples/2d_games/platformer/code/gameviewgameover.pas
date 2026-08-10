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

{ View when game is over (player died or cancelled). }
unit GameViewGameOver;

interface

uses Classes,
  CastleUIControls, CastleControls;

type
  TViewGameOver = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonMenu: TCastleButton;
  private
    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;

  end;

var
  ViewGameOver: TViewGameOver;

implementation

uses CastleSoundEngine,
  GameSound, GameViewMenu;

constructor TViewGameOver.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewgameover.castle-user-interface';
end;

procedure TViewGameOver.ClickMenu(Sender: TObject);
begin
  Container.View := ViewMenu;
end;

procedure TViewGameOver.Start;
begin
  inherited;
  ButtonMenu.OnClick := {$ifdef FPC}@{$endif}ClickMenu;

  { Play menu music }
  SoundEngine.LoopingChannel[0].Sound := NamedSound('MenuMusic');
end;

end.
