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

{ State pause. }
unit GameStatePause;

interface

uses Classes,
  CastleUIState, CastleControls;

type
  TStatePause = class(TUIState)
  private
    ButtonMenu: TCastleButton;
    ButtonResume: TCastleButton;

    procedure ClickResume(Sender: TObject);
    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;

  end;

var
  StatePause: TStatePause;

implementation

uses CastleSoundEngine, GameStateMenu, GameStatePlay;

constructor TStatePause.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatepause.castle-user-interface';
end;

procedure TStatePause.ClickResume(Sender: TObject);
begin
  StatePlay.ResumeGame;
  Pop;
end;

procedure TStatePause.ClickMenu(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

procedure TStatePause.Start;
begin
  inherited;

  ButtonResume := DesignedComponent('ButtonResume') as TCastleButton;
  ButtonResume.OnClick := {$ifdef FPC}@{$endif}ClickResume;

  ButtonMenu := DesignedComponent('ButtonMenu') as TCastleButton;
  ButtonMenu.OnClick := {$ifdef FPC}@{$endif}ClickMenu;

  { Play menu music }
  SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('menu_music');
end;

end.
