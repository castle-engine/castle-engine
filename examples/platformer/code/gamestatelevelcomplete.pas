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
    ButtonCredits: TCastleButton;

    procedure ClickCredits(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;

  end;

var
  StateLevelComplete: TStateLevelComplete;

implementation

uses CastleSoundEngine, GameStateCredits;

constructor TStateLevelComplete.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatelevelcomplete.castle-user-interface';
end;

procedure TStateLevelComplete.ClickCredits(Sender: TObject);
begin
  TUIState.Current := StateCredits;
end;

procedure TStateLevelComplete.Start;
begin
  inherited;

  ButtonCredits := DesignedComponent('ButtonCredits') as TCastleButton;
  ButtonCredits.OnClick := {$ifdef FPC}@{$endif}ClickCredits;

  { Play menu music }
  SoundEngine.LoopingChannel[0].Sound := SoundEngine.SoundFromName('menu_music');
end;

end.
