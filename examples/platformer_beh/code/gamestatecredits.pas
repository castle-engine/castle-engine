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

{ Credits screen }
unit GameStateCredits;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls;

type
  { Credits state }
  TStateCredits = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_menu.castle-user-interface. }
    ButtonCGE, ButtonGraphics, ButtonMusic, ButtonSources, ButtonMenu: TCastleButton;
    procedure ClickCGE(Sender: TObject);
    procedure ClickGraphics(Sender: TObject);
    procedure ClickMusic(Sender: TObject);
    procedure ClickSources(Sender: TObject);
    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  StateCredits: TStateCredits;

implementation

uses CastleApplicationProperties, CastleWindow, CastleOpenDocument,
  GameStateMenu;

{ TStateMenu ----------------------------------------------------------------- }

constructor TStateCredits.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatecredits.castle-user-interface';
end;

procedure TStateCredits.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  ButtonMenu := DesignedComponent('ButtonMenu') as TCastleButton;
  ButtonCGE := DesignedComponent('ButtonCGE') as TCastleButton;
  ButtonGraphics := DesignedComponent('ButtonGraphics') as TCastleButton;
  ButtonMusic := DesignedComponent('ButtonMusic') as TCastleButton;
  ButtonSources := DesignedComponent('ButtonSources') as TCastleButton;

  ButtonMenu.OnClick := {$ifdef FPC}@{$endif}ClickMenu;
  ButtonCGE.OnClick := {$ifdef FPC}@{$endif}ClickCGE;
  ButtonGraphics.OnClick := {$ifdef FPC}@{$endif}ClickGraphics;
  ButtonMusic.OnClick := {$ifdef FPC}@{$endif}ClickMusic;
  ButtonSources.OnClick := {$ifdef FPC}@{$endif}ClickSources;
end;

procedure TStateCredits.ClickMenu(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

procedure TStateCredits.ClickCGE(Sender: TObject);
begin
  OpenURL('https://castle-engine.io');
end;

procedure TStateCredits.ClickGraphics(Sender: TObject);
begin
  OpenURL('https://www.kenney.nl');
end;

procedure TStateCredits.ClickMusic(Sender: TObject);
begin
  OpenURL('https://www.akimaze.com');
end;

procedure TStateCredits.ClickSources(Sender: TObject);
begin
  OpenURL('https://github.com/castle-engine/castle-engine/tree/master/examples/platformer');
end;

end.
