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
    ButtonMenu: TCastleButton;
    procedure ClickMenu(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  StateCredits: TStateCredits;

implementation

uses CastleApplicationProperties, CastleWindow,
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

  ButtonMenu.OnClick := @ClickMenu;
end;

procedure TStateCredits.ClickMenu(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

end.
