{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display instructions. }
unit GameStateInstructions;

interface

uses CastleUIState, CastleControls, CastleWindow, CastleUIControls;

type
  TStateInstructions = class(TUIState)
  strict private
    ButtonClose: TCastleButton;
    procedure ClickClose(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateInstructions: TStateInstructions;

implementation

uses SysUtils, Classes,
  CastleComponentSerialize,
  GameStatePlay;

procedure TStateInstructions.Start;
var
  Ui: TCastleUserInterface;
  UiOwner: TComponent;
begin
  inherited;

  { UiOwner allows to search for components using FindRequiredComponent,
    and makes sure the entire UI will be freed when state stops
    (because UiOwner is owned by FreeAtStop). }
  UiOwner := TComponent.Create(FreeAtStop);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/state_instructions.castle-user-interface', UiOwner);
  InsertFront(Ui);

  ButtonClose := UiOwner.FindRequiredComponent('ButtonClose') as TCastleButton;
  ButtonClose.OnClick := @ClickClose;

  { do not pass clicks to state underneath }
  InterceptInput := true;
end;

procedure TStateInstructions.ClickClose(Sender: TObject);
begin
  TUIState.Pop(Self);
end;

end.
