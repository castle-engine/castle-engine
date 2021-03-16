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
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_instructions.castle-user-interface', FreeAtStop, UiOwner);

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
