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

uses Classes,
  CastleUIState, CastleControls, CastleWindow, CastleUIControls;

type
  TStateInstructions = class(TUIState)
  strict private
    ButtonClose: TCastleButton;
    procedure ClickClose(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  StateInstructions: TStateInstructions;

implementation

uses SysUtils,
  CastleComponentSerialize,
  GameStatePlay;

constructor TStateInstructions.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateinstructions.castle-user-interface';
end;

procedure TStateInstructions.Start;
begin
  inherited;

  ButtonClose := DesignedComponent('ButtonClose') as TCastleButton;
  ButtonClose.OnClick := {$ifdef FPC}@{$endif}ClickClose;

  { do not pass clicks to state underneath }
  InterceptInput := true;
end;

procedure TStateInstructions.ClickClose(Sender: TObject);
begin
  TUIState.Pop(Self);
end;

end.
