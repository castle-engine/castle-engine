{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Win screen. }
unit GameStateWin;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TStateWin = class(TUIState)
  private
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonBack: TCastleButton;
  end;

var
  StateWin: TStateWin;

implementation

uses GameStateMenu;

constructor TStateWin.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatewin.castle-user-interface';
end;

procedure TStateWin.Start;
begin
  inherited;
  ButtonBack.OnClick := {$ifdef FPC}@{$endif} ClickBack;
end;

procedure TStateWin.ClickBack(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

end.
