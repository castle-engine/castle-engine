{
  Copyright 2022-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Death (game lost) screen. }
unit GameViewDeath;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewDeath = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonBack: TCastleButton;
  private
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewDeath: TViewDeath;

implementation

uses GameViewMenu;

constructor TViewDeath.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdeath.castle-user-interface';
end;

procedure TViewDeath.Start;
begin
  inherited;
  ButtonBack.OnClick := {$ifdef FPC}@{$endif} ClickBack;
end;

procedure TViewDeath.ClickBack(Sender: TObject);
begin
  Container.View := ViewMenu;
end;

end.
