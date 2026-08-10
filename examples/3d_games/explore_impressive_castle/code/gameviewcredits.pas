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

{ Credits view. }
unit GameViewCredits;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewCredits = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonBack: TCastleButton;
    ButtonCredits: TCastleButton;
  private
    procedure ClickBack(Sender: TObject);
    procedure ClickCredits(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
  end;

var
  ViewCredits: TViewCredits;

implementation

uses CastleOpenDocument,
  GameViewMenu, GameViewportUnderUi;

procedure TViewCredits.ClickBack(Sender: TObject);
begin
  Container.View := ViewMenu;
end;

constructor TViewCredits.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewcredits.castle-user-interface';
end;

procedure TViewCredits.Start;
begin
  inherited;
  ButtonBack.OnClick  := {$ifdef FPC}@{$endif} ClickBack;
  ButtonCredits.OnClick  := {$ifdef FPC}@{$endif} ClickCredits;
  InsertBack(ViewportUnderUi);
end;

procedure TViewCredits.Stop;
begin
  RemoveControl(ViewportUnderUi);
  inherited;
end;

procedure TViewCredits.ClickCredits(Sender: TObject);
begin
  OpenUrl('https://castle-engine.io/credits');
end;

end.
