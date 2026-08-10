{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game over (player lost) view. }
unit GameViewGameOver;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewGameOver = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonTryAgain: TCastleButton;
  private
    procedure ClickTryAgain(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  ViewGameOver: TViewGameOver;

implementation

uses GameViewMain;

constructor TViewGameOver.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewgameover.castle-user-interface';
end;

procedure TViewGameOver.Start;
begin
  inherited;
  ButtonTryAgain.OnClick := {$ifdef FPC}@{$endif} ClickTryAgain;
end;

procedure TViewGameOver.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

procedure TViewGameOver.ClickTryAgain(Sender: TObject);
begin
  Container.View := ViewMain;
end;

end.
