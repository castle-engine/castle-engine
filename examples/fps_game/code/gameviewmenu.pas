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

{ Main menu, at the start of the game. }
unit GameViewMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleSoundEngine;

type
  TViewMenu = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonPlay: TCastleButton;
    ButtonQuit: TCastleButton;
    ButtonOptions: TCastleButton;
    ButtonCredits: TCastleButton;
  private
    procedure ClickCredits(Sender: TObject);
    procedure ClickPlay(Sender: TObject);
    procedure ClickOptions(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
  end;

var
  ViewMenu: TViewMenu;

implementation

uses CastleApplicationProperties, CastleWindow,
  GameViewOptions, GameViewPlay, GameViewCredits, GameViewportUnderUi;

constructor TViewMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmenu.castle-user-interface';
  DesignPreload := true; // make it fast to transition to this view
end;

procedure TViewMenu.Start;
begin
  inherited;

  ButtonPlay.OnClick := {$ifdef FPC}@{$endif} ClickPlay;
  ButtonOptions.OnClick := {$ifdef FPC}@{$endif} ClickOptions;
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif} ClickQuit;
  ButtonCredits.OnClick  := {$ifdef FPC}@{$endif} ClickCredits;

  // Hide "Quit" button on mobile/console platforms, where users don't expect such button
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;

  InsertBack(ViewportUnderUi);
end;

procedure TViewMenu.Stop;
begin
  RemoveControl(ViewportUnderUi);
  inherited;
end;

procedure TViewMenu.ClickCredits(Sender: TObject);
begin
  Container.View := ViewCredits;
end;

procedure TViewMenu.ClickPlay(Sender: TObject);
begin
  Container.View := ViewPlay;
end;

procedure TViewMenu.ClickOptions(Sender: TObject);
begin
  ViewOptions.OverGame := false;
  Container.View := ViewOptions;
end;

procedure TViewMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
