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

{ Simple "menu" user interface, that allows to run the game or quit. }
unit GameViewMenu;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleSoundEngine;

type
  { Simple "menu" user interface, that allows to run the game or quit. }
  TViewMenu = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonPlay, ButtonOptions, ButtonCredits, ButtonQuit: TCastleButton;
  private
    procedure ClickPlay(Sender: TObject);
    procedure ClickOptions(Sender: TObject);
    procedure ClickCredits(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewMenu: TViewMenu;

implementation

uses CastleApplicationProperties, CastleWindow,
  GameSound, GameViewPlay, GameViewOptions, GameViewCredits;

{ TViewMenu ----------------------------------------------------------------- }

constructor TViewMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmenu.castle-user-interface';
end;

procedure TViewMenu.Start;
begin
  inherited;

  ButtonPlay.OnClick := {$ifdef FPC}@{$endif}ClickPlay;
  ButtonOptions.OnClick := {$ifdef FPC}@{$endif}ClickOptions;
  ButtonCredits.OnClick := {$ifdef FPC}@{$endif}ClickCredits;

  ButtonQuit.OnClick := {$ifdef FPC}@{$endif}ClickQuit;
  // Hide "Quit" button on mobile/console platforms, where users don't expect such button
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;

  { Play menu music }
  SoundEngine.LoopingChannel[0].Sound := NamedSound('MenuMusic');
end;

procedure TViewMenu.ClickPlay(Sender: TObject);
begin
  Container.View := ViewPlay;
end;

procedure TViewMenu.ClickOptions(Sender: TObject);
begin
  Container.View := ViewOptions;
end;

procedure TViewMenu.ClickCredits(Sender: TObject);
begin
  Container.View := ViewCredits;
end;

procedure TViewMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
