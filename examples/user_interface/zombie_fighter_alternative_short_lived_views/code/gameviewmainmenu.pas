{
  Copyright 2016-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game view with main menu. }
unit GameViewMainMenu;

interface

uses Classes, CastleControls, CastleUIControls;

type
  TViewMainMenu = class(TCastleView)
  strict private
    ButtonNewGame, ButtonQuit: TCastleButton;
    procedure ClickNewGame(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

implementation

uses CastleColors, CastleWindow, CastleFilesUtils, CastleApplicationProperties,
  CastleUtils, CastleComponentSerialize,
  GameViewLoading;

{ TViewMainMenu ------------------------------------------------------------- }

constructor TViewMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmainmenu.castle-user-interface';
end;

procedure TViewMainMenu.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  ButtonNewGame := DesignedComponent('ButtonNewGame') as TCastleButton;
  ButtonQuit := DesignedComponent('ButtonQuit') as TCastleButton;

  { attach events }
  ButtonNewGame.OnClick := {$ifdef FPC}@{$endif}ClickNewGame;
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif}ClickQuit;

  // on some platforms, showing "Quit" button is not standard
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
end;

procedure TViewMainMenu.ClickNewGame(Sender: TObject);
begin
  Container.View := TViewLoading.CreateUntilStopped;
end;

procedure TViewMainMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
