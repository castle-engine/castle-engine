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

{ Display main menu. }
unit GameStateMainMenu;

interface

uses CastleUIState, CastleControls, CastleWindow, CastleUIControls;

type
  TStateMainMenu = class(TUIState)
  strict private
    ButtonPlayHexagonal: TCastleButton;
    ButtonPlayIsometricStaggered: TCastleButton;
    ButtonPlayIsometric: TCastleButton;
    ButtonPlayOrthogonal: TCastleButton;
    ButtonQuit: TCastleButton;
    procedure ClickHexagonal(Sender: TObject);
    procedure ClickIsometricStaggered(Sender: TObject);
    procedure ClickIsometric(Sender: TObject);
    procedure ClickOrthogonal(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses SysUtils, Classes,
  CastleComponentSerialize, CastleApplicationProperties,
  GameStatePlay;

procedure TStateMainMenu.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main_menu.castle-user-interface', FreeAtStop, UiOwner);

  ButtonPlayHexagonal := UiOwner.FindRequiredComponent('ButtonPlayHexagonal') as TCastleButton;
  ButtonPlayIsometricStaggered := UiOwner.FindRequiredComponent('ButtonPlayIsometricStaggered') as TCastleButton;
  ButtonPlayIsometric := UiOwner.FindRequiredComponent('ButtonPlayIsometric') as TCastleButton;
  ButtonPlayOrthogonal := UiOwner.FindRequiredComponent('ButtonPlayOrthogonal') as TCastleButton;
  ButtonQuit := UiOwner.FindRequiredComponent('ButtonQuit') as TCastleButton;

  ButtonPlayHexagonal.OnClick := @ClickHexagonal;
  ButtonPlayIsometricStaggered.OnClick := @ClickIsometricStaggered;
  ButtonPlayIsometric.OnClick := @ClickIsometric;
  ButtonPlayOrthogonal.OnClick := @ClickOrthogonal;
  ButtonQuit.OnClick := @ClickQuit;
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
end;

procedure TStateMainMenu.ClickHexagonal(Sender: TObject);
begin
  StatePlay.MapName := 'map-hexagonal';
  TUIState.Current := StatePlay;
end;

procedure TStateMainMenu.ClickIsometricStaggered(Sender: TObject);
begin
  StatePlay.MapName := 'map-isometric-staggered';
  TUIState.Current := StatePlay;
end;

procedure TStateMainMenu.ClickIsometric(Sender: TObject);
begin
  StatePlay.MapName := 'map-isometric';
  TUIState.Current := StatePlay;
end;

procedure TStateMainMenu.ClickOrthogonal(Sender: TObject);
begin
  StatePlay.MapName := 'map-orthogonal';
  TUIState.Current := StatePlay;
end;

procedure TStateMainMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
