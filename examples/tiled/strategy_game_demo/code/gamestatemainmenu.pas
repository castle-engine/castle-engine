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

uses Classes,
  CastleUIState, CastleControls, CastleWindow, CastleUIControls;

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
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses SysUtils,
  CastleComponentSerialize, CastleApplicationProperties,
  GameStatePlay;

constructor TStateMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemainmenu.castle-user-interface';
end;

procedure TStateMainMenu.Start;
begin
  inherited;

  ButtonPlayHexagonal := DesignedComponent('ButtonPlayHexagonal') as TCastleButton;
  ButtonPlayIsometricStaggered := DesignedComponent('ButtonPlayIsometricStaggered') as TCastleButton;
  ButtonPlayIsometric := DesignedComponent('ButtonPlayIsometric') as TCastleButton;
  ButtonPlayOrthogonal := DesignedComponent('ButtonPlayOrthogonal') as TCastleButton;
  ButtonQuit := DesignedComponent('ButtonQuit') as TCastleButton;

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
