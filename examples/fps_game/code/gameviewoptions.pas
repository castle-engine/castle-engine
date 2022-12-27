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

{ Game options, just volume for now. }
unit GameViewOptions;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleViewport, CastleSoundEngine;

type
  TViewOptions = class(TCastleView)
  private
    ButtonsVolume: array [0..10] of TCastleButton;
    procedure ClickBackMenu(Sender: TObject);
    procedure ClickBackGame(Sender: TObject);
    procedure ClickVolume(Sender: TObject);
  public
    { Whether this is displayed on top of ViewPlay
      or not (in which case this goes back to ViewMenu). }
    OverGame: Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonBackMenu, ButtonBackGame: TCastleButton;
    ViewportUnderUi: TCastleViewport;
    Fade: TCastleRectangleControl;
  end;

var
  ViewOptions: TViewOptions;

implementation

uses SysUtils,
  CastleLog,
  GameViewMenu;

constructor TViewOptions.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewoptions.castle-user-interface';
  DesignPreload := true; // make it fast to transition to this view
end;

procedure TViewOptions.Start;
var
  I: Integer;
begin
  inherited;
  for I := Low(ButtonsVolume) to High(ButtonsVolume) do
  begin
    ButtonsVolume[I] := DesignedComponent('ButtonVolume' + IntToStr(I)) as TCastleButton;
    ButtonsVolume[I].Tag := I;
    ButtonsVolume[I].OnClick := {$ifdef FPC}@{$endif} ClickVolume;
  end;
  ButtonBackMenu.OnClick := {$ifdef FPC}@{$endif} ClickBackMenu;
  ButtonBackGame.OnClick := {$ifdef FPC}@{$endif} ClickBackGame;

  ViewportUnderUi.Exists := not OverGame;
  Fade.Exists := OverGame;
  ButtonBackGame.Exists := OverGame;

  if OverGame then
    ButtonBackMenu.Caption := 'Back to menu (abort the game)'
  else
    ButtonBackMenu.Caption := 'Back to menu';
end;

procedure TViewOptions.ClickBackMenu(Sender: TObject);
begin
  Container.View := ViewMenu;
end;

procedure TViewOptions.ClickBackGame(Sender: TObject);
begin
  Container.PopView(Self);
end;

procedure TViewOptions.ClickVolume(Sender: TObject);
begin
  SoundEngine.Volume := (Sender as TCastleButton).Tag / High(ButtonsVolume);
end;

function TViewOptions.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) then
  begin
    { on Escape, go back to the most natural place, depending on OverGame }
    if OverGame then
      ClickBackGame(nil)
    else
      ClickBackMenu(nil);
    Exit(true);
  end;
end;

end.
