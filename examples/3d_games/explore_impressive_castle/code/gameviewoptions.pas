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

{ Game options, just volume for now. }
unit GameViewOptions;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleViewport, CastleSoundEngine;

type
  TViewOptions = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonBackMenu, ButtonBackGame: TCastleButton;
    Fade: TCastleRectangleControl;
    SliderVolume: TCastleIntegerSlider;
  private
    procedure ClickBackMenu(Sender: TObject);
    procedure ClickBackGame(Sender: TObject);
    procedure ChangeSliderVolume(Sender: TObject);
  public
    { Whether this is displayed on top of ViewPlay
      or not (in which case this goes back to ViewMenu). }
    OverGame: Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewOptions: TViewOptions;

implementation

uses SysUtils,
  CastleLog, CastleConfig,
  GameViewMenu, GameViewportUnderUi;

constructor TViewOptions.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewoptions.castle-user-interface';
  DesignPreload := true; // make it fast to transition to this view
end;

procedure TViewOptions.Start;
begin
  inherited;
  ButtonBackMenu.OnClick := {$ifdef FPC}@{$endif} ClickBackMenu;
  ButtonBackGame.OnClick := {$ifdef FPC}@{$endif} ClickBackGame;

  SliderVolume.Value := Round(SoundEngine.Volume * SliderVolume.Max);
  SliderVolume.OnChange := {$ifdef FPC}@{$endif} ChangeSliderVolume;

  if not OverGame then
    InsertBack(ViewportUnderUi);

  Fade.Exists := true; // looks bette also when from main menu
  ButtonBackGame.Exists := OverGame;

  if OverGame then
    ButtonBackMenu.Caption := 'BACK TO MENU (ABORT THE GAME)'
  else
    ButtonBackMenu.Caption := 'BACK TO MENU';
end;

procedure TViewOptions.Stop;
begin
  RemoveControl(ViewportUnderUi);
  inherited;
end;

procedure TViewOptions.ClickBackMenu(Sender: TObject);
begin
  Container.View := ViewMenu;
end;

procedure TViewOptions.ClickBackGame(Sender: TObject);
begin
  Container.PopView(Self);
end;

procedure TViewOptions.ChangeSliderVolume(Sender: TObject);
begin
  SoundEngine.Volume := SliderVolume.Value / SliderVolume.Max;

  { save volume to UserConfig, to make it saved for next game run }
  UserConfig.SetDeleteFloat('sound_volume', SoundEngine.Volume, 1);
  UserConfig.Save;
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
