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

{ Simple options user interface, that allows to change sound settings. }
unit GameStateOptions;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleSoundEngine;

type
  { Simple options user interface, that allows to change sound settings. }
  TStateOptions = class(TUIState)
  private
    { Components designed using CGE editor, loaded from state_menu.castle-user-interface. }
    ButtonMenu: TCastleButton;
    VolumeGroup: TCastleHorizontalGroup;
    MusicGroup: TCastleHorizontalGroup;

    procedure SetVolume(const Volume: Single);
    procedure SetMusic(const Music: Single);
    procedure ClickMenu(Sender: TObject);
    procedure ClickVolume(Sender: TObject);
    procedure ClickMusic(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  StateOptions: TStateOptions;

implementation

uses CastleApplicationProperties, CastleWindow, CastleConfig,
  GameStateMenu;

{ TStateMenu ----------------------------------------------------------------- }

constructor TStateOptions.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateoptions.castle-user-interface';
end;

procedure TStateOptions.Start;
var
  I: Integer;
  Button: TCastleButton;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  ButtonMenu := DesignedComponent('ButtonMenu') as TCastleButton;
  ButtonMenu.OnClick := @ClickMenu;

  VolumeGroup := DesignedComponent('HorizontalGroupVolume') as TCastleHorizontalGroup;
  for I := 1 to VolumeGroup.ControlsCount - 1 do
  begin
    Button := VolumeGroup.Controls[I] as TCastleButton;
    Button.OnClick := @ClickVolume;
  end;

  MusicGroup := DesignedComponent('HorizontalGroupMusic') as TCastleHorizontalGroup;
  for I := 1 to MusicGroup.ControlsCount - 1 do
  begin
    Button := MusicGroup.Controls[I] as TCastleButton;
    Button.OnClick := @ClickMusic;
  end;

  SetVolume(UserConfig.GetFloat('volume', 1.0));
  SetMusic(UserConfig.GetFloat('music', 1.0));
end;

procedure TStateOptions.SetVolume(const Volume: Single);
var
  I: Integer;
  MaxButtonIndex: Integer;
  Button: TCastleButton;
begin
  SoundEngine.Volume := Volume;
  UserConfig.SetFloat('volume', Volume);
  UserConfig.Save;

  MaxButtonIndex := Round(Volume * 10) + 1;

  for I := 1 to VolumeGroup.ControlsCount - 1 do
  begin
    Button := VolumeGroup.Controls[I] as TCastleButton;
    if I <= MaxButtonIndex then
      Button.CustomBackgroundNormal.URL := 'castle-data:/ui/red_circle.png'
    else
      Button.CustomBackgroundNormal.URL := 'castle-data:/ui/grey_circle.png';
  end;
end;

procedure TStateOptions.SetMusic(const Music: Single);
var
  I: Integer;
  MaxButtonIndex: Integer;
  Button: TCastleButton;
begin
  SoundEngine.LoopingChannel[0].Volume := Music;
  UserConfig.SetFloat('music', Music);
  UserConfig.Save;

  MaxButtonIndex := Round(Music * 10) + 1;

  for I := 1 to MusicGroup.ControlsCount - 1 do
  begin
    Button := MusicGroup.Controls[I] as TCastleButton;
    if I <= MaxButtonIndex then
      Button.CustomBackgroundNormal.URL := 'castle-data:/ui/red_circle.png'
    else
      Button.CustomBackgroundNormal.URL := 'castle-data:/ui/grey_circle.png';
  end;
end;

procedure TStateOptions.ClickMenu(Sender: TObject);
begin
  TUIState.Current := StateMenu;
end;

procedure TStateOptions.ClickVolume(Sender: TObject);
var
  ClickedButtonIndex: Integer;
begin
  ClickedButtonIndex := VolumeGroup.IndexOfControl(Sender as TCastleUserInterface);

  SetVolume((ClickedButtonIndex -1) / 10);
end;

procedure TStateOptions.ClickMusic(Sender: TObject);
var
  ClickedButtonIndex: Integer;
begin
  ClickedButtonIndex := MusicGroup.IndexOfControl(Sender as TCastleUserInterface);

  SetMusic((ClickedButtonIndex -1) / 10);
end;

end.
