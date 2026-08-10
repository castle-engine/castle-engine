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
unit GameViewOptions;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleSoundEngine;

type
  { Simple options user interface, that allows to change sound settings. }
  TViewOptions = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonMenu: TCastleButton;
    VolumeGroup: TCastleHorizontalGroup;
    MusicGroup: TCastleHorizontalGroup;
  private
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
  ViewOptions: TViewOptions;

implementation

uses CastleApplicationProperties, CastleWindow, CastleConfig,
  GameViewMenu;

{ TViewMenu ----------------------------------------------------------------- }

constructor TViewOptions.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewoptions.castle-user-interface';
end;

procedure TViewOptions.Start;
var
  I: Integer;
  Button: TCastleButton;
begin
  inherited;

  ButtonMenu.OnClick := {$ifdef FPC}@{$endif}ClickMenu;

  for I := 1 to VolumeGroup.ControlsCount - 1 do
  begin
    Button := VolumeGroup.Controls[I] as TCastleButton;
    Button.OnClick := {$ifdef FPC}@{$endif}ClickVolume;
  end;

  for I := 1 to MusicGroup.ControlsCount - 1 do
  begin
    Button := MusicGroup.Controls[I] as TCastleButton;
    Button.OnClick := {$ifdef FPC}@{$endif}ClickMusic;
  end;

  SetVolume(UserConfig.GetFloat('volume', 1.0));
  SetMusic(UserConfig.GetFloat('music', 1.0));
end;

procedure TViewOptions.SetVolume(const Volume: Single);
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

procedure TViewOptions.SetMusic(const Music: Single);
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

procedure TViewOptions.ClickMenu(Sender: TObject);
begin
  Container.View := ViewMenu;
end;

procedure TViewOptions.ClickVolume(Sender: TObject);
var
  ClickedButtonIndex: Integer;
begin
  ClickedButtonIndex := VolumeGroup.IndexOfControl(Sender as TCastleUserInterface);

  SetVolume((ClickedButtonIndex -1) / 10);
end;

procedure TViewOptions.ClickMusic(Sender: TObject);
var
  ClickedButtonIndex: Integer;
begin
  ClickedButtonIndex := MusicGroup.IndexOfControl(Sender as TCastleUserInterface);

  SetMusic((ClickedButtonIndex -1) / 10);
end;

end.
