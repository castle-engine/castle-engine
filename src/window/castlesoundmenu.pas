{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Menu items (suitable for TCastleOnScreenMenu) to control the TRepoSoundEngine. }
unit CastleSoundMenu;

{$I castleconf.inc}

interface

uses Classes,
  CastleOnScreenMenu, CastleSoundEngine, CastleUIControls,
  CastleControls;

type
  { On-screen menu item that displays sound information.
    Simply add it to your TCastleOnScreenMenu like

    @longCode(#
      OnScreenMenu.Add(TSoundInfoMenuItem.Create(OnScreenMenu));
    #)

    Also be sure to assign Application.MainWindow, it is used
    as the window from which we show modal dialog box with sound information. }
  TSoundInfoMenuItem = class(TCastleLabel)
  strict private
    procedure ButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { Float slider suitable for volume setting.
    Range is always [0 .. 1] and when the slider is exactly
    on 0.0 position it shows "Off". }
  TMenuVolumeSlider = class(TCastleFloatSlider)
    function ValueToStr(const AValue: Single): string; override;
  end;

  { On-screen menu item that allows to control sound volume.
    Simply add it to your TCastleOnScreenMenu like

    @longCode(#
      OnScreenMenu.Add(TSoundVolumeMenuItem.Create(OnScreenMenu));
    #) }
  TSoundVolumeMenuItem = class(TCastleLabel)
  private
    FSlider: TMenuVolumeSlider;
    property Slider: TMenuVolumeSlider read FSlider;
    procedure SliderValueChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    { Call this if volume changed by something outside of this class. }
    procedure Refresh;
  end;

  { On-screen menu item that allows to control music volume.
    Simply add it to your TCastleOnScreenMenu like

    @longCode(#
      OnScreenMenu.Add(TMusicVolumeMenuItem.Create(OnScreenMenu));
    #) }
  TMusicVolumeMenuItem = class(TCastleLabel)
  private
    FSlider: TMenuVolumeSlider;
    property Slider: TMenuVolumeSlider read FSlider;
    procedure SliderValueChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    { Call this if volume changed by something outside of this class. }
    procedure Refresh;
  end;

implementation

uses CastleWindow, CastleClassUtils, CastleUtils, CastleMessages;

{ TSoundInfoMenuItem ------------------------------------------------------- }

constructor TSoundInfoMenuItem.Create(AOwner: TComponent);
var
  Button: TCastleMenuButton;
begin
  inherited;
  Caption := 'View sound information';
  Button := TCastleMenuButton.Create(Self);
  Button.OnClick := @ButtonClick;
  InsertFront(Button);
end;

procedure TSoundInfoMenuItem.ButtonClick(Sender: TObject);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Append('Sound library (OpenAL) status:');
    S.Append('');
    Strings_AddSplittedString(S, SoundEngine.SoundInitializationReport, nl);

    if Application.MainWindow = nil then
      raise EInternalError.Create('You must assign Application.MainWindow to make clicks on TSoundInfoMenuItem work');

    MessageOK(Application.MainWindow, S);
  finally S.Free end;
end;

{ TMenuVolumeSlider ---------------------------------------------------- }

function TMenuVolumeSlider.ValueToStr(const AValue: Single): string;
begin
  if AValue = 0.0 then
    Result := 'Off' else
    Result := inherited ValueToStr(AValue);
end;

{ TSoundVolumeMenuItem ----------------------------------------------------- }

constructor TSoundVolumeMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Volume';

  FSlider := TMenuVolumeSlider.Create(Self);
  FSlider.Value := SoundEngine.Volume;
  FSlider.OnChange := @SliderValueChanged;
  InsertFront(FSlider);
end;

procedure TSoundVolumeMenuItem.SliderValueChanged(Sender: TObject);
begin
  SoundEngine.Volume := Slider.Value;
end;

procedure TSoundVolumeMenuItem.Refresh;
begin
  Slider.Value := SoundEngine.Volume;
end;

{ TMusicVolumeMenuItem ----------------------------------------------------- }

constructor TMusicVolumeMenuItem.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Music volume';

  FSlider := TMenuVolumeSlider.Create(Self);
  FSlider.Value := SoundEngine.MusicPlayer.MusicVolume;
  FSlider.OnChange := @SliderValueChanged;
  InsertFront(FSlider);
end;

procedure TMusicVolumeMenuItem.SliderValueChanged(Sender: TObject);
begin
  SoundEngine.MusicPlayer.MusicVolume := Slider.Value;
end;

procedure TMusicVolumeMenuItem.Refresh;
begin
  Slider.Value := SoundEngine.MusicPlayer.MusicVolume;
end;

end.
