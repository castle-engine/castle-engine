{
  Copyright 2006-2014 Michalis Kamburelis.

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

interface

uses CastleWindow, CastleOnScreenMenu, CastleSoundEngine, CastleUIControls,
  CastleControls;

type
  { An abstract class for CastleSoundMenu items.
    In the future maybe such idea will be incorporated into CastleOnScreenMenu unit. }
  TOnScreenMenuItem = class
  private
    FWindow: TCastleWindowCustom;
  protected
    property Window: TCastleWindowCustom read FWindow;
  public
    { Creates and adds this menu item to Menu. }
    constructor Create(AWindow: TCastleWindowCustom; Menu: TCastleOnScreenMenu);
    function Title: string; virtual; abstract;
    function Accessory: TUIControl; virtual;
    procedure Selected; virtual;
  end;

  TSoundMenuItem = class(TOnScreenMenuItem)
  end;

  TSoundInfoMenuItem = class(TSoundMenuItem)
  public
    function Title: string; override;
    procedure Selected; override;
  end;

  { Float slider suitable for volume setting.
    Range is always [0 .. 1] and when the slider is exactly
    on 0.0 position it shows "Off". }
  TMenuVolumeSlider = class(TCastleFloatSlider)
    function ValueToStr(const AValue: Single): string; override;
  end;

  TSoundVolumeMenuItem = class(TSoundMenuItem)
  private
    FSlider: TMenuVolumeSlider;
    property Slider: TMenuVolumeSlider read FSlider;
    procedure SliderValueChanged(Sender: TObject);
  public
    constructor Create(AWindow: TCastleWindowCustom; Menu: TCastleOnScreenMenu);

    { Call this if volume changed by something outside of this class. }
    procedure Refresh;

    function Title: string; override;
    function Accessory: TUIControl; override;
  end;

  TMusicVolumeMenuItem = class(TSoundMenuItem)
  private
    FSlider: TMenuVolumeSlider;
    property Slider: TMenuVolumeSlider read FSlider;
    procedure SliderValueChanged(Sender: TObject);
  public
    constructor Create(AWindow: TCastleWindowCustom; Menu: TCastleOnScreenMenu);

    { Call this if volume changed by something outside of this class. }
    procedure Refresh;

    function Title: string; override;
    function Accessory: TUIControl; override;
  end;

implementation

uses Classes, CastleClassUtils, CastleUtils, CastleMessages;

{ TOnScreenMenuItem ---------------------------------------------------------------- }

constructor TOnScreenMenuItem.Create(AWindow: TCastleWindowCustom; Menu: TCastleOnScreenMenu);
begin
  inherited Create;
  Menu.Add(Title, Accessory);
  FWindow := AWindow;
end;

function TOnScreenMenuItem.Accessory: TUIControl;
begin
  Result := nil;
end;

procedure TOnScreenMenuItem.Selected;
begin
end;

{ TSoundInfoMenuItem ------------------------------------------------------- }

function TSoundInfoMenuItem.Title: string;
begin
  Result := 'View sound information';
end;

procedure TSoundInfoMenuItem.Selected;
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Append('Sound library (OpenAL) status:');
    S.Append('');
    Strings_AddSplittedString(S, SoundEngine.SoundInitializationReport, nl);

    MessageOK(Window, S);
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

constructor TSoundVolumeMenuItem.Create(AWindow: TCastleWindowCustom; Menu: TCastleOnScreenMenu);
begin
  FSlider := TMenuVolumeSlider.Create(Menu);
  FSlider.Value := SoundEngine.Volume;
  FSlider.OnChange := @SliderValueChanged;
  inherited;
end;

function TSoundVolumeMenuItem.Title: string;
begin
  Result := 'Volume';
end;

function TSoundVolumeMenuItem.Accessory: TUIControl;
begin
  Result := FSlider;
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

constructor TMusicVolumeMenuItem.Create(AWindow: TCastleWindowCustom; Menu: TCastleOnScreenMenu);
begin
  FSlider := TMenuVolumeSlider.Create(Menu);
  FSlider.Value := SoundEngine.MusicPlayer.MusicVolume;
  FSlider.OnChange := @SliderValueChanged;
  inherited;
end;

function TMusicVolumeMenuItem.Title: string;
begin
  Result := 'Music volume';
end;

function TMusicVolumeMenuItem.Accessory: TUIControl;
begin
  Result := FSlider;
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
