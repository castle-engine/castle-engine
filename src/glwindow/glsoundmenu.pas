{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Menu items (suitable for GLMenu) to control the TXmlSoundEngine. }
unit GLSoundMenu;

interface

uses GLWindow, GLMenu, XmlSoundEngine;

type
  { An abstract class for GLSoundMenu items.
    In the future maybe such idea will be incorporated into GLMenu unit. }
  TCastleMenuItem = class
  private
    FWindow: TCastleWindowBase;
  protected
    property Window: TCastleWindowBase read FWindow;
  public
    { Creates and adds this menu item to Menu. }
    constructor Create(AWindow: TCastleWindowBase; Menu: TCastleMenu);
    function Title: string; virtual; abstract;
    function Accessory: TMenuAccessory; virtual;
    procedure Selected; virtual;
    procedure AccessoryValueChanged; virtual;
  end;

  TGLSoundMenuItem = class(TCastleMenuItem)
  private
    FSoundEngine: TXmlSoundEngine;
  protected
    property SoundEngine: TXmlSoundEngine read FSoundEngine;
  public
    constructor Create(AWindow: TCastleWindowBase; Menu: TCastleMenu;
      ASoundEngine: TXmlSoundEngine);
  end;

  TGLSoundInfoMenuItem = class(TGLSoundMenuItem)
  public
    function Title: string; override;
    procedure Selected; override;
  end;

  { Float slider suitable for volume setting.
    Range is always [0 .. 1] and when the slider is exactly
    on 0.0 position it shows "Off". }
  TCastleMenuVolumeSlider = class(TMenuFloatSlider)
    constructor Create(const AValue: Single);
    function ValueToStr(const AValue: Single): string; override;
  end;

  TGLSoundVolumeMenuItem = class(TGLSoundMenuItem)
  private
    FSlider: TCastleMenuVolumeSlider;
    property Slider: TCastleMenuVolumeSlider read FSlider;
  public
    constructor Create(AWindow: TCastleWindowBase; Menu: TCastleMenu;
      ASoundEngine: TXmlSoundEngine);

    { Call this if volume changed by something outside of this class. }
    procedure RefreshAccessory;

    function Title: string; override;
    function Accessory: TMenuAccessory; override;
    procedure AccessoryValueChanged; override;
  end;

  TGLMusicVolumeMenuItem = class(TGLSoundMenuItem)
  private
    FSlider: TCastleMenuVolumeSlider;
    property Slider: TCastleMenuVolumeSlider read FSlider;
  public
    constructor Create(AWindow: TCastleWindowBase; Menu: TCastleMenu;
      ASoundEngine: TXmlSoundEngine);

    { Call this if volume changed by something outside of this class. }
    procedure RefreshAccessory;

    function Title: string; override;
    function Accessory: TMenuAccessory; override;
    procedure AccessoryValueChanged; override;
  end;

implementation

uses Classes, CastleClassUtils, CastleUtils, GLWinMessages;

{ TCastleMenuItem ---------------------------------------------------------------- }

constructor TCastleMenuItem.Create(AWindow: TCastleWindowBase; Menu: TCastleMenu);
begin
  inherited Create;
  Menu.Items.AddObject(Title, Accessory);
  FWindow := AWindow;
end;

function TCastleMenuItem.Accessory: TMenuAccessory;
begin
  Result := nil;
end;

procedure TCastleMenuItem.Selected;
begin
end;

procedure TCastleMenuItem.AccessoryValueChanged;
begin
end;

{ TGLSoundMenuItem ----------------------------------------------------------- }

constructor TGLSoundMenuItem.Create(AWindow: TCastleWindowBase;
  Menu: TCastleMenu; ASoundEngine: TXmlSoundEngine);
begin
  inherited Create(AWindow, Menu);
  FSoundEngine := ASoundEngine;
end;

{ TGLSoundInfoMenuItem ------------------------------------------------------- }

function TGLSoundInfoMenuItem.Title: string;
begin
  Result := 'View sound information';
end;

procedure TGLSoundInfoMenuItem.Selected;
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Append('Sound library (OpenAL) status:');
    S.Append('');
    Strings_AddSplittedString(S, SoundEngine.SoundInitializationReport, nl);

    MessageOK(Window, S, taLeft);
  finally S.Free end;
end;

{ TCastleMenuVolumeSlider ---------------------------------------------------- }

constructor TCastleMenuVolumeSlider.Create(const AValue: Single);
begin
  inherited Create(0, 1, AValue);
end;

function TCastleMenuVolumeSlider.ValueToStr(const AValue: Single): string;
begin
  if AValue = 0.0 then
    Result := 'Off' else
    Result := inherited ValueToStr(AValue);
end;

{ TGLSoundVolumeMenuItem ----------------------------------------------------- }

constructor TGLSoundVolumeMenuItem.Create(AWindow: TCastleWindowBase; Menu: TCastleMenu;
  ASoundEngine: TXmlSoundEngine);
begin
  FSlider := TCastleMenuVolumeSlider.Create(ASoundEngine.Volume);
  inherited;
end;

function TGLSoundVolumeMenuItem.Title: string;
begin
  Result := 'Volume';
end;

function TGLSoundVolumeMenuItem.Accessory: TMenuAccessory;
begin
  Result := FSlider;
end;

procedure TGLSoundVolumeMenuItem.AccessoryValueChanged;
begin
  SoundEngine.Volume := Slider.Value;
end;

procedure TGLSoundVolumeMenuItem.RefreshAccessory;
begin
  Slider.Value := SoundEngine.Volume;
end;

{ TGLMusicVolumeMenuItem ----------------------------------------------------- }

constructor TGLMusicVolumeMenuItem.Create(AWindow: TCastleWindowBase; Menu: TCastleMenu;
  ASoundEngine: TXmlSoundEngine);
begin
  FSlider := TCastleMenuVolumeSlider.Create(ASoundEngine.MusicPlayer.MusicVolume);
  inherited;
end;

function TGLMusicVolumeMenuItem.Title: string;
begin
  Result := 'Music volume';
end;

function TGLMusicVolumeMenuItem.Accessory: TMenuAccessory;
begin
  Result := FSlider;
end;

procedure TGLMusicVolumeMenuItem.AccessoryValueChanged;
begin
  SoundEngine.MusicPlayer.MusicVolume := Slider.Value;
end;

procedure TGLMusicVolumeMenuItem.RefreshAccessory;
begin
  Slider.Value := SoundEngine.MusicPlayer.MusicVolume;
end;

end.
