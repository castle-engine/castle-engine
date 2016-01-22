{
  Copyright 2007-2014 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftMainMenu;

interface

uses RiftWindow;

var
  DebugMenuDesignerAllowed: boolean = false;

procedure DoMainMenu;

implementation

uses SysUtils, CastleWindow, CastleFilesUtils,
  CastleGLUtils, CastleMessages, CastleOnScreenMenu, CastleWindowModes, CastleUtils,
  CastleVectors, CastleSoundMenu, Classes, CastleStringUtils, CastleControls,
  CastleGLImages, CastleUIControls, CastleColors, CastleSoundEngine,
  CastleApplicationProperties,
  RiftData, RiftSound, RiftVideoOptions, RiftInspectCreatures, RiftPlay,
  RiftLocations, RiftGame, CastleKeysMouse, CastleRectangles;

{ menu classes and variables  ------------------------------------------------ }

type
  TRiftMenu = class(TCastleOnScreenMenu)
  public
    constructor Create(AOwner: TComponent); override;
    { Let rift callbacks handle the events too. }
    property ExclusiveEvents default false;
    { Since Rect is always fullsize, no point in visualizing focused. }
    property DrawFocusedBorder default false;
    property CaptureAllEvents default true;
  end;

  TRiftSubMenu = class(TRiftMenu)
  public
    SubMenuTitle: string;
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
  end;

  TRiftMainMenu = class(TRiftMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TRiftSoundMenu = class(TRiftSubMenu)
  public
    SoundInfo: TSoundInfoMenuItem;
    SoundVolume: TSoundVolumeMenuItem;
    MusicVolume: TMusicVolumeMenuItem;

    OpenALDeviceArgument: TCastleLabel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end;

  TChangeOpenALDeviceMenu = class(TRiftSubMenu)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end;

var
  MainMenu: TRiftMainMenu;
  SoundMenu: TRiftSoundMenu;
  ChangeOpenALDeviceMenu: TChangeOpenALDeviceMenu;

{ Actual menu procedures and CastleWindow callbacks ------------------------------ }

var
  GLMenuBg: TGLImage;
  UserQuit: boolean;

{ Sets CurrentMenu, taking care of adding this menu / removing existing menu
  (when new value is @nil) from Window.Controls. }
procedure SetCurrentMenu(const NewValue: TCastleOnScreenMenu);
begin
  Window.Controls.MakeSingle(TCastleOnScreenMenu, NewValue);
end;

procedure Resize(Container: TUIContainer);
begin
  OrthoProjection(0, Window.Width, 0, Window.Height);
end;

procedure Render(Container: TUIContainer);
begin
  GLMenuBg.Draw(0, 0);
end;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(CharEscape) then
    SetCurrentMenu(MainMenu);
end;

procedure CloseQuery(Container: TUIContainer);
begin
  UserQuit := true;
end;

procedure DoMainMenu;
var
  SavedMode: TGLMode;
begin
  SoundEngine.MusicPlayer.Sound := stMainMenuMusic;
  SavedMode := TGLMode.CreateReset(Window, @Render, @Resize, @CloseQuery);
  try
    Window.FpsShowOnCaption := DebugMenuFps;
    Window.AutoRedisplay := true;
    Window.OnPress := @Press;
    { actually we draw in 2D, but it's the current projection anyway }
    Window.RenderStyle := rs3D;

    SetCurrentMenu(MainMenu);

    UserQuit := false;
    repeat
      Application.ProcessMessage(true, true);
    until UserQuit;
  finally FreeAndNil(SavedMode); end;
end;

{ TRiftMenu ------------------------------------------------------------------ }

constructor TRiftMenu.Create(AOwner: TComponent);
begin
  inherited;
  CurrentItemBorderColor1 := Black;
  CurrentItemBorderColor2 := Vector4Single(186/255, 134/255,  88/255, 1.0);
  CurrentItemColor        := Vector4Single(252/255, 253/255, 200/255, 1.0);
  NonCurrentItemColor     := CurrentItemBorderColor2;

  Anchor(hpRight, -91);
  Anchor(vpTop, -62);

  DrawBackgroundRectangle := false;
  ExclusiveEvents := false;
  DrawFocusedBorder := false;
  CaptureAllEvents := true;
end;

{ TRiftMainMenu -------------------------------------------------------------- }

constructor TRiftMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  Add('New game');
  Add('Sound options');
  { TODO: this should be more hidden from user, in some debug menu }
  Add('Debug: inspect creatures');
  Add('Quit');
end;

procedure TRiftMainMenu.Click;

  procedure NewGame;
  begin
    CurrentLocation := StartLocation;
    WorldTime := 0;
    Play;
  end;

begin
  inherited;
  case CurrentItem of
    0: NewGame;
    1: SetCurrentMenu(SoundMenu);
    2: InspectCreatures;
    3: UserQuit := true;
    else raise EInternalError.Create('TRiftMainMenu.Click');
  end;
end;

{ TRiftSubMenu --------------------------------------------------------------- }

constructor TRiftSubMenu.Create(AOwner: TComponent);
begin
  inherited;

  Anchor(hpLeft, 54);
  Anchor(vpTop, -273);

  DrawBackgroundRectangle := true;
end;

procedure TRiftSubMenu.Render;
var
  SR: TRectangle;
const
  SubMenuTextColor: TCastleColor = (0.7, 0.7, 0.7, 1.0);
begin
  { background of submenu is mainmenu }
  MainMenu.Render;
  inherited;
  SR := ScreenRect;
  UIFont.Print(SR.Left, SR.Top - 20, SubMenuTextColor, SubMenuTitle + ' :');
end;

{ TRiftSoundMenu ------------------------------------------------------------- }

constructor TRiftSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  OpenALDeviceArgument := TCastleLabel.Create(Self);
  OpenALDeviceArgument.Text.Text := SoundEngine.DeviceNiceName;

  SoundInfo := TSoundInfoMenuItem.Create(Window, Self);
  SoundVolume := TSoundVolumeMenuItem.Create(Window, Self);
  MusicVolume := TMusicVolumeMenuItem.Create(Window, Self);
  Add('Sound output device', OpenALDeviceArgument);
  Add('Back to main menu');

  SubMenuTitle := 'Sound options';
end;

destructor TRiftSoundMenu.Destroy;
begin
  FreeAndNil(SoundInfo);
  FreeAndNil(SoundVolume);
  FreeAndNil(MusicVolume);
  inherited;
end;

procedure TRiftSoundMenu.Click;
begin
  inherited;

  case CurrentItem of
    0: SoundInfo.Selected;
    1: ;
    2: ;
    3: SetCurrentMenu(ChangeOpenALDeviceMenu);
    4: SetCurrentMenu(MainMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TChangeOpenALDeviceMenu ---------------------------------------------------- }

constructor TChangeOpenALDeviceMenu.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  for I := 0 to SoundEngine.Devices.Count - 1 do
    Add(SoundEngine.Devices[I].NiceName);
  Add('Cancel');

  SubMenuTitle := 'Change sound output device';
end;

destructor TChangeOpenALDeviceMenu.Destroy;
begin
  inherited;
end;

procedure TChangeOpenALDeviceMenu.Click;
begin
  inherited;

  if CurrentItem < SoundEngine.Devices.Count then
  begin
    SoundEngine.Device := SoundEngine.Devices[CurrentItem].Name;
    { ALCDevice value changed now to new value. }
    SoundMenu.OpenALDeviceArgument.Text.Text := SoundEngine.Devices[CurrentItem].NiceName;
    if not SoundEngine.ALActive then
      MessageOK(Window, SoundEngine.SoundInitializationReport);
  end;

  SetCurrentMenu(SoundMenu);
end;

{ initialization / finalization ---------------------------------------------- }

procedure ContextOpen;
begin
  MainMenu := TRiftMainMenu.Create(nil);
  SoundMenu := TRiftSoundMenu.Create(nil);
  ChangeOpenALDeviceMenu := TChangeOpenALDeviceMenu.Create(nil);

  GLMenuBg := TGLImage.Create(DataURLFromConfig(
    DataConfig.GetValue('main_menu/image', 'required_xml_value_missing')),
    [], Window.Width, Window.Height);
end;

procedure ContextClose;
begin
  FreeAndNil(MainMenu);
  FreeAndNil(SoundMenu);
  FreeAndNil(ChangeOpenALDeviceMenu);
  FreeAndNil(GLMenuBg);
end;

initialization
  ApplicationProperties.OnGLContextOpen.Add(@ContextOpen);
  ApplicationProperties.OnGLContextClose.Add(@ContextClose);
end.
