{
  Copyright 2007-2013 Michalis Kamburelis.

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
  RiftData, RiftSound, RiftVideoOptions, RiftInspectCreatures, RiftPlay,
  RiftLocations, RiftGame, CastleKeysMouse;

{ menu classes and variables  ------------------------------------------------ }

type
  TRiftMenu = class(TCastleOnScreenMenu)
  public
    constructor Create(AOwner: TComponent); override;
    { Let rift callbacks handle the events too. }
    property ExclusiveEvents default false;
    { Always treat like inside --- the menu is the only thing with
      which user interacts. }
    function PositionInside(const X, Y: Integer): boolean; override;
    { Since PositionInside is always @true, no point in visualizing focused. }
    property DrawFocusedBorder default false;
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

    OpenALDeviceArgument: TMenuArgument;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
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
  CurrentMenu: TCastleOnScreenMenu;
  GLMenuBg: TGLImage;
  UserQuit: boolean;

{ Sets CurrentMenu, taking care of adding this menu / removing existing menu
  (when new value is @nil) from Window.Controls. }
procedure SetCurrentMenu(const NewValue: TCastleOnScreenMenu);
begin
  CurrentMenu := NewValue;
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

  if DebugMenuDesignerAllowed and Event.IsKey(K_F12) then
  begin
    CurrentMenu.DesignerMode := not CurrentMenu.DesignerMode;
  end;
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
  PositionRelativeScreenX := prHigh;
  PositionRelativeScreenY := prHigh;
  PositionRelativeMenuX := prHigh;
  PositionRelativeMenuY := prHigh;
  Position := Vector2Integer(-91, -62);
  DrawBackgroundRectangle := false;
  ExclusiveEvents := false;
  DrawFocusedBorder := false;
end;

function TRiftMenu.PositionInside(const X, Y: Integer): boolean;
begin
  Result := true;
end;

{ TRiftMainMenu -------------------------------------------------------------- }

constructor TRiftMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  Items.Add('New game');
  Items.Add('Sound options');
  { TODO: this should be more hidden from user, in some debug menu }
  Items.Add('Debug: inspect creatures');
  Items.Add('Quit');
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
  Position := Vector2Integer(54, -273);
  PositionRelativeScreenX := prLow;
  PositionRelativeScreenY := prHigh;
  PositionRelativeMenuX := prLow;
  PositionRelativeMenuY := prHigh;
  DrawBackgroundRectangle := true;
end;

procedure TRiftSubMenu.Render;
const
  SubMenuTextColor: TCastleColor = (0.7, 0.7, 0.7, 1.0);
begin
  { background of submenu is mainmenu }
  MainMenu.Render;
  inherited;
  UIFont.Print(PositionAbsolute[0],
    PositionAbsolute[1] + AllItemsRectangle.Height - 20, SubMenuTextColor,
    SubMenuTitle + ' :');
end;

{ TRiftSoundMenu ------------------------------------------------------------- }

constructor TRiftSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  OpenALDeviceArgument := TMenuArgument.Create(450);
  OpenALDeviceArgument.Value := SoundEngine.DeviceNiceName;

  SoundInfo := TSoundInfoMenuItem.Create(Window, Self);
  SoundVolume := TSoundVolumeMenuItem.Create(Window, Self);
  MusicVolume := TMusicVolumeMenuItem.Create(Window, Self);
  Items.AddObject('Sound output device', OpenALDeviceArgument);
  Items.Add('Back to main menu');

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

procedure TRiftSoundMenu.AccessoryValueChanged;
begin
  case CurrentItem of
    1: SoundVolume.AccessoryValueChanged;
    2: MusicVolume.AccessoryValueChanged;
  end;
end;

{ TChangeOpenALDeviceMenu ---------------------------------------------------- }

constructor TChangeOpenALDeviceMenu.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;

  for I := 0 to SoundEngine.Devices.Count - 1 do
    Items.Add(SoundEngine.Devices[I].NiceName);
  Items.Add('Cancel');

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
    SoundMenu.OpenALDeviceArgument.Value := SoundEngine.Devices[CurrentItem].NiceName;
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
  OnGLContextOpen.Add(@ContextOpen);
  OnGLContextClose.Add(@ContextClose);
end.
