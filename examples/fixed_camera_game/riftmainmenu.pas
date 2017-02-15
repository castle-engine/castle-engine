{
  Copyright 2007-2017 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Display main on-screen menu. }
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
    constructor Create(AOwner: TComponent); override;
  end;

  TRiftMainMenu = class(TRiftMenu)
  strict private
    procedure ClickNewGame(Sender: TObject);
    procedure ClickSoundOptions(Sender: TObject);
    procedure ClickInspectCreatures(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TRiftSoundMenu = class(TRiftSubMenu)
  strict private
    procedure ClickChangeDevice(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    SoundDeviceArgument: TCastleMenuButton;
    constructor Create(AOwner: TComponent); override;
  end;

  TSoundDeviceMenu = class(TRiftSubMenu)
  strict private
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainMenu: TRiftMainMenu;
  SoundMenu: TRiftSoundMenu;
  SoundDeviceMenu: TSoundDeviceMenu;

{ Actual menu procedures and CastleWindow callbacks ------------------------------ }

var
  MenuBg: TCastleImageControl;
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
  SavedMode := TGLMode.CreateReset(Window, nil, @Resize, @CloseQuery);
  try
    Window.FpsShowOnCaption := DebugMenuFps;
    Window.AutoRedisplay := true;
    Window.OnPress := @Press;
    Window.Controls.InsertBack(MenuBg);

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
  Add('New game', @ClickNewGame);
  Add('Sound options', @ClickSoundOptions);
  { TODO: this should be more hidden from user, in some debug menu }
  Add('Debug: inspect creatures', @ClickInspectCreatures);
  Add('Quit', @ClickQuit);
end;

procedure TRiftMainMenu.ClickNewGame(Sender: TObject);
begin
  CurrentLocation := StartLocation;
  WorldTime := 0;
  Play;
end;

procedure TRiftMainMenu.ClickSoundOptions(Sender: TObject);
begin
  SetCurrentMenu(SoundMenu);
end;

procedure TRiftMainMenu.ClickInspectCreatures(Sender: TObject);
begin
  InspectCreatures;
end;

procedure TRiftMainMenu.ClickQuit(Sender: TObject);
begin
  UserQuit := true;
end;

{ TRiftSubMenu --------------------------------------------------------------- }

constructor TRiftSubMenu.Create(AOwner: TComponent);
begin
  inherited;

  Anchor(hpLeft, 54);
  Anchor(vpTop, -273);

  DrawBackgroundRectangle := true;
  NonFocusableItemColor := Vector4Single(0.7, 0.7, 0.7, 1.0);
end;

{ TRiftSoundMenu ------------------------------------------------------------- }

constructor TRiftSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  SoundDeviceArgument := TCastleMenuButton.Create(Self);
  SoundDeviceArgument.Caption := SoundEngine.DeviceCaption;
  SoundDeviceArgument.OnClick := @ClickChangeDevice;

  Add('Sound options:');
  Add(TSoundInfoMenuItem.Create(Self));
  Add(TSoundVolumeMenuItem.Create(Self));
  Add(TMusicVolumeMenuItem.Create(Self));
  Add('Sound output device', SoundDeviceArgument);
  Add('Back to main menu', @ClickBack);

  // select item 1 as default, because item 0 is the label
  CurrentItem := 1;
end;

procedure TRiftSoundMenu.ClickChangeDevice(Sender: TObject);
begin
  SetCurrentMenu(SoundDeviceMenu);
end;

procedure TRiftSoundMenu.ClickBack(Sender: TObject);
begin
  SetCurrentMenu(MainMenu);
end;

{ TSoundDeviceMenuButton ---------------------------------------------------- }

type
  TSoundDeviceMenuButton = class(TCastleMenuButton)
  public
    Device: TSoundDevice;
    procedure DoClick; override;
  end;

procedure TSoundDeviceMenuButton.DoClick;
begin
  inherited;

  SoundEngine.Device := Device.Name;
  SoundMenu.SoundDeviceArgument.Caption := SoundEngine.DeviceCaption;
  if not SoundEngine.ALActive then
    MessageOK(Window, SoundEngine.SoundInitializationReport);

  SetCurrentMenu(SoundMenu);
end;

{ TSoundDeviceMenu ---------------------------------------------------- }

constructor TSoundDeviceMenu.Create(AOwner: TComponent);
var
  I: Integer;
  D: TSoundDeviceMenuButton;
begin
  inherited;

  Add('Change sound output device:');
  for I := 0 to SoundEngine.Devices.Count - 1 do
  begin
    D := TSoundDeviceMenuButton.Create(Self);
    D.Device := SoundEngine.Devices[I];
    Add(D.Device.Caption, D);
  end;
  Add('Cancel', @ClickBack);

  // select item 1 as default, because item 0 is the label
  CurrentItem := 1;
end;

procedure TSoundDeviceMenu.ClickBack(Sender: TObject);
begin
  SetCurrentMenu(SoundMenu);
end;

{ initialization / finalization ---------------------------------------------- }

procedure ContextOpen;
begin
  MainMenu := TRiftMainMenu.Create(nil);
  SoundMenu := TRiftSoundMenu.Create(nil);
  SoundDeviceMenu := TSoundDeviceMenu.Create(nil);

  MenuBg := TCastleImageControl.Create(nil);
  MenuBg.URL := DataConfig.GetURL('main_menu/image');
  MenuBg.FullSize := true;
  MenuBg.Stretch := true;
end;

procedure ContextClose;
begin
  FreeAndNil(MainMenu);
  FreeAndNil(SoundMenu);
  FreeAndNil(SoundDeviceMenu);
  FreeAndNil(MenuBg);
end;

initialization
  ApplicationProperties.OnGLContextOpen.Add(@ContextOpen);
  ApplicationProperties.OnGLContextClose.Add(@ContextClose);
end.
