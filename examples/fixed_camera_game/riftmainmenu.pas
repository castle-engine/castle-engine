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
    OpenALDeviceArgument: TCastleMenuButton;
    constructor Create(AOwner: TComponent); override;
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

  OpenALDeviceArgument := TCastleMenuButton.Create(Self);
  OpenALDeviceArgument.Caption := SoundEngine.DeviceNiceName;
  OpenALDeviceArgument.OnClick := @ClickChangeDevice;

  Add(TSoundInfoMenuItem.Create(Self));
  Add(TSoundVolumeMenuItem.Create(Self));
  Add(TMusicVolumeMenuItem.Create(Self));
  Add('Sound output device', OpenALDeviceArgument);
  Add('Back to main menu', @ClickBack);

  SubMenuTitle := 'Sound options';
end;

procedure TRiftSoundMenu.ClickChangeDevice(Sender: TObject);
begin
  SetCurrentMenu(ChangeOpenALDeviceMenu);
end;

procedure TRiftSoundMenu.ClickBack(Sender: TObject);
begin
  SetCurrentMenu(MainMenu);
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
    SoundMenu.OpenALDeviceArgument.Caption := SoundEngine.Devices[CurrentItem].NiceName;
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

  GLMenuBg := TGLImage.Create(DataConfig.GetURL('main_menu/image'),
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
