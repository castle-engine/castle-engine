{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ State displaying main on-screen menu (TStateMainMenu). }
unit GameStateMainMenu;

interface

uses Classes,
  CastleUIState, CastleOnScreenMenu, CastleControls, CastleKeysMouse;

type
  { State displaying main on-screen menu. }
  TStateMainMenu = class(TUIState)
  private
    type
      TAbstractMenu = class(TCastleOnScreenMenu)
      public
        constructor Create(AOwner: TComponent); override;
      end;

      TRiftMainMenu = class(TAbstractMenu)
      strict private
        procedure ClickIntro(Sender: TObject);
        procedure ClickNewGame(Sender: TObject);
        procedure ClickSoundOptions(Sender: TObject);
        procedure ClickQuit(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); override;
      end;

      TAbstractSubMenu = class(TAbstractMenu)
      public
        constructor Create(AOwner: TComponent); override;
      end;

      TRiftSoundMenu = class(TAbstractSubMenu)
      strict private
        procedure ClickChangeDevice(Sender: TObject);
        procedure ClickBack(Sender: TObject);
      public
        SoundDeviceArgument: TCastleMenuButton;
        constructor Create(AOwner: TComponent); override;
      end;

      TSoundDeviceMenu = class(TAbstractSubMenu)
      strict private
        procedure ClickBack(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); override;
      end;

    var
      MainMenu: TRiftMainMenu;
      SoundMenu: TRiftSoundMenu;
      SoundDeviceMenu: TSoundDeviceMenu;
      CurrentMenu: TAbstractMenu;
      MenuBg: TCastleImageControl;
    procedure SetCurrentMenu(const NewValue: TAbstractMenu);
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses SysUtils,
  CastleFilesUtils, CastleMessages, CastleWindow, CastleUtils,
  CastleVectors, CastleSoundMenu, CastleStringUtils,
  CastleGLImages, CastleUIControls, CastleColors, CastleSoundEngine,
  CastleApplicationProperties, CastleRectangles,
  GameConfiguration, GameSound, GameStatePlay, GameLocations, GameStateIntro;

{ TStateMainMenu.TAbstractMenu ------------------------------------------------------------------ }

constructor TStateMainMenu.TAbstractMenu.Create(AOwner: TComponent);
begin
  inherited;
  CurrentItemBorderColor1 := Black;
  CurrentItemBorderColor2 := Vector4(186/255, 134/255,  88/255, 1.0);
  CurrentItemColor        := Vector4(252/255, 253/255, 200/255, 1.0);
  NonCurrentItemColor     := CurrentItemBorderColor2;

  Anchor(hpRight, -100);
  Anchor(vpTop, -100);

  DrawBackgroundRectangle := false;
  ExclusiveEvents := false;
  CaptureAllEvents := true;
  { Since we always capture clicks on the entire screen,
    no point in visualizing focused. }
  DrawFocusedBorder := false;
end;

{ TStateMainMenu.TRiftMainMenu -------------------------------------------------------------- }

constructor TStateMainMenu.TRiftMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  Add('New Game', @ClickNewGame);
  Add('Replay Intro', @ClickIntro);
  Add('Sound Options', @ClickSoundOptions);
  { on mobile, do not show quit -- users don't expect it,
    and also Application.Terminate cannot be used on iOS and Android. }
  if not ApplicationProperties.TouchDevice then
    Add('Quit', @ClickQuit);
end;

procedure TStateMainMenu.TRiftMainMenu.ClickIntro(Sender: TObject);
begin
  TUIState.Current := StateIntro;
end;

procedure TStateMainMenu.TRiftMainMenu.ClickNewGame(Sender: TObject);
begin
  TUIState.Current := StatePlay;
end;

procedure TStateMainMenu.TRiftMainMenu.ClickSoundOptions(Sender: TObject);
begin
  StateMainMenu.SetCurrentMenu(StateMainMenu.SoundMenu);
end;

procedure TStateMainMenu.TRiftMainMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

{ TStateMainMenu.TAbstractSubMenu --------------------------------------------------------------- }

constructor TStateMainMenu.TAbstractSubMenu.Create(AOwner: TComponent);
begin
  inherited;
end;

{ TStateMainMenu.TRiftSoundMenu ------------------------------------------------------------- }

constructor TStateMainMenu.TRiftSoundMenu.Create(AOwner: TComponent);
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

procedure TStateMainMenu.TRiftSoundMenu.ClickChangeDevice(Sender: TObject);
begin
  StateMainMenu.SetCurrentMenu(StateMainMenu.SoundDeviceMenu);
end;

procedure TStateMainMenu.TRiftSoundMenu.ClickBack(Sender: TObject);
begin
  StateMainMenu.SetCurrentMenu(StateMainMenu.MainMenu);
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
  StateMainMenu.SoundMenu.SoundDeviceArgument.Caption := SoundEngine.DeviceCaption;
  if not SoundEngine.ALActive then
    MessageOK(Application.MainWindow, SoundEngine.Information);

  StateMainMenu.SetCurrentMenu(StateMainMenu.SoundMenu);
end;

{ TStateMainMenu.TSoundDeviceMenu ---------------------------------------------------- }

constructor TStateMainMenu.TSoundDeviceMenu.Create(AOwner: TComponent);
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

procedure TStateMainMenu.TSoundDeviceMenu.ClickBack(Sender: TObject);
begin
  StateMainMenu.SetCurrentMenu(StateMainMenu.SoundMenu);
end;

{ TStateMainMenu ------------------------------------------------------------- }

procedure TStateMainMenu.SetCurrentMenu(const NewValue: TAbstractMenu);
begin
  if CurrentMenu <> nil then
    RemoveControl(CurrentMenu);
  CurrentMenu := NewValue;
  InsertFront(CurrentMenu);
end;

function TStateMainMenu.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  if Event.IsKey(CharEscape) then
  begin
    SetCurrentMenu(MainMenu);
    Result := true;
  end;
end;

procedure TStateMainMenu.Start;
begin
  inherited;

  SoundEngine.MusicPlayer.Sound := stMainMenuMusic;

  MenuBg := TCastleImageControl.Create(FreeAtStop);
  MenuBg.URL := GameConfig.GetURL('main_menu/image');
  MenuBg.FullSize := true;
  MenuBg.Stretch := true;
  InsertBack(MenuBg);

  MainMenu := TRiftMainMenu.Create(FreeAtStop);
  SoundMenu := TRiftSoundMenu.Create(FreeAtStop);
  SoundDeviceMenu := TSoundDeviceMenu.Create(FreeAtStop);

  SetCurrentMenu(MainMenu);
end;

end.
