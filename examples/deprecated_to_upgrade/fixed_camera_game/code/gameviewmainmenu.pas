{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View displaying main on-screen menu (TViewMainMenu). }
unit GameViewMainMenu;

interface

uses Classes,
  CastleUIControls, CastleOnScreenMenu, CastleControls, CastleKeysMouse;

type
  { View displaying main on-screen menu. }
  TViewMainMenu = class(TCastleView)
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
        SoundDeviceArgument: TCastleOnScreenMenuItem;
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
    procedure Stop; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  ViewMainMenu: TViewMainMenu;

implementation

uses SysUtils,
  CastleFilesUtils, CastleMessages, CastleWindow, CastleUtils,
  CastleVectors, CastleSoundMenu, CastleStringUtils,
  CastleGLImages, CastleColors, CastleSoundEngine,
  CastleApplicationProperties, CastleRectangles,
  GameConfiguration, GameSound, GameViewPlay, GameLocations, GameViewIntro;

{ TViewMainMenu.TAbstractMenu ------------------------------------------------------------------ }

constructor TViewMainMenu.TAbstractMenu.Create(AOwner: TComponent);
begin
  inherited;
  CurrentItemBorderColor1 := Black;
  CurrentItemBorderColor2 := Vector4(186/255, 134/255,  88/255, 1.0);
  CurrentItemColor        := Vector4(252/255, 253/255, 200/255, 1.0);
  NonCurrentItemColor     := CurrentItemBorderColor2;

  Anchor(hpRight, -100);
  Anchor(vpTop, -100);

  DrawBackgroundRectangle := false;
  CaptureAllEvents := true;
  DrawFocusedBorder := false;

  { Assign TCastleSound instances loaded in GameSound unit. }
  SoundClick := AllSounds.SoundMenuClick;
  SoundCurrentItemChanged := AllSounds.SoundMenuCurrentItemChanged;
end;

{ TViewMainMenu.TRiftMainMenu -------------------------------------------------------------- }

constructor TViewMainMenu.TRiftMainMenu.Create(AOwner: TComponent);
begin
  inherited;
  Add('New Game', {$ifdef FPC}@{$endif} ClickNewGame);
  Add('Replay Intro', {$ifdef FPC}@{$endif} ClickIntro);
  Add('Sound Options', {$ifdef FPC}@{$endif} ClickSoundOptions);
  { on mobile, do not show quit -- users don't expect it,
    and also Application.Terminate cannot be used on iOS and Android. }
  if not ApplicationProperties.TouchDevice then
    Add('Quit', {$ifdef FPC}@{$endif} ClickQuit);
end;

procedure TViewMainMenu.TRiftMainMenu.ClickIntro(Sender: TObject);
begin
  Container.View := ViewIntro;
end;

procedure TViewMainMenu.TRiftMainMenu.ClickNewGame(Sender: TObject);
begin
  Container.View := ViewPlay;
end;

procedure TViewMainMenu.TRiftMainMenu.ClickSoundOptions(Sender: TObject);
begin
  ViewMainMenu.SetCurrentMenu(ViewMainMenu.SoundMenu);
end;

procedure TViewMainMenu.TRiftMainMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

{ TViewMainMenu.TAbstractSubMenu --------------------------------------------------------------- }

constructor TViewMainMenu.TAbstractSubMenu.Create(AOwner: TComponent);
begin
  inherited;
end;

{ TViewMainMenu.TRiftSoundMenu ------------------------------------------------------------- }

constructor TViewMainMenu.TRiftSoundMenu.Create(AOwner: TComponent);
begin
  inherited;

  SoundDeviceArgument := TCastleOnScreenMenuItem.Create(Self);
  SoundDeviceArgument.Caption := 'Sound output device';
  SoundDeviceArgument.RightCaption := SoundEngine.DeviceCaption;
  SoundDeviceArgument.OnClick := {$ifdef FPC}@{$endif} ClickChangeDevice;

  Add('Sound options:');
  Add(TSoundInfoMenuItem.Create(Self));
  Add(TSoundVolumeMenuItem.Create(Self));
  Add(TMusicVolumeMenuItem.Create(Self));
  Add(SoundDeviceArgument);
  Add('Back to main menu', {$ifdef FPC}@{$endif} ClickBack);

  // select item 1 as default, because item 0 is the label
  CurrentItem := 1;
end;

procedure TViewMainMenu.TRiftSoundMenu.ClickChangeDevice(Sender: TObject);
begin
  ViewMainMenu.SetCurrentMenu(ViewMainMenu.SoundDeviceMenu);
end;

procedure TViewMainMenu.TRiftSoundMenu.ClickBack(Sender: TObject);
begin
  ViewMainMenu.SetCurrentMenu(ViewMainMenu.MainMenu);
end;

{ TSoundDeviceMenuButton ---------------------------------------------------- }

type
  TSoundDeviceMenuButton = class(TCastleOnScreenMenuItem)
  public
    Device: TSoundDevice;
    procedure DoClick; override;
  end;

procedure TSoundDeviceMenuButton.DoClick;
begin
  inherited;

  SoundEngine.Device := Device.Name;
  ViewMainMenu.SoundMenu.SoundDeviceArgument.RightCaption := SoundEngine.DeviceCaption;
  if not SoundEngine.IsContextOpenSuccess then
    MessageOK(Application.MainWindow, SoundEngine.Information);

  ViewMainMenu.SetCurrentMenu(ViewMainMenu.SoundMenu);
end;

{ TViewMainMenu.TSoundDeviceMenu ---------------------------------------------------- }

constructor TViewMainMenu.TSoundDeviceMenu.Create(AOwner: TComponent);
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
    D.Caption := D.Device.Caption;
    Add(D);
  end;
  Add('Cancel', {$ifdef FPC}@{$endif} ClickBack);

  // select item 1 as default, because item 0 is the label
  CurrentItem := 1;
end;

procedure TViewMainMenu.TSoundDeviceMenu.ClickBack(Sender: TObject);
begin
  ViewMainMenu.SetCurrentMenu(ViewMainMenu.SoundMenu);
end;

{ TViewMainMenu ------------------------------------------------------------- }

procedure TViewMainMenu.SetCurrentMenu(const NewValue: TAbstractMenu);
begin
  if CurrentMenu <> nil then
    RemoveControl(CurrentMenu);
  CurrentMenu := NewValue;
  InsertFront(CurrentMenu);
end;

function TViewMainMenu.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  if Event.IsKey(CharEscape) then
  begin
    SetCurrentMenu(MainMenu);
    Result := true;
  end;
end;

procedure TViewMainMenu.Start;
begin
  inherited;

  SoundEngine.LoopingChannel[0].Sound := AllSounds.SoundMainMenuMusic;

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

procedure TViewMainMenu.Stop;
begin
  { The menu instance will be freed because it's owned by FreeAtStop.

    We should still set CurrentMenu back to nil,
    otherwise starting view again would have CurrentMenu <> nil.
    It may lead to subtle bugs: in case CurrentMenu is (by accident)
    equal to MenuBg in Start, then "SetCurrentMenu(MainMenu)"
    will free the MenuBg. }
  CurrentMenu := nil;
  inherited;
end;

end.
