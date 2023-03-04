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

{ Game initialization. }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleScene, CastleControls, CastleLog, CastleWindow,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties,
  CastleGameNotifications, CastleVectors, CastleSoundEngine,
  CastleTransform, CastleConfig,
  GameSound, GameConfiguration, GameCreatures, GameLocations
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewIntro
  , GameViewMainMenu
  , GameViewPlay
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

{ load / save user config ---------------------------------------------------- }

procedure LoadUserConfig;
begin
  UserConfig.Load;
  SoundEngine.Volume := UserConfig.GetFloat('sound/volume', TSoundEngine.DefaultVolume);
  SoundEngine.LoopingChannel[0].Volume := UserConfig.GetFloat('sound/music/volume', TLoopingChannel.DefaultVolume);
end;

procedure SaveUserConfig;
begin
  UserConfig.SetDeleteFloat('sound/volume',
    SoundEngine.Volume, TSoundEngine.DefaultVolume);
  UserConfig.SetDeleteFloat('sound/music/volume',
    SoundEngine.LoopingChannel[0].Volume, TLoopingChannel.DefaultVolume);
  UserConfig.Save;
end;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');
  Window.Container.BackgroundColor := Black;

  { configure Notifications }
  Notifications.MaxMessages := 4;
  Notifications.Color := Vector4(0.8, 0.8, 0.8, 1.0);

  LoadUserConfig;
  InitializeSound;

  { load game configuration }
  GameConfig := TCastleConfig.Create(nil);
  GameConfig.URL := 'castle-data:/game.xml';
  CreatureKinds := TCreatureKindList.Create;
  Locations := TLocationList.Create;

  { Create views (see https://castle-engine.io/views ). }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewIntro := TViewIntro.Create(Application);
  ViewMainMenu := TViewMainMenu.Create(Application);
  ViewPlay := TViewPlay.Create(Application);
  {$endregion 'Castle View Creation'}

  Window.Container.View := ViewIntro;
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }
  Window.FpsShowOnCaption := true;
  Window.FullScreen := true;

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
finalization
  SaveUserConfig;
end.
