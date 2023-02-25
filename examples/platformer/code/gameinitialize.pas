{
  Copyright 2021-2021 Andrzej Kilijański, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file. }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleApplicationProperties, CastleColors, CastleConfig, CastleControls,
  CastleImages, CastleFilesUtils, CastleKeysMouse, CastleLog, CastleScene,
  CastleSceneCore, CastleSoundEngine, CastleUIControls,
  CastleVectors, CastleWindow
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewMenu
  , GameViewPlay
  , GameViewGameOver
  , GameViewLevelComplete
  , GameViewPause
  , GameViewOptions
  , GameViewCredits
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Load settings }
  UserConfig.Load;

  { Set up sound repository }
  SoundEngine.RepositoryURL := 'castle-data:/audio/index.xml';

  SoundEngine.Volume := UserConfig.GetFloat('volume', 1.0);
  SoundEngine.LoopingChannel[0].Volume := UserConfig.GetFloat('music', 1.0);

  { Adjust theme }
  Theme.TextColor := White;
  Theme.ImagesPersistent[tiButtonNormal].Url := 'castle-data:/ui/red_button06.png';
  Theme.ImagesPersistent[tiButtonNormal].ProtectedSides.AllSides := 6;
  Theme.ImagesPersistent[tiButtonFocused].Url := 'castle-data:/ui/red_button08.png';
  Theme.ImagesPersistent[tiButtonFocused].ProtectedSides.AllSides := 6;
  Theme.ImagesPersistent[tiButtonPressed].Url := 'castle-data:/ui/red_button07.png';
  Theme.ImagesPersistent[tiButtonPressed].ProtectedSides.AllSides := 6;

  { Create views (see https://castle-engine.io/views ). }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewPlay := TViewPlay.Create(Application);
  ViewMenu := TViewMenu.Create(Application);
  ViewGameOver := TViewGameOver.Create(Application);
  ViewLevelComplete := TViewLevelComplete.Create(Application);
  ViewPause := TViewPause.Create(Application);
  ViewOptions := TViewOptions.Create(Application);
  ViewCredits := TViewCredits.Create(Application);
  {$endregion 'Castle View Creation'}

  Window.Container.View := ViewMenu;
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

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
