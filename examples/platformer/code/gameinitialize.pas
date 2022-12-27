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

  { Create game views and set initial view }
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
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.
