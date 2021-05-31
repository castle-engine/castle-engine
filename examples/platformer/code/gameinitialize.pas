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
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleUIState, CastleImages,
  CastleVectors
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameStateMenu
  , GameStatePlay
  , GameStateGameOver
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindowBase;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Adjust theme }
  Theme.TextColor := White;
  Theme.ImagesPersistent[tiButtonNormal].Url := 'castle-data:/ui/red_button06.png';
  Theme.ImagesPersistent[tiButtonNormal].ProtectedSides.AllSides := 6;
  Theme.ImagesPersistent[tiButtonFocused].Url := 'castle-data:/ui/red_button08.png';
  Theme.ImagesPersistent[tiButtonFocused].ProtectedSides.AllSides := 6;
  Theme.ImagesPersistent[tiButtonPressed].Url := 'castle-data:/ui/red_button07.png';
  Theme.ImagesPersistent[tiButtonPressed].ProtectedSides.AllSides := 6;

  { Create game states and set initial state }
  {$region 'Castle State Creation'}
  // The content here may be automatically updated by CGE editor.
  StatePlay := TStatePlay.Create(Application);
  StateMenu := TStateMenu.Create(Application);
  StateGameOver := TStateGameOver.Create(Application);
  {$endregion 'Castle State Creation'}

  TUIState.Current := StateMenu;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.
