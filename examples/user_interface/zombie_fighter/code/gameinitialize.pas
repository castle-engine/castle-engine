{
  Copyright 2016-2021 Michalis Kamburelis.

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

uses SysUtils, Classes, CastleControls, CastleUtils, CastleFilesUtils,
  CastleColors, CastleUIControls, CastleUIState, CastleWindow,
  CastleApplicationProperties, CastleLog
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameStateMainMenu
  , GameStateLoading
  , GameStatePlay
  , GameStateAskDialog
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create game states and set initial state }
  {$region 'Castle State Creation'}
  // The content here may be automatically updated by CGE editor.
  StateMainMenu := TStateMainMenu.Create(Application);
  StateLoading := TStateLoading.Create(Application);
  StatePlay := TStatePlay.Create(Application);
  StateAskDialog := TStateAskDialog.Create(Application);
  {$endregion 'Castle State Creation'}

  { initialize first state }
  Window.Container.View := StateMainMenu;
end;

initialization
  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
end.
