{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic.  }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleUIState,
  GameStateMainMenu, GameStatePlay, GameStateInstructions, GameStateWin;

var
  Window: TCastleWindow;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings,
    for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Initialize TUIState instances and TUIState.Current }
  StateMainMenu := TStateMainMenu.Create(Application);
  StatePlay := TStatePlay.Create(Application);
  StateInstructions := TStateInstructions.Create(Application);
  StateWin := TStateWin.Create(Application);

  TUIState.Current := StateMainMenu;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
end.
