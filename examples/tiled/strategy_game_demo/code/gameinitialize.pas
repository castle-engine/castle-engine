{
  Copyright 2018-2022 Michalis Kamburelis.

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
  CastleUIControls, CastleApplicationProperties
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewMainMenu
  , GameViewPlay
  , GameViewInstructions
  , GameViewInstructions2
  , GameViewWin
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

{ routines ------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings,
    for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewMainMenu := TViewMainMenu.Create(Application);
  ViewPlay := TViewPlay.Create(Application);
  ViewInstructions := TViewInstructions.Create(Application);
  ViewInstructions2 := TViewInstructions2.Create(Application);
  ViewWin := TViewWin.Create(Application);
  {$endregion 'Castle View Creation'}

  Window.Container.View := ViewMainMenu;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
end.
