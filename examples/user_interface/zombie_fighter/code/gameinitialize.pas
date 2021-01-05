{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Initialize the game window and states. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes, CastleControls, CastleUtils, CastleFilesUtils,
  CastleColors, CastleUIControls, CastleUIState, CastleWindow,
  CastleApplicationProperties,
  GameStateMainMenu, GameStateLoading, GameStatePlay, GameStateAskDialog;

var
  Window: TCastleWindowBase;

procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { create all the states }
  StateMainMenu := TStateMainMenu.Create(Application);
  StateLoading := TStateLoading.Create(Application);
  StatePlay := TStatePlay.Create(Application);
  StateAskDialog := TStateAskDialog.Create(Application);

  { initialize first state }
  TUIState.Current := StateMainMenu;
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'zombie_fighter';

  Window := TCastleWindowBase.Create(Application);

  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.
