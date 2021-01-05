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
  CastleApplicationProperties, CastleLog,
  GameStateMainMenu, GameStateLoading, GameStatePlay, GameStateAskDialog;

var
  Window: TCastleWindowBase;

procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { initialize first state }
  TUIState.Current := TStateMainMenu.CreateUntilStopped;
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'zombie_fighter';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization.

    For programs, InitializeLog is not called here.
    Instead InitializeLog is done by the program main file,
    after command-line parameters are parsed. }
  if IsLibrary then
    InitializeLog;

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
finalization
  TUIState.Current := nil;
end.
