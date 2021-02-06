{
  Copyright 2013-2020 Michalis Kamburelis.

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

uses SysUtils, Classes,
  CastleWindow, CastleUIControls, CastleRectangles,
  CastleLog, CastleSceneCore, CastleStringUtils, CastleTimeUtils,
  CastleApplicationProperties, CastleUIState, CastleProgress, CastleWindowProgress,
  GameStateMain;

var
  Window: TCastleWindowBase;

{ One-time initialization. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create TStateMain that will handle "main" state of the game.
    Larger games may use multiple states,
    e.g. TStateMainMenu ("main menu state"),
    TStatePlay ("playing the game state"),
    TStateCredits ("showing the credits state") etc. }
  StateMain := TStateMain.Create(Application);
  TUIState.Current := StateMain;

  FrameProfiler.Enabled := true;
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'simple_3d_demo';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization.

    For programs, InitializeLog is not called here.
    Instead InitializeLog is done by the program main file,
    after command-line parameters are parsed. }
  if IsLibrary then
    InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  Progress.UserInterface := WindowProgressInterface;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Window.Caption := 'Cross-platform CGE test';
  Application.MainWindow := Window;
end.
