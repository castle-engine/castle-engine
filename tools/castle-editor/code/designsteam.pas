{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Steam integration for Castle Game Engine editor.
  See https://castle-engine.io/steam how to integrate Steam with your own games. }
unit DesignSteam;

interface

var
  // Did command-line parameters contain "--steam" option.
  SteamCommandLineParameter: Boolean = false;

const
  AppId = 2306540;

  // Steam achievements' names
  // Run any project|So it can walk. Can it dance?
  AchievementRun = 'achievement_run';
  // Encounter a compilation error|But my code is flawless!
  AchievementCompilationError = 'achievement_compilation_error';
  // Experience Access Violation (Segmentation Fault) Error|Don't panic and look at the backtrace.
  AchievementAccessViolation = 'achievement_access_violation';
  // Package any project|Ship it.
  AchievementPackage = 'achievement_package';
  // Encounter 100 compilation errors|If I had a penny for every error...
  Achievement100Errors = 'achievement_100_compilation_errors';

{ Initialize connection to Steam if command-line Parameters contain
  the "-steam" option (which should be indicated by SteamCommandLineParameter).

  Otherwise (when you run the editor without Steam)
  we don't initialize Steam integration -- this avoids restarting
  the editor through Steam (which could mean starting a different version)
  when you downloaded the CGE from https://castle-engine.io/download or itch.io.
  This way all downloads have the same editor binaries
  (we don't use conditional compilation to have Steam and non-Steam versions,
  this would complicate building) but running from outside the Steam (without
  "--steam") -- doesn't restart though Steam. }
procedure InitializeSteam;

procedure Achievement(const Name: string);

implementation

uses SysUtils,
  CastleSteam, CastleParameters, CastleLog;

var
  Steam: TCastleSteam;

procedure InitializeSteam;
begin
  if SteamCommandLineParameter then
  begin
    Steam := TCastleSteam.Create(AppId);
  end else
    WritelnLog('Steam', 'Running without Steam integration. To enable Steam, run with "--steam" command-line option.');
end;

procedure Achievement(const Name: string);
begin
  if Steam <> nil then
    Steam.SetAchievement(Name);
end;

finalization
  FreeAndNil(Steam);
end.
