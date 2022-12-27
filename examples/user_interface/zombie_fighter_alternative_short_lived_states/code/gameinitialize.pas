{
  Copyright 2016-2022 Michalis Kamburelis.

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
  CastleColors, CastleUIControls, CastleWindow,
  CastleApplicationProperties, CastleLog,
  GameViewMainMenu, GameViewLoading, GameViewPlay, GameViewAskDialog;

var
  Window: TCastleWindow;

procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { initialize first view }
  Window.Container.View := TViewMainMenu.CreateUntilStopped;
end;

initialization
  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
finalization
  Window.Container.View := nil;
end.
