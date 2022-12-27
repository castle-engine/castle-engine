{
  Copyright 2019-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleWindow, CastleControls, CastleLog, CastleSoundEngine,
  CastleFilesUtils, CastleKeysMouse, CastleColors, CastleTimeUtils,
  CastleUIControls, CastleApplicationProperties
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewMain
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Configure sound stuff }
  // Default is 16, but 8 is easier to display and test.
  SoundEngine.MaxAllocatedSources := 8;
  // Get infotmation in log when each sound is loaded.
  SoundEngine.LogSoundLoading := true;

  { Create game views and set initial view }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewMain := TViewMain.Create(Application);
  {$endregion 'Castle View Creation'}

  Window.Container.View := ViewMain;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  // Measure sound loading time.
  // The profile is automatically output to log at the end of ApplicationInitialize
  // (you could also output it explicitly by "WritelnLog(Profiler.Summary);",
  // and you can measure time explicitly by "Profiler" or "Timer" or "ProcessTimer".
  Profiler.Enabled := true;
end.
