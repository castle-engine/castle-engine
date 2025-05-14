{
  Copyright 2025-2025 Michalis Kamburelis.

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
  CastleWindow, CastleLog, CastleUIControls, CastleKeysMouse, CastleMessages,
  CastleUtils, CastleInternalFileMonitor, CastleUriUtils
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewProject
  , GameViewChooseProject
  , GameViewNewProject
  , GameViewChooseExistingProject
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Use a bit more modern buttons look, from castle-model-viewer }
  Theme.ImagesPersistent[tiButtonNormal].Url := 'castle-data:/theme/ButtonNormal.png';
  Theme.ImagesPersistent[tiButtonNormal].ProtectedSides.AllSides := 2;
  Theme.ImagesPersistent[tiButtonPressed].Url := 'castle-data:/theme/ButtonPressed.png';
  Theme.ImagesPersistent[tiButtonPressed].ProtectedSides.AllSides := 2;
  Theme.ImagesPersistent[tiButtonFocused].Url := 'castle-data:/theme/ButtonFocused.png';
  Theme.ImagesPersistent[tiButtonFocused].ProtectedSides.AllSides := 2;
  Theme.ImagesPersistent[tiButtonDisabled].Url := 'castle-data:/theme/ButtonDisabled.png';
  Theme.ImagesPersistent[tiButtonDisabled].ProtectedSides.AllSides := 2;

  // we will control inspector manually by code in ViewProject
  TCastleContainer.InputInspector.Key := keyNone;
  TCastleContainer.InputInspector.PressFingers := 0;

  { Make MessageOK use TCastleView and thus work on all systems, including iOS and web. }
  MessageOKPushesView := true;

  { Inside CGE editor,
    - CastleApplicationMode is never appRunning,
    - so CastleDesignMode is always true. }
  InternalCastleApplicationMode := appDesign;

  { Inside CGE editor, file monitor is always enabled. }
  FileMonitor.MakePossiblyEnabled;
  FileMonitor.Enabled := true;

  { Set InternalCastleDesignData to enable e.g. light components to
    load gizmos. }
  InternalCastleDesignData := ResolveCastleDataUrl('castle-data:/');

  { Create views (see https://castle-engine.io/views ). }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewProject := TViewProject.Create(Application);
  ViewChooseProject := TViewChooseProject.Create(Application);
  ViewNewProject := TViewNewProject.Create(Application);
  ViewChooseExistingProject := TViewChooseExistingProject.Create(Application);
  {$endregion 'Castle View Creation'}

  Window.Container.View := ViewChooseProject;
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  Window.FullScreen := true;

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
