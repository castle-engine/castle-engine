{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization.
  This unit is cross-platform.
  It will be used by both standalone and mobile platform-specific code. }
unit GameInitialize;

interface

implementation

uses SysUtils,
  { FMX unit, to lock the screen orientation, see below. }
  FMX.Forms,
  CastleWindow, CastleLog, CastleUIControls
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

  { Create views (see https://castle-engine.io/views ). }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewMain := TViewMain.Create(Application);
  {$endregion 'Castle View Creation'}

  Window.Container.View := ViewMain;
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := {$ifdef FPC}@{$endif} ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { On desktops, use a window with portrait proportions,
    as this example is designed for mobile devices in portrait orientation.
    See https://castle-engine.io/window_size . }
  Window.Width := 450;
  Window.Height := 800;

  { On mobile, lock the screen orientation to portrait.

    Note that you cannot do this using Delphi "Project -> Options ->
    Application -> Orientation". Delphi IDE implements that option by adding
    a line like below to the main program file (DPR), and it fails
    (with error that "Application.CreateForm" was not found), because
    the main program file of an application using TCastleWindow
    doesn't look like a standard FMX program. So we just do it here.

    Note that "FMX.Forms.Application" is the FMX application,
    different than "Application" from the CastleWindow unit. }
  FMX.Forms.Application.FormFactor.Orientations := [TFormOrientation.Portrait];

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
