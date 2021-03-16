{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleUIState,
  GameStateMain;

var
  Window: TCastleWindowBase;

{ One-time initialization of resources. }
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
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'cge-component-gallery';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization.

    For programs, InitializeLog is not called here.
    Instead InitializeLog is done by the program main file,
    after command-line parameters are parsed. }
  if IsLibrary then
    InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Window.Caption := 'Castle Game Engine Components';
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.
