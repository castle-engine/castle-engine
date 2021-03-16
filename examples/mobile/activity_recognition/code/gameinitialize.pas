{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file. }
unit GameInitialize;

interface

implementation

uses CastleWindow, CastleLog, CastleApplicationProperties, CastleUIState,
  GameStateMain;

var
  Window: TCastleWindowBase;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  StateMain := TStateMain.Create(Application);
  TUIState.Current := StateMain;
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'activity_recognition';

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
  Application.MainWindow := Window;

  { On desktops, change the initial window size to simulate a tall screen,
    like mobile in a "portrait" orientation. }
  {$if not (defined(CASTLE_IOS) or defined(ANDROID) or defined(CASTLE_NINTENDO_SWITCH))}
  Window.Height := Application.ScreenHeight * 5 div 6;
  Window.Width := Window.Height * 900 div 1600;
  {$endif}

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.
