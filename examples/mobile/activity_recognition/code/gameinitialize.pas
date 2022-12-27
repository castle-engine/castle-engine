{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file. }
unit GameInitialize;

interface

implementation

uses CastleWindow, CastleLog, CastleApplicationProperties
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
  Application.MainWindow := Window;

  { On desktops, change the initial window size to simulate a tall screen,
    like mobile in a "portrait" orientation. }
  {$if not (defined(CASTLE_IOS) or defined(ANDROID) or defined(CASTLE_NINTENDO_SWITCH))}
  Window.Height := Application.ScreenHeight * 5 div 6;
  Window.Width := Window.Height * 900 div 1600;
  {$endif}

  Window.ParseParameters; // allows to control window size / fullscreen on the command-line

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.
