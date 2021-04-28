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
  CastleUIControls, CastleApplicationProperties, CastleUIState, CastleImages,
  CastleVectors
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameStateMenu
  , GameStatePlay
  , GameStateGameOver
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindowBase;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  Image: TCastleImage;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Adjust theme }
  Image := LoadImage('castle-data:/ui/red_button06.png');
  Theme.Images[tiButtonNormal] := Image;
  Theme.OwnsImages[tiButtonNormal] := true;
  Theme.Corners[tiButtonNormal] := Vector4(6, 6, 6, 6);
  Theme.TextColor := White;

  Image := LoadImage('castle-data:/ui/red_button08.png');
  Theme.Images[tiButtonFocused] := Image;
  Theme.OwnsImages[tiButtonFocused] := true;
  Theme.Corners[tiButtonFocused] := Vector4(6, 6, 6, 6);

  Image := LoadImage('castle-data:/ui/red_button07.png');
  Theme.Images[tiButtonPressed] := Image;
  Theme.OwnsImages[tiButtonPressed] := true;
  Theme.Corners[tiButtonPressed] := Vector4(6, 6, 6, 6);

  { Create game states and set initial state }
  {$region 'Castle State Creation'}
  // The content here may be automatically updated by CGE editor.
  StatePlay := TStatePlay.Create(Application);
  StateMenu := TStateMenu.Create(Application);
  StateGameOver := TStateGameOver.Create(Application);
  {$endregion 'Castle State Creation'}

  StatePlay.DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
  StateMenu.DesignUrl := 'castle-data:/gamestatemenu.castle-user-interface';
  StateGameOver.DesignUrl := 'castle-data:/gamestategameover.castle-user-interface';

  TUIState.Current := StateMenu;
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'platformer_v1';

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
  Window.Caption := 'Platformer V1';
  Application.MainWindow := Window;

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.
