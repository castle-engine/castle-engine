{
  Copyright 2012-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example of a 3D FPS game. }
program fps_game;

uses SysUtils, CastleWindow, CastleWarnings, CastleConfig, CastleLevels,
  CastlePlayer, CastleSoundEngine, CastleProgress, CastleWindowProgress,
  CastleResources;

var
  Window: TCastleWindow;
  SceneManager: TGameSceneManager; //< same thing as Window.SceneManager
  Player: TPlayer; //< same thing as Window.SceneManager.Player

function MyGetApplicationName: string;
begin
  Result := 'fps_game';
end;

begin
  { We use standard FPC ApplicationName function for some names (e.g. Config
    file name), so make sure it's Ok. }
  OnGetApplicationName := @MyGetApplicationName;

  { Write warnings on standard output (console).
    By default, warnings are only written to log, and log by default goes nowhere. }
  OnWarning := @OnWarningWrite;

  Window := TCastleWindow.Create(Application);
  SceneManager := Window.SceneManager;

  { Parse command-line parameters.
    Options parsed by Window.ParseParameters are documented on
    http://castle-engine.sourceforge.net/opengl_options.php .
    Options parsed by SoundEngine.ParseParameters are documented on
    http://castle-engine.sourceforge.net/openal.php#section_options .
    Of course you can also handle your own options, see CastleParameters
    unit: use Parameters.Parse method (or just directly browse Parameters
    string list). }
  Window.FullScreen := true; { by default we open in fullscreen }
  Window.ParseParameters(StandardParseOptions);
  SoundEngine.ParseParameters;

  { Load configuration file. This loads configuration for various parts of the
    engine that add their callbacks to Config.OnLoad, Config.OnSave.
    Of course you can also use this for your game specific purposes,
    as Config is just standard FPC TXMLConfig class (with some extensions,
    see CastleXMLConfig unit). }
  Config.Load;

  { Open window (with OpenGL context). }
  Window.Open;

  { Show progress bars on our Window. }
  Progress.UserInterface := WindowProgressInterface;
  WindowProgressInterface.Window := Window;

  { Load all resources (creatures and items kinds) information from
    resource.xml files found inside ProgramDataPath.
    Similarly, load all available levels information from level.xml
    files inside ProgramDataPath. }
  Resources.LoadFromFiles;
  Levels.LoadFromFiles;

  { Create player. It's not strictly necessary to use Player, but it makes
    some stuff working better/simpler: Player automatically configures
    camera (to use game-like AWSD shortcuts from CastleInputs,
    to use gravity), it adds footsteps etc.
    It's best to assign SceneManager.Player before SceneManager.LoadLevel,
    then Player.Camera is automatically configured as SceneManager.Camera
    and it follows level's properties like PreferredHeight (from level's
    NavigationInfo.avatarSize). }
  Player := TPlayer.Create(SceneManager);
  SceneManager.Items.Add(Player);
  SceneManager.Player := Player;

  { Load initial level.
    This loads and adds 3D model of your level to the 3D world
    (that is to SceneManager.Items). It may also load initial creatures/items
    on levels, waypoints/sectors and other information from so-called
    "placeholders" on the level, see TGameSceneManager.LoadLevel documentation. }
  SceneManager.LoadLevel('example_level');

  { Run the game loop.
    In more advanced cases, you can also execute each step of the loop
    manually by Application.ProcessMessage, like
    "while Application.ProcessMessage(true, true)
      and <whatever you want> do <whatever you want>;" }
  Application.Run;

  { Save the configuration file. This is commented out for now,
    as this example program does not give user any UI to actually change
    any configuration. }
  //Config.Save;
end.
