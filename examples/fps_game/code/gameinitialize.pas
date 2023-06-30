{
  Copyright 2012-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example of a fully-working 3D FPS game.
  This is the main game unit, which contains most of the code.
  This is a cross-platform game code that will work on any platform
  (desktop or mobile). }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleWindow, CastleApplicationProperties, CastleConfig,
  CastleComponentSerialize, CastleSoundEngine, GameViewportUnderUi
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewPlay
  , GameViewMenu
  , GameViewOptions
  , GameViewDeath
  , GameViewWin
  , GameViewCredits
  {$endregion 'Castle Initialization Uses'};

var
  Window: TCastleWindow;

{ Initialize the game.
  This is assigned to Application.OnInitialize, and will be called only once. }
procedure ApplicationInitialize;

  procedure InitializeMusicSound;
  var
    Sounds: TComponent;
    MusicSound: TCastleSound;
  begin
    Sounds := TComponent.Create(Application);
    ComponentLoad('castle-data:/sounds.castle-component', Sounds);
    MusicSound := Sounds.FindRequiredComponent('MusicSound') as TCastleSound;
    SoundEngine.LoopingChannel[0].Sound := MusicSound;
  end;

begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create views (see https://castle-engine.io/views ). }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewPlay := TViewPlay.Create(Application);
  ViewMenu := TViewMenu.Create(Application);
  ViewOptions := TViewOptions.Create(Application);
  ViewDeath := TViewDeath.Create(Application);
  ViewWin := TViewWin.Create(Application);
  ViewCredits := TViewCredits.Create(Application);
  {$endregion 'Castle View Creation'}

  UserConfig.Load;
  SoundEngine.Volume := UserConfig.GetFloat('sound_volume', 1);

  InitializeMusicSound;

  LoadViewportUnderUi;

  Window.Container.View := ViewMenu;
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

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
