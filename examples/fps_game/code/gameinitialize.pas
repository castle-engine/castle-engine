{
  Copyright 2012-2022 Michalis Kamburelis.

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
  CastleWindow, CastleApplicationProperties,
  CastleComponentSerialize, CastleSoundEngine
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

  { Create game views and set initial view }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewPlay := TViewPlay.Create(Application);
  ViewMenu := TViewMenu.Create(Application);
  ViewOptions := TViewOptions.Create(Application);
  ViewDeath := TViewDeath.Create(Application);
  ViewWin := TViewWin.Create(Application);
  ViewCredits := TViewCredits.Create(Application);
  {$endregion 'Castle View Creation'}

  InitializeMusicSound;

  Window.Container.View := ViewMenu;
end;

initialization
  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  { Adjust window fullscreen state and size.
    Note that some platforms (like mobile) may ignore it.
    Examples how to set window fullscreen state and size:

      Window.FullScreen := true;

    or

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    or

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;
  }

  { You should not need to do *anything* more in the unit "initialization" section.
    Most of your game initialization should happen inside ApplicationInitialize.
    In particular, it is not allowed to read files before ApplicationInitialize
    (because in case of non-desktop platforms,
    some necessary resources may not be prepared yet). }
end.
