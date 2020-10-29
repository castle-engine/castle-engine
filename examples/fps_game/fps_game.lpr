{
  Copyright 2012-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example of a fully-working 3D FPS game.
  This is the desktop (standalone) code of game (not used by the mobile
  platforms, like Android or iOS). }
program fps_game;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

uses CastleWindow, GameInitialize;

begin
  { Set default Window size, and parse command-line parameters
    that may also affect Window size. }
  Application.MainWindow.FullScreen := true; { by default we open in fullscreen }
  Application.ParseStandardParameters;

  { Open the window. Shows the window, with an initialized OpenGL context. }
  Application.MainWindow.Open;

  { Run the application event loop.
    In more advanced cases, you can also execute each step of the loop
    manually by Application.ProcessMessage, like
    "while Application.ProcessMessage(true, true)
      and <whatever you want> do <whatever you want>;" }
  Application.Run;
end.
