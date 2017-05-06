{
  Copyright 2013-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$apptype GUI}

{ Main program for a standalone version of the game.
  This allows you to compile the same game game (in Game unit)
  as a normal, standalone executable for normal OSes (Linux, Windows, MacOSX...). }
program drawing_toy_standalone;
uses CastleWindow, Game;
begin
  Window.OpenAndRun;
end.
