{
  Copyright 2015-2015 Michalis Kamburelis.

  This file is part of "Alien Outpost".

  "Alien Outpost" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Alien Outpost" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Library to run the game as NPAPI plugin. }
library alienoutpost;
uses CastleWindow, Game;
{$ifdef MSWINDOWS}
  {$R plugin-automatic-windows-resources.res}
{$endif MSWINDOWS}
exports
  NP_GetPluginVersion,
  NP_GetMIMEDescription,
  NP_GetValue,
  NP_Initialize,
  NP_Shutdown,
  NP_GetEntryPoints;
end.
