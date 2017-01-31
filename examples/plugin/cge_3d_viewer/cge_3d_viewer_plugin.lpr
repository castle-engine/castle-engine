{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Library to run the viewer as NPAPI plugin. }
library cge_3d_viewer_plugin;
uses CastleWindow, Game;

{ This adds icons and version info for Windows, for a plugin version,
  automatically created by "castle-engine compile". }
{$ifdef MSWINDOWS} {$R plugin-automatic-windows-resources.res} {$endif MSWINDOWS}

exports
  NP_GetPluginVersion,
  NP_GetMIMEDescription,
  NP_GetValue,
  NP_Initialize,
  NP_Shutdown,
  NP_GetEntryPoints;
end.
