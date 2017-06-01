{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Program to run the game on desktop (standalone) platforms. }
program ${NAME_PASCAL}_standalone;

{$apptype GUI}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef MSWINDOWS} {$R automatic-windows-resources.res} {$endif MSWINDOWS}

uses CastleLog, CastleWindow, ${GAME_UNITS};

begin
  Application.Version := '${VERSION}';
  Application.ParseStandardParameters;

  { On standalone, activate log only after parsing command-line options.
    This allows to handle --version and --help command-line parameters
    without any extra output on Unix. }
  InitializeLog(Application.Version);

  Application.MainWindow.OpenAndRun;
end.
