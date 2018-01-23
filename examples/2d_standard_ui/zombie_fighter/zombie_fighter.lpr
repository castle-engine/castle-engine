{
  Copyright 2016-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile".
  Comment this out if you don't compile using our "castle-engine" build tool. }
{$ifdef MSWINDOWS} {$R automatic-windows-resources.res} {$endif MSWINDOWS}

uses CastleLog, CastleApplicationProperties, CastleWindow, Game;
begin
  Application.ParseStandardParameters;

  { On standalone, activate log only after parsing command-line options.
    This allows to handle --version and --help command-line parameters
    without any extra output on Unix. }
  InitializeLog;

  Application.MainWindow.OpenAndRun;
end.
