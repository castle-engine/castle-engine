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

{ Download an URL, write result on standard output (stdout).
  Writes diagnostic output, and progress, on standard error (stderr).
  This is a simple test of CastleDownload unit that allows you to simply grab
  any URL to a Stream.

  Try from command-line like
    castle_download http://castle-engine.sf.net/ > output.html
  or
    castle_download http://downloads.sourceforge.net/castle-engine/view3dscene-3.12.0-linux-i386.tar.gz > output.tar.gz
}

uses SysUtils, Classes, CastleDownload, CastleParameters, CastleClassUtils,
  CastleLog, CastleProgress, CastleProgressConsole;
var
  Stream: TStream;
begin
  EnableNetwork := true;
  InitializeLog;
  Progress.UserInterface := ProgressConsoleInterface;
  Parameters.CheckHigh(1);
  Stream := Download(Parameters[1]);
  try
    ReadGrowingStream(Stream, StdOutStream, false);
  finally FreeAndNil(Stream) end;
end.
