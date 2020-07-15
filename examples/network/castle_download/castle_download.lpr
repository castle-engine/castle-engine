{
  Copyright 2013-2020 Michalis Kamburelis.

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
    castle_download https://castle-engine.io/ > output.html
    castle_download https://castle-engine.io/latest.zip > output.zip
}

uses SysUtils, Classes,
  {$ifndef VER3_0} OpenSSLSockets, {$endif} // support HTTPS
  CastleDownload, CastleParameters, CastleClassUtils,
  CastleLog, CastleProgress, CastleProgressConsole;
var
  Stream: TStream;
begin
  EnableNetwork := true;
  LogEnableStandardOutput := false; // do not put log in stdout, it would be mixed with downloaded output
  InitializeLog;

  Progress.UserInterface := ProgressConsoleInterface;

  Parameters.CheckHigh(1);
  Stream := Download(Parameters[1]);
  try
    ReadGrowingStream(Stream, StdOutStream, false);
  finally FreeAndNil(Stream) end;
end.
