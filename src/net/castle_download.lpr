{
  Copyright 2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Download an URL, write result on standard output.
  Simple test of CastleDownloader unit.
  TODO: move to examples dir. }

uses SysUtils, Classes, CastleDownloader, CastleParameters, CastleClassUtils,
  CastleLog;
var
  Stream: TStream;
begin
  EnableNetwork := true;
  InitializeLog('1.0.0');
  Parameters.CheckHigh(1);
  Stream := DownloadURL(Parameters[1]);
  try
    ReadGrowingStream(Stream, StdOutStream, false);
  finally FreeAndNil(Stream) end;
end.
