{%MainUnit castleopendocument.pas}
{
  Copyright 2012-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on Lazarus LCL code, which has
  exactly the same license as our "Castle Game Engine":
  LGPL with static linking exception, see COPYING.txt for details.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ ExecuteCommand, used by OpenUrl and/or OpenDocument on some platforms. }

{$ifdef read_uses}
CastleInternalProcess,
{$endif}

{$ifdef read_implementation}

{ Simplified version of ExecuteCommand that doesn't wait for the process to finish. }
procedure ExecuteCommandNoWait(
  ExeName: String;
  const Options: array of string);
var
  P: TCastleProcess;
  I: Integer;
begin
  ExeName := AbsoluteExeName(ExeName);

  P := TCastleProcess.Create(nil);
  try
    P.Executable := ExeName;
    if High(Options) >= 0 then
      for I := Low(Options) to High(Options) do
        P.Parameters.Add(Options[I]);
    WritelnLog('Calling ' + ExeName);
    WritelnLog(P.Parameters.Text);

    P.Execute;
  finally FreeAndNil(P) end;
end;

{$endif read_implementation}
