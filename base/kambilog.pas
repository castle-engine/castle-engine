{
  Copyright 2006,2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Simple log unit. Has to be activated in your program (nothing in the
  kambi_vrml_game_engine units activates it automatically !) by InitializeLog.
  Various units of my engine print some logging info when @link(Log) is true.

  Prints log messages to StdOut, not to some external file.
  This is most useful and common behavior on Unixes.
  It also removed any problems with users asking "where can I find
  this log file you generated ?". The downside is that Windows users
  have to explicitly redirect StdOut of the GUI program to get log. }
unit KambiLog;

interface

{ Is logging active ? Initially no. Activate by InitializeLog. }
function Log: boolean;

procedure InitializeLog(const ProgramVersion: string);

{ Write to log file. Call this only if @link(Log) = @true.

  It was consciously decided that this will @italic(not be ignored) when
  @link(Log) = false, instead the programmer will have to always explicitly
  check @link(Log) value before calling this.
  That's because you shouldn't even waste time on constructing LogMessage
  for it when @link(Log) = false. }
procedure WritelnLog(const Title: string; const LogMessage: string);

{ Just like WritelnLog, but assumes that LogMessage already contains
  final newline --- so it doesn't add additional newline. }
procedure WriteLog(const Title: string; const LogMessage: string);

{ Just like WritelnLog, but wraps inside Title markers, so that
  it's nicely formatted even when LogMessage is multiline.
  LogMessage may be multiline but not terminated by final newline. }
procedure WritelnLogMultiline(const Title: string; const LogMessage: string);

implementation

uses Classes, KambiUtils, KambiClassUtils, KambiTimeUtils,
  SysUtils, KambiFilesUtils;

var
  FLog: boolean = false;

function Log: boolean;
begin
  Result := FLog;
end;

procedure InitializeLog(const ProgramVersion: string);

  procedure RaiseStdOutNotAvail;
  begin
    raise EWithHiddenClassName.Create(
      'You used --debug-log option but it seems that stdout (standard output) ' +
      'is not available. Under Windows you should explicitly ' +
      'redirect program''s stdout to make it available, e.g. ' +
      'run "' + ProgramName + ' --debug-log > ' + ProgramName + '.log".');
  end;

begin
  { Ideally, check for "StdOutStream = nil" should be all that is needed,
    and wrapping WritelnStr inside try...except should not be needed.
    But... see StdOutStream comments: you cannot
    depend on the fact that "StdOutStream <> nil means that stdout
    is actually available (because user redirected stdout etc.). }

  if StdOutStream = nil then
    RaiseStdOutNotAvail;

  try
    WritelnStr('Log for "' + ProgramName +
      '", version ' + ProgramVersion +
      '. Started on ' + DateTimeToAtStr(Now) + '.');
  except
    on E: EWriteError do RaiseStdOutNotAvail;
  end;

  { Set Log to true only once we succeded.

    Otherwise (when FLog := true would be done at the beginning of
    InitializeLog), if something is done in finally..end clauses surrounding
    InitializeLog, and it does "if Log then WritelnLog..." then it would
    try to write something to log --- even though we just jumped using
    RaiseStdOutNotAvail. }

  FLog := true;
end;

procedure WriteLog(const Title: string; const LogMessage: string);
begin
  WriteStr(Title + ': ' + LogMessage);
end;

procedure WritelnLog(const Title: string; const LogMessage: string);
begin
  WriteLog(Title, LogMessage + nl);
end;

procedure WritelnLogMultiline(const Title: string; const LogMessage: string);
begin
  WritelnStr(
    '-------------------- ' + Title + ' begin' +nl+
    LogMessage +nl+
    '-------------------- ' + Title + ' end');
end;

end.
