{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Logging. Log has to be activated in your program (nothing in the
  castle_game_engine units activates it automatically !) by InitializeLog.
  Various units of my engine print some logging info when @link(Log) is true.

  Prints log messages to StdOut, not to some external file.
  This is most useful and common behavior on Unixes.
  It also removed any problems with users asking "where can I find
  the log file?". The downside is that Windows users
  have to explicitly redirect StdOut of the GUI program to get the log. }
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

{ Write to log file. Call this only if @link(Log) = @true.

  This is a shortcut for @code(WritelnLog(Title, Format(LogMessageBase, Args))). }
procedure WritelnLog(const Title: string; const LogMessageBase: string;
  const Args: array of const);

{ Just like WritelnLog, but assumes that LogMessage already contains
  final newline --- so it doesn't add additional newline. }
procedure WriteLog(const Title: string; const LogMessage: string);

{ Just like WritelnLog, but wraps inside Title markers, so that
  it's nicely formatted even when LogMessage is multiline.
  LogMessage may be multiline and terminated by final newline. }
procedure WriteLogMultiline(const Title: string; const LogMessage: string);

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
      'run "' + ProgramBaseName + ' --debug-log > ' + ProgramBaseName + '.log".');
  end;

begin
  if Log then Exit; { ignore 2nd call to InitializeLog }

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
  WriteLog(Title, LogMessage + NL);
end;

procedure WritelnLog(const Title: string; const LogMessageBase: string;
  const Args: array of const);
begin
  WritelnLog(Title, Format(LogMessageBase, Args));
end;

procedure WriteLogMultiline(const Title: string; const LogMessage: string);
begin
  WritelnStr(
    '-------------------- ' + Title + ' begin' + NL +
    LogMessage +
    '-------------------- ' + Title + ' end');
end;

procedure WritelnLogMultiline(const Title: string; const LogMessage: string);
begin
  WriteLogMultiline(Title, LogMessage + NL);
end;

end.
