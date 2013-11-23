{
  Copyright 2006-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Logging. Log has to be activated in your program (nothing in the
  Castle Game Engine activates it automatically) by InitializeLog.
  Various units of the engine print some logging info when @link(Log) is true. }
unit CastleLog;

interface

uses Classes;

{ Is logging active. Initially no. Activate by InitializeLog. }
function Log: boolean;

{ Initialize logging.

  If you leave ALogStream as @nil (default),
  then we will prints log messages to StdOut, not to some external file.
  This is most useful and common behavior on Unixes, where most programs
  log to StdOut, and StdOut is always available.
  It also removes any problems with users asking "where can I find
  the log file?".
  The downside is that Windows users
  have to explicitly redirect StdOut of the GUI program to get the log,
  i.e. run your program from command-line like "program.exe > log.txt".
  Otherwise, GUI programs (with apptype GUI) do not have StdOut available
  under Windows. }
procedure InitializeLog(const ProgramVersion: string = 'Unknown';
  const ALogStream: TStream = nil);

{ Log message. Ignored when log is not initialized (@link(Log) is @false).

  Although we check @link(Log) here, you can also check it yourself
  before even calling this procedure. This way you can avoid spending time
  on constructing LogMessage. }
procedure WritelnLog(const Title: string; const LogMessage: string);

{ Format and log message.
  Ignored when log is not initialized (@link(Log) is @false).

  This is a shortcut for @code(WritelnLog(Title, Format(LogMessageBase, Args))). }
procedure WritelnLog(const Title: string; const LogMessageBase: string;
  const Args: array of const);

{ Log message, without appending newline at the end (given LogMessage
  should already contain a final newline). }
procedure WriteLog(const Title: string; const LogMessage: string);

{ Log multiline message.
  LogMessage may be multiline and must be terminated by final newline. }
procedure WriteLogMultiline(const Title: string; const LogMessage: string);

{ Log multiline message.
  LogMessage may be multiline and must @italic(not) be terminated by
  a final newline, because we will add final newline ourselves. }
procedure WritelnLogMultiline(const Title: string; const LogMessage: string);

implementation

uses CastleUtils, CastleClassUtils, CastleTimeUtils,
  SysUtils, CastleFilesUtils {$ifdef ANDROID}, CastleAndroidLog {$endif};

{ Dump backtrace (always to StdErr for now, regardless of LogStream)
  of each log.

  Displaying line info requires compiling your program with -gl.
  Unfortunately line info is not 100% reliable (sometimes it only works in gdb;
  sometimes it does not even work in gdb, but still you can use gdb's "info symbol xxx"
  to resolve addresses to method names).
  Depends very much on OS, debug info type, and FPC version. }
{ $define BACKTRACE_ON_LOG}


var
  FLog: boolean = false;
  LogStream: TStream;

function Log: boolean;
begin
  Result := FLog;
end;

procedure InitializeLog(const ProgramVersion: string;
  const ALogStream: TStream);

  procedure RaiseStdOutNotAvail;
  begin
    raise EWithHiddenClassName.Create(
      'Cannot write to log output stream. ' +
      'This usually means that you initialized log for a Windows GUI program, but stdout (standard output) ' +
      'is not available. Under Windows you should explicitly ' +
      'redirect program''s stdout to make it available, e.g. ' +
      'run "' + ApplicationName + ' --debug-log > ' + ApplicationName + '.log".');
  end;

begin
  if Log then Exit; { ignore 2nd call to InitializeLog }

  if ALogStream = nil then
  begin
    { Under Windows GUI program, StdOutStream may be nil.
      Ideally, check for "StdOutStream = nil" should be all that is needed.
      But... see StdOutStream comments: you cannot
      depend on the fact that "StdOutStream <> nil means that stdout
      is actually available (because user redirected stdout etc.).
      That is why the 1st WritelnStr below is wrapped inside try...except. }
    if StdOutStream = nil then
      RaiseStdOutNotAvail;
    LogStream := StdOutStream;
  end else
    LogStream := ALogStream;

  try
    WritelnStr(LogStream, 'Log for "' + ApplicationName +
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

procedure WriteLogRaw(const S: string); inline;
begin
  if Log then
  begin
    WriteStr(LogStream, S);
    {$ifdef BACKTRACE_ON_LOG}
    Dump_Stack(StdErr, Get_Frame);
    {$endif}
    {$ifdef ANDROID}
    AndroidLog(alInfo, S);
    {$endif}
  end;
end;

procedure WriteLog(const Title: string; const LogMessage: string);
begin
  if Log then
    WriteLogRaw(Title + ': ' + LogMessage);
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
  if Log then
    WriteLogRaw(
      '-------------------- ' + Title + ' begin' + NL +
      LogMessage +
      '-------------------- ' + Title + ' end' + NL);
end;

procedure WritelnLogMultiline(const Title: string; const LogMessage: string);
begin
  WriteLogMultiline(Title, LogMessage + NL);
end;

end.
