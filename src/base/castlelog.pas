{
  Copyright 2006-2014 Michalis Kamburelis.

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

  @param(ProgramVersion The version of your application, as a string.
    This is just an arbitrary string, that will be shown as "program version"
    in the log output.

    Leave empty if you don't want to use this.)

  @param(ALogStream Where to generate the log.

    If you leave ALogStream as @nil (default), the default log output
    is determined as follows:

    @unorderedList(
      @item(On Unix and on console Windows applications,
        the output goes to the standard output, StdOut.
        This is most useful and common behavior on Unix, where most programs
        log to StdOut, and StdOut is always available.

        This approach avoids any questions from users asking "where can I find
        the log file?". And it avoids technical questions like "should
        we create a new log file with new number when old log file exists,
        or just overwrite old file, or append to it?" or "which directory
        is user-writeable". Since the user must explicitly redirect the output
        to the file, (s)he knows where the log file is.

        Note that on Android, we also automatically log to Android-specific
        log facility (that you can browse using "adb logcat".)
      )

      @item(On Windows GUI applications, we create a file xxx.log
        in the current directory. Where xxx is from @code(ApplicatioName).

        GUI programs (with apptype GUI) do not have StdOut available
        under Windows (at least not always).
      )
    )
  )
}
procedure InitializeLog(const ProgramVersion: string = '';
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

uses CastleUtils, CastleClassUtils, CastleTimeUtils, CastleWarnings,
  CastleFilesUtils, CastleURIUtils,
  SysUtils {$ifdef ANDROID}, CastleAndroidLog {$endif};

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
  LogStreamOwned: boolean;

function Log: boolean;
begin
  Result := FLog;
end;

procedure InitializeLog(const ProgramVersion: string;
  const ALogStream: TStream);
var
  FirstLine: string;

  function InitializeLogFile(const LogFileName: string): boolean;
  begin
    try
      { without fmShareDenyNone, you cannot open the file while plugin runs }
      LogStream := TFileStream.Create(LogFileName, fmCreate or fmShareDenyNone);
    except
      on E: EFCreateError do
      begin
        { Special message when LogFileName non-empty (usual case on Windows).
          Merely warn when creating log file not possible.
          Normal in many "production" cases when the directory of exe/plugin may not be writeable. }
        OnWarning(wtMajor, 'Log', 'Cannot create log file "' + LogFileName + '". To dump log of GUI application on Windows, you have to run the application in a directory where you have write access, for example your user or Desktop directory.');
        Exit(false);
      end;
    end;
    LogStreamOwned := true;
    Result := true;
  end;

begin
  if Log then Exit; { ignore 2nd call to InitializeLog }

  LogStreamOwned := false;

  if ALogStream = nil then
  begin
    {$ifdef MSWINDOWS}
    { In Windows DLL, which may also be NPAPI plugin, be even more cautious:
      create .log file in user's directory. }
    if IsLibrary then
    begin
      if not InitializeLogFile(
        URIToFilenameSafe(ApplicationConfig(ApplicationName + '.log'))) then
        Exit;
    end else
    {$endif}
    if not IsConsole then
    begin
      { Under Windows GUI program, by default write to file .log
	in the current directory.

	Do not try to use StdOutStream anymore. In some cases, GUI program
	may have an stdout, when it is explicitly run like
	"xxx.exe --debug-log > xxx.log". But do not depend on it.
	Simply writing to xxx.log is more what people expect. }
      if not InitializeLogFile(
        ExpandFileName(ApplicationName + '.log')) then
	Exit;
    end else
      LogStream := StdOutStream;
  end else
    LogStream := ALogStream;

  FirstLine := 'Log for "' + ApplicationName + '".';
  if ProgramVersion <> '' then
    FirstLine += ' Version: ' + ProgramVersion + '.';
  FirstLine += ' Started on ' + DateTimeToAtStr(Now) + '.';
  WritelnStr(LogStream, FirstLine);

  { Set Log to true only once we succeded.

    Otherwise (when FLog := true would be done at the beginning of
    InitializeLog), if something is done in finally..end clauses surrounding
    InitializeLog, and it does "WritelnLog..." then it would
    try to write something to uninitialized log. }

  FLog := true;
end;

procedure WriteLogRaw(const S: string); inline;
begin
  if Log then
  begin
    WriteStr(LogStream, S); // we know that LogStream <> nil when FLog = true
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

finalization
  if LogStreamOwned then
  begin
    FreeAndNil(LogStream);
    FLog := false;
  end;
end.
