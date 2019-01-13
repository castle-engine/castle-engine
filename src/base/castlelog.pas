{
  Copyright 2006-2018 Michalis Kamburelis.

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

{$include castleconf.inc}

interface

uses Classes;

{ Is logging active. Initially no. Activate by InitializeLog. }
function Log: boolean;

type
  { Log date&time prefix style. }
  TLogTimePrefix = (
    { Default: no DateTime prefix is added. }
    ltNone,
    { Add time prefix to each log record. }
    ltTime,
    { Add date&time prefix to each log record. }
    ltDateTime);

{ Initialize logging.
  The default log output is documented on
  https://castle-engine.io/manual_log.php .

  @param(ALogStream Where to generate the log.

    If you leave ALogStream as @nil (default), the default log output
    is determined as follows:

    @unorderedList(
      @item(To a file called LogFileName, if you set this.)

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
      )

      @item(On Windows GUI applications, we create a file xxx.log
        in the current directory. Where xxx is from @code(ApplicatioName).

        GUI programs (with apptype GUI) do not have StdOut available
        under Windows (at least not always).
      )
    )

    Note that on Android, we also automatically log to Android-specific
    log facility (that you can browse using "adb logcat").
    This happens regardless of the ALogStream or LogFileName variables,
    all log entries *always* go to Android log.
  )

  @param(ALogTimePrefix optionally adds date&time prefix to each log record.)
}
procedure InitializeLog(
  const ALogStream: TStream = nil;
  const ALogTimePrefix: TLogTimePrefix = ltNone); overload;

procedure InitializeLog(const ProgramVersion: string;
  const ALogStream: TStream = nil;
  const ALogTimePrefix: TLogTimePrefix = ltNone); overload;
  deprecated 'to provide a Version to InitializeLog, set ApplicationProperties.Version earlier, instead of calling InitializeLog with an explicit ProgramVersion parameter';

{ Log message. Ignored when log is not initialized (@link(Log) is @false).

  Although we check @link(Log) here, you can also check it yourself
  before even calling this procedure. This way you can avoid spending time
  on constructing Message.

  When no Category, we use ApplicationName as a category. }
procedure WritelnLog(const Category: string; const Message: string); overload;
procedure WritelnLog(const Message: string); overload;

{ Format and log a message.
  Ignored when log is not initialized (@link(Log) is @false).
  This is a shortcut for @code(WritelnLog(Category, Format(MessageBase, Args))). }
procedure WritelnLog(const Category: string; const MessageBase: string;
  const Args: array of const); overload;
procedure WritelnLog(const MessageBase: string;
  const Args: array of const); overload;

{ Log message, without appending newline at the end (given Message
  should already contain a final newline). }
procedure WriteLog(const Category: string; const Message: string); overload;
  deprecated 'use WritelnLog, and do not add the final newline yourself to Message';

{ Log multiline message.
  The Message may, but doesn't have to, terminate with a newline --
  we will format it OK either way. }
procedure WritelnLogMultiline(const Category: string; const Message: string);

procedure WriteLogMultiline(const Category: string; const Message: string); deprecated 'use WritelnLogMultiline';

{ Log a warning, and call
  @link(TCastleApplicationProperties.OnWarning ApplicationProperties.OnWarning)
  event.

  This outputs a log message, if the log is initialized by @link(InitializeLog).
  We simply append the word "warning" to the Category, and pass arguments
  to WritelnLog.

  Then, @italic(regardless if the log is initialized or not),
  we also call @link(TCastleApplicationProperties.OnWarning ApplicationProperties.OnWarning).
  This allows to react to warnings e.g. by displaying a message dialog
  (like @code(ShowMessage) in Lazarus, or @link(MessageOK) in CastleMessages,
  or @link(TCastleWindowBase.MessageOK)).
  Or by raising an exception, if you want to be strict about warnings. }
procedure WritelnWarning(const Category: string; const Message: string); overload;
procedure WritelnWarning(const Message: string); overload;

{ A shortcut for @code(WritelnWarning(Category, Format(MessageBase, Args))). }
procedure WritelnWarning(const Category: string; const MessageBase: string;
  const Args: array of const); overload;
procedure WritelnWarning(const MessageBase: string;
  const Args: array of const); overload;

var
  { Dump backtrace (call stack) with each log.
    Displaying line info requires compiling your program with -gl. }
  BacktraceOnLog: boolean = false;

  { Current log date&time prefix style. Can be changed runtime. }
  LogTimePrefix: TLogTimePrefix;

  { Set this to a filename that should contain log,
    before calling @link(InitializeLog).
    This may be an absolute or relative (to the current directory
    at the time of InitializeLog call) path.
    It's your responsibility to choose a path that is writeable on current OS
    (you can e.g. use GetAppConfigDir function from FPC RTL). }
  LogFileName: String = '';

implementation

uses SysUtils,
  CastleUtils, CastleApplicationProperties, CastleClassUtils, CastleTimeUtils,
  CastleStringUtils
  {$ifdef ANDROID}, CastleAndroidInternalLog {$endif};

var
  FLog: boolean = false;
  LogStream: TStream;
  LogStreamOwned: boolean;

function Log: boolean;
begin
  Result := FLog;
end;

procedure InitializeLog(const ProgramVersion: string;
  const ALogStream: TStream;
  const ALogTimePrefix: TLogTimePrefix);
begin
  ApplicationProperties.Version := ProgramVersion;
  InitializeLog(ALogStream, ALogTimePrefix);
end;

procedure InitializeLog(
  const ALogStream: TStream;
  const ALogTimePrefix: TLogTimePrefix);
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
        WritelnWarning('Log', 'Cannot create log file "' + LogFileName + '". To dump log of GUI application on Windows, you have to run the application in a directory where you have write access, for example your user or Desktop directory.');
        Exit(false);
      end;
    end;
    LogStreamOwned := true;
    Result := true;
  end;

  { Similar to CastleFilesUtils.ApplicationConfig directory,
    but returns a filename, and doesn't depend on CastleFilesUtils and friends. }
  function ApplicationConfigPath: string;
  begin
    Result := InclPathDelim(GetAppConfigDir(false));
    if not ForceDirectories(Result) then
      raise Exception.CreateFmt('Cannot create directory for config file: "%s"',
        [Result]);
  end;

var
  InsideEditor: Boolean;
begin
  LogTimePrefix := ALogTimePrefix;

  if Log then Exit; { ignore 2nd call to InitializeLog }

  LogStreamOwned := false;

  InsideEditor := GetEnvironmentVariable('CASTLE_ENGINE_INSIDE_EDITOR') = 'true';

  if ALogStream <> nil then
  begin
    LogStream := ALogStream;
  end else
  if InsideEditor and (not IsLibrary) and (StdOutStream <> nil) then
  begin
    { In this case, we know we have StdOutStream initialized OK,
      even when IsConsole = false (because "castle-engine run"
      and "castle-editor" are calling subprocesses to capture output
      through pipes always).

      We want to log to StdOutStream, to send them to "castle-editor"
      output list. }
    LogStream := StdOutStream;
  end else
  { If not in CGE editor, then LogFileName takes precedence over everything else. }
  if LogFileName <> '' then
  begin
    if not InitializeLogFile(LogFileName) then
      Exit;
  end else
  { In a library (like Windows DLL), which may also be NPAPI plugin,
    be more cautious: create .log file in user's directory. }
  if IsLibrary then
  begin
    if not InitializeLogFile(ApplicationConfigPath + ApplicationName + '.log') then
      Exit;
  end else
  if not IsConsole then
  begin
    { Under Windows GUI program, by default write to file .log
      in the current directory.

      Do not try to use StdOutStream anymore. In some cases, GUI program
      may have an stdout, when it is explicitly run like
      "xxx.exe --debug-log > xxx.log". But do not depend on it.
      Simply writing to xxx.log is more what people expect. }
    if not InitializeLogFile(ExpandFileName(ApplicationName + '.log')) then
      Exit;
  end else
  begin
    LogStream := StdOutStream;
  end;

  FirstLine := 'Log for "' + ApplicationName + '".';
  if ApplicationProperties.Version <> '' then
    FirstLine := FirstLine + ' Version: ' + ApplicationProperties.Version + '.';
  FirstLine := FirstLine + ' Started on ' + DateTimeToAtStr(Now) + '.';
  WritelnStr(LogStream, FirstLine);
  WritelnStr(LogStream, 'Castle Game Engine version: ' + CastleEngineVersion + '.');
  WritelnStr(LogStream, 'Compiled with: ' + SCompilerDescription + '.');

  { Set Log to true only once we succeded.

    Otherwise (when FLog := true would be done at the beginning of
    InitializeLog), if something is done in finally..end clauses surrounding
    InitializeLog, and it does "WritelnLog..." then it would
    try to write something to uninitialized log. }

  FLog := true;

  if InsideEditor and (not IsLibrary) and (StdOutStream = nil) then
    WritelnWarning('Cannot send logs to the Castle Game Engine Editor through pipes.');
end;

procedure WriteLogRaw(const S: string); {$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  if Log then
  begin
    {$ifdef ANDROID}
    if BacktraceOnLog then
      AndroidLogRobust(alInfo, S + DumpStackToString(Get_Frame) + NL)
    else
      AndroidLogRobust(alInfo, S);
    {$else}
    // we know that LogStream <> nil when FLog = true
    {$ifdef FPC}
    if BacktraceOnLog then
      WriteStr(LogStream, S + DumpStackToString(Get_Frame) + NL)
    else
    {$endif}
      WriteStr(LogStream, S);
    {$endif}
  end;
end;

function LogTimePrefixStr: string;
begin
  case LogTimePrefix of
    ltNone: Result := '';
    ltTime: Result := FormatDateTime('tt', Now) + '> ';
    ltDateTime: Result := FormatDateTime('yyyy"-"mm"-"dd" "tt', Now) + '> ';
  end;
end;

procedure WriteLog(const Category: string; const Message: string);
begin
  if Log then
    WriteLogRaw(LogTimePrefixStr + Category + ': ' + Message);
end;

procedure WritelnLog(const Category: string; const Message: string);
begin
  // do not warn about using deprecated WriteLog here.
  // In the future, WriteLog should be moved to the "implementation" section
  // of the unit (internal), and undeprecated.
  {$warnings off}
  WriteLog(Category, Message + NL);
  {$warnings on}
end;

procedure WritelnLog(const Message: string);
begin
  WritelnLog(ApplicationName, Message);
end;

procedure WritelnLog(const Category: string; const MessageBase: string;
  const Args: array of const);
begin
  WritelnLog(Category, Format(MessageBase, Args));
end;

procedure WritelnLog(const MessageBase: string;
  const Args: array of const);
begin
  WritelnLog(ApplicationName, Format(MessageBase, Args));
end;

procedure WriteLogMultiline(const Category: string; const Message: string);
begin
  WritelnLogMultiline(Category, Message);
end;

procedure WritelnLogMultiline(const Category: string; const Message: string);
begin
  if Log then
  begin
    if LogTimePrefix <> ltNone then WriteLogRaw(LogTimePrefixStr + NL);
    WriteLogRaw(
      '-------------------- ' + Category + ' begin' + NL +
      // trim newlines at the end of Message
      TrimEndingNewline(Message) + NL +
      '-------------------- ' + Category + ' end' + NL)
  end;
end;

procedure WritelnWarning(const Category: string; const Message: string);
begin
  WritelnLog('Warning: ' + Category, Message);
  ApplicationProperties._Warning(Category, Message);
end;

procedure WritelnWarning(const Message: string);
begin
  WritelnWarning(ApplicationName, Message);
end;

procedure WritelnWarning(const Category: string; const MessageBase: string;
  const Args: array of const);
begin
  WritelnWarning(Category, Format(MessageBase, Args));
end;

procedure WritelnWarning(const MessageBase: string;
  const Args: array of const);
begin
  WritelnWarning(ApplicationName, MessageBase, Args);
end;

initialization
finalization
  if LogStreamOwned then
  begin
    FreeAndNil(LogStream);
    FLog := false;
  end;
end.
