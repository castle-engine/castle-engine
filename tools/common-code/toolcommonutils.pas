{
  Copyright 2014-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".
  Parts of this file are based on FPC packages/fcl-process/src/process.pp ,
  which conveniently uses *exactly* the same license as Castle Game Engine.

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Common utilities shared by various tools. }
unit ToolCommonUtils;

interface

uses Classes,
  CastleStringUtils;

var
  { Trivial verbosity global setting. }
  Verbose: boolean = false;

{ In case of console application with Verbose, writes to stduot.
  Otherwise, only to WritelnLog. }
procedure WritelnVerbose(const S: String);

{ Like @link(FindExe), but additionally look for the exe in
  Castle Game Engine bin/ subdirectory. }
function FindExeCastleTool(const ExeName: String): String;

var
  { When non-empty, determines the CastleEnginePath result unconditionally. }
  CastleEngineOverridePath: String;

{ Path to CGE main directory.
  Autodetected or obtained from $CASTLE_ENGINE_PATH environment variable.

  Returns empty String if it wasn't possible to get a valid value.
  Otherwise, the returned path always ends with path delimiter,
  and always exists. }
function CastleEnginePath: String;

{ Copy current environemt variables. }
function EnvironmentStrings: TStringList;

type
  { Line filtering used by MyRunCommandIndir and friends.
    You can process the variable Line in any way.
    Return whether to pass it (return @false to completely discard it,
    any edits to Line don't matter then). }
  TLineFiltering = function (var Line: String; const Data: Pointer): Boolean;

  TRunCommandFlag = (rcNoConsole);
  TRunCommandFlags = set of TRunCommandFlag;

{ Run command in the given directory with the given arguments,
  gathering the output (including error output, i.e. stdout and stderr)
  to a string.

  Allows for filtering the gathered output too.

  Like Process.RunCommandIndir in FPC >= 2.6.4,
  but also captures error output,
  and enables filtering.

  @param(ExeName Executable name, should be an absolute filename
    e.g. found by FindExe. This will avoid FPC errors (the default FPC
    algorithm searching $PATH may mistake binary for a directory with
    the same name).)

  @param(OutputString Standard output (stdout) and standard error output
    (stderr) of the command.)

  @param(LineFiltering Allows to filter the gathered lines, see TLineFiltering docs.)

  @param(LineFilteringData Passed to the LineFiltering callback.
    Ignored if LineFiltering is nil.)

  @param(If defined, this overrides all environment variables.
    The value is owned by this routine.
    You can start from EnvironmentStrings.)
}
procedure MyRunCommandIndir(
  const CurrentDirectory: string; const ExeName: string;
  const Options: array of string;
  out OutputString: string; out ExitStatus: integer;
  const LineFiltering: TLineFiltering = nil;
  const LineFilteringData: Pointer = nil;
  const Flags: TRunCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);

{ Run command in given directory with given arguments,
  gathering output and status to string, and also letting output
  to go to our output.

  @param(ExeName Executable name, should be an absolute filename
    e.g. found by FindExe. Like for MyRunCommandIndir.)

  @param(OutputString Stdout and stderr of the process.
    Like by MyRunCommandIndir.)

  @param(OverrideEnvironmentName
    When not empty, this environment variable has set
    value OverrideEnvironmentValue in the process.) }
procedure RunCommandIndirPassthrough(
  const CurrentDirectory: string; const ExeName: string;
  const Options: array of string;
  var OutputString: String; var ExitStatus: Integer;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '';
  const LineFiltering: TLineFiltering = nil;
  const LineFilteringData: Pointer = nil);

var
  ForcePipesPassthrough: Boolean = false;

{ Run command in given (or current) directory with given arguments,
  letting output (stdout and stderr) to go to our stdout / stderr.

  ExeName is searched on $PATH following standard OS conventions,
  if it's not already an absolute exe filename.

  Raises exception if command fails (detected by exit code <> 0).

  If OverrideEnvironmentName is <> '', then we set this one specific
  environment variable, to the value of OverrideEnvironmentValue.

  Global variable ForcePipesPassthrough can be used to force
  using pipes to communicate with the process.
  It should not be necessary (and it may introduce some performance drop,
  though I didn't observe any in practice)... except on Windows when you run
  "castle-engine run" in PowerShell in VS Code.
  For some reason, "castle-engine run" in CGE editor doesn't need it. }
procedure RunCommandSimple(
  const ExeName: string; const Options: array of string); overload;
procedure RunCommandSimple(
  const CurrentDirectory: string; const ExeName: string; const Options: array of string;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = ''); overload;

{ Run the command, and return immediately, without waiting for finish. }
procedure RunCommandNoWait(
  const CurrentDirectory: string;
  const ExeName: string; const Options: array of string;
  const Flags: TRunCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);

{ Determine and create a new (unique, with random number in the name) temp directory. }
function CreateTemporaryDir: string;

{ Make correct CGE project qualified name from any ProjectName. }
function MakeQualifiedName(ProjectName: String): String;

{ Make correct CGE project Pascal name from any ProjectName. }
function MakeProjectPascalName(ProjectName: String): String;

var
  { CGE manifest filename, designating CGE project root.
    Can be adjusted using `castle-engine --manifest-name=xxx`. }
  ManifestName: String = 'CastleEngineManifest.xml';

implementation

uses SysUtils, Process,
  CastleFilesUtils, CastleUtils, CastleURIUtils, CastleLog,
  ToolArchitectures;

procedure WritelnVerbose(const S: String);
begin
  if Verbose and IsConsole then
    WriteLn(S)
  else
    WriteLnLog(S);
end;

function FindExeCastleTool(const ExeName: String): String;
begin
  if CastleEnginePath <> '' then
  begin
    Result := CastleEnginePath + 'bin' + PathDelim + ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;

    { Look for exe wrapped in macOS application bundle,
      necessary to find view3dscene, castle-view-image in CGE bin. }
    {$ifdef DARWIN}
    Result := CastleEnginePath + 'bin' + PathDelim +
      ExeName + '.app' + PathDelim +
      'Contents' + PathDelim +
      'MacOS' + PathDelim +
      ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;
    {$endif}

    Result := CastleEnginePath + 'tools' + PathDelim + 'contrib' + PathDelim +
      CPUToString(DefaultCPU) + '-' + OSToString(DefaultOS) + PathDelim +
      ExeName + ExeExtension;
    if RegularFileExists(Result) then
      Exit;
  end;
  Result := FindExe(ExeName);
end;

function GetCastleEnginePathFromEnv: String;
begin
  Result := GetEnvironmentVariable('CASTLE_ENGINE_PATH');
  if Result = '' then
    Exit;

  Result := InclPathDelim(Result);

  if not DirectoryExists(Result) then
  begin
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable points to a non-existing directory "%s", ignoring',
      [Result]);
    Exit('');
  end;

  { $CASTLE_ENGINE_PATH environment variable may point to the directory
    - containing castle_game_engine/ as subdirectory (deprecated but allowed)
    - or containing castle-engine/ as subdirectory (deprecated but allowed)
    - or pointing straight to castle_game_engine/ or castle-engine/ directory. }
  if DirectoryExists(Result + 'castle_game_engine') then
  begin
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable points to a parent of "castle_game_engine" directory: "%s". This is deprecated, better change $CASTLE_ENGINE_PATH to include the "castle_game_engine" suffix.',
      [Result]);
    Result := Result + 'castle_game_engine' + PathDelim
  end else
  if DirectoryExists(Result + 'castle-engine') then
  begin
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable points to a parent of "castle-engine" directory: "%s". This is deprecated, better change $CASTLE_ENGINE_PATH to include the "castle-engine" suffix.',
      [Result]);
    Result := Result + 'castle-engine' + PathDelim;
  end;

  if not DirectoryExists(Result + 'src') then
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable defined, but we cannot find Castle Game Engine sources inside: "%s". We try to continue, assuming that engine unit paths are already specified within fpc.cfg file, otherwise compilation will fail.',
      [Result]);
  if not DirectoryExists(Result + 'tools' + PathDelim + 'build-tool' + PathDelim + 'data') then
    WritelnWarning('$CASTLE_ENGINE_PATH environment variable defined, but we cannot find build tool data inside: "%s". We try to continue, but some packaging operations will fail.',
      [Result]);
end;

{ Check is Path a sensible CGE sources path.
  Requires Path to end with PathDelim. }
function CheckCastlePath(const Path: String): Boolean;
begin
  Result :=
    DirectoryExists(Path + 'src') and
    DirectoryExists(Path + 'tools' + PathDelim + 'build-tool' + PathDelim + 'data');
end;

function GetCastleEnginePathFromExeName: String;
var
  ToolDir: String;
begin
  try
    // knowingly using deprecated ExeName, that should be non-deprecated and internal here
    {$warnings off}
    ToolDir := ExtractFileDir(ExeName);
    {$warnings on}
    { in case we're inside macOS bundle, use bundle path.
      This makes detection in case of CGE editor work OK. }
    {$ifdef DARWIN}
    if BundlePath <> '' then
      ToolDir := ExtractFileDir(ExclPathDelim(BundlePath));
    {$endif}

    { Check ../ of current exe, makes sense in released CGE version when
      tools are precompiled in bin/ subdirectory. }
    Result := InclPathDelim(ExtractFileDir(ToolDir));
    if CheckCastlePath(Result) then
      Exit;
    { Check ../../ of current exe, makes sense in development when
      each tool is compiled by various scripts in tools/xxx/ subdirectory. }
    Result := InclPathDelim(ExtractFileDir(ExtractFileDir(ToolDir)));
    if CheckCastlePath(Result) then
      Exit;

    Result := '';
  except
    on EExeNameNotAvailable do
      WritelnVerbose('Cannot detect CGE path because ExeName not available on this platform, and $CASTLE_ENGINE_PATH not defined');
  end;
end;

function GetCastleEnginePathSystemWide: String;
begin
  {$ifdef UNIX}
  Result := '/usr/src/castle-engine/';
  if CheckCastlePath(Result) then
    Exit;

  Result := '/usr/local/src/castle-engine/';
  if CheckCastlePath(Result) then
    Exit;
  {$endif}

  Result := '';
end;

var
  CastleEnginePathIsCached: Boolean;
  CastleEnginePathCached: String;

function CastleEnginePath: String;
begin
  { In case of CastleEngineOverridePath, ignore CastleEnginePathCached.
    This avoids clearing this cache when CastleEngineOverridePath changes
    at runtime, like in editor. }
  if CastleEngineOverridePath <> '' then
    Result := InclPathDelim(CastleEngineOverridePath)
  else
  if CastleEnginePathIsCached then
    Result := CastleEnginePathCached
  else
  begin
    // try to find CGE on $CASTLE_ENGINE_PATH
    Result := GetCastleEnginePathFromEnv;
    // try to find CGE on path relative to current exe
    if Result = '' then
      Result := GetCastleEnginePathFromExeName;
    // try to find CGE on system-wide paths
    if Result = '' then
      Result := GetCastleEnginePathSystemWide;

    if Result <> '' then
      WritelnVerbose('Castle Game Engine directory detected: ' + Result)
    else
      WritelnWarning('Castle Game Engine directory cannot be detected:' + NL +
        '- $CASTLE_ENGINE_PATH environment variable not defined, or points to an incorrect directory.' + NL +
        '- Moreover we cannot find Castle Game Engine looking at parent directory of this program exe.');

    CastleEnginePathIsCached := true;
    CastleEnginePathCached := Result;
  end;
end;

{ Output capturing interface -------------------------------- }

type
  { Read from Source stream (with the option to read without blocking) all contents.

    Collected contents can be obtained by calling @link(Collected)
    (it should be used only once all ReadAvailable and ReadEverything calls are done).

    The TCaptureOutput class doesn't process the contents in any way.
    When it reads newline characters (#10, #13) they are treated
    just like any other character.
    Descendants like TCaptureOutputFilter may have different behavior. }
  TCaptureOutput = class
  strict private
    const
     { Original code for this adjusted from fpc/trunk/packages/fcl-process/src/process.pp ,
       although reworked many times.
       MaxReadBytes is not too small to avoid fragmentation when reading large files. }
      MaxReadBytes = 65536;
    var
      OutputUsefulCount: Integer;
      OutputString: String;
  protected
    function ReadCore: Integer; virtual;
  public
    Source: TStream;

    { Also write everything you collect to out output (stdout).
      Note that it's not a perfect "passthrough": we collect both stdout and stderr
      from the child process, and output it always on our stdout.
      So we redirect child's stdout to our stderr. }
    Passthrough: Boolean;

    { Read from Source stream as much bytes as possible (without blocking waiting for more). }
    procedure ReadAvailable;

    { Read from Source stream all remaining bytes. }
    procedure ReadEverything;

    { Return read contents so far. Call this only after all the reading is done. }
    function Collected: String; virtual;

    { Create new instance of TCaptureOutput or a descendant,
      like TCaptureOutputFilter, suitable for given parameters. }
    class function Construct(const ASource: TStream;
      const ALineFiltering: TLineFiltering;
      const ALineFilteringData: Pointer): TCaptureOutput; static;
  end;

  { API just like ancestor, but filters the read lines through LineFiltering callback.
    Output line endings are also converted to native (LineEnding) along the way. }
  TCaptureOutputFilter = class(TCaptureOutput)
  strict private
    { Note that TCaptureOutputFilter has entirely different implementation than
      ancestor. While it also defines private MaxReadBytes and OutputString,
      they are used in very different way. }
    const
      MaxReadBytes = 65536;
    var
      OutputString: String;
      Buffer: String;
      PendingLine: String;
  protected
    function ReadCore: Integer; override;
  public
    LineFiltering: TLineFiltering; //< Never @nil
    LineFilteringData: Pointer;
    constructor Create;
    function Collected: String; override;
  end;

{ Output capturing: TCaptureOutput -------------------------------- }

function TCaptureOutput.ReadCore: Integer;
begin
  SetLength(OutputString, OutputUsefulCount + MaxReadBytes);
  Result := Source.Read(OutputString[1 + OutputUsefulCount], MaxReadBytes);
  if Passthrough then
    Write(Copy(OutputString, 1 + OutputUsefulCount, Result));
  if Result > 0 then
    Inc(OutputUsefulCount, Result);
end;

procedure TCaptureOutput.ReadAvailable;
var
  NumBytes: Integer;
begin
  NumBytes := ReadCore;
  if NumBytes = 0 then
  begin
    Sleep(100); // when no data, wait a bit instead of wasting CPU trying to query stream
    Exit;
  end;
end;

procedure TCaptureOutput.ReadEverything;
var
  NumBytes: Integer;
begin
  repeat
    NumBytes := ReadCore;
  until NumBytes <= 0;
end;

function TCaptureOutput.Collected: String;
begin
  SetLength(OutputString, OutputUsefulCount);
  Result := OutputString;
end;

class function TCaptureOutput.Construct(const ASource: TStream;
  const ALineFiltering: TLineFiltering;
  const ALineFilteringData: Pointer): TCaptureOutput; static;
begin
  if Assigned(ALineFiltering) then
  begin
    Result := TCaptureOutputFilter.Create;
    TCaptureOutputFilter(Result).LineFiltering := ALineFiltering;
    TCaptureOutputFilter(Result).LineFilteringData := ALineFilteringData;
  end else
  begin
    Result := TCaptureOutput.Create;
  end;
  Result.Source := ASource;
end;

{ Output capturing: TCaptureOutputFilter ------------------------------------- }

constructor TCaptureOutputFilter.Create;
begin
  inherited;
  SetLength(Buffer, MaxReadBytes);
end;

function TCaptureOutputFilter.ReadCore: Integer;
var
  NewlinePos, PreviousPendingLineLength, NextLineStart: Integer;
  Line: String;
  LineAllow: Boolean;
begin
  Result := Source.Read(Buffer[1], MaxReadBytes);

  if Result = 0 then
    Exit;

  PreviousPendingLineLength := Length(PendingLine);
  PendingLine := PendingLine + Copy(Buffer, 1, Result);

  // in a loop, remove as much lines from PendingLine as possible
  repeat
    // Using here CharsPosEx instead of CharsPos
    // is just optimization to not search entire PendingLine every time.
    NewlinePos := CharsPosEx([#10, #13], PendingLine, PreviousPendingLineLength + 1);

    if NewlinePos = 0 then Break;

    // we have a line -- filter it and possibly add to OutputString
    Line := Copy(PendingLine, 1, NewlinePos - 1);
    LineAllow := LineFiltering(Line, LineFilteringData);
    if LineAllow then
    begin
      OutputString := OutputString + Line + LineEnding;
      if Passthrough then
        Writeln(Line);
    end;

    { If this is followed by 2nd newline character, we want to consume it.

      TODO: We here assume that both newline characters will be returned by Source.Read,
      which may not be true (when newline is 2-character, like on #13#10,
      it is possible that line break will be split across MaxReadBytes borders).
      When this assumption will break, we'll just cause an additional newline,
      which is acceptable for now. }

    if (NewlinePos + 1 <= Length(PendingLine)) and
       { check we have #13 followed by #10 or #10 followed by #13.
         Be careful to *not* eat #10 followed by #10, as that would
         make us silently consume empty lines in files with Unix line ending. }
       ( ( (PendingLine[NewlinePos] = #10) and (PendingLine[NewlinePos + 1] = #13) ) or
         ( (PendingLine[NewlinePos] = #13) and (PendingLine[NewlinePos + 1] = #10) ) ) then
      NextLineStart := NewlinePos + 2
    else
      NextLineStart := NewlinePos + 1;

    PendingLine := SEnding(PendingLine, NextLineStart);
    PreviousPendingLineLength := 0; // reset for next CharsPosEx search
  until false;
end;

function TCaptureOutputFilter.Collected: String;
begin
  Result := OutputString + PendingLine;
  PendingLine := '';
end;

{ Running processes ---------------------------------------------------------- }

function EnvironmentStrings: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 1 to GetEnvironmentVariableCount do
    Result.Add(GetEnvironmentString(I));
end;

procedure MyRunCommandIndir(const CurrentDirectory: string;
  const ExeName: string;const Options: array of string;
  out OutputString: string; out ExitStatus: integer;
  const LineFiltering: TLineFiltering = nil;
  const LineFilteringData: Pointer = nil;
  const Flags: TRunCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);
var
  P: TProcess;
  I: Integer;
  Capture: TCaptureOutput;
begin
  // default out values
  OutputString := '';
  ExitStatus := 0;

  { Set to nil variables that will be later freed in the "finally" clause.
    This makes code a bit simpler, no need for nesting multiple try..finally clauses. }
  Capture := nil;
  P := nil;
  try
    P := TProcess.Create(nil);
    P.Executable := ExeName;
    if CurrentDirectory <> '' then
      P.CurrentDirectory := CurrentDirectory;
    if High(Options) >= 0 then
      for I := Low(Options) to High(Options) do
        P.Parameters.Add(Options[I]);
    WritelnVerbose('Calling ' + ExeName);
    WritelnVerbose(P.Parameters.Text);

    P.Options := [poUsePipes, poStderrToOutPut];
    if rcNoConsole in Flags then
      P.Options := P.Options + [poNoConsole];
    if OverrideEnvironment <> nil then
      P.Environment := OverrideEnvironment;
    P.Execute;

    Capture := TCaptureOutput.Construct(P.Output, LineFiltering, LineFilteringData);

    // wait until P finishes, read OutputString
    while P.Running do
      Capture.ReadAvailable;
    Capture.ReadEverything;
    OutputString := Capture.Collected;

    ExitStatus := P.ExitStatus;
  finally
    FreeAndNil(Capture);
    FreeAndNil(P);
  end;
end;

procedure RunCommandIndirPassthrough(const CurrentDirectory: string;
  const ExeName: string;
  const Options: array of string;
  var OutputString: String; var ExitStatus: Integer;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '';
  const LineFiltering: TLineFiltering = nil;
  const LineFilteringData: Pointer = nil);
var
  P: TProcess;
  I: Integer;
  NewEnvironment: TStringList;
  Capture: TCaptureOutput;
begin
  { Set to nil variables that will be later freed in the "finally" clause.
    This makes code a bit simpler, no need for nesting multiple try..finally clauses. }
  Capture := nil;
  P := nil;
  NewEnvironment := nil;
  try
    P := TProcess.Create(nil);
    P.Executable := ExeName;
    if CurrentDirectory <> '' then
      P.CurrentDirectory := CurrentDirectory;
    if High(Options) >= 0 then
     for I := Low(Options) to High(Options) do
       P.Parameters.Add(Options[I]);
    WritelnVerbose('Calling ' + ExeName);
    WritelnVerbose(P.Parameters.Text);

    if OverrideEnvironmentName <> '' then
    begin
      NewEnvironment := EnvironmentStrings;
      NewEnvironment.Values[OverrideEnvironmentName] := OverrideEnvironmentValue;
      P.Environment := NewEnvironment;
      // WritelnVerbose('Environment: ' + P.Environment.Text);
    end;

    P.Options := [poUsePipes, poStderrToOutPut];
    P.Execute;

    Capture := TCaptureOutput.Construct(P.Output, LineFiltering, LineFilteringData);
    Capture.Passthrough := true;

    // wait until P finishes, read OutputString
    while P.Running do
      Capture.ReadAvailable;
    Capture.ReadEverything;
    OutputString := Capture.Collected;

    ExitStatus := P.ExitStatus;
  finally
    FreeAndNil(Capture);
    FreeAndNil(P);
    FreeAndNil(NewEnvironment);
  end;
end;

procedure RunCommandSimple(
  const ExeName: string; const Options: array of string);
begin
  RunCommandSimple(GetCurrentDir, ExeName, Options);
end;

procedure RunCommandSimple(
  const CurrentDirectory: string; const ExeName: string; const Options: array of string;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '');

  { Run the given command, wait for it.
    The stdout/stderr is not captured to any string,
    it is only passed-through as our stdout/stderr.

    ( Use RunCommandIndirPassthrough to capture output to String and pass-through,
    use RunCommandIndir to only capture output to String. ) }
  procedure RunCommandNoPipes(const CurrentDirectory: string;
    const ExeName: string;
    const Options: array of string;
    var ExitStatus: Integer;
    const OverrideEnvironmentName: string = '';
    const OverrideEnvironmentValue: string = '');
  var
    P: TProcess;
    I: Integer;
    NewEnvironment: TStringList;
  begin
    { Set to nil variables that will be later freed in the "finally" clause.
      This makes code a bit simpler, no need for nesting multiple try..finally clauses. }
    P := nil;
    NewEnvironment := nil;
    try
      P := TProcess.Create(nil);
      P.Executable := ExeName;
      if CurrentDirectory <> '' then
        P.CurrentDirectory := CurrentDirectory;
      if High(Options) >= 0 then
       for I := Low(Options) to High(Options) do
         P.Parameters.Add(Options[I]);
      WritelnVerbose('Calling ' + ExeName);
      WritelnVerbose(P.Parameters.Text);

      if OverrideEnvironmentName <> '' then
      begin
        NewEnvironment := TStringList.Create;
        for I := 1 to GetEnvironmentVariableCount do
          NewEnvironment.Add(GetEnvironmentString(I));
        NewEnvironment.Values[OverrideEnvironmentName] := OverrideEnvironmentValue;
        P.Environment := NewEnvironment;
        // WritelnVerbose('Environment: ' + P.Environment.Text);
      end;

      P.Execute;

      Writeln('Castle Game Engine Internal: ProcessID: ', P.ProcessID);
      Flush(Output);

      P.WaitOnExit;

      ExitStatus := P.ExitStatus;
    finally
      FreeAndNil(P);
      FreeAndNil(NewEnvironment);
    end;
  end;

var
  ProcessStatus: Integer;
  AbsoluteExeName, IgnoredOutput: string;
begin
  { use FindExe to use our fixed PathFileSearch that does not accidentally find
    "ant" directory as "ant" executable }
  if IsPathAbsolute(ExeName) then
    AbsoluteExeName := ExeName
  else
  begin
    AbsoluteExeName := FindExe(ExeName);
    if AbsoluteExeName = '' then
      raise Exception.CreateFmt('Cannot find "%s" on environment variable $PATH. Make sure "%s" is installed and $PATH is configured correctly',
        [ExeName, ExeName]);
  end;

  if ForcePipesPassthrough then
    RunCommandIndirPassthrough(CurrentDirectory, AbsoluteExeName, Options,
      IgnoredOutput, ProcessStatus, OverrideEnvironmentName, OverrideEnvironmentValue)
  else
    RunCommandNoPipes(CurrentDirectory, AbsoluteExeName, Options,
      ProcessStatus, OverrideEnvironmentName, OverrideEnvironmentValue);

  // this will cause our own status be non-zero
  if ProcessStatus <> 0 then
    raise Exception.CreateFmt('Process "%s" (absolute path "%s") failed with exit status %d',
      [ExeName, AbsoluteExeName, ProcessStatus]);
end;

{ Under Unix, we need to solve the problem that
  parent process can die (and destroy child's IO handles)
  and the new process (executed by RunCommandNoWait) should stil be running OK.
  This is important when you execute in "castle-editor" the option
  to "Restart and Rebuild" editor, then "castle-editor" calls "castle-engine editor",
  and both "castle-editor" and "castle-engine" processes die
  (while the new CGE editor should continue running).

  - First solution was to execute using nohup.

    Unfortunately it doesn't work reliably enough.
    First execution of "Restart and Rebuild" is OK,
    but then doing again "Restart and Rebuild" from this editor
    (that is already under nohup) makes editor instance without output redirected to nohup.out
    (even when CurrentDirectory is random every time to make nohup.out land in different dir),
    possibly because nohup didn't detect
    that input/output should be redirected to file.

    In effect doing *again* "Restart and Rebuild" fails, as the process doesn't have
    access to own input/output.
    See https://trello.com/c/lDi33IRQ/20-linuxeditor-running-custom-editor-from-editor-gui-results-in-errors-stream-write-error-when-trying-to-run-another-child-applicat

  - New solution is to execute by shell, using shell to redirect to files.

    This works, as it always makes the run inside process (like "castle-editor") use output to files,
    so that parent (like "castle-engine editor" call) dying has no effect.

    The additional bonus is that we can put the output files in a temporary directory,
    or send output to /dev/null,
    regardless of the CurrentDirectory.
    In contrast, nohup always put nohup.out in current dir,
    forcing us to always set current dir to temporary dir (to avoid cluttering user-visible dir),
    which is not always comfortable (e.g. when editor runs build tool,
    it's easiest to set current directory = project directory).
}
{$define UNIX_RUN_NO_WAIT_BY_SHELL}

procedure RunCommandNoWait(
  const CurrentDirectory: string;
  const ExeName: string; const Options: array of string;
  const Flags: TRunCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);
var
  P: TProcess;
  I: Integer;
  {$ifdef UNIX} ShCommand, ShCommandOutput: String; {$endif}
begin
  P := TProcess.Create(nil);
  try
    P.CurrentDirectory := CurrentDirectory;

    {$if defined(UNIX) and defined(UNIX_RUN_NO_WAIT_BY_SHELL)}
    P.Executable := FindExe('sh');
    if P.Executable = '' then
      raise Exception.Create('Cannot find sh (standard shell) on $PATH');

    P.Parameters.Add('-c');

    ShCommand := '"' + ExeName + '"';
    for I := 0 to High(Options) do
      ShCommand += ' "' + Options[I] + '"';
    ShCommandOutput :=
      {$ifdef DEBUG_UNIX_RUN_NO_WAIT_BY_SHELL} InclPathDelim(CreateTemporaryDir) + 'run-process-no-wait-' + IntToStr(Random(100000)) + '.log'
      {$else} '/dev/null'
      {$endif};
    ShCommand += '< /dev/null > ' + ShCommandOutput + ' 2>&1';
    P.Parameters.Add(ShCommand);

    {$else}
    P.Executable := ExeName;

    { Old Unix solution with nohup, see above comments at UNIX_RUN_NO_WAIT_BY_SHELL }
    {$ifdef UNIX}
    NoHupExe := FindExe('nohup');
    if NoHupExe <> '' then
    begin
      P.Executable := NoHupExe;
      P.Parameters.Add(ExeName);
    end;
    {$endif}

    for I := Low(Options) to High(Options) do
      P.Parameters.Add(Options[I]);
    {$endif}

    { Under Windows, these options should make a process execute OK.
      Following http://wiki.lazarus.freepascal.org/Executing_External_Programs . }
    P.InheritHandles := false;
    P.ShowWindow := swoShow;
    if rcNoConsole in Flags then
      P.Options := P.Options + [poNoConsole];
    if OverrideEnvironment <> nil then
      P.Environment := OverrideEnvironment;

    WritelnVerbose('Calling ' + P.Executable); // show P.Executable, not ExeName, as code above may set other P.Executable
    WritelnVerbose('  With Working Directory: ' + P.CurrentDirectory);
    WritelnVerbose(P.Parameters.Text);

    P.Execute;
  finally FreeAndNil(P) end;
end;

function CreateTemporaryDir: string;
begin
  Result := InclPathDelim(GetTempDir(false)) +
    ApplicationName + IntToStr(Random(1000000));
  CheckForceDirectories(Result);
  WritelnVerbose('Created temporary dir for package: ' + Result);
end;

const
  AlphaNum = ['a'..'z', 'A'..'Z', '0'..'9'];

function MakeQualifiedName(ProjectName: String): String;
const
  { See ToolProject constant in CGE build tool. }
  QualifiedNameAllowedChars = AlphaNum + ['.'];
  QualifiedNameAllowedCharsFirst = QualifiedNameAllowedChars - ['.', '0'..'9'];
begin
  ProjectName := SDeleteChars(ProjectName, AllChars - QualifiedNameAllowedChars);
  if (ProjectName <> '') and not (ProjectName[1] in QualifiedNameAllowedCharsFirst) then
    ProjectName := 'project' + ProjectName;
  if ProjectName = '' then
    ProjectName := 'project'; // if ProjectName is left empty after above deletions, set it to anything
  Result := 'com.mycompany.' + ProjectName;
end;

function MakeProjectPascalName(ProjectName: String): String;
const
  ValidProjectPascalNameChars = AlphaNum + ['_'];
  ValidProjectPascalNameCharsFirst = ValidProjectPascalNameChars - ['0'..'9'];
begin
  ProjectName := SReplaceChars(ProjectName, AllChars - ValidProjectPascalNameChars, '_');
  if (ProjectName <> '') and not (ProjectName[1] in ValidProjectPascalNameCharsFirst) then
    ProjectName := 'project' + ProjectName;
  if ProjectName = '' then
    ProjectName := 'project'; // if ProjectName is left empty after above deletions, set it to anything
  Result := ProjectName;
end;

end.
