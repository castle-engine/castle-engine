{
  Copyright 2014-2019 Michalis Kamburelis.

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

uses CastleStringUtils;

var
  { Trivial verbosity global setting. }
  Verbose: boolean = false;

{ In case of console application with Verbose, writes to stduot.
  Otherwise, only to WritelnLog. }
procedure WritelnVerbose(const S: String);

{ Like @link(FindExe), but additionally look for the exe in
  Castle Game Engine bin/ subdirectory. }
function FindExeCastleTool(const ExeName: String): String;

{ Path to CGE main directory.
  Autodetected or obtained from $CASTLE_ENGINE_PATH environment variable.

  Returns empty String if it wasn't possible to get a valid value.
  Otherwise, the returned path always ends with path delimiter,
  and always exists. }
function CastleEnginePath: String;

{ Run command in given directory with given arguments,
  gathering output and status to string.
  Also gathers error output to the same string.
  Like Process.RunCommandIndir in FPC >= 2.6.4, but also captures error output.

  @param(ExeName Executable name, should be an absolute filename
    e.g. found by FindExe. This will avoid FPC errors (the default FPC
    algorithm searching $PATH may mistake binary for a directory with
    the same name).)

  @param(OutputString Standard output (stdout) and standard error output
    (stderr) of the command.)
}
procedure MyRunCommandIndir(
  const CurDir: string; const ExeName: string;
  const Options: array of string;
  out OutputString: string; out ExitStatus: integer);

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
  const CurDir: string; const ExeName: string;
  const Options: array of string;
  var OutputString:string; var ExitStatus:integer;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '');

{ Run command in given (or current) directory with given arguments,
  letting output (stdout and stderr) to go to our stdout.
  Command is searched on $PATH following standard OS conventions,
  if it's not already an absolute filename.
  Raises exception if command fails (detected by exit code <> 0). }
procedure RunCommandSimple(
  const ExeName: string; const Options: array of string); overload;
procedure RunCommandSimple(
  const CurDir: string; const ExeName: string; const Options: array of string;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = ''); overload;

{ Run the command, and return immediately, without waiting for finish.
  On Unix, TempPath is used as a location of temporary "nohup" file, it should exist. }
procedure RunCommandNoWait(
  const TempPath: string;
  const ExeName: string; const Options: array of string);

{ Determine and create a new (unique, with random number in the name) temp directory. }
function CreateTemporaryDir: string;

implementation

uses Classes, SysUtils, Process,
  CastleFilesUtils, CastleUtils, CastleURIUtils, CastleLog;

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

function GetCastleEnginePathFromExeName: String;

  function CheckCastlePath(const Path: String): Boolean;
  begin
    Result :=
      DirectoryExists(Path + 'src') and
      DirectoryExists(Path + 'tools' + PathDelim + 'build-tool' + PathDelim + 'data');
  end;

begin
  try
    // knowingly using deprecated ExeName, that should be non-deprecated and internal here
    {$warnings off}

    { Check ../ of current exe, makes sense in released CGE version when
      tools are precompiled in bin/ subdirectory. }
    Result := InclPathDelim(ExtractFileDir(ExtractFileDir(ExeName)));
    if CheckCastlePath(Result) then
      Exit;
    { Check ../ of current exe, makes sense in development when
      each tool is compiled by various scripts in tools/xxx/ subdirectory. }
    Result := InclPathDelim(ExtractFileDir(ExtractFileDir(ExtractFileDir(ExeName))));
    if CheckCastlePath(Result) then
      Exit;

    {$warnings on}

    Result := '';
  except
    on EExeNameNotAvailable do
      WritelnVerbose('Cannot detect CGE path because ExeName not available on this platform, and $CASTLE_ENGINE_PATH not defined');
  end;
end;

var
  CastleEnginePathIsCached: Boolean;
  CastleEnginePathCached: String;

function CastleEnginePath: String;
begin
  if CastleEnginePathIsCached then
    Result := CastleEnginePathCached
  else
  begin
    Result := GetCastleEnginePathFromEnv;
    if Result = '' then // if not $CASTLE_ENGINE_PATH, try to find CGE harder
      Result := GetCastleEnginePathFromExeName;

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

procedure MyRunCommandIndir(const CurDir: string;
  const ExeName: string;const Options: array of string;
  out OutputString: string; out ExitStatus: integer);
{ Adjusted from fpc/trunk/packages/fcl-process/src/process.pp }
Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.
var
  p : TProcess;
  i : integer;
  numbytes,bytesread : integer;
begin
  // default out values
  OutputString := '';
  ExitStatus := 0;

  p:=TProcess.create(nil);
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(Options)>=0 then
   for i:=low(Options) to high(Options) do
     p.Parameters.add(Options[i]);
  WritelnVerbose('Calling ' + ExeName);
  WritelnVerbose(P.Parameters.Text);

  try
    try
      p.Options := [poUsePipes, poStderrToOutPut];
      bytesread := 0;
      p.Execute;
      while p.Running do
      begin
        Setlength(OutputString,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(OutputString[1+bytesread], READ_BYTES);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes) else
          Sleep(100);
      end;
      repeat
        Setlength(OutputString,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(OutputString[1+bytesread], READ_BYTES);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
      until NumBytes <= 0;
      setlength(OutputString,BytesRead);
      ExitStatus:=p.ExitStatus;
    except
      on e : Exception do
      begin
        setlength(OutputString,BytesRead);
        raise;
      end;
    end;
  finally p.free end;
end;

procedure RunCommandIndirPassthrough(const CurDir: string;
  const ExeName: string;
  const Options: array of string;
  var OutputString:string; var ExitStatus:integer;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '');
{ Adjusted from fpc/trunk/packages/fcl-process/src/process.pp }
Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.
var
  p : TProcess;
  i : integer;
  numbytes,bytesread : integer;
  NewEnvironment: TStringList;
begin
  p:=TProcess.create(nil);
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(Options)>=0 then
   for i:=low(Options) to high(Options) do
     p.Parameters.add(Options[i]);
  WritelnVerbose('Calling ' + ExeName);
  WritelnVerbose(P.Parameters.Text);

  NewEnvironment := nil;
  try
    if OverrideEnvironmentName <> '' then
    begin
      NewEnvironment := TStringList.Create;
      for I := 1 to GetEnvironmentVariableCount do
        NewEnvironment.Add(GetEnvironmentString(I));
      NewEnvironment.Values[OverrideEnvironmentName] := OverrideEnvironmentValue;
      P.Environment := NewEnvironment;
      // WritelnVerbose('Environment: ' + P.Environment.Text);
    end;

    try
      p.Options := [poUsePipes, poStderrToOutPut];
      bytesread := 0;
      p.Execute;
      while p.Running do
      begin
        Setlength(OutputString,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(OutputString[1+bytesread], READ_BYTES);
        Write(Copy(OutputString, 1+bytesread, NumBytes)); // passthrough
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes) else
          Sleep(100);
      end;
      repeat
        Setlength(OutputString,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(OutputString[1+bytesread], READ_BYTES);
        Write(Copy(OutputString, 1+bytesread, NumBytes)); // passthrough
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
      until NumBytes <= 0;
      setlength(OutputString,BytesRead);
      ExitStatus:=p.ExitStatus;
    except
      on e : Exception do
      begin
        setlength(OutputString,BytesRead);
        raise;
      end;
    end;
  finally
    FreeAndNil(p);
    FreeAndNil(NewEnvironment);
  end;
end;

procedure RunCommandSimple(
  const ExeName: string; const Options: array of string);
begin
  RunCommandSimple(GetCurrentDir, ExeName, Options);
end;

procedure RunCommandSimple(
  const CurDir: string; const ExeName: string; const Options: array of string;
  const OverrideEnvironmentName: string = '';
  const OverrideEnvironmentValue: string = '');
var
  ProcessOutput: string;
  ProcessStatus: Integer;
  AbsoluteExeName: string;
begin
  { use FindExe to use our fixed PathFileSearch that does not accidentaly find
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

  RunCommandIndirPassthrough(CurDir, AbsoluteExeName, Options,
    ProcessOutput, ProcessStatus, OverrideEnvironmentName, OverrideEnvironmentValue);
  if ProcessStatus <> 0 then
    raise Exception.CreateFmt('"%s" (on $PATH as "%s") call failed with exit status %d',
      [ExeName, AbsoluteExeName, ProcessStatus]);
end;

procedure RunCommandNoWait(
  const TempPath: string;
  const ExeName: string; const Options: array of string);
var
  P: TProcess;
  I: Integer;
  {$ifdef UNIX} NoHupExe: String; {$endif}
begin
  P := TProcess.Create(nil);
  try
    P.Executable := ExeName;
    // this is useful on Unix, to place nohup.out inside temp directory
    P.CurrentDirectory := TempPath;

    { Under Unix, execute using nohup.
      This way the parent process can die (and destroy child's IO handles)
      and the new process will keep running OK.
      This is important when you execute in "castle-editor" the option
      to "Restart and Rebuild" editor, then "castle-editor" calls "castle-engine editor",
      and both "castle-editor" and "castle-engine" processes die
      (while the new CGE editor should continue running). }
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

    { Under Windows, these options should make a process execute OK.
      Following http://wiki.lazarus.freepascal.org/Executing_External_Programs . }
    P.InheritHandles := false;
    P.ShowWindow := swoShow;

    WritelnVerbose('Calling ' + ExeName);
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

end.
