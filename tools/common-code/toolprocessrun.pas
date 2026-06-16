{
  Copyright 2014-2026 Michalis Kamburelis.

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

{ Editor and build tool utilities to run processes, on top of
  CastleInternalProcess unit. }
unit ToolProcessRun;

interface

uses Classes,
  CastleStringUtils, CastleInternalProcess;

var
  ForcePipesPassthrough: Boolean = false;

{ Run command in given (or current) directory with given arguments,
  letting output (stdout and stderr) to go to our stdout / stderr.
  Waits for the command to finish.

  This is similar to the standard FPC ExecuteProcess.

  ExeName is either an absolute (with extension) filename or merely
  a name (without extension) to be searched on $PATH following standard OS conventions.
  Just like for @link(ExecuteCommand).

  Raises exception if command fails (detected by exit code <> 0).

  Global variable @link(ForcePipesPassthrough) can be used to force
  using pipes to communicate with the process (instead of just letting
  child process to use our stdout/stderr).
  It should not be necessary (and it may introduce some performance drop,
  though I didn't observe any in practice)... except on Windows when you run
  "castle-engine run" in PowerShell in VS Code.
  For some reason, "castle-engine run" in CGE editor doesn't need it.

  Writelns "magic" string 'Castle Game Engine Internal: ProcessID: '
  that is helpful for CGE editor to detect the process ID of the child
  process of "castle-engine run" and thus perform "Stop" more reliably.

  TODO: Some things make it "not good enough" to move to more general
  CastleInternalProcess:
  - "magic" string 'Castle Game Engine Internal: ProcessID: '
  - ForcePipesPassthrough complication, the need for it should be reseached,
    and either do it automatically on Windows, or remove.
  - maybe also introduce rename then:
    ExecuteCommandSimple -> Execute
    ExecuteCommand -> ExecuteCaptureOutput
}
procedure ExecuteCommandSimple(
  const ExeName: String; const Options: array of string); overload;
procedure ExecuteCommandSimple(
  const CurrentDirectory: String; ExeName: String; const Options: array of string;
  const OverrideEnvironment: TStringList = nil); overload;

{ Run the command, and return immediately, without waiting for it to finish.

  TODO: Not suitable to move to CastleInternalProcess, as implementation
  on Unix does more hacks that I'd like (sh, nohup). Ideally this should be
  a simpler wrapper around TCastleProcess. }
procedure ExecuteCommandNoWait(
  const CurrentDirectory: String;
  ExeName: String; const Options: array of string;
  const Flags: TExecuteCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);

implementation

uses SysUtils,
  CastleFilesUtils, CastleUtils, CastleUriUtils, CastleLog,
  CastleInternalArchitectures,
  ToolCommonUtils;

procedure ExecuteCommandSimple(
  const ExeName: String; const Options: array of string);
begin
  ExecuteCommandSimple(GetCurrentDir, ExeName, Options);
end;

procedure ExecuteCommandSimple(
  const CurrentDirectory: String; ExeName: String; const Options: array of string;
  const OverrideEnvironment: TStringList = nil);

  { Run the given command, wait for it.
    The stdout/stderr is not captured to any string,
    it is only passed-through as our stdout/stderr.

    This is in contrast to ExecuteCommand that captures output to String. }
  procedure ExecuteNoPipes(const CurrentDirectory: String;
    ExeName: String;
    const Options: array of string;
    var ExitStatus: Integer;
    const OverrideEnvironment: TStringList = nil);
  var
    P: TCastleProcess;
    I: Integer;
  begin
    ExeName := AbsoluteExeName(ExeName);

    P := TCastleProcess.Create(nil);
    try
      P.Executable := ExeName;
      if CurrentDirectory <> '' then
        P.CurrentDirectory := CurrentDirectory;
      if High(Options) >= 0 then
       for I := Low(Options) to High(Options) do
         P.Parameters.Add(Options[I]);
      WritelnVerbose('Calling ' + ExeName);
      WritelnVerbose(P.Parameters.Text);

      if OverrideEnvironment <> nil then
        P.Environment := OverrideEnvironment;

      P.Execute;

      Writeln('Castle Game Engine Internal: ProcessID: ', P.ProcessID);
      Flush(Output);

      P.WaitOnExit;

      ExitStatus := P.ExitStatus;
    finally
      FreeAndNil(P);
    end;
  end;

var
  ProcessStatus: Integer;
  IgnoredOutput: String;
begin
  if ForcePipesPassthrough then
    { TODO: In this case, we don't make
        Writeln('Castle Game Engine Internal: ProcessID: ', P.ProcessID);
      Though it doesn't seem a problem for anything on Windows.
      In practice this is used only to reliably doing "Stop" from CGE editor
      on running "castle-engine run" (for desktop or Android apps),
      and these work OK on Windows too. }
    ExecuteCommand(CurrentDirectory, ExeName, Options,
      IgnoredOutput, ProcessStatus, nil, nil, [], OverrideEnvironment, true)
  else
    ExecuteNoPipes(CurrentDirectory, ExeName, Options,
      ProcessStatus, OverrideEnvironment);

  // this will cause our own status be non-zero
  if ProcessStatus <> 0 then
    raise Exception.CreateFmt('Process "%s" failed with exit status %d',
      [ExeName, ProcessStatus]);
end;

{ Under Unix, we need to solve the problem that
  parent process can die (and destroy child's IO handles)
  and the new process (executed by ExecuteCommandNoWait) should stil be running OK.
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

{ Define to send output (when using UNIX_RUN_NO_WAIT_BY_SHELL)
  to a temporary file (whose filename is printed in verbose logs).
  This is useful for debugging, as you can check the output of the process.
  Otherwise, we send output to /dev/null. }
{.$define DEBUG_UNIX_RUN_NO_WAIT_BY_SHELL}

procedure ExecuteCommandNoWait(
  const CurrentDirectory: String;
  ExeName: String; const Options: array of string;
  const Flags: TExecuteCommandFlags = [];
  const OverrideEnvironment: TStringList = nil);
var
  P: TCastleProcess;
  I: Integer;
  {$ifdef UNIX} ShCommand, ShCommandOutput: String; {$endif}
begin
  ExeName := AbsoluteExeName(ExeName);

  P := TCastleProcess.Create(nil);
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
    ShCommand += ' < /dev/null > ' + ShCommandOutput + ' 2>&1';
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
    if ecNoConsole in Flags then
      P.Options := P.Options + [poNoConsole];
    if OverrideEnvironment <> nil then
      P.Environment := OverrideEnvironment;

    WritelnVerbose('Calling ' + P.Executable); // show P.Executable, not ExeName, as code above may set other P.Executable
    WritelnVerbose('  With Working Directory: ' + P.CurrentDirectory);
    WritelnVerbose(P.Parameters.Text);

    P.Execute;
  finally FreeAndNil(P) end;
end;

end.