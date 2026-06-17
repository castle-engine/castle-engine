{%MainUnit castleinternalprocess.pas}
{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Minimal TCastleProcess implementation on Unix, using the classic
  fork + exec POSIX API exposed by Delphi Posix.* units. }

{ Convert a list of Pascal strings into a NULL-terminated array of C strings
  (UTF-8 encoded), as expected by execvp / execve.
  We use this both for command-line arguments and for environment variables,
  as C API expects them in the same way.

  The UTF-8 buffers backing the C strings are returned in Buffers, which must be
  kept alive (not freed, not reused) as long as the returned array is used. }
procedure StringsToArgv(const Strings: TStrings;
  out Argv: TArray<PAnsiChar>; out Buffers: TArray<UTF8String>);
var
  I: Integer;
begin
  SetLength(Buffers, Strings.Count);
  SetLength(Argv, Strings.Count + 1);
  for I := 0 to Strings.Count - 1 do
  begin
    Buffers[I] := UTF8Encode(Strings[I]);
    Argv[I] := PAnsiChar(Buffers[I]);
  end;
  Argv[Strings.Count] := nil; // NULL terminator
end;

procedure TCastleProcess.Execute;
var
  ExeAndParameters: TCastleStringList;
  Argv, Envp: TArray<PAnsiChar>;
  ArgvBuffers, EnvpBuffers: TArray<UTF8String>;
  EnvpPtr: PPAnsiChar;
  CurrentDirUtf8: UTF8String;
begin
  { Prepare argv: executable name followed by parameters.
    Done before fork(), so that the child only needs async-signal-safe calls.

    See https://man7.org/linux/man-pages/man7/signal-safety.7.html about
    async-signal-safe functions.
    The care about async-signal-safe functions is "just in case"
    here, if the child process is executed in from a multi-threaded parent.
    See https://pubs.opengroup.org/onlinepubs/9699919799/functions/fork.html
    """
    If a multi-threaded process calls fork(), the new process shall contain
    a replica of the calling thread and its entire address space,
    possibly including the states of mutexes and other resources.
    Consequently, to avoid errors, the child process may only execute
    async-signal-safe operations until such time as one of the exec functions
    is called.
    """ }
  ExeAndParameters := TCastleStringList.Create;
  try
    ExeAndParameters.Add(Executable);
    ExeAndParameters.AddRange(FParameters);
    StringsToArgv(ExeAndParameters, Argv, ArgvBuffers);
  finally FreeAndNil(ExeAndParameters) end;

  { Prepare envp, only if Environment is specified.
    Otherwise leave EnvpPtr = nil to inherit the current environment. }
  EnvpPtr := nil;
  if FEnvironment.Count <> 0 then
  begin
    StringsToArgv(FEnvironment, Envp, EnvpBuffers);
    EnvpPtr := @Envp[0];
  end;

  { Prepare the current directory C string before fork(),
    so that the child only needs the async-signal-safe chdir call.
    As above -- the care about async-signal-safe functions is "just in case"
    here, if the child process is executed in from a multi-threaded parent. }
  CurrentDirUtf8 := UTF8Encode(FCurrentDirectory);

  FHandle := fork();
  if FHandle = -1 then
    raise EProcess.CreateFmt('Failed to fork to execute "%s": %s', [
      Executable,
      SysErrorMessage(GetLastError)
    ]);

  if FHandle = 0 then
  begin
    { This code runs in a child process: replace our image with the requested executable.
      We must use _exit (not Halt or Exit), to not run any Pascal finalization
      or flush file buffers inherited (duplicated) from the parent. }

    { Change to the requested current directory, if any.
      On failure (e.g. directory doesn't exist) abort the child instead of
      running the program in the wrong directory. }
    if (FCurrentDirectory <> '') and (__chdir(PAnsiChar(CurrentDirUtf8)) <> 0) then
      _exit(127);

    if EnvpPtr <> nil then
      execve(Argv[0], @Argv[0], EnvpPtr)
    else
      execvp(Argv[0], @Argv[0]);
    { This line is reached only when exec failed (e.g. file not found).
      In normal case, execve/execvp does not return, the child process
      executes the indicated program now, not our code anymore. }
    _exit(127);
  end;

  { This code runs in the parent process: continue as usual. }
end;

function TCastleProcess.Terminate(const AExitCode: Integer): Boolean;
begin
  Result :=
    (FHandle <> -1) and
    (FHandle <> 0) and
    (kill(FHandle, SIGKILL) = 0);
end;

function TCastleProcess.WaitOnExit: Boolean;
var
  Status, WaitResult: Integer;
begin
  if (FHandle = -1) or (FHandle = 0) then
    Exit(false);

  Status := 0;
  { Retry if waitpid was interrupted by a signal. }
  repeat
    WaitResult := waitpid(FHandle, @Status, 0);
  until (WaitResult <> -1) or (GetLastError <> EINTR);

  Result := WaitResult = FHandle;

  { Record the process exit code, see ExitStatus docs.
    Only meaningful when the process exited normally (not killed by a signal). }
  if Result and WIFEXITED(Status) then
    FExitStatus := WEXITSTATUS(Status);
end;
