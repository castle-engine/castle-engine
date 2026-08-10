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

{ Minimal TCastleProcess implementation on Windows, using WinAPI CreateProcess. }

{ Quote a single argument following the rules of the Microsoft C/C++ runtime
  and CommandLineToArgvW, so that the child process (when it parses the
  command-line back into arguments) sees exactly the given string.

  See https://learn.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-commandlinetoargvw
  and https://devblogs.microsoft.com/oldnewthing/20100917-00/?p=12833 for
  the rules and reasons.
  Yes, they are really weird, with "2n backslashes followed by a quotation mark...". }
function WindowsQuoteArgument(const S: String): String;
var
  I, BackslashCount: Integer;
begin
  { Empty argument, or argument without any special characters, is passed as-is.
    Only when it contains spaces, tabs or quotes we need to wrap it in quotes. }
  if (S <> '') and
     (Pos(' ', S) = 0) and
     (Pos(#9, S) = 0) and
     (Pos('"', S) = 0) then
    Exit(S);

  Result := '"';
  I := 1;
  while I <= Length(S) do
  begin
    { Count a run of backslashes. }
    BackslashCount := 0;
    while (I <= Length(S)) and (S[I] = '\') do
    begin
      Inc(BackslashCount);
      Inc(I);
    end;

    if I > Length(S) then
    begin
      { Backslashes at the end of the string: double them, so they are not
        interpreted as escaping the closing quote we add below. }
      Result := Result + StringOfChar('\', BackslashCount * 2);
      Break;
    end else
    if S[I] = '"' then
    begin
      { Backslashes before a quote: double them, then escape the quote itself. }
      Result := Result + StringOfChar('\', BackslashCount * 2 + 1) + '"';
      Inc(I);
    end else
    begin
      { Backslashes not followed by a quote are not special: emit them verbatim. }
      Result := Result + StringOfChar('\', BackslashCount) + S[I];
      Inc(I);
    end;
  end;
  Result := Result + '"';
end;

{ Build a single command-line string from the executable and parameters,
  with proper quoting for CreateProcess. }
function BuildCommandLine(const Executable: String;
  const Parameters: TStrings): String;
var
  I: Integer;
begin
  Result := WindowsQuoteArgument(Executable);
  for I := 0 to Parameters.Count - 1 do
    Result := Result + ' ' + WindowsQuoteArgument(Parameters[I]);
end;

{ Build the environment block for CreateProcess from the given strings.
  The block is a sequence of #0-terminated "name=value" strings,
  terminated by an additional #0.
  Returns '' (empty) when Env is empty, in which case the caller should pass
  nil to CreateProcess to inherit the current process environment. }
function BuildEnvironmentBlock(const Env: TStrings): String;
var
  I: Integer;
begin
  Result := '';
  if Env.Count = 0 then
    Exit;
  for I := 0 to Env.Count - 1 do
    Result := Result + Env[I] + #0;
  Result := Result + #0;
end;

procedure TCastleProcess.Execute;
var
  CommandLine, EnvironmentBlock: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CreationFlags: DWord;
  EnvironmentPtr: Pointer;
  CurrentDirPtr: PChar;
begin
  CommandLine := BuildCommandLine(Executable, FParameters);

  { CreateProcess may modify the command-line buffer in place,
    so make sure we pass a writable, uniquely-referenced string.
    See https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessw
    """
    The Unicode version of this function, CreateProcessW, can modify the contents of this string.
    """
    (again, WinAPI is crazy). }
  UniqueString(CommandLine);

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);

  CreationFlags := 0;
  EnvironmentBlock := BuildEnvironmentBlock(FEnvironment);
  if EnvironmentBlock <> '' then
  begin
    EnvironmentPtr := PChar(EnvironmentBlock);
    { Our strings are Unicode (PChar = PWideChar, we're in Delphi),
      so the environment block is a block of WideChar and must be flagged
      as such. }
    CreationFlags := CreationFlags or CREATE_UNICODE_ENVIRONMENT;
  end else
    EnvironmentPtr := nil;

  { When CurrentDirectory is empty, pass nil to inherit our current directory. }
  if FCurrentDirectory <> '' then
    CurrentDirPtr := PChar(FCurrentDirectory)
  else
    CurrentDirPtr := nil;

  if not CreateProcess(
       nil, // lpApplicationName: take it from the CommandLine passed as next param
       PChar(CommandLine),
       nil, // lpProcessAttributes
       nil, // lpThreadAttributes
       true, // bInheritHandles: let the child inherit our stdin/stdout/stderr
       CreationFlags,
       EnvironmentPtr,
       CurrentDirPtr,
       StartupInfo,
       ProcessInfo) then
    raise EProcess.CreateFmt('Failed to execute application "%s": %s', [
      Executable,
      SysErrorMessage(GetLastError)
    ]);

  { We don't need the thread handle, close it immediately. }
  CloseHandle(ProcessInfo.hThread);

  FHandle := ProcessInfo.hProcess;
end;

function TCastleProcess.Terminate(const AExitCode: Integer): Boolean;
begin
  Result := (FHandle <> 0) and
    TerminateProcess(FHandle, AExitCode);
end;

function TCastleProcess.WaitOnExit: Boolean;
var
  ExitCode: DWord;
begin
  Result :=
    (FHandle <> 0) and
    (WaitForSingleObject(FHandle, INFINITE) = WAIT_OBJECT_0);
  { Record the process exit code, see ExitStatus docs. }
  if Result and GetExitCodeProcess(FHandle, ExitCode) then
    FExitStatus := ExitCode;
end;
