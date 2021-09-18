{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Editor and build tool utilities to manage process recognition / waiting. }
unit ToolProcessWait;

interface

type
  { Type wide enough to contain process id on all platforms.
    Not defined with $ifdef,
    because we pass it through command-line and thus want to easily convert string<->integer,
    so it's not opaque anyway, we just need to explicitly decide on integer type
    that can fit everything.

    On Windows this is DWORD ( https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-getcurrentprocessid ).

    On Unix this is pid_t ( https://man7.org/linux/man-pages/man3/pid_t.3.html )
    which should be signed and no greater than CLong. In practice it is 32 or 64 bit signed
    integer. }
  TProcessId = Int64;

{ Get system-wide process identifier, that another process could use with WaitForProcessExit. }
function CurrentProcessId: TProcessId;

{ Wait (block execution) until another process finishes. }
procedure WaitForProcessExit(const ProcessId: TProcessId);

implementation

uses
  {$ifdef MSWINDOWS} Windows {$endif}
  {$ifdef UNIX} BaseUnix {$endif}
  , SysUtils,
  CastleUtils, CastleLog;

function CurrentProcessId: TProcessId;
begin
  Result :=
    {$ifdef MSWINDOWS} GetCurrentProcessId()
    {$else}
      {$ifdef UNIX} FpGetPid()
      {$else} 0
      {$endif}
    {$endif};
end;

procedure WaitForProcessExit(const ProcessId: TProcessId);
{$ifdef MSWINDOWS}
const
  WaitSeconds = 10;
var
  ProcessHandle: THandle;
  WaitResult, WinError: DWord;
begin
  ProcessHandle := OpenProcess(Windows.SYNCHRONIZE, false, ProcessId);
  if ProcessHandle <> 0 then // otherwise assume it has already terminated
  begin
    WaitResult := WaitForSingleObject(ProcessHandle, WaitSeconds * 1000);
    case WaitResult of
      WAIT_OBJECT_0: Exit; // OK, process finished
      WAIT_TIMEOUT:
        raise Exception.CreateFmt('Waited for process for %d seconds, without success. Aborting.', [
          WaitSeconds
        ]);
      WAIT_FAILED:
        begin
          WinError := GetLastError;
          raise Exception.CreateFmt('Waiting for process failed with error %d: %s.', [
            WinError,
            SysErrorMessage(WinError)
          ]);
        end;
      else
        raise Exception.CreateFmt('Waiting for process failed with unexpected error from WaitForSingleObject: %d.', [
          WaitResult
        ]);
    end;
  end;

{$else}
{$ifdef UNIX}

(* // This is only suitable for child processes, which is not our case.
var
  ProcessStatus, Res: CInt;
  Err: LongInt;
begin
  Res := FpWaitPid(ProcessId, @ProcessStatus, WNOHANG);
  if Res <> ProcessId then
  begin
    Err := FpGetErrno;
    raise Exception.CreateFmt('Waiting for process failed with error %d: %s.', [
      Err,
      SysErrorMessage(Err)
    ]);
  end;
*)

// Send a signal to test whether process works.
// SIGCHLD is ignored by default ( https://en.wikipedia.org/wiki/Signal_(IPC) ).
var
  Res: CInt;
begin
  repeat
    Res := FpKill(ProcessId, SIGCHLD);
    // once process stops existing, kill will return -1, and FpGetErrno = ESRCH
    if Res = -1 then
      Exit;
    Sleep(100);
  until false;

{$else}
begin
  WritelnWarning('Cannot wait for process on this platform, assuming it is OK to not wait.');
{$endif}
{$endif}
end;

end.
