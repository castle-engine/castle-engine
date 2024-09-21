{
  Copyright 2021-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Editor and build tool utilities to manage process recognition / waiting. }
unit ToolProcess;

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

{ Stop (kill) given process.

  Does not guarantee that the process will be killed, but makes reasonable effort.
  If it fails, it just reports a warning using WritelnWarning,
  and continues without any exception.
  The purpose for now is to stop running application from CGE editor,
  it can assume that application is not "incredibly stubborn" in refusing to quit.

  On Unix this right now sends SIGTERM (but *does not* follow with more brutal SIGKILL).

  On Windows it uses TerminateProcess.  }
procedure StopProcess(const ProcessId: TProcessId);

implementation

uses
  {$ifdef MSWINDOWS} Windows {$endif}
  {$ifdef UNIX} CTypes, BaseUnix {$endif}
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
    // once process stops existing, kill will return -1, and FpGetErrno = ESRCH = ESysESRCH
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

procedure StopProcess(const ProcessId: TProcessId);
{$ifdef MSWINDOWS}
var
  LastError: Integer;
  ProcessHandle: THandle;
begin
  { We need to change ProcessId (DWORD in WinAPI, unique process id in system)
    to a handle we can use with TerminateProcess. }
  ProcessHandle := OpenProcess(PROCESS_TERMINATE, WinBool(false), ProcessId);
  if ProcessHandle = 0 then
  begin
    LastError := GetLastOSError;
    WritelnWarning('Getting process handle (id: %d) to terminate it failed: WinAPI error %d: %s', [
      ProcessId,
      LastError,
      SysErrorMessage(LastError)
    ]);
    Exit;
  end;

  if not TerminateProcess(ProcessHandle, 0) then
  begin
    LastError := GetLastOSError;
    WritelnWarning('Stopping process (handle: %d) by TerminateProcess failed: WinAPI error %d: %s', [
      ProcessHandle,
      LastError,
      SysErrorMessage(LastError)
    ]);
  end;
{$endif}

{$ifdef UNIX}
var
  LastError: Integer;
begin
  if FpKill(ProcessId, SIGTERM) = 0 then
  begin
    { Sending SIGKILL right after SIGTERM is a bit brutal,
      i.e. we didn't give the ProcessId any time to react to SIGTERM and close nicely.
      But we don't want to introduce some delay here,
      we want TerminateChildrenHarder to be instant, to be responsive to user.
      So for now, we just don't follow it with SIGKILL.
    }

    (*
    if Process.Running then
    begin
      OutputList.AddLine(Format('Killing child process (id: %d) by SIGKILL', [ProcessId]), okInfo);
      if FpKill(ProcessId, SIGKILL) <> 0 then
        OutputList.AddLine(Format('Cannot send SIGKILL to child process (id: %d)', [ProcessId]), okWarning);
    end;
    *)
  end else
  begin
    LastError := GetLastOSError;
    WritelnWarning('Cannot send SIGTERM to child process (id: %d). OS error %d: %s', [
      ProcessId,
      LastError,
      SysErrorMessage(LastError)
    ]);
  end;
{$endif}

end;

end.
