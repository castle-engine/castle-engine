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

{ Running processes, using @link(TCastleProcess) class
  or simple routines like @link(ExecuteCommand), @link(ExecuteCommandCapture)
  and others.

  This is implemented in various ways depending on the compiler / platform:

  @unorderedlist(
    @item(When compiled with FPC, on platforms where FPC's Process unit is available,
      TCastleProcess descends from TProcess and offers just some improvements
      on top.
    )

    @item(TODO: When compiled with Delphi on Windows or Linux,
      we implement a simple TCastleProceess on our side, with a compatible API.
    )

    @item(
      On platforms where TCastleProcess is truly not implemented
      (maybe even not possible, if "running processes" doesn't make sense,
      e.g. on web) than this unit still compiles,
      and TCastleProcess is an empty class (no Execute etc.).
      You can guard from it using the CASTLE_PROCESS_AVAILABLE define from
      castleconf.inc.
    )
  )

  Throughout Castle Game Engine code, always use this unit instead of
  FPC's Process unit, TProcess class or @code(ExecuteProcess) utility.
  This way our code is Delphi-compatible, you benefit from some
  @link(TCastleProcess) improvements,
  and our family of @code(ExecuteCommand) and @code(ExecuteCommandCapture)
  routines offer some extra features
  (compared to FPC's @code(RunCommand) routines). }
unit CastleInternalProcess;

{$I castleconf.inc}

{ Is this FPC with Process unit.
  Below we rely on CASTLE_PROCESS_AVAILABLE being set based on platform
  in castleconf.inc. }
{$if defined(FPC) and defined(CASTLE_PROCESS_AVAILABLE)}
  {$define HAS_STANDARD_PROCESS}
{$endif}

{ Implement own minimal TCastleProcess here. }
{$if defined(CASTLE_PROCESS_AVAILABLE) and not defined(HAS_STANDARD_PROCESS)}
  {$define MINIMAL_PROCESS_IMPLEMENTATION}
{$endif}

{ On Windows, if application execution is blocked by "Smart App Control",
  ancestor TProcess makes super-unfriendly exception message
  "Failed to execute  : 4551" by doing:

    SErrCannotExecute= 'Failed to execute %s : %d'
    ....
    Raise EProcess.CreateFmt(SErrCannotExecute,[FCommandLine,GetLastError])

  Note that FCommandLine is empty in most cases,
  because we don't use deprecated CommandLine, making this error message even
  more cryptic.

  See more about "Smart App Control", it in practice blocks unsigned apps:
  - https://support.microsoft.com/en-us/windows/smart-app-control-frequently-asked-questions-285ea03d-fa88-4d56-882e-6698afdb7003
  - https://www.guidingtech.com/enable-or-disable-smart-app-control-in-windows-11/
  - https://www.reddit.com/r/Windows11/comments/1s2t50a/comment/ochifwy/

  Let's detect and convert it to something helpful for users,
  an exception with nicer message.
  We will make exception EWindowsSmartAppControlProtection with an improved
  message in this case.
  This may happen in many real cases -- when editor cannot execute FPC,
  lazbuild, or built user application, all possible when engine tools are not
  properly signed. }
{$if defined(HAS_STANDARD_PROCESS) and defined(MSWINDOWS)}
  {$define IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES}
{$endif}

interface

uses SysUtils, Classes,
  {$ifdef HAS_STANDARD_PROCESS} Process, {$endif}
  CastleUtils, CastleStringUtils;

type
  { Type wide enough to contain process id on all platforms.

    Not defined with $ifdef, as this is not opaque type in practice,
    CGE build tool even does tricks by passing it in text in one place
    (see internal ToolProcessRun).
    We just need to explicitly decide on integer type
    that can fit everything.

    On Windows this is DWORD ( https://docs.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-getcurrentprocessid ).

    On Unix this is pid_t ( https://man7.org/linux/man-pages/man3/pid_t.3.html )
    which should be signed and no greater than CLong. In practice it is 32 or 64 bit signed
    integer. }
  TProcessId = Int64;

{$if defined(HAS_STANDARD_PROCESS)}

{ Alias all types and constants (enum value)s) from Process unit,
  so that we can use them in our code by just using CastleInternalProcess unit.

  Note: we only care about a subset of Process unit:
  subset that we actually need, and subset that we also implement for non-FPC.
  This way we also avoid aliasing some types / constants not in all
  FPC versions (if some features were added since FPC 3.2.0,
  which we still support, see https://castle-engine.io/supported_compilers  ). }

type
  { }
  TProcessOption = Process.TProcessOption;
  TShowWindowOptions = Process.TShowWindowOptions;
  TStartupOption = Process.TStartupOption;
  TProcessPriority = Process.TProcessPriority;
  TProcessOptions = Process.TProcessOptions;
  TStartupOptions = Process.TStartupOptions;
  EProcess = Process.EProcess;

const
  poRunSuspended = Process.poRunSuspended;
  poWaitOnExit = Process.poWaitOnExit;
  poUsePipes = Process.poUsePipes;
  poStderrToOutPut = Process.poStderrToOutPut;
  poNoConsole = Process.poNoConsole;
  poNewConsole = Process.poNewConsole;
  poDefaultErrorMode = Process.poDefaultErrorMode;
  poNewProcessGroup = Process.poNewProcessGroup;
  poDebugProcess = Process.poDebugProcess;
  poDebugOnlyThisProcess = Process.poDebugOnlyThisProcess;
  poDetached = Process.poDetached;
  poPassInput = Process.poPassInput;

  swoNone = Process.swoNone;
  swoHIDE = Process.swoHIDE;
  swoMaximize = Process.swoMaximize;
  swoMinimize = Process.swoMinimize;
  swoRestore = Process.swoRestore;
  swoShow = Process.swoShow;
  swoShowDefault = Process.swoShowDefault;
  swoShowMaximized = Process.swoShowMaximized;
  swoShowMinimized = Process.swoShowMinimized;
  swoshowMinNOActive = Process.swoshowMinNOActive;
  swoShowNA = Process.swoShowNA;
  swoShowNoActivate = Process.swoShowNoActivate;
  swoShowNormal = Process.swoShowNormal;

  suoUseShowWindow = Process.suoUseShowWindow;
  suoUseSize = Process.suoUseSize;
  suoUsePosition = Process.suoUsePosition;
  suoUseCountChars = Process.suoUseCountChars;
  suoUseFillAttribute = Process.suoUseFillAttribute;

  ppHigh = Process.ppHigh;
  ppIdle = Process.ppIdle;
  ppNormal = Process.ppNormal;
  ppRealTime = Process.ppRealTime;

{$elseif defined(MINIMAL_PROCESS_IMPLEMENTATION)}

type
  TProcessOption = (
    poRunSuspended, poWaitOnExit, poUsePipes, poStderrToOutPut,
    poNoConsole, poNewConsole, poDefaultErrorMode, poNewProcessGroup,
    poDebugProcess, poDebugOnlyThisProcess, poDetached,
    poPassInput, poRunIdle);

  TShowWindowOptions = (
    swoNone, swoHIDE, swoMaximize, swoMinimize, swoRestore, swoShow,
    swoShowDefault, swoShowMaximized, swoShowMinimized,
    swoshowMinNOActive, swoShowNA, swoShowNoActivate, swoShowNormal);

  TStartupOption = (
    suoUseShowWindow, suoUseSize, suoUsePosition,
    suoUseCountChars, suoUseFillAttribute);

  TProcessPriority = (ppHigh, ppIdle, ppNormal, ppRealTime);

  TProcessOptions = set of TProcessOption;
  TStartupOptions = set of TStartupOption;
  EProcess = class(Exception);

{$endif}

type
  {$ifdef CASTLE_PROCESS_AVAILABLE}
  { Raised when Windows "Smart App Control" protection prevents executing a process.

    See more about "Smart App Control", it in practice blocks unsigned apps:

    @unorderedlist(
      @itemSpacing compact
      @item https://support.microsoft.com/en-us/windows/smart-app-control-frequently-asked-questions-285ea03d-fa88-4d56-882e-6698afdb7003
      @item https://www.guidingtech.com/enable-or-disable-smart-app-control-in-windows-11/
      @item https://www.reddit.com/r/Windows11/comments/1s2t50a/comment/ochifwy/
    )
  }
  EWindowsSmartAppControlProtection = class(EProcess);
  {$endif}

  { Running processes.

    On FPC, @link(TCastleProcess) descends and benefits from standard FPC
    TProcess class, merely adding some improvements on top of it.

    On Delphi, we implement of subset of the necessary functionality on our side,
    only for certain platforms. }
  TCastleProcess = class(
    {$ifdef HAS_STANDARD_PROCESS} TProcess {$else} TComponent {$endif} )
  strict private
    {$ifdef MINIMAL_PROCESS_IMPLEMENTATION}
    FExecutable: String;
    FCurrentDirectory: String;
    FParameters: TStrings;
    FEnvironment: TStrings;
    FOptions: TProcessOptions;
    FStartupOptions: TStartupOptions;
    FShowWindowOptions: TShowWindowOptions;
    FPriority: TProcessPriority;
    { Handle of the running process.

      On Windows this is a THandle, valid (non-zero) after Execute.

      On Unix this is pid_t, again valid (not 0, not -1) after successful Execute.
      -1 indicates we had error in fork() and process was not started. }
    FHandle: TProcessId;
    FExitStatus: Integer;
    procedure SetParameters(const Value: TStrings);
    procedure SetEnvironment(const Value: TStrings);
    {$endif MINIMAL_PROCESS_IMPLEMENTATION}
  public
    {$ifdef IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES}
    procedure Execute; override;
    {$endif IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES}

    {$ifdef MINIMAL_PROCESS_IMPLEMENTATION}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Absolute executable path, from @link(FindExe) or @link(AbsoluteExeName). }
    property Executable: String Read FExecutable Write FExecutable;

    { Parameters passed to the process. }
    property Parameters: TStrings Read FParameters Write SetParameters;

    { Current directory for the process.
      The child process will see this as its current directory
      (GetCurrentDir() from its point of view).) }
    property CurrentDirectory: String Read FCurrentDirectory Write FCurrentDirectory;

    { Process exit status, valid only after process finishes and you waited for
      it using @link(WaitOnExit). }
    property ExitStatus: Integer Read FExitStatus;

    { Start process. }
    procedure Execute; virtual;

    { Forcefully terminate process now.
      The AExitCode parameter is ignored on Unix, as we cannot dictate the
      child's exit code then.
      It is honored on Windows. }
    function Terminate(const AExitCode: Integer): Boolean; virtual;

    { Wait until process finishes. }
    function WaitOnExit: Boolean; virtual;

    { Environment variables passed to the process, if not empty.
      If you want to adjust process environment, then usually you want to start
      with @link(EnvironmentStrings), to start with the same environment as
      the current process, and then adjust it.
      If empty, the process inherits the environment of the current process. }
    property Environment: TStrings Read FEnvironment Write SetEnvironment;

    { Options, ignored in this minimal implementation for now. }
    property Options: TProcessOptions Read FOptions Write FOptions;

    { Startup options, ignored in this minimal implementation for now. }
    property StartupOptions: TStartupOptions Read FStartupOptions Write FStartupOptions;

    { ShowWindow (WinAPI) options, ignored in this minimal implementation for now. }
    property ShowWindow: TShowWindowOptions Read FShowWindowOptions Write FShowWindowOptions;

    { Process priority, ignored in this minimal implementation for now. }
    property Priority: TProcessPriority Read FPriority Write FPriority;

    { Internal handle of the running process.
      On Windows this is a THandle, on Unix this is pid_t.
      Valid after Execute. }
    property ProcessHandle: TProcessId Read FHandle;
    {$endif MINIMAL_PROCESS_IMPLEMENTATION}
  end;

{$define read_interface}
{$I castleinternalprocess_execute_command.inc}
{$undef read_interface}

implementation

uses
  { Windows unit is needed
    - for IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES
      (FPC on Windows)
    - for EnvironmentStrings implementation using
      GetEnvironmentStrings (Delphi on Windows)
    - for MINIMAL_PROCESS_IMPLEMENTATION on Windows. }
  {$if defined(IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES) or (defined(DELPHI) and defined(MSWINDOWS))}
  Windows,
  {$endif}
  {$if defined(MINIMAL_PROCESS_IMPLEMENTATION) and defined(UNIX)}
  Posix.Unistd, Posix.SysWait, Posix.Signal, Posix.Errno,
  {$endif}
  CastleFilesUtils, CastleLog;

{$define read_implementation}
{$I castleinternalprocess_execute_command.inc}
{$undef read_implementation}

{$ifdef IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES}
procedure TCastleProcess.Execute;

  function NiceApplicationName: String;
  begin
    if Executable <> '' then
      Result := Executable
    else
    {$warnings off} // avoid "deprecated" warning about CommandLine, we only use it to show, in case other code uses it
    if CommandLine <> '' then
      Result := CommandLine
    else
    {$warnings on}
      Result := '(unknown application)';
  end;

begin
  try
    inherited Execute;
  except
    on E: EProcess do
    begin
      { Ideally detection using GetLastError should be enough,
        and it's better than looking at message (that may be localized,
        as it's a resourcestring). But let's also check message, just in case
        something in the meantime obscures GetLastError in some Process
        implementation. }
      if (GetLastError = 4551) or
         ( (IsPrefix('Failed to execute', E.Message)) and
            IsSuffix(': 4551', E.Message) ) then
      begin
        raise EWindowsSmartAppControlProtection.CreateFmt(
          'Failed to execute application "%s" because of Windows "Smart App Control" protection layer. ' +
          'Please turn it temporarily off using "Windows Security -> App & browser control -> Smart App Control" setting.' +
          NL + NL +
          'Original error message: %s',
        [
          NiceApplicationName,
          E.Message
        ]);
        // do not let control pass to the re-raise below.
      end else
      { At least improve the message for other cases,
        by including useful application name. }
      if IsPrefix('Failed to execute :', E.Message) then
      begin
        E.Message :=
          Format('Failed to execute application "%s": ', [NiceApplicationName]) +
          PrefixRemove('Failed to execute :', E.Message, true);
      end;

      raise;
    end;
  end;
end;
{$endif IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES}

{$ifdef MINIMAL_PROCESS_IMPLEMENTATION}
constructor TCastleProcess.Create(AOwner: TComponent);
begin
  inherited;
  FParameters := TStringList.Create;
  FEnvironment := TStringList.Create;
end;

destructor TCastleProcess.Destroy;
begin
  {$ifdef MSWINDOWS}
  if FHandle <> 0 then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
  end;
  {$endif MSWINDOWS}
  FreeAndNil(FParameters);
  FreeAndNil(FEnvironment);
  inherited;
end;

procedure TCastleProcess.SetParameters(const Value: TStrings);
begin
  FParameters.Assign(Value);
end;

procedure TCastleProcess.SetEnvironment(const Value: TStrings);
begin
  FEnvironment.Assign(Value);
end;

{$if defined(MSWINDOWS)}
  {$I castleinternalprocess_minimal_process_windows.inc}
{$endif}

{$if defined(UNIX)}
  {$I castleinternalprocess_minimal_process_unix.inc}
{$endif}

{$endif MINIMAL_PROCESS_IMPLEMENTATION}

end.
