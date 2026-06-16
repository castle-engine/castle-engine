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
  or simple routines like @link(ExecuteCommand).

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
  and our family of @code(ExecuteCommand) routines offer some extra features
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

{ On Windows, if application execution is blocked by "Smart App Control"
  (another incarnation of anti-malware protection from Microsoft),
  ancestor TProcess makes super-unfriendly exception message
  "Failed to execute  : 4551" by doing:

    SErrCannotExecute= 'Failed to execute %s : %d'
    ....
    Raise EProcess.CreateFmt(SErrCannotExecute,[FCommandLine,GetLastError])

  Note that FCommandLine is empty in most cases,
  because we don't use deprecated CommandLine, making this error message even
  more cryptic.

  Let's detect and convert it to something helpful for users.
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
  { Running processes.

    On FPC, @link(TCastleProcess) descends and benefits from standard FPC
    TProcess class, merely adding some improvements on top of it.

    On Delphi, we implement of subset of the necessary functionality on our side,
    only for certain platforms. }
  TCastleProcess = class(
    {$ifdef HAS_STANDARD_PROCESS} TProcess {$else} TComponent {$endif} )
  public
    {$ifdef IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES}
    procedure Execute; override;
    {$endif IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES}
  end;

{$define read_interface}
{$I castleinternalprocess_execute_command.inc}
{$undef read_interface}

implementation

uses
  { Windows unit is needed both for IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES
    (FPC on Windows) and for EnvironmentStrings implementation using
    GetEnvironmentStrings (Delphi on Windows). }
  {$if defined(IMPROVE_WINDOWS_PROCESS_ERROR_MESSAGES) or (defined(DELPHI) and defined(MSWINDOWS))}
  Windows,
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
        E.Message := Format(
          'Failed to execute application "%s" because of Windows "Smart App Control" (anti-malware protection). ' +
          'Please turn it temporarily off using "Windows Security → App & browser control → Smart App Control" setting.' +
          NL + NL +
          'Original error message: %s',
        [
          NiceApplicationName,
          E.Message
        ]);
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

end.
