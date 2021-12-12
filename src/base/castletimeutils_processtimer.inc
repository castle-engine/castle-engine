{%MainUnit castletimeutils.pas}
{
  Copyright 2000-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleTimeUtils unit: ProcessTimer, measuring CPU usage of this process. }

{$ifdef read_interface}

type
  { Current time from @link(ProcessTimer).
    If possible, this measures only the CPU usage local to this process. }
  TProcessTimerResult = record
  private
    Value:
      {$if defined(MSWINDOWS) or defined(CASTLE_NINTENDO_SWITCH)}
      QWord // ProcessTimer just does CastleGetTickCount64
      {$else}
      clock_t // Unix
      {$endif};
  public
    { Seconds passed since this time sample up to now.
      Equivalent to @code(ProcessTimerSeconds(ProcessTimer, Self)) }
    function ElapsedTime: TFloatTime;
  end;

{ Current time, local to this process.
  Use this to measure and compare the time it takes your program to do
  some calculations.

  If possible, this measures only the CPU usage of this process.
  So it ignores delays caused by other processes doing something on your system,
  and it ignores things like waiting for hard disk (I/O).
  This is possible on Unix thanks to the @code(clock) API,
  see http://www.gnu.org/software/libc/manual/html_node/Processor-And-CPU-Time.html .
  On other platforms (like Windows), this simply measures real time that passed.

  You usually take two ProcessTimer values,
  subtract them with @link(ProcessTimerSeconds),
  and this is the time that passed -- in seconds. Like this:

  @longCode(#
  var
    TimeStart: TProcessTimerResult;
    Seconds: TFloatTime;
  begin
    TimeStart := ProcessTimer;
    // ...  do something time-consuming ...
    Seconds := ProcessTimerSeconds(ProcessTimer, TimeStart);
    // or: Seconds := TimeStart.ElapsedTime;
    WritelnLog('Seconds passed (in this process): %f', [Seconds]);
  end;
  #)
}
function ProcessTimer: TProcessTimerResult;

function ProcessTimerNow: TProcessTimerResult; deprecated 'use ProcessTimer';

{ Subtract two times obtained from @link(ProcessTimer),
  A-B, return a difference in seconds. }
function ProcessTimerSeconds(const a, b: TProcessTimerResult): TFloatTime;

{ Simple measure of process CPU time. Call ProcessTimerBegin at the beginning
  of your calculation, call ProcessTimerEnd at the end. ProcessTimerEnd
  returns a float number, with 1.0 being one second.

  Note that using this is unsafe in libraries, not to mention multi-threaded
  programs (it's not "reentrant") --- you risk that some other code
  called ProcessTimerBegin, and your ProcessTimerEnd doesn't measure
  what you think. So in general units, do not use this, use @link(ProcessTimer)
  and @link(ProcessTimerSeconds) instead.

  @groupBegin }
procedure ProcessTimerBegin; deprecated 'instead of this, better to use a local variable, and ProcessTimer and ProcessTimerSeconds';
function ProcessTimerEnd: TFloatTime; deprecated 'instead of this, better to use a local variable, and ProcessTimer and ProcessTimerSeconds';
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

function ProcessTimer: TProcessTimerResult;
{$if defined(MSWINDOWS) or defined(CASTLE_NINTENDO_SWITCH)}
begin
  Result.Value := CastleGetTickCount64;
{$else}
// other Unixes
var
  Dummy: tms;
begin
  { See console.tests/test_times/RESULTS,
    it seems that (at least on my Linux? Debian, Linux 2.4.20, libc-2.3.2)
    the only reliable way is to use return value from times (from Libc or FpTimes).
    tms.tms_utime, tms.tms_stime, clock() values are nonsense!
    This is not FPC bug as I tested this with C program too. }

  Result.Value := {$ifdef FPC} FpTimes {$else} Times {$endif} (Dummy);
{$endif}
end;

function ProcessTimerNow: TProcessTimerResult;
begin
  Result := ProcessTimer;
end;

function ProcessTimerSeconds(const a, b: TProcessTimerResult): TFloatTime;
const
  { Resolution of the timer used by @link(ProcessTimer). }
  ProcessTimersPerSec =
    {$if defined(MSWINDOWS) or defined(CASTLE_NINTENDO_SWITCH)}
      1000 // miliseconds from CastleGetTickCount64
    {$else}
      { What is the frequency of FpTimes ?
        sysconf (_SC_CLK_TCK) ?
        Or does sysconf exist only in Libc ? }
      { Values below were choosen experimentally for Linux and FreeBSD
        (and I know that on most UNIXes it should be 128, that's
        a traditional value) }
      {$ifdef LINUX} 100 {$else}
        {$ifdef DARWIN}
          { In /usr/include/ppc/_limits.h and
               /usr/include/i386/_limits.h
            __DARWIN_CLK_TCK is defined to 100. }
          100
        {$else}
          128
        {$endif}
      {$endif}
    {$endif};
begin
  {$I norqcheckbegin.inc}
  Result := (A.Value - B.Value) / ProcessTimersPerSec;
  {$I norqcheckend.inc}
end;

var
  LastProcessTimerBegin: TProcessTimerResult;

procedure ProcessTimerBegin;
begin
  LastProcessTimerBegin := ProcessTimer;
end;

function ProcessTimerEnd: TFloatTime;
begin
  Result := ProcessTimerSeconds(ProcessTimer, LastProcessTimerBegin);
end;

function TProcessTimerResult.ElapsedTime: TFloatTime;
begin
  Result := ProcessTimerSeconds(ProcessTimer, Self);
end;

{$endif read_implementation}
