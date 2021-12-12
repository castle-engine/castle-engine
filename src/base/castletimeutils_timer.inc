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

{ Part of CastleTimeUtils unit: Timer, measuring real time. }

{$ifdef read_interface}

type
  { Current time from @link(Timer). }
  TTimerResult = record
  private
    { The type of this could be platform-dependent. But for now, all platforms
      are happy with QWord. }
    Value: QWord;
  public
    { Seconds passed since this time sample up to now.
      Equivalent to @code(TimerSeconds(Timer, Self)) }
    function ElapsedTime: TFloatTime;
  end;

{ Timer to measure (real) time passed during some operations.
  It is a "real" time, which means that subtracting two values measures
  the actual time that passed between two events.
  Contrast this with @link(ProcessTimer) that tries to measure
  only CPU time used by the current process.

  Call Timer twice, and calculate the difference (in seconds)
  using the TimerSeconds. Like this:

  @longCode(#
  var
    TimeStart: TTimerResult;
    Seconds: TFloatTime;
  begin
    TimeStart := Timer;
    // ...  do something time-consuming ...
    Seconds := TimerSeconds(Timer, TimeStart);
    // or: Seconds := TimeStart.ElapsedTime;
    WritelnLog('Seconds passed: %f', [Seconds]);
  end;
  #)
}
function Timer: TTimerResult;

{ Subtract two times obtained from @link(Timer),
  A-B, return a difference in seconds. }
function TimerSeconds(const A, B: TTimerResult): TFloatTime;

{$endif read_interface}

{$ifdef read_implementation}

{ timer ---------------------------------------------------------- }

{$ifdef MSWINDOWS}
type
  TTimerFrequency = QWord;
  TTimerState = (tsNotInitialized, tsQueryPerformance, tsCastleGetTickCount64);

var
  FTimerState: TTimerState = tsNotInitialized;
  FTimerFrequency: TTimerFrequency;

{ Set FTimerState to something <> tsNotInitialized.
  Also set FTimerFrequency. }
procedure InitTimer;
begin
  if QueryPerformanceFrequency(Int64(FTimerFrequency)) then
    FTimerState := tsQueryPerformance else
  begin
    FTimerState := tsCastleGetTickCount64;
    FTimerFrequency := 1000;
  end;
end;

function TimerFrequency: TTimerFrequency;
begin
  if FTimerState = tsNotInitialized then InitTimer;

  Result := FTimerFrequency;
end;

function Timer: TTimerResult;
begin
  if FTimerState = tsNotInitialized then InitTimer;

  if FTimerState = tsQueryPerformance then
    QueryPerformanceCounter(Int64(Result.Value))
  else
  begin
    { Unfortunately, below will cast CastleGetTickCount64 back to 32-bit.
      Hopefully QueryPerformanceCounter is usually available. }
    Result.Value := CastleGetTickCount64;
  end;
end;
{$endif MSWINDOWS}

{$ifdef CASTLE_NINTENDO_SWITCH}
const
  TimerFrequency = 1000;

function Timer: TTimerResult;
begin
  Result.Value := CastleGetTickCount64;
end;
{$endif}

{$if defined(UNIX) and not defined(CASTLE_NINTENDO_SWITCH)}
type
  TTimerFrequency = LongWord;
const
  TimerFrequency: TTimerFrequency = 1000000;

{$ifdef ANDROID}
function Timer: TTimerResult;
var
  tp: TimeSpec;
begin
  { Android has three clocks we need use clock which never leaps forward
    or backward. This clock we can get by clock_gettime(CLOCK_MONOTONIC).
    The FpGettimeofday() uses "wall" clock which can go back or forward when
    synchronization comes more info:
    https://stackoverflow.com/questions/3832097/how-to-get-the-current-time-in-native-android-code
    https://developer.android.com/reference/android/os/SystemClock.html }

  clock_gettime(CLOCK_MONOTONIC, @tp);
  Result.Value := QWord(tp.tv_sec) * 1000000 + QWord(tp.tv_nsec div 1000);
end;

{$else}

function Timer: TTimerResult;
var
  tv: TTimeval;
begin
  {$ifdef FPC}
  FpGettimeofday(@tv, nil);
  {$else}
  Gettimeofday(tv, nil);
  {$endif}

  { We can fit whole TTimeval inside QWord, no problem. }
  Result.Value := QWord(tv.tv_sec) * 1000000 + QWord(tv.tv_usec);
end;
{$endif}
{$endif UNIX}

function TimerSeconds(const A, B: TTimerResult): TFloatTime;
begin
  Result := (A.Value - B.Value) / TimerFrequency;
end;

function TTimerResult.ElapsedTime: TFloatTime;
begin
  Result := TimerSeconds(Timer, Self);
end;

{$endif read_implementation}
