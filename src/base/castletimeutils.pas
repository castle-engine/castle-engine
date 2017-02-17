{
  Copyright 2000-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Time utilities. }
unit CastleTimeUtils;

{$I castleconf.inc}

interface

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX} BaseUnix, Unix, Dl, {$endif}
  SysUtils, Math;

type
  { Time in seconds. This is used throughout my engine to represent time
    as a floating-point value with good accuracy in seconds.

    Using the "double" precision (not just "single") is good to guarantee
    good accuracy. It is also the precision required for storing time in X3D.
    See also:
    https://randomascii.wordpress.com/2012/02/13/dont-store-that-in-a-float/
    https://twitter.com/ID_AA_Carmack/status/418158611664097280

    To test that "single" is not enough, open some animation in
    view3dscene, and change "on display" time pass to 1000.
    It goes even better if AutoRedisplay is @false, and LimitFPS is 0.0,
    and model is still for some time --- then we do many OnUpdate calls with
    very small SecondsPassed values. }
  TFloatTime = Double;

const
  OldestTime = -MaxDouble;

type
  TMilisecTime = QWord
    deprecated 'to measure time, better use Timer + TimerSeconds or ProcessTimer + ProcessTimerSeconds';

{ Check is SecondTime larger by at least TimeDelay than FirstTime.

  Simple implementation of this would be @code(SecondTime - FirstTime >= TimeDelay).

  FirstTime and SecondTime are milisecond times from some initial point.
  For example, they may be taken from a function like 32-bit GetTickCount
  (on older Windows; on newer we use GetTickCount64).
  Such time may "wrap".
  This function checks these times intelligently, using the assumption that
  the SecondTime is always "later" than the FirstTime, and only having to check
  if it's later by at least TimeDelay.

  Always TimeTickSecond(X, X, 0) = @true. that is, when both times
  are actually equal, it's correctly "later by zero miliseconds". }
function TimeTickSecondLater(const FirstTime, SecondTime, TimeDelay: TMilisecTime): boolean;
  deprecated 'to measure time, better use Timer + TimerSeconds or ProcessTimer + ProcessTimerSeconds';

{ Difference in times between SecondTime and FirstTime.

  Naive implementation would be just @code(SecondTime - FirstTime),
  this function does a little better: takes into account that times may "wrap"
  (see TimeTickSecondLater), and uses the assumption that
  the SecondTime for sure "later", to calculate hopefully correct difference. }
function TimeTickDiff(const FirstTime, SecondTime: TMilisecTime): TMilisecTime;
  deprecated 'to measure time, better use Timer + TimerSeconds or ProcessTimer + ProcessTimerSeconds. Also, this function has non-intuitive argument order, inconsistent with ProcessTimerSeconds and TimerSeconds';

{ Simply add and subtract two TMilisecTime values.

  These don't care if TMilisecTime is a point in time, or time interval.
  They simply add / subtract values. It's your problem if adding / subtracting
  them is sensible.

  Range checking is ignored. In particular, this means that if you subtract
  smaller value from larger value, the result will be like the time "wrapped"
  in between (since TMilisecTime range is limited).

  @groupBegin }
function MilisecTimesAdd(const t1, t2: TMilisecTime): TMilisecTime;
  deprecated 'to measure time, better use Timer + TimerSeconds or ProcessTimer + ProcessTimerSeconds';
function MilisecTimesSubtract(const t1, t2: TMilisecTime): TMilisecTime;
  deprecated 'to measure time, better use Timer + TimerSeconds or ProcessTimer + ProcessTimerSeconds';
{ @groupEnd }

{ Get current time, in miliseconds. On newer OSes (non-Windows,
  or Windows >= Windows Vista) this uses 64-bit int under the hood.
  Or older Windows versions it's based on 32-bit Windows.GetTickCount
  that measures time since system start, that will wrap in ~ 49 days. }
function GetTickCount64: TMilisecTime;
  deprecated 'to measure time, better use Timer + TimerSeconds or ProcessTimer + ProcessTimerSeconds';

const
  MinDateTime: TDateTime = MinDouble;

{ Convert DateTime to string in the form "date at time". }
function DateTimeToAtStr(DateTime: TDateTime): string;

{ ------------------------------------------------------------------------------
  @section(Measuring time (CPU usage of this process, if possible.) }

type
  { Current time from @link(ProcessTimer).
    If possible, this measures only the CPU usage local to this process. }
  TProcessTimerResult = object
  private
    Value:
      {$ifdef UNIX} clock_t {$endif}
      {$ifdef MSWINDOWS} DWord {$endif};
  end;

const
  { Resolution of the timer used by @link(ProcessTimer). }
  ProcessTimersPerSec
    {$ifdef UNIX}
      = { What is the frequency of FpTimes ?
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
            100 {$else}
              128 {$endif} {$endif}
    {$endif}
    {$ifdef MSWINDOWS} = 1000 { Using GetLastError } {$endif}
    deprecated 'do not use this, it should be only used internally; use ProcessTimerSeconds to compare two times';

{ Current time, local to this process.
  Use this to measure and compare the time it takes your program to do
  some calculations.

  If possible, this measures only the CPU usage of this process.
  So it ignores delays caused by other processes doing something on your system,
  and it ignores things like waiting for hard disk (I/O).
  This is possible on Unix thanks to the @code(clock) API,
  see http://www.gnu.org/software/libc/manual/html_node/Processor-And-CPU-Time.html .
  On other platforms (like Windows),
  this simply measures real time that passed.

  You take two ProcessTimer values, subtract them with @link(ProcessTimerSeconds),
  this is the time that passed -- in seconds. }
function ProcessTimer: TProcessTimerResult;

function ProcessTimerNow: TProcessTimerResult; deprecated 'use ProcessTimer';

{ Subtract two times obtained from @link(ProcessTimer),
  A-B, return a difference in ProcessTimersPerSec.

  Although it may just subtract two values, it may also do something more.
  For example, if timer resolution is only miliseconds, and it may wrap
  (just like TMilisecTime), then we may subtract values intelligently,
  taking into account that time could wrap (see TimeTickDiff). }
function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
  deprecated 'use ProcessTimerSeconds instead';

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

{ -----------------------------------------------------------------------------
  @section(Measuring real time.) }

type
  { Current time from @link(Timer). }
  TTimerResult = object
  private
    { The type of this could be platform-dependent. But for now, all platforms
      are happy with Int64. }
    Value: Int64;
  end;

{ Current time, to measure real time passed.
  This may be a time local to this process. It is a "real" time,
  which means that subtracting two values measures the actual time
  that passed between two events. Contrast this with @link(ProcessTimer)
  and friends that try to measure only CPU time used by the current process.

  Call Timer twice, and calculate the difference (in seconds)
  using the TimerSeconds. }
function Timer: TTimerResult;

{ Subtract two times obtained from @link(Timer),
  A-B, return a difference in seconds. }
function TimerSeconds(const A, B: TTimerResult): TFloatTime;

{ TFramesPerSecond ----------------------------------------------------------- }

type
  { Utility to measure frames per second, independent of actual
    rendering API. For example, it can be easily "plugged" into TCastleWindowCustom
    (see TCastleWindowCustom.FPS) or Lazarus GL control (see TCastleControlCustom.FPS).

    Things named "_" here are supposed to be internal to the TCastleWindowCustom /
    TCastleControlCustom and such implementations. Other properties can be
    controlled by the user of TCastleWindowCustom / TCastleControlCustom. }
  TFramesPerSecond = class
  private
    FFrameTime: TFloatTime;
    FRealTime: TFloatTime;
    FUpdateSecondsPassed: TFloatTime;
    DoZeroNextSecondsPassed: boolean;
    FUpdateStartTime: TTimerResult;
    LastRecalculateTime: TTimerResult;
    RenderStartTime: TTimerResult;
    { 0 means "no frame was rendered yet" }
    FramesRendered: Int64;
    { how much time passed inside frame rendering }
    FrameTimePassed: TTimerResult;
    FMaxSensibleSecondsPassed: TFloatTime;
  public
    const
      DefaultMaxSensibleSecondsPassed = 0.5;

    constructor Create;

    { Internal for Castle Game Engine.
      Called from CastleWindow or CastleControl.
      Don't call these methods yourself.
      @groupBegin
      @exclude }
    procedure _RenderBegin;
    { @exclude }
    procedure _RenderEnd;
    { @exclude }
    procedure _UpdateBegin;
    { @groupEnd }

    { Rendering speed in frames per second. This tells FPS,
      if we would only call Render (EventRender, OnRender) all the time.
      That is, this doesn't take into account time spent on other activities,
      like OnUpdate, and it doesn't take into account that frames are possibly
      not rendered continously (when AutoRedisplay = @false, we may render
      frames seldom, because there's no need to do it more often).

      @seealso RealTime }
    property FrameTime: TFloatTime read FFrameTime;

    { How many frames per second were rendered. This is a real number
      of EventRender (OnRender) calls per second. This means that it's actual
      speed of your program. Anything can slow this down, not only long
      EventRender (OnRender), but also slow processing of other events (like OnUpdate).
      Also, when AutoRedisplay = @false, this may be very low, since you
      just don't need to render frames continously.

      @seealso FrameTime }
    property RealTime: TFloatTime read FRealTime;

    { Track how much time passed since last Update call, using _UpdateBegin.

      The time is in seconds, 1.0 = 1 second.
      For two times faster computer UpdateSecondsPassed = 0.5,
      for two times slower UpdateSecondsPassed = 2.0. This is useful for doing
      time-based rendering, when you want to scale some changes
      by computer speed, to get perceived animation speed the same on every
      computer, regardless of computer's speed.

      This is calculated as a time between
      start of previous Update event and start of current Update event.
      So this really measures your whole loop time (unlike previous RenderSpeed
      that measured only EventRender (OnRender) speed).

      You can sanely use this only within EventUpdate (OnUpdate). }
    property UpdateSecondsPassed: TFloatTime read FUpdateSecondsPassed;

    { Limit the UpdateSecondsPassed variable, to avoid increasing time in game
      a lot when a game was hanging or otherwise waiting for some exceptional
      event from OS.
      Used only when non-zero.
      By default it's DefaultMaxSensibleSecondsPassed. }
    property MaxSensibleSecondsPassed: TFloatTime
      read FMaxSensibleSecondsPassed write FMaxSensibleSecondsPassed;

    { Forces UpdateSecondsPassed for the next Update call (using _UpdateBegin)
      to be zero.

      This is useful if you just came back from some lenghty
      state, like a GUI dialog box (like TCastleWindowCustom.FileDialog or modal boxes
      in CastleMessages --- but actually all our stuff already calls this
      as needed, TGLMode takes care of this). UpdateSecondsPassed would be ridicoulously
      long in such case (if our loop is totally stopped) or not relevant
      (if we do our loop, but with totally different callbacks, like
      CastleMessages). Instead, it's most sensible in such case to fake
      that UpdateSecondsPassed is 0.0, so things such as TCastleSceneCore.Time
      should not advance wildly just because we did GUI box.

      This forces the UpdateSecondsPassed to zero only once, that is only on the
      next update event (_UpdateBegin). Following update event (_UpdateBegin) will have
      UpdateSecondsPassed as usual (unless you call ZeroNextSecondsPassed again, of course). }
    procedure ZeroNextSecondsPassed;

    { Time of last Update call. }
    property UpdateStartTime: TTimerResult read FUpdateStartTime;

    { Current frame identifier.

      Changed when each container "update" event occurs,
      so this is equal during all @link(TInputListener.Update),
      @link(TUIControl.Render), @link(T3D.Update),
      @link(T3D.Render) occuring within the same frame.
      You can use this to avoid performing the same job many times
      in a single frame.

      Never zero.

      It's a class function, so you can access it like
      @code(TFramesPerSecond.FrameId),
      no need to have a TFramesPerSecond instance (which is usually
      accessed from TUIContainer, like @link(TUIContainer.Fps),
      @link(TCastleWindowCustom.Fps), @link(TCastleControlCustom.Fps). }
    class function FrameId: Int64;
  end;

implementation

uses CastleLog;

function TimeTickSecondLater(const FirstTime, SecondTime, TimeDelay: TMilisecTime): boolean;
var
  SecondTimeMinusDelay: Int64;
begin
  if Log and (FirstTime > SecondTime) then
    WritelnLog('Time', 'FirstTime > SecondTime for TimeTickSecondLater. Maybe 32-bit GetTickCount just wrapped (Windows XP? Otherwise, 64-bit GetTickCount64 should always be used), or maybe you swapped arguments for TimeTickSecondLater.');
  { Need 64 bit signed int to hold the result of QWord - QWord }
  {$I norqcheckbegin.inc}
  SecondTimeMinusDelay := SecondTime - TimeDelay;
  {$I norqcheckend.inc}
  if SecondTimeMinusDelay < 0 then
  begin
    // detected Windows with 32-bit GetTickCount, it just wrapped, fix
    SecondTimeMinusDelay := SecondTimeMinusDelay + High(LongWord);
    result := (FirstTime > SecondTime) and (FirstTime <= SecondTimeMinusDelay);
  end else
    result := FirstTime <= SecondTimeMinusDelay;
end;

function TimeTickDiff(const FirstTime, SecondTime: TMilisecTime): TMilisecTime;
begin
  {$warnings off} // knowingly using deprecated stuff in another deprecated
  result := MilisecTimesSubtract(SecondTime, FirstTime);
  {$warnings on}
{old implementation :

 if FirstTime <= SecondTime then
  result := SecondTime-FirstTime else
  result := High(LongWord) -FirstTime +SecondTime;
}
end;

{$I norqcheckbegin.inc}
function MilisecTimesAdd(const t1, t2: TMilisecTime): TMilisecTime;
begin result := t1+t2 end;

function MilisecTimesSubtract(const t1, t2: TMilisecTime): TMilisecTime;
begin result := t1-t2 end;
{$I norqcheckend.inc}

{$ifdef MSWINDOWS}
{ GetTickCount64 for Windows, from fpc/3.0.0/src/rtl/win/sysutils.pp }

{$IFNDEF WINCE}
type
  TGetTickCount64 = function : QWord; stdcall;

var
  WinGetTickCount64: TGetTickCount64 = Nil;
{$ENDIF}

function GetTickCount64: QWord;
{$IFNDEF WINCE}
var
  lib: THandle;
{$ENDIF}
begin
{$IFNDEF WINCE}
  { on Vista and newer there is a GetTickCount64 implementation }
  if Win32MajorVersion >= 6 then begin
    if not Assigned(WinGetTickCount64) then begin
      lib := LoadLibrary('kernel32.dll');
      WinGetTickCount64 := TGetTickCount64(
                             GetProcAddress(lib, 'GetTickCount64'));
    end;
    Result := WinGetTickCount64();
  end else
{$ENDIF}
    Result := Windows.GetTickCount;
end;
{$endif MSWINDOWS}

{$ifdef UNIX}
{ GetTickCount64 for Unix.
  Not based on, but in fact very similar idea as the one in
  FPC fpc/3.0.0/src/rtl/unix/sysutils.pp }

var
  {$warnings off} // knowingly using deprecated stuff
  LastGetTickCount64: TMilisecTime;
  {$warnings on}

{$I norqcheckbegin.inc}
function GetTickCount64: TMilisecTime;
var
  timeval: TTimeVal;
begin
  FpGettimeofday(@timeval, nil);

  { By doing tv_sec * 1000, we reject 3 most significant digits from tv_sec.
    That's Ok, since these digits change least often.
    And this way we get the 3 least significant digits to fill
    with tv_usec div 1000 (which must be < 1000, because tv_usec must be < 1 million). }

  Result := Int64(timeval.tv_sec) * 1000 + (timeval.tv_usec div 1000);

  { We cannot trust some Android systems to return increasing values here
    (Android device "Moto X Play", "XT1562", OS version 5.1.1).
    Maybe they synchronize the time from the Internet, and do not take care
    to keep it monotonic (unlike https://lwn.net/Articles/23313/ says?) }

  if Result < LastGetTickCount64 then
  begin
    WritelnLog('Time', 'Detected gettimeofday() going backwards on Unix, workarounding. This is known to happen on some Android devices');
    Result := LastGetTickCount64;
  end else
    LastGetTickCount64 := Result;
end;
{$I norqcheckend.inc}
{$endif UNIX}

function DateTimeToAtStr(DateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd" at "tt', DateTime);
end;

{ cross-platform process timers ---------------------------------------------- }

{$ifdef UNIX}
function ProcessTimer: TProcessTimerResult;
var
  Dummy: tms;
begin
  { See console.tests/test_times/RESULTS,
    it seems that (at least on my Linux? Debian, Linux 2.4.20, libc-2.3.2)
    the only reliable way is to use return value from times (from Libc or FpTimes).
    tms.tms_utime, tms.tms_stime, clock() values are nonsense!
    This is not FPC bug as I tested this with C program too. }

  Result.Value := FpTimes(Dummy);
end;

function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
begin
  Result.Value := A.Value - B.Value;
end;
{$endif UNIX}

{$ifdef MSWINDOWS}
function ProcessTimer: TProcessTimerResult;
begin
  { Deliberately using deprecated GetTickCount64 and friends.
    It should be internal in this unit. }
  {$warnings off}
  Result.Value := GetTickCount64;
  {$warnings on}
end;

function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
begin
  { Deliberately using deprecated GetTickCount64 and friends.
    It should be internal in this unit. }
  {$warnings off}
  Result.Value := TimeTickDiff(b.Value, a.Value);
  {$warnings on}
end;
{$endif MSWINDOWS}

function ProcessTimerNow: TProcessTimerResult;
begin
  Result := ProcessTimer;
end;

function ProcessTimerSeconds(const a, b: TProcessTimerResult): TFloatTime;
begin
  {$warnings off} // knowingly using deprecated stuff
  Result := ProcessTimerDiff(A, B).Value / ProcessTimersPerSec;
  {$warnings on}
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

{ timer ---------------------------------------------------------- }

{$ifdef MSWINDOWS}
type
  TTimerFrequency = Int64;
  TTimerState = (tsNotInitialized, tsQueryPerformance, tsGetTickCount64);

var
  FTimerState: TTimerState = tsNotInitialized;
  FTimerFrequency: TTimerFrequency;

{ Set FTimerState to something <> tsNotInitialized.
  Also set FTimerFrequency. }
procedure InitTimer;
begin
  if QueryPerformanceFrequency(FTimerFrequency) then
    FTimerState := tsQueryPerformance else
  begin
    FTimerState := tsGetTickCount64;
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
    QueryPerformanceCounter(Result.Value)
  else
  begin
    { Deliberately using deprecated GetTickCount64 and friends.
      It should be internal in this unit. }
    {$warnings off}
    { Unfortunately, below will cast GetTickCount64 back to 32-bit.
      Hopefully QueryPerformanceCounter is usually available. }
    Result.Value := GetTickCount64;
    {$warnings on}
  end;
end;
{$endif MSWINDOWS}

{$ifdef UNIX}
type
  TTimerFrequency = LongWord;
const
  TimerFrequency: TTimerFrequency = 1000000;
var
  LastTimer: TTimerResult;

function Timer: TTimerResult;
var
  tv: TTimeval;
begin
  FpGettimeofday(@tv, nil);

  { We can fit whole TTimeval inside Int64, no problem. }
  Result.Value := Int64(tv.tv_sec) * 1000000 + Int64(tv.tv_usec);

  { We cannot trust some Android systems to return increasing values here
    (Android device "Moto X Play", "XT1562", OS version 5.1.1).
    Maybe they synchronize the time from the Internet, and do not take care
    to keep it monotonic (unlike https://lwn.net/Articles/23313/ says?) }

  if Result.Value < LastTimer.Value then
  begin
    WritelnLog('Time', 'Detected gettimeofday() going backwards on Unix, workarounding. This is known to happen on some Android devices');
    Result.Value := LastTimer.Value;
  end else
    LastTimer.Value := Result.Value;
end;
{$endif UNIX}

function TimerSeconds(const A, B: TTimerResult): TFloatTime;
begin
  Result := (A.Value - B.Value) / TimerFrequency;
end;

{ TFramesPerSecond ----------------------------------------------------------- }

var
  FFrameId: Int64 = 1;

constructor TFramesPerSecond.Create;
const
  DefaultFps = 30.0;
begin
  inherited;

  { Just init times to some sensible default.

    For UpdateSecondsPassed this is actually not essential, since we call
    ZeroNextSecondsPassed anyway. But in case programmer will (incorrectly!)
    try to use UpdateSecondsPassed before _UpdateBegin call, it's useful to have
    here some predictable value. }
  FUpdateSecondsPassed := 1 / DefaultFps;
  FFrameTime := DefaultFps;
  FRealTime := DefaultFps;

  { the default is non-zero now, since all Android games need it }
  FMaxSensibleSecondsPassed := DefaultMaxSensibleSecondsPassed;

  ZeroNextSecondsPassed;
end;

procedure TFramesPerSecond._RenderBegin;
begin
  RenderStartTime := Timer;
end;

procedure TFramesPerSecond._RenderEnd;
const
  TimeToRecalculate = 1.0; { in seconds }
var
  NowTime: TTimerResult;
begin
  Inc(FramesRendered);
  FrameTimePassed.Value += Timer.Value - RenderStartTime.Value;

  NowTime := Timer;
  if TimerSeconds(NowTime, LastRecalculateTime) >= TimeToRecalculate then
  begin
    { update FRealTime, FFrameTime once for TimeToRecalculate time.
      This way they don't change rapidly.

      Previously we used more elaborate hacks for this (resetting
      their times after a longer periods, but keeping some previous
      results), but they were complex and bad: when the game speed
      was changing suddenly, FRealTime, FFrameTime should also change
      suddenly, not gradually increase / decrease. }

    FRealTime := FramesRendered / TimerSeconds(NowTime, LastRecalculateTime);

    if FrameTimePassed.Value > 0 then
      FFrameTime := FramesRendered * TimerFrequency / FrameTimePassed.Value
    else
      FFrameTime := 0;

    LastRecalculateTime := NowTime;
    FramesRendered := 0;
    FrameTimePassed.Value := 0;
  end;
end;

procedure TFramesPerSecond._UpdateBegin;
var
  NewUpdateStartTime: TTimerResult;
begin
  { update FUpdateSecondsPassed, DoZeroNextSecondsPassed, FUpdateStartTime }
  NewUpdateStartTime := Timer;

  if DoZeroNextSecondsPassed then
  begin
    FUpdateSecondsPassed := 0.0;
    DoZeroNextSecondsPassed := false;
  end else
  begin
    FUpdateSecondsPassed := TimerSeconds(NewUpdateStartTime, FUpdateStartTime);
    if MaxSensibleSecondsPassed > 0 then
      FUpdateSecondsPassed := Min(FUpdateSecondsPassed, MaxSensibleSecondsPassed);
  end;

  FUpdateStartTime := NewUpdateStartTime;

  Inc(FFrameId);
end;

procedure TFramesPerSecond.ZeroNextSecondsPassed;
begin
  DoZeroNextSecondsPassed := true;
end;

class function TFramesPerSecond.FrameId: Int64;
begin
  Result := FFrameId;
end;

end.
