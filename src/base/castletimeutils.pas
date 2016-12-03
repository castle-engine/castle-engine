{
  Copyright 2000-2016 Michalis Kamburelis.

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
  TMilisecTime = QWord;

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

{ Difference in times between SecondTime and FirstTime.

  Naive implementation would be just @code(SecondTime - FirstTime),
  this function does a little better: takes into account that times may "wrap"
  (see TimeTickSecondLater), and uses the assumption that
  the SecondTime for sure "later", to calculate hopefully correct difference. }
function TimeTickDiff(const FirstTime, SecondTime: TMilisecTime): TMilisecTime;

{ Simply add and subtract two TMilisecTime values.

  These don't care if TMilisecTime is a point in time, or time interval.
  They simply add / subtract values. It's your problem if adding / subtracting
  them is sensible.

  Range checking is ignored. In particular, this means that if you subtract
  smaller value from larger value, the result will be like the time "wrapped"
  in between (since TMilisecTime range is limited).

  @groupBegin }
function MilisecTimesAdd(const t1, t2: TMilisecTime): TMilisecTime;
function MilisecTimesSubtract(const t1, t2: TMilisecTime): TMilisecTime;
{ @groupEnd }

{ Get current time, in miliseconds. On newer OSes (non-Windows,
  or Windows >= Windows Vista) this uses 64-bit int under the hood.
  Or older Windows versions it's based on 32-bit Windows.GetTickCount
  that measures time since system start, that will wrap in ~ 49 days. }
function GetTickCount64: TMilisecTime;

const
  MinDateTime: TDateTime = MinDouble;

{ Convert DateTime to string in the form "date at time". }
function DateTimeToAtStr(DateTime: TDateTime): string;

{ ------------------------------------------------------------------------------
  @section(Process (CPU) Time measuring ) }

type
  { }
  TProcessTimerResult =
    {$ifdef UNIX} clock_t {$endif}
    {$ifdef MSWINDOWS} DWord {$endif};

const
  { Resolution of process timer.
    @seealso ProcessTimerNow }
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
    deprecated 'do not use this, it should not be needed; use ProcessTimerSeconds to compare two times';

{ Current value of process (CPU) timer.
  This can be used to measure how much CPU time your process used.
  Although note that on Windows there's no way to measure CPU time,
  so this simply measures real time that passed. Only under Unix
  this uses clock() call designed to actually measure CPU time.

  You take two ProcessTimerNow values, subtract them with ProcessTimerDiff,
  this is the time passed --- in resolution ProcessTimersPerSec.

  For simple usage, see ProcessTimerBegin and ProcessTimerEnd. }
function ProcessTimerNow: TProcessTimerResult;

{ Subtract two process (CPU) timer results, obtained from ProcessTimerNow.

  Although it may just subtract two values, it may also do something more.
  For example, if timer resolution is only miliseconds, and it may wrap
  (just like TMilisecTime), then we may subtract values intelligently,
  taking into account that time could wrap (see TimeTickDiff). }
function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
  deprecated 'use ProcessTimerSeconds instead';

{ Subtract two timer values, result is in seconds. }
function ProcessTimerSeconds(const a, b: TProcessTimerResult): TFloatTime;

{ Simple measure of process CPU time. Call ProcessTimerBegin at the beginning
  of your calculation, call ProcessTimerEnd at the end. ProcessTimerEnd
  returns a float number, with 1.0 being one second.

  Note that using this is unsafe in libraries, not to mention multi-threaded
  programs (it's not "reentrant") --- you risk that some other code
  called ProcessTimerBegin, and your ProcessTimerEnd doesn't measure
  what you think. So in general units, do not use this, use ProcessTimerNow
  and ProcessTimerDiff instead. In final programs (when you have full control)
  using these is comfortable and Ok.

  @groupBegin }
procedure ProcessTimerBegin;
function ProcessTimerEnd: TFloatTime;
{ @groupEnd }

{ -----------------------------------------------------------------------------
  @section(Timer) }
{ }

{$ifdef MSWINDOWS}
type
  TTimerResult = Int64;
  TTimerFrequency = Int64;

function TimerFrequency: TTimerFrequency;
{$endif MSWINDOWS}

{$ifdef UNIX}
type
  TTimerResult = Int64;
  TTimerFrequency = LongWord;

const
  TimerFrequency: TTimerFrequency = 1000000;
{$endif UNIX}

{ Measure passed real time. Note "real time" --- as opposed
  to e.g. process time (for this, see ProcessTimerNow and friends above).
  Call Timer twice, calculate the difference, and you get time
  passed --- with frequency in TimerFrequency.

  TimerFrequency says how much Timer gets larger during 1 second
  (how many "ticks" are during one second).

  Implementation details: Under Unix this uses gettimeofday.
  Under Windows this uses QueryPerformanceCounter/Frequency,
  unless WinAPI "performance timer" is not available, then standard
  GetTickCount64 is used. }
function Timer: TTimerResult;

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
    LastRecalculateTime: TMilisecTime;
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
  result := MilisecTimesSubtract(SecondTime, FirstTime);
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
  LastGetTickCount64: TMilisecTime;

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
function ProcessTimerNow: TProcessTimerResult;
var
  Dummy: tms;
begin
  { See console.tests/test_times/RESULTS,
    it seems that (at least on my Linux? Debian, Linux 2.4.20, libc-2.3.2)
    the only reliable way is to use return value from times (from Libc or FpTimes).
    tms.tms_utime, tms.tms_stime, clock() values are nonsense!
    This is not FPC bug as I tested this with C program too. }

  Result := FpTimes(Dummy);
end;

function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
begin
  Result := a - b;
end;
{$endif UNIX}

{$ifdef MSWINDOWS}
function ProcessTimerNow: TProcessTimerResult;
begin
  Result := GetTickCount64;
end;

function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
begin
  Result := TimeTickDiff(b, a);
end;
{$endif MSWINDOWS}

function ProcessTimerSeconds(const a, b: TProcessTimerResult): TFloatTime;
begin
  {$warnings off} // knowingly using deprecated stuff
  Result := ProcessTimerDiff(A, B) / ProcessTimersPerSec;
  {$warnings on}
end;

var
  LastProcessTimerBegin: TProcessTimerResult;

procedure ProcessTimerBegin;
begin
  LastProcessTimerBegin := ProcessTimerNow
end;

function ProcessTimerEnd: TFloatTime;
begin
  Result := ProcessTimerSeconds(ProcessTimerNow, LastProcessTimerBegin);
end;

{ timer ---------------------------------------------------------- }

{$ifdef MSWINDOWS}
type
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
    QueryPerformanceCounter(Result) else
    { Unfortunately, below will cast GetTickCount64 back to 32-bit.
      Hopefully QueryPerformanceCounter is usually available. }
    Result := GetTickCount64;
end;
{$endif MSWINDOWS}

{$ifdef UNIX}
var
  LastTimer: TTimerResult;

function Timer: TTimerResult;
var
  tv: TTimeval;
begin
  FpGettimeofday(@tv, nil);

  { We can fit whole TTimeval inside Int64, no problem. }
  Result := Int64(tv.tv_sec) * 1000000 + Int64(tv.tv_usec);

  { We cannot trust some Android systems to return increasing values here
    (Android device "Moto X Play", "XT1562", OS version 5.1.1).
    Maybe they synchronize the time from the Internet, and do not take care
    to keep it monotonic (unlike https://lwn.net/Articles/23313/ says?) }

  if Result < LastTimer then
  begin
    WritelnLog('Time', 'Detected gettimeofday() going backwards on Unix, workarounding. This is known to happen on some Android devices');
    Result := LastTimer;
  end else
    LastTimer := Result;
end;
{$endif UNIX}

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
  TimeToRecalculate = 1000; { in miliseconds }
var
  NowTime: TMilisecTime;
begin
  Inc(FramesRendered);
  FrameTimePassed += Timer - RenderStartTime;

  NowTime := GetTickCount64;
  if NowTime - LastRecalculateTime >= TimeToRecalculate then
  begin
    { update FRealTime, FFrameTime once for TimeToRecalculate time.
      This way they don't change rapidly.

      Previously we used more elaborate hacks for this (resetting
      their times after a longer periods, but keeping some previous
      results), but they were complex and bad: when the game speed
      was changing suddenly, FRealTime, FFrameTime should also change
      suddenly, not gradually increase / decrease. }

    FRealTime := FramesRendered * 1000 / (NowTime - LastRecalculateTime);

    if FrameTimePassed > 0 then
      FFrameTime := FramesRendered * TimerFrequency / FrameTimePassed else
      FFrameTime := 0;

    LastRecalculateTime := NowTime;
    FramesRendered := 0;
    FrameTimePassed := 0;
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
    FUpdateSecondsPassed := ((NewUpdateStartTime - FUpdateStartTime) / TimerFrequency);
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
