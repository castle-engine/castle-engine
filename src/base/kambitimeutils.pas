{
  Copyright 2000-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Time utilities. }
unit KambiTimeUtils;

{$I kambiconf.inc}

interface

uses
  {$ifdef MSWINDOWS}
    Windows,
  {$endif}
  {$ifdef UNIX}
    {$ifdef USE_LIBC} Libc, {$else} BaseUnix, Unix, Dl, {$endif}
  {$endif}
  SysUtils, Math;

type
  { Time in seconds. This is used throughout my engine to represent time
    as a floating-point value with good accuracy in seconds.
    In particular, for VRML / X3D time-dependent nodes.

    Implementation notes, about the choice of precision:

    @unorderedList(
      @item("Single" precision is sometimes @italic(not) enough for this.
        Proof: open rotate.kanim (from demo_models).
        Change "on display" time pass to 1000, wait a couple seconds
        (world time will reach a couple of thousands),
        change "on display" time pass back to 1.
        Result: with time as Single, animation becomes jagged.
        Reason: the precision loss of Single time, and the fact that
        Draw is not called all the time (because AutoRedisplay is false,
        and model is in Examine mode and is still (not rotating)),
        so incrementation steps of AnimationTime are very very small.

        Setting AutoRedisplay to true workarounds the problem too, but that's
        1. unacceptable to eat 100% CPU without a reason for utility like
        view3dscene 2. that's asking for trouble, after all even with
        AutoRedisplay = true the precision loss is there, it's just not
        noticeable... using better precision feels much safer.)

      @item(For X3D, SFTime has "Double" precision.
        Also "The Castle" and "The Rift" prooved it's enough in practice.

        I could have choosen Extended here,
        but for X3D sake (to avoid unnecessary floating-point convertions
        all around), let's stick to Double for now.)
    )
  }
  TKamTime = Double;

const
  OldestTime = -MaxDouble;

{ Hang the program for the specified number of miliseconds. }
procedure Delay(MiliSec: Word);

type
  TMilisecTime = LongWord;

{ Check is SecondTime larger by at least TimeDelay than FirstTime.

  Naive implementation of this would be @code(SecondTime - FirstTime >= TimeDelay).

  FirstTime and SecondTime are milisecond times from some initial point.
  For example, they may be taken from a function like GetTickCount.
  Such time may "wrap" (TMilisecTime, just a LongWord, is limited).
  This function checks these times intelligently, using the assumption that
  the SecondTime is always "later" than the FirstTime, and only having to check
  if it's later by at least TimeDelay.

  Always TimeTickSecond(X, X, 0) = @true. that is, when both times
  are actually equal, it's correctly "later by zero miliseconds". }
function TimeTickSecondLater(firstTime, secondTime, timeDelay: TMilisecTime): boolean;

{ Difference in times between SecondTime and FirstTime.

  Naive implementation would be just @code(SecondTime - FirstTime),
  this function does a little better: takes into account that times may "wrap"
  (see TimeTickSecondLater), and uses the assumption that
  the SecondTime for sure "later", to calculate hopefully correct difference. }
function TimeTickDiff(firstTime, secondTime: TMilisecTime): TMilisecTime;

{ Simply add and subtract two TMilisecTime values.

  These don't care if TMilisecTime is a point in time, or time interval.
  They simply add / subtract values. It's your problem if adding / subtracting
  them is sensible.

  Range checking is ignored. In particular, this means that if you subtract
  smaller value from larger value, the result will be like the time "wrapped"
  in between (since TMilisecTime range is limited).

  @groupBegin }
function MilisecTimesAdd(t1, t2: TMilisecTime): TMilisecTime;
function MilisecTimesSubtract(t1, t2: TMilisecTime): TMilisecTime;
{ @groupEnd }

{ Get current time, in miliseconds. Such time wraps after ~49 days.

  Under Windows, this is just a WinAPI GetTickCount call, it's a time
  since system start.

  Under Unix, similar result is obtained by gettimeofday call,
  and cutting off some digits. So under Unix it's not a time since system start,
  but since some arbitrary point. }
function GetTickCount: TMilisecTime;
 {$ifdef MSWINDOWS} stdcall; external KernelDLL name 'GetTickCount'; {$endif MSWINDOWS}

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
      {$ifdef USE_LIBC}
        : function: clock_t = Libc.CLK_TCK
      {$else}
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
    {$endif}
    {$ifdef MSWINDOWS} = 1000 { Using GetLastError } {$endif};

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
function ProcessTimerEnd: Double;
{ @groupEnd }

{ -----------------------------------------------------------------------------
  @section(Timer) }
{ }

{$ifdef MSWINDOWS}
type
  TKamTimerResult = Int64;
  TKamTimerFrequency = Int64;

function KamTimerFrequency: TKamTimerFrequency;
{$endif MSWINDOWS}

{$ifdef UNIX}
type
  TKamTimerResult = Int64;
  TKamTimerFrequency = LongWord;

const
  KamTimerFrequency: TKamTimerFrequency = 1000000;
{$endif UNIX}

{ KamTimer is used to measure passed "real time". "Real" as opposed
  to e.g. process time (see ProcessTimerNow and friends above).
  Call KamTimer twice, calculate the difference, and you get time
  passed --- with frequency in KamTimerFrequency.

  KamTimerFrequency says how much KamTimer gets larger during 1 second
  (how many "ticks" are during one second).

  Implementation details: Under Unix this uses gettimeofday.
  Under Windows this uses QueryPerformanceCounter/Frequency,
  unless WinAPI "performance timer" is not available, then standard
  GetTickCount is used. }
function KamTimer: TKamTimerResult;

{ TFramesPerSecond ----------------------------------------------------------- }

type
  { Utility to measure frames per second, independent of actual
    rendering API. For example, it can be easily "plugged" into TCastleWindowBase
    (see TCastleWindowBase.FPS) or Lazarus GL control (see TCastleControlCustom.FPS).

    Things named "_" here are supposed to be internal to the TCastleWindowBase /
    TCastleControlCustom and such implementations. Other properties can be
    controlled by the user of TCastleWindowBase / TCastleControlCustom. }
  TFramesPerSecond = class
  private
    FActive: boolean;
    FFrameTime: Double;
    FRealTime: Double;
    FIdleSpeed: Single;
    DoIgnoreNextIdleSpeed: boolean;
    FIdleStartTime: TKamTimerResult;
    LastRecalculateTime: TMilisecTime;
    RenderStartTime: TKamTimerResult;
    { 0 means "no frame was rendered yet" }
    FramesRendered: Int64;
    { how much time passed inside frame rendering }
    FrameTimePassed: TKamTimerResult;
  public
    constructor Create;

    procedure _RenderBegin;
    procedure _RenderEnd;
    procedure _IdleBegin;

    { Turn on/off frames per second measuring.
      Before using FrameTime or RealTime you must set Active to @true. }
    property Active: boolean read FActive write FActive;

    { Rendering speed in frames per second. This tells FPS,
      if we would only call Draw (EventDraw, OnDraw) all the time.
      That is, this doesn't take into account time spent on other activities,
      like OnIdle, and it doesn't take into account that frames are possibly
      not rendered continously (when AutoRedisplay = @false, we may render
      frames seldom, because there's no need to do it more often).

      @seealso RealTime }
    property FrameTime: Double read FFrameTime;

    { How many frames per second were rendered. This is a real number
      of EventDraw (OnDraw) calls per second. This means that it's actual
      speed of your program. Anything can slow this down, not only long
      EventDraw (OnDraw), but also slow processing of other events (like OnIdle).
      Also, when AutoRedisplay = @false, this may be very low, since you
      just don't need to render frames continously.

      @seealso FrameTime }
    property RealTime: Double read FRealTime;

    { @abstract(How much time passed since last EventIdle (OnIdle) call?)

      The time is in seconds, 1.0 = 1 second.
      For two times faster computer IdleSpeed = 0.5,
      for two times slower IdleSpeed = 2.0. This is useful for doing
      time-based rendering, when you want to scale some changes
      by computer speed, to get perceived animation speed the same on every
      computer, regardless of computer's speed.

      This is calculated as a time between
      start of previous Idle event and start of current Idle event.
      So this really measures your whole loop time (unlike previous DrawSpeed
      that measured only EventDraw (OnDraw) speed).

      You can sanely use this only within EventIdle (OnIdle).

      This is independent from @link(Active) setting, it works always. }
    property IdleSpeed: Single read FIdleSpeed;

    { Forces IdleSpeed within the next EventIdle (onIdle) call to be
      equal to 0.0.

      This is useful if you just came back from some lenghty
      state, like a GUI dialog box (like TCastleWindowBase.FileDialog or modal boxes
      in GLWinMessages --- but actually all our stuff already calls this
      as needed, TGLMode takes care of this). IdleSpeed would be ridicoulously
      long in such case (if our loop is totally stopped) or not relevant
      (if we do our loop, but with totally different callbacks, like
      GLWinMessages). Instead, it's most sensible in such case to fake
      that IdleSpeed is 0.0, so things such as T3DSceneCore.Time
      should not advance wildly just because we did GUI box.

      This forces the IdleSpeed to zero only once, that is only on the
      next EventIdle (OnIdle). Following EventIdle (OnIdle) will have
      IdleSpeed as usual (unless you call IgnoreNextIdleSpeed again, of course). }
    procedure IgnoreNextIdleSpeed;

    { Time of last idle call. }
    property IdleStartTime: TKamTimerResult read FIdleStartTime;
  end;

implementation

procedure Delay (MiliSec: Word);
begin
 SysUtils.Sleep(MiliSec);
end;

function TimeTickSecondLater(firstTime, secondTime, timeDelay: TMilisecTime): boolean;
{ trzeba uwazac na typy w tej procedurze.
  Dword to 32 bit unsigned int,
  Int64 to 64 bit signed int.
  I tak musi byc.
  Uwaga ! W Delphi 3, o ile dobrze pamietam, bylo Dword = longint ! tak nie moze byc ! }
var bigint: Int64;
begin
 bigint := secondTime-timeDelay;
 if bigint < 0 then
 begin
  bigint := bigint+High(TMilisecTime);
  result:=(firstTime > secondTime) and (firstTime <= bigint);
 end else result := firstTime <= bigint;
end;

function TimeTickDiff(firstTime, secondTime: TMilisecTime): TMilisecTime;
begin
 result := MilisecTimesSubtract(secondTime, firstTime);
{old implementation :

 if firstTime <= secondTime then
  result := secondTime-firstTime else
  result := High(TMilisecTime) -firstTime +secondTime;
}
end;

{$I norqcheckbegin.inc}
function MilisecTimesAdd(t1, t2: TMilisecTime): TMilisecTime;
begin result := t1+t2 end;

function MilisecTimesSubtract(t1, t2: TMilisecTime): TMilisecTime;
begin result := t1-t2 end;
{$I norqcheckend.inc}

{$ifndef MSWINDOWS}

{$I norqcheckbegin.inc}
function GetTickCount: TMilisecTime;
var timeval: TTimeVal;
begin
 {$ifdef USE_LIBC} gettimeofday(timeval, nil)
 {$else}           FpGettimeofday(@timeval, nil)
 {$endif};

 { Odrzucamy najbardziej znaczace cyfry z x -- i dobrze, bo w
   timeval.tv_sec najbardziej znaczace cyfry sa najmniej wazne bo najrzadziej
   sie zmieniaja.
   x*1000 jednoczesnie ma puste miejsca na trzech najmniej znaczaych cyfrach
   dziesietnych (mowiac po ludzku, po prostu x*1000 ma trzy zera na koncu).
   Wykorzystujemy te trzy miejsca na tv_usec div 1000 ktore na pewno zawiera
   sie w 0..999 bo tv_usec zawiera sie w 0..milion-1.

   W ten sposob zamenilismy timeval na jedna 32-bitowa liczbe w taki sposob
   ze odrzucilismy tylko najbardziej znaczace cyfry - a wiec lepiej
   sie nie da. W rezultacie otrzymana cyfra mierzy czas w milisekundach a wiec
   przewinie sie, podobnie jak Windowsowe GetTickCount, co jakies 49 dni
   (49 dni = 49* 24* 60* 60 *1000 milisekund = 4 233 600 000 a wiec okolice
   High(LongWord)). Tyle ze ponizsze GetTickCount nie liczy czasu poczawszy
   od startu windowsa wiec moze sie przewinac w kazdej chwili.

   Note: I used to have here some old code that instead of
     LongWord(timeval.tv_sec) * 1000
   was doing
     ( LongWord(timeval.tv_sec) mod (Int64(High(LongWord)) div 1000 + 1) ) * 1000
   but I longer think it's necessary. After all, I'm inside
   norqcheck begin/end so I don't have to care about such things,
   and everything should work OK.
 }

 Result := LongWord(timeval.tv_sec) * 1000 + Longword(timeval.tv_usec) div 1000;
end;
{$I norqcheckend.inc}

{$endif not MSWINDOWS}

function DateTimeToAtStr(DateTime: TDateTime): string;
begin
 result := FormatDateTime('yyyy"-"mm"-"dd" at "tt', DateTime);
end;

{ cross-platform process timery ---------------------------------------------- }

{$ifdef UNIX}
function ProcessTimerNow: TProcessTimerResult;
var Dummy: tms;
begin
 { See /c/mojepasy/console.testy/test_times/RESULTS,
   it occurs that (at least on my Linux ? Debian, Linux 2.4.20, libc-2.3.2)
   the only reliable way is to use return value from times
   (from Libc or FpTimes).

   tms.tms_utime, tms.tms_stime, clock() values are nonsense !

   This is not FPC bug as I tested this with C program too. }

 Result := {$ifdef USE_LIBC} times {$else} FpTimes {$endif} (Dummy);
end;

function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
begin
 Result := a - b;
end;
{$endif UNIX}

{$ifdef MSWINDOWS}
function ProcessTimerNow: TProcessTimerResult;
begin
  Result := GetTickCount;
end;

function ProcessTimerDiff(a, b: TProcessTimerResult): TProcessTimerResult;
begin
  Result := TimeTickDiff(b, a);
end;
{$endif MSWINDOWS}

var
  LastProcessTimerBegin: TProcessTimerResult;

procedure ProcessTimerBegin;
begin
  LastProcessTimerBegin := ProcessTimerNow
end;

function ProcessTimerEnd: Double;
begin
  Result := ProcessTimerDiff(ProcessTimerNow, LastProcessTimerBegin)
    / ProcessTimersPerSec;
end;

{ timer ---------------------------------------------------------- }

{$ifdef MSWINDOWS}
type
  TTimerState = (tsNotInitialized, tsQueryPerformance, tsGetTickCount);

var
  FTimerState: TTimerState = tsNotInitialized;
  FKamTimerFrequency: TKamTimerFrequency;

{ Set FTimerState to something <> tsNotInitialized.
  Also set FKamTimerFrequency. }
procedure InitKamTimer;
begin
  if QueryPerformanceFrequency(FKamTimerFrequency) then
    FTimerState := tsQueryPerformance else
  begin
    FTimerState := tsGetTickCount;
    FKamTimerFrequency := 1000;
  end;
end;

function KamTimerFrequency: TKamTimerFrequency;
begin
  if FTimerState = tsNotInitialized then InitKamTimer;

  Result := FKamTimerFrequency;
end;

function KamTimer: TKamTimerResult;
begin
  if FTimerState = tsNotInitialized then InitKamTimer;

  if FTimerState = tsQueryPerformance then
    QueryPerformanceCounter(Result) else
    Result := GetTickCount;
end;
{$endif MSWINDOWS}

{$ifdef UNIX}
function KamTimer: TKamTimerResult;
var
  tv: TTimeval;
begin
  {$ifdef USE_LIBC} gettimeofday(tv, nil)
  {$else}           FpGettimeofday(@tv, nil)
  {$endif};

  { w Int64 zmiesci sie cale TTimeval bez obcinania.
    Robie tylko odpowiednie casty na zapas zeby na pewno liczyl wszystko
    od poczatku jako Int64}
  Result := Int64(tv.tv_sec)*1000000 + Int64(tv.tv_usec);
end;
{$endif UNIX}

{ TFramesPerSecond ----------------------------------------------------------- }

constructor TFramesPerSecond.Create;
const
  DefaultFps = 30.0;
begin
  inherited;

  { Just init times to some sensible default.

    For IdleSpeed this is actually not essential, since we call
    IgnoreNextCompSpeed anyway. But in case programmer will (incorrectly!)
    try to use IdleSpeed before EventIdle (OnIdle) call, it's useful to have
    here some predictable value. }
  FIdleSpeed := 1 / DefaultFps;
  FFrameTime := DefaultFps;
  FRealTime := DefaultFps;

  IgnoreNextIdleSpeed;
end;

procedure TFramesPerSecond._RenderBegin;
begin
  if not Active then Exit;
  RenderStartTime := KamTimer;
end;

procedure TFramesPerSecond._RenderEnd;
const
  TimeToRecalculate = 1000; { in miliseconds }
var
  NowTime: TMilisecTime;
begin
  if not Active then Exit;

  Inc(FramesRendered);
  FrameTimePassed += KamTimer - RenderStartTime;

  NowTime := GetTickCount;
  if NowTime - LastRecalculateTime >= TimeToRecalculate then
  begin
    { update FRealTime, FFrameTime once for TimeToRecalculate time.
      This way they don't change rapidly.

      Previosuly we used more elaborate hacks for this (resetting
      their times after a longer periods, but keeping some previous
      results), but they were complex and bad: when the game speed
      was changing suddenly, FRealTime, FFrameTime should also change
      suddenly, not gradually increase / decrease. }

    FRealTime := FramesRendered * 1000 / (NowTime - LastRecalculateTime);

    if FrameTimePassed > 0 then
      FFrameTime := FramesRendered * KamTimerFrequency / FrameTimePassed else
      FFrameTime := 0;

    LastRecalculateTime := NowTime;
    FramesRendered := 0;
    FrameTimePassed := 0;
  end;
end;

procedure TFramesPerSecond._IdleBegin;
var
  NewIdleStartTime: TKamTimerResult;
begin
  { update FIdleSpeed, DoIgnoreNextIdleSpeed, FIdleStartTime }
  NewIdleStartTime := KamTimer;

  if DoIgnoreNextIdleSpeed then
  begin
    FIdleSpeed := 0.0;
    DoIgnoreNextIdleSpeed := false;
  end else
    FIdleSpeed := ((NewIdleStartTime - FIdleStartTime) / KamTimerFrequency);

  FIdleStartTime := NewIdleStartTime;
end;

procedure TFramesPerSecond.IgnoreNextIdleSpeed;
begin
  DoIgnoreNextIdleSpeed := true;
end;

end.
