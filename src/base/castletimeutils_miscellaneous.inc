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

{ Part of CastleTimeUtils unit: small and miscellaneous definitions. }

{$ifdef read_interface}

type
  { Time in seconds. This is used throughout my engine to represent time
    as a floating-point value with good accuracy in seconds.

    Using the "double" precision (not just "single") is good to guarantee
    good accuracy. It is also the precision required for storing time in X3D.
    See also:
    https://randomascii.wordpress.com/2012/02/13/dont-store-that-in-a-float/
    https://twitter.com/ID_AA_Carmack/status/418158611664097280

    To test that "single" is not enough, open some animation in
    castle-model-viewer, and change "on display" time pass to 1000.
    It goes even better if AutoRedisplay is @false, and LimitFps is 0.0,
    and model is still for some time --- then we do many OnUpdate calls with
    very small SecondsPassed values. }
  TFloatTime = Double;

const
  OldestTime = -MaxDouble;

type
  { @deprecated
    To measure time, better use Timer + TimerSeconds or ProcessTimer + ProcessTimerSeconds }
  TMilisecTime = QWord
    {$ifdef FPC}
    // This works in Delphi too, but is too noisy
    deprecated 'To measure time, better use Timer + TimerSeconds or ProcessTimer + ProcessTimerSeconds'
    {$endif};

{ Check is SecondTime larger by at least TimeDelay than FirstTime.

  Simple implementation of this would be @code(SecondTime - FirstTime >= TimeDelay).

  FirstTime and SecondTime are milisecond times from some initial point.
  For example, they may be taken from a function like 32-bit GetTickCount
  (but you actually should use GetTickCount64 with new compilers, never 32-bit GetTickCount).
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

const
  MinDateTime: TDateTime = MinDouble;

{ Convert DateTime to string in the form "date at time". }
function DateTimeToAtStr(const DateTime: TDateTime): string;

{$endif read_interface}

{$ifdef read_implementation}

function TimeTickSecondLater(const FirstTime, SecondTime, TimeDelay: TMilisecTime): boolean;
var
  SecondTimeMinusDelay: Int64;
begin
  if FirstTime > SecondTime then
    WritelnLog('Time', 'FirstTime > SecondTime for TimeTickSecondLater. Maybe 32-bit GetTickCount just wrapped (Windows XP? Otherwise, 64-bit GetTickCount64 should always be used), or maybe you swapped arguments for TimeTickSecondLater.');
  { Need 64 bit signed int to hold the result of QWord - QWord }
  {$I norqcheckbegin.inc}
  SecondTimeMinusDelay := SecondTime - TimeDelay;
  {$I norqcheckend.inc}
  if SecondTimeMinusDelay < 0 then
  begin
    // detected Windows with 32-bit GetTickCount, it just wrapped, fix
    SecondTimeMinusDelay := SecondTimeMinusDelay + High(UInt32);
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
  result := High(UInt32) -FirstTime +SecondTime;
}
end;

{$I norqcheckbegin.inc}
function MilisecTimesAdd(const t1, t2: TMilisecTime): TMilisecTime;
begin result := t1+t2 end;

function MilisecTimesSubtract(const t1, t2: TMilisecTime): TMilisecTime;
begin result := t1-t2 end;
{$I norqcheckend.inc}

function DateTimeToAtStr(const DateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd" at "tt', DateTime);
end;

{$endif read_implementation}
