{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ X3D time. }
unit X3DTime;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleUtils, CastleTimeUtils;

type
  { Complete timestamp for X3D events.
    For most purposes, you're interested only in it's @link(Seconds) field,
    this is the amount of continuous time that passed (in double-precision
    seconds).

    More precisely, timestamp is actually a sum of @italic(Seconds +
    PlusTicks ticks). PlusTicks is the number of discrete "ticks" that
    occurred with the same Seconds value. A tick is, conceptually, an
    infinitely small amount of time.

    Ticks are used when comparing time values: when two timestamps
    have the same @link(Seconds) value, then we can compare the PlusTicks
    field. Timestamps are equal only when both Seconds and PlusTicks values
    are exactly equal.

    The practical justification of PlusTicks usage:

    @orderedList(
      @item(First, note that
        X3D standard defines that within one timestamp, only one event may
        pass through one route. This mechanism allows to avoid loops in routes.)

      @item(Now note that when using TCastleSceneCore, Seconds is increased by
        TCastleSceneCore.IncreaseTime. TCastleSceneCore doesn't require how often
        should IncreaseTime be called, in particular you can call multiple
        times TCastleSceneCore.Press, TCastleSceneCore.Release, TCastleSceneCore.PointingDeviceMove
        without continuously updating time. You can even not update time at all,
        and still call TCastleSceneCore.Press and such.

        This is a good thing --- it allows TCastleSceneCore to be very flexible.
        The idea is that sensors are activated when user interface reports
        some event. You don't have to update time before every TCastleSceneCore.Press
        and such.)

      @item(The potential problem here is that when you call
        TCastleSceneCore.Press twice, without the TCastleSceneCore.IncreaseTime
        in between, then the second TCastleSceneCore.Press events will have "clogged" routes.
        Events send from the second TCastleSceneCore.Press may be blocked on routes,
        since they will be detected as occuring within the same timestamp,
        so (following VRML/X3D standard) they'll have to be ignored.)

      @item(Using "Seconds seconds + PlusTicks ticks" allows to avoid this.
        Each TCastleSceneCore.Press and such increases world time by 1 tick
        -- this way, the second TCastleSceneCore.Press will have one more tick, so will
        always be considered later, and things will work Ok.)
    ) }
  TX3DTime = record
    Seconds: TFloatTime;
    PlusTicks: Cardinal;

    class function Oldest: TX3DTime; static;

    class operator {$ifdef FPC}>{$else}GreaterThan{$endif} (const Time1, Time2: TX3DTime): boolean;
    class operator {$ifdef FPC}>={$else}GreaterThanOrEqual{$endif} (const Time1, Time2: TX3DTime): boolean;
    class operator {$ifdef FPC}<{$else}LessThan{$endif} (const Time1, Time2: TX3DTime): boolean;
    class operator {$ifdef FPC}<={$else}LessThanOrEqual{$endif} (const Time1, Time2: TX3DTime): boolean;
  end;
  PX3DTime = ^TX3DTime;

  TFloatTime = CastleTimeUtils.TFloatTime;

const
  OldestX3DTime: TX3DTime = (Seconds: OldestTime; PlusTicks: 0)
    deprecated 'use TX3DTime.Oldest';

type
  TX3DTimeList = {$ifdef FPC}specialize{$endif} TStructList<TX3DTime>;

implementation

class function TX3DTime.Oldest: TX3DTime;
const
  R: TX3DTime = (Seconds: OldestTime; PlusTicks: 0);
begin
  Result := R;
end;

class operator TX3DTime.{$ifdef FPC}>{$else}GreaterThan{$endif} (const Time1, Time2: TX3DTime): boolean;
begin
  Result := (Time1.Seconds > Time2.Seconds) or
    ( (Time1.Seconds = Time2.Seconds) and
      (Time1.PlusTicks > Time2.PlusTicks) );
end;

class operator TX3DTime.{$ifdef FPC}>={$else}GreaterThanOrEqual{$endif} (const Time1, Time2: TX3DTime): boolean;
begin
  Result := (Time1.Seconds > Time2.Seconds) or
    ( (Time1.Seconds = Time2.Seconds) and
      (Time1.PlusTicks >= Time2.PlusTicks) );
end;

class operator TX3DTime.{$ifdef FPC}<{$else}LessThan{$endif} (const Time1, Time2: TX3DTime): boolean;
begin
  Result := (Time1.Seconds < Time2.Seconds) or
    ( (Time1.Seconds = Time2.Seconds) and
      (Time1.PlusTicks < Time2.PlusTicks) );
end;

class operator TX3DTime.{$ifdef FPC}<={$else}LessThanOrEqual{$endif} (const Time1, Time2: TX3DTime): boolean;
begin
  Result := (Time1.Seconds < Time2.Seconds) or
    ( (Time1.Seconds = Time2.Seconds) and
      (Time1.PlusTicks <= Time2.PlusTicks) );
end;

end.
