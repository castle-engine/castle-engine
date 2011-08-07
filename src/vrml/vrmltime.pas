{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML time stuff. }
unit VRMLTime;

interface

uses KambiUtils, KambiTimeUtils, GenericStructList;

type
  { This is a complete timestamp for VRML events.
    For most purposes, you're interested only in it's @link(Seconds) field,
    this is the amount of continous time that passed (in double-precision
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
        VRML standard defines that within one timestamp, only one event may
        pass through one route. This mechanism allows to avoid loops in routes.)

      @item(Now note that when using TVRMLScene, Seconds is increased by
        TVRMLScene.IncreaseTime. TVRMLScene doesn't require how often
        should IncreaseTime be called, in particular you can call multiple
        times TVRMLScene.KeyDown, TVRMLScene.KeyUp, TVRMLScene.PointingDeviceMove
        without continously updating time. You can even not update time at all,
        and still call TVRMLScene.KeyDown and such.

        This is a good thing --- it allows TVRMLScene to be very flexible.
        The idea is that sensors are activated when user interface reports
        some event. You don't have to update time before every KeyDown / KeyUp
        and such.)

      @item(The potential problem here is that when you call
        TVRMLScene.KeyDown twice, without the TVRMLScene.IncreaseTime
        in between, then the second KeyDown event will have "clogged" routes.
        Events send from the second KeyDown may be blocked on routes,
        since they will be detected as occuring within the same timestamp,
        so (following VRML standard) they'll have to be ignored.)

      @item(Using "Seconds seconds + PlusTicks ticks" allows to avoid this.
        Each TVRMLScene.KeyDown and such increases world time by 1 tick
        -- this way, the second KeyDown will have one more tick, so will
        always be considered later, and things will work Ok.)
    ) }
  TVRMLTime = record
    Seconds: TKamTime;
    PlusTicks: Cardinal;
  end;
  PVRMLTime = ^TVRMLTime;

  TKamTime = KambiTimeUtils.TKamTime;

const
  OldestVRMLTime: TVRMLTime = (Seconds: OldestTime; PlusTicks: 0);

{$ifdef FPC_OBJFPC}
operator >  (const Time1: TVRMLTime; const Time2: TVRMLTime): boolean;
operator >= (const Time1: TVRMLTime; const Time2: TVRMLTime): boolean;
operator <  (const Time1: TVRMLTime; const Time2: TVRMLTime): boolean;
operator <= (const Time1: TVRMLTime; const Time2: TVRMLTime): boolean;
{$endif FPC_OBJFPC}

type
  TVRMLTimeList = specialize TGenericStructList<TVRMLTime>;

implementation

{$ifdef FPC_OBJFPC}
operator >  (const Time1: TVRMLTime; const Time2: TVRMLTime): boolean;
begin
  Result := (Time1.Seconds > Time2.Seconds) or
    ( (Time1.Seconds = Time2.Seconds) and
      (Time1.PlusTicks > Time2.PlusTicks) );
end;

operator >= (const Time1: TVRMLTime; const Time2: TVRMLTime): boolean;
begin
  Result := (Time1.Seconds > Time2.Seconds) or
    ( (Time1.Seconds = Time2.Seconds) and
      (Time1.PlusTicks >= Time2.PlusTicks) );
end;

operator <  (const Time1: TVRMLTime; const Time2: TVRMLTime): boolean;
begin
  Result := (Time1.Seconds < Time2.Seconds) or
    ( (Time1.Seconds = Time2.Seconds) and
      (Time1.PlusTicks < Time2.PlusTicks) );
end;

operator <= (const Time1: TVRMLTime; const Time2: TVRMLTime): boolean;
begin
  Result := (Time1.Seconds < Time2.Seconds) or
    ( (Time1.Seconds = Time2.Seconds) and
      (Time1.PlusTicks <= Time2.PlusTicks) );
end;
{$endif FPC_OBJFPC}

end.
