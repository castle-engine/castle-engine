{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Timer, running the @link(OnTimer) event periodically. }
  TCastleTimer = class(TCastleUserInterface)
  strict private
    FCounteractDelays: boolean;
    FIntervalSeconds, IntervalRemaining: TFloatTime;
    FOnTimer: TNotifyEvent;
    procedure SetIntervalSeconds(AValue: TFloatTime);
  protected
    procedure DoTimer; virtual;
  public
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { How often should we call OnTimer. Value of 0 means to call OnTimer
      in every Update event. }
    property IntervalSeconds: TFloatTime read FIntervalSeconds write SetIntervalSeconds;
    { The event called periodically. }
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    { Should we counteract the delays in timer by firing next event sooner.
      This helps to keep a constant frequency of timer events over a long time,
      and to keep multiple things (like multiple timer instances)
      perfectly synchronized with each other.
      But it may cause to execute a @italic(lot) of timer events at once,
      in case the application hung for some time. }
    property CounteractDelays: boolean
      read FCounteractDelays write FCounteractDelays default false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTimer --------------------------------------------------------------- }

procedure TCastleTimer.SetIntervalSeconds(AValue: TFloatTime);
begin
  if FIntervalSeconds <> AValue then
  begin
    FIntervalSeconds := AValue;
    IntervalRemaining := FIntervalSeconds;
  end;
end;

procedure TCastleTimer.DoTimer;
begin
  if Assigned(OnTimer) then
    OnTimer(Self);
end;

procedure TCastleTimer.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  if IntervalSeconds <> 0 then
  begin
    IntervalRemaining := IntervalRemaining - SecondsPassed;
    if CounteractDelays then
    begin
      while IntervalRemaining < 0 do
      begin
        IntervalRemaining := IntervalRemaining + IntervalSeconds;
        DoTimer;
      end;
    end else
    begin
      if IntervalRemaining < 0 then
      begin
        IntervalRemaining := IntervalSeconds;
        DoTimer;
      end;
    end;
  end else
  begin
    { for IntervalSeconds = 0, just call timer at every Update. }
    DoTimer;
  end;
end;

function TCastleTimer.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'IntervalSeconds') or
     (PropertyName = 'CounteractDelays') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
