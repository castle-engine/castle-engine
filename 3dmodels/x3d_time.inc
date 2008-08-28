{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}
  INodeX3DTimeDependentNode = interface(INodeX3DChildNode)
  ['{19D68914-F2BA-4CFA-90A1-F561DB264678}']
    property Fdloop: TSFBool { read FFdloop } { };
    property FdpauseTime: TSFTime { read FFdpauseTime } { };
    property FdresumeTime: TSFTime { read FFdresumeTime } { };
    property FdstartTime: TSFTime { read FFdstartTime } { };
    property FdstopTime: TSFTime { read FFdstopTime } { };
    property EventelapsedTime: TVRMLEvent { read FEventelapsedTime } { };
    property EventisActive: TVRMLEvent { read FEventisActive } { };
    property EventisPaused: TVRMLEvent { read FEventisPaused } { };
  end;

  TNodeX3DTimeDependentNode = class(TNodeX3DChildNode, INodeX3DTimeDependentNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdloop: TSFBool;
    public property Fdloop: TSFBool read FFdloop;

    private FFdpauseTime: TSFTime;
    public property FdpauseTime: TSFTime read FFdpauseTime;

    private FFdresumeTime: TSFTime;
    public property FdresumeTime: TSFTime read FFdresumeTime;

    private FFdstartTime: TSFTime;
    public property FdstartTime: TSFTime read FFdstartTime;

    private FFdstopTime: TSFTime;
    public property FdstopTime: TSFTime read FFdstopTime;

    { Event: SFTime, out } { }
    private FEventelapsedTime: TVRMLEvent;
    public property EventelapsedTime: TVRMLEvent read FEventelapsedTime;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    { Event: SFBool, out } { }
    private FEventisPaused: TVRMLEvent;
    public property EventisPaused: TVRMLEvent read FEventisPaused;
  end;

  TTimeFunction = function: TKamTime of object;

  { Common helper for all X3DTimeDependentNode descendants.
    This includes things descending from class TNodeX3DTimeDependentNode,
    and from interface INodeX3DTimeDependentNode.

    It would be cleaner to have Node declared as INodeX3DTimeDependentNode,
    and have INodeX3DTimeDependentNode contain common fields.
    Then a lot of fields of this class would not be needed, as they
    would be accessible as INodeX3DTimeDependentNode fields.
    TODO: maybe in the future. }
  TTimeDependentNodeHandler = class
  private
    FIsActive: boolean;
    FIsPaused: boolean;
    FElapsedTime: TKamTime;
    procedure SetIsActive(const Value: boolean);
    procedure SetIsPaused(const Value: boolean);
    procedure SetElapsedTime(const Value: TKamTime);
  public
    Node: TVRMLNode;

    CurrentTime: TKamTime;

    { These describe current state of this TimeDependentNode,
      see X3D specification about "Time" component.

      Their setting causes appropriate events to be generated
      (with Time = CurrentTime, so be sure to update it before changing
      these properties. SetWorldTime automatically does this for you.).

      @groupBegin }
    property IsActive: boolean read FIsActive write SetIsActive;
    property IsPaused: boolean read FIsPaused write SetIsPaused;
    property ElapsedTime: TKamTime read FElapsedTime write SetElapsedTime;
    { @groupEnd }

    { Cycle interval for this time-dependent node. }
    OnCycleInterval: TTimeFunction;
    function CycleInterval: TKamTime;

    Fdloop: TSFBool;
    FdpauseTime: TSFTime;
    FdresumeTime: TSFTime;
    FdstartTime: TSFTime;
    FdstopTime: TSFTime;

    EventelapsedTime: TVRMLEvent;
    EventisActive: TVRMLEvent;
    EventisPaused: TVRMLEvent;

    { Call this when world time increases.
      This is the most important method of this class, that basically
      implements time-dependent nodes operations.

      OldValue, NewValue and TimeIncrease must match, as produced
      by TVRMLScene.SetWorldTime and friends.

      TimeIncrease = 0.0 here means "TimeIncrease is unknown",
      this can happen only when were called by ResetWorldTime.
      In other circumstances, TimeIncrease must be > 0.

      References: see X3D specification "Time" component,
      8.2 ("concepts") for logic behind all those start/stop/pause/resumeTime,
      cycleInterval, loop properties.

      @param(SomethingChanged May only be set to @true (this method never
        sets this to @false), when some state of time-dependent node
        changed.) }
    procedure SetWorldTime(const OldValue, NewValue, TimeIncrease: TKamTime;
      var SomethingChanged: boolean);
  end;

  TNodeTimeSensor = class(TNodeX3DTimeDependentNode, INodeX3DSensorNode)
  private
    FTimeDependentNodeHandler: TTimeDependentNodeHandler;
    function CycleInterval: TKamTime;
    procedure EventElapsedTimeReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcycleInterval: TSFTime;
    public property FdcycleInterval: TSFTime read FFdcycleInterval;

    { Event: SFTime, out } { }
    private FEventcycleTime: TVRMLEvent;
    public property EventcycleTime: TVRMLEvent read FEventcycleTime;

    { Event: SFFloat, out } { }
    private FEventfraction_changed: TVRMLEvent;
    public property Eventfraction_changed: TVRMLEvent read FEventfraction_changed;

    { Event: SFTime, out } { }
    private FEventtime: TVRMLEvent;
    public property Eventtime: TVRMLEvent read FEventtime;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    property TimeDependentNodeHandler: TTimeDependentNodeHandler
      read FTimeDependentNodeHandler;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DTimeDependentNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdloop := TSFBool.Create(Self, 'loop', false);
  Fields.Add(FFdloop);

  FFdpauseTime := TSFTime.Create(Self, 'pauseTime', 0);
  Fields.Add(FFdpauseTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdresumeTime := TSFTime.Create(Self, 'resumeTime', 0);
  Fields.Add(FFdresumeTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstartTime := TSFTime.Create(Self, 'startTime', 0);
  Fields.Add(FFdstartTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstopTime := TSFTime.Create(Self, 'stopTime', 0);
  Fields.Add(FFdstopTime);
  { X3D specification comment: (-Inf,Inf) }

  FEventelapsedTime := TVRMLEvent.Create(Self, 'elapsedTime', TSFTime, false);
  Events.Add(FEventelapsedTime);

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventisPaused := TVRMLEvent.Create(Self, 'isPaused', TSFBool, false);
  Events.Add(FEventisPaused);

  DefaultContainerField := 'children';
end;

procedure TTimeDependentNodeHandler.SetIsActive(const Value: boolean);
begin
  if Value <> FIsActive then
  begin
    FIsActive := Value;
    EventIsActive.Send(Value, CurrentTime);
  end;
end;

procedure TTimeDependentNodeHandler.SetIsPaused(const Value: boolean);
begin
  if Value <> FIsPaused then
  begin
    FIsPaused := Value;
    EventIsPaused.Send(Value, CurrentTime);
  end;
end;

procedure TTimeDependentNodeHandler.SetElapsedTime(const Value: TKamTime);
begin
  if Value <> FElapsedTime then
  begin
    FElapsedTime := Value;
    EventElapsedTime.Send(Value, CurrentTime);
  end;
end;

function TTimeDependentNodeHandler.CycleInterval: TKamTime;
begin
  Assert(Assigned(OnCycleInterval));
  Result := OnCycleInterval();
end;

procedure TTimeDependentNodeHandler.SetWorldTime(
  const OldValue, NewValue, TimeIncrease: TKamTime;
  var SomethingChanged: boolean);

var
  NewIsActive: boolean;
  NewIsPaused: boolean;
  NewElapsedTime: TKamTime;

  { $define LOG_TIME_DEPENDENT_NODES}

  { Increase NewElapsedTime, taking care of
    CycleInterval and looping.
    StopOnNonLoopedEnd says what to do if NewElapsedTime passed
    CycleInterval and not looping. }
  procedure IncreaseElapsedTime(const Increase: TKamTime;
    StopOnNonLoopedEnd: boolean);
  begin
    NewElapsedTime := NewElapsedTime + Increase;

    if NewElapsedTime > CycleInterval then
    begin
      if CycleInterval <> 0 then
      begin
        if FdLoop.Value then
        begin
          NewElapsedTime := FloatModulo(NewElapsedTime, CycleInterval);
        end else
        begin
          if StopOnNonLoopedEnd then
            NewIsActive := false;
        end;
      end else
      begin
        { for cycleInterval = 0 this always remains 0 }
        NewElapsedTime := 0;

        if (not FdLoop.Value) and StopOnNonLoopedEnd then
          NewIsActive := false;
      end;
    end;
  end;

begin
  {$ifdef LOG_TIME_DEPENDENT_NODES}
  if Log then
    WritelnLog('TimeDependentNodes', Format('%s: time changes from %f (by +%f) to %f. Before state: active %s, paused %s, loop %s',
      [ Node.NodeTypeName,
        OldValue, TimeIncrease, NewValue,
        BoolToStr[IsActive], BoolToStr[IsPaused], BoolToStr[FdLoop.Value]
        ]));
  {$endif}

  CurrentTime := OldValue;

  { Note that each set of IsActive, IsPaused, ElapsedTime may generate events.
    So we cannot carelessly set them many times in this method,
    as double events are bad (besides possible unneeded overhead with
    propagating them, route ignore events at the same timestamp,
    since they may indicate loops in routes).

    Solution: below we will operate on local copies of these variables,
    like NewIsActive, NewIsPaused etc.
    Only at the end of this method we will actually set the properties,
    causing events (if their values changed). }

  { For ResetWorldTime, set time-dependent node properties to default
    (like after TNodeTimeHandler creation) at the beginning. }
  if TimeIncrease = 0 then
  begin
    NewIsActive := false;
    NewIsPaused := false;
    NewElapsedTime := 0;
  end else
  begin
    NewIsActive := IsActive;
    NewIsPaused := IsPaused;
    NewElapsedTime := ElapsedTime;
  end;

  if not NewIsActive then
  begin
    if (NewValue >= FdStartTime.Value) and
       ( (NewValue < FdStopTime.Value) or
         { stopTime is ignored if it's <= startTime }
         (FdStopTime.Value <= FdStartTime.Value) ) and
       { avoid starting the movie if it should be stopped according
         to loop and cycleInterval }
       not ( (NewValue - FdStartTime.Value >
             CycleInterval) and
             (not FdLoop.Value) ) then
    begin
      NewIsActive := true;
      NewIsPaused := false;
      NewElapsedTime := 0;
      { Do not advance by TimeIncrease (time from last WorldTime),
        advance only by the time passed since startTime. }
      IncreaseElapsedTime(NewValue - FdStartTime.Value, true);
      SomethingChanged := true;
    end;
  end else
  if NewIsPaused then
  begin
    if (NewValue >= FdResumeTime.Value) and
       (FdResumeTime.Value > FdPauseTime.Value) then
    begin
      NewIsPaused := false;
      { Advance only by the time passed since resumeTime. }
      IncreaseElapsedTime(NewValue - FdResumeTime.Value, true);
      SomethingChanged := true;
    end;
  end else
  begin
    SomethingChanged := true;

    if (NewValue >= FdStopTime.Value) and
       { stopTime is ignored if it's <= startTime }
       (FdStopTime.Value > FdStartTime.Value) then
    begin
      NewIsActive := false;
      { advance only to the stopTime }
      if TimeIncrease <> 0 then
        IncreaseElapsedTime(TimeIncrease -
          (NewValue - FdStopTime.Value), false);
    end else
    if (NewValue >= FdPauseTime.Value) and
       (FdPauseTime.Value > FdResumeTime.Value) then
    begin
      NewIsPaused := true;
      { advance only to the pauseTime }
      if TimeIncrease <> 0 then
        IncreaseElapsedTime(TimeIncrease -
          (NewValue - FdPauseTime.Value), false);
    end else
    begin
      { active and not paused movie }
      if TimeIncrease = 0 then
        NewElapsedTime := 0 else
        IncreaseElapsedTime(TimeIncrease, true);
    end;
  end;

  { now set actual IsActive, IsPaused, ElapsedTime properties from
    their NewXxx counterparts. We take care to set them in proper
    order, to send events in proper order:
    if you just activated the movie, then isActive should be sent first,
    before elapsedTime.
    If the movie was deactivated, then last elapsedTime should be sent last. }
  if NewIsActive then
  begin
    IsActive := NewIsActive;
    if not NewIsPaused then
    begin
      IsPaused := NewIsPaused;
      ElapsedTime := NewElapsedTime;
    end else
    begin
      ElapsedTime := NewElapsedTime;
      IsPaused := NewIsPaused;
    end;
  end else
  begin
    if not NewIsPaused then
    begin
      IsPaused := NewIsPaused;
      ElapsedTime := NewElapsedTime;
    end else
    begin
      ElapsedTime := NewElapsedTime;
      IsPaused := NewIsPaused;
    end;
    IsActive := NewIsActive;
  end;

  Assert(IsActive = NewIsActive);
  Assert(IsPaused = NewIsPaused);
  Assert(ElapsedTime = NewElapsedTime);

  {$ifdef LOG_TIME_DEPENDENT_NODES}
  if Log then
    WritelnLog('TimeDependentNodes', Format('%s: after: active %s, paused %s',
      [ Node.NodeTypeName,
        BoolToStr[IsActive],
        BoolToStr[IsPaused]]));
  {$endif}
end;

constructor TNodeTimeSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcycleInterval := TSFTime.Create(Self, 'cycleInterval', 1);
  Fields.Add(FFdcycleInterval);
  { X3D specification comment: (0,Inf) }

  FEventcycleTime := TVRMLEvent.Create(Self, 'cycleTime', TSFTime, false);
  Events.Add(FEventcycleTime);

  FEventfraction_changed := TVRMLEvent.Create(Self, 'fraction_changed', TSFFloat, false);
  Events.Add(FEventfraction_changed);

  FEventtime := TVRMLEvent.Create(Self, 'time', TSFTime, false);
  Events.Add(FEventtime);

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  DefaultContainerField := 'children';

  FTimeDependentNodeHandler := TTimeDependentNodeHandler.Create;
  FTimeDependentNodeHandler.Node := Self;
  FTimeDependentNodeHandler.Fdloop := Fdloop;
  FTimeDependentNodeHandler.FdpauseTime := FdpauseTime;
  FTimeDependentNodeHandler.FdresumeTime := FdresumeTime;
  FTimeDependentNodeHandler.FdstartTime := FdstartTime;
  FTimeDependentNodeHandler.FdstopTime := FdstopTime;
  FTimeDependentNodeHandler.EventisActive:= EventisActive;
  FTimeDependentNodeHandler.EventisPaused := EventisPaused;
  FTimeDependentNodeHandler.EventelapsedTime := EventelapsedTime;
  FTimeDependentNodeHandler.OnCycleInterval := @CycleInterval;

  { TODO: for now, elapsedTime is simply passed to set_fraction event. }
  EventelapsedTime.OnReceive.AppendItem(@EventElapsedTimeReceive);
end;

destructor TNodeTimeSensor.Destroy;
begin
  FreeAndNil(FTimeDependentNodeHandler);
  inherited;
end;

function TNodeTimeSensor.CycleInterval: TKamTime;
begin
  Result := FdCycleInterval.Value;
end;

class function TNodeTimeSensor.ClassNodeTypeName: string;
begin
  Result := 'TimeSensor';
end;

function TNodeTimeSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Node is since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTimeSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTimeSensor.EventElapsedTimeReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
begin
  { TODO: for now, elapsedTime/CycleInterval is simply passed to set_fraction event. }
  Eventfraction_changed.Send(
    Single((Value as TSFTime).Value / CycleInterval), Time);
  EventTime.Send(Time, Time);
end;

procedure RegisterTimeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeTimeSensor
  ]);
end;

{$endif read_implementation}
