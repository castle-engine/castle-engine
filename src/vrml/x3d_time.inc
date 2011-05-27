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

{$ifdef read_interface}
  TTimeDependentNodeHandler = class;

  { }
  INodeX3DTimeDependentNode = interface(INodeX3DChildNode)
  ['{19D68914-F2BA-4CFA-90A1-F561DB264678}']
  (*
    property Fdloop: TSFBool { read FFdloop } { };
    property FdpauseTime: TSFTime { read FFdpauseTime } { };
    property FdresumeTime: TSFTime { read FFdresumeTime } { };
    property FdstartTime: TSFTime { read FFdstartTime } { };
    property FdstopTime: TSFTime { read FFdstopTime } { };
    property EventelapsedTime: TVRMLEvent { read FEventelapsedTime } { };
    property EventisActive: TVRMLEvent { read FEventisActive } { };
    property EventisPaused: TVRMLEvent { read FEventisPaused } { };
  *)

    function GetTimeDependentNodeHandler: TTimeDependentNodeHandler;

    property TimeDependentNodeHandler: TTimeDependentNodeHandler
      read GetTimeDependentNodeHandler;
  end;

  TTimeFunction = function: TKamTime of object;

  { Common helper for all X3DTimeDependentNode descendants.
    This includes things descending from interface INodeX3DTimeDependentNode,
    in particular (but not only) descending from class
    TNodeX3DTimeDependentNode.

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
    FElapsedTimeInCycle: TKamTime;
    procedure SetIsActive(const Value: boolean);
    procedure SetIsPaused(const Value: boolean);
    procedure SetElapsedTime(const Value: TKamTime);
  public
    Node: TVRMLNode;

    CurrentTime: TVRMLTime;

    { These describe current state of this TimeDependentNode,
      see X3D specification about "Time" component.

      Their setting causes appropriate events to be generated
      (with Time = CurrentTime, so be sure to update it before changing
      these properties. SetTime automatically does this for you.).

      @groupBegin }
    property IsActive: boolean read FIsActive write SetIsActive;
    property IsPaused: boolean read FIsPaused write SetIsPaused;
    property ElapsedTime: TKamTime read FElapsedTime write SetElapsedTime;
    { @groupEnd }
  public
    { Cycle interval for this time-dependent node. }
    OnCycleInterval: TTimeFunction;
    function CycleInterval: TKamTime;
  public
    Fdloop: TSFBool;
    FdpauseTime: TSFTime;
    FdresumeTime: TSFTime;
    FdstartTime: TSFTime;
    FdstopTime: TSFTime;
    { May be @nil if node doesn't have an "enabled" field. }
    Fdenabled: TSFBool;

    EventelapsedTime: TVRMLEvent;
    EventisActive: TVRMLEvent;
    EventisPaused: TVRMLEvent;
    { May be @nil if node doesn't have a "cycleTime" event. }
    EventcycleTime: TVRMLEvent;

    { Like ElapsedTime, but spent in the current cycle.
      When cycleInterval = 0, this is always 0.
      When cycleInterval <> 0, this is always >= 0 and < cycleInterval . }
    property ElapsedTimeInCycle: TKamTime read FElapsedTimeInCycle;

    { Call this when world time increases.
      This is the most important method of this class, that basically
      implements time-dependent nodes operations.

      OldValue, NewValue and TimeIncrease must match, as produced
      by TVRMLScene.SetTime and friends.

      When ResetTime = true, this means that "TimeIncrease value is unknown"
      (you @italic(must) pass TimeIncrease = 0 in this case).
      This can happen only when were called by ResetTime.

      In other circumstances, TimeIncrease must be >= 0.
      (It's allowed to pass TimeIncrease = 0 and ResetTime = false, this doesn't
      advance the clock, but is a useful trick to force some update,
      see TVRMScene.ChangedField implementation.)

      References: see X3D specification "Time" component,
      8.2 ("concepts") for logic behind all those start/stop/pause/resumeTime,
      cycleInterval, loop properties.

      @returns(If some state of time-dependent node  changed.) }
    function SetTime(const OldValue, NewValue: TVRMLTime;
      const TimeIncrease: TKamTime; const ResetTime: boolean): boolean;
  end;

  TNodeX3DTimeDependentNode = class(TNodeX3DChildNode, INodeX3DTimeDependentNode)
  private
    FTimeDependentNodeHandler: TTimeDependentNodeHandler;
    { To satify INodeX3DTimeDependentNode }
    function GetTimeDependentNodeHandler: TTimeDependentNodeHandler;
  protected
    function CycleInterval: TKamTime; virtual; abstract;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

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

    property TimeDependentNodeHandler: TTimeDependentNodeHandler
      read FTimeDependentNodeHandler;
  end;

  TNodeTimeSensor = class(TNodeX3DTimeDependentNode, INodeX3DSensorNode)
  private
    procedure EventElapsedTimeReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  protected
    function CycleInterval: TKamTime; override;
  public
    procedure CreateNode; override;

    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ Field types for startTime/stopTime ----------------------------------------- }

type
  { SFTime that ignores it's input event when parent time-dependent node
    is active. }
  TSFTimeIgnoreWhenActive = class(TSFTime)
  protected
    function ExposedEventsFieldClass: TVRMLFieldClass; override;
    procedure ExposedEventReceive(Event: TVRMLEvent; AValue: TVRMLField;
      const Time: TVRMLTime); override;
  end;

  TSFStopTime = class(TSFTime)
  protected
    function ExposedEventsFieldClass: TVRMLFieldClass; override;
    procedure ExposedEventReceive(Event: TVRMLEvent; AValue: TVRMLField;
      const Time: TVRMLTime); override;
  end;

function TSFTimeIgnoreWhenActive.ExposedEventsFieldClass: TVRMLFieldClass;
begin
  Result := TSFTime;
end;

procedure TSFTimeIgnoreWhenActive.ExposedEventReceive(
  Event: TVRMLEvent; AValue: TVRMLField; const Time: TVRMLTime);

  { Is parent node an active X3DTimeDependentNode.

    Doing this inside local procedure to limit lifetime of INodeX3DTimeDependentNode,
    to secure against http://bugs.freepascal.org/view.php?id=10374 }
  function Active: boolean;
  begin
    Result := (ParentNode <> nil) and
      Supports(ParentNode, INodeX3DTimeDependentNode) and
      (ParentNode as INodeX3DTimeDependentNode).TimeDependentNodeHandler.IsActive;
  end;

begin
  if Active then Exit;
  inherited;
end;

function TSFStopTime.ExposedEventsFieldClass: TVRMLFieldClass;
begin
  Result := TSFTime;
end;

procedure TSFStopTime.ExposedEventReceive(Event: TVRMLEvent; AValue: TVRMLField;
  const Time: TVRMLTime);

  { Is parent node an active X3DTimeDependentNode, and AValue <= startTime.

    Doing this inside local procedure to limit lifetime of INodeX3DTimeDependentNode,
    to secure against http://bugs.freepascal.org/view.php?id=10374 }
  function Active: boolean;
  var
    H: TTimeDependentNodeHandler;
  begin
    Result := (ParentNode <> nil) and
      Supports(ParentNode, INodeX3DTimeDependentNode);
    if Result then
    begin
      H := (ParentNode as INodeX3DTimeDependentNode).TimeDependentNodeHandler;
      Result := H.IsActive and
        ( (AValue as TSFTime).Value <= H.FdStartTime.Value );
    end;
  end;

begin
  if Active then Exit;
  inherited;
end;

{ TTimeDependentNodeHandler -------------------------------------------------- }

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

function TTimeDependentNodeHandler.SetTime(
  const OldValue, NewValue: TVRMLTime;
  const TimeIncrease: TKamTime; const ResetTime: boolean): boolean;

var
  NewIsActive: boolean;
  NewIsPaused: boolean;
  NewElapsedTime: TKamTime;
  CycleTimeSend: boolean;
  CycleTime: TKamTime;

  { $define LOG_TIME_DEPENDENT_NODES}

  { Increase NewElapsedTime and ElapsedTimeInCycle, taking care of
    CycleInterval and looping.
    StopOnNonLoopedEnd says what to do if NewElapsedTime passed
    CycleInterval and not looping.

    May indicate that CycleTime should be send (by setting CycleTimeSend to true
    and CycleTime value) if the *new* cycle started. This means
    that new ElapsedTime reached the non-zero CycleInterval
    and loop = TRUE. }
  procedure IncreaseElapsedTime(const Increase: TKamTime;
    StopOnNonLoopedEnd: boolean);
  begin
    NewElapsedTime      := NewElapsedTime      + Increase;
    FElapsedTimeInCycle := FElapsedTimeInCycle + Increase;

    if FElapsedTimeInCycle > CycleInterval then
    begin
      if CycleInterval <> 0 then
      begin
        if FdLoop.Value then
        begin
          FElapsedTimeInCycle := FloatModulo(FElapsedTimeInCycle, CycleInterval);
          { Send the time value when the cycle started, which was
            a little earlier than CurrentTime: earlier by ElapsedTimeInCycle. }
          CycleTimeSend := true;
          CycleTime := CurrentTime.Seconds - ElapsedTimeInCycle;
        end else
        begin
          if StopOnNonLoopedEnd then
            NewIsActive := false;
        end;
      end else
      begin
        { for cycleInterval = 0 this always remains 0 }
        FElapsedTimeInCycle := 0;

        if (not FdLoop.Value) and StopOnNonLoopedEnd then
          NewIsActive := false;
      end;
    end;
  end;

begin
  { if ResetTime, then TimeIncrease must be always exactly 0 }
  Assert((not ResetTime) or (TimeIncrease = 0));

  {$ifdef LOG_TIME_DEPENDENT_NODES}
  if Log then
    WritelnLog('TimeDependentNodes', Format('%s: time changes from %f (by +%f) to %f. Before state: active %s, paused %s, loop %s',
      [ Node.NodeTypeName,
        OldValue.Seconds, TimeIncrease, NewValue.Seconds,
        BoolToStr[IsActive], BoolToStr[IsPaused], BoolToStr[FdLoop.Value]
        ]));
  {$endif}

  CurrentTime := NewValue;

  if (Fdenabled <> nil) and (not Fdenabled.Value) then
  begin
    IsActive := false;
    Exit;
  end;

  { Note that each set of IsActive, IsPaused, ElapsedTime may generate events.
    So we cannot carelessly set them many times in this method,
    as double events are bad (besides possible unneeded overhead with
    propagating them, route ignore events at the same timestamp,
    since they may indicate loops in routes).

    Solution: below we will operate on local copies of these variables,
    like NewIsActive, NewIsPaused etc.
    Only at the end of this method we will actually set the properties,
    causing events (if their values changed). }

  { For ResetTime, set time-dependent node properties to default
    (like after TNodeTimeHandler creation) at the beginning. }
  if ResetTime then
  begin
    NewIsActive := false;
    NewIsPaused := false;
    NewElapsedTime := 0;
    FElapsedTimeInCycle := 0;
  end else
  begin
    NewIsActive := IsActive;
    NewIsPaused := IsPaused;
    NewElapsedTime := ElapsedTime;
    { Leave ElapsedTimeInCycle as it was }
  end;

  CycleTimeSend := false;

  if not NewIsActive then
  begin
    if (NewValue.Seconds >= FdStartTime.Value) and
       ( (NewValue.Seconds < FdStopTime.Value) or
         { stopTime is ignored if it's <= startTime }
         (FdStopTime.Value <= FdStartTime.Value) ) and
       { avoid starting the movie if it should be stopped according
         to loop and cycleInterval }
       not ( (NewValue.Seconds - FdStartTime.Value >
             CycleInterval) and
             (not FdLoop.Value) ) then
    begin
      NewIsActive := true;
      NewIsPaused := false;
      NewElapsedTime := 0;
      FElapsedTimeInCycle := 0;

      { Do not advance by TimeIncrease (time from last Time),
        advance only by the time passed since startTime. }
      IncreaseElapsedTime(NewValue.Seconds - FdStartTime.Value, true);

      if not CycleTimeSend then
      begin
        { Then we still have the initial cycleTime event to generate
          (IncreaseElapsedTime didn't do it for us).
          This should be the "time at the beginning of the current cycle".

          Since IncreaseElapsedTime didn't detect a new cycle,
          so NewElapsedTime = NewValue.Seconds - FdStartTime.Value fits
          (is < ) within the CycleInterval. So startTime is the beginning
          of our cycle.

          Or StartedNewCycle = false may mean that CycleInterval is zero
          or loop = FALSE. We will check later (before actually sending
          cycleTime) that sensor is active, and if it's active ->
          we still should make the initial cycleTime.

          So in both cases, proper cycleTime is startTime. }
        CycleTimeSend := true;
        CycleTime := FdStartTime.Value;
      end;

      Result := true;
    end;
  end else
  if NewIsPaused then
  begin
    if (NewValue.Seconds >= FdResumeTime.Value) and
       (FdResumeTime.Value > FdPauseTime.Value) then
    begin
      NewIsPaused := false;
      { Advance only by the time passed since resumeTime. }
      IncreaseElapsedTime(NewValue.Seconds - FdResumeTime.Value, true);
      Result := true;
    end;
  end else
  begin
    Result := true;

    if (NewValue.Seconds >= FdStopTime.Value) and
       { stopTime is ignored if it's <= startTime }
       (FdStopTime.Value > FdStartTime.Value) then
    begin
      NewIsActive := false;
      { advance only to the stopTime }
      if TimeIncrease <> 0 then
        IncreaseElapsedTime(TimeIncrease -
          (NewValue.Seconds - FdStopTime.Value), false);
    end else
    if (NewValue.Seconds >= FdPauseTime.Value) and
       (FdPauseTime.Value > FdResumeTime.Value) then
    begin
      NewIsPaused := true;
      { advance only to the pauseTime }
      if TimeIncrease <> 0 then
        IncreaseElapsedTime(TimeIncrease -
          (NewValue.Seconds - FdPauseTime.Value), false);
    end else
    begin
      { active and not paused movie }
      if ResetTime then
      begin
        NewElapsedTime := 0;
        FElapsedTimeInCycle := 0;
      end else
        IncreaseElapsedTime(TimeIncrease, true);
    end;
  end;

  { now set actual IsActive, IsPaused, ElapsedTime properties from
    their NewXxx counterparts. We take care to set them in proper
    order, to send events in proper order:
    if you just activated the movie, then isActive should be sent first,
    before elapsedTime.
    If the movie was deactivated, then last elapsedTime should be sent last.

    Send cycleTime only if NewIsActive, and after sending isActive = TRUE. }
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

    if CycleTimeSend and (EventCycleTime <> nil) then
      EventCycleTime.Send(CycleTime, CurrentTime);
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

  { This will be true in most usual situations, but in some complicated
    setups sending isActive/isPaused/elapsedTime (and sending elapsedTime
    causes sending other events for TimeSensor) may cause sending another
    event to the same node, thus calling SetTime recursively,
    and changing values at the end. Example: rrtankticks when often
    clicking on firing the cannon. So these assertions do not have to be
    true in complicated scenes.

  Assert(IsActive = NewIsActive);
  Assert(IsPaused = NewIsPaused);
  Assert(ElapsedTime = NewElapsedTime);
  }

  {$ifdef LOG_TIME_DEPENDENT_NODES}
  if Log then
    WritelnLog('TimeDependentNodes', Format('%s: after: active %s, paused %s',
      [ Node.NodeTypeName,
        BoolToStr[IsActive],
        BoolToStr[IsPaused]]));
  {$endif}
end;

{ VRML/X3D nodes ------------------------------------------------------------- }

procedure TNodeX3DTimeDependentNode.CreateNode;
begin
  inherited;

  FFdloop := TSFBool.Create(Self, 'loop', false);
  Fields.Add(FFdloop);

  FFdpauseTime := TSFTime.Create(Self, 'pauseTime', 0);
   FdpauseTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdpauseTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdresumeTime := TSFTime.Create(Self, 'resumeTime', 0);
   FdresumeTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdresumeTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstartTime := TSFTimeIgnoreWhenActive.Create(Self, 'startTime', 0);
   FdstartTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdstartTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstopTime := TSFStopTime.Create(Self, 'stopTime', 0);
   FdstopTime.ChangesAlways := [chTimeStopStart];
  Fields.Add(FFdstopTime);
  { X3D specification comment: (-Inf,Inf) }

  FEventelapsedTime := TVRMLEvent.Create(Self, 'elapsedTime', TSFTime, false);
  Events.Add(FEventelapsedTime);

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventisPaused := TVRMLEvent.Create(Self, 'isPaused', TSFBool, false);
  Events.Add(FEventisPaused);

  DefaultContainerField := 'children';

  FTimeDependentNodeHandler := TTimeDependentNodeHandler.Create;
  FTimeDependentNodeHandler.Node := Self;
  FTimeDependentNodeHandler.Fdloop := Fdloop;
  FTimeDependentNodeHandler.FdpauseTime := FdpauseTime;
  FTimeDependentNodeHandler.FdresumeTime := FdresumeTime;
  FTimeDependentNodeHandler.FdstartTime := FdstartTime;
  FTimeDependentNodeHandler.FdstopTime := FdstopTime;
  { descendants shoud set FTimeDependentNodeHandler.Fdenabled }
  FTimeDependentNodeHandler.EventisActive:= EventisActive;
  FTimeDependentNodeHandler.EventisPaused := EventisPaused;
  FTimeDependentNodeHandler.EventelapsedTime := EventelapsedTime;
  { descendants shoud set FTimeDependentNodeHandler.EventCycleTime }
  { descendants shoud override CycleInterval }
  FTimeDependentNodeHandler.OnCycleInterval := @CycleInterval;
end;

destructor TNodeX3DTimeDependentNode.Destroy;
begin
  FreeAndNil(FTimeDependentNodeHandler);
  inherited;
end;

function TNodeX3DTimeDependentNode.GetTimeDependentNodeHandler: TTimeDependentNodeHandler;
begin
  Result := FTimeDependentNodeHandler;
end;

procedure TNodeTimeSensor.CreateNode;
begin
  inherited;

  FFdcycleInterval := TSFTimeIgnoreWhenActive.Create(Self, 'cycleInterval', 1);
  Fields.Add(FFdcycleInterval);
  { X3D specification comment: (0,Inf) }

  FEventcycleTime := TVRMLEvent.Create(Self, 'cycleTime', TSFTime, false);
  Events.Add(FEventcycleTime);
  { cycleTime_changed name is used e.g. by
    www.web3d.org/x3d/content/examples/Basic/StudentProjects/WallClock.x3d }
  FEventcycleTime.AddAlternativeName('cycleTime_changed', 0);

  FEventfraction_changed := TVRMLEvent.Create(Self, 'fraction_changed', TSFFloat, false);
  Events.Add(FEventfraction_changed);

  FEventtime := TVRMLEvent.Create(Self, 'time', TSFTime, false);
  Events.Add(FEventtime);

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  DefaultContainerField := 'children';

  { set TimeDependentNodeHandler }
  TimeDependentNodeHandler.Fdenabled := Fdenabled;
  TimeDependentNodeHandler.EventCycleTime := EventCycleTime;

  { On receiving new elapsedTime, we send other continous events. }
  EventelapsedTime.OnReceive.Add(@EventElapsedTimeReceive);
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
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Node is since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeTimeSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTimeSensor.EventElapsedTimeReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  Fraction: Single;
begin
  if FdEnabled.Value then
  begin
    Fraction := TimeDependentNodeHandler.ElapsedTimeInCycle / CycleInterval;
    if (Fraction = 0) and (Time.Seconds > FdStartTime.Value) then Fraction := 1;
    Eventfraction_changed.Send(Fraction, Time);

    EventTime.Send(Time.Seconds, Time);
  end;
end;

procedure RegisterTimeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeTimeSensor
  ]);
end;

{$endif read_implementation}
