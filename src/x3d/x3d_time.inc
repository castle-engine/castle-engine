{
  Copyright 2008-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TTimeDependentNodeHandler = class;

  { }
  IAbstractTimeDependentNode = interface(IAbstractChildNode)
  ['{19D68914-F2BA-4CFA-90A1-F561DB264678}']
    function GetTimeDependentNodeHandler: TTimeDependentNodeHandler;
    property TimeDependentNodeHandler: TTimeDependentNodeHandler
      read GetTimeDependentNodeHandler;
  end;

  TTimeFunction = function: TFloatTime of object;

  { Common helper for all X3DTimeDependentNode descendants.
    This includes things descending from interface IAbstractTimeDependentNode,
    in particular (but not only) descending from class
    TAbstractTimeDependentNode.

    It would be cleaner to have Node declared as IAbstractTimeDependentNode,
    and have IAbstractTimeDependentNode contain common fields.
    Then a lot of fields of this class would not be needed, as they
    would be accessible as IAbstractTimeDependentNode fields.
    TODO: maybe in the future. }
  TTimeDependentNodeHandler = class
  private
    FIsActive: boolean;
    FIsPaused: boolean;
    FElapsedTime: TFloatTime;
    FElapsedTimeInCycle: TFloatTime;
    StartTimeUsed, NonLoopedStopped: Double; //< same type as TSFTime.Value
    procedure SetIsActive(const Value: boolean);
    procedure SetIsPaused(const Value: boolean);
    procedure SetElapsedTime(const Value: TFloatTime);
  public
    Node: TX3DNode;

    { Current state of this time-dependent node
      (see X3D specification about "Time" component for exact meaning).
      Changing them automatically causes appropriate events to be generated.
      @groupBegin }
    property IsActive: boolean read FIsActive write SetIsActive;
    property IsPaused: boolean read FIsPaused write SetIsPaused;
    property ElapsedTime: TFloatTime read FElapsedTime write SetElapsedTime;
    { @groupEnd }
  public
    { Cycle interval for this time-dependent node. }
    OnCycleInterval: TTimeFunction;
    function CycleInterval: TFloatTime;
  public
    Fdloop: TSFBool;
    FdpauseTime: TSFTime;
    FdresumeTime: TSFTime;
    FdStartTime: TSFTime;
    FdstopTime: TSFTime;
    { May be @nil if node doesn't have an "enabled" field. }
    Fdenabled: TSFBool;

    EventelapsedTime: TSFTimeEvent;
    EventisActive: TSFBoolEvent;
    EventisPaused: TSFBoolEvent;
    { May be @nil if node doesn't have a "cycleTime" event. }
    EventcycleTime: TSFTimeEvent;

    { Like ElapsedTime, but spent in the current cycle.
      When cycleInterval = 0, this is always 0.
      When cycleInterval <> 0, this is always >= 0 and < cycleInterval . }
    property ElapsedTimeInCycle: TFloatTime read FElapsedTimeInCycle;

    { Call this when world time increases.
      This is the most important method of this class, that basically
      implements time-dependent nodes operations.

      NewTime and TimeIncrease are produced
      by TCastleSceneCore.SetTime and friends.

      When ResetTime = true, this means that "TimeIncrease value is unknown"
      (you @italic(must) pass TimeIncrease = 0 in this case).
      This can happen only when were called by ResetTime.

      In other circumstances, TimeIncrease must be >= 0.
      (It's allowed to pass TimeIncrease = 0 and ResetTime = false, this doesn't
      advance the clock, but is a useful trick to force some update,
      see TCastleSceneCore.ChangedField implementation.)

      References: see X3D specification "Time" component,
      8.2 ("concepts") for logic behind all those start/stop/pause/resumeTime,
      cycleInterval, loop properties.

      @returns(If some state of time-dependent node changed.) }
    function SetTime(const NewTime: TFloatTime;
      const TimeIncrease: TFloatTime; const ResetTime: boolean): boolean;
  end;

  { Time field, that ignores it's input event when parent time-dependent node
    is active. }
  TSFTimeIgnoreWhenActive = class(TSFTime)
  protected
    class function ExposedEventsFieldClass: TX3DFieldClass; override;
    procedure ExposedEventReceive(Event: TX3DEvent; AValue: TX3DField;
      const Time: TX3DTime); override;
  public
    NeverIgnore: Cardinal;
  end;

  { Time field, that ignores it's input event when parent time-dependent node
    is active and setting to value <= startTime. }
  TSFStopTime = class(TSFTime)
  protected
    class function ExposedEventsFieldClass: TX3DFieldClass; override;
    procedure ExposedEventReceive(Event: TX3DEvent; AValue: TX3DField;
      const Time: TX3DTime); override;
  public
    NeverIgnore: Cardinal;
  end;

  TAbstractTimeDependentNode = class(TAbstractChildNode, IAbstractTimeDependentNode)
  private
    FTimeDependentNodeHandler: TTimeDependentNodeHandler;
    { To satify IAbstractTimeDependentNode }
    function GetTimeDependentNodeHandler: TTimeDependentNodeHandler;
  protected
    { CycleInterval that is always > 0. }
    function SafeCycleInterval: TFloatTime;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    private FFdLoop: TSFBool;
    public property FdLoop: TSFBool read FFdLoop;

    private FFdPauseTime: TSFTime;
    public property FdPauseTime: TSFTime read FFdPauseTime;

    private FFdResumeTime: TSFTime;
    public property FdResumeTime: TSFTime read FFdResumeTime;

    private FFdStartTime: TSFTimeIgnoreWhenActive;
    public property FdStartTime: TSFTimeIgnoreWhenActive read FFdStartTime;

    private FFdStopTime: TSFStopTime;
    public property FdStopTime: TSFStopTime read FFdStopTime;

    { Event out } { }
    private FEventElapsedTime: TSFTimeEvent;
    public property EventElapsedTime: TSFTimeEvent read FEventElapsedTime;

    { Event out } { }
    private FEventIsActive: TSFBoolEvent;
    public property EventIsActive: TSFBoolEvent read FEventIsActive;

    { Event out } { }
    private FEventIsPaused: TSFBoolEvent;
    public property EventIsPaused: TSFBoolEvent read FEventIsPaused;

    { The cycle of a time-dependent node (like TTimeSensorNode) lasts for
      this long (in seconds).

      Note that on TTimeSensorNode descendants, you can also get and set this
      by the class helper TTimeSensorNode.CycleInterval.
      This of course gets/sets the same X3D "cycleInterval" value. }
    function CycleInterval: TFloatTime; virtual; abstract;

    property TimeDependentNodeHandler: TTimeDependentNodeHandler
      read FTimeDependentNodeHandler;
  end;

  TTimeSensorNode = class(TAbstractTimeDependentNode, IAbstractSensorNode)
  private
    FakingTime: Integer;
    procedure EventElapsedTimeReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCycleInterval: TSFTimeIgnoreWhenActive;
    public property FdCycleInterval: TSFTimeIgnoreWhenActive read FFdCycleInterval;

    { Event out } { }
    private FEventCycleTime: TSFTimeEvent;
    public property EventCycleTime: TSFTimeEvent read FEventCycleTime;

    { Event out } { }
    private FEventFraction_changed: TSFFloatEvent;
    public property EventFraction_changed: TSFFloatEvent read FEventFraction_changed;

    { Event out } { }
    private FEventTime: TSFTimeEvent;
    public property EventTime: TSFTimeEvent read FEventTime;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    function CycleInterval: TFloatTime; override;

    { Send TimeSensor output events, without actually activating the TimeSensor.

      This is useful in situations when you want the X3D scene state to reflect
      given time, but you do not want to activate sensor and generally you do
      not want to initialize anything that would continue animating on it's own.

      Using TimeSensor this way is contrary to the VRML/X3D specified behavior.
      But it's useful when we have VRML/X3D scene inside T3DResource,
      that is shared by many T3D instances (like creatures and items) that want to
      simultaneusly display different time moments of the same scene within
      a single frame. In other words, this is useful when a single scene is shared
      (if you have a 100 creatures in your game using the same 3D model,
      you don't want to create 100 copies of this 3D model in memory).

      We ignore TimeSensor.loop (FdLoop) field,
      instead we follow our own Loop parameter.
      We take into account TimeSensor.cycleInterval (FdCycleInterval) and
      TimeSensor.enabled (FdEnabled) fields, just like normal TimeSensor.
      We send out fraction_changed, elapsedTime and time X3D output events,
      and they should drive the rest of animation.

      @groupBegin }
    procedure FakeTime(const TimeInAnimation: TFloatTime; const Loop: boolean);
    procedure FakeTime(const TimeInAnimation: TFloatTime; const Loop: boolean;
      const TimeOfEvents: TX3DTime);
    { @groupEnd }
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ Field types for startTime/stopTime ----------------------------------------- }

class function TSFTimeIgnoreWhenActive.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TSFTime;
end;

procedure TSFTimeIgnoreWhenActive.ExposedEventReceive(
  Event: TX3DEvent; AValue: TX3DField; const Time: TX3DTime);

  { Is parent node an active X3DTimeDependentNode.

    Doing this inside local procedure to limit lifetime of IAbstractTimeDependentNode,
    to secure against http://bugs.freepascal.org/view.php?id=10374 }
  function Ignore: boolean;
  begin
    Result :=
      (NeverIgnore = 0) and
      (ParentNode <> nil) and
      Supports(ParentNode, IAbstractTimeDependentNode) and
      (ParentNode as IAbstractTimeDependentNode).TimeDependentNodeHandler.IsActive;
  end;

  procedure LogIgnored;
  var
    SceneName: string;
  begin
    if TX3DNode(ParentNode).Scene <> nil then
      SceneName := TX3DNode(ParentNode).Scene.Name else
      SceneName := '<scene-unknown>';
    WritelnLog('Change Ignored', 'A change of "' + TX3DNode(ParentNode).NiceName + '.' + Name + '" field (scene "' + SceneName + '") while the time-dependent node is active is ignored (the value remains unchanged), following the X3D specification.');
  end;

begin
  if Ignore then
  begin
    LogIgnored;
    Exit;
  end;
  inherited;
end;

class function TSFStopTime.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TSFTime;
end;

procedure TSFStopTime.ExposedEventReceive(Event: TX3DEvent; AValue: TX3DField;
  const Time: TX3DTime);

  { Is parent node an active X3DTimeDependentNode, and AValue <= startTime.

    Doing this inside local procedure to limit lifetime of IAbstractTimeDependentNode,
    to secure against http://bugs.freepascal.org/view.php?id=10374 }
  function Ignore: boolean;
  var
    H: TTimeDependentNodeHandler;
  begin
    Result :=
      (NeverIgnore = 0) and
      (ParentNode <> nil) and
      Supports(ParentNode, IAbstractTimeDependentNode);
    if Result then
    begin
      H := (ParentNode as IAbstractTimeDependentNode).TimeDependentNodeHandler;
      Result := H.IsActive and
        ( (AValue as TSFTime).Value <= H.FdStartTime.Value );
    end;
  end;

  procedure LogIgnored;
  var
    SceneName: string;
  begin
    if TX3DNode(ParentNode).Scene <> nil then
      SceneName := TX3DNode(ParentNode).Scene.Name else
      SceneName := '<scene-unknown>';
    WritelnLog('Change Ignored', 'A change of "' + TX3DNode(ParentNode).NiceName + '.' + Name + '" field (scene "' + SceneName + '"), to the value <= startTime, while the time-dependent node is active is ignored (the value remains unchanged), following the X3D specification.');
  end;

begin
  if Ignore then
  begin
    LogIgnored;
    Exit;
  end;
  inherited;
end;

{ TTimeDependentNodeHandler -------------------------------------------------- }

procedure TTimeDependentNodeHandler.SetIsActive(const Value: boolean);
begin
  if Value <> FIsActive then
  begin
    FIsActive := Value;
    EventIsActive.Send(Value, Node.Scene.NextEventTime);
  end;
end;

procedure TTimeDependentNodeHandler.SetIsPaused(const Value: boolean);
begin
  if Value <> FIsPaused then
  begin
    FIsPaused := Value;
    EventIsPaused.Send(Value, Node.Scene.NextEventTime);
  end;
end;

procedure TTimeDependentNodeHandler.SetElapsedTime(const Value: TFloatTime);
begin
  if Value <> FElapsedTime then
  begin
    FElapsedTime := Value;

    { NextEventTime below will automatically increase time tick
      of current time, and that's a good thing.

      Otherwise there was a bug when TCastleSceneCore.ResetTime
      (caused by ResetTimeAtLoad) was calling all time handlers
      and in effect changing elapsedTime on all TimeSensors to 0
      (see TTimeDependentNodeHandler.SetTime implementation). This was
      (and still is) causing elapsedTime and fraction_changed outputs
      generated even for inactive TimeSensors.

      So it must avoid routes loop warnings by increasing PlusTicks
      for next elapsedTime send
      (reproduction: escape_universe game restart.) }

    EventElapsedTime.Send(Value, Node.Scene.NextEventTime);
  end;
end;

function TTimeDependentNodeHandler.CycleInterval: TFloatTime;
begin
  Assert(Assigned(OnCycleInterval));
  Result := OnCycleInterval();
end;

function TTimeDependentNodeHandler.SetTime(
  const NewTime: TFloatTime;
  const TimeIncrease: TFloatTime; const ResetTime: boolean): boolean;

var
  NewIsActive: boolean;
  NewIsPaused: boolean;
  NewElapsedTime: TFloatTime;
  CycleTimeSend: boolean;
  CycleTime: TFloatTime;

  { $define LOG_TIME_DEPENDENT_NODES}

  { Increase NewElapsedTime and ElapsedTimeInCycle, taking care of
    CycleInterval and looping.
    StopOnNonLoopedEnd says what to do if NewElapsedTime passed
    CycleInterval and not looping.

    May indicate that CycleTime should be send (by setting CycleTimeSend to true
    and CycleTime value) if the *new* cycle started. This means
    that new ElapsedTime reached the non-zero CycleInterval
    and loop = TRUE. }
  procedure IncreaseElapsedTime(Increase: TFloatTime; StopOnNonLoopedEnd: boolean);
  begin
    { In case of time resetting, we can have wild differences
      between stopTime and NewTime, and Increase could get < 0.
      Consider for example trick with startTime := 0, stopTime := 1
      in UpdateNewPlayingAnimation in TCastleSceneCore.InternalSetTime.
      So make sure Increase here is always >= 0. }
    MaxVar(Increase, 0);

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
            a little earlier than NewTime: earlier by ElapsedTimeInCycle. }
          CycleTimeSend := true;
          CycleTime := NewTime - ElapsedTimeInCycle;
        end;
      end else
      begin
        { for cycleInterval = 0 this always remains 0 }
        FElapsedTimeInCycle := 0;
      end;

      if (not FdLoop.Value) and StopOnNonLoopedEnd then
      begin
        NewIsActive := false;
        // save NonLoopedStopped to avoid restarting the sensor,
        // see NonLoopedStopped usage below
        NonLoopedStopped := StartTimeUsed;
      end;
    end;
  end;

begin
  { if ResetTime, then TimeIncrease must be always exactly 0 }
  Assert((not ResetTime) or (TimeIncrease = 0));

  {$ifdef LOG_TIME_DEPENDENT_NODES}
  if Log then
    WritelnLog('TimeDependentNodes', Format('%s: time changes by +%f to %f. Cycle interval: %f, loop %s. Before: active %s, paused %s, elapsedTimeInCycle %f',
      [ Node.NiceName,
        TimeIncrease, NewTime,
        CycleInterval,
        BoolToStr(FdLoop.Value, true),
        BoolToStr(IsActive, true),
        BoolToStr(IsPaused, true),
        FElapsedTimeInCycle
      ]));
  {$endif}

  Result := false;

  if (Fdenabled <> nil) and (not Fdenabled.Value) then
  begin
    IsActive := false;
    Exit;
  end;

  { Note that each set of IsActive, IsPaused, ElapsedTime may generate
    an X3D event. So we should not carelessly set them many times in this method,
    as it would waste time (why send an event that something changes,
    when right after it you send another event that is changes?).

    Solution: below we will operate on local copies of these variables,
    like NewIsActive, NewIsPaused etc.
    Only at the end of this method we will actually set the properties,
    causing events (if their values changed). }

  { For ResetTime, set time-dependent node properties to default
    (like after TTimeDependentNodeHandler creation) at the beginning. }
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
    if (NewTime >= FdStartTime.Value) and
       ( (NewTime < FdStopTime.Value) or
         { stopTime is ignored if it's <= startTime }
         (FdStopTime.Value <= FdStartTime.Value) ) and
       { Avoid restarting the animation if it's non-looping animation
         that should remain stopped.

         One would think that equation "NewTime - FdStartTime.Value > CycleInterval"
         is enough to do it reliably, but it's not:
         - Testcase: Escape with ship_upgrades.json
         - In case the animation is started at the end of some time-consuming
           OnUpdate (like when TUIState starts), and the next OnUpdate may get large
           SecondsPassed (like 0.2 of second). This means that in initial SetTime
           call we get "NewTime - FdStartTime.Value" close to zero,
           but FElapsedTimeInCycle is set to something significant like 0.2.
         - This 0.2 difference will be preserved, since we only increase
           FElapsedTimeInCycle in each SetTime call. So eventually the cycle will
           end once FElapsedTimeInCycle > CycleInterval, but at this point
           "NewTime - FdStartTime.Value" will be around CycleInterval - 0.2,
           and so the check "NewTime - FdStartTime.Value > CycleInterval"
           will fail.
         - To workaround this, we use NonLoopedStopped, to clearly mark
           "this run of TimeSensor was already stopped, do not restart it".
       }
       not ( ( (NewTime - FdStartTime.Value > CycleInterval) or
               ( { does NonLoopedStopped indicate we shouldn't restart }
                 (NonLoopedStopped <> 0) and
                 (NonLoopedStopped = FdStartTime.Value)
               )
             ) and
             (not FdLoop.Value)
           ) then
    begin
      {$ifdef LOG_TIME_DEPENDENT_NODES}
      if Log then
        WritelnLog('TimeDependentNodes', 'Starting sensor. Time now %f, start time %f, cycle interval %f',
          [NewTime,
           FdStartTime.Value,
           CycleInterval]);
      {$endif}

      StartTimeUsed := FdStartTime.Value; // save, to later used for NonLoopedStopped
      NewIsActive := true;
      NewIsPaused := false;
      NewElapsedTime := 0;
      FElapsedTimeInCycle := 0;

      { Do not advance by TimeIncrease (time from last Time),
        advance only by the time passed since startTime. }
      IncreaseElapsedTime(NewTime - FdStartTime.Value, true);

      if not CycleTimeSend then
      begin
        { Then we still have the initial cycleTime event to generate
          (IncreaseElapsedTime didn't do it for us).
          This should be the "time at the beginning of the current cycle".

          Since IncreaseElapsedTime didn't detect a new cycle,
          so NewElapsedTime = NewTime - FdStartTime.Value fits
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
    if (NewTime >= FdResumeTime.Value) and
       (FdResumeTime.Value > FdPauseTime.Value) then
    begin
      NewIsPaused := false;
      { Advance only by the time passed since resumeTime. }
      IncreaseElapsedTime(NewTime - FdResumeTime.Value, true);
      Result := true;
    end;
  end else
  begin
    Result := true;

    if (NewTime >= FdStopTime.Value) and
       { stopTime is ignored if it's <= startTime }
       (FdStopTime.Value > FdStartTime.Value) then
    begin
      NewIsActive := false;
      { advance only to the stopTime }
      if TimeIncrease <> 0 then
        IncreaseElapsedTime(TimeIncrease - (NewTime - FdStopTime.Value), false);
    end else
    if (NewTime >= FdPauseTime.Value) and
       (FdPauseTime.Value > FdResumeTime.Value) then
    begin
      NewIsPaused := true;
      { advance only to the pauseTime }
      if TimeIncrease <> 0 then
        IncreaseElapsedTime(TimeIncrease - (NewTime - FdPauseTime.Value), false);
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
      EventCycleTime.Send(CycleTime, Node.Scene.NextEventTime);
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
    WritelnLog('TimeDependentNodes', Format('%s: after: active %s, paused %s, elapsedTimeInCycle %f',
      [ Node.NiceName,
        BoolToStr(IsActive, true),
        BoolToStr(IsPaused, true),
        FElapsedTimeInCycle
      ]));
  {$endif}
end;

{ VRML/X3D nodes ------------------------------------------------------------- }

procedure TAbstractTimeDependentNode.CreateNode;
begin
  inherited;

  FFdLoop := TSFBool.Create(Self, 'loop', false);
  AddField(FFdLoop);

  FFdPauseTime := TSFTime.Create(Self, 'pauseTime', 0);
   FdPauseTime.ChangesAlways := [chTimeStopStart];
  AddField(FFdPauseTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdResumeTime := TSFTime.Create(Self, 'resumeTime', 0);
   FdResumeTime.ChangesAlways := [chTimeStopStart];
  AddField(FFdResumeTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdStartTime := TSFTimeIgnoreWhenActive.Create(Self, 'startTime', 0);
   FdStartTime.ChangesAlways := [chTimeStopStart];
  AddField(FFdStartTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdStopTime := TSFStopTime.Create(Self, 'stopTime', 0);
   FdStopTime.ChangesAlways := [chTimeStopStart];
  AddField(FFdStopTime);
  { X3D specification comment: (-Inf,Inf) }

  FEventElapsedTime := TSFTimeEvent.Create(Self, 'elapsedTime', false);
  AddEvent(FEventElapsedTime);

  FEventIsActive := TSFBoolEvent.Create(Self, 'isActive', false);
  AddEvent(FEventIsActive);

  FEventIsPaused := TSFBoolEvent.Create(Self, 'isPaused', false);
  AddEvent(FEventIsPaused);

  DefaultContainerField := 'children';

  FTimeDependentNodeHandler := TTimeDependentNodeHandler.Create;
  FTimeDependentNodeHandler.Node := Self;
  FTimeDependentNodeHandler.Fdloop := FdLoop;
  FTimeDependentNodeHandler.FdpauseTime := FdPauseTime;
  FTimeDependentNodeHandler.FdresumeTime := FdResumeTime;
  FTimeDependentNodeHandler.FdStartTime := FdStartTime;
  FTimeDependentNodeHandler.FdstopTime := FdStopTime;
  { descendants shoud set FTimeDependentNodeHandler.Fdenabled }
  FTimeDependentNodeHandler.EventisActive:= EventisActive;
  FTimeDependentNodeHandler.EventisPaused := EventisPaused;
  FTimeDependentNodeHandler.EventelapsedTime := EventelapsedTime;
  { descendants shoud set FTimeDependentNodeHandler.EventCycleTime }
  { descendants shoud override CycleInterval }
  FTimeDependentNodeHandler.OnCycleInterval := @CycleInterval;
end;

destructor TAbstractTimeDependentNode.Destroy;
begin
  FreeAndNil(FTimeDependentNodeHandler);
  inherited;
end;

function TAbstractTimeDependentNode.GetTimeDependentNodeHandler: TTimeDependentNodeHandler;
begin
  Result := FTimeDependentNodeHandler;
end;

function TAbstractTimeDependentNode.SafeCycleInterval: TFloatTime;
begin
  Result := CycleInterval;
  if Result <= 0 then
    Result := 1;
end;

procedure TTimeSensorNode.CreateNode;
begin
  inherited;

  FFdCycleInterval := TSFTimeIgnoreWhenActive.Create(Self, 'cycleInterval', 1);
  AddField(FFdCycleInterval);
  { X3D specification comment: (0,Inf) }

  FEventCycleTime := TSFTimeEvent.Create(Self, 'cycleTime', false);
  AddEvent(FEventCycleTime);
  { cycleTime_changed name is used e.g. by
    www.web3d.org/x3d/content/examples/Basic/StudentProjects/WallClock.x3d }
  FEventCycleTime.AddAlternativeName('cycleTime_changed', 0);

  FEventFraction_changed := TSFFloatEvent.Create(Self, 'fraction_changed', false);
  AddEvent(FEventFraction_changed);

  FEventTime := TSFTimeEvent.Create(Self, 'time', false);
  AddEvent(FEventTime);

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
  AddField(FFdEnabled);

  DefaultContainerField := 'children';

  { set TimeDependentNodeHandler }
  TimeDependentNodeHandler.Fdenabled := FdEnabled;
  TimeDependentNodeHandler.EventCycleTime := EventCycleTime;

  { On receiving new elapsedTime, we send other continous events. }
  EventelapsedTime.OnReceive.Add(@EventElapsedTimeReceive);
end;

function TTimeSensorNode.CycleInterval: TFloatTime;
begin
  Result := FdCycleInterval.Value;
end;

class function TTimeSensorNode.ClassX3DType: string;
begin
  Result := 'TimeSensor';
end;

class function TTimeSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TTimeSensorNode.EventElapsedTimeReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Fraction: Single;
begin
  if (FakingTime = 0) and FdEnabled.Value then
  begin
    Fraction := TimeDependentNodeHandler.ElapsedTimeInCycle / SafeCycleInterval;
    if (Fraction = 0) and (Time.Seconds > FdStartTime.Value) then Fraction := 1;
    Eventfraction_changed.Send(Fraction, Time);

    EventTime.Send(Time.Seconds, Time);
  end;
end;

var
  FakeX3DTime: TX3DTime;

procedure TTimeSensorNode.FakeTime(const TimeInAnimation: TFloatTime; const Loop: boolean);
begin
  { This method may be called with any TimeInAnimation values sequence,
    e.g. decreasing.

    So we have to calculate TimeOfEvents independent from Time,
    and always growing (to avoid the normal VRML/X3D mechanism that prevents
    passing ROUTEs with an older timestamp (see TX3DRoute.EventReceive).
    We do this by simply using a fake timestamp that is always growing. }
  if FakeX3DTime.PlusTicks <> High(FakeX3DTime.PlusTicks) then
    Inc(FakeX3DTime.PlusTicks) else
  begin
    FakeX3DTime.Seconds += 1;
    FakeX3DTime.PlusTicks := 0;
  end;

  FakeTime(TimeInAnimation, Loop, FakeX3DTime);
end;

procedure TTimeSensorNode.FakeTime(const TimeInAnimation: TFloatTime; const Loop: boolean;
  const TimeOfEvents: TX3DTime);
begin
  if FdEnabled.Value then
  begin
    Inc(FakingTime);
    if Loop then
      EventFraction_changed.Send(Frac(TimeInAnimation / SafeCycleInterval), TimeOfEvents) else
      EventFraction_changed.Send(Clamped(TimeInAnimation / SafeCycleInterval, 0, 1), TimeOfEvents);
    EventElapsedTime.Send(TimeInAnimation, TimeOfEvents);
    EventTime.Send(TimeInAnimation, TimeOfEvents);
    Dec(FakingTime);
  end;
end;

procedure RegisterTimeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TTimeSensorNode
  ]);
end;

{$endif read_implementation}
