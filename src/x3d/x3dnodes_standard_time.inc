{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TInternalTimeDependentHandler = class;

  { Interface from which all time-dependent nodes are derived. }
  IAbstractTimeDependentNode = interface(IAbstractChildNode)
  ['{19D68914-F2BA-4CFA-90A1-F561DB264678}']

    function GetInternalTimeDependentHandler: TInternalTimeDependentHandler;
    property InternalTimeDependentHandler: TInternalTimeDependentHandler
      read GetInternalTimeDependentHandler;

    { Final length (in seconds) of this animation.
      For a looping animation, this is the time it takes to make a single cycle.
      Should always be > 0.

      This is not called @italic("duration"), as that word is reserved to mean
      @italic("unscaled time length") for X3D AudioClip and MovieTexture nodes.
      The final length, returned here, is the AudioClip.duration divided
      by AudioClip.pitch, or MovieTexture.duration / MovieTexture.speed.

      On TTimeSensorNode descendants, you can also get and set this
      by the property @link(TTimeSensorNode.CycleInterval). }
    function GetCycleInterval: TFloatTime;

    { Is the sensor enabled and running (ignoring whether it is paused).

      For a precise (and complicated:) specification of how this behaves,
      see the X3D TimeSensor.isActive specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor }
    function IsActive: boolean;

    { Is the sensor paused.

      For a precise (and complicated:) specification of how this behaves,
      see the X3D TimeSensor.isPaused specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor }
    function IsPaused: boolean;

    { Time in seconds since the sensor was activated and running,
      not counting any time while in pause state.

      For non-looping animations, note that the sensor automatically stops
      when the animation finishes, so ElapsedTime will not grow beyond
      the "duration". If you want to observe when the animation ended,
      you probably do not want to use this property. Instead
      observe when @link(IsActive) changed to @false, by registering
      a callback on @code(EventIsActive.OnReceive).
      See the examples/3d_rendering_processing/listen_on_x3d_events.lpr .

      BTW, the @italic("duration") mentioned above is usually just
      @link(GetCycleInterval), but sometimes it's a @link(GetCycleInterval)
      multiplied by some scaling factor, like in case of @link(TAudioClipNode)
      or @link(TMovieTextureNode)).

      For a precise (and complicated:) specification of how this behaves,
      see the X3D TimeSensor.elapsedTime specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor }
    function ElapsedTime: TFloatTime;

    { Time in seconds since the sensor was activated and running,
      in this cycle,
      not counting any time while in pause state.

      This is like ike @link(ElapsedTime), but counting only the current cycle.
      When @link(GetCycleInterval) = 0, this is always 0.
      When @link(GetCycleInterval) <> 0, this is always >= 0 and < CycleInterval . }
    function ElapsedTimeInCycle: TFloatTime;
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
  TInternalTimeDependentHandler = class
  strict private
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

    OnCycleInterval: TTimeFunction;

    FdLoop: TSFBool;
    FdPauseTime: TSFTime;
    FdResumeTime: TSFTime;
    FdStartTime: TSFTime;
    FdStopTime: TSFTime;
    { May be @nil if node doesn't have an "enabled" field. }
    FdEnabled: TSFBool;

    EventElapsedTime: TSFTimeEvent;
    EventIsActive: TSFBoolEvent;
    EventIsPaused: TSFBoolEvent;
    { May be @nil if node doesn't have a "cycleTime" event. }
    EventCycleTime: TSFTimeEvent;

    { Cycle interval for this time-dependent node. }
    function CycleInterval: TFloatTime;

    { Is the sensor enabled and running (ignoring whether it is paused).
      Changing this automatically causes appropriate events to be generated. }
    property IsActive: boolean read FIsActive write SetIsActive;

    { Is the sensor paused.
      Changing this automatically causes appropriate events to be generated. }
    property IsPaused: boolean read FIsPaused write SetIsPaused;

    { Time in seconds since the sensor was activated and running,
      not counting any time while in pause state.
      Changing this automatically causes appropriate events to be generated. }
    property ElapsedTime: TFloatTime read FElapsedTime write SetElapsedTime;

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
      see HandleChangeTimeStopStart in TCastleSceneCore.InternalChangedField implementation.)

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

  { Abstract node from which most (but not all) time-dependent nodes are derived. }
  TAbstractTimeDependentNode = class(TAbstractChildNode, IAbstractTimeDependentNode)
  private
    FInternalTimeDependentHandler: TInternalTimeDependentHandler;
    { To satify IAbstractTimeDependentNode }
    function GetInternalTimeDependentHandler: TInternalTimeDependentHandler;
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

    { Internal time-dependent logic handler. }
    property InternalTimeDependentHandler: TInternalTimeDependentHandler
      read FInternalTimeDependentHandler;

    { Final length (in seconds) of this animation.
      For a looping animation, this is the time it takes to make a single cycle.
      Should always be > 0.

      This is not called @italic("duration"), as that word is reserved to mean
      @italic("unscaled time length") for X3D AudioClip and MovieTexture nodes.
      The final length, returned here, is the AudioClip.duration divided
      by AudioClip.pitch, or MovieTexture.duration / MovieTexture.speed.

      On TTimeSensorNode descendants, you can also get and set this
      by the class helper property @link(TTimeSensorNode.CycleInterval).
      The name collision is harmless here, as both ways get the same
      value. }
    function GetCycleInterval: TFloatTime; virtual; abstract;

    { Is the sensor enabled and running (ignoring whether it is paused).

      For a precise (and complicated:) specification of how this behaves,
      see the X3D TimeSensor.isActive specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor }
    function IsActive: boolean;

    { Is the sensor paused.

      For a precise (and complicated:) specification of how this behaves,
      see the X3D TimeSensor.isPaused specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor }
    function IsPaused: boolean;

    { Time in seconds since the sensor was activated and running,
      not counting any time while in pause state.

      For non-looping animations, note that the sensor automatically stops
      when the animation finishes, so ElapsedTime will not grow beyond
      the "duration". If you want to observe when the animation ended,
      you probably do not want to use this property. Instead
      observe when @link(IsActive) changed to @false, by registering
      a callback on @code(EventIsActive.OnReceive).
      See the examples/3d_rendering_processing/listen_on_x3d_events.lpr .

      BTW, the @italic("duration") mentioned above is usually just
      @link(GetCycleInterval), but sometimes it's a @link(GetCycleInterval)
      multiplied by some scaling factor, like in case of @link(TAudioClipNode)
      or @link(TMovieTextureNode)).

      For a precise (and complicated:) specification of how this behaves,
      see the X3D TimeSensor.elapsedTime specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor }
    function ElapsedTime: TFloatTime;

    { Time in seconds since the sensor was activated and running,
      in this cycle,
      not counting any time while in pause state.

      This is like ike @link(ElapsedTime), but counting only the current cycle.
      When @link(GetCycleInterval) = 0, this is always 0.
      When @link(GetCycleInterval) <> 0, this is always >= 0 and < CycleInterval . }
    function ElapsedTimeInCycle: TFloatTime;

    {$I auto_generated_node_helpers/x3dnodes_x3dtimedependentnode.inc}
  end;

  { Generate events as time passes. }
  TTimeSensorNode = class(TAbstractTimeDependentNode, IAbstractSensorNode)
  private
    FakingTime: Integer;
    procedure EventElapsedTimeReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    procedure SetCycleInterval(const Value: TFloatTime);
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCycleInterval: TSFTimeIgnoreWhenActive;
    public property FdCycleInterval: TSFTimeIgnoreWhenActive read FFdCycleInterval;
    function GetCycleInterval: TFloatTime; override;
    property CycleInterval: TFloatTime read GetCycleInterval write SetCycleInterval;

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
      We send out isActive:=true, fraction_changed, elapsedTime and time X3D output events,
      and they should drive the rest of animation.

      @groupBegin }
    procedure FakeTime(const TimeInAnimation: TFloatTime; const ALoop: boolean);
    procedure FakeTime(const TimeInAnimation: TFloatTime; const ALoop: boolean;
      const TimeOfEvents: TX3DTime);
    { @groupEnd }

    {$I auto_generated_node_helpers/x3dnodes_timesensor.inc}
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
      (ParentNode as IAbstractTimeDependentNode).InternalTimeDependentHandler.IsActive;
  end;

  procedure LogIgnored;
  var
    SceneName: string;
  begin
    if TX3DNode(ParentNode).Scene <> nil then
      SceneName := TX3DNode(ParentNode).Scene.Name else
      SceneName := '<scene-unknown>';
    WritelnLog('Change Ignored', 'A change of "' + TX3DNode(ParentNode).NiceName + '.' + X3DName + '" field (scene "' + SceneName + '") while the time-dependent node is active is ignored (the value remains unchanged), following the X3D specification.');
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
    H: TInternalTimeDependentHandler;
  begin
    Result :=
      (NeverIgnore = 0) and
      (ParentNode <> nil) and
      Supports(ParentNode, IAbstractTimeDependentNode);
    if Result then
    begin
      H := (ParentNode as IAbstractTimeDependentNode).InternalTimeDependentHandler;
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
    WritelnLog('Change Ignored', 'A change of "' + TX3DNode(ParentNode).NiceName + '.' + X3DName + '" field (scene "' + SceneName + '"), to the value <= startTime, while the time-dependent node is active is ignored (the value remains unchanged), following the X3D specification.');
  end;

begin
  if Ignore then
  begin
    LogIgnored;
    Exit;
  end;
  inherited;
end;

{ TInternalTimeDependentHandler -------------------------------------------------- }

procedure TInternalTimeDependentHandler.SetIsActive(const Value: boolean);
begin
  if Value <> FIsActive then
  begin
    FIsActive := Value;
    EventIsActive.Send(Value, Node.Scene.NextEventTime);
  end;
end;

procedure TInternalTimeDependentHandler.SetIsPaused(const Value: boolean);
begin
  if Value <> FIsPaused then
  begin
    FIsPaused := Value;
    EventIsPaused.Send(Value, Node.Scene.NextEventTime);
  end;
end;

procedure TInternalTimeDependentHandler.SetElapsedTime(const Value: TFloatTime);
begin
  if Value <> FElapsedTime then
  begin
    FElapsedTime := Value;

    { NextEventTime below will automatically increase time tick
      of current time, and that's a good thing.

      Otherwise there was a bug when TCastleSceneCore.ResetTime
      (caused by ResetTimeAtLoad) was calling all time handlers
      and in effect changing elapsedTime on all TimeSensors to 0
      (see TInternalTimeDependentHandler.SetTime implementation). This was
      (and still is) causing elapsedTime and fraction_changed outputs
      generated even for inactive TimeSensors.

      So it must avoid routes loop warnings by increasing PlusTicks
      for next elapsedTime send
      (reproduction: escape_universe game restart.) }

    EventElapsedTime.Send(Value, Node.Scene.NextEventTime);
  end;
end;

function TInternalTimeDependentHandler.CycleInterval: TFloatTime;
begin
  Assert(Assigned(OnCycleInterval));
  Result := OnCycleInterval();
end;

function TInternalTimeDependentHandler.SetTime(
  const NewTime: TFloatTime;
  const TimeIncrease: TFloatTime; const ResetTime: boolean): boolean;

var
  NewIsActive: boolean;
  NewIsPaused: boolean;
  NewElapsedTime: TFloatTime;
  CycleTimeSend: boolean;
  CycleTime: TFloatTime;

  { $define LOG_TIME_DEPENDENT_NODES_OFTEN}
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

          {$ifdef LOG_TIME_DEPENDENT_NODES}
          if Log then
            WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), 'Next cycle (of a looping sensor)');
          {$endif}
        end;
      end else
      begin
        { for cycleInterval = 0 this always remains 0 }
        FElapsedTimeInCycle := 0;
      end;

      if (not FdLoop.Value) and StopOnNonLoopedEnd then
      begin
        {$ifdef LOG_TIME_DEPENDENT_NODES}
        if Log then
          WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), 'Stopping (because end of cycle of non-looping sensor)');
        {$endif}

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

  {$ifdef LOG_TIME_DEPENDENT_NODES_OFTEN}
  if Log then
    WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), Format('Time changes by +%f to %f. Cycle interval: %f, loop %s. Before: active %s, paused %s, elapsedTimeInCycle %f',
      [ TimeIncrease, NewTime,
        CycleInterval,
        BoolToStr(FdLoop.Value, true),
        BoolToStr(IsActive, true),
        BoolToStr(IsPaused, true),
        FElapsedTimeInCycle
      ]));
  {$endif}

  Result := false;

  if (FdEnabled <> nil) and (not FdEnabled.Value) then
  begin
    {$ifdef LOG_TIME_DEPENDENT_NODES}
    if IsActive and Log then
      WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), 'Stopping (because disabled)',
        [NewTime, FdStartTime.Value, CycleInterval]);
    {$endif}

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
    (like after TInternalTimeDependentHandler creation) at the beginning. }
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
        WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), 'Starting. Time now %f, start time %f, cycle interval %f',
          [NewTime, FdStartTime.Value, CycleInterval]);
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
      {$ifdef LOG_TIME_DEPENDENT_NODES}
      if Log then
        WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), 'Resuming');
      {$endif}

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

      {$ifdef LOG_TIME_DEPENDENT_NODES}
      if Log then
        WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), 'Stopped (because stopTime reached)');
      {$endif}
    end else
    if (NewTime >= FdPauseTime.Value) and
       (FdPauseTime.Value > FdResumeTime.Value) then
    begin
      NewIsPaused := true;
      { advance only to the pauseTime }
      if TimeIncrease <> 0 then
        IncreaseElapsedTime(TimeIncrease - (NewTime - FdPauseTime.Value), false);

      {$ifdef LOG_TIME_DEPENDENT_NODES}
      if Log then
        WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), 'Paused');
      {$endif}
    end else
    begin
      { active and not paused time-dependent node }
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

  {$ifdef LOG_TIME_DEPENDENT_NODES_OFTEN}
  if Log then
    WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), Format('SetTime finished. Active %s, paused %s, elapsedTimeInCycle %f',
      [ BoolToStr(IsActive, true),
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

  FInternalTimeDependentHandler := TInternalTimeDependentHandler.Create;
  FInternalTimeDependentHandler.Node := Self;
  FInternalTimeDependentHandler.FdLoop := FdLoop;
  FInternalTimeDependentHandler.FdPauseTime := FdPauseTime;
  FInternalTimeDependentHandler.FdResumeTime := FdResumeTime;
  FInternalTimeDependentHandler.FdStartTime := FdStartTime;
  FInternalTimeDependentHandler.FdStopTime := FdStopTime;
  { descendants should set FInternalTimeDependentHandler.FdEnabled }
  FInternalTimeDependentHandler.EventIsActive := EventIsActive;
  FInternalTimeDependentHandler.EventIsPaused := EventIsPaused;
  FInternalTimeDependentHandler.EventElapsedTime := EventElapsedTime;
  { descendants should set FInternalTimeDependentHandler.EventCycleTime }
  { descendants should override GetCycleInterval }
  FInternalTimeDependentHandler.OnCycleInterval := @GetCycleInterval;
end;

destructor TAbstractTimeDependentNode.Destroy;
begin
  FreeAndNil(FInternalTimeDependentHandler);
  inherited;
end;

function TAbstractTimeDependentNode.GetInternalTimeDependentHandler: TInternalTimeDependentHandler;
begin
  Result := FInternalTimeDependentHandler;
end;

function TAbstractTimeDependentNode.SafeCycleInterval: TFloatTime;
begin
  Result := GetCycleInterval;
  if Result <= 0 then
    Result := 1;
end;

function TAbstractTimeDependentNode.IsActive: boolean;
begin
  Result := InternalTimeDependentHandler.IsActive;
end;

function TAbstractTimeDependentNode.IsPaused: boolean;
begin
  Result := InternalTimeDependentHandler.IsPaused;
end;

function TAbstractTimeDependentNode.ElapsedTime: TFloatTime;
begin
  Result := InternalTimeDependentHandler.ElapsedTime;
end;

function TAbstractTimeDependentNode.ElapsedTimeInCycle: TFloatTime;
begin
  Result := InternalTimeDependentHandler.ElapsedTimeInCycle;
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

  { set InternalTimeDependentHandler }
  InternalTimeDependentHandler.FdEnabled := FdEnabled;
  InternalTimeDependentHandler.EventCycleTime := EventCycleTime;

  { On receiving new elapsedTime, we send other continous events. }
  EventElapsedTime.OnReceive.Add(@EventElapsedTimeReceive);
end;

function TTimeSensorNode.GetCycleInterval: TFloatTime;
begin
  Result := FdCycleInterval.Value;
end;

procedure TTimeSensorNode.SetCycleInterval(const Value: TFloatTime);
begin
  FdCycleInterval.Send(Value);
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
    Fraction := InternalTimeDependentHandler.ElapsedTimeInCycle / SafeCycleInterval;
    if (Fraction = 0) and (Time.Seconds > FdStartTime.Value) then Fraction := 1;
    Eventfraction_changed.Send(Fraction, Time);

    EventTime.Send(Time.Seconds, Time);
  end;
end;

var
  FakeX3DTime: TX3DTime;

procedure TTimeSensorNode.FakeTime(const TimeInAnimation: TFloatTime; const ALoop: boolean);
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

  FakeTime(TimeInAnimation, ALoop, FakeX3DTime);
end;

procedure TTimeSensorNode.FakeTime(const TimeInAnimation: TFloatTime; const ALoop: boolean;
  const TimeOfEvents: TX3DTime);
begin
  if FdEnabled.Value then
  begin
    Inc(FakingTime);
    EventIsActive.Send(true, TimeOfEvents);
    if ALoop then
      EventFraction_changed.Send(Frac(TimeInAnimation / SafeCycleInterval), TimeOfEvents)
    else
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
