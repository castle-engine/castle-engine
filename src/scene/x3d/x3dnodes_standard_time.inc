{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TTimeFunction = function: TFloatTime of object;

  { Functionality of all time-dependent nodes. }
  TTimeDependentFunctionality = class(TNodeFunctionality)
  strict private
    FIsActive: boolean;
    FIsPaused: boolean;
    FElapsedTime: TFloatTime;
    FElapsedTimeInCycle: TFloatTime;
    StartTimeUsed, NonLoopedStopped: Double; //< same type as TSFTime.Value
    procedure SetIsActive(const Value: boolean);
    procedure SetIsPaused(const Value: boolean);
    procedure SetElapsedTime(const Value: TFloatTime);
  private
    { These should be set by node classes instantiating TTimeDependentFunctionality.
      TODO: Should be solved cleaner, by abstract methods and then
      TTimeDependentFunctionality descendants. }

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

    OnCycleInterval: TTimeFunction;
  public
    { Propagate this to all send events, to make animation fade-in/out. }
    PartialSend: TPartialSend;

    { Final duration (in seconds) of this animation.
      For a looping animation, this is the time it takes to make a single cycle.
      Should always be > 0.

      The final length, returned here, is

      @unorderedList(
        @item(In case of TAudioClipNode: sound duration divided by @link(TAbstractSoundSourceNode.Pitch).)

        @item(In case of TMovieTextureNode: movie duration divided by @link(TMovieTextureNode.Speed).)

        @item(In case of TTimeSensorNode: just @link(TTimeSensorNode.CycleInterval).)
      ) }
    function CycleInterval: TFloatTime;

    { Is the sensor enabled and running (ignoring whether it is paused).

      For a precise specification of how this behaves,
      see the X3D TimeSensor.isActive specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor

      Changing this automatically causes appropriate events to be generated. }
    property IsActive: boolean read FIsActive write SetIsActive;

    { Is the sensor paused.

      For a precise specification of how this behaves,
      see the X3D TimeSensor.isPaused specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor

      Changing this automatically causes appropriate events to be generated. }
    property IsPaused: boolean read FIsPaused write SetIsPaused;

    { Time in seconds since the sensor was activated and running,
      not counting any time while in pause state.

      For non-looping animations, note that the sensor automatically stops
      when the animation finishes, so ElapsedTime will not grow beyond
      the @link(CycleInterval). If you want to observe when the animation ended,
      you probably do not want to use this property. Instead
      observe when @link(IsActive) changed to @false, by registering
      a callback on @code(EventIsActive.AddNotification).
      See the CGE examples/viewport_and_scenes/listen_on_x3d_events/ .
      Or, even better (simpler and more reliable) register a stop
      notification using @link(TPlayAnimationParameters.StopNotification) with
      @link(TCastleSceneCore.PlayAnimation).

      For a precise specification of how this behaves,
      see the X3D TimeSensor.elapsedTime specification at
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/time.html#TimeSensor

      Changing this automatically causes appropriate events to be generated. }
    property ElapsedTime: TFloatTime read FElapsedTime write SetElapsedTime;

    { Time in seconds since the sensor was activated and running,
      in this cycle,
      not counting any time while in pause state.

      This is like @link(ElapsedTime), but counting only the current cycle.
      When @link(CycleInterval) = 0, this is always 0.
      When @link(CycleInterval) <> 0, this is always >= 0 and < CycleInterval . }
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

  { Abstract node from which most (but not all) time-dependent nodes are derived. }
  TAbstractTimeDependentNode = class(TAbstractChildNode)
  private
    FTimeFunctionality: TTimeDependentFunctionality;
    { X3D rules state that sometimes input of StartTime, StopTime, CycleInterval
      should be ignored when animation is already running.
      As it is sometimes more bothersome than helpful, you can temporarily
      disable this mechanism (that is, @italic(never ignore)) by increasing this field.
      @exclude }
    InternalNeverIgnore: Cardinal;
    function IgnoreWhenActive(const ChangingField: TX3DField; const Event: TX3DEvent;
      const Value: TX3DField; const Time: TX3DTime): Boolean;
    function IgnoreStopTime(const ChangingField: TX3DField; const Event: TX3DEvent;
      const Value: TX3DField; const Time: TX3DTime): Boolean;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

    { Is the sensor enabled and running (ignoring whether it is paused).

      See @link(TTimeDependentFunctionality.IsActive). }
    function IsActive: Boolean;

    { Is the sensor paused.

      See @link(TTimeDependentFunctionality.IsPaused). }
    function IsPaused: Boolean;

    { Time in seconds since the sensor was activated and running,
      not counting any time while in pause state.

      See @link(TTimeDependentFunctionality.ElapsedTime). }
    function ElapsedTime: TFloatTime;

    { Time in seconds since the sensor was activated and running,
      in this cycle,
      not counting any time while in pause state.

      See @link(TTimeDependentFunctionality.ElapsedTimeInCycle). }
    function ElapsedTimeInCycle: TFloatTime;

  {$I auto_generated_node_helpers/x3dnodes_x3dtimedependentnode.inc}
  end;

  { Generate events as time passes. }
  TTimeSensorNode = class(TAbstractTimeDependentNode)
  strict private
    FakingTime: Integer;
    procedure EventElapsedTimeReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    { CycleInterval that is always > 0. }
    function SafeCycleInterval: TFloatTime;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

    { Send TimeSensor output events, without actually activating the TimeSensor.

      This is useful in situations when you want the X3D scene state to reflect
      given time, but you do not want to activate sensor and generally you do
      not want to initialize anything that would continue animating on it's own.

      Note: We ignore TimeSensor.loop (FdLoop) field,
      instead we follow our own ALoop parameter.
      We also ignore TimeSensor.fractionIncreasing field,
      instead we follow our own AFractionIncreasing parameter.
      We also ignore TimeSensor.enabled field,
      assuming that you always want to behave like it's enabled
      (in particular, this makes @link(TCastleSceneCore.ForceAnimationPose)
      always working, even if @link(TCastleSceneCore.PlayAnimation)
      set enabled = false on previously-stopped animation).

      We take into account TimeSensor.cycleInterval (FdCycleInterval),
      just like during normal TimeSensor behavior.
      We send out isActive:=true, fraction_changed, elapsedTime and time X3D
      output events, and they should drive the rest of animation.

      @italic(About the TimeOfEvents parameter):

      In X3D, if you send a message (which is what happens when various X3D nodes communicate,
      e.g. TimeSensors send messages to interpolators) the message will be ignored
      if it already passed through the same route (@link(TX3DRoute))
      with the timestamp >= than current.
      This is a simple mechanism to avoid route loops in X3D.

      It means you need to use increasing time each time you initiate an X3D event.

      The implementation of FakeTime without @code(TimeOfEvents) parameter
      solves it by using an internal, always increasing, time value.
      It will work, but only if you don't process events in your X3D graphs
      by any other means than this FakeTime time call.

      In general case, when using FakeTime together with @link(TCastleScene)
      that has active events (@link(TCastleSceneCore.ProcessEvents) = @true),
      you should call overloaded TTimeSensorNode.FakeTime with @code(TimeOfEvents) parameter.
      Use @link(TCastleSceneCore.NextEventTime MyScene.NextEventTime)
      as the value of @code(TimeOfEvents). NextEventTime internally returns current time,
      and also increases it by some special epsilon used only for comparison.

      @groupBegin }
    procedure FakeTime(const TimeInAnimation: TFloatTime;
      const ALoop, AFractionIncreasing: boolean;
      const PartialSend: TPartialSend = nil); overload;
    procedure FakeTime(const TimeInAnimation: TFloatTime;
      const ALoop, AFractionIncreasing: boolean;
      const TimeOfEvents: TX3DTime;
      const PartialSend: TPartialSend = nil); overload;
    { @groupEnd }

    { Start or stop the time sensor @italic(right now),
      by setting the fields of this time sensor.

      Usually you should prefer using the @link(TCastleSceneCore.PlayAnimation)
      or @link(TCastleSceneCore.StopAnimation) instead of these methods.
      The scene methods allow to play the animation with lots of features,
      and are more comfortable to use (e.g. you do not need to find
      the TTimeSensorNode reference).

      Using these methods directly is useful to play multiple animations
      simultaneously.

      @groupBegin }
    procedure Start(const ALoop: Boolean;
      const AFractionIncreasing: Boolean = true;
      const InitialTime: TFloatTime = 0);
    procedure Stop;
    { @groupEnd }

  {$I auto_generated_node_helpers/x3dnodes_timesensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TTimeDependentFunctionality -------------------------------------------------- }

procedure TTimeDependentFunctionality.SetIsActive(const Value: boolean);
begin
  if Value <> FIsActive then
  begin
    FIsActive := Value;
    EventIsActive.Send(Value, Parent.Scene.NextEventTime, PartialSend);
  end;
end;

procedure TTimeDependentFunctionality.SetIsPaused(const Value: boolean);
begin
  if Value <> FIsPaused then
  begin
    FIsPaused := Value;
    EventIsPaused.Send(Value, Parent.Scene.NextEventTime, PartialSend);
  end;
end;

procedure TTimeDependentFunctionality.SetElapsedTime(const Value: TFloatTime);
begin
  if Value <> FElapsedTime then
  begin
    FElapsedTime := Value;

    { NextEventTime below will automatically increase time tick
      of current time, and that's a good thing.

      Otherwise there was a bug when TCastleSceneCore.ResetTime
      (caused by ResetTimeAtLoad) was calling all time handlers
      and in effect changing elapsedTime on all TimeSensors to 0
      (see TTimeDependentFunctionality.SetTime implementation). This was
      (and still is) causing elapsedTime and fraction_changed outputs
      generated even for inactive TimeSensors.

      So it must avoid routes loop warnings by increasing PlusTicks
      for next elapsedTime send
      (reproduction: escape_universe game restart.) }

    EventElapsedTime.Send(Value, Parent.Scene.NextEventTime, PartialSend);
  end;
end;

function TTimeDependentFunctionality.CycleInterval: TFloatTime;
begin
  Assert(Assigned(OnCycleInterval));
  Result := OnCycleInterval();
end;

function TTimeDependentFunctionality.SetTime(
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
    (like after TTimeDependentFunctionality creation) at the beginning. }
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
           OnUpdate (like when TCastleView starts), and the next OnUpdate may get large
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
      EventCycleTime.Send(CycleTime, Parent.Scene.NextEventTime, PartialSend);
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
  WritelnLog(Format('Time-dependent node %s', [Node.NiceName]), Format('SetTime finished. Active %s, paused %s, elapsedTimeInCycle %f',
    [ BoolToStr(IsActive, true),
      BoolToStr(IsPaused, true),
      FElapsedTimeInCycle
    ]));
  {$endif}
end;

{ TAbstractTimeDependentNode ------------------------------------------------------------- }

constructor TAbstractTimeDependentNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;

  FTimeFunctionality := TTimeDependentFunctionality.Create(Self);
  FTimeFunctionality.FdLoop := FdLoop;
  FTimeFunctionality.FdPauseTime := FdPauseTime;
  FTimeFunctionality.FdResumeTime := FdResumeTime;
  FTimeFunctionality.FdStartTime := FdStartTime;
  FTimeFunctionality.FdStopTime := FdStopTime;
  { descendants should set FTimeFunctionality.FdEnabled }
  FTimeFunctionality.EventIsActive := EventIsActive;
  FTimeFunctionality.EventIsPaused := EventIsPaused;
  FTimeFunctionality.EventElapsedTime := EventElapsedTime;
  { descendants should set FTimeFunctionality.EventCycleTime }

  AddFunctionality(FTimeFunctionality);

  FdStartTime.OnInputIgnore := {$ifdef FPC}@{$endif} IgnoreWhenActive;
  FdStopTime.OnInputIgnore := {$ifdef FPC}@{$endif} IgnoreStopTime;
end;

function TAbstractTimeDependentNode.IgnoreWhenActive(
  const ChangingField: TX3DField; const Event: TX3DEvent;
  const Value: TX3DField; const Time: TX3DTime): Boolean;
begin
  Result := (InternalNeverIgnore = 0) and FTimeFunctionality.IsActive;
end;

function TAbstractTimeDependentNode.IgnoreStopTime(
  const ChangingField: TX3DField; const Event: TX3DEvent;
  const Value: TX3DField; const Time: TX3DTime): Boolean;
begin
  Result := (InternalNeverIgnore = 0) and
    FTimeFunctionality.IsActive and
    ( (Value as TSFTime).Value <= FdStartTime.Value );
end;

function TAbstractTimeDependentNode.IsActive: Boolean;
begin
  Result := FTimeFunctionality.IsActive;
end;

function TAbstractTimeDependentNode.IsPaused: Boolean;
begin
  Result := FTimeFunctionality.IsPaused;
end;

function TAbstractTimeDependentNode.ElapsedTime: TFloatTime;
begin
  Result := FTimeFunctionality.ElapsedTime;
end;

function TAbstractTimeDependentNode.ElapsedTimeInCycle: TFloatTime;
begin
  Result := FTimeFunctionality.ElapsedTimeInCycle;
end;

{ TTimeSensorNode ------------------------------------------------------------ }

constructor TTimeSensorNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;

  { set FTimeFunctionality properties }
  FTimeFunctionality.FdEnabled := FdEnabled;
  FTimeFunctionality.EventCycleTime := EventCycleTime;
  FTimeFunctionality.OnCycleInterval := {$ifdef FPC}@{$endif} GetCycleInterval;

  { On receiving new elapsedTime, we send other continuous events. }
  EventElapsedTime.AddNotification({$ifdef FPC}@{$endif} EventElapsedTimeReceive);

  FdCycleInterval.OnInputIgnore := {$ifdef FPC}@{$endif} IgnoreWhenActive;

  { cycleTime_changed name is used e.g. by
    www.web3d.org/x3d/content/examples/Basic/StudentProjects/WallClock.x3d }
  FEventCycleTime.AddAlternativeName('cycleTime_changed', 0);
end;

function TTimeSensorNode.SafeCycleInterval: TFloatTime;
begin
  Result := CycleInterval;
  if Result <= 0 then
    Result := 1;
end;

procedure TTimeSensorNode.EventElapsedTimeReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  Fraction: Single;
begin
  if (FakingTime = 0) and FdEnabled.Value then
  begin
    Fraction := FTimeFunctionality.ElapsedTimeInCycle / SafeCycleInterval;
    if (Fraction = 0) and (Time.Seconds > FdStartTime.Value) then Fraction := 1;
    if not FdFractionIncreasing.Value then
      Fraction := 1 - Fraction;
    Eventfraction_changed.Send(Fraction, Time, Value.InternalPartialSend);

    EventTime.Send(Time.Seconds, Time, Value.InternalPartialSend);
  end;
end;

var
  FakeX3DTime: TX3DTime;

procedure TTimeSensorNode.FakeTime(const TimeInAnimation: TFloatTime;
  const ALoop, AFractionIncreasing: boolean;
  const PartialSend: TPartialSend);
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
    FakeX3DTime.Seconds := FakeX3DTime.Seconds + 1;
    FakeX3DTime.PlusTicks := 0;
  end;

  FakeTime(TimeInAnimation, ALoop, AFractionIncreasing, FakeX3DTime, PartialSend);
end;

procedure TTimeSensorNode.FakeTime(const TimeInAnimation: TFloatTime;
  const ALoop, AFractionIncreasing: boolean;
  const TimeOfEvents: TX3DTime;
  const PartialSend: TPartialSend);
var
  Fraction: Single;
begin
  Inc(FakingTime);
  EventIsActive.Send(true, TimeOfEvents, PartialSend);

  if ALoop then
    Fraction := Frac(TimeInAnimation / SafeCycleInterval)
  else
    Fraction := Clamped(TimeInAnimation / SafeCycleInterval, 0, 1);
  if not AFractionIncreasing then
    Fraction := 1 - Fraction;
  EventFraction_changed.Send(Fraction, TimeOfEvents, PartialSend);

  EventElapsedTime.Send(TimeInAnimation, TimeOfEvents, PartialSend);
  EventTime.Send(TimeInAnimation, TimeOfEvents, PartialSend);
  Dec(FakingTime);
end;

procedure TTimeSensorNode.Stop;
begin
  { We want to stop time sensor from sending any further
    elapsedTime events (elapsedTime causes also fraction_changed).
    Sending of further elapsedTime means that two animations try to play
    at the same time, and modify the same transforms.

    One way to stop animation is to set

      PlayingAnimationNode.StopTime := Time;

    While it works in practice (no reproducible bug with it found),
    it has 2 problems:

    - There will be 1 additional frame when TimeSensor "finishes of"
      the animation, by remaining "TimeIncrease - (NewTime - FdStopTime.Value)"
      duration. This isn't a problem in practice, since it only happens for 1 frame
      (then the old TimeSensor becomes inactive), but it stil feels dirty/wasteful.

    - In case you will move time backwards, e.g. by ResetTime or ResetTimeAtLoad,
      you may get into trouble.
      You may accidentally activate a time sensor that is
      inactive now, but was active long time ago.

    To overcome the 2nd problem, we tried the trick below.
    It will work assuming you never reset time to something < 1.
    (If you do reset time to something very small, then typical TimeSensors will
    have problems anyway,
    see https://castle-engine.io/x3d_time_origin_considered_uncomfortable.php ).

      PlayingAnimationNode.StartTime := 0;
      PlayingAnimationNode.StopTime := 1;
      PlayingAnimationNode.Loop := false;

    But this:
    - Still has the "1 additional frame when TimeSensor finishes of" dirtyness.
    - Also, setting startTime while time-dependent node is active is ignored,
      X3D spec requires this, see our TSFTimeIgnoreWhenActive implementation.
      (bug reproduction: escape_universe, meteorite_1 dying).
      Although we remove this problem by InternalNeverIgnore hack.
    - (Obsolete argument:) Also, it's bad to unconditionally set "Loop" value.
      If user is using paDefault for animation, (s)he expects
      that PlayAnimation doesn't change it ever.

    So it's simpest and reliable to just set enabled=false on old TimeSensor.
    TTimeDependentFunctionality.SetTime then guarantees it will immediately stop
    sending elapsedTime events. }

  Enabled := false;
end;

procedure TTimeSensorNode.Start(const ALoop, AFractionIncreasing: Boolean;
  const InitialTime: TFloatTime);
begin
  Check(Scene <> nil, 'TTimeSensorNode.Start can only be called when node is part of scene');

  Loop := ALoop;
  FractionIncreasing := AFractionIncreasing;
  Enabled := true; // since Stop changes Enabled to false

  { Disable the "ignore" mechanism, otherwise
    setting startTime on a running TimeSensor would be ignored.
    Testcase: e.g. mana animation on dark_dragon and dragon_squash. }
  Inc(InternalNeverIgnore);

  { Assign StopTime and StartTime. }
  StopTime := 0;
  StartTime := Scene.Time - InitialTime;

  { Enable the "ignore" mechanism again, to follow X3D spec. }
  Dec(InternalNeverIgnore);
end;

procedure RegisterTimeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TTimeSensorNode
  ]);
end;

{$endif read_implementation}
