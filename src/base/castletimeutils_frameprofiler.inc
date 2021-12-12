{%MainUnit castletimeutils.pas}
{
  Copyright 2019-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleTimeUtils unit: TCastleFrameProfiler. }

{$ifdef read_interface}

type
  { Various possible per-frame times measured by TCastleFrameProfiler. }
  TFrameMetric = (
    { Time in TCastleContainer.EventBeforeRender. }
    fmBeforeRender,

    { Time of entire rendering, including TCastleContainer.EventRender
      (so, all TCastleUserInterface.Render and TCastleTransform.Render calls)
      and including final swap/flush.
      This includes all other fmRenderXxx times. }
    fmRender,
    { Time of swap/flush at the end of rendering, when we flush GPU commands,
      swap buffers (in case of double buffering), and wait for v-sync.
      If this is large, it generally means that you have "free" time in the application
      to use on anything (CPU or GPU work), because you don't use up most of the frame time. }
    fmRenderSwapFlush,
    { Time within TCastleTransform transformation.
      This should not be significant -- otherwise we'll have to optimize CGE TCastleTransform transformations. }
    //fmRenderTransform,
    { Time within TCastleScene.Render.
      Large time means that you have TCastleScene content that is time-consuming to render. }
    fmRenderScene,
    { Time within fmRenderScene spent filtering blended shapes.
      This should not be significant -- otherwise we'll have to optimize this bit in CGE. }
    //fmRenderShapesFilterBlending,

    { Time of TCastleContainer.EventUpdate,
      so most of the things outside of rendering.
      If this is large, it means you do something time-consuming aside from rendering,
      like creature AI or physics.
      This includes all other fmUpdateXxx times. }
    fmUpdate,
    fmUpdateGeneratedTextures,
    fmUpdateScene,
    fmUpdatePhysics
  );

  { Profiler gathering statistics about each game frame.
    Useful to measure where you spend time during the game frame.
    In contrast to @link(TCastleProfiler), this is not flexible to gather
    all possible data, or to keep all historic data.
    It is only specialized to gather some specific data, in each frame,
    to have statistics from the most recent frames.

    You have to enable the statistics gathering by setting @link(Enabled) to @true.
    That's it.
    In the log (see https://castle-engine.io/manual_log.php )
    you will see a frame statistics, displayed after every 60 frames gathered.

    Optionally, you can also report additional
    measurements during the frame.
    Do this by calling @link(Start) and @link(Stop) with String parameter,
    like this:

    @longCode(#
    procedure TMyGameState.Update;
    begin
      FrameProfiler.Start('MyTimeConsumingOperation');
      MyTimeConsumingOperation;
      FrameProfiler.Stop('MyTimeConsumingOperation');
    end;
    #)

    Use this class only through the singleton instance @link(FrameProfiler).

    The easiest way to enable this and view the data is through
    Castle Game Engine inspector. Just press F8 in a debug build to view it.
  }
  TCastleFrameProfiler = class
  private
    const
      MaxRecordFrames = 1000;
      MaxUserMetrics = 16;
      MetricsPerFrame =
        // frame total time
        1
        // time, for each TFrameMetric
        + Ord(High(TFrameMetric)) + 1
        // user metrics, MaxUserMetrics for render, update, other
        + 3 * MaxUserMetrics;
      IndexTotalMetric = 0;
    type
      TMetricIndex = 0..MetricsPerFrame - 1;
      TUserMetric = 0..MaxUserMetrics - 1;
    var
      FEnabled: Boolean;
      FramesTimes: array [0..MaxRecordFrames - 1, TMetricIndex] of TFloatTime;
      CurrentFrameStarted: array [TMetricIndex] of Boolean;
      { Using ProcessTimer would be better for calculations,
        but on Unix (the only platform where it's different from Timer) it has
        too low precision (1/100 of second) to be useful for this. }
      CurrentFrameStart: array [TMetricIndex] of TTimerResult;
      CurrentFrame: Integer;
      // if InitialFrame > CurrentFrame, then it means statistics wraps in FramesTimes array
      InitialFrame: Integer;
      UserMetricsRenderNames: array [TUserMetric] of String;
      UserMetricsUpdateNames: array [TUserMetric] of String;
      UserMetricsOtherNames: array [TUserMetric] of String;
      FFramesForSummary: Cardinal;
      FOnSummaryAvailable: TNotifyEvent;
      FLogSummary: Boolean;
    function IndexFrameMetric(const Metric: TFrameMetric): TMetricIndex;
    function IndexUserMetric(const Name: String; out Metric: TMetricIndex): Boolean;
    { StartCore and StopCore unify all metrics (frame total, TFrameMetric, user metrics)
      as a simple integer index. }
    procedure StartCore(const Metric: TMetricIndex);
    procedure StopCore(const Metric: TMetricIndex);
    { Gather frame information from start.
      This is useful if you're going to keep @link(Enabled) = @false for a longer amount of time,
      and then turn @link(Enabled) back to @true.
      You don't want to sum new frames to the old frames and make statistics from it. }
    procedure Reset;
    procedure SetEnabled(const Value: Boolean);
  public
    const
      DefaultFramesForSummary = 60;

    constructor Create;
    { No data is gathered when Enabled = false, the calls to @link(StartFrame),
      @link(Start), @link(Stop) are ignored then. }
    property Enabled: Boolean read FEnabled write SetEnabled;
    procedure StartFrame;
    procedure Start(const Metric: TFrameMetric); overload;
    procedure Stop(const Metric: TFrameMetric); overload;
    procedure Start(const UserMetric: String); overload;
    procedure Stop(const UserMetric: String); overload;

    { Gather this many frames for a summary. }
    property FramesForSummary: Cardinal read FFramesForSummary write FFramesForSummary
      default DefaultFramesForSummary;

    { Summary of times, use only from @link(OnSummaryAvailable).
      SummaryTotalFrameTime is the total time we spent this frame.
      SummaryTime are times of particular things within this frame.
      @groupBegin }
    function SummaryTotalFrameTime: TFloatTime;
    function SummaryTime(const FrameMetric: TFrameMetric): TFloatTime; overload;
    function SummaryTime(const UserMetric: String): TFloatTime; overload;
    { @groupEnd }

    { Notification when the summary is available, i.e. we have gathered FramesForSummary frames. }
    property OnSummaryAvailable: TNotifyEvent read FOnSummaryAvailable write FOnSummaryAvailable;

    { When the summary is available, output details to log. }
    property LogSummary: Boolean read FLogSummary write FLogSummary default false;
  end;

{ Single instance of @link(TCastleFrameProfiler) that you can use for frame profiling.
  See @link(TCastleFrameProfiler) for more documentation and examples.
  Castle Game Engine uses this to report render/update times automatically. }
function FrameProfiler: TCastleFrameProfiler;

{$endif read_interface}

{$ifdef read_implementation}

{ internal globals ----------------------------------------------------------- }

// not used for now
// function FrameMetricToStr(const Metric: TFrameMetric): String;
// begin
//   WriteStr(Result, Metric);
// end;

{ TCastleFrameProfiler ------------------------------------------------------- }

constructor TCastleFrameProfiler.Create;
begin
  inherited;
  { Since the memory usage may grow quickly once increasing MaxUserMetrics, watch it }
  // WritelnLog('FrameProfiler created, eats around %f MB of memory', [
  //   (SizeOf(FramesTimes) +
  //    SizeOf(CurrentFrameStarted) +
  //    SizeOf(CurrentFrameStart)) / (1024 * 1024)
  // ]);
  FFramesForSummary := DefaultFramesForSummary;
end;

procedure TCastleFrameProfiler.StartCore(const Metric: TMetricIndex);
begin
  if not FEnabled then Exit;

  if (Metric <> IndexFrameMetric(fmUpdateGeneratedTextures)) and
     CurrentFrameStarted[IndexFrameMetric(fmUpdateGeneratedTextures)] then
    Exit; // do not measure rendering times inside fmUpdateGeneratedTextures, they make output confusing

  if CurrentFrameStarted[Metric] then
  begin
    WritelnWarning('Frame metric %d not stopped yet (but started again). Possible causes: mismatched Start/Stop calls, or broken by exception', [
      Metric
    ]);
  end else
  begin
    CurrentFrameStarted[Metric] := true;
    CurrentFrameStart[Metric] := Timer;
  end;
end;

procedure TCastleFrameProfiler.StopCore(const Metric: TMetricIndex);
begin
  if not FEnabled then Exit;

  if (Metric <> IndexFrameMetric(fmUpdateGeneratedTextures)) and
     CurrentFrameStarted[IndexFrameMetric(fmUpdateGeneratedTextures)] then
    Exit; // do not measure rendering times inside fmUpdateGeneratedTextures, they make output confusing

  if not CurrentFrameStarted[Metric] then
  begin
    WritelnWarning('Frame metric %d not started. Possible cause: called FrameProfiler.Enabled between Start/Stop, this can be ignored.', [
      Metric
    ]);
  end else
  begin
    FramesTimes[CurrentFrame, Metric] := FramesTimes[CurrentFrame, Metric] +
      CurrentFrameStart[Metric].ElapsedTime;
    CurrentFrameStarted[Metric] := false;
  end;
end;

function TCastleFrameProfiler.SummaryTotalFrameTime: TFloatTime;
begin
  Result := FramesTimes[CurrentFrame, IndexTotalMetric];
end;

function TCastleFrameProfiler.SummaryTime(const FrameMetric: TFrameMetric): TFloatTime;
begin
  Result := FramesTimes[CurrentFrame, IndexFrameMetric(FrameMetric)];
end;

function TCastleFrameProfiler.SummaryTime(const UserMetric: String): TFloatTime;
var
  Metric: TMetricIndex;
begin
  if not IndexUserMetric(UserMetric, Metric) then
    raise Exception.CreateFmt('Cannot find user metric "%s"', [UserMetric]);
  Result := FramesTimes[CurrentFrame, Metric];
end;

procedure TCastleFrameProfiler.StartFrame;

  procedure DoLogSummary;
  var
    TotalTime: TFloatTime;

    function PercentTime(const MetricIndex: TMetricIndex): Integer; overload;
    begin
      Result := Round(100 * FramesTimes[CurrentFrame, MetricIndex] / TotalTime);
    end;

    function PercentTime(const FrameMetric: TFrameMetric): Integer; overload;
    begin
      Result := PercentTime(IndexFrameMetric(FrameMetric));
    end;

    function DisplayUserMetrics(const Names: array of String; const InitialIndex: TMetricIndex): String;
    var
      I: TUserMetric;
    begin
      Result := '';
      for I := 0 to MaxUserMetrics - 1 do
        if Names[I] <> '' then
          Result := Result + Format('  - %s: %d%%' + NL, [
            Names[I],
            PercentTime(InitialIndex + I)
          ]);
    end;

  begin
    TotalTime := SummaryTotalFrameTime;
    WritelnLogMultiline('FrameProfiler', Format(
      'Frame time: %f secs (we should have %f FPS based on this):' + NL +
      '- BeforeRender: %d%%' + NL +
      '- Render: %d%% (%f secs, we should have %f "only render FPS" based on this)' + NL +
      //'  - TCastleTransform.Render transformation: %d%%' + NL +
      '  - TCastleScene.Render: %d%%' + NL +
      //'    - ShapesFilterBlending: %d%%' + NL +
      '  - Swap / flush buffers (includes waiting for v-sync): %d%%' + NL +
      '%s' +
      '- Update: %d%%' + NL +
      '  - TCastleScene.UpdateGeneratedTextures: %d%%' + NL +
      '  - TCastleSceneCore.Update: %d%%' + NL +
      '  - Update Kraft Physics: %d%% (%.5f secs)' + NL +
      '%s' +
      '- Other:' + NL +
      '%s'
    , [
      TotalTime,
      1 / TotalTime,
      PercentTime(fmBeforeRender),
      PercentTime(fmRender),
      FramesTimes[CurrentFrame, IndexFrameMetric(fmRender)],
      1 / FramesTimes[CurrentFrame, IndexFrameMetric(fmRender)],
      PercentTime(fmRenderSwapFlush),
      //PercentTime(fmRenderTransform),
      PercentTime(fmRenderScene),
      //PercentTime(fmRenderShapesFilterBlending),
      DisplayUserMetrics(UserMetricsRenderNames, 1 + Ord(High(TFrameMetric)) + 1 + 0),
      PercentTime(fmUpdate),
      PercentTime(fmUpdateGeneratedTextures),
      PercentTime(fmUpdateScene),
      PercentTime(fmUpdatePhysics),
      FramesTimes[CurrentFrame, IndexFrameMetric(fmUpdatePhysics)],
      DisplayUserMetrics(UserMetricsUpdateNames, 1 + Ord(High(TFrameMetric)) + 1 + MaxUserMetrics),
      DisplayUserMetrics(UserMetricsOtherNames , 1 + Ord(High(TFrameMetric)) + 1 + 2 * MaxUserMetrics)
    ]));
  end;

var
  OtherMetric: TMetricIndex;
begin
  if not FEnabled then Exit;

  { Finish previous frame, if started.
    For IndexTotalMetric, we do not except the user code to ever call
    StopCore(IndexTotalMetric). }
  if CurrentFrameStarted[IndexTotalMetric] then
  begin
    Assert(FramesTimes[CurrentFrame, IndexTotalMetric] = 0); // should be cleared at the beginning of frame
    FramesTimes[CurrentFrame, IndexTotalMetric] := CurrentFrameStart[IndexTotalMetric].ElapsedTime;

    // check do have any other time started in this frame, and not stopped
    for OtherMetric := Low(TMetricIndex) to High(TMetricIndex) do
      if (OtherMetric <> IndexTotalMetric) and
         CurrentFrameStarted[OtherMetric] then
        begin
          WritelnWarning('Frame metric %d not stopped within this frame. Possible causes: mismatched Start/Stop calls, or broken by exception', [
            OtherMetric
          ]);
          FramesTimes[CurrentFrame, OtherMetric] := FramesTimes[CurrentFrame, OtherMetric] +
            CurrentFrameStart[OtherMetric].ElapsedTime;
        end;

    if (CurrentFrame > 0) and (CurrentFrame mod FramesForSummary = 0) then
    begin
      if Assigned(OnSummaryAvailable) then
        OnSummaryAvailable(Self);
      if LogSummary then
        DoLogSummary;
    end;

    CurrentFrame := (CurrentFrame + 1) mod MaxRecordFrames;
    FillChar(FramesTimes[CurrentFrame], SizeOf(FramesTimes[CurrentFrame]), 0);
    FillChar(CurrentFrameStarted, SizeOf(CurrentFrameStarted), 0);
    if InitialFrame = CurrentFrame then
      InitialFrame := (InitialFrame + 1) mod MaxRecordFrames;
  end;

  StartCore(IndexTotalMetric);
end;

procedure TCastleFrameProfiler.Reset;
begin
  CurrentFrame := 0;
  InitialFrame := 0;
  FillChar(FramesTimes[CurrentFrame], SizeOf(FramesTimes[CurrentFrame]), 0);
  FillChar(CurrentFrameStarted, SizeOf(CurrentFrameStarted), 0);
end;

procedure TCastleFrameProfiler.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not Value then // for now, we just always Reset when setting Enabled to false
      Reset;
  end;
end;

function TCastleFrameProfiler.IndexFrameMetric(const Metric: TFrameMetric): TMetricIndex;
begin
  Result := 1 + Ord(Metric);
end;

function TCastleFrameProfiler.IndexUserMetric(const Name: String; out Metric: TMetricIndex): Boolean;

  function FindUserMetricName(var Names: array of String; out Index: TUserMetric): Boolean;
  var
    Idx: TUserMetric;
  begin
    for Idx := Low(TUserMetric) to High(TUserMetric) do
      if Names[Idx] = Name then
      begin
        Index := Idx;
        Exit(true);
      end;

    // otherwise look for an empty slot
    for Idx := Low(TUserMetric) to High(TUserMetric) do
      if Names[Idx] = '' then
      begin
        Names[Idx] := Name;
        Index := Idx;
        Exit(true);
      end;

    WritelnWarning('Cannot find slot to store user metrics %s. Increase MaxUserMetrics in CastleTimeUtils.', [
      Name
    ]);
    Result := false;
  end;

var
  Multiplier: Integer;
  Index: TUserMetric;
begin
  Assert(Name <> '', 'User metric name cannot be empty, as we internally use empty metric names to mean "uninitialized"');

  { We have MaxUserMetrics slots for custom metrics during rendering,
    another MaxUserMetrics slots for custom metrics during updating,
    another MaxUserMetrics slots for custom metrics during neither of rendering/updating. }
  if CurrentFrameStarted[IndexFrameMetric(fmRender)] then
  begin
    if not FindUserMetricName(UserMetricsRenderNames, Index) then Exit(false);
    Multiplier := 0;
  end else
  if CurrentFrameStarted[IndexFrameMetric(fmUpdate)] then
  begin
    if not FindUserMetricName(UserMetricsUpdateNames, Index) then Exit(false);
    Multiplier := 1;
  end else
  begin
    if not FindUserMetricName(UserMetricsOtherNames, Index) then Exit(false);
    Multiplier := 2;
  end;

  Result := true;
  Metric := 1 +
    Ord(High(TFrameMetric)) + 1 +
    (Multiplier * MaxUserMetrics) + Index;
end;

procedure TCastleFrameProfiler.Start(const Metric: TFrameMetric);
begin
  StartCore(IndexFrameMetric(Metric));
end;

procedure TCastleFrameProfiler.Stop(const Metric: TFrameMetric);
begin
  StopCore(IndexFrameMetric(Metric));
end;

procedure TCastleFrameProfiler.Start(const UserMetric: String);
var
  Metric: TMetricIndex;
begin
  if not FEnabled then Exit;
  if not IndexUserMetric(UserMetric, Metric) then Exit;
  StartCore(Metric);
end;

procedure TCastleFrameProfiler.Stop(const UserMetric: String);
var
  Metric: TMetricIndex;
begin
  if not FEnabled then Exit;
  if not IndexUserMetric(UserMetric, Metric) then Exit;
  StopCore(Metric);
end;

{ global --------------------------------------------------------------------- }

var
  FFrameProfiler: TCastleFrameProfiler;

function FrameProfiler: TCastleFrameProfiler;
begin
  if FFrameProfiler = nil then
    FFrameProfiler := TCastleFrameProfiler.Create;
  Result := FFrameProfiler;
end;

{$endif read_implementation}
