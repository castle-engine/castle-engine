{%MainUnit castletimeutils.pas}
{
  Copyright 2019-2019 Michalis Kamburelis.

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
  TFrameMetric = (
    fmBeforeRender,
    fmRender,
    fmRenderTransform,
    fmRenderScene,
    fmRenderShapesFilterBlending,
    //fmRenderFrameProfiler,
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

    TODO: In the future we plan to implement UI to automatically
    show this in a pretty manner, like TCastleDebugOverlay .
  }
  TCastleFrameProfiler = class
  private
    const
      MaxRecordFrames = 1000;
      MaxUserMetrics = 16;
      MetricsPerFrame = 1 //< frame total time
        + Ord(High(TFrameMetric)) + 1 //< time, for each TFrameMetric
        + 3 * MaxUserMetrics; //< user metrics, MaxUserMetrics for render, update, other
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
    function IndexFrameMetric(const Metric: TFrameMetric): TMetricIndex;
    function IndexUserMetric(const Name: String; out Metric: TMetricIndex): Boolean;
    { StartCore and StopCore unify all metrics (frame total, TFrameMetric, user metrics)
      as a simple integer index. }
    procedure StartCore(const Metric: TMetricIndex);
    procedure StopCore(const Metric: TMetricIndex);
  public
    constructor Create;
    { No data is gathered when Enabled = false, the calls to @link(StartFrame),
      @link(Start), @link(Stop) are ignored then. }
    property Enabled: Boolean read FEnabled write FEnabled;
    procedure StartFrame;
    procedure Start(const Metric: TFrameMetric); overload;
    procedure Stop(const Metric: TFrameMetric); overload;
    procedure Start(const UserMetric: String); overload;
    procedure Stop(const UserMetric: String); overload;
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

  Assert(CurrentFrameStarted[Metric]);
  FramesTimes[CurrentFrame, Metric] := FramesTimes[CurrentFrame, Metric] +
    CurrentFrameStart[Metric].ElapsedTime;
  CurrentFrameStarted[Metric] := false;
end;

procedure TCastleFrameProfiler.StartFrame;

  procedure DisplaySummary;
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
    TotalTime := FramesTimes[CurrentFrame, IndexTotalMetric];
    WritelnLogMultiline('FrameProfiler', Format(
      'Frame time: %f secs (we should have %f FPS based on this):' + NL +
      '- BeforeRender: %d%%' + NL +
      '- Render: %d%% (%f secs, we should have %f "only render FPS" based on this)' + NL +
      '  - TCastleTransform.Render transformation: %d%%' + NL +
      '  - TCastleScene.Render: %d%%' + NL +
      '    - ShapesFilterBlending: %d%%' + NL +
      // '  - RenderFrameProfiler: %d%%' + NL +
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
      PercentTime(fmRenderTransform),
      PercentTime(fmRenderScene),
      PercentTime(fmRenderShapesFilterBlending),
      // PercentTime(fmRenderFrameProfiler),
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

const
  DisplaySummaryAtFrames = 60;
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

    // until we have a proper TCastleFrameProfilerDisplay, just dump it in the log every second
    if CurrentFrame mod DisplaySummaryAtFrames = 0 then
      DisplaySummary;

    CurrentFrame := (CurrentFrame + 1) mod MaxRecordFrames;
    FillChar(FramesTimes[CurrentFrame], SizeOf(FramesTimes[CurrentFrame]), 0);
    FillChar(CurrentFrameStarted, SizeOf(CurrentFrameStarted), 0);
    if InitialFrame = CurrentFrame then
      InitialFrame := (InitialFrame + 1) mod MaxRecordFrames;
  end;

  StartCore(IndexTotalMetric);
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
