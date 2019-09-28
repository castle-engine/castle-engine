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

{ Part of CastleTimeUtils unit: TInternalCastleFrameProfiler. }

{$ifdef read_interface}

type
  TFrameMetric = (
    fmFrame,
    fmBeforeRender,
    fmRender,
    fmRenderFrameProfiler,
    fmRenderScene,
    fmUpdate,
    fmUpdateScene
  );

  { Profiler gathering statistics about each game frame.
    Useful to measure where you spend time during the game frame.
    In contrast to @link(TCastleProfiler), this is not flexible to gather
    all possible data, or to keep all historic data.
    It is only specialized to gather some specific data, in each frame,
    to have statistics from the most recent frames.

    You usually do not need to use this class (or it's instance
    in @link(FrameProfile) at all).
    Instead just display it using the TCastleFrameProfilerDisplay.
  }
  TInternalCastleFrameProfiler = class
  private
    const
      MaxRecordFrames = 1000;
    var
      FEnabled: Boolean;
      FramesTimes: array [0..MaxRecordFrames - 1, TFrameMetric] of TFloatTime;
      CurrentFrameStarted: array [TFrameMetric] of Boolean;
      CurrentFrameStart: array [TFrameMetric] of TProcessTimerResult;
      CurrentFrame: Integer;
      // if InitialFrame > CurrentFrame, then it means statistics wraps in FramesTimes array
      InitialFrame: Integer;
  public
    { No data is gathered when Enabled = false, the calls to @link(StartFrame),
      @link(Start), @link(Stop) are ignored then. }
    property Enabled: Boolean read FEnabled write FEnabled;
    procedure Start(const Metric: TFrameMetric);
    procedure Stop(const Metric: TFrameMetric);
  end;

{ Single instance of TInternalCastleFrameProfiler that you can use for all profiling.
  Castle Game Engine uses it to report loading times of various frame operations
  automatically. }
function InternalFrameProfiler: TInternalCastleFrameProfiler;

{$endif read_interface}

{$ifdef read_implementation}

function FrameMetricToStr(const Metric: TFrameMetric): String;
begin
  WriteStr(Result, Metric);
end;

procedure TInternalCastleFrameProfiler.Start(const Metric: TFrameMetric);
var
  OtherMetric: TFrameMetric;
begin
  if not FEnabled then Exit;

  if Metric = fmFrame then
  begin
    { Finish previous frame, if started.
      For fmFrame, we do not except the other code to call Stop(fmFrame). }
    if CurrentFrameStarted[fmFrame] then
    begin
      Assert(FramesTimes[CurrentFrame, fmFrame] = 0); // should be cleared at the beginning of frame
      FramesTimes[CurrentFrame, fmFrame] := CurrentFrameStart[fmFrame].ElapsedTime;

      // check do have any other time started in this frame, and not stopped
      for OtherMetric in TFrameMetric do
        if (OtherMetric <> fmFrame) and
           CurrentFrameStarted[OtherMetric] then
          begin
            WritelnWarning('Frame time %s not stopped within this frame, probably broken by exception', [
              FrameMetricToStr(OtherMetric)
            ]);
            FramesTimes[CurrentFrame, OtherMetric] += CurrentFrameStart[OtherMetric].ElapsedTime;
          end;

      // until we have a proper TCastleFrameProfilerDisplay, just dump it in the log every second
      if CurrentFrame mod 60 = 0 then
      begin
        WritelnLogMultiline('FrameProfiler', Format(
          'Frame time: %f secs (we should have %f FPS based on this):' + NL +
          '- BeforeRender: %d%%' + NL +
          '- Render: %d%% (%f secs, we should have %f "only render FPS" based on this)' + NL +
          '  - RenderFrameProfiler: %d%%' + NL +
          '  - RenderScene: %d%%' + NL +
          '- Update: %d%%' + NL +
          '  - UpdateScene: %d%%', [
          FramesTimes[CurrentFrame, fmFrame],
          1 / FramesTimes[CurrentFrame, fmFrame],
          Round(100 * FramesTimes[CurrentFrame, fmBeforeRender] / FramesTimes[CurrentFrame, fmFrame]),
          Round(100 * FramesTimes[CurrentFrame, fmRender] / FramesTimes[CurrentFrame, fmFrame]),
          FramesTimes[CurrentFrame, fmRender],
          1 / FramesTimes[CurrentFrame, fmRender],
          Round(100 * FramesTimes[CurrentFrame, fmRenderFrameProfiler] / FramesTimes[CurrentFrame, fmFrame]),
          Round(100 * FramesTimes[CurrentFrame, fmRenderScene] / FramesTimes[CurrentFrame, fmFrame]),
          Round(100 * FramesTimes[CurrentFrame, fmUpdate] / FramesTimes[CurrentFrame, fmFrame]),
          Round(100 * FramesTimes[CurrentFrame, fmUpdateScene] / FramesTimes[CurrentFrame, fmFrame])
        ]));
      end;

      CurrentFrame := (CurrentFrame + 1) mod MaxRecordFrames;
      FillChar(FramesTimes[CurrentFrame], SizeOf(FramesTimes[CurrentFrame]), 0);
      FillChar(CurrentFrameStarted, SizeOf(CurrentFrameStarted), 0);
      if InitialFrame = CurrentFrame then
        InitialFrame := (InitialFrame + 1) mod MaxRecordFrames;
    end;
  end;

  if CurrentFrameStarted[Metric] then
  begin
    WritelnWarning('Frame time %s not stopped yet (but started again), probably broken by exception', [
      FrameMetricToStr(Metric)
    ]);
  end else
  begin
    CurrentFrameStarted[Metric] := true;
    CurrentFrameStart[Metric] := ProcessTimer;
  end;
end;

procedure TInternalCastleFrameProfiler.Stop(const Metric: TFrameMetric);
begin
  if not FEnabled then Exit;

  Assert(CurrentFrameStarted[Metric]);
  FramesTimes[CurrentFrame, Metric] += CurrentFrameStart[Metric].ElapsedTime;
  CurrentFrameStarted[Metric] := false;
end;

{ global --------------------------------------------------------------------- }

var
  FFrameProfiler: TInternalCastleFrameProfiler;

function InternalFrameProfiler: TInternalCastleFrameProfiler;
begin
  if FFrameProfiler = nil then
    FFrameProfiler := TInternalCastleFrameProfiler.Create;
  Result := FFrameProfiler;
end;

{$endif read_implementation}
