type
  { Measure the speed and frequency of
    UpdateAndRenderEverythingDuringRecursiveWindowProc.
    Modelled a bit on TFramesPerSecond. }
  TRecursiveUpdatesFps = record
  strict private
    { Used to update displayed measurement from time to time. }
    OnlyCallsFps: TFloatTime;
    RealFps: TFloatTime;
    LastRecalculateTime: TTimerResult;

    { Increased each time the call happens. }
    CallsCount: Cardinal;
    OnlyCallsTimePassed: TFloatTime;

    { Used only CallBegin and CallEnd, to properly update OnlyCallsTimePassed. }
    CallStartTime: TTimerResult;
  public
    { Update RealFps, OnlyCallsFps and potentially other fields.
      Output new measurements from time to time. }
    procedure UpdateFps;
    procedure CallBegin;
    procedure CallEnd;
  end;
var
  RecursiveUpdatesFps: TRecursiveUpdatesFps;

procedure TRecursiveUpdatesFps.UpdateFps;
const
  TimeToRecalculate = 1.0; { in seconds }
var
  NowTime: TTimerResult;
begin
  NowTime := Timer;
  if TimerSeconds(NowTime, LastRecalculateTime) >= TimeToRecalculate then
  begin
    RealFps := CallsCount / TimerSeconds(NowTime, LastRecalculateTime);

    if OnlyCallsTimePassed > 0 then
      OnlyCallsFps := CallsCount / OnlyCallsTimePassed
    else
      OnlyCallsFps := 0;

    if CallsCount <> 0 then
      WritelnLog('TRecursiveUpdatesFps: real: %f, only calls: %f', [
        RealFps,
        OnlyCallsFps
      ]);

    LastRecalculateTime := NowTime;
    CallsCount := 0;
    OnlyCallsTimePassed := 0;
  end;
end;

procedure TRecursiveUpdatesFps.CallBegin;
begin
  CallStartTime := Timer;
end;

procedure TRecursiveUpdatesFps.CallEnd;
begin
  Inc(CallsCount);
  OnlyCallsTimePassed := OnlyCallsTimePassed + TimerSeconds(Timer, CallStartTime);
end;
