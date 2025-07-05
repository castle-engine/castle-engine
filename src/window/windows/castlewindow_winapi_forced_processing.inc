{%MainUnit ../castlewindow.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ "Forced processing" is a mechanism to keep window being responsive
  (keep updating (e.g. animating stuff) and rendering)
  when user moves / resizes / operates menu of the window.

  Why is this necessary:

  - The default WinAPI behavior:

      The DefWindowProcW enters a modal loop in these cases
      (when user moves / resizes / operates menu of the window,
      which starts from WM_NCLBUTTONDOWN message).
      Our methods like ProcessMessage are not called in a loop anymore,
      instead we wait in a DispatchMessage call in top-level ProcessMessage.
      WinAPI will directly call WindowProc, creating a recursion.
      See http://stackoverflow.com/questions/3102074/win32-my-application-freezes-while-the-user-resizes-the-window

  - To keep updating+rendering during this time, we have to call the
    UpdateAndRenderEverything manually from the message processing.

  At the same time, we want to avoid window visibly lagging during this,
  see https://github.com/castle-engine/castle-engine/issues/672 .
}

{$ifdef read_window_interface}
// Below we define nested types and fields within TCastleWindow interface
private
  type
    { Measure the speed and frequency of "forced processing" calls,
      which are Application.UpdateAndRenderEverything done in special situation.
      Modelled a bit on TFramesPerSecond. }
    TForcedProcessingFps = record
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
      { Is it allowed to start new call, with CallBegin, *right now*.
        This may return @false to avoid doing to make "forced processing"
        too oftenm which would make Windows stutter when
        moving / resizign the window.
        See https://github.com/castle-engine/castle-engine/issues/672 . }
      function CheckLimit: Boolean;
    end;

    { Track is "forced processing" necessary and allowed now.
      This should be instantiated once in each TCastleWindow. }
    TForcedProcessingState = record
      { Do forced processing only when Necessary<>0 and Disallowed=0. }
      Necessary, Disallowed: Cardinal;
      { Keep track of how much time the "forced processing" calls to
        Application.UpdateAndRenderEverything take. }
      Fps: TForcedProcessingFps;
      { Reference back to TCastleWindow that manages this. }
      Window: TCastleWindow;
    end;

  var
    ForcedProcessingState: TForcedProcessingState;
{$endif read_window_interface}

{$ifdef read_implementation}

type
  { Track should "forced processing" be necessary and allowed now.
    This should be instantiated once in each TCastleWindow.WindowProc call. }
  TForcedProcessingMessageState = record
    MakeNecessary, MakeDisallowed: Boolean;
    procedure BeforeDefWindowProc(var State: TCastleWindow.TForcedProcessingState;
      const uMsg: UINT; const wParm: WPARAM; const lParm: LPARAM);
    procedure AfterDefWindowProc(var State: TCastleWindow.TForcedProcessingState);
  end;

{ TForcedProcessingFps ------------------------------------------------------- }

procedure TCastleWindow.TForcedProcessingFps.UpdateFps;
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

    { Log "forced processing" time. }
    {$define CASTLE_LOG_FORCED_PROCESSING}
    {$ifdef CASTLE_LOG_FORCED_PROCESSING}
    if CallsCount <> 0 then
      WritelnLog('ForcedProcessing calls per second: real: %f, only calls: %f', [
        RealFps,
        OnlyCallsFps
      ]);
    {$endif}

    LastRecalculateTime := NowTime;
    CallsCount := 0;
    OnlyCallsTimePassed := 0;
  end;
end;

procedure TCastleWindow.TForcedProcessingFps.CallBegin;
begin
  CallStartTime := Timer;
end;

procedure TCastleWindow.TForcedProcessingFps.CallEnd;
begin
  Inc(CallsCount);
  OnlyCallsTimePassed := OnlyCallsTimePassed + TimerSeconds(Timer, CallStartTime);
end;

function TCastleWindow.TForcedProcessingFps.CheckLimit: boolean;
const
  FpsLimit = 10.0;
begin
  Result :=
    // CallBegin never called
    (not CallStartTime.Initialized) or
    // CallBegin called more than 1 / FpsLimit secs ago
    (TimerSeconds(Timer, CallStartTime) > 1 / FpsLimit);
end;

{ TForcedProcessingMessageState ---------------------------------------------- }

procedure TForcedProcessingMessageState.BeforeDefWindowProc(
  var State: TCastleWindow.TForcedProcessingState;
  const uMsg: UINT; const wParm: WPARAM; const lParm: LPARAM);
begin
  State.Fps.UpdateFps;

  if (State.Necessary <> 0) and
     (State.Disallowed = 0) and
     (not State.Window.Closed) and
     State.Fps.CheckLimit then
  begin
    State.Fps.CallBegin;
    Application.UpdateAndRenderEverything;
    State.Fps.CallEnd;
  end;

  MakeNecessary := uMsg = WM_NCLBUTTONDOWN;

  { Disallow in certain cases, even when Necessary<>0. Prevents:
    1. crashes, when this is done from some messages.
    2. slowdown at initialization (castle-model-viewer main menu blinks for some
      time, with "busy" cursor, before it becomes responsive). }
  MakeDisallowed :=
    // user closes window by clicking on close button
    ((uMsg = WM_SYSCOMMAND) and (wParm = SC_CLOSE)) or
    // we close the window by code
    (uMsg = WM_NCACTIVATE) or
    // we recreate the window as fullscreen
    (uMsg = WM_IME_SETCONTEXT) or
    // we recreate the window as windowed?
    (uMsg = WM_ACTIVATE);

  if MakeNecessary then Inc(State.Necessary);
  if MakeDisallowed then Inc(State.Disallowed);
end;

procedure TForcedProcessingMessageState.AfterDefWindowProc(
  var State: TCastleWindow.TForcedProcessingState);
begin
  if MakeNecessary then Dec(State.Necessary);
  if MakeDisallowed then Dec(State.Disallowed);
end;

{$endif read_implementation}
