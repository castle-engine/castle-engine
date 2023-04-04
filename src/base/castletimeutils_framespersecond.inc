{%MainUnit castletimeutils.pas}
{
  Copyright 2000-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleTimeUtils unit: TFramesPerSecond. }

{$ifdef read_interface}

type
  TFrameId = QWord;

  { Measure frames per second.

    You should never create an instance of this class directly,
    instead just use the available instance in @link(TCastleContainer.Fps).
    The @link(TCastleContainer) instance is, in turn, automatically
    created for each @link(TCastleWindow) or @link(TCastleControl). }
  TFramesPerSecond = class
  private
    FOnlyRenderFps: TFloatTime;
    FRealFps: TFloatTime;
    FSecondsPassed: TFloatTime;
    DoZeroNextSecondsPassed: boolean;
    FUpdateStartTime: TTimerResult;
    LastRecalculateTime: TTimerResult;
    RenderStartTime: TTimerResult;
    { 0 means "no frame was rendered yet" }
    FramesRendered: QWord;
    { how much time passed inside frame rendering }
    OnlyRenderTimePassed: TTimerResult;
    FMaxSensibleSecondsPassed: TFloatTime;
    FWasSleeping, FSleeping: boolean;
  public
    const
      DefaultMaxSensibleSecondsPassed = 0.5;

    constructor Create;

    { Internal for Castle Game Engine.
      @groupBegin
      @exclude }
    constructor InternalCreate;
    { @exclude }
    procedure InternalRenderBegin;
    { @exclude }
    procedure InternalRenderEnd;
    { @exclude }
    procedure InternalSleeping;
    { @exclude }
    procedure InternalUpdateBegin;
    { @groupEnd }

    { Rendering speed, measured in frames per second, but accounting only
      time spent inside "render" calls (thus ignoring time spent on
      physics and other logic).

      This measures only time spend in @link(TCastleContainer.EventRender)
      method (and it's subordinates, like @link(TCastleUserInterface.Render),
      @link(TCastleScene.LocalRender),
      @link(TCastleWindow.OnRender)).
      It does not take into account time spent on other activities,
      like "update" calls, and it doesn't take into account that frames are possibly
      not rendered all the time (when AutoRedisplay = @false).

      See https://castle-engine.io/manual_optimization.php#section_fps
      for a detailed description what FPS mean and how they should be interpreted.

      @seealso RealFps }
    property OnlyRenderFps: TFloatTime read FOnlyRenderFps;
    property FrameTime: TFloatTime read FOnlyRenderFps;
      {$ifdef FPC} deprecated 'use OnlyRenderFps'; {$endif}

    { How many frames per second were actually rendered.
      This is the number of @link(TCastleContainer.EventRender) calls
      that actually happened within a real second of time.
      So it's an actual speed of your program.
      Anything can slow this down, not only long rendering,
      but also slow processing of other events (like "update" that does physics).

      When @link(TCastleWindow.AutoRedisplay) or
      @link(TCastleControl.AutoRedisplay) is @false,
      this may be very low, since we may not
      render the frames all the time (we may sleep for some time,
      or perform updates without rendering).
      In this case, the RealFps value may be confusing and useless
      (it does not reflect the speed of your application).
      Use the WasSleeping to detect this, and potentially hide the display
      of RealFps from user.

      See https://castle-engine.io/manual_optimization.php#section_fps
      for a detailed description what FPS mean and how they should be interpreted.

      @seealso OnlyRenderFps }
    property RealFps: TFloatTime read FRealFps;
    property RealTime: TFloatTime read FRealFps; {$ifdef FPC} deprecated 'use RealFps'; {$endif}

    { Some of the frames were not rendered, because the scene and camera
      were not moving. This happens only when
      @link(TCastleWindow.AutoRedisplay)
      or @link(TCastleControl.AutoRedisplay) are @false,
      and it basically indicates that the @link(RealFps) value is not a useful
      indicator of your application speed.

      See https://castle-engine.io/manual_optimization.php#section_fps
      for a detailed description what this means. }
    property WasSleeping: boolean read FWasSleeping;

    { Display current FPS (RealFps, OnlyRenderFps, taking into account WasSleeping). }
    function ToString: string; override;

    { How much time passed since the last "update".
      You should use this inside "update" events and methods
      (@link(TCastleContainer.EventUpdate),
      @link(TCastleUserInterface.Update),
      @link(TCastleTransform.Update)...) to scale the movement.
      This way, your animation will work with the same speed
      (objects will travel at the same speed),
      regardless of the system performance (regardless of how often
      the "update" event occurs).

      This is calculated as a time between
      start of previous "update" event and start of current "update" event. }
    property SecondsPassed: TFloatTime read FSecondsPassed;

    property UpdateSecondsPassed: TFloatTime read FSecondsPassed;
      {$ifdef FPC} deprecated 'use SecondsPassed'; {$endif}

    { Limit the SecondsPassed variable, to avoid increasing time in game
      a lot when a game was hanging or otherwise waiting for some exceptional
      event from OS.
      Used only when non-zero.
      By default it's DefaultMaxSensibleSecondsPassed. }
    property MaxSensibleSecondsPassed: TFloatTime
      read FMaxSensibleSecondsPassed write FMaxSensibleSecondsPassed;

    { Forces SecondsPassed for the next "update" call to be zero.

      This is useful if you just came back from some modal state,
      like a modal dialog box (like TCastleWindow.FileDialog or modal boxes
      in CastleMessages -- they already call this method).
      SecondsPassed could be ridicoulously long in such case
      (if our message loop is totally paused, as in TCastleWindow.FileDialog
      on Windows) or not relevant (if we do our message loop,
      but we display something entirely different, like CastleMessages).
      So it's best to pretend that SecondsPassed is 0.0,
      so things such as TCastleSceneCore.Time do not advance wildly
      just because we did a modal dialog.

      This forces the SecondsPassed to be zero at the next update event
      (InternalUpdateBegin). }
    procedure ZeroNextSecondsPassed;

    { Time of last Update call. }
    property UpdateStartTime: TTimerResult read FUpdateStartTime;

    { Current frame identifier.

      Changed when each container "update" event occurs,
      so this is equal during all @link(TCastleUserInterface.Update),
      @link(TCastleUserInterface.Render), @link(TCastleTransform.Update),
      @link(TCastleTransform.LocalRender) occuring within the same frame.
      You can use this to avoid performing the same job many times
      in a single frame.

      Never zero.

      It's a class function, so you can access it like
      @code(TFramesPerSecond.FrameId),
      no need to have a TFramesPerSecond instance (which is usually
      accessed from TCastleContainer, like @link(TCastleContainer.Fps),
      @link(TCastleWindow.Fps), @link(TCastleControl.Fps). }
    class function FrameId: TFrameId;

    { Frame identifier within which last render started.

      This is usually equal to @link(FrameId) or @link(FrameId) - 1
      (depending on the order, whether Update or Render happens first,
      which may be platform-dependent).
      Though if you disabled TCastleWindow.AutoRedisplay, it may be something
      even smaller than @link(FrameId) - 1.

      Never zero. (This allows to easily treat "frame id = zero" as
      "frame id that never happened" in some logic.)
    }
    class function RenderFrameId: TFrameId;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TFramesPerSecond ----------------------------------------------------------- }

var
  FFrameId: TFrameId = 1;
  FRenderFrameId: TFrameId = 1;

constructor TFramesPerSecond.Create;
begin
  raise Exception.Create('Do not create instance of TFramesPerSecond class directly, only use the already-created instance in TCastleContainer.Fps');
end;

constructor TFramesPerSecond.InternalCreate;
const
  DefaultFps = 60.0;
begin
  inherited;

  { init time measurements to some sensible defaults }
  FOnlyRenderFps := DefaultFps;
  FRealFps := DefaultFps;
  { for SecondsPassed this initialization is actually not essential,
    since we call ZeroNextSecondsPassed anyway.
    But in case programmer will (incorrectly!) try to use SecondsPassed
    before InternalUpdateBegin call, it's useful to have here some predictable value. }
  FSecondsPassed := 1 / DefaultFps;

  { the default is non-zero now, since all Android games need it }
  FMaxSensibleSecondsPassed := DefaultMaxSensibleSecondsPassed;

  ZeroNextSecondsPassed;
end;

procedure TFramesPerSecond.InternalRenderBegin;
begin
  RenderStartTime := Timer;
  FRenderFrameId := FFrameId;
end;

procedure TFramesPerSecond.InternalRenderEnd;
begin
  Inc(FramesRendered);
  OnlyRenderTimePassed.Value := OnlyRenderTimePassed.Value + Timer.Value - RenderStartTime.Value;
end;

procedure TFramesPerSecond.InternalSleeping;
begin
  FSleeping := true;
end;

function TFramesPerSecond.ToString: string;
begin
  if (RealFps = 0) and (OnlyRenderFps = 0) then
    Exit('no frames rendered');

  if WasSleeping then
    Result := 'no need to render all frames'
  else
    Result := FormatDot('%f', [RealFps]);
  Result := Result + FormatDot(' (only render: %f)', [OnlyRenderFps]);
end;

procedure TFramesPerSecond.InternalUpdateBegin;

  { Update RealFps, OnlyRenderFps, WasSleeping }
  procedure UpdateFps;
  const
    TimeToRecalculate = 1.0; { in seconds }
  var
    NowTime: TTimerResult;
  begin
    NowTime := Timer;
    if TimerSeconds(NowTime, LastRecalculateTime) >= TimeToRecalculate then
    begin
      { update FRealFps, FOnlyRenderFps once for TimeToRecalculate time.
        This way they don't change rapidly.

        Previously we used more elaborate hacks for this (resetting
        their times after a longer periods, but keeping some previous
        results), but they were complex and bad: when the game speed
        was changing suddenly, FRealFps, FOnlyRenderFps should also change
        suddenly, not gradually increase / decrease. }

      FRealFps := FramesRendered / TimerSeconds(NowTime, LastRecalculateTime);
      FWasSleeping := FSleeping;

      if OnlyRenderTimePassed.Value > 0 then
        FOnlyRenderFps := FramesRendered * TimerFrequency / OnlyRenderTimePassed.Value
      else
        FOnlyRenderFps := 0;

      LastRecalculateTime := NowTime;
      FramesRendered := 0;
      OnlyRenderTimePassed.Value := 0;
      FSleeping := false;
    end;
  end;

var
  NewUpdateStartTime: TTimerResult;
begin
  { update FSecondsPassed, DoZeroNextSecondsPassed, FUpdateStartTime }
  NewUpdateStartTime := Timer;

  if DoZeroNextSecondsPassed then
  begin
    FSecondsPassed := 0.0;
    DoZeroNextSecondsPassed := false;
  end else
  begin
    FSecondsPassed := TimerSeconds(NewUpdateStartTime, FUpdateStartTime);
    if MaxSensibleSecondsPassed > 0 then
      FSecondsPassed := Min(FSecondsPassed, MaxSensibleSecondsPassed);
  end;

  FUpdateStartTime := NewUpdateStartTime;

  Inc(FFrameId);

  UpdateFps;
end;

procedure TFramesPerSecond.ZeroNextSecondsPassed;
begin
  DoZeroNextSecondsPassed := true;
end;

class function TFramesPerSecond.FrameId: TFrameId;
begin
  Result := FFrameId;
end;

class function TFramesPerSecond.RenderFrameId: TFrameId;
begin
  Result := FRenderFrameId;
end;

{$endif read_implementation}
