{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Children added to @link(ScrollArea) can be scrolled vertically.
    We automatically show a scrollbar, and handle various scrolling inputs
    to be functional on both desktops and mobile
    (we handle scrolling by keys, mouse wheel, dragging by scrollbar,
    dragging the whole area - see @link(EnableDragging)). }
  TCastleScrollView = class(TCastleUserInterface)
  strict private
    FScrollArea: TCastleUserInterface;
    Scissor: TScissor;

    FScroll: Single;
    FScrollbarVisible: boolean;
    { Rects in screen coordinates (like RenderRect),
      after UI scaling and anchors. }
    ScrollbarFrame, ScrollbarSlider: TFloatRectangle;
    ScrollBarDragging: boolean;
    FKeyScrollSpeed, FWheelScrollSpeed: Single;
    FScrollBarWidth: Single;
    FEnableDragging: boolean;
    DragSinceLastUpdate, DragSpeed, TimeSinceDraggingStopped, TimeSinceDraggingStarted: Double;
    ScrollbarActive: Single;
    FTintScrollBarInactive: TCastleColor;
    procedure SetScroll(Value: Single);
    function ScrollBarWidthScaled: Single;
  protected
    { Min and max sensible values for @link(Scroll). }
    function ScrollMin: Single;
    function ScrollMax: Single;

    { How many pixels do we scroll. This corresponds to ScrollArea vertical anchor,
      so it's in unscaled pixels. Kept as a float, to allow smooth time-based changes.
      Note that setting it always clamps the value to a sensible range. }
    property Scroll: Single read FScroll write SetScroll;

    property ScrollbarVisible: boolean read FScrollbarVisible;
  public
    const
      DefaultKeyScrollSpeed = 200.0;
      DefaultWheelScrollSpeed = 20.0;
      DefaultScrollBarWidth = 20;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure RenderOverChildren; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;

    { Color and alpha tint to use when scrollbar is not used.
      May have some alpha, which makes scrollbar "make itself more opaque",
      and thus noticeable, when you start dragging.
      By default it's opaque white, which means that no tint is shown. }
    property TintScrollBarInactive: TCastleColor read FTintScrollBarInactive write FTintScrollBarInactive;
  published
    { Children you add here will be scrolled.
      To have a useful scrolling area:

      @unorderedList(
        @itemSpacing compact
        @item(Add some children to ScrollArea,)
        @item(Set proper size of the ScrollArea (you can set an explicit size,
          or use @link(TCastleUserInterface.AutoSizeToChildren)),)
        @item(Leave the ScrollArea anchors as default: ScrollArea should be
          anchored to the top-left corner of the parent.)
      ) }
    property ScrollArea: TCastleUserInterface read FScrollArea;
    { Speed of scrolling by keys, in pixels (before UI scaling) per second. }
    property KeyScrollSpeed: Single read FKeyScrollSpeed write FKeyScrollSpeed default DefaultKeyScrollSpeed;
    { Speed of scrolling by mouse wheel, in pixels (before UI scaling) per event. }
    property WheelScrollSpeed: Single read FWheelScrollSpeed write FWheelScrollSpeed default DefaultWheelScrollSpeed;
    { Width of the scroll bar. }
    property ScrollBarWidth: Single read FScrollBarWidth write FScrollBarWidth default DefaultScrollBarWidth;
    { Enable scrolling by dragging @italic(anywhere) in the scroll area.
      This is usually suitable for mobile devices.
      Note that this doesn't affect the dragging directly by the scrollbar,
      which is always enabled. }
    property EnableDragging: boolean read FEnableDragging write FEnableDragging default false;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleScrollView ---------------------------------------------------------- }

constructor TCastleScrollView.Create(AOwner: TComponent);
begin
  inherited;

  FKeyScrollSpeed := DefaultKeyScrollSpeed;
  FWheelScrollSpeed := DefaultWheelScrollSpeed;
  FScrollBarWidth := DefaultScrollBarWidth;
  FTintScrollBarInactive := White;

  FScrollArea := TCastleUserInterface.Create(Self);
  FScrollArea.SetSubComponent(true);
  FScrollArea.Name := 'ScrollArea';
  FScrollArea.Anchor(vpTop);
  InsertFront(FScrollArea);

  Scissor := TScissor.Create;
end;

destructor TCastleScrollView.Destroy;
begin
  FreeAndNil(Scissor);
  inherited;
end;

procedure TCastleScrollView.Render;
begin
  inherited;
  Scissor.Rect := RenderRect.Round;
  Scissor.Enabled := true;
end;

procedure TCastleScrollView.RenderOverChildren;
var
  Color: TCastleColor;
begin
  Scissor.Enabled := false;

  if ScrollBarVisible then
  begin
    Color := Lerp(ScrollbarActive, TintScrollBarInactive, White);
    Theme.Draw(ScrollbarFrame, tiScrollbarFrame, UIScale, Color);
    Theme.Draw(ScrollbarSlider, tiScrollbarSlider, UIScale, Color);
  end;
  inherited;
end;

procedure TCastleScrollView.SetScroll(Value: Single);
begin
  ClampVar(Value, ScrollMin, ScrollMax);
  if FScroll <> Value then
  begin
    FScroll := Value;
    FScrollArea.Anchor(vpTop, FScroll);
  end;
end;

procedure TCastleScrollView.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  { Calculate ScrollBarVisible, ScrollbarFrame, ScrollbarFrame }
  procedure UpdateScrollBarVisibleAndRects;
  var
    SR: TFloatRectangle;
  begin
    SR := RenderRect;

    ScrollbarFrame := SR.RightPart(ScrollBarWidthScaled);

    ScrollbarSlider := ScrollbarFrame;
    if ScrollArea.EffectiveHeight = 0 then
      FScrollBarVisible := false
    else
    begin
      ScrollbarSlider.Height := EffectiveHeight * SR.Height / ScrollArea.EffectiveHeight;
      FScrollBarVisible := ScrollbarFrame.Height > ScrollbarSlider.Height;
      { equivalent would be to set
          ScrollBarVisible := EffectiveHeight < ScrollArea.EffectiveHeight
        But this way makes it clear that MapRange below is valid, will not divide by zero. }
    end;
    if FScrollBarVisible then
      ScrollbarSlider.Bottom := ScrollbarSlider.Bottom +
        (MapRange(Scroll, ScrollMin, ScrollMax,
          ScrollbarFrame.Height - ScrollbarSlider.Height, 0)) else
    begin
      ScrollBarDragging := false;
      Scroll := ScrollMin; // make sure to shift to ScrollMin if scroll suddenly disappears
      DragSpeed := 0;
      TimeSinceDraggingStopped := 0;
      ScrollbarActive := 0;
    end;
  end;

  procedure HandleKeys;
  begin
    if ScrollBarVisible and HandleInput then
    begin
      if Container.Pressed[K_Up  ] then
      begin
        Scroll := Scroll - KeyScrollSpeed * SecondsPassed;
        TimeSinceDraggingStopped := 0;
      end;
      if Container.Pressed[K_Down] then
      begin
        Scroll := Scroll + KeyScrollSpeed * SecondsPassed;
        TimeSinceDraggingStopped := 0;
      end;
      HandleInput := not ExclusiveEvents;
    end;
  end;

  { Make the illusion of "inertial force" when dragging, by gradually
    decelerating dragging speed once user stops dragging.
    Also updates TimeSinceDraggingStopped. }
  procedure DraggingInertialForce;
  const
    DragDecelerationDuration = 0.5;
  var
    CurrentDragSpeed: Single;
  begin
    if ScrollbarVisible then
    begin
      if mbLeft in Container.MousePressed then
      begin
        { note that we update DragSpeed even when DragSinceLastUpdate = 0,
          which means user keeps pressing but doesn't drag }
        if not IsZero(SecondsPassed) then
          DragSpeed := DragSinceLastUpdate / SecondsPassed else
          DragSpeed := 0; // whatever sensible value
        TimeSinceDraggingStopped := 0;
      end else
      begin
        TimeSinceDraggingStopped := TimeSinceDraggingStopped + SecondsPassed;
        if (DragSpeed <> 0) and
           (TimeSinceDraggingStopped < DragDecelerationDuration) then
        begin
          CurrentDragSpeed := MapRange(
            TimeSinceDraggingStopped, 0, DragDecelerationDuration,
            DragSpeed, 0);
          Scroll := Scroll + CurrentDragSpeed * SecondsPassed;
          { stop inertial force if you reached the border of scroll }
          if CurrentDragSpeed > 0 then
          begin
            if Scroll = ScrollMax then TimeSinceDraggingStopped := DragDecelerationDuration;
          end else
          begin
            if Scroll = ScrollMin then TimeSinceDraggingStopped := DragDecelerationDuration;
          end;
        end;
      end;
      DragSinceLastUpdate := 0;
    end;
  end;

  { Update ScrollbarActive, TimeSinceDraggingStarted }
  procedure UpdateScrollBarActive;
  const
    AppearTime = 0.5;
    DisappearTime = 0.5;
  var
    NewScrollbarActive: Single;
  begin
    { update TimeSinceDragginStarted }
    if TimeSinceDraggingStopped = 0 then
    begin
      { dragging now }
      TimeSinceDraggingStarted := TimeSinceDraggingStarted + SecondsPassed;
      if TimeSinceDraggingStarted > AppearTime then
        NewScrollbarActive := 1 else
        NewScrollbarActive := TimeSinceDraggingStarted / AppearTime;
    end else
    begin
      { not dragging now }
      TimeSinceDraggingStarted := 0;
      if TimeSinceDraggingStopped > DisappearTime then
        NewScrollbarActive := 0 else
        NewScrollbarActive := 1 - TimeSinceDraggingStopped / DisappearTime;
    end;
    if ScrollbarActive <> NewScrollbarActive then
    begin
      ScrollbarActive := NewScrollbarActive;
      VisibleChange([chRender]);
    end;
  end;

begin
  inherited;
  UpdateScrollBarVisibleAndRects;
  HandleKeys;
  DraggingInertialForce;
  UpdateScrollBarActive;
end;

function TCastleScrollView.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  { if not ScrollBarVisible then there is no point in changing Scroll.

    This way we allow TStateDialog (that uses TDialog that uses TCastleScrollView)
    descendants like TStateDialogKey to handle
    K_PageDown, K_PageUp, K_Home and K_End keys and mouse wheel.
    And this is very good for MessageKey,
    when it's used e.g. to allow user to choose any TKey.
    Otherwise MessageKey would not be able to return
    K_PageDown, K_PageUp, etc. keys. }

  if ScrollBarVisible then
    case Event.EventType of
      itKey:
        case Event.Key of
          K_PageUp:   begin Scroll := Scroll - Height; Result := ExclusiveEvents; end;
          K_PageDown: begin Scroll := Scroll + Height; Result := ExclusiveEvents; end;
          K_Home:     begin Scroll := ScrollMin; Result := ExclusiveEvents; end;
          K_End:      begin Scroll := ScrollMax; Result := ExclusiveEvents; end;
        end;
      itMouseButton:
        begin
          if (Event.MouseButton = mbLeft) and ScrollBarVisible and
            ScrollbarFrame.Contains(Event.Position) then
          begin
            if Event.Position[1] < ScrollbarSlider.Bottom then
            begin
              Scroll := Scroll + Height;
              TimeSinceDraggingStopped := 0;
            end else
            if Event.Position[1] >= ScrollbarSlider.Top then
            begin
              Scroll := Scroll - Height;
              TimeSinceDraggingStopped := 0;
            end else
              ScrollBarDragging := true;
            Result := ExclusiveEvents;
          end;
        end;
      itMouseWheel:
        if Event.MouseWheelVertical then
        begin
          Scroll := Scroll - Event.MouseWheelScroll * WheelScrollSpeed;
          TimeSinceDraggingStopped := 0;
          Result := ExclusiveEvents;
        end;
    end;

  { We do not process mouse button clicks (or touches) here
    (not on an entire TCastleScrollView area -- the code above handles
    clicks inside ScrollbarFrame only).
    We only process dragging (with mbLeft) inside TCastleScrollView.Update
    and TCastleScrollView.Motion.

    But we still have to mark the mouse click as "handled" here.
    Otherwise some control underneath us (like TUIState with InterceptInput = true,
    like the one used by TStateDialogOK) could handle
    mouse and capture it (the TUIContainer automatically "captures" the control
    that returns "true" from Press event), and then we would not receive
    TCastleScrollView.Motion events anymore.

    (Testcase: show MessageOK or TStateDialogOK on mobile (Android, iOS)
    with a long text (to make scrolling relevant).)  }

  if EnableDragging and Event.IsMouseButton(mbLeft) and ScrollBarVisible then
    Result := ExclusiveEvents;
end;

function TCastleScrollView.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(mbLeft) then
  begin
    ScrollBarDragging := false;
    Result := ExclusiveEvents;
  end;
end;

function TCastleScrollView.Motion(const Event: TInputMotion): boolean;
var
  Drag: Single;
begin
  Result := inherited;
  if Result then Exit;

  if ScrollBarDragging then
  begin
    Scroll := Scroll + ((Event.OldPosition[1] - Event.Position[1]) / UIScale) /
      ScrollbarFrame.Height * ScrollArea.RenderRect.Height;
    TimeSinceDraggingStopped := 0;
    Result := ExclusiveEvents;
  end else
  if EnableDragging and (mbLeft in Event.Pressed) then
  begin
    Drag := ((Event.Position[1] - Event.OldPosition[1]) / UIScale);
    Scroll := Scroll + Drag;
    DragSinceLastUpdate := DragSinceLastUpdate + Drag;
    Result := ExclusiveEvents;
  end;
end;

function TCastleScrollView.ScrollMin: Single;
begin
  Result := 0;
end;

function TCastleScrollView.ScrollMax: Single;
begin
  Result := Max(0, ScrollArea.EffectiveHeight - EffectiveHeight);
end;

function TCastleScrollView.ScrollBarWidthScaled: Single;
begin
  Result := ScrollBarWidth * UIScale;
end;

{$endif read_implementation}
