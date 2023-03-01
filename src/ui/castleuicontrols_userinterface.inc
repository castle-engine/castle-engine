{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type

  { Basic user-interface class. All UI controls derive from this class,
    overriding chosen methods to react to some events.

    Every control can have children controls, see @link(Controls) and @link(ControlsCount).
    Parent control is recorded inside the @link(Parent). A control
    may only be a child of one other control --- that is, you cannot
    insert to the UI hierarchy the same control multiple times.

    Control may handle mouse/keyboard input, see @link(Press) and @link(Release)
    methods.

    Various methods return boolean saying if input event is handled.
    The idea is that not handled events are passed to the next
    control suitable. Handled events are generally not processed more
    --- otherwise the same event could be handled by more than one listener,
    which is bad. Generally, return ExclusiveEvents if anything (possibly)
    was done (you changed any field value etc.) as a result of this,
    and only return @false when you're absolutely sure that nothing was done
    by this control.

    Every control also has a position and size and takes some rectangular space
    on the container.

    The position is controlled using a @link(Translation) that works relative
    to the currently set anchors. You can change the position, adjusting
    also anchor, using an utility method @link(Anchor): one overloaded version
    of @link(Anchor) changes the horizontal position and anchors
    (@link(HorizontalAnchorSelf), @link(HorizontalAnchorParent), @link(Translation Translation.X)),
    the other overloaded version changes vertical position and anchors
    (@link(VerticalAnchorSelf), @link(VerticalAnchorParent), @link(Translation Translation.Y)).
    You can also change each individual property mentioned above.

    The size is controlled using the @link(Width), @link(WidthFraction),
    @link(Height), @link(HeightFraction), @link(FullSize), or @link(AutoSizeToChildren)
    properties.

    The size (before UI scaling is applied) can be queried using
    @link(EffectiveWidth) and @link(EffectiveHeight) methods.
    The rectangle where the control is visible (during rendering,
    after applying UI scaling) can be queried using @link(RenderRect)
    and @link(RenderRectWithBorder) methods.

    Note that some descendants perform auto-sizing,
    that is: their effective size follows some natural property of the control.
    For example, @link(TCastleLabel) size by default follows the @link(TCastleLabel.Caption)
    value, and ignores the explicit size set in @link(Width) and @link(Height).
    You can turn off auto-sizing by @link(TCastleLabel.AutoSize),
    to manually control the size using @link(Width) and @link(Height).

    All the coordinates passed here should follow the Castle Game Engine
    convention that (0, 0) is left-bottom window corner. }
  TCastleUserInterface = class(TCastleComponent)
  strict private
    type
      TEnumerator = record
      private
        FList: TInternalChildrenControls;
        FPosition: Integer;
        function GetCurrent: TCastleUserInterface; inline;
      public
        constructor Create(const AList: TInternalChildrenControls);
        function MoveNext: Boolean; inline;
        property Current: TCastleUserInterface read GetCurrent;
      end;
    var
      FOnVisibleChange: TCastleUserInterfaceChangeEvent;
      FContainer: TCastleContainer;
      FLastSeenUIScale: Single;
      FCursor: TMouseCursor;
      {$ifdef FPC}
      FOnCursorChange: TNotifyEvent;
      FHasHorizontalAnchor: boolean;
      FHasVerticalAnchor: boolean;
      {$endif}
      FExclusiveEvents: boolean;
      FOnUpdate: TUiUpdateEvent;
      FOnPress: TUiPressReleaseEvent;
      FOnRelease: TUiPressReleaseEvent;
      FOnMotion: TUiMotionEvent;
      FVisible: Boolean;
      FDisableContextOpenClose: Cardinal;
      FFocused: boolean;
      FGLInitialized: boolean;
      FExists: boolean;
      FControls: TInternalChildrenControls;
      FLeft: Single;
      FBottom: Single;
      FHorizontalAnchorSelf, FHorizontalAnchorParent: THorizontalPosition;
      FTranslation: TVector2;
      FVerticalAnchorSelf, FVerticalAnchorParent: TVerticalPosition;
      FEnableUIScaling: boolean;
      FKeepInFront, FCapturesEvents: boolean;
      FWidth, FHeight: Single;
      FWidthFraction, FHeightFraction: Single;
      FFullSize: boolean;
      FBorder: TBorder;
      FBorderColor: TCastleColor;
      FAutoSizeToChildren: Boolean;
      FAutoSizeToChildrenPaddingRight: Single;
      FAutoSizeToChildrenPaddingTop: Single;
      FSizeFromChildrenValid: Boolean;
      FSizeFromChildrenWidth, FSizeFromChildrenHeight: Single;
      FCachedRectWithoutAnchors: TFloatRectangle;
      FUseCachedRectWithoutAnchors: Cardinal; // <> 0 if we should use FCachedRectWithoutAnchors
      FInsideRectWithoutAnchors: Boolean;
      FCulling: Boolean;
      FClipChildren: Boolean;
      FOnRender, FOnInternalMouseEnter, FOnInternalMouseLeave: TUiNotifyEvent;
      FCustomTheme: TCastleTheme;
      FCustomThemeObserver: TFreeNotificationObserver;
      FTooltipControl: TCastleUserInterface;
      FTooltipControlObserver: TFreeNotificationObserver;

    procedure SerializeChildrenAdd(const C: TComponent);
    procedure SerializeChildrenClear;
    procedure SerializeChildrenEnumerate(const Proc: TGetChildProc);
    procedure BorderChange(Sender: TObject);
    procedure SetExists(const Value: boolean);
    function GetControls(const I: Integer): TCastleUserInterface;
    procedure SetControls(const I: Integer; const Item: TCastleUserInterface);
    procedure CreateControls;
    procedure SetAutoSizeToChildren(const Value: Boolean);
    procedure SetAutoSizeToChildrenPaddingRight(const Value: Single);
    procedure SetAutoSizeToChildrenPaddingTop(const Value: Single);

    { This takes care of some internal quirks with saving Left property
      correctly. (Because TComponent doesn't declare, but saves/loads a "magic"
      property named Left during streaming. This is used to place non-visual
      components on the form. Our Left is completely independent from this.) }
    procedure ReadRealLeft(Reader: TReader);
    procedure WriteRealLeft(Writer: TWriter);

    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);

    { Support for deprecated properties }
    procedure SetLeft(const Value: Single);
    procedure SetBottom(const Value: Single);
    function GetHorizontalAnchorDelta: Single;
    function GetVerticalAnchorDelta: Single;
    procedure SetHorizontalAnchorDelta(const Value: Single);
    procedure SetVerticalAnchorDelta(const Value: Single);

    procedure SetHorizontalAnchorSelf(const Value: THorizontalPosition);
    procedure SetHorizontalAnchorParent(const Value: THorizontalPosition);
    procedure SetVerticalAnchorSelf(const Value: TVerticalPosition);
    procedure SetVerticalAnchorParent(const Value: TVerticalPosition);
    procedure SetEnableUIScaling(const Value: boolean);
    procedure SetTranslation(const Value: TVector2);

    procedure SetFullSize(const Value: boolean);
    procedure SetBorderColor(const Value: TCastleColor);
    procedure SetWidthFraction(const Value: Single);
    procedure SetHeightFraction(const Value: Single);
    procedure SetCulling(const Value: Boolean);
    procedure SetClipChildren(const Value: Boolean);
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetCustomTheme(const Value: TCastleTheme);
    procedure CustomThemeFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetTooltipControl(const Value: TCastleUserInterface);
    procedure TooltipControlFreeNotification(const Sender: TFreeNotificationObserver);

    { Position and size of this control, assuming it exists,
      in the local coordinates (relative to the parent 2D control,
      although without anchors applied).

      The implementation in this class follows the algorithm
      documented at @link(EffectiveRect).

      @unorderedList(
        @item(@bold(This must ignore)
          the current value of the @link(Exists) method
          and @link(Exists) property, that is: the result of this function
          assumes that control does exist.)

        @item(@bold(This must ignore) the anchors. Their effect is applied
          outside of this method, in RectWithAnchors.)

        @item(@bold(This must take into account) UI scaling.
          This method must calculate a result already multiplied by @link(UIScale).
        )
      )

      @italic(Why float-based coordinates?)

      Using the float-based rectangles and coordinates is better than integer
      in case you use UI scaling (@link(TCastleContainer.UIScaling)).
      It may sound counter-intuitive, but calculating everything using
      floats results in more precision and better speed
      (because we avoid various rounding in the middle of calculations).
      Without UI scaling, it doesn't matter, use whichever ones are more
      comfortable.

      Using the float-based coordinates is also natural for animations. }
    function RectWithoutAnchors: TFloatRectangle;
    { Like @link(RectWithoutAnchors), but may be temporarily cached.
      Inside TCastleUserInterface implementation, always use this instead
      of calling RectWithoutAnchors. }
    function FastRectWithoutAnchors: TFloatRectangle;
    { Like @link(RectWithoutAnchors) but with anchors effect applied. }
    function RectWithAnchors(
      const CalculateEvenWithoutContainer: boolean = false): TFloatRectangle;
    { How to translate local coordinates to the container.
      Looks at our and parent's anchors (@link(Translation)) and positon (Left, Bottom).

      As such it is useful to modify the children UI control position.

      If you use UI scaling, this works in final coordinates
      (after scaling, real pixels on screen). }
    function LocalToContainerTranslation: TVector2;
    { Like LocalToContainerTranslation but also add additional shift because of our Border. }
    function LocalToContainerTranslationShiftBorder: TVector2;

    procedure SetCursor(const Value: TMouseCursor);
  private
    FParent: TCastleUserInterface;
    FLastSeenContainerWidth, FLastSeenContainerHeight: Integer;

    { Show Name and ClassName for debug/log purposes. }
    function DebugName: String;

    { Recalculate UIScale property value, call UIScaleChanged if needed. }
    procedure CheckUIScaleChanged;
    { Recalculate FLastSeenContainerWidth/Height, call Resize if needed. }
    procedure CheckResize;

    procedure RecursiveRender(const ViewportRect: TRectangle);
  protected
    procedure Loaded; override;

    { Show and hide UI used to render tooltip (may come from user TooltipControl or some internal component).
      @exclude }
    procedure InternalSetTooltipExists(const Value: Boolean); virtual;

    { Container sizes.
      @groupBegin }
    function ContainerWidth: Cardinal; virtual;
    function ContainerHeight: Cardinal; virtual;
    function ContainerRect: TRectangle; virtual;
    function ContainerSizeKnown: boolean; virtual;
    { @groupEnd }

    {$ifdef FPC}
    { Called when @link(Cursor) changed.
      In TCastleUserInterface class, just calls OnCursorChange. }
    procedure DoCursorChange; virtual;
      deprecated 'better override VisibleChange and watch for chCursor in Changes';
    {$endif}

    procedure DefineProperties(Filer: TFiler); override;

    procedure UIScaleChanged; virtual;

    //procedure DoCursorChange; override;

    { Controls that have a preferred size should override this.
      By default this contains values derived from
      @link(Width), @link(WidthFraction),
      @link(Height), @link(HeightFraction), with @link(Border) subtracted.

      Note that the arguments should be already scaled, i.e. multiplied by UIScale,
      i.e. expressed in final device pixels.

      Note that the returned PreferredWidth and PreferredHeight
      must not include the space for @link(Border). Border size will be
      added later. }
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); virtual;

    { Called right before calculating size.
      This is your only chance to adjust e.g. children size,
      if you need them to be synchronized with something,
      and you may use @link(AutoSizeToChildren).

      In most use-cases, you rather adjust preferred size by overriding
      @link(PreferredSize). }
    procedure BeforeSizing; virtual;

    procedure DoInternalMouseEnter; virtual;
    procedure DoInternalMouseLeave; virtual;

    { Theme that should be used by this control.
      Either CustomTheme or global @link(Theme). }
    function Theme: TCastleTheme;
  public
    const
      DefaultWidth = 100.0;
      DefaultHeight = 100.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function GetEnumerator: TEnumerator;
    procedure InternalSetContainer(const Value: TCastleContainer); virtual;

    (*Handle press or release of a key, mouse button or mouse wheel.
      Return @true if the event was somehow handled,
      which prevents from passing this event to other UI controls.

      When implementing in descendants it is best to override it like this:

      @longCode(#
      function TMyControl.Press(const Event: TInputPressRelease): boolean;
      begin
        Result := inherited;
        if Result then Exit; // exit if ancestor already handled this event

        if Event.IsKey(keyEnter) then
        begin
          // do something in reaction to Enter key
          ...
          // notify container that event was handled; ExclusiveEvents is true by default
          Exit(ExclusiveEvents);
        end;

        if Event.IsMouseButton(buttonLeft) then
        begin
          // do something in reaction to left mouse button press
          ...
          // notify container that event was handled; ExclusiveEvents is true by default
          Exit(ExclusiveEvents);
        end;
      end;
      #)

      These events are generated for all UI controls, whether they are considered
      "interactive" or not.
      These events are generated for non-interactive controls
      like @link(TCastleRectangleControl) or @link(TCastleLabel) as well.
      For example, these events ignore the @link(TCastleButton.Enabled) state,
      they are generated always (see https://github.com/castle-engine/castle-engine/issues/413 ).
      Use instead @link(TCastleButton.OnClick) to detect clicks on a button in a way that honors
      the @link(TCastleButton.Enabled) state.

      When a control returns @true from @link(Press), it means it starts to "capture"
      subsequent mouse events: subsequent mouse moves and release will be send
      to this control even if mouse will move outside of this control.

      The events PreviewPress and PreviewRelease are passed first to
      the parent control, before children have a chance to process this event.
      In partcular, overriding them makes sense if you draw something that "looks clickable"
      in @link(TCastleUserInterface.RenderOverChildren).

      The events Press and Release are passed to the parent only
      after the children had a chance to process this event.
      Overriding them makes sense if you draw something that "looks clickable"
      in @link(TCastleUserInterface.Render), which is the standard place you should draw stuff.
      For example our TCastleButton draws there.

      Note that releasing of the mouse wheel is not reported now by any backend.
      Only releasing of keys and mouse buttons is reported.

      @groupBegin *)
    function Press(const Event: TInputPressRelease): boolean; virtual;
    function Release(const Event: TInputPressRelease): boolean; virtual;
    function PreviewPress(const Event: TInputPressRelease): boolean; virtual;
    function PreviewRelease(const Event: TInputPressRelease): boolean; virtual;
    { @groupEnd }

    { Motion of mouse or touch. }
    function Motion(const Event: TInputMotion): boolean; virtual;

    { Rotation detected by 3D sensor.
      Used for example by 3Dconnexion devices.

      @param X   X axis (tilt forward/backwards)
      @param Y   Y axis (rotate)
      @param Z   Z axis (tilt sidewards)
      @param Angle   Angle of rotation (in radians)
      @param(SecondsPassed The time passed since last SensorRotation call.
        This is necessary because some sensors, e.g. 3Dconnexion,
        may *not* reported as often as normal @link(Update) calls.) }
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; virtual;

    { Translation detected by 3D sensor.
      Used for example by 3Dconnexion devices.

      @param X   X axis (move left/right)
      @param Y   Y axis (move up/down)
      @param Z   Z axis (move forward/backwards)
      @param Length   Length of the vector consisting of the above
      @param(SecondsPassed The time passed since last SensorRotation call.
        This is necessary because some sensors, e.g. 3Dconnexion,
        may *not* reported as often as normal @link(Update) calls.) }
    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; virtual;

    { Axis movement detected by joystick.

      @param JoyID ID of joystick with pressed button
      @param Axis Number of moved axis }
    function JoyAxisMove(const JoyID, Axis: Byte): boolean; virtual;
      deprecated 'use event Joysticks.OnAxisMove, or just observe Joysticks[0].Axis';

    { Joystick button pressed.

      @param JoyID ID of joystick with pressed button
      @param Button Number of pressed button }
    function JoyButtonPress(const JoyID, Button: Byte): boolean; virtual;
      deprecated 'use Joysticks.OnButtonDown or Joysticks.OnButtonPress';

    { Control may do here anything that must be continuously repeated.
      E.g. camera handles here falling down due to gravity,
      rotating model in Examine mode, and many more.

      @param(SecondsPassed Should be calculated like TFramesPerSecond.SecondsPassed,
        and usually it's in fact just taken from TCastleWindow.Fps.SecondsPassed.)

      This method may be used, among many other things, to continuously
      react to the fact that user pressed some key (or mouse button).
      For example, if holding some key should move some 3D object,
      you should do something like:

      @longCode(#
        if HandleInput then
        begin
          if Container.Pressed[keyArrowRight] then
            Transform.Position := Transform.Position + Vector3(SecondsPassed * 10, 0, 0);
          HandleInput := not ExclusiveEvents;
        end;
      #)

      Instead of directly using a key code, consider also
      using TInputShortcut that makes the input key nicely configurable.
      See engine tutorial about handling inputs.

      Multiplying movement by SecondsPassed makes your
      operation frame-rate independent. Object will move by 10
      units in a second, regardless of how many FPS your game has.

      The code related to HandleInput is important if you write
      a generally-useful control that should nicely cooperate with all other
      controls, even when placed on top of them or under them.
      The correct approach is to only look at pressed keys/mouse buttons
      if HandleInput is @true. Moreover, if you did check
      that HandleInput is @true, and you did actually handle some keys,
      then you have to set @code(HandleInput := not ExclusiveEvents).
      As ExclusiveEvents is @true in normal circumstances,
      this will prevent the other controls (behind the current control)
      from handling the keys (they will get HandleInput = @false).
      And this is important to avoid doubly-processing the same key press,
      e.g. if two controls react to the same key, only the one on top should
      process it.

      Note that to handle a single press / release (like "switch
      light on when pressing a key") you should rather
      use @link(Press) and @link(Release) methods. Use this method
      only for continuous handling (like "holding this key makes
      the light brighter and brighter").

      To understand why such HandleInput approach is needed,
      realize that the "Update" events are called
      differently than simple mouse and key events like "Press" and "Release".
      "Press" and "Release" events
      return whether the event was somehow "handled", and the container
      passes them only to the controls under the mouse (decided by
      @link(TCastleUserInterface.CapturesEventsAtPosition)). And as soon as some control says it "handled"
      the event, other controls (even if under the mouse) will not
      receive the event.

      This approach is not suitable for Update events. Some controls
      need to do the Update job all the time,
      regardless of whether the control is under the mouse and regardless
      of what other controls already did. So all controls (well,
      all controls that exist, in case of TCastleUserInterface,
      see TCastleUserInterface.Exists) receive Update calls.

      So the "handled" status is passed through HandleInput.
      If a control is not under the mouse, it will receive HandleInput
      = @false. If a control is under the mouse, it will receive HandleInput
      = @true as long as no other control on top of it didn't already
      change it to @false. }
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); virtual;

    { Called always when something important inside this control (or it's children)
      changed. To be more precise, this is called when something mentioned among
      the @link(TCastleUserInterfaceChange) enumerated items changed.

      This is always called with Changes <> [] (non-empty set). }
    procedure VisibleChange(const Changes: TCastleUserInterfaceChanges;
      const ChangeInitiatedByChildren: boolean = false); overload; virtual;

    { Called always when something important inside this control (or it's children)
      changed. See @link(VisibleChange) for details about when and how this is called.

      Be careful when handling this event. Various changes may cause this,
      so be prepared to handle it at any moment, even in the middle when UI control
      is changing. It may also occur very often.
      It's usually safest to only set a boolean flag like
      "something should be recalculated" when this event happens,
      and do the actual recalculation later. }
    property OnVisibleChange: TCastleUserInterfaceChangeEvent
      read FOnVisibleChange write FOnVisibleChange;

    { Allow window containing this control to suspend waiting for user input.
      Typically you want to override this to return @false when you do
      something in the overridden @link(Update) method.

      In this class, this simply returns always @true.

      @seeAlso TCastleWindow.AllowSuspendForInput }
    function AllowSuspendForInput: boolean; virtual;

    { Event called when the container (component or window with OpenGL context)
      size changes.

      You can resize/reposition your component here,
      for example set @link(TCastleUserInterface.Left) or @link(TCastleUserInterface.Bottom),
      to react to parent size changes.
      This is called only when the OpenGL context of the container
      is initialized, so you can be sure that this is called only between
      GLContextOpen and GLContextClose.

      We also call this once when inserting into the controls list
      (like @link(TCastleWindow.Controls) or
      @link(TCastleControl.Controls) or inside parent TCastleUserInterface),
      if inserting into the container/parent
      with already initialized OpenGL context. If inserting into the container/parent
      without OpenGL context initialized, it will be called later,
      when OpenGL context will get initialized, right after GLContextOpen.

      In other words, this is always called to let the control know
      the size of the container, if and only if the OpenGL context is
      initialized. }
    procedure Resize; virtual;

    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); virtual; deprecated 'use Resize';

    { Container of this control. When adding control to container's Controls
      list (like TCastleWindow.Controls) container will automatically
      set itself here, and when removing from container this will be changed
      back to @nil.

      May be @nil if this control is not yet inserted into any container. }
    property Container: TCastleContainer read FContainer;

    {$ifdef FPC}
    { Event called when the @link(Cursor) property changes.
      This event is, in normal circumstances, used by the Container,
      so you should not use it in your own programs. }
    property OnCursorChange: TNotifyEvent
      read FOnCursorChange write FOnCursorChange;
      deprecated 'use OnVisibleChange (or override VisibleChange) and watch for Changes that include chCursor';
    {$endif}

    { Design note: ExclusiveEvents is not published now, as it's too "obscure"
      (for normal usage you don't want to deal with it). Also, it's confusing
      on TCastleSceneCore, the name suggests it relates to ProcessEvents (VRML events,
      totally not related to this property that is concerned with handling
      TCastleUserInterface events.) }

    { Should we disable further mouse / keys handling for events that
      we already handled in this control. If @true, then our events will
      return @true for mouse and key events handled.

      This means that events will not be simultaneously handled by both this
      control and some other (or camera or normal window callbacks),
      which is usually more sensible, but sometimes somewhat limiting. }
    property ExclusiveEvents: boolean
      read FExclusiveEvents write FExclusiveEvents default true;

    property Controls [const Index: Integer]: TCastleUserInterface read GetControls write SetControls;
    function ControlsCount: Integer;

    { Add child control, at the front of other children. }
    procedure InsertFront(const NewItem: TCastleUserInterface); overload;
    procedure InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertFront(const NewItems: TCastleUserInterfaceList); overload;

    { Add child control, at the back of other children. }
    procedure InsertBack(const NewItem: TCastleUserInterface); overload;
    procedure InsertBackIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertBack(const NewItems: TCastleUserInterfaceList); overload;

    { Add child control at specified position.
      It is usually easier to use @link(InsertFront) or @link(InsertBack),
      if possible. }
    procedure InsertControl(const Index: Integer; const NewItem: TCastleUserInterface);

    { Remove control added by @link(InsertFront) or @link(InsertBack).

      Note that this method @italic(does not) free the Item instance.
      This method merely removes Item from our controls,
      so that Item is no longer rendered as part of this parent.
      It is still the caller's responsibility to make sure the Item is freed
      at some point.

      See https://castle-engine.io/modern_pascal_introduction.html#_freeing_classes
      for description how to free instances in Pascal.
      As TCastleUserInterface descends from TComponent, a simple and proper solution
      is often just to create it with some "owner" (like @link(TCastleView.FreeAtStop) or @link(Application))
      that will take care of freeing it. }
    procedure RemoveControl(const Item: TCastleUserInterface);

    { Index of child control, or -1 if not present. }
    function IndexOfControl(const Item: TCastleUserInterface): Integer;

    { Remove all child controls added by @link(InsertFront) or @link(InsertBack).

      Note that, just like @link(RemoveControl), this method @italic(does not) free
      the children controls. This method merely removes children controls,
      so they are no longer rendered as part of the parent.
      It is still the caller's responsibility to make sure the children controls
      are freed at some point.

      See https://castle-engine.io/modern_pascal_introduction.html#_freeing_classes
      for description how to free instances in Pascal.
      As TCastleUserInterface descends from TComponent, a simple and proper solution
      is often just to create it with some "owner" (like @link(TCastleView.FreeAtStop) or @link(Application))
      that will take care of freeing it. }
    procedure ClearControls;

    {$ifdef FPC}
    property GetExists: Boolean read FExists; deprecated 'use Exists';
    {$endif}

    { Does this control capture events under this container position.
      The default implementation simply checks whether Position
      is inside @link(RenderRect).
      It also checks whether @link(CapturesEvents) is @true.

      Always treated like @false when Exists returns @false,
      so the implementation of this method only needs to make checks assuming that
      Exists = @true.  }
    function CapturesEventsAtPosition(const Position: TVector2): boolean; virtual;

    { Prepare your resources, right before drawing.
      Called only when @link(Exists) and GLInitialized.

      @bold(Do not explicitly call this method.)
      Instead, render controls by adding them to the
      @link(TCastleContainer.Controls) list, or render them explicitly
      (for off-screen rendering) by @link(TCastleContainer.RenderControl).
    }
    procedure BeforeRender; virtual;

    { Render a control. Called only when @link(Exists) and GLInitialized,
      you can depend on it in the implementation of this method.

      @bold(Do not explicitly call this method.)
      Instead, render controls by adding them to the
      @link(TCastleContainer.Controls) list, or render them explicitly
      (for off-screen rendering) by @link(TCastleContainer.RenderControl).
      This is method should only be overridden in your own code.

      Before calling this method we always set some OpenGL state,
      and you can depend on it (and you can carelessly change it,
      as it will be reset again before rendering other control).

      OpenGL state always set:

      @unorderedList(
        @item(Viewport is set to include whole container.)

        @item(Depth test is off.)

        @item(@italic((For fixed-function pipeline:))
          The 2D orthographic projection is always set at the beginning.
          Useful for 2D controls, 3D controls can just override projection
          matrix, e.g. use @link(CastleRenderContext.PerspectiveProjection).)

        @item(@italic((For fixed-function pipeline:))
          The modelview matrix is set to identity. The matrix mode
          is always modelview.)

        @item(@italic((For fixed-function pipeline:))
          The raster position is set to (0,0).
          The (deprecated) WindowPos is also set to (0,0).)

        @item(@italic((For fixed-function pipeline:))
          Texturing, lighting, fog is off.)
      )

      Beware that GLSL @link(TRenderContext.CurrentProgram RenderContext.CurrentProgram) has undefined value when this is called.
      You should always set it, before making direct OpenGL drawing calls
      (all the engine drawing routines of course do it already, this is only a concern
      if you make direct OpenGL / OpenGLES calls). }
    procedure Render; virtual;

    { Render a control contents @italic(over the children controls).
      This is analogous to @link(Render), but it executes after children are drawn.
      You should usually prefer to override @link(Render) instead of this method,
      as the convention is that the parent is underneath children. }
    procedure RenderOverChildren; virtual;

    { Render a tooltip of this control. If you want to have tooltip for
      this control detected, you have to override TooltipExists.
      Then the TooltipRender will be called.

      TooltipRender is called in the same way as @link(Render).
      TooltipRender is always called as a last (front-most) control.
      Argument TooltipPosition is the left-bottom (in screen space, regardless of UIScaling)
      suggested position of the tooltip.

      It is simplest to descend from TCastleUserInterfaceFont,
      that implements simple @link(TCastleUserInterfaceFont.Tooltip) property
      and overrides these two methods as necessary.

      @groupBegin }
    function TooltipExists: Boolean; virtual; deprecated 'use TooltipControl to customize tooltip look';
    procedure TooltipRender(const TooltipPosition: TVector2); virtual; deprecated 'use TooltipControl to customize tooltip look';
    { @groupEnd }

    { Initialize your OpenGL resources.

      This is called when OpenGL context of the container is created,
      or when the control is added to the already existing context.
      In other words, this is the moment when you can initialize
      OpenGL resources, like display lists, VBOs, OpenGL texture names, etc.

      As an exception, this is called regardless of the Exists value.
      This way a control can prepare it's resources, regardless if it exists now. }
    procedure GLContextOpen; virtual;

    { Destroy your OpenGL resources.

      Called when OpenGL context of the container is destroyed.
      Also called when controls is removed from the container
      @code(Controls) list. Also called from the destructor.

      You should release here any resources that are tied to the
      OpenGL context. In particular, the ones created in GLContextOpen.

      As an exception, this is called regardless of the Exists value.
      This way a control can release it's resources, regardless if it exists now. }
    procedure GLContextClose; virtual;

    property GLInitialized: boolean read FGLInitialized default false;

    { @exclude
      This is dirty internal hack, and will be removed one day.
      Please don't use this.

      When non-zero, control will not receive GLContextOpen and
      GLContextClose events when it is added/removed from the
      @link(TCastleContainer.Controls) list.

      This can be useful as an optimization, to keep the OpenGL resources
      created even for controls that are not present on the
      @link(TCastleContainer.Controls) list. @italic(This must used
      very, very carefully), as bad things will happen if the actual OpenGL
      context will be destroyed while the control keeps the OpenGL resources
      (because it had DisableContextOpenClose > 0). The control will then
      remain having incorrect OpenGL resource handles, and will try to use them,
      causing OpenGL errors or at least weird display artifacts.

      Most of the time, when you think of using this, you should instead
      use the @link(TCastleUserInterface.Exists) property. This allows you to keep the control
      of the @link(TCastleContainer.Controls) list, and it will be receive
      GLContextOpen and GLContextClose events as usual, but will not exist
      for all other purposes.

      Using this mechanism is only sensible if you want to reliably hide a control,
      but also allow readding it to the @link(TCastleContainer.Controls) list,
      and then you want to show it again. This is useful for CastleInternalWindowModes,
      that must push (and then pop) the controls, but then allows the caller
      to modify the controls list. And some games, e.g. castle1, add back
      some (but not all) of the just-hidden controls. For example the TCastleNotifications
      instance is added back, to be visible even in the menu mode.
      This means that CastleInternalWindowModes cannot just modify the TCastleContainer.Exists
      value, leaving the control on the @link(TCastleContainer.Controls) list:
      it would leave the TCastleUserInterface existing many times on the @link(TCastleContainer.Controls)
      list, with the undefined TCastleContainer.Exists value. }
    property DisableContextOpenClose: Cardinal
      read FDisableContextOpenClose write FDisableContextOpenClose;

   { Called when this control becomes or stops being focused,
     that is: under the mouse cursor and will receive events.

     This updates Focused property.
     This also calls OnInternalMouseEnter / OnInternalMouseLeave. }
    procedure SetFocused(const Value: boolean); virtual;

    { See @link(SetFocused). }
    property Focused: boolean read FFocused write SetFocused;

    { Visual parent control. This control is rendered within the parent,
      and it's anchors and coordinates are relative to the parent.
      Parent may be @nil, which means that the @link(Container)
      (if set) is our direct parent. }
    property Parent: TCastleUserInterface read FParent;

    { Control position and size.
      Use this to base other controls size/position on this control's
      calculated size/position.

      The algorithm that determines control position and size is like this:

      @unorderedList(
        @item(
          When @link(AutoSizeToChildren) is @true:

          The control size is adjusted to children, to surround them.
          @link(Left) and @link(Bottom) and our anchors still matter,
          but our @link(Width) and @link(Height) do not matter.
          See the @link(AutoSizeToChildren) documentation for details.
        )

        @item(
          Otherwise, when @link(FullSize) is @true:

          The control always fills the whole parent.
          If the control is added directly to the container,
          it will fill the whole container (TCastleWindow or TCastleControl).
        )
        @item(
          Otherwise (when @link(FullSize) and @link(AutoSizeToChildren)
          are both @false):

          The position and size of the control is determined by
          the @link(Left), @link(Bottom), @link(Width), @link(Height) properties.

          Moreover, @link(WidthFraction), if non-zero, specifies the control width
          as a fraction of the parent width. E.g. WidthFraction = 1.0 means that width
          equals parent, 0.5 means it's half the width of the parent etc.
          In this case the explicit @link(Width) is ignored.

          Similarly you can use @link(HeightFraction) to express height as a fraction of parent.
          When @link(HeightFraction) is not zero, then
          the explicit @link(Height) is ignored.

          If at least one of the resulting sizes is zero, the rectangle is empty
          (it is @link(TFloatRectangle.Empty)).
        )
      )

      The above describes the size of the @italic(content and border, summed).
      The content itself may be a little smaller, when the @link(Border) is
      used. The size of the "content itself" is returned
      only by @code(RenderRect) (in already-scaled coordinates,
      i.e. using final device pixels).

      The anchors (see @link(Anchor)) also affect the final position of the control.

      Note that some descendants do @italic(auto-sizing) by default.
      This means that some of these properties may be ignored,
      and instead the calculated size (EffectiveWidth, EffectiveHeight)
      depends on some core values of the control.

      For example:

      @unorderedList(
        @item TCastleImageControl adjusts to image (unless you set @link(TCastleImageControl.Stretch) to @true),
        @item TCastleLabel adjusts to caption (unless you set @link(TCastleLabel.AutoSize) to @false),
        @item TCastleButton adjusts to caption and icon (unless you set @link(TCastleButton.AutoSize) to @false),
        @item TCastleVerticalGroup adjusts to children sizes,
        @item ... and so on.
      )

      Consult the documentation of each descendant for the exact
      specification of the size behavior.

      This method returns the rectangle in local coordinates
      (relative to the parent 2D control).
      The returned rectangle size includes the border size.
      The returned rectangle size uses "unscaled" coordinates,
      which means that they correspond to the sizes you set e.g. in
      @link(TCastleUserInterface.Width),
      @link(TCastleUserInterface.Height), and translation units you use with
      e.g. @link(TCastleUserInterface.Anchor).

      If you're looking for a way to query the size of the control
      in final scaled coordinates (in real device pixels), and not relative
      to the parent position, use @link(RenderRect)
      or @link(RenderRectWithBorder) instead.

      This method ignores the current value of the @link(Exists) method
      and @link(Exists) property, that is: the result of this function
      assumes that control does exist.

      @seealso TCastleUserInterface.EffectiveWidth
      @seealso TCastleUserInterface.EffectiveHeight
    }
    function EffectiveRect: TFloatRectangle;

    { Calculated width of the control, without UI scaling.
      Useful if you want to base other controls size/position on this control
      calculated size.

      Unlike the @link(Width) property,
      this is the @italic(effective) size, not the desired size.
      So this is already processed by any auto-sizing mechanism
      (e.g. TCastleImageControl may adjust it's own size to the underlying image,
      depending on settings), it is changed by @link(FullSize) and so on.

      It is always equal to just @code(EffectiveRect.Width).
      It is 0 when EffectiveRect is empty.

      @seealso EffectiveRect }
    function EffectiveWidth: Single;

    { Calculated height of the control, without UI scaling.
      Useful if you want to base other controls size/position on this control
      calculated size.

      Unlike the @link(Height) property,
      this is the @italic(effective) size, not the desired size.
      So this is already processed by any auto-sizing mechanism
      (e.g. TCastleImageControl may adjust it's own size to the underlying image,
      depending on settings), it is changed by @link(FullSize) and so on.

      It is always equal to just @code(EffectiveRect.Height).
      It is 0 when EffectiveRect is empty.

      @seealso EffectiveRect }
    function EffectiveHeight: Single;

    { @link(EffectiveWidth) without @link(Border) size. }
    function EffectiveWidthForChildren: Single;
    { @link(EffectiveHeight) without @link(Border) size. }
    function EffectiveHeightForChildren: Single;

    function CalculatedWidth: Cardinal; deprecated 'use EffectiveWidth';
    function CalculatedHeight: Cardinal; deprecated 'use EffectiveHeight';
    function CalculatedRect: TRectangle; deprecated 'use EffectiveRect';

    { Position and size of this control, assuming it exists, in container
      coordinates (in final device pixels).
      The primary use of this is inside @link(Render).
      A proper UI control should adjust to draw precisely in this rectangle. }
    function RenderRect: TFloatRectangle;
    function RenderRectWithBorder: TFloatRectangle;

    function ScreenRect: TRectangle; deprecated 'use RenderRect';

    { Convert position relative to container (in final device coordinates, without UI scaling, by default)
      into position relative to this UI control (in coordinates with UI scaling).
      Useful e.g. to convert mouse/touch position from
      @link(TInputPressRelease.Position) into position useful for children
      of this UI control.

      The exact definition is that using the result of this to set our child
      @link(Translation), assuming the child is anchored to the left-bottom (the default state)
      and child has Left = Bottom = 0, sets child position exactly to
      the indicated point on the container.

      @param(ContainerPositionScaled
        If @true (default) then the container position (the ContainerPosition parameter)
        is assumed to be in final device pixels,
        i.e. the window area in X is from 0 to @link(TCastleContainer.Width),
        in Y from 0 and @link(TCastleContainer.Height).
        This is useful when the parameter comes e.g. from @link(TInputPressRelease.Position).

        If @false then the container position (the ContainerPosition parameter)
        is assumed to be in unscaled device pixels,
        i.e. the window area in X is from 0 to @link(TCastleContainer.UnscaledWidth),
        in Y from 0 and @link(TCastleContainer.UnscaledHeight).
      )
    }
    function ContainerToLocalPosition(ContainerPosition: TVector2;
      const ContainerPositionScaled: Boolean = true): TVector2;

    { Convert position relative to this UI control (in coordinates with UI scaling)
      into relative to container (in final device coordinates, without UI scaling, by default).
      Reverses @link(ContainerToLocalPosition).

      @param(ContainerPositionScaled
        If @true (default) then the container position (the result of this method)
        is assumed to be in final device pixels,
        i.e. the window area in X is from 0 to @link(TCastleContainer.Width),
        in Y from 0 and @link(TCastleContainer.Height).

        If @false then the container position (the result of this method)
        is assumed to be in unscaled device pixels,
        i.e. the window area in X is from 0 to @link(TCastleContainer.UnscaledWidth),
        in Y from 0 and @link(TCastleContainer.UnscaledHeight).
      )
    }
    function LocalToContainerPosition(const LocalPosition: TVector2;
      const ContainerPositionScaled: Boolean = true): TVector2;

    { Rectangle filling the parent control (or container), in local coordinates.
      Since this is in local coordinates, the returned rectangle Left and Bottom
      are always zero, unless parent has Border (the returned rectangle
      is shrunk by Parent.Border).
      This is already scaled by UI scaling. }
    function ParentRect: TFloatRectangle;

    { Adjust position to align us with the parent horizontally.
      The resulting @link(EffectiveRect) and @link(RenderRect) and @link(RenderRectWithBorder)
      will immediately reflect the new position.

      Note that position set here (anchors, @link(Translation))
      are ignored when @link(FullSize) is set to true.
      In case of @link(FullSize), the control fills the parent perfectly.

      This utility method sets @link(HorizontalAnchorSelf), @link(HorizontalAnchorParent),
      @link(Translation Translation.X). }
    procedure Anchor(const AHorizontalAnchor: THorizontalPosition;
      const TranslationX: Single = 0); overload;

    { Adjust position to align us with the parent horizontally.
      The resulting @link(EffectiveRect) and @link(RenderRect) and @link(RenderRectWithBorder)
      will immediately reflect the new position.

      Note that position set here (anchors, @link(Translation))
      are ignored when @link(FullSize) is set to true.
      In case of @link(FullSize), the control fills the parent perfectly.

      This utility method sets @link(HorizontalAnchorSelf), @link(HorizontalAnchorParent),
      @link(Translation Translation.X). }
    procedure Anchor(
      const AHorizontalAnchorSelf, AHorizontalAnchorParent: THorizontalPosition;
      const TranslationX: Single = 0); overload;

    { Adjust position to align us to the parent vertically.
      The resulting @link(EffectiveRect) and @link(RenderRect) and @link(RenderRectWithBorder)
      will immediately reflect the new position.

      Note that position set here (anchors, @link(Translation))
      are ignored when @link(FullSize) is set to true.
      In case of @link(FullSize), the control fills the parent perfectly.

      This utility method sets @link(VerticalAnchorSelf), @link(VerticalAnchorParent),
      @link(Translation Translation.Y). }
    procedure Anchor(const AVerticalAnchor: TVerticalPosition;
      const TranslationY: Single = 0); overload;

    { Adjust position to align us to the parent vertically.
      The resulting @link(EffectiveRect) and @link(RenderRect) and @link(RenderRectWithBorder)
      will immediately reflect the new position.

      Note that position set here (anchors, @link(Translation))
      are ignored when @link(FullSize) is set to true.
      In case of @link(FullSize), the control fills the parent perfectly.

      This utility method sets @link(VerticalAnchorSelf), @link(VerticalAnchorParent),
      @link(Translation Translation.Y). }
    procedure Anchor(
      const AVerticalAnchorSelf, AVerticalAnchorParent: TVerticalPosition;
      const TranslationY: Single = 0); overload;

    {$warnings off} // using deprecated TPositionRelative in deprecated

    { Immediately position the control with respect to the parent
      by adjusting @link(Left).
      Deprecated, use @link(Align) with THorizontalPosition. }
    procedure AlignHorizontal(
      const ControlPosition: TPositionRelative = prMiddle;
      const ContainerPosition: TPositionRelative = prMiddle;
      const X: Single = 0); deprecated 'use Anchor';

    { Immediately position the control with respect to the parent
      by adjusting @link(Left).

      Note that using @link(Anchor) is often more comfortable than this method,
      since you only need to set anchor once (for example, right after creating
      the control). In contract, adjusting position using this method
      will typically need to be repeated at each window on resize,
      like in @link(TCastleWindow.OnResize). }
    procedure Align(
      const ControlPosition: THorizontalPosition;
      const ContainerPosition: THorizontalPosition;
      const X: Single = 0); overload; deprecated 'use Anchor';

    { Immediately position the control with respect to the parent
      by adjusting @link(Bottom).
      Deprecated, use @link(Align) with TVerticalPosition. }
    procedure AlignVertical(
      const ControlPosition: TPositionRelative = prMiddle;
      const ContainerPosition: TPositionRelative = prMiddle;
      const Y: Single = 0); deprecated 'use Anchor';

    { Immediately position the control with respect to the parent
      by adjusting @link(Bottom).

      Note that using @link(Anchor) is often more comfortable than this method,
      since you only need to set anchor once (for example, right after creating
      the control). In contract, adjusting position using this method
      will typically need to be repeated at each window on resize,
      like in @link(TCastleWindow.OnResize). }
    procedure Align(
      const ControlPosition: TVerticalPosition;
      const ContainerPosition: TVerticalPosition;
      const Y: Single = 0); overload; deprecated 'use Anchor';

    {$warnings on}

    { Immediately center the control within the parent,
      both horizontally and vertically.

      Note that using @link(Anchor) is often more comfortable than this method,
      since you only need to set anchor once. For example, right after creating
      the control call @code(Anchor(hpMiddle); Anchor(vpMiddle);).
      In contrast, adjusting position using this method
      will typically need to be repeated at each window on resize,
      like in @link(TCastleWindow.OnResize). }
    procedure Center; deprecated 'use Anchor(hpMiddle); Anchor(vpMiddle); to reliably center the control';

    { UI scale of this control, derived from container
      (see @link(TCastleContainer.UIScaling) and @link(EnableUIScaling)). }
    function UIScale: Single; virtual;

    { Override this to prevent resizing some dimension in CGE editor. }
    procedure EditorAllowResize(out ResizeWidth, ResizeHeight: Boolean;
      out Reason: String); virtual;

    {$ifdef FPC}
    property FloatWidth: Single read FWidth write SetWidth stored false;
      deprecated 'use Width';
    property FloatHeight: Single read FHeight write SetHeight stored false;
      deprecated 'use Height';
    {$endif}

    { Position of the control, relative to the anchor at parent
      (by default, relative to left-bottom corner of the parent). }
    property AnchorDelta: TVector2 read FTranslation write SetTranslation;
      {$ifdef FPC}deprecated 'use Translation';{$endif}

    { Position of the control, relative to the anchor at parent
      (by default, relative to left-bottom corner of the parent). }
    property Translation: TVector2 read FTranslation write SetTranslation;

    { Keep the control in front of other controls (with KeepInFront=@false)
      when inserting.

      TODO: Do not change this property while the control is already
      a child of something. }
    property KeepInFront: boolean read FKeepInFront write FKeepInFront
      default false;

    { Color of the @link(Border), by default completely transparent black. }
    property BorderColor: TCastleColor read FBorderColor write SetBorderColor;

    {$ifdef FPC}
    property HasHorizontalAnchor: boolean
      read FHasHorizontalAnchor write FHasHorizontalAnchor stored false;
      deprecated 'this property does not do anything anymore, anchors are always active';
    property HasVerticalAnchor: boolean
      read FHasVerticalAnchor write FHasVerticalAnchor stored false;
      deprecated 'this property does not do anything anymore, anchors are always active';
    {$endif}

    { Delta between our border and parent.
      @seealso Anchor }
    property HorizontalAnchorDelta: Single
      read GetHorizontalAnchorDelta write SetHorizontalAnchorDelta {$ifdef FPC}default 0{$endif};
      {$ifdef FPC}deprecated 'use Translation.X';{$endif}
    { Delta between our border and parent.
      @seealso Anchor }
    property VerticalAnchorDelta: Single
      read GetVerticalAnchorDelta write SetVerticalAnchorDelta {$ifdef FPC}default 0{$endif};
      {$ifdef FPC}deprecated 'use Translation.Y';{$endif}

    { Is the control possibly visible.
      This is always @true when @link(Culling) is @false (the default). }
    property Visible: Boolean read FVisible;

    { Enable or disable UI scaling for this particular control.
      See more about UI scaling on @link(TCastleContainer.UIScaling) and
      @link(TCastleUserInterface.UIScale). Setting this to @false forces
      @link(TCastleUserInterface.UIScale) to always return 1.0.

      Note that this does not work recursively, i.e. it does not affect
      the children of this control. Setting this to @false does not prevent
      UI scaling on children (you have to turn it off explicitly for children too,
      if you need to disable UI scaling recursively).

      @italic(The use-cases for changing this property to "false" are very rare.
      Usually you should use UI scaling for 100% of your UI.) }
    property EnableUIScaling: boolean
      read FEnableUIScaling write SetEnableUIScaling default true;

    { Capture input events (keyboard, mouse, joystick).
      If @false, then the methods like @link(Press) and @link(Release) will never be called,
      and @link(Update) will always be called with HandleInput = @false.
      The control will never behave like focused.

      The only exception is when this control is set as @link(TCastleContainer.ForceCaptureInput).
      In this case, the control will receive inputs.
      In other words, @link(TCastleContainer.ForceCaptureInput) overrides
      the intent of this property.

      @italic(This property may be replaced by something like
      "CaptureInput" or just universal "Enabled" in the future.) }
    property CapturesEvents: boolean read FCapturesEvents write FCapturesEvents
      default true;

    { Called when control starts being under the mouse cursor and will receive events.
      See @link(SetFocused), this is called when @link(Focused) changes from @false to @true.

      This is called "Internal" now, because we do not guarantee it's 100%
      always paired with @link(OnInternalMouseLeave). A different approach to this may
      be done in the future releases. }
    property OnInternalMouseEnter: TUiNotifyEvent
      read FOnInternalMouseEnter write FOnInternalMouseEnter;

    { Called when control stops being under the mouse cursor and will receive events.
      See @link(SetFocused), this is called when @link(Focused) changes from @true to @false.

      This is called "Internal" now, because we do not guarantee it's 100%
      always paired with @link(OnInternalMouseEnter). A different approach to this may
      be done in the future releases. }
    property OnInternalMouseLeave: TUiNotifyEvent
      read FOnInternalMouseLeave write FOnInternalMouseLeave;

    { Is the @link(FullSize) actually active.
      Checks @link(FullSize) and not @link(AutoSizeToChildren),
      i.e. it accounts that AutoSizeToChildren overrides FullSize
      (since they cannot both work at the same time). }
    function EffectiveFullSize: Boolean;
  published
    { Mouse cursor over this control.
      When user moves mouse over the Container, the currently focused
      (topmost under the cursor) control determines the mouse cursor look. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Event that occurs continuously on each control.
      See @link(Update) for details. }
    property OnUpdate: TUiUpdateEvent read FOnUpdate write FOnUpdate;

    { An input (key, mouse button, mouse wheel) was pressed.
      See @link(Press) for details. }
    property OnPress: TUiPressReleaseEvent read FOnPress write FOnPress;

    { An input (key, mouse button, mouse wheel) was released.
      See @link(Release) for details. }
    property OnRelease: TUiPressReleaseEvent read FOnRelease write FOnRelease;

    { Pointer (mouse or finger) moved.
      See @link(Motion) for details. }
    property OnMotion: TUiMotionEvent read FOnMotion write FOnMotion;

    { Control is being displayed.
      See @link(Render) for details.
      This event it called @italic(after) the @link(Render) method
      of this control finished. }
    property OnRender: TUiNotifyEvent read FOnRender write FOnRender;

    { Not existing control is not visible, it doesn't receive input
      and generally doesn't exist from the point of view of user.
      This is mostly equivalent to just removing this control from parent list.

      Non-existing controls still receive
      @link(GLContextOpen), @link(GLContextClose), @link(Resize) events. }
    property Exists: boolean read FExists write SetExists default true;

    { Position from the left side of the parent control.

      It's usually better to use anchors instead of this property.
      For example, instead of setting @code(Control.Left := 10),
      you can call @code(Control.Anchor(hpLeft, 10)),
      or just change @link(Translation Translation.X).

      Note that the effects of this property and @link(Translation Translation.X)
      are summed, if you set both.
      We advise to leave @link(Left) as zero and just use anchors in new code. }
    property Left: Single read FLeft write SetLeft stored false {$ifdef FPC}default 0{$endif};
      {$ifdef FPC}deprecated 'use Translation.X';{$endif}

    { Position from the bottom side of the parent control.

      It's usually better to use anchors instead of this property.
      For example, instead of setting @code(Control.Bottom := 10),
      you can call @code(Control.Anchor(vpBottom, 10)),
      or just change @link(Translation Translation.Y).

      Note that the effects of this property and @link(Translation Translation.Y)
      are summed, if you set both.
      We advise to leave @link(Bottom) as zero and just use anchors in new code. }
    property Bottom: Single read FBottom write SetBottom {$ifdef FPC}default 0{$endif};
      {$ifdef FPC}deprecated 'use Translation.Y';{$endif}

    { When @name, the control will always fill the whole parent area.
      @seealso TCastleUserInterface.EffectiveRect
      @seealso TCastleUserInterface.EffectiveWidth
      @seealso TCastleUserInterface.EffectiveHeight }
    property FullSize: boolean read FFullSize write SetFullSize default false;

    { These properties determine the control size.
      See the @link(EffectiveRect) documentation for details how the size
      is calculated.

      @seealso TCastleUserInterface.EffectiveRect
      @seealso TCastleUserInterface.EffectiveWidth
      @seealso TCastleUserInterface.EffectiveHeight
      @groupBegin }
    property Width: Single read FWidth write SetWidth {$ifdef FPC}default DefaultWidth{$endif};
    property Height: Single read FHeight write SetHeight {$ifdef FPC}default DefaultHeight{$endif};
    property WidthFraction: Single read FWidthFraction write SetWidthFraction {$ifdef FPC}default 0.0{$endif};
    property HeightFraction: Single read FHeightFraction write SetHeightFraction {$ifdef FPC}default 0.0{$endif};
    { @groupEnd }

    { Adjust size to encompass all the children.
      The properties @link(FullSize), @link(Width), @link(Height)
      are ignored in this case.
      Our @link(Left) and @link(Bottom) still matter.

      Our size is adjusted to all existing children sizes and positions.

      We add @link(AutoSizeToChildrenPaddingTop) to the resulting height,
      and @link(AutoSizeToChildrenPaddingRight) to the resulting width.
    }
    property AutoSizeToChildren: Boolean
      read FAutoSizeToChildren write SetAutoSizeToChildren default false;

    { Padding added when @link(AutoSizeToChildren) is used.
      TODO: Should be AutoSizeToChildrenPaddingHorizontal, there's nothing that makes it specific
      to "right" or "left" side now. }
    property AutoSizeToChildrenPaddingRight: Single
      read FAutoSizeToChildrenPaddingRight
      write SetAutoSizeToChildrenPaddingRight {$ifdef FPC}default 0{$endif};
    { Padding added when @link(AutoSizeToChildren) is used.
      TODO: Should be AutoSizeToChildrenPaddingVertical, there's nothing that makes it specific
      to "top" or "bottom" side now. }
    property AutoSizeToChildrenPaddingTop: Single
      read FAutoSizeToChildrenPaddingTop
      write SetAutoSizeToChildrenPaddingTop {$ifdef FPC}default 0{$endif};

    { Which @bold(our) border to align with parent.
      @seealso Anchor }
    property HorizontalAnchorSelf: THorizontalPosition
      read FHorizontalAnchorSelf write SetHorizontalAnchorSelf default hpLeft;
    { Which @bold(parent) border is aligned to our @link(HorizontalAnchorSelf) border.
      @seealso Anchor }
    property HorizontalAnchorParent: THorizontalPosition
      read FHorizontalAnchorParent write SetHorizontalAnchorParent default hpLeft;

    { Which @bold(our) border to align with parent.
      @seealso Anchor }
    property VerticalAnchorSelf: TVerticalPosition
      read FVerticalAnchorSelf write SetVerticalAnchorSelf default vpBottom;
    { Which @bold(parent) border is aligned to our @link(VerticalAnchorSelf) border.
      @seealso Anchor }
    property VerticalAnchorParent: TVerticalPosition
      read FVerticalAnchorParent write SetVerticalAnchorParent default vpBottom;

    { Optimize rendering and event processing
      by checking whether the control can be visible.
      The visibility is checked by looking at container rectangle,
      and all possible clipping parents
      (set by @link(TCastleScrollView), or any other control with
      @link(ClipChildren)).

      This is useful for UI controls that have expensive rendering or other events
      (e.g. they do something non-trivial in @link(Render) or @link(RenderOverChildren),
      or they include a lot of children controls).
      And they may be placed off-screen,
      or they may be outside of a parent with clipping,
      which is often the case when the parent is @link(TCastleScrollView). }
    property Culling: Boolean read FCulling write SetCulling default false;

    { Clip the rendering of children.

      By default this is @false and so the control
      can draw outside of it's designated rectangle (@link(RenderRect)).
      Although all the standard UI controls are carefully implemented
      such that their @link(Render) and @link(RenderOverChildren)
      draw only within their @link(RenderRect).
      But it is easy to place children controls outside of this rectangle.

      When this property is @true, the rendering is clipped,
      so the "overflowing" parts are never visible.
      This affects both @link(Render) and @link(RenderOverChildren)
      of this control, as well as all children rendering. }
    property ClipChildren: Boolean read FClipChildren write SetClipChildren default false;

    { Border (by default transparent) of the control.
      Border adds a space from the control content
      (drawn within @link(RenderRect)) to the control rectangle
      (returned by @link(RenderRectWithBorder)) (scaled), or @link(EffectiveRect)
      (unscaled) and friends).

      It is transparent by default, but you can change it by @link(BorderColor).

      Border works always, regardless of @link(FullSize) or @link(AutoSizeToChildren).

      One obvious use-case is to use this for visual space
      (empty space or a colorful frame, separating this control from the rest).

      Another use-case is to reserve a predictable space
      for as additional (sibling) control within the same parent.
      E.g. you can set FullSize=true and Border.Top=100,
      and this way there is always a strip with Height=100 at the top
      of the parent, where you can insert another control (with Height=100,
      anchored to the top). }
    property Border: TBorder read FBorder;

    { Use a custom instance of TCastleTheme to determine the look of this control. }
    property CustomTheme: TCastleTheme read FCustomTheme write SetCustomTheme;

    { If set, the indicated control will act as a tooltip:
      we will make it existing when we should show the tooltip for this control,
      we will make it no longer existing when the tooltip for this should be hidden.
      Initially the TooltipControl should usually have @link(Exists)=false,
      because you usually do not want to show it until we start showing it as a tooltip.

      If TooltipControl is assigned then TCastleUserInterfaceFont
      no longer does tooltip rendering.
      The @link(TCastleUserInterfaceFont.Tooltip) text is ignored.

      The visual parent of TooltipControl can be whatever is comfortable.
      That is, it is OK to make TooltipControl parent of this control,
      and it is OK not to: you can as well place TooltipControl as e.g. parent of container
      or design root. }
    property TooltipControl: TCastleUserInterface read FTooltipControl write SetTooltipControl;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleuserinterface_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleUserInterfaceClass = class of TCastleUserInterface;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleUserInterface.TEnumerator ------------------------------------------------- }

function TCastleUserInterface.TEnumerator.GetCurrent: TCastleUserInterface;
begin
  Result := FList[FPosition];
end;

constructor TCastleUserInterface.TEnumerator.Create(const AList: TInternalChildrenControls);
begin
//  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TCastleUserInterface.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  { Note that FList may be nil, as TCastleUserInterface.FControls may be nil when empty. }
  Result := (FList <> nil) and (FPosition < FList.Count);
end;

{ TCastleUserInterface ----------------------------------------------------------------- }

constructor TCastleUserInterface.Create(AOwner: TComponent);
begin
  inherited;
  FExclusiveEvents := true;
  FCursor := mcDefault;
  FVisible := true;
  FExists := true;
  FEnableUIScaling := true;
  FCapturesEvents := true;
  FWidth := DefaultWidth;
  FHeight := DefaultHeight;
  FBorder := TBorder.Create({$ifdef FPC}@{$endif}BorderChange);
  FLastSeenUIScale := 1.0;

  FCustomThemeObserver := TFreeNotificationObserver.Create(Self);
  FCustomThemeObserver.OnFreeNotification := {$ifdef FPC}@{$endif} CustomThemeFreeNotification;

  FTooltipControlObserver := TFreeNotificationObserver.Create(Self);
  FTooltipControlObserver.OnFreeNotification := {$ifdef FPC}@{$endif} TooltipControlFreeNotification;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleuserinterface_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

function TCastleUserInterface.Press(const Event: TInputPressRelease): boolean;
begin
  Result := false;
  if Assigned(OnPress) then
    OnPress(Self, Event, Result);
end;

function TCastleUserInterface.Release(const Event: TInputPressRelease): boolean;
begin
  Result := false;
  if Assigned(OnRelease) then
    OnRelease(Self, Event, Result);
end;

function TCastleUserInterface.PreviewPress(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function TCastleUserInterface.PreviewRelease(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function TCastleUserInterface.Motion(const Event: TInputMotion): boolean;
begin
  Result := false;
  if Assigned(OnMotion) then
    OnMotion(Self, Event, Result);
end;

function TCastleUserInterface.SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean;
begin
  Result := false;
end;

function TCastleUserInterface.SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean;
begin
  Result := false;
end;

function TCastleUserInterface.JoyAxisMove(const JoyID, Axis: Byte): boolean;
begin
  Result := False;
end;

function TCastleUserInterface.JoyButtonPress(const JoyID, Button: Byte): boolean;
begin
  Result := False;
end;

procedure TCastleUserInterface.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  if Assigned(OnUpdate) then
    OnUpdate(Self, SecondsPassed, HandleInput);
end;

procedure TCastleUserInterface.VisibleChange(const Changes: TCastleUserInterfaceChanges;
  const ChangeInitiatedByChildren: boolean);
begin
  Assert(Changes <> [], 'Never call VisibleChange with an empty set');

  { Debug:
  Writeln(ClassName, '.VisibleChange: [',
    Iff(chRender    in Changes, 'chRender, '   , ''),
    Iff(chRectangle in Changes, 'chRectangle, ', ''),
    Iff(chCursor    in Changes, 'chCursor, '   , ''),
    Iff(chCamera    in Changes, 'chCamera, '   , ''),
    Iff(chExists    in Changes, 'chExists, '   , ''),
    Iff(chChildren  in Changes, 'chChildren, ' , ''),
    ']'
  ); }

  if Container <> nil then
    Container.ControlsVisibleChange(Self, Changes, ChangeInitiatedByChildren);
  if Assigned(OnVisibleChange) then
    OnVisibleChange(Self, Changes, ChangeInitiatedByChildren);
  if Parent <> nil then
    Parent.VisibleChange(Changes, true);
  if [chRectangle, chChildren, chChildrenExists] * Changes <> [] then
    FSizeFromChildrenValid := false;
end;

function TCastleUserInterface.AllowSuspendForInput: boolean;
begin
  Result := true;
end;

procedure TCastleUserInterface.Resize;
begin
  {$warnings off}
  ContainerResize(ContainerWidth, ContainerHeight); // call deprecated, to keep it working
  {$warnings on}
end;

procedure TCastleUserInterface.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
end;

function TCastleUserInterface.ContainerWidth: Cardinal;
begin
  if ContainerSizeKnown then
    Result := Container.Width else
    Result := 0;
end;

function TCastleUserInterface.ContainerHeight: Cardinal;
begin
  if ContainerSizeKnown then
    Result := Container.Height else
    Result := 0;
end;

function TCastleUserInterface.ContainerRect: TRectangle;
begin
  if ContainerSizeKnown then
    Result := Container.Rect
  else
    Result := TRectangle.Empty;
end;

function TCastleUserInterface.ContainerSizeKnown: boolean;
begin
  { Note that ContainerSizeKnown is calculated looking at current Container,
    without waiting for Resize. This way it works even before
    we receive Resize method, which may happen to be useful:
    if you insert some TCastleUserInterface to a window before it's open,
    and then you do something inside OnOpen that wants to render
    this viewport (which may happen if you simply initialize a progress bar
    without any predefined loading_image). TCastleUserInterface did not receive
    a Resize in this case yet (it will receive it from OnResize,
    which happens after OnOpen).

    See castle_game_engine/tests/testcontainer.pas for cases
    when this is really needed. }

  Result := (Container <> nil) and
    (not (csDestroying in Container.ComponentState)) and
    Container.GLInitialized;
end;

procedure TCastleUserInterface.SetCursor(const Value: TMouseCursor);
begin
  if Value <> FCursor then
  begin
    FCursor := Value;
    VisibleChange([chCursor]);
    {$ifdef FPC}
    {$warnings off} // keep deprecated method working
    DoCursorChange;
    {$warnings on}
    {$endif}
  end;
end;

{$ifdef FPC}
procedure TCastleUserInterface.DoCursorChange;
begin
  {$warnings off} // keep deprecated event working
  if Assigned(OnCursorChange) then OnCursorChange(Self);
  {$warnings on}
end;
{$endif FPC}

destructor TCastleUserInterface.Destroy;
begin
  GLContextClose;
  FreeAndNil(FControls);
  FreeAndNil(FBorder);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleuserinterface_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleUserInterface.CustomSerialization(const SerializationProcess: TSerializationProcess);
var
  OldDelta: Single;
begin
  inherited;
  SerializationProcess.ReadWriteList('Children',
    {$ifdef FPC}@{$endif}SerializeChildrenEnumerate,
    {$ifdef FPC}@{$endif}SerializeChildrenAdd,
    {$ifdef FPC}@{$endif}SerializeChildrenClear);

  OldDelta := 0;
  SerializationProcess.ReadWriteSingle('HorizontalAnchorDelta', OldDelta, false);
  if OldDelta <> 0 then
  begin
    Translation := Vector2(OldDelta, Translation.Y);
    // too verbose for now, we have lots of designs with old names
    // WritelnWarning('Upgrading deprecated HorizontalAnchorDelta to Translation.X on component "%s"', [
    //   Name
    // ]);
  end;

  OldDelta := 0;
  SerializationProcess.ReadWriteSingle('VerticalAnchorDelta', OldDelta, false);
  if OldDelta <> 0 then
  begin
    Translation := Vector2(Translation.X, OldDelta);
    // too verbose for now, we have lots of designs with old names
    // WritelnWarning('Upgrading deprecated VerticalAnchorDelta to Translation.Y on component "%s"', [
    //   Name
    // ]);
  end;
end;

procedure TCastleUserInterface.SerializeChildrenEnumerate(const Proc: TGetChildProc);
var
  I: Integer;
begin
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      { Do not save SubComponents, like TCastleScrollView.ScrollArea.
        They are saved separately, as published properties.
        Also do not save csTransient components, like loaded TCastleDesign
        child, or TCastleCheckbox children. They are automatically managed
        by the parent, and they should not be saved in normal fashion. }
      if [csSubComponent, csTransient] * FControls[I].ComponentStyle = [] then
        Proc(FControls[I]);
end;

procedure TCastleUserInterface.SerializeChildrenAdd(const C: TComponent);
begin
  InsertFront(C as TCastleUserInterface);
end;

procedure TCastleUserInterface.SerializeChildrenClear;
var
  I: Integer;
begin
  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do // downto, as list may shrink during loop
      if FControls[I].ComponentStyle * [csSubComponent, csTransient] = [] then
        FControls[I].Free; // will remove itself from children list
end;

procedure TCastleUserInterface.CreateControls;
begin
  if FControls = nil then
  begin
    FControls := TInternalChildrenControls.Create(Self);
    FControls.Container := Container;
  end;
end;

procedure TCastleUserInterface.InsertFront(const NewItem: TCastleUserInterface);
begin
  CreateControls;
  FControls.InsertFront(NewItem);
end;

procedure TCastleUserInterface.InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
begin
  CreateControls;
  FControls.InsertFrontIfNotExists(NewItem);
end;

procedure TCastleUserInterface.InsertFront(const NewItems: TCastleUserInterfaceList);
begin
  CreateControls;
  FControls.InsertFront(NewItems);
end;

procedure TCastleUserInterface.InsertBack(const NewItem: TCastleUserInterface);
begin
  CreateControls;
  FControls.InsertBack(NewItem);
end;

procedure TCastleUserInterface.InsertBackIfNotExists(const NewItem: TCastleUserInterface);
begin
  CreateControls;
  FControls.InsertBackIfNotExists(NewItem);
end;

procedure TCastleUserInterface.InsertBack(const NewItems: TCastleUserInterfaceList);
begin
  CreateControls;
  FControls.InsertBack(NewItems);
end;

procedure TCastleUserInterface.InsertControl(const Index: Integer; const NewItem: TCastleUserInterface);
begin
  CreateControls;
  FControls.Insert(Index, NewItem);
end;

procedure TCastleUserInterface.RemoveControl(const Item: TCastleUserInterface);
begin
  if FControls <> nil then
    FControls.Remove(Item);
end;

function TCastleUserInterface.IndexOfControl(const Item: TCastleUserInterface
  ): Integer;
begin
  if FControls <> nil then
    Result := FControls.IndexOf(Item)
  else
    Result := -1;
end;

procedure TCastleUserInterface.ClearControls;
begin
  if FControls <> nil then
    FControls.Clear;
end;

function TCastleUserInterface.GetControls(const I: Integer): TCastleUserInterface;
begin
  Result := FControls[I];
end;

procedure TCastleUserInterface.SetControls(const I: Integer; const Item: TCastleUserInterface);
begin
  FControls[I] := Item;
end;

function TCastleUserInterface.ControlsCount: Integer;
begin
  if FControls <> nil then
    Result := FControls.Count else
    Result := 0;
end;

procedure TCastleUserInterface.InternalSetContainer(const Value: TCastleContainer);
var
  I: Integer;
begin
  FContainer := Value;
  if FControls <> nil then
  begin
    FControls.Container := Value;
    for I := 0 to FControls.Count - 1 do
      FControls[I].InternalSetContainer(Value);
  end;
end;

procedure TCastleUserInterface.SetEnableUIScaling(const Value: boolean);
begin
  if FEnableUIScaling <> Value then
  begin
    FEnableUIScaling := Value;
    CheckUIScaleChanged;
  end;
end;

procedure TCastleUserInterface.UIScaleChanged;
begin
end;

{ No point in doing anything? We should propagate it to to parent like T3D?
procedure TCastleUserInterface.DoCursorChange;
begin
  inherited;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].DoCursorChange;
end;
}

function TCastleUserInterface.CapturesEventsAtPosition(const Position: TVector2): boolean;
var
  SR: TFloatRectangle;
begin
  if not CapturesEvents then
    Exit(false);

  SR := RenderRect;
  Result := SR.Contains(Position) or
    { if the control covers the whole Container, it *always* captures events,
      even when mouse position is unknown yet, or outside the window. }
    (ContainerSizeKnown and
     (SR.Left <= 0) and
     (SR.Bottom <= 0) and
     (SR.Width >= ContainerWidth) and
     (SR.Height >= ContainerHeight));
end;

function TCastleUserInterface.TooltipExists: boolean;
begin
  Result := TooltipControl <> nil;
end;

function TCastleUserInterface.UIScale: Single;
begin
  Result := FLastSeenUIScale;
end;

procedure TCastleUserInterface.CheckUIScaleChanged;
var
  NewUIScale: Single;
begin
  if (Container <> nil) and EnableUIScaling then
    NewUIScale := Container.UIScale
  else
    NewUIScale := 1;
  if FLastSeenUIScale <> NewUIScale then
  begin
    FLastSeenUIScale := NewUIScale;
    UIScaleChanged;
  end;
end;

procedure TCastleUserInterface.CheckResize;
var
  NewContainerWidth, NewContainerHeight: Integer;
begin
  if Container <> nil then
  begin
    NewContainerWidth := Container.Width;
    NewContainerHeight := Container.Height;
  end else
  begin
    NewContainerWidth := 0;
    NewContainerHeight := 0;
  end;
  if (FLastSeenContainerWidth <> NewContainerWidth) or
     (FLastSeenContainerHeight <> NewContainerHeight) then
  begin
    FLastSeenContainerWidth := NewContainerWidth;
    FLastSeenContainerHeight := NewContainerHeight;
    Resize;
  end;
end;

function TCastleUserInterface.DebugName: String;
begin
  if Name <> '' then
    Result := '"' + Name + '" (' + ClassName + ')'
  else
    Result := '<unnamed> (' + ClassName + ')';
end;

procedure TCastleUserInterface.BeforeRender;
begin
end;

procedure TCastleUserInterface.Render;
begin
end;

procedure TCastleUserInterface.RenderOverChildren;
begin
end;

procedure TCastleUserInterface.RecursiveRender(const ViewportRect: TRectangle);

  { Draw 4 borders.
    RectBorder is equal to Self.RenderRectWithBorder,
    but it is already calculated by the caller. }
  procedure DrawBorder(const RectBorder: TFloatRectangle);

    procedure DrawBorderRectangle(const R: TFloatRectangle);
    begin
      if not R.IsEmpty then
        DrawRectangle(R, BorderColor);
    end;

  var
    RectLeftRightBorders: TFloatRectangle;
  begin
    if FBorderColor[3] = 0 then Exit; // early exit in a common case
    { RenderControlPrepare necessary, since TCastleViewport could have
      changed RenderContext.Viewport. }
    TCastleContainer.RenderControlPrepare(ViewportRect);

    DrawBorderRectangle(RectBorder.TopPart(Border.TotalTop * UIScale));
    DrawBorderRectangle(RectBorder.BottomPart(Border.TotalBottom * UIScale));

    { Draw left and right borders from a smaller rectangle.
      This way we do not overdraw border corners, which is important
      in case BorderColor[3] is e.g. 0.5. Drawing corner twice would
      make it seem more opaque.
      Unfortunately artifacts are visible (the right part is visibly
      shifted by ~1 pixel to the right), so we don't do it always now. }
    RectLeftRightBorders := RectBorder.
      RemoveTop(Border.TotalTop * UIScale).
      RemoveBottom(Border.TotalBottom * UIScale);
    DrawBorderRectangle(RectLeftRightBorders.RightPart(Border.TotalRight * UIScale));
    DrawBorderRectangle(RectLeftRightBorders.LeftPart(Border.TotalLeft * UIScale));
  end;

  procedure CacheRectBegin;
  begin
    // the calculation inside will use Rect a number of times, make it faster
    if FUseCachedRectWithoutAnchors = 0 then
      FCachedRectWithoutAnchors := RectWithoutAnchors;
    Inc(FUseCachedRectWithoutAnchors);
  end;

  procedure CacheRectEnd;
  begin
    Dec(FUseCachedRectWithoutAnchors);
  end;

  function ClippingRect: TFloatRectangle;
  var
    ResultInt, ScissorRect: TRectangle;
  begin
    ResultInt := ContainerRect;
    if RenderContext.FinalScissor(ScissorRect) then
      ResultInt := ResultInt * ScissorRect;
    Result := FloatRectangle(ResultInt);
  end;

var
  Scissor: TScissor;

  procedure ClipChildrenBegin;
  begin
    if ClipChildren then
    begin
      Scissor := TScissor.Create;
      Scissor.Rect := RenderRect.Round;
      Scissor.Enabled := true;
    end else
      Scissor := nil;
  end;

  procedure ClipChildrenEnd;
  begin
    if Scissor <> nil then
    begin
      Scissor.Enabled := false;
      FreeAndNil(Scissor);
    end;
  end;

var
  I: Integer;
  R: TFloatRectangle;
begin
  if Exists then
  begin
    if Culling and not FVisible then CheckUIScaleChanged;

    CacheRectBegin;

    R := RenderRectWithBorder;

    FVisible := (not Culling) or ClippingRect.Collides(R);
    if FVisible then
    begin
      DrawBorder(R);

      ClipChildrenBegin;

      { We check GLInitialized, because it may happen that a control
        did not receive GLContextOpen yet, in case we cause some rendering
        during TCastleContainer.EventOpen (e.g. because some TCastleUserInterface.GLContextOpen
        calls Window.Screenshot, so everything is rendered
        before even the rest of controls received TCastleUserInterface.GLContextOpen).
        See castle_game_engine/tests/testcontainer.pas . }

      if GLInitialized then
      begin
        TCastleContainer.RenderControlPrepare(ViewportRect);
        Render;

        if Assigned(OnRender) then
        begin
          TCastleContainer.RenderControlPrepare(ViewportRect);
          OnRender(Self);
        end;
      end;

      for I := 0 to ControlsCount - 1 do
        Controls[I].RecursiveRender(ViewportRect);

      if GLInitialized then
      begin
        TCastleContainer.RenderControlPrepare(ViewportRect);
        RenderOverChildren;
      end;

      ClipChildrenEnd;
    end;

    CacheRectEnd;
  end;
end;

procedure TCastleUserInterface.TooltipRender(const TooltipPosition: TVector2);
begin
end;

procedure TCastleUserInterface.GLContextOpen;
begin
  FGLInitialized := true;
end;

procedure TCastleUserInterface.GLContextClose;
begin
  FGLInitialized := false;
end;

procedure TCastleUserInterface.DoInternalMouseEnter;
begin
  if Assigned(OnInternalMouseEnter) then
    OnInternalMouseEnter(Self);
end;

procedure TCastleUserInterface.DoInternalMouseLeave;
begin
  if Assigned(OnInternalMouseLeave) then
    OnInternalMouseLeave(Self);
end;

procedure TCastleUserInterface.SetFocused(const Value: boolean);
var
  OldValue: Boolean;
begin
  OldValue := FFocused;
  FFocused := Value;
  if (not OldValue) and Value then
    DoInternalMouseEnter
  else
  if OldValue and (not Value) then
    DoInternalMouseLeave;
end;

procedure TCastleUserInterface.SetExists(const Value: boolean);
begin
  if FExists <> Value then
  begin
    FExists := Value;
    VisibleChange([chExists]);
    if Parent <> nil then
      Parent.VisibleChange([chChildrenExists]);
  end;
end;

procedure TCastleUserInterface.BorderChange(Sender: TObject);
begin
  VisibleChange([chRectangle]);
end;

{ We store Left property value in file under "TUIControlPos_Real_Left" name,
  to avoid clashing with TComponent magic "left" property name.
  The idea how to do this is taken from TComponent's own implementation
  of it's "left" magic property (rtl/objpas/classes/compon.inc). }

procedure TCastleUserInterface.ReadRealLeft(Reader: TReader);
begin
  FLeft := Reader.ReadSingle;
end;

procedure TCastleUserInterface.WriteRealLeft(Writer: TWriter);
begin
  Writer.WriteSingle(FLeft);
end;

procedure TCastleUserInterface.ReadLeft(Reader: TReader);
var
  D: LongInt;
begin
  D := DesignInfo;
  LongRec(D).Lo:=Reader.ReadInteger;
  DesignInfo := D;
end;

procedure TCastleUserInterface.ReadTop(Reader: TReader);
var
  D: LongInt;
begin
  D := DesignInfo;
  LongRec(D).Hi:=Reader.ReadInteger;
  DesignInfo := D;
end;

procedure TCastleUserInterface.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TCastleUserInterface.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

procedure TCastleUserInterface.DefineProperties(Filer: TFiler);
Var Ancestor : TComponent;
    Temp : longint;
begin
  { Don't call inherited that defines magic left/top.
    This would make reading design-time "left" broken, it seems that our
    declaration of Left with "stored false" would then prevent the design-time
    Left from ever loading.

    Instead, we'll save design-time "Left" below, under a special name. }

  Filer.DefineProperty('TUIControlPos_RealLeft',
    {$ifdef FPC}@{$endif} ReadRealLeft,
    {$ifdef FPC}@{$endif} WriteRealLeft,
    FLeft <> 0);

    // TODO: unfinished tests
(*
  Filer.DefineProperty('Controls',
    {$ifdef FPC}@{$endif} ReadControls,
    {$ifdef FPC}@{$endif} WriteControls,
    ControlsCount <> 0);
*)

  { Code from fpc/trunk/rtl/objpas/classes/compon.inc }
  Temp:=0;
  Ancestor:=TComponent(Filer.Ancestor);
  If Assigned(Ancestor) then Temp:=Ancestor.DesignInfo;
  Filer.Defineproperty('TUIControlPos_Design_Left',
    {$ifdef FPC}@{$endif} readleft,
    {$ifdef FPC}@{$endif} writeleft,
    (longrec(DesignInfo).Lo<>Longrec(temp).Lo));
  Filer.Defineproperty('TUIControlPos_Design_Top',
    {$ifdef FPC}@{$endif} readtop,
    {$ifdef FPC}@{$endif} writetop,
    (longrec(DesignInfo).Hi<>Longrec(temp).Hi));
end;

function TCastleUserInterface.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if      PropertyName = 'Exists' then
    Result := [psBasic]
  else if PropertyName = 'FullSize' then
    Result := [psBasic, psLayout]
  else if (PropertyName = 'Width') or
          (PropertyName = 'Height') or
          (PropertyName = 'HeightFraction') or
          (PropertyName = 'WidthFraction') or
          (PropertyName = 'AutoSizeToChildren') or
          (PropertyName = 'AutoSizeToChildrenPaddingTop') or
          (PropertyName = 'AutoSizeToChildrenPaddingRight') or
          (PropertyName = 'TranslationPersistent') or
          (PropertyName = 'HorizontalAnchorSelf') or
          (PropertyName = 'HorizontalAnchorParent') or
          (PropertyName = 'VerticalAnchorSelf') or
          (PropertyName = 'VerticalAnchorParent') or
          // (PropertyName = 'Left') or // deprecated
          // (PropertyName = 'Bottom') or // deprecated
          (PropertyName = 'Border') or
          (PropertyName = 'BorderColorPersistent') then
    Result := [psLayout]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleUserInterface.SetLeft(const Value: Single);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetBottom(const Value: Single);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    VisibleChange([chRectangle]);
  end;
end;

{$warnings off} // using deprecated in deprecated

procedure TCastleUserInterface.Align(
  const ControlPosition: THorizontalPosition;
  const ContainerPosition: THorizontalPosition;
  const X: Single = 0);
begin
  Left := FastRectWithoutAnchors.AlignCore(ControlPosition, ParentRect, ContainerPosition, X);
end;

procedure TCastleUserInterface.Align(
  const ControlPosition: TVerticalPosition;
  const ContainerPosition: TVerticalPosition;
  const Y: Single = 0);
begin
  Bottom := FastRectWithoutAnchors.AlignCore(ControlPosition, ParentRect, ContainerPosition, Y);
end;

procedure TCastleUserInterface.AlignHorizontal(
  const ControlPosition: TPositionRelative;
  const ContainerPosition: TPositionRelative;
  const X: Single);
begin
  Align(
    THorizontalPosition(ControlPosition),
    THorizontalPosition(ContainerPosition), X);
end;

procedure TCastleUserInterface.AlignVertical(
  const ControlPosition: TPositionRelative;
  const ContainerPosition: TPositionRelative;
  const Y: Single);
begin
  Align(
    TVerticalPosition(ControlPosition),
    TVerticalPosition(ContainerPosition), Y);
end;

procedure TCastleUserInterface.Center;
begin
  Align(hpMiddle, hpMiddle);
  Align(vpMiddle, vpMiddle);
end;

{$warnings on}

procedure TCastleUserInterface.BeforeSizing;
begin
end;

procedure TCastleUserInterface.PreferredSize(var PreferredWidth, PreferredHeight: Single);
begin
  // nothing to be done here
end;

function TCastleUserInterface.RectWithoutAnchors: TFloatRectangle;

  procedure UpdateSizeFromChildren;
  var
    I: Integer;
    C: TCastleUserInterface;
    ChildRect: TFloatRectangle;
  begin
    if FSizeFromChildrenValid then Exit;
    FSizeFromChildrenValid := true;
    FSizeFromChildrenWidth := 0;
    FSizeFromChildrenHeight := 0;

    for I := 0 to ControlsCount - 1 do
    begin
      C := Controls[I];
      if not C.Exists then Continue;

      { Calculate ChildRect.
        We cannot use C.EffectiveRect or C.RectWithAnchors now,
        as they would query ParentRect, thus recursively asking for our
        current size.
        Although FSizeFromChildrenValid would prevent from entering an infinite
        loop, it would be still unreliable to use such result. }
      ChildRect := C.FastRectWithoutAnchors.ScaleAround0(1 / UIScale);
      if ChildRect.IsEmpty then
        Continue;

      { We had 2, much more complicated and more limited, implementations of this :)
        Note that the instruction below makes sense both for both when
        C.HorizontalAnchorSelf = C.HorizontalAnchorParent = hpLeft and when
        C.HorizontalAnchorSelf = C.HorizontalAnchorParent = hpRight.
      }

      MaxVar(FSizeFromChildrenWidth , ChildRect.Left   + ChildRect.Width  + Abs(C.Translation.X) + FBorder.TotalWidth);
      MaxVar(FSizeFromChildrenHeight, ChildRect.Bottom + ChildRect.Height + Abs(C.Translation.Y) + FBorder.TotalHeight);
    end;

    FSizeFromChildrenWidth  := FSizeFromChildrenWidth + AutoSizeToChildrenPaddingRight;
    FSizeFromChildrenHeight := FSizeFromChildrenHeight + AutoSizeToChildrenPaddingTop;
  end;

var
  PR: TFloatRectangle;
  W, H, BorderW, BorderH: Single;
begin
  if FInsideRectWithoutAnchors then
  begin
    WriteLnWarning('UI control ' + DebugName + ' encountered an endless loop when trying to calculate it''s size. This means that UI child size depends on the parent (e.g. using FullSize or WidthFraction or HeightFraction),' + ' while at the same time UI parent size depends on the child (e.g. using AutoSizeToChildren).');
    Exit(TFloatRectangle.Empty);
  end;
  FInsideRectWithoutAnchors := true;

  BeforeSizing;

  if AutoSizeToChildren then
  begin
    UpdateSizeFromChildren;
    if (FSizeFromChildrenWidth <= 0) or
       (FSizeFromChildrenHeight <= 0) then
      Result := TFloatRectangle.Empty
    else
      {$warnings off} // using deprecated Left, Bottom to keep them working
      Result := FloatRectangle(Left, Bottom, FSizeFromChildrenWidth, FSizeFromChildrenHeight).
        ScaleAround0(UIScale);
      {$warnings on}
  end else
  if FullSize then
  begin
    Result := ParentRect;
  end else
  begin
    W := Width * UIScale;
    H := Height * UIScale;
    if (WidthFraction <> 0) or (HeightFraction <> 0) then
    begin
      PR := ParentRect;
      if WidthFraction <> 0 then
        W := WidthFraction * PR.Width;
      if HeightFraction <> 0 then
        H := HeightFraction * PR.Height;
    end;

    // subtract Border from W, H
    BorderW := Border.TotalWidth * UIScale;
    BorderH := Border.TotalHeight * UIScale;
    W := W - BorderW;
    H := H - BorderH;

    PreferredSize(W, H);

    // add Border around Result
    W := W + BorderW;
    H := H + BorderH;

    if (W > 0) and (H > 0) then
    begin
      {$warnings off} // using deprecated Left, Bottom to keep them working
      Result := FloatRectangle(Left * UIScale, Bottom * UIScale, W, H);
      {$warnings on}
      //Result.Left := Result.Left - FBorder.TotalLeft * UIScale; // no need to
      //Result.Bottom := Result.Bottom - FBorder.TotalBottom * UIScale; // no need to
    end else
      Result := TFloatRectangle.Empty;
  end;

  FInsideRectWithoutAnchors := false;
end;

function TCastleUserInterface.EffectiveRect: TFloatRectangle;
begin
  Result := RectWithAnchors(true).ScaleAround0(1 / UIScale);
end;

function TCastleUserInterface.EffectiveWidth: Single;
var
  R: TFloatRectangle;
begin
  { Naive implementation:
  Result := EffectiveRect.Width; }

  { Optimized implementation, knowing that RectWithAnchors(true) does not
    change RectWithoutAnchors.Width:
  Result := RectWithoutAnchors.ScaleAround0(1 / UIScale).Width; }

  { Optimized implementation: }
  R := FastRectWithoutAnchors;
  if R.IsEmpty then
    Result := 0
  else
  begin
    Result := R.Width / UIScale;
    //Assert(Result = EffectiveRect.Width);
  end;
end;

function TCastleUserInterface.EffectiveHeight: Single;
var
  R: TFloatRectangle;
begin
  R := FastRectWithoutAnchors;
  if R.IsEmpty then
    Result := 0
  else
  begin
    Result := R.Height / UIScale;
    //Assert(Result = EffectiveRect.Height);
  end;
end;

function TCastleUserInterface.CalculatedWidth: Cardinal;
begin
  Result := Round(EffectiveWidth);
end;

function TCastleUserInterface.CalculatedHeight: Cardinal;
begin
  Result := Round(EffectiveHeight);
end;

function TCastleUserInterface.CalculatedRect: TRectangle;
begin
  Result := EffectiveRect.Round;
end;

function TCastleUserInterface.FastRectWithoutAnchors: TFloatRectangle;
begin
  if FUseCachedRectWithoutAnchors <> 0 then
    Result := FCachedRectWithoutAnchors
  else
    Result := RectWithoutAnchors;
end;

function TCastleUserInterface.EffectiveFullSize: Boolean;
begin
  Result := FullSize and not AutoSizeToChildren;
end;

function TCastleUserInterface.RectWithAnchors(const CalculateEvenWithoutContainer: boolean): TFloatRectangle;
var
  PR: TFloatRectangle;
begin
  if (not ContainerSizeKnown) and
     (not CalculateEvenWithoutContainer) then
    { Don't call virtual Rect in this state, Rect implementations
      typically assume that ParentRect is sensible.
      This is crucial, various programs will crash without it. }
    Exit(TFloatRectangle.Empty);

  Result := FastRectWithoutAnchors;

  { apply my Anchors and parent Border }
  if not EffectiveFullSize then
  begin
    PR := ParentRect;
    Result.Left := Result.Left +
      Result.AlignCore(HorizontalAnchorSelf, PR, HorizontalAnchorParent,
        UIScale * Translation.X);
    Result.Bottom := Result.Bottom +
      Result.AlignCore(VerticalAnchorSelf, PR, VerticalAnchorParent,
        UIScale * Translation.Y);
  end;
end;

function TCastleUserInterface.RenderRectWithBorder: TFloatRectangle;
var
  T: TVector2;
begin
  Result := RectWithAnchors;
  { transform local to container }
  if Parent <> nil then
  begin
    T := Parent.LocalToContainerTranslation;
    Result.Left := Result.Left + T[0];
    Result.Bottom := Result.Bottom + T[1];
  end;
end;

function TCastleUserInterface.RenderRect: TFloatRectangle;
begin
  Result := RenderRectWithBorder;
  if FBorder.Exists then // optimize common case
    Result := Result.
      RemoveTop   (FBorder.TotalTop    * UIScale).
      RemoveRight (FBorder.TotalRight  * UIScale).
      RemoveBottom(FBorder.TotalBottom * UIScale).
      RemoveLeft  (FBorder.TotalLeft   * UIScale);
end;

function TCastleUserInterface.ScreenRect: TRectangle;
begin
  Result := RenderRect.Round;
end;

function TCastleUserInterface.LocalToContainerTranslation: TVector2;
var
  RA: TFloatRectangle;
begin
  if Parent <> nil then
    Result := Parent.LocalToContainerTranslation
  else
    Result := TVector2.Zero;

  RA := RectWithAnchors;
  Result.X := Result.X + RA.Left;
  Result.Y := Result.Y + RA.Bottom;
end;

function TCastleUserInterface.LocalToContainerTranslationShiftBorder: TVector2;
begin
  Result := LocalToContainerTranslation;
  if FBorder.Exists then // optimize common case
  begin
    Result.X := Result.X + FBorder.TotalLeft   * UIScale;
    Result.Y := Result.Y + FBorder.TotalBottom * UIScale;
  end;
end;

function TCastleUserInterface.ContainerToLocalPosition(ContainerPosition: TVector2;
  const ContainerPositionScaled: Boolean): TVector2;
begin
  { LocalToContainerTranslationShiftBorder is always in scaled coordinates.
    So to implement ContainerPositionScaled = false, we need to multiply by UIScale,
    only to later divide by it...
    This should be cleaned up. But then, more routines like RectWithAnchors,
    ParentRect will require an alternative "unscaled" variant,
    or we should just use "unscaled" coordinates more throughout the internal code. }
  if not ContainerPositionScaled then
    ContainerPosition := ContainerPosition * UIScale;
  Result := (ContainerPosition - LocalToContainerTranslationShiftBorder) / UIScale;
end;

function TCastleUserInterface.LocalToContainerPosition(const LocalPosition: TVector2;
  const ContainerPositionScaled: Boolean): TVector2;
begin
  Result := LocalPosition * UIScale + LocalToContainerTranslationShiftBorder;
  if not ContainerPositionScaled then
    Result := Result / UIScale;
end;

function TCastleUserInterface.ParentRect: TFloatRectangle;
begin
  if Parent <> nil then
  begin
    Result := Parent.FastRectWithoutAnchors;
    Result.Left := 0;
    Result.Bottom := 0;

    if Parent.FBorder.Exists then // optimize common case
      Result := Result.
        RemoveTop   (Parent.FBorder.TotalTop    * UIScale).
        RemoveRight (Parent.FBorder.TotalRight  * UIScale).
        RemoveBottom(Parent.FBorder.TotalBottom * UIScale).
        RemoveLeft  (Parent.FBorder.TotalLeft   * UIScale);
  end else
    Result := FloatRectangle(ContainerRect);
end;

procedure TCastleUserInterface.SetHorizontalAnchorSelf(const Value: THorizontalPosition);
begin
  if FHorizontalAnchorSelf <> Value then
  begin
    FHorizontalAnchorSelf := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetHorizontalAnchorParent(const Value: THorizontalPosition);
begin
  if FHorizontalAnchorParent <> Value then
  begin
    FHorizontalAnchorParent := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetTranslation(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FTranslation, Value) then
  begin
    FTranslation := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleUserInterface.GetHorizontalAnchorDelta: Single;
begin
  Result := Translation.X;
end;

function TCastleUserInterface.GetVerticalAnchorDelta: Single;
begin
  Result := Translation.Y;
end;

procedure TCastleUserInterface.SetHorizontalAnchorDelta(const Value: Single);
begin
  Translation := Vector2(Value, Translation.Y);
end;

procedure TCastleUserInterface.SetVerticalAnchorDelta(const Value: Single);
begin
  Translation := Vector2(Translation.X, Value);
end;

procedure TCastleUserInterface.SetVerticalAnchorSelf(const Value: TVerticalPosition);
begin
  if FVerticalAnchorSelf <> Value then
  begin
    FVerticalAnchorSelf := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetVerticalAnchorParent(const Value: TVerticalPosition);
begin
  if FVerticalAnchorParent <> Value then
  begin
    FVerticalAnchorParent := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.Anchor(const AHorizontalAnchor: THorizontalPosition;
  const TranslationX: Single);
begin
  HorizontalAnchorSelf := AHorizontalAnchor;
  HorizontalAnchorParent := AHorizontalAnchor;
  Translation := Vector2(TranslationX, Translation.Y);
end;

procedure TCastleUserInterface.Anchor(
  const AHorizontalAnchorSelf, AHorizontalAnchorParent: THorizontalPosition;
  const TranslationX: Single);
begin
  HorizontalAnchorSelf := AHorizontalAnchorSelf;
  HorizontalAnchorParent := AHorizontalAnchorParent;
  Translation := Vector2(TranslationX, Translation.Y);
end;

procedure TCastleUserInterface.Anchor(const AVerticalAnchor: TVerticalPosition;
  const TranslationY: Single);
begin
  VerticalAnchorSelf := AVerticalAnchor;
  VerticalAnchorParent := AVerticalAnchor;
  Translation := Vector2(Translation.X, TranslationY);
end;

procedure TCastleUserInterface.Anchor(
  const AVerticalAnchorSelf, AVerticalAnchorParent: TVerticalPosition;
  const TranslationY: Single);
begin
  VerticalAnchorSelf := AVerticalAnchorSelf;
  VerticalAnchorParent := AVerticalAnchorParent;
  Translation := Vector2(Translation.X, TranslationY);
end;

procedure TCastleUserInterface.SetWidth(const Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetWidthFraction(const Value: Single);
begin
  if FWidthFraction <> Value then
  begin
    FWidthFraction := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetHeightFraction(const Value: Single);
begin
  if FHeightFraction <> Value then
  begin
    FHeightFraction := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetFullSize(const Value: boolean);
begin
  if FFullSize <> Value then
  begin
    FFullSize := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.SetBorderColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FBorderColor, Value) then
  begin
    FBorderColor := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleUserInterface.SetAutoSizeToChildren(const Value: Boolean);
begin
  if FAutoSizeToChildren <> Value then
  begin
    FAutoSizeToChildren := Value;
    FSizeFromChildrenValid := false;
  end;
end;

procedure TCastleUserInterface.SetAutoSizeToChildrenPaddingRight(const Value: Single);
begin
  if FAutoSizeToChildrenPaddingRight <> Value then
  begin
    FAutoSizeToChildrenPaddingRight := Value;
    FSizeFromChildrenValid := false;
  end;
end;

procedure TCastleUserInterface.SetAutoSizeToChildrenPaddingTop(const Value: Single);
begin
  if FAutoSizeToChildrenPaddingTop <> Value then
  begin
    FAutoSizeToChildrenPaddingTop := Value;
    FSizeFromChildrenValid := false;
  end;
end;

procedure TCastleUserInterface.SetCulling(const Value: Boolean);
begin
  if FCulling <> Value then
  begin
    FCulling := Value;
    if not Value then
      FVisible := true; // Visible is always true when Culling is false
    VisibleChange([chRender]);
  end;
end;

procedure TCastleUserInterface.SetCustomTheme(const Value: TCastleTheme);
begin
  if FCustomTheme <> Value then
  begin
    FCustomThemeObserver.Observed := Value;
    FCustomTheme := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleUserInterface.CustomThemeFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  CustomTheme := nil;
end;

function TCastleUserInterface.Theme: TCastleTheme;
begin
  if CustomTheme <> nil then
    Result := CustomTheme
  else
    Result := CastleUIControls.Theme;
end;

procedure TCastleUserInterface.SetTooltipControl(const Value: TCastleUserInterface);
begin
  if FTooltipControl <> Value then
  begin
    FTooltipControlObserver.Observed := Value;
    FTooltipControl := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleUserInterface.TooltipControlFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  TooltipControl := nil;
end;

procedure TCastleUserInterface.SetClipChildren(const Value: Boolean);
begin
  if FClipChildren <> Value then
  begin
    FClipChildren := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleUserInterface.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  ResizeWidth := true;
  ResizeHeight := true;
  Reason := '';

  if AutoSizeToChildren then
  begin
    ResizeWidth := false;
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "AutoSizeToChildren" to change size.');
  end;

  if FullSize then
  begin
    ResizeWidth := false;
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn off "FullSize" to change size.');
  end;

  if WidthFraction <> 0 then
  begin
    ResizeWidth := false;
    Reason := SAppendPart(Reason, NL, 'Set "WidthFraction" to 0 to be able to freely change "Width".');
  end;

  if HeightFraction <> 0 then
  begin
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Set "HeightFraction" to 0 to be able to freely change "Height".');
  end;
end;

function TCastleUserInterface.EffectiveHeightForChildren: Single;
begin
  Result := Max(0, EffectiveHeight - Border.TotalHeight);
end;

function TCastleUserInterface.EffectiveWidthForChildren: Single;
begin
  Result := Max(0, EffectiveWidth - Border.TotalWidth);
end;

function TCastleUserInterface.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FControls);
end;

procedure TCastleUserInterface.Loaded;
begin
  inherited;

  {$warnings off} // using deprecated to warn about it
  if Left <> 0 then
    WritelnWarning('Left is deprecated (on TCastleUserInterface "%s"). Instead: Add it to Translation.X', [
      Name
    ]);
  if Bottom <> 0 then
    WritelnWarning('Bottom is deprecated (on TCastleUserInterface "%s"). Instead: Add it to Translation.Y', [
      Name
    ]);
  {$warnings on}
end;

procedure TCastleUserInterface.InternalSetTooltipExists(const Value: Boolean);
begin
  if TooltipControl <> nil then
    TooltipControl.Exists := Value;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleuserinterface_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
