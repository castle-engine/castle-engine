{
  Copyright 2009-2014 Michalis Kamburelis,
            2015 Tomasz Wojtyś.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ User interface (2D) basic classes: @link(TUIControl) and @link(TUIContainer). }
unit CastleUIControls;

interface

uses SysUtils, Classes, FGL,
  CastleKeysMouse, CastleUtils, CastleClassUtils,
  CastleGenericLists, CastleRectangles, CastleTimeUtils, pk3DConnexion,
  CastleImages, CastleVectors, CastleJoysticks;

const
  { Default value for container's Dpi, as is usually set on desktops. }
  DefaultDpi = 96;
  DefaultTooltipDelay = 1000;
  DefaultTooltipDistance = 10;

type
  { Determines the order in which TUIControl.Render is called.
    All 3D controls are always under all 2D controls.
    See TUIControl.Render, TUIControl.RenderStyle. }
  TRenderStyle = (rs2D, rs3D);

  TUIControl = class;
  TChildrenControls = class;
  TUIContainer = class;

  TContainerEvent = procedure (Container: TUIContainer);
  TContainerObjectEvent = procedure (Container: TUIContainer) of object;
  TInputPressReleaseEvent = procedure (Container: TUIContainer; const Event: TInputPressRelease);
  TInputMotionEvent = procedure (Container: TUIContainer; const Event: TInputMotion);

  TTouch = object
  public
    { Index of the finger/mouse. Always simply zero on traditional desktops with
      just a single mouse device. For devices with multi-touch
      (and/or possible multiple mouse pointers) this is actually useful
      to connect touches from different frames into a single move action.
      In other words: the FinderIndex stays constant while user moves
      a finger over a touch device.

      All the touches on a TUIContainer.Touches array always have different
      FingerIndex value.

      Note that the index of TTouch structure in TUIContainer.Touches array is
      @italic(not) necessarily equal to the FingerIndex. It cannot be ---
      imagine you press 1st finger, then press 2nd finger, then let go of
      the 1st finger. The FingerIndex of the 2nd finger cannot change
      (to keep events sensibly reporting the same touch),
      so there has to be a temporary "hole" in FinderIndex numeration. }
    FingerIndex: TFingerIndex;

    { Position of the touch over a device.

      Position (0, 0) is the window's bottom-left corner.
      This is consistent with how our 2D controls (TUIControl)
      treat all positions.

      The position is expressed as a float value, to support backends
      that can report positions with sub-pixel accuracy.
      For example GTK and Android can do it, although it depends on
      underlying hardware capabilities as well.
      The top-right corner or the top-right pixel has the coordinates
      (Width, Height).
      Note that if you want to actually draw something at the window's
      edge (for example, paint the top-right pixel of the window with some
      color), then the pixel coordinates are (Width - 1, Height - 1).
      The idea is that the whole top-right pixel is an area starting
      in (Width - 1, Height - 1) and ending in (Width, Height).

      Note that we have mouse capturing (when user presses and holds
      the mouse button, all the following mouse events are reported to this
      window, even when user moves the mouse outside of the window).
      This is typical of all window libraries (GTK, LCL etc.).
      This implicates that mouse positions are sometimes tracked also
      when mouse is outside the window, which means that mouse position
      may be outside the rectangle (0, 0) - (Width, Height),
      so it may even be negative. }
    Position: TVector2Single;
  end;
  PTouch = ^TTouch;

  TTouchList = class(specialize TGenericStructList<TTouch>)
  private
    { Find an item with given FingerIndex, or -1 if not found. }
    function FindFingerIndex(const FingerIndex: TFingerIndex): Integer;
    function GetFingerIndexPosition(const FingerIndex: TFingerIndex): TVector2Single;
    procedure SetFingerIndexPosition(const FingerIndex: TFingerIndex;
      const Value: TVector2Single);
  public
    { Gets or sets a position corresponding to given FingerIndex.
      If there is no information for given FingerIndex on the list,
      the getter will return zero, and the setter will automatically create
      and add appropriate information. }
    property FingerIndexPosition[const FingerIndex: TFingerIndex]: TVector2Single
      read GetFingerIndexPosition write SetFingerIndexPosition;
    { Remove a touch item for given FingerIndex. }
    procedure RemoveFingerIndex(const FingerIndex: TFingerIndex);
  end;

  { Possible values for TUIContainer.UIScaling. }
  TUIScaling = (
    { Do not scale UI. }
    usNone,

    { Scale to fake that the container sizes enclose
      @link(TUIContainer.UIReferenceWidth) and
      @link(TUIContainer.UIReferenceHeight).
      So one size will be equal to reference size, and the other will be equal
      or larger to reference.

      Controls that look at @link(TUIControl.UIScale) will be affected by this.
      Together with anchors (see @link(TUIControl.HasHorizontalAnchor)
      and friends), this allows to easily design a scalable UI. }
    usEncloseReferenceSize,

    { Scale to fake that the container sizes fit inside
      @link(TUIContainer.UIReferenceWidth) and
      @link(TUIContainer.UIReferenceHeight).
      So one size will be equal to reference size, and the other will be equal
      or smaller to reference.

      Controls that look at @link(TUIControl.UIScale) will be affected by this.
      Together with anchors (see @link(TUIControl.HasHorizontalAnchor)
      and friends), this allows to easily design a scalable UI. }
    usFitReferenceSize,

    { Scale to fake that the container sizes are smaller/larger
      by an explicit factor @link(TUIContainer.UIExplicitScale).
      Controls that look at @link(TUIControl.UIScale) will be affected by this.

      Like usEncloseReferenceSize or usFitReferenceSize,
      this allows to design a scalable UI.
      In this case, the scale factor has to be calculated by your code
      (by default @link(TUIContainer.UIExplicitScale) is 1.0 and the engine will
      not modify it automatically in any way),
      which allows customizing the scale to your requirements.

      For example derive the @link(TUIContainer.UIExplicitScale) from
      the @link(TUIContainer.Dpi) value to keep UI controls at constant
      @italic(physical) (real-world) size, at least on devices where
      @link(TUIContainer.Dpi) is a real value (right now: iOS). }
    usExplicitScale
  );

  { Abstract user interface container. Connects OpenGL context management
    code with Castle Game Engine controls (TUIControl, that is the basis
    for all our 2D and 3D rendering). When you use TCastleWindowCustom
    (a window) or TCastleControlCustom (Lazarus component), they provide
    you a non-abstact implementation of TUIContainer.

    Basically, this class manages a @link(Controls) list.

    We pass our inputs (mouse / key / touch events) to the controls
    on this list. Input goes to the front-most
    (that is, last on the @link(Controls) list) control under
    the event position (or mouse position, or the appropriate touch position).
    We use @link(TUIControl.CapturesEventsAtPosition) to decide this
    (by default it simply checks control's @link(TUIControl.ScreenRect)
    vs the given position).
    As long as the event is not handled,
    we search for the next control that can handle this event and
    returns @link(TUIControl.CapturesEventsAtPosition) = @true.

    We also call various methods to every control.
    These include @link(TUIControl.Update), @link(TUIControl.Render),
    @link(TUIControl.Resize). }
  TUIContainer = class abstract(TComponent)
  private
    type
      TFingerIndexCaptureMap = specialize TFPGMap<TFingerIndex, TUIControl>;
    var
    FOnOpen, FOnClose: TContainerEvent;
    FOnOpenObject, FOnCloseObject: TContainerObjectEvent;
    FOnBeforeRender, FOnRender: TContainerEvent;
    FOnResize: TContainerEvent;
    FOnPress, FOnRelease: TInputPressReleaseEvent;
    FOnMotion: TInputMotionEvent;
    FOnUpdate: TContainerEvent;
    { FControls cannot be declared as TChildrenControls to avoid
      http://bugs.freepascal.org/view.php?id=22495 }
    FControls: TObject;
    FRenderStyle: TRenderStyle;
    FFocus: TUIControl;
    { Capture controls, for each FingerIndex.
      The values in this map are never nil. }
    FCaptureInput: TFingerIndexCaptureMap;
    FForceCaptureInput: TUIControl;
    FTooltipDelay: TMilisecTime;
    FTooltipDistance: Cardinal;
    FTooltipVisible: boolean;
    FTooltipPosition: TVector2Single;
    HasLastPositionForTooltip: boolean;
    LastPositionForTooltip: TVector2Single;
    LastPositionForTooltipTime: TTimerResult;
    Mouse3d: T3DConnexionDevice;
    Mouse3dPollTimer: Single;
    FUIScaling: TUIScaling;
    FUIReferenceWidth: Integer;
    FUIReferenceHeight: Integer;
    FUIExplicitScale: Single;
    FCalculatedUIScale: Single; //< set on all children
    procedure ControlsVisibleChange(Sender: TObject);
    { Called when the control C is destroyed or just removed from Controls list. }
    procedure DetachNotification(const C: TUIControl);
    function UseForceCaptureInput: boolean;
    procedure SetUIScaling(const Value: TUIScaling);
    procedure SetUIReferenceWidth(const Value: Integer);
    procedure SetUIReferenceHeight(const Value: Integer);
    procedure SetUIExplicitScale(const Value: Single);
    procedure UpdateUIScale;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { These should only be get/set by a container provider,
      like TCastleWindow or TCastleControl.
      @groupBegin }
    property OnOpen: TContainerEvent read FOnOpen write FOnOpen;
    property OnOpenObject: TContainerObjectEvent read FOnOpenObject write FOnOpenObject;
    property OnBeforeRender: TContainerEvent read FOnBeforeRender write FOnBeforeRender;
    property OnRender: TContainerEvent read FOnRender write FOnRender;
    property OnResize: TContainerEvent read FOnResize write FOnResize;
    property OnClose: TContainerEvent read FOnClose write FOnClose;
    property OnCloseObject: TContainerObjectEvent read FOnCloseObject write FOnCloseObject;
    property OnPress: TInputPressReleaseEvent read FOnPress write FOnPress;
    property OnRelease: TInputPressReleaseEvent read FOnRelease write FOnRelease;
    property OnMotion: TInputMotionEvent read FOnMotion write FOnMotion;
    property OnUpdate: TContainerEvent read FOnUpdate write FOnUpdate;
    { @groupEnd }

    procedure SetCursor(const Value: TMouseCursor); virtual; abstract;
    property Cursor: TMouseCursor write SetCursor;

    function GetMousePosition: TVector2Single; virtual; abstract;
    procedure SetMousePosition(const Value: TVector2Single); virtual; abstract;
    function GetTouches(const Index: Integer): TTouch; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Propagate the event to all the @link(Controls) and to our own OnXxx callbacks.
      Usually these are called by a container provider,
      like TCastleWindow or TCastleControl. But it is also allowed to call them
      manually to fake given event.
      @groupBegin }
    procedure EventOpen(const OpenWindowsCount: Cardinal); virtual;
    procedure EventClose(const OpenWindowsCount: Cardinal); virtual;
    function EventPress(const Event: TInputPressRelease): boolean; virtual;
    function EventRelease(const Event: TInputPressRelease): boolean; virtual;
    procedure EventUpdate; virtual;
    procedure EventMotion(const Event: TInputMotion); virtual;
    function AllowSuspendForInput: boolean;
    procedure EventBeforeRender; virtual;
    procedure EventRender; virtual; abstract;
    procedure EventResize; virtual;
    { @groupEnd }

    { Controls listening for events (user input, resize, and such) of this container.

      Usually you explicitly add / delete controls to this list.
      Also, freeing the control that is on this list
      automatically removes it from this list (using the TComponent.Notification
      mechanism).

      Controls on the list should be specified in back-to-front order.
      That is, controls at the beginning of this list
      are rendered first, and are last to catch some events, since the rest
      of controls covers them. }
    function Controls: TChildrenControls;

    { Returns the control that should receive input events first,
      or @nil if none. More precisely, this is the first on Controls
      list that is enabled and under the mouse cursor.
      @nil is returned when there's no enabled control under the mouse cursor. }
    property Focus: TUIControl read FFocus;

    { When the tooltip should be shown (mouse hovers over a control
      with a tooltip) then the TooltipVisible is set to @true,
      and TooltipPosition indicate left-bottom suggested position
      of the tooltip.

      The tooltip is only detected when TUIControl.TooltipExists.
      See TUIControl.TooltipExists and TUIControl.TooltipStyle and
      TUIControl.TooltipRender.
      For simple purposes just set TUIControlFont.Tooltip to something
      non-empty.
      @groupBegin }
    property TooltipVisible: boolean read FTooltipVisible;
    property TooltipPosition: TVector2Single read FTooltipPosition;
    { @groupEnd }

    { Redraw the contents of of this window, at the nearest good time.
      The redraw will not happen immediately, we will only "make a note"
      that we should do it soon.
      Redraw means that we call EventBeforeRender (OnBeforeRender), EventRender
      (OnRender), then we flush OpenGL commands, swap buffers etc.

      Calling this on a closed container (with GLInitialized = @false)
      is allowed and ignored. }
    procedure Invalidate; virtual; abstract;

    { Is the OpenGL context initialized. }
    function GLInitialized: boolean; virtual; abstract;

    function Width: Integer; virtual; abstract;
    function Height: Integer; virtual; abstract;
    function Rect: TRectangle; virtual; abstract;

    property MousePosition: TVector2Single
      read GetMousePosition write SetMousePosition;

    function Dpi: Integer; virtual; abstract;

    { Mouse buttons currently pressed. }
    function MousePressed: TMouseButtons; virtual; abstract;

    { Is the window focused now, which means that keys/mouse events
      are directed to this window. }
    function Focused: boolean; virtual; abstract;

    { Keys currently pressed. }
    function Pressed: TKeysPressed; virtual; abstract;

    function Fps: TFramesPerSecond; virtual; abstract;

    property Touches[Index: Integer]: TTouch read GetTouches;
    function TouchesCount: Integer; virtual; abstract;

    { Called by controls within this container when something could
      change the container focused control (or it's cursor) or Focused or MouseLook.
      In practice, called when TUIControl.Cursor or
      @link(TUIControl.CapturesEventsAtPosition) (and so also
      @link(TUIControl.ScreenRect)) results change.

      This recalculates the focused control and the final cursor of
      the container, looking at Container's Controls,
      testing @link(TUIControl.CapturesEventsAtPosition) with current mouse position,
      and looking at Cursor property of the focused control.

      When you add / remove some control
      from the Controls list, or when you move mouse (focused changes)
      this will also be automatically called
      (since focused control or final container cursor may also change then). }
    procedure UpdateFocusAndMouseCursor;

    { Internal for implementing mouse look in cameras. }
    function IsMousePositionForMouseLook: boolean;
    { Internal for implementing mouse look in cameras. }
    procedure MakeMousePositionForMouseLook;

    { Force passing events to given control first, regardless if this control is under mouse cursor.
      This control also always has focus.

      An example when this is useful is when you use camera MouseLook,
      and the associated viewport does not fill the full window
      (TCastleAbstractViewport.FullSize is @false, and actual sizes are smaller
      than window, and may not include window center). In this case you want
      to make sure that motion events get passed to this control,
      and that this control has focus (to keep mouse cursor hidden).

      This is used only if it is also present on our @link(Controls) list,
      as it doesn't make sense otherwise.
      We also cannot reliably track it's existence when it's outside our @link(Controls) list
      (and we don't want to eagerly @nil this property automatically). }
    property ForceCaptureInput: TUIControl
      read FForceCaptureInput write FForceCaptureInput;
  published
    { How OnRender callback fits within various Render methods of our
      @link(Controls).

      @unorderedList(
        @item(rs2D means that OnRender is called at the end,
          after all our @link(Controls) (3D and 2D) are drawn.)

        @item(rs3D means that OnRender is called after all other
          @link(Controls) with rs3D draw style, but before any 2D
          controls.

          This is suitable if you want to draw something 3D,
          that may be later covered by 2D controls.)
      )
    }
    property RenderStyle: TRenderStyle
      read FRenderStyle write FRenderStyle default rs2D;

    property TooltipDelay: TMilisecTime read FTooltipDelay write FTooltipDelay
      default DefaultTooltipDelay;
    property TooltipDistance: Cardinal read FTooltipDistance write FTooltipDistance
      default DefaultTooltipDistance;

    { Enable automatic scaling of the UI.
      This is great when the container size may vary widly (for example,
      on mobile devices, although it becomes more and more sensible for desktops
      too). See @link(TUIScaling) values for precise description how it works. }
    property UIScaling: TUIScaling
      read FUIScaling write SetUIScaling default usNone;

    { Reference width and height to which we fit the container size
      (as seen by TUIControl implementations) when UIScaling is
      usEncloseReferenceSize or usFitReferenceSize.
      See @link(usEncloseReferenceSize) and @link(usFitReferenceSize)
      for precise description how this works.
      Set both these properties, or set only one (and leave the other as zero).
      @groupBegin }
    property UIReferenceWidth: Integer
      read FUIReferenceWidth write SetUIReferenceWidth default 0;
    property UIReferenceHeight: Integer
      read FUIReferenceHeight write SetUIReferenceHeight default 0;
    { @groupEnd }

    { Scale of the container size (as seen by TUIControl implementations)
      when UIScaling is usExplicitScale.
      See @link(usExplicitScale) for precise description how this works. }
    property UIExplicitScale: Single
      read FUIExplicitScale write SetUIExplicitScale default 1.0;
  end;

  { Base class for things that listen to user input: cameras and 2D controls. }

  { TInputListener }

  TInputListener = class(TComponent)
  private
    FOnVisibleChange: TNotifyEvent;
    FContainer: TUIContainer;
    FCursor: TMouseCursor;
    FOnCursorChange: TNotifyEvent;
    FExclusiveEvents: boolean;
    procedure SetCursor(const Value: TMouseCursor);
  protected
    { Container sizes.
      @groupBegin }
    function ContainerWidth: Cardinal;
    function ContainerHeight: Cardinal;
    function ContainerRect: TRectangle;
    function ContainerSizeKnown: boolean;
    { @groupEnd }

    procedure SetContainer(const Value: TUIContainer); virtual;
    { Called when @link(Cursor) changed.
      In TUIControl class, just calls OnCursorChange. }
    procedure DoCursorChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    (*Handle press or release of a key, mouse button or mouse wheel.
      Return @true if the event was somehow handled.

      In this class this always returns @false, when implementing
      in descendants you should override it like

      @longCode(#
  Result := inherited;
  if Result then Exit;
  { ... And do the job here.
    In other words, the handling of events in inherited
    class should have a priority. }
#)

      Note that releasing of mouse wheel is not implemented for now,
      neither by CastleWindow or Lazarus CastleControl.
      @groupBegin *)
    function Press(const Event: TInputPressRelease): boolean; virtual;
    function Release(const Event: TInputPressRelease): boolean; virtual;
    { @groupEnd }

    { Motion of mouse or touch. }
    function Motion(const Event: TInputMotion): boolean; virtual;

    { Rotation detected by sensor.
      Used for example by 3Dconnexion devices or touch controls.

      @param X   X axis (tilt forward/backwards)
      @param Y   Y axis (rotate)
      @param Z   Z axis (tilt sidewards)
      @param Angle   Angle of rotation
      @param(SecondsPassed The time passed since last SensorRotation call.
        This is necessary because some sensors, e.g. 3Dconnexion,
        may *not* reported as often as normal @link(Update) calls.) }
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; virtual;

    { Translation detected by sensor.
      Used for example by 3Dconnexion devices or touch controls.

      @param X   X axis (move left/right)
      @param Y   Y axis (move up/down)
      @param Z   Z axis (move forward/backwards)
      @param Length   Length of the vector consisting of the above
      @param(SecondsPassed The time passed since last SensorRotation call.
        This is necessary because some sensors, e.g. 3Dconnexion,
        may *not* reported as often as normal @link(Update) calls.) }
    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; virtual;

    { Axis movement detected by joystick

      @param JoyID ID of joystick with pressed button
      @param Axis Number of moved axis }
    function JoyAxisMove(const JoyID, Axis: Byte): boolean; virtual;

    { Joystick button pressed.

      @param JoyID ID of joystick with pressed button
      @param Button Number of pressed button }
    function JoyButtonPress(const JoyID, Button: Byte): boolean; virtual;

    { Control may do here anything that must be continously repeated.
      E.g. camera handles here falling down due to gravity,
      rotating model in Examine mode, and many more.

      @param(SecondsPassed Should be calculated like TFramesPerSecond.UpdateSecondsPassed,
        and usually it's in fact just taken from TCastleWindowCustom.Fps.UpdateSecondsPassed.)

      This method may be used, among many other things, to continously
      react to the fact that user pressed some key (or mouse button).
      For example, if holding some key should move some 3D object,
      you should do something like:

@longCode(#
if HandleInput then
begin
  if Container.Pressed[K_Right] then
    Transform.Position += Vector3Single(SecondsPassed * 10, 0, 0);
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
      only for continous handling (like "holding this key makes
      the light brighter and brighter").

      To understand why such HandleInput approach is needed,
      realize that the "Update" events are called
      differently than simple mouse and key events like "Press" and "Release".
      "Press" and "Release" events
      return whether the event was somehow "handled", and the container
      passes them only to the controls under the mouse (decided by
      @link(TUIControl.CapturesEventsAtPosition)). And as soon as some control says it "handled"
      the event, other controls (even if under the mouse) will not
      receive the event.

      This approach is not suitable for Update events. Some controls
      need to do the Update job all the time,
      regardless of whether the control is under the mouse and regardless
      of what other controls already did. So all controls (well,
      all controls that exist, in case of TUIControl,
      see TUIControl.GetExists) receive Update calls.

      So the "handled" status is passed through HandleInput.
      If a control is not under the mouse, it will receive HandleInput
      = @false. If a control is under the mouse, it will receive HandleInput
      = @true as long as no other control on top of it didn't already
      change it to @false. }
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); virtual;

    { Called always when some visible part of this control
      changes. In the simplest case, this is used by the controls manager to
      know when we need to redraw the control.

      In this class this simply calls OnVisibleChange (if assigned). }
    procedure VisibleChange; virtual;

    { Called always when some visible part of this control
      changes. In the simplest case, this is used by the controls manager to
      know when we need to redraw the control.

      Be careful when handling this event, various changes may cause this,
      so be prepared to handle OnVisibleChange at every time.

      @seealso VisibleChange }
    property OnVisibleChange: TNotifyEvent
      read FOnVisibleChange write FOnVisibleChange;

    { Allow window containing this control to suspend waiting for user input.
      Typically you want to override this to return @false when you do
      something in the overridden @link(Update) method.

      In this class, this simply returns always @true.

      @seeAlso TCastleWindowCustom.AllowSuspendForInput }
    function AllowSuspendForInput: boolean; virtual;

    { You can resize/reposition your component here,
      for example set @link(TUIControl.Left) or @link(TUIControl.Bottom), to react to parent
      size changes.
      Called always when the container (component or window with OpenGL context)
      size changes. Called only when the OpenGL context of the container
      is initialized, so you can be sure that this is called only between
      GLContextOpen and GLContextClose.

      We also make sure to call this once when inserting into
      the controls list
      (like @link(TCastleWindowCustom.Controls) or
      @link(TCastleControlCustom.Controls) or inside parent TUIControl),
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
      list (like TCastleWindowCustom.Controls) container will automatically
      set itself here, and when removing from container this will be changed
      back to @nil.

      May be @nil if this control is not yet inserted into any container. }
    property Container: TUIContainer read FContainer write SetContainer;

    { Mouse cursor over this control.
      When user moves mouse over the Container, the currently focused
      (topmost under the cursor) control determines the mouse cursor look. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Event called when the @link(Cursor) property changes.
      This event is, in normal circumstances, used by the Container,
      so you should not use it in your own programs. }
    property OnCursorChange: TNotifyEvent
      read FOnCursorChange write FOnCursorChange;

    { Design note: ExclusiveEvents is not published now, as it's too "obscure"
      (for normal usage you don't want to deal with it). Also, it's confusing
      on TCastleSceneCore, the name suggests it relates to ProcessEvents (VRML events,
      totally not related to this property that is concerned with handling
      TUIControl events.) }

    { Should we disable further mouse / keys handling for events that
      we already handled in this control. If @true, then our events will
      return @true for mouse and key events handled.

      This means that events will not be simultaneously handled by both this
      control and some other (or camera or normal window callbacks),
      which is usually more sensible, but sometimes somewhat limiting. }
    property ExclusiveEvents: boolean
      read FExclusiveEvents write FExclusiveEvents default true;
  end;

  { Position for relative layout of one control in respect to another.
    @deprecated Deprecated, rather use cleaner
    THorizontalPosition and TVerticalPosition.
  }
  TPositionRelative = (
    prLow,
    prMiddle,
    prHigh
  ) deprecated;

  TUIControlList = class;

  { Basic 2D control class. All controls derive from this class,
    overriding chosen methods to react to some events.
    Various user interface containers (things that directly receive messages
    from something outside, like operating system, windowing library etc.)
    implement support for such controls.

    Control has children controls, see @link(Controls) and @link(ControlsCount).
    Parent control is recorded inside @link(Parent). A control
    may only be a child of one other control --- that is, you cannot
    insert to the 2D hierarchy the same control multiple times
    (in T3D hierarchy, such trick is allowed).

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

    Every control also has a position and takes some rectangular space
    on the container.

    The position is controlled using the @link(Left), @link(Bottom) fields.
    The rectangle where the control is visible can be queried using
    the @link(Rect) or @link(ScreenRect) methods.

    Note that each descendant has it's own definition of the size of the control.
    E.g. some descendants may automatically calculate the size
    (based on text or images or such placed within the control).
    Some descendants may allow to control the size explicitly
    using fields like Width, Height, FullSize.
    Some descendants may allow both approaches, switchable by
    property like TCastleButton.AutoSize or TCastleImageControl.Stretch.

    All screen (mouse etc.) coordinates passed here should be in the usual
    window system coordinates, that is (0, 0) is left-top window corner.
    (Note that this is contrary to the usual OpenGL 2D system,
    where (0, 0) is left-bottom window corner.) }
  TUIControl = class(TInputListener)
  private
    FDisableContextOpenClose: Cardinal;
    FFocused: boolean;
    FGLInitialized: boolean;
    FExists: boolean;
    FRenderStyle: TRenderStyle;
    FControls: TChildrenControls;
    FLeft: Integer;
    FBottom: Integer;
    FParent: TUIControl; //< null means that parent is our owner
    FHasHorizontalAnchor: boolean;
    FHorizontalAnchor: THorizontalPosition;
    FHorizontalAnchorDelta: Integer;
    FHasVerticalAnchor: boolean;
    FVerticalAnchor: TVerticalPosition;
    FVerticalAnchorDelta: Integer;
    FEnableUIScaling: boolean;
    procedure SetExists(const Value: boolean);
    function GetControls(const I: Integer): TUIControl;
    procedure SetControls(const I: Integer; const Item: TUIControl);
    procedure CreateControls;

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

    procedure SetLeft(const Value: Integer);
    procedure SetBottom(const Value: Integer);

    procedure SetHasHorizontalAnchor(const Value: boolean);
    procedure SetHorizontalAnchor(const Value: THorizontalPosition);
    procedure SetHorizontalAnchorDelta(const Value: Integer);
    procedure SetHasVerticalAnchor(const Value: boolean);
    procedure SetVerticalAnchor(const Value: TVerticalPosition);
    procedure SetVerticalAnchorDelta(const Value: Integer);
    function RectWithAnchors: TRectangle;
    procedure SetEnableUIScaling(const Value: boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetContainer(const Value: TUIContainer); override;

    { UI scale of this control, derived from container
      (see @link(TUIContainer.UIScaling).

      All the drawing and measuring inside your control must take this into
      account. The final @link(Rect) result must already take this scaling
      into account, so that parent controls may depend on it. In the simplest
      case, your @code(Rect) should return

@longCode(#
function TMyControl.Rect: TRectangle;
begin
  Result := Rectangle(Left, Bottom, Width, Height).ScaleAround0(UIScale);
end;
#)

      This is so simple only when your drawing code 100% adjusts
      the drawn contents to the passed @link(ScreenRect). It's not always
      that easy, that's why we don't do it automatically in the engine. }
    function UIScale: Single;

    { The left-bottom corner scaled by UIScale,
      useful for implementing overridden @code(Rect) methods. }
    function LeftBottomScaled: TVector2Integer;

    procedure UIScaleChanged; virtual;

    //procedure DoCursorChange; override;

    { Keep the control in front of other controls (with KeepInFront=@false)
      when inserting. TODO: This is more a hack than a nice solution.
      It also assumes that the result is constant for given instance lifetime. }
    function KeepInFront: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Controls [Index: Integer]: TUIControl read GetControls write SetControls;
    function ControlsCount: Integer;

    { Add child control, at the front of other children. }
    procedure InsertFront(const NewItem: TUIControl);
    procedure InsertFrontIfNotExists(const NewItem: TUIControl);
    procedure InsertFront(const NewItems: TUIControlList);

    { Add child control, at the back of other children. }
    procedure InsertBack(const NewItem: TUIControl);
    procedure InsertBackIfNotExists(const NewItem: TUIControl);
    procedure InsertBack(const NewItems: TUIControlList);

    { Remove control added by @link(InsertFront) or @link(InsertBack). }
    procedure RemoveControl(Item: TUIControl);

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; override;
    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    //procedure VisibleChange; override;
    { }
    function AllowSuspendForInput: boolean; override;
    procedure Resize; override;

    { Return whether item really exists, see @link(Exists).
      Non-existing item does not receive any of the render or input or update calls.
      They only receive @link(GLContextOpen), @link(GLContextClose), @link(Resize)
      calls.

      It TUIControl class, this returns the value of @link(Exists) property.
      May be overridden in descendants, to return something more complicated,
      but it should always be a logical "and" with the inherited @link(GetExists)
      implementation (so setting the @code(Exists := false) will always work),
      like

@longCode(#
  Result := (inherited GetExists) and MyComplicatedConditionForExists;
#) }
    function GetExists: boolean; virtual;

    { Does this control capture events under this screen position.
      The default implementation simply checks whether Position
      is inside ScreenRect now.

      Always treated like @false when GetExists returns @false,
      so the implementation of this method only needs to make checks assuming that
      GetExists = @true.  }
    function CapturesEventsAtPosition(const Position: TVector2Single): boolean; virtual;

    { Prepare your resources, right before drawing.
      Called only when @link(GetExists) and GLInitialized. }
    procedure BeforeRender; virtual;

    { Render a control. Called only when @link(GetExists) and GLInitialized,
      you can depend on it in the implementation of this method.

      Before calling this method we always set some OpenGL state,
      and you can depend on it (and you can carelessly change it,
      as it will be reset again before rendering other control).
      (In Castle Game Engine < 5.1.0, the rules were more complicated
      and depending on RenderStyle. This is no longer the case,
      RenderStyle now determines only the render order,
      allowing TCastleSceneManager to be used in the middle of 2D controls.)

      OpenGL state always set:

      @unorderedList(
        @item(@italic((For fixed-function pipeline.))
          The 2D orthographic projection is always set at the beginning.
          Useful for 2D controls, 3D controls can just override projection
          matrix, e.g. use @link(PerspectiveProjection).)

        @item(glViewport is set to include whole container.)

        @item(@italic((For fixed-function pipeline.))
          The modelview matrix is set to identity. The matrix mode
          is always identity.)

        @item(The raster position @italic((for fixed-function pipeline.))
          and (deprecated) WindowPos are set to 0,0.)

        @item(Scissor is off, depth test is off.)

        @item(@italic((For fixed-function pipeline.))
          Texturing, lighting, fog is off.)
      ) }
    procedure Render; virtual;

    { Determines the rendering order.
      All controls with RenderStyle = rs3D are drawn first.
      Then all the controls with RenderStyle = rs2D are drawn.
      Among the controls with equal RenderStyle, their order
      on TUIContainer.Controls list determines the rendering order. }
    property RenderStyle: TRenderStyle read FRenderStyle write FRenderStyle default rs2D;

    { Render a tooltip of this control. If you want to have tooltip for
      this control detected, you have to override TooltipExists.
      Then the TCastleWindowCustom.TooltipVisible will be detected,
      and your TooltipRender will be called.

      The values of rs2D and rs3D are interpreted in the same way
      as RenderStyle. And TooltipRender is called in the same way as @link(Render),
      so e.g. you can safely assume that modelview matrix is identity
      and (for 2D) WindowPos is zero.
      TooltipRender is always called as a last (front-most) 2D or 3D control.

      @groupBegin }
    function TooltipStyle: TRenderStyle; virtual;
    function TooltipExists: boolean; virtual;
    procedure TooltipRender; virtual;
    { @groupEnd }

    { Initialize your OpenGL resources.

      This is called when OpenGL context of the container is created,
      or when the control is added to the already existing context.
      In other words, this is the moment when you can initialize
      OpenGL resources, like display lists, VBOs, OpenGL texture names, etc.

      As an exception, this is called regardless of the GetExists value.
      This way a control can prepare it's resources, regardless if it exists now. }
    procedure GLContextOpen; virtual;

    { Destroy your OpenGL resources.

      Called when OpenGL context of the container is destroyed.
      Also called when controls is removed from the container
      @code(Controls) list. Also called from the destructor.

      You should release here any resources that are tied to the
      OpenGL context. In particular, the ones created in GLContextOpen.

      As an exception, this is called regardless of the GetExists value.
      This way a control can release it's resources, regardless if it exists now. }
    procedure GLContextClose; virtual;

    property GLInitialized: boolean read FGLInitialized default false;

    { When non-zero, control will not receive GLContextOpen and
      GLContextClose events when it is added/removed from the
      @link(TUIContainer.Controls) list.

      This can be useful as an optimization, to keep the OpenGL resources
      created even for controls that are not present on the
      @link(TUIContainer.Controls) list. @italic(This must used
      very, very carefully), as bad things will happen if the actual OpenGL
      context will be destroyed while the control keeps the OpenGL resources
      (because it had DisableContextOpenClose > 0). The control will then
      remain having incorrect OpenGL resource handles, and will try to use them,
      causing OpenGL errors or at least weird display artifacts.

      Most of the time, when you think of using this, you should instead
      use the @link(TUIControl.Exists) property. This allows you to keep the control
      of the @link(TUIContainer.Controls) list, and it will be receive
      GLContextOpen and GLContextClose events as usual, but will not exist
      for all other purposes.

      Using this mechanism is only sensible if you want to reliably hide a control,
      but also allow readding it to the @link(TUIContainer.Controls) list,
      and then you want to show it again. This is useful for CastleWindowModes,
      that must push (and then pop) the controls, but then allows the caller
      to modify the controls list. And some games, e.g. castle1, add back
      some (but not all) of the just-hidden controls. For example the TCastleNotifications
      instance is added back, to be visible even in the menu mode.
      This means that CastleWindowModes cannot just modify the TUIContainer.Exists
      value, leaving the control on the @link(TUIContainer.Controls) list:
      it would leave the TUIControl existing many times on the @link(TUIContainer.Controls)
      list, with the undefined TUIContainer.Exists value. }
    property DisableContextOpenClose: Cardinal
      read FDisableContextOpenClose write FDisableContextOpenClose;

   { Called when this control becomes or stops being focused.
      In this class, they simply update Focused property. }
    procedure SetFocused(const Value: boolean); virtual;

    property Focused: boolean read FFocused write SetFocused;

    property Parent: TUIControl read FParent;

    { Position and size of this control, assuming it exists,
      in local coordinates (relative to parent 2D control).
      This must ignore the current value of the @link(GetExists) method
      and @link(Exists) property, that is: the result of this function
      assumes that control does exist.

      In this class, returns empty rectangle (zero width and height) with
      Left and Bottom correctly set . }
    function Rect: TRectangle; virtual;

    { Position and size of this control, assuming it exists, in screen (container)
      coordinates. }
    function ScreenRect: TRectangle;

    { How to translate local coords to screen. }
    function LocalToScreenTranslation: TVector2Integer;

    { Rectangle filling the parent control (or coordinates), in local coordinates.
      Since this is in local coordinates, the returned rectangle Left and Bottom
      are always zero. }
    function ParentRect: TRectangle;

    { Quick way to enable horizontal anchor, to automatically keep this
      control aligned to parent. Sets @link(HasHorizontalAnchor),
      @link(HorizontalAnchor), @link(HorizontalAnchorDelta). }
    procedure Anchor(const AHorizontalAnchor: THorizontalPosition;
      const AHorizontalAnchorDelta: Integer = 0);

    { Quick way to enable vertical anchor, to automatically keep this
      control aligned to parent. Sets @link(HasVerticalAnchor),
      @link(VerticalAnchor), @link(VerticalAnchorDelta). }
    procedure Anchor(const AVerticalAnchor: TVerticalPosition;
      const AVerticalAnchorDelta: Integer = 0);

    { Manually position the control with respect to the parent
      by adjusting @link(Left).
      Deprecated, use @link(Align) with THorizontalPosition. }
    procedure AlignHorizontal(
      const ControlPosition: TPositionRelative = prMiddle;
      const ContainerPosition: TPositionRelative = prMiddle;
      const X: Integer = 0); deprecated 'use Align, or use even simpler HasHorizontalAnchor, HorizontalAnchor, HorizontalAnchorDelta';

    { Manually position the control with respect to the parent
      by adjusting @link(Left).

      Note that in simple cases, you can achieve the same functionality
      by HasHorizontalAnchor, HorizontalAnchor, HorizontalAnchorDelta. }
    procedure Align(
      const ControlPosition: THorizontalPosition;
      const ContainerPosition: THorizontalPosition;
      const X: Integer = 0);

    { Manually position the control with respect to the parent
      by adjusting @link(Bottom).
      Deprecated, use @link(Align) with TVerticalPosition. }
    procedure AlignVertical(
      const ControlPosition: TPositionRelative = prMiddle;
      const ContainerPosition: TPositionRelative = prMiddle;
      const Y: Integer = 0); deprecated 'use Align, or use even simpler HasVerticalAnchor, VerticalAnchor, VerticalAnchorDelta';

    { Manually position the control with respect to the parent
      by adjusting @link(Bottom).

      Note that in simple cases, you can achieve the same functionality
      by HasVerticalAnchor, VerticalAnchor, VerticalAnchorDelta. }
    procedure Align(
      const ControlPosition: TVerticalPosition;
      const ContainerPosition: TVerticalPosition;
      const Y: Integer = 0);

    { Manually center the control within the parent,
      both horizontally and vertically.

      Note that in simple cases, you can achieve the same functionality
      by HasHorizontalAnchor, HorizontalAnchor, HasVerticalAnchor, VerticalAnchor. }
    procedure Center;
  published
    { Not existing control is not visible, it doesn't receive input
      and generally doesn't exist from the point of view of user.
      You can also remove this from controls list (like
      @link(TCastleWindowCustom.Controls)), but often it's more comfortable
      to set this property to false. }
    property Exists: boolean read FExists write SetExists default true;

    property Left: Integer read FLeft write SetLeft stored false default 0;
    property Bottom: Integer read FBottom write SetBottom default 0;

    { Automatically adjust horizontal position to align us to
      the parent horizontally. Note that the value of @link(Left) remains
      unchanged (it is just ignored), using the anchors only modifies the output
      of the @link(ScreenRect) value that should be used for rendering/physics.

      @italic(Anchor distance is automatically affected by @link(TUIContainer.UIScaling).) }
    property HasHorizontalAnchor: boolean
      read FHasHorizontalAnchor write SetHasHorizontalAnchor default false;
    { Which border to align (both our and parent border),
      only used if @link(HasHorizontalAnchor). }
    property HorizontalAnchor: THorizontalPosition
      read FHorizontalAnchor write SetHorizontalAnchor default hpLeft;
    { Delta between our border and parent,
      only used if @link(HasHorizontalAnchor). }
    property HorizontalAnchorDelta: Integer
      read FHorizontalAnchorDelta write SetHorizontalAnchorDelta default 0;

    { Automatically adjust vertical position to align us to
      the parent vertically. Note that the value of @link(Bottom) remains
      unchanged (it is tjust ignored), using the anchors only modifies the output
      of the @link(ScreenRect) value that should be used for rendering/physics.

      @italic(Anchor distance is automatically affected by @link(TUIContainer.UIScaling).) }
    property HasVerticalAnchor: boolean
      read FHasVerticalAnchor write SetHasVerticalAnchor default false;
    { Which border to align (both our and parent border),
      only used if @link(HasVerticalAnchor). }
    property VerticalAnchor: TVerticalPosition
      read FVerticalAnchor write SetVerticalAnchor default vpBottom;
    { Delta between our border and parent,
      only used if @link(HasVerticalAnchor). }
    property VerticalAnchorDelta: Integer
      read FVerticalAnchorDelta write SetVerticalAnchorDelta default 0;

    { Enable or disable UI scaling for this particular control.
      See more about UI scaling on @link(TUIContainer.UIScaling) and
      @link(TUIControl.UIScale). Setting this to @false forces
      @link(TUIControl.UIScale) to always return 1.0.

      Note that this does not work recursively, i.e. it does not affect
      the children of this control. Setting this to @false does not prevent
      UI scaling on children (you have to turn it off explicitly for children too,
      if you need to disable UI scaling recursively). }
    property EnableUIScaling: boolean
      read FEnableUIScaling write SetEnableUIScaling default true;
  end;

  { Simple list of TUIControl instances. }
  TUIControlList = class(specialize TFPGObjectList<TUIControl>)
  public
    { Add child control, at the front of other children. }
    procedure InsertFront(const NewItem: TUIControl);
    procedure InsertFrontIfNotExists(const NewItem: TUIControl);
    procedure InsertFront(const NewItems: TUIControlList);

    { Add child control, at the back of other children. }
    procedure InsertBack(const NewItem: TUIControl);
    procedure InsertBackIfNotExists(const NewItem: TUIControl);
    procedure InsertBack(const NewItems: TUIControlList);

    { Insert, honoring @link(TUIControl.KeepInFront). }
    procedure InsertWithZOrder(Index: Integer; const Item: TUIControl);
  end;

  TUIControlClass = class of TUIControl;

  TUIControlPos = TUIControl deprecated 'use TUIControl class';
  TUIRectangularControl = TUIControl deprecated 'use TUIControl class';

  { List of UI controls, with a parent control and container.
    Ordered from back to front.
    Used for @link(TUIContainer.Controls). }
  TChildrenControls = class
  private
    FParent: TUIControl;
    FContainer: TUIContainer;

    procedure RegisterContainer(const C: TUIControl; const AContainer: TUIContainer);
    procedure UnregisterContainer(const C: TUIControl; const AContainer: TUIContainer);
    procedure SetContainer(const AContainer: TUIContainer);
    property Container: TUIContainer read FContainer write SetContainer;

    type
      TMyObjectList = class(TCastleObjectList)
        Parent: TChildrenControls;
        { Pass notifications to Parent. }
        procedure Notify(Ptr: Pointer; Action: TListNotification); override;
      end;
      TCaptureFreeNotifications = class(TComponent)
      protected
        Parent: TChildrenControls;
        { Pass notifications to Parent. }
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      end;
    var
      FList: TMyObjectList;
      FCaptureFreeNotifications: TCaptureFreeNotifications;

    {$ifndef VER2_6}
    { Using this causes random crashes in -dRELEASE with FPC 2.6.x. }
    type
      TEnumerator = class
      private
        FList: TChildrenControls;
        FPosition: Integer;
        function GetCurrent: TUIControl;
      public
        constructor Create(AList: TChildrenControls);
        function MoveNext: Boolean;
        property Current: TUIControl read GetCurrent;
      end;
    {$endif}

    function GetItem(const I: Integer): TUIControl;
    procedure SetItem(const I: Integer; const Item: TUIControl);
    { React to add/remove notifications. }
    procedure Notify(Ptr: Pointer; Action: TListNotification);
  public
    constructor Create(AParent: TUIControl);
    destructor Destroy; override;

    {$ifndef VER2_6}
    function GetEnumerator: TEnumerator;
    {$endif}

    property Items[I: Integer]: TUIControl read GetItem write SetItem; default;
    function Count: Integer;
    procedure Assign(const Source: TChildrenControls);
    { Remove the Item from this list.
      Note that the given Item should always exist only once on a list
      (it is not allowed to add it multiple times), so there's no @code(RemoveAll)
      method. }
    procedure Remove(const Item: TUIControl);
    procedure Clear;
    procedure Add(const Item: TUIControl); deprecated 'use InsertFront or InsertBack';
    procedure Insert(Index: Integer; const Item: TUIControl);
    function IndexOf(const Item: TUIControl): Integer;

    { Make sure that NewItem is the only instance of given ReplaceClass
      on the list, replacing old item if necesssary.
      See TCastleObjectList.MakeSingle for precise description. }
    function MakeSingle(ReplaceClass: TUIControlClass; NewItem: TUIControl;
      AddFront: boolean = true): TUIControl;

    { Add at the beginning of the list. }
    procedure InsertFront(const NewItem: TUIControl);
    procedure InsertFrontIfNotExists(const NewItem: TUIControl);
    procedure InsertFront(const NewItems: TUIControlList);

    { Add at the end of the list. }
    procedure InsertBack(const NewItem: TUIControl);
    procedure InsertBackIfNotExists(const NewItem: TUIControl);
    procedure InsertBack(const NewItems: TUIControlList);

    { BeginDisableContextOpenClose disables sending
      TUIControl.GLContextOpen and TUIControl.GLContextClose to all the controls
      on the list. EndDisableContextOpenClose ends this.
      They work by increasing / decreasing the TUIControl.DisableContextOpenClose
      for all the items on the list.

      @groupBegin }
    procedure BeginDisableContextOpenClose;
    procedure EndDisableContextOpenClose;
    { @groupEnd }
  end;

  TGLContextEvent = procedure;

  TGLContextEventList = class(specialize TGenericStructList<TGLContextEvent>)
  public
    { Call all items, first to last. }
    procedure ExecuteForward;
    { Call all items, last to first. }
    procedure ExecuteBackward;
  end;

{ Global callbacks called when OpenGL context (like Lazarus TCastleControl
  or TCastleWindow) is open/closed.
  Useful for things that want to be notified
  about OpenGL context existence, but cannot refer to a particular instance
  of TCastleControl or TCastleWindow.

  Note that we may have many OpenGL contexts (TCastleWindow or TCastleControl)
  open simultaneously. They all share OpenGL resources.
  OnGLContextOpen is called when first OpenGL context is open,
  that is: no previous context was open.
  OnGLContextClose is called when last OpenGL context is closed,
  that is: no more contexts remain open.
  Note that this implies that they may be called many times:
  e.g. if you open one window, then close it, then open another
  window then close it.

  Callbacks on OnGLContextOpen are called from first to last.
  Callbacks on OnGLContextClose are called in reverse order,
  so OnGLContextClose[0] is called last.

  @groupBegin }
function OnGLContextOpen: TGLContextEventList;
function OnGLContextClose: TGLContextEventList;
{ @groupEnd }

function IsGLContextOpen: boolean;

const
  { Deprecated name for rs2D. }
  ds2D = rs2D deprecated;
  { Deprecated name for rs3D. }
  ds3D = rs3D deprecated;

  prLeft = prLow deprecated;
  prRight = prHigh deprecated;
  prBottom = prLow deprecated;
  prTop = prHigh deprecated;

  hpLeft   = CastleRectangles.hpLeft  ;
  hpMiddle = CastleRectangles.hpMiddle;
  hpRight  = CastleRectangles.hpRight ;
  vpBottom = CastleRectangles.vpBottom;
  vpMiddle = CastleRectangles.vpMiddle;
  vpTop    = CastleRectangles.vpTop   ;

implementation

uses CastleLog, CastleWarnings;

{ TTouchList ----------------------------------------------------------------- }

function TTouchList.FindFingerIndex(const FingerIndex: TFingerIndex): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].FingerIndex = FingerIndex then
      Exit;
  Result := -1;
end;

function TTouchList.GetFingerIndexPosition(const FingerIndex: TFingerIndex): TVector2Single;
var
  Index: Integer;
begin
  Index := FindFingerIndex(FingerIndex);
  if Index <> -1 then
    Result := L[Index].Position else
    Result := ZeroVector2Single;
end;

procedure TTouchList.SetFingerIndexPosition(const FingerIndex: TFingerIndex;
  const Value: TVector2Single);
var
  Index: Integer;
  NewTouch: PTouch;
begin
  Index := FindFingerIndex(FingerIndex);
  if Index <> -1 then
    L[Index].Position := Value else
  begin
    NewTouch := Add;
    NewTouch^.FingerIndex := FingerIndex;
    NewTouch^.Position := Value;
  end;
end;

procedure TTouchList.RemoveFingerIndex(const FingerIndex: TFingerIndex);
var
  Index: Integer;
begin
  Index := FindFingerIndex(FingerIndex);
  if Index <> -1 then
    Delete(Index);
end;

{ TUIContainer --------------------------------------------------------------- }

constructor TUIContainer.Create(AOwner: TComponent);
begin
  inherited;
  FControls := TChildrenControls.Create(nil);
  TChildrenControls(FControls).Container := Self;
  FRenderStyle := rs2D;
  FTooltipDelay := DefaultTooltipDelay;
  FTooltipDistance := DefaultTooltipDistance;
  FCaptureInput := TFingerIndexCaptureMap.Create;
  FUIScaling := usNone;
  FUIExplicitScale := 1.0;
  FCalculatedUIScale := 1.0; // default safe value, in case some TUIControl will look here

  { connect 3D device - 3Dconnexion device }
  Mouse3dPollTimer := 0;
  try
    Mouse3d := T3DConnexionDevice.Create('Castle Control');
  except
    on E: Exception do
      if Log then WritelnLog('3D Mouse', 'Exception %s when initializing T3DConnexionDevice: %s',
        [E.ClassName, E.Message]);
  end;
end;

destructor TUIContainer.Destroy;
begin
  FreeAndNil(FControls);
  FreeAndNil(Mouse3d);
  FreeAndNil(FCaptureInput);
  inherited;
end;

procedure TUIContainer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TUIControl) then
    DetachNotification(TUIControl(AComponent));
end;

procedure TUIContainer.DetachNotification(const C: TUIControl);
var
  CaptureIndex: Integer;
begin
  if C = FFocus then FFocus := nil;

  CaptureIndex := FCaptureInput.IndexOfData(C);
  if CaptureIndex <> -1 then
    FCaptureInput.Delete(CaptureIndex);
end;

function TUIContainer.UseForceCaptureInput: boolean;
begin
  Result :=
    (ForceCaptureInput <> nil) and
    (Controls.FList.IndexOf(ForceCaptureInput) <> -1) and
    { note that before we checked "Controls.IndexOf(ForceCaptureInput) <> -1", we cannot
      even assume that ForceCaptureInput is a valid (not freed yet) reference. }
    ForceCaptureInput.GetExists;
end;

procedure TUIContainer.UpdateFocusAndMouseCursor;

  function CalculateFocus: TUIControl;
  var
    I: Integer;
    C: TUIControl;
  begin
    for I := Controls.Count - 1 downto 0 do
    begin
      C := Controls[I];
      if (not (csDestroying in C.ComponentState)) and
         C.GetExists and
         C.CapturesEventsAtPosition(MousePosition) then
        Exit(C);
    end;
    Result := nil;
  end;

  function CalculateMouseCursor: TMouseCursor;
  var
    I: Integer;
    C: TUIControl;
  begin
    if Focus <> nil then
    begin
      Result := Focus.Cursor;

      for I := Controls.Count - 1 downto 0 do
      begin
        C := Controls[I];
        if (not (csDestroying in C.ComponentState)) and
           C.GetExists and
           (C.Cursor = mcForceNone) and
           C.CapturesEventsAtPosition(MousePosition) then
        begin
          Result := mcForceNone;
          Break;
        end;
      end;

      { do not hide when container is not focused (mouse look doesn't work
        then too, so better to not hide mouse) }
      if (not Focused) and (Result in [mcNone, mcForceNone]) then
        Result := mcDefault;
    end else
      Result := mcDefault;
  end;

var
  NewFocus: TUIControl;
begin
  { since this is called at the end of TChildrenControls.Notify after
    some control is removed, we're paranoid here about checking csDestroying. }

  if UseForceCaptureInput and
     (not (csDestroying in ForceCaptureInput.ComponentState)) then
    NewFocus := ForceCaptureInput else
  if (FCaptureInput.IndexOf(0) <> -1) and
     (not (csDestroying in FCaptureInput[0].ComponentState)) then
    NewFocus := FCaptureInput[0] else
    NewFocus := CalculateFocus;

  if NewFocus <> Focus then
  begin
    if Focus <> nil then Focus.Focused := false;
    FFocus := NewFocus;
    if Focus <> nil then Focus.Focused := true;
  end;

  Cursor := CalculateMouseCursor;
end;

procedure TUIContainer.EventUpdate;

  procedure UpdateTooltip;
  var
    T: TTimerResult;
    NewTooltipVisible: boolean;
  begin
    { Update TooltipVisible and LastPositionForTooltip*.
      Idea is that user must move the mouse very slowly to activate tooltip. }

    T := Fps.UpdateStartTime;
    if (not HasLastPositionForTooltip) or
       (PointsDistanceSqr(LastPositionForTooltip, MousePosition) >
        Sqr(TooltipDistance)) then
    begin
      HasLastPositionForTooltip := true;
      LastPositionForTooltip := MousePosition;
      LastPositionForTooltipTime := T;
      NewTooltipVisible := false;
    end else
      NewTooltipVisible :=
        { make TooltipVisible only when we're over a control that has
          focus. This avoids unnecessary changing of TooltipVisible
          (and related Invalidate) when there's no tooltip possible. }
        (Focus <> nil) and
        Focus.TooltipExists and
        ( (1000 * (T - LastPositionForTooltipTime)) div
          TimerFrequency > TooltipDelay );

    if FTooltipVisible <> NewTooltipVisible then
    begin
      FTooltipVisible := NewTooltipVisible;

      if TooltipVisible then
      begin
        { when setting TooltipVisible from false to true,
          update LastPositionForTooltip. We don't want to hide the tooltip
          at the slightest jiggle of the mouse :) On the other hand,
          we don't want to update LastPositionForTooltip more often,
          as it would disable the purpose of TooltipDistance: faster
          mouse movement should hide the tooltip. }
        LastPositionForTooltip := MousePosition;
        { also update TooltipPosition }
        FTooltipPosition := MousePosition;
      end;

      Invalidate;
    end;
  end;

var
  I, J, K: Integer;
  C: TUIControl;
  HandleInput: boolean;
  Dummy: boolean;
  Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
  Mouse3dPollSpeed: Single;
const
  Mouse3dPollDelay = 0.05;
begin
  UpdateTooltip;

  { 3D Mouse }
  if Assigned(Mouse3D) and Mouse3D.Loaded then
  begin
    Mouse3dPollTimer -= Fps.UpdateSecondsPassed;
    if Mouse3dPollTimer < 0 then
    begin
      { get values from sensor }
      Mouse3dPollSpeed := -Mouse3dPollTimer + Mouse3dPollDelay;
      Tx := 0; { make sure they are initialized }
      Ty := 0;
      Tz := 0;
      TLength := 0;
      Mouse3D.GetSensorTranslation(Tx, Ty, Tz, TLength);
      Rx := 0; { make sure they are initialized }
      Ry := 0;
      Rz := 0;
      RAngle := 0;
      Mouse3D.GetSensorRotation(Rx, Ry, Rz, RAngle);

      { send to all 2D controls, including viewports }
      for I := Controls.Count - 1 downto 0 do
      begin
        C := Controls[I];
        if C.GetExists and C.CapturesEventsAtPosition(MousePosition) then
        begin
          C.SensorTranslation(Tx, Ty, Tz, TLength, Mouse3dPollSpeed);
          C.SensorRotation(Rx, Ry, Rz, RAngle, Mouse3dPollSpeed);
        end;
      end;

      { set timer.
        The "repeat ... until" below should not be necessary under normal
        circumstances, as Mouse3dPollDelay should be much larger than typical
        frequency of how often this is checked. But we do it for safety
        (in case something else, like AI or collision detection,
        slows us down *a lot*). }
      repeat Mouse3dPollTimer += Mouse3dPollDelay until Mouse3dPollTimer > 0;
    end;
  end;

  { Joysticks }
  if Assigned(Joysticks) then
  begin
    Joysticks.Poll;

    for I := 0 to Joysticks.JoyCount - 1 do
    begin
      for J := 0 to Joysticks.GetJoy(I)^.Info.Count.Buttons -1 do
      begin
        //Joysticks.Down(I, J);
        //Joysticks.Up(I, J);
        if Joysticks.Press(I, J) then
          for K := Controls.Count - 1 downto 0 do
          begin
            C := Controls[K];
            if C.GetExists and C.CapturesEventsAtPosition(MousePosition) then
            begin
              C.JoyButtonPress(I, J);
            end;
          end;
      end;
      for J := 0 to Joysticks.GetJoy(I)^.Info.Count.Axes -1 do
      begin
        if Joysticks.AxisPos(I, J) <> 0 then
          for K := Controls.Count - 1 downto 0 do
          begin
            C := Controls[K];
            if C.GetExists and C.CapturesEventsAtPosition(MousePosition) then
            begin
              C.JoyAxisMove(I, J);
            end;
          end;
      end;
    end;
  end;

  { Although we call Update for all the existing controls, we look
    at CapturesEventsAtPosition and track HandleInput values.
    See TUIControl.Update for explanation. }

  HandleInput := true;

  { ForceCaptureInput has the 1st chance to process inputs }
  if UseForceCaptureInput then
    ForceCaptureInput.Update(Fps.UpdateSecondsPassed, HandleInput);

  I := 0; // while loop, in case some Update method changes the Controls list
  while I < Controls.Count do
  begin
    C := Controls[I];
    if C.GetExists and (C <> ForceCaptureInput) then
    begin
      if C.CapturesEventsAtPosition(MousePosition) then
      begin
        C.Update(Fps.UpdateSecondsPassed, HandleInput);
      end else
      begin
        Dummy := false;
        C.Update(Fps.UpdateSecondsPassed, Dummy);
      end;
    end;
    Inc(I);
  end;

  if Assigned(OnUpdate) then OnUpdate(Self);
end;

function TUIContainer.EventPress(const Event: TInputPressRelease): boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := false;

  { pass to ForceCaptureInput }
  if UseForceCaptureInput then
  begin
    if ForceCaptureInput.Press(Event) then
      Exit(true);
  end;

  { pass to all Controls }
  for I := Controls.Count - 1 downto 0 do
  begin
    C := Controls[I];
    if C.GetExists and C.CapturesEventsAtPosition(Event.Position) and (C <> ForceCaptureInput) then
      if C.Press(Event) then
      begin
        { We have to check whether C.Container = Self. That is because
          the implementation of control's Press method could remove itself
          from our Controls list. Consider e.g. TCastleOnScreenMenu.Press
          that may remove itself from the Window.Controls list when clicking
          "close menu" item. We cannot, in such case, save a reference to
          this control in FCaptureInput, because we should not speak with it
          anymore (we don't know when it's destroyed, we cannot call it's
          Release method because it has Container = nil, and so on). }
        if (Event.EventType = itMouseButton) and
           (C.Container = Self) then
          FCaptureInput[Event.FingerIndex] := C;
        Exit(true);
      end;
  end;

  { pass to container event }
  if Assigned(OnPress) then
  begin
    OnPress(Self, Event);
    Result := true;
  end;
end;

function TUIContainer.EventRelease(const Event: TInputPressRelease): boolean;
var
  I, CaptureIndex: Integer;
  C, Capture: TUIControl;
begin
  Result := false;

  { pass to ForceCaptureInput }
  if UseForceCaptureInput then
  begin
    if ForceCaptureInput.Release(Event) then
      Exit(true);
  end;

  { pass to control holding capture }

  CaptureIndex := FCaptureInput.IndexOf(Event.FingerIndex);

  if (CaptureIndex <> -1) and
     not FCaptureInput.Data[CaptureIndex].GetExists then
  begin
    { No longer capturing, since the GetExists returns false now.
      We do not send any events to non-existing controls. }
    FCaptureInput.Delete(CaptureIndex);
    CaptureIndex := -1;
  end;

  if CaptureIndex <> -1 then
    Capture := FCaptureInput.Data[CaptureIndex] else
    Capture := nil;
  if (CaptureIndex <> -1) and (MousePressed = []) then
  begin
    { No longer capturing, but will receive the Release event. }
    FCaptureInput.Delete(CaptureIndex);
  end;

  if (Capture <> nil) and (Capture <> ForceCaptureInput) then
  begin
    Result := Capture.Release(Event);
    Exit;
  end;

  { pass to all Controls }
  for I := Controls.Count - 1 downto 0 do
  begin
    C := Controls[I];
    if C.GetExists and C.CapturesEventsAtPosition(Event.Position) and (C <> ForceCaptureInput) then
      if C.Release(Event) then
        Exit(true);
  end;

  { pass to container event }
  if Assigned(OnRelease) then
  begin
    OnRelease(Self, Event);
    Result := true;
  end;
end;

procedure TUIContainer.EventOpen(const OpenWindowsCount: Cardinal);
var
  I: Integer;
  C: TUIControl;
begin
  if OpenWindowsCount = 1 then
    OnGLContextOpen.ExecuteForward;

  { Call GLContextOpen on controls before OnOpen,
    this way OnOpen has controls with GLInitialized = true,
    so using SaveScreen etc. makes more sense there. }
  for I := Controls.Count - 1 downto 0 do
  begin
    C := Controls[I];
    { Check here C.GLInitialized to not call C.GLContextOpen twice.
      Control may have GL resources already initialized if it was added
      e.g. from Application.OnInitialize before EventOpen. }
    if not C.GLInitialized then
      C.GLContextOpen;
  end;

  if Assigned(OnOpen) then OnOpen(Self);
  if Assigned(OnOpenObject) then OnOpenObject(Self);
end;

procedure TUIContainer.EventClose(const OpenWindowsCount: Cardinal);
var
  I: Integer;
  C: TUIControl;
begin
  { Call GLContextClose on controls after OnClose,
    consistent with inverse order in OnOpen. }
  if Assigned(OnCloseObject) then OnCloseObject(Self);
  if Assigned(OnClose) then OnClose(Self);

  { call GLContextClose on controls before OnClose.
    This may be called from Close, which may be called from TCastleWindowCustom destructor,
    so prepare for Controls being possibly nil now. }
  if Controls <> nil then
  begin
    for I := Controls.Count - 1 downto 0 do
    begin
      C := Controls[I];
      if C.GLInitialized then
        C.GLContextClose;
    end;
  end;

  if OpenWindowsCount = 1 then
    OnGLContextClose.ExecuteBackward;
end;

function TUIContainer.AllowSuspendForInput: boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := true;

  { Do not suspend when you're over a control that may have a tooltip,
    as EventUpdate must track and eventually show tooltip. }
  if (Focus <> nil) and Focus.TooltipExists then
    Exit(false);

  for I := Controls.Count - 1 downto 0 do
  begin
    C := Controls[I];
    if C.GetExists then
    begin
      Result := C.AllowSuspendForInput;
      if not Result then Exit;
    end;
  end;
end;

procedure TUIContainer.EventMotion(const Event: TInputMotion);
var
  I, CaptureIndex: Integer;
  C: TUIControl;
begin
  UpdateFocusAndMouseCursor;

  { pass to ForceCaptureInput }
  if UseForceCaptureInput then
  begin
    if ForceCaptureInput.Motion(Event) then
      Exit;
  end;

  { pass to control holding capture }

  CaptureIndex := FCaptureInput.IndexOf(Event.FingerIndex);

  if (CaptureIndex <> -1) and
     not FCaptureInput.Data[CaptureIndex].GetExists then
  begin
    { No longer capturing, since the GetExists returns false now.
      We do not send any events to non-existing controls. }
    FCaptureInput.Delete(CaptureIndex);
    CaptureIndex := -1;
  end;

  if (CaptureIndex <> -1) and (FCaptureInput.Data[CaptureIndex] <> ForceCaptureInput) then
  begin
    FCaptureInput.Data[CaptureIndex].Motion(Event);
    Exit;
  end;

  { pass to all Controls }
  for I := Controls.Count - 1 downto 0 do
  begin
    C := Controls[I];
    if C.GetExists and C.CapturesEventsAtPosition(Event.Position) and (C <> ForceCaptureInput) then
    begin
      if C.Motion(Event) then
        Exit;
    end;
  end;

  { pass to container event }
  if Assigned(OnMotion) then
    OnMotion(Self, Event);
end;

procedure TUIContainer.ControlsVisibleChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TUIContainer.EventBeforeRender;
var
  I: Integer;
  C: TUIControl;
begin
  for I := Controls.Count - 1 downto 0 do
  begin
    C := Controls[I];
    if C.GetExists and C.GLInitialized then
      C.BeforeRender;
  end;

  if Assigned(OnBeforeRender) then OnBeforeRender(Self);
end;

procedure TUIContainer.EventResize;
var
  I: Integer;
  C: TUIControl;
begin
  if UIScaling in [usEncloseReferenceSize, usFitReferenceSize] then
    { usXxxReferenceSize adjust current Width/Height to reference,
      so the FCalculatedUIScale must be adjusted on each resize. }
    UpdateUIScale;

  for I := Controls.Count - 1 downto 0 do
  begin
    C := Controls[I];
    C.Resize;
  end;

  { This way control's get Resize before our OnResize,
    useful to process them all reliably in OnResize. }
  if Assigned(OnResize) then OnResize(Self);
end;

function TUIContainer.Controls: TChildrenControls;
begin
  Result := TChildrenControls(FControls);
end;

function TUIContainer.IsMousePositionForMouseLook: boolean;
var
  P: TVector2Single;
begin
  P := MousePosition;
  Result := (P[0] = Width div 2) and (P[1] = Height div 2);
end;

procedure TUIContainer.MakeMousePositionForMouseLook;
begin
  { Paranoidally check is position different, to avoid setting
    MousePosition in every Update. Setting MousePosition should be optimized
    for this case (when position is already set), but let's check anyway.

    This also avoids infinite loop, when setting MousePosition,
    getting Motion event, setting MousePosition, getting Motion event...
    in a loop.
    Not really likely (as messages will be queued, and some
    MousePosition setting will finally just not generate event Motion),
    but I want to safeguard anyway. }

{
  WritelnLog('ml', Format('Mouse Position is %f,%f. Good for mouse look? %s. Setting pos to %f,%f if needed',
    [MousePosition[0],
     MousePosition[1],
     BoolToStr[IsMousePositionForMouseLook],
     Single(Width div 2),
     Single(Height div 2)]));
}

  if (not IsMousePositionForMouseLook) and Focused then
    { Note: setting to float position (ContainerWidth/2, ContainerHeight/2)
      seems simpler, but is risky: we if the backend doesn't support sub-pixel accuracy,
      we will never be able to position mouse exactly at half pixel. }
    MousePosition := Vector2Single(Width div 2, Height div 2);
end;

procedure TUIContainer.SetUIScaling(const Value: TUIScaling);
begin
  if FUIScaling <> Value then
  begin
    FUIScaling := Value;
    UpdateUIScale;
  end;
end;

procedure TUIContainer.SetUIReferenceWidth(const Value: Integer);
begin
  if FUIReferenceWidth <> Value then
  begin
    FUIReferenceWidth := Value;
    UpdateUIScale;
  end;
end;

procedure TUIContainer.SetUIReferenceHeight(const Value: Integer);
begin
  if FUIReferenceHeight <> Value then
  begin
    FUIReferenceHeight := Value;
    UpdateUIScale;
  end;
end;

procedure TUIContainer.SetUIExplicitScale(const Value: Single);
begin
  if FUIExplicitScale <> Value then
  begin
    FUIExplicitScale := Value;
    UpdateUIScale;
  end;
end;

procedure TUIContainer.UpdateUIScale;
var
  S: Single;
  I: Integer;
begin
  case UIScaling of
    usNone         : S := 1;
    usExplicitScale: S := UIExplicitScale;
    usEncloseReferenceSize, usFitReferenceSize:
      begin
        { don't do this before our Width/Height are sensible }
        if not GLInitialized then Exit;

        S := 1;
        if (UIReferenceWidth <> 0) and (Width > 0) then
        begin
          S := Width / UIReferenceWidth;
          if (UIReferenceHeight <> 0) and (Height > 0) then
            if UIScaling = usEncloseReferenceSize then
              MinVar(S, Height / UIReferenceHeight) else
              MaxVar(S, Height / UIReferenceHeight);
        end else
        if (UIReferenceHeight <> 0) and (Height > 0) then
          S := Height / UIReferenceHeight;
        WritelnLog('Scaling', 'Automatic scaling to reference sizes %dx%d in effect. Calculated scale is %f, which simulates surface of size %dx%d',
          [UIReferenceWidth, UIReferenceHeight, S, Round(Width / S), Round(Height / S)]);
      end;
    else raise EInternalError.Create('UIScaling unknown');
  end;

  FCalculatedUIScale := S;

  for I := 0 to Controls.Count - 1 do
    Controls[I].UIScaleChanged;
end;

{ TInputListener ------------------------------------------------------------- }

constructor TInputListener.Create(AOwner: TComponent);
begin
  inherited;
  FExclusiveEvents := true;
  FCursor := mcDefault;
end;

function TInputListener.Press(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function TInputListener.Release(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function TInputListener.Motion(const Event: TInputMotion): boolean;
begin
  Result := false;
end;

function TInputListener.SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean;
begin
  Result := false;
end;

function TInputListener.SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean;
begin
  Result := false;
end;

function TInputListener.JoyAxisMove(const JoyID, Axis: Byte): boolean;
begin
  Result := False;
end;

function TInputListener.JoyButtonPress(const JoyID, Button: Byte): boolean;
begin
  Result := False;
end;

procedure TInputListener.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
end;

procedure TInputListener.VisibleChange;
begin
  if Assigned(OnVisibleChange) then
    OnVisibleChange(Self);
end;

function TInputListener.AllowSuspendForInput: boolean;
begin
  Result := true;
end;

procedure TInputListener.Resize;
begin
  {$warnings off}
  ContainerResize(ContainerWidth, ContainerHeight); // call deprecated, to keep it working
  {$warnings on}
end;

procedure TInputListener.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
end;

function TInputListener.ContainerWidth: Cardinal;
begin
  if ContainerSizeKnown then
    Result := Container.Width else
    Result := 0;
end;

function TInputListener.ContainerHeight: Cardinal;
begin
  if ContainerSizeKnown then
    Result := Container.Height else
    Result := 0;
end;

function TInputListener.ContainerRect: TRectangle;
begin
  if ContainerSizeKnown then
    Result := Container.Rect else
    Result := TRectangle.Empty;
end;

function TInputListener.ContainerSizeKnown: boolean;
begin
  { Note that ContainerSizeKnown is calculated looking at current Container,
    without waiting for Resize. This way it works even before
    we receive Resize method, which may happen to be useful:
    if you insert a SceneManager to a window before it's open (like it happens
    with standard scene manager in TCastleWindow and TCastleControl),
    and then you do something inside OnOpen that wants to render
    this viewport (which may happen if you simply initialize a progress bar
    without any predefined loading_image). Scene manager did not receive
    a Resize in this case yet (it will receive it from OnResize,
    which happens after OnOpen).

    See castle_game_engine/tests/testcontainer.pas for cases
    when this is really needed. }

  Result := (Container <> nil) and
    (not (csDestroying in Container.ComponentState)) and
    Container.GLInitialized;
end;

procedure TInputListener.SetCursor(const Value: TMouseCursor);
begin
  if Value <> FCursor then
  begin
    FCursor := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
    DoCursorChange;
  end;
end;

procedure TInputListener.DoCursorChange;
begin
  if Assigned(OnCursorChange) then OnCursorChange(Self);
end;

procedure TInputListener.SetContainer(const Value: TUIContainer);
begin
  FContainer := Value;
end;

{ TUIControl ----------------------------------------------------------------- }

constructor TUIControl.Create(AOwner: TComponent);
begin
  inherited;
  FExists := true;
  FEnableUIScaling := true;
end;

destructor TUIControl.Destroy;
begin
  GLContextClose;
  FreeAndNil(FControls);
  inherited;
end;

function TUIControl.KeepInFront: boolean;
begin
  Result := false;
end;

procedure TUIControl.CreateControls;
begin
  if FControls = nil then
  begin
    FControls := TChildrenControls.Create(Self);
    TChildrenControls(FControls).Container := Container;
  end;
end;

procedure TUIControl.InsertFront(const NewItem: TUIControl);
begin
  CreateControls;
  FControls.InsertFront(NewItem);
end;

procedure TUIControl.InsertFrontIfNotExists(const NewItem: TUIControl);
begin
  CreateControls;
  FControls.InsertFrontIfNotExists(NewItem);
end;

procedure TUIControl.InsertFront(const NewItems: TUIControlList);
begin
  CreateControls;
  FControls.InsertFront(NewItems);
end;

procedure TUIControl.InsertBack(const NewItem: TUIControl);
begin
  CreateControls;
  FControls.InsertBack(NewItem);
end;

procedure TUIControl.InsertBackIfNotExists(const NewItem: TUIControl);
begin
  CreateControls;
  FControls.InsertBackIfNotExists(NewItem);
end;

procedure TUIControl.InsertBack(const NewItems: TUIControlList);
begin
  CreateControls;
  FControls.InsertBack(NewItems);
end;

procedure TUIControl.RemoveControl(Item: TUIControl);
begin
  if FControls <> nil then
    FControls.Remove(Item);
end;

function TUIControl.GetControls(const I: Integer): TUIControl;
begin
  Result := FControls[I];
end;

procedure TUIControl.SetControls(const I: Integer; const Item: TUIControl);
begin
  FControls[I] := Item;
end;

function TUIControl.ControlsCount: Integer;
begin
  if FControls <> nil then
    Result := FControls.Count else
    Result := 0;
end;

procedure TUIControl.SetContainer(const Value: TUIContainer);
var
  I: Integer;
begin
  inherited;
  if FControls <> nil then
  begin
    TChildrenControls(FControls).Container := Value;
    for I := 0 to FControls.Count - 1 do
      FControls[I].SetContainer(Value);
  end;
end;

procedure TUIControl.SetEnableUIScaling(const Value: boolean);
begin
  if FEnableUIScaling <> Value then
  begin
    FEnableUIScaling := Value;
    UIScaleChanged;
  end;
end;

function TUIControl.UIScale: Single;
begin
  if ContainerSizeKnown and EnableUIScaling then
    Result := Container.FCalculatedUIScale else
    Result := 1.0;
end;

function TUIControl.LeftBottomScaled: TVector2Integer;
begin
  Result := Vector2Integer(
    Round(UIScale * Left), Round(UIScale * Bottom));
end;

procedure TUIControl.UIScaleChanged;
var
  I: Integer;
begin
  inherited;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].UIScaleChanged;
end;

{ No point in doing anything? We should propagate it to to parent like T3D?
procedure TUIControl.DoCursorChange;
begin
  inherited;
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      FControls[I].DoCursorChange;
end;
}

function TUIControl.Press(const Event: TInputPressRelease): boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := inherited;
  if Result then Exit;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := Controls[I];
      if C.GetExists and
         C.CapturesEventsAtPosition(Event.Position) and
         C.Press(Event) then
        Exit(true);
    end;
end;

function TUIControl.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := inherited;
  if Result then Exit;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := Controls[I];
      if C.GetExists and
         C.CapturesEventsAtPosition(Event.Position) and
         C.Release(Event) then
        Exit(true);
    end;
end;

function TUIControl.Motion(const Event: TInputMotion): boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := inherited;
  if Result then Exit;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := Controls[I];
      if C.GetExists and
         C.CapturesEventsAtPosition(Event.Position) and
         C.Motion(Event) then
        Exit(true);
    end;
end;

function TUIControl.SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := inherited;
  if Result then Exit;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := Controls[I];
      if C.GetExists and
         C.CapturesEventsAtPosition(Container.MousePosition) and
         C.SensorRotation(X, Y, Z, Angle, SecondsPassed) then
        Exit(true);
    end;
end;

function TUIControl.SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := inherited;
  if Result then Exit;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := Controls[I];
      if C.GetExists and
         C.CapturesEventsAtPosition(Container.MousePosition) and
         C.SensorTranslation(X, Y, Z, Length, SecondsPassed) then
        Exit(true);
    end;
end;

procedure TUIControl.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  I: Integer;
  Dummy: boolean;
  C: TUIControl;
begin
  inherited;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := Controls[I];
      if C.GetExists then
        if C.CapturesEventsAtPosition(Container.MousePosition) then
        begin
          C.Update(SecondsPassed, HandleInput);
        end else
        begin
          Dummy := false;
          C.Update(SecondsPassed, Dummy);
        end;
    end;
end;

function TUIControl.AllowSuspendForInput: boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := inherited;
  if not Result then Exit;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := FControls[I];
      if C.GetExists and (not C.AllowSuspendForInput) then
        Exit(false);
    end;
end;

procedure TUIControl.Resize;
var
  I: Integer;
  C: TUIControl;
begin
  inherited;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := FControls[I];
      C.Resize;
    end;
end;

function TUIControl.CapturesEventsAtPosition(const Position: TVector2Single): boolean;
var
  I: Integer;
  C: TUIControl;
  SR: TRectangle;
begin
  SR := ScreenRect;
  Result := SR.Contains(Position) or
    { if the control covers the whole Container, it *always* captures events,
      even when mouse position is unknown yet, or outside the window. }
    (ContainerSizeKnown and
     (SR.Left <= 0) and
     (SR.Bottom <= 0) and
     (SR.Width >= ContainerWidth) and
     (SR.Height >= ContainerHeight));
  if Result then Exit;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := FControls[I];
      if C.GetExists and C.CapturesEventsAtPosition(Position) then
        Exit(true);
    end;
end;

function TUIControl.TooltipExists: boolean;
var
  I: Integer;
  C: TUIControl;
begin
  Result := false;

  if FControls <> nil then
    for I := FControls.Count - 1 downto 0 do
    begin
      C := FControls[I];
      if C.GetExists and C.TooltipExists then
        Exit(true);
    end;
end;

procedure TUIControl.BeforeRender;
var
  I: Integer;
  C: TUIControl;
begin
  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
    begin
      C := FControls[I];
      if C.GetExists and C.GLInitialized then
        C.BeforeRender;
    end;
end;

procedure TUIControl.Render;
begin
end;

function TUIControl.TooltipStyle: TRenderStyle;
begin
  Result := rs2D;
end;

procedure TUIControl.TooltipRender;
begin
end;

procedure TUIControl.GLContextOpen;
var
  I: Integer;
  C: TUIControl;
begin
  FGLInitialized := true;

  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
    begin
      C := FControls[I];
      if not C.GLInitialized then
        C.GLContextOpen;
    end;
end;

procedure TUIControl.GLContextClose;
var
  I: Integer;
  C: TUIControl;
begin
  FGLInitialized := false;

  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
    begin
      C := FControls[I];
      if C.GLInitialized then
        C.GLContextClose;
    end;
end;

function TUIControl.GetExists: boolean;
begin
  Result := FExists;
end;

procedure TUIControl.SetFocused(const Value: boolean);
begin
  FFocused := Value;
end;

procedure TUIControl.SetExists(const Value: boolean);
begin
  if FExists <> Value then
  begin
    FExists := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

{ We store Left property value in file under "tuicontrolpos_real_left" name,
  to avoid clashing with TComponent magic "left" property name.
  The idea how to do this is taken from TComponent's own implementation
  of it's "left" magic property (rtl/objpas/classes/compon.inc). }

procedure TUIControl.ReadRealLeft(Reader: TReader);
begin
  FLeft := Reader.ReadInteger;
end;

procedure TUIControl.WriteRealLeft(Writer: TWriter);
begin
  Writer.WriteInteger(FLeft);
end;

procedure TUIControl.ReadLeft(Reader: TReader);
var
  D: LongInt;
begin
  D := DesignInfo;
  LongRec(D).Lo:=Reader.ReadInteger;
  DesignInfo := D;
end;

procedure TUIControl.ReadTop(Reader: TReader);
var
  D: LongInt;
begin
  D := DesignInfo;
  LongRec(D).Hi:=Reader.ReadInteger;
  DesignInfo := D;
end;

procedure TUIControl.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Lo);
end;

procedure TUIControl.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(DesignInfo).Hi);
end;

procedure TUIControl.DefineProperties(Filer: TFiler);
Var Ancestor : TComponent;
    Temp : longint;
begin
  { Don't call inherited that defines magic left/top.
    This would make reading design-time "left" broken, it seems that our
    declaration of Left with "stored false" would then prevent the design-time
    Left from ever loading.

    Instead, we'll save design-time "Left" below, under a special name. }

  Filer.DefineProperty('TUIControlPos_RealLeft', @ReadRealLeft, @WriteRealLeft,
    FLeft <> 0);

  { Code from fpc/trunk/rtl/objpas/classes/compon.inc }
  Temp:=0;
  Ancestor:=TComponent(Filer.Ancestor);
  If Assigned(Ancestor) then Temp:=Ancestor.DesignInfo;
  Filer.Defineproperty('TUIControlPos_Design_Left',@readleft,@writeleft,
                       (longrec(DesignInfo).Lo<>Longrec(temp).Lo));
  Filer.Defineproperty('TUIControlPos_Design_Top',@readtop,@writetop,
                       (longrec(DesignInfo).Hi<>Longrec(temp).Hi));
end;

procedure TUIControl.SetLeft(const Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

procedure TUIControl.SetBottom(const Value: Integer);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
  end;
end;

procedure TUIControl.Align(
  const ControlPosition: THorizontalPosition;
  const ContainerPosition: THorizontalPosition;
  const X: Integer = 0);
begin
  Left := Rect.AlignCore(ControlPosition, ParentRect, ContainerPosition, X);
end;

procedure TUIControl.Align(
  const ControlPosition: TVerticalPosition;
  const ContainerPosition: TVerticalPosition;
  const Y: Integer = 0);
begin
  Bottom := Rect.AlignCore(ControlPosition, ParentRect, ContainerPosition, Y);
end;

procedure TUIControl.AlignHorizontal(
  const ControlPosition: TPositionRelative;
  const ContainerPosition: TPositionRelative;
  const X: Integer);
begin
  Align(
    THorizontalPosition(ControlPosition),
    THorizontalPosition(ContainerPosition), X);
end;

procedure TUIControl.AlignVertical(
  const ControlPosition: TPositionRelative;
  const ContainerPosition: TPositionRelative;
  const Y: Integer);
begin
  Align(
    TVerticalPosition(ControlPosition),
    TVerticalPosition(ContainerPosition), Y);
end;

procedure TUIControl.Center;
begin
  Align(hpMiddle, hpMiddle);
  Align(vpMiddle, vpMiddle);
end;

function TUIControl.Rect: TRectangle;
begin
  Result := Rectangle(LeftBottomScaled, 0, 0);
end;

(*
function TUIControl.LocalRect: TRectangle;
begin
  {$warnings off}
  Result := Rect; // make deprecated Rect work by calling it here
  {$warnings off}
end;
*)

function TUIControl.RectWithAnchors: TRectangle;

  { Faster versions of TRectangle.AlignCore, knowing that ParentRect
    ("other" rectangle) has Left/Bottom = (0,0) in local coords
    (so no need to worry about them) and that ThisPosition = OtherPosition. }

  function FastAlignCore(
    const Position: THorizontalPosition;
    const ThisWidth, OtherRectWidth: Integer;
    const X: Integer): Integer;
  begin
    case Position of
      hpLeft  : Result := X;
      hpMiddle: Result := X - ThisWidth div 2 + OtherRectWidth div 2;
      hpRight : Result := X - ThisWidth       + OtherRectWidth;
    end;
  end;

  function FastAlignCore(
    const Position: TVerticalPosition;
    const ThisHeight, OtherRectHeight: Integer;
    const Y: Integer): Integer;
  begin
    case Position of
      vpBottom: Result := Y;
      vpMiddle: Result := Y - ThisHeight div 2 + OtherRectHeight div 2;
      vpTop   : Result := Y - ThisHeight       + OtherRectHeight;
    end;
  end;

var
  PR: TRectangle;
begin
  if not ContainerSizeKnown then
    { Don't call virtual Rect in this state, Rect implementations
      typically assume that ParentRect is sensible.
      This is crucial, various programs will crash without it. }
    Exit(TRectangle.Empty);

  Result := Rect;

  { apply anchors }
  if HasHorizontalAnchor or HasVerticalAnchor then
    PR := ParentRect; // only PR.Left / PR.Bottom are unused, so no need to get ParentRectAnchored
  if HasHorizontalAnchor then
    Result.Left := FastAlignCore(HorizontalAnchor, Result.Width , PR.Width ,
      Round(UIScale * HorizontalAnchorDelta));
  if HasVerticalAnchor then
    Result.Bottom := FastAlignCore(VerticalAnchor, Result.Height, PR.Height,
      Round(UIScale * VerticalAnchorDelta));
end;

function TUIControl.ScreenRect: TRectangle;
var
  T: TVector2Integer;
begin
  Result := RectWithAnchors;
  { transform local to screen space }
  T := LocalToScreenTranslation;
  Result.Left := Result.Left + T[0];
  Result.Bottom := Result.Bottom + T[1];
end;

function TUIControl.LocalToScreenTranslation: TVector2Integer;
var
  RA: TRectangle;
begin
  if Parent <> nil then
  begin
    Result := Parent.LocalToScreenTranslation;
    RA := Parent.RectWithAnchors;
    Result[0] += RA.Left;
    Result[1] += RA.Bottom;
  end else
    Result := ZeroVector2Integer;
end;

function TUIControl.ParentRect: TRectangle;
begin
  if Parent <> nil then
  begin
    Result := Parent.Rect;
    Result.Left := 0;
    Result.Bottom := 0;
  end else
    Result := ContainerRect;
end;

procedure TUIControl.SetHasHorizontalAnchor(const Value: boolean);
begin
  if FHasHorizontalAnchor <> Value then
  begin
    FHasHorizontalAnchor := Value;
    VisibleChange;
  end;
end;

procedure TUIControl.SetHorizontalAnchor(const Value: THorizontalPosition);
begin
  if FHorizontalAnchor <> Value then
  begin
    FHorizontalAnchor := Value;
    VisibleChange;
  end;
end;

procedure TUIControl.SetHorizontalAnchorDelta(const Value: Integer);
begin
  if FHorizontalAnchorDelta <> Value then
  begin
    FHorizontalAnchorDelta := Value;
    VisibleChange;
  end;
end;

procedure TUIControl.SetHasVerticalAnchor(const Value: boolean);
begin
  if FHasVerticalAnchor <> Value then
  begin
    FHasVerticalAnchor := Value;
    VisibleChange;
  end;
end;

procedure TUIControl.SetVerticalAnchor(const Value: TVerticalPosition);
begin
  if FVerticalAnchor <> Value then
  begin
    FVerticalAnchor := Value;
    VisibleChange;
  end;
end;

procedure TUIControl.SetVerticalAnchorDelta(const Value: Integer);
begin
  if FVerticalAnchorDelta <> Value then
  begin
    FVerticalAnchorDelta := Value;
    VisibleChange;
  end;
end;

procedure TUIControl.Anchor(const AHorizontalAnchor: THorizontalPosition;
  const AHorizontalAnchorDelta: Integer);
begin
  HasHorizontalAnchor := true;
  HorizontalAnchor := AHorizontalAnchor;
  HorizontalAnchorDelta := AHorizontalAnchorDelta;
end;

procedure TUIControl.Anchor(const AVerticalAnchor: TVerticalPosition;
  const AVerticalAnchorDelta: Integer);
begin
  HasVerticalAnchor := true;
  VerticalAnchor := AVerticalAnchor;
  VerticalAnchorDelta := AVerticalAnchorDelta;
end;

{ TChildrenControls ------------------------------------------------------------- }

constructor TChildrenControls.Create(AParent: TUIControl);
begin
  inherited Create;
  FParent := AParent;
  FList := TMyObjectList.Create;
  FList.Parent := Self;
  FCaptureFreeNotifications := TCaptureFreeNotifications.Create(nil);
  FCaptureFreeNotifications.Parent := Self;
end;

destructor TChildrenControls.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FCaptureFreeNotifications);
  inherited;
end;

function TChildrenControls.GetItem(const I: Integer): TUIControl;
begin
  Result := TUIControl(FList.Items[I]);
end;

procedure TChildrenControls.SetItem(const I: Integer; const Item: TUIControl);
begin
  FList.Items[I] := Item;
end;

function TChildrenControls.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TChildrenControls.BeginDisableContextOpenClose;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    with TUIControl(FList.Items[I]) do
      DisableContextOpenClose := DisableContextOpenClose + 1;
end;

procedure TChildrenControls.EndDisableContextOpenClose;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    with TUIControl(FList.Items[I]) do
      DisableContextOpenClose := DisableContextOpenClose - 1;
end;

procedure TChildrenControls.InsertFront(const NewItem: TUIControl);
begin
  Insert(Count, NewItem);
end;

procedure TChildrenControls.InsertFrontIfNotExists(const NewItem: TUIControl);
begin
  if FList.IndexOf(NewItem) = -1 then
    InsertFront(NewItem);
end;

procedure TChildrenControls.InsertFront(const NewItems: TUIControlList);
var
  I: Integer;
begin
  for I := 0 to NewItems.Count - 1 do
    InsertFront(NewItems[I]);
end;

procedure TChildrenControls.InsertBack(const NewItem: TUIControl);
begin
  FList.Insert(0, NewItem);
end;

procedure TChildrenControls.InsertBackIfNotExists(const NewItem: TUIControl);
begin
  if FList.IndexOf(NewItem) = -1 then
    InsertBack(NewItem);
end;

procedure TChildrenControls.Add(const Item: TUIControl);
begin
  InsertFront(Item);
end;

procedure TChildrenControls.Insert(Index: Integer; const Item: TUIControl);
var
  I: Integer;
begin
  { TODO: code duplicated with TUIControlList.InsertWithZOrder }
  Index := Clamped(Index, 0, Count);
  if Item.KeepInFront or
     (Count = 0) or
     (Index = 0) or
     (not Items[Index - 1].KeepInFront) then
    FList.Insert(Index, Item) else
  begin
    for I := Index - 1 downto 0 do
      if not Items[I].KeepInFront then
      begin
        FList.Insert(I + 1, Item);
        Exit;
      end;
    { everything has KeepInFront = true }
    FList.Insert(0, Item);
  end;
end;

function TChildrenControls.IndexOf(const Item: TUIControl): Integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TChildrenControls.InsertBack(const NewItems: TUIControlList);
var
  I: Integer;
begin
  for I := NewItems.Count - 1 downto 0 do
    InsertBack(NewItems[I]);
end;

{$ifndef VER2_6}
function TChildrenControls.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;
{$endif}

procedure TChildrenControls.Notify(Ptr: Pointer; Action: TListNotification);
var
  C: TUIControl;
begin
  { TODO: while this updating works cool,
    if the Parent or Container is destroyed
    before children --- the children will keep invalid reference. }

  C := TUIControl(Ptr);
  case Action of
    lnAdded:
      begin
        if ((C.FContainer <> nil) or (C.FParent <> nil)) and
           ((Container <> nil) or (FParent <> nil)) then
          OnWarning(wtMajor, 'UI', 'Inserting to the UI list (InsertFront, InsertBack) an item that is already a part of other UI list. The result is undefined, you cannot insert the same TUIControl instance multiple times.');
        C.FreeNotification(FCaptureFreeNotifications);
        if Container <> nil then RegisterContainer(C, FContainer);
        C.FParent := FParent;
      end;
    lnExtracted, lnDeleted:
      begin
        C.FParent := nil;
        if Container <> nil then UnregisterContainer(C, FContainer);
        C.RemoveFreeNotification(FCaptureFreeNotifications);
      end;
    else raise EInternalError.Create('TChildrenControls.Notify action?');
  end;

  { This notification may get called during FreeAndNil(FControls)
    in TUIContainer.Destroy. Then FControls is already nil, and we're
    getting remove notification for all items (as FreeAndNil first sets
    object to nil). Testcase: lets_take_a_walk exit. }
  if (FContainer <> nil) and
     (FContainer.FControls <> nil) then
  begin
    FContainer.UpdateFocusAndMouseCursor;
    FContainer.Invalidate;
  end;
end;

procedure TChildrenControls.TCaptureFreeNotifications.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { We have to remove a reference to the object from list.
    This is crucial, various methods assume that all objects on
    the list are always valid objects (no invalid references,
    even for a short time). }

  if (Operation = opRemove) and (AComponent is TUIControl) then
    Parent.FList.Remove(AComponent);
end;

procedure TChildrenControls.Assign(const Source: TChildrenControls);
begin
  FList.Assign(Source.FList);
end;

procedure TChildrenControls.Remove(const Item: TUIControl);
begin
  FList.Remove(Item);
end;

procedure TChildrenControls.Clear;
begin
  FList.Clear;
end;

function TChildrenControls.MakeSingle(ReplaceClass: TUIControlClass; NewItem: TUIControl;
  AddFront: boolean): TUIControl;
begin
  Result := FList.MakeSingle(ReplaceClass, NewItem, AddFront) as TUIControl;
end;

procedure TChildrenControls.RegisterContainer(
  const C: TUIControl; const AContainer: TUIContainer);
begin
  { Make sure AContainer.ControlsVisibleChange (which in turn calls Invalidate)
    will be called when a control calls OnVisibleChange.

    We only change OnVisibleChange from @nil to it's own internal callback
    (when adding a control), and from it's own internal callback to @nil
    (when removing a control).
    This means that if user code will assign OnVisibleChange callback to some
    custom method --- we will not touch it anymore. That's safer.
    Athough in general user code should not change OnVisibleChange for controls
    on this list, to keep automatic Invalidate working. }
  if C.OnVisibleChange = nil then
    C.OnVisibleChange := @AContainer.ControlsVisibleChange;

  { Register AContainer to be notified of control destruction. }
  C.FreeNotification(AContainer);

  C.Container := AContainer;
  C.UIScaleChanged;

  if AContainer.GLInitialized then
  begin
    if C.DisableContextOpenClose = 0 then
      C.GLContextOpen;
    { Call initial Resize for control.
      If window OpenGL context is not yet initialized, defer it to
      the Open time, then our initial EventResize will be called
      that will do Resize on every control. }
    C.Resize;
  end;
end;

procedure TChildrenControls.UnregisterContainer(
  const C: TUIControl; const AContainer: TUIContainer);
begin
  if AContainer.GLInitialized and
     (C.DisableContextOpenClose = 0) then
    C.GLContextClose;

  if C.OnVisibleChange = @AContainer.ControlsVisibleChange then
    C.OnVisibleChange := nil;

  C.RemoveFreeNotification(AContainer);
  AContainer.DetachNotification(C);

  C.Container := nil;
end;

procedure TChildrenControls.SetContainer(const AContainer: TUIContainer);
var
  I: Integer;
begin
  if FContainer <> AContainer then
  begin
    if FContainer <> nil then
      for I := 0 to Count - 1 do
        UnregisterContainer(Items[I], FContainer);
    FContainer := AContainer;
    if FContainer <> nil then
      for I := 0 to Count - 1 do
        RegisterContainer(Items[I], FContainer);
  end;
end;

{ TChildrenControls.TMyObjectList ----------------------------------------------- }

procedure TChildrenControls.TMyObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  Parent.Notify(Ptr, Action);
end;

{ TChildrenControls.TEnumerator ------------------------------------------------- }

{$ifndef VER2_6}
function TChildrenControls.TEnumerator.GetCurrent: TUIControl;
begin
  Result := FList.Items[FPosition];
end;

constructor TChildrenControls.TEnumerator.Create(AList: TChildrenControls);
begin
  inherited Create;
  FList := AList;
  FPosition := -1;
end;

function TChildrenControls.TEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;
{$endif}

{ TUIControlList ------------------------------------------------------------- }

procedure TUIControlList.InsertFront(const NewItem: TUIControl);
begin
  InsertWithZOrder(Count, NewItem);
end;

procedure TUIControlList.InsertFrontIfNotExists(const NewItem: TUIControl);
begin
  if IndexOf(NewItem) = -1 then
    InsertFront(NewItem);
end;

procedure TUIControlList.InsertFront(const NewItems: TUIControlList);
var
  I: Integer;
begin
  for I := 0 to NewItems.Count - 1 do
    InsertFront(NewItems[I]);
end;

procedure TUIControlList.InsertBack(const NewItem: TUIControl);
begin
  InsertWithZOrder(0, NewItem);
end;

procedure TUIControlList.InsertBackIfNotExists(const NewItem: TUIControl);
begin
  if IndexOf(NewItem) = -1 then
    InsertBack(NewItem);
end;

procedure TUIControlList.InsertBack(const NewItems: TUIControlList);
var
  I: Integer;
begin
  for I := NewItems.Count - 1 downto 0 do
    InsertBack(NewItems[I]);
end;

procedure TUIControlList.InsertWithZOrder(Index: Integer; const Item: TUIControl);
var
  I: Integer;
begin
  Index := Clamped(Index, 0, Count);
  if Item.KeepInFront or
     (Count = 0) or
     (Index = 0) or
     (not Items[Index - 1].KeepInFront) then
    Insert(Index, Item) else
  begin
    for I := Index - 1 downto 0 do
      if not Items[I].KeepInFront then
      begin
        Insert(I + 1, Item);
        Exit;
      end;
    { everything has KeepInFront = true }
    Insert(0, Item);
  end;
end;


{ TGLContextEventList -------------------------------------------------------- }

var
  FIsGLContextOpen: boolean;

function IsGLContextOpen: boolean;
begin
  Result := FIsGLContextOpen;
end;

procedure TGLContextEventList.ExecuteForward;
var
  I: Integer;
begin
  FIsGLContextOpen := true;
  for I := 0 to Count - 1 do
    Items[I]();
end;

procedure TGLContextEventList.ExecuteBackward;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I]();
  FIsGLContextOpen := false;
end;

var
  FOnGLContextOpen, FOnGLContextClose: TGLContextEventList;

function OnGLContextOpen: TGLContextEventList;
begin
  if FOnGLContextOpen = nil then
    FOnGLContextOpen := TGLContextEventList.Create;
  Result := FOnGLContextOpen;
end;

function OnGLContextClose: TGLContextEventList;
begin
  if FOnGLContextClose = nil then
    FOnGLContextClose := TGLContextEventList.Create;
  Result := FOnGLContextClose;
end;

finalization
  FreeAndNil(FOnGLContextOpen);
  FreeAndNil(FOnGLContextClose);
end.
