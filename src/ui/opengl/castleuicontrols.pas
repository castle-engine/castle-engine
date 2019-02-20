{
  Copyright 2009-2018 Michalis Kamburelis, Tomasz Wojtyś.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ User interface basic classes: @link(TCastleUserInterface), @link(TUIContainer). }
unit CastleUIControls;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  CastleKeysMouse, CastleUtils, CastleClassUtils, CastleGLUtils, CastleFonts,
  CastleRectangles, CastleTimeUtils, CastleInternalPk3DConnexion, CastleColors,
  CastleImages, CastleVectors, CastleJoysticks, CastleApplicationProperties;

const
  { Default value for container's Dpi, as is usually set on desktops. }
  DefaultDpi = 96.0;
  DefaultTooltipDelay = 1.0;
  DefaultTooltipDistance = 10;

type
  TInputListener = class;
  TCastleUserInterface = class;
  TChildrenControls = class;
  TUIContainer = class;

  TContainerEvent = procedure (Container: TUIContainer);
  TContainerObjectEvent = procedure (Container: TUIContainer) of object;
  TInputPressReleaseEvent = procedure (Container: TUIContainer; const Event: TInputPressRelease);
  TInputMotionEvent = procedure (Container: TUIContainer; const Event: TInputMotion);

  TUiNotifyEvent = procedure (const Sender: TCastleUserInterface) of object;
  TUiUpdateEvent = procedure (const Sender: TInputListener;
    const SecondsPassed: Single; var HandleInput: Boolean) of object;
  TUiPressReleaseEvent = procedure (const Sender: TInputListener;
    const Event: TInputPressRelease; var Handled: Boolean) of object;
  TUiMotionEvent = procedure (const Sender: TInputListener;
    const Event: TInputMotion; var Handled: Boolean) of object;

  { Tracking of a touch by a single finger, used by TTouchList. }
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
      This is consistent with how our 2D controls (TCastleUserInterface)
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
    Position: TVector2;
  end;
  PTouch = ^TTouch;

  { Tracking of multi-touch, a position of each finger on the screen. }
  TTouchList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TTouch>)
  private
    { Find an item with given FingerIndex, or -1 if not found. }
    function FindFingerIndex(const FingerIndex: TFingerIndex): Integer;
    function GetFingerIndexPosition(const FingerIndex: TFingerIndex): TVector2;
    procedure SetFingerIndexPosition(const FingerIndex: TFingerIndex;
      const Value: TVector2);
  public
    { Gets or sets a position corresponding to given FingerIndex.
      If there is no information for given FingerIndex on the list,
      the getter will return zero, and the setter will automatically create
      and add appropriate information. }
    property FingerIndexPosition[const FingerIndex: TFingerIndex]: TVector2
      read GetFingerIndexPosition write SetFingerIndexPosition;
    { Remove a touch item for given FingerIndex. }
    procedure RemoveFingerIndex(const FingerIndex: TFingerIndex);
  end;

  TCastleUserInterfaceList = class;

  { Possible values for TUIContainer.UIScaling. }
  TUIScaling = (
    { Do not scale UI. }
    usNone,

    { Scale to fake that the container sizes enclose
      @link(TUIContainer.UIReferenceWidth) and
      @link(TUIContainer.UIReferenceHeight).
      So one size will be equal to reference size, and the other will be equal
      or larger to reference.

      Controls that look at @link(TCastleUserInterface.UIScale) will be affected by this.
      Together with anchors (see @link(TCastleUserInterface.Anchor)),
      this allows to easily design a scalable UI. }
    usEncloseReferenceSize,

    { Scale to fake that the container sizes fit inside
      @link(TUIContainer.UIReferenceWidth) and
      @link(TUIContainer.UIReferenceHeight).
      So one size will be equal to reference size, and the other will be equal
      or smaller to reference.

      Controls that look at @link(TCastleUserInterface.UIScale) will be affected by this.
      Together with anchors (see @link(TCastleUserInterface.Anchor)),
      this allows to easily design a scalable UI. }
    usFitReferenceSize,

    { Scale to fake that the container sizes are smaller/larger
      by an explicit factor @link(TUIContainer.UIExplicitScale).
      Controls that look at @link(TCastleUserInterface.UIScale)
      will be affected by this.

      Like usEncloseReferenceSize or usFitReferenceSize,
      this allows to design a scalable UI.
      In this case, the scale factor has to be calculated by your code
      (by default @link(TUIContainer.UIExplicitScale) is 1.0 and the engine will
      not modify it automatically in any way),
      which allows customizing the scale to your requirements. }
    usExplicitScale,

    { Scale to pretend that the container size is smaller/larger
      by a factor derived from @link(TUIContainer.Dpi).

      This allows to adjust controls to:

      @unorderedList(
        @item(The physical size
          (@link(TUIContainer.Dpi) on iOS and Android really reflects physical size).)
        @item(Or at least to follow user preferred UI scaling
          (@link(TUIContainer.Dpi) desktop is often more like "configurable
          way to scale UI" than derived from actual monitor physical size).)
      )

      This is the same scaling done by normal desktop applications,
      e.g. using Lazarus LCL. Using this scaling is best if your user interface
      using CGE should match (the size of) the user interface done by Lazarus LCL.
    }
    usDpiScale
  );

  { Things that can cause @link(TInputListener.VisibleChange) notification. }
  TCastleUserInterfaceChange = (
    { The look of this control changed.
      This concerns all the things that affect what @link(TCastleUserInterface.Render) does.

      Note that changing chRectangle implies that the look changed too.
      So when chRectangle is in Changes, you should always behave
      like chRender is also in Changes, regardless if it's there or not. }
    chRender,

    { The rectangle (size or position) of the control changed.
      This concerns all the things that affect
      @link(TCastleUserInterface.Rect) or our position inside parent (anchors).

      Note that this is not (necessarily) called when the screen position changed
      just because the parent screen position changed.
      We only notify when the size or position changed with respect to the parent.

      Note that changing chRectangle implies that the look changed too.
      So when chRectangle is in Changes, you should always behave
      like chRender is also in Changes, regardless if it's there or not. }
    chRectangle,

    chCursor,

    { Used by @link(TCamera) descendants to notify that the current
      camera view (position, direction, up and everything related to it) changed. }
    chCamera,

    chExists,

    { A (direct) child control was added or removed. }
    chChildren,

    { A (direct) child control @link(TCastleUserInterface.Exists) value changed. }
    chChildrenExists
  );
  TCastleUserInterfaceChanges = set of TCastleUserInterfaceChange;
  TCastleUserInterfaceChangeEvent = procedure(const Sender: TInputListener;
    const Changes: TCastleUserInterfaceChanges; const ChangeInitiatedByChildren: boolean)
    of object;

  { Abstract user interface container. Connects OpenGL context management
    code with Castle Game Engine controls (TCastleUserInterface, that is the basis
    for all our 2D and 3D rendering). When you use TCastleWindowBase
    (a window) or TCastleControlBase (Lazarus component), they provide
    you a non-abstact implementation of TUIContainer.

    Basically, this class manages a @link(Controls) list.

    We pass our inputs (mouse / key / touch events) to the controls
    on this list. Input goes to the front-most
    (that is, last on the @link(Controls) list) control under
    the event position (or mouse position, or the appropriate touch position).
    We use @link(TCastleUserInterface.CapturesEventsAtPosition) to decide this
    (by default it simply checks control's @link(TCastleUserInterface.RenderRect)
    vs the given position).
    As long as the event is not handled,
    we search for the next control that can handle this event and
    returns @link(TCastleUserInterface.CapturesEventsAtPosition) = @true.

    We also call various methods to every control.
    These include @link(TInputListener.Update), @link(TCastleUserInterface.Render),
    @link(TInputListener.Resize). }
  TUIContainer = class abstract(TComponent)
  private
    type
      TFingerIndexCaptureMap = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<TFingerIndex, TCastleUserInterface>;
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
    FFocus, FNewFocus: TCastleUserInterfaceList;
    { Capture controls, for each FingerIndex.
      The values in this map are never nil. }
    FCaptureInput: TFingerIndexCaptureMap;
    FForceCaptureInput: TCastleUserInterface;
    FTooltipDelay: Single;
    FTooltipDistance: Cardinal;
    FTooltipVisible: boolean;
    FTooltipPosition: TVector2;
    HasLastPositionForTooltip: boolean;
    LastPositionForTooltip: TVector2;
    LastPositionForTooltipTime: TTimerResult;
    Mouse3d: T3DConnexionDevice;
    Mouse3dPollTimer: Single;
    FUIScaling: TUIScaling;
    FUIReferenceWidth: Single;
    FUIReferenceHeight: Single;
    FUIExplicitScale: Single;
    FCalculatedUIScale: Single; //< set on all children
    FFps: TFramesPerSecond;
    FPressed: TKeysPressed;
    FMousePressed: CastleKeysMouse.TMouseButtons;
    FIsMousePositionForMouseLook: boolean;
    FFocusAndMouseCursorValid: boolean;
    FOverrideCursor: TMouseCursor;
    FDefaultFont: TCastleFont;
    FContext: TRenderContext;
    FBackgroundEnable: Boolean;
    FBackgroundColor: TCastleColor;

    procedure ControlsVisibleChange(const Sender: TInputListener;
      const Changes: TCastleUserInterfaceChanges; const ChangeInitiatedByChildren: boolean);
    { Called when the control C is destroyed or just removed from Controls list. }
    procedure DetachNotification(const C: TCastleUserInterface);
    function UseForceCaptureInput: boolean;
    function TryGetFingerOfControl(const C: TCastleUserInterface; out Finger: TFingerIndex): boolean;
    procedure SetUIScaling(const Value: TUIScaling);
    procedure SetUIReferenceWidth(const Value: Single);
    procedure SetUIReferenceHeight(const Value: Single);
    procedure SetUIExplicitScale(const Value: Single);
    procedure UpdateUIScale;
    procedure SetForceCaptureInput(const Value: TCastleUserInterface);
    class procedure RenderControlPrepare(const ViewportRect: TRectangle); static;
    function PassEvents(const C: TCastleUserInterface;
      const CheckMousePosition: Boolean = true): Boolean;
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

    procedure SetInternalCursor(const Value: TMouseCursor); virtual;
    property Cursor: TMouseCursor write SetInternalCursor;
      deprecated 'do not set this, engine will override this. Set TCastleUserInterface.Cursor of your UI controls to control the Cursor.';
    property InternalCursor: TMouseCursor write SetInternalCursor;

    function GetMousePosition: TVector2; virtual;
    procedure SetMousePosition(const Value: TVector2); virtual;
    function GetTouches(const Index: Integer): TTouch; virtual;

    { Get the default UI scale of controls.
      Useful only when GLInitialized, when we know that our size is sensible.
      Almost all UI code should rather be placed in TCastleUserInterface,
      and use TCastleUserInterface.UIScale,
      not directly accessing Container.DefaultUIScale or
      Container.FCalculatedUIScale. }
    function DefaultUIScale: Single;
  public
    const
      DefaultBackgroundColor: TVector4 = (Data: (0.1, 0.1, 0.1, 1));

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Propagate the event to all the @link(Controls) and to our own OnXxx callbacks.

      These methods are called automatically when necessary,
      so usually you don't call them. But in rare cases, it makes sense
      to "fake" some event by calling these methods.

      Most of these methods are called automatically
      by the container owner, like TCastleWindow or TCastleControl.
      Some are called by @link(EventUpdate),
      which is special in this regard, as @link(EventUpdate) is not only
      responsible for calling @link(TInputListener.Update) on all @link(Controls),
      it also calls
      @link(EventJoyAxisMove),
      @link(EventJoyButtonPress),
      @link(EventSensorRotation),
      @link(EventSensorTranslation).

      @groupBegin }
    procedure EventOpen(const OpenWindowsCount: Cardinal); virtual;
    procedure EventClose(const OpenWindowsCount: Cardinal); virtual;
    function EventPress(const Event: TInputPressRelease): boolean; virtual;
    function EventRelease(const Event: TInputPressRelease): boolean; virtual;
    procedure EventUpdate; virtual;
    procedure EventMotion(const Event: TInputMotion); virtual;
    function AllowSuspendForInput: boolean;
    procedure EventBeforeRender; virtual;
    procedure EventRender; virtual;
    procedure EventResize; virtual;
    function EventJoyAxisMove(const JoyID, Axis: Byte): boolean; virtual;
    function EventJoyButtonPress(const JoyID, Button: Byte): boolean; virtual;
    function EventSensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; virtual;
    function EventSensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; virtual;
    { @groupEnd }

    { Controls listening for events (user input, resize, and such) of this container.

      Usually you explicitly add / remove controls to this list
      using the @link(TChildrenControls.InsertFront Controls.InsertFront) or
      @link(TChildrenControls.InsertBack Controls.InsertBack) methods.
      Freeing any control that is on this list
      automatically removes it from this list (we use the TComponent.Notification
      mechanism).

      Controls on the list should be specified in back-to-front order.
      That is, controls at the beginning of this list
      are rendered first, and are last to catch some events, since the rest
      of controls cover them. }
    function Controls: TChildrenControls;

    { Returns the controls that should receive input events,
      from back to front. So the front-most control, that should receive events first,
      is last on this list. }
    property Focus: TCastleUserInterfaceList read FFocus;

    { When the tooltip should be shown (mouse hovers over a control
      with a tooltip) then the TooltipVisible is set to @true,
      and TooltipPosition indicate left-bottom (in screen space, regardless of UIScaling)
      suggested position of the tooltip.

      The tooltip is only detected when TCastleUserInterface.TooltipExists.
      See TCastleUserInterface.TooltipExists and TCastleUserInterface.TooltipStyle and
      TCastleUserInterface.TooltipRender.
      For simple purposes just set TCastleUserInterfaceFont.Tooltip to something
      non-empty.
      @groupBegin }
    property TooltipVisible: boolean read FTooltipVisible;
    property TooltipPosition: TVector2 read FTooltipPosition;
    { @groupEnd }

    { Redraw the contents of of this window, at the nearest suitable time.
      This method does not redraw immediately
      (it does not call @link(EventBeforeRender) and @link(EventRender) inside),
      it only makes sure that they will be called @italic(very soon).
      Calling this on a closed container (with GLInitialized = @false)
      is allowed and ignored. }
    procedure Invalidate; virtual;

    { Is the OpenGL context initialized. }
    function GLInitialized: boolean; virtual;

    { Container size, in pixels.
      This is expressed in real device pixels.
      Prefer using @link(UnscaledWidth) instead of this. @link(UnscaledWidth)
      is more natural when you use UI scaling (@link(UIScaling)),
      and it's simply equal to @name when UI scaling is not used. }
    function Width: Integer; virtual; abstract;

    { Container size, in pixels.
      This is expressed in real device pixels.
      Prefer using @link(UnscaledHeight) instead of this. @link(UnscaledHeight)
      is more natural when you use UI scaling (@link(UIScaling)),
      and it's simply equal to @name when UI scaling is not used. }
    function Height: Integer; virtual; abstract;

    { Container size, in pixels.
      This is expressed in real device pixels, using @link(Width) and @link(Height).
      Prefer using @link(UnscaledRect) instead of this. @link(UnscaledRect)
      is more natural when you use UI scaling (@link(UIScaling)),
      and it's simply equal to @name when UI scaling is not used. }
    function Rect: TRectangle; virtual;

    { Translucent status bar height in the container, in pixels.
      This is expressed in real device pixels.
      Prefer using @link(StatusBarHeight) instead of this. }
    function ScaledStatusBarHeight: Cardinal; virtual;

    { Container width as seen by controls with UI scaling.
      In other words, this is the real @link(Width) with UI scaling
      reversed (divided). Suitable to adjust size of your UI controls
      to container, when UI scaling is used.

      This is equivalent to just @link(Width) when UIScaling is usNone
      (default).

      Note: the name "unscaled" may seem a little unintuitive, but it's consistent.
      We call UI sizes "scaled" when they are expressed in real device pixels,
      because they are usually calculated as "desired size * UIScaling".
      So the UI size is "unscaled" when it's expressed in your "desired size".
      We usually don't use the prefix "unscaled" (e.g. @link(TCastleButton.Width)
      is "unscaled" by we don't call it "UnscaledWidth"; every property inside
      TCastleButton is actually "unscaled"). But here, we use prefix "unscaled",
      because the @link(TUIContainer.Width) is (for historic reasons) the "real" size.

      @seealso UnscaledHeight }
    function UnscaledWidth: Single;

    { Container height as seen by controls with UI scaling.
      @seealso UnscaledWidth }
    function UnscaledHeight: Single;

    { Container rectangle as seen by controls with UI scaling.
      @seealso UnscaledWidth }
    function UnscaledRect: TFloatRectangle;

    { Translucent status bar height inside the container as seen by controls
      with UI scaling.

      Status bar occupies the top part of the container height. Invisible
      status bar returns height equal zero.
      @seealso UnscaledWidth }
    function StatusBarHeight: Single;

    { Current mouse position.
      See @link(TTouch.Position) for a documentation how this is expressed. }
    property MousePosition: TVector2
      read GetMousePosition write SetMousePosition;

    { Dots per inch, specifying the relation of screen pixels to physical size. }
    function Dpi: Single; virtual;

    { Currently pressed mouse buttons. When this changes, you're always
      notified by @link(OnPress) or @link(OnRelease) events.

      This value is always current, in particular it's already updated
      before we call events @link(OnPress) or @link(OnRelease). }
    property MousePressed: TMouseButtons read FMousePressed write FMousePressed;

    { Is the window focused now, which means that keys/mouse events
      are directed to this window. }
    function Focused: boolean; virtual;

    { Keys currently pressed. }
    property Pressed: TKeysPressed read FPressed;

    { Measures application speed. }
    property Fps: TFramesPerSecond read FFps;

    { Currently active touches on the screen.
      This tracks currently pressed fingers, in case of touch devices
      (mobile, like Android and iOS).
      In case of desktops, it tracks the current mouse position,
      regardless if any mouse button is currently pressed.

      Indexed from 0 to TouchesCount - 1.
      @seealso TouchesCount
      @seealso TTouch }
    property Touches[Index: Integer]: TTouch read GetTouches;

    { Count of currently active touches (mouse or fingers pressed) on the screen.
      @seealso Touches }
    function TouchesCount: Integer; virtual;

    property Context: TRenderContext read FContext;

    { Render a TCastleUserInterface (along with all it's children).

      This method can be used to render UI control into an image,
      @link(TGLImage), when it is surrounded by
      @link(TGLImage.RenderToImageBegin)
      and @link(TGLImage.RenderToImageEnd).
      See example ../../../examples/3d_rendering_processing/render_3d_to_image.lpr.

      It can also be used with more low-level @link(TGLRenderToTexture).
      See example ../../../examples/3d_rendering_processing/render_3d_to_texture_and_use_as_quad.lpr.

      This is a good method to render the UI control off-screen.
      It can render any UI control, including e.g. TCastleSceneManager
      with 3D stuff inside TCastleScene.

      The contents of the @link(Controls) list doesn't matter for this method.
      In particular, it doesn't matter if the Control (given as a parameter)
      is present on the list of current @link(Controls).
      This method explicitly renders the given Control parameter (and it's children),
      nothing more, nothing less.

      More details what this method does:

      @unorderedList(
        @item(Temporarily sets
          @link(TInputListener.Container Control.Container), if needed.)

        @item(Makes sure OpenGL resources of the control are initialized.
          If needed, it calls
          @link(TCastleUserInterface.GLContextOpen Control.GLContextOpen) and
          @link(TCastleUserInterface.GLContextClose Control.GLContextClose)
          around.
          This is needed when you want to perform off-screen rendering,
          but the control's OpenGL resources are not initialized yet,
          e.g. because it is not present on the @link(Controls) list.

          Note that doing this repeatedly may be a slowdown
          (how much, it depends on the actual TCastleUserInterface
          -- some controls do nothing in TCastleUserInterface.GLContextOpen,
          some controls do a lot).
          If you want to repeatedly call @link(RenderControl) on the
          same Control, it is more efficient
          to first explicitly create it's OpenGL resources,
          e.g. by calling
          @link(TCastleUserInterface.GLContextOpen Control.GLContextOpen) explicitly.
          Or adding the control to the @link(Controls) list.
        )

        @item(Calls @link(TInputListener.Resize Control.Resize),
          required by some controls (like scene manager) to know viewport size.)

        @item(Calls @link(TCastleUserInterface.BeforeRender Control.BeforeRender),
          required by some controls (like scene manager)
          to prepare resources (like generated textures,
          important for mirrors for screenshots in batch mode).)
      )
    }
    procedure RenderControl(const Control: TCastleUserInterface;
      const ViewportRect: TRectangle);

    { Capture the current container (window) contents to an image
      (or straight to an image file, like png).

      Note that only capturing from the double-buffered OpenGL
      windows (which the default for our TCastleWindow and TCastleControl)
      is reliable. Internally, these methods may need to redraw the screen
      to the back buffer, because that's the only guaranteed way to capture
      OpenGL drawing (you have to capture the back buffer, before swap).

      @groupBegin }
    procedure SaveScreen(const URL: string); overload;
    function SaveScreen: TRGBImage; overload;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload; virtual;
    function SaveScreen(const SaveRect: TFloatRectangle): TRGBImage; overload;
    { @groupEnd }

    { This is internal, and public only for historic reasons.
      @exclude

      Called by controls within this container when something could
      change the container focused control (in @link(TUIContainer.Focus))
      (or it's cursor) or @link(TUIContainer.Focused) or MouseLook.
      In practice, called when TCastleUserInterface.Cursor or
      @link(TCastleUserInterface.CapturesEventsAtPosition) (and so also
      @link(TCastleUserInterface.RenderRect)) results change.

      In practice, it's called through VisibleChange now.

      This recalculates the focused control and the final cursor of
      the container, looking at Container's Controls,
      testing @link(TCastleUserInterface.CapturesEventsAtPosition) with current mouse position,
      and looking at Cursor property of various controls.

      When you add / remove some control
      from the Controls list, or when you move mouse (focused changes)
      this will also be automatically called
      (since focused control or final container cursor may also change then). }
    procedure UpdateFocusAndMouseCursor;

    { Internal for implementing mouse look in cameras. @exclude

      This used to be a function that returns is
      MousePosition perfectly equal to (Width div 2, Height div 2).
      But this is unreliable on Windows 10, where SetCursorPos seems
      to work with some noticeable delay, so IsMousePositionForMouseLook
      was too often considered false.

      So now we just track are we after MakeMousePositionForMouseLook
      (and not after something that moves the cursor wildly, like
      switching windows with Alt+Tab). }
    property IsMousePositionForMouseLook: boolean
      read FIsMousePositionForMouseLook
      write FIsMousePositionForMouseLook;
    { Internal for implementing mouse look in cameras. @exclude }
    procedure MakeMousePositionForMouseLook;

    { Force passing events to the given control first,
      regardless if this control is under the mouse cursor.
      Before we even send events to the currently "capturing" control
      (for example, when you're dragging the slider, it is "capturing" mouse
      events until you release the mouse), they are send to this control.

      The control given here will always have focus
      (that is, @link(TCastleUserInterface.Focused) will be set to true
      shortly after it becomes the ForceCaptureInput).

      An example when this is useful is when you use camera MouseLook,
      and the associated viewport does not fill the full window
      (TCastleAbstractViewport.FullSize is @false, and actual sizes are smaller
      than window, and may not include window center). In this case you want
      to make sure that motion events get passed to this control,
      and that this control has focus (to keep mouse cursor hidden).

      The engine itself @italic(never) automatically sets this property.
      It is up to your application code to set this, if you need. }
    property ForceCaptureInput: TCastleUserInterface
      read FForceCaptureInput write SetForceCaptureInput;

    { When this is not mcDefault, it sets the cursor, regardless of
      cursor specified at the @link(TCastleUserInterface.Cursor) value of
      the focused control. It even takes precedence over any control using
      mcForceNone (so it can force the cursor to be visible anyway). }
    property OverrideCursor: TMouseCursor read FOverrideCursor write FOverrideCursor
      default mcDefault;

    { When the control accepts the "press" event, it automatically captures
      the following motion and release events, hijacking them from other controls,
      regardless of the mouse cursor position. This is usually desirable,
      to allow the control to handle the dragging.
      But sometimes you want to cancel the dragging, and allow other controls
      to handle the following motion and release events, in which case calling this
      method helps. }
    procedure ReleaseCapture(const C: TCastleUserInterface);

    { Load visual settings from an XML file.
      See https://castle-engine.io/manual_castle_settings.php
      for a documentation of the file format.

      This loads UIScaling, UIReferenceWidth, UIReferenceHeight, DefaultFont.
      In the future, it may load more things configurable by the CGE editor
      and CastleSettings.xml file. }
    procedure LoadSettings(const SettingsUrl: String);

    { Delay in seconds before showing the tooltip. }
    property TooltipDelay: Single read FTooltipDelay write FTooltipDelay
      default DefaultTooltipDelay;
    property TooltipDistance: Cardinal read FTooltipDistance write FTooltipDistance
      default DefaultTooltipDistance;

    { Enable automatic scaling of the UI.

      This allows your UI to look correctly on various window sizes
      (great both for mobile and desktop, where window size may vary wildly).
      The idea is that you can set UI controls sizes
      (like @link(TCastleUserInterface.Width),
      @link(TCastleUserInterface.Height)) to a simple constant values.
      And you should also set appropriate anchors
      (choose wisely whether to anchor e.g. to left or right,
      as the simulated window size still has variable aspect ratio).
      And the result will look good on any actual window size.
      All the controls will be scaled to fill the same window part.
      The scaling is actually done by scaling the coordinates, so there's
      no quality loss, whole rendering just adjusts to the actual window size
      in a smart way.

      See @link(TUIScaling) values for precise description how it works. }
    property UIScaling: TUIScaling
      read FUIScaling write SetUIScaling default usNone;

    { Reference width and height to which we fit the container size
      (as seen by TCastleUserInterface implementations) when UIScaling is
      usEncloseReferenceSize or usFitReferenceSize.
      See @link(usEncloseReferenceSize) and @link(usFitReferenceSize)
      for precise description how this works.
      Set both these properties, or set only one (and leave the other as zero).
      @groupBegin }
    property UIReferenceWidth: Single
      read FUIReferenceWidth write SetUIReferenceWidth default 0;
    property UIReferenceHeight: Single
      read FUIReferenceHeight write SetUIReferenceHeight default 0;
    { @groupEnd }

    { Scale of the container size (as seen by TCastleUserInterface implementations)
      when UIScaling is usExplicitScale.
      See @link(usExplicitScale) for precise description how this works. }
    property UIExplicitScale: Single
      read FUIExplicitScale write SetUIExplicitScale default 1.0;

    { Default font (type, size) to be used by all user interface controls.
      Note that each UI control can customize the used font and/or size
      using properties @link(TCastleUserInterfaceFont.CustomFont),
      @link(TCastleUserInterfaceFont.FontSize).

      If this is @nil, we use the global font @link(UIFont)
      that is always assigned. }
    property DefaultFont: TCastleFont read FDefaultFont write FDefaultFont;

    { Before rendering anything else,
      fill the color buffer with @link(BackgroundColor).
      By default this is @true.

      You can set this to @false to gain a little speed,
      if you know you always draw something that fills the whole container.
      For example:

      @unorderedList(
        @item(Use @link(TCastleRectangleControl) with
          @link(TCastleUserInterface.FullSize FullSize) = @true and set
          @link(TCastleRectangleControl.Color) as desired,)

        @item(or use @link(TCastleSceneManager) with
          @link(TCastleUserInterface.FullSize) = @true and
          @link(TCastleAbstractViewport.Transparent) = @false and set
          @link(TCastleSceneManager.BackgroundColor) as desired,)

        @item(eventually you can also call
          @link(TRenderContext.Clear RenderContext.Clear)
          at the beginning of your rendering in @link(OnRender).
          This is the least advised method, as @link(OnRender)
          is performed after drawing all other controls,
          so doing
          @link(TRenderContext.Clear RenderContext.Clear)
          there would force you to make
          all your drawing in @link(OnRender).)
      )

      If you set this to @false, but do not draw something else
      over the entire container, then the
      screen contents at the beginning are undefined.
    }
    property BackgroundEnable: Boolean
      read FBackgroundEnable write FBackgroundEnable default true;

    { Color that fills the window by default.
      By default it is @link(DefaultBackgroundColor), which is very dark gray. }
    property BackgroundColor: TCastleColor
      read FBackgroundColor write FBackgroundColor;
  end;

  { Configurable border size for @link(TCastleUserInterface.Border). }
  TBorder = class(TPersistent)
  strict private
    FTop, FRight, FBottom, FLeft, FAllSides: Single;
    FOnChange: TNotifyEvent;
    procedure SetAllSides(const AValue: Single);
    procedure SetBottom(const AValue: Single);
    procedure SetLeft(const AValue: Single);
    procedure SetRight(const AValue: Single);
    procedure SetTop(const AValue: Single);
  public
    constructor Create(const AOnChange: TNotifyEvent);

    { Total top border (Top + AllSides). }
    function TotalTop: Single;
    { Total right border (Right + AllSides). }
    function TotalRight: Single;
    { Total bottom border (Bottom + AllSides). }
    function TotalBottom: Single;
    { Total left border (Left + AllSides). }
    function TotalLeft: Single;

    { Total horizontal border (TotalLeft + TotalRight). }
    function TotalWidth: Single;
    { Total vertical border (TotalTop + TotalBottom). }
    function TotalHeight: Single;

    { Anything not zero? }
    function Exists: Boolean;
  published
    property Top: Single read FTop write SetTop default 0;
    property Right: Single read FRight write SetRight default 0;
    property Bottom: Single read FBottom write SetBottom default 0;
    property Left: Single read FLeft write SetLeft default 0;
    property AllSides: Single read FAllSides write SetAllSides default 0;
  end;

  { Base class for things that listen to user input. }
  TInputListener = class(TCastleComponent)
  private
    FOnVisibleChange: TCastleUserInterfaceChangeEvent;
    FContainer: TUIContainer;
    FLastSeenContainerWidth, FLastSeenContainerHeight: Integer;
    FLastSeenUIScale: Single;
    FCursor: TMouseCursor;
    FOnCursorChange: TNotifyEvent;
    FExclusiveEvents: boolean;
    FOnUpdate: TUiUpdateEvent;
    FOnPress: TUiPressReleaseEvent;
    FOnRelease: TUiPressReleaseEvent;
    FOnMotion: TUiMotionEvent;
    FVisible: Boolean;
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
      In TCastleUserInterface class, just calls OnCursorChange. }
    procedure DoCursorChange; virtual;
      deprecated 'better override VisibleChange and watch for chCursor in Changes';
  public
    constructor Create(AOwner: TComponent); override;

    (*Handle press or release of a key, mouse button or mouse wheel.
      Return @true if the event was somehow handled,
      which prevents from passing this event to other UI controls.

      When implementing in descendants it is best to override it like this:

      @longCode(#
      function TMyControl.Press(const Event: TInputPressRelease): boolean;
      begin
        Result := inherited;
        if Result then Exit; // exit if ancestor already handled event

        if Event.IsKey(keyEnter) then
        begin
          // do something in reaction on Enter
          Exit(ExclusiveEvents); // ExclusiveEvents is true by default
        end;

        if Event.IsMouseButton(mbLeft) then
        begin
          // do something in reaction on Enter
          Exit(ExclusiveEvents); // ExclusiveEvents is true by default
        end;
      end;
      #)

      Note that releasing of the mouse wheel is not implemented for now,
      neither by CastleWindow or Lazarus CastleControl.

      The events PreviewPress and PreviewRelease are passed first to
      the parent control, before children have a chance to process this event.
      Overriding them makes sense if you draw something
      in @link(RenderOverChildren).

      The events Press and Release are passed to the parent only
      after the children had a chance to process this event.
      Overriding them makes sense if you draw something
      in @link(Render). This is usually more natural, and adviced.

      @groupBegin *)
    function Press(const Event: TInputPressRelease): boolean; virtual;
    function Release(const Event: TInputPressRelease): boolean; virtual;
    function PreviewPress(const Event: TInputPressRelease): boolean; virtual;
    function PreviewRelease(const Event: TInputPressRelease): boolean; virtual;
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

      @param(SecondsPassed Should be calculated like TFramesPerSecond.SecondsPassed,
        and usually it's in fact just taken from TCastleWindowBase.Fps.SecondsPassed.)

      This method may be used, among many other things, to continously
      react to the fact that user pressed some key (or mouse button).
      For example, if holding some key should move some 3D object,
      you should do something like:

      @longCode(#
        if HandleInput then
        begin
          if Container.Pressed[K_Right] then
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
      only for continous handling (like "holding this key makes
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
      see TCastleUserInterface.GetExists) receive Update calls.

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
    procedure VisibleChange(const RectOrCursorChanged: boolean = false); overload; virtual;
      deprecated 'use VisibleChange overload with (TCastleUserInterfaceChanges,boolean) parameters';

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

      @seeAlso TCastleWindowBase.AllowSuspendForInput }
    function AllowSuspendForInput: boolean; virtual;

    { You can resize/reposition your component here,
      for example set @link(TCastleUserInterface.Left) or @link(TCastleUserInterface.Bottom), to react to parent
      size changes.
      Called always when the container (component or window with OpenGL context)
      size changes. Called only when the OpenGL context of the container
      is initialized, so you can be sure that this is called only between
      GLContextOpen and GLContextClose.

      We also make sure to call this once when inserting into
      the controls list
      (like @link(TCastleWindowBase.Controls) or
      @link(TCastleControlBase.Controls) or inside parent TCastleUserInterface),
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
      list (like TCastleWindowBase.Controls) container will automatically
      set itself here, and when removing from container this will be changed
      back to @nil.

      May be @nil if this control is not yet inserted into any container. }
    property Container: TUIContainer read FContainer write SetContainer;

    { Event called when the @link(Cursor) property changes.
      This event is, in normal circumstances, used by the Container,
      so you should not use it in your own programs. }
    property OnCursorChange: TNotifyEvent
      read FOnCursorChange write FOnCursorChange;
      deprecated 'use OnVisibleChange (or override VisibleChange) and watch for Changes that include chCursor';

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

  published
    { Mouse cursor over this control.
      When user moves mouse over the Container, the currently focused
      (topmost under the cursor) control determines the mouse cursor look. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Event that occurs continously on each control.
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

  { Basic user-interface class. All controls derive from this class,
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

    The position is controlled using the @link(Anchor) methods
    or anchor properties, like @link(HorizontalAnchorSelf),
    @link(HorizontalAnchorParent), @link(HorizontalAnchorDelta).
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
  TCastleUserInterface = class(TInputListener)
  private
    FDisableContextOpenClose: Cardinal;
    FFocused: boolean;
    FGLInitialized: boolean;
    FExists: boolean;
    FControls: TChildrenControls;
    FLeft: Single;
    FBottom: Single;
    FParent: TCastleUserInterface;
    FHasHorizontalAnchor: boolean;
    FHorizontalAnchorSelf, FHorizontalAnchorParent: THorizontalPosition;
    FHorizontalAnchorDelta: Single;
    FHasVerticalAnchor: boolean;
    FVerticalAnchorSelf, FVerticalAnchorParent: TVerticalPosition;
    FVerticalAnchorDelta: Single;
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
    FSizeFromChildrenRect: TFloatRectangle;
    FCachedRectWithoutAnchors: TFloatRectangle;
    FUseCachedRectWithoutAnchors: Cardinal; // <> 0 if we should use FCachedRectWithoutAnchors
    FInsideRectWithoutAnchors: Boolean;
    FCulling: Boolean;
    FClipChildren: Boolean;
    FOnRender: TUiNotifyEvent;

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

    procedure SetLeft(const Value: Single);
    procedure SetBottom(const Value: Single);

    procedure SetHorizontalAnchorSelf(const Value: THorizontalPosition);
    procedure SetHorizontalAnchorParent(const Value: THorizontalPosition);
    procedure SetHorizontalAnchorDelta(const Value: Single);
    procedure SetVerticalAnchorSelf(const Value: TVerticalPosition);
    procedure SetVerticalAnchorParent(const Value: TVerticalPosition);
    procedure SetVerticalAnchorDelta(const Value: Single);
    procedure SetEnableUIScaling(const Value: boolean);

    procedure SetFullSize(const Value: boolean);
    procedure SetBorderColor(const Value: TCastleColor);
    procedure SetWidthFraction(const Value: Single);
    procedure SetHeightFraction(const Value: Single);
    procedure SetCulling(const Value: Boolean);
    procedure SetClipChildren(const Value: Boolean);
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);

    { Position and size of this control, assuming it exists,
      in the local coordinates (relative to the parent 2D control,
      although without anchors applied).

      The implementation in this class follows the algorithm
      documented at @link(EffectiveRect).

      @unorderedList(
        @item(@bold(This must ignore)
          the current value of the @link(GetExists) method
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
      in case you use UI scaling (@link(TUIContainer.UIScaling)).
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

    procedure RecursiveRender(const ViewportRect: TRectangle);

    { Recalculate UIScale property value, call UIScaleChanged if needed. }
    procedure CheckUIScaleChanged;
    { Recalculate FLastSeenContainerWidth/Height, call Resize if needed. }
    procedure CheckResize;

    { Show Name and ClassName for debug/log purposes. }
    function DebugName: String;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetContainer(const Value: TUIContainer); override;

    procedure UIScaleChanged; virtual;

    //procedure DoCursorChange; override;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

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
  public
    const
      DefaultWidth = 100.0;
      DefaultHeight = 100.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure VisibleChange(const Changes: TCastleUserInterfaceChanges;
      const ChangeInitiatedByChildren: boolean = false); override;
    procedure InternalAddChild(const C: TComponent); override;
    function PropertySection(const PropertyName: String): TPropertySection; override;

    property Controls [Index: Integer]: TCastleUserInterface read GetControls write SetControls;
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

    { Remove control added by @link(InsertFront) or @link(InsertBack). }
    procedure RemoveControl(const Item: TCastleUserInterface);

    { Index of child control, or -1 if not present. }
    function IndexOfControl(const Item: TCastleUserInterface): Integer;

    { Remove all child controls added by @link(InsertFront) or @link(InsertBack). }
    procedure ClearControls;

    { Return whether item really exists, see @link(Exists).
      Non-existing item does not receive any of the render or input or update calls.
      They only receive @link(GLContextOpen), @link(GLContextClose), @link(Resize)
      calls.

      It TCastleUserInterface class, this returns the value of @link(Exists) property.
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
      is inside @link(RenderRect).
      It also checks whether @link(CapturesEvents) is @true.

      Always treated like @false when GetExists returns @false,
      so the implementation of this method only needs to make checks assuming that
      GetExists = @true.  }
    function CapturesEventsAtPosition(const Position: TVector2): boolean; virtual;

    { Prepare your resources, right before drawing.
      Called only when @link(GetExists) and GLInitialized.

      @bold(Do not explicitly call this method.)
      Instead, render controls by adding them to the
      @link(TUIContainer.Controls) list, or render them explicitly
      (for off-screen rendering) by @link(TUIContainer.RenderControl).
    }
    procedure BeforeRender; virtual;

    { Render a control. Called only when @link(GetExists) and GLInitialized,
      you can depend on it in the implementation of this method.

      @bold(Do not explicitly call this method.)
      Instead, render controls by adding them to the
      @link(TUIContainer.Controls) list, or render them explicitly
      (for off-screen rendering) by @link(TUIContainer.RenderControl).
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
          matrix, e.g. use @link(CastleGLUtils.PerspectiveProjection).)

        @item(@italic((For fixed-function pipeline:))
          The modelview matrix is set to identity. The matrix mode
          is always modelview.)

        @item(@italic((For fixed-function pipeline:))
          The raster position is set to (0,0).
          The (deprecated) WindowPos is also set to (0,0).)

        @item(@italic((For fixed-function pipeline:))
          Texturing, lighting, fog is off.)
      )

      Beware that GLSL @link(TGLSLProgram.Current) has undefined value when this is called.
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
      Then the TCastleWindowBase.TooltipVisible will be detected,
      and your TooltipRender will be called.

      TooltipRender is called in the same way as @link(Render),
      so e.g. you can safely assume that modelview matrix is identity
      and (for 2D) WindowPos is zero.
      TooltipRender is always called as a last (front-most) control.

      @groupBegin }
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
      use the @link(TCastleUserInterface.Exists) property. This allows you to keep the control
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
      it would leave the TCastleUserInterface existing many times on the @link(TUIContainer.Controls)
      list, with the undefined TUIContainer.Exists value. }
    property DisableContextOpenClose: Cardinal
      read FDisableContextOpenClose write FDisableContextOpenClose;

   { Called when this control becomes or stops being focused,
     that is: under the mouse cursor and will receive events.
     In this class, they simply update Focused property.
     You can override this to react to mouse enter / mouse exit events. }
    procedure SetFocused(const Value: boolean); virtual;

    property Focused: boolean read FFocused write SetFocused;

    { Visual parent control. This control is rendered within the parent,
      and it's anchors and coordinates are relative to the parent.
      Parent may be @nil, which means that the @link(Container)
      (if set) is our direct parent. }
    property Parent: TCastleUserInterface read FParent;

    function Rect: TFloatRectangle; virtual;
      deprecated 'instead of using this, use EffectiveRect or RenderRect depending on the situation; instead of overriding this, override PreferredSize';

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

      This method ignores the current value of the @link(GetExists) method
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

    { Position and size of this control, assuming it exists, in screen (container)
      coordinates. The primary use of this is inside @link(Render).
      A proper UI control should adjust to draw precisely in this rectangle. }
    function RenderRect: TFloatRectangle;
    function RenderRectWithBorder: TFloatRectangle;

    function ScreenRect: TRectangle; deprecated 'use RenderRect';

    { How to translate local coords to screen.
      If you use UI scaling, this works in final coordinates
      (after scaling, real pixels on screen). }
    function LocalToScreenTranslation: TVector2;

    { Rectangle filling the parent control (or container), in local coordinates.
      Since this is in local coordinates, the returned rectangle Left and Bottom
      are always zero, unless parent has Border (the returned rectangle
      is shrunk by Parent.Border).
      This is already scaled by UI scaling. }
    function ParentRect: TFloatRectangle;

    { Quick way to enable horizontal anchor, to automatically keep this
      control aligned to parent. Sets
      @link(HorizontalAnchorSelf), @link(HorizontalAnchorParent),
      @link(HorizontalAnchorDelta). }
    procedure Anchor(const AHorizontalAnchor: THorizontalPosition;
      const AHorizontalAnchorDelta: Single = 0); overload;

    { Quick way to enable horizontal anchor, to automatically keep this
      control aligned to parent. Sets
      @link(HorizontalAnchorSelf), @link(HorizontalAnchorParent),
      @link(HorizontalAnchorDelta). }
    procedure Anchor(
      const AHorizontalAnchorSelf, AHorizontalAnchorParent: THorizontalPosition;
      const AHorizontalAnchorDelta: Single = 0); overload;

    { Quick way to enable vertical anchor, to automatically keep this
      control aligned to parent. Sets
      @link(VerticalAnchorSelf), @link(VerticalAnchorParent),
      @link(VerticalAnchorDelta). }
    procedure Anchor(const AVerticalAnchor: TVerticalPosition;
      const AVerticalAnchorDelta: Single = 0); overload;

    { Quick way to enable vertical anchor, to automatically keep this
      control aligned to parent. Sets
      @link(VerticalAnchorSelf), @link(VerticalAnchorParent),
      @link(VerticalAnchorDelta). }
    procedure Anchor(
      const AVerticalAnchorSelf, AVerticalAnchorParent: TVerticalPosition;
      const AVerticalAnchorDelta: Single = 0); overload;

    { Immediately position the control with respect to the parent
      by adjusting @link(Left).
      Deprecated, use @link(Align) with THorizontalPosition. }
    procedure AlignHorizontal(
      const ControlPosition: TPositionRelative = prMiddle;
      const ContainerPosition: TPositionRelative = prMiddle;
      const X: Single = 0); deprecated 'use Align or Anchor';

    { Immediately position the control with respect to the parent
      by adjusting @link(Left).

      Note that using @link(Anchor) is often more comfortable than this method,
      since you only need to set anchor once (for example, right after creating
      the control). In contract, adjusting position using this method
      will typically need to be repeated at each window on resize,
      like in @link(TCastleWindowBase.OnResize). }
    procedure Align(
      const ControlPosition: THorizontalPosition;
      const ContainerPosition: THorizontalPosition;
      const X: Single = 0); overload;

    { Immediately position the control with respect to the parent
      by adjusting @link(Bottom).
      Deprecated, use @link(Align) with TVerticalPosition. }
    procedure AlignVertical(
      const ControlPosition: TPositionRelative = prMiddle;
      const ContainerPosition: TPositionRelative = prMiddle;
      const Y: Single = 0); deprecated 'use Align or Anchor';

    { Immediately position the control with respect to the parent
      by adjusting @link(Bottom).

      Note that using @link(Anchor) is often more comfortable than this method,
      since you only need to set anchor once (for example, right after creating
      the control). In contract, adjusting position using this method
      will typically need to be repeated at each window on resize,
      like in @link(TCastleWindowBase.OnResize). }
    procedure Align(
      const ControlPosition: TVerticalPosition;
      const ContainerPosition: TVerticalPosition;
      const Y: Single = 0); overload;

    { Immediately center the control within the parent,
      both horizontally and vertically.

      Note that using @link(Anchor) is often more comfortable than this method,
      since you only need to set anchor once. For example, right after creating
      the control call @code(Anchor(hpMiddle); Anchor(vpMiddle);).
      In contrast, adjusting position using this method
      will typically need to be repeated at each window on resize,
      like in @link(TCastleWindowBase.OnResize). }
    procedure Center;

    { UI scale of this control, derived from container
      (see @link(TUIContainer.UIScaling) and @link(EnableUIScaling)). }
    property UIScale: Single read FLastSeenUIScale;

    { Override this to prevent resizing some dimension in CGE editor. }
    procedure EditorAllowResize(out ResizeWidth, ResizeHeight: Boolean;
      out Reason: String); virtual;

    property FloatWidth: Single read FWidth write SetWidth stored false;
      deprecated 'use Width';
    property FloatHeight: Single read FHeight write SetHeight stored false;
      deprecated 'use Height';

    { Keep the control in front of other controls (with KeepInFront=@false)
      when inserting.

      TODO: Do not change this property while the control is already
      a children of something. }
    property KeepInFront: boolean read FKeepInFront write FKeepInFront
      default false;

    { Color of the @link(Border), by default completely transparent black. }
    property BorderColor: TCastleColor read FBorderColor write SetBorderColor;

    property HasHorizontalAnchor: boolean
      read FHasHorizontalAnchor write FHasHorizontalAnchor stored false;
      deprecated 'this property does not do anything anymore, anchors are always active';
    property HasVerticalAnchor: boolean
      read FHasVerticalAnchor write FHasVerticalAnchor stored false;
      deprecated 'this property does not do anything anymore, anchors are always active';

    { Is the control possibly visible.
      This is always @true when @link(Culling) is @false (the default). }
    property Visible: Boolean read FVisible;
  published
    { Control is being displayed.
      See @link(Render) for details.
      This event it called @italic(after) the @link(Render) method
      of this control finished. }
    property OnRender: TUiNotifyEvent read FOnRender write FOnRender;

    { Not existing control is not visible, it doesn't receive input
      and generally doesn't exist from the point of view of user.
      You can also remove this from controls list (like
      @link(TCastleWindowBase.Controls)), but often it's more comfortable
      to set this property to false. }
    property Exists: boolean read FExists write SetExists default true;

    { Position from the left side of the parent control.

      It's usually better to use anchors instead of this property.
      For example, instead of setting @code(Control.Left := 10),
      you can call @code(Control.Anchor(hpLeft, 10)),
      or just change @link(HorizontalAnchorDelta).

      Note that the effects of this property and @link(HorizontalAnchorDelta)
      are summed, if you set both. }
    property Left: Single read FLeft write SetLeft stored false default 0;

    { Position from the bottom side of the parent control.

      It's usually better to use anchors instead of this property.
      For example, instead of setting @code(Control.Bottom := 10),
      you can call @code(Control.Anchor(vpBottom, 10)),
      or just change @link(VerticalAnchorDelta).

      Note that the effects of this property and @link(VerticalAnchorDelta)
      are summed, if you set both. }
    property Bottom: Single read FBottom write SetBottom default 0;

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
    property Width: Single read FWidth write SetWidth default DefaultWidth;
    property Height: Single read FHeight write SetHeight default DefaultHeight;
    property WidthFraction: Single read FWidthFraction write SetWidthFraction default 0.0;
    property HeightFraction: Single read FHeightFraction write SetHeightFraction default 0.0;
    { @groupEnd }

    { Adjust size to encompass all the children.
      The properties @link(FullSize), @link(Width), @link(Height)
      are ignored in this case.
      Our @link(Left) and @link(Bottom) still matter.

      Our size is adjusted to all existing children sizes and positions.

      Note that this works only for simple children anchors:
      the left edge of the child to the left edge of the parent,
      the bottom edge of the child to the bottom edge of the parent,
      and so on.
      It is impossible to adjust this calculation to all possible children anchors
      values, since the interpretation of anchors depends on the parent size.

      On the right and top side, we add @link(AutoSizeToChildrenPaddingTop)
      and @link(AutoSizeToChildrenPaddingRight). To make left/bottom padding,
      simply move all children away from the left/bottom edge.
    }
    property AutoSizeToChildren: Boolean
      read FAutoSizeToChildren write SetAutoSizeToChildren default false;

    { Padding added when @link(AutoSizeToChildren) is used. }
    property AutoSizeToChildrenPaddingRight: Single
      read FAutoSizeToChildrenPaddingRight
      write SetAutoSizeToChildrenPaddingRight default 0;
    { Padding added when @link(AutoSizeToChildren) is used. }
    property AutoSizeToChildrenPaddingTop: Single
      read FAutoSizeToChildrenPaddingTop
      write SetAutoSizeToChildrenPaddingTop default 0;

    { Adjust position to align us to the parent horizontally.
      The resulting @link(EffectiveRect) and @link(RenderRect) and @link(RenderRectWithBorder)
      will immediately reflect the new position.

      Note that @link(HorizontalAnchorDelta) is summed with @link(Left).
      It's usually most natural to leave @link(Left) as zero when
      using anchors.

      Note that anchors (as well as @link(Left) and @link(Bottom))
      are ignored when @link(FullSize) is set to true.
      In case of @link(FullSize), the control fills the parent perfectly.

      @italic(Anchor distance is automatically affected by @link(TUIContainer.UIScaling).)

      Which @bold(our) border to align (it's aligned
      to parent @link(HorizontalAnchorParent) border). }
    property HorizontalAnchorSelf: THorizontalPosition
      read FHorizontalAnchorSelf write SetHorizontalAnchorSelf default hpLeft;
    { Which @bold(parent) border is aligned to our @link(HorizontalAnchorSelf) border. }
    property HorizontalAnchorParent: THorizontalPosition
      read FHorizontalAnchorParent write SetHorizontalAnchorParent default hpLeft;
    { Delta between our border and parent. }
    property HorizontalAnchorDelta: Single
      read FHorizontalAnchorDelta write SetHorizontalAnchorDelta default 0;

    { Adjust position to align us to the parent vertically.
      The resulting @link(EffectiveRect) and @link(RenderRect) and @link(RenderRectWithBorder)
      will immediately reflect the new position.

      Note that @link(VerticalAnchorDelta) is summed with @link(Bottom).
      It's usually most natural to leave @link(Bottom) as zero when
      using anchors.

      Note that anchors (as well as @link(Left) and @link(Bottom))
      are ignored when @link(FullSize) is set to true.
      In case of @link(FullSize), the control fills the parent perfectly.

      @italic(Anchor distance is automatically affected by @link(TUIContainer.UIScaling).)

      Which @bold(our) border to align (it's aligned
      to parent @link(VerticalAnchorParent) border). }
    property VerticalAnchorSelf: TVerticalPosition
      read FVerticalAnchorSelf write SetVerticalAnchorSelf default vpBottom;
    { Which @bold(parent) border is aligned to our @link(VerticalAnchorSelf) border. }
    property VerticalAnchorParent: TVerticalPosition
      read FVerticalAnchorParent write SetVerticalAnchorParent default vpBottom;
    { Delta between our border and parent. }
    property VerticalAnchorDelta: Single
      read FVerticalAnchorDelta write SetVerticalAnchorDelta default 0;

    { Enable or disable UI scaling for this particular control.
      See more about UI scaling on @link(TUIContainer.UIScaling) and
      @link(TCastleUserInterface.UIScale). Setting this to @false forces
      @link(TCastleUserInterface.UIScale) to always return 1.0.

      Note that this does not work recursively, i.e. it does not affect
      the children of this control. Setting this to @false does not prevent
      UI scaling on children (you have to turn it off explicitly for children too,
      if you need to disable UI scaling recursively). }
    property EnableUIScaling: boolean
      read FEnableUIScaling write SetEnableUIScaling default true;

    { Capture input events (keyboard, mouse, joystick).
      If @false, then the methods like @link(Press) and @link(Release) will never be called,
      and @link(Update) will always be called with HandleInput = @false.
      The control will never behave like focused.

      The only exception is when this control is set as @link(TUIContainer.ForceCaptureInput).
      In this case, the control will receive inputs.
      In other words, @link(TUIContainer.ForceCaptureInput) overrides
      the intent of this property. }
    property CapturesEvents: boolean read FCapturesEvents write FCapturesEvents
      default true;

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

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleuserinterface_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Simple list of TCastleUserInterface instances. }
  TCastleUserInterfaceList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TCastleUserInterface>)
  public
    { Add child control, at the front of other children. }
    procedure InsertFront(const NewItem: TCastleUserInterface); overload;
    procedure InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertFront(const NewItems: TCastleUserInterfaceList); overload;

    { Add child control, at the back of other children. }
    procedure InsertBack(const NewItem: TCastleUserInterface); overload;
    procedure InsertBackIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertBack(const NewItems: TCastleUserInterfaceList); overload;

    { Insert, honoring @link(TCastleUserInterface.KeepInFront). }
    procedure InsertWithZOrder(Index: Integer; const Item: TCastleUserInterface);
  end;

  TCastleUserInterfaceClass = class of TCastleUserInterface;

  { List of UI controls, with a parent control and container.
    Ordered from back to front.
    Used for @link(TUIContainer.Controls). }
  TChildrenControls = class
  private
    FParent: TCastleUserInterface;
    FContainer: TUIContainer;

    procedure RegisterContainer(const C: TCastleUserInterface; const AContainer: TUIContainer);
    procedure UnregisterContainer(const C: TCastleUserInterface; const AContainer: TUIContainer);
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
        function GetCurrent: TCastleUserInterface;
      public
        constructor Create(AList: TChildrenControls);
        function MoveNext: Boolean;
        property Current: TCastleUserInterface read GetCurrent;
      end;
    {$endif}

    function GetItem(const I: Integer): TCastleUserInterface;
    procedure SetItem(const I: Integer; const Item: TCastleUserInterface);
    { React to add/remove notifications. }
    procedure Notify(Ptr: Pointer; Action: TListNotification);
  public
    constructor Create(AParent: TCastleUserInterface);
    destructor Destroy; override;

    {$ifndef VER2_6}
    function GetEnumerator: TEnumerator;
    {$endif}

    property Items[I: Integer]: TCastleUserInterface read GetItem write SetItem; default;
    function Count: Integer;
    procedure Assign(const Source: TChildrenControls);
    { Remove the Item from this list.
      Note that the given Item should always exist only once on a list
      (it is not allowed to add it multiple times), so there's no @code(RemoveAll)
      method. }
    procedure Remove(const Item: TCastleUserInterface);
    procedure Clear;
    procedure Add(const Item: TCastleUserInterface); deprecated 'use InsertFront or InsertBack';
    procedure Insert(Index: Integer; const Item: TCastleUserInterface);
    function IndexOf(const Item: TCastleUserInterface): Integer;

    { Make sure that NewItem is the only instance of given ReplaceClass
      on the list, replacing old item if necesssary.
      See TCastleObjectList.MakeSingle for precise description. }
    function MakeSingle(ReplaceClass: TCastleUserInterfaceClass; NewItem: TCastleUserInterface;
      AddFront: boolean = true): TCastleUserInterface;

    { Add at the end of the list. }
    procedure InsertFront(const NewItem: TCastleUserInterface); overload;
    procedure InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertFront(const NewItems: TCastleUserInterfaceList); overload;

    { Add at the beginning of the list. }
    procedure InsertBack(const NewItem: TCastleUserInterface); overload;
    procedure InsertBackIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertBack(const NewItems: TCastleUserInterfaceList); overload;

    procedure InsertIfNotExists(const Index: Integer; const NewItem: TCastleUserInterface); deprecated 'use InsertFrontIfNotExists or InsertBackIfNotExists';
    procedure AddIfNotExists(const NewItem: TCastleUserInterface); deprecated 'use InsertFrontIfNotExists or InsertBackIfNotExists';

    { BeginDisableContextOpenClose disables sending
      TCastleUserInterface.GLContextOpen and TCastleUserInterface.GLContextClose to all the controls
      on the list. EndDisableContextOpenClose ends this.
      They work by increasing / decreasing the TCastleUserInterface.DisableContextOpenClose
      for all the items on the list.

      @groupBegin }
    procedure BeginDisableContextOpenClose;
    procedure EndDisableContextOpenClose;
    { @groupEnd }
  end;

  TUIControlPos = TCastleUserInterface deprecated 'use TCastleUserInterface';
  TUIRectangularControl = TCastleUserInterface deprecated 'use TCastleUserInterface';
  TUIControl = TCastleUserInterface deprecated 'use TCastleUserInterface';
  TUIControlSizeable = TCastleUserInterface deprecated 'use TCastleUserInterface';
  TCastleUserInterfaceRect = TCastleUserInterface deprecated 'use TCastleUserInterface';
  // TODO: These aliases will soon be deprecated too.
  { }
  TUIControlChange = TCastleUserInterfaceChange;
  TUIControlChanges = TCastleUserInterfaceChanges;
  TUIControlList = TCastleUserInterfaceList;
  TUIControlChangeEvent = TCastleUserInterfaceChangeEvent;

function OnGLContextOpen: TGLContextEventList; deprecated 'use ApplicationProperties.OnGLContextOpen';
function OnGLContextClose: TGLContextEventList; deprecated 'use ApplicationProperties.OnGLContextClose';
function IsGLContextOpen: boolean; deprecated 'use ApplicationProperties.IsGLContextOpen';

const
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

type
  { Internal for communication with CastleWindow or CastleControl,
    useful by CastleUIState.
    See @link(OnMainContainer).
    @exclude }
  TOnMainContainer = function: TUIContainer of object;

var
  { Internal for communication with CastleWindow or CastleControl,
    useful by CastleUIState.
    @exclude }
  OnMainContainer: TOnMainContainer = nil;

  { Are we inside Castle Game Engine designer mode. }
  CastleDesignMode: Boolean;

{ Render control contents to an RGBA image, using off-screen rendering.
  The background behind the control is filled with BackgroundColor
  (which may be transparent, e.g. with alpha = 0).

  The rendering is done using off-screen FBO.
  Which means that you can request any size, you are @bold(not) limited
  to your current window / control size.

  Make sure that the control is nicely positioned to fill the ViewportRect.
  Usually you want to adjust control size and position,
  and disable UI scaling (set TCastleUserInterface.EnableUIScaling = @false
  if you use TUIContainer.UIScaling).

  This is the @italic(easiest) way to make off-screen rendering,
  i.e. to render 3D things (like TCastleScene or TCastleSceneManager)
  into an image. This is @italic(not the fastest way), as it creates
  new TGLRenderToTexture instance each time,
  and it grabs the image contents to CPU.
  If you want a faster approach, use @link(TUIContainer.RenderControl)
  and render into @link(TGLImage) using @link(TGLImage.RenderToImageBegin)
  and @link(TGLImage.RenderToImageEnd).
}
function RenderControlToImage(const Container: TUIContainer;
  const Control: TCastleUserInterface;
  const ViewportRect: TRectangle;
  const BackgroundColor: TCastleColor): TRGBAlphaImage; overload;

implementation

uses DOM, TypInfo, Math,
  CastleLog, CastleComponentSerialize, CastleXMLUtils, CastleStringUtils,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleGLImages;

{ TTouchList ----------------------------------------------------------------- }

function TTouchList.FindFingerIndex(const FingerIndex: TFingerIndex): Integer;
begin
  for Result := 0 to Count - 1 do
    if List^[Result].FingerIndex = FingerIndex then
      Exit;
  Result := -1;
end;

function TTouchList.GetFingerIndexPosition(const FingerIndex: TFingerIndex): TVector2;
var
  Index: Integer;
begin
  Index := FindFingerIndex(FingerIndex);
  if Index <> -1 then
    Result := List^[Index].Position
  else
    Result := TVector2.Zero;
end;

procedure TTouchList.SetFingerIndexPosition(const FingerIndex: TFingerIndex;
  const Value: TVector2);
var
  Index: Integer;
  NewTouch: PTouch;
begin
  Index := FindFingerIndex(FingerIndex);
  if Index <> -1 then
    List^[Index].Position := Value else
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
  FTooltipDelay := DefaultTooltipDelay;
  FTooltipDistance := DefaultTooltipDistance;
  FCaptureInput := TFingerIndexCaptureMap.Create;
  FUIScaling := usNone;
  FUIExplicitScale := 1.0;
  FCalculatedUIScale := 1.0; // default safe value, in case some TCastleUserInterface will look here
  FFocus := TCastleUserInterfaceList.Create(false);
  FNewFocus := TCastleUserInterfaceList.Create(false);
  FFps := TFramesPerSecond.Create;
  FPressed := TKeysPressed.Create;
  FContext := TRenderContext.Create;
  FBackgroundEnable := true;
  FBackgroundColor := DefaultBackgroundColor;

  { connect 3D device - 3Dconnexion device }
  Mouse3dPollTimer := 0;
  try
    Mouse3d := T3DConnexionDevice.Create('Castle Control');
  except
    on E: Exception do
      WritelnLog('3D Mouse', 'Exception %s when initializing T3DConnexionDevice: %s',
        [E.ClassName, E.Message]);
  end;
end;

destructor TUIContainer.Destroy;
begin
  if RenderContext = FContext then
    RenderContext := nil;
  FreeAndNil(FContext);

  FreeAndNil(FPressed);
  FreeAndNil(FFps);
  FreeAndNil(FControls);
  FreeAndNil(Mouse3d);
  FreeAndNil(FCaptureInput);
  FreeAndNil(FFocus);
  FreeAndNil(FNewFocus);
  { set to nil by SetForceCaptureInput, to detach free notification }
  ForceCaptureInput := nil;
  inherited;
end;

procedure TUIContainer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { Note: do not move niling FForceCaptureInput to DetachNotification,
    as we don't want to nil the public property too eagerly, the value
    of ForceCaptureInput should remain stable. }
  if (Operation = opRemove) and (AComponent = FForceCaptureInput) then
    { set to nil by SetForceCaptureInput to clean nicely }
    ForceCaptureInput := nil;

  if (Operation = opRemove) and (AComponent is TCastleUserInterface) then
    DetachNotification(TCastleUserInterface(AComponent));
end;

procedure TUIContainer.SetForceCaptureInput(const Value: TCastleUserInterface);
begin
  if FForceCaptureInput <> Value then
  begin
    if FForceCaptureInput <> nil then
      FForceCaptureInput.RemoveFreeNotification(Self);
    FForceCaptureInput := Value;
    if FForceCaptureInput <> nil then
      FForceCaptureInput.FreeNotification(Self);
  end;
end;

procedure TUIContainer.DetachNotification(const C: TCastleUserInterface);
var
  Index: Integer;
  FingerIndex: TFingerIndex;
begin
  if FFocus <> nil then
  begin
    Index := FFocus.IndexOf(C);
    if Index <> -1 then
    begin
      C.Focused := false;
      FFocus.Delete(Index);
    end;
  end;

  if FCaptureInput <> nil then
  begin
    while TryGetFingerOfControl(C, FingerIndex) do
      FCaptureInput.Remove(FingerIndex);
  end;
end;

function TUIContainer.TryGetFingerOfControl(const C: TCastleUserInterface; out Finger: TFingerIndex): boolean;
var
  FingerControlPair: TFingerIndexCaptureMap.TDictionaryPair;
begin
  { search for control C among the FCaptureInput values, and return corresponding key }
  for FingerControlPair in FCaptureInput do
    if FingerControlPair.Value = C then
    begin
      Finger := FingerControlPair.Key;
      Result := true;
      Exit;
    end;
  Result := false;
end;

function TUIContainer.UseForceCaptureInput: boolean;
begin
  Result :=
    (ForceCaptureInput <> nil) and
    (ForceCaptureInput.Container = Self) { currently added to our Controls tree } and
    ForceCaptureInput.GetExists;
end;

function TUIContainer.PassEvents(const C: TCastleUserInterface;
  const CheckMousePosition: Boolean): Boolean;
begin
  Result :=
    (not (csDestroying in C.ComponentState)) and
    C.GetExists and
    ((not CheckMousePosition) or C.CapturesEventsAtPosition(MousePosition)) and
    C.FVisible;
end;

procedure TUIContainer.UpdateFocusAndMouseCursor;
var
  AnythingForcesNoneCursor: boolean;

  { Scan all Controls, recursively.
    Update (add) to FNewFocus, update (set to true) AnythingForcesNoneCursor. }
  procedure CalculateNewFocus;

    { AllowAddingToFocus is used to keep track whether we should
      do FNewFocus.Add on new controls. This way when one control obscures
      another, the obscured control does not land on the FNewFocus list.
      However, the obscured control can still affect the AnythingForcesNoneCursor
      value. }
    procedure RecursiveCalculateNewFocus(const C: TCastleUserInterface; var AllowAddingToFocus: boolean);
    var
      I: Integer;
      ChildAllowAddingToFocus: boolean;
    begin
      if PassEvents(C) then
      begin
        if C.Cursor = mcForceNone then
          AnythingForcesNoneCursor := true;

        if AllowAddingToFocus then
        begin
          FNewFocus.Add(C);
          // siblings to C, obscured by C, will not be added to FNewFocus
          AllowAddingToFocus := false;
        end;

        // our children can be added to FNewFocus
        ChildAllowAddingToFocus := true;
        for I := C.ControlsCount - 1 downto 0 do
          RecursiveCalculateNewFocus(C.Controls[I], ChildAllowAddingToFocus);
      end;
    end;

  var
    I: Integer;
    AllowAddingToFocus: boolean;
  begin
    AllowAddingToFocus := true;
    for I := Controls.Count - 1 downto 0 do
      RecursiveCalculateNewFocus(Controls[I], AllowAddingToFocus);
  end;

  { Possibly adds the control to FNewFocus and
    updates the AnythingForcesNoneCursor if needed. }
  procedure AddInFrontOfNewFocus(const C: TCastleUserInterface);
  begin
    if (not (csDestroying in C.ComponentState)) and
       (FNewFocus.IndexOf(C) = -1) then
    begin
      FNewFocus.Add(C);
      if C.Cursor = mcForceNone then
        AnythingForcesNoneCursor := true;
    end;
  end;

  function CalculateMouseCursor: TMouseCursor;
  begin
    Result := mcDefault;

    if Focus.Count <> 0 then
      Result := Focus.Last.Cursor;

    if AnythingForcesNoneCursor then
      Result := mcForceNone;

    if OverrideCursor <> mcDefault then
      Result := OverrideCursor;

    { do not hide when container is not focused (mouse look doesn't work
      then too, so better to not hide mouse) }
    if (not Focused) and (Result in [mcNone, mcForceNone]) then
      Result := mcDefault;
  end;

var
  I: Integer;
  Tmp: TCastleUserInterfaceList;
  ControlUnderFinger0: TCastleUserInterface;
begin
  { since this is called at the end of TChildrenControls.Notify after
    some control is removed, we're paranoid here about checking csDestroying. }

  { FNewFocus is only used by this method. It is only managed by TCastleUserInterface
    to avoid constructing/destructing it in every
    TUIContainer.UpdateFocusAndMouseCursor call. }
  FNewFocus.Clear;
  AnythingForcesNoneCursor := false;

  { Do not scan Controls for focus when csDestroying (in which case Controls
    list may be invalid). Testcase: exit with Alt + F4 from zombie_fighter
    StateAskDialog. }
  if not (csDestroying in ComponentState) then
  begin
    { calculate new FNewFocus value, update AnythingForcesNoneCursor }
    CalculateNewFocus;
    { add controls capturing the input (since they should have Focused = true to
      show them as receiving input) on top of other controls
      (so that e.g. TCastleOnScreenMenu underneath pressed-down button is
      also still focused) }
    if UseForceCaptureInput then
      AddInFrontOfNewFocus(ForceCaptureInput) else
    if FCaptureInput.TryGetValue(0, ControlUnderFinger0) then
      AddInFrontOfNewFocus(ControlUnderFinger0);

    { update TCastleUserInterface.Focused values, based on differences between FFocus and FNewFocus }
    for I := 0 to FNewFocus.Count - 1 do
      if FFocus.IndexOf(FNewFocus[I]) = -1 then
        FNewFocus[I].Focused := true;
    for I := 0 to FFocus.Count - 1 do
      if FNewFocus.IndexOf(FFocus[I]) = -1 then
        FFocus[I].Focused := false;
  end;

  { swap FFocus and FNewFocus, so that FFocus changes to new value,
    and the next UpdateFocusAndMouseCursor has ready FNewFocus value. }
  Tmp := FFocus;
  FFocus := FNewFocus;
  FNewFocus := Tmp;

  InternalCursor := CalculateMouseCursor;
  FFocusAndMouseCursorValid := true;
end;

function TUIContainer.EventSensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean;

  function RecursiveSensorRotation(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        if RecursiveSensorRotation(C.Controls[I]) then
          Exit(true);

      if C.SensorRotation(X, Y, Z, Angle, SecondsPassed) then
        Exit(true);
    end;

    Result := false;
  end;

var
  I: Integer;
begin
  { exit as soon as something returns "true", meaning the event is handled }
  for I := Controls.Count - 1 downto 0 do
    if RecursiveSensorRotation(Controls[I]) then
      Exit(true);
  Result := false;
end;

function TUIContainer.EventSensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean;

  function RecursiveSensorTranslation(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        if RecursiveSensorTranslation(C.Controls[I]) then
          Exit(true);

      if C.SensorTranslation(X, Y, Z, Length, SecondsPassed) then
        Exit(true);
    end;

    Result := false;
  end;

var
  I: Integer;
begin
  { exit as soon as something returns "true", meaning the event is handled }
  for I := Controls.Count - 1 downto 0 do
    if RecursiveSensorTranslation(Controls[I]) then
      Exit(true);
  Result := false;
end;

function TUIContainer.EventJoyAxisMove(const JoyID, Axis: Byte): boolean;

  function RecursiveJoyAxisMove(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        if RecursiveJoyAxisMove(C.Controls[I]) then
          Exit(true);

      if C.JoyAxisMove(JoyID, Axis) then
        Exit(true);
    end;

    Result := false;
  end;

var
  I: Integer;
begin
  { exit as soon as something returns "true", meaning the event is handled }
  for I := Controls.Count - 1 downto 0 do
    if RecursiveJoyAxisMove(Controls[I]) then
      Exit(true);
  Result := false;
end;

function TUIContainer.EventJoyButtonPress(const JoyID, Button: Byte): boolean;

  function RecursiveJoyButtonPress(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        if RecursiveJoyButtonPress(C.Controls[I]) then
          Exit(true);

      if C.JoyButtonPress(JoyID, Button) then
        Exit(true);
    end;

    Result := false;
  end;

var
  I: Integer;
begin
  { exit as soon as something returns "true", meaning the event is handled }
  for I := Controls.Count - 1 downto 0 do
    if RecursiveJoyButtonPress(Controls[I]) then
      Exit(true);
  Result := false;
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
       { reset the time counter to show tooltip, if you moved mouse/finger
         significantly }
       (PointsDistanceSqr(LastPositionForTooltip, MousePosition) >
        Sqr(TooltipDistance)) or
       { on touch devices, the time counter to show tooltip doesn't advance
         if we don't keep the finger pressed down }
       (ApplicationProperties.TouchDevice and (MousePressed = [])) then
    begin
      HasLastPositionForTooltip := true;
      LastPositionForTooltip := MousePosition;
      LastPositionForTooltipTime := T;
      NewTooltipVisible := false;
    end else
      { TODO: allow tooltips on other controls on Focus list,
        if Focus.Last.TooltipExists = false but other control on Focus
        has tooltips.
        Set something like TooltipFocusIndex or just TooltipControl
        to pass correct control to TUIContainer.EventRender then,
        right now we hardcoded there rendering of Focus.Last tooltip. }
      NewTooltipVisible :=
        { make TooltipVisible only when we're over a control that has
          focus. This avoids unnecessary changing of TooltipVisible
          (and related Invalidate) when there's no tooltip possible. }
        (Focus.Count <> 0) and
        Focus.Last.TooltipExists and
        (TimerSeconds(T, LastPositionForTooltipTime) > TooltipDelay);

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

  procedure RecursiveUpdate(const C: TCastleUserInterface; var HandleInput: boolean);
  var
    I: Integer;
    Dummy: boolean;
  begin
    if PassEvents(C, false) then
    begin
      { go downward, from front to back.
        Important for controls watching/setting HandleInput,
        e.g. for sliders/OnScreenMenu to block the scene manager underneath
        from processing arrow keys. }
      I := C.ControlsCount - 1;
      while I >= 0 do
      begin
        // coded this way in case some Update method changes the Controls list
        if I < C.ControlsCount then
          RecursiveUpdate(C.Controls[I], HandleInput);
        Dec(I);
      end;

      if C <> ForceCaptureInput then
      begin
        { Although we call Update for all the existing controls, we look
          at CapturesEventsAtPosition and track HandleInput values.
          See TCastleUserInterface.Update for explanation. }
        if C.CapturesEventsAtPosition(MousePosition) then
        begin
          C.Update(Fps.SecondsPassed, HandleInput);
        end else
        begin
          { controls where CapturesEventsAtPosition = false always get
            HandleInput parameter set to false. }
          Dummy := false;
          C.Update(Fps.SecondsPassed, Dummy);
        end;
      end;
    end;
  end;

  procedure Update3dMouse;
  const
    Mouse3dPollDelay = 0.05;
  var
    Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
    Mouse3dPollSpeed: Single;
  begin
    if Assigned(Mouse3D) and Mouse3D.Loaded then
    begin
      Mouse3dPollTimer := Mouse3dPollTimer - Fps.SecondsPassed;
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
        EventSensorTranslation(Tx, Ty, Tz, TLength, Mouse3dPollSpeed);
        EventSensorRotation(Rx, Ry, Rz, RAngle, Mouse3dPollSpeed);

        { set timer.
          The "repeat ... until" below should not be necessary under normal
          circumstances, as Mouse3dPollDelay should be much larger than typical
          frequency of how often this is checked. But we do it for safety
          (in case something else, like AI or collision detection,
          slows us down *a lot*). }
        repeat
          Mouse3dPollTimer := Mouse3dPollTimer + Mouse3dPollDelay;
        until Mouse3dPollTimer > 0;
      end;
    end;
  end;

  procedure UpdateJoysticks;
  var
    I, J: Integer;
  begin
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
            EventJoyButtonPress(I, J);
        end;
        for J := 0 to Joysticks.GetJoy(I)^.Info.Count.Axes -1 do
        begin
          if Joysticks.AxisPos(I, J) <> 0 then
            EventJoyAxisMove(I, J);
        end;
      end;
    end;
  end;

var
  I: Integer;
  HandleInput: boolean;
begin
  Fps._UpdateBegin;

  UpdateTooltip;

  if not FFocusAndMouseCursorValid then
    UpdateFocusAndMouseCursor; // sets FFocusAndMouseCursorValid to true

  Update3dMouse;
  UpdateJoysticks;

  HandleInput := true;

  { ForceCaptureInput has the 1st chance to process inputs }
  if UseForceCaptureInput then
    ForceCaptureInput.Update(Fps.SecondsPassed, HandleInput);

  I := Controls.Count - 1;
  while I >= 0 do
  begin
    // coded this way in case some Update method changes the Controls list
    if I < Controls.Count then
      RecursiveUpdate(Controls[I], HandleInput);
    Dec(I);
  end;

  if Assigned(OnUpdate) then OnUpdate(Self);
end;

function TUIContainer.EventPress(const Event: TInputPressRelease): boolean;

  function RecursivePress(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      { try C.PreviewPress }
      if (C <> ForceCaptureInput) and C.PreviewPress(Event) then
      begin
        if (Event.EventType = itMouseButton) and
           // See below for explanation of "C.Container = Self" comparison.
           (C.Container = Self) then
          FCaptureInput.AddOrSetValue(Event.FingerIndex, C);
        Exit(true);
      end;

      { try to pass press to C children }
      for I := C.ControlsCount - 1 downto 0 do
        { checking "I < C.ControlsCount" below is a poor safeguard in case
          some Press handler changes the Controls.Count.
          At least we will not crash. }
        if (I < C.ControlsCount) and RecursivePress(C.Controls[I]) then
          Exit(true);

      { try C.Press }
      if (C <> ForceCaptureInput) and C.Press(Event) then
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
          FCaptureInput.AddOrSetValue(Event.FingerIndex, C);
        Exit(true);
      end;
    end;

    Result := false;
  end;

var
  I: Integer;
begin
  Result := false;

  { pass to ForceCaptureInput }
  if UseForceCaptureInput then
  begin
    if ForceCaptureInput.PreviewPress(Event) then
      Exit(true);
    if ForceCaptureInput.Press(Event) then
      Exit(true);
  end;

  { pass to all Controls with TCastleUserInterface.Press event }
  for I := Controls.Count - 1 downto 0 do
    { checking "I < Controls.Count" below is a poor safeguard in case
      some Press handler changes the Controls.Count.
      At least we will not crash. }
    if (I < Controls.Count) and RecursivePress(Controls[I]) then
      Exit(true);

  { pass to container event }
  if Assigned(OnPress) then
  begin
    OnPress(Self, Event);
    Result := true;
  end;
end;

function TUIContainer.EventRelease(const Event: TInputPressRelease): boolean;

  function RecursiveRelease(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      { try C.PreviewRelease }
      if (C <> ForceCaptureInput) and C.PreviewRelease(Event) then
        Exit(true);

      { try to pass release to C children }
      for I := C.ControlsCount - 1 downto 0 do
        if RecursiveRelease(C.Controls[I]) then
          Exit(true);

      { try C.Release }
      if (C <> ForceCaptureInput) and C.Release(Event) then
        Exit(true);
    end;

    Result := false;
  end;

var
  I: Integer;
  Capture: TCastleUserInterface;
begin
  Result := false;

  { pass to ForceCaptureInput }
  if UseForceCaptureInput then
  begin
    if ForceCaptureInput.PreviewRelease(Event) then
      Exit(true);
    if ForceCaptureInput.Release(Event) then
      Exit(true);
  end;

  { pass to control holding capture }

  if not FCaptureInput.TryGetValue(Event.FingerIndex, Capture) then
    Capture := nil;

  if (Capture <> nil) and not Capture.GetExists then
  begin
    { No longer capturing, since the GetExists returns false now.
      We do not send any events to non-existing controls. }
    FCaptureInput.Remove(Event.FingerIndex);
    Capture := nil;
  end;

  if (Capture <> nil) and (MousePressed = []) then
  begin
    { No longer capturing, but do not set Capture to nil (it should receive the Release event). }
    FCaptureInput.Remove(Event.FingerIndex);
  end;

  if (Capture <> nil) and (Capture <> ForceCaptureInput) then
  begin
    Result := Capture.PreviewRelease(Event);
    Result := Capture.Release(Event);
    Exit; // if something is capturing the input, prevent other controls from getting the events
  end;

  { pass to all Controls with TCastleUserInterface.Release event }
  for I := Controls.Count - 1 downto 0 do
    if RecursiveRelease(Controls[I]) then
      Exit(true);

  { pass to container event }
  if Assigned(OnRelease) then
  begin
    OnRelease(Self, Event);
    Result := true;
  end;
end;

procedure TUIContainer.ReleaseCapture(const C: TCastleUserInterface);
var
  FingerIndex: TFingerIndex;
begin
  while TryGetFingerOfControl(C, FingerIndex) do
    FCaptureInput.Remove(FingerIndex);
end;

procedure TUIContainer.EventOpen(const OpenWindowsCount: Cardinal);

  procedure RecursiveGLContextOpen(const C: TCastleUserInterface);
  var
    I: Integer;
  begin
    for I := C.ControlsCount - 1 downto 0 do
      RecursiveGLContextOpen(C.Controls[I]);

    { Check here C.GLInitialized to not call C.GLContextOpen twice.
      Control may have GL resources already initialized if it was added
      e.g. from Application.OnInitialize before EventOpen. }
    if not C.GLInitialized then
      C.GLContextOpen;
  end;

var
  I: Integer;
begin
  if OpenWindowsCount = 1 then
    ApplicationProperties._GLContextOpen;

  { Call GLContextOpen on controls before OnOpen,
    this way OnOpen has controls with GLInitialized = true,
    so using SaveScreen etc. makes more sense there. }
  for I := Controls.Count - 1 downto 0 do
    RecursiveGLContextOpen(Controls[I]);

  if Assigned(OnOpen) then OnOpen(Self);
  if Assigned(OnOpenObject) then OnOpenObject(Self);
end;

procedure TUIContainer.EventClose(const OpenWindowsCount: Cardinal);

  procedure RecursiveGLContextClose(const C: TCastleUserInterface);
  var
    I: Integer;
  begin
    for I := C.ControlsCount - 1 downto 0 do
      RecursiveGLContextClose(C.Controls[I]);

    if C.GLInitialized then
      C.GLContextClose;
  end;

var
  I: Integer;
begin
  { Call SetFocused(false) for all focused controls,
    to e.g. enable DB-aware controls to react. }
  for I := 0 to FFocus.Count - 1 do
    FFocus[I].Focused := false;

  { Call GLContextClose on controls after OnClose,
    consistent with inverse order in OnOpen. }
  if Assigned(OnCloseObject) then OnCloseObject(Self);
  if Assigned(OnClose) then OnClose(Self);

  { call GLContextClose on controls before OnClose.
    This may be called from Close, which may be called from TCastleWindowBase destructor,
    so prepare for Controls being possibly nil now. }
  if Controls <> nil then
    for I := Controls.Count - 1 downto 0 do
      RecursiveGLContextClose(Controls[I]);

  if OpenWindowsCount = 1 then
  begin
    ApplicationProperties._GLContextClose;

    { recreate FContext instance, to reset every variable when context is closed }
    if RenderContext = FContext then
      RenderContext := nil;
    FreeAndNil(FContext);
    FContext := TRenderContext.Create;
  end;
end;

function TUIContainer.AllowSuspendForInput: boolean;

  function RecursiveAllowSuspendForInput(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C, false) then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        if not RecursiveAllowSuspendForInput(C.Controls[I]) then
          Exit(false);

      if not C.AllowSuspendForInput then
        Exit(false);
    end;

    Result := true;
  end;

var
  I: Integer;
begin
  { Do not suspend when you're over a control that may have a tooltip,
    as EventUpdate must track and eventually show tooltip. }
  if (Focus.Count <> 0) and Focus.Last.TooltipExists then
    Exit(false);

  for I := Controls.Count - 1 downto 0 do
    if not RecursiveAllowSuspendForInput(Controls[I]) then
      Exit(false);

  Result := true;
end;

procedure TUIContainer.EventMotion(const Event: TInputMotion);

  function RecursiveMotion(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      { try to pass release to C children }
      for I := C.ControlsCount - 1 downto 0 do
        if RecursiveMotion(C.Controls[I]) then
          Exit(true);

      { try C.Motion itself }
      if (C <> ForceCaptureInput) and C.Motion(Event) then
        Exit(true);
    end;

    Result := false;
  end;

var
  I: Integer;
  Capture: TCastleUserInterface;
begin
  UpdateFocusAndMouseCursor;

  { pass to ForceCaptureInput }
  if UseForceCaptureInput then
  begin
    if ForceCaptureInput.Motion(Event) then
      Exit;
  end;

  { pass to control holding capture }

  if not FCaptureInput.TryGetValue(Event.FingerIndex, Capture) then
    Capture := nil;

  if (Capture <> nil) and not Capture.GetExists then
  begin
    { No longer capturing, since the GetExists returns false now.
      We do not send any events to non-existing controls. }
    FCaptureInput.Remove(Event.FingerIndex);
    Capture := nil;
  end;

  if (Capture <> nil) and (Capture <> ForceCaptureInput) then
  begin
    Capture.Motion(Event);
    Exit; // if something is capturing the input, prevent other controls from getting the events
  end;

  { pass to all Controls }
  for I := Controls.Count - 1 downto 0 do
    if RecursiveMotion(Controls[I]) then
      Exit;

  { pass to container event }
  if Assigned(OnMotion) then
    OnMotion(Self, Event);
end;

procedure TUIContainer.ControlsVisibleChange(const Sender: TInputListener;
  const Changes: TCastleUserInterfaceChanges; const ChangeInitiatedByChildren: boolean);
begin
  { We abort when ChangeInitiatedByChildren = true,
    because this event will be also called with ChangeInitiatedByChildren = false
    for every possible change.
    So this makes a possible optimization for some TUIContainer descendants:
    no need to call Invalidate so many times. }

  if not ChangeInitiatedByChildren then
  begin
    Invalidate;
    if [chRectangle, chCursor, chExists, chChildren] * Changes <> [] then
      FFocusAndMouseCursorValid := false;
  end;
end;

procedure TUIContainer.EventBeforeRender;

  procedure RecursiveBeforeRender(const C: TCastleUserInterface);
  var
    I: Integer;
  begin
    if PassEvents(C, false) then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        RecursiveBeforeRender(C.Controls[I]);

      if C.GLInitialized then
      begin
        C.CheckUIScaleChanged;
        C.CheckResize;
        C.BeforeRender;
      end;
    end;
  end;

var
  I: Integer;
begin
  for I := Controls.Count - 1 downto 0 do
    RecursiveBeforeRender(Controls[I]);

  if Assigned(OnBeforeRender) then OnBeforeRender(Self);
end;

class procedure TUIContainer.RenderControlPrepare(const ViewportRect: TRectangle); static;
begin
  if GLFeatures.EnableFixedFunction then
  begin
    { Set state that is guaranteed for Render2D calls,
      but TCastleUserInterface.Render cannot change it carelessly. }
    {$ifndef OpenGLES}
    glDisable(GL_LIGHTING);
    glDisable(GL_FOG);
    {$endif}
    GLEnableTexture(CastleGLUtils.etNone);
  end;

  glDisable(GL_DEPTH_TEST);

  RenderContext.Viewport := ViewportRect;
  OrthoProjection(FloatRectangle(0, 0, ViewportRect.Width, ViewportRect.Height));

  if GLFeatures.EnableFixedFunction then
  begin
    { Set OpenGL state that may be changed carelessly, and has some
      guaranteed value, for Render2d calls. }
    {$ifndef OpenGLES} glLoadIdentity; {$endif}
    {$warnings off}
    CastleGLUtils.WindowPos := Vector2Integer(0, 0);
    {$warnings on}
  end;
end;

procedure TUIContainer.EventRender;
var
  I: Integer;
begin
  if BackgroundEnable then
    RenderContext.Clear([cbColor], BackgroundColor);

  { draw controls in "to" order, back to front }
  for I := 0 to Controls.Count - 1 do
    Controls[I].RecursiveRender(Rect);

  if TooltipVisible and (Focus.Count <> 0) then
  begin
    RenderControlPrepare(Rect);
    Focus.Last.TooltipRender;
  end;

  RenderControlPrepare(Rect);
  if Assigned(OnRender) then OnRender(Self);
end;

procedure TUIContainer.RenderControl(const Control: TCastleUserInterface;
  const ViewportRect: TRectangle);
var
  NeedsContainerSet, NeedsGLOpen: boolean;
  OldContainer: TUIContainer;
begin
  NeedsContainerSet := Control.Container <> Self;
  NeedsGLOpen := not Control.GLInitialized;

  { TODO: calling the methods below is not recursive,
    it will not prepare the children correctly. }
  if NeedsContainerSet then
  begin
    OldContainer := Control.Container;
    Control.Container := Self;
  end;
  if NeedsGLOpen then
    Control.GLContextOpen;
  Control.CheckResize;
  Control.BeforeRender;
  Control.RecursiveRender(ViewportRect);

  { TODO: calling the methods below is not recursive,
    it will not unprepare the children correctly. }
  if NeedsContainerSet then
    Control.Container := OldContainer;
  if NeedsGLOpen then
    Control.GLContextClose;
end;

procedure TUIContainer.EventResize;
begin
  if UIScaling in [usEncloseReferenceSize, usFitReferenceSize] then
    { usXxxReferenceSize adjust current Width/Height to reference,
      so the FCalculatedUIScale must be adjusted on each resize. }
    UpdateUIScale;

  { Note that we don't cause TCastleUserInterface.Resize calls now.
    They are done before BeforeRender, this way culled UI controls
    (when using @link(TCastleUserInterface.Culling))
    are only updated when they are actually visible.
    This can significantly speed things up when Culling makes sense
    (lots of off-screen controls). }

  { This way control's get Resize before our OnResize,
    useful to process them all reliably in OnResize. }
  if Assigned(OnResize) then OnResize(Self);
end;

function TUIContainer.Controls: TChildrenControls;
begin
  Result := TChildrenControls(FControls);
end;

procedure TUIContainer.MakeMousePositionForMouseLook;
var
  Middle: TVector2;
begin
  if Focused then
  begin
    Middle := Vector2(Width div 2, Height div 2);

    { Check below is MousePosition different than Middle, to avoid setting
      MousePosition in every Update. Setting MousePosition should be optimized
      for this case (when position is already set), but let's check anyway.

      This also avoids infinite loop, when setting MousePosition,
      getting Motion event, setting MousePosition, getting Motion event...
      in a loop.
      Not really likely (as messages will be queued, and some
      MousePosition setting will finally just not generate event Motion),
      but I want to safeguard anyway. }

    if (not IsMousePositionForMouseLook) or
       (not TVector2.PerfectlyEquals(MousePosition, Middle)) then
    begin
      { Note: setting to float position (ContainerWidth/2, ContainerHeight/2)
        seems simpler, but is risky: we if the backend doesn't support sub-pixel accuracy,
        we will never be able to position mouse exactly at half pixel. }
      MousePosition := Middle;

      IsMousePositionForMouseLook := true;
    end;
  end;
end;

procedure TUIContainer.SetUIScaling(const Value: TUIScaling);
begin
  if FUIScaling <> Value then
  begin
    FUIScaling := Value;
    UpdateUIScale;
  end;
end;

procedure TUIContainer.SetUIReferenceWidth(const Value: Single);
begin
  if FUIReferenceWidth <> Value then
  begin
    FUIReferenceWidth := Value;
    UpdateUIScale;
  end;
end;

procedure TUIContainer.SetUIReferenceHeight(const Value: Single);
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

function TUIContainer.DefaultUIScale: Single;
begin
  case UIScaling of
    usNone         : Result := 1;
    usExplicitScale: Result := UIExplicitScale;
    usDpiScale     : Result := Dpi / DefaultDpi;
    usEncloseReferenceSize, usFitReferenceSize:
      begin
        Result := 1;

        { don't do adjustment before our Width/Height are sensible }
        if not GLInitialized then Exit;

        if (UIReferenceWidth <> 0) and (Width > 0) then
        begin
          Result := Width / UIReferenceWidth;
          if (UIReferenceHeight <> 0) and (Height > 0) then
            if UIScaling = usEncloseReferenceSize then
              MinVar(Result, Height / UIReferenceHeight) else
              MaxVar(Result, Height / UIReferenceHeight);
        end else
        if (UIReferenceHeight <> 0) and (Height > 0) then
          Result := Height / UIReferenceHeight;
        // Too talkative when resizing a window (also in castle-editor)
        // WritelnLog('Scaling', 'Automatic scaling to reference sizes %f x %f in effect. Actual window size is %d x %d. Calculated scale is %f, which simulates surface of size %f x %f.',
        //   [UIReferenceWidth, UIReferenceHeight,
        //    Width, Height,
        //    Result, Width / Result, Height / Result]);
      end;
    else raise EInternalError.Create('UIScaling unknown');
  end;
end;

procedure TUIContainer.UpdateUIScale;
begin
  FCalculatedUIScale := DefaultUIScale;

  { Note that we don't cause TCastleUserInterface.UIScaleChanged calls now.
    They are done before BeforeRender, this way culled UI controls
    (when using @link(TCastleUserInterface.Culling))
    are only updated when they are actually visible.
    This can significantly speed things up when Culling makes sense
    (lots of off-screen controls). }
end;

function TUIContainer.UnscaledWidth: Single;
begin
  Result := Width / FCalculatedUIScale;
end;

function TUIContainer.UnscaledHeight: Single;
begin
  Result := Height / FCalculatedUIScale;
end;

function TUIContainer.UnscaledRect: TFloatRectangle;
begin
  Result := FloatRectangle(Rect);
  if not Result.IsEmpty then
  begin
    Result.Width  := Result.Width  / FCalculatedUIScale;
    Result.Height := Result.Height / FCalculatedUIScale;
  end;
end;

function TUIContainer.StatusBarHeight: Single;
begin
  Result := ScaledStatusBarHeight / FCalculatedUIScale;
end;

function TUIContainer.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  EventBeforeRender;
  EventRender;
  { This is correct if we use double-buffer. }
  Result := SaveScreen_NoFlush(SaveRect, cbBack);
end;

procedure TUIContainer.SaveScreen(const URL: string);
var
  Image: TRGBImage;
begin
  Image := SaveScreen;
  try
    WritelnLog('SaveScreen', 'Screen saved to ' + URL);
    SaveImage(Image, URL);
  finally FreeAndNil(Image) end;
end;

function TUIContainer.SaveScreen: TRGBImage;
begin
  Result := SaveScreen(Rect);
end;

function TUIContainer.SaveScreen(const SaveRect: TFloatRectangle): TRGBImage;
begin
  Result := SaveScreen(SaveRect.Round);
end;

function TUIContainer.Dpi: Single;
begin
  { Default implementation, if you cannot query real dpi value of the screen. }
  Result := DefaultDpi;
end;

function TUIContainer.Rect: TRectangle;
begin
  Result := Rectangle(0, 0, Width, Height);
end;

function TUIContainer.ScaledStatusBarHeight: Cardinal;
begin
  Result := 0;
end;

procedure TUIContainer.Invalidate;
begin
  { Default implementation, does nothing, assuming the main program redraws in a loop. }
end;

function TUIContainer.Focused: boolean;
begin
  { Default implementation, assuming that the context is always focused. }
  Result := true;
end;

procedure TUIContainer.SetInternalCursor(const Value: TMouseCursor);
begin
  { Default implementation, ignores new cursor value. }
end;

function TUIContainer.GetMousePosition: TVector2;
begin
  { Default implementation, assuming mouse is glued to the middle of the screen.
    Some methods, like TUIContainer.UpdateFocusAndMouseCursor,
    use this to calculate "focused" CGE control.

    This returns the same value as TUIContainer.MakeMousePositionForMouseLook sets,
    so the "mouse look" will report "no movement", which seems reasonable if we don't
    know mouse position.
  }
  Result := Vector2(Width div 2, Height div 2);
end;

procedure TUIContainer.SetMousePosition(const Value: TVector2);
begin
  { Default implementation, ignores new mouse position. }
end;

function TUIContainer.GetTouches(const Index: Integer): TTouch;
begin
  Assert(Index = 0, 'Base TUIContainer implementation does not support multi-touch, so you can only access Touches[0]');
  Result.FingerIndex := 0;
  Result.Position := MousePosition;
end;

function TUIContainer.TouchesCount: Integer;
begin
  Result := 1;
end;

function TUIContainer.GLInitialized: boolean;
begin
  { Default implementation, assuming the OpenGL context is always initialized. }
  Result := true;
end;

procedure TUIContainer.LoadSettings(const SettingsUrl: String);

  function UIScalingToString(const UIScaling: TUIScaling): String;
  begin
    Result := SEnding(GetEnumName(TypeInfo(TUIScaling), Ord(UIScaling)), 3);
  end;

  function UIScalingFromString(const S: String): TUIScaling;
  begin
    for Result := Low(TUIScaling) to High(TUIScaling) do
      if S = UIScalingToString(Result) then
        Exit;
    raise Exception.CreateFmt('Not a valid value for UIScaling: %s', [S]);
  end;

type
  TDynIntegerArray = array of Integer;

  function ParseIntegerList(const S: String): TDynIntegerArray;
  var
    IntegerList: TIntegerList;
    SeekPos: Integer;
    Token: String;
  begin
    IntegerList := TIntegerList.Create;
    try
      SeekPos := 1;
      repeat
        Token := NextToken(S, SeekPos);
        if Token = '' then Break;
        IntegerList.Add(StrToInt(Token));
      until false;

      if IntegerList.Count = 0 then
        raise Exception.Create('sizes_at_load parameter is an empty list in CastleSettings.xml');

      Result := IntegerList.ToArray;
    finally FreeAndNil(IntegerList) end;
  end;

const
  DefaultUIScaling = usNone;
  DefaultUIReferenceWidth = 0;
  DefaultUIReferenceHeight = 0;
var
  SettingsDoc: TXMLDocument;
  E: TDOMElement;

  // font stuff
  DefaultFontUrl: String;
  DefaultFontSize, DefaultFontLoadSize: Cardinal;
  DefaultFontAntiAliased: Boolean;
  NewDefaultFont: TCastleFont;
  AllSizesAtLoadStr: String;
  AllSizesAtLoad: TDynIntegerArray;

  NewUIScaling: TUIScaling;
  NewUIReferenceWidth, NewUIReferenceHeight: Single;
begin
  // initialize defaults
  NewUIScaling := DefaultUIScaling;
  NewUIReferenceWidth := DefaultUIReferenceWidth;
  NewUIReferenceHeight := DefaultUIReferenceHeight;
  NewDefaultFont := nil;

  SettingsDoc := URLReadXML(SettingsUrl);
  try
    if SettingsDoc.DocumentElement.TagName8 <> 'castle_settings' then
      raise Exception.Create('The root element must be <castle_settings>');

    E := SettingsDoc.DocumentElement.Child('ui_scaling', false);
    if E <> nil then
    begin
      NewUIScaling := UIScalingFromString(
        E.AttributeStringDef('mode', UIScalingToString(DefaultUIScaling)));
      NewUIReferenceWidth :=
        E.AttributeSingleDef('reference_width', DefaultUIReferenceWidth);
      NewUIReferenceHeight :=
        E.AttributeSingleDef('reference_height', DefaultUIReferenceHeight);
    end;

    E := SettingsDoc.DocumentElement.Child('default_font', false);
    if E <> nil then
    begin
      DefaultFontUrl := E.AttributeURL('url', SettingsUrl);
      DefaultFontSize := E.AttributeCardinalDef('size', 20);
      DefaultFontAntiAliased := E.AttributeBooleanDef('anti_aliased', true);

      if E.AttributeString('sizes_at_load', AllSizesAtLoadStr) then
      begin
        AllSizesAtLoad := ParseIntegerList(AllSizesAtLoadStr);
        NewDefaultFont := TCustomizedFont.Create(Self);
        TCustomizedFont(NewDefaultFont).Load(DefaultFontUrl, AllSizesAtLoad, DefaultFontAntiAliased);
      end else
      begin
        DefaultFontLoadSize := E.AttributeCardinalDef('size_at_load', DefaultFontSize);
        NewDefaultFont := TTextureFont.Create(Self);
        TTextureFont(NewDefaultFont).Load(DefaultFontUrl, DefaultFontLoadSize, DefaultFontAntiAliased);
      end;
      NewDefaultFont.Size := DefaultFontSize;
    end;
  finally FreeAndNil(SettingsDoc) end;

  UIScaling := NewUIScaling;
  UIReferenceWidth := NewUIReferenceWidth;
  UIReferenceHeight := NewUIReferenceHeight;
  DefaultFont := NewDefaultFont;
end;

{ TBorder -------------------------------------------------------------------- }

constructor TBorder.Create(const AOnChange: TNotifyEvent);
begin
  inherited Create;
  FOnChange := AOnChange;
end;

procedure TBorder.SetAllSides(const AValue: Single);
begin
  if FAllSides = AValue then Exit;
  FAllSides := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBorder.SetBottom(const AValue: Single);
begin
  if FBottom = AValue then Exit;
  FBottom := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBorder.SetLeft(const AValue: Single);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBorder.SetRight(const AValue: Single);
begin
  if FRight = AValue then Exit;
  FRight := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBorder.SetTop(const AValue: Single);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TBorder.TotalTop: Single;
begin
  Result := FAllSides + FTop;
end;

function TBorder.TotalRight: Single;
begin
  Result := FAllSides + FRight;
end;

function TBorder.TotalBottom: Single;
begin
  Result := FAllSides + FBottom;
end;

function TBorder.TotalLeft: Single;
begin
  Result := FAllSides + FLeft;
end;

function TBorder.TotalWidth: Single;
begin
  Result := 2 * FAllSides + FRight + FLeft;
end;

function TBorder.TotalHeight: Single;
begin
  Result := 2 * FAllSides + FTop + FBottom;
end;

function TBorder.Exists: Boolean;
begin
  Result := (FAllSides <> 0) or
    (FTop <> 0) or
    (FRight <> 0) or
    (FBottom <> 0) or
    (FLeft <> 0);
end;

{ TInputListener ------------------------------------------------------------- }

constructor TInputListener.Create(AOwner: TComponent);
begin
  inherited;
  FExclusiveEvents := true;
  FCursor := mcDefault;
  FVisible := true;
end;

function TInputListener.Press(const Event: TInputPressRelease): boolean;
begin
  Result := false;
  if Assigned(OnPress) then
    OnPress(Self, Event, Result);
end;

function TInputListener.Release(const Event: TInputPressRelease): boolean;
begin
  Result := false;
  if Assigned(OnRelease) then
    OnRelease(Self, Event, Result);
end;

function TInputListener.PreviewPress(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function TInputListener.PreviewRelease(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function TInputListener.Motion(const Event: TInputMotion): boolean;
begin
  Result := false;
  if Assigned(OnMotion) then
    OnMotion(Self, Event, Result);
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
  if Assigned(OnUpdate) then
    OnUpdate(Self, SecondsPassed, HandleInput);
end;

procedure TInputListener.VisibleChange(const RectOrCursorChanged: boolean = false);
begin
end;

procedure TInputListener.VisibleChange(const Changes: TCastleUserInterfaceChanges;
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

  if not ChangeInitiatedByChildren then
  begin
    {$warnings off}
    VisibleChange([chRectangle, chCursor] * Changes <> []);
    {$warnings on}
  end;

  if Container <> nil then
    Container.ControlsVisibleChange(Self, Changes, ChangeInitiatedByChildren);
  if Assigned(OnVisibleChange) then
    OnVisibleChange(Self, Changes, ChangeInitiatedByChildren);
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
    Result := Container.Rect
  else
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
    VisibleChange([chCursor]);
    {$warnings off} // keep deprecated method working
    DoCursorChange;
    {$warnings on}
  end;
end;

procedure TInputListener.DoCursorChange;
begin
  {$warnings off} // keep deprecated event working
  if Assigned(OnCursorChange) then OnCursorChange(Self);
  {$warnings on}
end;

procedure TInputListener.SetContainer(const Value: TUIContainer);
begin
  FContainer := Value;
end;

{ TCastleUserInterface ----------------------------------------------------------------- }

constructor TCastleUserInterface.Create(AOwner: TComponent);
begin
  inherited;
  FExists := true;
  FEnableUIScaling := true;
  FCapturesEvents := true;
  FWidth := DefaultWidth;
  FHeight := DefaultHeight;
  FBorder := TBorder.Create(@BorderChange);
  FLastSeenUIScale := 1.0;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleuserinterface_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

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

procedure TCastleUserInterface.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  inherited;
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

procedure TCastleUserInterface.InternalAddChild(const C: TComponent);
begin
  // matches TCastleUserInterface.GetChildren implementation
  InsertFront(C as TCastleUserInterface);
end;

procedure TCastleUserInterface.CreateControls;
begin
  if FControls = nil then
  begin
    FControls := TChildrenControls.Create(Self);
    TChildrenControls(FControls).Container := Container;
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

procedure TCastleUserInterface.SetContainer(const Value: TUIContainer);
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
  Result := false;
end;

procedure TCastleUserInterface.CheckUIScaleChanged;
var
  NewUIScale: Single;
begin
  if (Container <> nil) and EnableUIScaling then
    NewUIScale := Container.FCalculatedUIScale
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
    { RenderControlPrepare necessary, since TCastleSceneManager could have
      changed RenderContext.Viewport. }
    TUIContainer.RenderControlPrepare(ViewportRect);

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
  if GetExists then
  begin
    CacheRectBegin;

    R := RenderRectWithBorder;

    FVisible := (not Culling) or ClippingRect.Collides(R);
    if FVisible then
    begin
      DrawBorder(R);

      ClipChildrenBegin;

      { We check GLInitialized, because it may happen that a control
        did not receive GLContextOpen yet, in case we cause some rendering
        during TUIContainer.EventOpen (e.g. because some TCastleUserInterface.GLContextOpen
        calls Window.Screenshot, so everything is rendered
        before even the rest of controls received TCastleUserInterface.GLContextOpen).
        See castle_game_engine/tests/testcontainer.pas . }

      if GLInitialized then
      begin
        TUIContainer.RenderControlPrepare(ViewportRect);
        Render;

        if Assigned(OnRender) then
        begin
          TUIContainer.RenderControlPrepare(ViewportRect);
          OnRender(Self);
        end;
      end;

      for I := 0 to ControlsCount - 1 do
        Controls[I].RecursiveRender(ViewportRect);

      if GLInitialized then
      begin
        TUIContainer.RenderControlPrepare(ViewportRect);
        RenderOverChildren;
      end;

      ClipChildrenEnd;
    end;

    CacheRectEnd;
  end;
end;

procedure TCastleUserInterface.TooltipRender;
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

function TCastleUserInterface.GetExists: boolean;
begin
  Result := FExists;
end;

procedure TCastleUserInterface.SetFocused(const Value: boolean);
begin
  FFocused := Value;
end;

procedure TCastleUserInterface.VisibleChange(const Changes: TCastleUserInterfaceChanges;
  const ChangeInitiatedByChildren: boolean);
begin
  inherited;
  if Parent <> nil then
    Parent.VisibleChange(Changes, true);
  if [chRectangle, chChildren] * Changes <> [] then
    FSizeFromChildrenValid := false;
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
    {$ifdef CASTLE_OBJFPC}@{$endif} ReadRealLeft,
    {$ifdef CASTLE_OBJFPC}@{$endif} WriteRealLeft,
    FLeft <> 0);

    // TODO: unfinished tests
(*
  Filer.DefineProperty('Controls',
    {$ifdef CASTLE_OBJFPC}@{$endif} ReadControls,
    {$ifdef CASTLE_OBJFPC}@{$endif} WriteControls,
    ControlsCount <> 0);
*)

  { Code from fpc/trunk/rtl/objpas/classes/compon.inc }
  Temp:=0;
  Ancestor:=TComponent(Filer.Ancestor);
  If Assigned(Ancestor) then Temp:=Ancestor.DesignInfo;
  Filer.Defineproperty('TUIControlPos_Design_Left',
    {$ifdef CASTLE_OBJFPC}@{$endif} readleft,
    {$ifdef CASTLE_OBJFPC}@{$endif} writeleft,
    (longrec(DesignInfo).Lo<>Longrec(temp).Lo));
  Filer.Defineproperty('TUIControlPos_Design_Top',
    {$ifdef CASTLE_OBJFPC}@{$endif} readtop,
    {$ifdef CASTLE_OBJFPC}@{$endif} writetop,
    (longrec(DesignInfo).Hi<>Longrec(temp).Hi));
end;

function TCastleUserInterface.PropertySection(
  const PropertyName: String): TPropertySection;
begin
  case PropertyName of
    'Exists':
      Result := psBasic;
    'FullSize',
    'Width',
    'Height',
    'HeightFraction',
    'WidthFraction',
    'AutoSizeToChildren',
    'AutoSizeToChildrenPaddingTop',
    'AutoSizeToChildrenPaddingRight',
    'HorizontalAnchorSelf',
    'HorizontalAnchorDelta',
    'HorizontalAnchorParent',
    'VerticalAnchorSelf',
    'VerticalAnchorDelta',
    'VerticalAnchorParent',
    'Left',
    'Bottom',
    'Border',
    'BorderColorPersistent':
      Result := psLayout;
    else
      Result := inherited PropertySection(PropertyName);
  end;
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

function TCastleUserInterface.Rect: TFloatRectangle;
begin
  Result := TFloatRectangle.Empty;
end;

procedure TCastleUserInterface.BeforeSizing;
begin
end;

procedure TCastleUserInterface.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  R: TFloatRectangle;
begin
  { Keep the old code, written when Rect was not deprecated
    and PreferredSize was not available, working. }
  {$warnings off}
  R := Rect;
  {$warnings on}
  if (not R.IsEmpty) then // this means that Rect was overridden
  begin
    PreferredWidth := R.Width;
    PreferredHeight := R.Height;
  end;
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
    FSizeFromChildrenRect := TFloatRectangle.Empty;

    for I := 0 to ControlsCount - 1 do
    begin
      C := Controls[I];
      if not C.GetExists then Continue;

      { Calculate ChildRect.
        We cannot use C.EffectiveRect or C.RectWithAnchors now,
        as they would query ParentRect, thus recursively asking for our
        current size.
        Although FSizeFromChildrenValid would prevent from entering an infinite
        loop, it would be still unreliable to use such result. }
      ChildRect := C.FastRectWithoutAnchors.ScaleAround0(1 / UIScale);
      if not ChildRect.IsEmpty then
      begin
        // apply C anchors, at least some cases

        if (C.HorizontalAnchorSelf = hpLeft) and
           (C.HorizontalAnchorParent = hpLeft) then
          ChildRect.Left := ChildRect.Left + C.HorizontalAnchorDelta
        else
        if (C.HorizontalAnchorSelf = hpRight) and
           (C.HorizontalAnchorParent = hpRight) then
          // when right anchor has delta -10, increase width + 10
          ChildRect.Width := ChildRect.Width - C.HorizontalAnchorDelta;

        if (C.VerticalAnchorSelf = vpBottom) and
           (C.VerticalAnchorParent = vpBottom) then
          ChildRect.Bottom := ChildRect.Bottom + C.VerticalAnchorDelta
        else
        if (C.VerticalAnchorSelf = vpTop) and
           (C.VerticalAnchorParent = vpTop) then
          ChildRect.Height := ChildRect.Height - C.VerticalAnchorDelta;

        // apply Border shift
        ChildRect.Left := ChildRect.Left + FBorder.TotalLeft;
        ChildRect.Bottom := ChildRect.Bottom + FBorder.TotalBottom;
      end;

      FSizeFromChildrenRect := FSizeFromChildrenRect + ChildRect;
    end;

    if not FSizeFromChildrenRect.IsEmpty then
    begin
      FSizeFromChildrenRect.Width :=
        FSizeFromChildrenRect.Width + FBorder.TotalRight + AutoSizeToChildrenPaddingRight;
      FSizeFromChildrenRect.Height :=
        FSizeFromChildrenRect.Height + FBorder.TotalTop + AutoSizeToChildrenPaddingTop;
    end;
  end;

var
  PR: TFloatRectangle;
  W, H, BorderW, BorderH: Single;
begin
  if FInsideRectWithoutAnchors then
  begin
    WriteLnWarning('UI control ' + DebugName + ' encountered an endless loop when trying to calculate it''s size. This means that UI child size depends on the parent (e.g. using FullSize or WidthFraction or HeightFraction), while at the same time UI parent size depends on the child (e.g. using AutoSizeToChildren).');
    Exit(TFloatRectangle.Empty);
  end;
  FInsideRectWithoutAnchors := true;

  BeforeSizing;

  if AutoSizeToChildren then
  begin
    UpdateSizeFromChildren;
    if FSizeFromChildrenRect.IsEmpty then
      Result := TFloatRectangle.Empty
    else
      { We do not use FSizeFromChildrenRect.Left/Bottom.
        This would shift children:
        Imagine a child with anchor (on the left) equal 100.
        If this would increase our resulting Rect.Left by 100,
        then child would still have to move another 100 to the left,
        thus landing at total left = 200. }
      Result := FloatRectangle(Left, Bottom,
        FSizeFromChildrenRect.Right, FSizeFromChildrenRect.Top).
        ScaleAround0(UIScale);
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
      Result := FloatRectangle(Left * UIScale, Bottom * UIScale, W, H);
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
  if not FullSize then
  begin
    PR := ParentRect;
    Result.Left := Result.Left +
      Result.AlignCore(HorizontalAnchorSelf, PR, HorizontalAnchorParent,
        UIScale * HorizontalAnchorDelta);
    Result.Bottom := Result.Bottom +
      Result.AlignCore(VerticalAnchorSelf, PR, VerticalAnchorParent,
        UIScale * VerticalAnchorDelta);
  end;
end;

function TCastleUserInterface.RenderRectWithBorder: TFloatRectangle;
var
  T: TVector2;
begin
  Result := RectWithAnchors;
  { transform local to screen space }
  T := LocalToScreenTranslation;
  Result.Left := Result.Left + T[0];
  Result.Bottom := Result.Bottom + T[1];
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

function TCastleUserInterface.LocalToScreenTranslation: TVector2;
var
  RA: TFloatRectangle;
begin
  if Parent <> nil then
  begin
    Result := Parent.LocalToScreenTranslation;
    RA := Parent.RectWithAnchors;
    Result.Data[0] := Result.Data[0] + RA.Left;
    Result.Data[1] := Result.Data[1] + RA.Bottom;
  end else
    Result := TVector2.Zero;
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

procedure TCastleUserInterface.SetHorizontalAnchorDelta(const Value: Single);
begin
  if FHorizontalAnchorDelta <> Value then
  begin
    FHorizontalAnchorDelta := Value;
    VisibleChange([chRectangle]);
  end;
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

procedure TCastleUserInterface.SetVerticalAnchorDelta(const Value: Single);
begin
  if FVerticalAnchorDelta <> Value then
  begin
    FVerticalAnchorDelta := Value;
    VisibleChange([chRectangle]);
  end;
end;

procedure TCastleUserInterface.Anchor(const AHorizontalAnchor: THorizontalPosition;
  const AHorizontalAnchorDelta: Single);
begin
  HorizontalAnchorSelf := AHorizontalAnchor;
  HorizontalAnchorParent := AHorizontalAnchor;
  HorizontalAnchorDelta := AHorizontalAnchorDelta;
end;

procedure TCastleUserInterface.Anchor(
  const AHorizontalAnchorSelf, AHorizontalAnchorParent: THorizontalPosition;
  const AHorizontalAnchorDelta: Single);
begin
  HorizontalAnchorSelf := AHorizontalAnchorSelf;
  HorizontalAnchorParent := AHorizontalAnchorParent;
  HorizontalAnchorDelta := AHorizontalAnchorDelta;
end;

procedure TCastleUserInterface.Anchor(const AVerticalAnchor: TVerticalPosition;
  const AVerticalAnchorDelta: Single);
begin
  VerticalAnchorSelf := AVerticalAnchor;
  VerticalAnchorParent := AVerticalAnchor;
  VerticalAnchorDelta := AVerticalAnchorDelta;
end;

procedure TCastleUserInterface.Anchor(
  const AVerticalAnchorSelf, AVerticalAnchorParent: TVerticalPosition;
  const AVerticalAnchorDelta: Single);
begin
  VerticalAnchorSelf := AVerticalAnchorSelf;
  VerticalAnchorParent := AVerticalAnchorParent;
  VerticalAnchorDelta := AVerticalAnchorDelta;
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

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleuserinterface_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TChildrenControls ------------------------------------------------------------- }

constructor TChildrenControls.Create(AParent: TCastleUserInterface);
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

function TChildrenControls.GetItem(const I: Integer): TCastleUserInterface;
begin
  Result := TCastleUserInterface(FList.Items[I]);
end;

procedure TChildrenControls.SetItem(const I: Integer; const Item: TCastleUserInterface);
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
    with TCastleUserInterface(FList.Items[I]) do
      DisableContextOpenClose := DisableContextOpenClose + 1;
end;

procedure TChildrenControls.EndDisableContextOpenClose;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    with TCastleUserInterface(FList.Items[I]) do
      DisableContextOpenClose := DisableContextOpenClose - 1;
end;

procedure TChildrenControls.InsertFront(const NewItem: TCastleUserInterface);
begin
  Insert(Count, NewItem);
end;

procedure TChildrenControls.InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
begin
  if FList.IndexOf(NewItem) = -1 then
    InsertFront(NewItem);
end;

procedure TChildrenControls.InsertFront(const NewItems: TCastleUserInterfaceList);
var
  I: Integer;
begin
  for I := 0 to NewItems.Count - 1 do
    InsertFront(NewItems[I]);
end;

procedure TChildrenControls.InsertBack(const NewItem: TCastleUserInterface);
begin
  FList.Insert(0, NewItem);
end;

procedure TChildrenControls.InsertBackIfNotExists(const NewItem: TCastleUserInterface);
begin
  if FList.IndexOf(NewItem) = -1 then
    InsertBack(NewItem);
end;

procedure TChildrenControls.Add(const Item: TCastleUserInterface);
begin
  InsertFront(Item);
end;

procedure TChildrenControls.Insert(Index: Integer; const Item: TCastleUserInterface);
var
  I: Integer;
begin
  { TODO: code duplicated with TCastleUserInterfaceList.InsertWithZOrder }
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

procedure TChildrenControls.InsertIfNotExists(const Index: Integer; const NewItem: TCastleUserInterface);
begin
  Insert(Index, NewItem);
end;

procedure TChildrenControls.AddIfNotExists(const NewItem: TCastleUserInterface);
begin
  Insert(Count, NewItem);
end;

function TChildrenControls.IndexOf(const Item: TCastleUserInterface): Integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TChildrenControls.InsertBack(const NewItems: TCastleUserInterfaceList);
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
  C: TCastleUserInterface;
begin
  { TODO: while this updating works cool,
    if the Parent or Container is destroyed
    before children --- the children will keep invalid reference. }

  C := TCastleUserInterface(Ptr);
  case Action of
    lnAdded:
      begin
        if ((C.FContainer <> nil) or (C.FParent <> nil)) and
           ((Container <> nil) or (FParent <> nil)) then
          WritelnWarning('UI', 'Inserting to the UI list (InsertFront, InsertBack) an item that is already a part of other UI list: ' + C.DebugName + '. The result is undefined, you cannot insert the same TCastleUserInterface instance multiple times.');
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
    if (FParent <> nil) and not (csDestroying in FParent.ComponentState) then
      FParent.VisibleChange([chChildren])
    else
    begin
      { if this is top-most control inside Container,
        we need to call ControlsVisibleChange directly. }
      Container.ControlsVisibleChange(C, [chChildren], false);
    end;
  end;
end;

procedure TChildrenControls.TCaptureFreeNotifications.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { We have to remove a reference to the object from list.
    This is crucial, various methods assume that all objects on
    the list are always valid objects (no invalid references,
    even for a short time). }

  if (Operation = opRemove) and (AComponent is TCastleUserInterface) then
    Parent.FList.Remove(AComponent);
end;

procedure TChildrenControls.Assign(const Source: TChildrenControls);
begin
  FList.Assign(Source.FList);
end;

procedure TChildrenControls.Remove(const Item: TCastleUserInterface);
begin
  FList.Remove(Item);
end;

procedure TChildrenControls.Clear;
begin
  FList.Clear;
end;

function TChildrenControls.MakeSingle(ReplaceClass: TCastleUserInterfaceClass; NewItem: TCastleUserInterface;
  AddFront: boolean): TCastleUserInterface;
begin
  Result := FList.MakeSingle(ReplaceClass, NewItem, AddFront) as TCastleUserInterface;
end;

procedure TChildrenControls.RegisterContainer(
  const C: TCastleUserInterface; const AContainer: TUIContainer);
begin
  { Register AContainer to be notified of control destruction. }
  C.FreeNotification(AContainer);

  C.Container := AContainer;
  C.CheckUIScaleChanged;

  { Make sure Resize is called at the nearest opportunity.
    For historic reasons, some controls may depend that Resize happens
    immediately after adding control to the container,
    they may even initialize GL resources there. }
  C.FLastSeenContainerWidth := 0;
  C.FLastSeenContainerHeight := 0;

  if AContainer.GLInitialized then
  begin
    if C.DisableContextOpenClose = 0 then
      C.GLContextOpen;
    AContainer.Invalidate;
    { Call initial Resize for control.
      If window OpenGL context is not yet initialized, defer it to later
      (we will call CheckResize before BeforeRender). }
    C.CheckResize;
  end;
end;

procedure TChildrenControls.UnregisterContainer(
  const C: TCastleUserInterface; const AContainer: TUIContainer);
begin
  if AContainer.GLInitialized and
     (C.DisableContextOpenClose = 0) then
    C.GLContextClose;

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
function TChildrenControls.TEnumerator.GetCurrent: TCastleUserInterface;
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

{ TCastleUserInterfaceList ------------------------------------------------------------- }

procedure TCastleUserInterfaceList.InsertFront(const NewItem: TCastleUserInterface);
begin
  InsertWithZOrder(Count, NewItem);
end;

procedure TCastleUserInterfaceList.InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
begin
  if IndexOf(NewItem) = -1 then
    InsertFront(NewItem);
end;

procedure TCastleUserInterfaceList.InsertFront(const NewItems: TCastleUserInterfaceList);
var
  I: Integer;
begin
  for I := 0 to NewItems.Count - 1 do
    InsertFront(NewItems[I]);
end;

procedure TCastleUserInterfaceList.InsertBack(const NewItem: TCastleUserInterface);
begin
  InsertWithZOrder(0, NewItem);
end;

procedure TCastleUserInterfaceList.InsertBackIfNotExists(const NewItem: TCastleUserInterface);
begin
  if IndexOf(NewItem) = -1 then
    InsertBack(NewItem);
end;

procedure TCastleUserInterfaceList.InsertBack(const NewItems: TCastleUserInterfaceList);
var
  I: Integer;
begin
  for I := NewItems.Count - 1 downto 0 do
    InsertBack(NewItems[I]);
end;

procedure TCastleUserInterfaceList.InsertWithZOrder(Index: Integer; const Item: TCastleUserInterface);
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

{ globals -------------------------------------------------------------------- }

function OnGLContextOpen: TGLContextEventList;
begin
  Result := ApplicationProperties.OnGLContextOpen;
end;

function OnGLContextClose: TGLContextEventList;
begin
  Result := ApplicationProperties.OnGLContextClose;
end;

function IsGLContextOpen: boolean;
begin
  Result := ApplicationProperties.IsGLContextOpen;
end;

function RenderControlToImage(const Container: TUIContainer;
  const Control: TCastleUserInterface;
  const ViewportRect: TRectangle;
  const BackgroundColor: TCastleColor): TRGBAlphaImage;

  function CreateTargetTexture(const W, H: Integer): TGLTextureId;
  var
    InitialImage: TCastleImage;
  begin
    InitialImage := TRGBAlphaImage.Create(W, H);
    try
      InitialImage.URL := 'generated:/temporary-render-to-texture';
      InitialImage.Clear(Vector4Byte(255, 0, 255, 255));
      Result := LoadGLTexture(InitialImage,
        TextureFilter(minNearest, magNearest),
        Texture2DClampToEdge, nil, true);
    finally FreeAndNil(InitialImage) end;
  end;

var
  W, H: Integer;
  RenderToTexture: TGLRenderToTexture;
  TargetTexture: TGLTextureId;
begin
  W := ViewportRect.Width;
  H := ViewportRect.Height;
  RenderToTexture := TGLRenderToTexture.Create(W, H);
  try
    // RenderToTexture.Buffer := tbNone;
    // RenderToTexture.ColorBufferAlpha := true;

    RenderToTexture.Buffer := tbColor;
    TargetTexture := CreateTargetTexture(W, H);
    RenderToTexture.SetTexture(TargetTexture, GL_TEXTURE_2D);
    RenderToTexture.GLContextOpen;
    RenderToTexture.RenderBegin;

    { actually render }
    RenderContext.Clear([cbColor], BackgroundColor);
    Container.RenderControl(Control, ViewportRect);

    RenderToTexture.RenderEnd;

    Result := TRGBAlphaImage.Create(W, H);
    SaveTextureContents(Result, TargetTexture);

    RenderToTexture.GLContextClose;

    glFreeTexture(TargetTexture);
  finally FreeAndNil(RenderToTexture) end;
end;

procedure DoInitialization;
begin
  RegisterSerializableComponent(TCastleUserInterface, 'Empty Rectangle');
end;

initialization
  DoInitialization;
end.
