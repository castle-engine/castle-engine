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

  { Abstract user interface container. Connects OpenGL context management
    code with Castle Game Engine controls (TCastleUserInterface, that is the basis
    for all our 2D and 3D rendering). When you use TCastleWindow
    (a window) or TCastleControl (Lazarus component), they provide
    you a non-abstract implementation of TCastleContainer.

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
    These include @link(TCastleUserInterface.Update), @link(TCastleUserInterface.Render),
    @link(TCastleUserInterface.Resize). }
  TCastleContainer = class abstract(TComponent)
  strict private
    type
      TFingerIndexCaptureMap = {$ifdef FPC}specialize{$endif} TDictionary<TFingerIndex, TCastleUserInterface>;
    var
    FOnOpen, FOnClose: TContainerEvent;
    FOnOpenObject, FOnCloseObject: TContainerObjectEvent;
    FOnBeforeRender, FOnRender: TContainerEvent;
    FOnResize: TContainerEvent;
    FOnPress, FOnRelease: TInputPressReleaseEvent;
    FOnMotion: TInputMotionEvent;
    FOnUpdate: TContainerEvent;
    FFocus, FNewFocus: TCastleUserInterfaceList;
    { Capture controls, for each FingerIndex.
      The values in this map are never nil. }
    FCaptureInput: TFingerIndexCaptureMap;
    FForceCaptureInput: TCastleUserInterface;
    FTooltipDelay: Single;
    FTooltipDistance: Cardinal;
    FTooltipPosition: TVector2;
    FTooltipParent: TCastleUserInterface; // @nil if do not display
    HasLastPositionForTooltip: boolean;
    LastPositionForTooltip: TVector2;
    LastPositionForTooltipTime: TTimerResult;
    Mouse3d: T3DConnexionDevice;
    Mouse3dPollTimer: Single;
    FDpi: Single;
    FUIScaling: TUIScaling;
    FUIReferenceWidth: Single;
    FUIReferenceHeight: Single;
    FUIExplicitScale: Single;
    FUIScale: Single;
    FFps: TFramesPerSecond;
    FPressed: TKeysPressed;
    FMousePressed: TCastleMouseButtons;
    FMouseLookIgnoreNextMotion: Boolean;
    FMouseLookWaitingForMiddle: Boolean;
    FMouseLookMotionToSubtract: TVector2;
    FFocusAndMouseCursorValid: boolean;
    FOverrideCursor: TMouseCursor;
    FDefaultFont: TCastleAbstractFont;
    FContext: TRenderContext;
    FBackgroundEnable: Boolean;
    FBackgroundColor: TCastleColor;
    FInputInspector: TInputInspector;
    FInspector: TCastleUserInterface; //< always TCastleInspector

    function GetView: TCastleView;
    procedure SetView(const Value: TCastleView);
    function GetFrontView: TCastleView;
    function GetViewStack(const Index: Integer): TCastleView;

    function UseForceCaptureInput: boolean;
    function TryGetFingerOfControl(const C: TCastleUserInterface; out Finger: TFingerIndex): boolean;
    procedure SetDpi(const Value: Single);
    procedure SetUIScaling(const Value: TUIScaling);
    procedure SetUIReferenceWidth(const Value: Single);
    procedure SetUIReferenceHeight(const Value: Single);
    procedure SetUIExplicitScale(const Value: Single);
    procedure UpdateUIScale;
    procedure SetForceCaptureInput(const Value: TCastleUserInterface);
    function PassEvents(const C: TCastleUserInterface;
      const CheckMousePosition: Boolean = true): Boolean; overload;
    function PassEvents(const C: TCastleUserInterface;
      const EventPosition: TVector2;
      const CheckEventPosition: Boolean = true): Boolean; overload;
    function PassEvents(const C: TCastleUserInterface;
      const Event: TInputPressRelease;
      const CheckEventPosition: Boolean = true): Boolean; overload;
    function PassEvents(const C: TCastleUserInterface;
      const Event: TInputMotion;
      const CheckEventPosition: Boolean = true): Boolean; overload;
    function GetInspectorKey: TKey;
    procedure SetInspectorKey(const Value: TKey);
    { Create / destroy FInspector. }
    procedure ToggleInspector;
  private
    FViewStack: TCastleViewList;
    FDisableStackChange: Cardinal;
    FControls: TInternalChildrenControls;
    procedure ControlsVisibleChange(const Sender: TCastleUserInterface;
      const Changes: TCastleUserInterfaceChanges; const ChangeInitiatedByChildren: boolean);
    class procedure RenderControlPrepare(const ViewportRect: TRectangle);
    { Called when the control C is destroyed or just removed from Controls list. }
    procedure DetachNotification(const C: TCastleUserInterface);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { These should only be get/set by a container owner,
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
    {$ifdef FPC}
    property Cursor: TMouseCursor write SetInternalCursor;
      deprecated 'do not set this, engine will override this. Set TCastleUserInterface.Cursor of your UI controls to control the Cursor.';
    {$endif}
    property InternalCursor: TMouseCursor write SetInternalCursor;

    function GetMousePosition: TVector2; virtual;
    procedure SetMousePosition(const Value: TVector2); virtual;
    function GetTouches(const Index: Integer): TTouch; virtual;

    { Does setting MousePosition also causes Motion / OnMotion events.
      While we tried to make everything work reliably always,
      the mouse look logic just needs to know this.

      It is easy to test it in practice.
      Just run examples/window/window_events.lpr,
      move mouse around, and press "5" (this does "MousePosition := window middle"). }
    function SettingMousePositionCausesMotion: Boolean; virtual;
  public
    const
      DefaultBackgroundColor: TVector4 = (X: 0.1; Y: 0.1; Z: 0.1; W: 1);

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
      responsible for calling @link(TCastleUserInterface.Update) on all @link(Controls),
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

    { Dots (pixels) per inch. Describes how many pixels fit on a physical inch.
      So this is determined by the screen resolution in pixels,
      and by the physical size of the device.

      Some systems may expose a value that actually reflects user preference
      "how to scale the user-interface", where 96 (DefaultDpi) is default.
      So do not depend that it is actually related to the physical monitor size.
      See https://developer.gnome.org/gdk2/stable/GdkScreen.html#gdk-screen-set-resolution . }
    property Dpi: Single read FDpi write SetDpi {$ifdef FPC}default DefaultDpi{$endif};

    { Get the default UI scale of controls.
      Useful only when GLInitialized, when we know that our size is sensible.

      UI code should prefer to use TCastleUserInterface.UIScale,
      not directly accessing this TCastleContainer.UIScale
      (this way the code respects the TCastleUserInterface.EnableUIScaling value). }
    property UIScale: Single read FUIScale;

    { @exclude }
    class function InternalCalculateUIScale(
      const AUIScaling: TUIScaling;
      const AUIReferenceWidth: Single;
      const AUIReferenceHeight: Single;
      const AUIExplicitScale: Single;
      const ADpi, AWidth, AHeight: Single): Single;

    { Controls listening for events (user input, resize, and such) of this container.

      Usually you explicitly add / remove controls to this list
      using the @link(TInternalChildrenControls.InsertFront Controls.InsertFront) or
      @link(TInternalChildrenControls.InsertBack Controls.InsertBack) methods.
      Freeing any control that is on this list
      automatically removes it from this list (we use the TComponent.Notification
      mechanism).

      Controls on the list should be specified in back-to-front order.
      That is, controls at the beginning of this list
      are rendered first, and are last to catch some events, since the rest
      of controls cover them. }
    function Controls: TInternalChildrenControls;

    { Returns the controls that should receive input events,
      from back to front. So the front-most control, that should receive events first,
      is last on this list. }
    property Focus: TCastleUserInterfaceList read FFocus;

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
      We usually don't use the prefix "unscaled" (e.g. @link(TCastleUserInterface.Width)
      is "unscaled" by we don't call it "UnscaledWidth"). But here, we use prefix "unscaled",
      because the @link(TCastleContainer.Width) is (for historic reasons) the "real" size.

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

    { Currently pressed mouse buttons. When this changes, you're always
      notified by @link(OnPress) or @link(OnRelease) events.

      This value is always current, in particular it's already updated
      before we call events @link(OnPress) or @link(OnRelease). }
    property MousePressed: TCastleMouseButtons read FMousePressed write FMousePressed;

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
    property Touches[const Index: Integer]: TTouch read GetTouches;

    { Count of currently active touches (mouse or fingers pressed) on the screen.
      @seealso Touches }
    function TouchesCount: Integer; virtual;

    property Context: TRenderContext read FContext;

    { Render a TCastleUserInterface (along with all it's children).

      This method can be used to render UI control into an image,
      @link(TDrawableImage), when it is surrounded by
      @link(TDrawableImage.RenderToImageBegin)
      and @link(TDrawableImage.RenderToImageEnd).
      See CGE examples/viewport_and_scenes/render_3d_to_image/ .

      It can also be used with more low-level @link(TGLRenderToTexture).
      See CGE examples/viewport_and_scenes/render_3d_to_texture_and_use_as_quad/ .

      This is a good method to render the UI control off-screen.
      It can render any UI control, including e.g. TCastleViewport
      with 3D stuff inside TCastleScene.

      The contents of the @link(Controls) list doesn't matter for this method.
      In particular, it doesn't matter if the Control (given as a parameter)
      is present on the list of current @link(Controls).
      This method explicitly renders the given Control parameter (and it's children),
      nothing more, nothing less.

      More details what this method does:

      @unorderedList(
        @item(Temporarily sets
          @link(TCastleUserInterface.Container Control.Container), if needed.)

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

        @item(Calls @link(TCastleUserInterface.Resize Control.Resize),
          which may be expected by some controls.)

        @item(Calls @link(TCastleUserInterface.BeforeRender Control.BeforeRender),
          which may be expected by some controls.)
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

    { Capture the current container (window) contents to an image with alpha.

      An example:
      @includeCode(../../examples/short_api_samples/save_screen_rgba/save_screen_rgba.dpr)
      @groupBegin }
    function SaveScreenRgba(const SaveRect: TRectangle): TRGBAlphaImage; overload;
    function SaveScreenRgba: TRGBAlphaImage; overload;
    { @groupEnd }

    { Capture the current container (window) contents to an image and save it to file,
      following the current platform/user preferred directory to store screenshots.

      On Windows, this saves files to user's "My Pictures" directory.
      On Unix (using freedesktop standard) this saves files to directory like ~/Pictures .
      On macOS, this saves files to the home directory right now.
      On other platforms, it may follow the most established convention,
      or abort if no place (where we have permissions to store screenshots) exists.

      You can use SaveScreenPath yourself to have more control over the target location.

      Returns the saved file URL, so that you can e.g. show it to user. }
    function SaveScreenToDefaultFile: String;

    { This is internal, and public only for historic reasons.
      @exclude

      Called by controls within this container when something could
      change the container focused control (in @link(TCastleContainer.Focus))
      (or it's cursor) or @link(TCastleContainer.Focused) or MouseLook.
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

    { Internal. @exclude
      See @link(MouseLookDelta). }
    procedure MouseLookIgnoreNextMotion;

    { For tracking mouse look. See @link(MouseLookDelta). }
    procedure MouseLookPress;

    { For tracking mouse look. See @link(MouseLookDelta). }
    procedure MouseLookUpdate;

    { Read mouse position delta from ControlRect middle,
      and try to set mouse position to ControlRect middle.

      ControlRect is usually the container rectangle (in final device coordinates)
      but in general it should be the rectangle of control over which we should do mouse look
      (so TCastleUserInterface.RenderRect).

      This can be used to perform "mouse look" or a similar effect,
      when user doesn't see the mouse cursor, but user can move something by dragging with mouse.
      Moreover, user should not notice any "bounds" to this dragging (that's why we try
      to keep mouse position in ControlRect middle,
      to avoid screen borders from acting as constrains on mouse movement).

      This is automatically used by @link(TCastleWalkNavigation.MouseLook).
      You can use it yourself for custom effects "like mouse look". The template to use this
      is below.
      See the CGE example examples/user_interface/dragging_test/ for a working code
      demonstrating this.

      @longCode(#

      function TMyState.Press(const Event: TInputPressRelease): Boolean;
      begin
        Result := inherited;
        if Result then Exit;

        if Event.IsMouseButton(buttonLeft) then
        begin
          Drag := true;
          Cursor := mcForceNone;
          Container.MouseLookPress;
        end;
      end;

      function TMyState.Release(const Event: TInputPressRelease): Boolean;
      begin
        Result := inherited;
        if Result then Exit;

        if Event.IsMouseButton(buttonLeft) then
        begin
          Drag := false;
          Cursor := mcDefault;
        end;
      end;

      procedure TMyState.Update(const SecondsPassed: Single;
        var HandleInput: Boolean);
      begin
        inherited;
        if Drag then
          Container.MouseLookUpdate;
      end;

      function TNewFightUi.Motion(const Event: TInputMotion): Boolean;
      var
        Delta: TVector2;
      begin
        Result := inherited;
        if Result then Exit;

        if Drag then
        begin
          Delta := Container.MouseLookDelta(Event, Container.Rect);
          // ...
          // Use Delta to perform any logic you want.
          // It may be zero if mouse was not positioned correctly yet,
          // just make sure that Delta=zero does nothing.
          // You can use Delta / UiScale to adjust to UI scale
          // (user will then have to move mouse by more pixels on a larger screen to achieve the same Delta).
          // ...
        end;
      end;

      #)
    }
    function MouseLookDelta(const Event: TInputMotion; const ControlRect: TFloatRectangle): TVector2; overload;

    function MouseLookDelta(const Event: TInputMotion): TVector2; overload;
      deprecated 'use MouseLookDelta overload with ControlRect parameter';

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
      (TCastleViewport.FullSize is @false, and actual sizes are smaller
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
      {$ifdef FPC}default DefaultTooltipDelay{$endif};
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
      read FUIReferenceWidth write SetUIReferenceWidth {$ifdef FPC}default 0{$endif};
    property UIReferenceHeight: Single
      read FUIReferenceHeight write SetUIReferenceHeight {$ifdef FPC}default 0{$endif};
    { @groupEnd }

    { Scale of the container size (as seen by TCastleUserInterface implementations)
      when UIScaling is usExplicitScale.
      See @link(usExplicitScale) for precise description how this works. }
    property UIExplicitScale: Single
      read FUIExplicitScale write SetUIExplicitScale {$ifdef FPC}default 1.0{$endif};

    { Default font (type, size) to be used by all user interface controls.
      Note that each UI control can customize the used font and/or size
      using properties @link(TCastleUserInterfaceFont.CustomFont),
      @link(TCastleUserInterfaceFont.FontSize).

      If this is @nil, we use the global font @link(UIFont)
      that is always assigned. }
    property DefaultFont: TCastleAbstractFont read FDefaultFont write FDefaultFont;

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

        @item(or use @link(TCastleViewport) with
          @link(TCastleUserInterface.FullSize) = @true and
          @link(TCastleViewport.Transparent) = @false and set
          @link(TCastleViewport.BackgroundColor) as desired,)

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

    { Input (key, touch) to toggle the inspector at any point in the application.
      By default this is possible in debug builds, using key F8,
      or pressing 3 fingers for 1 second (latter is useful on multi-touch devices). }
    property InputInspector: TInputInspector read FInputInspector;

    { @deprecated Use InputInspector.Key now. }
    property InspectorKey: TKey read GetInspectorKey write SetInspectorKey;
      {$ifdef FPC}deprecated 'use InputInspector';{$endif}

    { Current view. Simply assign to this property to change the current view.

      In case multiple views are active (only possible
      if you used the @link(PushView) method), this property returns the @italic(bottom) view
      (use @link(FrontView) to get @italic(top) view).
      Setting this property resets whole view stack.

      @bold(When is it allowed to change the view?)

      While in theory you can change current view stack (assigning @link(View)
      or using @link(PushView) / @link(PopView)) at any moment,
      but remember that stopping the view frees also the view UI.
      So you should not change the current view stack within events/overriden methods
      of classes like TCastleUserInterface, TCastleTransform, TCastleBehavior
      that could be destroyed by the view stop.

      The simpler advise is: @italic(Assign to @link(View) or use @link(PushView) / @link(PopView)
      only from the overridden TCastleView methods.
      Like TMyView.Update or TMyView.Press).

      Note that you cannot change current view stack when another change is in progress.
      That is, you cannot change view from within TMyView.Start/Resume/Pause/Stop.
    }
    property View: TCastleView read GetView write SetView;

    { The view in the front (top-most view on the stack).

      In case you used @link(PushView), this returns the top-most (most recently pushed) view.

      If there is only one (or none) view, e.g. because you never used @link(PushView),
      then this property returns the same thing as @link(View). }
    property FrontView: TCastleView read GetFrontView;

    { Pushing the view adds it at the top of the view stack,
      this makes new view to be displayed on top of previous ones.

      The view known as @link(View) is conceptually at the bottom of view stack, always.
      When it is nil, then pushing new view sets the @link(View) view.
      Otherwise @link(View) view is left as-it-is, new view is added on top. }
    procedure PushView(const NewView: TCastleView);

    { Pop the current top-most view, reversing the @link(PushView) operation. }
    procedure PopView; overload;

    { Pop the current top-most view, reversing the @link(PushView) operation,
      also checking whether the current top-most view is as expected.

      Makes a warning, and does nothing, if the current top-most view
      is different than indicated. This is usually a safer (more chance
      to easily catch bugs) version of PopView than the parameter-less version. }
    procedure PopView(const CheckFrontView: TCastleView); overload;

    { Count of views in the view stack.
      View stack is managed using @link(View) / @link(PushView) / @link(PopView). }
    function ViewStackCount: Integer;

    { Access any view within the view stack.
      Use with indexes between 0 and ViewStackCount - 1.
      View stack is managed using @link(View) / @link(PushView) / @link(PopView). }
    property ViewStack [const Index: Integer]: TCastleView read GetViewStack;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ Changing view during view Start/Stop/Push/Pop is not reliable,
  e.g. doing TCastleContainer.PushView during another TCastleContainer.PushView will not result
  in proper stack. }
procedure ErrorStackChangeDisabled(const Message: String);
begin
  raise EInternalError.Create(Message);
end;

{ TCastleContainer --------------------------------------------------------------- }

constructor TCastleContainer.Create(AOwner: TComponent);
begin
  inherited;

  FInputInspector := TInputInspector.Create(Self);
  FInputInspector.Key := {$ifdef DEBUG} keyF8 {$else} keyNone {$endif};
  FInputInspector.KeyModifiers := [];
  FInputInspector.PressFingers := {$ifdef DEBUG} 3 {$else} 0 {$endif};
  FInputInspector.PressTime := 1;

  FControls := TInternalChildrenControls.Create(nil);
  FControls.Container := Self;
  FTooltipDelay := DefaultTooltipDelay;
  FTooltipDistance := DefaultTooltipDistance;
  FCaptureInput := TFingerIndexCaptureMap.Create;
  FDpi := DefaultDpi;
  FUIScaling := usNone;
  FUIExplicitScale := 1.0;
  FUIScale := 1.0; // default safe value, in case some TCastleUserInterface will look here
  FFocus := TCastleUserInterfaceList.Create(false);
  FNewFocus := TCastleUserInterfaceList.Create(false);
  FFps := TFramesPerSecond.InternalCreate;
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

destructor TCastleContainer.Destroy;
begin
  if FViewStack <> nil then
  begin
    View := nil; // should clean FViewStack
    FreeAndNil(FViewStack);
  end;
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

procedure TCastleContainer.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TCastleContainer.SetForceCaptureInput(const Value: TCastleUserInterface);
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

procedure TCastleContainer.DetachNotification(const C: TCastleUserInterface);
var
  Index: Integer;
  FingerIndex: TFingerIndex;
begin
  if C = FTooltipParent then
  begin
    if FTooltipParent <> nil then
      FTooltipParent.InternalSetTooltipExists(false);
    FTooltipParent := nil;
  end;

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

function TCastleContainer.TryGetFingerOfControl(const C: TCastleUserInterface; out Finger: TFingerIndex): boolean;
var
  FingerControlPair: {$ifdef FPC}TFingerIndexCaptureMap.TDictionaryPair{$else}TPair<TFingerIndex, TCastleUserInterface>{$endif};
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

function TCastleContainer.UseForceCaptureInput: boolean;
begin
  Result :=
    (ForceCaptureInput <> nil) and
    (ForceCaptureInput.Container = Self) { currently added to our Controls tree } and
    ForceCaptureInput.Exists;
end;

function TCastleContainer.PassEvents(const C: TCastleUserInterface;
  const EventPosition: TVector2;
  const CheckEventPosition: Boolean = true): Boolean;
begin
  Result :=
    (not (csDestroying in C.ComponentState)) and
    C.Exists and
    ((not CheckEventPosition) or C.CapturesEventsAtPosition(EventPosition)) and
    C.Visible;
end;

function TCastleContainer.PassEvents(const C: TCastleUserInterface;
  const Event: TInputPressRelease;
  const CheckEventPosition: Boolean = true): Boolean;
begin
  Result := PassEvents(C, Event.Position, CheckEventPosition);
end;

function TCastleContainer.PassEvents(const C: TCastleUserInterface;
  const Event: TInputMotion;
  const CheckEventPosition: Boolean = true): Boolean;
begin
  Result := PassEvents(C, Event.Position, CheckEventPosition);
end;

function TCastleContainer.PassEvents(const C: TCastleUserInterface;
  const CheckMousePosition: Boolean): Boolean;
begin
  Result := PassEvents(C, MousePosition, CheckMousePosition);
end;

procedure TCastleContainer.UpdateFocusAndMouseCursor;
var
  AnythingForcesNoneCursor: boolean;

  { Scan all Controls, recursively.
    Update (add) to FNewFocus, update (set to true) AnythingForcesNoneCursor. }
  procedure CalculateNewFocus;

    procedure RecursiveCalculateNewFocus(const C: TCastleUserInterface);
    var
      I: Integer;
    begin
      if PassEvents(C) then
      begin
        if C.Cursor = mcForceNone then
          AnythingForcesNoneCursor := true;

        FNewFocus.Add(C);

        // Iterate in back-to-front order, because that's the order on Focus list.
        for I := 0 to C.ControlsCount - 1 do
          RecursiveCalculateNewFocus(C.Controls[I]);
      end;
    end;

  var
    I: Integer;
  begin
    { Note that even if one sibling obscures another (they both satisfy
      CapturesEventsAtPosition) both siblings are added to the FNewFocus
      (later to Focus) list.
      That's because they all can receive input event (like Press),
      assuming that all controls return "not handled" (return false from Press).
      This is crucial to make some "invisible" controls (like TCastleNavigation
      or TCastleInspector) work seamlessly, they should not prevent
      other controls from appearing on Focus list.
   }

    // Iterate in back-to-front order, because that's the order on Focus list.
    for I := 0 to Controls.Count - 1 do
      RecursiveCalculateNewFocus(Controls[I]);
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
  var
    FocusIndex: Integer;
  begin
    Result := mcDefault;

    if Focus.Count <> 0 then
    begin
      { Calculate cursor looking at Focus.Last.Cursor,
        unless that's mcDefault then look at previous control on Focus list,
        and so on.
        This is crucial e.g. to allow TCastleViewport to display
        "hand" cursor over TouchSensor, even when TCastleXxxNavigation within
        this viewport has focus.
      }
      FocusIndex := Focus.Count;
      repeat
        Dec(FocusIndex);
        Result := Focus[FocusIndex].Cursor;
      until (Result <> mcDefault) or (FocusIndex = 0);
    end;

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
  { since this is called at the end of TInternalChildrenControls.Notify after
    some control is removed, we're paranoid here about checking csDestroying. }

  { FNewFocus is only used by this method. It is only managed by TCastleUserInterface
    to avoid constructing/destructing it in every
    TCastleContainer.UpdateFocusAndMouseCursor call. }
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
    if not (ApplicationProperties.TouchDevice) then
    begin
      for I := 0 to FNewFocus.Count - 1 do
        if FFocus.IndexOf(FNewFocus[I]) = -1 then
          FNewFocus[I].Focused := true;
      for I := 0 to FFocus.Count - 1 do
        if FNewFocus.IndexOf(FFocus[I]) = -1 then
          FFocus[I].Focused := false;
    end
  end;

  { swap FFocus and FNewFocus, so that FFocus changes to new value,
    and the next UpdateFocusAndMouseCursor has ready FNewFocus value. }
  Tmp := FFocus;
  FFocus := FNewFocus;
  FNewFocus := Tmp;

  InternalCursor := CalculateMouseCursor;
  FFocusAndMouseCursorValid := true;
end;

function TCastleContainer.EventSensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean;

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

function TCastleContainer.EventSensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean;

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

function TCastleContainer.EventJoyAxisMove(const JoyID, Axis: Byte): boolean;

  function RecursiveJoyAxisMove(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        if RecursiveJoyAxisMove(C.Controls[I]) then
          Exit(true);

      {$warnings off} // keeping deprecated working
      if C.JoyAxisMove(JoyID, Axis) then
        Exit(true);
      {$warnings on}
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

function TCastleContainer.EventJoyButtonPress(const JoyID, Button: Byte): boolean;

  function RecursiveJoyButtonPress(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C) then
    begin
      for I := C.ControlsCount - 1 downto 0 do
        if RecursiveJoyButtonPress(C.Controls[I]) then
          Exit(true);

      {$warnings off} // keeping deprecated working
      if C.JoyButtonPress(JoyID, Button) then
        Exit(true);
      {$warnings on}
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

procedure TCastleContainer.ToggleInspector;
begin
  if FInspector = nil then
  begin
    FInspector := TCastleInspector.Create(Self);
    Controls.InsertFront(FInspector);
  end else
  begin
    { Turning off inspector does not merely hide it by Exists:=false,
      to make sure it doesn't consume any resources when not used.
      This is important, as it will be used in wildest scenarios
      (all CGE applications) and it may gather logs when existing. }
    FreeAndNil(FInspector);
  end;
end;

procedure TCastleContainer.EventUpdate;

  procedure UpdateTooltip;
  var
    T: TTimerResult;
    NewTooltipParent: TCastleUserInterface;
    I: Integer;
  begin
    { Update FTooltipParent and LastPositionForTooltip*.
      Idea is that user must move the mouse very slowly to activate tooltip. }

    NewTooltipParent := nil;
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
    end else
    if TimerSeconds(T, LastPositionForTooltipTime) > TooltipDelay then
    begin
      { Any control on Focus can cause a tooltip.
        This is especially useful when an invisible control like TCastleWalkNavigation
        covers other controls. }
      for I := Focus.Count - 1 downto 0 do
        {$warnings off} // using deprecated, it should be internal
        if Focus[I].TooltipExists then
        {$warnings on}
        begin
          NewTooltipParent := Focus[I];
          Break;
        end;
    end;

    if FTooltipParent <> NewTooltipParent then
    begin
      { hide tooltip }
      if FTooltipParent <> nil then
        FTooltipParent.InternalSetTooltipExists(false);

      FTooltipParent := NewTooltipParent;

      if FTooltipParent <> nil then
      begin
        { When changing FTooltipParent, update LastPositionForTooltip. }
        LastPositionForTooltip := MousePosition;
        { also update TooltipPosition }
        FTooltipPosition := MousePosition;
        { show tooltip }
        FTooltipParent.InternalSetTooltipExists(true);
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
        e.g. for sliders/OnScreenMenu to block the navigation in TCastleViewport underneath
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
    Joys: TJoysticks;
    I, J: Integer;
  begin
    Joys := Joysticks;
    if Joys.Initialized then
    begin
      Joys.InternalPoll;

      for I := 0 to Joys.Count - 1 do
      begin
        for J := 0 to Joys[I].Info.Count.Buttons -1 do
        begin
          //Joys.Down(I, J);
          //Joys.Up(I, J);
          if Joys.Press(I, J) then
            EventJoyButtonPress(I, J);
        end;
        for J := 0 to Joys[I].Info.Count.Axes -1 do
        begin
          if Joys.AxisPos(I, J) <> 0 then
            EventJoyAxisMove(I, J);
        end;
      end;
    end;
  end;

var
  I: Integer;
  HandleInput: boolean;
begin
  Fps.InternalUpdateBegin;

  if FInputInspector.IsPressed(Self, Fps.SecondsPassed) then
    ToggleInspector;

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

function TCastleContainer.EventPress(const Event: TInputPressRelease): boolean;

  function RecursivePress(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C, Event) then
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

  if FInputInspector.IsEvent(Event) then
  begin
    ToggleInspector;
    Exit(true);
  end;

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

function TCastleContainer.EventRelease(const Event: TInputPressRelease): boolean;

  function RecursiveRelease(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C, Event) then
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

  if (Capture <> nil) and not Capture.Exists then
  begin
    { No longer capturing, since the Exists returns false now.
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

procedure TCastleContainer.ReleaseCapture(const C: TCastleUserInterface);
var
  FingerIndex: TFingerIndex;
begin
  while TryGetFingerOfControl(C, FingerIndex) do
    FCaptureInput.Remove(FingerIndex);
end;

procedure TCastleContainer.EventOpen(const OpenWindowsCount: Cardinal);

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

procedure TCastleContainer.EventClose(const OpenWindowsCount: Cardinal);

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
    This may be called from Close, which may be called from TCastleWindow destructor,
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

function TCastleContainer.AllowSuspendForInput: boolean;

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
  if FTooltipParent <> nil then
    Exit(false);

  for I := Controls.Count - 1 downto 0 do
    if not RecursiveAllowSuspendForInput(Controls[I]) then
      Exit(false);

  Result := true;
end;

procedure TCastleContainer.EventMotion(const Event: TInputMotion);

  function RecursiveMotion(const C: TCastleUserInterface): boolean;
  var
    I: Integer;
  begin
    if PassEvents(C, Event) then
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

  if (Capture <> nil) and not Capture.Exists then
  begin
    { No longer capturing, since the Exists returns false now.
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

procedure TCastleContainer.ControlsVisibleChange(const Sender: TCastleUserInterface;
  const Changes: TCastleUserInterfaceChanges; const ChangeInitiatedByChildren: boolean);
begin
  { We abort when ChangeInitiatedByChildren = true,
    because this event will be also called with ChangeInitiatedByChildren = false
    for every possible change.
    So this makes a possible optimization for some TCastleContainer descendants:
    no need to call Invalidate so many times. }

  if not ChangeInitiatedByChildren then
  begin
    Invalidate;
    if [chRectangle, chCursor, chExists, chChildren] * Changes <> [] then
      FFocusAndMouseCursorValid := false;
  end;
end;

procedure TCastleContainer.EventBeforeRender;

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

class procedure TCastleContainer.RenderControlPrepare(const ViewportRect: TRectangle);
begin
  if GLFeatures.EnableFixedFunction then
  begin
    { Set OpenGL state that is guaranteed for TCastleUserInterface.Render calls,
      but TCastleUserInterface.Render cannot change it carelessly. }
    {$ifndef OpenGLES}
    RenderContext.FixedFunctionLighting := false;
    glDisable(GL_FOG);
    {$endif}
    GLEnableTexture(etNone);
  end;

  RenderContext.DepthTest := false;

  RenderContext.Viewport := ViewportRect;
  OrthoProjection(FloatRectangle(0, 0, ViewportRect.Width, ViewportRect.Height));

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    { Set OpenGL state that may be changed carelessly, and has some
      guaranteed value, for TCastleUserInterface.Render calls. }
    glLoadIdentity;
    {$warnings off}
    CastleInternalGLUtils.WindowPos := Vector2Integer(0, 0);
    {$warnings on}
  end;
  {$endif}
end;

procedure TCastleContainer.EventRender;
var
  I: Integer;
begin
  if BackgroundEnable then
    RenderContext.Clear([cbColor], BackgroundColor);

  { draw controls in "to" order, back to front }
  for I := 0 to Controls.Count - 1 do
    Controls[I].InternalRecursiveRender(Rect);

  if FTooltipParent <> nil then
  begin
    RenderControlPrepare(Rect);
    {$warnings off} // using deprecated to keep it working, also to enable TCastleUserInterfaceFont to display tooltip
    FTooltipParent.TooltipRender(FTooltipPosition);
    {$warnings on}
  end;

  RenderControlPrepare(Rect);
  if Assigned(OnRender) then OnRender(Self);
end;

procedure TCastleContainer.RenderControl(const Control: TCastleUserInterface;
  const ViewportRect: TRectangle);
var
  NeedsContainerSet, NeedsGLOpen: boolean;
  OldContainer: TCastleContainer;
begin
  NeedsContainerSet := Control.Container <> Self;
  NeedsGLOpen := not Control.GLInitialized;

  { TODO: calling the methods below is not recursive,
    it will not prepare the children correctly. }
  if NeedsContainerSet then
  begin
    OldContainer := Control.Container;
    Control.InternalSetContainer(Self);
  end else
    OldContainer := nil; // only to silence warning
  if NeedsGLOpen then
    Control.GLContextOpen;
  Control.CheckResize;
  Control.BeforeRender;
  Control.InternalRecursiveRender(ViewportRect);

  { TODO: calling the methods below is not recursive,
    it will not unprepare the children correctly. }
  if NeedsContainerSet then
    Control.InternalSetContainer(OldContainer);
  if NeedsGLOpen then
    Control.GLContextClose;
end;

procedure TCastleContainer.EventResize;
begin
  if UIScaling in [usEncloseReferenceSize, usFitReferenceSize] then
    { usXxxReferenceSize adjust current Width/Height to reference,
      so the FUIScale must be adjusted on each resize. }
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

function TCastleContainer.Controls: TInternalChildrenControls;
begin
  Result := FControls;
end;

function TCastleContainer.SettingMousePositionCausesMotion: Boolean;
begin
  Result := true;
end;

procedure TCastleContainer.MouseLookPress;
begin
  FMouseLookIgnoreNextMotion := false;
  FMouseLookWaitingForMiddle := false;
  FMouseLookMotionToSubtract := TVector2.Zero;
end;

procedure TCastleContainer.MouseLookUpdate;
begin
  { Never "capture" the motion of the mouse when MouseLook.

    Without this, using mouse look and then pressing some mouse button that it handled
    (e.g. to shoot), makes the subsequent dragging not work
    -- it is blocked for a short time before releasing left mouse button
    and then it is blocked until you release right mouse button too
    (because of "if (Capture <> nil) and (MousePressed = []) then" condition).

    Testcases:

    - examples/physics/physics_3d_shooter/

      Mouse look is active when buttonRight is held.
      buttonLeft does shoot.

      So without this fix, shooting while mouse looking
      (clicking buttonLeft while holding buttonRight)
      stops mouse look, even though user still holds buttonRight.

    - examples/fps_game/

      As above, though mouse look can also be permanent (after F4).

      It's a problem in any case (whether mouse look is permanent by F4,
      or temporary by holding buttonRight): clicking with buttonLeft stops
      mouse look from working.

    - examples/third_person_navigation/

      We have permanent mouse look by default.

      As both buttonLeft (shoot) and buttonRight (toggle aim) perform some function,
      they both stop mouse look from working.
  }
  FCaptureInput.Clear;
  // this is like ReleaseCapture(...) for all possible arguments
end;

procedure TCastleContainer.MouseLookIgnoreNextMotion;
begin
  FMouseLookIgnoreNextMotion := true;
end;

function TCastleContainer.MouseLookDelta(const Event: TInputMotion): TVector2;
begin
  Result := MouseLookDelta(Event, FloatRectangle(Rect));
end;

function TCastleContainer.MouseLookDelta(const Event: TInputMotion;
  const ControlRect: TFloatRectangle): TVector2;

  { There's a lot of tricky things in implementing this correctly.

    1. Initially we tried to set mouse position to container center very often
       (in every Update, and/or in every MouseLookDelta).

       But then:
       Doing "Result := Event.Position - Event.OldPosition" is bad.
       Instead "Result := Event.Position - Middle" is better:

       That is because setting MousePosition may (but doesn't have to)
       generate another Motion in the container to destination position.

       Subtracting Middle (instead of Container.Position, previous
       known mouse position) solves it. This way

       - The Motion caused by MakeMousePositionedForMouseLook will not do
         anything bad, as MouseChange wil be 0 then.

       - In case MakeMousePositionedForMouseLook does not cause Motion,
         we will not measure the changes as too much. Consider this:

          - player moves mouse to MiddleX-10
          - Motion is generated, I rotate camera by "-10" horizontally
          - Setting MousePosition sets mouse to the Middle,
            but this time no Motion is generated
          - player moved mouse to MiddleX+10. Although mouse was
            positioned on Middle, TCastleWindow thinks that the mouse
            is still positioned on Middle-10, and I will get "+20" move
            for player (while I should get only "+10")

    2. Unfortunately "Result := Event.Position - Middle"
       is also bad, in case there's unspecified delay in applying SetMousePosition.
       Effectively, we would apply some differences in positions many times,
       making the speed of MouseLookDelta faster than normal dragging
       (and the exact factor "how much faster" can depend on mouse sensitivity,
       how much is SetMousePosition delayed (on Windows 10 it can be delayed a lot,
       more than one frame very often)).

       This was easily seen in Unholy fight, and dragging_test example in CGE.

       The new fix is that we allow mouse in a large area in the center,
       and we do SetMousePosition only when it's outside of this large area.
       This allows to easily reject Motion events that report such move,
       as they will have ValidArea(new position)=true but ValidArea(old position)=false.

       In case the Motion event has a summed value of "moving to center by SetMousePosition
       + movement from user", we will ignore too much, but this should
       not have noticeable effect on speed (as it will happen seldom).

    3. Actually, 2 was also invalid.

       The new fix is to just use motion delta always,
       and use explicit SettingMousePositionCausesMotion knowwledge
       to eventually subtract FMouseLookMotionToSubtract.

    4. Another problem is when player switches to another window, moves the mouse,
       than goes Alt+Tab back to our window.
       Next mouse move would cause huge change,
       because it's really *not* from the middle of the screen.

       Solution to this is to use FMouseLookIgnoreNextMotion
       for the next event.

    5. Note that earlier I tried to reposition mouse for mouse look at various
       moments. I tried calling MakeMousePositionedForMouseLook from Press, Motion,
       Update... all these approaches had some drawbacks, and eventually
       they have one big drawback: this could result in Motion that isn't rejected
       below, causing weird first movement.
  }

  { Position is roughly within the center of the window. }
  function ValidArea(const P, Middle: TVector2; const Margin: Single): Boolean;
  begin
    Result := (Abs(P.X - Middle.X) < Margin * ControlRect.Width) and
              (Abs(P.Y - Middle.Y) < Margin * ControlRect.Height);
  end;

var
  Middle: TVector2;
begin
  Result := TVector2.Zero;

  Middle := ControlRect.Center;
  { Round to integers, because if CastleWindow backend doesn't support
    sup-pixel accuracy for SetMousePosition, then it ignores the fractional
    part of SetMousePosition argument.
    Such fractional part would only cause later "error by 0.5 (or other epsilon)"
    when using FMouseLookMotionToSubtract to modify Result. }
  Middle.X := Trunc(Middle.X);
  Middle.Y := Trunc(Middle.Y);

  if FMouseLookIgnoreNextMotion then
  begin
    FMouseLookIgnoreNextMotion := false;
    FMouseLookWaitingForMiddle := false;
    FMouseLookMotionToSubtract := TVector2.Zero;
    Exit;
  end;

  Result := Event.Position - Event.OldPosition;

  { If FMouseLookWaitingForMiddle then we have set MousePosition := Middle,
    and now we wait for MousePosition to be reported near Middle.

    Until this happens, we don't apply FMouseLookMotionToSubtract,
    (as it would cause unexpected shift before the "MousePosition := Middle"
    is actually realized).

    We still set MousePosition := Middle again
    (although we already wait for it to happen),
    in case waiting on "ValidArea(Event.Position, Middle, 1 / 8)" is not reliable.
  }
  if FMouseLookWaitingForMiddle and ValidArea(Event.Position, Middle, 1 / 8) then
    FMouseLookWaitingForMiddle := false;

  if (not FMouseLookWaitingForMiddle) and SettingMousePositionCausesMotion then
  begin
    Result := Result - FMouseLookMotionToSubtract;
    FMouseLookMotionToSubtract := TVector2.Zero;
  end;

  // When mouse gets too far away from Middle, move it back to Middle.
  if not ValidArea(Event.Position, Middle, 1 / 4) then
  begin
    if not FMouseLookWaitingForMiddle then
      FMouseLookMotionToSubtract := FMouseLookMotionToSubtract + Middle - Event.Position;
    MousePosition := Middle;
    FMouseLookWaitingForMiddle := true;
  end;
end;

procedure TCastleContainer.SetUIScaling(const Value: TUIScaling);
begin
  if FUIScaling <> Value then
  begin
    FUIScaling := Value;
    UpdateUIScale;
  end;
end;

procedure TCastleContainer.SetDpi(const Value: Single);
begin
  if FDpi <> Value then
  begin
    FDpi := Value;
    if UIScaling = usDpiScale then
       UpdateUIScale;
  end;
end;

procedure TCastleContainer.SetUIReferenceWidth(const Value: Single);
begin
  if FUIReferenceWidth <> Value then
  begin
    FUIReferenceWidth := Value;
    UpdateUIScale;
  end;
end;

procedure TCastleContainer.SetUIReferenceHeight(const Value: Single);
begin
  if FUIReferenceHeight <> Value then
  begin
    FUIReferenceHeight := Value;
    UpdateUIScale;
  end;
end;

procedure TCastleContainer.SetUIExplicitScale(const Value: Single);
begin
  if FUIExplicitScale <> Value then
  begin
    FUIExplicitScale := Value;
    UpdateUIScale;
  end;
end;

class function TCastleContainer.InternalCalculateUIScale(
  const AUIScaling: TUIScaling;
  const AUIReferenceWidth: Single;
  const AUIReferenceHeight: Single;
  const AUIExplicitScale: Single;
  const ADpi, AWidth, AHeight: Single): Single;
begin
  case AUIScaling of
    usNone         : Result := 1;
    usExplicitScale: Result := AUIExplicitScale;
    usDpiScale     :
      begin
        WritelnLog('UI scaling', 'Using dots (pixels) per inch %f (%f * default %f)',
          [ADpi, ADpi / DefaultDpi, DefaultDpi]);
        Result := ADpi / DefaultDpi;
      end;
    usEncloseReferenceSize, usFitReferenceSize:
      begin
        Result := 1;
        if (AUIReferenceWidth <> 0) and (AWidth > 0) then
        begin
          Result := AWidth / AUIReferenceWidth;
          if (AUIReferenceHeight <> 0) and (AHeight > 0) then
            if AUIScaling = usEncloseReferenceSize then
              MinVar(Result, AHeight / AUIReferenceHeight)
            else
              MaxVar(Result, AHeight / AUIReferenceHeight);
        end else
        if (AUIReferenceHeight <> 0) and (AHeight > 0) then
          Result := AHeight / AUIReferenceHeight;
        // Too talkative when resizing a window (also in castle-editor)
        // WritelnLog('Scaling', 'Automatic scaling to reference sizes %f x %f in effect. Actual window size is %d x %d. Calculated scale is %f, which simulates surface of size %f x %f.',
        //   [AUIReferenceWidth, AUIReferenceHeight,
        //    AWidth, AHeight,
        //    Result, AWidth / Result, AHeight / Result]);
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('UIScaling unknown');
    {$endif}
  end;
end;


procedure TCastleContainer.UpdateUIScale;
begin
  if (not GLInitialized) and
     (UIScaling in [usEncloseReferenceSize, usFitReferenceSize]) then
    // don't adjust FUIScale before our Width/Height are sensible
    FUIScale := 1
  else
    FUIScale := InternalCalculateUIScale(
      UIScaling, UIReferenceWidth, UIReferenceHeight, UIExplicitScale,
      Dpi, Width, Height);

  { Note that we don't cause TCastleUserInterface.UIScaleChanged calls now.
    They are done before BeforeRender, this way culled UI controls
    (when using @link(TCastleUserInterface.Culling))
    are only updated when they are actually visible.
    This can significantly speed things up when Culling makes sense
    (lots of off-screen controls). }
end;

function TCastleContainer.UnscaledWidth: Single;
begin
  Result := Width / FUIScale;
end;

function TCastleContainer.UnscaledHeight: Single;
begin
  Result := Height / FUIScale;
end;

function TCastleContainer.UnscaledRect: TFloatRectangle;
begin
  Result := FloatRectangle(Rect);
  if not Result.IsEmpty then
  begin
    Result.Width  := Result.Width  / FUIScale;
    Result.Height := Result.Height / FUIScale;
  end;
end;

function TCastleContainer.StatusBarHeight: Single;
begin
  Result := ScaledStatusBarHeight / FUIScale;
end;

function TCastleContainer.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  EventBeforeRender;
  EventRender;
  { This is correct if we use double-buffer. }
  Result := SaveScreen_NoFlush(SaveRect, cbBack);
end;

procedure TCastleContainer.SaveScreen(const URL: string);
var
  Image: TRGBImage;
begin
  Image := SaveScreen;
  try
    WritelnLog('SaveScreen', 'Screen saved to ' + URL);
    SaveImage(Image, URL);
  finally FreeAndNil(Image) end;
end;

function TCastleContainer.SaveScreen: TRGBImage;
begin
  Result := SaveScreen(Rect);
end;

function TCastleContainer.SaveScreen(const SaveRect: TFloatRectangle): TRGBImage;
begin
  Result := SaveScreen(SaveRect.Round);
end;

function TCastleContainer.SaveScreenToDefaultFile: String;
var
  Path: String;
begin
  Path := SaveScreenPath;
  if Path <> '' then
  begin
    Result := FilenameToURISafe(FileNameAutoInc(Path + ApplicationName, '_screen_%d.png'));
    SaveScreen(Result);
    WritelnLog('Screen saved to ' + Result);
  end else
    Result := '';
end;

function TCastleContainer.SaveScreenRgba(const SaveRect: TRectangle): TRGBAlphaImage;
begin
  EventBeforeRender;
  EventRender;
  { This is correct if we use double-buffer. }
  Result := SaveScreen_NoFlush(TRGBAlphaImage, SaveRect, cbBack) as TRGBAlphaImage;
end;

function TCastleContainer.SaveScreenRgba: TRGBAlphaImage;
begin
  Result := SaveScreenRgba(Rect);
end;

function TCastleContainer.Rect: TRectangle;
begin
  Result := Rectangle(0, 0, Width, Height);
end;

function TCastleContainer.ScaledStatusBarHeight: Cardinal;
begin
  Result := 0;
end;

procedure TCastleContainer.Invalidate;
begin
  { Default implementation, does nothing, assuming the main program redraws in a loop. }
end;

function TCastleContainer.Focused: boolean;
begin
  { Default implementation, assuming that the context is always focused. }
  Result := true;
end;

procedure TCastleContainer.SetInternalCursor(const Value: TMouseCursor);
begin
  { Default implementation, ignores new cursor value. }
end;

function TCastleContainer.GetMousePosition: TVector2;
begin
  { Default implementation, assuming mouse is glued to the middle of the screen.
    Some methods, like TCastleContainer.UpdateFocusAndMouseCursor,
    use this to calculate "focused" CGE control.
  }
  Result := Vector2(Width / 2, Height / 2);
end;

procedure TCastleContainer.SetMousePosition(const Value: TVector2);
begin
  { Default implementation, ignores new mouse position. }
end;

function TCastleContainer.GetTouches(const Index: Integer): TTouch;
begin
  Assert(Index = 0, 'Base TCastleContainer implementation does not support multi-touch, so you can only access Touches[0]');
  Result.FingerIndex := 0;
  Result.Position := MousePosition;
end;

function TCastleContainer.TouchesCount: Integer;
begin
  Result := 1;
end;

function TCastleContainer.GLInitialized: boolean;
begin
  { Default implementation, assuming the OpenGL context is always initialized. }
  Result := true;
end;

procedure TCastleContainer.LoadSettings(const SettingsUrl: String);
begin
  if not ApplicationProperties.IsGLContextOpen then
    raise Exception.Create('Rendering context not open when calling TUIContainer.LoadSettings. Call LoadSettings later, e.g. in Application.OnInitialize, TCastleWindow.OnOpen, TCastleControl.OnOpen');
  SettingsLoad(Self, SettingsUrl);
end;

function TCastleContainer.GetInspectorKey: TKey;
begin
  Result := FInputInspector.Key;
end;

procedure TCastleContainer.SetInspectorKey(const Value: TKey);
begin
  FInputInspector.Key := Value;
end;

function TCastleContainer.GetView: TCastleView;
begin
  if (FViewStack = nil) or
     (FViewStack.Count = 0) then
    Result := nil
  else
    Result := FViewStack[0];
end;

function TCastleContainer.GetFrontView: TCastleView;
begin
  if (FViewStack = nil) or
     (FViewStack.Count = 0) then
    Result := nil
  else
    Result := FViewStack[FViewStack.Count - 1];
end;

procedure TCastleContainer.SetView(const Value: TCastleView);
begin
  { exit early if there's nothing to do }
  if (ViewStackCount = 0) and (Value = nil) then
    Exit;
  if (ViewStackCount = 1) and (FViewStack[0] = Value) then
    Exit;

  if FDisableStackChange <> 0 then
    ErrorStackChangeDisabled('Cannot change TCastleView.View from inside of TCastleView.Start/Resume/Pause/Stop');

  { Remove and finish topmost view.
    The loop is written to work even when some view Stop method
    changes views. }
  while ViewStackCount <> 0 do
    PopView;
  { deallocate empty FViewStack }
  if Value = nil then
    FreeAndNil(FViewStack);

  PushView(Value);
end;

procedure TCastleContainer.PushView(const NewView: TCastleView);
begin
  if NewView <> nil then
  begin
    if FDisableStackChange <> 0 then
      ErrorStackChangeDisabled('Cannot call TCastleContainer.Push from inside of TCastleView.Start/Resume/Pause/Stop');

    Inc(FDisableStackChange);
    try
      { pause previous top-most view }
      if (FViewStack <> nil) and
         (FViewStack.Count <> 0) then
        FViewStack.Last.Pause;

      { create FViewStack on demand now }
      if FViewStack = nil then
        FViewStack := TCastleViewList.Create(false);
      FViewStack.Add(NewView);
      NewView.InternalStart(Self);
      NewView.Resume;
    finally
      Dec(FDisableStackChange)
    end;
  end;
end;

procedure TCastleContainer.PopView;
var
  SavedFrontView: TCastleView;
begin
  if FDisableStackChange <> 0 then
    ErrorStackChangeDisabled('Cannot call TCastleContainer.Pop from inside of TCastleView.Start/Resume/Pause/Stop');

  Inc(FDisableStackChange);
  try
    SavedFrontView := FrontView;
    SavedFrontView.Pause;
    SavedFrontView.InternalStop;
    if SavedFrontView = FrontView then
      FViewStack.Delete(FViewStack.Count - 1)
    else
      WritelnWarning('TCastleView', 'Topmost view is no longer topmost after its Stop method. Do not change view stack from view Stop methods.');

    if SavedFrontView.FFreeWhenStopped then
      FreeAndNil(SavedFrontView);

    { resume new top-most view }
    if (FViewStack <> nil) and
       (FViewStack.Count <> 0) then
      FViewStack.Last.Resume;
  finally
    Dec(FDisableStackChange)
  end;
end;

procedure TCastleContainer.PopView(const CheckFrontView: TCastleView);
begin
  if (FViewStack = nil) or (FViewStack.Count = 0) then
  begin
    WritelnWarning('TCastleView', 'Cannot pop UI view, that stack is empty');
    Exit;
  end;
  if FViewStack.Last <> CheckFrontView then
  begin
    WritelnWarning('TCastleView', 'Cannot pop UI view, front (top-most) view is expected to be ' + CheckFrontView.ClassName + ', but is ' + FViewStack.Last.ClassName);
    Exit;
  end;

  PopView;
end;

function TCastleContainer.ViewStackCount: Integer;
begin
  if FViewStack = nil then
    Result := 0
  else
    Result := FViewStack.Count;
end;

function TCastleContainer.GetViewStack(const Index: Integer): TCastleView;
begin
  if FViewStack = nil then
    raise EInternalError.CreateFmt('TCastleView.GetViewStack: view stack is empty, cannot get view index %d',
      [Index]);
  Result := FViewStack[Index];
end;

{$endif read_implementation}
