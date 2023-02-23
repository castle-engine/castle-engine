{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Component with OpenGL context suitable for 2D and 3D rendering
  of "Castle Game Engine". }
unit CastleControl;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils,
  StdCtrls, OpenGLContext, Controls, Forms, LCLVersion, LCLType,CustomTimer,
  CastleRectangles, CastleVectors, CastleKeysMouse, CastleUtils, CastleTimeUtils,
  CastleUIControls, CastleRenderOptions,
  CastleImages, CastleGLVersion, CastleLCLUtils,
  CastleGLImages, CastleApplicationProperties;

{ Define this for new Lazarus that has Options (with ocoRenderAtDesignTime)
  (see issue https://bugs.freepascal.org/view.php?id=32026 ). }
{$ifdef PASDOC}
  {$define HAS_RENDER_AT_DESIGN_TIME}
{$else}
  {$if LCL_FULLVERSION >= 1090000}
    {$define HAS_RENDER_AT_DESIGN_TIME}
  {$endif}
{$endif}

const
  DefaultLimitFPS = TCastleApplicationProperties.DefaultLimitFPS
    deprecated 'use TCastleApplicationProperties.DefaultLimitFPS';

type
  TCastleControl = class;

  { TCastleContainer that cooperates with TCastleControl. }
  TCastleControlContainer = class(TCastleContainer)
  strict private
    FDesignUrl: String;
    FDesignLoaded: TCastleUserInterface;
    FDesignLoadedOwner: TComponent;
    procedure SetDesignUrl(const Value: String);
    procedure LoadDesign;
  private
    Parent: TCastleControl;
    procedure UnLoadDesign;
  protected
    function GetMousePosition: TVector2; override;
    procedure SetMousePosition(const Value: TVector2); override;
  public
    constructor Create(AParent: TCastleControl); reintroduce;
    procedure Invalidate; override;
    function GLInitialized: boolean; override;
    function Width: Integer; override;
    function Height: Integer; override;
    procedure SetInternalCursor(const Value: TMouseCursor); override;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; override; overload;
    function Dpi: Single; override;

    procedure EventOpen(const OpenWindowsCount: Cardinal); override;
    procedure EventClose(const OpenWindowsCount: Cardinal); override;
    function EventPress(const Event: TInputPressRelease): boolean; override;
    function EventRelease(const Event: TInputPressRelease): boolean; override;
    procedure EventUpdate; override;
    procedure EventMotion(const Event: TInputMotion); override;
    procedure EventBeforeRender; override;
    procedure EventRender; override;
    procedure EventResize; override;
  public
    { When the DesignUrl is set you can use this method to find
      loaded components. Like this:

      @longCode(#
      MyButton := MyCastleControl.DesignedComponent('MyButton') as TCastleButton;
      #)

      When the name is not found, raises exception (unless Required is @false,
      then it returns @nil).

      @seealso DesignUrl }
    function DesignedComponent(const ComponentName: String;
      const Required: Boolean = true): TComponent;
  published
    { Load and show the design (.castle-user-interface file).
      You can reference the loaded components by name using @link(DesignedComponent).

      If you have more complicated control flow,
      we recommend to leave this property empty, and split your management
      into a number of states (TCastleView) instead.
      In this case, load design using TCastleView.DesignUrl.
      This property makes it however easy to use .castle-user-interface
      in simple cases, when TCastleControl just shows one UI.

      The design loaded here is visible also at design-time,
      when editing the form in Lazarus/Delphi.
      Though we have no way to edit it now in Lazarus/Delphi (you have to use CGE editor
      to edit the design), so it is just a preview in this case.

      See https://castle-engine.io/control_on_form for documentation how to use TCastleControl. }
    property DesignUrl: String read FDesignUrl write SetDesignUrl;
  end;

  { Control to render everything (3D or 2D) with Castle Game Engine.

    See https://castle-engine.io/control_on_form for a documentation
    how to use this.

    You can use this with TCastleView, following https://castle-engine.io/control_on_form instructions.
    In this case, all user interface creation and event handling should
    be inside some state.

    You can also add any user-interface controls to the @link(Controls) property.
    User-interface controls are any @link(TCastleUserInterface) descendants,
    like @link(TCastleImageControl) or @link(TCastleButton) or @link(TCastleViewport).
    Use events like @link(OnPress) to react to events.
    Use event @link(OnUpdate) to do something continuously.

    By default, the control is filled with simple color from
    @link(TCastleContainer.BackgroundColor Container.BackgroundColor).

    This control is an alternative to rendering things using TCastleWindow.
    Note that you cannot use both TCastleControl and TCastleWindow
    within the same application. }
  TCastleControl = class(TCustomOpenGLControl)
  strict private
    FContainer: TCastleControlContainer;
    FMousePosition: TVector2;
    FGLInitialized: boolean;
    FAutoRedisplay: boolean;
    { manually track when we need to be repainted, useful for AggressiveUpdate }
    Invalidated: boolean;
    FOnOpen: TNotifyEvent;
    FOnBeforeRender: TNotifyEvent;
    FOnRender: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnPress: TControlInputPressReleaseEvent;
    FOnRelease: TControlInputPressReleaseEvent;
    FOnMotion: TControlInputMotionEvent;
    FOnUpdate: TNotifyEvent;
    FKeyPressHandler: TLCLKeyPressHandler;
    FAutoFocus: Boolean;
    FPaintTimer:TCustomTimer;

    { Sometimes, releasing shift / alt / ctrl keys will not be reported
      properly to KeyDown / KeyUp. Example: opening a menu
      through Alt+F for "_File" will make keydown for Alt,
      but not keyup for it, and DoExit will not be called,
      so ReleaseAllKeysAndMouse will not be called.

      To counteract this, call this method when Shift state is known,
      to update Pressed when needed. }
    procedure UpdateShiftState(const Shift: TShiftState);

    procedure KeyPressHandlerPress(Sender: TObject;
      const Event: TInputPressRelease);

    procedure SetMousePosition(const Value: TVector2);
    procedure SetAutoRedisplay(const Value: boolean);
    function GetDesignUrl: String;
    procedure SetDesignUrl(const Value: String);

    { Force DoUpdate and Paint (if invalidated) events to happen,
      if sufficient time (based on LimitFPS, that in this case acts like
      "desired FPS") passed.
      This is needed when user "clogs" the GTK / WinAPI / Qt etc. event queue.
      In this case Lazarus (LCL) doesn't automatically fire the idle and repaint
      events.

      The behavior of Lazarus application Idle events is such that they
      are executed only when there are no events left to process.
      This makes sense, and actually follows the docs and the name "idle".

      In contrast, our DoUpdate expects to be run continuously, that is:
      about the same number
      of times per second as the screen Redraw (and if the screen doesn't need to
      be redrawn, our DoUpdate should still run a sensible number of times
      per second --- around the same value as LimitFPS, or (when LimitFPS
      is set to 0, meaning "unused") as many times as possible).
      For our DoUpdate, it should not matter whether your event
      loop has something left to process. We need this,
      since typical games / 3D simulations must try to update animations and
      repaint at a constant rate, even when user is moving around.

      The problem is most obvious when moving the mouse, for example when using
      the mouse look to walk and look around in Walk mode (TCastleWalkNavigation.MouseLook),
      or when dragging with mouse
      in Examine mode. The event loop is then typically busy processing mouse move
      events all the time, so it's never/seldom empty (note: it doesn't mean that
      event loop is clogged, as mouse move events can be potentially accumulated
      at various levels --- LCL, underlying widgetset like GTK, underlying system
      like XWindows etc. I think in practice XWindows does it, but I'm not sure).
      Our program should however still be responsive. Not only the screen should
      be redrawn, regardless if our event loop is empty or not, but also
      our Update event should be continuously called. But if we just use LCL Idle/Redraw
      behavior (that descends from other widgetsets) then you may find that:
      - during mouse look things "stutter" --- no Idle, not even Redraw,
        happens regularly.
      - during mouse drag Redraw may be regular, but still Idle are not called
        (so e.g. animations do not move, instead they suddenly jump a couple
        of seconds
        forward when you stop dragging after a couple of seconds).

      Note that TCastleWindow (with backends other than LCL) do not have this
      problem. Maybe we process events faster, so that we don't get clogged
      during MouseLook?

      We can't fix it by hacking Application methods,
      especially as LCL Application.ProcessMessage may handle a "batch"
      of events (for example, may be ~ 100 GTK messages, see
      TGtkWidgetSet.AppProcessMessages in lazarus/trunk/lcl/interfaces/gtk/gtkwidgetset.inc).
      So instead we hack it from the inside: from time to time
      (more precisely, LimitFPS times per second),
      when receving an often occuring event (right now: just MouseMove),
      we'll call the DoUpdate, and (if pending Invalidate call) Paint methods.

      In theory, we could call this on every event (key down, mouse down etc.).
      But in practice:
      - Doing this from KeyDown would make redraw when moving by only holding
        down some keys stutter a little (screen seems like not refreshed fast
        enough). Reason for this stutter is not known,
        it also stutters in case of mouse move, but we have no choice in this case:
        either update with stuttering, or not update (continuously) at all.
        TCastleWindow doesn't have this problem, mouse look is smooth there.
      - It's also not needed from events other than mouse move.

      In theory, for LimitFPS = 0, we should just do this every time.
      But this would overload the system
      (you would see smooth animation and rendering, but there will be latency
      with respect to handling input, e.g. mouse move will be processed with
      a small delay). So we use MaxDesiredFPS to cap it. }
    procedure AggressiveUpdate;
  private
    class function GetMainContainer: TCastleContainer;
    procedure OnTimerPaint(Sender : TObject);
  protected
    procedure DestroyHandle; override;
    procedure DoExit; override;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    procedure DoUpdate; virtual;

    property GLInitialized: boolean read FGLInitialized;
  public
    class var
      { Central control.

        This is only important now if you use deprecated way of setting TCastleView,
        using class properties/methods TUIState.Current, TUIState.Push.
        If instead you use new way of setting TCastleView,
        using container properties/methods TCastleContainer.Current, TCastleContainer.Push,
        then this value isn't useful.

        This means that in new applications, you probably have no need to set this value. }
      MainControl: TCastleControl deprecated 'this should no longer be useful, if you change views using MyControl.Container.View := .. or MyControl.Container.PushView(...)';

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { List of user-interface controls currently active.
      You can add your TCastleUserInterface instances
      (like TCastleViewport, TCastleButton and much more) to this list.
      We will pass events to these controls, draw them etc.
      See @link(TCastleContainer.Controls) for details. }
    function Controls: TInternalChildrenControls;

    function MakeCurrent(SaveOldToStack: boolean = false): boolean; override;
    procedure Invalidate; override;
    procedure Paint; override;

    { Keys currently pressed. }
    function Pressed: TKeysPressed;
    { Mouse buttons currently pressed.
      See @link(TCastleContainer.MousePressed) for details. }
    function MousePressed: TCastleMouseButtons;
    procedure ReleaseAllKeysAndMouse;

    { Current mouse position.
      See @link(TTouch.Position) for a documentation how this is expressed. }
    property MousePosition: TVector2 read FMousePosition write SetMousePosition;

    { Application speed. }
    function Fps: TFramesPerSecond;

    { Capture the current control contents to an image.
      @groupBegin }
    procedure SaveScreen(const URL: string); overload;
    function SaveScreen: TRGBImage; overload;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload;
    { @groupEnd }

    { Color buffer where we draw, and from which it makes sense to grab pixels.
      Use only if you save the screen using low-level SaveScreen_NoFlush function.
      Usually, you should save the screen using the simpler @link(SaveScreen) method,
      and then the @name is not useful. }
    function SaveScreenBuffer: TColorBuffer;

    { Rectangle representing the inside of this container.
      Always (Left,Bottom) are zero, and (Width,Height) correspond to container
      sizes. }
    function Rect: TRectangle;

    function DesignedComponent(const ComponentName: String): TComponent;
      deprecated 'use Container.DesignedComponent';

    { Be cafeful about comments in the published section.
      They are picked up and shown automatically by Lazarus Object Inspector,
      and it has it's own logic, much much dumber than what PasDoc sees.
      There seems no way to hide comment there.

      We publish most, but not all, stuff from inherited TCustomOpenGLControl.

      Exceptions:
      - Don't publish these, as not every widgetset has them:
        property RedBits;
        property GreenBits;
        property BlueBits;

      - Don't publish these, as we have our own events for this:
        property OnResize;
        property OnClick;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnMouseWheel;
        property OnMouseWheelDown;
        property OnMouseWheelUp;
        property OnPaint;

      - Don't use, engine handles this completely:
        property OnMakeCurrent;
        property AutoResizeViewport;
    }
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property OpenGLMajorVersion;
    property OpenGLMinorVersion;
    property MultiSampling;
    property AlphaBits;
    property DepthBits;
    property StencilBits default DefaultStencilBits;
    property AUXBuffers;
    {$ifdef HAS_RENDER_AT_DESIGN_TIME}
    property Options;
    {$endif}
    property OnChangeBounds;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property TabOrder;
    property TabStop default true;

    { Automatically make this control focused (receiving key input)
      when user clicks on it.

      If this is @true, consider showing it in some way, e.g. draw something special
      in OnRender when this control is focused. You can check "Focused" property
      ( https://lazarus-ccr.sourceforge.io/docs/lcl/controls/twincontrol.focused.html )
      or FormXxx.ActiveControl or register OnEnter / OnExit LCL events. }
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default false;

    { Access Castle Game Engine container properties and events,
      not specific for Lazarus LCL. }
    property Container: TCastleControlContainer read FContainer;

    { Event called when the OpenGL context is created.

      You can initialize things that require OpenGL context now.
      Often you do not need to use this callback (engine components will
      automatically create/release OpenGL resource when necessary).
      You usually will also want to implement OnClose callback that
      should release stuff you create here.

      Often, instead of using this callback, it's cleaner to derive new classes
      from TCastleUserInterface class or it's descendants,
      and override their GLContextOpen / GLContextClose methods to react to
      context being open/closed. Using such TCastleUserInterface classes
      is usually easier, as you add/remove them from controls whenever
      you want (e.g. you add them in ApplicationInitialize),
      and underneath they create/release/create again the OpenGL resources
      when necessary.

      Note that we automatically initialize necessary Castle Game Engine resources
      when context is created (@link(GLVersion), @link(GLFeatures) and more).
    }
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;

    { Event called when the context is closed, right before the OpenGL context
      is destroyed. This is your last chance to release OpenGL resources,
      like textures, shaders, display lists etc. This is a counterpart
      to OnOpen event. }
    property OnClose: TNotifyEvent read FOnClose write FOnClose;

    { Event always called right before OnRender.
      These two events, OnBeforeRender and OnRender,
      will be always called sequentially as a pair.

      The only difference between these two events is that
      time spent in OnBeforeRender
      is NOT counted as "frame time"
      by Fps.OnlyRenderFps. This is useful when you have something that needs
      to be done from time to time right before OnRender and that is very
      time-consuming. It such cases it is not desirable to put such time-consuming
      task inside OnRender because this would cause a sudden big change in
      Fps.OnlyRenderFps value. So you can avoid this by putting
      this in OnBeforeRender. }
    property OnBeforeRender: TNotifyEvent read FOnBeforeRender write FOnBeforeRender;

    { Render window contents here.

      Called when window contents must be redrawn,
      e.g. after creating a window, after resizing a window, after uncovering
      the window etc. You can also request yourself a redraw of the window
      by the Invalidate method, which will cause this event to be called
      at nearest good time.

      Note that calling Invalidate while in EventRender (OnRender) is not ignored.
      It instructs to call EventRender (OnRender) again, as soon as possible.

      When you have some controls on the @link(Controls) list,
      the OnRender event is done @bold(last).
      So here you can draw on top of the existing UI controls.
      To draw something underneath the existing controls, create a new TCastleUserInterface
      and override it's @link(TCastleUserInterface.Render) and insert it to the controls
      using @code(Controls.InsertBack(MyBackgroundControl);). }
    property OnRender: TNotifyEvent read FOnRender write FOnRender;

    { Called when the control size (@code(Width), @code(Height)) changes.
      It's also guaranteed to be called right after the OnOpen event. }
    property OnResize: TNotifyEvent read FOnResize write FOnResize;

    { Called when user presses a key or mouse button or moves mouse wheel. }
    property OnPress: TControlInputPressReleaseEvent read FOnPress write FOnPress;

    { Called when user releases a pressed key or mouse button.

      It's called right after @code(Pressed[Key]) changed from true to false.

      The TInputPressRelease structure, passed as a parameter to this event,
      contains the exact information what was released.

      Note that reporting characters for "key release" messages is not
      perfect, as various key combinations (sometimes more than one?) may lead
      to generating given character. We have some intelligent algorithm
      for this, used to make Characters table and to detect
      this C for OnRelease callback. The idea is that a character is released
      when the key that initially caused the press of this character is
      also released.

      This solves in a determined way problems like
      "what happens if I press Shift, then X,
      then release Shift, then release X". (will "X" be correctly
      released as pressed and then released? yes.
      will small "x" be reported as released at the end? no, as it was never
      pressed.) }
    property OnRelease: TControlInputPressReleaseEvent read FOnRelease write FOnRelease;

    { Mouse or a finger on touch device moved.

      For a mouse, remember you always have the currently
      pressed mouse buttons in MousePressed. When this is called,
      the MousePosition property records the @italic(previous)
      mouse position, while callback parameter NewMousePosition gives
      the @italic(new) mouse position. }
    property OnMotion: TControlInputMotionEvent read FOnMotion write FOnMotion;

    { Continuously occuring event.
      This event is called roughly as regularly as redraw,
      and you should use this to update your game state.

      Note that this is different than LCL "idle" event,
      as it's guaranteed to be run continuously, even when your application
      is clogged with events (like when using TCastleWalkNavigation.MouseLook).

      Note: As we need to continuously call the "update" event (to update animations
      and more), we listen on the Lazarus Application "idle" event,
      and tell it that we're never "done" with our work.
      We do this only when at least one instance of TCastleControl
      is created, and never at design-time.
      This means that your own "idle" events (registered through LCL
      TApplicationProperties.OnIdle or Application.AddOnIdleHandler)
      may be never executed, because really the application is never idle.

      If you want to reliably do some continuous work, use Castle Game Engine
      features to do it. There are various alternative ways:

      @unorderedList(
        @item(Register an event on @link(OnUpdate) of this component,)
        @item(Add custom @link(TCastleUserInterface) instance to the @link(Controls) list
          with overridden @link(TCastleUserInterface.Update) method,)
        @item(Register an event on @link(TCastleApplicationProperties.OnUpdate
          ApplicationProperties.OnUpdate) from the @link(CastleApplicationProperties)
          unit.)
      )
    }
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;

    { Should we automatically redraw the window all the time,
      without the need for an @link(Invalidate) call.
      If @true (the default), OnRender will called constantly.

      If your game may have a still screen (nothing animates),
      then this approach is a little unoptimal, as we use CPU and GPU
      for drawing, when it's not needed. In such case, you can set this
      property to @false, and make sure that you call
      @link(Invalidate) always when you need to redraw the screen.
      Note that the engine components always call @link(Invalidate) when
      necessary, so usually you should only call it yourself if you provide
      a custom @link(OnRender) implementation. }
    property AutoRedisplay: boolean read FAutoRedisplay write SetAutoRedisplay
      default true;

    { Load and show the design (.castle-user-interface file).
      You can reference the loaded components by name using @link(DesignedComponent).

      If you have more complicated control flow,
      we recommend to leave this property empty, and split your management
      into a number of states (TCastleView) instead.
      In this case, load design using TCastleView.DesignUrl.
      This property makes it however easy to use .castle-user-interface
      in simple cases, when TCastleControl just shows one UI.

      The design loaded here is visible also at design-time,
      when editing the form in Lazarus/Delphi.
      Though we have to way to edit it now in Lazarus/Delphi (you have to use CGE editor
      to edit the design), so it is just a preview in this case. }
    property DesignUrl: String read GetDesignUrl write SetDesignUrl stored false;
      deprecated 'use Container.DesignUrl';
  end;

  TCastleControlCustom = TCastleControl deprecated 'use TCastleControl';

  { Note: we need this deprecated class to be a separate class,
    not just an alias for TCastleControl,
    to be able to register it using RegisterNoIcon,
    to support in old projects. }
  TCastleControlBase = class (TCastleControl) end deprecated 'use TCastleControl';

procedure Register;

function GetLimitFPS: Single;
  deprecated 'use ApplicationProperties.LimitFPS';
procedure SetLimitFPS(const Value: Single);
  deprecated 'use ApplicationProperties.LimitFPS';
property LimitFPS: Single read GetLimitFPS write SetLimitFPS;

implementation

uses Math, Contnrs, LazUTF8, Clipbrd,
  CastleControls, CastleGLUtils, CastleStringUtils, CastleLog, CastleRenderContext,
  CastleURIUtils, CastleComponentSerialize, CastleInternalLclDesign;

// TODO: We never call Fps._Sleeping, so Fps.WasSleeping will be always false.
// This may result in confusing Fps.ToString in case AutoRedisplay was false.

// TODO: Try an alternative OnUpdate implementation using TTimer with Interval=1.

{ globals -------------------------------------------------------------------- }

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleControl
  ]);
  // register deprecated components in a way that they can be serialized, but are not visible on LCL palette
  RegisterNoIcon([
    {$warnings off}
    TCastleControlBase
    {$warnings on}
  ]);
end;

var
  { All TCastleControl instances created. We use this to share OpenGL contexts,
    as all OpenGL contexts in our engine must share OpenGL resources
    (our OnGLContextOpen and such callbacks depend on it,
    and it makes implementation much easier). }
  ControlsList: TComponentList;

  ControlsOpen: Cardinal;

{ Limit FPS ------------------------------------------------------------------ }

var
  LastLimitFPSTime: TTimerResult;

procedure DoLimitFPS;
var
  NowTime: TTimerResult;
  TimeRemainingFloat: Single;
begin
  if ApplicationProperties.LimitFPS > 0 then
  begin
    NowTime := Timer;

    { When this is run for the 1st time, LastLimitFPSTime is zero,
      so NowTime - LastLimitFPSTime is huge, so we will not do any Sleep
      and only update LastLimitFPSTime.

      For the same reason, it is not a problem if you do not call DoLimitFPS
      often enough (for example, you do a couple of ProcessMessage calls
      without DoLimitFPS for some reason), or when user temporarily sets
      LimitFPS to zero and then back to 100.0.
      In every case, NowTime - LastLimitFPSTime will be large, and no sleep
      will happen. IOW, in the worst case --- we will not limit FPS,
      but we will *never* slow down the program when it's not really necessary. }

    TimeRemainingFloat :=
      { how long I should wait between _LimitFPS calls }
      1 / ApplicationProperties.LimitFPS -
      { how long I actually waited between _LimitFPS calls }
      TimerSeconds(NowTime, LastLimitFPSTime);
    { Don't do Sleep with too small values.
      It's better to have larger FPS values than limit,
      than to have them too small. }
    if TimeRemainingFloat > 0.001 then
    begin
      Sleep(Round(1000 * TimeRemainingFloat));
      LastLimitFPSTime := Timer;
    end else
      LastLimitFPSTime := NowTime;
  end;
end;

function GetLimitFPS: Single;
begin
  Result := ApplicationProperties.LimitFPS;
end;

procedure SetLimitFPS(const Value: Single);
begin
  ApplicationProperties.LimitFPS := Value;
end;

{ TCastleApplicationIdle -------------------------------------------------- }

type
  TCastleApplicationIdle = class
    class procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  end;

class procedure TCastleApplicationIdle.ApplicationIdle(Sender: TObject; var Done: Boolean);
var
  I: Integer;
  C: TCastleControl;
begin
  { This should never be registered in design mode, to not conflict
    (by DoLimitFPS, or Done setting) with using Lazarus IDE. }
  Assert(not (csDesigning in Application.ComponentState));

  { Call DoUpdate for all TCastleControl instances. }
  for I := 0 to ControlsList.Count - 1 do
  begin
    C := ControlsList[I] as TCastleControl;
    C.DoUpdate;
  end;
  ApplicationProperties._Update;

  DoLimitFPS;

  { With Done := true (this is actually default Done value here),
    ApplicationIdle events are not occuring as often
    as we need. Test e.g. GTK2 with clicking on spheres on
    demo_models/sensors_pointing_device/touch_sensor_tests.x3dv .
    That's because Done := true allows for WidgetSet.AppWaitMessage
    inside lcl/include/application.inc .
    We don't want that, we want continuous DoUpdate events.

    So we have to use Done := false.

    Unfortunately, Done := false prevents other idle actions
    (other TApplicationProperties.OnIdle) from working.
    See TApplication.Idle and TApplication.NotifyIdleHandler implementation
    in lcl/include/application.inc .
    To at least allow all TCastleControl work, we use a central
    ApplicationIdle callback (we don't use separate TApplicationProperties
    for each TCastleControl; in fact, we don't need TApplicationProperties
    at all). }

  Done := false;
end;

var
  ApplicationIdleSet: boolean;

{ TCastleControlContainer ---------------------------------------------------- }

procedure TCastleControlContainer.LoadDesign;

{ Note: implementation of LoadDesign, UnLoadDesign and friends follow similar
  methods in TCastleView. Here they are much simplified, as we have no concept
  of "started" / "stopped", so no DesignPreload too. }

var
  OldCastleApplicationMode: TCastleApplicationMode;
begin
  if DesignUrl <> '' then
  begin
    { Make sure InternalCastleApplicationMode is correct, to
      - not e.g. do physics in Lazarus/Delphi form designer.
      - not show design-time stuff in DesignUrl loaded in CGE editor "help->system information".

      Note that we restore later InternalCastleApplicationMode.
      This way we avoid changing InternalCastleApplicationMode for future loads,
      when TCastleControl is used inside castle-editor.
      Testcase:
        in CGE editor:
        - open tools/castle-editor project
        - double click on demo design in data/demo_animation/
        - open help->system information (this uses TCastleControl too, with DesignUrl assigned)
        - close help->system information
        - close design
        - reopen design
    }
    OldCastleApplicationMode := InternalCastleApplicationMode;
    try
      if csDesigning in ComponentState then
        InternalCastleApplicationMode := appDesign
      else
        InternalCastleApplicationMode := appRunning;
      FixApplicationDataInIDE; // in case DesignUrl uses castle-data: protocol, which is most often the case

      FDesignLoadedOwner := TComponent.Create(nil);
      try
        FDesignLoaded := UserInterfaceLoad(DesignUrl, FDesignLoadedOwner);
        {$ifdef HAS_RENDER_AT_DESIGN_TIME}
        Parent.Options := Parent.Options + [ocoRenderAtDesignTime];
        {$endif}
      except
        { If loading design file failed, and we're inside form designer,
          merely report a warning. This allows deserializing LFMs with broken URLs. }
        on E: Exception do
        begin
          if CastleDesignMode then // looks at InternalCastleApplicationMode
          begin
            WritelnWarning('TCastleControl', 'Failed to load design "%s": %s', [
              URIDisplay(DesignUrl),
              ExceptMessage(E)
            ]);
            Exit;
          end else
            raise;
        end;
      end;
      Controls.InsertFront(FDesignLoaded);
    finally
      InternalCastleApplicationMode := OldCastleApplicationMode;
    end;
  end;
end;

procedure TCastleControlContainer.UnLoadDesign;
begin
  FreeAndNil(FDesignLoadedOwner);
  FDesignLoaded := nil; // freeing FDesignLoadedOwner must have freed this too
end;

procedure TCastleControlContainer.SetDesignUrl(const Value: String);
begin
  if FDesignUrl <> Value then
  begin
    UnLoadDesign;
    FDesignUrl := Value;
    LoadDesign;
  end;
end;

function TCastleControlContainer.DesignedComponent(const ComponentName: String;
  const Required: Boolean = true): TComponent;
begin
  if FDesignLoaded <> nil then
    Result := FDesignLoadedOwner.FindComponent(ComponentName)
  else
    Result := nil;

  if Required and (Result = nil) then
    raise EComponentNotFound.CreateFmt('Cannot find component named "%s" in design "%s"', [
      ComponentName,
      URIDisplay(DesignUrl)
    ]);
end;

constructor TCastleControlContainer.Create(AParent: TCastleControl);
begin
  inherited Create(AParent); // AParent must be a component Owner to show published properties of container in LFM
  Parent := AParent;
end;

procedure TCastleControlContainer.Invalidate;
begin
  Parent.Invalidate;
end;

function TCastleControlContainer.GLInitialized: boolean;
begin
  Result := Parent.GLInitialized;
end;

function TCastleControlContainer.Width: Integer;
begin
  Result := Parent.Width;
end;

function TCastleControlContainer.Height: Integer;
begin
  Result := Parent.Height;
end;

function TCastleControlContainer.GetMousePosition: TVector2;
begin
  Result := Parent.MousePosition;
end;

procedure TCastleControlContainer.SetMousePosition(const Value: TVector2);
begin
  Parent.MousePosition := Value;
end;

procedure TCastleControlContainer.SetInternalCursor(const Value: TMouseCursor);
var
  NewCursor: TCursor;
begin
  NewCursor := CursorCastleToLCL(Value);

  { Check explicitly "Cursor <> NewCursor", to avoid changing LCL property Cursor
    too often. The SetInternalCursor may be called very often (in each mouse move).
    (It is probably already optimized in LCL,
    and in window manager too, but it's safer to not depend on it). }
  if Parent.Cursor <> NewCursor then
    Parent.Cursor := NewCursor;
end;

function TCastleControlContainer.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  if Parent.MakeCurrent then
  begin
    EventBeforeRender;
    EventRender;
  end;
  Result := SaveScreen_NoFlush(Rect, Parent.SaveScreenBuffer);
end;

function TCastleControlContainer.Dpi: Single;
begin
  Result := Screen.PixelsPerInch;
end;

procedure TCastleControlContainer.EventOpen(const OpenWindowsCount: Cardinal);
begin
  inherited;
  if Assigned(Parent.OnOpen) then
    Parent.OnOpen(Parent);
end;

procedure TCastleControlContainer.EventClose(const OpenWindowsCount: Cardinal);
begin
  if Assigned(Parent.OnClose) then
    Parent.OnClose(Parent);
  inherited;
end;

function TCastleControlContainer.EventPress(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if (not Result) and Assigned(Parent.OnPress) then
  begin
    Parent.OnPress(Parent, Event);
    Result := true;
  end;
end;

function TCastleControlContainer.EventRelease(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if (not Result) and Assigned(Parent.OnRelease) then
  begin
    Parent.OnRelease(Parent, Event);
    Result := true;
  end;
end;

procedure TCastleControlContainer.EventUpdate;
begin
  inherited;
  if Assigned(Parent.OnUpdate) then
    Parent.OnUpdate(Parent);
end;

procedure TCastleControlContainer.EventMotion(const Event: TInputMotion);
begin
  inherited;
  if Assigned(Parent.OnMotion) then
    Parent.OnMotion(Parent, Event);
end;

procedure TCastleControlContainer.EventBeforeRender;
begin
  inherited;
  if Assigned(Parent.OnBeforeRender) then
    Parent.OnBeforeRender(Parent);
end;

procedure TCastleControlContainer.EventRender;
begin
  inherited;
  if Assigned(Parent.OnRender) then
    Parent.OnRender(Parent);
end;

procedure TCastleControlContainer.EventResize;
begin
  inherited;
  if Assigned(Parent.OnResize) then
    Parent.OnResize(Parent);
end;

{ TCastleControl -------------------------------------------------- }

constructor TCastleControl.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := true;
  FAutoRedisplay := true;
  FKeyPressHandler := TLCLKeyPressHandler.Create;
  FKeyPressHandler.OnPress := @KeyPressHandlerPress;
  StencilBits := DefaultStencilBits;

  {$ifdef DARWIN}
  { On macOS, request "core" OpenGL context, otherwise we'll never get newer OpenGL than 2.1 }
  OpenGLMajorVersion := 3;
  OpenGLMinorVersion := 2;
  { Just like in castlewindow_cocoa.inc, force modern at this point,
    to avoid TGLFeatures using GLExt functions that use deprecated
    OpenGL glGetString(GL_EXTENSIONS). }
  TGLFeatures.RequestCapabilities := rcForceModern;
  {$endif}

  FContainer := TCastleControlContainer.Create(Self);
  { SetSubComponent and Name setting (must be unique only within TCastleControl,
    so no troubles) are necessary to store it in LFM and display in object inspector
    nicely. }
  FContainer.SetSubComponent(true);
  FContainer.Name := 'Container';

  // TODO: what if ControlsList[0] was created but it not active?
  // Does this maybe explain crash with docked editor?
  // TODO: Or is it because we never remove from ControlsList?
  if ControlsList.Count <> 0 then
    SharedControl := ControlsList[0] as TCastleControl;
  ControlsList.Add(Self);

  FPaintTimer := TCustomTimer.Create(Self);
  FPaintTimer.Interval := 1000 div 60;
  FPaintTimer.OnTimer := {$Ifdef fpc}@{$endif}OnTimerPaint;
  FPaintTimer.Enabled := True;

  Invalidated := false;

  { Note that we can depend that csDesigning is already set in constructor.
    This is good, otherwise we would use ApplicationIdle also in Lazarus IDE. }
  if (not (csDesigning in ComponentState)) and (not ApplicationIdleSet) then
  begin
    ApplicationIdleSet := true;
    Application.AddOnIdleHandler(@(TCastleApplicationIdle(nil).ApplicationIdle));
  end;
end;

destructor TCastleControl.Destroy;
begin
  Container.UnLoadDesign;

  if ApplicationIdleSet and
     (ControlsList <> nil) and
     { If ControlsList.Count will become 0 after this destructor,
       then unregisted our idle callback.
       If everyhting went Ok, ControlsList.Count = 1 should always imply
       that we're the only control there. But check "ControlsList[0] = Self"
       in case we're in destructor because there was an exception
       in the constructor. }
     (ControlsList.Count = 1) and
     (ControlsList[0] = Self) then
  begin
    ApplicationIdleSet := false;
    Application.RemoveOnIdleHandler(@(TCastleApplicationIdle(nil).ApplicationIdle));
  end;

  FreeAndNil(FPaintTimer);
  FreeAndNil(FContainer);
  FreeAndNil(FKeyPressHandler);
  inherited;
end;

procedure TCastleControl.SetAutoRedisplay(const Value: boolean);
begin
  FAutoRedisplay := value;
  if Value then Invalidate;
end;

{ Initial idea was to do

procedure TCastleControl.CreateHandle;
begin
  Writeln('TCastleControl.CreateHandle ', GLInitialized,
    ' ', OnGLContextOpen <> nil);
  inherited CreateHandle;
  if not GLInitialized then
  begin
    GLInitialized := true;
    Container.EventOpen;
  end;
  Writeln('TCastleControl.CreateHandle end');
end;

Reasoning: looking at implementation of OpenGLContext,
actual creating and destroying of OpenGL contexts
(i.e. calls to LOpenGLCreateContext and LOpenGLDestroyContextInfo)
is done within Create/DestroyHandle.

Why this was wrong ? Because under GTK LOpenGLCreateContext
only creates gtk_gl_area --- it doesn't *realize* it yet !
Which means that actually LOpenGLCreateContext doesn't create
OpenGL context. Looking at implementation of GLGtkGlxContext
we see that only during MakeCurrent the widget is guaranteed
to be realized. }

function TCastleControl.MakeCurrent(SaveOldToStack: boolean): boolean;
begin
  { This call makes no sense when OpenGL context is no longer available,
    which means Handle = 0.
    Inherited would make error - LOpenGLMakeCurrent in LCL would
    make "RaiseGDBException('LOpenGLSwapBuffers Handle=0');".
    For some reason, it may be reported as EDivByZero, "Division by zero".

    Better to just exit with false. }
  if Handle = 0 then
    Exit(false);

  Result := inherited MakeCurrent(SaveOldToStack);

  RenderContext := Container.Context;

  if not GLInitialized then
  begin
    FGLInitialized := true;
    GLInformationInitialize;
    // _GLContextEarlyOpen is not really necessary here now, but we call it for consistency
    ApplicationProperties._GLContextEarlyOpen;
    Inc(ControlsOpen);
    Container.EventOpen(ControlsOpen);
    Resize; // will call Container.EventResize
    Invalidate;
  end;
end;

procedure TCastleControl.DestroyHandle;
begin
  if GLInitialized then
  begin
    Container.EventClose(ControlsOpen);
    Dec(ControlsOpen);
    FGLInitialized := false;
  end;
  inherited DestroyHandle;
end;

procedure TCastleControl.Resize;
begin
  inherited;

  { Call MakeCurrent here, to make sure CastleUIControls always get
    Resize with good GL context. }
  if GLInitialized and MakeCurrent then
    Container.EventResize;
end;

procedure TCastleControl.Invalidate;
begin
  Invalidated := true;
  { invalidate is executed by PaintTimer
  //inherited;
  }
end;

procedure TCastleControl.ReleaseAllKeysAndMouse;

  { This does a subset of MouseUp implementation, only caring about updating CGE state now. }
  procedure CastleMouseUp(const MyButton: TCastleMouseButton);
  begin
    Container.MousePressed := Container.MousePressed - [MyButton];
    Container.EventRelease(InputMouseButton(MousePosition, MyButton, 0));
  end;

  { This does a subset of KeyUp implementation, only caring about updating CGE state now. }
  procedure CastleKeyUp(const MyKey: TKey);
  var
    MyKeyString: String;
  begin
    { Do this before anything else, in particular before even Pressed.KeyUp below.
      This may call OnPress (which sets Pressed to true). }
    FKeyPressHandler.Flush;

    Pressed.KeyUp(MyKey, MyKeyString);

    if (MyKey <> keyNone) or (MyKeyString <> '') then
      Container.EventRelease(InputKey(MousePosition, MyKey, MyKeyString));
  end;

var
  Key: TKey;
  MouseButton: TCastleMouseButton;
begin
  { This should also take care of releasing Characters. }
  for Key := Low(Key) to High(Key) do
    if Pressed[Key] then
      CastleKeyUp(Key);

  for MouseButton := Low(MouseButton) to High(MouseButton) do
    if MouseButton in MousePressed then
      CastleMouseUp(MouseButton);

  Container.MouseLookIgnoreNextMotion;
end;

procedure TCastleControl.UpdateShiftState(const Shift: TShiftState);
begin
  Pressed.Keys[keyShift] := ssShift in Shift;
  Pressed.Keys[keyAlt  ] := ssAlt   in Shift;
  Pressed.Keys[keyCtrl ] := ssCtrl  in Shift;
end;

procedure TCastleControl.KeyPressHandlerPress(Sender: TObject;
  const Event: TInputPressRelease);
var
  NewEvent: TInputPressRelease;
begin
  // Key or KeyString non-empty, our TLCLKeyPressHandler already checks it
  Assert((Event.Key <> keyNone) or (Event.KeyString <> ''));

  NewEvent := Event;
  NewEvent.Position := MousePosition;
  NewEvent.KeyRepeated :=
    // Key already pressed
    ((NewEvent.Key = keyNone) or Pressed.Keys[NewEvent.Key]) and
    // KeyString already pressed
    ((NewEvent.KeyString = '') or Pressed.Strings[NewEvent.KeyString]);

  { Note that Event has invalid position (TLCLKeyPressHandler always sends
    zero). So all the following code has to use NewEvent instead. }

  Pressed.KeyDown(NewEvent.Key, NewEvent.KeyString);

  Container.EventPress(NewEvent);

  { The result of "Container.EventPress" (whether the key was handled)
    is for now not used anywhere.
    Passing it back to LCL is not possible, since we do not process keys
    directly in TCastleControl.KeyDown, we wait for a matching
    UTFKeyPress. }
end;

procedure TCastleControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  { Do this before EventPress
    (would be nice to also do it after Pressed.KeyDown inside
    TCastleControl.KeyPressHandlerPress, but ignore for now) }
  UpdateShiftState(Shift);

  inherited KeyDown(Key, Shift); { LCL OnKeyDown before our callbacks }

  FKeyPressHandler.KeyDown(Key, Shift);

  { Do not change focus by arrow keys, this would break our handling of arrows
    over TCastleControl. We can prevent Lazarus from interpreting these
    keys as focus-changing (actually, Lazarus tells widget manager that these
    are already handled) by setting them to zero. }
  if (Key = VK_Down) or
     (Key = VK_Up) or
     (Key = VK_Right) or
     (Key = VK_Left) then
    Key := 0;
end;

procedure TCastleControl.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key); { LCL OnUTF8KeyPress before our callbacks }
  FKeyPressHandler.UTF8KeyPress(UTF8Key);
end;

procedure TCastleControl.KeyUp(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  MyKeyString: String;
begin
  { Do this before anything else, in particular before even Pressed.KeyUp below.
    This may call OnPress (which sets Pressed to true). }
  FKeyPressHandler.BeforeKeyUp(Key, Shift);

  MyKey := KeyLCLToCastle(Key, Shift);
  if MyKey <> keyNone then
    Pressed.KeyUp(MyKey, MyKeyString);

  UpdateShiftState(Shift); { do this after Pressed update above, and before EventRelease }

  { Do not change focus by arrow keys, this breaks our handling of them.
    See KeyDown for more comments. }
  if (Key = VK_Down) or
     (Key = VK_Up) or
     (Key = VK_Right) or
     (Key = VK_Left) then
    Key := 0;

  inherited KeyUp(Key, Shift); { LCL OnKeyUp before our callbacks }

  if (MyKey <> keyNone) or (MyKeyString <> '') then
    if Container.EventRelease(InputKey(MousePosition, MyKey, MyKeyString)) then
      Key := 0; // handled
end;

procedure TCastleControl.MouseDown(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: TCastleMouseButton;
begin
  if AutoFocus and not Focused then
    SetFocus;

  FMousePosition := Vector2(X, Height - 1 - Y);

  if MouseButtonLCLToCastle(Button, MyButton) then
    Container.MousePressed := Container.MousePressed + [MyButton];

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  inherited MouseDown(Button, Shift, X, Y); { LCL OnMouseDown before our callbacks }

  if MouseButtonLCLToCastle(Button, MyButton) then
    Container.EventPress(InputMouseButton(MousePosition, MyButton, 0,
      ModifiersDown(Container.Pressed)));
end;

procedure TCastleControl.MouseUp(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: TCastleMouseButton;
begin
  FMousePosition := Vector2(X, Height - 1 - Y);

  if MouseButtonLCLToCastle(Button, MyButton) then
    Container.MousePressed := Container.MousePressed - [MyButton];

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  inherited MouseUp(Button, Shift, X, Y); { LCL OnMouseUp before our callbacks }

  if MouseButtonLCLToCastle(Button, MyButton) then
    Container.EventRelease(InputMouseButton(MousePosition, MyButton, 0));
end;

procedure TCastleControl.AggressiveUpdate;
const
  MaxDesiredFPS = TCastleApplicationProperties.DefaultLimitFPS;
var
  DesiredFPS: Single;
begin
  if ApplicationProperties.LimitFPS <= 0 then
    DesiredFPS := MaxDesiredFPS
  else
    DesiredFPS := Min(MaxDesiredFPS, ApplicationProperties.LimitFPS);
  if TimerSeconds(Timer, Fps.UpdateStartTime) > 1 / DesiredFPS then
  begin
    DoUpdate;
    if Invalidated then Paint;
  end;
end;

procedure TCastleControl.MouseMove(Shift: TShiftState; NewX, NewY: Integer);
begin
  { check GLInitialized, because it seems it can be called before GL context
    is created (on Windows) or after it's destroyed (sometimes on Linux).
    We don't want to pass anything to Container in such case. }

  if GLInitialized then
  begin
    Container.EventMotion(InputMotion(MousePosition,
      Vector2(NewX, Height - 1 - NewY), MousePressed, 0));

    // change FMousePosition *after* EventMotion, callbacks may depend on it
    FMousePosition := Vector2(NewX, Height - 1 - NewY);

    UpdateShiftState(Shift);
    AggressiveUpdate;
  end;

  inherited MouseMove(Shift, NewX, NewY);
end;

function TCastleControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := Container.EventPress(InputMouseWheel(MousePosition, WheelDelta/120, true,
    ModifiersDown(Container.Pressed)));
  AggressiveUpdate;
  if Result then Exit;

  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TCastleControl.DoUpdate;
begin
  if AutoRedisplay then Invalidate;
  FKeyPressHandler.Flush; // finish any pending key presses

  { Update event also requires that proper OpenGL context is current.

    This matters because OpenGL resources may be used durign update,
    e.g. TCastleScene.Update will update auto-generated textures,
    doing e.g. TGLGeneratedCubeMapTextureNode.Update.
    This should run in proper OpenGL context.
    Esp. as not all resources must be shared between contexts:
    FBO are not shared in new OpenGL versions, see
    https://stackoverflow.com/questions/4385655/is-it-possible-to-share-an-opengl-framebuffer-object-between-contexts-threads

    Testcase: open examples/mobile/simple_3d_demo/ in editor,
    open main design,
    click on previews with GeneratedCubeMap like castle_with_lights_and_camera.wrl .
    Without this fix, we'll have an OpenGL error.

    Doing MakeCurrent here is consistent with TCastleWindow.DoUpdate . }
  MakeCurrent;
  Container.EventUpdate;
end;

procedure TCastleControl.DoExit;
begin
  inherited;
  ReleaseAllKeysAndMouse;
end;

procedure TCastleControl.Paint;
begin
  { Note that we don't call here inherited, instead doing everything ourselves. }
  if MakeCurrent then
  begin
    { clear Invalidated before rendering, so that calling Invalidate in OnRender works }
    Invalidated := false;
    Container.EventBeforeRender;
    Fps._RenderBegin;
    try
      Container.EventRender;
      DoOnPaint; // call OnPaint, like it would be a top-most TCastleUserInterface
      if GLVersion.BuggySwapNonStandardViewport then
        RenderContext.Viewport := Rect;
      SwapBuffers;
      // it seems calling Invalidate from Paint doesn't work, so we'll
      // have to do it elsewhere
      // if AutoRedisplay then Invalidate;
    finally Fps._RenderEnd end;
  end;
end;

function TCastleControl.SaveScreenBuffer: TColorBuffer;
begin
  if DoubleBuffered then
    Result := cbBack else
    Result := cbFront;
end;

procedure TCastleControl.SaveScreen(const URL: string);
begin
  Container.SaveScreen(URL);
end;

function TCastleControl.SaveScreen: TRGBImage;
begin
  Result := Container.SaveScreen;
end;

function TCastleControl.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  Result := Container.SaveScreen(SaveRect);
end;

procedure TCastleControl.SetMousePosition(const Value: TVector2);
var
  NewCursorPos: TPoint;
begin
  NewCursorPos := ControlToScreen(
    Point(Floor(Value[0]), Height - 1 - Floor(Value[1])));

  { Do not set Mouse.CursorPos to the same value, to make sure we don't cause
    unnecessary OnMotion on some systems while actual MousePosition didn't change. }
  if (NewCursorPos.x <> Mouse.CursorPos.x) or (NewCursorPos.y <> Mouse.CursorPos.y) then
    Mouse.CursorPos := NewCursorPos;
end;

function TCastleControl.MousePressed: TCastleMouseButtons;
begin
  Result := Container.MousePressed;
end;

function TCastleControl.Pressed: TKeysPressed;
begin
  Result := Container.Pressed;
end;

function TCastleControl.Fps: TFramesPerSecond;
begin
  Result := Container.Fps;
end;

function TCastleControl.Rect: TRectangle;
begin
  Result := Container.Rect;
end;

function TCastleControl.DesignedComponent(const ComponentName: String
  ): TComponent;
begin
  Result := Container.DesignedComponent(ComponentName);
end;

function TCastleControl.Controls: TInternalChildrenControls;
begin
  Result := Container.Controls;
end;

class function TCastleControl.GetMainContainer: TCastleContainer;
begin
  {$warnings off} // using MainControl just to keep it working
  if MainControl <> nil then
    Result := MainControl.Container
  else
    Result := nil;
  {$warnings on}
end;

procedure TCastleControl.OnTimerPaint(Sender: TObject);
begin
  if Invalidated then
    Inherited Invalidate;
end;

function TCastleControl.GetDesignUrl: String;
begin
  Result := Container.DesignUrl;
end;

procedure TCastleControl.SetDesignUrl(const Value: String);
begin
  Container.DesignUrl := Value;
end;

{ TLCLClipboard ----------------------------------------------------------- }

type
  TLCLClipboard = class(TCastleClipboard)
  protected
    function GetAsText: string; override;
    procedure SetAsText(const Value: string); override;
  end;

function TLCLClipboard.GetAsText: string;
begin
  Result := UTF8ToSys(Clipbrd.Clipboard.AsText);
end;

procedure TLCLClipboard.SetAsText(const Value: string);
begin
  Clipbrd.Clipboard.AsText := SysToUTF8(Value);
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitializeClipboard;
begin
  // make the Clipboard in CastleControls integrated with LCL clipboard
  RegisterClipboard(TLCLClipboard.Create);
end;

initialization
  ControlsList := TComponentList.Create(false);
  InitializeClipboard;
  OnMainContainer := @TCastleControl(nil).GetMainContainer;
finalization
  OnMainContainer := nil;
  FreeAndNil(ControlsList);
end.
