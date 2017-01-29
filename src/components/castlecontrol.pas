{
  Copyright 2008-2017 Michalis Kamburelis.

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
  Classes, SysUtils, OpenGLContext, Controls, Forms, CastleRectangles,
  CastleVectors, CastleKeysMouse, CastleUtils, CastleTimeUtils, StdCtrls,
  CastleUIControls, CastleCameras, X3DNodes, CastleScene, CastleLevels,
  CastleImages, CastleGLVersion, CastleSceneManager,
  CastleGLImages, CastleGLContainer, Castle2DSceneManager;

const
  DefaultLimitFPS = 100.0;

type
  TControlInputPressReleaseEvent = procedure (Sender: TObject; const Event: TInputPressRelease) of object;
  TControlInputMotionEvent = procedure (Sender: TObject; const Event: TInputMotion) of object;

  { OpenGL control, with extensions for "Castle Game Engine", including
    @link(Controls) list for TUIControl instances.
    Use a descendant TCastleControl to have a ready
    @link(TCastleControl.SceneManager) for 3D world.

    This extends TOpenGLControl, adding various features:

    @unorderedList(
      @item(@link(Controls) list where you can easily add TUIControl instances
        (like TCastleOnScreenMenu, TCastleButton and more).
        We will pass events to these controls, draw them etc.)

      @item(Continously called @link(DoUpdate) method, that allows to handle
        TUIControl.Update. This is something different than LCL "idle" event,
        as it's guaranteed to be run continously, even when your application
        is clogged with events (like when using TWalkCamera.MouseLook).)

      @item(Automatically calls GLInformationInitialize
        when OpenGL context is initialized. This will initialize GLVersion,
        GLUVersion, GLFeatures.)

      @item(FPS (frames per second) counter inside @link(Fps).)

      @item(Tracks pressed keys @link(Pressed) and mouse buttons @link(MousePressed)
        and mouse position @link(MousePosition).)
    ) }
  TCastleControlCustom = class(TCustomOpenGLControl)
  private
    type
      { Non-abstact implementation of TUIContainer that cooperates with
        TCastleControlCustom. }
      TContainer = class(TGLContainer)
      private
        Parent: TCastleControlCustom;
      protected
        function GetMousePosition: TVector2Single; override;
        procedure SetMousePosition(const Value: TVector2Single); override;
      public
        constructor Create(AParent: TCastleControlCustom); reintroduce;
        procedure Invalidate; override;
        function GLInitialized: boolean; override;
        function Width: Integer; override;
        function Height: Integer; override;
        function Rect: TRectangle; override;
        function Dpi: Integer; override;
        function MousePressed: TMouseButtons; override;
        function Focused: boolean; override;
        function Pressed: TKeysPressed; override;
        function Fps: TFramesPerSecond; override;
        procedure SetInternalCursor(const Value: TMouseCursor); override;
        function GetTouches(const Index: Integer): TTouch; override;
        function TouchesCount: Integer; override;
        function SaveScreen(const SaveRect: TRectangle): TRGBImage; override; overload;

        procedure EventOpen(const OpenWindowsCount: Cardinal); override;
        procedure EventClose(const OpenWindowsCount: Cardinal); override;
        function EventPress(const Event: TInputPressRelease): boolean; override;
        function EventRelease(const Event: TInputPressRelease): boolean; override;
        procedure EventUpdate; override;
        procedure EventMotion(const Event: TInputMotion); override;
        procedure EventBeforeRender; override;
        procedure EventRender; override;
        procedure EventResize; override;
      end;
    var
    FContainer: TContainer;
    FMousePosition: TVector2Single;
    FGLInitialized: boolean;
    FPressed: TKeysPressed;
    FMousePressed: CastleKeysMouse.TMouseButtons;
    FAutoRedisplay: boolean;
    { manually track when we need to be repainted, useful for AggressiveUpdate }
    Invalidated: boolean;
    FFps: TFramesPerSecond;
    FOnOpen: TNotifyEvent;
    FOnBeforeRender: TNotifyEvent;
    FOnRender: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnPress: TControlInputPressReleaseEvent;
    FOnRelease: TControlInputPressReleaseEvent;
    FOnMotion: TControlInputMotionEvent;
    FOnUpdate: TNotifyEvent;

    { Sometimes, releasing shift / alt / ctrl keys will not be reported
      properly to KeyDown / KeyUp. Example: opening a menu
      through Alt+F for "_File" will make keydown for Alt,
      but not keyup for it, and DoExit will not be called,
      so ReleaseAllKeysAndMouse will not be called.

      To counteract this, call this method when Shift state is known,
      to update Pressed when needed. }
    procedure UpdateShiftState(const Shift: TShiftState);

    procedure SetMousePosition(const Value: TVector2Single);
    procedure SetAutoRedisplay(const Value: boolean);
  protected
    procedure DestroyHandle; override;
    procedure DoExit; override;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { List of user-interface controls currently active.
      See @link(TUIContainer.Controls) for details. }
    function Controls: TChildrenControls;

    function MakeCurrent(SaveOldToStack: boolean = false): boolean; override;
    procedure Invalidate; override;
    procedure Paint; override;

    { Keys currently pressed. }
    property Pressed: TKeysPressed read FPressed;
    { Mouse buttons currently pressed.
      See @link(TUIContainer.MousePressed) for details. }
    property MousePressed: CastleKeysMouse.TMouseButtons read FMousePressed;
    procedure ReleaseAllKeysAndMouse;

    { Current mouse position.
      See @link(TTouch.Position) for a documentation how this is expressed. }
    property MousePosition: TVector2Single read FMousePosition write SetMousePosition;

    property Fps: TFramesPerSecond read FFps;

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
    property StencilBits;
    property AUXBuffers;
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

    property Container: TContainer read FContainer;

    { Event called when the OpenGL context is created,
      you can initialize things that require OpenGL context now.
      Often you do not need to use this callback (engine components will
      automatically create/release OpenGL resource when necessary),
      unless you deal with lower-level OpenGL resource managing (e.g. using
      TGLImageCore).
      You usually will also want to implement OnClose callback that
      should release stuff you create here.

      Often, instead of using this callback, it's cleaner to derive new classes
      from TUIControl class or it's descendants,
      and override their GLContextOpen / GLContextClose methods to react to
      context being open/closed. Using such TUIControl classes
      is usually easier, as you add/remove them from controls whenever
      you want (e.g. you add them in ApplicationInitialize),
      and underneath they create/release/create again the OpenGL resources
      when necessary. }
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
      by Fps.FrameTime. This is useful when you have something that needs
      to be done from time to time right before OnRender and that is very
      time-consuming. It such cases it is not desirable to put such time-consuming
      task inside OnRender because this would cause a sudden big change in
      Fps.FrameTime value. So you can avoid this by putting
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

      When you have some controls on the @link(Controls) list
      (in particular, the @link(TCastleControl.SceneManager) is also on this list),
      the OnRender event is done @bold(last).
      So here you can draw on top of the existing UI controls.
      To draw something underneath the existing controls, create a new TUIControl
      and override it's @link(TUIControl.Render) and insert it to the controls
      using @code(Controls.InsertBack(MyBackgroundControl);). }
    property OnRender: TNotifyEvent read FOnRender write FOnRender;

    { Called when the control size (@code(Width), @code(Height)) changes.
      It's also guaranteed to be called
      right after the OnOpen event.

      Our OpenGL context is already "current" when this event is called
      (MakeCurrent is done right before), like for other events.
      This is a good place to set OpenGL viewport and projection matrix.

      In the usual case, the SceneManager takes care of setting appropriate
      OpenGL projection, so you don't need to do anything here. }
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

    { Continously occuring event.
      This event is called at least as regularly as redraw,
      so it is continously called even when your game
      is overwhelmed by messages (like mouse moves) and redraws. }
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
  end;

  { Same as TGameSceneManager, redefined only to work as a sub-component
    of TCastleControl, otherwise Lazarus fails to update the uses clause
    correctly and you cannot edit the events of CastleControl1.SceneManager
    subcomponent. }
  TControlGameSceneManager = class(TGameSceneManager)
  end;

  { Render 3D world and GUI controls.
    Add your game stuff (3D descending from @link(T3D), like @link(TCastleScene))
    to the scene manager available in @link(SceneManager) property.
    Add your GUI stuff to the @link(TCastleControlCustom.Controls) property
    (from ancestor TCastleControlCustom).

    You can directly access the SceneManager and configure it however you like.

    You have comfortable @link(Load) method that simply loads a single 3D model
    to your world. }
  TCastleControl = class(TCastleControlCustom)
  private
    FSceneManager: TControlGameSceneManager;

    function GetShadowVolumes: boolean;
    function GetShadowVolumesRender: boolean;
    function GetOnCameraChanged: TNotifyEvent;
    procedure SetShadowVolumes(const Value: boolean);
    procedure SetShadowVolumesRender(const Value: boolean);
    procedure SetOnCameraChanged(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;

    { Load a single 3D model to your world
      (removing other models, and resetting the camera).

      This is nice for simple 3D model browsers, but usually for games you
      don't want to use this method --- it's more flexible to create TCastleScene
      yourself, and add it to scene manager yourself, see engine examples like
      scene_manager_basic.lpr. }
    procedure Load(const SceneURL: string);
    procedure Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);

    function MainScene: TCastleScene;
    function Camera: TCamera;
  published
    property SceneManager: TControlGameSceneManager read FSceneManager;

    property OnCameraChanged: TNotifyEvent
      read GetOnCameraChanged write SetOnCameraChanged;

    { See TCastleAbstractViewport.ShadowVolumes. }
    property ShadowVolumes: boolean
      read GetShadowVolumes write SetShadowVolumes
      default TCastleAbstractViewport.DefaultShadowVolumes;

    { See TCastleAbstractViewport.ShadowVolumesRender. }
    property ShadowVolumesRender: boolean
      read GetShadowVolumesRender write SetShadowVolumesRender default false;
  end;

  { Same as T2DSceneManager, redefined only to work as a sub-component
    of TCastleControl, otherwise Lazarus fails to update the uses clause
    correctly and you cannot edit the events of CastleControl1.SceneManager
    subcomponent. }
  TControl2DSceneManager = class(T2DSceneManager)
  end;

  { Render 2D game world and GUI.
    Add your game stuff, like @link(T2DScene), to the scene manager available
    in @link(SceneManager) property. Add your GUI stuff
    to the @link(TCastleControlCustom.Controls) property (from ancestor
    TCastleControlCustom).

    You can directly access the SceneManager and configure it however you like.

    The difference between this and @link(TCastleControl) is that this provides
    a scene manager descending from @link(T2DSceneManager), which is a little more
    comfortable for typical 2D games. See @link(T2DSceneManager) description
    for details. In principle, you can use any of these control classes
    to develop any mix of 3D or 2D game. }
  TCastle2DControl = class(TCastleControlCustom)
  private
    FSceneManager: TControl2DSceneManager;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SceneManager: TControl2DSceneManager read FSceneManager;
  end;

procedure Register;

var
  { Limit the number of (real) frames per second inside TCastleControl
    rendering, to not hog the CPU.
    Set to zero to not limit.

    See TCastleWindow.ProcessMessage documentation about WaitToLimitFPS
    parameter, and see TCastleApplication.LimitFPS documentation.

    The mechanism does mean sleeping in your process, so it's a global
    thing, not just a property of TCastleControl.
    However, the mechanism is activated only when some TCastleControl
    component is used, and only when LCL idle is fired (so we have no pending
    events, as LCL idle is "lazy" and fires only when process is really idle),
    and not at Lazarus design time.

    When we may be clogged with events (like when using mouse look)
    this has an additional meaning:
    it is then used to force TCastleControl.DoUpdate and (if needed) repaint
    at least this often. So it is not only a limit,
    it's more like "the desired number of FPS".
    Although it's capped by MaxDesiredFPS (100), which is applied when
    LimitFPS > MaxDesiredFPS or when LimitFPS = 0 (which means "infinity"). }
  LimitFPS: Single = DefaultLimitFPS;

implementation

uses LCLType, CastleGL, CastleGLUtils, CastleStringUtils, X3DLoad, Math,
  CastleLog, Contnrs, CastleLCLUtils, CastleApplicationProperties;

procedure Register;
begin
  RegisterComponents('Castle', [
    { TCastleControlCustom is not registered on a palette anymore,
      as the simplest approach is to use TCastleControl with
      scene manager instance already created.
      This avoids questions about "which one to use" (common at engine
      versions 2.x).

      Developers familiar with our architecture that want
      to use TCastleControlCustom (but not TCastleControl)
      will have to use it from code.
      Final decision about it (should it be visible on palette for advanced uses,
      and risk confusing novice users?) is still unsure (report on forum
      if you have any opinion). }
    { TCastleControlCustom, }
    TCastleControl,
    TCastle2DControl]);
end;

var
  { All TCastleControl instances created. We use this to share OpenGL contexts,
    as all OpenGL contexts in our engine must share OpenGL resources
    (our OnGLContextOpen and such callbacks depend on it,
    and it makes implementation much easier). }
  CastleControls: TComponentList;

  ControlsOpen: Cardinal;

{ Limit FPS ------------------------------------------------------------------ }

var
  LastLimitFPSTime: TTimerResult;

procedure DoLimitFPS;
var
  NowTime: TTimerResult;
  TimeRemainingFloat: Single;
begin
  if LimitFPS > 0 then
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
      1 / LimitFPS -
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

{ TCastleApplicationIdle -------------------------------------------------- }

type
  TCastleApplicationIdle = class
    class procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  end;

class procedure TCastleApplicationIdle.ApplicationIdle(Sender: TObject; var Done: Boolean);
var
  I: Integer;
  C: TCastleControlCustom;
begin
  { This should never be registered in design mode, to not conflict
    (by DoLimitFPS, or Done setting) with using Lazarus IDE. }
  Assert(not (csDesigning in Application.ComponentState));

  { Call DoUpdate for all TCastleControl instances. }
  for I := 0 to CastleControls.Count - 1 do
  begin
    C := CastleControls[I] as TCastleControlCustom;
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
    We don't want that, we want continous DoUpdate events.

    So we have to use Done := false.

    Unfortunately, Done := false prevents other idle actions
    (other TApplicationProperties.OnIdle) from working.
    See TApplication.Idle and TApplication.NotifyIdleHandler implementation
    in lcl/include/application.inc .
    To at least allow all TCastleControlCustom work, we use a central
    ApplicationIdle callback (we don't use separate TApplicationProperties
    for each TCastleControl; in fact, we don't need TApplicationProperties
    at all). }

  Done := false;
end;

var
  ApplicationIdleSet: boolean;

{ TCastleControlCustom.TContainer ----------------------------------------------------- }

constructor TCastleControlCustom.TContainer.Create(AParent: TCastleControlCustom);
begin
  inherited Create(AParent); // AParent must be a component Owner to show published properties of container in LFM
  Parent := AParent;
end;

procedure TCastleControlCustom.TContainer.Invalidate;
begin
  Parent.Invalidate;
end;

function TCastleControlCustom.TContainer.GLInitialized: boolean;
begin
  Result := Parent.GLInitialized;
end;

function TCastleControlCustom.TContainer.Width: Integer;
begin
  Result := Parent.Width;
end;

function TCastleControlCustom.TContainer.Height: Integer;
begin
  Result := Parent.Height;
end;

function TCastleControlCustom.TContainer.Rect: TRectangle;
begin
  Result := Parent.Rect;
end;

function TCastleControlCustom.TContainer.GetMousePosition: TVector2Single;
begin
  Result := Parent.MousePosition;
end;

procedure TCastleControlCustom.TContainer.SetMousePosition(const Value: TVector2Single);
begin
  Parent.MousePosition := Value;
end;

function TCastleControlCustom.TContainer.Dpi: Integer;
begin
  Result := DefaultDpi; //Parent.Dpi; // for now, TCastleControl doesn't expose any useful Dpi
end;

function TCastleControlCustom.TContainer.MousePressed: TMouseButtons;
begin
  Result := Parent.MousePressed;
end;

function TCastleControlCustom.TContainer.Focused: boolean;
begin
  Result := true; // TODO: for now, TCastleControl always pretends to be focused
end;

function TCastleControlCustom.TContainer.Pressed: TKeysPressed;
begin
  Result := Parent.Pressed;
end;

function TCastleControlCustom.TContainer.Fps: TFramesPerSecond;
begin
  Result := Parent.Fps;
end;

procedure TCastleControlCustom.TContainer.SetInternalCursor(const Value: TMouseCursor);
var
  NewCursor: TCursor;
begin
  NewCursor := CursorCastleToLCL[Value];

  { check explicitly "Cursor <> NewCursor" --- we will call UpdateFocusAndMouseCursor
    very often (in each mouse move), and we don't want to depend on LCL
    optimizing "Cursor := Cursor" to avoid some potentially expensive window
    manager call. }
  if Parent.Cursor <> NewCursor then
    Parent.Cursor := NewCursor;
end;

function TCastleControlCustom.TContainer.GetTouches(const Index: Integer): TTouch;
begin
  Assert(Index = 0, 'TCastleControlCustom always has only one item in Touches array, with index 0');
  Result.FingerIndex := 0;
  Result.Position := Parent.MousePosition;
end;

function TCastleControlCustom.TContainer.TouchesCount: Integer;
begin
  Result := 1;
end;

function TCastleControlCustom.TContainer.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  if Parent.MakeCurrent then
  begin
    EventBeforeRender;
    EventRender;
  end;
  Result := SaveScreen_NoFlush(Rect, Parent.SaveScreenBuffer);
end;

procedure TCastleControlCustom.TContainer.EventOpen(const OpenWindowsCount: Cardinal);
begin
  inherited;
  if Assigned(Parent.FOnOpen) then
    Parent.FOnOpen(Parent);
end;

procedure TCastleControlCustom.TContainer.EventClose(const OpenWindowsCount: Cardinal);
begin
  if Assigned(Parent.FOnClose) then
    Parent.FOnClose(Parent);
  inherited;
end;

function TCastleControlCustom.TContainer.EventPress(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if (not Result) and Assigned(Parent.FOnPress) then
  begin
    Parent.FOnPress(Parent, Event);
    Result := true;
  end;
end;

function TCastleControlCustom.TContainer.EventRelease(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if (not Result) and Assigned(Parent.FOnRelease) then
  begin
    Parent.FOnRelease(Parent, Event);
    Result := true;
  end;
end;

procedure TCastleControlCustom.TContainer.EventUpdate;
begin
  inherited;
  if Assigned(Parent.FOnUpdate) then
    Parent.FOnUpdate(Parent);
end;

procedure TCastleControlCustom.TContainer.EventMotion(const Event: TInputMotion);
begin
  inherited;
  if Assigned(Parent.FOnMotion) then
    Parent.FOnMotion(Parent, Event);
end;

procedure TCastleControlCustom.TContainer.EventBeforeRender;
begin
  inherited;
  if Assigned(Parent.FOnBeforeRender) then
    Parent.FOnBeforeRender(Parent);
end;

procedure TCastleControlCustom.TContainer.EventRender;
begin
  inherited;
  if Assigned(Parent.FOnRender) then
    Parent.FOnRender(Parent);
end;

procedure TCastleControlCustom.TContainer.EventResize;
begin
  inherited;
  if Assigned(Parent.FOnResize) then
    Parent.FOnResize(Parent);
end;

{ TCastleControlCustom -------------------------------------------------- }

constructor TCastleControlCustom.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := true;
  FFps := TFramesPerSecond.Create;
  FPressed := TKeysPressed.Create;
  FAutoRedisplay := true;

  FContainer := TContainer.Create(Self);
  { SetSubComponent and Name setting (must be unique only within TCastleControl,
    so no troubles) are necessary to store it in LFM and display in object inspector
    nicely. }
  FContainer.SetSubComponent(true);
  FContainer.Name := 'Container';

  if CastleControls.Count <> 0 then
    SharedControl := CastleControls[0] as TCastleControl;
  CastleControls.Add(Self);

  Invalidated := false;

  if (not (csDesigning in ComponentState)) and (not ApplicationIdleSet) then
  begin
    ApplicationIdleSet := true;
    Application.AddOnIdleHandler(@(TCastleApplicationIdle(nil).ApplicationIdle));
  end;
end;

destructor TCastleControlCustom.Destroy;
begin
  if ApplicationIdleSet and
     (CastleControls <> nil) and
     { If CastleControls.Count will become 0 after this destructor,
       then unregisted our idle callback.
       If everyhting went Ok, CastleControls.Count = 1 should always imply
       that we're the only control there. But check "CastleControls[0] = Self"
       in case we're in destructor because there was an exception
       in the constructor. }
     (CastleControls.Count = 1) and
     (CastleControls[0] = Self) then
  begin
    ApplicationIdleSet := false;
    Application.RemoveOnIdleHandler(@(TCastleApplicationIdle(nil).ApplicationIdle));
  end;

  FreeAndNil(FPressed);
  FreeAndNil(FFps);
  FreeAndNil(FContainer);
  inherited;
end;

procedure TCastleControlCustom.SetAutoRedisplay(const Value: boolean);
begin
  FAutoRedisplay := value;
  if Value then Invalidate;
end;

{ Initial idea was to do

procedure TCastleControlCustom.CreateHandle;
begin
  Writeln('TCastleControlCustom.CreateHandle ', GLInitialized,
    ' ', OnGLContextOpen <> nil);
  inherited CreateHandle;
  if not GLInitialized then
  begin
    GLInitialized := true;
    Container.EventOpen;
  end;
  Writeln('TCastleControlCustom.CreateHandle end');
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

function TCastleControlCustom.MakeCurrent(SaveOldToStack: boolean): boolean;
begin
  Result := inherited MakeCurrent(SaveOldToStack);

  RenderContext := Container.Context;

  if not GLInitialized then
  begin
    FGLInitialized := true;
    GLInformationInitialize;
    Inc(ControlsOpen);
    Container.EventOpen(ControlsOpen);
    Resize; // will call Container.EventResize
    Invalidate;
  end;
end;

procedure TCastleControlCustom.DestroyHandle;
begin
  if GLInitialized then
  begin
    Container.EventClose(ControlsOpen);
    Dec(ControlsOpen);
    FGLInitialized := false;
  end;
  inherited DestroyHandle;
end;

procedure TCastleControlCustom.Resize;
begin
  inherited;

  { Call MakeCurrent here, to make sure CastleUIControls always get
    Resize with good GL context. }
  if GLInitialized and MakeCurrent then
    Container.EventResize;
end;

procedure TCastleControlCustom.Invalidate;
begin
  Invalidated := true;
  inherited;
end;

procedure TCastleControlCustom.ReleaseAllKeysAndMouse;
begin
  Pressed.Clear;
  FMousePressed := [];
end;

procedure TCastleControlCustom.UpdateShiftState(const Shift: TShiftState);
begin
  Pressed.Keys[K_Shift] := ssShift in Shift;
  Pressed.Keys[K_Alt  ] := ssAlt   in Shift;
  Pressed.Keys[K_Ctrl ] := ssCtrl  in Shift;
end;

procedure TCastleControlCustom.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
begin
  KeyLCLToCastle(Key, Shift, MyKey, Ch);
  if (MyKey <> K_None) or (Ch <> #0) then
    Pressed.KeyDown(MyKey, Ch);

  UpdateShiftState(Shift); { do this after Pressed update above, and before EventPress }

  { Do not change focus by arrow keys, this would breaks our handling of arrows
    over TCastleControl. We can prevent Lazarus from interpreting these
    keys as focus-changing (actually, Lazarus tells widget managet that these
    are already handled) by setting them to zero.
    Note: our MyKey/Ch (passed to KeyDownEvent) are calculated earlier,
    so they will correctly capture arrow keys. }
  if (Key = VK_Down) or
     (Key = VK_Up) or
     (Key = VK_Right) or
     (Key = VK_Left) then
    Key := 0;

  inherited KeyDown(Key, Shift); { LCL OnKeyDown before our callbacks }

  if (MyKey <> K_None) or (Ch <> #0) then
    if Container.EventPress(InputKey(MousePosition, MyKey, Ch)) then
      Key := 0; // handled
end;

procedure TCastleControlCustom.KeyUp(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
begin
  KeyLCLToCastle(Key, Shift, MyKey, Ch);
  if MyKey <> K_None then
    Pressed.KeyUp(MyKey, Ch);

  UpdateShiftState(Shift); { do this after Pressed update above, and before EventRelease }

  { Do not change focus by arrow keys, this breaks our handling of them.
    See KeyDown for more comments. }
  if (Key = VK_Down) or
     (Key = VK_Up) or
     (Key = VK_Right) or
     (Key = VK_Left) then
    Key := 0;

  inherited KeyUp(Key, Shift); { LCL OnKeyUp before our callbacks }

  if (MyKey <> K_None) or (Ch <> #0) then
    if Container.EventRelease(InputKey(MousePosition, MyKey, Ch)) then
      Key := 0; // handled
end;

procedure TCastleControlCustom.MouseDown(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: CastleKeysMouse.TMouseButton;
begin
  FMousePosition := Vector2Single(X, Height - 1 - Y);

  if MouseButtonLCLToCastle(Button, MyButton) then
    Include(FMousePressed, MyButton);

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  inherited MouseDown(Button, Shift, X, Y); { LCL OnMouseDown before our callbacks }

  if MouseButtonLCLToCastle(Button, MyButton) then
    Container.EventPress(InputMouseButton(MousePosition, MyButton, 0));
end;

procedure TCastleControlCustom.MouseUp(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: CastleKeysMouse.TMouseButton;
begin
  FMousePosition := Vector2Single(X, Height - 1 - Y);

  if MouseButtonLCLToCastle(Button, MyButton) then
    Exclude(FMousePressed, MyButton);

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  inherited MouseUp(Button, Shift, X, Y); { LCL OnMouseUp before our callbacks }

  if MouseButtonLCLToCastle(Button, MyButton) then
    Container.EventRelease(InputMouseButton(MousePosition, MyButton, 0));
end;

procedure TCastleControlCustom.MouseMove(Shift: TShiftState; NewX, NewY: Integer);

  { Force DoUpdate and Paint (if invalidated) events to happen,
    if sufficient time (based on LimitFPS, that in this case acts like
    "desired FPS") passed.
    This is needed when user "clogs" the GTK / WinAPI / Qt etc. event queue.
    In this case Lazarus (LCL) doesn't automatically fire the idle and repaint
    events.

    The behavior of Lazarus application Idle events is such that they
    are executed only when there are no events left to process.
    This makes sense, and actually follows the docs and the name "idle".

    In contrast, our DoUpdate expects to be run continously, that is:
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
    the mouse look to walk and look around in Walk mode (TWalkCamera.MouseLook),
    or when dragging with mouse
    in Examine mode. The event loop is then typically busy processing mouse move
    events all the time, so it's never/seldom empty (note: it doesn't mean that
    event loop is clogged, as mouse move events can be potentially accumulated
    at various levels --- LCL, underlying widgetset like GTK, underlying system
    like XWindows etc. I think in practice XWindows does it, but I'm not sure).
    Our program should however still be responsive. Not only the screen should
    be redrawn, regardless if our event loop is empty or not, but also
    our Update event should be continously called. But if we just use LCL Idle/Redraw
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
      either update with stuttering, or not update (continously) at all.
      TCastleWindow doesn't have this problem, mouse look is smooth there.
    - It's also not needed from events other than mouse move.

    In theory, for LimitFPS = 0, we should just do this every time.
    But this would overload the system
    (you would see smooth animation and rendering, but there will be latency
    with respect to handling input, e.g. mouse move will be processed with
    a small delay). So we use MaxDesiredFPS to cap it. }
  procedure AggressiveUpdate;
  const
    MaxDesiredFPS = DefaultLimitFPS;
  var
    DesiredFPS: Single;
  begin
    if LimitFPS <= 0 then
      DesiredFPS := MaxDesiredFPS else
      DesiredFPS := Min(MaxDesiredFPS, LimitFPS);
    if TimerSeconds(Timer, Fps.UpdateStartTime) > 1 / DesiredFPS then
    begin
      DoUpdate;
      if Invalidated then Paint;
    end;
  end;

begin
  { check GLInitialized, because it seems it can be called before GL context
    is created (on Windows) or after it's destroyed (sometimes on Linux).
    We don't want to pass anything to Container in such case. }

  if GLInitialized then
  begin
    Container.EventMotion(InputMotion(MousePosition,
      Vector2Single(NewX, Height - 1 - NewY), MousePressed, 0));

    // change FMousePosition *after* EventMotion, callbacks may depend on it
    FMousePosition := Vector2Single(NewX, Height - 1 - NewY);

    UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }
    AggressiveUpdate;
  end;

  inherited MouseMove(Shift, NewX, NewY);
end;

function TCastleControlCustom.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := Container.EventPress(InputMouseWheel(MousePosition, WheelDelta/120, true));
  if Result then Exit;

  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TCastleControlCustom.DoUpdate;
begin
  if AutoRedisplay then Invalidate;
  Fps._UpdateBegin;
  Container.EventUpdate;
end;

procedure TCastleControlCustom.DoExit;
begin
  inherited;
  ReleaseAllKeysAndMouse;
end;

procedure TCastleControlCustom.Paint;
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
      DoOnPaint; // call OnPaint, like it would be a top-most TUIControl
      if GLVersion.BuggySwapNonStandardViewport then
        glViewport(Rect);
      SwapBuffers;
      // it seems calling Invalidate from Paint doesn't work, so we'll
      // have to do it elsewhere
      // if AutoRedisplay then Invalidate;
    finally Fps._RenderEnd end;
  end;
end;

function TCastleControlCustom.SaveScreenBuffer: TColorBuffer;
begin
  if DoubleBuffered then
    Result := cbBack else
    Result := cbFront;
end;

procedure TCastleControlCustom.SaveScreen(const URL: string);
begin
  Container.SaveScreen(URL);
end;

function TCastleControlCustom.SaveScreen: TRGBImage;
begin
  Result := Container.SaveScreen;
end;

function TCastleControlCustom.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  Result := Container.SaveScreen(SaveRect);
end;

procedure TCastleControlCustom.SetMousePosition(const Value: TVector2Single);
begin
  Mouse.CursorPos := ControlToScreen(Point(
    Floor(Value[0]), Height - 1 - Floor(Value[1])));
end;

function TCastleControlCustom.Rect: TRectangle;
begin
  Result := Rectangle(0, 0, Width, Height);
end;

function TCastleControlCustom.Controls: TChildrenControls;
begin
  Result := Container.Controls;
end;

{ TCastleControl ----------------------------------------------------------- }

constructor TCastleControl.Create(AOwner: TComponent);
begin
  inherited;

  FSceneManager := TControlGameSceneManager.Create(Self);
  { SetSubComponent and Name setting (must be unique only within TCastleControl,
    so no troubles) are necessary to store it in LFM and display in object inspector
    nicely. }
  FSceneManager.SetSubComponent(true);
  FSceneManager.Name := 'SceneManager';
  Controls.InsertFront(SceneManager);
end;

procedure TCastleControl.Load(const SceneURL: string);
begin
  Load(Load3D(SceneURL, false), true);
end;

procedure TCastleControl.Load(ARootNode: TX3DRootNode; const OwnsRootNode: boolean);
begin
  { destroy MainScene and Camera, we will recreate them }
  SceneManager.MainScene.Free;
  SceneManager.MainScene := nil;
  SceneManager.Items.Clear;
  SceneManager.Camera.Free;

  SceneManager.MainScene := TCastleScene.Create(Self);
  SceneManager.MainScene.Load(ARootNode, OwnsRootNode);
  SceneManager.Items.Add(SceneManager.MainScene);

  { initialize octrees titles }
  MainScene.TriangleOctreeProgressTitle := 'Building triangle octree';
  MainScene.ShapeOctreeProgressTitle := 'Building shape octree';

  { just to make our Camera always non-nil.
    Useful for model_3d_viewer that wants to initialize NavigationType
    from camera. }
  SceneManager.Camera := SceneManager.CreateDefaultCamera;
end;

function TCastleControl.MainScene: TCastleScene;
begin
  Result := SceneManager.MainScene;
end;

function TCastleControl.Camera: TCamera;
begin
  Result := SceneManager.Camera;
end;

function TCastleControl.GetShadowVolumes: boolean;
begin
  Result := SceneManager.ShadowVolumes;
end;

procedure TCastleControl.SetShadowVolumes(const Value: boolean);
begin
  SceneManager.ShadowVolumes := Value;
end;

function TCastleControl.GetShadowVolumesRender: boolean;
begin
  Result := SceneManager.ShadowVolumesRender;
end;

procedure TCastleControl.SetShadowVolumesRender(const Value: boolean);
begin
  SceneManager.ShadowVolumesRender := Value;
end;

function TCastleControl.GetOnCameraChanged: TNotifyEvent;
begin
  Result := SceneManager.OnCameraChanged;
end;

procedure TCastleControl.SetOnCameraChanged(const Value: TNotifyEvent);
begin
  SceneManager.OnCameraChanged := Value;
end;

{ TCastle2DControl ----------------------------------------------------------- }

constructor TCastle2DControl.Create(AOwner: TComponent);
begin
  inherited;

  FSceneManager := TControl2DSceneManager.Create(Self);
  { SetSubComponent and Name setting (must be unique only within TCastleControl,
    so no troubles) are necessary to store it in LFM and display in object inspector
    nicely. }
  FSceneManager.SetSubComponent(true);
  FSceneManager.Name := 'SceneManager';
  Controls.InsertFront(SceneManager);
end;

initialization
  CastleControls := TComponentList.Create(false);
finalization
  FreeAndNil(CastleControls);
end.
