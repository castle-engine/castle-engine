{
  Copyright 2008-2014 Michalis Kamburelis.

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenGLContext, Controls, Forms, CastleRectangles,
  CastleVectors, CastleKeysMouse, CastleUtils, CastleTimeUtils, StdCtrls,
  CastleUIControls, CastleCameras, X3DNodes, CastleScene, CastleLevels,
  CastleImages, CastleGLVersion, pk3DConnexion, CastleSceneManager,
  CastleGLImages, CastleGLContainer;

const
  DefaultLimitFPS = 100.0;

type
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
        and mouse position (@link(MouseX), @link(MouseY)).)
    ) }
  TCastleControlCustom = class(TOpenGLControl)
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
        function Pressed: TKeysPressed; override;
        function Fps: TFramesPerSecond; override;
        procedure SetCursor(const Value: TMouseCursor); override;
      end;
    var
    FContainer: TContainer;
    FMousePosition: TVector2Single;
    FGLInitialized: boolean;
    FPressed: TKeysPressed;
    FMousePressed: CastleKeysMouse.TMouseButtons;

    { manually track when we need to be repainted, useful for AggressiveUpdate }
    Invalidated: boolean;

    FFps: TFramesPerSecond;

    { Sometimes, releasing shift / alt / ctrl keys will not be reported
      properly to KeyDown / KeyUp. Example: opening a menu
      through Alt+F for "_File" will make keydown for Alt,
      but not keyup for it, and DoExit will not be called,
      so ReleaseAllKeysAndMouse will not be called.

      To counteract this, call this method when Shift state is known,
      to update Pressed when needed. }
    procedure UpdateShiftState(const Shift: TShiftState);

    function GetOnOpen: TContainerEvent;
    procedure SetOnOpen(const Value: TContainerEvent);
    function GetOnBeforeRender: TContainerEvent;
    procedure SetOnBeforeRender(const Value: TContainerEvent);
    function GetOnRender: TContainerEvent;
    procedure SetOnRender(const Value: TContainerEvent);
    function GetOnResize: TContainerEvent;
    procedure SetOnResize(const Value: TContainerEvent);
    function GetOnClose: TContainerEvent;
    procedure SetOnClose(const Value: TContainerEvent);
    function GetOnUpdate: TContainerEvent;
    procedure SetOnUpdate(const Value: TContainerEvent);
    function GetOnPress: TInputPressReleaseEvent;
    procedure SetOnPress(const Value: TInputPressReleaseEvent);
    function GetOnRelease: TInputPressReleaseEvent;
    procedure SetOnRelease(const Value: TInputPressReleaseEvent);
    function GetOnMotion: TInputMotionEvent;
    procedure SetOnMotion(const Value: TInputMotionEvent);
    procedure SetMousePosition(const Value: TVector2Single);
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

    function Controls: TUIControlList;

    function MakeCurrent(SaveOldToStack: boolean = false): boolean; override;
    procedure Invalidate; override;
    procedure Paint; override;

    property Pressed: TKeysPressed read FPressed;
    property MousePressed: CastleKeysMouse.TMouseButtons read FMousePressed;
    procedure ReleaseAllKeysAndMouse;

    property MousePosition: TVector2Single read FMousePosition write SetMousePosition;

    property Fps: TFramesPerSecond read FFps;

    { Color buffer where we draw, and from which it makes sense to grab pixels.
      Use with SaveScreen_NoFlush. }
    function SaveScreenBuffer: TColorBuffer;

    { Capture the current control contents to an image.
      These functions take care of flushing any pending redraw operations
      and capturing the screen contents correctly. }
    function SaveScreen: TRGBImage;

    { Rectangle representing the inside of this container.
      Always (Left,Bottom) are zero, and (Width,Height) correspond to container
      sizes. }
    function Rect: TRectangle;

    property OnOpen: TContainerEvent read GetOnOpen write SetOnOpen;
    property OnBeforeRender: TContainerEvent read GetOnBeforeRender write SetOnBeforeRender;
    property OnRender: TContainerEvent read GetOnRender write SetOnRender;
    property OnResize: TContainerEvent read GetOnResize write SetOnResize;
    property OnClose: TContainerEvent read GetOnClose write SetOnClose;
    property OnPress: TInputPressReleaseEvent read GetOnPress write SetOnPress;
    property OnRelease: TInputPressReleaseEvent read GetOnRelease write SetOnRelease;
    property OnMotion: TInputMotionEvent read GetOnMotion write SetOnMotion;
    property OnUpdate: TContainerEvent read GetOnUpdate write SetOnUpdate;
  published
    property TabOrder;
    property TabStop default true;
    property Container: TContainer read FContainer;
  end;

  { Lazarus component with an OpenGL context, most comfortable to render 3D worlds
    with 2D controls above. Add your 3D stuff to the scene manager
    available in @link(SceneManager) property. Add your 2D stuff
    to the @link(TCastleControlCustom.Controls) property (from ancestor TCastleControlCustom).

    You can directly access the SceneManager and configure it however you like.

    You have comfortable @link(Load) method that simply loads a single 3D model
    to your world. }
  TCastleControl = class(TCastleControlCustom)
  private
    FSceneManager: TGameSceneManager;

    function GetShadowVolumes: boolean;
    function GetShadowVolumesRender: boolean;
    function GetOnCameraChanged: TNotifyEvent;
    procedure SetShadowVolumes(const Value: boolean);
    procedure SetShadowVolumesRender(const Value: boolean);
    procedure SetOnCameraChanged(const Value: TNotifyEvent);
  public
    constructor Create(AOwner :TComponent); override;

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
    property SceneManager: TGameSceneManager read FSceneManager;

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
  CastleLog, Contnrs, CastleLCLUtils;

procedure Register;
begin
  RegisterComponents('Castle', [
    { For engine 3.0.0, TCastleControlCustom is not registered on palette,
      as the suggested usage for everyone is to take TCastleControl with
      scene manager instance already created.
      In engine 2.x, I was getting questions about which one to use,
      and it seems  that noone grokked the difference between the two.
      Final decision about it (should it be visible on palette for advanced uses,
      and risk confusing novice users?) is still unsure (report on forum
      if you have any opinion). }
    { TCastleControlCustom, }
    TCastleControl]);
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
      (NowTime - LastLimitFPSTime) / TimerFrequency;
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

function TCastleControlCustom.TContainer.Pressed: TKeysPressed;
begin
  Result := Parent.Pressed;
end;

function TCastleControlCustom.TContainer.Fps: TFramesPerSecond;
begin
  Result := Parent.Fps;
end;

procedure TCastleControlCustom.TContainer.SetCursor(const Value: TMouseCursor);
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

{ TCastleControlCustom -------------------------------------------------- }

constructor TCastleControlCustom.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := true;
  FFps := TFramesPerSecond.Create;
  FPressed := TKeysPressed.Create;

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
    ContainerResize with good GL context. }
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
    if Container.EventPress(InputKey(MyKey, Ch)) then
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
    if Container.EventRelease(InputKey(MyKey, Ch)) then
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
    if Timer - Fps.UpdateStartTime > TimerFrequency / DesiredFPS then
    begin
      DoUpdate;
      if Invalidated then Paint;
    end;
  end;

begin
  Container.EventMotion(InputMotion(MousePosition,
    Vector2Single(NewX, Height - 1 - NewY), MousePressed, 0));

  // change FMousePosition *after* EventMotion, callbacks may depend on it
  FMousePosition := Vector2Single(NewX, Height - 1 - NewY);

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }
  AggressiveUpdate;

  inherited MouseMove(Shift, NewX, NewY);
end;

function TCastleControlCustom.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := Container.EventPress(InputMouseWheel(WheelDelta/120, true));
  if Result then Exit;

  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TCastleControlCustom.DoUpdate;
begin
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
      if GLVersion.BuggySwapNonStandardViewport then
        glViewport(Rect);
      SwapBuffers;
    finally Fps._RenderEnd end;
  end;
end;

function TCastleControlCustom.SaveScreenBuffer: TColorBuffer;
begin
  if DoubleBuffered then
    Result := cbBack else
    Result := cbFront;
end;

function TCastleControlCustom.SaveScreen: TRGBImage;
begin
  if MakeCurrent then
  begin
    Container.EventBeforeRender;
    Container.EventRender;
  end;
  Result := SaveScreen_NoFlush(Rect, SaveScreenBuffer);
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

function TCastleControlCustom.Controls: TUIControlList;
begin
  Result := Container.Controls;
end;

function TCastleControlCustom.GetOnOpen: TContainerEvent;
begin
  Result := Container.OnOpen;
end;

procedure TCastleControlCustom.SetOnOpen(const Value: TContainerEvent);
begin
  Container.OnOpen := Value;
end;

function TCastleControlCustom.GetOnBeforeRender: TContainerEvent;
begin
  Result := Container.OnBeforeRender;
end;

procedure TCastleControlCustom.SetOnBeforeRender(const Value: TContainerEvent);
begin
  Container.OnBeforeRender := Value;
end;

function TCastleControlCustom.GetOnRender: TContainerEvent;
begin
  Result := Container.OnRender;
end;

procedure TCastleControlCustom.SetOnRender(const Value: TContainerEvent);
begin
  Container.OnRender := Value;
end;

function TCastleControlCustom.GetOnResize: TContainerEvent;
begin
  Result := Container.OnResize;
end;

procedure TCastleControlCustom.SetOnResize(const Value: TContainerEvent);
begin
  Container.OnResize := Value;
end;

function TCastleControlCustom.GetOnClose: TContainerEvent;
begin
  Result := Container.OnClose;
end;

procedure TCastleControlCustom.SetOnClose(const Value: TContainerEvent);
begin
  Container.OnClose := Value;
end;

function TCastleControlCustom.GetOnUpdate: TContainerEvent;
begin
  Result := Container.OnUpdate;
end;

procedure TCastleControlCustom.SetOnUpdate(const Value: TContainerEvent);
begin
  Container.OnUpdate := Value;
end;

function TCastleControlCustom.GetOnPress: TInputPressReleaseEvent;
begin
  Result := Container.OnPress;
end;

procedure TCastleControlCustom.SetOnPress(const Value: TInputPressReleaseEvent);
begin
  Container.OnPress := Value;
end;

function TCastleControlCustom.GetOnRelease: TInputPressReleaseEvent;
begin
  Result := Container.OnRelease;
end;

procedure TCastleControlCustom.SetOnRelease(const Value: TInputPressReleaseEvent);
begin
  Container.OnRelease := Value;
end;

function TCastleControlCustom.GetOnMotion: TInputMotionEvent;
begin
  Result := Container.OnMotion;
end;

procedure TCastleControlCustom.SetOnMotion(const Value: TInputMotionEvent);
begin
  Container.OnMotion := Value;
end;

{ TCastleControl ----------------------------------------------------------- }

constructor TCastleControl.Create(AOwner :TComponent);
begin
  inherited;

  FSceneManager := TGameSceneManager.Create(Self);
  { SetSubComponent and Name setting (must be unique only within TCastleControl,
    so no troubles) are necessary to store it in LFM and display in object inspector
    nicely. }
  FSceneManager.SetSubComponent(true);
  FSceneManager.Name := 'SceneManager';
  Controls.Add(SceneManager);
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

initialization
  CastleControls := TComponentList.Create(false);
finalization
  FreeAndNil(CastleControls);
end.
