{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL context component with Kambi improvements (TKamOpenGLControl). }
unit KambiGLControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenGLContext, Controls, Forms,
  VectorMath, KeysMouse, KambiUtils, KambiTimeUtils, StdCtrls, UIControls;

const
  { Default value for TKamOpenGLControlCore.AggressiveUpdateGap.
    "1000 div 60" means that we strike for 60 frames per second,
    although this is gross approximation (no guarantees, of course;
    especially if your Idle / Draw take a long time). }
  DefaultAggressiveUpdateGap = 1000 div 60;

  { Default value for TKamOpenGLControlCore.AggressiveUpdate }
  DefaultAggressiveUpdate = false;

  DefaultTooltipDelay = 1000;
  DefaultTooltipDistance = 10;

type
  { OpenGL control, with a couple of extensions for "Kambi VRML game engine".
    You will usually prefer to use TKamOpenGLControl instead of directly this
    class, TKamOpenGLControl adds some very useful features like
    @link(TKamOpenGLControl.Controls).

    Provides OnGLContextOpen and OnGLContextClose events.

    Provides comfortable Idle method. And a special AggressiveUpdate hack
    to be able to continously update (call Idle and Draw) even when the window
    system clogs us with events (this typically happens when user moves the mouse
    and we use TWalkCamera.MouseLook).

    Also, this automatically calls LoadAllExtensions
    when GL context is initialized. This will initialize all extensions
    and set GLVersion variables, describing OpenGL version
    and available extensions. }
  TKamOpenGLControlCore = class(TOpenGLControl)
  private
    FMouseX: Integer;
    FMouseY: Integer;
    FOnBeforeDraw: TNotifyEvent;
    FOnDraw: TNotifyEvent;
    FGLInitialized: boolean;
    FPressed: TKeysPressed;
    FMousePressed: KeysMouse.TMouseButtons;

    FAggressiveUpdate: boolean;
    FAggressiveUpdateGap: TMilisecTime;
    LastAggressiveUpdateTime: TMilisecTime; { tracked only when AggressiveUpdate }
    Invalidated: boolean; { tracked only when AggressiveUpdate }

    FOnGLContextOpen: TNotifyEvent;
    FOnGLContextClose: TNotifyEvent;

    FFps: TFramesPerSecond;

    ApplicationProperties: TApplicationProperties;
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);

    procedure AggressiveUpdateTick;

    { Sometimes, releasing shift / alt / ctrl keys will not be reported
      properly to KeyDown / KeyUp. Example: opening a menu
      through Alt+F for "_File" will make keydown for Alt,
      but not keyup for it, and DoExit will not be called,
      so ReleaseAllKeysAndMouse will not be called.

      To counteract this, call this method when Shift state is known,
      to update Pressed when needed. }
    procedure UpdateShiftState(const Shift: TShiftState);

    { For IUIContainer interface. Private, since when you have a class
      instance, you just use class properties (that read directly from a field,
      without the overhead of a function call). }
    function GetMouseX: Integer;
    function GetMouseY: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetMousePressed: TMouseButtons;
    function GetPressed: TKeysPressed;
  protected
    procedure DestroyHandle; override;
    procedure DoExit; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Integer); override;

    { Overriden KeyDown, KeyUp methods call some necessary stuff (like inherited,
      and updating the Pressed, MousePressed values) and call respective
      *Event method.

      Note that this means that OnKeyDown / OnKeyUp are always fired
      (this is contrary to the TGLUIWindow OnKeyDown / OnKeyUp behaviour,
      that is called only if no TKamOpenGLControl.Controls processed
      this key).

      The cleanest way to react to key / mouse events is to handle it in
      a specific TUIControl descendant. Overriding methods like KeyDown,
      KeyDownEvent, or using OnKeyDown event should be a last resort.

      @groupBegin }
    procedure KeyDownEvent(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUpEvent(var Key: Word; Shift: TShiftState); virtual;
    procedure MouseDownEvent(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); virtual;
    procedure MouseUpEvent(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); virtual;
    procedure MouseMoveEvent(Shift: TShiftState; NewX, NewY: Integer); virtual;
    { @groupEnd }

    { In this class this just calls OnGLContextOpen.

      Note that always after initializing OpenGL context, we also call
      Resize (OnResize event). And we call Invalidate
      (so at the first opportunity, Paint (with OnPaint,
      DoDraw (OnDraw), DoBeforeDraw (OnBeforeDraw), will also get called). }
    procedure DoGLContextOpen; virtual;

    { In this class this just calls OnGLContextClose. }
    procedure DoGLContextClose; virtual;

    property GLInitialized: boolean read FGLInitialized;

    procedure DoBeforeDraw; virtual;
    procedure DoDraw; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function MakeCurrent(SaveOldToStack: boolean = false): boolean; override;
    procedure Invalidate; override;
    procedure Paint; override;

    procedure Idle; virtual;

    property Pressed: TKeysPressed read FPressed;
    property MousePressed: KeysMouse.TMouseButtons read FMousePressed;
    procedure ReleaseAllKeysAndMouse;

    property MouseX: Integer read FMouseX;
    property MouseY: Integer read FMouseY;

    property Fps: TFramesPerSecond read FFps;

    { Place mouse cursor at NewMouseX and NewMouseY.
      Position is specified relative to this window's upper-top corner
      (more specifically, OpenGL area upper-top corner),
      just like MouseX and MouseY properties.

      Note that the actually set position may be different than requested,
      for example if part of the window is offscreen then
      window manager will probably refuse to move mouse cursor offscreen.

      This @italic(may) generate normal OnMouseMove event, just as if the
      user moved the mouse. But it's also allowed to not do this.

      Ignored when window is closed. }
    procedure SetMousePosition(const NewMouseX, NewMouseY: Integer);
  published
    { This will be called right after GL context
      will be initialized. }
    property OnGLContextOpen: TNotifyEvent
      read FOnGLContextOpen write FOnGLContextOpen;

    { This will be called right before GL context
      will be destroyed. }
    property OnGLContextClose: TNotifyEvent
      read FOnGLContextClose write FOnGLContextClose;

    property OnBeforeDraw: TNotifyEvent read FOnBeforeDraw write FOnBeforeDraw;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;

    { Force Idle and Paint (if invalidated) events to happen continously.

      You almost always want this to happen. Without this, when user "clogs"
      the GTK / WinAPI / Qt etc. event queue, Lazarus (LCL) doesn't continously
      fire the "Idle" events (used to update various state of our 3D world)
      and repaint events. This is somewhat tolerable for normal UI programs,
      that really "do" something only in response to user actions.
      But typical games / 3D simulations must try to update animations and
      repaint at a constant rate. Which means that we want "Idle" to be fired
      continously (not really only when application stays "idle"),
      and we want redraw to happen when needed (you signal the need to redraw
      by Invalidate call).

      The most visible usage of this is when using TWalkCamera.MouseLook.
      Walking with mouse look typically produces a continous stream
      of mouse move events, usually interspersed with key down events
      (since you usually press forward / back / strafe keys at the same
      time when looking around with mouse). Without AggressiveUpdate,
      this really works badly.

      So what does it do? We do not have the tools to hack Lazarus
      event control from the outside --- existing Application methods
      allow us to process a single "batch" of events, but this is too much
      (for example, may be ~ 100 GTK messages, see
      TGtkWidgetSet.AppProcessMessages in lazarus/trunk/lcl/interfaces/gtk/gtkwidgetset.inc).
      So instead we hack from the inside: from time to time
      (more precisely, after AggressiveUpdateGap miliseconds since last Idle + Paint end),
      when receving key or mouse events (KeyDown, MouseDown, MouseMove etc.),
      we'll call the Idle, and (if pending Invalidate call) Paint methods.

      Do not set too small, like 0, or you'll overload the system
      (you will see smooth animation and rendering, but there will be latency
      with respect to handling input, e.g. mouse move will be processed with
      a small delay).

      @groupBegin }
    property AggressiveUpdate: boolean
      read FAggressiveUpdate write FAggressiveUpdate default DefaultAggressiveUpdate;
    property AggressiveUpdateGap: TMilisecTime
      read FAggressiveUpdateGap write FAggressiveUpdateGap default DefaultAggressiveUpdateGap;
    { @groupEnd }

    property TabOrder;
    property TabStop default true;
  end;

  { OpenGL control, with extensions for "Kambi VRML game engine", including
    @link(Controls) list for TUIControl instances.

    Keeps a @link(Controls) list, so you can easily add TUIControl instances
    to this window (like TGLMenu, TKamSceneManager and more).
    We will pass events to these controls, draw them etc. }
  TKamOpenGLControl = class(TKamOpenGLControlCore, IUIContainer)
  private
    FControls: TUIControlList;
    FUseControls: boolean;
    FOnDrawStyle: TUIControlDrawStyle;
    FFocus: TUIControl;
    FTooltipDelay: TMilisecTime;
    FTooltipDistance: Cardinal;
    FTooltipVisible: boolean;
    FTooltipX, FTooltipY: Integer;
    LastPositionForTooltip: boolean;
    LastPositionForTooltipX, LastPositionForTooltipY: Integer;
    LastPositionForTooltipTime: TKamTimerResult;
    procedure ControlsVisibleChange(Sender: TObject);
    procedure SetUseControls(const Value: boolean);
    procedure UpdateFocusAndMouseCursor;
    function GetTooltipX: Integer;
    function GetTooltipY: Integer;
  protected
    procedure KeyDownEvent(var Key: Word; Shift: TShiftState); override;
    procedure KeyUpEvent(var Key: Word; Shift: TShiftState); override;
    procedure MouseDownEvent(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUpEvent(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMoveEvent(Shift: TShiftState; NewX, NewY: Integer); override;
    procedure DoBeforeDraw; override;
    procedure DoDraw; override;
    procedure Resize; override;
    procedure DoGLContextOpen; override;
    procedure DoGLContextClose; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Enable @link(Controls) list processing.

      @italic(Messing with this is very dangerous), that's why it's
      visibility is only protected (although could be even pubilshed, technically).
      This makes all controls miss all their events, including some critical
      notification events like TUIControl.GLContextOpen, TUIControl.GLContextClose,
      TUIControl.ContainerResize.

      You can reliably only turn this off temporarily, when you know that
      no events (or at least no meaningful events, like resize or control
      add/remove) will reach the window during this time. }
    property UseControls: boolean
      read FUseControls write SetUseControls default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Idle; override;

    { Returns the control that should receive input events first,
      or @nil if none. More precisely, this is the first on Controls
      list that is enabled and under the mouse cursor.
      @nil is returned when there's no enabled control under the mouse cursor,
      or when UseControls = @false. }
    property Focus: TUIControl read FFocus;

    { When the tooltip should be shown (mouse hovers over a control
      with a tooltip) then the TooltipVisible is set to @true,
      and TooltipX, TooltipY indicate left-bottom suggested position
      of the tooltip.

      The tooltip is only detected when TUIControl.TooltipStyle <> dsNone.
      See TUIControl.TooltipStyle and TUIControl.DrawTooltip.
      For simple purposes just set TKamGLFontControl.Tooltip to something
      non-empty.
      @groupBegin }
    property TooltipVisible: boolean read FTooltipVisible;
    property TooltipX: Integer read FTooltipX;
    property TooltipY: Integer read FTooltipY;
    { @groupEnd }

  published
    { How OnDraw callback fits within various Draw methods of our
      @link(Controls).
      See TGLUIWindow.OnDrawStyle for full description. }
    property OnDrawStyle: TUIControlDrawStyle
      read FOnDrawStyle write FOnDrawStyle default dsNone;

    { Controls listening for user input (keyboard / mouse) to this window.

      Usually you explicitly add / delete controls to this list.
      Also, freeing the control that is on this list
      automatically removes it from this list (using the TComponent.Notification
      mechanism). }
    property Controls: TUIControlList read FControls;

    property TooltipDelay: TMilisecTime read FTooltipDelay write FTooltipDelay
      default DefaultTooltipDelay;
    property TooltipDistance: Cardinal read FTooltipDistance write FTooltipDistance
      default DefaultTooltipDistance;
  end;

{ Convert Key (Lazarus key code) to my TKey.

  In addition, this tries to convert Key to a character (MyCharKey).
  It's awful that this function has to do convertion to Char,
  but that's the way of VCL and LCL: KeyPress and KeyDown
  are separate events. While I want to have them in one event,
  and passed as one event to TUIControl.KeyDown. }
procedure LKeyToMyKey(const Key: Word; Shift: TShiftState;
  out MyKey: TKey; out MyCharKey: char);

{ Convert Lazarus Controls.TMouseButton value to my KeysMouse.TMouseButton.

  (By coincidence, my type name and values are the same as used by LCL;
  but beware --- the order of values in my type is different (mbMiddle
  is in the middle in my type)). }
function LMouseButtonToMyMouseButton(
  const MouseButton: Controls.TMouseButton;
  out MyMouseButton: KeysMouse.TMouseButton): boolean;

procedure Register;

implementation

uses LCLType, GL, GLU, GLExt, KambiGLUtils, KambiStringUtils;

procedure Register;
begin
  RegisterComponents('Kambi', [TKamOpenGLControl]);
end;

{ TKamOpenGLControlCoreCore -------------------------------------------------- }

constructor TKamOpenGLControlCore.Create(AOwner: TComponent);
begin
  inherited;
  FFps := TFramesPerSecond.Create;
  FPressed := TKeysPressed.Create;

  FAggressiveUpdate := DefaultAggressiveUpdate;
  FAggressiveUpdateGap := DefaultAggressiveUpdateGap;
  LastAggressiveUpdateTime := 0;
  Invalidated := false;

  ApplicationProperties := TApplicationProperties.Create(Self);
  ApplicationProperties.OnIdle := @ApplicationPropertiesIdle;
end;

destructor TKamOpenGLControlCore.Destroy;
begin
  FreeAndNil(FPressed);
  FreeAndNil(FFps);
  inherited;
end;

procedure TKamOpenGLControlCore.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  Idle;
  Done := false;
end;

{ Initial idea was to do

procedure TKamOpenGLControlCore.CreateHandle;
begin
  Writeln('TKamOpenGLControlCore.CreateHandle ', GLInitialized,
    ' ', OnGLContextOpen <> nil);
  inherited CreateHandle;
  if not GLInitialized then
  begin
    GLInitialized := true;
    DoGLContextOpen;
  end;
  Writeln('TKamOpenGLControlCore.CreateHandle end');
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

function TKamOpenGLControlCore.MakeCurrent(SaveOldToStack: boolean): boolean;
begin
  Result := inherited MakeCurrent(SaveOldToStack);

  if not GLInitialized then
  begin
    FGLInitialized := true;
    DoGLContextOpen;

    Resize;
    { TODO: why it's not enough to call Resize; here?
      Long time ago, observed on Windows, later also on GTK 2.
      Reproducible e.g. with simple_3d_camera Lazarus demo. }
    if Assigned(OnResize) then OnResize(Self);

    Invalidate;
  end;
end;

procedure TKamOpenGLControlCore.Invalidate;
begin
  Invalidated := true; { will be actually used only when AggressiveUpdate }
  inherited;
end;

procedure TKamOpenGLControlCore.DestroyHandle;
begin
  if GLInitialized then
  begin
    DoGLContextClose;
    FGLInitialized := false;
  end;
  inherited DestroyHandle;
end;

procedure TKamOpenGLControlCore.DoGLContextOpen;
begin
  LoadAllExtensions;

  if Assigned(OnGLContextOpen) then
    OnGLContextOpen(Self);
end;

procedure TKamOpenGLControlCore.DoGLContextClose;
begin
  if Assigned(OnGLContextClose) then
    OnGLContextClose(Self);
end;

procedure TKamOpenGLControlCore.ReleaseAllKeysAndMouse;
begin
  Pressed.Clear;
  FMousePressed := [];
end;

procedure TKamOpenGLControlCore.AggressiveUpdateTick;
begin
  if AggressiveUpdate then
  begin
    if TimeTickSecondLater(LastAggressiveUpdateTime, GetTickCount, AggressiveUpdateGap) then
    begin
      Idle;
      if Invalidated then Paint;

      { We have to resist the temptation of optimizing below by reusing previous
        GetTickCount result here for speed. This could make our aggressive
        update overloading the event loop with repaints.
        Imagine that Idle + Paint would take > AggressiveUpdateGap
        (quite possible, if your scene is complex and you're constantly
        repainting, e.g. observed with mouse look walking + rotating on
        cubemap_with_dynamic_world.x3d). Then we would effectively repeat
        Idle + Paint in every event (like, on every MouseMove), making
        "lag" between painting and actualy processed events.

        True, this "overloading" is always possible with AggressiveUpdate
        anyway (by definition AggressiveUpdate does something non-optimal
        with events). But at least this way, AggressiveUpdateGap provides
        some working security against this overloading. }

      LastAggressiveUpdateTime := GetTickCount;
    end;
  end;
end;

procedure TKamOpenGLControlCore.UpdateShiftState(const Shift: TShiftState);
begin
  Pressed.Keys[K_Shift] := ssShift in Shift;
  Pressed.Keys[K_Alt  ] := ssAlt   in Shift;
  Pressed.Keys[K_Ctrl ] := ssCtrl  in Shift;
end;

procedure TKamOpenGLControlCore.KeyDownEvent(var Key: Word; Shift: TShiftState);
begin
end;

procedure TKamOpenGLControlCore.KeyUpEvent(var Key: Word; Shift: TShiftState);
begin
end;

procedure TKamOpenGLControlCore.MouseDownEvent(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TKamOpenGLControlCore.MouseUpEvent(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TKamOpenGLControlCore.MouseMoveEvent(Shift: TShiftState; NewX, NewY: Integer);
begin
end;

procedure TKamOpenGLControlCore.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
begin
  LKeyToMyKey(Key, Shift, MyKey, Ch);
  if (MyKey <> K_None) or (Ch <> #0) then
    Pressed.KeyDown(MyKey, Ch);

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  AggressiveUpdateTick;

  inherited KeyDown(Key, Shift);  { OnKeyDown before KeyDownEvent }

  KeyDownEvent(Key, Shift);
end;

procedure TKamOpenGLControlCore.KeyUp(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
begin
  LKeyToMyKey(Key, Shift, MyKey, Ch);
  if MyKey <> K_None then
    Pressed.KeyUp(MyKey, Ch);

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  AggressiveUpdateTick;

  inherited KeyUp(Key, Shift); { OnKeyUp before KeyUpEvent }

  KeyUpEvent(Key, Shift);
end;

procedure TKamOpenGLControlCore.MouseDown(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: KeysMouse.TMouseButton;
begin
  FMouseX := X;
  FMouseY := Y;

  if LMouseButtonToMyMouseButton(Button, MyButton) then
    Include(FMousePressed, MyButton);

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  AggressiveUpdateTick;

  inherited MouseDown(Button, Shift, X, Y); { OnMouseDown before MouseDownEvent }

  MouseDownEvent(Button, Shift, X, Y);
end;

procedure TKamOpenGLControlCore.MouseUp(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: KeysMouse.TMouseButton;
begin
  FMouseX := X;
  FMouseY := Y;

  if LMouseButtonToMyMouseButton(Button, MyButton) then
    Exclude(FMousePressed, MyButton);

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  AggressiveUpdateTick;

  inherited MouseUp(Button, Shift, X, Y); { OnMouseUp before MouseUpEvent }

  MouseUpEvent(Button, Shift, X, Y);
end;

procedure TKamOpenGLControlCore.MouseMove(Shift: TShiftState; NewX, NewY: Integer);
begin
  MouseMoveEvent(Shift, NewX, NewY);

  { OnMouseMove after MouseMoveEvent (MouseX, MouseY must be old values
    inside MouseMoveEvent) }
  FMouseX := NewX;
  FMouseY := NewY;

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  AggressiveUpdateTick;

  inherited MouseMove(Shift, NewX, NewY);
end;

procedure TKamOpenGLControlCore.Idle;
begin
  Fps._IdleBegin;
end;

procedure TKamOpenGLControlCore.DoExit;
begin
  inherited;
  ReleaseAllKeysAndMouse;
end;

procedure TKamOpenGLControlCore.DoBeforeDraw;
begin
  if Assigned(OnBeforeDraw) then
    OnBeforeDraw(Self);
end;

procedure TKamOpenGLControlCore.DoDraw;
begin
  if Assigned(OnDraw) then
    OnDraw(Self);
end;

procedure TKamOpenGLControlCore.Paint;
begin
  { Note that we don't call here inherited, instead doing everything ourselves. }
  if MakeCurrent then
  begin
    DoBeforeDraw;
    Fps._RenderBegin;
    try
      DoDraw;
      SwapBuffers;
    finally Fps._RenderEnd end;
    Invalidated := false; { used only when AggressiveUpdate }
  end;
end;

procedure TKamOpenGLControlCore.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  Mouse.CursorPos := ControlToScreen(Point(NewMouseX, NewMouseY));
end;

function TKamOpenGLControlCore.GetMouseX: Integer;
begin
  Result := FMouseX;
end;

function TKamOpenGLControlCore.GetMouseY: Integer;
begin
  Result := FMouseY;
end;

function TKamOpenGLControlCore.GetWidth: Integer;
begin
  Result := Width;
end;

function TKamOpenGLControlCore.GetHeight: Integer;
begin
  Result := Height;
end;

function TKamOpenGLControlCore.GetMousePressed: TMouseButtons;
begin
  Result := FMousePressed;
end;

function TKamOpenGLControlCore.GetPressed: TKeysPressed;
begin
  Result := FPressed;
end;

{ TControlledUIControlList ----------------------------------------------------- }

type
  { TUIControlList descendant that takes care to react to list add/remove
    notifications, doing appropriate operations with parent Container. }
  TControlledUIControlList = class(TUIControlList)
  private
    Container: TKamOpenGLControl;
  public
    constructor Create(const FreeObjects: boolean; const AContainer: TKamOpenGLControl);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

constructor TControlledUIControlList.Create(const FreeObjects: boolean;
  const AContainer: TKamOpenGLControl);
begin
  inherited Create(FreeObjects);
  Container := AContainer;
end;

procedure TControlledUIControlList.Notify(Ptr: Pointer; Action: TListNotification);
var
  C: TUIControl absolute Ptr;
begin
  C := TUIControl(Ptr);
  case Action of
    lnAdded:
      begin
        { Make sure Container.ControlsVisibleChange will be called
          when a control calls OnVisibleChange. }
        if C.OnVisibleChange = nil then
          C.OnVisibleChange := @Container.ControlsVisibleChange;

        { Register Container to be notified of control destruction. }
        C.FreeNotification(Container);

        C.Container := Container;

        { Call initial ContainerResize for control.
          If Container OpenGL context is not yet initialized, defer it to
          the Init time, then our initial EventResize will be called
          that will do ContainerResize on every control. }
        if Container.GLInitialized then
        begin
          C.GLContextOpen;
          C.ContainerResize(Container.Width, Container.Height);
        end;
      end;
    lnExtracted, lnDeleted:
      begin
        if Container.GLInitialized then
          C.GLContextClose;

        if C.OnVisibleChange = @Container.ControlsVisibleChange then
          C.OnVisibleChange := nil;

        C.RemoveFreeNotification(Container);

        C.Container := nil;
      end;
    else raise EInternalError.Create('TControlledUIControlList.Notify action?');
  end;

  if Container.FControls <> nil then
    Container.UpdateFocusAndMouseCursor;
end;

{ TKamOpenGLControl --------------------------------------------------------- }

constructor TKamOpenGLControl.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := true;
  FControls := TControlledUIControlList.Create(false, Self);
  FUseControls := true;
  FOnDrawStyle := dsNone;
  FTooltipDelay := DefaultTooltipDelay;
  FTooltipDistance := DefaultTooltipDistance;
end;

destructor TKamOpenGLControl.Destroy;
begin
  FreeAndNil(FControls);
  inherited;
end;

procedure TKamOpenGLControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  { We have to remove a reference to the object from Controls list.
    This is crucial: TControlledUIControlList.Notify,
    and some Controls.MakeSingle calls, assume that all objects on
    the Controls list are always valid objects (no invalid references,
    even for a short time). }
  if (Operation = opRemove) and (AComponent is TUIControl) then
  begin
    Controls.DeleteAll(AComponent);
    if AComponent = FFocus then FFocus := nil;
  end;
end;

procedure TKamOpenGLControl.UpdateFocusAndMouseCursor;

  function CalculateFocus: TUIControl;
  var
    I: Integer;
  begin
    if not UseControls then Exit(nil);

    for I := 0 to Controls.Count - 1 do
    begin
      Result := Controls[I];
      if Result.PositionInside(MouseX, MouseY) then
        Exit;
    end;

    Result := nil;
  end;

  function CalculateMouseCursor: TMouseCursor;
  begin
    if Focus <> nil then
      Result := Focus.Cursor else
      Result := mcDefault;
  end;

const
  MyCursorToLazCursor: array [TMouseCursor] of TCursor =
  ( crDefault, crNone, crDefault { mcCustom treat like mcDefault },
    crArrow, crHourGlass, crIBeam, crHandPoint );

var
  NewFocus: TUIControl;
  NewCursor: TCursor;
begin
  NewFocus := CalculateFocus;

  if NewFocus <> Focus then
  begin
    if (Focus <> nil) and UseControls then Focus.Focused := false;
    FFocus := NewFocus;
    { No need to check UseControls above: if Focus <> nil then we know
      UseControls was true during CalculateFocus. }
    if (Focus <> nil) then Focus.Focused := true;
  end;

  NewCursor := MyCursorToLazCursor[CalculateMouseCursor];
  { check explicitly "Cursor <> NewCursor" --- we will call UpdateFocusAndMouseCursor
    very often (in each mouse move), and we don't want to depend on Lazarus
    optimizing "Cursor := Cursor" to avoid some potentially expensive window
    manager call. }
  if Cursor <> NewCursor then
    Cursor := NewCursor;
end;

procedure TKamOpenGLControl.Idle;

  procedure UpdateTooltip;
  var
    T: TKamTimerResult;
    NewTooltipVisible: boolean;
  begin
    { Update TooltipVisible and LastPositionForTooltip*.
      Idea is that user must move the mouse very slowly to activate tooltip. }

    T := Fps.IdleStartTime;
    if (not LastPositionForTooltip) or
       (Sqr(LastPositionForTooltipX - MouseX) +
        Sqr(LastPositionForTooltipY - MouseY) > Sqr(TooltipDistance)) then
    begin
      LastPositionForTooltip := true;
      LastPositionForTooltipX := MouseX;
      LastPositionForTooltipY := MouseY;
      LastPositionForTooltipTime := T;
      NewTooltipVisible := false;
    end else
      NewTooltipVisible :=
        { make TooltipVisible only when we're over a control that has
          focus. This avoids unnecessary changing of TooltipVisible
          (and related PostRedisplay) when there's no tooltip possible. }
        (Focus <> nil) and
        (Focus.TooltipStyle <> dsNone) and
        ( (1000 * (T - LastPositionForTooltipTime)) div
          KamTimerFrequency > TooltipDelay );

    if FTooltipVisible <> NewTooltipVisible then
    begin
      FTooltipVisible := NewTooltipVisible;

      if TooltipVisible then
      begin
        { when setting TooltipVisible from false to true,
          update LastPositionForTooltipX/Y. We don't want to hide the tooltip
          at the slightest jiggle of the mouse :) On the other hand,
          we don't want to update LastPositionForTooltipX/Y more often,
          as it would disable the purpose of TooltipDistance: faster
          mouse movement should hide the tooltip. }
        LastPositionForTooltipX := MouseX;
        LastPositionForTooltipY := MouseY;
        { also update TooltipX/Y }
        FTooltipX := MouseX;
        FTooltipY := MouseY;
      end;

      Invalidate;
    end;
  end;

var
  I: Integer;
  C: TUIControl;
  HandleMouseAndKeys: boolean;
  Dummy: boolean;
begin
  if UseControls then
  begin
    UpdateTooltip;

    { Although we call Idle for all the controls, we look
      at PositionInside and track HandleMouseAndKeys values.
      See TUIControl.Idle for explanation. }

    HandleMouseAndKeys := true;

    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if HandleMouseAndKeys and C.PositionInside(MouseX, MouseY) then
      begin
        HandleMouseAndKeys := not C.ExclusiveEvents;
        C.Idle(Fps.IdleSpeed, true, HandleMouseAndKeys);
      end else
      begin
        Dummy := not C.ExclusiveEvents;
        C.Idle(Fps.IdleSpeed, false, Dummy);
      end;
    end;
  end;

  inherited;
end;

procedure TKamOpenGLControl.KeyDownEvent(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
  C: TUIControl;
  I: Integer;
begin
  LKeyToMyKey(Key, Shift, MyKey, Ch);

  if (MyKey <> K_None) or (Ch <> #0) and UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.KeyDown(Key, Ch) then
        begin
          Key := 0;
          Exit;
        end;
    end;
  end;

  inherited;
end;

procedure TKamOpenGLControl.KeyUpEvent(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
  C: TUIControl;
  I: Integer;
begin
  LKeyToMyKey(Key, Shift, MyKey, Ch);

  if (MyKey <> K_None) or (Ch <> #0) and UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.KeyUp(Key, Ch) then
        begin
          Key := 0;
          Exit;
        end;
    end;
  end;

  inherited;
end;

procedure TKamOpenGLControl.MouseDownEvent(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: KeysMouse.TMouseButton;
  C: TUIControl;
  I: Integer;
begin
  if LMouseButtonToMyMouseButton(Button, MyButton) and UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.MouseDown(MyButton) then
          Exit;
    end;
  end;

  inherited;
end;

procedure TKamOpenGLControl.MouseUpEvent(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: KeysMouse.TMouseButton;
  C: TUIControl;
  I: Integer;
begin
  if LMouseButtonToMyMouseButton(Button, MyButton) and UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.MouseUp(MyButton) then
          Exit;
    end;
  end;

  inherited;
end;

procedure TKamOpenGLControl.MouseMoveEvent(Shift: TShiftState; NewX, NewY: Integer);
var
  C: TUIControl;
  I: Integer;
begin
  UpdateFocusAndMouseCursor;

  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.MouseMove(MouseX, MouseY, NewX, NewY) then Exit;
    end;
  end;

  inherited;
end;

procedure TKamOpenGLControl.ControlsVisibleChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TKamOpenGLControl.DoBeforeDraw;
var
  I: Integer;
begin
  inherited;

  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].BeforeDraw;
  end;
end;

procedure TKamOpenGLControl.DoDraw;

  { Call Draw for all controls having DrawStyle = ds3D.

    Also (since we call DrawStyle for everything anyway)
    calculates AnythingWants2D = if any control returned DrawStyle = ds2D.
    If not, you can later avoid even changing projection to 2D. }
  procedure Draw3D(out AnythingWants2D: boolean);
  var
    I: Integer;
    C: TUIControl;
  begin
    AnythingWants2D := false;

    if UseControls then
    begin
      { draw controls in "downto" order, back to front }
      for I := Controls.Count - 1 downto 0 do
      begin
        C := Controls[I];
        case C.DrawStyle of
          ds2D: AnythingWants2D := true;
          { Set OpenGL state that may be changed carelessly, and has some
            guanteed value, for TUIControl.Draw calls. }
          ds3D: begin glLoadIdentity; C.Draw; end;
        end;
      end;

      if TooltipVisible and (Focus <> nil) then
        case Focus.TooltipStyle of
          ds2D: AnythingWants2D := true;
          ds3D: begin glLoadIdentity; Focus.DrawTooltip; end;
        end;
    end;

    case OnDrawStyle of
      ds2D: AnythingWants2D := true;
      ds3D: begin glLoadIdentity; inherited DoDraw; end;
    end;
  end;

  procedure Draw2D;
  var
    C: TUIControl;
    I: Integer;
  begin
    glPushAttrib(GL_ENABLE_BIT);
      { Set and push/pop OpenGL state that is guaranteed for Draw2D calls,
        but TUIControl.Draw cannot change it carelessly. }
      glDisable(GL_LIGHTING);
      glDisable(GL_DEPTH_TEST);
      glDisable(GL_TEXTURE_2D);
      if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
      if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);

      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      gluOrtho2D(0, Width, 0, Height);
      glMatrixMode(GL_MODELVIEW);
      try

        if UseControls then
        begin
          { draw controls in "downto" order, back to front }
          for I := Controls.Count - 1 downto 0 do
          begin
            C := Controls[I];

            if C.DrawStyle = ds2D then
            begin
              { Set OpenGL state that may be changed carelessly, and has some
                guanteed value, for Draw2d calls. }
              glLoadIdentity;
              glRasterPos2i(0, 0);
              C.Draw;
            end;
          end;

          if TooltipVisible and (Focus <> nil) and (Focus.TooltipStyle = ds2D) then
          begin
            glLoadIdentity;
            glRasterPos2i(0, 0);
            Focus.DrawTooltip;
          end;
        end;

        if OnDrawStyle = ds2D then
        begin
          glLoadIdentity;
          glRasterPos2i(0, 0);
          inherited DoDraw;
        end;

      finally
        glMatrixMode(GL_PROJECTION);
        glPopMatrix;
        glMatrixMode(GL_MODELVIEW);
      end;
    glPopAttrib;
  end;

var
  AnythingWants2D: boolean;
begin
  Draw3D(AnythingWants2D);

  if AnythingWants2D then
    Draw2D;

  if OnDrawStyle = dsNone then
    inherited;
end;

procedure TKamOpenGLControl.Resize;
var
  I: Integer;
begin
  inherited;

  { Call MakeCurrent here, to make sure UIControls always get
    ContainerResize with good GL context. }
  if GLInitialized and UseControls and MakeCurrent then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].ContainerResize(Width, Height);
  end;
end;

procedure TKamOpenGLControl.DoGLContextOpen;
var
  I: Integer;
begin
  inherited;

  { call GLContextOpen on controls after inherited (OnGLContextOpen). }
  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].GLContextOpen;
  end;
end;


procedure TKamOpenGLControl.DoGLContextClose;
var
  I: Integer;
begin
  { call GLContextClose on controls before inherited (OnGLContextClose).
    This may be called from Close, which may be called from TGLWindow destructor,
    so prepare for Controls being possibly nil now. }
  if UseControls and (Controls <> nil) then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].GLContextClose;
  end;

  inherited;
end;

procedure TKamOpenGLControl.SetUseControls(const Value: boolean);
begin
  if Value <> UseControls then
  begin
    FUseControls := Value;
    { Focus must always be @nil when UseControls = false }
    UpdateFocusAndMouseCursor;
  end;
end;

function TKamOpenGLControl.GetTooltipX: Integer;
begin
  Result := FTooltipX;
end;

function TKamOpenGLControl.GetTooltipY: Integer;
begin
  Result := FTooltipY;
end;

{ global routines ------------------------------------------------------------ }

procedure LKeyToMyKey(const Key: Word; Shift: TShiftState;
  out MyKey: TKey; out MyCharKey: char);
begin
  MyKey := K_None;
  MyCharKey := #0;

  case Key of
    VK_BACK: MyKey := K_BackSpace;
    VK_TAB: MyKey := K_Tab;
    VK_RETURN: MyKey := K_Enter;
    VK_SHIFT: MyKey := K_Shift;
    VK_CONTROL: MyKey := K_Ctrl;
    VK_MENU: MyKey := K_Alt;
    VK_ESCAPE: MyKey := K_Escape;
    VK_SPACE: MyKey := K_Space;
    VK_PRIOR: MyKey := K_PageUp;
    VK_NEXT: MyKey := K_PageDown;
    VK_END: MyKey := K_End;
    VK_HOME: MyKey := K_Home;
    VK_LEFT: MyKey := K_Left;
    VK_UP: MyKey := K_Up;
    VK_RIGHT: MyKey := K_Right;
    VK_DOWN: MyKey := K_Down;
    VK_INSERT: MyKey := K_Insert;
    VK_DELETE: MyKey := K_Delete;
    VK_ADD: MyKey := K_Numpad_Plus;
    VK_SUBTRACT: MyKey := K_Numpad_Minus;
    VK_SNAPSHOT: MyKey := K_PrintScreen;
    VK_NUMLOCK: MyKey := K_NumLock;
    VK_SCROLL: MyKey := K_ScrollLock;
    VK_CAPITAL: MyKey := K_CapsLock;
    VK_PAUSE: MyKey := K_Pause;
    VK_OEM_COMMA: MyKey := K_Comma;
    VK_OEM_PERIOD: MyKey := K_Period;
    VK_NUMPAD0: MyKey := K_Numpad_0;
    VK_NUMPAD1: MyKey := K_Numpad_1;
    VK_NUMPAD2: MyKey := K_Numpad_2;
    VK_NUMPAD3: MyKey := K_Numpad_3;
    VK_NUMPAD4: MyKey := K_Numpad_4;
    VK_NUMPAD5: MyKey := K_Numpad_5;
    VK_NUMPAD6: MyKey := K_Numpad_6;
    VK_NUMPAD7: MyKey := K_Numpad_7;
    VK_NUMPAD8: MyKey := K_Numpad_8;
    VK_NUMPAD9: MyKey := K_Numpad_9;
    VK_CLEAR: MyKey := K_Numpad_Begin;
    VK_MULTIPLY: MyKey := K_Numpad_Multiply;
    VK_DIVIDE: MyKey := K_Numpad_Divide;
    VK_OEM_MINUS: MyKey := K_Minus;
    VK_OEM_PLUS: MyKey := K_Equal;

    Ord('0') .. Ord('9'):
      begin
        MyKey := K_0  + Key - Ord('0');
        MyCharKey := Chr(Key);
      end;

    Ord('A') .. Ord('Z'):
      begin
        MyKey := K_A  + Key - Ord('A');
        MyCharKey := Chr(Key);
        if not (ssShift in Shift) then
          MyCharKey := LoCase(MyCharKey);
      end;

    VK_F1 .. VK_F12  : MyKey := K_F1 + Key - VK_F1;
  end;
end;

function LMouseButtonToMyMouseButton(
  const MouseButton: Controls.TMouseButton;
  out MyMouseButton: KeysMouse.TMouseButton): boolean;
begin
  Result := true;
  case MouseButton of
    Controls.mbLeft  : MyMouseButton := KeysMouse.mbLeft;
    Controls.mbRight : MyMouseButton := KeysMouse.mbRight;
    Controls.mbMiddle: MyMouseButton := KeysMouse.mbMiddle;
    else Result := false;
  end;
end;

initialization
end.

