unit KambiGLControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenGLContext, Navigation, Controls, Forms,
  VectorMath, KeysMouse, KambiUtils, KambiTimeUtils, StdCtrls, UIControls;

type
  { OpenGL control, with a couple of extensions for Kambi VRML game engine.
    You will usually prefer to use TKamOpenGLControl instead of directly this
    class, TKamOpenGLControl adds some very useful features like
    @link(TKamOpenGLControl.Navigator), @link(TKamOpenGLControl.Controls).

    Provides OnGLContextInit and OnGLContextClose events.

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
    FContextInitialized: boolean;

    FOnGLContextInit: TNotifyEvent;
    FOnGLContextClose: TNotifyEvent;

    FFps: TFramesPerSecond;

    ApplicationProperties: TApplicationProperties;
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
  protected
    procedure DestroyHandle; override;
    procedure DoExit; override;
    procedure Paint; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Integer); override;

    { In this class this just calls OnGLContextInit. }
    procedure DoGLContextInit; virtual;

    { In this class this just calls OnGLContextClose. }
    procedure DoGLContextClose; virtual;

    property ContextInitialized: boolean read FContextInitialized;

    procedure DoBeforeDraw; virtual;
    procedure DoDraw; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function MakeCurrent(SaveOldToStack: boolean = false): boolean; override;

    procedure Idle; virtual;

    KeysDown: TKeysBooleans;
    MousePressed: KeysMouse.TMouseButtons;
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
    property OnGLContextInit: TNotifyEvent
      read FOnGLContextInit write FOnGLContextInit;

    { This will be called right before GL context
      will be destroyed. }
    property OnGLContextClose: TNotifyEvent
      read FOnGLContextClose write FOnGLContextClose;

     property OnBeforeDraw: TNotifyEvent read FOnBeforeDraw write FOnBeforeDraw;
     property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
  end;

  { OpenGL control, with extensions for Kambi VRML game engine, including
    @link(Controls) and @link(Navigator) properties.

    Keeps a @link(Controls) list, so you can easily add TUIControl instances
    to this window (like navigators (TExamineNavigator, TWalkNavigator),
    TGLMenu and more). We will pass events to these controls, draw them etc.,
    everything only if UseControls = @true. See TKamOpenGLControl for more
    detailed documentation how @link(Controls) are treated. }
  TKamOpenGLControl = class(TKamOpenGLControlCore, IUIContainer)
  private
    FControls: TUIControlList;
    FCursorNonMouseLook: TCursor;
    FUseControls: boolean;
    FNavigator: TNavigator;

    procedure SetCursorNonMouseLook(const Value: TCursor);
    procedure SetNavigator(const Value: TNavigator);
    function ReallyUseMouseLook: boolean;
    procedure ControlsVisibleChange(Sender: TObject);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Integer); override;
    procedure DoDraw; override;
    procedure Resize; override;
    procedure DoGLContextClose; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Idle; override;

    { Controls listening for user input (keyboard / mouse) to this window.

      Usually you explicitly add / delete controls to this list.
      Also, freeing the control that is on this list (Navigator or not)
      automatically removes it from this list (using the TComponent.Notification
      mechanism). }
    property Controls: TUIControlList read FControls;

    property UseControls: boolean
      read FUseControls write FUseControls default true;

    { Returns the control that should receive input events first,
      or @nil if none. More precisely, this is the first on Controls
      list that is enabled and under the mouse cursor.
      @nil is returned when there's no enabled control under the mouse cursor,
      or when UseControls = @false. }
    function Focus: TUIControl;

    { Navigator instance used. Initially it's nil.
      Set this to give user a method for navigating in 3D scene.

      When assigning navigator instance we'll take care to make it
      the one and only one TNavigator instance on Controls list.
      Assigning here @nil removes it from Controls list.

      For now, you should not add / remove TNavigator instances to
      the Controls list directly. }
    property Navigator: TNavigator
      read FNavigator write SetNavigator;

    { These are shortcuts for writing
      TExamineNavigator(Navigator) and TWalkNavigator(Navigator).
      In DEBUG version they use operator "as" but in RELEASE
      version they use direct type-casts for speed.

      @groupBegin }
    function ExamineNav: TExamineNavigator;
    function WalkNav: TWalkNavigator;
    { @groupEnd }

    { Calculate a ray picked by WindowX, WindowY position on the window.
      Use this only when Navigator <> nil.

      ViewAngleDegX, ViewAngleDegY are your camera view angles.

      WindowX, WindowY are given in the same style as MouseX, MouseY:
      WindowX = 0 is left, WindowY = 0 is top.

      This uses @link(PrimaryRay) call. }
    procedure Ray(const WindowX, WindowY: Integer;
      const ViewAngleDegX, ViewAngleDegY: Single;
      out Ray0, RayVector: TVector3Single);

    procedure UpdateMouseLook;

    property CursorNonMouseLook: TCursor
      read FCursorNonMouseLook write SetCursorNonMouseLook
      default crDefault;
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

uses LCLType, RaysWindow, GL, GLU, GLExt, KambiGLUtils, KambiStringUtils;

procedure Register;
begin
  RegisterComponents('Kambi', [TKamOpenGLControl]);
end;

{ TKamOpenGLControlCoreCore -------------------------------------------------- }

constructor TKamOpenGLControlCore.Create(AOwner: TComponent);
begin
  inherited;
  FFps := TFramesPerSecond.Create;

  ApplicationProperties := TApplicationProperties.Create(Self);
  ApplicationProperties.OnIdle := @ApplicationPropertiesIdle;
end;

destructor TKamOpenGLControlCore.Destroy;
begin
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
  Writeln('TKamOpenGLControlCore.CreateHandle ', ContextInitialized,
    ' ', OnGLContextInit <> nil);
  inherited CreateHandle;
  if not ContextInitialized then
  begin
    ContextInitialized := true;
    DoGLContextInit;
  end;
  Writeln('TKamOpenGLControlCore.CreateHandle end');
end;

Reasoning: looking at implementation of OpenGLContext,
actual creating and destroying of OpenGL contexts
(i.e. calls to LOpenGLCreateContext and LOpenGLDestroyContextInfo)
is done within Create/DesrtoyHandle.

Why this was wrong ? Because under GTK LOpenGLCreateContext
only creates gtk_gl_area --- it doesn't *realize* it yet !
Which means that actually LOpenGLCreateContext doesn't create
OpenGL context. Looking at implementation of GLGtkGlxContext
we see that only during MakeCurrent the widget is guaranteed
to be realized. }

function TKamOpenGLControlCore.MakeCurrent(SaveOldToStack: boolean): boolean;
begin
  Result := inherited MakeCurrent(SaveOldToStack);

  if not ContextInitialized then
  begin
    FContextInitialized := true;
    DoGLContextInit;
  end;
end;

procedure TKamOpenGLControlCore.DestroyHandle;
begin
  if ContextInitialized then
  begin
    DoGLContextClose;
    FContextInitialized := false;
  end;
  inherited DestroyHandle;
end;

procedure TKamOpenGLControlCore.DoGLContextInit;
begin
  LoadAllExtensions;

  if Assigned(OnGLContextInit) then
    OnGLContextInit(Self);
end;

procedure TKamOpenGLControlCore.DoGLContextClose;
begin
  if Assigned(OnGLContextClose) then
    OnGLContextClose(Self);
end;

procedure TKamOpenGLControlCore.ReleaseAllKeysAndMouse;
begin
  FillChar(KeysDown, SizeOf(KeysDown), 0);
  MousePressed := [];
end;

procedure TKamOpenGLControlCore.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
begin
  inherited KeyDown(Key, Shift);

  LKeyToMyKey(Key, Shift, MyKey, Ch);

  { Tests: Writeln('Key down : ', KeyToStr(MyKey),
    ' ', Ord(Ch), ' ', Ch); }

  if MyKey <> K_None then
    KeysDown[MyKey] := true;
end;

procedure TKamOpenGLControlCore.KeyUp(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  Ch: char;
begin
  inherited KeyUp(Key, Shift);

  LKeyToMyKey(Key, Shift, MyKey, Ch);

  { Tests: Writeln('Key up : ', KeyToStr(MyKey),
    ' ', Ord(Ch), ' ', Ch); }

  if MyKey <> K_None then
    KeysDown[MyKey] := false;
end;

procedure TKamOpenGLControlCore.MouseDown(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: KeysMouse.TMouseButton;
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseX := X;
  FMouseY := Y;

  if LMouseButtonToMyMouseButton(Button, MyButton) then
  begin
    Include(MousePressed, MyButton);
  end;
end;

procedure TKamOpenGLControlCore.MouseUp(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: KeysMouse.TMouseButton;
begin
  inherited MouseUp(Button, Shift, X, Y);

  FMouseX := X;
  FMouseY := Y;

  if LMouseButtonToMyMouseButton(Button, MyButton) then
  begin
    Exclude(MousePressed, MyButton);
  end;
end;

procedure TKamOpenGLControlCore.MouseMove(Shift: TShiftState; NewX, NewY: Integer);
begin
  inherited;

  FMouseX := NewX;
  FMouseY := NewY;
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
  DoBeforeDraw;
  Fps._RenderBegin;
  try
    DoDraw;
    SwapBuffers;
  finally Fps._RenderEnd end;
end;

procedure TKamOpenGLControlCore.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  { TODO }
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

        { Call initial ContainerResize for control.
          If Container OpenGL context is not yet initialized, defer it to
          the Init time, then our initial EventResize will be called
          that will do ContainerResize on every control. }
        if Container.ContextInitialized then
          C.ContainerResize(Container.Width, Container.Height);

        { Register Container to be notified of control destruction. }
        C.FreeNotification(Container);

        C.Container := Container;
      end;
    lnExtracted, lnDeleted:
      begin
        if C.OnVisibleChange = @Container.ControlsVisibleChange then
          C.OnVisibleChange := nil;

        C.RemoveFreeNotification(Container);

        C.Container := nil;
      end;
    else raise EInternalError.Create('TControlledUIControlList.Notify action?');
  end;
end;

{ TKamOpenGLControl --------------------------------------------------------- }

constructor TKamOpenGLControl.Create(AOwner: TComponent);
begin
  inherited;
  FControls := TControlledUIControlList.Create(false, Self);
  FUseControls := true;
end;

destructor TKamOpenGLControl.Destroy;
begin
  FreeAndNil(FControls);
  inherited;
end;

procedure TKamOpenGLControl.SetNavigator(const Value: TNavigator);
begin
  if FNavigator <> Value then
  begin
    FNavigator := Value;
    { replace / add at the end of Controls current Navigator }
    Controls.MakeSingle(TNavigator, Value);
  end;
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
    if AComponent = FNavigator then
      FNavigator := nil;
  end;
end;

function TKamOpenGLControl.Focus: TUIControl;
var
  I: Integer;
begin
  if not UseControls then Exit(nil);

  for I := 0 to Controls.Count - 1 do
  begin
    Result := Controls.Items[I];
    if Result.PositionInside(MouseX, MouseY) then
      Exit;
  end;

  Result := nil;
end;

procedure TKamOpenGLControl.Idle;
var
  I: Integer;
  ThisListener, F: TUIControl;
begin
  { Although we call Idle for all listeners (and navigators),
    we will pass pressed keys/mouse buttons info only for the "focused"
    listener (or navigator, if no listener focused).

    This makes sense: if you want to check in Idle some pressed state,
    and react to it, you do not want many listeners to react to your
    keys. For example, when pressing key left over TGLMenu, you do not
    want to let navigator to also capture this left key down. }
  F := Focus;

  if UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
    begin
      ThisListener := Controls.Items[I];
      if F = ThisListener then
        ThisListener.Idle(Fps.IdleSpeed, @KeysDown, {TODO:@CharactersDown}nil, MousePressed) else
        ThisListener.Idle(Fps.IdleSpeed, nil, nil, []);
    end;
  end;

  inherited;
end;

procedure TKamOpenGLControl.KeyDown(var Key: Word; Shift: TShiftState);
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
      C := Controls.Items[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.KeyDown(Key, Ch, @KeysDown) then begin Key := 0; Exit; end;
    end;
  end;

  { TODO: do not block inherited so easily, so part of "inherited" must be
    always done }

  inherited;
end;

procedure TKamOpenGLControl.MouseDown(Button: Controls.TMouseButton;
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
      C := Controls.Items[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.MouseDown(MouseX, MouseY, MyButton, MousePressed) then Exit;
    end;
  end;

  { TODO: do not block inherited so easily, so part of "inherited" must be
    always done }

  inherited;
end;

procedure TKamOpenGLControl.MouseUp(Button: Controls.TMouseButton;
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
      C := Controls.Items[I];
      if C.PositionInside(MouseX, MouseY) then
        if C.MouseUp(MouseX, MouseY, MyButton, MousePressed) then Exit;
    end;
  end;

  { TODO: do not block inherited so easily, so part of "inherited" must be
    always done }

  inherited;
end;

function TKamOpenGLControl.ExamineNav: TExamineNavigator;
begin
  Result :=
    {$ifdef DEBUG} Navigator as TExamineNavigator
    {$else} TExamineNavigator(Navigator)
    {$endif};
end;

function TKamOpenGLControl.WalkNav: TWalkNavigator;
begin
  Result :=
    {$ifdef DEBUG} Navigator as TWalkNavigator
    {$else} TWalkNavigator(Navigator)
    {$endif};
end;

procedure TKamOpenGLControl.Ray(const WindowX, WindowY: Integer;
  const ViewAngleDegX, ViewAngleDegY: Single;
  out Ray0, RayVector: TVector3Single);
var
  Dir, Up: TVector3Single;
begin
  Navigator.GetCameraVectors(Ray0, Dir, Up);
  RayVector := PrimaryRay(WindowX, Height - WindowY,
    Width, Height,
    Ray0, Dir, Up,
    ViewAngleDegX, ViewAngleDegY);
end;

function TKamOpenGLControl.ReallyUseMouseLook: boolean;
begin
  Result := (Navigator <> nil) and Navigator.MouseLook;
end;

procedure TKamOpenGLControl.SetCursorNonMouseLook(
  const Value: TCursor);
begin
  if Value <> FCursorNonMouseLook then
  begin
    FCursorNonMouseLook := Value;
    if not ReallyUseMouseLook then
      Cursor := CursorNonMouseLook;
  end;
end;

procedure TKamOpenGLControl.UpdateMouseLook;
var
  ML: boolean;
begin
  ML := ReallyUseMouseLook;
  if ML then
    Cursor := crNone else
    Cursor := CursorNonMouseLook;
  if ML then
    SetMousePosition(Width div 2, Height div 2);
end;

procedure TKamOpenGLControl.MouseMove(Shift: TShiftState; NewX, NewY: Integer);
var
  MiddleScreenWidth: Integer;
  MiddleScreenHeight: Integer;
  F: TUIControl;
  Handled: boolean;
begin
  Handled := false;

  F := Focus; { control in Focus is always Enabled, no need to check it }
  if F <> nil then
  begin
    { Handling mouse look = true requires special treatment here. }
    if F.MouseLook then
    begin
      MiddleScreenWidth := Width div 2;
      MiddleScreenHeight := Height div 2;

      { Reasoning of  this check: see TKamOpenGLControl analogous comments. }
      if (MouseX = MiddleScreenWidth) and
         (MouseY = MiddleScreenHeight) then
        Handled := F.MouseMove(
          MouseX, MouseY, NewX, NewY, MousePressed, @KeysDown);

      { I check the condition below to avoid calling SetMousePosition,
        OnMouseMove, SetMousePosition, OnMouseMove... in a loop.
        Not really likely (as messages will be queued, and some
        SetMousePosition will finally just not generate OnMouseMove),
        but I want to safeguard anyway. }
      if (NewX <> MiddleScreenWidth) or (NewY <> MiddleScreenHeight) then
        SetMousePosition(MiddleScreenWidth, MiddleScreenHeight);
    end else
    begin
      Handled := F.MouseMove(MouseX, MouseY, NewX, NewY, MousePressed, @KeysDown);
    end;
  end;

  if Handled then Exit;

  inherited;
end;

procedure TKamOpenGLControl.ControlsVisibleChange(Sender: TObject);
begin
  Invalidate;
end;

procedure WindowDraw2D(GLWinPtr: Pointer);
var
  GLWin: TKamOpenGLControl absolute GLWinPtr;
  C, F: TUIControl;
  I: Integer;
begin
  F := GLWin.Focus;
  { draw controls in "downto" order, back to front }
  for I := GLWin.Controls.Count - 1 downto 0 do
  begin
    C := GLWin.Controls[I];

    if C.IsDraw2D then
    begin
      { Set OpenGL state that may be changed carelessly, and has some
        guanteed value, for Draw2d calls. }
      glLoadIdentity;
      glRasterPos2i(0, 0);
      C.Draw2D(C = F);
    end;
  end;
end;

procedure TKamOpenGLControl.DoDraw;

  { Does any control wants it's Draw2D called. If not, we can avoid
    even changing projection to 2D. This also takes care of checking
    UseControls --- if UseControls = @false, this always returns @false. }
  function AnyDraw2D: boolean;
  var
    I: Integer;
  begin
    Result := false;
    if UseControls then
    begin
      for I := 0 to Controls.Count - 1 do
      begin
        Result := Controls[I].IsDraw2D;
        if Result then Break;
      end;
    end;
  end;

begin
  inherited;

  if AnyDraw2D then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      { Set and push/pop OpenGL state that is guaranteed for Draw2D calls,
        but Draw2D cannot change it carelessly. }
      glDisable(GL_LIGHTING);
      glDisable(GL_DEPTH_TEST);
      glDisable(GL_TEXTURE_2D);
      if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
      if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);

      glProjectionPushPopOrtho2D(@WindowDraw2D, Self, 0, Width, 0, Height);

    glPopAttrib;
  end;
end;

procedure TKamOpenGLControl.Resize;
var
  I: Integer;
begin
  inherited;

  if ContextInitialized and UseControls then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].ContainerResize(Width, Height);
  end;
end;

procedure TKamOpenGLControl.DoGLContextClose;
var
  I: Integer;
begin
  { call GLContextClose on controls before inherited (OnClose).
    This may be called from Close, which may be called from TGLWindow destructor,
    so prepare for Controls being possibly nil now. }
  if UseControls and (Controls <> nil) then
  begin
    for I := 0 to Controls.Count - 1 do
      Controls[I].GLContextClose;
  end;

  inherited;
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

