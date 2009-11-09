unit KambiGLControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenGLContext, Navigation, Controls, Forms,
  VectorMath, Keys, KambiUtils, KambiTimeUtils, StdCtrls;

type
  { This adds some comfortable things to TOpenGLControl.

    First of all, it can report it's events to given
    TNavigator instance. This makes easy to integrate TOpenGLControl
    with Navigation features.
    This is a little analogous to TGLWindowNavigated class ---
    TGLWindowNavigated was a descendant of TGLWindow that eased
    raporting to TNavigator instance.

    Also, this provides OnGLContextInit and OnGLContextClose events.

    Also, this automatically calls LoadAllExtensions
    when GL context is initialized. This will initialize all extensions
    and set GLVersion variables, descripting OpenGL version
    and available extensions.

    TODO: integrate also MouseLook features of TNavigator,
    call MouseMove from TWalkNavigator and TExamineNavigator. }

  { TKamOpenGLControl }

  TKamOpenGLControl = class(TOpenGLControl)
  private
    FMouseX: Integer;
    FMouseY: Integer;
    FOnBeforeDraw: TNotifyEvent;
    FOnDraw: TNotifyEvent;
    FOwnsNavigator: boolean;
    FUseNavigator: boolean;
    FNavigator: TNavigator;
    FContextInitialized: boolean;

    function ReallyUseNavigator: boolean;

    FOnGLContextInit: TNotifyEvent;
    FOnGLContextClose: TNotifyEvent;

    FFps: TFramesPerSecond;

    ApplicationProperties: TApplicationProperties;
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
  protected
    procedure DestroyHandle; override;

    { In this class this just calls OnGLContextInit. }
    procedure DoGLContextInit; virtual;

    { In this class this just calls OnGLContextClose. }
    procedure DoGLContextClose; virtual;

    property ContextInitialized: boolean read FContextInitialized;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    procedure MouseDown(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: Controls.TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Integer); override;
    procedure Paint; override;

    procedure DoBeforeDraw; virtual;
    procedure DoDraw; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Navigator instance used. Initially it's nil. }
    property Navigator: TNavigator
      read FNavigator write FNavigator;

    { If @true then Navigator will be freed in our destructor.
      Note that, unlike in TGLWindowNavigated,
      default value of this property is false. }
    property OwnsNavigator: boolean
      read FOwnsNavigator write FOwnsNavigator default false;

    { If not UseNavigator or Navigator = nil then this
      will behave like normal TOpenGLControl. }
    property UseNavigator: boolean
      read FUseNavigator write FUseNavigator default true;

    { These are shortcuts for writing
      TExamineNavigator(Navigator) and TWalkNavigator(Navigator).
      In DEBUG version they use operator "as" but in RELEASE
      version they use direct type-casts for speed.

      @groupBegin }
    function ExamineNav: TExamineNavigator;
    function WalkNav: TWalkNavigator;
    { @groupEnd }

    procedure PostRedisplayOnMatrixChanged(
      ChangedNavigator: TNavigator);

    { Calculate a ray picked by WindowX, WindowY position on the window.
      Use this only when Navigator <> nil.

      ViewAngleDegX, ViewAngleDegY are your camera view angles.

      WindowX, WindowY are given in the same style as MouseX, MouseY:
      WindowX = 0 is left, WindowY = 0 is top.

      This uses @link(PrimaryRay) call. }
    procedure Ray(const WindowX, WindowY: Integer;
      const ViewAngleDegX, ViewAngleDegY: Single;
      out Ray0, RayVector: TVector3Single);

    function MakeCurrent(SaveOldToStack: boolean = false): boolean; override;

    procedure Idle; virtual;

    KeysDown: TKeysBooleans;
    MousePressed: Navigation.TMouseButtons;
    procedure ReleaseAllKeysAndMouse;

    property MouseX: Integer read FMouseX;
    property MouseY: Integer read FMouseY;

    property Fps: TFramesPerSecond read FFps;
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

{ This converts Key (Lazarus key codes) to my TKey value.

  In addition, this tries to convert Key to a character (MyCharKey).
  It's awful that this function has to do convertion to Char,
  but that's the way of VCL and LCL: KeyPress and KeyDown
  are separate events. While I want to have them in one event,
  and passed as one event to Navigator.KeyDown. }
procedure LKeyToMyKey(const Key: Word; Shift: TShiftState;
  out MyKey: TKey; out MyCharKey: char);

procedure Register;

implementation

uses LCLType, RaysWindow, GL, GLU, GLExt, KambiGLUtils, KambiStringUtils;

procedure Register;
begin
  RegisterComponents('Kambi', [TKamOpenGLControl]);
end;

{ TKamOpenGLControl --------------------------------------------------------- }

constructor TKamOpenGLControl.Create(AOwner: TComponent);
begin
  inherited;
  UseNavigator := true;
  OwnsNavigator := false;

  FFps := TFramesPerSecond.Create;

  ApplicationProperties := TApplicationProperties.Create(Self);
  ApplicationProperties.OnIdle := @ApplicationPropertiesIdle;
end;

destructor TKamOpenGLControl.Destroy;
begin
  if OwnsNavigator then Navigator.Free;
  FreeAndNil(FFps);
  inherited;
end;

procedure TKamOpenGLControl.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  Idle;
  Done := false;
end;

function TKamOpenGLControl.ReallyUseNavigator: boolean;
begin
  Result := UseNavigator and (Navigator <> nil);
end;

{ Initial idea was to do

procedure TKamOpenGLControl.CreateHandle;
begin
  Writeln('TKamOpenGLControl.CreateHandle ', ContextInitialized,
    ' ', OnGLContextInit <> nil);
  inherited CreateHandle;
  if not ContextInitialized then
  begin
    ContextInitialized := true;
    DoGLContextInit;
  end;
  Writeln('TKamOpenGLControl.CreateHandle end');
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

function TKamOpenGLControl.MakeCurrent(SaveOldToStack: boolean): boolean;
begin
  Result := inherited MakeCurrent(SaveOldToStack);

  if not ContextInitialized then
  begin
    FContextInitialized := true;
    DoGLContextInit;
  end;
end;

procedure TKamOpenGLControl.DestroyHandle;
begin
  if ContextInitialized then
  begin
    DoGLContextClose;
    FContextInitialized := false;
  end;
  inherited DestroyHandle;
end;

procedure TKamOpenGLControl.DoGLContextInit;
begin
  LoadAllExtensions;

  if Assigned(OnGLContextInit) then
    OnGLContextInit(Self);
end;

procedure TKamOpenGLControl.DoGLContextClose;
begin
  if Assigned(OnGLContextClose) then
    OnGLContextClose(Self);
end;

procedure TKamOpenGLControl.PostRedisplayOnMatrixChanged(
  ChangedNavigator: TNavigator);
begin
  Invalidate;
end;

procedure TKamOpenGLControl.ReleaseAllKeysAndMouse;
begin
  FillChar(KeysDown, SizeOf(KeysDown), 0);
  MousePressed := [];
end;

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

procedure TKamOpenGLControl.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  MyCharKey: char;
begin
  inherited KeyDown(Key, Shift);

  LKeyToMyKey(Key, Shift, MyKey, MyCharKey);

  { Tests: Writeln('Key down : ', KeyToStr(MyKey),
    ' ', Ord(MyCharKey), ' ', MyCharKey); }

  if MyKey <> K_None then
    KeysDown[MyKey] := true;

  if ReallyUseNavigator and
    ( (MyKey <> K_None) or (MyCharKey <> #0) ) and
    Navigator.KeyDown(MyKey, MyCharKey, @KeysDown) then
    Key := 0;
end;

procedure TKamOpenGLControl.KeyUp(var Key: Word; Shift: TShiftState);
var
  MyKey: TKey;
  MyCharKey: char;
begin
  inherited KeyUp(Key, Shift);

  LKeyToMyKey(Key, Shift, MyKey, MyCharKey);

  { Tests: Writeln('Key up : ', KeyToStr(MyKey),
    ' ', Ord(MyCharKey), ' ', MyCharKey); }

  if MyKey <> K_None then
    KeysDown[MyKey] := false;
end;

{ This converts Lazarus TMouseButton values to my TMouseButton values.

  (By coincidense, my type name and values are the same as used by LCL;
  but beware --- the order of values in my type is different (mbMiddle
  is in the middle in my type)). }
function LMouseButtonToMyMouseButton(
  const MouseButton: Controls.TMouseButton;
  out MyMouseButton: Navigation.TMouseButton): boolean;
begin
  Result := true;
  case MouseButton of
    Controls.mbLeft  : MyMouseButton := Navigation.mbLeft;
    Controls.mbRight : MyMouseButton := Navigation.mbRight;
    Controls.mbMiddle: MyMouseButton := Navigation.mbMiddle;
    else Result := false;
  end;
end;

procedure TKamOpenGLControl.MouseDown(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: Navigation.TMouseButton;
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseX := X;
  FMouseY := Y;

  if LMouseButtonToMyMouseButton(Button, MyButton) then
  begin
    Include(MousePressed, MyButton);
    if ReallyUseNavigator then
      Navigator.MouseDown(MyButton);
  end;
end;

procedure TKamOpenGLControl.MouseUp(Button: Controls.TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MyButton: Navigation.TMouseButton;
begin
  inherited MouseUp(Button, Shift, X, Y);

  FMouseX := X;
  FMouseY := Y;

  if LMouseButtonToMyMouseButton(Button, MyButton) then
  begin
    Exclude(MousePressed, MyButton);
  end;

end;

procedure TKamOpenGLControl.MouseMove(Shift: TShiftState; NewX, NewY: Integer);
begin
  inherited;

  if ReallyUseNavigator and
     (Navigator is TExamineNavigator) then
    TExamineNavigator(Navigator).MouseMove(
      MouseX, MouseY, NewX, NewY, MousePressed, @KeysDown);

  FMouseX := NewX;
  FMouseY := NewY;
end;

procedure TKamOpenGLControl.Idle;
begin
  Fps._IdleBegin;

  if ReallyUseNavigator then
    Navigator.Idle(Fps.IdleSpeed, @KeysDown, nil, MousePressed);
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

procedure TKamOpenGLControl.DoExit;
begin
  inherited;
  ReleaseAllKeysAndMouse;
end;

procedure TKamOpenGLControl.DoBeforeDraw;
begin
  if Assigned(OnBeforeDraw) then
    OnBeforeDraw(Self);
end;

procedure TKamOpenGLControl.DoDraw;
begin
  if Assigned(OnDraw) then
    OnDraw(Self);
end;

procedure TKamOpenGLControl.Paint;
begin
  DoBeforeDraw;
  Fps._RenderBegin;
  try
    DoDraw;
    SwapBuffers;
  finally Fps._RenderEnd end;
end;

initialization
end.

