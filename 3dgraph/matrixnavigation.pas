{
  Copyright 2003-2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(@link(TMatrixNavigator) class and descendants.) }

unit MatrixNavigation;

interface

uses SysUtils, VectorMath, KambiUtils, Keys, Boxes3d;

type
  TMouseButton = (mbLeft, mbMiddle, mbRight);
  TMouseButtons = set of TMouseButton;

const
  DefaultFallingDownStartSpeed = 0.5;
  DefaultGrowingSpeed = 1.0;
  DefaultHeadBobbing = 0.1;
  DefaultCrouchHeight = 0.5;
  DefaultMaxJumpHeight = 1.0;
  DefaultMinAngleRadFromGravityUp = { 10 degress } Pi / 18; { }
  DefaultRotationHorizontalSpeed = 3.0;
  DefaultRotationVerticalSpeed = 2.0;
  DefaultFallingDownSpeedIncrease = 13/12;
  DefaultMouseLookHorizontalSensitivity = 0.09;
  DefaultMouseLookVerticalSensitivity = 0.09;
  DefaultHeadBobbingDistance = 20.0;
  DefaultJumpSpeedMultiply = 2.0;
  DefaultJumpPower = 0.18;

type
  { }
  TMatrixNavigator = class;
  TMatrixNavigatorNotifyFunc = procedure (Navigator: TMatrixNavigator) of object;

  TInputShortcut = class;
  TInputShortcutChangedFunc = procedure (Shortcut: TInputShortcut) of object;

  { An input shortcut represents a keyboard and/or mouse shortcut
    for some command. Up to two key shortcuts may be assigned to a single
    item, and one mouse shortcut, and one character shortcut.

    Normal "key shortcut" is identified by Keys.TKey value.

    "Character shortcut" differs from "key shortcut" because it's identified
    by produced character. We don't deal here how this character is produced
    (for example, character "X" may produced on normal systems
    by Shift + "x" or by pressing "x" while "caps lock" is on;
    but this is not dealt with in this unit (it's usually provided
    by operating system / GUI toolkit to GLWindow unit),
    we just get characters passed to TMatrixNavigator.KeyDown
    and such methods.) }
  TInputShortcut = class
  private
    FKey1: TKey;
    FKey2: TKey;
    FCharacter: Char;
    FMouseButtonUse: boolean;
    FMouseButton: TMouseButton;

    procedure SetKey1(const Value: TKey);
    procedure SetKey2(const Value: TKey);
    procedure SetCharacter(const Value: Char);
    procedure SetMouseButtonUse(const Value: boolean);
    procedure SetMouseButton(const Value: TMouseButton);

    FDefaultKey1: TKey;
    FDefaultKey2: TKey;
    FDefaultCharacter: Char;
    FDefaultMouseButtonUse: boolean;
    FDefaultMouseButton: TMouseButton;

    FOnChanged: TInputShortcutChangedFunc;
  protected
    procedure Changed; virtual;
  public
    { Constructor. Key/mouse shortcuts passed here are interpreted
      as the default shortcuts (so they will be used in subsequent
      MakeDefault) and they are also the initial values for Key1, Key2 etc.
      properties. }
    constructor Create(AKey1: TKey; AKey2: TKey; ACharacter: Char;
      AMouseButtonUse: boolean;
      AMouseButton: TMouseButton);

    { Key shortcuts for given command. You can set any of them to K_None
      to indicate that no key is assigned.
      @groupBegin }
    property Key1: TKey read FKey1 write SetKey1;
    property Key2: TKey read FKey2 write SetKey2;
    { @groupEnd }

    { Character shortcut for given command. You can set this to #0
      to indicate that no character shortcut is assigned. }
    property Character: Char read FCharacter write SetCharacter;

    { Mouse shortcut for given command. You can set MouseButtonUse to @false
      if you don't want to use this.
      @groupBegin }
    property MouseButtonUse: boolean read FMouseButtonUse write SetMouseButtonUse;
    property MouseButton: TMouseButton read FMouseButton write SetMouseButton;
    { @groupEnd }

    { Default values for properties key/mouse.
      You can change them --- this will change what MakeDefault does.

      Note that setting these properties doesn't automatically set
      corresponding "current" property. E.g. @code(DefaultKey1 := K_Space;)
      doesn't change the value of Key1 property --- only DefaultKey1
      changes. You can explicitly change Key1 property, or just call
      MakeDefault afterwards, if you want this to happen.
      @groupBegin }
    property DefaultKey1: TKey read FDefaultKey1 write FDefaultKey1;
    property DefaultKey2: TKey read FDefaultKey2 write FDefaultKey2;
    property DefaultCharacter: Char
      read FDefaultCharacter write FDefaultCharacter;
    property DefaultMouseButtonUse: boolean
      read FDefaultMouseButtonUse write FDefaultMouseButtonUse;
    property DefaultMouseButton: TMouseButton
      read FDefaultMouseButton write FDefaultMouseButton;
    { @groupEnd }

    procedure MakeDefault;

    { This assigns to this object the default values from Source. }
    procedure AssignFromDefault(Source: TInputShortcut);

    { This copies Source properties to this object.
      It always copies "current" properties (Key1, Key2, Character,
      MouseButtonUse, MouseButton), and optionally (if CopyDefaults)
      also copies the DefaultXxx properties. }
    procedure Assign(Source: TInputShortcut; CopyDefaults: boolean);

    { This sets both keys to K_None, Character to #0 and MouseButtonUse
      to @false, effectively making this input shortcut impossible
      to enter by the user. }
    procedure MakeClear;

    { Given a set of currently pressed keys and mouse buttons,
      decide whether this input is currently pressed.

      You can pass @nil as KeysDown, then it will be assumed
      that no keys are currently pressed. (Otherwise, the contents
      pointed to by KeysDown pointer will never be modified;
      KeysDown is a pointer only to allow you to pass @nil here).
      Note that we assume that KeysDown behave like TGLWindow.KeysDown
      property, i.e. KeysDown[K_None] is always @false.

      Analogously, you can pass @nil as CharactersDown. }
    function IsPressed(KeysDown: PKeysBooleans;
      CharactersDown: PCharactersBooleans;
      const MousePressed: TMouseButtons): boolean;

    { Check does given Key or ACharacter correspond to this input shortcut.
      If Key = K_None and ACharacter = #0, result is always @false. }
    function IsKey(Key: TKey; ACharacter: Char): boolean;

    { Check does given mouse button correspond to this input shortcut.
      In practice, just checks MouseButtonUse and if @true, compares
      AMouseButton with MouseButton. }
    function IsMouseButton(AMouseButton: TMouseButton): boolean;

    { Check does given key/character (if not MouseEvent)
      or mouse button (if MouseEvent)
      correspond to this input shortcut.

      Basically, this is a "dispatcher" that simply calls IsKey or
      IsMouseButton method. Depending on the MouseEvent value,
      some of the other parameters (Key/ACharacter or AMouseButton) are
      simply ignored.

      This is often comfortable method if you want to squeeze calling
      IsKey and IsMouseButton into one procedure
      in the implementation --- see e.g. TMatrixWalker.EventDown. }
    function IsEvent(MouseEvent: boolean; Key: TKey;
      ACharacter: Char;
      AMouseButton: TMouseButton): boolean;

    { Describe this input shortcut. If it's not active at all
      (like after MakeClear), we will use NoneString. }
    function Description(const NoneString: string): string;

    { If assigned, this will be called always right after the key/character/mouse
      shortcut value changed. Note that this is called only when
      the "current" values (Key1, Key2, Character, MouseButtonUse, MouseButton)
      changed, and it's not called when just the DefaultXxx values changed. }
    property OnChanged: TInputShortcutChangedFunc
      read FOnChanged write FOnChanged;
  end;

  { TMatrixNavigator implements user navigation in 3D scene.
    You control navigation parameters and provide user input
    to this class by various methods and properties.
    And on the "output", this class generates a simple 4x4 matrix by
    @link(Matrix) method.

    In the most common case, when using this
    with OpenGL program, you can simply load this matrix like
    @code(glLoadMatrix(Navigator.Matrix);) at the beginning of your
    Display, Draw etc. function. In other words,
    @code(glLoadMatrix(Navigator.Matrix);) is a drop-in replacement
    for calls like @code(gluLookAt) that setup a camera.

    This class is not tied to any OpenGL specifics, any VRML specifics,
    and GLWindow etc. --- this class is fully flexible and may be used
    in any 3D program, whether using GLWindow, OpenGL or not.

    Various TMatrixNavigator descendants implement various navigation
    methods, for example TMatrixExaminer allows the user to rotate
    and scale the model (imagine that you're holding a 3D model in your
    hands and you look at it from various sides) and TMatrixWalker
    implements typical navigation in the style of first-person shooter
    games.

    Short guide how to use any descendant of this class in typical scenario:

    @orderedList(
      @item(Create an instance of some class descendant from TMatrixNavigator,
        supplying OnMatrixChanged callback (typicall a procedure that simply
        does Glwin.PostRedisplay).)

      @item(Define start properties of this class. Each TMatrixNavigator
        should have an @code(Init) method, these are designed to take
        a couple of the most critical configuration parameters.)

      @item(Call navigator events when you receive them from the user.
        For example, in your OnKeyDown callback, you want to call
        @code(Navigator.KeyDown) passing the parameters of pressed key.
        If your OnIdle callback you want to call @code(Navigator.Idle)
        (assuming that you work with TMatrixNavigatorWithIdle descendant).

        If you plan to use this with TGLWindow, then consider using
        TGLWindowNavigated class, this will take care of passing events
        to appropriate navigator calls.)

      @item(Call @code(glMultMatrix(Navigator.Matrix)) or
        @code(glLoadMatrix(Navigator.Matrix)) at the beginning of your
        OnDraw callback.)
    )

    See @code(kambi_vrml_game_engine/opengl/examples/demo_matrix_navigation.pasprogram)
    example program in engine sources for simple demo how to use this class. }
  TMatrixNavigator = class
  protected
    { This is called always when @link(Matrix) changed.
      Actually, when any
      property (of this class or descendant) changed, for example even
      changes to TMatrixWalker.MoveHorizontalSpeed result in matrix
      changed event.

      In this class this simply calls OnMatrixChanged (if assigned).

      (It may also do some other internal things (e.g. recalculating
      the frustum in TMatrixWalker), so always call inherited when
      overriding this.) }
    procedure MatrixChanged; virtual;
  public
    { Event called always when @link(Matrix) changed. Actually, when any
      property (of this class or descendant) changed, for example even
      changes to TMatrixWalker.MoveHorizontalSpeed result in matrix changed
      event.

      It may be @nil but usually you really want to react to matrix changes.
      Be careful when writing this callback, any change of navigator
      property may cause this, so be prepared to handle OnMatrixChanged
      at every time. }
    OnMatrixChanged: TMatrixNavigatorNotifyFunc;
    constructor Create(const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
      virtual;

    { Current camera matrix. You should multiply every 3D point of your
      scene by this matrix, which usually simply means that you should
      do @code(glLoadMatrix) or @code(glMultMatrix) of this matrix. }
    function Matrix: TMatrix4Single; virtual; abstract;

    { Extract only rotation from your current camera @link(Matrix).
      This is useful for rendering skybox in 3D programs
      (e.g. for Background VRML 97 node). }
    function RotationOnlyMatrix: TMatrix4Single; virtual; abstract;

    (*Handle key press event.
      Returns @true if the key was somehow handled.

      In this class this always returns @false, when implementing
      in descendants you should override it like

      @longCode(#
  Result := inherited;
  if Result then Exit;
  { ... And do the job here.
    In other words, the handling of keys in inherited
    class should have a priority. }
#)

      @param(KeysDown You can pass here a boolean table indicating
        which keys are pressed. You can pass @nil if you don't know this.

        (Contents of table passed here will never be modified anyway.
        This is a pointer only so that you can simply pass @nil here.)) *)
    function KeyDown(key: TKey; c: char; KeysDown: PKeysBooleans): boolean; virtual;

    (*Handle mouse down event.

      Just like KeyDown, this returns whether the event was handled.
      Descendants should always override this like
      @longCode(#
  Result := inherited;
  if Result then Exit;
  { ... do the job here ... }
#) *)
    function MouseDown(Button: TMouseButton): boolean; virtual;
  end;

  TMatrixNavigatorClass = class of TMatrixNavigator;

  { A descendant of @inherited that has an Idle method. }
  TMatrixNavigatorWithIdle = class(TMatrixNavigator)
  public
    { Call this often, to respond to user actions and to perform
      other tasks (falling down due to gravity in Walker mode,
      rotating model in Examine mode, and many more).

      @param(CompSpeed Should be calculated like TGLWindow.IdleSpeed,
        and usually it's in fact just taken from Glwin.IdleSpeed.)

      @param(KeysDown What keys are pressed currently ?
        You pass here a pointer to a boolean table indicating
        which keys are currently pressed. Or you can pass @nil
        here if you don't know it. Just like for @link(KeyDown) method.)

      @param(CharactersDown What character codes are pressed currently ?
        Analogous to KeysDown, you can pass here a pointer or @nil.)

      @param(MousePressed Which mouse buttons are currently pressed ?) }
    procedure Idle(const CompSpeed: Single;
      KeysDown: PKeysBooleans;
      CharactersDown: PCharactersBooleans;
      const MousePressed: TMouseButtons); virtual; abstract;
  end;

  T3BoolInputs = array [0..2, boolean] of TInputShortcut;

  { Navigate the 3D model in examine mode, like you would hold
    a box with the model inside.
    The model is displayed around MoveAmount 3D point,
    it's rotated by RotationAngle and scaled by ScaleFactor
    (scaled around MoveAmount point). }
  TMatrixExaminer = class(TMatrixNavigatorWithIdle)
  private
    FRotationsSpeed, FRotationsAngle, FMoveAMount, FModelBoxMiddle: TVector3Single;
    FScaleFactor: Single;
    FModelBox: TBox3d;
    procedure SetRotationsSpeed(const Value: TVector3Single);
    procedure SetRotationsAngle(const Value: TVector3Single);
    procedure SetScaleFactor(const Value: Single);
    procedure SetMoveAmount(const Value: TVector3Single);
    procedure SetModelBox(const Value: TBox3d);

    FInputs_Move: T3BoolInputs;
    FInputs_Rotate: T3BoolInputs;
    FInput_ScaleLarger: TInputShortcut;
    FInput_ScaleSmaller: TInputShortcut;
    FInput_Home: TInputShortcut;
    FInput_StopRotating: TInputShortcut;

    function EventDown(MouseEvent: boolean; Key: TKey;
      ACharacter: Char;
      AMouseButton: TMouseButton): boolean;
  public
    constructor Create(const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
      override;
    destructor Destroy; override;

    function Matrix: TMatrix4Single; override;
    function RotationOnlyMatrix: TMatrix4Single; override;
    procedure Idle(const CompSpeed: Single;
      KeysDown: PKeysBooleans;
      CharactersDown: PCharactersBooleans;
      const MousePressed: TMouseButtons); override;
    function KeyDown(key: TKey; c: char; KeysDown: PKeysBooleans): boolean; override;
    function MouseDown(Button: TMouseButton): boolean; override;

    { Mose move event.

      Like for KeyDown and Idle, you can pass @nil if you don't know this. }
    function MouseMove(OldX, OldY, NewX, NewY: Integer;
      const MousePressed: TMouseButtons; KeysDown: PKeysBooleans): boolean;

    { Current camera properties ---------------------------------------------- }

    { RotationsAngle is the current rotation of the model, around each base
      axis. RotationsSpeed is the speed how fast RotationsAngle change.
      Rotation is done around ModelBox middle (with MoveAmount added).
      Default values of these are zero vectors.
      @groupBegin }
    property RotationsSpeed: TVector3Single
      read FRotationsSpeed write SetRotationsSpeed;
    property RotationsAngle: TVector3Single
      read FRotationsAngle write SetRotationsAngle;
    { @groupEnd }

    { MoveAmount says how to translate the model.
      It's always added to the middle of ModelBox, this is usually
      comfortable.

      The default value of this is zero vector.
      If you want to just see the whole model,
      you may want to set this to something like

        @preformatted(MoveAmount := Middle of ModelBox + (0, 0, -2 * ModelSize))

      Actually, @link(Init) method does the above for you. }
    property MoveAmount: TVector3Single read FMoveAmount write SetMoveAmount;

    { How the mode is scaled. Scaling is done around MoveAmount added to
      the middle of ModelBox. }
    property ScaleFactor: Single
      read FScaleFactor write SetScaleFactor default 1;

    { The aproximate size of 3D model that will be viewed.
      This is the crucial property of this class that you have to set,
      to make the navigation work best.

      The idea is that usually this is the only property that you have to set.
      ScaleFactor, MoveAmount, RotationsSpeed will be almost directly
      controlled by user (through KeyDown and other events).
      RotationsAngle will be automatically modified by @link(Idle).

      So often you only need to set ModelBox, once,
      and everything else will work smoothly. }
    property ModelBox: TBox3d read FModelBox write SetModelBox; { = EmptyBox3d }

    { Initializes most important properties of this class:
      ModelBox, and MoveAmount. MoveAmount is set such that
      ModelBox is nicely viewed.

      Rest of the properties will be set to their default values.

      In other words, this is just a shortcut to setting ModelBox
      and then calling @link(Home). }
    procedure Init(const AModelBox: TBox3d);
    procedure Home;

    { Methods performing navigation.
      Usually you want to just leave this for user to control. --------------- }

    { StopRotating sets RotationsSpeed to zero, stopping the rotation of
      the model. }
    procedure StopRotating;

    { Rotate adds SpeedChange (in degrees) to RotationSpeed[Coord],
      thus making rotation faster. }
    procedure Rotate(coord: integer; const SpeedChange: Single);

    procedure Scale(const ScaleBy: Single);
    procedure Move(coord: integer; const MoveDistance: Single);

    { User inputs ------------------------------------------------------------ }

    { TODO: tak samo jak TMatrixWalker, przydaloby sie moc podawac
      tutaj za jednym zamachem char+TKey+modifiers zamiast tylko char
      lub tylko TKey.
      W tym momencie klawisze Inputs_Move dzialaja gdy ModifiersDown = [mkCtrl],
      a pozostale klawisze gdy ModifiersDown = []. }

    { }
    property Inputs_Move: T3BoolInputs read FInputs_Move;
    property Inputs_Rotate: T3BoolInputs read FInputs_Rotate;
    property Input_ScaleLarger: TInputShortcut read FInput_ScaleLarger;
    property Input_ScaleSmaller: TInputShortcut read FInput_ScaleSmaller;
    property Input_Home: TInputShortcut read FInput_Home;
    property Input_StopRotating: TInputShortcut read FInput_StopRotating;
  end;

  TMatrixWalker = class;

  { See @link(TMatrixWalker.DoMoveAllowed) and
    @link(TMatrixWalker.OnMoveAllowed) }
  TMoveAllowedFunc = function(Navigator: TMatrixWalker;
    const ProposedNewPos: TVector3Single;
    out NewPos: TVector3Single;
    const BecauseOfGravity: boolean): boolean of object;

  { See @link(TMatrixWalker.OnFalledDown). }
  TFalledDownNotifyFunc = procedure (Navigator: TMatrixWalker;
    const FallenHeight: Single) of object;

  TGetCameraHeight = procedure (Navigator: TMatrixWalker;
    out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single)
    of object;

  { Navigation by walking (first-person-shooter-like moving) in 3D scene.
    Camera is defined by it's position, looking direction
    and up vector, user can rotate and move camera using various keys. }
  TMatrixWalker = class(TMatrixNavigatorWithIdle)
  private
    FCameraPos, FCameraDir, FCameraUp,
    FInitialCameraPos, FInitialCameraDir, FInitialCameraUp: TVector3Single;
    FGravityUp: TVector3Single;

    FMoveHorizontalSpeed, FMoveVerticalSpeed: Single;
    FRotationHorizontalSpeed, FRotationVerticalSpeed: Single;
    FPreferGravityUpForRotations: boolean;
    FPreferGravityUpForMoving: boolean;

    procedure SetCameraPos(const Value: TVector3Single);
    procedure SetCameraDir(const Value: TVector3Single);
    procedure SetCameraUp(const Value: TVector3Single);

    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_RightRot: TInputShortcut;
    FInput_LeftRot: TInputShortcut;
    FInput_RightStrafe: TInputShortcut;
    FInput_LeftStrafe: TInputShortcut;
    FInput_UpRotate: TInputShortcut;
    FInput_DownRotate: TInputShortcut;
    FInput_UpMove: TInputShortcut;
    FInput_DownMove: TInputShortcut;
    FInput_GravityUp: TInputShortcut;
    FInput_MoveSpeedInc: TInputShortcut;
    FInput_MoveSpeedDec: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FInput_Crouch: TInputShortcut;

    FAllowSlowerRotations: boolean;
    FCheckModsDown: boolean;

    FMinAngleRadFromGravityUp: Single;

    FMouseLook: boolean;
    FMouseLookHorizontalSensitivity: Single;
    FMouseLookVerticalSensitivity: Single;

    procedure RotateAroundGravityUp(const AngleDeg: Single);
    procedure RotateAroundUp(const AngleDeg: Single);
    procedure RotateHorizontal(const AngleDeg: Single);
    procedure RotateVertical(const AngleDeg: Single);

    procedure Jump;

    function EventDown(MouseEvent: boolean; Key: TKey;
      ACharacter: Char;
      AMouseButton: TMouseButton): boolean;

    { Private things related to frustum ---------------------------- }

    FProjectionMatrix: TMatrix4Single;
    FFrustum: TFrustum;
    procedure RecalculateFrustum;
    procedure SetProjectionMatrix(const Value: TMatrix4Single);

    { Private things related to gravity ---------------------------- }

    FCameraPreferredHeight: Single;
    FIsFallingDown: boolean;
    FFallingDownStartPos: TVector3Single;
    FOnFalledDown: TFalledDownNotifyFunc;
    FFallingDownStartSpeed: Single;
    FFallingDownSpeed: Single;
    FFallingDownSpeedIncrease: Single;
    FGravity: boolean;
    FOnGetCameraHeight: TGetCameraHeight;
    FGrowingSpeed: Single;
    { This is used by FallingDownEffect to temporary modify Matrix result
      by rotating CameraUp around CameraDir. In degress. }
    Fde_CameraUpRotate: Single;
    { This is used by FallingDownEffect to consistently rotate us.
      This is either -1, 0 or +1. }
    Fde_RotateHorizontal: Integer;
    FFallingDownEffect: boolean;

    FMaxJumpHeight: Single;
    FIsJumping: boolean;
    FJumpHeight: Single;
    FJumpPower: Single;
    FJumpSpeedMultiply: Single;

    FHeadBobbing: Single;
    HeadBobbingPosition: Single;
    FHeadBobbingDistance: Single;
    function UseHeadBobbing: boolean;

    FCrouchHeight: Single;
    FIsCrouching: boolean;

    FFallingOnTheGround: boolean;
    FFallingOnTheGroundAngleIncrease: boolean;

    FIsOnTheGround: boolean;
    FIsWalkingOnTheGround: boolean;

    function RealCameraPreferredHeightNoHeadBobbing: Single;
    function RealCameraPreferredHeightMargin: Single;

    FInvertVerticalMouseLook: boolean;
  protected
    { }
    procedure MatrixChanged; override;
  public
    constructor Create(const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
      override;
    destructor Destroy; override;

    procedure DoGetCameraHeight(
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single); virtual;

    function Matrix: TMatrix4Single; override;
    function RotationOnlyMatrix: TMatrix4Single; override;
    procedure Idle(const CompSpeed: Single;
      KeysDown: PKeysBooleans;
      CharactersDown: PCharactersBooleans;
      const MousePressed: TMouseButtons); override;
    function KeyDown(key: TKey; c: char; KeysDown: PKeysBooleans): boolean; override;

    { This is used by @link(DoMoveAllowed), see there for description. }
    OnMoveAllowed: TMoveAllowedFunc;

    { @abstract(DoMoveAllowed will be used when user will move in the scene,
      i.e. when user will want to change CameraPos.)

      ProposedNewPos is the position where the user wants to move
      (current user position is always stored in CameraPos,
      so you can calculate move direction by ProposedNewPos-CameraPos).

      This is the place to "plug in" your collision detection
      into this object.

      DoMoveAllowed should return false if no move is allowed.
      Else it should return true and set NewPos to the position
      where user should be moved. E.g. if you're doing a simple
      test for collisions (with yes/no results), you will always
      want to set NewPos to ProposedNewPos when returning true.
      But you can also do more sophisticated calculations and
      sometimes not allow user to move to ProposedNewPos, but allow
      him to move instead to some other close position.
      E.g. look what's happening in quake (or just any first-person
      3d game) when you're trying to walk "into the wall"
      at angle like 30 degrees: you're blocked,
      i.e. you obviously can't walk into the wall, but your position
      changes a bit and you're slowly moving alongside the wall.
      That's how you can use NewPos: you can return true and set
      NewPos to something that is not exactly ProposedNewPos
      (but is close to ProposedNewPos).

      Note that it's allowed to modify NewPos when returning false.
      This is meaningless, but may be comfortable for implementor
      of DoMoveAllowed.

      BecauseOfGravity says whether this move is caused by gravity
      dragging the camera down. Can happen only if @link(Gravity)
      is @true. You can use BecauseOfGravity to control DoMoveAllowed
      behavior --- e.g. view3dscene will not allow camera to move
      outside scene's bounding box when BecauseOfGravity
      (because this would mean that camera falls down infinitely),
      on the other hand when BecauseOfGravity is @false moving
      outside bounding box is allowed (to allow viewer to look at the
      scene from "the outside").

      Basic implementation of DoMoveAllowed in this class:
      If OnMoveAllowed = nil then returns true and sets NewPos to
      ProposedNewPos (so move is always allowed).
      Else calls OnMoveAllowed. }
    function DoMoveAllowed(const ProposedNewPos: TVector3Single;
      out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; virtual;

    { Keys --------------------------------------------------------- }

    { TODO: przydaloby sie rozwiazac tu sprawe z modifiers jakos bardziej
      elegancko. Kazdy klawisz to powinien byc kod + flagi modifierow.
      W tej chwili klawisze wszystkie ponizsze klawisze dzialaja gdy
      wszystkie modifiery sa OFF, za wyjatkiem Input_Right/LeftRot i
      Input_Up/DownRotate ktore uzyskuja specjalne znaczenie gdy dziala modifier
      Ctrl (see AllowSlowerRotations). }

    { }
    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_LeftRot: TInputShortcut read FInput_LeftRot;
    property Input_RightRot: TInputShortcut read FInput_RightRot;
    property Input_LeftStrafe: TInputShortcut read FInput_LeftStrafe;
    property Input_RightStrafe: TInputShortcut read FInput_RightStrafe;
    property Input_UpRotate: TInputShortcut read FInput_UpRotate;
    property Input_DownRotate: TInputShortcut read FInput_DownRotate;
    property Input_UpMove: TInputShortcut read FInput_UpMove;
    property Input_DownMove: TInputShortcut read FInput_DownMove;
    property Input_GravityUp: TInputShortcut read FInput_GravityUp;

    { Note that Input_MoveSpeedInc and Input_MoveSpeedDec change
      both MoveHorizontalSpeed and MoveVerticalSpeed.
      @groupBegin }
    property Input_MoveSpeedInc: TInputShortcut read FInput_MoveSpeedInc;
    property Input_MoveSpeedDec: TInputShortcut read FInput_MoveSpeedDec;
    { @groupEnd }

    { Note that jumping and crouching works only when @link(Gravity) works.
      @groupBegin }
    property Input_Jump: TInputShortcut read FInput_Jump;
    property Input_Crouch: TInputShortcut read FInput_Crouch;
    { @groupEnd }

    { If @true then all rotation keys
      (Input_RightRot, Input_LeftRot, Input_UpRotate, Input_DownRotate)
      will work 10x slower when Ctrl modified is pressed. }
    property AllowSlowerRotations: boolean
      read FAllowSlowerRotations write FAllowSlowerRotations
      default true;

    { @abstract(Do we check what key modifiers are pressed and do something
      differently based on it?)

      If @true then all keys work only when no modifiers or only shift are
      pressed. Additionally when Ctrl is pressed (and AllowSlowerRotations) then
      rotation keys work 10x slower. Other keys with other modifiers
      don't work. We allow shift, because to press character "+" on non-numpad
      keyboard (useful on laptops, where numpad is difficult) you
      probably need to press shift.

      If @false then all keys work as usual, no matter what
      modifiers are pressed. And rotation keys never work 10x slower
      (AllowSlowerRotations is ignored). }
    property CheckModsDown: boolean
      read FCheckModsDown write FCheckModsDown
      default true;

    { General stuff ----------------------------------------------------- }

    { Moving speeds. Default values for both are 1.0,
      this is comfortable to display them to user (you can nicely
      display "1.0" as default moving speed). Note that the length
      of CameraDir also affects moving speed.
      @groupBegin }
    property MoveHorizontalSpeed: Single
      read FMoveHorizontalSpeed write FMoveHorizontalSpeed default 1.0;
    property MoveVerticalSpeed: Single
      read FMoveVerticalSpeed write FMoveVerticalSpeed default 1.0;
    { @groupEnd }

    property RotationHorizontalSpeed: Single
      read FRotationHorizontalSpeed write FRotationHorizontalSpeed
      default DefaultRotationHorizontalSpeed;

    property RotationVerticalSpeed: Single
      read FRotationVerticalSpeed write FRotationVerticalSpeed
      default DefaultRotationVerticalSpeed;

    { Camera position, looking direction and up vector.

      Initially (after creating this object) they are equal to
      InitialCameraPos, InitialCameraDir, InitialCameraUp.
      Also @link(Init) and @link(Home) methods reset them to respective
      InitialCameraXxx values.

      The length of CameraDir vector @bold(is significant) ---
      together with MoveHorizontalSpeed and MoveVerticalSpeed it determines
      the moving speed.

      More precisely: each horizontal move in Idle moves by distance
      VectorLen(CameraDir) * MoveXxxSpeed * CompSpeed * 50.
      So in 1/50 CompSpeed unit (which is 1/50 of second if it came from
      normal TGLWindow.IdleSpeed), we move by VectorLen(CameraDir) * MoveXxxSpeed.
      So we move 50 * VectorLen(CameraDir) * MoveXxxSpeed units per second.

      When setting CameraDir, CameraUp will always be automatically
      adjusted to be orthogonal to CameraDir. And vice versa ---
      when setting CameraUp, CameraDir will be adjusted.
      But don't worry, the lengths of adjusted CameraUp or CameraDir
      will always be preserved.

      @groupBegin }
    property CameraPos: TVector3Single read FCameraPos write SetCameraPos;
    property CameraDir: TVector3Single read FCameraDir write SetCameraDir;
    property CameraUp : TVector3Single read FCameraUp  write SetCameraUp;
    { @groupEnd }

    { This is the upward direction of the world in which player moves.

      This indicates how @link(Gravity) works.

      This is also the "normal" value for both CameraUp and
      InitialCameraUp --- one that means that player is looking straight
      foward. This is used for features like PreferGravityUpForRotations
      and/or PreferGravityUpForMoving. }
    property GravityUp: TVector3Single read FGravityUp write FGravityUp;

    { If PreferGravityUpForRotations or PreferGravityUpForMoving
      then various operations are done with respect
      to GravityUp, otherwise they are done with
      respect to current CameraUp.

      With PreferGravityUpForRotations, this affects rotations:
      horizontal rotations (Input_LeftRot and Input_RightRot)
      and rotations caused by MouseLook.
      Also vertical rotations are bounded by MinAngleRadFromGravityUp
      when PreferGravityUpForRotations.

      Note that you can change it freely at runtime,
      and when you set PreferGravityUpForRotations from @false to @true
      then in nearest Idle
      calls CameraUp will be gradually fixed, so that CameraDir and CameraUp
      and GravityUp are on the same plane. Also CameraDir may be adjusted
      to honour MinAngleRadFromGravityUp.

      With PreferGravityUpForMoving, this affects moving:
      horizontal moving (forward, backward, strafe),
      and vertical moving (Input_UpMove and Input_DownMove).
      E.g. when PreferGravityUpForMoving then forward/backward keys are tied
      to horizontal plane defined by GravityUp.
      When not PreferGravityUpForMoving then forward/backward try to move
      you just in the CameraDir. Which is usually more handy when
      e.g. simulating flying.

      It's a delicate decision how to set them, because generally
      all the decisions are "somewhat correct" --- they just sometimes
      "feel incorrect" for player.

      @unorderedList(
        @item(
          First of all, if the scene is not "naturally oriented"
          around GravityUp, then you @bold(may) set
          PreferGravityUpForRotations as @false and you @bold(should)
          leave PreferGravityUpForMoving and @link(Gravity) to @false.

          By the scene "naturally oriented around GravityUp"
          I mean that we have some proper GravityUp,
          not just some guessed GravityUp that may
          be incorrect. For example when view3dscene loads a VRML model
          without any camera definition then it assumes that "up vector"
          is (0, 1, 0), because this is more-or-less VRML standard
          suggested by VRML spec. But this may be very inappopriate,
          for example the scene may be actually oriented with (0, 0, 1)
          up vector in mind.

          Other examples of the scenes without any
          "naturally oriented around GravityUp" may be some
          "outer space" scene without any gravity.)

        @item(
          With PreferGravityUpForRotations the "feeling" of GravityUp
          is stronger for user, because GravityUp, CameraUp and CameraDir
          always define the same plane in 3D space (i.e. along with the
          4th point, (0, 0, 0), for camera eye). Raising/bowing the head
          doesn't break this assumption.

          Without PreferGravityUpForRotations, we quickly start to do rotations
          in an awkward way --- once you do some vertical rotation,
          you changed CameraUp, and next horizontal rotation will be
          done versus new CameraUp.

          If your GravityUp is good, then you generally should
          leave PreferGravityUpForRotations to @true. Unless you really @bold(want)
          the player to feel movements as "awkward", e.g. when you
          want to simulate this "outer space without any gravity" feeling.)

        @item(
          If your GravityUp is good, then you generally should set
          PreferGravityUpForMoving just like Gravity.

          E.g. when the player is flying / swimming etc. he will probably prefer
          PreferGravityUpForMoving = @false, because this way he will not have to
          press Input_UpMove and Input_DownMove. Simply pressing Input_Forward
          and Input_Backward and doing rotations will be enough to move
          freely in 3D space.

          When gravity works, PreferGravityUpForMoving = @true is better,
          otherwise player would unnecessarily try to jump when looking up.)
      )

      @groupBegin }
    property PreferGravityUpForRotations: boolean
      read FPreferGravityUpForRotations write FPreferGravityUpForRotations default true;

    property PreferGravityUpForMoving: boolean
      read FPreferGravityUpForMoving write FPreferGravityUpForMoving default true;
    { @groupEnd }

    { Initial camera values.

      InitialCameraUp will be automatically corrected on change, such that
      InitialCameraDir and InitialCameraUp will still define the same plane
      but will also be orthogonal. This also means that you obviously cannot set
      InitialCameraUp to be parallel to InitialCameraDir.

      Default value of InitialCameraPos is (0, 0, 0), InitialCameraDir is
      (0, -1, 0), InitialCameraUp is (0, 1, 0).

      @groupBegin }
    property InitialCameraPos: TVector3Single read FInitialCameraPos;
    property InitialCameraDir: TVector3Single read FInitialCameraDir;
    property InitialCameraUp : TVector3Single read FInitialCameraUp;
    { @groupEnd }

    procedure SetInitialCameraLookDir(const AInitialCameraPos, AInitialCameraDir,
      AInitialCameraUp: TVector3Single);
    procedure SetInitialCameraLookAt(const AInitialCameraPos, AInitialCameraCenter,
      AInitialCameraUp: TVector3Single);

    { This returns CameraDir vector rotated such that it is
      orthogonal to GravityUp. This way it returns CameraDir projected
      on the gravity horizontal plane, which neutralizes such things
      like raising / bowing your head.

      Note that when CameraDir and GravityUp are parallel,
      this just returns current CameraDir --- because in such case
      we can't project CameraDir on the horizontal plane. }
    function CameraDirInGravityPlane: TVector3Single;

    { This sets in one go most important properties of this class.
      Sets initial camera properties (InitialCameraXxx),
      sets current camera properties to them (CameraXxx := InitialCameraXxx).

      It will also call CorrectCameraPreferredHeight(ACameraRadius),
      because this is important thing that's too easy to otherwise forget.
      Just pass ACameraRadius = 0.0 if you don't really want this. }
    procedure Init(const AInitialCameraPos, AInitialCameraDir,
      AInitialCameraUp: TVector3Single;
      const AGravityUp: TVector3Single;
      const ACameraPreferredHeight: Single;
      const ACameraRadius: Single); overload;

    { Alternative Init that sets camera properties such that
      an object inside Box is more or less "visible good".
      Sets InitialCameraXxx properties to make it look right,
      sets current CameraXxx properties to InitialCameraXxx.
      Sets GravityUp to the same thing as InitialCameraUp.
      Sets also CameraPreferredHeight to make it behave "sensibly". }
    procedure Init(const box: TBox3d; const ACameraRadius: Single); overload;

    procedure Home;

    { This sets the minimal angle (in radians) between GravityUp
      and CameraDir, and also between -GravityUp and CameraDir.
      This way vertical rotations (like Input_UpRotate,
      Input_DownRotate) are "bounded" to not allow player to do something
      strange, i.e. bow your head too much and raise your head too much.

      This is used only when PreferGravityUpForRotations
      is @true and when it's <> 0.0.

      This must be always between 0 and Pi/2. Value of Pi/2 will effectively
      disallow vertical rotations (although you should rather do this in
      a "cleaner way" by calling MakeClear on Input_UpRotate and Input_DownRotate). }
    property MinAngleRadFromGravityUp: Single
      read FMinAngleRadFromGravityUp write FMinAngleRadFromGravityUp
      default DefaultMinAngleRadFromGravityUp;

    { If true, then player is able to rotate the view (horizontally
      and vertically, equivalent to Input_LeftRot, Input_RightRot,
      Input_UpRotate, Input_DownRotate) by moving the mouse.

      You have to call MouseMove of this object to achieve this.
      Also note that there are things that you have to take care
      of yourself when enabling this property:
      @unorderedList(
        @item(After calling MouseMove you should reposition your mouse
          at the middle of your window (or the screen), to avoid the situation
          when mouse movement is blocked by screen borders.)
        @item(You usually will want to hide the mouse cursor, as showing
          it always bouncing at the middle of the window/screen
          doesn't look sensible.)
      ) }
    property MouseLook: boolean read FMouseLook write FMouseLook default false;

    { These control mouse look sensitivity.
      They say how much angle change is produced by 1 pixel change
      (for MouseXChange, MouseYChange in MouseMove).
      You can change this, to better adjust to user.

      @groupBegin }
    property MouseLookHorizontalSensitivity: Single
      read FMouseLookHorizontalSensitivity write FMouseLookHorizontalSensitivity
      default DefaultMouseLookHorizontalSensitivity;
    property MouseLookVerticalSensitivity: Single
      read FMouseLookVerticalSensitivity write FMouseLookVerticalSensitivity
      default DefaultMouseLookVerticalSensitivity;
    { @groupEnd }

    { If this is @true and MouseLook works, then the meaning of vertical mouse
      movement is inverted: when user moves mouse up, he looks down.
      Many players are more comfortable with such configuration,
      and many games implement it (usually by calling it "Invert mouse"
      for short). }
    property InvertVerticalMouseLook: boolean
      read FInvertVerticalMouseLook write FInvertVerticalMouseLook
      default false;

    { Call this to actually make MouseLook work.
      MouseXChange and MouseYChange are differences between current
      and previous window coords
      (like in TGLWindow.MouseX/MouseY, so 0,0 is top-left corner). }
    procedure MouseMove(MouseXChange, MouseYChange: Integer);

    function MouseDown(Button: TMouseButton): boolean; override;

    { Things related to frustum ---------------------------------------- }

    { Projection matrix that you should pass here to have Frustum
      calculated for you.

      This is initially IdentityMatrix4Single.
      This is not modified anywhere from this class.
      *You* should modify it, you should set it to projection matrix
      that you use, if you want to use Frustum value.
      This is used whenever Frustum is recalculated. }
    property ProjectionMatrix: TMatrix4Single
      read FProjectionMatrix write SetProjectionMatrix;

    { The current camera (viewing frustum, based on
      @link(ProjectionMatrix) (set by you) and @link(Matrix) (calculated here).
      This is recalculated whenever one of these two properties change.
      Be sure to set @link(ProjectionMatrix) before using this. }
    property Frustum: TFrustum read FFrustum;

    { Things related to gravity ---------------------------------------- }

    { This unlocks a couple of features and automatic behaviors
      related to gravity. Gravity always drags the camera down to
      -GravityUp.

      Summary of things done by gravity:
      @unorderedList(
        @item(It uses OnGetCameraHeight to get camera height above the ground.)
        @item(It allows player to jump. See Input_Jump, IsJumping, MaxJumpHeight,
          JumpSpeedMultiply.)
        @item(It allows player to crouch. See Input_Crouch, CrouchHeight.)
        @item(It tries to keep CameraPos above the ground on
          CameraPreferredHeight height.)
        @item(When current height is too small --- CameraPos is moved up.
          See GrowingSpeed.)
        @item(When current height is too large --- we're falling down.
          See IsFallingDown, OnFalledDown, FallingDownStartSpeed,
          FallingDownSpeedIncrease, FallingDownEffect.)
        @item(It does head bobbing. See HeadBobbing, HeadBobbingDistance.)
      )

      While there are many properties allowing you to control
      gravity behavior, most of them have initial values that should be
      sensible in all cases. The only things that you really want to take
      care of are: OnGetCameraHeight and CameraPreferredHeight.
      Everything else should basically work auto-magically.

      Note that Gravity setting is independent from
      PreferGravityUpForRotations or PreferGravityUpForMoving settings ---
      PreferGravityUpXxx say how the player controls work,
      Gravity says what happens to player due to ... well, due to gravity. }
    property Gravity: boolean
      read FGravity write FGravity default false;

    { When @link(Gravity) is on, CameraPos tries to stay CameraPreferredHeight
      above the ground. Temporary it may be lower (player can
      shortly "duck" when he falls from high).

      This must always be >= 0.
      You should set this to something greater than zero to get sensible
      behavior of some things related to @link(Gravity),
      and also you should set OnGetCameraHeight.

      See CorrectCameraPreferredHeight for important property
      of CameraPreferredHeight that you should keep. }
    property CameraPreferredHeight: Single
      read FCameraPreferredHeight write FCameraPreferredHeight default 0.0;

    { This procedure corrects CameraPreferredHeight based on your CameraRadius
      and on current HeadBobbing.

      Exactly what and why is done: if you do any kind of collision
      detection with some CameraRadius, then
      you should make sure that RealCameraPreferredHeight is always >= of your
      CameraRadius, otherwise strange effects may happen when crouching
      or when head bobbing forces camera to go down.

      Exactly, the required equation is
@preformatted(
  MinimumRealCameraPreferredHeight :=
    CameraPreferredHeight * CrouchHeight * (1 - HeadBobbing);
)
      and always must be
@preformatted(
  MinimumRealCameraPreferredHeight >= RealCameraPreferredHeight
)

      Reasoning: otherwise this class would "want camera to fall down"
      (because we will always be higher than RealCameraPreferredHeight)
      but your OnMoveAllowed would not allow it (because CameraRadius
      would not allow it). Note that this class doesn't keep value
      of your CameraRadius, because collision detection
      is (by design) never done by this class --- it's always
      delegated to OnGetCameraHeight and OnMoveAllowed.
      Also, it's not exactly forced @italic(how) you should force this
      condition to hold. Sometimes the good solution is to adjust
      CameraRadius, not to adjust CameraPreferredHeight.

      Anyway, this method will make sure that this condition
      holds by eventually adjusting (making larger) CameraPreferredHeight.
      Note that for CameraRadius = 0.0 this will always leave
      CameraPreferredHeight as it is. }
    procedure CorrectCameraPreferredHeight(const CameraRadius: Single);

    { Assign here the callback (or override DoGetCameraHeight)
      to say what is the current height of camera above the ground.
      This should be calculated like collision of ray from CameraPos
      in direction -GravityUp with the scene.

      This event returns IsAboveTheGround: bolean (set to @false
      to indicate that the ray doesn't hit the scene at all,
      so the camera is not above the ground at all) and
      SqrHeightAboveTheGround (meaningfull only if IsAboveTheGround;
      this is the Sqr(height of camera above the ground)).

      Implementation of DoGetCameraHeight in this class
      initializes IsAboveTheGround to @false and then calls
      OnGetCameraHeight, if assigned. }
    property OnGetCameraHeight: TGetCameraHeight
      read FOnGetCameraHeight
      write FOnGetCameraHeight;

    { This is called when camera was falling down for some time,
      and suddenly stopped (this means that camera "hit the ground").
      Of course this is used only when @link(Gravity) is @true
      (it can also be called shortly after you changed
      @link(Gravity) from @true to @false, so don't simply assert
      here that @link(Gravity) is @true).

      It can be useful in games to do some things
      (maybe basing on FallenHeight parameter passed to this callback)
      like lowering player's health and/or making some effects (displaying
      "blackout" or playing sound like "Ouh!" etc.). }
    property OnFalledDown: TFalledDownNotifyFunc
      read FOnFalledDown write FOnFalledDown;

    { Initial speed of falling down.
      Of course this is used only when @link(Gravity) is true.

      Note that while falling down,
      the camera will actually fall with greater and greated speed
      (this adds more realism to the gravity effect...).
      Note that this is always relative to CameraDir length.
      CameraDir determines moving speed --- and so it determines
      also falling speed. The default DefaultFallingDownStartSpeed
      is chosen to be something sensible, to usually get nice effect
      of falling.

      You can change it at any time, but note that if you change this
      while IsFallingDown is @true, then you will not change the
      "current falling down speed". You will change only the falling down
      speed used the next time. }
    property FallingDownStartSpeed: Single
      read FFallingDownStartSpeed write FFallingDownStartSpeed
      default DefaultFallingDownStartSpeed;

    { When falling down, the speed increases.
      Set this to 1.0 to fall down with constant speed
      (taken from FallingDownStartSpeed). }
    property FallingDownSpeedIncrease: Single
      read FFallingDownSpeedIncrease write FFallingDownSpeedIncrease
      default DefaultFallingDownSpeedIncrease;

    property IsFallingDown: boolean read FIsFallingDown;

    { If IsFallingDown, then this will force IsFallingDown to false
      @bold(without calling OnFallenDown). It's much like forcing
      the opinion that "camera is not falling down right now".

      Of course, if in the nearest Idle we will find out (using
      GetCameraHeight) that camera is too high above the ground,
      then we will start falling down again, setting IsFallingDown
      back to true. (but then we will start falling down from the beginning,
      starting at given CameraPos and with initial falling down speed).

      This is useful to call if you just changed CameraPos because
      e.g. the player teleported somewhere (or e.g. game levels changed).
      In this case you just want to forget the fact that camera
      was falling down --- no consequences (like lowering player's
      health, redout etc.). }
    procedure CancelFallingDown;

    { This triggers a nice effect when falling down from high.
      Camera dir rotates slightly, and camera up temporary rotates
      around camera up. This makes nice visual effect, so usually
      you will want this.

      Of course this is meaningfull only when @link(Gravity) works.

      Note that changing it from @true to @false doesn't immediately
      "cancel out" this effect if it's currently in progress.
      It only prevents this effect from starting again. }
    property FallingDownEffect: boolean
      read FFallingDownEffect write FFallingDownEffect default true;

    { When @link(Gravity) works and camera height above the ground
      is less than CameraPreferredHeight, then we try to "grow",
      i.e. camera position increases along the GravityUp
      so that camera height above the ground is closer to
      CameraPreferredHeight. This property (together with length of
      CameraDir, that always determines every moving speed)
      determines the speed of this growth. }
    property GrowingSpeed: Single
      read FGrowingSpeed write FGrowingSpeed
      default DefaultGrowingSpeed;

    { How high can you jump ?
      The max jump distance is calculated as
      MaxJumpHeight * CameraPreferredHeight, see MaxJumpDistance. }
    property MaxJumpHeight: Single
      read FMaxJumpHeight write FMaxJumpHeight default DefaultMaxJumpHeight;

    { Returns just MaxJumpHeight * CameraPreferredHeight,
      see MaxJumpHeight for explanation. }
    function MaxJumpDistance: Single;

    { Camera is in the middle of a "jump" move right now. }
    property IsJumping: boolean read FIsJumping;

    property JumpSpeedMultiply: Single
      read FJumpSpeedMultiply write FJumpSpeedMultiply
      default DefaultJumpSpeedMultiply;

    property JumpPower: Single read FJumpPower write FJumpPower
      default DefaultJumpPower;

    { When you move horizontally, you get "head bobbing" effect
      --- camera position slightly changes it's vertical position,
      going a little up, then a little down, then a little up again etc.

      This property mutiplied by CameraPreferredHeight
      says how much head bobbing can move you along GravityUp.
      Set this to 0 to disable head bobbing.
      This must always be < 1.0. For sensible effects, this should
      be rather close to 0.0.

      Of course this is meaningfull only when @link(Gravity) works. }
    property HeadBobbing: Single
      read FHeadBobbing write FHeadBobbing default DefaultHeadBobbing;

    { This controls head bobbing frequency.

      I increase HeadBobbingPosition such that
      HeadBobbingPosition increase of 1
      means that player moved horizontally by
        VectorLen(Direction) * MoveHorizontalSpeed * HeadBobbingDistance. }
    property HeadBobbingDistance: Single
      read FHeadBobbingDistance write FHeadBobbingDistance
      default DefaultHeadBobbingDistance;

    { This defines the preferred height of camera when crouching.
      This is always mutiplied to CameraPreferredHeight.
      This should always be <= 1 (CrouchHeight = 1 effectively disables
      crouching, although it's better to do this by calling MakeClear
      on Input_Crouch). }
    property CrouchHeight: Single
      read FCrouchHeight write FCrouchHeight default DefaultCrouchHeight;

    { Is player crouching right now ? }
    property IsCrouching: boolean read FIsCrouching;

    { This is CameraPreferredHeight slightly modified by head bobbing
      and crouch. It can be useful for collision detection
      between camera and something. }
    function RealCameraPreferredHeight: Single;

    { This makes a visual effect of camera falling down horizontally
      on the ground. Nice to use when player died, and you want to show
      that it's body falled on the ground.
      This works by gradually changing CameraUp such that
      it gets orthogonal to GravityUp. }
    procedure FallOnTheGround;

    { @true when the effect caused by FallOnTheGround is stil in motion. }
    property FallingOnTheGround: boolean read FFallingOnTheGround;

    { This is @true when gravity works (that is @link(Gravity) is @true),
      and player is standing stable on the ground. This is set in every Idle.

      You can use this e.g. to make some effects when player is on some
      special ground (standing or walking), e.g. hurt player when he's
      standing on some toxical ground.

      @seealso IsWalkingOnTheGround }
    property IsOnTheGround: boolean read FIsOnTheGround;

    { This is @true when gravity works (that is @link(Gravity) is @true),
      and player is standing stable on the ground, and player is moving
      horizontally. In other words, this is like "IsOnTheGround and (s)he's
      walking". This is set in every Idle.

      The intention is that you can use this to make
      some "footsteps" sound for the player. }
    property IsWalkingOnTheGround: boolean read FIsWalkingOnTheGround;
  end;

{ See TMatrixWalker.CorrectCameraPreferredHeight.
  This is a global version, sometimes may be useful. }
procedure CorrectCameraPreferredHeight(var CameraPreferredHeight: Single;
  const CameraRadius: Single; const CrouchHeight, HeadBobbing: Single);

const
  MouseButtonStr: array [TMouseButton] of string = ('left', 'middle', 'right');

implementation

uses Math, KambiStringUtils;

{ TInputShortcut ------------------------------------------------------------- }

constructor TInputShortcut.Create(AKey1: TKey; AKey2: TKey; ACharacter: Char;
  AMouseButtonUse: boolean; AMouseButton: TMouseButton);
begin
  inherited Create;
  FDefaultKey1 := AKey1;
  FDefaultKey2 := AKey2;
  FDefaultCharacter := ACharacter;
  FDefaultMouseButtonUse := AMouseButtonUse;
  FDefaultMouseButton := AMouseButton;
  MakeDefault;
end;

procedure TInputShortcut.MakeDefault;
begin
  AssignFromDefault(Self);
end;

procedure TInputShortcut.AssignFromDefault(Source: TInputShortcut);
begin
  FKey1 := Source.DefaultKey1;
  FKey2 := Source.DefaultKey2;
  FCharacter := Source.DefaultCharacter;
  FMouseButtonUse := Source.DefaultMouseButtonUse;
  FMouseButton := Source.DefaultMouseButton;

  { I don't set here properties, but directly set FXxx fields,
    so that I can call Changed only once. }
  Changed;
end;

procedure TInputShortcut.Assign(Source: TInputShortcut; CopyDefaults: boolean);
begin
  if CopyDefaults then
  begin
    DefaultKey1 := Source.DefaultKey1;
    DefaultKey2 := Source.DefaultKey2;
    DefaultCharacter := Source.DefaultCharacter;
    DefaultMouseButtonUse := Source.DefaultMouseButtonUse;
    DefaultMouseButton := Source.DefaultMouseButton;
  end;

  FKey1 := Source.Key1;
  FKey2 := Source.Key2;
  FCharacter := Source.Character;
  FMouseButtonUse := Source.MouseButtonUse;
  FMouseButton := Source.MouseButton;

  { I don't set here properties, but directly set FXxx fields,
    so that I can call Changed only once. }
  Changed;
end;

procedure TInputShortcut.MakeClear;
begin
  FKey1 := K_None;
  FKey2 := K_None;
  FCharacter := #0;
  FMouseButtonUse := false;

  { I don't set here properties, but directly set FXxx fields,
    so that I can call Changed only once. }
  Changed;
end;

function TInputShortcut.IsPressed(KeysDown: PKeysBooleans;
  CharactersDown: PCharactersBooleans;
  const MousePressed: TMouseButtons): boolean;
begin
  Result :=
    ( (KeysDown <> nil) and (KeysDown^[Key1] or KeysDown^[Key2]) ) or
    ( (CharactersDown <> nil ) and (CharactersDown^[Character]) ) or
    ( MouseButtonUse and (MouseButton in MousePressed) );
end;

function TInputShortcut.IsKey(Key: TKey; ACharacter: Char): boolean;
begin
  Result :=
    ( (Key <> K_None) and ( (Key = Key1) or (Key = Key2) ) ) or
    ( (Character <> #0) and (Character = ACharacter) );
end;

function TInputShortcut.IsMouseButton(AMouseButton: TMouseButton): boolean;
begin
  Result := MouseButtonUse and (AMouseButton = MouseButton);
end;

function TInputShortcut.IsEvent(MouseEvent: boolean; Key: TKey;
  ACharacter: Char;
  AMouseButton: TMouseButton): boolean;
begin
  if MouseEvent then
    Result := IsMouseButton(AMouseButton) else
    Result := IsKey(Key, ACharacter);
end;

function TInputShortcut.Description(const NoneString: string): string;
begin
  Result := '';

  { It's important for this description to be really compact (as it's
    used in situations like menu items (see "The Castle")), that's why
    I mess with checking various cases and trying to make shorter string
    for this. }

  if (Key1 <> K_None) or (Key2 <> K_None) then
  begin
    if (Key1 <> K_None) and (Key2 <> K_None) then
      Result := Format('key "%s" or "%s"', [KeyToStr(Key1), KeyToStr(Key2)]) else
    if Key1 <> K_None then
      Result += Format('key "%s"', [KeyToStr(Key1)]) else
      Result += Format('key "%s"', [KeyToStr(Key2)]);
  end;

  if Character <> #0 then
  begin
    if Result <> '' then Result += ' or ';
    Result += Format('char "%s"', [CharToNiceStr(Character)]);
  end;

  if MouseButtonUse then
  begin
    if Result <> '' then Result += ' or ';
    Result += Format('mouse "%s"', [MouseButtonStr[MouseButton]]);
  end;

  if Result = '' then
    Result := NoneString;
end;

procedure TInputShortcut.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TInputShortcut.SetKey1(const Value: TKey);
begin
  FKey1 := Value;
  Changed;
end;

procedure TInputShortcut.SetKey2(const Value: TKey);
begin
  FKey2 := Value;
  Changed;
end;

procedure TInputShortcut.SetCharacter(const Value: Char);
begin
  FCharacter := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButtonUse(const Value: boolean);
begin
  FMouseButtonUse := Value;
  Changed;
end;

procedure TInputShortcut.SetMouseButton(const Value: TMouseButton);
begin
  FMouseButton := Value;
  Changed;
end;

{ TMatrixNavigator ------------------------------------------------------------ }

procedure TMatrixNavigator.MatrixChanged;
begin
 if Assigned(OnMatrixChanged) then OnMatrixChanged(Self);
end;

constructor TMatrixNavigator.Create(
  const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
begin
 inherited Create;
 OnMatrixChanged := AOnMatrixChanged;
end;

function TMatrixNavigator.KeyDown(key: TKey; c: char;
  KeysDown: PKeysBooleans): boolean;
begin
  Result := false;
end;

function TMatrixNavigator.MouseDown(Button: TMouseButton): boolean;
begin
  Result := false;
end;

{ TMatrixExaminer ------------------------------------------------------------ }

constructor TMatrixExaminer.Create(
  const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
type
  T3BoolKeys = array [0..2, boolean] of TKey;
const
  DefaultInputs_Move: T3BoolKeys =
    ((K_Left, K_Right), (K_Down, K_Up), (K_PageDown, K_PageUp));
  DefaultInputs_Rotate: T3BoolKeys =
    ((K_Up, K_Down), (K_Left, K_Right), (K_PageDown, K_PageUp));
var
  I: Integer;
  B: boolean;
begin
  inherited;

  FScaleFactor := 1;
  FModelBox := EmptyBox3d;

  for I := 0 to 2 do
    for B := false to true do
    begin
      FInputs_Move[I, B] := TInputShortcut.Create(DefaultInputs_Move[I, B],
        K_None, #0, false, mbLeft);
      FInputs_Rotate[I, B] := TInputShortcut.Create(DefaultInputs_Rotate[I, B],
        K_None, #0, false, mbLeft);
    end;

  { For scale larger/smaller we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  FInput_ScaleLarger  := TInputShortcut.Create(K_Numpad_Plus , K_None, '+', false, mbLeft);
  FInput_ScaleSmaller := TInputShortcut.Create(K_Numpad_Minus, K_None, '-', false, mbLeft);

  FInput_Home         := TInputShortcut.Create(K_Home        , K_None, #0 , false, mbLeft);
  FInput_StopRotating := TInputShortcut.Create(K_Space       , K_None, #0 , true , mbLeft);
end;

destructor TMatrixExaminer.Destroy;
var
  I: Integer;
  B: boolean;
begin
  for I := 0 to 2 do
    for B := false to true do
    begin
      FreeAndNil(FInputs_Move[I, B]);
      FreeAndNil(FInputs_Rotate[I, B]);
    end;
  FreeAndNil(FInput_ScaleLarger);
  FreeAndNil(FInput_ScaleSmaller);
  FreeAndNil(FInput_Home);
  FreeAndNil(FInput_StopRotating);
  inherited;
end;

function TMatrixExaminer.Matrix: TMatrix4Single;
begin
 result := TranslationMatrix(VectorAdd(MoveAmount, FModelBoxMiddle));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[0], Vector3Single(1, 0, 0)));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[1], Vector3Single(0, 1, 0)));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[2], Vector3Single(0, 0, 1)));
 result := MultMatrices(result, ScalingMatrix(Vector3Single(ScaleFactor, ScaleFactor, ScaleFactor)));
 result := MultMatrices(result, TranslationMatrix(VectorNegate(FModelBoxMiddle)));
end;

function TMatrixExaminer.RotationOnlyMatrix: TMatrix4Single;
begin
 result := RotationMatrixDeg(RotationsAngle[0], Vector3Single(1, 0, 0));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[1], Vector3Single(0, 1, 0)));
 result := MultMatrices(result, RotationMatrixDeg(RotationsAngle[2], Vector3Single(0, 0, 1)));
end;

procedure TMatrixExaminer.Idle(const CompSpeed: Single;
  KeysDown: PKeysBooleans;
  CharactersDown: PCharactersBooleans;
  const MousePressed: TMouseButtons);
var i: integer;
    move_change, rot_speed_change, scale_change: Single;
    ModsDown: TModifierKeys;
begin
  ModsDown := ModifiersDown(KeysDown);

  { porownujemy RotationsAngle z (0, 0, 0). W ten sposob jezeli wywolales ostatnio
    StopRotating to teraz nie bedziemy w Idle ciagle generowac MatrixChanged. }
  if (RotationsSpeed[0] <> 0) or
     (RotationsSpeed[1] <> 0) or
     (RotationsSpeed[2] <> 0) then
  begin
    for i := 0 to 2 do RotationsAngle[i] += RotationsSpeed[i]* CompSpeed * 50;
    MatrixChanged;
  end;

  if IsEmptyBox3d(ModelBox) then
    move_change := CompSpeed else
    move_change := Box3dAvgSize(ModelBox) * CompSpeed;
  rot_speed_change := 5 * CompSpeed;

  { we will apply CompSpeed to scale_change later }
  scale_change := 1.5;

  if ModsDown = [mkCtrl] then
  begin
    for i := 0 to 2 do
    begin
      if Inputs_Move[i, true ].IsPressed(KeysDown, CharactersDown, MousePressed) then
        Move(i, +move_change);
      if Inputs_Move[i, false].IsPressed(KeysDown, CharactersDown, MousePressed) then
        Move(i, -move_change);
    end;
  end else
  if ModsDown = [] then
  begin
    for i := 0 to 2 do
    begin
      if Inputs_Rotate[i, true ].IsPressed(KeysDown, CharactersDown, MousePressed) then
        Rotate(i, +rot_speed_change);
      if Inputs_Rotate[i, false].IsPressed(KeysDown, CharactersDown, MousePressed) then
        Rotate(i, -rot_speed_change);
    end;
  end;

  if Input_ScaleLarger.IsPressed(KeysDown, CharactersDown, MousePressed) then
    Scale(Power(scale_change, CompSpeed));
  if Input_ScaleSmaller.IsPressed(KeysDown, CharactersDown, MousePressed) then
    Scale(Power(1 / scale_change, CompSpeed));
end;

procedure TMatrixExaminer.StopRotating;
begin FRotationsSpeed := ZeroVector3Single; MatrixChanged; end;

procedure TMatrixExaminer.Rotate(coord: integer; const SpeedChange: Single);
begin FRotationsSpeed[coord] += SpeedChange; MatrixChanged; end;

procedure TMatrixExaminer.Scale(const ScaleBy: Single);
begin FScaleFactor *= ScaleBy; MatrixChanged; end;

procedure TMatrixExaminer.Move(coord: integer; const MoveDistance: Single);
begin FMoveAmount[coord] += MoveDistance; MatrixChanged; end;

procedure TMatrixExaminer.Init(const AModelBox: TBox3d);
begin
 ModelBox := AModelBox;
 Home;
end;

{ TMatrixExaminer.Set* properties }

procedure TMatrixExaminer.SetRotationsSpeed(const Value: TVector3Single);
begin FRotationsSpeed := Value; MatrixChanged; end;

procedure TMatrixExaminer.SetRotationsAngle(const Value: TVector3Single);
begin FRotationsAngle := Value; MatrixChanged; end;

procedure TMatrixExaminer.SetScaleFactor(const Value: Single);
begin FScaleFactor := Value; MatrixChanged; end;

procedure TMatrixExaminer.SetMoveAmount(const Value: TVector3Single);
begin FMoveAmount := Value; MatrixChanged; end;

procedure TMatrixExaminer.Home;
begin
 if IsEmptyBox3d(FModelBox) then
  FMoveAmount := Vector3Single(0, 0, 0) { any dummy value } else
  FMoveAmount := VectorAdd(
    VectorNegate(FModelBoxMiddle),
    Vector3Single(0, 0, -Box3dAvgSize(FModelBox)*2));
 FRotationsAngle := ZeroVector3Single;
 FRotationsSpeed := ZeroVector3Single;
 FScaleFactor := 1.0;

 MatrixChanged;
end;

procedure TMatrixExaminer.SetModelBox(const Value: TBox3d);
begin
  FModelBox := Value;
  if IsEmptyBox3d(FModelBox) then
    FModelBoxMiddle := Vector3Single(0, 0, 0) { any dummy value } else
    FModelBoxMiddle := Box3dMiddle(FModelBox);
  MatrixChanged;
end;

function TMatrixExaminer.EventDown(MouseEvent: boolean; Key: TKey;
  ACharacter: Char;
  AMouseButton: TMouseButton): boolean;
begin
  if Input_StopRotating.IsEvent(MouseEvent, Key, ACharacter, AMouseButton) then
  begin
    StopRotating;
    Result := true;
  end else
  if Input_Home.IsEvent(MouseEvent, Key, ACharacter, AMouseButton) then
  begin
    Home;
    Result := true;
  end else
    Result := false;
end;

function TMatrixExaminer.KeyDown(key: TKey; c: char;
  KeysDown: PKeysBooleans): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if ModifiersDown(KeysDown) <> [] then Exit;

  Result := EventDown(false, Key, C, mbLeft);
end;

function TMatrixExaminer.MouseDown(Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := EventDown(true, K_None, #0, Button);
end;

function TMatrixExaminer.MouseMove(OldX, OldY, NewX, NewY: Integer;
  const MousePressed: TMouseButtons; KeysDown: PKeysBooleans): boolean;
var
  Size: Single;
  ModsDown: TModifierKeys;
begin
  Result := false;

  { Shortcuts: I'll try to make them intelligent, which means
    "mostly matching shortcuts in other programs" (like Blender) and
    "accessible to all users" (which means that e.g. I don't want to use
    middle mouse button, as many users have only 2 mouse buttons (or even 1),
    besides GNOME hig says users seldom try out other than the 1st button).

    Let's check what others use:

    Blender:
    - rotating: on bmMiddle
    - moving left/right/down/up: on Shift + mbMiddle
    - moving closer/further: on Ctrl + mbMiddle
      (moving down brings closer, up brings further; horizontal move ignored)
    Both Shift and Ctrl pressed do nothing.

    vrweb:
    - rotating: mbMiddle
    - moving closer/further: mbRight (like in Blender: down closer, up further,
      horizontal doesn't matter)
    - moving left/right/down/up: mbLeft

    GIMP normalmap 3d preview:
    - rotating: mbLeft
    - moving closer/further: mbRight (like in Blender: down closer, up further,
      horizontal doesn't matter)
    - no moving left/right/down/up.

    My thoughts and conclusions:
    - rotating seems most natural in Examine mode (that's where this navigation
      mode is the most comfortable), so it should be on bmLeft (like normalmap)
      with no modifiers (like Blender).
    - moving closer/further: 2nd most important in Examine mode, in my opinion.
      Goes to mbRight. For people with 1 mouse button, and Blender analogy,
      it's also on Ctrl + mbLeft.
    - moving left/right/down/up: mbMiddle.
      For people with no middle button, and Blender analogy, it's also on
      Shift + mbLeft.

    This achieves a couple of nice goals:
    - everything is available with only mbLeft, for people with 1 mouse button.
    - Blender analogy: you can say to just switch "mbMiddle" to "mbLeft",
      and it works the same
    - OTOH, for people with 3 mouse buttons, that do not catch the fact that
      keyboard modifiers change the navigation, also each mb (without modifier)
      does something different.
  }

  { Optimization, since MouseMove occurs very often: when nothing pressed,
    do nothing. }
  if MousePressed = [] then Exit;

  ModsDown := ModifiersDown(KeysDown) * [mkShift, mkCtrl];

  { Rotating }
  if (mbLeft in MousePressed) and (ModsDown = []) then
  begin
    FRotationsAngle[1] += (NewX - OldX) / 2;
    FRotationsAngle[0] += (NewY - OldY) / 2;
    MatrixChanged;
    Result := true;
  end else

  { Moving uses box size, so requires non-empty box. }

  { Note: checks for (ModsDown = []) are not really needed below,
    mkRight / Middle don't serve any other purpose anyway.
    But I think that it improves user ability to "discover" these shortcuts
    and keys, otherwise it seems strange that shift/ctrl change the
    meaning of mbLeft but they don't change the meaning of mbRight / Middle ? }

  { Moving closer/further }
  if (not IsEmptyBox3d(FModelBox)) and
     ( ( (mbRight in MousePressed) and (ModsDown = []) ) or
       ( (mbLeft in MousePressed) and (ModsDown = [mkCtrl]) ) ) then
  begin
    Size := Box3dAvgSize(FModelBox);
    FMoveAmount[2] += Size * (NewY - OldY) / 200;
    MatrixChanged;
    Result := true;
  end;

  { Moving left/right/down/up }
  if (not IsEmptyBox3d(FModelBox)) and
     ( ( (mbMiddle in MousePressed) and (ModsDown = []) ) or
       ( (mbLeft in MousePressed) and (ModsDown = [mkShift]) ) ) then
  begin
    Size := Box3dAvgSize(FModelBox);
    FMoveAmount[0] -= Size * (OldX - NewX) / 200;
    FMoveAmount[1] -= Size * (NewY - OldY) / 200;
    MatrixChanged;
    Result := true;
  end;
end;

{ TMatrixWalker ---------------------------------------------------------------- }

constructor TMatrixWalker.Create(
  const AOnMatrixChanged: TMatrixNavigatorNotifyFunc);
begin
  inherited;
  FInitialCameraPos := Vector3Single(0, 0, 0);  FCameraPos := InitialCameraPos;
  FInitialCameraDir := Vector3Single(0, 0, -1); FCameraDir := InitialCameraDir;
  FInitialCameraUp  := Vector3Single(0, 1, 0);  FCameraUp  := InitialCameraUp;

  FMoveHorizontalSpeed := 1;
  FMoveVerticalSpeed := 1;
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FRotationVerticalSpeed := DefaultRotationVerticalSpeed;
  FFallingDownStartSpeed := DefaultFallingDownStartSpeed;
  FFallingDownSpeedIncrease := DefaultFallingDownSpeedIncrease;
  FPreferGravityUpForRotations := true;
  FPreferGravityUpForMoving := true;
  FGravity := false;
  FGrowingSpeed := DefaultGrowingSpeed;
  FFallingDownEffect := true;
  FIsJumping := false;
  FHeadBobbing := DefaultHeadBobbing;
  FCrouchHeight := DefaultCrouchHeight;
  FMaxJumpHeight := DefaultMaxJumpHeight;
  FMinAngleRadFromGravityUp := DefaultMinAngleRadFromGravityUp;
  FAllowSlowerRotations := true;
  FCheckModsDown := true;
  FMouseLookHorizontalSensitivity := DefaultMouseLookHorizontalSensitivity;
  FMouseLookVerticalSensitivity := DefaultMouseLookVerticalSensitivity;
  FHeadBobbingDistance := DefaultHeadBobbingDistance;
  FJumpSpeedMultiply := DefaultJumpSpeedMultiply;
  FJumpPower := DefaultJumpPower;
  FInvertVerticalMouseLook := false;

  FInput_Forward      := TInputShortcut.Create(K_Up          , K_None, #0, false, mbLeft);
  FInput_Backward     := TInputShortcut.Create(K_Down        , K_None, #0, false, mbLeft);
  FInput_LeftRot      := TInputShortcut.Create(K_Left        , K_None, #0, false, mbLeft);
  FInput_RightRot     := TInputShortcut.Create(K_Right       , K_None, #0, false, mbLeft);
  FInput_LeftStrafe   := TInputShortcut.Create(K_Comma       , K_None, #0, false, mbLeft);
  FInput_RightStrafe  := TInputShortcut.Create(K_Period      , K_None, #0, false, mbLeft);
  FInput_UpRotate     := TInputShortcut.Create(K_PageUp      , K_None, #0, false, mbLeft);
  FInput_DownRotate   := TInputShortcut.Create(K_PageDown    , K_None, #0, false, mbLeft);
  FInput_UpMove       := TInputShortcut.Create(K_Insert      , K_None, #0, false, mbRight);
  FInput_DownMove     := TInputShortcut.Create(K_Delete      , K_None, #0, false, mbLeft);
  FInput_GravityUp    := TInputShortcut.Create(K_Home        , K_None, #0, false, mbLeft);

  { For move speed inc/dev we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  FInput_MoveSpeedInc := TInputShortcut.Create(K_Numpad_Plus , K_None, '+', false, mbLeft);
  FInput_MoveSpeedDec := TInputShortcut.Create(K_Numpad_Minus, K_None, '-', false, mbLeft);

  FInput_Jump         := TInputShortcut.Create(K_A           , K_None, #0, false, mbRight);
  FInput_Crouch       := TInputShortcut.Create(K_Z           , K_None, #0, false, mbLeft);

  FProjectionMatrix := IdentityMatrix4Single;
end;

destructor TMatrixWalker.Destroy;
begin
  FreeAndNil(FInput_Forward);
  FreeAndNil(FInput_Backward);
  FreeAndNil(FInput_LeftRot);
  FreeAndNil(FInput_RightRot);
  FreeAndNil(FInput_LeftStrafe);
  FreeAndNil(FInput_RightStrafe);
  FreeAndNil(FInput_UpRotate);
  FreeAndNil(FInput_DownRotate);
  FreeAndNil(FInput_UpMove);
  FreeAndNil(FInput_DownMove);
  FreeAndNil(FInput_GravityUp);
  FreeAndNil(FInput_MoveSpeedInc);
  FreeAndNil(FInput_MoveSpeedDec);
  FreeAndNil(FInput_Jump);
  FreeAndNil(FInput_Crouch);
  inherited;
end;

function TMatrixWalker.Matrix: TMatrix4Single;
begin
  { Yes, below we compare Fde_CameraUpRotate with 0.0 using normal
    (precise) <> operator. Don't worry --- Fde_Stabilize in Idle
    will take care of eventually setting Fde_CameraUpRotate to
    a precise 0.0. }
  if Fde_CameraUpRotate <> 0.0 then
    Result := LookDirMatrix(CameraPos, CameraDir,
      RotatePointAroundAxisDeg(Fde_CameraUpRotate, CameraUp, CameraDir)) else
    Result := LookDirMatrix(CameraPos, CameraDir, CameraUp);
end;

function TMatrixWalker.RotationOnlyMatrix: TMatrix4Single;
begin
 result := LookDirMatrix(ZeroVector3Single, CameraDir, CameraUp);
end;

function TMatrixWalker.DoMoveAllowed(const ProposedNewPos: TVector3Single;
  out NewPos: TVector3Single; const BecauseOfGravity: boolean): boolean;
begin
 if Assigned(OnMoveAllowed) then
  Result := OnMoveAllowed(Self, ProposedNewPos, NewPos, BecauseOfGravity) else
 begin
  Result := true;
  NewPos := ProposedNewPos;
 end;
end;

procedure TMatrixWalker.DoGetCameraHeight(
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
begin
  IsAboveTheGround := false;
  if Assigned(OnGetCameraHeight) then
    OnGetCameraHeight(Self, IsAboveTheGround, SqrHeightAboveTheGround);
end;

function TMatrixWalker.UseHeadBobbing: boolean;
begin
  Result := Gravity and (HeadBobbing <> 0.0);
end;

function TMatrixWalker.RealCameraPreferredHeightNoHeadBobbing: Single;
begin
  Result := CameraPreferredHeight;

  if IsCrouching then
    Result *= CrouchHeight;
end;

function TMatrixWalker.RealCameraPreferredHeight: Single;
var
  BobbingModifier: Single;
begin
  Result := RealCameraPreferredHeightNoHeadBobbing;

  if UseHeadBobbing then
  begin
    { HeadBobbingPosition = 0 means that head is at lowest position.
      HeadBobbingPosition = 0.5 means that head is at highest position.
      HeadBobbingPosition = 1.0 means that head is at lowest position again.

      Larger HeadBobbingPosition work like Frac(HeadBobbingPosition)
      (i.e. function HeadBobbingPosition -> BobbingModifier
      is periodic with period = 1.0). }

    BobbingModifier := Frac(HeadBobbingPosition);

    if BobbingModifier <= 0.5 then
      BobbingModifier := MapRange(BobbingModifier, 0.0, 0.5, -1, +1) else
      BobbingModifier := MapRange(BobbingModifier, 0.5, 1.0, +1, -1);

    { Most game tutorials and codes advice that head bobbing be done with sinus,
      as below. But actually I found that the visual difference between
      sin-based head bobbing and linear-based (like above) head bobbing
      is not noticeable, so I'm using linear-based right now (as it's
      a little faster --- no trig calculation needed, although this
      could be avoided with sinus lookup table).

      If however you prefer sin-based head bobbing, uncomment line below
      and comment out 3 lines "if BobbingModifier <= 0.5 then ...." above.

    BobbingModifier := Sin(BobbingModifier * 2 * Pi);
    }

    BobbingModifier *= Result * HeadBobbing;
    Result += BobbingModifier;
  end;
end;

function TMatrixWalker.RealCameraPreferredHeightMargin: Single;
begin
  { I tried using here something smaller like
    SingleEqualityEpsilon, but this was not good. }
  Result := RealCameraPreferredHeight * 0.01;
end;

procedure TMatrixWalker.RotateAroundGravityUp(const AngleDeg: Single);
var Axis: TVector3Single;
begin
 { nie obracamy cameraDir wokol cameraUp, takie obroty w polaczeniu z
   obrotami vertical moglyby sprawic ze kamera staje sie przechylona w
   stosunku do plaszczyny poziomu (plaszczyzny dla ktorej wektorem normalnym
   jest GravityUp) (a my chcemy zeby zawsze plaszczyzna wyznaczana przez
   wektory Dir i Up byla prostopadla do plaszczyzny poziomu - bo to po prostu
   daje wygodniejsze sterowanie (chociaz troche bardziej ograniczone -
   jestesmy wtedy w jakis sposob uwiazani do plaszczyzny poziomu)).

   Acha, i jeszcze jedno : zeby trzymac zawsze obroty w ta sama strone
   (ze np. strzalka w lewo zawsze powoduje ze swiat ze obraca w prawo
   wzgledem nas) musze czasami obracac sie wokol GravityUp, a czasem
   wokol -GravityUp.
 }
 if AngleRadBetweenVectors(CameraUp, GravityUp) > Pi/2 then
  Axis := VectorNegate(GravityUp) else
  Axis := GravityUp;

 FCameraUp := RotatePointAroundAxisDeg(AngleDeg, CameraUp, Axis);
 FCameraDir := RotatePointAroundAxisDeg(AngleDeg, CameraDir, Axis);

 MatrixChanged;
end;

procedure TMatrixWalker.RotateAroundUp(const AngleDeg: Single);
begin
 { W TYM MIEJSCU POTRZEBUJEMY aby cameraDir i cameraUp byly prostopadle ! }
 CameraDir := RotatePointAroundAxisDeg(AngleDeg, CameraDir, CameraUp);
end;

procedure TMatrixWalker.RotateHorizontal(const AngleDeg: Single);
begin
  if PreferGravityUpForRotations then
    RotateAroundGravityUp(AngleDeg) else
    RotateAroundUp(AngleDeg);
end;

procedure TMatrixWalker.RotateVertical(const AngleDeg: Single);
var
  Side: TVector3Single;
  AngleRad: Single;

  procedure DoRealRotate;
  begin
    { Rotate CameraUp around Side }
    FCameraUp := RotatePointAroundAxisRad(AngleRad, CameraUp,  Side);
    { Rotate CameraDir around Side }
    FCameraDir := RotatePointAroundAxisRad(AngleRad, CameraDir, Side);
  end;

var
  AngleRadBetween: Single;
begin
  AngleRad := DegToRad(AngleDeg);

  if PreferGravityUpForRotations and (MinAngleRadFromGravityUp <> 0.0) then
  begin
    Side := VectorProduct(CameraDir, GravityUp);
    if IsZeroVector(Side) then
    begin
      { Brutally adjust CameraDir and CameraUp to be correct.
        This should happen only if your code was changing values of
        PreferGravityUpForRotations and MinAngleRadFromGravityUp at runtime.
        E.g. first you let CameraDir and CameraUp to be incorrect,
        and then you set PreferGravityUpForRotations to @true and
        MinAngleRadFromGravityUp
        to > 0 --- and suddenly we find that CameraUp can be temporarily bad. }
      FCameraDir := InitialCameraDir;
      FCameraUp := InitialCameraUp;

      { Now check Side again. If it's still bad, this means that the
        InitialCameraDir is parallel to GravityUp. This shouldn't
        happen if you correctly set InitialCameraDir and GravityUp.
        So just pick any sensible FCameraDir to satisfy MinAngleRadFromGravityUp
        for sure.

        This is a common problem on some VRML models:
        - You wanted to place your camera such that camera looking direction
          is in +Y or -Y (and camera up is e.g. +Z).
        - You did this by using untransformed PerspectiveCamera/Viewpoint node.
        But VRML (2.0 spec, I also do this in VMRL 1.0)
        gravity is set by transforming (0, 1, 0) by PerspectiveCamera/Viewpoint
        node transformation.
        So the above will mean that gravity vector is parallel to your
        looking direction. }
      Side := VectorProduct(CameraDir, GravityUp);
      if IsZeroVector(Side) then
      begin
        FCameraDir := AnyPerpVector(GravityUp);
        FCameraUp := GravityUp;
      end;
    end else
    begin
      { Calculate AngleRadBetween, and possibly adjust AngleRad. }
      AngleRadBetween := AngleRadBetweenVectors(CameraDir, GravityUp);
      if AngleRadBetween - AngleRad < MinAngleRadFromGravityUp then
        AngleRad := AngleRadBetween - MinAngleRadFromGravityUp else
      if AngleRadBetween - AngleRad > Pi - MinAngleRadFromGravityUp then
        AngleRad := AngleRadBetween - (Pi - MinAngleRadFromGravityUp);

      DoRealRotate;
    end;
  end else
  begin
    Side := VectorProduct(CameraDir, CameraUp);
    DoRealRotate;
  end;

  MatrixChanged;
end;

procedure TMatrixWalker.Idle(const CompSpeed: Single;
  KeysDown: PKeysBooleans;
  CharactersDown: PCharactersBooleans;
  const MousePressed: TMouseButtons);

  { Like Move, but you pass here final ProposedNewPos }
  function MoveTo(const ProposedNewPos: TVector3Single;
    const BecauseOfGravity: boolean): boolean;
  var
    NewPos: TVector3Single;
  begin
    Result := DoMoveAllowed(ProposedNewPos, NewPos, BecauseOfGravity);
    if Result then
      { Note that setting CameraPos automatically calls MatrixChanged }
      CameraPos := NewPos;
  end;

  { Tries to move CameraPos to CameraPos + MoveVector.
    Returns DoMoveAllowed result. So if it returns @false,
    you know that CameraPos didn't change (on the other hand,
    if it returns @true, you don't know anything --- maybe CameraPos
    didn't change, maybe it changed to CameraPos + MoveVector,
    maybe it changed to something different). }
  function Move(const MoveVector: TVector3Single;
    const BecauseOfGravity: boolean): boolean;
  begin
    Result := MoveTo(VectorAdd(CameraPos, MoveVector), BecauseOfGravity);
  end;

var
  { This is initally false. It's used by MoveHorizontal while head bobbing,
    to avoid updating HeadBobbingPosition more than once in the same Idle call.

    Updating it more than once is bad --- try e.g. holding Input_Forward
    with one of the strafe keys: you move and it's very noticeable
    that HeadBobbing seems faster. That's because
    when holding both Input_Forward and Input_StrafeRight, you shouldn't
    do HeadBobbing twice in one Idle --- you should do it only Sqrt(2).
    When you will also hold Input_RotateRight at the same time --- situation
    gets a little complicated...

    The good solution seems to just do head bobbing only once.
    In some special cases this means that head bobbing will be done
    *less often* than it should be, but this doesn't hurt. }
  HeadBobbingAlreadyDone: boolean;

  { MoveHorizontal call sets this to @true to indicate that some
    horizontal move was done. }
  MoveHorizontalDone: boolean;

  { Multiply must be +1 or -1 }
  procedure MoveHorizontal(const Multiply: Integer = 1);
  var
    Direction: TVector3Single;
  var
    AJumpMultiply: Single;
  begin
    if IsJumping then
      AJumpMultiply := JumpSpeedMultiply else
      AJumpMultiply := 1.0;

    { Update HeadBobbingPosition }
    if (not IsJumping) and UseHeadBobbing and (not HeadBobbingAlreadyDone) then
    begin
      HeadBobbingPosition += CompSpeed * 50 / HeadBobbingDistance;
      HeadBobbingAlreadyDone := true;
    end;

    MoveHorizontalDone := true;

    if PreferGravityUpForMoving then
      Direction := CameraDirInGravityPlane else
      Direction := CameraDir;

    Move(VectorScale(Direction,
      MoveHorizontalSpeed * CompSpeed * 50 * Multiply * AJumpMultiply), false);
  end;

  procedure MoveVertical(const Multiply: Integer);

    procedure MoveVerticalCore(const PreferredUpVector: TVector3Single);
    begin
      Move(VectorScale(PreferredUpVector,
        { VectorLen(CameraDir) / VectorLen(PreferredUpVector) * }
        Sqrt(VectorLenSqr(CameraDir) / VectorLenSqr(PreferredUpVector)) *
        MoveVerticalSpeed * CompSpeed * 50 * Multiply), false);
    end;

  begin
    if PreferGravityUpForMoving then
      MoveVerticalCore(GravityUp) else
      MoveVerticalCore(CameraUp);
  end;

  { This is just like RotateHorizontal, but it uses
    PreferGravityUpForMoving to decide which rotation to use.
    This way when PreferGravityUpForMoving, then we rotate versus GravityUp,
    move in GravityUp plane, and then rotate back versus GravityUp.
    If not PreferGravityUpForMoving, then we do all this versus CameraUp.
    And so everything works. }
  procedure RotateHorizontalForStrafeMove(const AngleDeg: Single);
  begin
    if PreferGravityUpForMoving then
      RotateAroundGravityUp(AngleDeg) else
      RotateAroundUp(AngleDeg);
  end;

  procedure CheckRotates(SpeedScale: Single);
  { sprawdz czy wcisnieto KeyRight/LeftRot i jesli tak to zareaguj odpowiednio.
    Uzyj SpeedScale aby skalowac szybkosc obracania sie, tzn. defaltowa
    szybkosc obracania sie = 1.0 }
  begin
    if Input_RightRot.IsPressed(KeysDown, CharactersDown, MousePressed) then
      RotateHorizontal(-RotationHorizontalSpeed * CompSpeed * 50 * SpeedScale);
    if Input_LeftRot.IsPressed(KeysDown, CharactersDown, MousePressed) then
      RotateHorizontal(+RotationHorizontalSpeed * CompSpeed * 50 * SpeedScale);

    if Input_UpRotate.IsPressed(KeysDown, CharactersDown, MousePressed) then
      RotateVertical(+RotationVerticalSpeed * CompSpeed * 50 * SpeedScale);
    if Input_DownRotate.IsPressed(KeysDown, CharactersDown, MousePressed) then
      RotateVertical(-RotationVerticalSpeed * CompSpeed * 50 * SpeedScale);
  end;

  { Things related to gravity --- jumping, taking into account
    falling down and keeping RealCameraPreferredHeight above the ground. }
  procedure GravityIdle;
  var
    IsAboveTheGround: boolean;
    SqrHeightAboveTheGround: Single;

    function TryJump: boolean;
    var
      ThisJumpHeight: Single;
    begin
      Result := IsJumping;

      if Result then
      begin
        { jump. This means:
          1. update FJumpHeight and FJumpPower and move CameraPos
          2. or set FIsJumping to false when jump ends }

        ThisJumpHeight := MaxJumpDistance * FJumpPower * CompSpeed * 50;
        FJumpHeight += ThisJumpHeight;

        if FJumpHeight > MaxJumpDistance then
          FIsJumping := false else
        begin
          { do jumping }
          Move(VectorAdjustToLength(GravityUp, ThisJumpHeight), false);

          { Initially it was my intention to decrease FJumpPower
            at each point. But this doesn't make any nice visible effect,
            moreover it can't guarentee that every jump will sooner or later
            reach MaxJumpDistance. And we want for every jump to
            sooner or later reach MaxJumpDistance.

            FJumpPower *= Power(0.95, CompSpeed * 50);

            So the line above is commented out, and jumping is done with
            constant speed FJumpPower. So every jump sooner or later reaches
            MaxJumpDistance. }
        end;
      end;
    end;

   function TryFde_Stabilize: boolean; forward;

    { If our height above the ground is < RealCameraPreferredHeight
      then we try to "grow".

      (this may happen because of many things --- e.g. user code
      just changed CameraPreferredHeight to something larger
      (because e.g. "duck mode" ended), or we just ended falling dowm
      from high). }
    function TryGrow: boolean;
    var
      GrowingVectorLength: Single;
    begin
      Result :=
        IsAboveTheGround and
        (SqrHeightAboveTheGround <
          Sqr(RealCameraPreferredHeight - RealCameraPreferredHeightMargin));

      if Result then
      begin
        { calculate GrowingVectorLength }
        { Well, we have to resign here from operating on Sqrs,
          we need actual values. }
        GrowingVectorLength := Min(
          { TODO --- use CameraPreferredHeight here ? }
          VectorLen(CameraDir) * GrowingSpeed * CompSpeed * 50,
          RealCameraPreferredHeight - Sqrt(SqrHeightAboveTheGround));

        Move(VectorAdjustToLength(GravityUp, GrowingVectorLength), true);

        { When growing, TryFde_Stabilize also must be done.
          Otherwise when player walks horizontally on the flat surface
          for some time then "Falling down effect" activates --- because
          player is always in TryGrow or TryFallingDown. So one of them
          (TryGrow or TryFallingDown) *must* allow "Falling down effect"
          to stabilize itself. Obviously TryFallingDown can't (this would
          be against the idea of this effect) so TryGrow does it... }
        TryFde_Stabilize;
      end;
    end;

    function TryFallingDown: boolean;

      { Return +1 or -1, randomly. }
      function RandomPlusMinus: Integer;
      begin
        Result := Random(2);
        if Result = 0 then
          Result := -1;
      end;

    const
      Fde_VerticalRotateDeviation = 1.0;
      Fde_HorizontalRotateDeviation = 0.3;
    var
      CameraPosBefore: TVector3Single;
      SqrFallingDownVectorLength: Single;
    begin
      Result := false;

      { Note that if we got here, then TryGrow returned false,
        which means that (assuming OnGetCameraHeight is correctly assigned)
        we are not above the ground, or
          SqrHeightAboveTheGround >=
            Sqr(RealCameraPreferredHeight - RealCameraPreferredHeightMargin)
        However we check here for something stronger:
          SqrHeightAboveTheGround >
            Sqr(RealCameraPreferredHeight + RealCameraPreferredHeightMargin)

        This is important, because this way we avoid the unpleasant
        "bouncing" effect when in one Idle we decide that camera
        is falling down, in next Idle we decide that it's growing,
        in next Idle it falls down again etc. In TryGrow we try
        to precisely set our CameraPos, so that it hits exactly
        at RealCameraPreferredHeight -- which means that after TryGrow,
        in next Idle TryGrow should not cause growing and TryFallingDown
        should not cause falling down. }
      if IsAboveTheGround and
         (SqrHeightAboveTheGround <=
           Sqr(RealCameraPreferredHeight + RealCameraPreferredHeightMargin)) then
      begin
        FIsFallingDown := false;
        Exit;
      end;

      { Make sure that FallingDownSpeed is initialized.
        When IsFallingDown, we know it's initialized (because setting
        "FIsFallingDown := true;" is done only in the piece of code below...),
        otherwise we make sure it's set to it's starting value. }
      if not FIsFallingDown then
        FFallingDownSpeed := FallingDownStartSpeed;

      { try to fall down }
      CameraPosBefore := CameraPos;

      { calculate SqrFallingDownVectorLength.

        Note that we make sure that SqrFallingDownVectorLength is no longer
        than SqrHeightAboveTheGround --- this way we avoid the problem
        that when FFallingDownSpeed would get very big,
        we couldn't fall down any more (while in fact we should then fall down
        very quickly).

        Actually, we even do more. We make sure that
        FallingDownVectorLength is no longer than
        (HeightAboveTheGround - RealCameraPreferredHeight).
        Initially I wanted to do here
          MinTo1st(SqrFallingDownVectorLength, SqrHeightAboveTheGround);
        i.e. to allow camera to fall below RealCameraPreferredHeight.

        But this didn't work like it should. Why ?
        See above for the trick that I have to do with
        RealCameraPreferredHeightMargin above (to not cause
        "unpleasant bouncing" when swapping FallingDown and TryGrow).
        If I could fall down here below RealCameraPreferredHeight then

        1. It *will not* cause the desired "nice" effect (of automatically
           "ducking" when falling down from high), because of comparison
           (the one with RealCameraPreferredHeightMargin) above.

        2. It *will* cause the undesired unpleasant swapping between
           FallingDown and TryGrow.

        So it's totally bad thing to do.

        This means that I should limit myself to not fall down
        below RealCameraPreferredHeight. And that's what I'm doing. }
      SqrFallingDownVectorLength :=
        VectorLenSqr(CameraDir) * Sqr(FFallingDownSpeed * CompSpeed * 50);
      if IsAboveTheGround then
        MinTo1st(SqrFallingDownVectorLength,
          Sqr(Sqrt(SqrHeightAboveTheGround) - RealCameraPreferredHeight));

      if Move(VectorScale(GravityUp,
         - Sqrt(SqrFallingDownVectorLength /
             VectorLenSqr(GravityUp))), true) and
        (not VectorsPerfectlyEqual(CameraPos, CameraPosBefore)) then
      begin
        if not IsFallingDown then
        begin
          FFallingDownStartPos := CameraPosBefore;

          { Why do I init here FFallingDownSpeed ? A few lines above I did
              if not FIsFallingDown then
                FFallingDownSpeed := FallingDownStartSpeed;
            to init FFallingDownSpeed (I had to do it to calculate
            SqrFallingDownVectorLength). So why initing it again here ?

            Answer: Because Move above called MoveTo, that set CameraPos
            that actually called MatrixChanged that called OnMatrixChanged.
            And OnMatrixChanged is used callback and user could do there
            things like
            - Changing FallingDownStartSpeed (but still it's unspecified
              whether we have to apply this change, right ?)
            - Calling CancelFallingDown and *then* changing FallingDownStartSpeed.
              And in this case, we *must* honour it, because here user
              expects that we will use FallingDownStartSpeed if we want
              to fall down. (of course, one call to "Move" with old
              "FallingDownStartSpeed" was already done, that's unavoidable...). }
          FFallingDownSpeed := FallingDownStartSpeed;

          FIsFallingDown := true;
        end;

        Result := true;

        if IsAboveTheGround and
          (SqrHeightAboveTheGround < Sqr(RealCameraPreferredHeight * 1.1)) then
        begin
          { This check is needed, otherwise when you're walking down even from
            the most slight hill then you get

            1. FallingDownEffect
            2. OnFalledDown is called seldom and with large heights.

            Why ? Because MoveHorizontal calls are done between GravityIdle
            calls, and the move can be quite fast. So even though the player is
            actually quite closely following the terrain, we would constantly
            have IsFallingDown := true. Consider a large hill that is almost
            flat --- when walking down the hill, we would get IsFallingDown
            := true, FallingDownSpeed and FallingDownEffect would raise,
            and at the end OnFalledDown would be called with parameters
            like player fell down from the top of the hill to the ground
            (which can cause e.g. player losing life).

            The check for RealCameraPreferredHeight * 1.1 above and
            setting FIsFallingDown cure the situation. OnFalledDown will
            be called more often indicating very small fallen down heights,
            and FallingDownSpeed and FallingDownEffect will not be able
            to raise high as long as player follows terrain closely.

            Of course we're setting here FIsFallingDown := false even though
            the player is not exactly on the terrain --- but he's very close.
            In the next GravityIdle call we will again bring him a little
            down, set FIsFallingDown to @true, and then set it back to @false
            by line below. }
          FIsFallingDown := false;
        end else
        begin
          { This is where we do FallingDownEffect.

            Note that I do FallingDownEffect *before* increasing
            FFallingDownSpeed below.

            1. reason (ideological, not really that important...) is that
               FallingDownEffect is a penalty equivalent to FFallingDownSpeed that
               was already used --- not to the future FFallingDownSpeed.

            2. reason (practical, and real :) is that when the program
               was in some non-3d drawing state (e.g. displaying menu, or
               displaying progress bar because the VRML model was just loaded)
               then CompSpeed indicates (truly) that a lot of time elapsed
               since last Idle. This means that it's common that at the same moment
               when IsFallingDown changed suddenly to @true, CompSpeed may be large
               and we're better not using this too much... A practical bug demo:
               open in view3dscene (it does progress bar in OpenGL, so will cause
               large CompSpeed) any model with gravity on and camera slightly
               higher then CameraPreferredHeight (we want to trigger IsFallingDown
               right when the model is loaded). E.g. run "view3dscene
               kambi_vrml_test_suite/vrml_1/kambi_extensions/navigation_info_tests/speed_2.wrl".
               If FallingDownSpeedIncrease will be done before FallingDownEffect,
               then you'll see that at the very first frame FFallingDownSpeed
               was increased so much (because CompSpeed was large) that it triggered
               FallingDownEffect. Even though the falling down distance was really small...

               Maybe in the future I'll workaround it differently.
               One idea is that FFallingDownSpeed should be made smaller if the
               falled down distance is small. Or just don't call GravityIdle after the first
               model load, to avoid using large CompSpeed ?

               LATER NOTE: note that the (2.) problem above may be non-existing
               now, since we use IdleSpeed and we have IgnoreNextIdleSpeed to
               set IdleSpeed to zero in such cases. }
          if FallingDownEffect and
             (FFallingDownSpeed > FallingDownStartSpeed * 3) then
          begin
            if FFallingDownSpeed > FallingDownStartSpeed * 5 then
            begin
              if Fde_RotateHorizontal = 0 then
                Fde_RotateHorizontal := RandomPlusMinus;
              RotateAroundGravityUp(Fde_RotateHorizontal *
                Fde_HorizontalRotateDeviation * CompSpeed * 50);
            end;

            if Fde_CameraUpRotate < 0 then
              Fde_CameraUpRotate -= Fde_VerticalRotateDeviation * CompSpeed * 50 else
            if Fde_CameraUpRotate > 0 then
              Fde_CameraUpRotate += Fde_VerticalRotateDeviation * CompSpeed * 50 else
              Fde_CameraUpRotate := RandomPlusMinus *
                                    Fde_VerticalRotateDeviation * CompSpeed * 50;

            MatrixChanged;
          end;

          { Note that when changing FFallingDownSpeed below I'm using CompSpeed * 50.
            And also above when using FFallingDownSpeed, I multipled
            FFallingDownSpeed * CompSpeed * 50. This is correct:
            - changing position based on FallingDownSpeed is a "velocity"
            - changing FallingDownSpeed below is "acceleration"
            And both acceleration and velocity must be time-based. }
          if FallingDownSpeedIncrease <> 1.0 then
            FFallingDownSpeed *= Power(FallingDownSpeedIncrease, CompSpeed * 50);
        end;
      end else
        FIsFallingDown := false;
    end;

    function TryFde_Stabilize: boolean;
    const
      Fde_VerticalRotateNormalization = 7;
    var
      Change: Single;
    begin
      Result := (Fde_RotateHorizontal <> 0) or (Fde_CameraUpRotate <> 0);

      { Bring Fde_Xxx vars back to normal (zero) values. }

      Fde_RotateHorizontal := 0;

      if Fde_CameraUpRotate <> 0.0 then
      begin
        { Note that we try to immediately bring CameraUpRotate to
          range (-360, 360) here. E.g. no need to gradually bring back
          CameraUpRotate from 360.0 to 0.0 --- this doesn't cause
          any interesting visual effect (and the only reason for
          CameraUpRotate is a visual effect)... }
        Change := Trunc(Abs(Fde_CameraUpRotate) / 360.0) * 360.0 +
          Fde_VerticalRotateNormalization * CompSpeed * 50;

        if Fde_CameraUpRotate < 0 then
          Fde_CameraUpRotate := Min(Fde_CameraUpRotate + Change, 0.0) else
          Fde_CameraUpRotate := Max(Fde_CameraUpRotate - Change, 0.0);

        MatrixChanged;
      end;
    end;

    function TryFallingOnTheGround: boolean;
    var
      Angle, AngleRotate: Single;
    begin
      Result := FFallingOnTheGround;
      if not Result then
        Exit;

      Angle := AngleRadBetweenVectors(CameraUp, GravityUp);

      if FloatsEqual(Angle, HalfPi) then
      begin
        { FallingOnTheGround effect stops here. }
        FFallingOnTheGround := false;
        Exit;
      end;

      AngleRotate := 0.1 * CompSpeed * 50;
      MinTo1st(AngleRotate, Abs(Angle - HalfPi));
      if not FFallingOnTheGroundAngleIncrease then
        AngleRotate := -AngleRotate;

      CameraUp := RotatePointAroundAxisRad(AngleRotate, CameraUp,
        CameraDirInGravityPlane);
    end;

    procedure DoFalledDown;
    var
      BeginPos, EndPos, EndToBegin: TVector3Single;
      Coord: Integer;
    begin
      if Assigned(OnFalledDown) then
      begin
        { Note that I project CameraPos and FFallingDownStartPos
          onto GravityUp vector to calculate FalledHeight. }
        BeginPos := PointOnLineClosestToPoint(ZeroVector3Single, GravityUp, FFallingDownStartPos);
        EndPos   := PointOnLineClosestToPoint(ZeroVector3Single, GravityUp, CameraPos);
        EndToBegin := VectorSubtract(BeginPos, EndPos);

        { Now check that EndToBegin points in the same direction as GravityUp.
          If not, then EndPos is actually *higher* than BeginPos,
          so we were not really falling down. That can happen, various growing
          and jumping things can cause such "false flying". For OnFalledDown
          only the real falling down (from somewhere higher to lower) should
          be reported. }
        Coord := MaxAbsVectorCoord(EndToBegin);
        if (EndToBegin[Coord] >= 0) <> (GravityUp[Coord] >= 0) then
          Exit;

        OnFalledDown(Self, VectorLen(EndToBegin));
      end;
    end;

    procedure HeadBobbingGoesDown;
    const
      HeadBobbingGoingDownSpeed = 0.1;
    var
      FracHeadBobbingPosition: Single;
    begin
      if UseHeadBobbing and (not HeadBobbingAlreadyDone) then
      begin
        { If head bobbing is active, but player did not move during
          this Idle call, and no gravity effect is in work
          then player is standing still on the ground.

          This means that his head bobbing should go down as far as
          possible. This means that HeadBobbingPosition should
          go to nearest integer value.

          Note that we avoid changing HeadBobbingPosition by less
          than SingleEqualityEpsilon, just to be on the safe side
          and avoid any "corner cases", when HeadBobbingPosition
          would switch between going up and down repeatedly. }
        FracHeadBobbingPosition := Frac(HeadBobbingPosition);
        if FracHeadBobbingPosition > 0.5 then
        begin
          if 1 - FracHeadBobbingPosition > SingleEqualityEpsilon then
            HeadBobbingPosition +=
              Min(HeadBobbingGoingDownSpeed * CompSpeed * 50,
                1 - FracHeadBobbingPosition);
        end else
        begin
          if FracHeadBobbingPosition > SingleEqualityEpsilon then
            HeadBobbingPosition -=
              Min(HeadBobbingGoingDownSpeed * CompSpeed * 50,
                FracHeadBobbingPosition);
        end;
      end;
    end;

    function GetIsOnTheGround: boolean;
    var
      MinHeightAboveTheGround, MaxHeightAboveTheGround, H: Single;
    begin
      H := RealCameraPreferredHeightNoHeadBobbing;
      MinHeightAboveTheGround := (H - H * HeadBobbing) * 0.99;
      MaxHeightAboveTheGround := (H + H * HeadBobbing) * 1.01;
      Result := IsAboveTheGround and
        (Sqr(MinHeightAboveTheGround) <= SqrHeightAboveTheGround) and
        (SqrHeightAboveTheGround <= Sqr(MaxHeightAboveTheGround));
    end;

  var
    OldIsFallingDown: boolean;
  begin
    OldIsFallingDown := IsFallingDown;

    if Gravity then
    begin
      { calculate IsAboveTheGround, SqrHeightAboveTheGround }
      DoGetCameraHeight(IsAboveTheGround, SqrHeightAboveTheGround);

      FIsOnTheGround := GetIsOnTheGround;
      FIsWalkingOnTheGround := MoveHorizontalDone and FIsOnTheGround;

      if not TryJump then
        if not TryGrow then
          if not TryFallingDown then
            if not TryFde_Stabilize then
              { Note that we don't do FallingOnTheGround effect until all
                other effects (jumping, growing, falling on the ground
                and stabilizing after falling on the ground) will finish
                their work. }
              if not TryFallingOnTheGround then
                HeadBobbingGoesDown;
    end else
    begin
      FIsFallingDown := false;
      TryFde_Stabilize;
    end;

    if OldIsFallingDown and (not IsFallingDown) then
      DoFalledDown;
  end;

  procedure PreferGravityUpForRotationsIdle;
  (* This is a good piece of work and seemed to work OK,
     but it's too much untested right now to let it work.

     It's needed only when you'll start to change
     PreferGravityUpForRotations from false to true in runtime,
     to avoid making player feel "awkward" rotations.

     Temporary I don't need it.

  var
    TargetPlane: TVector4Single;
    TargetPlaneDir: TVector3Single absolute TargetPlane;
    TargetCameraUp: TVector3Single;
    AngleRadBetweenTargetAndGravity: Single;
    AngleRadBetweenTarget, AngleRadBetweenTargetChange: Single;
    NewCameraUp: TVector3Single;
  begin
    if PreferGravityUp then
    begin
      { TODO: Correcting MinAngleRadFromGravityUp }

      { Correct CameraUp such that GravityUp, CameraDir and CameraUp
        are on the same plane.

        Math:
          TargetPlane := common plane of GravityUp and CameraDir,
          given by (A, B, C) = VectorProduct(GravityUp, CameraDir)
          and D = 0 (because point (0, 0, 0) is part of this plane).

          We check whether CameraUp is on this TargetPlane too.

          If not, we find TargetCameraUp = nearest point to CameraUp
          lying on this TargetPlane. We want our CameraUp be pointing
          like GravityUp, not in the other way, so if the angle between
          GravityUp and TargetCameraUp is > 90 degress we negate
          TargetCameraUp. If the angle is exactly 90 degress then
          TargetCameraUp is simply equal to GravityUp.

          And then we make the angle between TargetCameraUp and CameraUp
          smaller. }

      TargetPlaneDir := VectorProduct(GravityUp, CameraDir);
      if not IsZero(
         (TargetPlaneDir[0] * FCameraUp[0]) +
         (TargetPlaneDir[1] * FCameraUp[1]) +
         (TargetPlaneDir[2] * FCameraUp[2])) then
      begin
        TargetPlane[3] := 0;

        Writeln('corrrecting');

        { calculate TargetCameraUp }
        TargetCameraUp := PointOnPlaneClosestToPoint(TargetPlane, FCameraUp);
        AngleRadBetweenTargetAndGravity :=
          AngleRadBetweenVectors(TargetCameraUp, GravityUp);
        if FloatsEqual(AngleRadBetweenTargetAndGravity, HalfPi) then
          TargetCameraUp := GravityUp else
        if AngleRadBetweenTargetAndGravity > HalfPi then
          VectorNegateTo1st(TargetCameraUp);

        AngleRadBetweenTarget :=
          AngleRadBetweenVectors(TargetCameraUp, FCameraUp);
        AngleRadBetweenTargetChange := 0.01 * CompSpeed * 50;
        if AngleRadBetweenTarget > AngleRadBetweenTargetChange then
        begin
          NewCameraUp := FCameraUp;
          MakeVectorsAngleRadOnTheirPlane(NewCameraUp, TargetCameraUp,
            AngleRadBetweenTarget - AngleRadBetweenTargetChange);
          CameraUp := NewCameraUp;
        end else
          CameraUp := TargetCameraUp;
      end;
    end;
    *)
  begin
  end;

var
  ModsDown: TModifierKeys;
begin
  ModsDown := ModifiersDown(KeysDown);

  HeadBobbingAlreadyDone := false;
  MoveHorizontalDone := false;

  FIsCrouching := Input_Crouch.IsPressed(KeysDown, CharactersDown, MousePressed);

  if (not CheckModsDown) or
     (ModsDown - [mkShift] = []) then
  begin
    CheckRotates(1.0);

    if Input_Forward.IsPressed(KeysDown, CharactersDown, MousePressed) then
      MoveHorizontal;
    if Input_Backward.IsPressed(KeysDown, CharactersDown, MousePressed) then
      MoveHorizontal(-1);

    if Input_RightStrafe.IsPressed(KeysDown, CharactersDown, MousePressed) then
    begin
      RotateHorizontalForStrafeMove(-90);
      MoveHorizontal;
      RotateHorizontalForStrafeMove(90);
    end;

    if Input_LeftStrafe.IsPressed(KeysDown, CharactersDown, MousePressed) then
    begin
      RotateHorizontalForStrafeMove(90);
      MoveHorizontal;
      RotateHorizontalForStrafeMove(-90);
    end;

    { A simple implementation of Input_UpMove was
        RotateVertical(90); Move(MoveVerticalSpeed * CompSpeed * 50); RotateVertical(-90)
      Similarly, simple implementation of Input_DownMove was
        RotateVertical(-90); Move(MoveVerticalSpeed * CompSpeed * 50); RotateVertical(90)
      But this is not good, because when PreferGravityUp, we want to move
      along the GravityUp. (Also later note: RotateVertical is now bounded by
      MinAngleRadFromGravityUp). }
    if Input_UpMove.IsPressed(KeysDown, CharactersDown, MousePressed) then
      MoveVertical( 1);
    if Input_DownMove.IsPressed(KeysDown, CharactersDown, MousePressed) then
      MoveVertical(-1);

    { zmiana szybkosci nie wplywa na Matrix (nie od razu). Ale wywolujemy
      MatrixChanged - zmienilismy swoje wlasciwosci, moze sa one np. gdzies
      wypisywane w oknie na statusie i okno potrzebuje miec PostRedisplay po zmianie
      Move*Speed ?.

      How to apply CompSpeed * 50 here ?
      I can't just ignore CompSpeed * 50, but I can't also write
        FMoveHorizontalSpeed *= 1.1 * CompSpeed * 50;
      What I want is such (pl: ci±g³a) function that e.g.
        F(FMoveHorizontalSpeed, 2) = F(F(FMoveHorizontalSpeed, 1), 1)
      I.e. CompSpeed * 50 = 2 should work just like doing the same change twice.
      So F is FMoveHorizontalSpeed * Power(1.1, CompSpeed * 50)
      Easy!
    }
    if Input_MoveSpeedInc.IsPressed(KeysDown, CharactersDown, MousePressed) then
    begin
      FMoveHorizontalSpeed *= Power(1.1, CompSpeed * 50);
      FMoveVerticalSpeed *= Power(1.1, CompSpeed * 50);
      MatrixChanged;
    end;

    if Input_MoveSpeedDec.IsPressed(KeysDown, CharactersDown, MousePressed) then
    begin
      FMoveHorizontalSpeed /= Power(1.1, CompSpeed * 50);
      FMoveVerticalSpeed /= Power(1.1, CompSpeed * 50);
      MatrixChanged;
    end;
  end else
  if AllowSlowerRotations and (ModsDown = [mkCtrl]) then
  begin
    CheckRotates(0.1);
  end;

  PreferGravityUpForRotationsIdle;

  { These may be set to @true only inside GravityIdle }
  FIsWalkingOnTheGround := false;
  FIsOnTheGround := false;

  GravityIdle;
end;

procedure TMatrixWalker.Home;
begin
  { I don't set here CameraXxx properties, instead I actually directly set
    FCameraXxx fields. Reason:

    1. Speed (this way it's enough to call MatrixChanged only once).

    2. Also remember that setting CameraDir and CameraUp properties is followed
       by adjustment of up vector (to be orthogonal to dir vector).
       So I require that given Dir and Up initial camera vectors
       may not be parallel. But I can't guarantee that InitialCameraUp
       is not parallel to current CameraDir.

       Besides in this case I know that this adjustment is not needed,
       since InitialCameraDir and InitialCameraUp are already adjusted
       if necessary. }
  FCameraPos := InitialCameraPos;
  FCameraDir := InitialCameraDir;
  FCameraUp := InitialCameraUp;
  MatrixChanged;
end;

procedure TMatrixWalker.Jump;
var
  IsAboveTheGround: boolean;
  SqrHeightAboveTheGround: Single;
begin
  if IsJumping or IsFallingDown or (not Gravity) then Exit;

  { Merely checking for IsFallingDown is not enough, because IsFallingDown
    may be triggered with some latency. E.g. consider user that holds
    Input_Jump key down: whenever jump will end (in GravityIdle),
    Input_Jump.IsKey = true will cause another jump to be immediately
    (before IsFallingDown will be set to true) initiated.
    This is of course bad, because user holding Input_Jump key down
    would be able to jump to any height. The only good thing to do
    is to check whether player really has some ground beneath his feet
    to be able to jump. }

  { calculate IsAboveTheGround, SqrHeightAboveTheGround }
  DoGetCameraHeight(IsAboveTheGround, SqrHeightAboveTheGround);

  if (not IsAboveTheGround) or
     (SqrHeightAboveTheGround >
        Sqr(RealCameraPreferredHeight + RealCameraPreferredHeightMargin)) then
    Exit;

  FIsJumping := true;
  FJumpHeight := 0.0;
end;

function TMatrixWalker.EventDown(MouseEvent: boolean; Key: TKey;
  ACharacter: Char;
  AMouseButton: TMouseButton): boolean;
begin
  if Input_GravityUp.IsEvent(MouseEvent, Key, ACharacter, AMouseButton) then
  begin
    CameraUp := GravityUp;
    Result := true;
  end else
  if Input_Jump.IsEvent(MouseEvent, Key, ACharacter, AMouseButton) then
  begin
    Jump;
    Result := true;
  end else
    Result := false;
end;

function TMatrixWalker.KeyDown(key: TKey; c: char; KeysDown: PKeysBooleans): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if (not CheckModsDown) or
     (ModifiersDown(KeysDown) - [mkShift] = []) then
  begin
    Result := EventDown(false, Key, C, mbLeft);
  end;
end;

function TMatrixWalker.MouseDown(Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := EventDown(true, K_None, #0, Button);
end;

procedure TMatrixWalker.Init(
  const AInitialCameraPos, AInitialCameraDir, AInitialCameraUp: TVector3Single;
  const AGravityUp: TVector3Single;
  const ACameraPreferredHeight: Single;
  const ACameraRadius: Single);
begin
  SetInitialCameraLookDir(AInitialCameraPos, AInitialCameraDir, AInitialCameraUp);
  FGravityUp := AGravityUp;
  CameraPreferredHeight := ACameraPreferredHeight;
  CorrectCameraPreferredHeight(ACameraRadius);
  Home;
end;

procedure TMatrixWalker.Init(const Box: TBox3d; const ACameraRadius: Single);
var Pos: TVector3Single;
    AvgSize: Single;
begin
 if IsEmptyBox3d(Box) then
  Init(Vector3Single(0, 0, 0),
       Vector3Single(0, 0, -1),
       Vector3Single(0, 1, 0),
       Vector3Single(0, 1, 0) { GravityUp is the same as InitialCameraUp },
       0.0 { whatever }, ACameraRadius) else
 begin
  AvgSize := Box3dAvgSize(Box);
  Pos[0] := Box[0, 0]-AvgSize;
  Pos[1] := (Box[0, 1]+Box[1, 1])/2;
  Pos[2] := (Box[0, 2]+Box[1, 2])/2;
  Init(Pos, VectorAdjustToLength(UnitVector3Single[0], AvgSize*0.1),
    UnitVector3Single[2],
    UnitVector3Single[2] { GravityUp is the same as InitialCameraUp },
    AvgSize * 0.1, ACameraRadius);
 end;
end;

procedure TMatrixWalker.SetInitialCameraLookDir(const AInitialCameraPos,
  AInitialCameraDir, AInitialCameraUp: TVector3Single);
begin
  FInitialCameraPos := AInitialCameraPos;
  FInitialCameraDir := AInitialCameraDir;
  FInitialCameraUp := AInitialCameraUp;
  MakeVectorsOrthoOnTheirPlane(FInitialCameraUp, FInitialCameraDir);
  MatrixChanged;
end;

procedure TMatrixWalker.SetInitialCameraLookAt(const AInitialCameraPos,
  AInitialCameraCenter, AInitialCameraUp: TVector3Single);
begin
  SetInitialCameraLookDir(AInitialCameraPos,
    VectorSubtract(AInitialCameraCenter, AInitialCameraPos),
    AInitialCameraUp);
end;

procedure TMatrixWalker.SetCameraPos(const Value: TVector3Single);
begin
  FCameraPos := Value;
  MatrixChanged;
end;

procedure TMatrixWalker.SetCameraDir(const Value: TVector3Single);
begin
  FCameraDir := Value;
  MakeVectorsOrthoOnTheirPlane(FCameraUp, FCameraDir);
  MatrixChanged;
end;

procedure TMatrixWalker.SetCameraUp(const Value: TVector3Single);
begin
  FCameraUp := Value;
  MakeVectorsOrthoOnTheirPlane(FCameraDir, FCameraUp);
  MatrixChanged;
end;

procedure TMatrixWalker.RecalculateFrustum;
begin
  CalculateFrustum(FFrustum, ProjectionMatrix, Matrix);
end;

procedure TMatrixWalker.MatrixChanged;
begin
  RecalculateFrustum;
  inherited;
end;

procedure TMatrixWalker.SetProjectionMatrix(const Value: TMatrix4Single);
begin
  FProjectionMatrix := Value;
  RecalculateFrustum;
end;

procedure TMatrixWalker.CorrectCameraPreferredHeight(const CameraRadius: Single);
begin
  MatrixNavigation.CorrectCameraPreferredHeight(
    FCameraPreferredHeight, CameraRadius, CrouchHeight, HeadBobbing);
end;

function TMatrixWalker.MaxJumpDistance: Single;
begin
  Result := MaxJumpHeight * CameraPreferredHeight;
end;

function TMatrixWalker.CameraDirInGravityPlane: TVector3Single;
begin
  Result := CameraDir;

  if not VectorsParallel(Result, GravityUp) then
    MakeVectorsOrthoOnTheirPlane(Result, GravityUp);
end;

procedure TMatrixWalker.FallOnTheGround;
begin
  FFallingOnTheGround := true;

  { Mathematically reasoning, this should be smarter.
    I mean that we should randomize FFallingOnTheGroundAngleIncrease
    *only* if CameraUp is parallel to GravityUp ?
    Otherwise CameraUp could change through some strange path ?

    But current effect seems to behave good in all situations...
    In any case, CameraUp going through some strange path will only
    be noticeable for a very short time, so I don't think that's a real
    problem... unless I see some example when it looks bad. }

  FFallingOnTheGroundAngleIncrease := Random(2) = 0;
end;

procedure TMatrixWalker.CancelFallingDown;
begin
  { Fortunately implementation of this is brutally simple right now. }
  FIsFallingDown := false;
end;

procedure TMatrixWalker.MouseMove(MouseXChange, MouseYChange: Integer);
begin
  if MouseLook then
  begin
    if MouseXChange <> 0 then
      RotateHorizontal(-MouseXChange * MouseLookHorizontalSensitivity);
    if MouseYChange <> 0 then
    begin
      if InvertVerticalMouseLook then
        MouseYChange := -MouseYChange;
      RotateVertical(-MouseYChange * MouseLookVerticalSensitivity);
    end;
  end;
end;

{ global ------------------------------------------------------------ }

procedure CorrectCameraPreferredHeight(var CameraPreferredHeight: Single;
  const CameraRadius: Single; const CrouchHeight, HeadBobbing: Single);
var
  NewCameraPreferredHeight: Single;
begin
  { We have requirement that
      CameraPreferredHeight * CrouchHeight * (1 - HeadBobbing) >= CameraRadius
    So
      CameraPreferredHeight >= CameraRadius / (CrouchHeight * (1 - HeadBobbing));

    I make it even a little larger (that's the reason for "* 1.01") to be
    sure to avoid floating-point rounding errors. }

  NewCameraPreferredHeight := 1.01 * CameraRadius /
    (CrouchHeight * (1 - HeadBobbing));

  if CameraPreferredHeight < NewCameraPreferredHeight then
    CameraPreferredHeight := NewCameraPreferredHeight;
end;

end.
