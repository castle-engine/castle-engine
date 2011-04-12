{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Cameras to navigate in 3D space (TExamineCamera, TWalkCamera, TUniversalCamera). }

unit Cameras;

interface

uses SysUtils, VectorMath, KambiUtils, KeysMouse, Boxes3D, Quaternions, Frustum,
  UIControls, Classes, RaysWindow, Base3D, KambiTimeUtils;

const
  DefaultFallingDownStartSpeed = 0.5;
  DefaultGrowingSpeed = 1.0;
  DefaultHeadBobbing = 0.02;
  DefaultCrouchHeight = 0.5;
  DefaultMaxJumpHeight = 1.0;
  DefaultMinAngleRadFromGravityUp = { 10 degress } Pi / 18; { }
  DefaultRotationHorizontalSpeed = 150;
  DefaultRotationVerticalSpeed = 100;
  DefaultFallingDownSpeedIncrease = 13/12;
  DefaultMouseLookHorizontalSensitivity = 0.09;
  DefaultMouseLookVerticalSensitivity = 0.09;
  DefaultHeadBobbingTime = 0.4;
  DefaultJumpSpeedMultiply = 2.0;
  DefaultJumpPower = 9.0;
  { Default value for TCamera.CameraRadius.
    Matches the default VRML/X3D NavigationInfo.avatarSize[0]. }
  DefaultCameraRadius = 0.25;

type
  { }
  TCamera = class;

  TInputShortcut = class;
  TInputShortcutChangedFunc = procedure (Shortcut: TInputShortcut) of object;

  { An input shortcut represents a keyboard and/or mouse shortcut
    for some command. Up to two key shortcuts may be assigned to a single
    item, one mouse button shortcut, one character shortcut,
    and one mouse wheel shortcut.

    Normal "key shortcut" is identified by Keys.TKey value.

    "Character shortcut" differs from "key shortcut" because it's identified
    by produced character. We don't deal here how this character is produced
    (for example, character "X" may produced on normal systems
    by Shift + "x" or by pressing "x" while "caps lock" is on;
    but this is not dealt with in this unit (it's usually provided
    by operating system / GUI toolkit to GLWindow unit),
    we just get characters passed to TCamera.KeyDown and such methods.) }
  TInputShortcut = class
  private
    FKey1: TKey;
    FKey2: TKey;
    FCharacter: Char;
    FMouseButtonUse: boolean;
    FMouseButton: TMouseButton;
    FMouseWheel: TMouseWheelDirection;

    procedure SetKey1(const Value: TKey);
    procedure SetKey2(const Value: TKey);
    procedure SetCharacter(const Value: Char);
    procedure SetMouseButtonUse(const Value: boolean);
    procedure SetMouseButton(const Value: TMouseButton);
    procedure SetMouseWheel(const Value: TMouseWheelDirection);
  private
    FDefaultKey1: TKey;
    FDefaultKey2: TKey;
    FDefaultCharacter: Char;
    FDefaultMouseButtonUse: boolean;
    FDefaultMouseButton: TMouseButton;
    FDefaultMouseWheel: TMouseWheelDirection;

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
      AMouseButton: TMouseButton;
      const AMouseWheel: TMouseWheelDirection = mwNone);

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

    { Mouse wheel to activate this command. Note that mouse wheels cannot be
      continously pressed (our method IsPressed doesn't look at it),
      so this is only suitable for commands that work in steps
      (not continously). }
    property MouseWheel: TMouseWheelDirection read FMouseWheel
      write SetMouseWheel default mwNone;

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
    property DefaultMouseWheel: TMouseWheelDirection
      read FDefaultMouseWheel write FDefaultMouseWheel;
    { @groupEnd }

    procedure MakeDefault;

    { This assigns to this object the default values from Source. }
    procedure AssignFromDefault(Source: TInputShortcut);

    { This copies Source properties to this object.
      It always copies "current" properties (Key1, Key2, Character,
      MouseButtonUse, MouseButton, MouseWheel), and optionally (if CopyDefaults)
      also copies the DefaultXxx properties. }
    procedure Assign(Source: TInputShortcut; CopyDefaults: boolean);

    { Make this input impossible to activate by the user.
      This sets both keys to K_None, Character to #0, MouseButtonUse
      to @false, and MouseWheel to mwNone. }
    procedure MakeClear;

    { Given a set of currently pressed keys and mouse buttons,
      decide whether this input is currently pressed. }
    function IsPressed(Pressed: TKeysPressed;
      const MousePressed: TMouseButtons): boolean;

    { Looking at Container's currently pressed keys and mouse buttons,
      decide whether this input is currently pressed. }
    function IsPressed(Container: IUIContainer): boolean;

    { Check does given Key or ACharacter correspond to this input shortcut.
      If Key = K_None and ACharacter = #0, result is always @false. }
    function IsKey(Key: TKey; ACharacter: Char): boolean;

    { Check does given mouse button correspond to this input shortcut.
      In practice, just checks MouseButtonUse and if @true, compares
      AMouseButton with MouseButton. }
    function IsMouseButton(AMouseButton: TMouseButton): boolean;

    function IsMouseWheel(const AMouseWheel: TMouseWheelDirection): boolean;

    { Check does given key or mouse button or mouse wheel use activates
      this shortcut.

      For key/character press, set AKey <> K_None or ACharacter <> #0.
      For mouse button press, set AMousePress to @true
      and pass relevant AMouseButton. For mouse wheel, pass AMouseWheel
      <> mwNone. Pass only one of these three events here,
      for example if you AMousePress to @true then pass
      AKey = K_None and ACharacter = #0 and AMouseWheel = mwNone.

      Basically, this is a "dispatcher" that simply calls one of the IsKey or
      IsMouseButton or IsMouseWheel methods. It's sometimes more comfortable
      to use this instead of taking care of them separately. }
    function IsEvent(AKey: TKey; ACharacter: Char;
      AMousePress: boolean; AMouseButton: TMouseButton;
      AMouseWheel: TMouseWheelDirection): boolean;

    { Describe this input shortcut. If it's not active at all
      (like after MakeClear), we will use NoneString. }
    function Description(const NoneString: string): string;

    { If assigned, this will be called always right after the key/character/mouse
      shortcut value changed. Note that this is called only when
      the "current" values (Key1, Key2, Character, MouseButtonUse, MouseButton,
      MouseWheel) changed, and it's not called when just
      the DefaultXxx values changed. }
    property OnChanged: TInputShortcutChangedFunc
      read FOnChanged write FOnChanged;
  end;

  { Handle user navigation in 3D scene.
    You control camera parameters and provide user input
    to this class by various methods and properties.
    You can investigate the current camera configuration by many methods,
    the most final is the @link(Matrix) method that
    generates a simple 4x4 camera matrix.

    This class is not tied to any OpenGL specifics, any VRML specifics,
    and GLWindow etc. --- this class is fully flexible and may be used
    in any 3D program, whether using GLWindow, OpenGL etc. or not.

    Various TCamera descendants implement various navigation
    methods, for example TExamineCamera allows the user to rotate
    and scale the model (imagine that you're holding a 3D model in your
    hands and you look at it from various sides) and TWalkCamera
    implements typical navigation in the style of first-person shooter
    games.

    The most comfortable way to use a camera is with a scene manager
    (TKamSceneManager). You can create your camera instance,
    call it's @code(Init) method (this is initializes most important properties),
    and assign it to TKamSceneManager.Camera property.
    This way SceneManager will pass all necessary window events to the camera,
    and when drawing SceneManager will load camera matrix like
    @code(glLoadMatrix(Camera.Matrix);).
    In fact, if you do not assign anything to TKamSceneManager.Camera property,
    then the default camera will be created for you. So @italic(when
    using TKamSceneManager, you do not have to do anything to use a camera)
    --- default camera will be created and automatically used for you.

    See @code(kambi_vrml_game_engine/examples/glwindow/demo_camera.lpr)
    example program in engine sources for simple demo how to use this class. }
  TCamera = class(TUIControl)
  private
    VisibleChangeSchedule: Cardinal;
    IsVisibleChangeScheduled: boolean;
    FIgnoreAllInputs: boolean;
    FInitialPosition, FInitialDirection, FInitialUp: TVector3Single;
    FProjectionMatrix: TMatrix4Single;
    FCameraRadius: Single;

    Animation: boolean;
    AnimationEndTime: TKamTime;
    AnimationCurrentTime: TKamTime;

    AnimationBeginPosition: TVector3Single;
    AnimationBeginDirection: TVector3Single;
    AnimationBeginUp: TVector3Single;
    AnimationEndPosition: TVector3Single;
    AnimationEndDirection: TVector3Single;
    AnimationEndUp: TVector3Single;

    FFrustum: TFrustum;

    procedure RecalculateFrustum;
  protected
    { Mechanism to schedule VisibleChange calls.

      This mechanism allows to defer calling VisibleChange.
      Idea: BeginVisibleChangeSchedule increases internal VisibleChangeSchedule
      counter, EndVisibleChangeSchedule decreases it and calls
      actual VisibleChange if counter is zero and some
      ScheduleVisibleChange was called in between.

      When ScheduleVisibleChange is called when counter is zero,
      VisibleChange is called immediately, so it's safe to always
      use ScheduleVisibleChange instead of direct VisibleChange
      in this class. }
    procedure BeginVisibleChangeSchedule;
    procedure ScheduleVisibleChange;
    procedure EndVisibleChangeSchedule;

    procedure SetIgnoreAllInputs(const Value: boolean); virtual;
    procedure SetProjectionMatrix(const Value: TMatrix4Single); virtual;
    procedure SetCameraRadius(const Value: Single); virtual;

    function IsAnimation: boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    { Called always when some visible part of this control
      changes. In the simplest case, this is used by the controls manager to
      know when we need to redraw the control.

      In case of the TCamera class, we assume that changes
      to the @link(TCamera.Matrix), and other properties (for example even
      changes to TWalkCamera.MoveSpeed), are "visible",
      and they also result in this event. }
    procedure VisibleChange; override;

    { Current camera matrix. You should multiply every 3D point of your
      scene by this matrix, which usually simply means that you should
      do @code(glLoadMatrix) or @code(glMultMatrix) of this matrix. }
    function Matrix: TMatrix4Single; virtual; abstract;

    { Extract only rotation from your current camera @link(Matrix).
      This is useful for rendering skybox in 3D programs
      (e.g. for Background VRML 97 node) and generally to transform
      directions between world and camera space.

      It's guaranteed that this is actually only 3x3 matrix,
      the 4th row and 4th column are all zero except the lowest right item
      which is 1.0. }
    function RotationMatrix: TMatrix4Single; virtual; abstract;

    { If true, we will ignore all inputs passed to this class.
      So this camera will not handle any key/mouse events.
      This is useful to implement e.g. VRML "NONE" navigation type. }
    property IgnoreAllInputs: boolean
      read FIgnoreAllInputs write SetIgnoreAllInputs default false;

    { Things related to frustum ---------------------------------------- }

    { The current camera (viewing frustum, based on
      @link(ProjectionMatrix) (set by you) and @link(Matrix) (calculated here).
      This is recalculated whenever one of these two properties change.
      Be sure to set @link(ProjectionMatrix) before using this. }
    property Frustum: TFrustum read FFrustum;

    { Projection matrix that you should pass here to have Frustum
      calculated for you.

      This is initially IdentityMatrix4Single.
      This is not modified anywhere from this class.
      *You* should modify it, you should set it to projection matrix
      that you use, if you want to use Frustum value.
      This is used whenever Frustum is recalculated. }
    property ProjectionMatrix: TMatrix4Single
      read FProjectionMatrix write SetProjectionMatrix;

    { The radius of a sphere around the camera
      that makes collisions with the world.

      @unorderedList(
        @item(Collision detection routines use this.)
        @item(It determines the projection near plane (that must be slightly
          smaller than this radius) for 3D rendering.)
        @item(
          Walk camera uses this for automatically correcting
          CameraPreferredHeight, otherwise weird things could happen
          if your avatar height is too small compared to camera radius.
          See @link(CorrectCameraPreferredHeight).

          Especially useful if you let
          user change CameraPreferredHeight at runtime by
          Input_IncreaseCameraPreferredHeight, Input_DcreaseCameraPreferredHeight.

          This is actually the whole use of CameraRadius inside Cameras unit
          and classes. But the code all around the engine also looks for
          this CameraRadius, and the camera is a natural place to keep this
          information.)
      ) }
    property CameraRadius: Single
      read FCameraRadius write SetCameraRadius default DefaultCameraRadius;

    { Express current view as camera vectors: position, direction, up.

      Returned Dir and Up must be orthogonal.
      Returned Dir and Up and GravityUp are already normalized. }
    procedure GetView(out APos, ADir, AUp: TVector3Single); virtual; abstract;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3Single); virtual; abstract;
    function GetPosition: TVector3Single; virtual; abstract;

    { Set camera view from vectors: position, direction, up.

      Direction, Up and GravityUp do not have to be normalized.
      They cannot be parallel (will be fixed internally to be exactly orthogonal). }
    procedure SetView(const APos, ADir, AUp: TVector3Single); virtual; abstract;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3Single); virtual; abstract;

    function PositionInside(const X, Y: Integer): boolean; override;

    { Calculate a 3D ray picked by the WindowX, WindowY position on the window.
      Uses current Container, which means that you have to add this camera
      to TGLUIWindow.Controls or TKamOpenGLControl.Controls before
      using this method.

      PerspectiveView, PerspectiveViewAngles and OrthoViewDimensions
      describe your projection, required for calculating the ray properly.
      See TKamSceneManager.PerspectiveView for their specification.

      WindowX, WindowY are given in the same style as MouseX, MouseY:
      WindowX = 0 is left, WindowY = 0 is top. }
    procedure Ray(const WindowX, WindowY: Integer;
      const PerspectiveView: boolean;
      const PerspectiveViewAngles: TVector2Single;
      const OrthoViewDimensions: TVector4Single;
      out Ray0, RayVector: TVector3Single);

    { Calculate a ray picked by current mouse position on the window.
      Uses current Container (both to get it's size and to get current
      mouse position), which means that you have to add this camera
      to TGLUIWindow.Controls or TKamOpenGLControl.Controls before
      using this method. }
    procedure MouseRay(
      const PerspectiveView: boolean;
      const PerspectiveViewAngles: TVector2Single;
      const OrthoViewDimensions: TVector4Single;
      out Ray0, RayVector: TVector3Single);

    { Calculate a ray picked by WindowX, WindowY position on the viewport,
      assuming current viewport dimensions are as given.
      This doesn't look at our container sizes at all.

      PerspectiveView, PerspectiveViewAngles and OrthoViewDimensions
      describe your projection, required for calculating the ray properly.
      See TKamSceneManager.PerspectiveView for their specification.

      WindowX, WindowY are given in the same style as MouseX, MouseY:
      WindowX = 0 is left, WindowY = 0 is top.
      To understand WindowY (with respect to bottom),
      we also need separate WindowHeight. }
    procedure CustomRay(
      const ViewportLeft, ViewportBottom: Integer;
      const ViewportWidth, ViewportHeight, WindowHeight: Cardinal;
      const WindowX, WindowY: Integer;
      const PerspectiveView: boolean;
      const PerspectiveViewAngles: TVector2Single;
      const OrthoViewDimensions: TVector4Single;
      out Ray0, RayVector: TVector3Single);

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    { Animate a camera smoothly into another camera settings.
      This will gradually change our settings (only the most important
      settings, that determine actual camera view, i.e. @link(Matrix) result)
      into another camera.

      Current OtherCamera settings will be internally copied during this call.
      So you can even free OtherCamera instance immediately after calling this.

      When we're during camera animation, @link(Idle) doesn't do other stuff
      (e.g. gravity for TWalkCamera doesn't work, rotating for TExamineCamera
      doesn't work). This also means that the key/mouse controls of the camera
      do not work. Instead, we remember the source and target position
      (at the time AnimateTo was called) of the camera,
      and smoothly interpolate camera parameters to match the target.

      Once the animation stops, @link(Idle) goes back to normal: gravity
      in TWalkCamera works again, rotating in TExamineCamera works again etc.

      Calling AnimateTo while the previous animation didn't finish yet
      is OK. This simply cancels the previous animation,
      and starts the new animation from the current position.

      @italic(Descendants implementors notes:) In this class,
      almost everything is handled (through GetView / SetView).
      In descendants you have to only ignore key/mouse/idle events
      when IsAnimation is @true.
      (Although each Idle would override the view anyway, but for
      stability it's best to explicitly ignore them --- you never know
      how often Idle will be called.)

      @groupBegin }
    procedure AnimateTo(OtherCamera: TCamera; const Time: TKamTime);
    procedure AnimateTo(const Pos, Dir, Up: TVector3Single; const Time: TKamTime);
    { @groupEnd }

    { Initial camera values.

      InitialDirection and InitialUp must be always normalized,
      and orthogonal.

      Default value of InitialPosition is (0, 0, 0), InitialDirection is
      (0, -1, 0), InitialUp is (0, 1, 0).

      @groupBegin }
    property InitialPosition : TVector3Single read FInitialPosition;
    property InitialDirection: TVector3Single read FInitialDirection;
    property InitialUp       : TVector3Single read FInitialUp;
    { @groupEnd }

    { Set three initial camera vectors.

      AInitialDirection and AInitialUp will be automatically normalized.
      Corresponding properties (InitialDirection and InitialUp) will always
      contain normalized values.

      AInitialUp will be also automatically corrected to be orthogonal
      to AInitialDirection. We will correct AInitialUp to make it orthogonal,
      but still preserving the plane they were indicating together with
      AInitialDirection. Do not ever give here
      AInitialUp that is parallel to AInitialDirection.

      If TransformCurrentCamera = @true, then they will also
      try to change current camera relative to the initial vectors changes.
      This implements VRML desired behavior that
      "viewer position/orientation is conceptually a child of
      viewpoint position/orientation, and when viewpoint position/orientation
      changes, viewer should also change". }
    procedure SetInitialView(
      const AInitialPosition: TVector3Single;
      AInitialDirection, AInitialUp: TVector3Single;
      const TransformCurrentCamera: boolean); virtual;

    { Jump to initial camera view (set by SetInitialView). }
    procedure GoToInitial; virtual;

    function PreventsComfortableDragging: boolean; virtual;
  end;

  TCameraClass = class of TCamera;

  T3BoolInputs = array [0..2, boolean] of TInputShortcut;

  { Navigate the 3D model in examine mode, like you would hold
    a box with the model inside.
    The model is displayed around MoveAmount 3D point,
    it's rotated by @link(Rotations) and scaled by ScaleFactor
    (scaled around MoveAmount point). }
  TExamineCamera = class(TCamera)
  private
    FMoveAmount, FCenterOfRotation: TVector3Single;
    FRotations: TQuaternion;
    { Speed of rotations.

      This could be implemented as a quaternion,
      it even was implemented like this (and working!) for a couple
      of minutes. But this caused one problem: in Idle, I want to
      apply FRotationsAnim to Rotations *scaled by CompSpeed*.
      There's no efficient way with quaternions to say "take only CompSpeed
      fraction of angle encoded in FRotationsAnim", AFAIK.
      The only way would be to convert FRotationsAnim back to AxisAngle,
      then scale angle, then convert back to quaternion... which makes
      the whole exercise useless. }
    FRotationsAnim: TVector3Single;
    FScaleFactor: Single;
    FModelBox: TBox3D;

    procedure SetRotationsAnim(const Value: TVector3Single);
    procedure SetRotations(const Value: TQuaternion);
    procedure SetScaleFactor(const Value: Single);
    procedure SetMoveAmount(const Value: TVector3Single);
    procedure SetModelBox(const Value: TBox3D);
    procedure SetCenterOfRotation(const Value: TVector3Single);
    function Zoom(const Factor: Single): boolean;
  private
    FInputs_Move: T3BoolInputs;
    FInputs_Rotate: T3BoolInputs;
    FInput_ScaleLarger: TInputShortcut;
    FInput_ScaleSmaller: TInputShortcut;
    FInput_Home: TInputShortcut;
    FInput_StopRotating: TInputShortcut;

    function EventDown(AKey: TKey; ACharacter: Char;
      AMousePress: boolean; AMouseButton: TMouseButton;
      AMouseWheel: TMouseWheelDirection): boolean;
  private
    FMouseNavigation: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Matrix: TMatrix4Single; override;

    function MatrixInverse: TMatrix4Single;

    function RotationMatrix: TMatrix4Single; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function KeyDown(Key: TKey; C: char): boolean; override;
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;
    function MouseWheel(const Scroll: Single; const Vertical: boolean): boolean; override;

    { Current camera properties ---------------------------------------------- }

    { Current rotation of the model.
      Rotation is done around ModelBox middle (with MoveAmount added). }
    property Rotations: TQuaternion read FRotations write SetRotations;

    { Continous rotation animation, applied each Idle to Rotations. }
    property RotationsAnim: TVector3Single read FRotationsAnim write SetRotationsAnim;

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

    property CenterOfRotation: TVector3Single read FCenterOfRotation write SetCenterOfRotation;

    { How the model is scaled. Scaling is done around MoveAmount added to
      the middle of ModelBox. @italic(May never be zero (or too near zero).) }
    property ScaleFactor: Single
      read FScaleFactor write SetScaleFactor default 1;

    { The aproximate size of 3D model that will be viewed.
      This is the crucial property of this class that you have to set,
      to make the navigation work best.
      Setting this sets also CenterOfRotation to the middle of the box.

      The idea is that usually this is the only property that you have to set.
      ScaleFactor, MoveAmount, RotationsAnim will be almost directly
      controlled by user (through KeyDown and other events).
      @link(Rotations) will be automatically modified by @link(Idle).

      So often you only need to set ModelBox, once,
      and everything else will work smoothly.

      Initially this is EmptyBox3D. }
    property ModelBox: TBox3D read FModelBox write SetModelBox;

    { Initialize most important properties of this class:
      sets ModelBox and goes to a nice view over the entire scene.

      In other words, this is just a shortcut to setting ModelBox
      and then calling @link(Home). }
    procedure Init(const AModelBox: TBox3D; const ACameraRadius: Single);

    { Go to a nice view over the entire scene. }
    procedure Home;

    { Methods performing navigation.
      Usually you want to just leave this for user to control. --------------- }

    { Sets RotationsAnim to zero, stopping the rotation of the model. }
    procedure StopRotating;

    { Adds small rotation around base axis Coord to the RotationsAnim,
      thus making rotation faster. }
    procedure Rotate(coord: integer; const SpeedChange: Single);

    procedure Scale(const ScaleBy: Single);
    procedure Move(coord: integer; const MoveDistance: Single);

    { User inputs ------------------------------------------------------------ }

    { TODO: tak samo jak TWalkCamera, przydaloby sie moc podawac
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

    property MouseNavigation: boolean
      read FMouseNavigation write FMouseNavigation default true;

    { TODO: for historical reasons, ExclusiveEvents is @false by default
      for TExamineCamera. }
    property ExclusiveEvents default false;

    procedure GetView(out APos, ADir, AUp: TVector3Single); override;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3Single); override;
    function GetPosition: TVector3Single; override;
    procedure SetView(const APos, ADir, AUp: TVector3Single); override;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3Single); override;

    function PreventsComfortableDragging: boolean; override;
  end;

  TWalkCamera = class;

  { See @link(TWalkCamera.DoMoveAllowed) and
    @link(TWalkCamera.OnMoveAllowed) }
  TMoveAllowedFunc = function(Camera: TWalkCamera;
    const ProposedNewPos: TVector3Single;
    out NewPos: TVector3Single;
    const BecauseOfGravity: boolean): boolean of object;

  { See @link(TWalkCamera.OnFalledDown). }
  TFalledDownNotifyFunc = procedure (Camera: TWalkCamera;
    const FallenHeight: Single) of object;

  TGetHeightAbove = procedure (Camera: TWalkCamera;
    out IsAbove: boolean; out AboveHeight: Single;
    out AboveGround: P3DTriangle)
    of object;

  { Navigation by walking (first-person-shooter-like moving) in 3D scene.
    Camera is defined by it's position, looking direction
    and up vector, user can rotate and move camera using various keys. }
  TWalkCamera = class(TCamera)
  private
    FPosition, FDirection, FUp,
    FGravityUp: TVector3Single;
    FMoveHorizontalSpeed, FMoveVerticalSpeed, FMoveSpeed: Single;
    FRotationHorizontalSpeed, FRotationVerticalSpeed: Single;
    FPreferGravityUpForRotations: boolean;
    FPreferGravityUpForMoving: boolean;
    FIsAbove: boolean;
    FAboveHeight: Single;
    FAboveGround: P3DTriangle;
    FMouseLook: boolean;

    procedure SetPosition(const Value: TVector3Single);
    procedure SetDirection(const Value: TVector3Single);
    procedure SetUp(const Value: TVector3Single);
    procedure SetMouseLook(const Value: boolean);
    procedure SetGravityUp(const Value: TVector3Single);
  private
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
    FInput_IncreaseCameraPreferredHeight: TInputShortcut;
    FInput_DecreaseCameraPreferredHeight: TInputShortcut;
    FInput_GravityUp: TInputShortcut;
    FInput_MoveSpeedInc: TInputShortcut;
    FInput_MoveSpeedDec: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FInput_Crouch: TInputShortcut;

    FAllowSlowerRotations: boolean;
    FCheckModsDown: boolean;

    FMinAngleRadFromGravityUp: Single;

    FMouseLookHorizontalSensitivity: Single;
    FMouseLookVerticalSensitivity: Single;

    procedure RotateAroundGravityUp(const AngleDeg: Single);
    procedure RotateAroundUp(const AngleDeg: Single);
    procedure RotateHorizontal(const AngleDeg: Single);
    procedure RotateVertical(const AngleDeg: Single);

    { Jump.

      Returns if a jump was actually done. For example, you cannot
      jump when there's no gravity, or you're already in the middle
      of the jump. Can be useful to determine if key was handled and such. }
    function Jump: boolean;

    function EventDown(AKey: TKey; ACharacter: Char;
      AMousePress: boolean; AMouseButton: TMouseButton;
      AMouseWheel: TMouseWheelDirection): boolean;
  private
    { Private things related to gravity ---------------------------- }

    FCameraPreferredHeight: Single;
    FIsFallingDown: boolean;
    FFallingDownStartPos: TVector3Single;
    FOnFalledDown: TFalledDownNotifyFunc;
    FFallingDownStartSpeed: Single;
    FFallingDownSpeed: Single;
    FFallingDownSpeedIncrease: Single;
    FGravity: boolean;
    FOnGetHeightAbove: TGetHeightAbove;
    FGrowingSpeed: Single;
    { This is used by FallingDownEffect to temporary modify Matrix result
      by rotating Up around Direction. In degress. }
    Fde_UpRotate: Single;
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
    FHeadBobbingTime: Single;
    function UseHeadBobbing: boolean;

  private
    FCrouchHeight: Single;
    FIsCrouching: boolean;

    FFallingOnTheGround: boolean;
    FFallingOnTheGroundAngleIncrease: boolean;

    FIsOnTheGround: boolean;
    FIsWalkingOnTheGround: boolean;

    FInvertVerticalMouseLook: boolean;
    FOnMoveAllowed: TMoveAllowedFunc;

    function RealCameraPreferredHeightNoHeadBobbing: Single;
    function RealCameraPreferredHeightMargin: Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Call OnGetHeightAbove callback, updating IsAbove, AboveHeight, AboveGround. }
    procedure UpdateHeightAbove; virtual;

    function Matrix: TMatrix4Single; override;
    function RotationMatrix: TMatrix4Single; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function KeyDown(Key: TKey; C: char): boolean; override;

    { This is used by @link(DoMoveAllowed), see there for description. }
    property OnMoveAllowed: TMoveAllowedFunc read FOnMoveAllowed write FOnMoveAllowed;

    { @abstract(DoMoveAllowed will be used when user will move in the scene,
      i.e. when user will want to change @link(Position).)

      ProposedNewPos is the position where the user wants to move
      (current user position is always stored in Position,
      so you can calculate move direction by ProposedNewPos - Position).

      This is the place to "plug in" your collision detection
      into camera.

      Returns false if no move is allowed.
      Otherwise returns true and sets NewPos to the position
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
      lower that some minimal plane when BecauseOfGravity
      (because this would mean that camera falls down infinitely),
      on the other hand when BecauseOfGravity is @false moving
      outside bounding box is allowed (to allow camera to look at the
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
    property Input_IncreaseCameraPreferredHeight: TInputShortcut read FInput_IncreaseCameraPreferredHeight;
    property Input_DecreaseCameraPreferredHeight: TInputShortcut read FInput_DecreaseCameraPreferredHeight;
    property Input_DownMove: TInputShortcut read FInput_DownMove;
    property Input_GravityUp: TInputShortcut read FInput_GravityUp;

    { Input_MoveSpeedInc and Input_MoveSpeedDec change the MoveSpeed.
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
      rotation keys work 10x slower. Also Increase/DecreaseCameraPreferredHeight
      work only when Ctrl pressed.
      Other keys with other modifiers
      don't work. We allow shift, because to press character "+" on non-numpad
      keyboard (useful on laptops, where numpad is difficult) you
      probably need to press shift.

      If @false then all keys work as usual, no matter what
      modifiers are pressed. And rotation keys never work 10x slower
      (AllowSlowerRotations is ignored),
      also Increase/DecreaseCameraPreferredHeight are ignored. }
    property CheckModsDown: boolean
      read FCheckModsDown write FCheckModsDown
      default true;

    { General stuff ----------------------------------------------------- }

    { Moving speeds. MoveHorizontalSpeed is only for horizontal movement,
      MoveVerticalSpeed is only for vertical, and MoveSpeed simply affects
      both types of movement. Effectively, we always scale the speed
      of movement by either @code(MoveHorizontalSpeed * MoveSpeed) or
      @code(MoveVerticalSpeed * MoveSpeed).

      We move by distance @code(MoveSpeed * MoveHorizontalSpeed (or MoveVerticalSpeed))
      during one second. Assuming "normal circumstances",
      namely that CompSpeed provided to @link(Idle) method
      is expressed in seconds (which is the case, when you use
      camera with TGLWindow.Controls or TKamSceneManager.Camera).
      So if you leave MoveHorizontalSpeed = MoveVerticalSpeed = 1 (as default),
      MoveSpeed expressed the speed in nice units / per second.

      Default values for all these speed properties is 1.0,
      so you simply move by 1 unit per second.

      @groupBegin }
    property MoveHorizontalSpeed: Single
      read FMoveHorizontalSpeed write FMoveHorizontalSpeed default 1.0;
    property MoveVerticalSpeed: Single
      read FMoveVerticalSpeed write FMoveVerticalSpeed default 1.0;
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed default 1.0;
    { @groupEnd }

    { Rotation keys speed, in degrees per second.
      @groupBegin }
    property RotationHorizontalSpeed: Single
      read FRotationHorizontalSpeed write FRotationHorizontalSpeed
      default DefaultRotationHorizontalSpeed;

    property RotationVerticalSpeed: Single
      read FRotationVerticalSpeed write FRotationVerticalSpeed
      default DefaultRotationVerticalSpeed;
    { @groupEnd }

    { Camera position, looking direction and up vector.

      Initially (after creating this object) they are equal to
      InitialPosition, InitialDirection, InitialUp.
      Also @link(Init) and @link(GoToInitial) methods reset them to these
      initial values.

      The @link(Direction) and @link(Up) vectors should always be normalized
      (have length 1). When setting them by these properties, we will normalize
      them automatically.

      Note that since engine >= 2.2.0 the @link(Direction) vector
      should always be normalized (length 1), and so you cannot change
      speed by changing it. See MoveSpeed, MoveHorizontalSpeed, MoveVerticalSpeed instead.

      When setting @link(Direction), @link(Up) will always be automatically
      adjusted to be orthogonal to @link(Direction). And vice versa ---
      when setting @link(Up), @link(Direction) will be adjusted.

      @groupBegin }
    property Position : TVector3Single read FPosition  write SetPosition;
    property Direction: TVector3Single read FDirection write SetDirection;
    property Up       : TVector3Single read FUp        write SetUp;
    { @groupEnd }

    { This is the upward direction of the world in which player moves.
      Must be always normalized (when setting this property, we take
      care to normalize it).

      This indicates how @link(Gravity) works.

      This is also the "normal" value for both @link(Up) and
      InitialUp --- one that means that player is looking straight
      foward. This is used for features like PreferGravityUpForRotations
      and/or PreferGravityUpForMoving.

      The default value of this vector is (0, 1, 0) (same as the default
      InitialUp and Up vectors). }
    property GravityUp: TVector3Single read FGravityUp write SetGravityUp;

    { If PreferGravityUpForRotations or PreferGravityUpForMoving
      then various operations are done with respect
      to GravityUp, otherwise they are done with
      respect to current @link(Up).

      With PreferGravityUpForRotations, this affects rotations:
      horizontal rotations (Input_LeftRot and Input_RightRot)
      and rotations caused by MouseLook.
      Also vertical rotations are bounded by MinAngleRadFromGravityUp
      when PreferGravityUpForRotations.

      Note that you can change it freely at runtime,
      and when you set PreferGravityUpForRotations from @false to @true
      then in nearest Idle
      calls @link(Up) will be gradually fixed, so that @link(Direction) and @link(Up)
      and GravityUp are on the same plane. Also @link(Direction) may be adjusted
      to honour MinAngleRadFromGravityUp.

      With PreferGravityUpForMoving, this affects moving:
      horizontal moving (forward, backward, strafe),
      and vertical moving (Input_UpMove and Input_DownMove).
      E.g. when PreferGravityUpForMoving then forward/backward keys are tied
      to horizontal plane defined by GravityUp.
      When not PreferGravityUpForMoving then forward/backward try to move
      you just in the @link(Direction). Which is usually more handy when
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
          is stronger for user, because GravityUp, @link(Up) and @link(Direction)
          always define the same plane in 3D space (i.e. along with the
          4th point, (0, 0, 0), for camera eye). Raising/bowing the head
          doesn't break this assumption.

          Without PreferGravityUpForRotations, we quickly start to do rotations
          in an awkward way --- once you do some vertical rotation,
          you changed @link(Up), and next horizontal rotation will be
          done versus new @link(Up).

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

    { This returns @link(Direction) vector rotated such that it is
      orthogonal to GravityUp. This way it returns @link(Direction) projected
      on the gravity horizontal plane, which neutralizes such things
      like raising / bowing your head.

      Note that when @link(Direction) and GravityUp are parallel,
      this just returns current @link(Direction) --- because in such case
      we can't project @link(Direction) on the horizontal plane. }
    function DirectionInGravityPlane: TVector3Single;

    { Set the most important properties of this camera, in one call.
      Sets initial camera properties (InitialPosition, InitialDirection,
      InitialUp),
      sets current camera properties to them (Position := InitialPosition
      and so on).

      Given here AInitialDirection, AInitialUp, AGravityUp will be normalized,
      and AInitialUp will be adjusted to be orthogonal to AInitialDirection
      (see SetInitialView).

      Sets also CameraPreferredHeight and CameraRadius.
      CameraPreferredHeight may be adjusted to be sensible
      (by calling CorrectCameraPreferredHeight(ACameraRadius)).
      You can pass ACameraRadius = 0.0 if you really don't want this
      CameraPreferredHeight adjustment. }
    procedure Init(const AInitialPosition, AInitialDirection,
      AInitialUp: TVector3Single;
      const AGravityUp: TVector3Single;
      const ACameraPreferredHeight: Single;
      const ACameraRadius: Single); overload;

    { Alternative Init that sets camera properties such that
      an object inside Box is more or less "visible good".
      Sets InitialCameraXxx properties to make it look right,
      sets current CameraXxx properties to InitialCameraXxx.
      Sets GravityUp to the same thing as InitialUp.
      Sets also CameraPreferredHeight to make it behave "sensibly". }
    procedure Init(const box: TBox3D; const ACameraRadius: Single); overload;

    { Deprecated name in TWalkCamera for GoToInitial. @deprecated }
    procedure Home;

    { This sets the minimal angle (in radians) between GravityUp
      and @link(Direction), and also between -GravityUp and @link(Direction).
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

    { Use mouse look to navigate (rotate the camera).

      This also makes mouse cursor of Container hidden, and forces
      mouse position to the middle of the window
      (to avoid the situation when mouse movement is blocked by screen borders). }
    property MouseLook: boolean read FMouseLook write SetMouseLook default false;

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

    { Call when mouse moves. Must be called to make MouseLook work. }
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;

    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseWheel(const Scroll: Single; const Vertical: boolean): boolean; override;

    { Things related to gravity ---------------------------------------- }

    { This unlocks a couple of features and automatic behaviors
      related to gravity. Gravity always drags the camera down to
      -GravityUp.

      Summary of things done by gravity:
      @unorderedList(
        @item(It uses OnGetHeightAbove to get camera height above the ground.)
        @item(It allows player to jump. See Input_Jump, IsJumping, MaxJumpHeight,
          JumpSpeedMultiply.)
        @item(It allows player to crouch. See Input_Crouch, CrouchHeight.)
        @item(It tries to keep @link(Position) above the ground on
          CameraPreferredHeight height.)
        @item(When current height is too small --- @link(Position) is moved up.
          See GrowingSpeed.)
        @item(When current height is too large --- we're falling down.
          See IsFallingDown, OnFalledDown, FallingDownStartSpeed,
          FallingDownSpeedIncrease, FallingDownEffect.)
        @item(It does head bobbing. See HeadBobbing, HeadBobbingTime.)
      )

      While there are many properties allowing you to control
      gravity behavior, most of them have initial values that should be
      sensible in all cases. The only things that you really want to take
      care of are: OnGetHeightAbove and CameraPreferredHeight.
      Everything else should basically work auto-magically.

      Note that Gravity setting is independent from
      PreferGravityUpForRotations or PreferGravityUpForMoving settings ---
      PreferGravityUpXxx say how the player controls work,
      Gravity says what happens to player due to ... well, due to gravity. }
    property Gravity: boolean
      read FGravity write FGravity default false;

    { When @link(Gravity) is on, @link(Position) tries to stay CameraPreferredHeight
      above the ground. Temporary it may be lower (player can
      shortly "duck" when he falls from high).

      This must always be >= 0.
      You should set this to something greater than zero to get sensible
      behavior of some things related to @link(Gravity),
      and also you should set OnGetHeightAbove.

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
      delegated to OnGetHeightAbove and OnMoveAllowed.
      Also, it's not exactly forced @italic(how) you should force this
      condition to hold. Sometimes the good solution is to adjust
      CameraRadius, not to adjust CameraPreferredHeight.

      Anyway, this method will make sure that this condition
      holds by eventually adjusting (making larger) CameraPreferredHeight.
      Note that for CameraRadius = 0.0 this will always leave
      CameraPreferredHeight as it is. }
    procedure CorrectCameraPreferredHeight;

    { Assign here the callback (or override UpdateHeightAbove)
      to say what is the current height of camera above the ground.
      This should be calculated like collision of ray from @link(Position)
      in direction -GravityUp with the scene.
      See TBase3D.OnGetHeightAbove for specification what returned parameters
      mean.

      Implementation of UpdateHeightAbove in this class
      calls OnGetHeightAbove, if assigned. (If not assigned,
      we assume no collision: IsAbove = @false, AboveHeight = MaxSingle,
      AboveGround = @nil). }
    property OnGetHeightAbove: TGetHeightAbove
      read FOnGetHeightAbove
      write FOnGetHeightAbove;

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
      Note that this is always relative to @link(Direction) length.
      @link(Direction) determines moving speed --- and so it determines
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
      GetHeightAbove) that camera is too high above the ground,
      then we will start falling down again, setting IsFallingDown
      back to true. (but then we will start falling down from the beginning,
      starting at given @link(Position) and with initial falling down speed).

      This is useful to call if you just changed @link(Position) because
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
      @link(Direction), that always determines every moving speed)
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

    { How fast do you jump. More precisely, during one second, you reach
      @code(MaxJumpDistance * JumpPower) distance. Note that this is
      independent from @italic(how high can you jump) --- when you reach
      MaxJumpDistance height, you stop jumping anyway. }
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

    { Controls head bobbing frequency. In the time of HeadBobbingTime seconds,
      we do full head bobbing sequence (camera swing up, then down again). }
    property HeadBobbingTime: Single
      read FHeadBobbingTime write FHeadBobbingTime
      default DefaultHeadBobbingTime;

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
      This works by gradually changing @link(Up) such that
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

    procedure GetView(out APos, ADir, AUp: TVector3Single); override;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3Single); override;
    function GetPosition: TVector3Single; override;
    procedure SetView(const APos, ADir, AUp: TVector3Single); override;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3Single); override;

    { Last known information about whether camera is over the ground.
      Updated by every UpdateHeightAbove call, using
      OnGetHeightAbove callback.

      Note that these are updated only when UpdateHeightAbove
      is continously called, which in practice means:
      only when @link(Gravity) is @true.

      Note that we do not (and, currently, cannot) track here if
      AboveGround pointer will be eventually released (which may happen
      if you release your 3D scene, or rebuild scene causing octree rebuild).
      This is not a problem for camera class, since we do not use this
      pointer for anything. But if you use this pointer,
      then you may want to take care to eventually set it @nil when
      your octree or such is released.

      @groupBegin }
    property IsAbove: boolean read FIsAbove;
    property AboveHeight: Single read FAboveHeight;
    property AboveGround: P3DTriangle read FAboveGround write FAboveGround;
    { @groupEnd }
  end;

  TCameraNavigationClass = (ncExamine, ncWalk);
  TCameraNavigationType = (ntExamine, ntWalk, ntFly, ntNone);

  { Camera that allows any kind of navigation (Examine, Walk).
    You can switch between navigation types, while preserving the camera view.

    This simply keeps an TExamineCamera and TWalkCamera instances inside,
    and passes events (key, mouse presses, idle) to the current one.
    Properties (like camera position, direction, up vectors) are simply
    set on both instances simultaneously.

    For some uses you can even directly access the internal camera instances
    inside @link(Examine) and @link(Walk) properties. However, do not
    change them directly @italic(when you can use instead a property of
    this class). For example, it is Ok to directly change input key
    by @noAutoLink(@code(Walk.Input_Forward)) (see TWalkCamera.Input_Forward).
    However, do not directly call @noAutoLink(@code(Walk.SetInitialView))
    (see TWalkCamera.SetInitialView), instead use a method of this class:
    TUniversalCamera.SetInitialView. This way both @link(Examine)
    and @link(Walk) will be kept in synch. }
  TUniversalCamera = class(TCamera)
  private
    FExamine: TExamineCamera;
    FWalk: TWalkCamera;
    FNavigationClass: TCameraNavigationClass;
    procedure SetNavigationClass(const Value: TCameraNavigationClass);
    function GetNavigationType: TCameraNavigationType;
    procedure SetNavigationType(const Value: TCameraNavigationType);
  protected
    procedure SetIgnoreAllInputs(const Value: boolean); override;
    procedure SetProjectionMatrix(const Value: TMatrix4Single); override;
    procedure SetContainer(const Value: IUIContainer); override;
    procedure SetCameraRadius(const Value: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Examine: TExamineCamera read FExamine;
    property Walk: TWalkCamera read FWalk;
    { Current (determined by NavigationClass) internal camera,
      that is either @link(Examine) or @link(Walk). }
    function Current: TCamera;

    function Matrix: TMatrix4Single; override;
    function RotationMatrix: TMatrix4Single; override;
    procedure GetView(out APos, ADir, AUp: TVector3Single); override;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3Single); override;
    function GetPosition: TVector3Single; override;
    procedure SetView(const APos, ADir, AUp: TVector3Single); override;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3Single); override;

    function PositionInside(const X, Y: Integer): boolean; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function KeyDown(Key: TKey; C: char): boolean; override;
    function KeyUp(Key: TKey; C: char): boolean; override;
    function MouseDown(const Button: TMouseButton): boolean; override;
    function MouseUp(const Button: TMouseButton): boolean; override;
    function MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean; override;
    function MouseWheel(const Scroll: Single; const Vertical: boolean): boolean; override;

    procedure ContainerResize(const AContainerWidth, AContainerHeight: Cardinal); override;

    procedure SetInitialView(
      const AInitialPosition: TVector3Single;
      AInitialDirection, AInitialUp: TVector3Single;
      const TransformCurrentCamera: boolean); override;

    function PreventsComfortableDragging: boolean; override;
  published
    { Choose navigation method by choosing particular camera class.
      The names of this correspond to camera classes (TExamineCamera,
      TWalkCamera). }
    property NavigationClass: TCameraNavigationClass
      read FNavigationClass write SetNavigationClass default ncExamine;

    { Choose navigation method by choosing particular camera class,
      and gravity and some other properties.

      This is a shortcut property for reading / writing
      a couple of other properties. When you set this, a couple of other
      properties are set. When you read this, we determine a sensible
      answer from a couple of other properties values.

      Setting this sets:
      @unorderedList(
        @itemSpacing compact
        @item NavigationClass,
        @item IgnoreAllInputs,
        @item Walk.Gravity (see TWalkCamera.Gravity),
        @item Walk.PreferGravityUpForRotations (see TWalkCamera.PreferGravityUpForRotations),
        @item Walk.PreferGravityUpForMoving (see TWalkCamera.PreferGravityUpForMoving)
      )

      If you write to NavigationType, then you @italic(should not) touch the
      above properties directly. That's because not every combination of
      above properties correspond to some sensible value of NavigationType.
      If you directly set some weird configuration, reading NavigationType will
      try it's best to determine the closest TCameraNavigationType value
      that is similar to your configuration. }
    property NavigationType: TCameraNavigationType
      read GetNavigationType write SetNavigationType default ntExamine;
  end;

{ See TWalkCamera.CorrectCameraPreferredHeight.
  This is a global version, sometimes may be useful. }
procedure CorrectCameraPreferredHeight(var CameraPreferredHeight: Single;
  const CameraRadius: Single; const CrouchHeight, HeadBobbing: Single);

const
  { Default camera direction and up vectors, used to define the meaning
    of "camera orientation" for CamDirUp2Orient routines.
    These match VRML/X3D default camera values.
    @groupBegin }
  DefaultCameraDirection: TVector3Single = (0, 0, -1);
  DefaultCameraUp: TVector3Single = (0, 1, 0);
  { @groupEnd }

{ Convert camera direction and up vectors into VRML "orientation" vector.

  Orientation expresses CamDir and CamUp as 4-item vector
  (SFRotation). First three items are the Axis (normalized) and the
  4th is the Angle (in radians). Meaning: if you rotate the standard
  direction and up (see DefaultCameraDirection, DefaultCameraUp) around Axis
  by the Angle, then you get CamDir and CamUp.

  Given here CamDir and CamUp must be orthogonal and non-zero.
  Their lengths are not relevant (that is, you don't need to normalize them
  before passing here).

  @groupBegin }
function CamDirUp2Orient(const CamDir, CamUp: TVector3Single): TVector4Single;
procedure CamDirUp2Orient(const CamDir, CamUp: TVector3Single;
  out OrientAxis: TVector3Single; out OrientRadAngle: Single);
{ @groupEnd }

{ Convert camera direction and up vectors into "rotation quaternion" of
  VRML "orientation".

  VRML orientation expresses camera direction and up as a rotation.
  This means that you should rotate the standard
  direction and up (see DefaultCameraDirection, DefaultCameraUp) by this rotation
  to get CamDir and CamUp.

  Given here CamDir and CamUp must be orthogonal and non-zero.
  Their lengths are not relevant (that is, you don't need to normalize them
  before passing here).

  @groupBegin }
function CamDirUp2OrientQuat(CamDir, CamUp: TVector3Single): TQuaternion;
{ @groupEnd }

{ Calculate sensible camera configuration to see the whole Box.

  WantedDirection and WantedUp indicate desired look direction/up axis
  (0, 1 or 2 for X, Y or Z). WantedDirectionPositive and WantedUpPositive
  indicate if we want the positive axis. Obviously look direction and up
  cannot be parallel, so WantedDirection must be different than WantedUp.

  Returned Direction, Up, GravityUp are normalized. }
procedure CameraViewpointForWholeScene(const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean;
  out Position, Direction, Up, GravityUp: TVector3Single);

procedure Register;

implementation

uses Math, KambiStringUtils;

procedure Register;
begin
  RegisterComponents('Kambi', [TExamineCamera, TWalkCamera, TUniversalCamera]);
end;

{ Define this to have Input_RightRot/LeftRot (right / left arrow keys by default)
  work in "single step" mode (single press => one rotation by 5 degrees)
  instead of normal "continous" mode (smooth rotation when you hold the key
  pressed).

  Only in the Walk mode.

  Note that even in the "single step" mode, holding the key for a longer time
  will cause successive rotations, since key-down events are repeated.
  (Just like in a text editor holding a letter key for some time will
  cause inserting the same letter again and again...) This could be
  removed in SINGLE_STEP_ROTATION code, but it's not --- it's useful and
  desired IMHO :) }
{ $define SINGLE_STEP_ROTATION}

const
  DefaultDirection: TVector3Single = (0, 0, -1);
  DefaultUp: TVector3Single = (0, 1, 0);

{ TInputShortcut ------------------------------------------------------------- }

constructor TInputShortcut.Create(AKey1: TKey; AKey2: TKey; ACharacter: Char;
  AMouseButtonUse: boolean; AMouseButton: TMouseButton;
  const AMouseWheel: TMouseWheelDirection);
begin
  inherited Create;
  FDefaultKey1 := AKey1;
  FDefaultKey2 := AKey2;
  FDefaultCharacter := ACharacter;
  FDefaultMouseButtonUse := AMouseButtonUse;
  FDefaultMouseButton := AMouseButton;
  FDefaultMouseWheel := AMouseWheel;
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
  FMouseWheel := Source.DefaultMouseWheel;

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
    DefaultMouseWheel := Source.DefaultMouseWheel;
  end;

  FKey1 := Source.Key1;
  FKey2 := Source.Key2;
  FCharacter := Source.Character;
  FMouseButtonUse := Source.MouseButtonUse;
  FMouseButton := Source.MouseButton;
  FMouseWheel := Source.MouseWheel;

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
  FMouseWheel := mwNone;

  { I don't set here properties, but directly set FXxx fields,
    so that I can call Changed only once. }
  Changed;
end;

function TInputShortcut.IsPressed(Pressed: TKeysPressed;
  const MousePressed: TMouseButtons): boolean;
begin
  Result :=
    ( (Pressed <> nil) and (Pressed.Keys[Key1] or
                            Pressed.Keys[Key2] or
                            Pressed.Characters[Character]) ) or
    ( MouseButtonUse and (MouseButton in MousePressed) );
end;

function TInputShortcut.IsPressed(Container: IUIContainer): boolean;
begin
  Result := IsPressed(Container.Pressed, Container.MousePressed);
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

function TInputShortcut.IsMouseWheel(const AMouseWheel: TMouseWheelDirection): boolean;
begin
  Result := (AMouseWheel <> mwNone) and (AMouseWheel = MouseWheel);
end;

function TInputShortcut.IsEvent(AKey: TKey; ACharacter: Char;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection): boolean;
begin
  if AMousePress then
    Result := IsMouseButton(AMouseButton) else
  if AMouseWheel <> mwNone then
    Result := IsMouseWheel(AMouseWheel) else
    Result := IsKey(AKey, ACharacter);
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

  if MouseWheel <> mwNone then
  begin
    if Result <> '' then Result += ' or ';
    Result += Format('wheel "%s"', [MouseWheelDirectionStr[MouseWheel]]);
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

procedure TInputShortcut.SetMouseWheel(const Value: TMouseWheelDirection);
begin
  FMouseWheel := Value;
  Changed;
end;

{ TCamera ------------------------------------------------------------ }

constructor TCamera.Create(AOwner: TComponent);
begin
  inherited;
  FProjectionMatrix := IdentityMatrix4Single;
  FInitialPosition  := Vector3Single(0, 0, 0);
  FInitialDirection := DefaultDirection;
  FInitialUp        := DefaultUp;
  FCameraRadius := DefaultCameraRadius;
end;

procedure TCamera.VisibleChange;
begin
  RecalculateFrustum;
  inherited;
end;

procedure TCamera.BeginVisibleChangeSchedule;
begin
  { IsVisibleChangeScheduled = false always when VisibleChangeSchedule = 0. }
  Assert((VisibleChangeSchedule <> 0) or (not IsVisibleChangeScheduled));

  Inc(VisibleChangeSchedule);
end;

procedure TCamera.ScheduleVisibleChange;
begin
  if VisibleChangeSchedule = 0 then
    VisibleChange else
    IsVisibleChangeScheduled := true;
end;

procedure TCamera.EndVisibleChangeSchedule;
begin
  Dec(VisibleChangeSchedule);
  if (VisibleChangeSchedule = 0) and IsVisibleChangeScheduled then
  begin
    VisibleChange;
    IsVisibleChangeScheduled := false;
  end;
end;

procedure TCamera.SetIgnoreAllInputs(const Value: boolean);
begin
  FIgnoreAllInputs := Value;
end;

procedure TCamera.RecalculateFrustum;
begin
  FFrustum.Init(ProjectionMatrix, Matrix);
end;

procedure TCamera.SetProjectionMatrix(const Value: TMatrix4Single);
begin
  FProjectionMatrix := Value;
  RecalculateFrustum;
end;

procedure TCamera.SetCameraRadius(const Value: Single);
begin
  FCameraRadius := Value;
end;

function TCamera.PositionInside(const X, Y: Integer): boolean;
begin
  Result := Exists; { always inside }
end;

procedure TCamera.Ray(const WindowX, WindowY: Integer;
  const PerspectiveView: boolean;
  const PerspectiveViewAngles: TVector2Single;
  const OrthoViewDimensions: TVector4Single;
  out Ray0, RayVector: TVector3Single);
begin
  Assert(ContainerSizeKnown, 'Camera container size not known yet (probably camera not added to Controls list), cannot use TCamera.Ray');
  CustomRay(0, 0, ContainerWidth, ContainerHeight, ContainerHeight,
    WindowX, WindowY,
    PerspectiveView, PerspectiveViewAngles, OrthoViewDimensions, Ray0, RayVector);
end;

procedure TCamera.MouseRay(
  const PerspectiveView: boolean;
  const PerspectiveViewAngles: TVector2Single;
  const OrthoViewDimensions: TVector4Single;
  out Ray0, RayVector: TVector3Single);
begin
  Assert(ContainerSizeKnown, 'Camera container size not known yet (probably camera not added to Controls list), cannot use TCamera.MouseRay');
  CustomRay(0, 0, ContainerWidth, ContainerHeight, ContainerHeight,
    Container.MouseX, Container.MouseY,
    PerspectiveView, PerspectiveViewAngles, OrthoViewDimensions, Ray0, RayVector);
end;

procedure TCamera.CustomRay(
  const ViewportLeft, ViewportBottom: Integer;
  const ViewportWidth, ViewportHeight, WindowHeight: Cardinal;
  const WindowX, WindowY: Integer;
  const PerspectiveView: boolean;
  const PerspectiveViewAngles: TVector2Single;
  const OrthoViewDimensions: TVector4Single;
  out Ray0, RayVector: TVector3Single);
var
  Pos, Dir, Up: TVector3Single;
begin
  GetView(Pos, Dir, Up);

  PrimaryRay(
    WindowX - ViewportLeft, (WindowHeight - WindowY) - ViewportBottom,
    ViewportWidth, ViewportHeight,
    Pos, Dir, Up,
    PerspectiveView, PerspectiveViewAngles, OrthoViewDimensions,
    Ray0, RayVector);
end;

procedure TCamera.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;
  if Exists and Animation then
  begin
    AnimationCurrentTime += CompSpeed;
    if AnimationCurrentTime > AnimationEndTime then
    begin
      Animation := false;
      { When animation ended, make sure you're exactly at the final view. }
      SetView(AnimationEndPosition, AnimationEndDirection, AnimationEndUp);
    end else
    begin
      SetView(
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginPosition , AnimationEndPosition),
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginDirection, AnimationEndDirection),
        Lerp(AnimationCurrentTime / AnimationEndTime, AnimationBeginUp       , AnimationEndUp));
    end;
  end;
end;

procedure TCamera.AnimateTo(const Pos, Dir, Up: TVector3Single; const Time: TKamTime);
begin
  GetView(
    AnimationBeginPosition,
    AnimationBeginDirection,
    AnimationBeginUp);

  AnimationEndPosition := Pos;
  AnimationEndDirection := Dir;
  AnimationEndUp := Up;

  AnimationEndTime := Time;
  AnimationCurrentTime := 0;
  { No point in doing animation (especially since it blocks camera movement
    for Time seconds) if we're already there. }
  Animation := not (
    VectorsEqual(AnimationBeginPosition , AnimationEndPosition) and
    VectorsEqual(AnimationBeginDirection, AnimationEndDirection) and
    VectorsEqual(AnimationBeginUp       , AnimationEndUp));
end;

procedure TCamera.AnimateTo(OtherCamera: TCamera; const Time: TKamTime);
var
  Pos, Dir, Up: TVector3Single;
begin
  OtherCamera.GetView(Pos, Dir, Up);
  AnimateTo(Pos, Dir, Up, Time);
end;

function TCamera.IsAnimation: boolean;
begin
  Result := Animation;
end;

procedure TCamera.SetInitialView(
  const AInitialPosition: TVector3Single;
  AInitialDirection, AInitialUp: TVector3Single;
  const TransformCurrentCamera: boolean);
var
  OldInitialOrientation, NewInitialOrientation, Orientation: TQuaternion;
  Pos, Dir, Up: TVector3Single;
begin
  NormalizeTo1st(AInitialDirection);
  NormalizeTo1st(AInitialUp);
  MakeVectorsOrthoOnTheirPlane(AInitialUp, AInitialDirection);

  if TransformCurrentCamera then
  begin
    GetView(Pos, Dir, Up);

    VectorAddTo1st(Pos, VectorSubtract(AInitialPosition, FInitialPosition));

    if not (VectorsPerfectlyEqual(FInitialDirection, AInitialDirection) and
            VectorsPerfectlyEqual(FInitialUp       , AInitialUp ) ) then
    begin
      OldInitialOrientation := CamDirUp2OrientQuat(FInitialDirection, FInitialUp);
      NewInitialOrientation := CamDirUp2OrientQuat(AInitialDirection, AInitialUp);
      Orientation           := CamDirUp2OrientQuat(Dir, Up);

      { I want new Orientation :=
          (Orientation - OldInitialOrientation) + NewInitialOrientation. }
      Orientation := QuatMultiply(QuatConjugate(OldInitialOrientation), Orientation);
      Orientation := QuatMultiply(NewInitialOrientation, Orientation);

      { Now that we have Orientation, transform it into new Dir/Up. }
      Dir := QuatRotate(Orientation, DefaultCameraDirection);
      Up  := QuatRotate(Orientation, DefaultCameraUp);
    end;

    { This will do ScheduleVisibleChange }
    SetView(Pos, Dir, Up);
  end;

  FInitialPosition  := AInitialPosition;
  FInitialDirection := AInitialDirection;
  FInitialUp        := AInitialUp;
end;

procedure TCamera.GoToInitial;
begin
  SetView(FInitialPosition, FInitialDirection, FInitialUp);
end;

function TCamera.PreventsComfortableDragging: boolean;
begin
  Result := false;
end;

{ TExamineCamera ------------------------------------------------------------ }

constructor TExamineCamera.Create(AOwner: TComponent);
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

  FMouseNavigation := true;
  ExclusiveEvents := false;

  FModelBox := EmptyBox3D;
  FMoveAmount := ZeroVector3Single;
  FRotations := QuatIdentityRot;
  FRotationsAnim := ZeroVector3Single;
  FScaleFactor := 1;

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

destructor TExamineCamera.Destroy;
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

function TExamineCamera.Matrix: TMatrix4Single;
begin
  Result := TranslationMatrix(VectorAdd(MoveAmount, FCenterOfRotation));
  Result := MatrixMult(Result, QuatToRotationMatrix(Rotations));
  Result := MatrixMult(Result, ScalingMatrix(Vector3Single(ScaleFactor, ScaleFactor, ScaleFactor)));
  Result := MatrixMult(Result, TranslationMatrix(VectorNegate(FCenterOfRotation)));
end;

function TExamineCamera.MatrixInverse: TMatrix4Single;
begin
  { This inverse always exists, assuming ScaleFactor is <> 0. }

  Result := TranslationMatrix(VectorNegate(VectorAdd(MoveAmount, FCenterOfRotation)));
  Result := MatrixMult(QuatToRotationMatrix(QuatConjugate(Rotations)), Result);
  Result := MatrixMult(ScalingMatrix(Vector3Single(1/ScaleFactor, 1/ScaleFactor, 1/ScaleFactor)), Result);
  Result := MatrixMult(TranslationMatrix(FCenterOfRotation), Result);
end;

function TExamineCamera.RotationMatrix: TMatrix4Single;
begin
  Result := QuatToRotationMatrix(Rotations);
end;

procedure TExamineCamera.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
var i: integer;
    move_change, rot_speed_change, scale_change: Single;
    ModsDown: TModifierKeys;
    RotChange: Single;
begin
  inherited;

  { Do not handle keys or rotations etc. }
  if IsAnimation or (not Exists) then Exit;

  { If given RotationsAnim component is zero, no need to change current Rotations.
    What's more important, this avoids the need to call VisibleChange,
    so things like PostRedisplay will not be continously called when
    model doesn't rotate.

    We check using exact equality <> 0, this is Ok since the main point is to
    avoid work when StopRotating was called and user didn't touch arrow
    keys (that increase RotationsAnim). Exact equality is Ok check
    to detect this. }

  if not PerfectlyZeroVector(FRotationsAnim) then
  begin
    RotChange := CompSpeed;

    if FRotationsAnim[0] <> 0 then
      FRotations := QuatMultiply(QuatFromAxisAngle(UnitVector3Single[0],
        FRotationsAnim[0] * RotChange), FRotations);

    if FRotationsAnim[1] <> 0 then
      FRotations := QuatMultiply(QuatFromAxisAngle(UnitVector3Single[1],
        FRotationsAnim[1] * RotChange), FRotations);

    if FRotationsAnim[2] <> 0 then
      FRotations := QuatMultiply(QuatFromAxisAngle(UnitVector3Single[2],
        FRotationsAnim[2] * RotChange), FRotations);

    QuatLazyNormalizeTo1st(FRotations);

    VisibleChange;
  end;

  if HandleMouseAndKeys and (not IgnoreAllInputs) then
  begin
    if IsEmptyOrZeroBox3D(ModelBox) then
      move_change := CompSpeed else
      move_change := Box3DAvgSize(ModelBox) * CompSpeed;
    rot_speed_change := 5 * CompSpeed;

    { we will apply CompSpeed to scale_change later }
    scale_change := 1.5;

    ModsDown := ModifiersDown(Container.Pressed);

    if ModsDown = [mkCtrl] then
    begin
      for i := 0 to 2 do
      begin
        if Inputs_Move[i, true ].IsPressed(Container) then
          Move(i, +move_change);
        if Inputs_Move[i, false].IsPressed(Container) then
          Move(i, -move_change);
      end;
    end else
    if ModsDown = [] then
    begin
      for i := 0 to 2 do
      begin
        if Inputs_Rotate[i, true ].IsPressed(Container) then
          Rotate(i, +rot_speed_change);
        if Inputs_Rotate[i, false].IsPressed(Container) then
          Rotate(i, -rot_speed_change);
      end;
    end;

    if Input_ScaleLarger.IsPressed(Container) then
      Scale(Power(scale_change, CompSpeed));
    if Input_ScaleSmaller.IsPressed(Container) then
      Scale(Power(1 / scale_change, CompSpeed));
  end;
end;

function TExamineCamera.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

procedure TExamineCamera.StopRotating;
begin
  FRotationsAnim := ZeroVector3Single;
  VisibleChange;
end;

procedure TExamineCamera.Rotate(coord: integer; const SpeedChange: Single);
begin
  FRotationsAnim[coord] += SpeedChange;
  VisibleChange;
end;

procedure TExamineCamera.Scale(const ScaleBy: Single);
begin FScaleFactor *= ScaleBy; VisibleChange; end;

procedure TExamineCamera.Move(coord: integer; const MoveDistance: Single);
begin FMoveAmount[coord] += MoveDistance; VisibleChange; end;

procedure TExamineCamera.Init(const AModelBox: TBox3D; const ACameraRadius: Single);
begin
 ModelBox := AModelBox;
 CameraRadius := ACameraRadius;
 Home;
end;

{ TExamineCamera.Set* properties }

procedure TExamineCamera.SetRotationsAnim(const Value: TVector3Single);
begin FRotationsAnim := Value; VisibleChange; end;

procedure TExamineCamera.SetRotations(const Value: TQuaternion);
begin FRotations := Value; VisibleChange; end;

procedure TExamineCamera.SetScaleFactor(const Value: Single);
begin FScaleFactor := Value; VisibleChange; end;

procedure TExamineCamera.SetMoveAmount(const Value: TVector3Single);
begin FMoveAmount := Value; VisibleChange; end;

procedure TExamineCamera.SetCenterOfRotation(const Value: TVector3Single);
begin FCenterOfRotation := Value; VisibleChange; end;

procedure TExamineCamera.Home;
var
  Direction, Up, GravityUp: TVector3Single;
begin
  { Make the same view as
    CameraViewpointForWholeScene call in TVRMLScene.CameraFromViewpoint,
    to make "Home" behavior same as going to the default viewpoint.
    Nice for user. }
  CameraViewpointForWholeScene(ModelBox, 2, 1, false, true,
    FMoveAmount, Direction, Up, GravityUp);
  FMoveAmount := - FMoveAmount;

  { Just reset the rest of stuff }
  FRotations := QuatIdentityRot;
  FRotationsAnim := ZeroVector3Single;
  FScaleFactor := 1.0;

  VisibleChange;
end;

procedure TExamineCamera.SetModelBox(const Value: TBox3D);
begin
  FModelBox := Value;
  if IsEmptyBox3D(FModelBox) then
    FCenterOfRotation := Vector3Single(0, 0, 0) { any dummy value } else
    FCenterOfRotation := Box3DMiddle(FModelBox);
  VisibleChange;
end;

function TExamineCamera.EventDown(AKey: TKey; ACharacter: Char;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection): boolean;
begin
  if IgnoreAllInputs or IsAnimation then Exit(false);

  if Input_StopRotating.IsEvent(AKey, ACharacter, AMousePress, AMouseButton, AMouseWheel) then
  begin
    StopRotating;
    Result := ExclusiveEvents;
  end else
  if Input_Home.IsEvent(AKey, ACharacter, AMousePress, AMouseButton, AMouseWheel) then
  begin
    Home;
    Result := ExclusiveEvents;
  end else
    Result := false;
end;

function TExamineCamera.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  if ModifiersDown(Container.Pressed) <> [] then Exit;

  Result := EventDown(Key, C, false, mbLeft, mwNone);
end;

function TExamineCamera.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := EventDown(K_None, #0, true, Button, mwNone);
end;

function TExamineCamera.Zoom(const Factor: Single): boolean;
var
  Size: Single;
  OldMoveAmount, OldPosition: TVector3Single;
begin
  Result := not IsEmptyOrZeroBox3D(FModelBox);
  if Result then
  begin
    Size := Box3DAvgSize(FModelBox);

    OldMoveAmount := FMoveAmount;
    OldPosition := GetPosition;

    FMoveAmount[2] += Size * Factor;

    { Cancel zoom in, don't allow to go to the other side of the model too far.
      Note that Box3DPointDistance = 0 when you're inside the box,
      so zoomin in/out inside the box is still always allowed.
      See http://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&t=24 }
    if (Factor > 0) and
       (Box3DPointDistance(FModelBox, GetPosition) >
        Box3DPointDistance(FModelBox, OldPosition)) then
    begin
      FMoveAmount := OldMoveAmount;
      Exit(false);
    end;

    VisibleChange
  end;
end;

function TExamineCamera.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
var
  Size: Single;
  ModsDown: TModifierKeys;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

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
    or should be ignored, do nothing. }
  if (Container.MousePressed = []) or (not MouseNavigation) or
     IgnoreAllInputs or IsAnimation then
    Exit;

  ModsDown := ModifiersDown(Container.Pressed) * [mkShift, mkCtrl];

  { Rotating }
  if (mbLeft in Container.MousePressed) and (ModsDown = []) then
  begin
    FRotations := QuatMultiply(
      QuatFromAxisAngle(Vector3Single(0, 1, 0), (NewX - OldX) / 100),
      FRotations);
    FRotations := QuatMultiply(
      QuatFromAxisAngle(Vector3Single(1, 0, 0), (NewY - OldY) / 100),
      FRotations);
    VisibleChange;
    Result := ExclusiveEvents;
  end else

  { Moving uses box size, so requires non-empty box. }

  { Note: checks for (ModsDown = []) are not really needed below,
    mkRight / Middle don't serve any other purpose anyway.
    But I think that it improves user ability to "discover" these shortcuts
    and keys, otherwise it seems strange that shift/ctrl change the
    meaning of mbLeft but they don't change the meaning of mbRight / Middle ? }

  { Moving closer/further }
  if ( (mbRight in Container.MousePressed) and (ModsDown = []) ) or
     ( (mbLeft in Container.MousePressed) and (ModsDown = [mkCtrl]) ) then
  begin
    if Zoom((NewY - OldY) / 200) then
      Result := ExclusiveEvents;
  end;

  { Moving left/right/down/up }
  if (not IsEmptyBox3D(FModelBox)) and
     ( ( (mbMiddle in Container.MousePressed) and (ModsDown = []) ) or
       ( (mbLeft in Container.MousePressed) and (ModsDown = [mkShift]) ) ) then
  begin
    Size := Box3DAvgSize(FModelBox);
    FMoveAmount[0] -= Size * (OldX - NewX) / 200;
    FMoveAmount[1] -= Size * (NewY - OldY) / 200;
    VisibleChange;
    Result := ExclusiveEvents;
  end;
end;

function TExamineCamera.MouseWheel(const Scroll: Single; const Vertical: boolean): boolean;
begin
  Result := inherited;
  if Result or (not Exists) or
    (not MouseNavigation) or IgnoreAllInputs or IsAnimation or
    (ModifiersDown(Container.Pressed) * [mkShift, mkCtrl] <> []) then
    Exit;

  { For now, this is hardcoded, we don't call EventDown here }

  if Zoom(Scroll / 10) then
    Result := ExclusiveEvents;
end;

procedure TExamineCamera.GetView(
  out APos, ADir, AUp: TVector3Single);
var
  M: TMatrix4Single;
begin
  M := MatrixInverse;

  { These MatrixMultPoint/Direction should never fail with ETransformedResultInvalid.
    That's because M is composed from translations, rotations, scaling,
    which preserve points/directions (4th component in homogeneus coordinates)
    nicely. }

  APos := MatrixMultPoint(M, Vector3Single(0, 0, 0));
  ADir := MatrixMultDirection(M, DefaultDirection);
  AUp  := MatrixMultDirection(M, DefaultUp);
end;

procedure TExamineCamera.GetView(out APos, ADir, AUp, AGravityUp: TVector3Single);
begin
  GetView(APos, ADir, AUp);
  AGravityUp := DefaultUp; { nothing more sensible for Examine camera }
end;

function TExamineCamera.GetPosition: TVector3Single;
begin
  Result := MatrixMultPoint(MatrixInverse, Vector3Single(0, 0, 0));
end;

procedure TExamineCamera.SetView(const APos, ADir, AUp: TVector3Single);
var
  Up: TVector3Single;
begin
  FMoveAmount := -APos;

  { Make good (orthogonal to ADir) up vector, CamDirUp2OrientQuat requires this }
  Up := AUp;
  MakeVectorsOrthoOnTheirPlane(Up, ADir);

  FRotations := QuatConjugate(CamDirUp2OrientQuat(ADir, Up));

{ TODO: remove this testing, once "hard case" in CamDirUp2OrientQuat
  will be handled Ok:

  if not VectorsEqual(QuatRotate(FRotations, ADir), DefaultDirection, 0.01) then
  begin
    Writeln('oh yes, dir wrong: ', VectorToNiceStr(QuatRotate(FRotations, ADir)));
    Writeln('  q: ', VectorToNiceStr(FRotations.Vector4));
  end;

  if not VectorsEqual(QuatRotate(FRotations, Up), DefaultUp, 0.01) then
    Writeln('oh yes, up wrong: ', VectorToNiceStr(QuatRotate(FRotations, Up)));
}

  { We have to fix our FMoveAmount, since our TExamineCamera.Matrix
    applies our move *first* before applying rotation
    (and this is good, as it allows rotating around object center,
    not around camera).

    Alternative implementation of this would call QuatToRotationMatrix and
    then simulate multiplying this rotation matrix * translation matrix
    of FMoveAmount. But we can do this directly.

    We also note at this point that rotation is done around
    (FMoveAmount + FCenterOfRotation). But FCenterOfRotation is not
    included in MoveAmount. }
  FMoveAmount := QuatRotate(FRotations, FMoveAmount + FCenterOfRotation)
    - FCenterOfRotation;

  { Reset ScaleFactor to 1, this way the camera view corresponds
    exactly to the wanted SetView view. }
  FScaleFactor := 1;

  { Stopping the rotation animation wasn't really promised in SetView
    interface. But this is nice for user, otherwise after e.g. jumping
    to viewpoint you may find yourself still rotating --- usually distracting. }
  FRotationsAnim := ZeroVector3Single;

  ScheduleVisibleChange;
end;

procedure TExamineCamera.SetView(const APos, ADir, AUp, AGravityUp: TVector3Single);
begin
  SetView(APos, ADir, AUp);
  { Ignore AGravityUp }
end;

function TExamineCamera.PreventsComfortableDragging: boolean;
begin
  Result := true;
end;

{ TWalkCamera ---------------------------------------------------------------- }

constructor TWalkCamera.Create(AOwner: TComponent);
begin
  inherited;
  FPosition  := InitialPosition;
  FDirection := InitialDirection;
  FUp        := InitialUp;
  FGravityUp := DefaultUp;

  FMoveHorizontalSpeed := 1;
  FMoveVerticalSpeed := 1;
  FMoveSpeed := 1;
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
  FHeadBobbingTime := DefaultHeadBobbingTime;
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
  FInput_IncreaseCameraPreferredHeight := TInputShortcut.Create(K_Insert , K_None, #0, false, mbLeft);
  FInput_DecreaseCameraPreferredHeight := TInputShortcut.Create(K_Delete , K_None, #0, false, mbLeft);
  FInput_GravityUp    := TInputShortcut.Create(K_Home        , K_None, #0, false, mbLeft);

  { For move speed inc/dev we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  FInput_MoveSpeedInc := TInputShortcut.Create(K_Numpad_Plus , K_None, '+', false, mbLeft);
  FInput_MoveSpeedDec := TInputShortcut.Create(K_Numpad_Minus, K_None, '-', false, mbLeft);

  FInput_Jump         := TInputShortcut.Create(K_A           , K_None, #0, false, mbRight);
  FInput_Crouch       := TInputShortcut.Create(K_Z           , K_None, #0, false, mbLeft);
end;

destructor TWalkCamera.Destroy;
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
  FreeAndNil(FInput_IncreaseCameraPreferredHeight);
  FreeAndNil(FInput_DecreaseCameraPreferredHeight);
  FreeAndNil(FInput_GravityUp);
  FreeAndNil(FInput_MoveSpeedInc);
  FreeAndNil(FInput_MoveSpeedDec);
  FreeAndNil(FInput_Jump);
  FreeAndNil(FInput_Crouch);
  inherited;
end;

function TWalkCamera.Matrix: TMatrix4Single;
begin
  { Yes, below we compare Fde_UpRotate with 0.0 using normal
    (precise) <> operator. Don't worry --- Fde_Stabilize in Idle
    will take care of eventually setting Fde_UpRotate to
    a precise 0.0. }
  if Fde_UpRotate <> 0.0 then
    Result := LookDirMatrix(Position, Direction,
      RotatePointAroundAxisDeg(Fde_UpRotate, Up, Direction)) else
    Result := LookDirMatrix(Position, Direction, Up);
end;

function TWalkCamera.RotationMatrix: TMatrix4Single;
begin
 result := FastLookDirMatrix(Direction, Up);
end;

function TWalkCamera.DoMoveAllowed(const ProposedNewPos: TVector3Single;
  out NewPos: TVector3Single; const BecauseOfGravity: boolean): boolean;
begin
 if Assigned(OnMoveAllowed) then
  Result := OnMoveAllowed(Self, ProposedNewPos, NewPos, BecauseOfGravity) else
 begin
  Result := true;
  NewPos := ProposedNewPos;
 end;
end;

procedure TWalkCamera.UpdateHeightAbove;
begin
  if Assigned(OnGetHeightAbove) then
    OnGetHeightAbove(Self, FIsAbove, FAboveHeight, FAboveGround) else
  begin
    FIsAbove := false;
    FAboveHeight := MaxSingle;
    FAboveGround := nil;
  end;
end;

function TWalkCamera.UseHeadBobbing: boolean;
begin
  Result := Gravity and (HeadBobbing <> 0.0);
end;

function TWalkCamera.RealCameraPreferredHeightNoHeadBobbing: Single;
begin
  Result := CameraPreferredHeight;

  if IsCrouching then
    Result *= CrouchHeight;
end;

function TWalkCamera.RealCameraPreferredHeight: Single;
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

function TWalkCamera.RealCameraPreferredHeightMargin: Single;
begin
  { I tried using here something smaller like
    SingleEqualityEpsilon, but this was not good. }
  Result := RealCameraPreferredHeight * 0.01;
end;

procedure TWalkCamera.RotateAroundGravityUp(const AngleDeg: Single);
var Axis: TVector3Single;
begin
 { nie obracamy Direction wokol Up, takie obroty w polaczeniu z
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
 if AngleRadBetweenVectors(Up, GravityUp) > Pi/2 then
  Axis := VectorNegate(GravityUp) else
  Axis := GravityUp;

 FUp := RotatePointAroundAxisDeg(AngleDeg, Up, Axis);
 FDirection := RotatePointAroundAxisDeg(AngleDeg, Direction, Axis);

 ScheduleVisibleChange;
end;

procedure TWalkCamera.RotateAroundUp(const AngleDeg: Single);
begin
  { We know that RotatePointAroundAxisDeg below doesn't change the length
    of the Direction (so it will remain normalized) and it will keep
    Direction and Up vectors orthogonal. }
  FDirection := RotatePointAroundAxisDeg(AngleDeg, FDirection, FUp);
  ScheduleVisibleChange;
end;

procedure TWalkCamera.RotateHorizontal(const AngleDeg: Single);
begin
  if PreferGravityUpForRotations then
    RotateAroundGravityUp(AngleDeg) else
    RotateAroundUp(AngleDeg);
end;

procedure TWalkCamera.RotateVertical(const AngleDeg: Single);
var
  Side: TVector3Single;
  AngleRad: Single;

  procedure DoRealRotate;
  begin
    { Rotate Up around Side }
    FUp        := RotatePointAroundAxisRad(AngleRad, Up,        Side);
    { Rotate Direction around Side }
    FDirection := RotatePointAroundAxisRad(AngleRad, Direction, Side);
  end;

var
  AngleRadBetween: Single;
begin
  AngleRad := DegToRad(AngleDeg);

  if PreferGravityUpForRotations and (MinAngleRadFromGravityUp <> 0.0) then
  begin
    Side := VectorProduct(Direction, GravityUp);
    if ZeroVector(Side) then
    begin
      { Brutally adjust Direction and Up to be correct.
        This should happen only if your code was changing values of
        PreferGravityUpForRotations and MinAngleRadFromGravityUp at runtime.
        E.g. first you let Direction and Up to be incorrect,
        and then you set PreferGravityUpForRotations to @true and
        MinAngleRadFromGravityUp
        to > 0 --- and suddenly we find that Up can be temporarily bad. }
      FDirection := InitialDirection;
      FUp := InitialUp;

      { Now check Side again. If it's still bad, this means that the
        InitialDirection is parallel to GravityUp. This shouldn't
        happen if you correctly set InitialDirection and GravityUp.
        So just pick any sensible FDirection to satisfy MinAngleRadFromGravityUp
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
      Side := VectorProduct(Direction, GravityUp);
      if ZeroVector(Side) then
      begin
        FDirection := AnyOrthogonalVector(GravityUp);
        FUp := GravityUp;
      end;
    end else
    begin
      { Calculate AngleRadBetween, and possibly adjust AngleRad. }
      AngleRadBetween := AngleRadBetweenVectors(Direction, GravityUp);
      if AngleRadBetween - AngleRad < MinAngleRadFromGravityUp then
        AngleRad := AngleRadBetween - MinAngleRadFromGravityUp else
      if AngleRadBetween - AngleRad > Pi - MinAngleRadFromGravityUp then
        AngleRad := AngleRadBetween - (Pi - MinAngleRadFromGravityUp);

      DoRealRotate;
    end;
  end else
  begin
    Side := VectorProduct(Direction, Up);
    DoRealRotate;
  end;

  ScheduleVisibleChange;
end;

procedure TWalkCamera.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);

  { Like Move, but you pass here final ProposedNewPos }
  function MoveTo(const ProposedNewPos: TVector3Single;
    const BecauseOfGravity: boolean): boolean;
  var
    NewPos: TVector3Single;
  begin
    Result := DoMoveAllowed(ProposedNewPos, NewPos, BecauseOfGravity);
    if Result then
      { Note that setting Position automatically calls ScheduleVisibleChange }
      Position := NewPos;
  end;

  { Tries to move Position to Position + MoveVector.
    Returns DoMoveAllowed result. So if it returns @false,
    you know that Position didn't change (on the other hand,
    if it returns @true, you don't know anything --- maybe Position
    didn't change, maybe it changed to Position + MoveVector,
    maybe it changed to something different). }
  function Move(const MoveVector: TVector3Single;
    const BecauseOfGravity: boolean): boolean;
  begin
    Result := MoveTo(VectorAdd(Position, MoveVector), BecauseOfGravity);
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
    Dir: TVector3Single;
  var
    AJumpMultiply: Single;
  begin
    if IsJumping then
      AJumpMultiply := JumpSpeedMultiply else
      AJumpMultiply := 1.0;

    { Update HeadBobbingPosition }
    if (not IsJumping) and UseHeadBobbing and (not HeadBobbingAlreadyDone) then
    begin
      HeadBobbingPosition += CompSpeed / HeadBobbingTime;
      HeadBobbingAlreadyDone := true;
    end;

    MoveHorizontalDone := true;

    if PreferGravityUpForMoving then
      Dir := DirectionInGravityPlane else
      Dir := Direction;

    Move(VectorScale(Dir,
      MoveSpeed * MoveHorizontalSpeed * CompSpeed * Multiply * AJumpMultiply), false);
  end;

  procedure MoveVertical(const Multiply: Integer);

    { Provided PreferredUpVector must be already normalized. }
    procedure MoveVerticalCore(const PreferredUpVector: TVector3Single);
    begin
      Move(VectorScale(PreferredUpVector,
        MoveSpeed * MoveVerticalSpeed * CompSpeed * Multiply), false);
    end;

  begin
    if PreferGravityUpForMoving then
      MoveVerticalCore(GravityUp) else
      MoveVerticalCore(Up);
  end;

  { This is just like RotateHorizontal, but it uses
    PreferGravityUpForMoving to decide which rotation to use.
    This way when PreferGravityUpForMoving, then we rotate versus GravityUp,
    move in GravityUp plane, and then rotate back versus GravityUp.
    If not PreferGravityUpForMoving, then we do all this versus Up.
    And so everything works. }
  procedure RotateHorizontalForStrafeMove(const AngleDeg: Single);
  begin
    if PreferGravityUpForMoving then
      RotateAroundGravityUp(AngleDeg) else
      RotateAroundUp(AngleDeg);
  end;

  { Check are keys for left/right/down/up rotations are pressed, and handle them.
    SpeedScale = 1 indicates a normal rotation speed, you can use it to scale
    the rotation speed to specific purposes. }
  procedure CheckRotates(SpeedScale: Single);
  begin
    {$ifndef SINGLE_STEP_ROTATION}
    if Input_RightRot.IsPressed(Container) then
      RotateHorizontal(-RotationHorizontalSpeed * CompSpeed * SpeedScale);
    if Input_LeftRot.IsPressed(Container) then
      RotateHorizontal(+RotationHorizontalSpeed * CompSpeed * SpeedScale);
    {$endif not SINGLE_STEP_ROTATION}

    if Input_UpRotate.IsPressed(Container) then
      RotateVertical(+RotationVerticalSpeed * CompSpeed * SpeedScale);
    if Input_DownRotate.IsPressed(Container) then
      RotateVertical(-RotationVerticalSpeed * CompSpeed * SpeedScale);
  end;

  { Things related to gravity --- jumping, taking into account
    falling down and keeping RealCameraPreferredHeight above the ground. }
  procedure GravityIdle;

    function TryJump: boolean;
    var
      ThisJumpHeight: Single;
    begin
      Result := IsJumping;

      if Result then
      begin
        { jump. This means:
          1. update FJumpHeight and FJumpPower and move Position
          2. or set FIsJumping to false when jump ends }

        ThisJumpHeight := MaxJumpDistance * FJumpPower * CompSpeed;
        FJumpHeight += ThisJumpHeight;

        if FJumpHeight > MaxJumpDistance then
          FIsJumping := false else
        begin
          { do jumping }
          Move(VectorScale(GravityUp, ThisJumpHeight), false);

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
      Result := AboveHeight < RealCameraPreferredHeight - RealCameraPreferredHeightMargin;

      if Result then
      begin
        { calculate GrowingVectorLength }
        GrowingVectorLength := Min(
          MoveSpeed * MoveVerticalSpeed * GrowingSpeed * CompSpeed,
          RealCameraPreferredHeight - AboveHeight);

        Move(VectorScale(GravityUp, GrowingVectorLength), true);

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
      Fde_VerticalRotateDeviation = 50.0;
      Fde_HorizontalRotateDeviation = 15.0;
    var
      PositionBefore: TVector3Single;
      FallingDownVectorLength: Single;
    begin
      Result := false;

      { Note that if we got here, then TryGrow returned false,
        which means that (assuming OnGetHeightAbove is correctly assigned)
        we are not above the ground, or
          AboveHeight >=
            RealCameraPreferredHeight - RealCameraPreferredHeightMargin
        However we require something stronger to continue:
          AboveHeight >
            RealCameraPreferredHeight + RealCameraPreferredHeightMargin

        This is important, because this way we avoid the unpleasant
        "bouncing" effect when in one Idle we decide that camera
        is falling down, in next Idle we decide that it's growing,
        in next Idle it falls down again etc. In TryGrow we try
        to precisely set our Position, so that it hits exactly
        at RealCameraPreferredHeight -- which means that after TryGrow,
        in next Idle TryGrow should not cause growing and TryFallingDown
        should not cause falling down. }
      if AboveHeight <=
           RealCameraPreferredHeight + RealCameraPreferredHeightMargin then
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
      PositionBefore := Position;

      { calculate FallingDownVectorLength.

        Note that we make sure that FallingDownVectorLength is no longer
        than AboveHeight --- this way we avoid the problem
        that when FFallingDownSpeed would get very big,
        we couldn't fall down any more (while in fact we should then fall down
        very quickly).

        Actually, we even do more. We make sure that
        FallingDownVectorLength is no longer than
        (AboveHeight - RealCameraPreferredHeight).
        Initially I wanted to do here
          MinTo1st(FallingDownVectorLength, AboveHeight);
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
      FallingDownVectorLength :=
        MoveSpeed * MoveVerticalSpeed * FFallingDownSpeed * CompSpeed;
      MinTo1st(FallingDownVectorLength, AboveHeight - RealCameraPreferredHeight);

      if Move(VectorScale(GravityUp, - FallingDownVectorLength), true) and
        (not VectorsPerfectlyEqual(Position, PositionBefore)) then
      begin
        if not IsFallingDown then
        begin
          FFallingDownStartPos := PositionBefore;

          { Why do I init here FFallingDownSpeed ? A few lines above I did
              if not FIsFallingDown then
                FFallingDownSpeed := FallingDownStartSpeed;
            to init FFallingDownSpeed (I had to do it to calculate
            FallingDownVectorLength). So why initing it again here ?

            Answer: Because Move above called MoveTo, that set Position
            that actually called ScheduleVisibleChange that possibly
            called OnVisibleChange.
            And OnVisibleChange is used callback and user could do there
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

        if AboveHeight < RealCameraPreferredHeight * 1.1 then
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
               demo_models/vrml_1/kambi_extensions/navigation_info_tests/speed_2.wrl".
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
                Fde_HorizontalRotateDeviation * CompSpeed);
            end;

            if Fde_UpRotate < 0 then
              Fde_UpRotate -= Fde_VerticalRotateDeviation * CompSpeed else
            if Fde_UpRotate > 0 then
              Fde_UpRotate += Fde_VerticalRotateDeviation * CompSpeed else
              Fde_UpRotate := RandomPlusMinus *
                              Fde_VerticalRotateDeviation * CompSpeed;

            ScheduleVisibleChange;
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
      Fde_VerticalRotateNormalization = 7 * 50;
    var
      Change: Single;
    begin
      Result := (Fde_RotateHorizontal <> 0) or (Fde_UpRotate <> 0);

      { Bring Fde_Xxx vars back to normal (zero) values. }

      Fde_RotateHorizontal := 0;

      if Fde_UpRotate <> 0.0 then
      begin
        { Note that we try to immediately bring UpRotate to
          range (-360, 360) here. E.g. no need to gradually bring back
          UpRotate from 360.0 to 0.0 --- this doesn't cause
          any interesting visual effect (and the only reason for
          UpRotate is a visual effect)... }
        Change := Trunc(Abs(Fde_UpRotate) / 360.0) * 360.0 +
          Fde_VerticalRotateNormalization * CompSpeed;

        if Fde_UpRotate < 0 then
          Fde_UpRotate := Min(Fde_UpRotate + Change, 0.0) else
          Fde_UpRotate := Max(Fde_UpRotate - Change, 0.0);

        ScheduleVisibleChange;
      end;
    end;

    function TryFallingOnTheGround: boolean;
    var
      Angle, AngleRotate: Single;
    begin
      Result := FFallingOnTheGround;
      if not Result then
        Exit;

      Angle := AngleRadBetweenVectors(Up, GravityUp);

      if FloatsEqual(Angle, HalfPi, 0.01) then
      begin
        { FallingOnTheGround effect stops here. }
        FFallingOnTheGround := false;
        Exit;
      end;

      AngleRotate := CompSpeed * 5;
      MinTo1st(AngleRotate, Abs(Angle - HalfPi));
      if not FFallingOnTheGroundAngleIncrease then
        AngleRotate := -AngleRotate;

      Up := RotatePointAroundAxisRad(AngleRotate, Up,
        DirectionInGravityPlane);
    end;

    procedure DoFalledDown;
    var
      BeginPos, EndPos, EndToBegin: TVector3Single;
      Coord: Integer;
    begin
      if Assigned(OnFalledDown) then
      begin
        { Note that I project Position and FFallingDownStartPos
          onto GravityUp vector to calculate FalledHeight. }
        BeginPos := PointOnLineClosestToPoint(ZeroVector3Single, GravityUp, FFallingDownStartPos);
        EndPos   := PointOnLineClosestToPoint(ZeroVector3Single, GravityUp, Position);
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
      HeadBobbingGoingDownSpeed = 5;
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
            HeadBobbingPosition += Min(HeadBobbingGoingDownSpeed * CompSpeed,
              1 - FracHeadBobbingPosition);
        end else
        begin
          if FracHeadBobbingPosition > SingleEqualityEpsilon then
            HeadBobbingPosition -= Min(HeadBobbingGoingDownSpeed * CompSpeed,
              FracHeadBobbingPosition);
        end;
      end;
    end;

    function GetIsOnTheGround: boolean;
    var
      MinAboveHeight, MaxAboveHeight, H: Single;
    begin
      H := RealCameraPreferredHeightNoHeadBobbing;
      MinAboveHeight := (H - H * HeadBobbing) * 0.99;
      MaxAboveHeight := (H + H * HeadBobbing) * 1.01;
      Result := IsAbove and
        (MinAboveHeight <= AboveHeight) and
        (AboveHeight <= MaxAboveHeight);
    end;

  var
    OldIsFallingDown: boolean;
  begin
    OldIsFallingDown := IsFallingDown;

    if Gravity then
    begin
      { calculate IsAbove, AboveHeight }
      UpdateHeightAbove;

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
    TargetUp: TVector3Single;
    AngleRadBetweenTargetAndGravity: Single;
    AngleRadBetweenTarget, AngleRadBetweenTargetChange: Single;
    NewUp: TVector3Single;
  begin
    if PreferGravityUp then
    begin
      { TODO: Correcting MinAngleRadFromGravityUp }

      { Correct Up such that GravityUp, Direction and Up
        are on the same plane.

        Math:
          TargetPlane := common plane of GravityUp and Direction,
          given by (A, B, C) = VectorProduct(GravityUp, Direction)
          and D = 0 (because point (0, 0, 0) is part of this plane).

          We check whether Up is on this TargetPlane too.

          If not, we find TargetUp = nearest point to Up
          lying on this TargetPlane. We want our Up be pointing
          like GravityUp, not in the other way, so if the angle between
          GravityUp and TargetUp is > 90 degress we negate
          TargetUp. If the angle is exactly 90 degress then
          TargetUp is simply equal to GravityUp.

          And then we make the angle between TargetUp and Up
          smaller. }

      TargetPlaneDir := VectorProduct(GravityUp, Direction);
      if not Zero(
         (TargetPlaneDir[0] * FUp[0]) +
         (TargetPlaneDir[1] * FUp[1]) +
         (TargetPlaneDir[2] * FUp[2])) then
      begin
        TargetPlane[3] := 0;

        Writeln('corrrecting');

        { calculate TargetUp }
        TargetUp := PointOnPlaneClosestToPoint(TargetPlane, FUp);
        AngleRadBetweenTargetAndGravity :=
          AngleRadBetweenVectors(TargetUp, GravityUp);
        if FloatsEqual(AngleRadBetweenTargetAndGravity, HalfPi) then
          TargetUp := GravityUp else
        if AngleRadBetweenTargetAndGravity > HalfPi then
          VectorNegateTo1st(TargetUp);

        AngleRadBetweenTarget := AngleRadBetweenVectors(TargetUp, FUp);
        AngleRadBetweenTargetChange := 0.5 * CompSpeed;
        if AngleRadBetweenTarget > AngleRadBetweenTargetChange then
        begin
          NewUp := FUp;
          MakeVectorsAngleRadOnTheirPlane(NewUp, TargetUp,
            AngleRadBetweenTarget - AngleRadBetweenTargetChange);
          Up := NewUp;
        end else
          Up := TargetUp;
      end;
    end;
    *)
  begin
  end;

  procedure ChangeCameraPreferredHeight(const Increase: Integer);
  begin
    CameraPreferredHeight := CameraPreferredHeight +
      { It's best to scale CameraPreferredHeight changes by MoveSpeed,
        to make it faster/slower depending on scene size
        (which usually corresponds to move speed). }
      Increase * MoveSpeed * CompSpeed * 0.2;

    CorrectCameraPreferredHeight;

    { Why ScheduleVisibleChange here? Reasoning the same as for
      MoveSpeedInc/Dec changes. }
    ScheduleVisibleChange;
  end;

  procedure PositionMouseLook;
  begin
    { Why reposition mouse for MouseLook here?

      1. Older approach was to reposition only at UpdateMouseLook,
         which was automatically called by camera's SetMouseLook.
         But this turned out to reposition mouse too often:

         MouseLook may be true for a very short time.

         For example, consider castle, where MouseLook is usually true
         during the game, but it's off in game menu (TGLMenu) and start screen.
         So when you're in the game, and choose "End game", game menu
         closes (immediately bringing back MouseLook = true by TGLMode.Destroy
         restoring everything), but game mode immediately closes and goes
         back to start screen. Effect: mouse cursor is forced to the middle
         of the screen, without any apparent (for user) reason.

      2. Later approach: just not reposition mouse at all just
         because MoseLook = true.  Only reposition from
         TWalkCamera.MouseMove.

         This requires the MouseMove handler to only work when initial
         mouse position is at the screen middle,
         otherwise initial mouse look would generate large move.
         But in fact TWalkCamera.MouseMove already does this, so it's all Ok.

         Unfortunately, this isn't so nice: sometimes you really want your
         mouse repositioned even before you move it:
         - e.g. when entering castle game, it's strange that mouse cursor
           is temporarily visible, until you move the mouse.
         - worse: when mouse cursor is outside castle window, you have
           to move mouse first over the window, before mouse look catches up.

      So we have to reposition the mouse, but not too eagerly.
      Idle seems a good moment. }
    if MouseLook and
       ContainerSizeKnown and
       (Container <> nil) and
       { Paranoidally check is position different, to avoid calling
         SetMousePosition in every Idle. SetMousePosition should be optimized
         for this case (when position is already set), but let's check anyway. }
       (Container.MouseX <> ContainerWidth div 2) and
       (Container.MouseY <> ContainerHeight div 2) then
      Container.SetMousePosition(ContainerWidth div 2, ContainerHeight div 2);
  end;

var
  ModsDown: TModifierKeys;
begin
  inherited;

  PositionMouseLook;

  { Do not handle keys or gravity etc. }
  if IsAnimation or (not Exists) then Exit;

  ModsDown := ModifiersDown(Container.Pressed);

  HeadBobbingAlreadyDone := false;
  MoveHorizontalDone := false;

  BeginVisibleChangeSchedule;
  try
    if HandleMouseAndKeys and not IgnoreAllInputs then
    begin
      FIsCrouching := Input_Crouch.IsPressed(Container);

      if (not CheckModsDown) or
         (ModsDown - [mkShift] = []) then
      begin
        CheckRotates(1.0);

        if Input_Forward.IsPressed(Container) then
          MoveHorizontal;
        if Input_Backward.IsPressed(Container) then
          MoveHorizontal(-1);

        if Input_RightStrafe.IsPressed(Container) then
        begin
          RotateHorizontalForStrafeMove(-90);
          MoveHorizontal;
          RotateHorizontalForStrafeMove(90);
        end;

        if Input_LeftStrafe.IsPressed(Container) then
        begin
          RotateHorizontalForStrafeMove(90);
          MoveHorizontal;
          RotateHorizontalForStrafeMove(-90);
        end;

        { A simple implementation of Input_UpMove was
            RotateVertical(90); Move(MoveVerticalSpeed * MoveSpeed * CompSpeed); RotateVertical(-90)
          Similarly, simple implementation of Input_DownMove was
            RotateVertical(-90); Move(MoveVerticalSpeed * MoveSpeed * CompSpeed); RotateVertical(90)
          But this is not good, because when PreferGravityUp, we want to move
          along the GravityUp. (Also later note: RotateVertical is now bounded by
          MinAngleRadFromGravityUp). }

        if Input_UpMove.IsPressed(Container) then
          MoveVertical( 1);
        if Input_DownMove.IsPressed(Container) then
          MoveVertical(-1);

        { zmiana szybkosci nie wplywa na Matrix (nie od razu). Ale wywolujemy
          ScheduleVisibleChange - zmienilismy swoje wlasciwosci, moze sa one np. gdzies
          wypisywane w oknie na statusie i okno potrzebuje miec PostRedisplay po zmianie
          Move*Speed ?.

          How to apply CompSpeed here ?
          I can't just ignore CompSpeed, but I can't also write
            FMoveSpeed *= 10 * CompSpeed;
          What I want is such continous function that e.g.
            F(FMoveSpeed, 10) = F(F(FMoveSpeed, 1), 1)
          I.e. CompSpeed = 10 should work just like doing the same change twice.
          So F is FMoveSpeed * Power(10, CompSpeed)
          Easy!
        }
        if Input_MoveSpeedInc.IsPressed(Container) then
        begin
          MoveSpeed := MoveSpeed * Power(10, CompSpeed);
          ScheduleVisibleChange;
        end;

        if Input_MoveSpeedDec.IsPressed(Container) then
        begin
          MoveSpeed := MoveSpeed / Power(10, CompSpeed);
          ScheduleVisibleChange;
        end;
      end else
      if ModsDown = [mkCtrl] then
      begin
        if AllowSlowerRotations then
          CheckRotates(0.1);

        { Either MoveSpeedInc/Dec work, or Increase/DecreaseCameraPreferredHeight,
          as they by default have the same shortcuts, so should not work
          together. }
        if ModsDown = [mkCtrl] then
        begin
          if Input_IncreaseCameraPreferredHeight.IsPressed(Container) then
            ChangeCameraPreferredHeight(+1);
          if Input_DecreaseCameraPreferredHeight.IsPressed(Container) then
            ChangeCameraPreferredHeight(-1);
        end;
      end;
    end;

    PreferGravityUpForRotationsIdle;

    { These may be set to @true only inside GravityIdle }
    FIsWalkingOnTheGround := false;
    FIsOnTheGround := false;

    GravityIdle;
  finally
    EndVisibleChangeSchedule;
  end;
end;

function TWalkCamera.Jump: boolean;
begin
  Result := false;

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

  { calculate IsAbove, AboveHeight }
  UpdateHeightAbove;

  if AboveHeight > RealCameraPreferredHeight + RealCameraPreferredHeightMargin then
    Exit;

  FIsJumping := true;
  FJumpHeight := 0.0;
  Result := true;
end;

function TWalkCamera.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

function TWalkCamera.EventDown(AKey: TKey; ACharacter: Char;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection): boolean;
begin
  if IgnoreAllInputs or IsAnimation then Exit(false);

  {$ifdef SINGLE_STEP_ROTATION}
  if Input_RightRot.IsEvent(AKey, ACharacter, AMousePress, AMouseButton, AMouseWheel) then
    RotateHorizontal(-5) else
  if Input_LeftRot.IsEvent(AKey, ACharacter, AMousePress, AMouseButton, AMouseWheel) then
    RotateHorizontal(+5) else
  {$endif SINGLE_STEP_ROTATION}

  if Input_GravityUp.IsEvent(AKey, ACharacter, AMousePress, AMouseButton, AMouseWheel) then
  begin
    if VectorsParallel(Direction, GravityUp) then
    begin
      { We can't carelessly set Up to something parallel to GravityUp
        in this case.

        Yes, this situation can happen: for example open a model with
        no viewpoint in VRML in view3dscene (so default viewpoint,
        both gravity and Up = +Y is used). Then change GravityUp
        by menu and press Home (Input_GravityUp). }

      FUp := GravityUp;
      FDirection := AnyOrthogonalVector(FUp);
      ScheduleVisibleChange;
    end else
      Up := GravityUp;
    Result := ExclusiveEvents;
  end else
  if Input_Jump.IsEvent(AKey, ACharacter, AMousePress, AMouseButton, AMouseWheel) then
  begin
    Result := Jump and ExclusiveEvents;
  end else
    Result := false;
end;

function TWalkCamera.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  if (not CheckModsDown) or
     (ModifiersDown(Container.Pressed) - [mkShift] = []) then
  begin
    Result := EventDown(Key, C, false, mbLeft, mwNone);
  end;
end;

function TWalkCamera.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := EventDown(K_None, #0, true, Button, mwNone);
end;

function TWalkCamera.MouseWheel(const Scroll: Single; const Vertical: boolean): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := EventDown(K_None, #0, false, mbLeft,
    MouseWheelDirection(Scroll, Vertical));
end;

procedure TWalkCamera.Init(
  const AInitialPosition, AInitialDirection, AInitialUp: TVector3Single;
  const AGravityUp: TVector3Single;
  const ACameraPreferredHeight: Single;
  const ACameraRadius: Single);
begin
  SetInitialView(AInitialPosition, AInitialDirection, AInitialUp, false);
  FGravityUp := Normalized(AGravityUp);
  CameraPreferredHeight := ACameraPreferredHeight;
  CameraRadius := ACameraRadius;
  CorrectCameraPreferredHeight;
  GoToInitial;
end;

procedure TWalkCamera.Init(const Box: TBox3D; const ACameraRadius: Single);
var Pos: TVector3Single;
    AvgSize: Single;
begin
 if IsEmptyOrZeroBox3D(Box) then
  Init(Vector3Single(0, 0, 0),
       DefaultDirection,
       DefaultUp,
       Vector3Single(0, 1, 0) { GravityUp is the same as InitialUp },
       0 { whatever }, ACameraRadius) else
 begin
  AvgSize := Box3DAvgSize(Box);
  Pos[0] := Box[0, 0]-AvgSize;
  Pos[1] := (Box[0, 1]+Box[1, 1])/2;
  Pos[2] := (Box[0, 2]+Box[1, 2])/2;
  Init(Pos, UnitVector3Single[0],
    UnitVector3Single[2],
    UnitVector3Single[2] { GravityUp is the same as InitialUp },
    AvgSize * 5, ACameraRadius);
 end;
end;

procedure TWalkCamera.Home;
begin
  GoToInitial;
end;

procedure TWalkCamera.SetPosition(const Value: TVector3Single);
begin
  FPosition := Value;
  ScheduleVisibleChange;
end;

procedure TWalkCamera.SetDirection(const Value: TVector3Single);
begin
  FDirection := Normalized(Value);
  MakeVectorsOrthoOnTheirPlane(FUp, FDirection);
  ScheduleVisibleChange;
end;

procedure TWalkCamera.SetUp(const Value: TVector3Single);
begin
  FUp := Normalized(Value);
  MakeVectorsOrthoOnTheirPlane(FDirection, FUp);
  ScheduleVisibleChange;
end;

procedure TWalkCamera.CorrectCameraPreferredHeight;
begin
  Cameras.CorrectCameraPreferredHeight(
    FCameraPreferredHeight, CameraRadius, CrouchHeight, HeadBobbing);
end;

function TWalkCamera.MaxJumpDistance: Single;
begin
  Result := MaxJumpHeight * CameraPreferredHeight;
end;

function TWalkCamera.DirectionInGravityPlane: TVector3Single;
begin
  Result := Direction;

  if not VectorsParallel(Result, GravityUp) then
    MakeVectorsOrthoOnTheirPlane(Result, GravityUp);
end;

procedure TWalkCamera.FallOnTheGround;
begin
  FFallingOnTheGround := true;

  { Mathematically reasoning, this should be smarter.
    I mean that we should randomize FFallingOnTheGroundAngleIncrease
    *only* if Up is parallel to GravityUp ?
    Otherwise Up could change through some strange path ?

    But current effect seems to behave good in all situations...
    In any case, Up going through some strange path will only
    be noticeable for a very short time, so I don't think that's a real
    problem... unless I see some example when it looks bad. }

  FFallingOnTheGroundAngleIncrease := Random(2) = 0;
end;

procedure TWalkCamera.CancelFallingDown;
begin
  { Fortunately implementation of this is brutally simple right now. }
  FIsFallingDown := false;
end;

procedure TWalkCamera.SetMouseLook(const Value: boolean);
begin
  if FMouseLook <> Value then
  begin
    FMouseLook := Value;
    if FMouseLook then
      Cursor := mcNone else
      Cursor := mcDefault;
  end;
end;

function TWalkCamera.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
var
  MouseXChange, MouseYChange: Single;
  MiddleWidth: Integer;
  MiddleHeight: Integer;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  if MouseLook and (not IgnoreAllInputs) and ContainerSizeKnown and
    (not IsAnimation) then
  begin
    MiddleWidth := ContainerWidth div 2;
    MiddleHeight := ContainerHeight div 2;

    { Note that SetMousePosition may (but doesn't have to)
      generate another MouseMove in the container to destination position.
      This can cause some problems:

      1. Consider this:

         - player moves mouse to MiddleX-10
         - MouseMove is generated, I rotate camera by "-10" horizontally
         - SetMousePosition sets mouse to the Middle,
           but this time no MouseMove is generated
         - player moved mouse to MiddleX+10. Although mouse was
           positioned on Middle, TGLWindow thinks that the mouse
           is still positioned on Middle-10, and I will get "+20" move
           for player (while I should get only "+10")

         Fine solution for this would be to always subtract
         MiddleWidth and MiddleHeight below
         (instead of previous values, OldX and OldY).
         But this causes another problem:

      2. What if player switches to another window, moves the mouse,
         than goes alt+tab back to our window ? Next mouse move will
         be stupid, because it's really *not* from the middle of the screen.

      The solution for both problems: you have to check that previous
      position, OldX and OldY, are indeed equal to
      MiddleWidth and MiddleHeight. This way we know that
      this is good move, that qualifies to perform mouse move.

      And inside, we can calculate the difference
      by subtracing new - old position, knowing that old = middle this
      will always be Ok.

      Later: see TGLWindow.UpdateMouseLook implementation notes,
      we actually depend on the fact that MouseLook checks and works
      only if mouse position is at the middle. }
    if (OldX = MiddleWidth) and
       (OldY = MiddleHeight) then
    begin
      { MouseXChange and MouseYChange are differences between current
        and previous window coords
        (like in TGLWindow.MouseX/MouseY, so 0,0 is top-left corner). }
      MouseXChange := NewX - OldX;
      MouseYChange := NewY - OldY;

      if MouseXChange <> 0 then
        RotateHorizontal(-MouseXChange * MouseLookHorizontalSensitivity);
      if MouseYChange <> 0 then
      begin
        if InvertVerticalMouseLook then
          MouseYChange := -MouseYChange;
        RotateVertical(-MouseYChange * MouseLookVerticalSensitivity);
      end;

      Result := ExclusiveEvents;
    end;

    { I check the condition below to avoid calling SetMousePosition,
      getting MouseMove event, SetMousePosition, getting MouseMove event... in a loop.
      Not really likely (as messages will be queued, and some
      SetMousePosition will finally just not generate event MouseMove),
      but I want to safeguard anyway. }
    if (NewX <> MiddleWidth) or (NewY <> MiddleHeight) then
      Container.SetMousePosition(MiddleWidth, MiddleHeight);
  end;
end;

procedure TWalkCamera.GetView(
  out APos, ADir, AUp: TVector3Single);
begin
  APos := FPosition;
  ADir := FDirection;
  AUp  := FUp;
end;

procedure TWalkCamera.GetView(out APos, ADir, AUp, AGravityUp: TVector3Single);
begin
  GetView(APos, ADir, AUp);
  AGravityUp := GravityUp;
end;

function TWalkCamera.GetPosition: TVector3Single;
begin
  Result := FPosition;
end;

procedure TWalkCamera.SetView(const APos, ADir, AUp: TVector3Single);
begin
  FPosition := APos;
  FDirection := Normalized(ADir);
  FUp := Normalized(AUp);
  MakeVectorsOrthoOnTheirPlane(FUp, FDirection);

  ScheduleVisibleChange;
end;

procedure TWalkCamera.SetView(const APos, ADir, AUp, AGravityUp: TVector3Single);
begin
  GravityUp := AGravityUp;
  SetView(APos, ADir, AUp);
end;

procedure TWalkCamera.SetGravityUp(const Value: TVector3Single);
begin
  FGravityUp := Normalized(Value);
end;

{ TExamineCameraInUniversal -------------------------------------------------- }

type
  TExamineCameraInUniversal = class(TExamineCamera)
  private
    { Owning TUniversalCamera }
    Universal: TUniversalCamera;
  public
    procedure VisibleChange; override;
  protected
    function IsAnimation: boolean; override;
    procedure DoCursorChange; override;
  end;

function TExamineCameraInUniversal.IsAnimation: boolean;
begin
  Result := (inherited IsAnimation) or Universal.IsAnimation;
end;

procedure TExamineCameraInUniversal.VisibleChange;
begin
  inherited;
  { Call parent VisibleChange when children change. }
  Universal.VisibleChange;
end;

procedure TExamineCameraInUniversal.DoCursorChange;
begin
  { update Universal.Cursor, in case we're the current camera }
  Universal.Cursor := Universal.Current.Cursor;
end;

{ TWalkCameraInUniversal -------------------------------------------------- }

type
  TWalkCameraInUniversal = class(TWalkCamera)
  private
    { Owning TUniversalCamera }
    Universal: TUniversalCamera;
  protected
    function IsAnimation: boolean; override;
    procedure DoCursorChange; override;
  public
    procedure VisibleChange; override;
  end;

function TWalkCameraInUniversal.IsAnimation: boolean;
begin
  Result := (inherited IsAnimation) or Universal.IsAnimation;
end;

procedure TWalkCameraInUniversal.VisibleChange;
begin
  inherited;
  { Call parent VisibleChange when children change. }
  Universal.VisibleChange;
end;

procedure TWalkCameraInUniversal.DoCursorChange;
begin
  { update Universal.Cursor, in case we're the current camera }
  Universal.Cursor := Universal.Current.Cursor;
end;

{ TUniversalCamera ----------------------------------------------------------- }

constructor TUniversalCamera.Create(AOwner: TComponent);
begin
  inherited;
  FExamine := TExamineCameraInUniversal.Create(nil);
  TExamineCameraInUniversal(FExamine).Universal := Self;
  { Useful and works sensibly with our view3dscene events that pass
    mouse / keys to VRML/X3D scene. This way in Examine mode you can
    activate pointing device sensors.
    Note: This is the default now. }
  FExamine.ExclusiveEvents := false;

  FWalk := TWalkCameraInUniversal.Create(nil);
  TWalkCameraInUniversal(FWalk).Universal := Self;
end;

destructor TUniversalCamera.Destroy;
begin
  FreeAndNil(FExamine);
  FreeAndNil(FWalk);
  inherited;
end;

function TUniversalCamera.Current: TCamera;
begin
  if FNavigationClass = ncExamine then
    Result := FExamine else
    Result := FWalk;
end;

function TUniversalCamera.Matrix: TMatrix4Single;
begin
  Result := Current.Matrix;
end;

function TUniversalCamera.RotationMatrix: TMatrix4Single;
begin
  Result := Current.RotationMatrix;
end;

procedure TUniversalCamera.GetView(out APos, ADir, AUp: TVector3Single);
begin
  Current.GetView(APos, ADir, AUp);
end;

procedure TUniversalCamera.GetView(out APos, ADir, AUp, AGravityUp: TVector3Single);
begin
  Current.GetView(APos, ADir, AUp, AGravityUp);
end;

function TUniversalCamera.GetPosition: TVector3Single;
begin
  Result := Current.GetPosition;
end;

procedure TUniversalCamera.SetView(const APos, ADir, AUp: TVector3Single);
begin
  FExamine.SetView(APos, ADir, AUp);
  FWalk.SetView(APos, ADir, AUp);
end;

procedure TUniversalCamera.SetView(const APos, ADir, AUp, AGravityUp: TVector3Single);
begin
  FExamine.SetView(APos, ADir, AUp, AGravityUp);
  FWalk.SetView(APos, ADir, AUp, AGravityUp);
end;

procedure TUniversalCamera.SetCameraRadius(const Value: Single);
begin
  inherited;
  FExamine.CameraRadius := Value;
  FWalk.CameraRadius := Value;
end;

procedure TUniversalCamera.SetIgnoreAllInputs(const Value: boolean);
begin
  inherited;
  FExamine.IgnoreAllInputs := Value;
  FWalk.IgnoreAllInputs := Value;
end;

procedure TUniversalCamera.SetProjectionMatrix(const Value: TMatrix4Single);
begin
  { This calls RecalculateFrustum on all 3 cameras, while only once
    is needed... But speed should not be a problem here, this is seldom used. }
  inherited;
  FExamine.ProjectionMatrix := Value;
  FWalk.ProjectionMatrix := Value;
end;

function TUniversalCamera.PositionInside(const X, Y: Integer): boolean;
begin
  Result := Current.PositionInside(X, Y);
end;

procedure TUniversalCamera.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;

  LetOthersHandleMouseAndKeys := not Current.ExclusiveEvents;
  Current.Idle(CompSpeed, HandleMouseAndKeys, LetOthersHandleMouseAndKeys);
end;

function TUniversalCamera.AllowSuspendForInput: boolean;
begin
  Result := Current.AllowSuspendForInput;
end;

function TUniversalCamera.KeyDown(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := Current.KeyDown(Key, C);
end;

function TUniversalCamera.KeyUp(Key: TKey; C: char): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := Current.KeyUp(Key, C);
end;

function TUniversalCamera.MouseDown(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := Current.MouseDown(Button);
end;

function TUniversalCamera.MouseUp(const Button: TMouseButton): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := Current.MouseUp(Button);
end;

function TUniversalCamera.MouseMove(const OldX, OldY, NewX, NewY: Integer): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := Current.MouseMove(OldX, OldY, NewX, NewY);
end;

function TUniversalCamera.MouseWheel(const Scroll: Single; const Vertical: boolean): boolean;
begin
  Result := inherited;
  if Result or (not Exists) then Exit;

  Result := Current.MouseWheel(Scroll, Vertical);
end;

procedure TUniversalCamera.SetContainer(const Value: IUIContainer);
begin
  inherited;
  FWalk.Container := Value;
  FExamine.Container := Value;
end;

procedure TUniversalCamera.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
  inherited;
  FWalk.ContainerResize(AContainerWidth, AContainerHeight);
  FExamine.ContainerResize(AContainerWidth, AContainerHeight);
end;

procedure TUniversalCamera.SetInitialView(
  const AInitialPosition: TVector3Single;
  AInitialDirection, AInitialUp: TVector3Single;
  const TransformCurrentCamera: boolean);
begin
  { Pass TransformCurrentCamera = false to inherited.
    This way inherited updates our Initial* properties, but does not
    call Get/SetView (these would set our children cameras,
    which isn't needed as we do it manually below). }
  inherited SetInitialView(
    AInitialPosition, AInitialDirection, AInitialUp, false);

  FExamine.SetInitialView(
    AInitialPosition, AInitialDirection, AInitialUp, TransformCurrentCamera);
  FWalk.SetInitialView(
    AInitialPosition, AInitialDirection, AInitialUp, TransformCurrentCamera);
end;

procedure TUniversalCamera.SetNavigationClass(const Value: TCameraNavigationClass);
var
  Position, Direction, Up: TVector3Single;
begin
  if FNavigationClass <> Value then
  begin
    Current.GetView(Position, Direction, Up);
    FNavigationClass := Value;
    Current.SetView(Position, Direction, Up);
    { our Cursor should always reflect Current.Cursor }
    Cursor := Current.Cursor;
  end;
end;

function TUniversalCamera.GetNavigationType: TCameraNavigationType;
begin
  if IgnoreAllInputs then
    Result := ntNone else
  if NavigationClass = ncExamine then
    Result := ntExamine else
  if Walk.Gravity then
    Result := ntWalk else
    Result := ntFly;
end;

procedure TUniversalCamera.SetNavigationType(const Value: TCameraNavigationType);
begin
  { This is not a pure optimization in this case.
    If you set some weird values, then (without this check)
    doing "NavigationType := NavigationType" would not be NOOP. }
  if Value = GetNavigationType then Exit;

  { set default values (for Walk camera and IgnoreAllInputs),
    may be changed later by this method. This way every setting
    of SetNavigationType sets them, regardless of value, which seems
    consistent. }
  Walk.Gravity := false;
  Walk.PreferGravityUpForRotations := true;
  Walk.PreferGravityUpForMoving := true;
  IgnoreAllInputs := false;

  { This follows the same logic as TVRMLScene.CameraFromNavigationInfo }

  { set NavigationClass, and eventually adjust Walk properties }
  case Value of
    ntExamine: NavigationClass := ncExamine;
    ntWalk:
      begin
        NavigationClass := ncWalk;
        Walk.Gravity := true;
      end;
    ntFly:
      begin
        NavigationClass := ncWalk;
        Walk.PreferGravityUpForMoving := false;
      end;
    ntNone:
      begin
        NavigationClass := ncWalk;
        IgnoreAllInputs := true;
      end;
    else raise EInternalError.Create('TUniversalCamera.SetNavigationType: Value?');
  end;
end;

function TUniversalCamera.PreventsComfortableDragging: boolean;
begin
  Result := Current.PreventsComfortableDragging;
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

function CamDirUp2OrientQuat(CamDir, CamUp: TVector3Single): TQuaternion;

{ This was initially based on Stephen Chenney's ANSI C code orient.c,
  available still from here: http://vrmlworks.crispen.org/tools.html
  I rewrote it a couple of times, possibly removing and possibly adding
  some bugs :)

  Idea: we want to convert CamDir and CamUp into VRML orientation,
  which is a rotation from DefaultDirection/DefaultUp into CamDir/Up.

  1) Take vector orthogonal to standard DefaultDirection and CamDir.
     Rotate around it, to match DefaultDirection with CamDir.

  2) Now rotate around CamDir such that standard up (already rotated
     by 1st transform) matches with CamUp. We know it's possible,
     since CamDir and CamUp are orthogonal and normalized,
     just like standard DefaultDirection/DefaultUp.

  Combine these two rotations and you have the result.

  How to combine two rotations, such that in the end you get nice
  single rotation? That's where quaternions rule.
}

  function QuatFromAxisAngleCos(const Axis: TVector3Single;
    const AngleRadCos: Single): TQuaternion;
  begin
    Result := QuatFromAxisAngle(Axis, ArcCos(Clamped(AngleRadCos, -1.0, 1.0)));
  end;

var Rot1Axis, Rot2Axis, StdCamUpAfterRot1: TVector3Single;
    Rot1Quat, Rot2Quat: TQuaternion;
    Rot1CosAngle, Rot2CosAngle: Single;
begin
 NormalizeTo1st(CamDir);
 NormalizeTo1st(CamUp);

 { calculate Rot1Quat }
 Rot1Axis := Normalized( VectorProduct(DefaultCameraDirection, CamDir) );
 { Rot1Axis may be zero if DefaultCameraDirection and CamDir are parallel.
   When they point in the same direction, then it doesn't matter
   (rotation will be by 0 angle anyway), but when they are in opposite
   direction we want to do some rotation, so we need some non-zero
   sensible Rot1Axis. }
 if ZeroVector(Rot1Axis) then
   Rot1Axis := DefaultCameraUp;
 Rot1CosAngle := VectorDotProduct(DefaultCameraDirection, CamDir);
 Rot1Quat := QuatFromAxisAngleCos(Rot1Axis, Rot1CosAngle);

 { calculate Rot2Quat }
 StdCamUpAfterRot1 := QuatRotate(Rot1Quat, DefaultCameraUp);
 { wiemy ze Rot2Axis to CamDir lub -CamDir. Wyznaczamy je jednak w tak
   prosty sposob bo nie przychodzi mi teraz do glowy inny sposob jak rozpoznac
   czy powinnismy tu wziac CamDir czy -CamDir (chodzi o to zeby pozniej obrot
   o Rot2CosAngle byl w dobra strone) }
 Rot2Axis := Normalized( VectorProduct(StdCamUpAfterRot1, CamUp) );
 if ZeroVector(Rot2Axis) then
   Rot2Axis := CamDir;
 Rot2CosAngle := VectorDotProduct(StdCamUpAfterRot1, CamUp);
 Rot2Quat := QuatFromAxisAngleCos(Rot2Axis, Rot2CosAngle);

 { calculate Result = combine Rot1 and Rot2 (yes, the order
   for QuatMultiply is reversed) }
 Result := QuatMultiply(Rot2Quat, Rot1Quat);
end;

procedure CamDirUp2Orient(const CamDir, CamUp: TVector3Single;
  out OrientAxis: TVector3Single; out OrientRadAngle: Single);
begin
  { Call CamDirUp2OrientQuat,
    and extract the axis and angle from the quaternion. }
  QuatToAxisAngle(CamDirUp2OrientQuat(CamDir, CamUp), OrientAxis, OrientRadAngle);
end;

function CamDirUp2Orient(const CamDir, CamUp: TVector3Single): TVector4Single;
var OrientAxis: TVector3Single;
    OrientAngle: Single;
begin
 CamDirUp2Orient(CamDir, CamUp, OrientAxis, OrientAngle);
 result := Vector4Single(OrientAxis, OrientAngle);
end;

procedure CameraViewpointForWholeScene(const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean;
  out Position, Direction, Up, GravityUp: TVector3Single);
var
  Offset: Single;
begin
  Direction := UnitVector3Single[WantedDirection];
  if not WantedDirectionPositive then VectorNegateTo1st(Direction);

  Up := UnitVector3Single[WantedUp];
  if not WantedUpPositive then VectorNegateTo1st(Up);

  if IsEmptyBox3D(Box) then
  begin
    Position  := ZeroVector3Single;
  end else
  begin
    Position := Box3DMiddle(Box);
    Offset := 1.5 * Box3DAvgSize(Box);

    if WantedDirectionPositive then
      Position[WantedDirection] := Box[0, WantedDirection] - Offset else
      Position[WantedDirection] := Box[1, WantedDirection] + Offset;
  end;

  { GravityUp is just always equal Up here. }
  GravityUp := Up;
end;

end.
