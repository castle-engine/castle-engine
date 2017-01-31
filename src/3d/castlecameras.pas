{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Cameras to navigate in 3D space (TExamineCamera, TWalkCamera, TUniversalCamera). }
unit CastleCameras;

{$I castleconf.inc}

interface

uses SysUtils, CastleVectors, CastleUtils, CastleKeysMouse, CastleBoxes, CastleQuaternions,
  CastleFrustum, CastleUIControls, Classes, CastleRays, CastleTimeUtils, CastleInputs,
  CastleTriangles, CastleRectangles;

type
  { Possible navigation input types in cameras, set in TCamera.Input. }
  TCameraInput = (
    { Normal input types. This includes all inputs available as
      Input_Xxx properties in TCamera descendants.
      They are all fully configurable (as TInputShortcut class),
      they may be mouse button presses, mouse wheel clicks, or key presses.
      You can always clear some shortcut (like @code(WalkCamera.Input_Forward.MakeClear))
      to disable a specific shortcut.
      Excluding ciNormal from TCamera.Input is an easy way to disable @italic(all)
      shortcuts. }
    ciNormal,

    { Mouse and touch dragging. Both TExamineCamera and TWalkCamera implement their own,
      special reactions to mouse dragging, that allows to navigate / rotate
      while pressing specific mouse buttons.

      Note that mouse dragging is automatically disabled when
      @link(TWalkCamera.MouseLook) is used. }
    ciMouseDragging,

    { Navigation using 3D mouse devices, like the ones from 3dconnexion. }
    ci3dMouse);
  TCameraInputs = set of TCameraInput;

  TNavigationClass = (ncExamine, ncWalk);
  TNavigationType = (ntExamine, ntTurntable, ntWalk, ntFly, ntNone);

  { Base class camera.

    TODO: this is separate from TInputListener class only to avoid FPC 2.6.4
    bug Internal error 200610054 when using the stabs debug info. }
  TCameraInputListener = class(TComponent)
  private
    FOnVisibleChange: TNotifyEvent;
    FContainer: TUIContainer;
    FCursor: TMouseCursor;
    FOnCursorChange: TNotifyEvent;
    FExclusiveEvents: boolean;
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
      In TUIControl class, just calls OnCursorChange. }
    procedure DoCursorChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    (*Handle press or release of a key, mouse button or mouse wheel.
      Return @true if the event was somehow handled.

      In this class this always returns @false, when implementing
      in descendants you should override it like

      @longCode(#
        Result := inherited;
        if Result then Exit;
        { ... And do the job here.
          In other words, the handling of events in inherited
          class should have a priority. }
      #)

      Note that releasing of mouse wheel is not implemented for now,
      neither by CastleWindow or Lazarus CastleControl.
      @groupBegin *)
    function Press(const Event: TInputPressRelease): boolean; virtual;
    function Release(const Event: TInputPressRelease): boolean; virtual;
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

      @param(SecondsPassed Should be calculated like TFramesPerSecond.UpdateSecondsPassed,
        and usually it's in fact just taken from TCastleWindowCustom.Fps.UpdateSecondsPassed.)

      This method may be used, among many other things, to continously
      react to the fact that user pressed some key (or mouse button).
      For example, if holding some key should move some 3D object,
      you should do something like:

      @longCode(#
      if HandleInput then
      begin
        if Container.Pressed[K_Right] then
          Transform.Position += Vector3Single(SecondsPassed * 10, 0, 0);
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
      @link(TUIControl.CapturesEventsAtPosition)). And as soon as some control says it "handled"
      the event, other controls (even if under the mouse) will not
      receive the event.

      This approach is not suitable for Update events. Some controls
      need to do the Update job all the time,
      regardless of whether the control is under the mouse and regardless
      of what other controls already did. So all controls (well,
      all controls that exist, in case of TUIControl,
      see TUIControl.GetExists) receive Update calls.

      So the "handled" status is passed through HandleInput.
      If a control is not under the mouse, it will receive HandleInput
      = @false. If a control is under the mouse, it will receive HandleInput
      = @true as long as no other control on top of it didn't already
      change it to @false. }
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); virtual;

    { Called always when some visible part of this control
      changes. In the simplest case, this is used by the controls manager to
      know when we need to redraw the control.

      In this class this simply calls OnVisibleChange (if assigned). }
    procedure VisibleChange; virtual;

    { Called always when some visible part of this control
      changes. In the simplest case, this is used by the controls manager to
      know when we need to redraw the control.

      Be careful when handling this event, various changes may cause this,
      so be prepared to handle OnVisibleChange at every time.

      @seealso VisibleChange }
    property OnVisibleChange: TNotifyEvent
      read FOnVisibleChange write FOnVisibleChange;

    { Allow window containing this control to suspend waiting for user input.
      Typically you want to override this to return @false when you do
      something in the overridden @link(Update) method.

      In this class, this simply returns always @true.

      @seeAlso TCastleWindowCustom.AllowSuspendForInput }
    function AllowSuspendForInput: boolean; virtual;

    { You can resize/reposition your component here,
      for example set @link(TUIControl.Left) or @link(TUIControl.Bottom), to react to parent
      size changes.
      Called always when the container (component or window with OpenGL context)
      size changes. Called only when the OpenGL context of the container
      is initialized, so you can be sure that this is called only between
      GLContextOpen and GLContextClose.

      We also make sure to call this once when inserting into
      the controls list
      (like @link(TCastleWindowCustom.Controls) or
      @link(TCastleControlCustom.Controls) or inside parent TUIControl),
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
      list (like TCastleWindowCustom.Controls) container will automatically
      set itself here, and when removing from container this will be changed
      back to @nil.

      May be @nil if this control is not yet inserted into any container. }
    property Container: TUIContainer read FContainer write SetContainer;

    { Mouse cursor over this control.
      When user moves mouse over the Container, the currently focused
      (topmost under the cursor) control determines the mouse cursor look. }
    property Cursor: TMouseCursor read FCursor write SetCursor default mcDefault;

    { Event called when the @link(Cursor) property changes.
      This event is, in normal circumstances, used by the Container,
      so you should not use it in your own programs. }
    property OnCursorChange: TNotifyEvent
      read FOnCursorChange write FOnCursorChange;

    { Design note: ExclusiveEvents is not published now, as it's too "obscure"
      (for normal usage you don't want to deal with it). Also, it's confusing
      on TCastleSceneCore, the name suggests it relates to ProcessEvents (VRML events,
      totally not related to this property that is concerned with handling
      TUIControl events.) }

    { Should we disable further mouse / keys handling for events that
      we already handled in this control. If @true, then our events will
      return @true for mouse and key events handled.

      This means that events will not be simultaneously handled by both this
      control and some other (or camera or normal window callbacks),
      which is usually more sensible, but sometimes somewhat limiting. }
    property ExclusiveEvents: boolean
      read FExclusiveEvents write FExclusiveEvents default true;
  end;

  { Handle user navigation in 3D scene.
    You control camera parameters and provide user input
    to this class by various methods and properties.
    You can investigate the current camera configuration by many methods,
    the most final is the @link(Matrix) method that
    generates a simple 4x4 camera matrix.

    This class is not tied to any OpenGL specifics, any VRML specifics,
    and CastleWindow etc. --- this class is fully flexible and may be used
    in any 3D program, whether using CastleWindow, OpenGL etc. or not.

    Various TCamera descendants implement various navigation
    methods, for example TExamineCamera allows the user to rotate
    and scale the model (imagine that you're holding a 3D model in your
    hands and you look at it from various sides) and TWalkCamera
    implements typical navigation in the style of first-person shooter
    games.

    The most comfortable way to use a camera is with a scene manager
    (TCastleSceneManager). You can create your camera instance,
    call it's @code(Init) method (this is initializes most important properties),
    and assign it to TCastleSceneManager.Camera property.
    This way SceneManager will pass all necessary window events to the camera,
    and when drawing SceneManager will load camera matrix like
    @code(glLoadMatrix(Camera.Matrix);).
    In fact, if you do not assign anything to TCastleSceneManager.Camera property,
    then the default camera will be created for you. So @italic(when
    using TCastleSceneManager, you do not have to do anything to use a camera)
    --- default camera will be created and automatically used for you. }
  TCamera = class(TCameraInputListener)
  private
    VisibleChangeSchedule: Cardinal;
    IsVisibleChangeScheduled: boolean;
    FInput: TCameraInputs;
    FInitialPosition, FInitialDirection, FInitialUp: TVector3Single;
    FProjectionMatrix: TMatrix4Single;
    FRadius: Single;
    FEnableDragging: boolean;

    FAnimation: boolean;
    AnimationEndTime: TFloatTime;
    AnimationCurrentTime: TFloatTime;

    AnimationBeginPosition: TVector3Single;
    AnimationBeginDirection: TVector3Single;
    AnimationBeginUp: TVector3Single;
    AnimationEndPosition: TVector3Single;
    AnimationEndDirection: TVector3Single;
    AnimationEndUp: TVector3Single;

    FFrustum: TFrustum;

    procedure RecalculateFrustum;
  protected
    { Needed for ciMouseDragging navigation.
      Checking MouseDraggingStarted means that we handle only dragging that
      was initialized on viewport (since the viewport passed events to camera).
      MouseDraggingStarted -1 means none, otherwise it's the finder index
      (to support multitouch). }
    MouseDraggingStarted: Integer;
    MouseDraggingStart: TVector2Single;

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

    procedure SetInput(const Value: TCameraInputs); virtual;
    procedure SetEnableDragging(const Value: boolean); virtual;
    function GetIgnoreAllInputs: boolean;
    procedure SetIgnoreAllInputs(const Value: boolean);
    { Setter of the @link(ProjectionMatrix) property.
      TCamera descendants may override this.
      In normal circumstances, you should not call it anywhere (it's automatically
      called by the scene manager). }
    procedure SetProjectionMatrix(const Value: TMatrix4Single); virtual;
    procedure SetRadius(const Value: Single); virtual;

    function GetPositionInternal: TVector3Single; virtual; abstract;
    procedure SetPosition(const Value: TVector3Single); virtual; abstract;

    function ReallyEnableMouseDragging: boolean; virtual;
  public
    const
      { Default value for TCamera.Radius.
        Matches the default VRML/X3D NavigationInfo.avatarSize[0]. }
      DefaultRadius = 0.25;
      DefaultInput = [ciNormal, ciMouseDragging, ci3dMouse];

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
      (e.g. for VRML/X3D Background node) and generally to transform
      directions between world and camera space.

      It's guaranteed that this is actually only 3x3 matrix,
      the 4th row and 4th column are all zero except the lowest right item
      which is 1.0. }
    function RotationMatrix: TMatrix4Single; virtual; abstract;

    { Deprecated, use more flexible @link(Input) instead.
      @code(IgnoreAllInputs := true) is equivalent to @code(Input := []),
      @code(IgnoreAllInputs := false) is equivalent to @code(Input := DefaultInput).
      @deprecated }
    property IgnoreAllInputs: boolean
      read GetIgnoreAllInputs write SetIgnoreAllInputs default false; deprecated;

    { Things related to frustum ---------------------------------------- }

    { The current camera (viewing frustum, based on
      @link(ProjectionMatrix) (set by you) and @link(Matrix) (calculated here).
      This is recalculated whenever one of these two properties change.
      Be sure to set @link(ProjectionMatrix) before using this. }
    property Frustum: TFrustum read FFrustum;

    { Projection matrix of the camera.
      Camera needs to know this to calculate @link(Frustum),
      which in turn allows rendering code to use frustum culling.

      In normal circumstances, if you use our @italic(scene manager)
      and viewport (@link(TCastleAbstractViewport)) for rendering,
      this is automatically correctly set for you. }
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
          PreferredHeight, otherwise weird things could happen
          if your avatar height is too small compared to camera radius.
          See @link(CorrectPreferredHeight).

          Especially useful if you let
          user change PreferredHeight at runtime by
          Input_IncreasePreferredHeight, Input_DecreasePreferredHeight.

          This is actually the whole use of @link(Radius) inside @link(CastleCameras) unit
          and classes. But the code all around the engine also looks for
          this @link(Radius), and the camera is a natural place to keep this
          information.)
      ) }
    property Radius: Single read FRadius write SetRadius default DefaultRadius;

    { Express current view as camera vectors: position, direction, up.

      Returned Dir and Up must be orthogonal.
      Returned Dir and Up and GravityUp are already normalized. }
    procedure GetView(out APos, ADir, AUp: TVector3Single); virtual; abstract;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3Single); virtual; abstract;

    { Set camera view from vectors: position, direction, up.

      Direction, Up and GravityUp do not have to be normalized,
      we will normalize them internally if necessary.
      But make sure they are non-zero.

      We will automatically fix Direction and Up to be orthogonal, if necessary:
      when AdjustUp = @true (the default) we will adjust the up vector
      (preserving the given direction value),
      otherwise we will adjust the direction (preserving the given up value). }
    procedure SetView(const APos, ADir, AUp: TVector3Single;
      const AdjustUp: boolean = true); virtual; abstract;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3Single;
      const AdjustUp: boolean = true); virtual; abstract;

    property Position: TVector3Single read GetPositionInternal write SetPosition;
    function GetPosition: TVector3Single; deprecated 'use Position property';
    function GetGravityUp: TVector3Single; virtual; abstract;

    { Calculate a 3D ray picked by the WindowX, WindowY position on the window.
      Uses current Container, which means that you have to add this camera
      to TCastleWindowCustom.Controls or TCastleControlCustom.Controls before
      using this method.

      Projection (read-only here) describe your projection,
      required for calculating the ray properly.
      Resulting RayDirection is always normalized.

      WindowPosition is given in the same style as TUIContainer.MousePosition:
      (0, 0) is bottom-left. }
    procedure Ray(const WindowPosition: TVector2Single;
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3Single); deprecated 'use CustomRay with proper viewport sizes, or use higher-level utilities like SceneManager.MouseRayHit instead';

    { Calculate a ray picked by current mouse position on the window.
      Uses current Container (both to get it's size and to get current
      mouse position), which means that you have to add this camera
      to TCastleWindowCustom.Controls or TCastleControlCustom.Controls before
      using this method.

      @seealso Ray
      @seealso CustomRay }
    procedure MouseRay(
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3Single); deprecated 'use CustomRay with proper viewport sizes, or use higher-level utilities like SceneManager.MouseRayHit instead';

    { Calculate a ray picked by WindowPosition position on the viewport,
      assuming current viewport dimensions are as given.
      This doesn't look at our container sizes at all.

      Projection (read-only here) describe projection,
      required for calculating the ray properly.

      Resulting RayDirection is always normalized.

      WindowPosition is given in the same style as TUIContainer.MousePosition:
      (0, 0) is bottom-left. }
    procedure CustomRay(
      const Viewport: TRectangle;
      const WindowPosition: TVector2Single;
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3Single);

    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;

    { Animate a camera smoothly into another camera settings.
      This will gradually change our settings (only the most important
      settings, that determine actual camera view, i.e. @link(Matrix) result)
      into another camera.

      Current OtherCamera settings will be internally copied during this call.
      So you can even free OtherCamera instance immediately after calling this.

      When we're during camera animation, @link(Update) doesn't do other stuff
      (e.g. gravity for TWalkCamera doesn't work, rotating for TExamineCamera
      doesn't work). This also means that the key/mouse controls of the camera
      do not work. Instead, we remember the source and target position
      (at the time AnimateTo was called) of the camera,
      and smoothly interpolate camera parameters to match the target.

      Once the animation stops, @link(Update) goes back to normal: gravity
      in TWalkCamera works again, rotating in TExamineCamera works again etc.

      Calling AnimateTo while the previous animation didn't finish yet
      is OK. This simply cancels the previous animation,
      and starts the new animation from the current position.

      @italic(Descendants implementors notes:) In this class,
      almost everything is handled (through GetView / SetView).
      In descendants you have to only ignore key/mouse/Update events
      when IsAnimation is @true.
      (Although each Update would override the view anyway, but for
      stability it's best to explicitly ignore them --- you never know
      how often Update will be called.)

      @groupBegin }
    procedure AnimateTo(OtherCamera: TCamera; const Time: TFloatTime);
    procedure AnimateTo(const Pos, Dir, Up: TVector3Single; const Time: TFloatTime);
    { @groupEnd }

    function Animation: boolean; virtual;

    { Initial camera values.

      InitialDirection and InitialUp must be always normalized,
      and orthogonal.

      Default value of InitialPosition is (0, 0, 0), InitialDirection is
      DefaultCameraDirection = (0, -1, 0), InitialUp is
      DefaultCameraUp = (0, 1, 0).

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
      This implements VRML/X3D desired behavior that
      "viewer position/orientation is conceptually a child of
      viewpoint position/orientation, and when viewpoint position/orientation
      changes, viewer should also change". }
    procedure SetInitialView(
      const AInitialPosition: TVector3Single;
      AInitialDirection, AInitialUp: TVector3Single;
      const TransformCurrentCamera: boolean); virtual;

    { Jump to initial camera view (set by SetInitialView). }
    procedure GoToInitial; virtual;

    function GetNavigationType: TNavigationType; virtual; abstract;

    { Is mouse dragging allowed by scene manager.
      This is an additional condition to enable mouse dragging,
      above the existing ciMouseDragging in Input.
      It is set internally by scene manager, to prevent camera navigation by
      dragging when we already drag a 3D item (like X3D TouchSensor). }
    property EnableDragging: boolean read FEnableDragging write SetEnableDragging;
  published
    { Input methods available to user. See documentation of TCameraInput
      type for possible values and their meaning.

      To disable any user interaction with camera (for example,
      to implement X3D "NONE" navigation type) you can simply set this to empty. }
    property Input: TCameraInputs read FInput write SetInput default DefaultInput;
  end;

  TCameraClass = class of TCamera;

  T3BoolInputs = array [0..2, boolean] of TInputShortcut;

  { Navigate the 3D model in examine mode, like you would hold
    a box with the model inside.
    The model is moved by @link(Translation),
    rotated by @link(Rotations) and scaled by @link(ScaleFactor). }
  TExamineCamera = class(TCamera)
  private
    FTranslation, FCenterOfRotation: TVector3Single;
    FRotations: TQuaternion;
    FDragMoveSpeed, FKeysMoveSpeed: Single;
    { Speed of rotations. Always zero when RotationAccelerate = false.

      This could be implemented as a quaternion,
      it even was implemented like this (and working!) for a couple
      of minutes. But this caused one problem: in Update, I want to
      apply FRotationsAnim to Rotations *scaled by SecondsPassed*.
      There's no efficient way with quaternions to say "take only SecondsPassed
      fraction of angle encoded in FRotationsAnim", AFAIK.
      The only way would be to convert FRotationsAnim back to AxisAngle,
      then scale angle, then convert back to quaternion... which makes
      the whole exercise useless. }
    FRotationsAnim: TVector3Single;
    FScaleFactor: Single;
    FModelBox: TBox3D;
    FRotationAccelerate: boolean;
    FRotationAccelerationSpeed: Single;
    FRotationSpeed: Single;
    FPosition, FDirection, FUp: TVector3Single;
    FTurntable: boolean;

    FInputs_Move: T3BoolInputs;
    FInputs_Rotate: T3BoolInputs;
    FInput_ScaleLarger: TInputShortcut;
    FInput_ScaleSmaller: TInputShortcut;
    FInput_Home: TInputShortcut;
    FInput_StopRotating: TInputShortcut;

    procedure SetRotationsAnim(const Value: TVector3Single);
    procedure SetRotations(const Value: TQuaternion);
    procedure SetScaleFactor(const Value: Single);
    procedure SetTranslation(const Value: TVector3Single);
    procedure SetModelBox(const Value: TBox3D);
    procedure SetCenterOfRotation(const Value: TVector3Single);
    function Zoom(const Factor: Single): boolean;
    procedure SetRotationAccelerate(const Value: boolean);

    function GetInput_MoveXInc: TInputShortcut;
    function GetInput_MoveXDec: TInputShortcut;
    function GetInput_MoveYInc: TInputShortcut;
    function GetInput_MoveYDec: TInputShortcut;
    function GetInput_MoveZInc: TInputShortcut;
    function GetInput_MoveZDec: TInputShortcut;
    function GetInput_RotateXInc: TInputShortcut;
    function GetInput_RotateXDec: TInputShortcut;
    function GetInput_RotateYInc: TInputShortcut;
    function GetInput_RotateYDec: TInputShortcut;
    function GetInput_RotateZInc: TInputShortcut;
    function GetInput_RotateZDec: TInputShortcut;

    function GetMouseNavigation: boolean;
    procedure SetMouseNavigation(const Value: boolean);
  protected
    function GetPositionInternal: TVector3Single; override;
    procedure SetPosition(const Value: TVector3Single); override;
  public
    const
      DefaultRotationAccelerationSpeed = 5.0;
      DefaultRotationSpeed = 2.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Matrix: TMatrix4Single; override;

    function MatrixInverse: TMatrix4Single;

    function RotationMatrix: TMatrix4Single; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;

    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; override;
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; override;

    { Current camera properties ---------------------------------------------- }

    { Current rotation of the model.
      Rotation is done around ModelBox middle (with @link(Translation) added). }
    property Rotations: TQuaternion read FRotations write SetRotations;

    { Continous rotation animation, applied each Update to Rotations. }
    property RotationsAnim: TVector3Single read FRotationsAnim write SetRotationsAnim;

    { How fast user moves the scene by mouse/touch dragging. }
    property DragMoveSpeed: Single read FDragMoveSpeed write FDragMoveSpeed default 1.0;

    { How fast user moves the scene by pressing keys. }
    property KeysMoveSpeed: Single read FKeysMoveSpeed write FKeysMoveSpeed default 1.0;

    property MoveAmount: TVector3Single read FTranslation write SetTranslation;
      deprecated 'use Translation';

    { How much to move the model. By default, zero. }
    property Translation: TVector3Single read FTranslation write SetTranslation;

    { Center of rotation and scale, relative to @link(Translation).
      By default, zero.
      Setting the @link(ModelBox) or calling @link(Init) sets this to the middle
      of the model bounding box (@link(ModelBox)), which is usually most natural. }
    property CenterOfRotation: TVector3Single read FCenterOfRotation write SetCenterOfRotation;

    { Turntable rotates the scene around its Y axis instead of current camera axis. }
    property Turntable: boolean
      read FTurntable write FTurntable default false;

    { How the model is scaled.
      Scaling is done around @link(Translation) + @link(CenterOfRotation).
      @italic(This property may never be zero (or too near zero).) }
    property ScaleFactor: Single
      read FScaleFactor write SetScaleFactor default 1;

    { The aproximate size of 3D model that will be viewed.
      This is the crucial property of this class that you have to set,
      to make the navigation work best.
      Setting this sets also CenterOfRotation to the middle of the box.

      This is usually the only property that you have to set.
      The rest, like @link(ScaleFactor), @link(Translation), @link(RotationsAnim)
      will be almost directly controlled by user (through @link(Press)
      and other events).
      @link(Rotations) will be automatically modified by @link(Update).

      So often you only need to set ModelBox, once,
      and everything else will work smoothly.

      Initially this is EmptyBox3D. }
    property ModelBox: TBox3D read FModelBox write SetModelBox;

    { Initialize most important properties of this class:
      sets ModelBox and goes to a nice view over the entire scene.

      In other words, this is just a shortcut to setting ModelBox,
      setting suitable initial view by SetInitialView,
      and then going to initial view by GoToInitial. }
    procedure Init(const AModelBox: TBox3D; const ARadius: Single);

    { Methods performing navigation.
      Usually you want to just leave this for user to control. --------------- }

    { Sets RotationsAnim to zero, stopping the rotation of the model. }
    function StopRotating: boolean;

    procedure Scale(const ScaleBy: Single); deprecated 'set ScaleFactor instead of using this method';
    procedure Move(coord: integer; const MoveDistance: Single); deprecated 'set Translation instead of using this method';

    { User inputs ------------------------------------------------------------ }

    { Alternative ways to access Input_Move/Rotate(X|Y|Z)(Inc|Dec).
      Index the array (2nd index true means increase) instead of having
      to use the full identifier.
      @groupBegin }
    property Inputs_Move: T3BoolInputs read FInputs_Move;
    property Inputs_Rotate: T3BoolInputs read FInputs_Rotate;
    { @groupEnd }

    procedure GetView(out APos, ADir, AUp: TVector3Single); override;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3Single); override;
    function GetGravityUp: TVector3Single; override;
    procedure SetView(const APos, ADir, AUp: TVector3Single;
      const AdjustUp: boolean = true); override;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3Single;
      const AdjustUp: boolean = true); override;

    procedure VisibleChange; override;
    function GetNavigationType: TNavigationType; override;

    { TODO: Input_Xxx not published, although setting them in object inspector
      actually works Ok. They are not published, because they would be always
      stored in lfm (because each has different defaults, so they
      would be stored even if developer didn't touch them),
      and we may want to break compatibility here at some point
      (when implementing 3rd-person cameras). If they would be stored in lfm
      (always), breaking compatibility would be bad (causing errors
      when reading old lfm files about missing properties,
      *even if developer didn't customize any of these Input_Xxx properties*).
      Also, the defaults would be stored in lfm file.

      Until I am sure that this is how I want to presents inputs
      (see CastleInputs discussion about local vs global),
      better to keep it only in public.
    }
    { }
    property Input_MoveXInc: TInputShortcut read GetInput_MoveXInc;
    property Input_MoveXDec: TInputShortcut read GetInput_MoveXDec;
    property Input_MoveYInc: TInputShortcut read GetInput_MoveYInc;
    property Input_MoveYDec: TInputShortcut read GetInput_MoveYDec;
    property Input_MoveZInc: TInputShortcut read GetInput_MoveZInc;
    property Input_MoveZDec: TInputShortcut read GetInput_MoveZDec;
    property Input_RotateXInc: TInputShortcut read GetInput_RotateXInc;
    property Input_RotateXDec: TInputShortcut read GetInput_RotateXDec;
    property Input_RotateYInc: TInputShortcut read GetInput_RotateYInc;
    property Input_RotateYDec: TInputShortcut read GetInput_RotateYDec;
    property Input_RotateZInc: TInputShortcut read GetInput_RotateZInc;
    property Input_RotateZDec: TInputShortcut read GetInput_RotateZDec;
    property Input_ScaleLarger: TInputShortcut read FInput_ScaleLarger;
    property Input_ScaleSmaller: TInputShortcut read FInput_ScaleSmaller;
    property Input_Home: TInputShortcut read FInput_Home;
    property Input_StopRotating: TInputShortcut read FInput_StopRotating;
  published
    { @Deprecated Include/exclude ciMouseDragging from @link(Input) instead. }
    property MouseNavigation: boolean
      read GetMouseNavigation write SetMouseNavigation default true; deprecated;

    { When @true, rotation keys make the rotation faster, and the model keeps
      rotating even when you don't hold any keys. When @false, you have to
      hold rotation keys to rotate. }
    property RotationAccelerate: boolean
      read FRotationAccelerate write SetRotationAccelerate default true;

    { Speed to change the rotation acceleration,
      used when RotationAccelerate = @true. }
    property RotationAccelerationSpeed: Single
      read FRotationAccelerationSpeed
      write FRotationAccelerationSpeed
      default DefaultRotationAccelerationSpeed;

    { Speed to change the rotation, used when RotationAccelerate = @false. }
    property RotationSpeed: Single
      read FRotationSpeed
      write FRotationSpeed
      default DefaultRotationSpeed;
  end;

  TWalkCamera = class;

  { What mouse dragging does in TWalkCamera. }
  TMouseDragMode = (
    { Moves avatar continously in the direction of mouse drag
      (default for TWalkCamera.MouseDragMode). }
    mdWalk,
    { Rotates the head when mouse is moved. }
    mdRotate,
    { Ignores the dragging. }
    mdNone);

  { See @link(TWalkCamera.DoMoveAllowed) and
    @link(TWalkCamera.OnMoveAllowed) }
  TMoveAllowedFunc = function(Camera: TWalkCamera;
    const ProposedNewPos: TVector3Single;
    out NewPos: TVector3Single;
    const BecauseOfGravity: boolean): boolean of object;

  { See @link(TWalkCamera.OnFall). }
  TFallNotifyFunc = procedure (Camera: TWalkCamera;
    const FallHeight: Single) of object;

  THeightEvent = function (Camera: TWalkCamera;
    const Position: TVector3Single;
    out AboveHeight: Single; out AboveGround: P3DTriangle): boolean of object;

  { Navigation by walking (first-person-shooter-like moving) in 3D scene.
    Camera is defined by it's position, looking direction
    and up vector, user can rotate and move camera using various keys. }
  TWalkCamera = class(TCamera)
  private
    FPosition, FDirection, FUp,
    FGravityUp: TVector3Single;
    FMoveHorizontalSpeed, FMoveVerticalSpeed, FMoveSpeed: Single;
    FRotationHorizontalSpeed, FRotationVerticalSpeed: Single;
    FRotationHorizontalPivot: Single;
    FPreferGravityUpForRotations: boolean;
    FPreferGravityUpForMoving: boolean;
    FIsAbove: boolean;
    FAboveHeight: Single;
    FAboveGround: P3DTriangle;
    FMouseLook: boolean;
    FMouseDragMode: TMouseDragMode;

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
    FInput_IncreasePreferredHeight: TInputShortcut;
    FInput_DecreasePreferredHeight: TInputShortcut;
    FInput_GravityUp: TInputShortcut;
    FInput_MoveSpeedInc: TInputShortcut;
    FInput_MoveSpeedDec: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FInput_Crouch: TInputShortcut;
    FInput_Run: TInputShortcut;

    FAllowSlowerRotations: boolean;
    FCheckModsDown: boolean;

    FMinAngleRadFromGravityUp: Single;

    FMouseLookHorizontalSensitivity: Single;
    FMouseLookVerticalSensitivity: Single;

    { This is initally false. It's used by MoveHorizontal while head bobbing,
      to avoid updating HeadBobbingPosition more than once in the same Update call.

      Updating it more than once is bad --- try e.g. holding Input_Forward
      with one of the strafe keys: you move and it's very noticeable
      that HeadBobbing seems faster. That's because
      when holding both Input_Forward and Input_StrafeRight, you shouldn't
      do HeadBobbing twice in one Update --- you should do it only Sqrt(2).
      When you will also hold Input_RotateRight at the same time --- situation
      gets a little complicated...

      The good solution seems to just do head bobbing only once.
      In some special cases this means that head bobbing will be done
      *less often* than it should be, but this doesn't hurt. }
    HeadBobbingAlreadyDone: boolean;

    { MoveHorizontal call sets this to @true to indicate that some
      horizontal move was done. }
    MoveHorizontalDone: boolean;

    FMoveForward, FMoveBackward: boolean;

    procedure RotateAroundGravityUp(const AngleDeg: Single);
    procedure RotateAroundUp(const AngleDeg: Single);
    procedure RotateHorizontal(const AngleDeg: Single);
    procedure RotateVertical(const AngleDeg: Single);

    { Like Move, but you pass here final ProposedNewPos. }
    function MoveTo(const ProposedNewPos: TVector3Single;
      const BecauseOfGravity, CheckClimbHeight: boolean): boolean;
    { Try to move from current Position to Position + MoveVector.
      Checks DoMoveAllowed, also (if CheckClimbHeight is @true)
      checks the ClimbHeight limit.

      Returns @false if move was not possible and Position didn't change.
      Returns @true is some move occured (but don't assume too much:
      possibly we didn't move to exactly Position + MoveVector
      because of wall sliding). }
    function Move(const MoveVector: TVector3Single;
      const BecauseOfGravity, CheckClimbHeight: boolean): boolean;
    { Forward or backward move. Multiply must be +1 or -1. }
    procedure MoveHorizontal(const SecondsPassed: Single; const Multiply: Integer = 1);
    { Up or down move, only when flying (ignored when @link(Gravity) is @true). }
    procedure MoveVertical(const SecondsPassed: Single; const Multiply: Integer);
    { Like RotateHorizontal, but it uses
      PreferGravityUpForMoving to decide which rotation to use.
      This way when PreferGravityUpForMoving, then we rotate versus GravityUp,
      move in GravityUp plane, and then rotate back versus GravityUp.
      If not PreferGravityUpForMoving, then we do all this versus Up.
      And so everything works. }
    procedure RotateHorizontalForStrafeMove(const AngleDeg: Single);

    { Call always after horizontal rotation (but before ScheduleVisibleChange).
      This will eventually adjust FPosition for RotationHorizontalPivot <> 0. }
    procedure AdjustForRotationHorizontalPivot(const OldDirection: TVector3Single);

    { Jump.

      Returns if a jump was actually done. For example, you cannot
      jump when there's no gravity, or you're already in the middle
      of the jump. Can be useful to determine if key was handled and such. }
    function Jump: boolean;
  private
    { Private things related to gravity ---------------------------- }

    FPreferredHeight: Single;
    FFalling: boolean;
    FFallingStartPosition: TVector3Single;
    FOnFall: TFallNotifyFunc;
    FFallSpeedStart: Single;
    FFallSpeed: Single;
    FFallSpeedIncrease: Single;
    FGravity: boolean;
    FOnHeight: THeightEvent;
    FGrowSpeed: Single;
    { This is used by FallingEffect to temporary modify Matrix result
      by rotating Up around Direction. In degress. }
    Fde_UpRotate: Single;
    { This is used by FallingEffect to consistently rotate us.
      This is either -1, 0 or +1. }
    Fde_RotateHorizontal: Integer;
    FFallingEffect: boolean;
    FClimbHeight: Single;

    FJumpMaxHeight: Single;
    FIsJumping: boolean;
    FJumpHeight: Single;
    FJumpTime: Single;
    FJumpHorizontalSpeedMultiply: Single;

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
    FMouseDraggingHorizontalRotationSpeed, FMouseDraggingVerticalRotationSpeed: Single;

    function RealPreferredHeightNoHeadBobbing: Single;
    function RealPreferredHeightMargin: Single;
  protected
    { Call OnHeight callback. }
    procedure Height(const APosition: TVector3Single;
      out AIsAbove: boolean;
      out AnAboveHeight: Single; out AnAboveGround: P3DTriangle); virtual;

    function GetPositionInternal: TVector3Single; override;
    procedure SetPosition(const Value: TVector3Single); override;

    function ReallyEnableMouseDragging: boolean; override;
  public
    const
      DefaultFallSpeedStart = 0.5;
      DefaultGrowSpeed = 1.0;
      DefaultHeadBobbing = 0.02;
      DefaultCrouchHeight = 0.5;
      DefaultJumpMaxHeight = 1.0;
      DefaultMinAngleRadFromGravityUp = { 10 degress } Pi / 18; { }
      DefaultRotationHorizontalSpeed = 150;
      DefaultRotationVerticalSpeed = 100;
      DefaultFallSpeedIncrease = 13/12;
      DefaultMouseLookHorizontalSensitivity = 0.09;
      DefaultMouseLookVerticalSensitivity = 0.09;
      DefaultHeadBobbingTime = 0.5;
      DefaultJumpHorizontalSpeedMultiply = 2.0;
      DefaultJumpTime = 1.0 / 8.0;
      DefaultMouseDraggingHorizontalRotationSpeed = 0.1;
      DefaultMouseDraggingVerticalRotationSpeed = 0.1;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Matrix: TMatrix4Single; override;
    function RotationMatrix: TMatrix4Single; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; override;
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; override;

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
      move speed by scaling this vector.
      Use MoveSpeed, MoveHorizontalSpeed, MoveVerticalSpeed instead.

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
      then in nearest Update
      calls @link(Up) will be gradually fixed, so that @link(Direction) and @link(Up)
      and GravityUp are on the same plane. Also @link(Direction) may be adjusted
      to honour MinAngleRadFromGravityUp.

      With PreferGravityUpForMoving, this affects moving:
      horizontal moving (forward, backward, strafe),
      and vertical moving (Input_Jump and Input_Crouch when @link(Gravity) is @false).
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
          press Input_Jump and Input_Crouch. Simply pressing Input_Forward
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

    { Return @link(Direction) vector rotated such that it is
      orthogonal to GravityUp. This way it returns @link(Direction) projected
      on the gravity horizontal plane, which neutralizes such things
      like raising / bowing your head.
      Result is always normalized (length 1).

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

      Sets also PreferredHeight and Radius.
      PreferredHeight may be adjusted to be sensible
      (by calling CorrectPreferredHeight(ARadius)).
      You can pass ARadius = 0.0 if you really don't want this
      PreferredHeight adjustment. }
    procedure Init(const AInitialPosition, AInitialDirection,
      AInitialUp: TVector3Single;
      const AGravityUp: TVector3Single;
      const APreferredHeight: Single;
      const ARadius: Single); overload;

    { Alternative Init that sets camera properties such that
      an object inside Box is more or less "visible good".
      Sets InitialCameraXxx properties to make it look right,
      sets current CameraXxx properties to InitialCameraXxx.
      Sets GravityUp to the same thing as InitialUp.
      Sets also PreferredHeight to make it behave "sensibly". }
    procedure Init(const box: TBox3D; const ARadius: Single); overload;

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
      They say how much angle change is produced by moving mouse by 1 pixel.
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

    { What mouse dragging does. Used only when ciMouseDragging in @link(Input). }
    property MouseDragMode: TMouseDragMode
      read FMouseDragMode write FMouseDragMode default mdWalk;

    function Motion(const Event: TInputMotion): boolean; override;

    { Things related to gravity ---------------------------------------- }

    { This unlocks a couple of features and automatic behaviors
      related to gravity. Gravity always drags the camera down to
      -GravityUp.

      Summary of things done by gravity:
      @unorderedList(
        @item(It uses OnHeight to get camera height above the ground.)
        @item(It allows player to jump. See Input_Jump, IsJumping, JumpMaxHeight,
          JumpHorizontalSpeedMultiply.)
        @item(It allows player to crouch. See Input_Crouch, CrouchHeight.)
        @item(It tries to keep @link(Position) above the ground on
          PreferredHeight height.)
        @item(When current height is too small --- @link(Position) is moved up.
          See GrowSpeed.)
        @item(When current height is too large --- we're falling down.
          See Falling, OnFall, FallSpeedStart,
          FallSpeedIncrease, FallingEffect.)
        @item(It does head bobbing. See HeadBobbing, HeadBobbingTime.)
      )

      While there are many properties allowing you to control
      gravity behavior, most of them have initial values that should be
      sensible in all cases. The only things that you really want to take
      care of are: OnHeight and PreferredHeight.
      Everything else should basically work auto-magically.

      Note that Gravity setting is independent from
      PreferGravityUpForRotations or PreferGravityUpForMoving settings ---
      PreferGravityUpXxx say how the player controls work,
      Gravity says what happens to player due to ... well, due to gravity. }
    property Gravity: boolean
      read FGravity write FGravity default false;

    { When @link(Gravity) is on, @link(Position) tries to stay PreferredHeight
      above the ground. Temporary it may be lower (player can
      shortly "duck" when he falls from high).

      This must always be >= 0.
      You should set this to something greater than zero to get sensible
      behavior of some things related to @link(Gravity),
      and also you should set OnHeight.

      See CorrectPreferredHeight for important property
      of PreferredHeight that you should keep. }
    property PreferredHeight: Single
      read FPreferredHeight write FPreferredHeight default 0.0;

    { This procedure corrects PreferredHeight based on your Radius
      and on current HeadBobbing.

      Exactly what and why is done: if you do any kind of collision
      detection with some Radius, then
      you should make sure that RealPreferredHeight is always >= of your
      Radius, otherwise strange effects may happen when crouching
      or when head bobbing forces camera to go down.

      Exactly, the required equation is

      @preformatted(
        MinimumRealPreferredHeight :=
          PreferredHeight * CrouchHeight * (1 - HeadBobbing);
      )

      and always must be

      @preformatted(
        MinimumRealPreferredHeight >= RealPreferredHeight
      )

      Reasoning: otherwise this class would "want camera to fall down"
      (because we will always be higher than RealPreferredHeight)
      but your OnMoveAllowed would not allow it (because Radius
      would not allow it). Note that this class doesn't keep value
      of your Radius, because collision detection
      is (by design) never done by this class --- it's always
      delegated to OnHeight and OnMoveAllowed.
      Also, it's not exactly forced @italic(how) you should force this
      condition to hold. Sometimes the good solution is to adjust
      Radius, not to adjust PreferredHeight.

      Anyway, this method will make sure that this condition
      holds by eventually adjusting (making larger) PreferredHeight.
      Note that for Radius = 0.0 this will always leave
      PreferredHeight as it is. }
    procedure CorrectPreferredHeight;

    { The tallest height that you can climb.
      This is checked in each single horizontal move when @link(Gravity) works.
      Must be >= 0. Value 0 means there is no limit (and makes a small speedup).

      This is reliable to prevent user from climbing stairs and such,
      when vertical walls are really vertical (not just steep-almost-vertical).

      It's not 100% reliable to prevent player from climbing steep hills.
      That's because, depending on how often an event processing occurs,
      you actually climb using less or more steps.
      So even a very steep hill can be always
      climbed on a computer with very fast speed, because with large FPS you
      effectively climb it using a lot of very small steps (assuming that
      FPS limit is not enabled, that is CastleWindow.TCastleApplication.LimitFPS
      or CastleControl.LimitFPS is zero).

      Remember that user can still try jumping to climb on high obstactes.
      See JumpMaxHeight for a way to control jumping.

      For a 100% reliable way to prevent user from reaching some point,
      that does not rely on specific camera/gravity settings,
      you should build actual walls in 3D (invisible walls
      can be created by Collision.proxy in VRML/X3D). }
    property ClimbHeight: Single read FClimbHeight write FClimbHeight;

    { Assign here the callback (or override @link(Height))
      to say what is the current height of camera above the ground.
      This should be calculated like collision of ray from @link(Position)
      in direction -GravityUp with the scene.
      See T3D.Height for specification what returned parameters
      mean.

      Implementation of @link(Height) in this class
      calls OnHeight, if assigned. (If not assigned,
      we assume no collision: IsAbove = @false, AboveHeight = MaxSingle,
      AboveGround = @nil). }
    property OnHeight: THeightEvent read FOnHeight write FOnHeight;

    { Notification that we have been falling down for some time,
      and suddenly stopped (which means we "hit the ground").
      Of course this is used only when @link(Gravity) is @true
      (it can also be called shortly after you changed
      @link(Gravity) from @true to @false, so don't simply assert
      here that @link(Gravity) is @true).

      This event can be useful in games, for example to lower player's health,
      and/or make a visual effect (like a "red out" indicating pain)
      and/or make a sound effect ("Ouch!" or "Thud!" or such sounds).
      You can look at FallHeight parameter, given to the callback,
      e.g. to gauge how much health decreases. }
    property OnFall: TFallNotifyFunc
      read FOnFall write FOnFall;

    { Initial speed of falling down.
      Of course this is used only when @link(Gravity) is true.

      Note that while falling down,
      the camera will actually fall with greater and greated speed
      (this adds more realism to the gravity effect...).
      Note that this is always relative to @link(Direction) length.
      @link(Direction) determines moving speed --- and so it determines
      also falling speed. The default DefaultFallSpeedStart
      is chosen to be something sensible, to usually get nice effect
      of falling.

      You can change it at any time, but note that if you change this
      while Falling is @true, then you will not change the
      "current falling down speed". You will change only the falling down
      speed used the next time. }
    property FallSpeedStart: Single
      read FFallSpeedStart write FFallSpeedStart
      default DefaultFallSpeedStart;

    { When falling down, the speed increases.
      Set this to 1.0 to fall down with constant speed
      (taken from FallSpeedStart). }
    property FallSpeedIncrease: Single
      read FFallSpeedIncrease write FFallSpeedIncrease
      default DefaultFallSpeedIncrease;

    { Are we currently falling down because of gravity. }
    property Falling: boolean read FFalling write FFalling;

    { If Falling, then this will force Falling to false
      @bold(without calling OnFallenDown). It's much like forcing
      the opinion that "camera is not falling down right now".

      Of course, if in the nearest Update we will find out (using
      OnHeight) that camera is too high above the ground,
      then we will start falling down again, setting Falling
      back to true. (but then we will start falling down from the beginning,
      starting at given @link(Position) and with initial falling down speed).

      This is useful to call if you just changed @link(Position) because
      e.g. the player teleported somewhere (or e.g. game levels changed).
      In this case you just want to forget the fact that camera
      was falling down --- no consequences (like lowering player's
      health, fadeout etc.). }
    procedure CancelFalling;

    { Make a nice dizzying camera effect when falling down.
      This adds temporary camera rotations simulating that you
      rotate randomly and helplessly when falling down.

      Of course this is meaningfull only when @link(Gravity) works.

      Note that changing it from @true to @false doesn't immediately
      "cancel out" this effect if it's currently in progress.
      It only prevents this effect from starting again. }
    property FallingEffect: boolean
      read FFallingEffect write FFallingEffect default true;

    { When @link(Gravity) works and camera height above the ground
      is less than PreferredHeight, then we try to "grow",
      i.e. camera position increases along the GravityUp
      so that camera height above the ground is closer to
      PreferredHeight. This property (together with length of
      @link(Direction), that always determines every moving speed)
      determines the speed of this growth. }
    property GrowSpeed: Single
      read FGrowSpeed write FGrowSpeed
      default DefaultGrowSpeed;

    { How high can you jump ?
      The max jump distance is calculated as
      JumpMaxHeight * PreferredHeight, see MaxJumpDistance. }
    property JumpMaxHeight: Single
      read FJumpMaxHeight write FJumpMaxHeight default DefaultJumpMaxHeight;

    { Returns just JumpMaxHeight * PreferredHeight,
      see JumpMaxHeight for explanation. }
    function MaxJumpDistance: Single;

    { Camera is in the middle of a "jump" move right now. }
    property IsJumping: boolean read FIsJumping;

    { Scales the speed of horizontal moving during jump. }
    property JumpHorizontalSpeedMultiply: Single
      read FJumpHorizontalSpeedMultiply write FJumpHorizontalSpeedMultiply
      default DefaultJumpHorizontalSpeedMultiply;

    { How fast do you jump up. This is the time, in seconds, in takes
      to reach MaxJumpDistance height when jumping. }
    property JumpTime: Single read FJumpTime write FJumpTime
      default DefaultJumpTime;

    { When you move horizontally, you get "head bobbing" effect
      --- camera position slightly changes it's vertical position,
      going a little up, then a little down, then a little up again etc.

      This property mutiplied by PreferredHeight
      says how much head bobbing can move you along GravityUp.
      Set this to 0 to disable head bobbing.
      This must always be < 1.0. For sensible effects, this should
      be rather close to 0.0.

      Of course this is meaningfull only when @link(Gravity) works. }
    property HeadBobbing: Single
      read FHeadBobbing write FHeadBobbing default DefaultHeadBobbing;

    { Controls head bobbing frequency. In the time of HeadBobbingTime seconds,
      we do full head bobbing sequence (camera swing up, then down again).

      Note that if you do a footsteps sound in your game (see
      stPlayerFootstepsDefault or TMaterialProperty.FootstepsSound)
      then you will want this property to match your footsteps sound length,
      things feel and sound natural then.
      Also, often it sounds better to record two footsteps inside
      a single sound file, in which case the footstep sound length should be twice
      as long as this property. For example, record 2 steps inside a 1-second long
      footstep sound, and set this property to 0.5 a second (which is a default
      in fact). }
    property HeadBobbingTime: Single
      read FHeadBobbingTime write FHeadBobbingTime
      default DefaultHeadBobbingTime;

    { This defines the preferred height of camera when crouching.
      This is always mutiplied to PreferredHeight.
      This should always be <= 1 (CrouchHeight = 1 effectively disables
      crouching, although it's better to do this by calling MakeClear
      on Input_Crouch). }
    property CrouchHeight: Single
      read FCrouchHeight write FCrouchHeight default DefaultCrouchHeight;

    { Is player crouching right now. }
    property IsCrouching: boolean read FIsCrouching;

    { This is PreferredHeight slightly modified by head bobbing
      and crouch. It can be useful for collision detection
      between camera and something. }
    function RealPreferredHeight: Single;

    { This makes a visual effect of camera falling down horizontally
      on the ground. Nice to use when player died, and you want to show
      that it's body falled on the ground.
      This works by gradually changing @link(Up) such that
      it gets orthogonal to GravityUp. }
    procedure FallOnTheGround;

    { @true when the effect caused by FallOnTheGround is stil in motion. }
    property FallingOnTheGround: boolean read FFallingOnTheGround;

    { This is @true when gravity works (that is @link(Gravity) is @true),
      and player is standing stable on the ground. This is set in every Update.

      You can use this e.g. to make some effects when player is on some
      special ground (standing or walking), e.g. hurt player when he's
      standing on some toxical ground.

      @seealso IsWalkingOnTheGround }
    property IsOnTheGround: boolean read FIsOnTheGround;

    { This is @true when gravity works (that is @link(Gravity) is @true),
      and player is standing stable on the ground, and player is moving
      horizontally. In other words, this is like "IsOnTheGround and (s)he's
      walking". This is set in every Update.

      The intention is that you can use this to make
      some "footsteps" sound for the player. }
    property IsWalkingOnTheGround: boolean read FIsWalkingOnTheGround;

    procedure GetView(out APos, ADir, AUp: TVector3Single); override;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3Single); override;
    function GetGravityUp: TVector3Single; override;
    procedure SetView(const ADir, AUp: TVector3Single;
      const AdjustUp: boolean = true);
    procedure SetView(const APos, ADir, AUp: TVector3Single;
      const AdjustUp: boolean = true); override;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3Single;
      const AdjustUp: boolean = true); override;
    function GetNavigationType: TNavigationType; override;

    { Change up vector, keeping the direction unchanged.
      If necessary, the up vector provided here will be fixed to be orthogonal
      to direction.
      See T3DOrient.UpPrefer for detailed documentation what this does. }
    procedure UpPrefer(const AUp: TVector3Single);

    { Last known information about whether camera is over the ground.
      Updated by using @link(Height) call. For normal TCamera descendants,
      this means using OnHeight callback.

      These are updated only when @link(Height)
      is continously called, which in practice means:
      only when @link(Gravity) is @true.

      We do not (and, currently, cannot) track here if
      AboveGround pointer will be eventually released (which may happen
      if you release your 3D scene, or rebuild scene causing octree rebuild).
      This is not a problem for camera class, since we do not use this
      pointer for anything. But if you use this pointer,
      then you may want to take care to eventually set it to @nil when
      your octree or such is released.

      @groupBegin }
    property IsAbove: boolean read FIsAbove;
    property AboveHeight: Single read FAboveHeight;
    property AboveGround: P3DTriangle read FAboveGround write FAboveGround;
    { @groupEnd }

    { TODO: Input_Xxx not published. See TExamineCamera Input_Xxx notes
      for reasoning. }
    { }
    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_LeftRot: TInputShortcut read FInput_LeftRot;
    property Input_RightRot: TInputShortcut read FInput_RightRot;
    property Input_LeftStrafe: TInputShortcut read FInput_LeftStrafe;
    property Input_RightStrafe: TInputShortcut read FInput_RightStrafe;
    property Input_UpRotate: TInputShortcut read FInput_UpRotate;
    property Input_DownRotate: TInputShortcut read FInput_DownRotate;
    property Input_IncreasePreferredHeight: TInputShortcut read FInput_IncreasePreferredHeight;
    property Input_DecreasePreferredHeight: TInputShortcut read FInput_DecreasePreferredHeight;
    property Input_GravityUp: TInputShortcut read FInput_GravityUp;
    property Input_Run: TInputShortcut read FInput_Run;

    { Change the MoveSpeed.
      @groupBegin }
    property Input_MoveSpeedInc: TInputShortcut read FInput_MoveSpeedInc;
    property Input_MoveSpeedDec: TInputShortcut read FInput_MoveSpeedDec;
    { @groupEnd }

    { Jumping and crouching (when @link(Gravity) = @true) or flying up / down
      (when @link(Gravity) = @false).
      @groupBegin }
    property Input_Jump: TInputShortcut read FInput_Jump;
    property Input_Crouch: TInputShortcut read FInput_Crouch;
    { @groupEnd }

    { Move forward, just like Input_Forward would be pressed. }
    property MoveForward: boolean read FMoveForward write FMoveForward;
    { Move backward, just like Input_Backward would be pressed. }
    property MoveBackward: boolean read FMoveBackward write FMoveBackward;
  published
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
      rotation keys work 10x slower. Also Increase/DecreasePreferredHeight
      work only when Ctrl pressed.
      Other keys with other modifiers
      don't work. We allow shift, because to press character "+" on non-numpad
      keyboard (useful on laptops, where numpad is difficult) you
      probably need to press shift.

      If @false then all keys work as usual, no matter what
      modifiers are pressed. And rotation keys never work 10x slower
      (AllowSlowerRotations is ignored),
      also Increase/DecreasePreferredHeight are ignored. }
    property CheckModsDown: boolean
      read FCheckModsDown write FCheckModsDown
      default true;

    { Moving speeds. MoveHorizontalSpeed is only for horizontal movement,
      MoveVerticalSpeed is only for vertical, and MoveSpeed simply affects
      both types of movement. Effectively, we always scale the speed
      of movement by either @code(MoveHorizontalSpeed * MoveSpeed) or
      @code(MoveVerticalSpeed * MoveSpeed).

      We move by distance @code(MoveSpeed * MoveHorizontalSpeed (or MoveVerticalSpeed))
      during one second. Assuming "normal circumstances",
      namely that SecondsPassed provided to @link(Update) method
      is expressed in seconds (which is the case, when you use
      camera as TCastleSceneManager.Camera).
      So if you leave MoveHorizontalSpeed = MoveVerticalSpeed = 1 (as default),
      MoveSpeed expresses the speed in nice units / per second.

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

    { Speed (degrees per pixel delta) of rotations by mouse dragging.
      Relevant only if ciMouseDragging in @link(Input), and MouseDragMode is mdRotate.
      Separate for horizontal and vertical, this way you can e.g. limit
      (or disable) vertical rotations, useful for games where you mostly
      look horizontally and accidentally looking up/down is more confusing
      than useful.
      @groupBegin }
    property MouseDraggingHorizontalRotationSpeed: Single
      read FMouseDraggingHorizontalRotationSpeed write FMouseDraggingHorizontalRotationSpeed
      default DefaultMouseDraggingHorizontalRotationSpeed;
    property MouseDraggingVerticalRotationSpeed: Single
      read FMouseDraggingVerticalRotationSpeed write FMouseDraggingVerticalRotationSpeed
      default DefaultMouseDraggingVerticalRotationSpeed;
    { @groupEnd }

    { Horizontal rotation can rotate around a vector that is RotationHorizontalPivot units
      forward before the camera. This is a poor-mans way to implement some 3rd camera game.
      Note that when non-zero this may (for now) move the camera without actually checking
      OnMoveAllowed. }
    property RotationHorizontalPivot: Single
      read FRotationHorizontalPivot write FRotationHorizontalPivot default 0;
  end;

  { Camera that allows any kind of navigation (Examine, Walk).
    You can switch between navigation types, while preserving the camera view.

    This simply keeps an TExamineCamera and TWalkCamera instances inside,
    and passes events (key, mouse presses, Update) to the current one.
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
    FNavigationClass: TNavigationClass;
    procedure SetNavigationClass(const Value: TNavigationClass);
    procedure SetNavigationType(const Value: TNavigationType);
  protected
    procedure SetInput(const Value: TCameraInputs); override;
    procedure SetEnableDragging(const Value: boolean); override;
    procedure SetProjectionMatrix(const Value: TMatrix4Single); override;
    procedure SetContainer(const Value: TUIContainer); override;
    procedure SetRadius(const Value: Single); override;
    function GetPositionInternal: TVector3Single; override;
    procedure SetPosition(const Value: TVector3Single); override;
  public
    constructor Create(AOwner: TComponent); override;

    { Current (determined by NavigationClass) internal camera,
      that is either @link(Examine) or @link(Walk). }
    function Current: TCamera;

    function Matrix: TMatrix4Single; override;
    function RotationMatrix: TMatrix4Single; override;
    procedure GetView(out APos, ADir, AUp: TVector3Single); override;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3Single); override;
    function GetGravityUp: TVector3Single; override;
    procedure SetView(const APos, ADir, AUp: TVector3Single;
      const AdjustUp: boolean = true); override;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3Single;
      const AdjustUp: boolean = true); override;

    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;

    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; override;
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; override;

    procedure Resize; override;
    function GetNavigationType: TNavigationType; override;

    procedure SetInitialView(
      const AInitialPosition: TVector3Single;
      AInitialDirection, AInitialUp: TVector3Single;
      const TransformCurrentCamera: boolean); override;
  published
    property Examine: TExamineCamera read FExamine;
    property Walk: TWalkCamera read FWalk;

    { Choose navigation method by choosing particular camera class.
      The names of this correspond to camera classes (TExamineCamera,
      TWalkCamera). }
    property NavigationClass: TNavigationClass
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
        @item Input (and derived deprecated properties IgnoreAllInputs and MouseNavigation),
        @item Walk.Gravity (see TWalkCamera.Gravity),
        @item Walk.PreferGravityUpForRotations (see TWalkCamera.PreferGravityUpForRotations),
        @item Walk.PreferGravityUpForMoving (see TWalkCamera.PreferGravityUpForMoving)
      )

      If you write to NavigationType, then you @italic(should not) touch the
      above properties directly. That's because not every combination of
      above properties correspond to some sensible value of NavigationType.
      If you directly set some weird configuration, reading NavigationType will
      try it's best to determine the closest TNavigationType value
      that is similar to your configuration. }
    property NavigationType: TNavigationType
      read GetNavigationType write SetNavigationType default ntExamine;
  end;

{ See TWalkCamera.CorrectPreferredHeight.
  This is a global version, sometimes may be useful. }
procedure CorrectPreferredHeight(var PreferredHeight: Single;
  const Radius: Single; const CrouchHeight, HeadBobbing: Single);

const
  { Default camera direction and up vectors, used to define the meaning
    of "camera orientation" for CamDirUp2Orient routines.
    These match VRML/X3D default camera values.
    @groupBegin }
  DefaultCameraDirection: TVector3Single = (0, 0, -1);
  DefaultCameraUp: TVector3Single = (0, 1, 0);
  { @groupEnd }

{ Convert camera direction and up vectors into VRML/X3D "orientation" vector.

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
  VRML/X3D "orientation".

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

uses Math, CastleStringUtils, CastleLog;

procedure Register;
begin
  RegisterComponents('Castle', [TExamineCamera, TWalkCamera, TUniversalCamera]);
end;

{ TCameraInputListener ------------------------------------------------------------- }

constructor TCameraInputListener.Create(AOwner: TComponent);
begin
  inherited;
  FExclusiveEvents := true;
  FCursor := mcDefault;
end;

function TCameraInputListener.Press(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function TCameraInputListener.Release(const Event: TInputPressRelease): boolean;
begin
  Result := false;
end;

function TCameraInputListener.Motion(const Event: TInputMotion): boolean;
begin
  Result := false;
end;

function TCameraInputListener.SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean;
begin
  Result := false;
end;

function TCameraInputListener.SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean;
begin
  Result := false;
end;

function TCameraInputListener.JoyAxisMove(const JoyID, Axis: Byte): boolean;
begin
  Result := False;
end;

function TCameraInputListener.JoyButtonPress(const JoyID, Button: Byte): boolean;
begin
  Result := False;
end;

procedure TCameraInputListener.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
end;

procedure TCameraInputListener.VisibleChange;
begin
  if Assigned(OnVisibleChange) then
    OnVisibleChange(Self);
end;

function TCameraInputListener.AllowSuspendForInput: boolean;
begin
  Result := true;
end;

procedure TCameraInputListener.Resize;
begin
  {$warnings off}
  ContainerResize(ContainerWidth, ContainerHeight); // call deprecated, to keep it working
  {$warnings on}
end;

procedure TCameraInputListener.ContainerResize(const AContainerWidth, AContainerHeight: Cardinal);
begin
end;

function TCameraInputListener.ContainerWidth: Cardinal;
begin
  if ContainerSizeKnown then
    Result := Container.Width else
    Result := 0;
end;

function TCameraInputListener.ContainerHeight: Cardinal;
begin
  if ContainerSizeKnown then
    Result := Container.Height else
    Result := 0;
end;

function TCameraInputListener.ContainerRect: TRectangle;
begin
  if ContainerSizeKnown then
    Result := Container.Rect else
    Result := TRectangle.Empty;
end;

function TCameraInputListener.ContainerSizeKnown: boolean;
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

procedure TCameraInputListener.SetCursor(const Value: TMouseCursor);
begin
  if Value <> FCursor then
  begin
    FCursor := Value;
    if Container <> nil then Container.UpdateFocusAndMouseCursor;
    DoCursorChange;
  end;
end;

procedure TCameraInputListener.DoCursorChange;
begin
  if Assigned(OnCursorChange) then OnCursorChange(Self);
end;

procedure TCameraInputListener.SetContainer(const Value: TUIContainer);
begin
  FContainer := Value;
end;

{ TCamera ------------------------------------------------------------ }

constructor TCamera.Create(AOwner: TComponent);
begin
  inherited;
  FProjectionMatrix := IdentityMatrix4Single;
  FInitialPosition  := Vector3Single(0, 0, 0);
  FInitialDirection := DefaultCameraDirection;
  FInitialUp        := DefaultCameraUp;
  FRadius := DefaultRadius;
  FInput  := DefaultInput;
  MouseDraggingStarted := -1;
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
    { Set IsVisibleChangeScheduled first.
      That is because VisibleChange may be overriden and/or may call
      various callbacks, and these callbacks in turn may again call
      BeginVisibleChangeSchedule. And BeginVisibleChangeSchedule must start
      with good state, see assertion there. }
    IsVisibleChangeScheduled := false;
    VisibleChange;
  end;
end;

procedure TCamera.SetInput(const Value: TCameraInputs);
begin
  FInput := Value;
end;

procedure TCamera.SetEnableDragging(const Value: boolean);
begin
  FEnableDragging := Value;
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

procedure TCamera.SetRadius(const Value: Single);
begin
  FRadius := Value;
end;

procedure TCamera.Ray(const WindowPosition: TVector2Single;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3Single);
begin
  Assert(ContainerSizeKnown, 'Camera container size not known yet (probably camera not added to Controls list), cannot use TCamera.Ray');
  CustomRay(ContainerRect, WindowPosition, Projection, RayOrigin, RayDirection);
end;

procedure TCamera.MouseRay(
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3Single);
begin
  Assert(ContainerSizeKnown, 'Camera container size not known yet (probably camera not added to Controls list), cannot use TCamera.MouseRay');
  CustomRay(ContainerRect, Container.MousePosition, Projection, RayOrigin, RayDirection);
end;

procedure TCamera.CustomRay(
  const Viewport: TRectangle;
  const WindowPosition: TVector2Single;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3Single);
var
  Pos, Dir, Up: TVector3Single;
begin
  GetView(Pos, Dir, Up);

  PrimaryRay(
    WindowPosition[0] - Viewport.Left,
    WindowPosition[1] - Viewport.Bottom,
    Viewport.Width, Viewport.Height,
    Pos, Dir, Up,
    Projection,
    RayOrigin, RayDirection);
end;

procedure TCamera.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  if FAnimation then
  begin
    AnimationCurrentTime += SecondsPassed;
    if AnimationCurrentTime > AnimationEndTime then
    begin
      FAnimation := false;
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

procedure TCamera.AnimateTo(const Pos, Dir, Up: TVector3Single; const Time: TFloatTime);
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
  FAnimation := not (
    VectorsEqual(AnimationBeginPosition , AnimationEndPosition) and
    VectorsEqual(AnimationBeginDirection, AnimationEndDirection) and
    VectorsEqual(AnimationBeginUp       , AnimationEndUp));
end;

procedure TCamera.AnimateTo(OtherCamera: TCamera; const Time: TFloatTime);
var
  Pos, Dir, Up: TVector3Single;
begin
  OtherCamera.GetView(Pos, Dir, Up);
  AnimateTo(Pos, Dir, Up, Time);
end;

function TCamera.Animation: boolean;
begin
  Result := FAnimation;
end;

procedure TCamera.SetInitialView(
  const AInitialPosition: TVector3Single;
  AInitialDirection, AInitialUp: TVector3Single;
  const TransformCurrentCamera: boolean);
var
  OldInitialOrientation, NewInitialOrientation, Orientation: TQuaternion;
  Pos, Dir, Up: TVector3Single;
begin
  NormalizeVar(AInitialDirection);
  NormalizeVar(AInitialUp);
  MakeVectorsOrthoOnTheirPlane(AInitialUp, AInitialDirection);

  if TransformCurrentCamera then
  begin
    GetView(Pos, Dir, Up);

    VectorAddVar(Pos, VectorSubtract(AInitialPosition, FInitialPosition));

    if not (VectorsPerfectlyEqual(FInitialDirection, AInitialDirection) and
            VectorsPerfectlyEqual(FInitialUp       , AInitialUp ) ) then
    begin
      OldInitialOrientation := CamDirUp2OrientQuat(FInitialDirection, FInitialUp);
      NewInitialOrientation := CamDirUp2OrientQuat(AInitialDirection, AInitialUp);
      Orientation           := CamDirUp2OrientQuat(Dir, Up);

      { I want new Orientation :=
          (Orientation - OldInitialOrientation) + NewInitialOrientation. }
      Orientation := OldInitialOrientation.Conjugate * Orientation;
      Orientation := NewInitialOrientation * Orientation;

      { Now that we have Orientation, transform it into new Dir/Up. }
      Dir := Orientation.Rotate(DefaultCameraDirection);
      Up  := Orientation.Rotate(DefaultCameraUp);
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

function TCamera.GetIgnoreAllInputs: boolean;
begin
  Result := Input = [];
end;

procedure TCamera.SetIgnoreAllInputs(const Value: boolean);
begin
  if Value then
    Input := [] else
    Input := DefaultInput;
end;

function TCamera.ReallyEnableMouseDragging: boolean;
begin
  Result := (ciMouseDragging in Input) and EnableDragging;
end;

function TCamera.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if (Event.EventType = itMouseButton) and
     ReallyEnableMouseDragging then
  begin
    MouseDraggingStart := Container.MousePosition;
    MouseDraggingStarted := Event.FingerIndex;
  end;
end;

function TCamera.Release(const Event: TInputPressRelease): boolean;
begin
  if Event.EventType = itMouseButton then
    MouseDraggingStarted := -1;
  Result := inherited;
end;

function TCamera.GetPosition: TVector3Single;
begin
  Result := GetPositionInternal;
end;

{ TExamineCamera ------------------------------------------------------------ }

constructor TExamineCamera.Create(AOwner: TComponent);
type
  T3BoolKeys = array [0..2, boolean] of TKey;
const
  DefaultInputs_Move: T3BoolKeys =
    ((K_Left, K_Right), (K_Down, K_Up), (K_None, K_None));
  DefaultInputs_Rotate: T3BoolKeys =
    ((K_Up, K_Down), (K_Left, K_Right), (K_None, K_None));
  CoordToStr: array [0..2] of string = ('X', 'Y', 'Z');
  IncreaseToStr: array [boolean] of string = ('Dec', 'Inc');
var
  I: Integer;
  B: boolean;
begin
  inherited;

  FModelBox := EmptyBox3D;
  FTranslation := ZeroVector3Single;
  FRotations := QuatIdentityRot;
  FRotationsAnim := ZeroVector3Single;
  FScaleFactor := 1;
  FDragMoveSpeed := 1;
  FKeysMoveSpeed := 1;
  FRotationAccelerate := true;
  FRotationAccelerationSpeed := DefaultRotationAccelerationSpeed;
  FRotationSpeed := DefaultRotationSpeed;

  for I := 0 to 2 do
    for B := false to true do
    begin
      FInputs_Move[I, B] := TInputShortcut.Create(Self);
      FInputs_Move[I, B].Name := 'Input_Move' + CoordToStr[I] + IncreaseToStr[B];
      FInputs_Move[I, B].SetSubComponent(true);
      FInputs_Move[I, B].Assign(DefaultInputs_Move[I, B]);

      FInputs_Rotate[I, B] := TInputShortcut.Create(Self);
      FInputs_Rotate[I, B].Name := 'Input_Rotate' + CoordToStr[I] + IncreaseToStr[B];
      FInputs_Rotate[I, B].SetSubComponent(true);
      FInputs_Rotate[I, B].Assign(DefaultInputs_Rotate[I, B]);
    end;

  { For scale larger/smaller we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  FInput_ScaleLarger  := TInputShortcut.Create(Self);
   Input_ScaleLarger.Name := 'Input_ScaleLarger';
   Input_ScaleLarger.SetSubComponent(true);
   Input_ScaleLarger.Assign(K_Numpad_Plus, K_None, '+');

  FInput_ScaleSmaller := TInputShortcut.Create(Self);
   Input_ScaleSmaller.Name := 'Input_ScaleSmaller';
   Input_ScaleSmaller.SetSubComponent(true);
   Input_ScaleSmaller.Assign(K_Numpad_Minus, K_None, '-');

  FInput_Home := TInputShortcut.Create(Self);
   Input_Home.Name := 'Input_Home';
   Input_Home.SetSubComponent(true);
   Input_Home.Assign(K_None);

  FInput_StopRotating := TInputShortcut.Create(Self);
   Input_StopRotating.Name := 'Input_StopRotating';
   Input_StopRotating.SetSubComponent(true);
   Input_StopRotating.Assign(K_Space, K_None, #0, true, mbLeft);
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
  Result := TranslationMatrix(VectorAdd(Translation, FCenterOfRotation));
  Result := MatrixMult(Result, Rotations.ToRotationMatrix);
  Result := MatrixMult(Result, ScalingMatrix(Vector3Single(ScaleFactor, ScaleFactor, ScaleFactor)));
  Result := MatrixMult(Result, TranslationMatrix(VectorNegate(FCenterOfRotation)));
end;

function TExamineCamera.MatrixInverse: TMatrix4Single;
begin
  { This inverse always exists, assuming ScaleFactor is <> 0. }

  Result := TranslationMatrix(VectorNegate(VectorAdd(Translation, FCenterOfRotation)));
  Result := MatrixMult(Rotations.Conjugate.ToRotationMatrix, Result);
  Result := MatrixMult(ScalingMatrix(Vector3Single(1/ScaleFactor, 1/ScaleFactor, 1/ScaleFactor)), Result);
  Result := MatrixMult(TranslationMatrix(FCenterOfRotation), Result);
end;

function TExamineCamera.RotationMatrix: TMatrix4Single;
begin
  Result := Rotations.ToRotationMatrix;
end;

procedure TExamineCamera.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  { Increase speed of rotating, or just rotation angle
    (depending on RotationAccelerate). Direction must be -1 or +1. }
  procedure RotateSpeedOrAngle(const Coord: Integer; const Direction: Integer);
  const
    MaxRotationSpeed = 6.0; { this prevents rotations getting too wild speed }
  begin
    if RotationAccelerate then
      FRotationsAnim[coord] :=
        Clamped(FRotationsAnim[coord] +
          RotationAccelerationSpeed * SecondsPassed * Direction,
          -MaxRotationSpeed, MaxRotationSpeed) else
      FRotations := QuatFromAxisAngle(UnitVector3Single[Coord],
        RotationSpeed * SecondsPassed * Direction) * FRotations;
    ScheduleVisibleChange;
  end;

var
  i: integer;
  MoveChange, ScaleChange: Single;
  ModsDown: TModifierKeys;
  RotChange: Single;
  MoveChangeVector: TVector3Single;
begin
  inherited;

  { Do not handle keys or rotations etc. }
  if Animation then Exit;

  { If given RotationsAnim component is zero, no need to change current Rotations.
    What's more important, this avoids the need to call VisibleChange,
    so things like Invalidate will not be continously called when
    model doesn't rotate.

    We check using exact equality <> 0, this is Ok since the main point is to
    avoid work when StopRotating was called and user didn't touch arrow
    keys (that increase RotationsAnim). Exact equality is Ok check
    to detect this. }

  if not PerfectlyZeroVector(FRotationsAnim) then
  begin
    RotChange := SecondsPassed;

    if FRotationsAnim[0] <> 0 then
      FRotations := QuatFromAxisAngle(UnitVector3Single[0],
        FRotationsAnim[0] * RotChange) * FRotations;

    if FRotationsAnim[1] <> 0 then
    begin
      if Turntable then
        FRotations := FRotations * QuatFromAxisAngle(UnitVector3Single[1],
          FRotationsAnim[1] * RotChange) else
        FRotations := QuatFromAxisAngle(UnitVector3Single[1],
          FRotationsAnim[1] * RotChange) * FRotations;
    end;

    if FRotationsAnim[2] <> 0 then
      FRotations := QuatFromAxisAngle(UnitVector3Single[2],
        FRotationsAnim[2] * RotChange) * FRotations;

    FRotations.LazyNormalize;

    ScheduleVisibleChange;
  end;

  if HandleInput and (ciNormal in Input) then
  begin
    if ModelBox.IsEmptyOrZero then
      MoveChange := KeysMoveSpeed * SecondsPassed else
      MoveChange := KeysMoveSpeed * ModelBox.AverageSize * SecondsPassed;

    { we will apply SecondsPassed to ScaleChange later }
    ScaleChange := 1.5;

    ModsDown := ModifiersDown(Container.Pressed);

    if ModsDown = [mkCtrl] then
    begin
      for i := 0 to 2 do
      begin
        if Inputs_Move[i, true ].IsPressed(Container) then
        begin
          MoveChangeVector := ZeroVector3Single;
          MoveChangeVector[I] := MoveChange;
          Translation := Translation + MoveChangeVector;

          HandleInput := not ExclusiveEvents;
        end;
        if Inputs_Move[i, false].IsPressed(Container) then
        begin
          MoveChangeVector := ZeroVector3Single;
          MoveChangeVector[I] := -MoveChange;
          Translation := Translation + MoveChangeVector;

          HandleInput := not ExclusiveEvents;
        end;
      end;
    end else
    if ModsDown = [] then
    begin
      for i := 0 to 2 do
      begin
        if Inputs_Rotate[i, true ].IsPressed(Container) then
        begin
          RotateSpeedOrAngle(i, +1);
          HandleInput := not ExclusiveEvents;
        end;
        if Inputs_Rotate[i, false].IsPressed(Container) then
        begin
          RotateSpeedOrAngle(i, -1);
          HandleInput := not ExclusiveEvents;
        end;
      end;
    end;

    if Input_ScaleLarger.IsPressed(Container) then
    begin
      ScaleFactor := ScaleFactor * Power(ScaleChange, SecondsPassed);
      HandleInput := not ExclusiveEvents;
    end;
    if Input_ScaleSmaller.IsPressed(Container) then
    begin
      ScaleFactor := ScaleFactor * Power(1 / ScaleChange, SecondsPassed);
      HandleInput := not ExclusiveEvents;
    end;
  end;
end;

function TExamineCamera.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

procedure TExamineCamera.SetRotationAccelerate(const Value: boolean);
begin
  if FRotationAccelerate <> Value then
  begin
    FRotationAccelerate := Value;
    FRotationsAnim := ZeroVector3Single;
  end;
end;

function TExamineCamera.StopRotating: boolean;
begin
  Result := not PerfectlyZeroVector(FRotationsAnim);
  if Result then
  begin
    FRotationsAnim := ZeroVector3Single;
    ScheduleVisibleChange;
  end;
end;

procedure TExamineCamera.Scale(const ScaleBy: Single);
begin FScaleFactor *= ScaleBy; ScheduleVisibleChange; end;

procedure TExamineCamera.Move(coord: integer; const MoveDistance: Single);
begin FTranslation[coord] += MoveDistance; ScheduleVisibleChange; end;

function TExamineCamera.SensorTranslation(const X, Y, Z, Length: Double;
  const SecondsPassed: Single): boolean;
var
  Size: Single;
  Moved: boolean;
  MoveSize: Double;
begin
  if not (ci3dMouse in Input) then Exit(false);
  if FModelBox.IsEmptyOrZero then Exit(false);
  Result := true;

  Moved := false;
  Size := FModelBox.AverageSize;
  MoveSize := Length * SecondsPassed / 5000;

  if Abs(X)>5 then   { left / right }
  begin
    FTranslation[0] += Size * X * MoveSize;
    Moved := true;
  end;

  if Abs(Y)>5 then   { up / down }
  begin
    FTranslation[1] += Size * Y * MoveSize;
    Moved := true;
  end;

  if Moved then
    ScheduleVisibleChange;

  if Abs(Z)>5 then   { backward / forward }
    Zoom(Z * MoveSize / 2);
end;

function TExamineCamera.SensorRotation(const X, Y, Z, Angle: Double;
  const SecondsPassed: Single): boolean;
var
  NewRotation: TQuaternion;
  Moved: boolean;
  RotationSize: Double;
begin
  if not (ci3dMouse in Input) then Exit(false);
  Result := true;

  Moved := false;
  RotationSize := SecondsPassed * Angle / 50;
  NewRotation := FRotations;

  if Abs(X) > 0.4 then      { tilt forward / backward}
  begin
    NewRotation := QuatFromAxisAngle(Vector3Single(1, 0, 0), X * RotationSize) * NewRotation;
    Moved := true;
  end;

  if Abs(Y) > 0.4 then      { rotate }
  begin
    if Turntable then
      NewRotation := NewRotation *
        QuatFromAxisAngle(Vector3Single(0, 1, 0), Y * RotationSize) else
      NewRotation := QuatFromAxisAngle(Vector3Single(0, 1, 0), Y * RotationSize) *
        NewRotation;
    Moved := true;
  end;

  if (Abs(Z) > 0.4) and (not Turntable) then      { tilt sidewards }
  begin
    NewRotation := QuatFromAxisAngle(Vector3Single(0, 0, 1), Z * RotationSize) * NewRotation;
    Moved := true;
  end;

  if Moved then
  begin
    FRotations := NewRotation;
    ScheduleVisibleChange;
  end;
end;

procedure TExamineCamera.Init(const AModelBox: TBox3D; const ARadius: Single);
var
  Pos, Dir, Up, GravityUp: TVector3Single;
begin
  ModelBox := AModelBox;
  Radius := ARadius;

  CameraViewpointForWholeScene(ModelBox, 2, 1, false, true,
    Pos, Dir, Up, GravityUp);
  SetInitialView(Pos, Dir, Up, false);
  GoToInitial;
end;

{ TExamineCamera.Set* properties }

procedure TExamineCamera.SetRotationsAnim(const Value: TVector3Single);
begin FRotationsAnim := Value; ScheduleVisibleChange; end;

procedure TExamineCamera.SetRotations(const Value: TQuaternion);
begin FRotations := Value; ScheduleVisibleChange; end;

procedure TExamineCamera.SetScaleFactor(const Value: Single);
begin FScaleFactor := Value; ScheduleVisibleChange; end;

procedure TExamineCamera.SetTranslation(const Value: TVector3Single);
begin FTranslation := Value; ScheduleVisibleChange; end;

procedure TExamineCamera.SetCenterOfRotation(const Value: TVector3Single);
begin FCenterOfRotation := Value; ScheduleVisibleChange; end;

procedure TExamineCamera.SetModelBox(const Value: TBox3D);
begin
  FModelBox := Value;
  if FModelBox.IsEmpty then
    FCenterOfRotation := Vector3Single(0, 0, 0) { any dummy value } else
    FCenterOfRotation := FModelBox.Center;
  ScheduleVisibleChange;
end;

function TExamineCamera.Press(const Event: TInputPressRelease): boolean;
var
  ZoomScale: Single;
begin
  Result := inherited;
  if Result or
     (not (ciNormal in Input)) or
     Animation or
     (ModifiersDown(Container.Pressed) <> []) then
    Exit;

  if Event.EventType <> itMouseWheel then
  begin
    if Input_StopRotating.IsEvent(Event) then
    begin
      { If StopRotating was useless, do not mark the event as "handled".
        This is nice, otherwise on an empty TCastleControl/Window mouse clicks
        are "mysteriously" intercepted, since the default scene manager creates
        examine camera, and it captures left mouse click as Input_StopRotating. }
      if StopRotating then
        Result := ExclusiveEvents;
    end else
    if Input_Home.IsEvent(Event) then
    begin
      GoToInitial;
      Result := ExclusiveEvents;
    end else
      Result := false;
  end else
  begin
    { For now, doing Zoom on mouse wheel is hardcoded, we don't call EventDown here }

    if Turntable then
      ZoomScale := 40 else
      ZoomScale := 10;
    if Zoom(Event.MouseWheelScroll / ZoomScale) then
       Result := ExclusiveEvents;
  end;
end;

function TExamineCamera.Zoom(const Factor: Single): boolean;
var
  Size: Single;
  OldTranslation, OldPosition: TVector3Single;
begin
  Result := not FModelBox.IsEmptyOrZero;
  if Result then
  begin
    Size := FModelBox.AverageSize;

    OldTranslation := FTranslation;
    OldPosition := Position;

    FTranslation[2] += Size * Factor;

    { Cancel zoom in, don't allow to go to the other side of the model too far.
      Note that Box3DPointDistance = 0 when you're inside the box,
      so zoomin in/out inside the box is still always allowed.
      See http://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&t=24 }
    if (Factor > 0) and
       (FModelBox.PointDistance(Position) >
        FModelBox.PointDistance(OldPosition)) then
    begin
      FTranslation := OldTranslation;
      Exit(false);
    end;

    VisibleChange
  end;
end;

function TExamineCamera.Motion(const Event: TInputMotion): boolean;
var
  Size: Single;
  ModsDown: TModifierKeys;
  DoZooming, DoMoving: boolean;
  MoveDivConst: Single;

  function DragRotation: TQuaternion;

    { Returns new rotation }
    function XYRotation(const Scale: Single): TQuaternion;
    begin
      if Turntable then
        Result :=
          QuatFromAxisAngle(Vector3Single(1, 0, 0), Scale * (Event.OldPosition[1] - Event.Position[1]) / MoveDivConst) *
          FRotations *
          QuatFromAxisAngle(Vector3Single(0, 1, 0), Scale * (Event.Position[0] - Event.OldPosition[0]) / MoveDivConst) else
        Result :=
          QuatFromAxisAngle(Vector3Single(1, 0, 0), Scale * (Event.OldPosition[1] - Event.Position[1]) / MoveDivConst) *
          QuatFromAxisAngle(Vector3Single(0, 1, 0), Scale * (Event.Position[0] - Event.OldPosition[0]) / MoveDivConst);
    end;

  var
    W2, H2, AvgX, AvgY, ZRotAngle, ZRotRatio: Single;
  begin
    if (not ContainerSizeKnown) or Turntable then
    begin
      Result := XYRotation(1);
    end else
    begin
      { When the cursor is close to the window edge, make rotation around Z axis.
        This is called "virtual trackball" on
        http://audilab.bme.mcgill.ca/~funnell/graphics/graphics3dview.html . }
      { clamp, since mouse positions may be wild }
      AvgX := (Event.Position[0] + Event.OldPosition[0]) / 2;
      AvgY := (Event.Position[1] + Event.OldPosition[1]) / 2;
      { let physical size affect scaling speed }
      W2 := Container.Width  * 96 / (Container.Dpi * 2); // multiply by 96 to keep old constants working
      H2 := Container.Height * 96 / (Container.Dpi * 2);
      { calculate rotation around Z }
      ZRotAngle :=
        ArcTan2((Event.OldPosition[1] - H2) / H2, (Event.OldPosition[0] - W2) / W2) -
        ArcTan2((Event.   Position[1] - H2) / H2, (Event.   Position[0] - W2) / W2);
      { ArcTan2 is in [-pi,pi]. When the mouse passes the border
        of this range, we have to be secure. }
      if ZRotAngle > Pi then
        ZRotAngle := 2 * Pi - ZRotAngle else
      if ZRotAngle < -Pi then
        ZRotAngle := 2 * Pi + ZRotAngle;
      { how much do we want Z rotation, i.e. how far are we from window middle,
        in 0..1 }
      ZRotRatio := Min(1.0, Sqrt(Sqr((AvgX - W2) / W2) + Sqr((AvgY - H2) / H2)));
      Result :=
        QuatFromAxisAngle(Vector3Single(0, 0, -1), ZRotRatio * ZRotAngle) *
        XYRotation(1 - ZRotRatio);
    end;
  end;

begin
  Result := inherited;
  if Result then Exit;

  if Container <> nil then
    MoveDivConst := Container.Dpi else
    MoveDivConst := 100;

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
      mode is the most comfortable), so it should be on mbLeft (like normalmap)
      with no modifiers (like Blender).
    - moving closer/further: 2nd most important action in Examine mode, IMO.
      Goes to mbRight. For people with 1 mouse button, and for Blender analogy,
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

  { When dragging should be ignored, or (it's an optimization to check it
    here early, Motion occurs very often) when nothing pressed, do nothing. }
  if (Container.MousePressed = []) or
     (not ReallyEnableMouseDragging) or
     (MouseDraggingStarted <> Event.FingerIndex) or
     Animation then
    Exit;

  ModsDown := ModifiersDown(Container.Pressed) * [mkShift, mkCtrl];

  { Rotating }
  if (mbLeft in Container.MousePressed) and (ModsDown = []) then
  begin
    if Turntable then
      FRotations := DragRotation {old FRotations already included in XYRotation} else
      FRotations := DragRotation * FRotations;
    ScheduleVisibleChange;
    Result := ExclusiveEvents;
  end;

  { Moving uses box size, so requires non-empty box. }

  { Note: checks for (ModsDown = []) are not really needed below,
    mkRight / Middle don't serve any other purpose anyway.
    But I think that it improves user ability to "discover" these shortcuts
    and keys, otherwise it seems strange that shift/ctrl change the
    meaning of mbLeft but they don't change the meaning of mbRight / Middle ? }

  { Moving closer/further }
  if Turntable then
    DoZooming := (mbMiddle in Container.MousePressed) else
    DoZooming := ( (mbRight in Container.MousePressed) and (ModsDown = []) ) or
                 ( (mbLeft in Container.MousePressed) and (ModsDown = [mkCtrl]) );
  if DoZooming then
  begin
    if Zoom((Event.OldPosition[1] - Event.Position[1]) / (2*MoveDivConst)) then
      Result := ExclusiveEvents;
  end;

  { Moving left/right/down/up }
  if Turntable then
    DoMoving := (not FModelBox.IsEmpty) and (mbRight in Container.MousePressed)
  else
    DoMoving := (not FModelBox.IsEmpty) and
               ( ( (mbMiddle in Container.MousePressed) and (ModsDown = []) ) or
                 ( (mbLeft in Container.MousePressed) and (ModsDown = [mkShift]) ) );
  if DoMoving then
  begin
    Size := FModelBox.AverageSize;
    FTranslation[0] -= DragMoveSpeed * Size * (Event.OldPosition[0] - Event.Position[0]) / (2*MoveDivConst);
    FTranslation[1] -= DragMoveSpeed * Size * (Event.OldPosition[1] - Event.Position[1]) / (2*MoveDivConst);
    ScheduleVisibleChange;
    Result := ExclusiveEvents;
  end;
end;

procedure TExamineCamera.GetView(out APos, ADir, AUp: TVector3Single);
begin
  APos := FPosition;
  ADir := FDirection;
  AUp  := FUp;
end;

procedure TExamineCamera.VisibleChange;
var
  M: TMatrix4Single;
begin
  { calculate our pos/dir/up vectors here.
    This allows our GetView to work immediately fast, at the expense of doing
    the below calculations always. In practice, this is good,
    as e.g. TCastleSceneManager.CameraVisibleChange calls GetView *always*.
    So assume that GetView is called very often, and make it instant. }
  M := MatrixInverse;

  { These MatrixMultPoint/Direction should never fail with ETransformedResultInvalid.
    That's because M is composed from translations, rotations, scaling,
    which preserve points/directions (4th component in homogeneus coordinates)
    nicely. }
  FPosition  := MatrixMultPoint(M, ZeroVector3Single);
  FDirection := MatrixMultDirection(M, DefaultCameraDirection);
  FUp        := MatrixMultDirection(M, DefaultCameraUp);

  { In case of ScaleFactor, it is possible that M is such that dir/up
    are not normalized. Fix them now, GetView guarantees normalized vectors. }
  if ScaleFactor <> 1 then
  begin
    NormalizeVar(FDirection);
    NormalizeVar(FUp);
  end;

  inherited;
end;

procedure TExamineCamera.GetView(out APos, ADir, AUp, AGravityUp: TVector3Single);
begin
  GetView(APos, ADir, AUp);
  AGravityUp := GetGravityUp;
end;

function TExamineCamera.GetPositionInternal: TVector3Single;
begin
  Result := MatrixMultPoint(MatrixInverse, Vector3Single(0, 0, 0));
end;

procedure TExamineCamera.SetPosition(const Value: TVector3Single);
begin
  { a subset of what SetView does }
  FTranslation := -Value;
  FTranslation := FRotations.Rotate(FTranslation + FCenterOfRotation)
    - FCenterOfRotation;
  ScheduleVisibleChange;
end;

function TExamineCamera.GetGravityUp: TVector3Single;
begin
  Result := DefaultCameraUp; { nothing more sensible for Examine camera }
end;

procedure TExamineCamera.SetView(const APos, ADir, AUp: TVector3Single;
  const AdjustUp: boolean);
var
  Dir, Up: TVector3Single;
begin
  FTranslation := -APos;

  { Make vectors orthogonal, CamDirUp2OrientQuat requires this }
  Dir := ADir;
  Up := AUp;
  if AdjustUp then
    MakeVectorsOrthoOnTheirPlane(Up, Dir) else
    MakeVectorsOrthoOnTheirPlane(Dir, Up);

  FRotations := CamDirUp2OrientQuat(Dir, Up).Conjugate;

{ Testing of "hard case" in CamDirUp2OrientQuat.
  This should always succeed now, many cases tested automatically
  by TTestCastleCameras.TestOrientationFromBasicAxes.

  if not VectorsEqual(QuatRotate(FRotations, Normalized(Dir)), DefaultCameraDirection, 0.01) then
  begin
    Writeln('oh yes, dir wrong: ', VectorToNiceStr(QuatRotate(FRotations, Normalized(Dir))));
    Writeln('  q: ', VectorToNiceStr(FRotations.Vector4));
  end;

  if not VectorsEqual(QuatRotate(FRotations, Normalized(Up)), DefaultCameraUp, 0.01) then
    Writeln('oh yes, up wrong: ', VectorToNiceStr(QuatRotate(FRotations, Normalized(Up))));
}

  { We have to fix our FTranslation, since our TExamineCamera.Matrix
    applies our move *first* before applying rotation
    (and this is good, as it allows rotating around object center,
    not around camera).

    Alternative implementation of this would call QuatToRotationMatrix and
    then simulate multiplying this rotation matrix * translation matrix
    of FTranslation. But we can do this directly.

    We also note at this point that rotation is done around
    (FTranslation + FCenterOfRotation). But FCenterOfRotation is not
    included in Translation. }
  FTranslation := FRotations.Rotate(FTranslation + FCenterOfRotation)
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

procedure TExamineCamera.SetView(const APos, ADir, AUp, AGravityUp: TVector3Single;
  const AdjustUp: boolean);
begin
  SetView(APos, ADir, AUp, AdjustUp);
  { Ignore AGravityUp }
end;

function TExamineCamera.GetInput_MoveXInc: TInputShortcut; begin Result := Inputs_Move[0, true ] end;
function TExamineCamera.GetInput_MoveXDec: TInputShortcut; begin Result := Inputs_Move[0, false] end;
function TExamineCamera.GetInput_MoveYInc: TInputShortcut; begin Result := Inputs_Move[1, true ] end;
function TExamineCamera.GetInput_MoveYDec: TInputShortcut; begin Result := Inputs_Move[1, false] end;
function TExamineCamera.GetInput_MoveZInc: TInputShortcut; begin Result := Inputs_Move[2, true ] end;
function TExamineCamera.GetInput_MoveZDec: TInputShortcut; begin Result := Inputs_Move[2, false] end;
function TExamineCamera.GetInput_RotateXInc: TInputShortcut; begin Result := Inputs_Rotate[0, true ] end;
function TExamineCamera.GetInput_RotateXDec: TInputShortcut; begin Result := Inputs_Rotate[0, false] end;
function TExamineCamera.GetInput_RotateYInc: TInputShortcut; begin Result := Inputs_Rotate[1, true ] end;
function TExamineCamera.GetInput_RotateYDec: TInputShortcut; begin Result := Inputs_Rotate[1, false] end;
function TExamineCamera.GetInput_RotateZInc: TInputShortcut; begin Result := Inputs_Rotate[2, true ] end;
function TExamineCamera.GetInput_RotateZDec: TInputShortcut; begin Result := Inputs_Rotate[2, false] end;

function TExamineCamera.GetMouseNavigation: boolean;
begin
  Result := ciMouseDragging in Input;
end;

procedure TExamineCamera.SetMouseNavigation(const Value: boolean);
begin
  if Value then
    Input := Input + [ciMouseDragging] else
    Input := Input - [ciMouseDragging];
end;

function TExamineCamera.GetNavigationType: TNavigationType;
begin
  if Turntable then
    Result := ntTurntable else
    Result := ntExamine;
end;

{ TWalkCamera ---------------------------------------------------------------- }

constructor TWalkCamera.Create(AOwner: TComponent);
begin
  inherited;
  FPosition  := InitialPosition;
  FDirection := InitialDirection;
  FUp        := InitialUp;
  FGravityUp := DefaultCameraUp;

  FMoveHorizontalSpeed := 1;
  FMoveVerticalSpeed := 1;
  FMoveSpeed := 1;
  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FRotationVerticalSpeed := DefaultRotationVerticalSpeed;
  FFallSpeedStart := DefaultFallSpeedStart;
  FFallSpeedIncrease := DefaultFallSpeedIncrease;
  FPreferGravityUpForRotations := true;
  FPreferGravityUpForMoving := true;
  FGravity := false;
  FGrowSpeed := DefaultGrowSpeed;
  FFallingEffect := true;
  FIsJumping := false;
  FHeadBobbing := DefaultHeadBobbing;
  FCrouchHeight := DefaultCrouchHeight;
  FJumpMaxHeight := DefaultJumpMaxHeight;
  FMinAngleRadFromGravityUp := DefaultMinAngleRadFromGravityUp;
  FAllowSlowerRotations := true;
  FCheckModsDown := true;
  FMouseLookHorizontalSensitivity := DefaultMouseLookHorizontalSensitivity;
  FMouseLookVerticalSensitivity := DefaultMouseLookVerticalSensitivity;
  FHeadBobbingTime := DefaultHeadBobbingTime;
  FJumpHorizontalSpeedMultiply := DefaultJumpHorizontalSpeedMultiply;
  FJumpTime := DefaultJumpTime;
  FInvertVerticalMouseLook := false;
  FMouseDraggingHorizontalRotationSpeed := DefaultMouseDraggingHorizontalRotationSpeed;
  FMouseDraggingVerticalRotationSpeed := DefaultMouseDraggingVerticalRotationSpeed;

  FInput_Forward                 := TInputShortcut.Create(Self);
  FInput_Backward                := TInputShortcut.Create(Self);
  FInput_LeftRot                 := TInputShortcut.Create(Self);
  FInput_RightRot                := TInputShortcut.Create(Self);
  FInput_LeftStrafe              := TInputShortcut.Create(Self);
  FInput_RightStrafe             := TInputShortcut.Create(Self);
  FInput_UpRotate                := TInputShortcut.Create(Self);
  FInput_DownRotate              := TInputShortcut.Create(Self);
  FInput_IncreasePreferredHeight := TInputShortcut.Create(Self);
  FInput_DecreasePreferredHeight := TInputShortcut.Create(Self);
  FInput_GravityUp               := TInputShortcut.Create(Self);
  FInput_MoveSpeedInc            := TInputShortcut.Create(Self);
  FInput_MoveSpeedDec            := TInputShortcut.Create(Self);
  FInput_Jump                    := TInputShortcut.Create(Self);
  FInput_Crouch                  := TInputShortcut.Create(Self);
  FInput_Run                     := TInputShortcut.Create(Self);

  Input_Forward                 .Assign(K_W, K_Up);
  Input_Backward                .Assign(K_S, K_Down);
  Input_LeftRot                 .Assign(K_Left);
  Input_RightRot                .Assign(K_Right);
  Input_LeftStrafe              .Assign(K_A);
  Input_RightStrafe             .Assign(K_D);
  Input_UpRotate                .Assign(K_None);
  Input_DownRotate              .Assign(K_None);
  Input_IncreasePreferredHeight .Assign(K_Insert);
  Input_DecreasePreferredHeight .Assign(K_Delete);
  Input_GravityUp               .Assign(K_None);
  { For move speed we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  Input_MoveSpeedInc            .Assign(K_Numpad_Plus , K_None, '+');
  Input_MoveSpeedDec            .Assign(K_Numpad_Minus, K_None, '-');
  Input_Jump                    .Assign(K_Space);
  Input_Crouch                  .Assign(K_C);
  Input_Run                     .Assign(K_Shift);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_LeftRot                .SetSubComponent(true);
  Input_RightRot               .SetSubComponent(true);
  Input_LeftStrafe             .SetSubComponent(true);
  Input_RightStrafe            .SetSubComponent(true);
  Input_UpRotate               .SetSubComponent(true);
  Input_DownRotate             .SetSubComponent(true);
  Input_IncreasePreferredHeight.SetSubComponent(true);
  Input_DecreasePreferredHeight.SetSubComponent(true);
  Input_GravityUp              .SetSubComponent(true);
  Input_MoveSpeedInc           .SetSubComponent(true);
  Input_MoveSpeedDec           .SetSubComponent(true);
  Input_Jump                   .SetSubComponent(true);
  Input_Crouch                 .SetSubComponent(true);
  Input_Run                    .SetSubComponent(true);

  Input_Forward                .Name := 'Input_Forward';
  Input_Backward               .Name := 'Input_Backward';
  Input_LeftRot                .Name := 'Input_LeftRot';
  Input_RightRot               .Name := 'Input_RightRot';
  Input_LeftStrafe             .Name := 'Input_LeftStrafe';
  Input_RightStrafe            .Name := 'Input_RightStrafe';
  Input_UpRotate               .Name := 'Input_UpRotate';
  Input_DownRotate             .Name := 'Input_DownRotate';
  Input_IncreasePreferredHeight.Name := 'Input_IncreasePreferredHeight';
  Input_DecreasePreferredHeight.Name := 'Input_DecreasePreferredHeight';
  Input_GravityUp              .Name := 'Input_GravityUp';
  Input_MoveSpeedInc           .Name := 'Input_MoveSpeedInc';
  Input_MoveSpeedDec           .Name := 'Input_MoveSpeedDec';
  Input_Jump                   .Name := 'Input_Jump';
  Input_Crouch                 .Name := 'Input_Crouch';
  Input_Run                    .Name := 'Input_Run';
end;

destructor TWalkCamera.Destroy;
begin
  inherited;
end;

function TWalkCamera.Matrix: TMatrix4Single;
begin
  { Yes, below we compare Fde_UpRotate with 0.0 using normal
    (precise) <> operator. Don't worry --- Fde_Stabilize in Update
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

procedure TWalkCamera.Height(const APosition: TVector3Single;
  out AIsAbove: boolean;
  out AnAboveHeight: Single; out AnAboveGround: P3DTriangle);
begin
  if Assigned(OnHeight) then
    AIsAbove := OnHeight(Self, APosition, AnAboveHeight, AnAboveGround) else
  begin
    AIsAbove := false;
    AnAboveHeight := MaxSingle;
    AnAboveGround := nil;
  end;
end;

function TWalkCamera.UseHeadBobbing: boolean;
begin
  Result := Gravity and (HeadBobbing <> 0.0);
end;

function TWalkCamera.RealPreferredHeightNoHeadBobbing: Single;
begin
  Result := PreferredHeight;

  if IsCrouching then
    Result *= CrouchHeight;
end;

function TWalkCamera.RealPreferredHeight: Single;
var
  BobbingModifier: Single;
begin
  Result := RealPreferredHeightNoHeadBobbing;

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

function TWalkCamera.RealPreferredHeightMargin: Single;
begin
  { I tried using here something smaller like
    SingleEqualityEpsilon, but this was not good. }
  Result := RealPreferredHeight * 0.01;
end;

procedure TWalkCamera.AdjustForRotationHorizontalPivot(const OldDirection: TVector3Single);
var
  Pivot, OldDirectionInGravityPlane: TVector3Single;
begin
  if RotationHorizontalPivot <> 0 then
  begin
    if PreferGravityUpForRotations then
    begin
      Pivot := Position  + OldDirection * RotationHorizontalPivot;
      FPosition := Pivot -    Direction * RotationHorizontalPivot;
    end else
    begin
      OldDirectionInGravityPlane := Direction;
      if not VectorsParallel(OldDirectionInGravityPlane, GravityUp) then
        MakeVectorsOrthoOnTheirPlane(OldDirectionInGravityPlane, GravityUp);
      Pivot := Position  + OldDirectionInGravityPlane * RotationHorizontalPivot;
      FPosition := Pivot -    DirectionInGravityPlane * RotationHorizontalPivot;
    end;
  end;
end;

procedure TWalkCamera.RotateAroundGravityUp(const AngleDeg: Single);
var
  Axis, OldDirection: TVector3Single;
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
  OldDirection := Direction;
  FDirection := RotatePointAroundAxisDeg(AngleDeg, Direction, Axis);
  AdjustForRotationHorizontalPivot(OldDirection);

  ScheduleVisibleChange;
end;

procedure TWalkCamera.RotateAroundUp(const AngleDeg: Single);
var
  OldDirection: TVector3Single;
begin
  { We know that RotatePointAroundAxisDeg below doesn't change the length
    of the Direction (so it will remain normalized) and it will keep
    Direction and Up vectors orthogonal. }
  OldDirection := Direction;
  FDirection := RotatePointAroundAxisDeg(AngleDeg, FDirection, FUp);
  AdjustForRotationHorizontalPivot(OldDirection);

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

function TWalkCamera.MoveTo(const ProposedNewPos: TVector3Single;
  const BecauseOfGravity, CheckClimbHeight: boolean): boolean;
var
  NewPos: TVector3Single;
  NewIsAbove: boolean;
  NewAboveHeight, OldAbsoluteHeight, NewAbsoluteHeight: Single;
  NewAboveGround: P3DTriangle;
begin
  Result := DoMoveAllowed(ProposedNewPos, NewPos, BecauseOfGravity);

  if Result and Gravity and CheckClimbHeight and (ClimbHeight <> 0) and IsAbove and
    { if we're already below ClimbHeight then do not check if new position
      satisfies ClimbHeight requirement. This may prevent camera blocking
      in weird situations, e.g. if were forcefully pushed into some position
      (e.g. because player is hit by a missile with a knockback, or teleported
      or such). }
    (AboveHeight > ClimbHeight) then
  begin
    Height(NewPos, NewIsAbove, NewAboveHeight, NewAboveGround);
    if NewIsAbove then
    begin
      OldAbsoluteHeight := VectorDotProduct(GravityUp, Position);
      NewAbsoluteHeight := VectorDotProduct(GravityUp, NewPos);
      Result := not (
        AboveHeight - NewAboveHeight - (OldAbsoluteHeight - NewAbsoluteHeight) >
        ClimbHeight );
      if Log and not Result then
        WritelnLog('Camera', 'Blocked move because of ClimbHeight.');
    end;
  end;

  if Result then
    { Note that setting Position automatically calls ScheduleVisibleChange }
    Position := NewPos;
end;

function TWalkCamera.Move(const MoveVector: TVector3Single;
  const BecauseOfGravity, CheckClimbHeight: boolean): boolean;
begin
  Result := MoveTo(VectorAdd(Position, MoveVector), BecauseOfGravity, CheckClimbHeight);
end;

procedure TWalkCamera.MoveHorizontal(const SecondsPassed: Single; const Multiply: Integer = 1);
var
  Dir: TVector3Single;
  Multiplier: Single;
begin
  Multiplier := MoveSpeed * MoveHorizontalSpeed * SecondsPassed * Multiply;
  if IsJumping then
    Multiplier *= JumpHorizontalSpeedMultiply;
  if Input_Run.IsPressed(Container) then
    Multiplier *= 2;

  { Update HeadBobbingPosition }
  if (not IsJumping) and UseHeadBobbing and (not HeadBobbingAlreadyDone) then
  begin
    HeadBobbingPosition += SecondsPassed / HeadBobbingTime;
    HeadBobbingAlreadyDone := true;
  end;

  MoveHorizontalDone := true;

  if PreferGravityUpForMoving then
    Dir := DirectionInGravityPlane else
    Dir := Direction;

  Move(Dir * Multiplier, false, true);
end;

procedure TWalkCamera.MoveVertical(const SecondsPassed: Single; const Multiply: Integer);

  { Provided PreferredUpVector must be already normalized. }
  procedure MoveVerticalCore(const PreferredUpVector: TVector3Single);
  var
    Multiplier: Single;
  begin
    Multiplier := MoveSpeed * MoveVerticalSpeed * SecondsPassed * Multiply;
    if Input_Run.IsPressed(Container) then
      Multiplier *= 2;
    Move(PreferredUpVector * Multiplier, false, false);
  end;

begin
  if not Gravity then
  begin
    if PreferGravityUpForMoving then
      MoveVerticalCore(GravityUp) else
      MoveVerticalCore(Up);
  end;
end;

procedure TWalkCamera.RotateHorizontalForStrafeMove(const AngleDeg: Single);
begin
  if PreferGravityUpForMoving then
    RotateAroundGravityUp(AngleDeg) else
    RotateAroundUp(AngleDeg);
end;

function TWalkCamera.ReallyEnableMouseDragging: boolean;
begin
  Result := (inherited ReallyEnableMouseDragging) and not MouseLook;
end;

procedure TWalkCamera.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  { Check are keys for left/right/down/up rotations are pressed, and handle them.
    SpeedScale = 1 indicates a normal rotation speed, you can use it to scale
    the rotation speed to specific purposes. }
  procedure CheckRotates(SpeedScale: Single);
  begin
    if Input_RightRot.IsPressed(Container) then
      RotateHorizontal(-RotationHorizontalSpeed * SecondsPassed * SpeedScale);
    if Input_LeftRot.IsPressed(Container) then
      RotateHorizontal(+RotationHorizontalSpeed * SecondsPassed * SpeedScale);
    if Input_UpRotate.IsPressed(Container) then
      RotateVertical(+RotationVerticalSpeed * SecondsPassed * SpeedScale);
    if Input_DownRotate.IsPressed(Container) then
      RotateVertical(-RotationVerticalSpeed * SecondsPassed * SpeedScale);
  end;

  { Things related to gravity --- jumping, taking into account
    falling down and keeping RealPreferredHeight above the ground. }
  procedure GravityUpdate;

    function TryJump: boolean;
    var
      ThisJumpHeight: Single;
    begin
      Result := IsJumping;

      if Result then
      begin
        { jump. This means:
          1. update FJumpHeight and move Position
          2. or set FIsJumping to false when jump ends }
        ThisJumpHeight := MaxJumpDistance * SecondsPassed / FJumpTime;
        FJumpHeight += ThisJumpHeight;

        if FJumpHeight > MaxJumpDistance then
          FIsJumping := false else
          { do jumping }
          Move(GravityUp * ThisJumpHeight, false, false);
      end;
    end;

   function TryFde_Stabilize: boolean; forward;

    { If our height above the ground is < RealPreferredHeight
      then we try to "grow".

      (this may happen because of many things --- e.g. user code
      just changed PreferredHeight to something larger
      (because e.g. "duck mode" ended), or we just ended falling dowm
      from high). }
    function TryGrow: boolean;
    var
      GrowingVectorLength: Single;
    begin
      Result := AboveHeight < RealPreferredHeight - RealPreferredHeightMargin;

      if Result then
      begin
        { calculate GrowingVectorLength }
        GrowingVectorLength := Min(
          MoveSpeed * MoveVerticalSpeed * GrowSpeed * SecondsPassed,
          RealPreferredHeight - AboveHeight);

        Move(VectorScale(GravityUp, GrowingVectorLength), true, false);

        { When growing, TryFde_Stabilize also must be done.
          Otherwise when player walks horizontally on the flat surface
          for some time then "Falling down effect" activates --- because
          player is always in TryGrow or TryFalling. So one of them
          (TryGrow or TryFalling) *must* allow "Falling down effect"
          to stabilize itself. Obviously TryFalling can't (this would
          be against the idea of this effect) so TryGrow does it... }
        TryFde_Stabilize;
      end;
    end;

    function TryFalling: boolean;
    const
      Fde_VerticalRotateDeviation = 50.0;
      Fde_HorizontalRotateDeviation = 15.0;
    var
      PositionBefore: TVector3Single;
      FallingVectorLength: Single;
    begin
      Result := false;

      { Note that if we got here, then TryGrow returned false,
        which means that (assuming OnHeight is correctly assigned)
        we are not above the ground, or
          AboveHeight >=
            RealPreferredHeight - RealPreferredHeightMargin
        However we require something stronger to continue:
          AboveHeight >
            RealPreferredHeight + RealPreferredHeightMargin

        This is important, because this way we avoid the unpleasant
        "bouncing" effect when in one Update we decide that camera
        is falling down, in next Update we decide that it's growing,
        in next Update it falls down again etc. In TryGrow we try
        to precisely set our Position, so that it hits exactly
        at RealPreferredHeight -- which means that after TryGrow,
        in next Update TryGrow should not cause growing and TryFalling
        should not cause falling down. }
      if AboveHeight <=
           RealPreferredHeight + RealPreferredHeightMargin then
      begin
        FFalling := false;
        Exit;
      end;

      { Make sure that FallSpeed is initialized.
        When Falling, we know it's initialized (because setting
        "FFalling := true;" is done only in the piece of code below...),
        otherwise we make sure it's set to it's starting value. }
      if not FFalling then
        FFallSpeed := FallSpeedStart;

      { try to fall down }
      PositionBefore := Position;

      { calculate FallingVectorLength.

        Note that we make sure that FallingVectorLength is no longer
        than AboveHeight --- this way we avoid the problem
        that when FFallSpeed would get very big,
        we couldn't fall down any more (while in fact we should then fall down
        very quickly).

        Actually, we even do more. We make sure that
        FallingVectorLength is no longer than
        (AboveHeight - RealPreferredHeight).
        Initially I wanted to do here
          MinVar(FallingVectorLength, AboveHeight);
        i.e. to allow camera to fall below RealPreferredHeight.

        But this didn't work like it should. Why ?
        See above for the trick that I have to do with
        RealPreferredHeightMargin above (to not cause
        "unpleasant bouncing" when swapping Falling and TryGrow).
        If I could fall down here below RealPreferredHeight then

        1. It *will not* cause the desired "nice" effect (of automatically
           "ducking" when falling down from high), because of comparison
           (the one with RealPreferredHeightMargin) above.

        2. It *will* cause the undesired unpleasant swapping between
           Falling and TryGrow.

        So it's totally bad thing to do.

        This means that I should limit myself to not fall down
        below RealPreferredHeight. And that's what I'm doing. }
      FallingVectorLength :=
        MoveSpeed * MoveVerticalSpeed * FFallSpeed * SecondsPassed;
      MinVar(FallingVectorLength, AboveHeight - RealPreferredHeight);

      if Move(VectorScale(GravityUp, - FallingVectorLength), true, false) and
        (not VectorsPerfectlyEqual(Position, PositionBefore)) then
      begin
        if not Falling then
        begin
          FFallingStartPosition := PositionBefore;

          { Why do I init here FFallSpeed ? A few lines above I did
              if not FFalling then
                FFallSpeed := FallSpeedStart;
            to init FFallSpeed (I had to do it to calculate
            FallingVectorLength). So why initing it again here ?

            Answer: Because Move above called MoveTo, that set Position
            that actually called ScheduleVisibleChange that possibly
            called OnVisibleChange.
            And OnVisibleChange is used callback and user could do there
            things like
            - Changing FallSpeedStart (but still it's unspecified
              whether we have to apply this change, right ?)
            - Calling CancelFalling and *then* changing FallSpeedStart.
              And in this case, we *must* honour it, because here user
              expects that we will use FallSpeedStart if we want
              to fall down. (of course, one call to "Move" with old
              "FallSpeedStart" was already done, that's unavoidable...). }
          FFallSpeed := FallSpeedStart;

          FFalling := true;
        end;

        Result := true;

        if AboveHeight < RealPreferredHeight * 1.1 then
        begin
          { This check is needed, otherwise when you're walking down even from
            the most slight hill then you get

            1. FallingEffect
            2. OnFall is called seldom and with large heights.

            Why ? Because MoveHorizontal calls are done between GravityUpdate
            calls, and the move can be quite fast. So even though the player is
            actually quite closely following the terrain, we would constantly
            have Falling := true. Consider a large hill that is almost
            flat --- when walking down the hill, we would get Falling
            := true, FallSpeed and FallingEffect would raise,
            and at the end OnFall would be called with parameters
            like player fell down from the top of the hill to the ground
            (which can cause e.g. player losing life).

            The check for RealPreferredHeight * 1.1 above and
            setting FFalling cure the situation. OnFall will
            be called more often indicating very small fallen down heights,
            and FallSpeed and FallingEffect will not be able
            to raise high as long as player follows terrain closely.

            Of course we're setting here FFalling := false even though
            the player is not exactly on the terrain --- but he's very close.
            In the next GravityUpdate call we will again bring him a little
            down, set FFalling to @true, and then set it back to @false
            by line below. }
          FFalling := false;
        end else
        begin
          { This is where we do FallingEffect.

            Note that I do FallingEffect *before* increasing
            FFallSpeed below.

            1. reason (ideological, not really that important...) is that
               FallingEffect is a penalty equivalent to FFallSpeed that
               was already used --- not to the future FFallSpeed.

            2. reason (practical, and real :) is that when the program
               was in some non-3d drawing state (e.g. displaying menu, or
               displaying progress bar because the VRML model was just loaded)
               then SecondsPassed indicates (truly) that a lot of time elapsed
               since last Update. This means that it's common that at the same moment
               when Falling changed suddenly to @true, SecondsPassed may be large
               and we're better not using this too much... A practical bug demo:
               open in view3dscene (it does progress bar in OpenGL, so will cause
               large SecondsPassed) any model with gravity on and camera slightly
               higher then PreferredHeight (we want to trigger Falling
               right when the model is loaded). E.g. run
               "view3dscene demo_models/navigation/speed_2.wrl".
               If FallSpeedIncrease will be done before FallingEffect,
               then you'll see that at the very first frame FFallSpeed
               was increased so much (because SecondsPassed was large) that it triggered
               FallingEffect. Even though the falling down distance was really small...

               Maybe in the future I'll workaround it differently.
               One idea is that FFallSpeed should be made smaller if the
               falled down distance is small. Or just don't call GravityUpdate after the first
               model load, to avoid using large SecondsPassed ?

               LATER NOTE: note that the (2.) problem above may be non-existing
               now, since we use SecondsPassed and we have ZeroNextSecondsPassed to
               set SecondsPassed to zero in such cases. }
          if FallingEffect and
             (FFallSpeed > FallSpeedStart * 3) then
          begin
            if FFallSpeed > FallSpeedStart * 5 then
            begin
              if Fde_RotateHorizontal = 0 then
                Fde_RotateHorizontal := RandomPlusMinus;
              RotateAroundGravityUp(Fde_RotateHorizontal *
                Fde_HorizontalRotateDeviation * SecondsPassed);
            end;

            if Fde_UpRotate < 0 then
              Fde_UpRotate -= Fde_VerticalRotateDeviation * SecondsPassed else
            if Fde_UpRotate > 0 then
              Fde_UpRotate += Fde_VerticalRotateDeviation * SecondsPassed else
              Fde_UpRotate := RandomPlusMinus *
                              Fde_VerticalRotateDeviation * SecondsPassed;

            ScheduleVisibleChange;
          end;

          { Note that when changing FFallSpeed below I'm using SecondsPassed * 50.
            And also above when using FFallSpeed, I multipled
            FFallSpeed * SecondsPassed * 50. This is correct:
            - changing position based on FallSpeed is a "velocity"
            - changing FallSpeed below is "acceleration"
            And both acceleration and velocity must be time-based. }
          if FallSpeedIncrease <> 1.0 then
            FFallSpeed *= Power(FallSpeedIncrease, SecondsPassed * 50);
        end;
      end else
        FFalling := false;
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
          Fde_VerticalRotateNormalization * SecondsPassed;

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

      AngleRotate := SecondsPassed * 5;
      MinVar(AngleRotate, Abs(Angle - HalfPi));
      if not FFallingOnTheGroundAngleIncrease then
        AngleRotate := -AngleRotate;

      Up := RotatePointAroundAxisRad(AngleRotate, Up, DirectionInGravityPlane);
    end;

    procedure DoFall;
    var
      BeginPos, EndPos, FallVector: TVector3Single;
    begin
      if Assigned(OnFall) then
      begin
        { Project Position and FFallingStartPosition
          onto GravityUp vector to calculate fall height. }
        BeginPos := PointOnLineClosestToPoint(ZeroVector3Single, GravityUp, FFallingStartPosition);
        EndPos   := PointOnLineClosestToPoint(ZeroVector3Single, GravityUp, Position);
        FallVector := BeginPos - EndPos;

        { Because of various growing and jumping effects (imagine you jump up
          onto a taller pillar) it may turn out that we're higher at the end
          at the end of fall. Do not report it to OnFall event in this case. }
        if VectorDotProduct(GravityUp, Normalized(FallVector)) <= 0 then
          Exit;

        OnFall(Self, VectorLen(FallVector));
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
          this Update call, and no gravity effect is in work
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
            HeadBobbingPosition += Min(HeadBobbingGoingDownSpeed * SecondsPassed,
              1 - FracHeadBobbingPosition);
        end else
        begin
          if FracHeadBobbingPosition > SingleEqualityEpsilon then
            HeadBobbingPosition -= Min(HeadBobbingGoingDownSpeed * SecondsPassed,
              FracHeadBobbingPosition);
        end;
      end;
    end;

    function GetIsOnTheGround: boolean;
    var
      MinAboveHeight, MaxAboveHeight, H: Single;
    begin
      H := RealPreferredHeightNoHeadBobbing;
      MinAboveHeight := (H - H * HeadBobbing) * 0.99;
      MaxAboveHeight := (H + H * HeadBobbing) * 1.01;
      Result := IsAbove and
        (MinAboveHeight <= AboveHeight) and
        (AboveHeight <= MaxAboveHeight);
    end;

  var
    OldFalling: boolean;
  begin
    OldFalling := Falling;

    if Gravity then
    begin
      { update IsAbove, AboveHeight }
      Height(Position, FIsAbove, FAboveHeight, FAboveGround);

      FIsOnTheGround := GetIsOnTheGround;
      FIsWalkingOnTheGround := MoveHorizontalDone and FIsOnTheGround;

      if not TryJump then
        if not TryGrow then
          if not TryFalling then
            if not TryFde_Stabilize then
              { Note that we don't do FallingOnTheGround effect until all
                other effects (jumping, growing, falling on the ground
                and stabilizing after falling on the ground) will finish
                their work. }
              if not TryFallingOnTheGround then
                HeadBobbingGoesDown;
    end else
    begin
      FFalling := false;
      TryFde_Stabilize;
    end;

    if OldFalling and (not Falling) then
      DoFall;
  end;

  procedure PreferGravityUpForRotationsUpdate;
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
          VectorNegateVar(TargetUp);

        AngleRadBetweenTarget := AngleRadBetweenVectors(TargetUp, FUp);
        AngleRadBetweenTargetChange := 0.5 * SecondsPassed;
        if AngleRadBetweenTarget > AngleRadBetweenTargetChange then
        begin
          NewUp := FUp;
          MakeVectorsAngleRadOnTheirPlane(NewUp, TargetUp,
            AngleRadBetweenTarget - AngleRadBetweenTargetChange, NewUp);
          Up := NewUp;
        end else
          Up := TargetUp;
      end;
    end;
    *)
  begin
  end;

  procedure ChangePreferredHeight(const Increase: Integer);
  begin
    PreferredHeight := PreferredHeight +
      { It's best to scale PreferredHeight changes by MoveSpeed,
        to make it faster/slower depending on scene size
        (which usually corresponds to move speed). }
      Increase * MoveSpeed * SecondsPassed * 0.2;

    CorrectPreferredHeight;

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
         during the game, but it's off in game menu (TCastleOnScreenMenu) and start screen.
         So when you're in the game, and choose "End game", game menu
         closes (immediately bringing back MouseLook = true by TGLMode.Destroy
         restoring everything), but game mode immediately closes and goes
         back to start screen. Effect: mouse cursor is forced to the middle
         of the screen, without any apparent (for user) reason.

      2. Later approach: just not reposition mouse at all just
         because MoseLook = true.  Only reposition from
         TWalkCamera.Motion.

         This requires the Motion handler to only work when initial
         mouse position is at the screen middle,
         otherwise initial mouse look would generate large move.
         But in fact TWalkCamera.Motion already does this, so it's all Ok.

         Unfortunately, this isn't so nice: sometimes you really want your
         mouse repositioned even before you move it:
         - e.g. when entering castle game, it's strange that mouse cursor
           is temporarily visible, until you move the mouse.
         - worse: when mouse cursor is outside castle window, you have
           to move mouse first over the window, before mouse look catches up.

      So we have to reposition the mouse, but not too eagerly.
      Update seems a good moment. }
    if MouseLook and (Container <> nil) then
      Container.MakeMousePositionForMouseLook;
  end;

  procedure MoveViaMouseDragging(Delta: TVector2Single);
  var
    MoveSizeX, MoveSizeY: Single;
  const
    Tolerance = 5;  { 5px tolerance for not-moving }
  begin
    MoveSizeX := 0;
    MoveSizeY := 0;
    if Abs(Delta[0]) < Tolerance then
      Delta[0] := 0
    else
    begin
      MoveSizeX := (Abs(Delta[0]) - Tolerance) / 100;
      if MoveSizeX > 1.0 then MoveSizeX := 1.0;
    end;
    if Abs(Delta[1]) < Tolerance then
      Delta[1] := 0
    else
    begin
      MoveSizeY := (Abs(Delta[1]) - Tolerance) / 100;
      if MoveSizeY > 1.0 then MoveSizeY := 1.0;
    end;

    if mbLeft in Container.MousePressed then
    begin
      if Delta[1] < -Tolerance then
        MoveHorizontal(-MoveSizeY * SecondsPassed, 1); { forward }
      if Delta[1] > Tolerance then
        MoveHorizontal(-MoveSizeY * SecondsPassed, -1); { backward }

      if Abs(Delta[0]) > Tolerance then
        RotateHorizontal(-Delta[0] / 4 * SecondsPassed); { rotate }
    end
    else if mbRight in Container.MousePressed then
    begin
      if Delta[0] < -Tolerance then
      begin
        RotateHorizontalForStrafeMove(90);
        MoveHorizontal(MoveSizeX * SecondsPassed, 1);  { strife left }
        RotateHorizontalForStrafeMove(-90);
      end;
      if Delta[0] > Tolerance then
      begin
        RotateHorizontalForStrafeMove(-90);
        MoveHorizontal(MoveSizeX * SecondsPassed, 1);  { strife right }
        RotateHorizontalForStrafeMove(90);
      end;

      if Delta[1] < -5 then
        MoveVertical(-MoveSizeY * SecondsPassed, 1);    { fly up }
      if Delta[1] > 5 then
        MoveVertical(-MoveSizeY * SecondsPassed, -1);   { fly down }
    end;
  end;

var
  ModsDown: TModifierKeys;
begin
  inherited;

  PositionMouseLook;

  { Do not handle keys or gravity etc. }
  if Animation then Exit;

  ModsDown := ModifiersDown(Container.Pressed);

  HeadBobbingAlreadyDone := false;
  MoveHorizontalDone := false;

  BeginVisibleChangeSchedule;
  try
    if HandleInput then
    begin
      if ciNormal in Input then
      begin
        HandleInput := not ExclusiveEvents;
        FIsCrouching := Gravity and Input_Crouch.IsPressed(Container);

        if (not CheckModsDown) or
           (ModsDown - Input_Run.Modifiers = []) then
        begin
          CheckRotates(1.0);

          if Input_Forward.IsPressed(Container) or MoveForward then
            MoveHorizontal(SecondsPassed, 1);
          if Input_Backward.IsPressed(Container) or MoveBackward then
            MoveHorizontal(SecondsPassed, -1);

          if Input_RightStrafe.IsPressed(Container) then
          begin
            RotateHorizontalForStrafeMove(-90);
            MoveHorizontal(SecondsPassed, 1);
            RotateHorizontalForStrafeMove(90);
          end;

          if Input_LeftStrafe.IsPressed(Container) then
          begin
            RotateHorizontalForStrafeMove(90);
            MoveHorizontal(SecondsPassed, 1);
            RotateHorizontalForStrafeMove(-90);
          end;

          { A simple implementation of Input_Jump was
              RotateVertical(90); Move(MoveVerticalSpeed * MoveSpeed * SecondsPassed); RotateVertical(-90)
            Similarly, simple implementation of Input_Crouch was
              RotateVertical(-90); Move(MoveVerticalSpeed * MoveSpeed * SecondsPassed); RotateVertical(90)
            But this is not good, because when PreferGravityUp, we want to move
            along the GravityUp. (Also later note: RotateVertical is now bounded by
            MinAngleRadFromGravityUp). }

          if Input_Jump.IsPressed(Container) then
            MoveVertical(SecondsPassed, 1);
          if Input_Crouch.IsPressed(Container) then
            MoveVertical(SecondsPassed, -1);

          { zmiana szybkosci nie wplywa na Matrix (nie od razu). Ale wywolujemy
            ScheduleVisibleChange - zmienilismy swoje wlasciwosci, moze sa one np. gdzies
            wypisywane w oknie na statusie i okno potrzebuje miec Invalidate po zmianie
            Move*Speed ?.

            How to apply SecondsPassed here ?
            I can't just ignore SecondsPassed, but I can't also write
              FMoveSpeed *= 10 * SecondsPassed;
            What I want is such continous function that e.g.
              F(FMoveSpeed, 10) = F(F(FMoveSpeed, 1), 1)
            I.e. SecondsPassed = 10 should work just like doing the same change twice.
            So F is FMoveSpeed * Power(10, SecondsPassed)
            Easy!
          }
          if Input_MoveSpeedInc.IsPressed(Container) then
          begin
            MoveSpeed := MoveSpeed * Power(10, SecondsPassed);
            ScheduleVisibleChange;
          end;

          if Input_MoveSpeedDec.IsPressed(Container) then
          begin
            MoveSpeed := MoveSpeed / Power(10, SecondsPassed);
            ScheduleVisibleChange;
          end;
        end else
        if ModsDown = [mkCtrl] then
        begin
          if AllowSlowerRotations then
            CheckRotates(0.1);

          { Either MoveSpeedInc/Dec work, or Increase/DecreasePreferredHeight,
            as they by default have the same shortcuts, so should not work
            together. }
          if ModsDown = [mkCtrl] then
          begin
            if Input_IncreasePreferredHeight.IsPressed(Container) then
              ChangePreferredHeight(+1);
            if Input_DecreasePreferredHeight.IsPressed(Container) then
              ChangePreferredHeight(-1);
          end;
        end;
      end;

      { mouse dragging navigation }
      if (MouseDraggingStarted <> -1) and
         ReallyEnableMouseDragging and
         ((mbLeft in Container.MousePressed) or (mbRight in Container.MousePressed)) and
         { Enable dragging only when no modifiers (except Input_Run,
           which must be allowed to enable running) are pressed.
           This allows application to handle e.g. ctrl + dragging
           in some custom ways (like view3dscene selecting a triangle). }
         (Container.Pressed.Modifiers - Input_Run.Modifiers = []) and
         (MouseDragMode = mdWalk) then
      begin
        HandleInput := not ExclusiveEvents;
        MoveViaMouseDragging(Container.MousePosition - MouseDraggingStart);
      end;
    end;

    PreferGravityUpForRotationsUpdate;

    { These may be set to @true only inside GravityUpdate }
    FIsWalkingOnTheGround := false;
    FIsOnTheGround := false;

    GravityUpdate;
  finally
    EndVisibleChangeSchedule;
  end;
end;

function TWalkCamera.Jump: boolean;
begin
  Result := false;

  if IsJumping or Falling or (not Gravity) then Exit;

  { Merely checking for Falling is not enough, because Falling
    may be triggered with some latency. E.g. consider user that holds
    Input_Jump key down: whenever jump will end (in GravityUpdate),
    Input_Jump.IsKey = true will cause another jump to be immediately
    (before Falling will be set to true) initiated.
    This is of course bad, because user holding Input_Jump key down
    would be able to jump to any height. The only good thing to do
    is to check whether player really has some ground beneath his feet
    to be able to jump. }

  { update IsAbove, AboveHeight }
  Height(Position, FIsAbove, FAboveHeight, FAboveGround);

  if AboveHeight > RealPreferredHeight + RealPreferredHeightMargin then
    Exit;

  FIsJumping := true;
  FJumpHeight := 0.0;
  Result := true;
end;

function TWalkCamera.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

function TWalkCamera.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if (Event.EventType = itKey) and
     CheckModsDown and
     (ModifiersDown(Container.Pressed) - Input_Run.Modifiers <> []) then
    Exit;

  if (Event.EventType = itMouseButton) and
     ReallyEnableMouseDragging and
     (MouseDragMode = mdNone) then
  begin
    MouseDraggingStarted := -1;
    Result := false;
    Exit;
  end;

  if (Event.EventType = itMouseWheel) and
     ReallyEnableMouseDragging and
     (MouseDragMode <> mdRotate) and
     Event.MouseWheelVertical then
  begin
    RotateVertical(-Event.MouseWheelScroll * 3);
    Result := true;
    Exit;
  end;

  if (not (ciNormal in Input)) or Animation then Exit(false);

  if Input_GravityUp.IsEvent(Event) then
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
  if Input_Jump.IsEvent(Event) then
  begin
    Result := Jump and ExclusiveEvents;
  end else
    Result := false;
end;

function TWalkCamera.SensorTranslation(const X, Y, Z, Length: Double;
  const SecondsPassed: Single): boolean;
var
  MoveSize: Double;
begin
  if not (ci3dMouse in Input) then Exit(false);
  Result := true;

  MoveSize := Length * SecondsPassed / 5000;

  if Z > 5 then
    MoveHorizontal(Z * MoveSize, -1); { backward }
  if Z < -5 then
    MoveHorizontal(-Z * MoveSize, 1); { forward }

  if X > 5 then
  begin
    RotateHorizontalForStrafeMove(-90);
    MoveHorizontal(X * MoveSize, 1);  { right }
    RotateHorizontalForStrafeMove(90);
  end;
  if X < -5 then
  begin
    RotateHorizontalForStrafeMove(90);
    MoveHorizontal(-X * MoveSize, 1); { left }
    RotateHorizontalForStrafeMove(-90);
  end;

  if Y > 5 then
    MoveVertical(Y * MoveSize, 1);    { up }
  if Y < -5 then
    MoveVertical(-Y * MoveSize, -1);  { down }
end;

function TWalkCamera.SensorRotation(const X, Y, Z, Angle: Double;
  const SecondsPassed: Single): boolean;
begin
  if not (ci3dMouse in Input) then Exit(false);
  Result := true;

  if Abs(X) > 0.4 then      { tilt forward / backward }
    RotateVertical(X * Angle * 2 * SecondsPassed);
  if Abs(Y) > 0.4 then      { rotate }
    RotateHorizontal(Y * Angle * 2 * SecondsPassed);
  {if Abs(Z) > 0.4 then ?} { tilt sidewards }
end;

procedure TWalkCamera.Init(
  const AInitialPosition, AInitialDirection, AInitialUp: TVector3Single;
  const AGravityUp: TVector3Single;
  const APreferredHeight: Single;
  const ARadius: Single);
begin
  SetInitialView(AInitialPosition, AInitialDirection, AInitialUp, false);
  FGravityUp := Normalized(AGravityUp);
  PreferredHeight := APreferredHeight;
  Radius := ARadius;
  CorrectPreferredHeight;
  GoToInitial;
end;

procedure TWalkCamera.Init(const Box: TBox3D; const ARadius: Single);
var Pos: TVector3Single;
    AvgSize: Single;
begin
 if Box.IsEmptyOrZero then
  Init(Vector3Single(0, 0, 0),
       DefaultCameraDirection,
       DefaultCameraUp,
       Vector3Single(0, 1, 0) { GravityUp is the same as InitialUp },
       0 { whatever }, ARadius) else
 begin
  AvgSize := Box.AverageSize;
  Pos[0] := Box.Data[0, 0]-AvgSize;
  Pos[1] := (Box.Data[0, 1]+Box.Data[1, 1])/2;
  Pos[2] := (Box.Data[0, 2]+Box.Data[1, 2])/2;
  Init(Pos, UnitVector3Single[0],
    UnitVector3Single[2],
    UnitVector3Single[2] { GravityUp is the same as InitialUp },
    AvgSize * 5, ARadius);
 end;
end;

function TWalkCamera.GetPositionInternal: TVector3Single;
begin
  Result := FPosition;
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

procedure TWalkCamera.UpPrefer(const AUp: TVector3Single);
begin
  FUp := Normalized(AUp);
  MakeVectorsOrthoOnTheirPlane(FUp, FDirection);
  ScheduleVisibleChange;
end;

procedure TWalkCamera.CorrectPreferredHeight;
begin
  CastleCameras.CorrectPreferredHeight(
    FPreferredHeight, Radius, CrouchHeight, HeadBobbing);
end;

function TWalkCamera.MaxJumpDistance: Single;
begin
  Result := JumpMaxHeight * PreferredHeight;
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

  FFallingOnTheGroundAngleIncrease := RandomBoolean;
end;

procedure TWalkCamera.CancelFalling;
begin
  { Fortunately implementation of this is brutally simple right now. }
  FFalling := false;
end;

procedure TWalkCamera.SetMouseLook(const Value: boolean);
begin
  if FMouseLook <> Value then
  begin
    FMouseLook := Value;
    if FMouseLook then
      Cursor := mcForceNone else
      Cursor := mcDefault;
  end;
end;

function TWalkCamera.Motion(const Event: TInputMotion): boolean;
var
  MouseChange: TVector2Single;
begin
  Result := inherited;
  if Result or (Event.FingerIndex <> 0) then Exit;

  if (ciNormal in Input) and MouseLook and Container.Focused and
    ContainerSizeKnown and (not Animation) then
  begin
    { Note that setting MousePosition may (but doesn't have to)
      generate another Motion in the container to destination position.
      This can cause some problems:

      1. Consider this:

         - player moves mouse to MiddleX-10
         - Motion is generated, I rotate camera by "-10" horizontally
         - Setting MousePosition sets mouse to the Middle,
           but this time no Motion is generated
         - player moved mouse to MiddleX+10. Although mouse was
           positioned on Middle, TCastleWindowCustom thinks that the mouse
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

      Later: see TCastleWindowCustom.UpdateMouseLook implementation notes,
      we actually depend on the fact that MouseLook checks and works
      only if mouse position is at the middle. }
    if Container.IsMousePositionForMouseLook then
    begin
      MouseChange := Event.Position - Container.MousePosition;

      if MouseChange[0] <> 0 then
        RotateHorizontal(-MouseChange[0] * MouseLookHorizontalSensitivity);
      if MouseChange[1] <> 0 then
      begin
        if InvertVerticalMouseLook then
          MouseChange[1] := -MouseChange[1];
        RotateVertical(MouseChange[1] * MouseLookVerticalSensitivity);
      end;

      Result := ExclusiveEvents;
    end;

    Container.MakeMousePositionForMouseLook;
    Exit;
  end;

  if (MouseDraggingStarted <> -1) and
    (MouseDragMode = mdRotate) and
    (not Animation) and
    (not MouseLook) then
  begin
    MouseChange := Event.Position - Container.MousePosition;
    if MouseChange[0] <> 0 then
      RotateHorizontal(-MouseChange[0] * MouseDraggingHorizontalRotationSpeed);
    if MouseChange[1] <> 0 then
      RotateVertical(MouseChange[1] * MouseDraggingVerticalRotationSpeed);
    Result := ExclusiveEvents;
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

function TWalkCamera.GetGravityUp: TVector3Single;
begin
  Result := GravityUp;
end;

procedure TWalkCamera.SetView(const ADir, AUp: TVector3Single;
  const AdjustUp: boolean);
begin
  FDirection := Normalized(ADir);
  FUp := Normalized(AUp);
  if AdjustUp then
    MakeVectorsOrthoOnTheirPlane(FUp, FDirection) else
    MakeVectorsOrthoOnTheirPlane(FDirection, FUp);

  ScheduleVisibleChange;
end;

procedure TWalkCamera.SetView(const APos, ADir, AUp: TVector3Single;
  const AdjustUp: boolean);
begin
  FPosition := APos;
  FDirection := Normalized(ADir);
  FUp := Normalized(AUp);
  if AdjustUp then
    MakeVectorsOrthoOnTheirPlane(FUp, FDirection) else
    MakeVectorsOrthoOnTheirPlane(FDirection, FUp);

  ScheduleVisibleChange;
end;

procedure TWalkCamera.SetView(const APos, ADir, AUp, AGravityUp: TVector3Single;
  const AdjustUp: boolean);
begin
  GravityUp := AGravityUp;
  SetView(APos, ADir, AUp, AdjustUp);
end;

procedure TWalkCamera.SetGravityUp(const Value: TVector3Single);
begin
  FGravityUp := Normalized(Value);
end;

function TWalkCamera.GetNavigationType: TNavigationType;
begin
  if Gravity then
    Result := ntWalk else
    Result := ntFly;
end;

{ TExamineCameraInUniversal -------------------------------------------------- }

type
  TExamineCameraInUniversal = class(TExamineCamera)
  private
    { Owning TUniversalCamera }
    Universal: TUniversalCamera;
  public
    procedure VisibleChange; override;
    function Animation: boolean; override;
  protected
    procedure DoCursorChange; override;
  end;

function TExamineCameraInUniversal.Animation: boolean;
begin
  Result := (inherited Animation) or Universal.Animation;
end;

procedure TExamineCameraInUniversal.VisibleChange;
begin
  inherited;
  { Call parent ScheduleVisibleChange when children change. }
  Universal.ScheduleVisibleChange;
end;

procedure TExamineCameraInUniversal.DoCursorChange;
begin
  inherited;
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
    procedure DoCursorChange; override;
  public
    procedure VisibleChange; override;
    function Animation: boolean; override;
  end;

function TWalkCameraInUniversal.Animation: boolean;
begin
  Result := (inherited Animation) or Universal.Animation;
end;

procedure TWalkCameraInUniversal.VisibleChange;
begin
  inherited;
  { Call parent ScheduleVisibleChange when children change. }
  Universal.ScheduleVisibleChange;
end;

procedure TWalkCameraInUniversal.DoCursorChange;
begin
  inherited;
  { update Universal.Cursor, in case we're the current camera }
  Universal.Cursor := Universal.Current.Cursor;
end;

{ TUniversalCamera ----------------------------------------------------------- }

constructor TUniversalCamera.Create(AOwner: TComponent);
begin
  inherited;
  FExamine := TExamineCameraInUniversal.Create(Self);
  TExamineCameraInUniversal(FExamine).Universal := Self;
  Examine.Name := 'Examine';
  Examine.SetSubComponent(true);

  FWalk := TWalkCameraInUniversal.Create(Self);
  TWalkCameraInUniversal(FWalk).Universal := Self;
  Walk.Name := 'Walk';
  Walk.SetSubComponent(true);
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

function TUniversalCamera.GetPositionInternal: TVector3Single;
begin
  Result := Current.Position;
end;

procedure TUniversalCamera.SetPosition(const Value: TVector3Single);
begin
  Current.Position := Value;
end;

function TUniversalCamera.GetGravityUp: TVector3Single;
begin
  Result := Current.GetGravityUp;
end;

procedure TUniversalCamera.SetView(const APos, ADir, AUp: TVector3Single;
  const AdjustUp: boolean);
begin
  { Note that both Xxx.SetView calls below do Xxx.VisibleChange at the end,
    which in turn call our own ScheduleVisibleChange.

    Using Begin/EndVisibleChangeSchedule is more than just an optimization
    (to avoid calling our own VisibleChange at least twice) here.
    It is actually required for correctness.
    That is becasue VisibleChange method may be overriden and/or call various
    callbacks that may in turn change the camera again.

    - So these VisibleChange callbacks should be called only once our state
      is consistent, not in the middle (like at the end of FExamine.SetView,
      when FExamine state is not consistent with FWalk state yet).

    - Also, there are cases when variable aliasing would cause our const
      parameters to change. Consider view3dscene with
      demo_models/navigation/transition_multiple_viewpoints.x3dv ,
      when transition 1 ends: our TCamera.Update will then
      call TUniversalCamera.SetView with AnimationEndXxx parameters.
      Without Begin/EndVisibleChangeSchedule, the VisibleChange calls
      inside will cause TCastleSceneCore.CameraChanged
      that causes NavigationInfo.transitionComplete event,
      which in turn (if X3D file sends Viewpoint.set_bind to immediately
      start another transition) may cause TUniversalCamera.AnimateTo call,
      that changes AnimationEndXxx parameters... Accidentally also changing
      our current "const" Pos, Dir, Up parameters. This would cause us to blink
      the final MyViewpoint3 position at the beginning of transition from
      MyViewpoint2 to MyViewpoint3.
  }

  BeginVisibleChangeSchedule;
  try
    FExamine.SetView(APos, ADir, AUp, AdjustUp);
    FWalk.SetView(APos, ADir, AUp, AdjustUp);
  finally EndVisibleChangeSchedule end;
end;

procedure TUniversalCamera.SetView(const APos, ADir, AUp, AGravityUp: TVector3Single;
  const AdjustUp: boolean);
begin
  BeginVisibleChangeSchedule;
  try
    FExamine.SetView(APos, ADir, AUp, AGravityUp, AdjustUp);
    FWalk.SetView(APos, ADir, AUp, AGravityUp, AdjustUp);
  finally EndVisibleChangeSchedule end;
end;

procedure TUniversalCamera.SetRadius(const Value: Single);
begin
  inherited;
  FExamine.Radius := Value;
  FWalk.Radius := Value;
end;

procedure TUniversalCamera.SetInput(const Value: TCameraInputs);
begin
  inherited;
  FExamine.Input := Value;
  FWalk.Input := Value;
end;

procedure TUniversalCamera.SetEnableDragging(const Value: boolean);
begin
  inherited;
  FExamine.EnableDragging := Value;
  FWalk.EnableDragging := Value;
end;

procedure TUniversalCamera.SetProjectionMatrix(const Value: TMatrix4Single);
begin
  { This calls RecalculateFrustum on all 3 cameras, while only once
    is needed... But speed should not be a problem here, this is seldom used. }
  inherited;
  FExamine.ProjectionMatrix := Value;
  FWalk.ProjectionMatrix := Value;
end;

procedure TUniversalCamera.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
begin
  inherited;
  Current.Update(SecondsPassed, HandleInput);
end;

function TUniversalCamera.SensorTranslation(const X, Y, Z, Length: Double;
  const SecondsPassed: Single): boolean;
begin
  Result := Current.SensorTranslation(X, Y, Z, Length, SecondsPassed);
end;

function TUniversalCamera.SensorRotation(const X, Y, Z, Angle: Double;
  const SecondsPassed: Single): boolean;
begin
  Result := Current.SensorRotation(X, Y, Z, Angle, SecondsPassed);
end;

function TUniversalCamera.AllowSuspendForInput: boolean;
begin
  Result := Current.AllowSuspendForInput;
end;

function TUniversalCamera.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := Current.Press(Event);
end;

function TUniversalCamera.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := Current.Release(Event);
end;

function TUniversalCamera.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;
  if Result then Exit;

  Result := Current.Motion(Event);
end;

procedure TUniversalCamera.SetContainer(const Value: TUIContainer);
begin
  inherited;
  FWalk.Container := Value;
  FExamine.Container := Value;
end;

procedure TUniversalCamera.Resize;
begin
  inherited;
  FWalk.Resize;
  FExamine.Resize;
end;

procedure TUniversalCamera.SetInitialView(
  const AInitialPosition: TVector3Single;
  AInitialDirection, AInitialUp: TVector3Single;
  const TransformCurrentCamera: boolean);
begin
  BeginVisibleChangeSchedule;
  try
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
  finally EndVisibleChangeSchedule end;
end;

procedure TUniversalCamera.SetNavigationClass(const Value: TNavigationClass);
var
  Pos, Dir, Up: TVector3Single;
begin
  if FNavigationClass <> Value then
  begin
    Current.GetView(Pos, Dir, Up);
    FNavigationClass := Value;

    { SetNavigationClass may be called when Dir and Up
      are both perfectly zero, from TCastleSceneCore.CreateCamera
      that creates a camera and first calls CameraFromNavigationInfo
      (that sets NavigationClass) before calling CameraFromViewpoint
      (that sets sensible view vectors). We protect from it, to not call
      SetView with Dir and Up zero.

      Although for now this isn't really needed, as all SetView implementations
      behave Ok, because
      1. MakeVectorsOrthoOnTheirPlane with both dir/up = zero is Ok
         (it leaves the 1st argument as zero (because
         AnyOrthogonalVector(zero) = zero)),
      2. CamDirUp2OrientQuat also gracefully accepts dir/up = zero
         (but it doesn't have to, it's documentation requires only non-zero
         vectors).
      But, for the future, protect from it, since the doc for SetView guarantees
      correct behavior only for dir/up non-zero. }

    if not (PerfectlyZeroVector(Dir) and PerfectlyZeroVector(Up)) then
      Current.SetView(Pos, Dir, Up);
    { our Cursor should always reflect Current.Cursor }
    Cursor := Current.Cursor;
  end;
end;

function TUniversalCamera.GetNavigationType: TNavigationType;
begin
  if Input = [] then
    Result := ntNone else
    Result := Current.GetNavigationType;
end;

procedure TUniversalCamera.SetNavigationType(const Value: TNavigationType);
begin
  { This is not a pure optimization in this case.
    If you set some weird values, then (without this check)
    doing "NavigationType := NavigationType" would not be NOOP. }
  if Value = GetNavigationType then Exit;

  { set default values (for Walk camera and Input),
    may be changed later by this method. This way every setting
    of SetNavigationType sets them, regardless of value, which seems
    consistent. }
  Walk.Gravity := false;
  Walk.PreferGravityUpForRotations := true;
  Walk.PreferGravityUpForMoving := true;
  Examine.Turntable := false;
  Input := DefaultInput;

  { This follows the same logic as TCastleSceneCore.CameraFromNavigationInfo }

  { set NavigationClass, and eventually adjust Walk properties }
  case Value of
    ntExamine: NavigationClass := ncExamine;
    ntTurntable:
      begin
        NavigationClass := ncExamine;
        Examine.Turntable := true;
      end;
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
        Input := [];
      end;
    else raise EInternalError.Create('TUniversalCamera.SetNavigationType: Value?');
  end;
end;

{ global ------------------------------------------------------------ }

procedure CorrectPreferredHeight(var PreferredHeight: Single;
  const Radius: Single; const CrouchHeight, HeadBobbing: Single);
var
  NewPreferredHeight: Single;
begin
  { We have requirement that
      PreferredHeight * CrouchHeight * (1 - HeadBobbing) >= Radius
    So
      PreferredHeight >= Radius / (CrouchHeight * (1 - HeadBobbing));

    I make it even a little larger (that's the reason for "* 1.01") to be
    sure to avoid floating-point rounding errors. }

  NewPreferredHeight := 1.01 * Radius /
    (CrouchHeight * (1 - HeadBobbing));

  if PreferredHeight < NewPreferredHeight then
    PreferredHeight := NewPreferredHeight;
end;

function CamDirUp2OrientQuat(CamDir, CamUp: TVector3Single): TQuaternion;

{ This was initially based on Stephen Chenney's ANSI C code orient.c,
  available still from here: http://vrmlworks.crispen.org/tools.html
  I rewrote it a couple of times, possibly removing and possibly adding
  some bugs :)

  Idea: we want to convert CamDir and CamUp into VRML orientation,
  which is a rotation from DefaultCameraDirection/DefaultCameraUp into CamDir/Up.

  1) Take vector orthogonal to standard DefaultCameraDirection and CamDir.
     Rotate around it, to match DefaultCameraDirection with CamDir.

  2) Now rotate around CamDir such that standard up (already rotated
     by 1st transform) matches with CamUp. We know it's possible,
     since CamDir and CamUp are orthogonal and normalized,
     just like standard DefaultCameraDirection/DefaultCameraUp.

  Combine these two rotations and you have the result.

  How to combine two rotations, such that in the end you get nice
  single rotation? That's where quaternions rule.
}

  function QuatFromAxisAngleCos(const Axis: TVector3Single;
    const AngleRadCos: Single): TQuaternion;
  begin
    Result := QuatFromAxisAngle(Axis, ArcCos(Clamped(AngleRadCos, -1.0, 1.0)));
  end;

var
  Rot1Axis, Rot2Axis, StdCamUpAfterRot1: TVector3Single;
  Rot1Quat, Rot2Quat: TQuaternion;
  Rot1CosAngle, Rot2CosAngle: Single;
begin
  NormalizeVar(CamDir);
  NormalizeVar(CamUp);

  { calculate Rot1Quat }
  Rot1Axis := VectorProduct(DefaultCameraDirection, CamDir);
  { Rot1Axis may be zero if DefaultCameraDirection and CamDir are parallel.
    When they point in the same direction, then it doesn't matter
    (rotation will be by 0 angle anyway), but when they are in opposite
    direction we want to do some rotation, so we need some non-zero
    sensible Rot1Axis. }
  if ZeroVector(Rot1Axis) then
    Rot1Axis := DefaultCameraUp else
    { Normalize *after* checking ZeroVector, otherwise normalization
      could change some almost-zero vector into a (practically random)
      vector of length 1. }
    NormalizeVar(Rot1Axis);
  Rot1CosAngle := VectorDotProduct(DefaultCameraDirection, CamDir);
  Rot1Quat := QuatFromAxisAngleCos(Rot1Axis, Rot1CosAngle);

  { calculate Rot2Quat }
  StdCamUpAfterRot1 := Rot1Quat.Rotate(DefaultCameraUp);
  { We know Rot2Axis should be either CamDir or -CamDir. But how do we know
    which one? (To make the rotation around it in correct direction.)
    Calculating Rot2Axis below is a solution. }
  Rot2Axis := VectorProduct(StdCamUpAfterRot1, CamUp);

  (*We could now do NormalizeVar(Rot2Axis),
    after making sure it's not zero. Like

    { we need larger epsilon for ZeroVector below, in case
      StdCamUpAfterRot1 is = -CamUp.
      testcameras.pas contains testcases that require it. }
    if ZeroVector(Rot2Axis, 0.001) then
      Rot2Axis := CamDir else
      { Normalize *after* checking ZeroVector, otherwise normalization
        could change some almost-zero vector into a (practically random)
        vector of length 1. }
      NormalizeVar(Rot2Axis);

    And later do

      { epsilon for VectorsEqual 0.001 is too small }
      Assert( VectorsEqual(Rot2Axis,  CamDir, 0.01) or
              VectorsEqual(Rot2Axis, -CamDir, 0.01),
        Format('CamDirUp2OrientQuat failed for CamDir, CamUp: (%s), (%s)',
          [ VectorToRawStr(CamDir), VectorToRawStr(CamUp) ]));

    However, as can be seen in above comments, this requires some careful
    adjustments of epsilons, so it's somewhat numerically unstable.
    It's better to just use now the knowledge that Rot2Axis
    is either CamDir or -CamDir, and choose one of them. *)
  if AreParallelVectorsSameDirection(Rot2Axis, CamDir) then
    Rot2Axis :=  CamDir else
    Rot2Axis := -CamDir;

  Rot2CosAngle := VectorDotProduct(StdCamUpAfterRot1, CamUp);
  Rot2Quat := QuatFromAxisAngleCos(Rot2Axis, Rot2CosAngle);

  { calculate Result = combine Rot1 and Rot2 (yes, the order
    for QuatMultiply is reversed) }
  Result := Rot2Quat * Rot1Quat;
end;

procedure CamDirUp2Orient(const CamDir, CamUp: TVector3Single;
  out OrientAxis: TVector3Single; out OrientRadAngle: Single);
begin
  { Call CamDirUp2OrientQuat,
    and extract the axis and angle from the quaternion. }
  CamDirUp2OrientQuat(CamDir, CamUp).ToAxisAngle(OrientAxis, OrientRadAngle);
end;

function CamDirUp2Orient(const CamDir, CamUp: TVector3Single): TVector4Single;
var
  OrientAxis: TVector3Single;
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
  if not WantedDirectionPositive then VectorNegateVar(Direction);

  Up := UnitVector3Single[WantedUp];
  if not WantedUpPositive then VectorNegateVar(Up);

  if Box.IsEmpty then
  begin
    Position  := ZeroVector3Single;
  end else
  begin
    Position := Box.Center;
    Offset := 2 * Box.AverageSize;

    if WantedDirectionPositive then
      Position[WantedDirection] := Box.Data[0, WantedDirection] - Offset else
      Position[WantedDirection] := Box.Data[1, WantedDirection] + Offset;
  end;

  { GravityUp is just always equal Up here. }
  GravityUp := Up;
end;

end.
