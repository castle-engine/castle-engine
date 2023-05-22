{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Third-person navigation (TCastleThirdPersonNavigation). }
unit CastleThirdPersonNavigation;

{$I castleconf.inc}

{.$define CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}

interface

uses SysUtils, Classes,
  CastleKeysMouse, CastleScene, CastleVectors, CastleCameras,
  CastleTransform, CastleInputs, CastleClassUtils;

type
  { Used by TCastleThirdPersonNavigation.AimAvatar. }
  TAimAvatar = (aaNone, aaHorizontal, aaFlying);

  { How does the avatar change transformation (for movement and rotations). }
  TChangeTransformation = (
    { Automatically determines best way to change transformation.

      Right now, it means we

      @unorderedList(
        @item(Behave like ctVelocity if avatar is under the control of the physics engine.
          This means avatar has both TCastleRigidBody and TCastleCollider behaviors and
          @link(TCastleRigidBody.Exists) is @true.)
        @item(Behave like ctDirect otherwise.)
      )

      To be precise, we look at @link(TCastleThirdPersonNavigation.AvatarHierarchy) or
      (if it's @nil) at @link(TCastleThirdPersonNavigation.Avatar).
      In this transform, we check existence of TCastleRigidBody and TCastleCollider.

      In the future, this auto-detection may change,
      to follow our best recommended practices.
      In particular, when ctForce approach (see below) will be fully implemented,
      it will likely become the new automatic behavior when
      TCastleRigidBody and TCastleCollider behaviors are present. }
    ctAuto,

    { Directly change the avatar @link(TCastleTransform.Translation),
      @link(TCastleTransform.Rotation).

      You @italic(should not) have physics components (TCastleRigidBody and TCastleCollider
      and @link(TCastleRigidBody.Exists) = @true)
      set up on the @link(TCastleThirdPersonNavigation.AvatarHierarchy) or
      @link(TCastleThirdPersonNavigation.Avatar) in this case.
      Having physics components will make it impossible to change @link(TCastleTransform.Translation),
      @link(TCastleTransform.Rotation) each frame.

      This also means that if you want to have gravity (and stair climbing),
      you need to use deprecated @link(TCastleTransform.Gravity),
      @link(TCastleTransform.GrowSpeed), @link(TCastleTransform.FallSpeed).
      They are part of the old simple physics engine:
      https://castle-engine.io/physics#_old_system_for_collisions_and_gravity .

      TODO: Jumping and falling doesn't work in this case.
    }
    ctDirect,

    { Change the avatar using rigid body @link(TCastleRigidBody.LinearVelocity),
      @link(TCastleRigidBody.AngularVelocity).

      This is not fully realistic (instead of calculating velocities explicitly
      we should be using forces). But it cooperates nicely with physics engine.

      It requires a TCastleRigidBody and TCastleCollider components
      to be attached to the @link(TCastleThirdPersonNavigation.AvatarHierarchy)
      (or @link(TCastleThirdPersonNavigation.Avatar), if @link(TCastleThirdPersonNavigation.AvatarHierarchy) is @nil).
      Also @link(TCastleRigidBody.Exists) must be @true to make navigation have any effect.

      This also means that gravity should be handled by the physics engine.
      You should not use deprecated @link(TCastleTransform.Gravity),
      @link(TCastleTransform.GrowSpeed), @link(TCastleTransform.FallSpeed) in this case.

      TODO: Climbing stairs doesn't work in this case (but you can jump on them).
    }
    ctVelocity

    {$ifdef CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE},

    { Change the avatar using rigid body forces like @link(TCastleRigidBody.AddForce),
      @link(TCastleRigidBody.AddTorque).

      This is realistic and cooperates nicely with physics engine.

      It requires a TCastleRigidBody and TCastleCollider components
      to be attached to the @link(TCastleThirdPersonNavigation.AvatarHierarchy)
      (or @link(TCastleThirdPersonNavigation.Avatar), if @link(TCastleThirdPersonNavigation.AvatarHierarchy) is @nil).
      Also @link(TCastleRigidBody.Exists) must be @true to make navigation have any effect.

      TODO: Unfinished, not really functional now. }
    ctForce
    {$endif}
  );

  { 3rd-person camera navigation.
    Create an instance of this and assign it to @link(TCastleViewport.Navigation) to use.
    Be sure to also assign @link(Avatar).
    Call @link(Init) once the parameters that determine initial camera location are all set.

    Turn on @link(TCastleMouseLookNavigation.MouseLook MouseLook) to allow user to move
    the mouse to orbit with the camera around the avatar.
    When AimAvatar is aaNone (default), it allows to look at the avatar easily
    from any side (e.g. you can then see avatar's face easily).
    When @link(AimAvatar) is aaHorizontal or aaFlying, rotating allows to point
    the avatar at the appropriate direction.

    Using keys AWSD and arrows you can move and rotate the avatar,
    and the camera will follow.

    Using the mouse wheel you can get closer / further to the avatar.

    See also the news post with demo movie about this component:
    https://castle-engine.io/wp/2020/06/29/third-person-navigation-with-avatar-component-in-castle-game-engine/
  }
  TCastleThirdPersonNavigation = class(TCastleMouseLookNavigation)
  strict private
    FAvatar: TCastleScene;
    FAvatarHierarchy: TCastleTransform;
    FAvatarRotationSpeed: Single;
    FInitialHeightAboveTarget: Single;
    FDistanceToAvatarTarget: Single;
    FAimAvatar: TAimAvatar;
    FAvatarTarget: TVector3;
    FCameraSpeed: Single;
    {$ifdef AVATAR_TARGET_FORWARD}
    FAvatarTargetForward: TVector3;
    {$endif}
    FMoveSpeed, FCrouchSpeed, FRunSpeed, FJumpSpeed: Single;
    FRotationSpeed: Single;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_RightRotate: TInputShortcut;
    FInput_LeftRotate: TInputShortcut;
    FInput_RightStrafe: TInputShortcut;
    FInput_LeftStrafe: TInputShortcut;
    FInput_Crouch: TInputShortcut;
    FInput_Run: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FCameraDistanceChangeSpeed: Single;
    FMinDistanceToAvatarTarget: Single;
    FMaxDistanceToAvatarTarget: Single;
    FImmediatelyFixBlockedCamera: Boolean;
    FAnimationIdle: String;
    FAnimationRotate: String;
    FAnimationWalk: String;
    FAnimationRun: String;
    FAnimationCrouch: String;
    FAnimationCrouchIdle: String;
    FAnimationCrouchRotate: String;
    FAnimationJump: String;
    FAnimationFall: String;
    FCameraFollows: Boolean;
    FAvatarFreeObserver: TFreeNotificationObserver;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    SetAnimationWarningsDone: Cardinal;
    FChangeTransformation: TChangeTransformation;
    FWasJumpInput: Boolean;
    { Zero we can't control avatar in air, one we have full control }
    FAirMovementControl: Single;
    FAirRotationControl: Single;
    WarningDonePhysicsNotNecessary,
      WarningDoneRigidBodyNecessary,
      WarningDoneColliderNecessary: Boolean;
    function RealAvatarHierarchy: TCastleTransform;
    procedure SetAvatar(const Value: TCastleScene);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
    function CameraPositionInitial(const A: TCastleTransform): TVector3; overload;
    { Get (returns) initial camera position to look at avatar,
      and get (as TargetWorldPos) target look position.
      Note: TargetWorldPos may be equal to result of CameraPositionInitial
      in the extreme cases. }
    function CameraPositionInitial(const A: TCastleTransform; out TargetWorldPos: TVector3): TVector3; overload;
    { Returns MaxSingle if no limit.
      Note that CameraDir doesn't have to be normalized. }
    function CameraMaxDistanceToTarget(const A: TCastleTransform; const CameraLookPos: TVector3;
      const CameraDir: TVector3): Single;
    { Update camera, to avoid having something collidable between camera position and AvatarTarget.
      Note that CameraDir doesn't have to be normalized. }
    procedure FixCameraForCollisions(var CameraPos: TVector3; const CameraDir: TVector3);
    { Return V rotated such that it is
      orthogonal to GravUp. This way it returns V projected
      on the gravity horizontal plane.
      Result retains the V length (so it is always normalized if V is normalized).

      Note that when V and GravUp are parallel,
      this just returns current V --- because in such case
      we can't project V on the horizontal plane. }
    function ToGravityPlane(const V: TVector3; const GravUp: TVector3): TVector3;
    function AnimationIdleStored: Boolean;
    function AnimationRotateStored: Boolean;
    function AnimationWalkStored: Boolean;
    function AnimationRunStored: Boolean;
    function AnimationCrouchStored: Boolean;
    function AnimationCrouchIdleStored: Boolean;
    function AnimationCrouchRotateStored: Boolean;
    function AnimationJumpStored: Boolean;
    function AnimationFallStored: Boolean;
    procedure SetInitialHeightAboveTarget(const Value: Single);
    procedure SetDistanceToAvatarTarget(const Value: Single);
    procedure MySetAvatarTargetForPersistent(const AValue: TVector3);
    procedure SetCameraFollows(const Value: Boolean);
    procedure AvatarFreeNotification(const Sender: TFreeNotificationObserver);
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    function MovementControlFactor(const AvatarOnGround: Boolean): Single;
    function RotationControlFactor(const AvatarOnGround: Boolean): Single;
  protected
    procedure ProcessMouseLookDelta(const Delta: TVector2); override;
    function Zoom(const Factor: Single): Boolean; override;

    { Make avatar play given animation.

      The desired animation is specified as a list of strings,
      from the most preferred name of the animation to the least preferred.
      This allows to perform a "fallback" mechanism in case some animations
      are missing on the model.
      For example if AnimationCrouch is not available,
      we will use AnimationCrouchIdle instead,
      and if that is also not available we will use AnimationIdle.
      Some basic animations must still exist -- if even AnimationIdle
      doesn't exist, we'll just show a warning.

      The default implementation in @className changes
      @link(TCastleSceneCore.AutoAnimation Avatar.AutoAnimation) method.
      It does nothing if @link(Avatar) is @nil.

      Moreover the default implementation implements a "fallback" mechanism
      in case some animations are not available in the scene.
      This is checked using @link(TCastleSceneCore.HasAnimation Avatar.HasAnimation).

      This method is virtual so you can override it in descendants to apply
      the animation in any way, e.g. to a hierarchy of scenes composed from MD3
      pieces. When overriding this, you don't need to call @code(inherited).
      If you override this to apply the animation to something else than @link(Avatar),
      then it may even be reasonable to leave @link(Avatar) as @nil,
      and only set @link(AvatarHierarchy).

      The implementation of this method (after performing the "fallback" mechanism
      described above to find the real name on the AnimationName list)
      should check whether the object is not @italic(already playing the same animation).
      This is important to avoid unnecessary animation restarts. }
    procedure SetAnimation(const AnimationNames: array of String); virtual;
  public
    const
      DefaultInitialHeightAboveTarget = 1.0;
      DefaultDistanceToAvatarTarget = 4.0;
      DefaultAvatarRotationSpeed = 10;
      DefaultAvatarTarget: TVector3 = (X: 0; Y: 2; Z: 0);
      DefaultCameraSpeed = 10;
      {$ifdef AVATAR_TARGET_FORWARD}
      DefaultAvatarTargetForward: TVector3 = (X: 0; Y: 2; Z: 0);
      {$endif}
      DefaultMoveSpeed = 1.0;
      DefaultCrouchSpeed = 0.5;
      DefaultRunSpeed = 2.0;
      DefaultJumpSpeed = 5.0;
      DefaultRotationSpeed = Pi * 150 / 180;
      DefaultCameraDistanceChangeSpeed = 1;
      DefaultMinDistanceToAvatarTarget = 0.5;
      DefaultMaxDistanceToAvatarTarget = 10;
      DefaultAnimationIdle = 'idle';
      DefaultAnimationRotate = 'rotate';
      DefaultAnimationWalk = 'walk';
      DefaultAnimationRun = 'run';
      DefaultAnimationCrouch = 'crouch';
      DefaultAnimationCrouchIdle = 'crouch_idle';
      DefaultAnimationCrouchRotate = 'crouch_rotate';
      DefaultAnimationJump = 'jump';
      DefaultAnimationFall = 'fall';
      DefaultAirMovementControl = 0.5;
      DefaultAirRotationControl = 0.5;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Makes camera be positioned with respect to the current properties and avatar.
      Always call this explicitly once.
      Use this after setting properties like @link(Avatar),
      @link(AvatarHierarchy), @link(DistanceToAvatarTarget),
      @link(InitialHeightAboveTarget).

      At design-time (in CGE editor), this is automatically called after
      changing relevant properties of this navigation. }
    procedure Init;

    { Translation, from the avatar origin, to the "target" of the avatar where camera
      looks at. This is usually head, and this vector should just describe the height
      of head above the ground.
      By default this is DefaultAvatarTarget = (0, 2, 0). }
    property AvatarTarget: TVector3 read FAvatarTarget write FAvatarTarget;

    {$ifdef AVATAR_TARGET_FORWARD}
    { When the camera looks directly behind the avatar's back,
      it points at AvatarTargetForward, not AvatarTarget.
      This allows to place AvatarTargetForward more in forward (and maybe higher/lower)
      than avatar's head.
      This allows to look more "ahead".

      The effective target is a result of lerp between
      AvatarTargetForward and AvatarTarget, depending on how much is camera now close to
      the initial position "looking from the back of avatar".

      The camera is still always rotating around AvatarTarget
      (so you rotate around avatar's head, even if you look far ahead).
      By default this is DefaultAvatarTargetForward = (0, 2, 0).

      TODO: Not implemented now. I made initial implementation using Lerp,
      and it just works awful -- camera rotates unexpectedly, the user feels
      losing control of the camera at certain angles.
    }
    property AvatarTargetForward: TVector3 read FAvatarTargetForward write FAvatarTargetForward;
    {$endif}

    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_LeftRotate: TInputShortcut read FInput_LeftRotate;
    property Input_RightRotate: TInputShortcut read FInput_RightRotate;
    property Input_LeftStrafe: TInputShortcut read FInput_LeftStrafe;
    property Input_RightStrafe: TInputShortcut read FInput_RightStrafe;
    property Input_Crouch: TInputShortcut read FInput_Crouch;
    property Input_Run: TInputShortcut read FInput_Run;
    property Input_Jump: TInputShortcut read FInput_Jump;

    function Input_CameraCloser: TInputShortcut; deprecated 'use Input_ZoomIn';
    function Input_CameraFurther: TInputShortcut; deprecated 'use Input_ZoomOut';
  published
    { Zooming in this navigation mode makes camera move closer/further from avatar. }
    property ZoomEnabled default true;

    property MouseLookHorizontalSensitivity;
    property MouseLookVerticalSensitivity;
    property InvertVerticalMouseLook;

    { Does camera follow the avatar, by default yes.

      When this is @false, camera remains unchanged by anything here
      (avatar movement/rotations, mouse look, even by calling @link(Init)).
      Some properties of this then are meaningless (e.g. @link(DistanceToAvatarTarget)).

      However, all the inputs to control the avatar continue to work. }
    property CameraFollows: Boolean read FCameraFollows write SetCameraFollows default true;

    { Avatar scene, that is animated, moved and rotated when this navigation changes.
      This navigation component will just call @code(Avatar.AutoAnimation := 'xxx') when necessary.
      Currently we require the following animations to exist: walk, idle.

      When AvatarHierarchy is @nil, then @name is directly moved and rotated
      to move avatar.
      Otherwise, AvatarHierarchy is moved, and @name should be inside AvatarHierarchy.

      This scene should be part of @link(TCastleViewport.Items)
      to make this navigation work, in particular when you call @link(Init). }
    property Avatar: TCastleScene read FAvatar write SetAvatar;

    { Optional avatar hierarchy that is moved and rotated when this navigation changes.
      When this is @nil, we just move and rotate the @link(Avatar).
      When this is non-nil, then we only move and rotate this AvatarHierarchy.

      If @link(AvatarHierarchy) is non-nil, then it should contain
      @link(Avatar) as a child. @link(AvatarHierarchy) can even be equal to @link(Avatar)
      (it is equivalent to just leaving @link(AvatarHierarchy) as @nil).

      This object should be part of @link(TCastleViewport.Items)
      to make this navigation work, in particular when you call @link(Init). }
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;

    { When @link(AimAvatar), this is avatar's rotation speed (in radians per second).
      Should make avatar rotation "catch up" (with some delay after camera rotation. }
    property AvatarRotationSpeed: Single read FAvatarRotationSpeed write FAvatarRotationSpeed
      {$ifdef FPC}default DefaultAvatarRotationSpeed{$endif};

    { Camera position tracks the desired position with given speed (in units per second).
      This makes camera adjust to avatar moving (because of input, or because of gravity
      or other external code) and to not being blocked by the collider. }
    property CameraSpeed: Single read FCameraSpeed write FCameraSpeed
      {$ifdef FPC}default DefaultCameraSpeed{$endif};

    { If not aaNone then rotating the camera also rotates (with some delay) the avatar,
      to face the same direction as the camera.
      This allows to rotate the avatar with mouse look (which is comfortable),
      on the other hand it takes away some flexibility,
      e.g. you cannot look at avatar's face for a long time anymore. }
    property AimAvatar: TAimAvatar read FAimAvatar write FAimAvatar default aaNone;

    { Initial height of camera above the AvatarTarget.
      Together with DistanceToAvatarTarget this determines the initial camera position,
      set by @link(Init).
      It is not used outside of @link(Init). }
    property InitialHeightAboveTarget: Single read FInitialHeightAboveTarget write SetInitialHeightAboveTarget
      {$ifdef FPC}default DefaultInitialHeightAboveTarget{$endif};

    { Immediately (not with delay of CameraSpeed) update camera to never block avatar
      view by a wall, enemy etc. When it is @true, we avoid seeing an invalid geometry
      (e.g. from the wrong side of the wall or inside a creature) @italic(ever),
      but in exchange the camera sometimes has to be adjusted very abrtupty (testcase:
      third_person_navigation demo, stand in the middle of moving enemies, and look around). }
    property ImmediatelyFixBlockedCamera: Boolean read FImmediatelyFixBlockedCamera write FImmediatelyFixBlockedCamera
      default false;

    { Preferred distance from camera to the avatar target (head).
      User can change it with Input_ZoomIn, Input_ZoomOut if you set these inputs
      to some key/mouse button/mouse wheel. }
    property DistanceToAvatarTarget: Single read FDistanceToAvatarTarget write SetDistanceToAvatarTarget
      {$ifdef FPC}default DefaultDistanceToAvatarTarget{$endif};
    { Speed with which Input_ZoomIn, Input_ZoomOut can change DistanceToAvatarTarget. }
    property CameraDistanceChangeSpeed: Single read FCameraDistanceChangeSpeed write FCameraDistanceChangeSpeed
      {$ifdef FPC}default DefaultCameraDistanceChangeSpeed{$endif};
    { Limit of the distance to avatar, used when changing DistanceToAvatarTarget,
      and also when deciding how to adjust camera to avoid collisions.
      @groupBegin }
    property MinDistanceToAvatarTarget: Single read FMinDistanceToAvatarTarget write FMinDistanceToAvatarTarget
      {$ifdef FPC}default DefaultMinDistanceToAvatarTarget{$endif};
    property MaxDistanceToAvatarTarget: Single read FMaxDistanceToAvatarTarget write FMaxDistanceToAvatarTarget
      {$ifdef FPC}default DefaultMaxDistanceToAvatarTarget{$endif};
    { @groupEnd }

    { Speed of movement by keys. }
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed
      {$ifdef FPC}default DefaultMoveSpeed{$endif};
    { Speed of movement by keys, when crouching. }
    property CrouchSpeed: Single read FCrouchSpeed write FCrouchSpeed
      {$ifdef FPC}default DefaultCrouchSpeed{$endif};
    { Speed of movement by keys, when running. }
    property RunSpeed: Single read FRunSpeed write FRunSpeed
      {$ifdef FPC}default DefaultRunSpeed{$endif};
    { Speed of jump by keys. }
    property JumpSpeed: Single read FJumpSpeed write FJumpSpeed
      {$ifdef FPC}default DefaultJumpSpeed{$endif};
    { Speed of rotating by keys, in radians per second. }
    property RotationSpeed: Single read FRotationSpeed write FRotationSpeed
      {$ifdef FPC}default DefaultRotationSpeed{$endif};

    { Should we have control on the avatar movement in the air. Must be >= 0.

      @unorderedList(
        @item(0 -> no control in the air)
        @item(1 -> full control in the air, just like on the ground)
        @item(between 0 and 1 -> limited control, smoothly changes between no control and full control)
        @item(above 1 -> in the air you move even faster than on the ground)
      )
    }
    property AirMovementControl: Single read FAirMovementControl write FAirMovementControl
      {$ifdef FPC}default DefaultAirMovementControl{$endif};

    { Should we have control on the avatar rotation in the air.

      @unorderedList(
        @item(0 -> no control in the air)
        @item(1 -> full control in the air, just like on the ground)
        @item(between 0 and 1 -> limited control, smoothly changes between no control and full control)
        @item(above 1 -> in the air you rotate even faster than on the ground)
      )
    }
    property AirRotationControl: Single read FAirRotationControl write FAirRotationControl
      {$ifdef FPC}default DefaultAirRotationControl{$endif};

    { Animation when character is not moving, not rotating and not crouching.
      Default 'idle'. }
    property AnimationIdle: String read FAnimationIdle write FAnimationIdle stored AnimationIdleStored nodefault;
    { Animation when character is rotating, but otherwise remains in place
      (not moving) and it is not crouching.
      Default 'rotate'. }
    property AnimationRotate: String read FAnimationRotate write FAnimationRotate stored AnimationRotateStored nodefault;
    { Animation when character is walking.
      Default 'walk'. }
    property AnimationWalk: String read FAnimationWalk write FAnimationWalk stored AnimationWalkStored nodefault;
    { Animation when character is running.
      Default 'run'. }
    property AnimationRun: String read FAnimationRun write FAnimationRun stored AnimationRunStored nodefault;
    { Animation when character is moving while crouching.
      Default 'crouch'. }
    property AnimationCrouch: String read FAnimationCrouch write FAnimationCrouch stored AnimationCrouchStored nodefault;
    { Animation when character is crouching (Input_Crouch is pressed) but not moving or rotating.
      Default 'crouch_idle'. }
    property AnimationCrouchIdle: String read FAnimationCrouchIdle write FAnimationCrouchIdle stored AnimationCrouchIdleStored nodefault;
    { Animation when character is crouching (Input_Crouch is pressed) and rotating, but not moving.
      Default 'crouch_rotate'.}
    property AnimationCrouchRotate: String read FAnimationCrouchRotate write FAnimationCrouchRotate stored AnimationCrouchRotateStored nodefault;
    { Animation when character is jumping (Input_Jump is pressed).
      Default 'jump'.}
    property AnimationJump: String read FAnimationJump write FAnimationJump stored AnimationJumpStored nodefault;
    { Animation when character is fall.
      Default 'fall'.}
    property AnimationFall: String read FAnimationFall write FAnimationFall stored AnimationFallStored nodefault;

    { Camera will keep at least this distance from walls. }
    property Radius;

    { How does the avatar change transformation (for movement and rotations).
      This determines whether we update @link(TCastleTransform.Translation),
      @link(TCastleTransform.Rotation) directly or use physics (TCastleRigidBody)
      velocities or forces.

      See TChangeTransformation for possible values are their meaning.

      By default, this is ctAuto, which means that we detect whether you have
      physics behaviors (TCastleRigidBody, TCastleCollider, with TCastleRigidBody.Exists)
      set up on the avatar.

      @unorderedList(
        @item(If yes, we will use physics behaviors, and change transformation
          using the velocity of TCastleRigidBody.)
        @item(Otherwise (if you don't have physics behaviors), we will directly change
          @link(TCastleTransform.Translation), @link(TCastleTransform.Rotation).)
      )
    }
    property ChangeTransformation: TChangeTransformation read FChangeTransformation write FChangeTransformation
      default ctAuto;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlethirdpersonnavigation_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

implementation

uses Math,
  CastleUtils, CastleStringUtils, CastleComponentSerialize, CastleLog, CastleBoxes;

{ TCastleThirdPersonNavigation ----------------------------------------------- }

constructor TCastleThirdPersonNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FCameraFollows := true;
  FAvatarTarget := DefaultAvatarTarget;
  {$ifdef AVATAR_TARGET_FORWARD}
  FAvatarTargetForward := DefaultAvatarTargetForward;
  {$endif}
  FAvatarRotationSpeed := DefaultAvatarRotationSpeed;
  FAimAvatar := aaNone;
  FCameraSpeed := DefaultCameraSpeed;
  FInitialHeightAboveTarget := DefaultInitialHeightAboveTarget;
  FDistanceToAvatarTarget := DefaultDistanceToAvatarTarget;
  FMoveSpeed := DefaultMoveSpeed;
  FCrouchSpeed := DefaultCrouchSpeed;
  FRunSpeed := DefaultRunSpeed;
  FJumpSpeed := DefaultJumpSpeed;
  FRotationSpeed := DefaultRotationSpeed;
  FCameraDistanceChangeSpeed := DefaultCameraDistanceChangeSpeed;
  FMinDistanceToAvatarTarget := DefaultMinDistanceToAvatarTarget;
  FMaxDistanceToAvatarTarget := DefaultMaxDistanceToAvatarTarget;
  FAnimationIdle := DefaultAnimationIdle;
  FAnimationRotate := DefaultAnimationRotate;
  FAnimationWalk := DefaultAnimationWalk;
  FAnimationRun := DefaultAnimationRun;
  FAnimationCrouch := DefaultAnimationCrouch;
  FAnimationCrouchIdle := DefaultAnimationCrouchIdle;
  FAnimationCrouchRotate := DefaultAnimationCrouchRotate;
  FAnimationJump := DefaultAnimationJump;
  FAnimationFall := DefaultAnimationFall;
  ZoomEnabled := true;

  FAvatarFreeObserver := TFreeNotificationObserver.Create(Self);
  FAvatarFreeObserver.OnFreeNotification := {$ifdef FPC}@{$endif}AvatarFreeNotification;
  FAvatarHierarchyFreeObserver := TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification := {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;

  FInput_Forward                 := TInputShortcut.Create(Self);
  FInput_Backward                := TInputShortcut.Create(Self);
  FInput_LeftRotate              := TInputShortcut.Create(Self);
  FInput_RightRotate             := TInputShortcut.Create(Self);
  FInput_LeftStrafe              := TInputShortcut.Create(Self);
  FInput_RightStrafe             := TInputShortcut.Create(Self);
  FInput_Crouch                  := TInputShortcut.Create(Self);
  FInput_Run                     := TInputShortcut.Create(Self);
  FInput_Jump                    := TInputShortcut.Create(Self);

  Input_Forward                 .Assign(keyW, keyArrowUp);
  Input_Backward                .Assign(keyS, keyArrowDown);
  Input_LeftRotate              .Assign(keyArrowLeft, keyA);
  Input_RightRotate             .Assign(keyArrowRight, keyD);
  Input_LeftStrafe              .Assign(keyNone);
  Input_RightStrafe             .Assign(keyNone);
  Input_Crouch                  .Assign(keyCtrl);
  Input_Run                     .Assign(keyShift);
  Input_Jump                    .Assign(keySpace);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_LeftRotate             .SetSubComponent(true);
  Input_RightRotate            .SetSubComponent(true);
  Input_LeftStrafe             .SetSubComponent(true);
  Input_RightStrafe            .SetSubComponent(true);
  Input_Crouch                 .SetSubComponent(true);
  Input_Run                    .SetSubComponent(true);
  Input_Jump                   .SetSubComponent(true);

  Input_Forward                .Name := 'Input_Forward';
  Input_Backward               .Name := 'Input_Backward';
  Input_LeftRotate             .Name := 'Input_LeftRotate';
  Input_RightRotate            .Name := 'Input_RightRotate';
  Input_LeftStrafe             .Name := 'Input_LeftStrafe';
  Input_RightStrafe            .Name := 'Input_RightStrafe';
  Input_Crouch                 .Name := 'Input_Crouch';
  Input_Run                    .Name := 'Input_Run';
  Input_Jump                   .Name := 'Input_Jump';

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlethirdpersonnavigation_persistent_vectors.inc}
  {$undef read_implementation_constructor}

  // override vector change method, to call Init in design mode when this changes
  AvatarTargetPersistent.InternalSetValue := {$ifdef FPC}@{$endif}MySetAvatarTargetForPersistent;

  FChangeTransformation := ctAuto;
  FWasJumpInput := false;
  FAirMovementControl := DefaultAirMovementControl;
  FAirRotationControl := DefaultAirRotationControl;
end;

procedure TCastleThirdPersonNavigation.MySetAvatarTargetForPersistent(const AValue: TVector3);
begin
  SetAvatarTargetForPersistent(AValue);
end;

destructor TCastleThirdPersonNavigation.Destroy;
begin
  { set to nil by SetXxx, to detach free notification }
  Avatar := nil;
  AvatarHierarchy := nil;

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlethirdpersonnavigation_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleThirdPersonNavigation.RealAvatarHierarchy: TCastleTransform;
begin
  if AvatarHierarchy <> nil then
    Result := AvatarHierarchy
  else
    Result := Avatar;
end;

procedure TCastleThirdPersonNavigation.SetAvatar(const Value: TCastleScene);
begin
  if FAvatar <> Value then
  begin
    FAvatar := Value;
    FAvatarFreeObserver.Observed := Value;
    SetAnimationWarningsDone := 0;
  end;
end;

procedure TCastleThirdPersonNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if FAvatarHierarchy <> Value then
  begin
    FAvatarHierarchy := Value;
    FAvatarHierarchyFreeObserver.Observed := Value;
  end;
end;

function TCastleThirdPersonNavigation.CameraPositionInitial(const A: TCastleTransform): TVector3;
var
  TargetWorldPos: TVector3;
begin
  Result := CameraPositionInitial(A, TargetWorldPos); // ignore resulting TargetWorldPos
end;

function TCastleThirdPersonNavigation.ToGravityPlane(const V: TVector3; const GravUp: TVector3): TVector3;
begin
  Result := V;
  if not VectorsParallel(Result, GravUp) then
    MakeVectorsOrthoOnTheirPlane(Result, GravUp);
end;

function TCastleThirdPersonNavigation.CameraPositionInitial(const A: TCastleTransform; out TargetWorldPos: TVector3): TVector3;
var
  GravUp: TVector3;
  TargetWorldDir: TVector3;
  HorizontalShiftFromTarget: Single;
begin
  TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);
  TargetWorldDir := A.WorldTransform.MultDirection(TCastleTransform.DefaultDirection[A.Orientation]);

  if DistanceToAvatarTarget < InitialHeightAboveTarget then
  begin
    WritelnWarning('DistanceToAvatarTarget (%f) should not be smaller than InitialHeightAboveTarget (%f)', [
      DistanceToAvatarTarget,
      InitialHeightAboveTarget
    ]);
    // This effectively assumes that DistanceToAvatarTarget = InitialHeightAboveTarget
    HorizontalShiftFromTarget := 0;
  end else
  begin
    { InitialHeightAboveTarget, HorizontalShiftFromTarget, DistanceToAvatarTarget
      create a right triangle, so
      InitialHeightAboveTarget^2 + HorizontalShiftFromTarget^2 = DistanceToAvatarTarget^2
    }
    HorizontalShiftFromTarget := Sqrt(Sqr(DistanceToAvatarTarget) - Sqr(InitialHeightAboveTarget));
  end;

  GravUp := Camera.GravityUp;

  Result := TargetWorldPos
    + GravUp * InitialHeightAboveTarget
    - ToGravityPlane(TargetWorldDir, GravUp) * HorizontalShiftFromTarget;
end;

procedure TCastleThirdPersonNavigation.FixCameraForCollisions(
  var CameraPos: TVector3; const CameraDir: TVector3);
var
  MaxDistance: Single;
  A: TCastleTransform;
  TargetWorldPos: TVector3;
begin
  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);
    MaxDistance := CameraMaxDistanceToTarget(A, TargetWorldPos, CameraDir);
    if PointsDistanceSqr(CameraPos, TargetWorldPos) > Sqr(MaxDistance) then
      // Note that CameraDir is not necessarily normalized now
      CameraPos := TargetWorldPos - CameraDir.AdjustToLength(MaxDistance);
  end;
end;

function TCastleThirdPersonNavigation.CameraMaxDistanceToTarget(
  const A: TCastleTransform; const CameraLookPos: TVector3;
  const CameraDir: TVector3): Single;
var
  CollisionDistance: Single;
  SavedAPickable: Boolean;
begin
  Result := MaxSingle;
  SavedAPickable := A.Pickable;
  A.Pickable := false;
  try
    if A.World.WorldRayCast(CameraLookPos, -CameraDir, CollisionDistance) <> nil then
    begin
      { Use MinDistanceToAvatarTarget to secure in case wall is closer than Radius
        (CollisionDistance - Radius negative)
        or just to close to head.
        Then use MinDistanceToAvatarTarget. }
      Result := Max(MinDistanceToAvatarTarget, CollisionDistance - Radius);
    end;
  finally A.Pickable := SavedAPickable end;
end;

procedure TCastleThirdPersonNavigation.Init;
var
  GravUp: TVector3;
  A: TCastleTransform;
  CameraPos, CameraDir, CameraUp, TargetWorldPos: TVector3;
begin
  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    if CameraFollows then
    begin
      GravUp := Camera.GravityUp;

      CameraPos := CameraPositionInitial(A, TargetWorldPos);
      CameraDir := TargetWorldPos - CameraPos;
      if CameraDir.IsZero then
      begin
        { This condition didn't occur in actual tests, this is paranoid check. }
        WritelnWarning('Increase DistanceToAvatarTarget (%f) and/or InitialHeightAboveTarget (%f) to make initial camera further from target', [
          DistanceToAvatarTarget,
          InitialHeightAboveTarget
        ]);
        CameraDir := TVector3.One[0];
      end;
      CameraUp := GravUp; // will be adjusted to be orthogonal to Dir by SetView
      FixCameraForCollisions(CameraPos, CameraDir);
      Camera.SetView(CameraPos, CameraDir, CameraUp);
    end;

    SetAnimation([AnimationIdle]);
    if Avatar <> nil then
      Avatar.ForceInitialAnimationPose;
  end;
end;

procedure TCastleThirdPersonNavigation.ProcessMouseLookDelta(const Delta: TVector2);
var
  ToCamera, GravUp: TVector3;
  A: TCastleTransform;

  { Change ToCamera by applying DeltaY from mouse look. }
  procedure ProcessVertical(DeltaY: Single);
  const
    { Do not allow to look exactly up or exactly down,
      as then further vertical moves would be undefined,
      so you would not be able to "get out" of such rotation. }
    MinAngleFromZenith = 0.1;
  var
    Side: TVector3;
    AngleToUp, AngleToDown, MaxChange: Single;
  begin
    Side := -TVector3.CrossProduct(ToCamera, GravUp);
    if DeltaY > 0 then
    begin
      AngleToDown := AngleRadBetweenVectors(ToCamera, -GravUp);
      MaxChange := Max(0, AngleToDown - MinAngleFromZenith);
      if DeltaY > MaxChange then
        DeltaY := MaxChange;
    end else
    begin
      AngleToUp := AngleRadBetweenVectors(ToCamera, GravUp);
      MaxChange := Max(0, AngleToUp - MinAngleFromZenith);
      if DeltaY < -MaxChange then
        DeltaY := -MaxChange;
    end;
    ToCamera := RotatePointAroundAxisRad(DeltaY, ToCamera, Side);
  end;

  procedure ProcessHorizontal(const DeltaX: Single);
  begin
    ToCamera := RotatePointAroundAxisRad(-DeltaX, ToCamera, GravUp);
  end;

var
  CameraPos, CameraDir, CameraUp, TargetWorldPos, LookPos: TVector3;
begin
  inherited;
  if not CameraFollows then
   Exit;

  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    Camera.GetWorldView(CameraPos, CameraDir, CameraUp);
    GravUp := Camera.GravityUp;

    TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);
    // Since camera may update with some delay, we may not look exactly at TargetWorldPos if avatar moved
    LookPos := PointOnLineClosestToPoint(CameraPos, CameraDir, TargetWorldPos);

    ToCamera := CameraPos - LookPos;
    if ToCamera.IsZero then
    begin
      WritelnWarning('TCastleThirdPersonNavigation camera position at look target, increase DistanceToAvatarTarget');
      Exit;
    end;

    ProcessVertical(Delta[1]);
    ProcessHorizontal(Delta[0]);

    CameraPos := LookPos + ToCamera;
    CameraDir := LookPos - CameraPos;
    CameraUp := GravUp; // will be adjusted to be orthogonal to Dir by SetWorldView
    if ImmediatelyFixBlockedCamera then
      FixCameraForCollisions(CameraPos, CameraDir);
    Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
  end;
end;

function TCastleThirdPersonNavigation.Zoom(const Factor: Single): Boolean;
var
  A: TCastleTransform;

  procedure CameraDistanceChange(DistanceChange: Single);
  begin
    DistanceChange := DistanceChange * CameraDistanceChangeSpeed;
    DistanceToAvatarTarget := Clamped(DistanceToAvatarTarget + DistanceChange,
      MinDistanceToAvatarTarget, MaxDistanceToAvatarTarget);

    { The actual change in Camera.Position, caused by changing DistanceToAvatarTarget,
      will be done smoothly in UpdateCamera. }
  end;

begin
  Result := false;
  if not Valid then Exit;

  A := RealAvatarHierarchy;
  if (A <> nil) and (InternalViewport <> nil) then
  begin
    CameraDistanceChange(-Factor);
    Result := true;
  end;
end;

procedure TCastleThirdPersonNavigation.SetAnimation(const AnimationNames: array of String);
const
  MaxSetAnimationWarnings = 10;
var
  AnimName: String;
begin
  if Avatar <> nil then
  begin
    Assert(High(AnimationNames) >= 0); // at least one animation name provided
    for AnimName in AnimationNames do
      if Avatar.HasAnimation(AnimName) then
      begin
        if not CastleDesignMode then
          Avatar.AutoAnimation := AnimName; // do not change serialized AutoAnimation
        Exit;
      end;
    if SetAnimationWarningsDone < MaxSetAnimationWarnings then
    begin
      WritelnWarning('No useful animation exists on the avatar to show in the current state.' +NL +
        'Tried: %s.' +NL +
        'Add the animations to your model, or set the TCastleThirdPersonNavigation.AnimationXxx properties to point to the existing animations.', [
        GlueStrings(AnimationNames, ', ')
      ]);
      Inc(SetAnimationWarningsDone);
      if SetAnimationWarningsDone = MaxSetAnimationWarnings then
        WritelnWarning('Further warnings about avatar animations will not be done, to not flood the log, until you assign new Avatar value');
    end;
  end;
end;

procedure TCastleThirdPersonNavigation.Update(const SecondsPassed: Single;
  var HandleInput: Boolean);
type
  TIsOnGround = (igGround, igJumping, igFalling);
  TSpeedType = (stNormal, stCrouch, stRun);
var
  { Variables useful in all nested routines below }
  A: TCastleTransform;
  RBody: TCastleRigidBody;
  Collider: TCastleCollider;
  Speed: Single;

  { Warn if the avatar has TCastleRigidBody and TCastleCollider and TCastleRigidBody.Exists. }
  procedure CheckNotPhysics;
  begin
    if (RBody <> nil) and (Collider <> nil) and RBody.Exists then
    begin
      if not WarningDonePhysicsNotNecessary then
      begin
        WarningDonePhysicsNotNecessary := true;
        WritelnWarning('For this TCastleThirdPersonNavigation.Transformation, remove physics behaviors (TCastleRigidBody, TCastleCollider) from avatar or set TCastleRigidBody.Exists to false');
      end;
    end;
  end;

  { Realize ctDirect transformation method. }
  procedure DoDirect(var MovingHorizontally, Rotating: Boolean; var IsOnGround: TIsOnGround);
  var
    T: TVector3;
  begin
    CheckNotPhysics;

    T := TVector3.Zero;
    if Input_Forward.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      T := T + A.Direction * Speed * SecondsPassed;
    end;
    if Input_Backward.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      T := T - A.Direction * Speed * SecondsPassed;
    end;
    if Input_RightStrafe.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      T := T + TVector3.CrossProduct(A.Direction, A.Up) * Speed * SecondsPassed;
    end;
    if Input_LeftStrafe.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      T := T - TVector3.CrossProduct(A.Direction, A.Up) * Speed * SecondsPassed;
    end;

    if Input_RightRotate.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      A.Direction := RotatePointAroundAxisRad(-RotationSpeed * SecondsPassed, A.Direction, A.Up);
      { TODO: when AimAvatar, this is overridden by UpdateAimAvatar soon.
        In effect, keys AD don't work when AimAvatar <> aaNone. }
    end;
    if Input_LeftRotate.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      A.Direction := RotatePointAroundAxisRad(RotationSpeed * SecondsPassed, A.Direction, A.Up);
      { TODO: when AimAvatar, this is overridden by UpdateAimAvatar soon.
        In effect, keys AD don't work when AimAvatar <> aaNone. }
    end;

    if not T.IsPerfectlyZero then
      A.Move(T, false);
  end;

  { If avatar does not have TCastleRigidBody and TCastleCollider and TCastleRigidBody.Exists,
    warn and return @false. }
  function CheckPhysics: Boolean;
  begin
    if (RBody = nil) or (not RBody.Exists) then
    begin
      if not WarningDoneRigidBodyNecessary then
      begin
        WarningDoneRigidBodyNecessary := true;
        WritelnWarning('For this TCastleThirdPersonNavigation.Transformation, you must add TCastleRigidBody to the avatar and leave TCastleRigidBody.Exists = true');
      end;
      Exit(false);
    end;

    if Collider = nil then
    begin
      if not WarningDoneColliderNecessary then
      begin
        WarningDoneColliderNecessary := true;
        WritelnWarning('For this TCastleThirdPersonNavigation.Transformation, you must add TCastleCollider to the avatar');
      end;
      Exit(false);
    end;

    Result := true;
  end;

  { Realize ctVelocity transformation method. }
  procedure DoVelocity(var MovingHorizontally, Rotating: Boolean; var IsOnGround: TIsOnGround);
  var
    IsOnGroundBool: Boolean;
    Vel: TVector3;
    VLength: Single;
    AvatarBoundingBox: TBox3D;
    AvatarHeight: Single;
    MaxHorizontalVelocityChange: Single;
    Acceleration: Single;
    HVelocity: TVector3;
    VVelocity: Single;
    MoveDirection: TVector3;
    GroundRayCast: TPhysicsRayCastResult;
    DistanceToGround: Single;
    Jump: Single;
    RayOrigin: TVector3;
    DeltaSpeed: Single;
    DeltaAngular: Single;
  begin
    if not CheckPhysics then
      Exit;

    { How fast should avatar change it's speed }
    Acceleration := Speed * 3 / 60;
    MaxHorizontalVelocityChange := Acceleration * 60;
    DeltaSpeed := 0;

    { Check player is on ground, we use avatar size multiplied by ten to try
      found ground. Distance is used to check we should set animation to fall
      or we are almost on ground so use default animation.

      We need add Collider.Translation because sometimes rigid body origin can be
      under the collider. And ray will be casted under the floor. }
    AvatarBoundingBox := A.BoundingBox;
    AvatarHeight := AvatarBoundingBox.SizeY;
    RayOrigin := A.Translation + Collider.Translation;

    { TODO: In the ideal world, the way we check for ground collisions
      (and determine Ground, IsOnGround)
      should be independent from ChangeTransformation.

      ChangeTransformation says how we change the transformation.

      We should still have option to use

      - PhysicsRayCast (maybe from TCastleAbstractRootTransform, as it should
        not require having TCastleRigidBody on avatar) to detect ground
      - or Height / WorldHeight calls that cooperate with old simple physics.

      And we should update IsOnGround in all ChangeTransformation modes.

      But in practice, now ctDirect forces to do gravity using old physics
      (because it forbids TCastleRigidBody on avatar),
      and ctVelocity forces to do gravity using new physics
      (because it requires TCastleRigidBody on avatar).

      So checking for ground (collisions) is not independent from ChangeTransformation.
      When ctVelocity, we have to check for ground using real physics (PhysicsRayCast),
      it would make no sense to use old simple physics. }

    GroundRayCast := RBody.PhysicsRayCast(
      RayOrigin,
      Vector3(0, -1, 0),
      AvatarHeight * 3
    );

    { Four more checks - player should slide down when player just
      on the edge, but sometimes it stay and center ray don't "see" that we are
      on ground }
    if not GroundRayCast.Hit then
      GroundRayCast := RBody.PhysicsRayCast(
        RayOrigin + Vector3(AvatarBoundingBox.SizeX * 0.49, 0, 0),
        Vector3(0, -1, 0),
        AvatarHeight * 3
      );

    if not GroundRayCast.Hit then
      GroundRayCast := RBody.PhysicsRayCast(
        RayOrigin + Vector3(-AvatarBoundingBox.SizeX * 0.49, 0, 0),
        Vector3(0, -1, 0),
        AvatarHeight * 3
      );

    if not GroundRayCast.Hit then
      GroundRayCast := RBody.PhysicsRayCast(
        RayOrigin + Vector3(0, 0, AvatarBoundingBox.SizeZ * 0.49),
        Vector3(0, -1, 0),
        AvatarHeight * 3
      );

    if not GroundRayCast.Hit then
      GroundRayCast := RBody.PhysicsRayCast(
        RayOrigin + Vector3(0, 0, -AvatarBoundingBox.SizeZ * 0.49),
        Vector3(0, -1, 0),
        AvatarHeight * 3
      );

    if GroundRayCast.Hit then
    begin
      DistanceToGround := GroundRayCast.Distance;

      { When collider has own translation we need substract it from distance
        becouse distance will be too big }
      DistanceToGround  := DistanceToGround - Collider.Translation.Y;

      { Sometimes rigid body center point can be under the collider so
        the distance can be negative }
      if DistanceToGround < 0 then
        DistanceToGround := 0;

      IsOnGroundBool := DistanceToGround < AvatarHeight * 0.1;
    end else
    begin
      IsOnGroundBool := false;
      DistanceToGround := -1; // For animation checking
    end;

    if Input_Forward.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      DeltaSpeed := MaxHorizontalVelocityChange * SecondsPassed * MovementControlFactor(IsOnGroundBool);
      MoveDirection := A.Direction;
    end;
    if Input_Backward.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      DeltaSpeed := MaxHorizontalVelocityChange * SecondsPassed * MovementControlFactor(IsOnGroundBool);
      MoveDirection := -A.Direction;
    end;
    if IsOnGroundBool and Input_RightStrafe.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      DeltaSpeed := MaxHorizontalVelocityChange * SecondsPassed;
      MoveDirection := TVector3.CrossProduct(A.Direction, A.Up);
    end;
    if IsOnGroundBool and Input_LeftStrafe.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      DeltaSpeed := MaxHorizontalVelocityChange * SecondsPassed * MovementControlFactor(IsOnGroundBool);
      MoveDirection := -TVector3.CrossProduct(A.Direction, A.Up);
    end;

    Jump := 0;
    if Input_Jump.IsPressed(Container) and (not FWasJumpInput) and IsOnGroundBool then
    begin
      //if  and (not FWasJumpInput) and IsOnGroundBool
      FWasJumpInput := true;
      MovingHorizontally := false;
      Jump := JumpSpeed;
    end else
      FWasJumpInput := false;

    DeltaAngular := 0;
    if Input_RightRotate.IsPressed(Container) then
    begin
      DeltaAngular := -RotationSpeed * 60 * SecondsPassed * RotationControlFactor(IsOnGroundBool);
    end;
    if Input_LeftRotate.IsPressed(Container) then
    begin
      DeltaAngular := RotationSpeed * 60 * SecondsPassed * RotationControlFactor(IsOnGroundBool);
    end;

    // jumping
    if not IsZero(Jump) then
    begin
      Vel := RBody.LinearVelocity;
      Vel.Y := Jump;
      RBody.LinearVelocity := Vel;
    end else
    // moving
    if not IsZero(DeltaSpeed) then
    begin
      Vel := RBody.LinearVelocity;
      if IsOnGroundBool then
      begin
        { On ground we simply change direction to current one that's
          helps do things like strafe or fast change direction from
          forward to backward }
        HVelocity := Vel;
        HVelocity.Y := 0;
        VVelocity := Vel.Y;
        // maybe use LengthSqrt?
        VLength := HVelocity.Length;
        VLength := VLength + DeltaSpeed;
        if VLength > Speed then
            VLength := Speed;
        Vel := MoveDirection * VLength;

        if IsZero(Jump) then
          Vel.Y := VVelocity
        else
          Vel.Y := Jump;
      end else
      begin
        { In air we can't simply change movement direction, we will just
          modify current one a little based on FAirMovementControl factor.
          Notice that by default FAirMovementControl = 0 so no change
          will be made. }

        Vel := Vel + MoveDirection * DeltaSpeed;

        { Here we only check speed is not faster than max speed }
        HVelocity := Vel;
        HVelocity.Y := 0;
        VVelocity := Vel.Y;
        VLength := HVelocity.Length;
        { Check max speed }
        if VLength > Speed then
        begin
            VLength := Speed;
            Vel.Y := 0;
            Vel := Vel.Normalize * VLength;

            { Add gravity here }
            Vel.Y := VVelocity;
        end;
      end;

      RBody.LinearVelocity := Vel;
    end else
    if IsOnGroundBool then
    begin
      // slowing down the avatar only on ground
      Vel := RBody.LinearVelocity;
      Vel.X := 0;
      Vel.Z := 0;
      RBody.LinearVelocity := Vel;
    end;

    // rotation
    if not IsZero(DeltaAngular) then
    begin
      RBody.AngularVelocity := Vector3(0, 1, 0) * DeltaAngular;
      Rotating := true;
    end
    else
    begin
      RBody.AngularVelocity := Vector3(0, 0, 0);
      Rotating := false;
    end;

    IsOnGround := igGround;
    if not IsOnGroundBool then
    begin
      // TODO: 0.1 should not be hardcoded
      if RBody.LinearVelocity.Y > 0.1 then
        IsOnGround := igJumping
      else
      { When avatar falls we change animation to fall only when the distance
        to ground is smaller than 1/4 of avatar height. This fixes changing
        animation from walk to fall on small steps like in stairs.

        DistanceToGround < 0 means that we are in air and ground
        was not found.

        TODO: 0.25 should not be hardcoded. }
      if (DistanceToGround < 0) or (DistanceToGround > A.LocalBoundingBox.SizeY * 0.25) then
        IsOnGround := igFalling;
    end;
  end;

  {$ifdef CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}
  // TODO: Not finished.
  procedure DoForce(var MovingHorizontally, Rotating: Boolean; var IsOnGround: TIsOnGround);
  var
    DeltaForce: Single;
    Torque: Single;
    MoveDirection: TVector3;
  begin
    if not CheckPhysics then
      Exit;

    DeltaForce := 0;

    if Input_Forward.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      DeltaForce := Speed * 2 * Collider.Mass * SecondsPassed * 60 {* MovementControlFactor(IsOnGround)};
      //WritelnLog('DeltaForce ' + FloatToStr(DeltaForce));
      //MoveDirection := A.WorldToLocal(A.Direction); // for AddCenterForce
      MoveDirection := A.Direction;
    end;

    if Input_Backward.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      DeltaForce := Speed * 2 * Collider.Mass * SecondsPassed * 60 {* MovementControlFactor(IsOnGround)};
      //WritelnLog('DeltaForce ' + FloatToStr(DeltaForce));
      //MoveDirection := A.WorldToLocal(-A.Direction); // for AddCenterForce
      MoveDirection := -A.Direction;
    end;

    Torque := 0;
    if Input_RightRotate.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      Torque := -RotationSpeed * 60 * SecondsPassed {* RotationControlFactor(IsOnGround)};
    end;
    if Input_LeftRotate.IsPressed(Container) then
    begin
      MovingHorizontally := true;
      Torque := RotationSpeed * 60 * SecondsPassed {* RotationControlFactor(IsOnGround)};
    end;

    if not IsZero(Torque) then
    begin
      RBody.AddTorque(A.Up * Torque);
      Rotating := true;
    end else
    if not RBody.AngularVelocity.IsZero(0.1) then
    begin
      Rotating := true;
      { TODO: In case of space ship this is not OK.}
    end else
    begin
      //RBody.AngularVelocity := Vector3(0, 0, 0);
      Rotating := false;
    end;

    if MovingHorizontally then
    begin
      RBody.AddForce(MoveDirection * DeltaForce, true);
      //RBody.ApplyImpulse(MoveDirection * DeltaForce, A.Translation);
      //RBody.ApplyImpulse(MoveDirection * DeltaForce, Collider.Translation);
    end;
  end;
  {$endif CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}

  { Make camera follow the A.Translation.
    Following the character also makes sure that camera stays updated
    (keeps DistanceToAvatarTarget)
    when the avatar is being moved by other routines (e.g. because A.Gravity is working).

    Also avoid camera being blocked by some wall.
    This needs to be redone, in case some wall blocks us e.g. because of collisions.

    Does not follow the perfect location instantly,
    which makes a nice effect when character is moving fast.
    It's inportant to avoid sudden camera moves on sudden avatar moves,
    e.g. changing Y when going up/down stairs. }
  procedure UpdateCamera;
  var
    TargetWorldPos, CameraPos, CameraDir, CameraUp, CameraPosTarget, CameraDirToTarget: TVector3;
    MaxDistance: Single;
  begin
    if not CameraFollows then
      Exit;

    TargetWorldPos := A.WorldTransform.MultPoint(AvatarTarget);

    Camera.GetView(CameraPos, CameraDir, CameraUp);

    { We use CameraDirToTarget, not CameraDir, because (since we update with delay)
      camera may look at a slightly shifted point.
      But we still adjust camera position to look (without blockers) at avatar. }
    CameraDirToTarget := TargetWorldPos - CameraPos;

    { We need to check both CameraPosTarget and final CameraPos for collisions.
      But it would be wasteful to call FixCameraForCollisions 2 times,
      to calculate mostly the same.
      So we use one call to CameraMaxDistanceToTarget. }
    MaxDistance := CameraMaxDistanceToTarget(A, TargetWorldPos, CameraDirToTarget);

    { No need to use CameraDir.AdjustToLength(xxx) as we know CameraDir is normalized.
      Note that this is not entirely correct: we use distance we obtained with CameraDirToTarget,
      but our desired camera direction is CameraDir (i.e. unchanged from current camera direction). }
    CameraPosTarget := TargetWorldPos - CameraDir * Min(MaxDistance, DistanceToAvatarTarget);

    CameraPos := SmoothTowards(CameraPos, CameraPosTarget, SecondsPassed, CameraSpeed);
    if ImmediatelyFixBlockedCamera then
    begin
      if PointsDistanceSqr(CameraPos, TargetWorldPos) > Sqr(MaxDistance) then
        CameraPos := TargetWorldPos - CameraDir * MaxDistance;
    end;

    Camera.SetView(CameraPos, CameraDir, CameraUp);
  end;

  { Rotate avatar if needed by AimAvatar.
    Returns are we rotating now. }
  function UpdateAimAvatar: Boolean;
  const
    AngleEpsilon = 0.01;
  var
    TargetDir: TVector3;
    Angle: Single;
  begin
    Result := false;
    if AimAvatar = aaNone then Exit;

    // calculate TargetDir, in the same coordinate space as A.Direction
    TargetDir := Camera.Direction;
    if AimAvatar = aaHorizontal then
      TargetDir := ToGravityPlane(TargetDir, Camera.GravityUp);
    TargetDir := A.Parent.WorldToLocalDirection(TargetDir);

    Angle := AngleRadBetweenVectors(TargetDir, A.Direction);
    if Angle > AngleEpsilon then
    begin
      MinVar(Angle, AvatarRotationSpeed * SecondsPassed);
      A.Direction := RotatePointAroundAxisRad(Angle, A.Direction, -TVector3.CrossProduct(TargetDir, A.Direction));
      Result := true;
    end;
  end;

  { Call SetAnimation to set proper animation of the avatar (Avatar.AutoAnimation).
    Calls SetAnimation once and exactly once (in all possible branches of this routine). }
  procedure UpdateAnimation(const MovingHorizontally, Rotating: Boolean; const IsOnGround: TIsOnGround;
    const SpeedType: TSpeedType);
  begin
    { TODO: In case we use AimAvatar and you move mouse very slowly for short amounts,
      we may switch very quickly between AnimationIdle and AnimationRotate.
      This makes somewhat bad look in third_person_navigation, and though it uses
      DefaultAnimationTransition <> 0.
      Should we protect from it here, to introduce minimal time to change
      animation between rotating/non-rotating variant? }

    case IsOnGround of
      igJumping: SetAnimation([AnimationJump, AnimationIdle]);
      igFalling: SetAnimation([AnimationFall, AnimationIdle]);
      else
        begin
          if MovingHorizontally then
          begin
            case SpeedType of
              stNormal: SetAnimation([AnimationWalk, AnimationIdle]);
              stRun   : SetAnimation([AnimationRun, AnimationIdle]);
              stCrouch: SetAnimation([AnimationCrouch, AnimationCrouchIdle, AnimationIdle]);
              {$ifndef COMPILER_CASE_ANALYSIS}
              else raise EInternalError.Create('SpeedType?');
              {$endif}
            end;
          end else
          begin
            if SpeedType = stCrouch then
            begin
              if Rotating then
                SetAnimation([AnimationCrouchRotate, AnimationCrouch, AnimationCrouchIdle, AnimationIdle])
              else
                SetAnimation([AnimationCrouchIdle, AnimationCrouch, AnimationIdle]);
            end else
            begin
              { Note that stRun behaves the same as stNormal when Moving = false.
                This just means user holds Shift (Input_Run) but not actually moving by AWSD. }
              if Rotating then
                SetAnimation([AnimationRotate, AnimationWalk, AnimationIdle])
              else
                SetAnimation([AnimationIdle]);
            end;
          end;
        end;
    end;
  end;

var
  SpeedType: TSpeedType;
  MovingHorizontally, Rotating: Boolean;
  IsOnGround: TIsOnGround;
begin
  inherited;
  if not Valid then Exit;

  A := RealAvatarHierarchy;
  RBody := A.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  Collider := A.FindBehavior(TCastleCollider) as TCastleCollider;

  if (A = nil) or (InternalViewport = nil) then
    Exit;

  if Input_Run.IsPressed(Container) then
  begin
    SpeedType := stRun;
    Speed := RunSpeed;
  end else
  if Input_Crouch.IsPressed(Container) then
  begin
    SpeedType := stCrouch;
    Speed := CrouchSpeed;
  end else
  begin
    SpeedType := stNormal;
    Speed := MoveSpeed;
  end;

  MovingHorizontally := false;
  Rotating := false;
  IsOnGround := igGround;

  case FChangeTransformation of
    ctAuto:
      if (RBody <> nil) and RBody.Exists and (Collider <> nil) then
        DoVelocity(MovingHorizontally, Rotating, IsOnGround)
      else
        DoDirect(MovingHorizontally, Rotating, IsOnGround);
    ctDirect: DoDirect(MovingHorizontally, Rotating, IsOnGround);
    ctVelocity: DoVelocity(MovingHorizontally, Rotating, IsOnGround);
    {$ifdef CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}
    ctForce: DoForce(MovingHorizontally, Rotating, IsOnGround);
    {$endif CASTLE_UNFINISHED_CHANGE_TRANSFORMATION_BY_FORCE}
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleThirdPersonNavigation.FTransformation?');
    {$endif}
  end;

  { Note: always execute UpdateAimAvatar, regardless of Rotating.
    So wite "UpdateAimAvatar or Rotating" and not "Rotating or UpdateAimAvatar". }
  Rotating := UpdateAimAvatar or Rotating;

  UpdateAnimation(MovingHorizontally, Rotating, IsOnGround, SpeedType);

  UpdateCamera;
end;

{ Since String values cannot have default properties,
  we use "stored" methods to avoid storing default value. }

function TCastleThirdPersonNavigation.AnimationIdleStored: Boolean;
begin
  Result := FAnimationIdle <> DefaultAnimationIdle;
end;

function TCastleThirdPersonNavigation.AnimationRotateStored: Boolean;
begin
  Result := FAnimationRotate <> DefaultAnimationRotate;
end;

function TCastleThirdPersonNavigation.AnimationWalkStored: Boolean;
begin
  Result := FAnimationWalk <> DefaultAnimationWalk;
end;

function TCastleThirdPersonNavigation.AnimationRunStored: Boolean;
begin
  Result := FAnimationRun <> DefaultAnimationRun;
end;

function TCastleThirdPersonNavigation.AnimationCrouchStored: Boolean;
begin
  Result := FAnimationCrouch <> DefaultAnimationCrouch;
end;

function TCastleThirdPersonNavigation.AnimationCrouchIdleStored: Boolean;
begin
  Result := FAnimationCrouchIdle <> DefaultAnimationCrouchIdle;
end;

function TCastleThirdPersonNavigation.AnimationCrouchRotateStored: Boolean;
begin
  Result := FAnimationCrouchRotate <> DefaultAnimationCrouchRotate;
end;

function TCastleThirdPersonNavigation.AnimationJumpStored: Boolean;
begin
  Result := FAnimationJump <> DefaultAnimationJump;
end;

function TCastleThirdPersonNavigation.AnimationFallStored: Boolean;
begin
  Result := FAnimationFall <> DefaultAnimationFall;
end;

procedure TCastleThirdPersonNavigation.SetInitialHeightAboveTarget(const Value: Single);
begin
  if FInitialHeightAboveTarget <> Value then
  begin
    FInitialHeightAboveTarget := Value;
  end;
end;

procedure TCastleThirdPersonNavigation.SetDistanceToAvatarTarget(const Value: Single);
begin
  if FDistanceToAvatarTarget <> Value then
  begin
    FDistanceToAvatarTarget := Value;
  end;
end;

procedure TCastleThirdPersonNavigation.SetCameraFollows(const Value: Boolean);
begin
  if FCameraFollows <> Value then
  begin
    FCameraFollows := Value;
  end;
end;

function TCastleThirdPersonNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'CameraFollows', 'AvatarTarget', 'Avatar', 'AvatarHierarchy', 'Radius',
       'AimAvatar', 'MoveSpeed', 'CrouchSpeed', 'RunSpeed', 'JumpSpeed', 'RotationSpeed',
       'AirMovementControl', 'AirRotationControl',
       'AnimationIdle', 'AnimationWalk', 'AnimationRun', 'AnimationJump', 'AnimationRotate',
       'AnimationCrouch', 'AnimationCrouchIdle', 'AnimationCrouchRotate','AnimationFall',
       'InitialHeightAboveTarget', 'DistanceToAvatarTarget', 'ChangeTransformation'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleThirdPersonNavigation.AvatarFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Avatar := nil;
end;

procedure TCastleThirdPersonNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy := nil;
end;

function TCastleThirdPersonNavigation.MovementControlFactor(
  const AvatarOnGround: Boolean): Single;
begin
  if AvatarOnGround then
    Exit(1.0)
  else
    Result := FAirMovementControl;
end;

function TCastleThirdPersonNavigation.RotationControlFactor(
  const AvatarOnGround: Boolean): Single;
begin
  if AvatarOnGround then
    Exit(1.0)
  else
    Result := FAirRotationControl;
end;

function TCastleThirdPersonNavigation.Input_CameraCloser: TInputShortcut;
begin
  Result := Input_ZoomIn;
end;

function TCastleThirdPersonNavigation.Input_CameraFurther: TInputShortcut;
begin
  Result := Input_ZoomOut;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlethirdpersonnavigation_persistent_vectors.inc}
{$undef read_implementation_methods}

initialization
  RegisterSerializableComponent(TCastleThirdPersonNavigation, ['Navigation', 'Third-Person']);
end.
