{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Camera and navigation (TCastleCamera, TCastleExamineNavigation, TCastleWalkNavigation). }
unit CastleCameras;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleVectors, CastleUtils, CastleKeysMouse, CastleBoxes, CastleQuaternions,
  CastleFrustum, CastleUIControls, CastleInternalRays, CastleProjection, CastleTimeUtils,
  CastleInputs, CastleTriangles, CastleRectangles, CastleClassUtils,
  CastleInternalCameraGestures;

type
  { Possible navigation input types for @link(TCastleNavigation.Input). }
  TNavigationInput = (
    { Normal input types. This includes all inputs available as
      Input_Xxx properties in TCastleNavigation descendants.
      They are all fully configurable (as TInputShortcut class),
      they may be mouse button presses, mouse wheel clicks, or key presses.
      You can always clear some shortcut (like @code(TCastleWalkNavigation.Input_Forward.MakeClear))
      to disable a specific shortcut.
      Excluding niNormal from TCastleNavigation.Input is an easy way to disable @italic(all)
      shortcuts. }
    niNormal,

    { Mouse and touch dragging. Both TCastleExamineNavigation and TCastleWalkNavigation implement their own,
      special reactions to mouse dragging, that allows to navigate / rotate
      while pressing specific mouse buttons.

      Note that mouse dragging is automatically disabled when
      @link(TCastleWalkNavigation.MouseLook) is used. }
    niMouseDragging,

    { Touch gestures, like multi-touch pinch or pan gesture. }
    niGesture,

    { Navigation using 3D mouse devices, like the ones from 3dconnexion. }
    ni3dMouse
  );
  TNavigationInputs = set of TNavigationInput;

  TCameraInput = TNavigationInput deprecated 'use TNavigationInput';
  TCameraInputs = TNavigationInputs deprecated 'use TNavigationInputs';

  { Navigation type that determines various navigation properties,
    used by @link(TCastleViewport.NavigationType). }
  TNavigationType = (
    { Examine mode, comfortable to rotate the scene like an item held in your hand.
      Uses TCastleExamineNavigation. }
    ntExamine,
    { Turntable mode, similar to examine mode, but with a bit different interpretation
      of moves.
      Uses TCastleExamineNavigation. }
    ntTurntable,
    { Walk mode, comfortable to walk around the scene with gravity.
      Uses TCastleWalkNavigation. }
    ntWalk,
    { Fly mode, comfortable to move around around the scene without gravity.
      Uses TCastleWalkNavigation. }
    ntFly,
    { Disable user navigation on the scene.
      Uses TCastleWalkNavigation. }
    ntNone
  );

  EViewportNotAssigned = class(Exception);

  { Value of @link(TCastlePerspective.FieldOfViewAxis). }
  TFieldOfViewAxis = (
    { @link(TCastlePerspective.FieldOfView)
      specifies the angle along the smaller viewport axis.

      E.g. on a full-screen viewport, on a typical desktop screen,
      with a typical panoramic window (wide, not tall),
      this will determine the vertical axis angle.
      The horizontal axis will be adjusted following the aspect ratio. }
    faSmallest,
    { @link(TCastlePerspective.FieldOfView)
      specifies the angle along the larger viewport axis.
      The other axis will be adjusted, following the aspect ratio. }
    faLargest,
    { @link(TCastlePerspective.FieldOfView)
      specifies the angle along the horizontal axis.
      The vertical axis will be adjusted, following the aspect ratio. }
    faHorizontal,
    { @link(TCastlePerspective.FieldOfView)
      specifies the angle along the vertical axis.
      The horizontal axis will be adjusted, following the aspect ratio. }
    faVertical
  );

  TCastleCamera = class;
  TCastleNavigation = class;

  { Subcomponent used in @link(TCastleCamera.Perspective) to set perspective
    projection parameters.

    Do not create instances of this class yourself,
    these are automatically created by TCastleCamera. }
  TCastlePerspective = class(TCastleComponent)
  strict private
    FFieldOfView: Single;
    FFieldOfViewAxis: TFieldOfViewAxis;
    procedure SetFieldOfView(const Value: Single);
    procedure SetFieldOfViewAxis(const Value: TFieldOfViewAxis);
    function IsStoredFieldOfView: Boolean;
  private
    Camera: TCastleCamera;
  public
    const
      DefaultFieldOfView = Pi / 4;
      DefaultFieldOfViewAxis = faSmallest;

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Perspective field of view angle, in radians.
      The @link(FieldOfViewAxis) determines whether this is horizontal
      or vertical angle. }
    property FieldOfView: Single read FFieldOfView write SetFieldOfView
      stored IsStoredFieldOfView {$ifdef FPC}default DefaultFieldOfView{$endif};

    { Which axis is determined explicitly by @link(FieldOfView).
      @seealso TFieldOfViewAxis }
    property FieldOfViewAxis: TFieldOfViewAxis
      read FFieldOfViewAxis write SetFieldOfViewAxis default DefaultFieldOfViewAxis;
  end;

  { Subcomponent used in @link(TCastleCamera.Orthographic) to set orthographic
    projection parameters.

    Do not create instances of this class yourself,
    these are automatically created by TCastleCamera. }
  TCastleOrthographic = class(TCastleComponent)
  strict private
    FOrigin: TVector2;
    FWidth, FHeight, FScale: Single;
    FEffectiveWidth, FEffectiveHeight: Single;
    FStretch, WarningEffectiveSizeZeroDone: Boolean;
    procedure SetOrigin(const Value: TVector2);
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetScale(const Value: Single);
    procedure SetStretch(const Value: Boolean);
  private
    Camera: TCastleCamera;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Additional translation of the camera.
      The camera movement applied here is always scaled by
      the calculated orthographic projection width and height.

      By default this equals (0,0) which means that TCastleCamera.Position
      determines what is visible in the left-bottom corner of the viewport.
      This matches the typical 2D drawing coordinates used throughout our engine.
      In other words, if the camera is at position (0,0,whatever),
      then the (0,0) position in 2D is in the left-bottom corner of the TCastleViewport.

      You can change it e.g. to (0.5,0.5) to move the camera to
      the middle of the viewport.
      In effect, if the camera is at position (0,0,whatever),
      then the (0,0) position is in the center of the TCastleViewport.

      Both values of @name make sense,
      it depends on the game type and how you prefer to think in 2D coordinates.
      And how do you want the result to behave when aspect ratio changes:

      @unorderedList(
        @item(With (0.5,0.5), things will stay "glued"
          to the center.)
        @item(With (0,0), things will stay "glued"
          to the left-bottom corner.)
      )
    }
    property Origin: TVector2 read FOrigin write SetOrigin;

    procedure InternalSetEffectiveSize(const W, H: Single);

    { Currently used projection width and height, calculated following
      the algorithm described at @link(Width) and @link(Height).
      @groupBegin }
    property EffectiveWidth: Single read FEffectiveWidth;
    property EffectiveHeight: Single read FEffectiveHeight;
    { @groupEnd }
  published
    { Orthographic projection width and height.

      You can leave one or both of them as zero (default) to automatically
      calculate effective projection width and height
      (in @link(EffectiveWidth) and @link(EffectiveHeight)):

      @unorderedList(
        @item(When both @link(Width) and @link(Height) are zero,
          then the effective projection width and height
          are based on the viewport width and height.
          That is, they will follow
          @link(TCastleUserInterface.EffectiveWidth TCastleViewport.EffectiveWidth)
          and
          @link(TCastleUserInterface.EffectiveHeight TCastleViewport.EffectiveHeight).
        )

        @item(When exactly one of @link(Width) and @link(Height) is non-zero,
          then it explicitly determines the projection width or height accordingly.
          This allows to easily display the same piece of the game world,
          regardless of the viewport size.

          The other size is then calculated to follow the aspect ratio
          of the viewport control.
        )

        @item(When both @link(Width) and @link(Height) are non-zero,
          they determine the projection width and height following the algorithm outlined below.
          This also allows to easily display the same piece of the game world,
          regardless of the viewport size.

          @unorderedList(
            @item(When @link(Stretch) = @false (default), they determine the @italic(minimum)
              projection width and height along the given axis.

              If the displayed viewport aspect ratio will be different than given
              @link(Width) and @link(Height) ratio, then these values will be
              treated as minimum values, and they will be adjusted (one of them will be increased)
              for the purposes of rendering.
              You can read the @link(EffectiveWidth), @link(EffectiveHeight) to know
              the adjusted values.

              Note that the @link(TCastleCamera.Position) is considered to be relative
              to unadjusted @link(Width) and @link(Height), not to the adjusted
              @link(EffectiveWidth), @link(EffectiveHeight).
              In effect, when @link(Origin) is zero, the @link(TCastleCamera.Position) does not point
              to the left-bottom of the whole viewport.
              It points to the left-bottom of the rectangle of aspect ratio
              @link(Width) / @link(Height) within the viewport.
              This way the enlarged viewport shows equal amount of additional space on the left and
              the right (or bottom and top) of the @link(Width) / @link(Height) rectangle within.
            )

            @item(When @link(Stretch) = @true, these values are used directly,
              even if it means that aspect ratio of the projection
              will not reflect the aspect ratio of the viewport on screen.

              This allows to implement some tricks, like @italic(Military Projection),
              https://github.com/castle-engine/castle-engine/issues/290 .)
          )
        )
      )

      In all the cases, the resulting size is also multiplied by @link(Scale),
      by default 1.0.

      You can read @link(EffectiveWidth) and @link(EffectiveHeight)
      to learn the actual projection width and height, calculated using
      the above algorithm.

      @groupBegin }
    property Width: Single read FWidth write SetWidth {$ifdef FPC}default 0{$endif};
    property Height: Single read FHeight write SetHeight {$ifdef FPC}default 0{$endif};
    { @groupEnd }

    { Scales the projection size derived from @link(Width) and @link(Height).

      The effect of this scale is also affected by the @link(Origin).
      When @link(Origin) is zero, this behaves like scaling around left-botttom corner
      of the viewport.
      When @link(Origin) is (0.5,0.5), this behaves like scaling around
      the middle of the viewport. }
    property Scale: Single read FScale write SetScale {$ifdef FPC}default 1{$endif};

    { Allow non-proportional stretch of projection.
      In effect the @link(Width) and @link(Height)
      (if both non-zero) are applied directly, without correcting them to follow
      aspect ratio of the viewport. }
    property Stretch: Boolean read FStretch write SetStretch default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  { Camera determines viewer position and orientation in a 3D or 2D world.

    An instance of this class is automatically available
    in @link(TCastleViewport.Camera).
    @italic(Do not instantiate this class yourself.)

    Note that this class does not handle any user input to modify the camera.
    For this, see TCastleNavigation descendants. }
  TCastleCamera = class(TCastleComponent)
  strict private
    FPosition, FDirection, FUp, FGravityUp: TVector3;
    FInitialPosition, FInitialDirection, FInitialUp: TVector3;
    FProjectionMatrix: TMatrix4;
    FProjectionNear, FProjectionFar: Single;
    FEffectiveProjectionNear, FEffectiveProjectionFar: Single;
    FProjectionType: TProjectionType;

    FAnimation: boolean;
    AnimationEndTime: TFloatTime;
    AnimationCurrentTime: TFloatTime;

    AnimationBeginPosition: TVector3;
    AnimationBeginDirection: TVector3;
    AnimationBeginUp: TVector3;
    AnimationEndPosition: TVector3;
    AnimationEndDirection: TVector3;
    AnimationEndUp: TVector3;

    FFrustum: TFrustum;
    FPerspective: TCastlePerspective;
    FOrthographic: TCastleOrthographic;

    procedure RecalculateFrustum;

    procedure SetPosition(const Value: TVector3);
    procedure SetDirection(const Value: TVector3);
    procedure SetUp(const Value: TVector3);
    procedure SetGravityUp(const Value: TVector3);
    procedure SetInitialPosition(const Value: TVector3);
    procedure SetInitialDirection(const Value: TVector3);
    procedure SetInitialUp(const Value: TVector3);

    { Setter of the @link(ProjectionMatrix) property. }
    procedure SetProjectionMatrix(const Value: TMatrix4);

    procedure SetProjectionNear(const Value: Single);
    procedure SetProjectionFar(const Value: Single);
    procedure SetProjectionType(const Value: TProjectionType);
  private
    procedure VisibleChange;
  protected
    procedure Loaded; override;
  public
    { Associated viewport.
      Do not set this directly, instead always set @link(TCastleViewport.Navigation).
      @exclude }
    InternalViewport: TCastleUserInterface;

    InternalOnSceneBoundViewpointChanged,
    InternalOnSceneBoundViewpointVectorsChanged,
    InternalOnSceneBoundNavigationInfoChanged: TNotifyEvent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Express current view as camera vectors: position, direction, up.

      Returned Dir and Up must be orthogonal.
      Returned Dir and Up and GravityUp are already normalized. }
    procedure GetView(out APos, ADir, AUp: TVector3); overload;
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3); overload;

    { Set camera view from vectors: position, direction, up.

      Direction, Up and GravityUp do not have to be normalized,
      we will normalize them internally if necessary.
      But make sure they are non-zero.

      We will automatically fix Direction and Up to be orthogonal, if necessary:
      when AdjustUp = @true (the default) we will adjust the up vector
      (preserving the given direction value),
      otherwise we will adjust the direction (preserving the given up value). }
    procedure SetView(const ADir, AUp: TVector3;
      const AdjustUp: boolean = true); overload;
    procedure SetView(const APos, ADir, AUp: TVector3;
      const AdjustUp: boolean = true); overload;
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3;
      const AdjustUp: boolean = true); overload;

    { Camera position, looking direction and up vector.

      Call @link(GoToInitial) to set the current vectors to initial vectors,
      making them equal to InitialPosition, InitialDirection, InitialUp.
      You can also use @code(Init) method on some navigation descendants
      like @link(TCastleExamineNavigation.Init) and @link(TCastleWalkNavigation.Init).

      The @link(Direction) and @link(Up) vectors should always be normalized
      (have length 1). When setting them by these properties, we will normalize
      them automatically, so their given length is actually ignored.

      When setting @link(Direction), @link(Up) will always be automatically
      adjusted to be orthogonal to @link(Direction). And vice versa ---
      when setting @link(Up), @link(Direction) will be adjusted.

      @groupBegin }
    property Position : TVector3 read FPosition  write SetPosition;
    property Direction: TVector3 read FDirection write SetDirection;
    property Up       : TVector3 read FUp        write SetUp;
    { @groupEnd }

    { Change up vector, keeping the direction unchanged.
      If necessary, the up vector provided here will be fixed to be orthogonal
      to direction.
      See TCastleTransform.UpPrefer for detailed documentation what this does. }
    procedure UpPrefer(const AUp: TVector3);

    { "Up" direction of the world in which player moves.
      Always normalized (when setting this property, we take
      care to normalize the provided vector).

      This determines in which direction @link(TCastleWalkNavigation.Gravity) works.

      This is also the "normal" value for both @link(Up) and
      @link(InitialUp) --- one that means that player is looking
      straight foward. This is used for features like PreferGravityUpForRotations
      and/or PreferGravityUpForMoving.

      The default value of this vector is (0, 1, 0) (same as the default
      @link(Up) and
      @link(InitialUp) vectors). }
    property GravityUp: TVector3 read FGravityUp write SetGravityUp;

    { Camera matrix, transforming from world space into camera space. }
    function Matrix: TMatrix4;

    { Inverse of @link(Matrix), transforming from camera space into world space. }
    function MatrixInverse: TMatrix4;

    { Extract only rotation from your current camera @link(Matrix).
      This is useful for rendering skybox in 3D programs
      (e.g. for VRML/X3D Background node) and generally to transform
      directions between world and camera space.

      It's guaranteed that this is actually only 3x3 matrix,
      the 4th row and 4th column are all zero except the lowest right item
      which is 1.0. }
    function RotationMatrix: TMatrix4;

    { The current camera (viewing frustum, based on
      @link(ProjectionMatrix) (set by you) and @link(Matrix) (calculated here).
      This is recalculated whenever one of these two properties change.
      Be sure to set @link(ProjectionMatrix) before using this. }
    property Frustum: TFrustum read FFrustum;

    { Projection matrix of the camera.
      Camera needs to know this to calculate @link(Frustum),
      which in turn allows rendering code to use frustum culling.

      In normal circumstances, if you use @link(TCastleViewport) for rendering,
      this is automatically correctly set for you. }
    property ProjectionMatrix: TMatrix4
      read FProjectionMatrix write SetProjectionMatrix;

    { Calculate a ray picked by WindowPosition position on the viewport,
      assuming current viewport dimensions are as given.
      This doesn't look at our container sizes at all.

      Projection (read-only here) describe projection,
      required for calculating the ray properly.

      Resulting RayDirection is always normalized.

      WindowPosition is given in the same style as TCastleContainer.MousePosition:
      (0, 0) is bottom-left. }
    procedure CustomRay(
      const ViewportRect: TFloatRectangle;
      const WindowPosition: TVector2;
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3);

    { Animate a camera smoothly into another camera settings.
      This will gradually change our settings (only the most important
      settings, that determine actual camera view, i.e. @link(Matrix) result)
      into another camera.

      Current OtherCamera settings will be internally copied during this call.
      So you can even free OtherCamera instance immediately after calling this.

      Calling AnimateTo while the previous animation didn't finish yet
      is OK. This simply cancels the previous animation,
      and starts the new animation from the current position.

      @groupBegin }
    procedure AnimateTo(const OtherCamera: TCastleCamera;
      const Time: TFloatTime); overload;
    procedure AnimateTo(const APos, ADir, AUp: TVector3;
      const Time: TFloatTime); overload;
    { @groupEnd }

    { Are we currently during animation (caused by @link(AnimateTo)).

      TCastleNavigation descendants may use it to abort normal
      input handling. E.g. when camera is animating,
      then the gravity in TCastleWalkNavigation should not work,
      key/mouse handling in TCastleWalkNavigation shoult not work etc. }
    function Animation: boolean;

    { Initial camera values. Can be set by @link(SetInitialView).
      Camera vectors are reset to these values by @link(GoToInitial).
      You can also use @code(Init) method on some navigation descendants
      like @link(TCastleExamineNavigation.Init) and @link(TCastleWalkNavigation.Init)
      to set initial vectors @italic(and) current vectors at the same time.

      InitialDirection and InitialUp must be always normalized,
      and orthogonal.

      Default value of InitialPosition is (0, 0, 0),
      InitialDirection is DefaultCameraDirection = (0, -1, 0),
      InitialUp is DefaultCameraUp = (0, 1, 0).

      @groupBegin }
    property InitialPosition : TVector3 read FInitialPosition  write SetInitialPosition;
    property InitialDirection: TVector3 read FInitialDirection write SetInitialDirection;
    property InitialUp       : TVector3 read FInitialUp        write SetInitialUp;
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
      const AInitialPosition: TVector3;
      AInitialDirection, AInitialUp: TVector3;
      const TransformCurrentCamera: boolean);

    { Jump to initial camera view (set by SetInitialView). }
    procedure GoToInitial;

    { Set the initial camera vectors (InitialPosition, InitialDirection...),
      and set current camera vectors (Position, Direction...) to the same values.

      Given here AInitialDirection, AInitialUp, AGravityUp will be normalized,
      and AInitialUp will be adjusted to be orthogonal to AInitialDirection
      (see SetInitialView). }
    procedure Init(const AInitialPosition, AInitialDirection, AInitialUp,
      AGravityUp: TVector3);

    { Called by TCastleViewport to make @link(AnimateTo) working.
      Internal. @exclude }
    procedure Update(const SecondsPassed: Single);

    procedure Free; deprecated 'do not Free camera instance explicitly, only the TCastleViewport should create and destroy TCastleViewport.Camera; this method does nothing now';

    { Currently used projection near.
      Derived from @link(ProjectionNear) and possibly scene sizes. }
    property EffectiveProjectionNear: Single read FEffectiveProjectionNear;
    { Currently used projection far.
      Derived from @link(ProjectionFar) and possibly scene sizes.
      May be equal ZFarInfinity. }
    property EffectiveProjectionFar: Single read FEffectiveProjectionFar;
    // @exclude
    procedure InternalSetEffectiveProjection(
      const AEffectiveProjectionNear, AEffectiveProjectionFar: Single);
  published
    { Projection near plane distance.

      For perspective projection, values <= 0 are invalid,
      and imply that a suitable value should actually be automatically
      calculated (looking at the world size).

      For orthographic projection, all values are valid and reasonable,
      including 0 and < 0 values. }
    property ProjectionNear: Single read FProjectionNear write SetProjectionNear {$ifdef FPC}default 0{$endif};

    { Projection far plane distance.
      Use 0 to auto-calculate this each frame, based on world bounding box.
      If shadow volumes are used, this may be overridden to be infinite. }
    property ProjectionFar: Single read FProjectionFar write SetProjectionFar {$ifdef FPC}default 0{$endif};

    { Perspective or orthographic projection.
      Depending on it, we either use @link(Perspective) or @link(Orthographic) settings. }
    property ProjectionType: TProjectionType
      read FProjectionType write SetProjectionType default ptPerspective;

    { Perspective projection properties, used only if @link(ProjectionType) is ptPerspective. }
    property Perspective: TCastlePerspective read FPerspective;

    { Orthographic projection properties, used only if @link(ProjectionType) is ptOrthographic. }
    property Orthographic: TCastleOrthographic read FOrthographic;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCameraClass = class of TCamera;
    // deprecated 'use "class of TCastleNavigation"';
    // using deprecated (without a String too) breaks Lazarus Code Tools now

  { }
  T3BoolInputs = array [0..2, boolean] of TInputShortcut;

  { See @link(TCastleNavigation.MoveAllowed) and
    @link(TCastleNavigation.OnMoveAllowed) }
  TMoveAllowedFunc = function (const Sender: TCastleNavigation;
    const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
    const Radius: Single; const BecauseOfGravity: Boolean): boolean of object;

  { See @link(TCastleNavigation.OnFall). }
  TFallNotifyFunc = procedure (const Sender: TCastleNavigation;
    const FallHeight: Single) of object;

  { See @link(TCastleNavigation.OnInternalHeight). }
  THeightEvent = function (const Sender: TCastleNavigation;
    const Position: TVector3;
    out AboveHeight: Single; out AboveGround: PTriangle): Boolean of object;

  { Handle user input to modify viewport's camera.

    Create an instance of this class, and set it as @link(TCastleViewport.Navigation) value.
    It will become a child control of the associated @link(TCastleViewport).

    In many ways, this is just a normal @link(TCastleUserInterface) descendant.
    E.g. it processes input just like any other @link(TCastleUserInterface) descendant
    (there isn't any special mechanism through which @link(TCastleViewport) passes
    input to the navigation),
    the @link(Exists) property works and so on.
    Setting it as @link(TCastleViewport.Navigation)
    (as opposed to just adding it manually by @link(TCastleUserInterface.InsertFront)
    to the viewport) serves just two purposes: we set internal link to the viewport,
    and we make sure to remove previous @link(TCastleViewport.Navigation) value
    from children.

    The point of the above explanation is that you can modify
    @link(TCastleViewport.Camera) (move, rotate and do other stuff with camera)
    from any place in the code.
    You don't @italic(need) to use an ancestor of TCastleNavigation to manipulate
    the camera. TCastleNavigation is a comfortable way to encapsulate
    common navigation methods, but it's not the only way to move the camera.

    Various TCastleNavigation descendants implement various navigation
    methods, for example TCastleExamineNavigation allows the user to rotate
    and scale the model (imagine that you're holding a 3D model in your
    hands and you look at it from various sides) and TCastleWalkNavigation
    implements typical navigation in the style of first-person shooter
    games. }
  TCastleNavigation = class(TCastleUserInterface)
  private
    FInput: TNavigationInputs;
    FRadius: Single;
    FPreferredHeight: Single;
    FMoveHorizontalSpeed, FMoveVerticalSpeed, FMoveSpeed: Single;
    FHeadBobbing: Single;
    FHeadBobbingTime: Single;
    FClimbHeight: Single;
    FModelBox: TBox3D;
    FCrouchHeight: Single;
    FOnMoveAllowed, FOnInternalMoveAllowed: TMoveAllowedFunc;
    FOnInternalHeight: THeightEvent;
    FOnFall: TFallNotifyFunc;

    function GetPosition: TVector3;
    function GetDirection: TVector3;
    function GetUp: TVector3;
    function GetGravityUp: TVector3;
    procedure SetPosition(const Value: TVector3);
    procedure SetDirection(const Value: TVector3);
    procedure SetUp(const Value: TVector3);
    procedure SetGravityUp(const Value: TVector3);
    function GetProjectionMatrix: TMatrix4;
    procedure SetProjectionMatrix(const Value: TMatrix4);
    function GetFrustum: TFrustum;
    function GoodModelBox: TBox3D;
    function GetIgnoreAllInputs: boolean;
    procedure SetIgnoreAllInputs(const Value: boolean);
  protected
    { Needed for niMouseDragging navigation.
      Checking MouseDraggingStarted means that we handle only dragging that
      was initialized on viewport (since the viewport passed events to navigation).
      MouseDraggingStarted -1 means none, otherwise it's the finder index
      (to support multitouch). }
    MouseDraggingStarted: Integer;
    MouseDraggingStart: TVector2;

    { Behave as if @link(Input) is like this.
      This allows to disable input on paused viewport. }
    function UsingInput: TNavigationInputs;

    function ReallyEnableMouseDragging: boolean; virtual;

    { Check collisions to determine how high above ground is given point.
      Calls OnInternalHeight callback. }
    procedure Height(const APosition: TVector3;
      out AIsAbove: Boolean;
      out AnAboveHeight: Single; out AnAboveGround: PTriangle);

    { Check collisions to determine can the object move.
      Object wants to move from OldPos to ProposedNewPos.

      Returns @false if no move is allowed.
      Otherwise returns @true and sets NewPos to the position
      where user should be moved.

      If you're doing a simple
      check for collisions, you will always
      want to set NewPos to ProposedNewPos when returning @true.

      But you can also do more sophisticated calculations and
      sometimes not allow user to move to ProposedNewPos, but allow
      him to move instead to some other close position.
      For example when doing "wall sliding" (common in FPS games):
      when you're trying to walk "into the wall", you move along the wall instead.

      It's allowed to modify NewPos when returning @false.
      It makes no effect.

      BecauseOfGravity says whether this move is caused by gravity
      dragging the player down. You can use BecauseOfGravity e.g. to implement
      @link(TCastleViewport.PreventInfiniteFallingDown).

      Implementation calls OnMoveAllowed and OnInternalMoveAllowed. }
    function MoveAllowed(
      const OldPos: TVector3; ProposedNewPos: TVector3; out NewPos: TVector3;
      const Radius: Single; const BecauseOfGravity: Boolean): Boolean;
  public
    const
      { Default value for TCastleNavigation.Radius.
        Matches the default VRML/X3D NavigationInfo.avatarSize[0]. }
      DefaultRadius = 0.25;
      { Default value for TCastleNavigation.PreferredHeight.
        Matches the default VRML/X3D NavigationInfo.avatarSize[1]. }
      DefaultPreferredHeight = 1.6;
      DefaultInput = [niNormal, niMouseDragging, ni3dMouse, niGesture];
      DefaultHeadBobbingTime = 0.5;
      DefaultHeadBobbing = 0.02;
      DefaultCrouchHeight = 0.5;

    var
      { Associated viewport.
        Do not set this directly, instead always set @link(TCastleViewport.Navigation).
        @exclude }
      InternalViewport: TCastleUserInterface;

    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Used by @link(MoveAllowed), see there for description.
      You can assign this property. }
    property OnMoveAllowed: TMoveAllowedFunc read FOnMoveAllowed write FOnMoveAllowed;

    { Used by @link(MoveAllowed), see there for description.
      This property is used internally by TCastleViewport. }
    property OnInternalMoveAllowed: TMoveAllowedFunc
      read FOnInternalMoveAllowed write FOnInternalMoveAllowed;

    { Assign here the callback to check collisions
      and determine what is the current height of position above the ground.
      This should be calculated like collision of ray from @link(Position)
      in direction -GravityUp with the world.
      See @link(TCastleTransform.Height) for specification what returned parameters
      mean. }
    property OnInternalHeight: THeightEvent read FOnInternalHeight write FOnInternalHeight;

    { Notification that we have been falling down for some time due to gravity,
      and suddenly stopped (which means we "hit the ground").

      This event can be useful in games, for example to lower player's health,
      and/or make a visual effect (like a "red out" indicating pain)
      and/or make a sound effect ("Ouch!" or "Thud!" or such sounds).
      You can look at FallHeight parameter, given to the callback,
      e.g. to gauge how much health decreases. }
    property OnFall: TFallNotifyFunc read FOnFall write FOnFall;

    { Associated TCastleCamera of the viewport.
      @raises EViewportNotAssigned If Viewport not assigned yet. }
    function Camera: TCastleCamera;

    { Camera matrix, transforming from world space into camera space. }
    function Matrix: TMatrix4; deprecated 'use Viewport.Camera.Matrix';

    { Inverse of @link(Matrix), transforming from camera space into world space. }
    function MatrixInverse: TMatrix4; deprecated 'use Viewport.Camera.MatrixInverse';

    { Extract only rotation from your current camera @link(Matrix).
      This is useful for rendering skybox in 3D programs
      (e.g. for VRML/X3D Background node) and generally to transform
      directions between world and camera space.

      It's guaranteed that this is actually only 3x3 matrix,
      the 4th row and 4th column are all zero except the lowest right item
      which is 1.0. }
    function RotationMatrix: TMatrix4; deprecated 'use Viewport.Camera.RotationMatrix';

    {$ifdef FPC}
    { Deprecated, use more flexible @link(Input) instead.
      @code(IgnoreAllInputs := true) is equivalent to @code(Input := []),
      @code(IgnoreAllInputs := false) is equivalent to @code(Input := DefaultInput).
      @deprecated }
    property IgnoreAllInputs: boolean
      read GetIgnoreAllInputs write SetIgnoreAllInputs default false; deprecated;

    { Things related to frustum ---------------------------------------- }

    { The current camera viewing frustum, based on
      @link(ProjectionMatrix) (set by the outside) and @link(Matrix) (calculated here).
      This is recalculated whenever one of these two properties change.
      Be sure to set @link(ProjectionMatrix) before using this. }
    property Frustum: TFrustum read GetFrustum; deprecated 'use Viewport.Camera.Frustum';

    { Projection matrix of the camera.
      Camera needs to know this to calculate @link(Frustum),
      which in turn allows rendering code to use frustum culling.

      In normal circumstances, if you use TCastleViewport for rendering,
      this is automatically correctly set. }
    property ProjectionMatrix: TMatrix4
      read GetProjectionMatrix write SetProjectionMatrix; deprecated 'use Viewport.Camera.ProjectionMatrix';
    {$endif FPC}

    { The radius of a sphere around the camera
      that makes collisions with the world.

      @unorderedList(
        @item(Collision detection routines use this.)
        @item(It determines the projection near plane (that must be slightly
          smaller than this radius), see also @link(TCastleCamera.ProjectionNear).)
        @item(
          Walk navigation uses this for automatically correcting
          PreferredHeight, otherwise weird things could happen
          if your avatar height is too small compared to the camera radius.
          See @link(CorrectPreferredHeight).

          Especially useful if you let
          user change PreferredHeight at runtime by
          Input_IncreasePreferredHeight, Input_DecreasePreferredHeight.
        )
      ) }
    property Radius: Single read FRadius write FRadius {$ifdef FPC}default DefaultRadius{$endif};

    { Express current view as camera vectors: position, direction, up.

      Returned Dir and Up must be orthogonal.
      Returned Dir and Up and GravityUp are already normalized. }
    procedure GetView(out APos, ADir, AUp: TVector3); overload;
      deprecated 'use Viewport.Camera.GetView';
    procedure GetView(out APos, ADir, AUp, AGravityUp: TVector3); overload;
      deprecated 'use Viewport.Camera.GetView';

    { Set camera view from vectors: position, direction, up.

      Direction, Up and GravityUp do not have to be normalized,
      we will normalize them internally if necessary.
      But make sure they are non-zero.

      We will automatically fix Direction and Up to be orthogonal, if necessary:
      when AdjustUp = @true (the default) we will adjust the up vector
      (preserving the given direction value),
      otherwise we will adjust the direction (preserving the given up value). }
    procedure SetView(const APos, ADir, AUp: TVector3;
      const AdjustUp: boolean = true); overload;
      deprecated 'use Viewport.Camera.SetView';
    procedure SetView(const APos, ADir, AUp, AGravityUp: TVector3;
      const AdjustUp: boolean = true); overload;
      deprecated 'use Viewport.Camera.SetView';

    {$ifdef FPC}
    { Camera position, looking direction and up vector.

      Initially (after creating this object) they are equal to
      InitialPosition, InitialDirection, InitialUp.
      Also @link(GoToInitial) methods reset them to these
      initial values.

      The @link(Direction) and @link(Up) vectors should always be normalized
      (have length 1). When setting them by these properties, we will normalize
      them automatically, so their given length is actually ignored.

      When setting @link(Direction), @link(Up) will always be automatically
      adjusted to be orthogonal to @link(Direction). And vice versa ---
      when setting @link(Up), @link(Direction) will be adjusted.

      @groupBegin }
    property Position : TVector3 read GetPosition  write SetPosition; deprecated 'use Viewport.Camera.Position';
    property Direction: TVector3 read GetDirection write SetDirection; deprecated 'use Viewport.Camera.Direction';
    property Up       : TVector3 read GetUp        write SetUp; deprecated 'use Viewport.Camera.Up';
    { @groupEnd }

    { "Up" direction of the world in which player moves.
      Always normalized (when setting this property, we take
      care to normalize the provided vector).

      This determines in which direction @link(TCastleWalkNavigation.Gravity) works.

      This is also the "normal" value for both @link(Up) and
      @link(InitialUp) --- one that means that player is looking
      straight foward. This is used for features like PreferGravityUpForRotations
      and/or PreferGravityUpForMoving.

      The default value of this vector is (0, 1, 0) (same as the default
      @link(Up) and
      @link(InitialUp) vectors). }
    property GravityUp: TVector3 read GetGravityUp write SetGravityUp; deprecated 'use Viewport.Camera.GravityUp';
    {$endif FPC}

    { Calculate a 3D ray picked by the WindowX, WindowY position on the window.

      Uses current container size, which means that it assumes that viewport
      fills the whole window,
      and requires that it's added to @link(TCastleViewport.Navigation)
      and this @link(TCastleViewport) must be part of
      TCastleWindow.Controls or TCastleControl.Controls.

      Projection (read-only here) describe your projection,
      required for calculating the ray properly.
      Resulting RayDirection is always normalized.

      WindowPosition is given in the same style as TCastleContainer.MousePosition:
      (0, 0) is bottom-left. }
    procedure Ray(const WindowPosition: TVector2;
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3); deprecated 'use Viewport.Camera.CustomRay with proper viewport sizes, or use higher-level utilities like Viewport.MouseRayHit instead';

    { Calculate a ray picked by current mouse position on the window.

      Uses current container size, which means that it assumes that viewport
      fills the whole window,
      and requires that it's added to @link(TCastleViewport.Navigation)
      and this @link(TCastleViewport) must be part of
      TCastleWindow.Controls or TCastleControl.Controls.

      @seealso Ray
      @seealso CustomRay }
    procedure MouseRay(
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3); deprecated 'use Viewport.Camera.CustomRay with proper viewport sizes, or use higher-level utilities like Viewport.MouseRayHit instead';

    { Calculate a ray picked by WindowPosition position on the viewport,
      assuming current viewport dimensions are as given.
      This doesn't look at our container sizes at all.

      Projection (read-only here) describe projection,
      required for calculating the ray properly.

      Resulting RayDirection is always normalized.

      WindowPosition is given in the same style as TCastleContainer.MousePosition:
      (0, 0) is bottom-left. }
    procedure CustomRay(
      const ViewportRect: TRectangle;
      const WindowPosition: TVector2;
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3); overload;
      deprecated 'use Viewport.Camera.CustomRay';
    procedure CustomRay(
      const ViewportRect: TFloatRectangle;
      const WindowPosition: TVector2;
      const Projection: TProjection;
      out RayOrigin, RayDirection: TVector3); overload; deprecated 'use Viewport.Camera.CustomRay';

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;

    procedure AnimateTo(const OtherCamera: TCastleCamera; const Time: TFloatTime); overload; deprecated 'use Viewport.Camera.AnimateTo';
    procedure AnimateTo(const OtherNavigation: TCastleNavigation; const Time: TFloatTime); overload; deprecated 'use AnimateTo with TCastleCamera, not TCastleNavigation';
    procedure AnimateTo(const APos, ADir, AUp: TVector3; const Time: TFloatTime); overload; deprecated 'use Viewport.Camera.AnimateTo';
    function Animation: boolean; deprecated 'use Viewport.Camera.Animation';
    function InitialPosition : TVector3; deprecated 'use Viewport.Camera.InitialPosition';
    function InitialDirection: TVector3; deprecated 'use Viewport.Camera.InitialDirection';
    function InitialUp       : TVector3; deprecated 'use Viewport.Camera.InitialUp';
    procedure SetInitialView(
      const AInitialPosition: TVector3;
      AInitialDirection, AInitialUp: TVector3;
      const TransformCurrentCamera: boolean);
      deprecated 'use Viewport.Camera.SetInitialView';
    procedure GoToInitial; deprecated 'use Viewport.Camera.GoToInitial';

    { By default this returns ntNone.
      Internal navigation descendants can return something else,
      to cooperate with @link(TCastleViewport.NavigationType).
      Your custom navigation descendants can just return ntNone. }
    function GetNavigationType: TNavigationType; virtual;

    { Height above the ground, only used by @link(TCastleWalkNavigation) descendant
      when @link(TCastleWalkNavigation.Gravity) is @true.
      The @link(Position) tries to stay PreferredHeight above the ground.
      Temporarily it may still be lower (e.g. player can
      shortly "duck" when he falls from high).

      This must always be >= 0.
      You should set this to something greater than zero to get sensible
      behavior of some things related to @link(TCastleWalkNavigation.Gravity).

      See CorrectPreferredHeight for important property
      of PreferredHeight that you should keep. }
    property PreferredHeight: Single
      read FPreferredHeight write FPreferredHeight {$ifdef FPC}default DefaultPreferredHeight{$endif};

    { Correct PreferredHeight based on @link(Radius)
      and on current @link(HeadBobbing).

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
      would not allow it).

      This method will make sure that this condition
      holds by eventually adjusting (making larger) PreferredHeight.
      Note that for Radius = 0.0 this will always leave
      PreferredHeight as it is. }
    procedure CorrectPreferredHeight;

    { Preferred height when crouching.
      This is always mutiplied to PreferredHeight.
      This should always be <= 1 (CrouchHeight = 1 effectively disables
      crouching, although it's better to do this by calling MakeClear
      on Input_Crouch). }
    property CrouchHeight: Single
      read FCrouchHeight write FCrouchHeight {$ifdef FPC}default DefaultCrouchHeight{$endif};

    { When @link(TCastleWalkNavigation) moves, it may make a "head bobbing" effect,
      by moving the camera a bit up and down.

      This property mutiplied by PreferredHeight
      says how much head bobbing can move you along GravityUp.
      Set this to 0 to disable head bobbing.
      This must always be < 1.0. For sensible effects, this should
      be rather close to 0.0, for example 0.02.

      This is meaningfull only when @link(TCastleWalkNavigation.Gravity) works. }
    property HeadBobbing: Single
      read FHeadBobbing write FHeadBobbing {$ifdef FPC}default DefaultHeadBobbing{$endif};

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
      {$ifdef FPC}default DefaultHeadBobbingTime{$endif};

    { Moving speeds, only used by @link(TCastleWalkNavigation) descendant.
      MoveHorizontalSpeed is only for horizontal movement,
      MoveVerticalSpeed is only for vertical, and MoveSpeed simply affects
      both types of movement. Effectively, we always scale the speed
      of movement by either @code(MoveHorizontalSpeed * MoveSpeed) or
      @code(MoveVerticalSpeed * MoveSpeed).

      We move by distance @code(MoveSpeed * MoveHorizontalSpeed (or MoveVerticalSpeed))
      during one second.
      So if you leave MoveHorizontalSpeed = MoveVerticalSpeed = 1 (as default),
      MoveSpeed expresses the speed in nice units / per second.

      Default values for all these speed properties is 1.0,
      so you simply move by 1 unit per second.

      @groupBegin }
    property MoveHorizontalSpeed: Single
      read FMoveHorizontalSpeed write FMoveHorizontalSpeed {$ifdef FPC}default 1.0{$endif};
    property MoveVerticalSpeed: Single
      read FMoveVerticalSpeed write FMoveVerticalSpeed {$ifdef FPC}default 1.0{$endif};
    property MoveSpeed: Single read FMoveSpeed write FMoveSpeed {$ifdef FPC}default 1.0{$endif};
    { @groupEnd }

    { The tallest height that you can climb,
      only used by @link(TCastleWalkNavigation) descendant
      when @link(TCastleWalkNavigation.Gravity) is @true.
      This is checked in each single horizontal move when @link(TCastleWalkNavigation.Gravity) works.
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
      See @link(TCastleWalkNavigation.JumpMaxHeight) for a way to control jumping.

      For a 100% reliable way to prevent user from reaching some point,
      that does not rely on specific navigation settings,
      you should build actual walls in 3D (invisible walls
      can be created by Collision.proxy in VRML/X3D). }
    property ClimbHeight: Single read FClimbHeight write FClimbHeight;

    { Approximate size of 3D world that is viewed,
      used by @link(TCastleExamineNavigation) descendant.
      Determines speed of movement and zooming.
      Initially this is TBox3D.Empty. }
    property ModelBox: TBox3D read FModelBox write FModelBox;

    { Input methods available to user. See documentation of TNavigationInput
      type for possible values and their meaning.

      To disable any user interaction with this navigation
      you can simply set this to empty.
      You can also leave @link(TCastleViewport.Navigation) as @nil. }
    property Input: TNavigationInputs read FInput write FInput default DefaultInput;
  published
    // By default this captures events from whole parent, which should be whole Viewport.
    property FullSize default true;
  end;

  { Navigate the 3D model in examine mode, like you would hold
    a box with the model inside. }
  TCastleExamineNavigation = class(TCastleNavigation)
  private
    type
      { Camera pos/dir/up expressed as vectors more comfortable
        for Examine methods. }
      TExamineVectors = record
        Translation: TVector3;
        Rotations: TQuaternion;
      end;

    var
      FMoveEnabled: Boolean;
      FRotationEnabled: Boolean;
      FZoomEnabled: Boolean;
      FDragMoveSpeed, FKeysMoveSpeed: Single;
      FExactMovement: Boolean;
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
      FRotationsAnim: TVector3;
      FScaleFactorMin, FScaleFactorMax: Single;
      FRotationAccelerate: boolean;
      FRotationAccelerationSpeed: Single;
      FRotationSpeed: Single;
      FTurntable: boolean;
      FPinchGestureRecognizer: TCastlePinchPanGestureRecognizer;

      FInputs_Move: T3BoolInputs;
      FInputs_Rotate: T3BoolInputs;
      FInput_ScaleLarger: TInputShortcut;
      FInput_ScaleSmaller: TInputShortcut;
      FInput_Home: TInputShortcut;
      FInput_StopRotating: TInputShortcut;

      FMouseButtonRotate, FMouseButtonMove, FMouseButtonZoom: TCastleMouseButton;

    procedure SetRotationsAnim(const Value: TVector3);
    function GetRotations: TQuaternion;
    procedure SetRotations(const Value: TQuaternion);
    function GetScaleFactor: Single;
    procedure SetScaleFactor(const Value: Single);
    procedure SetScaleFactorMin(const Value: Single);
    procedure SetScaleFactorMax(const Value: Single);
    function GetTranslation: TVector3;
    procedure SetTranslation(const Value: TVector3);
    { Negative Factor makes "zoom out", positive makes "zoom on",
      zero makes nothing. }
    function Zoom(const Factor: Single): boolean;
    procedure SetRotationAccelerate(const Value: boolean);
    procedure OnGestureRecognized(Sender: TObject);

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

    { Center of rotation and scale, relative to @link(Translation). }
    function CenterOfRotation: TVector3;

    function GetExamineVectors: TExamineVectors;
    procedure SetExamineVectors(const Value: TExamineVectors);
    property ExamineVectors: TExamineVectors read GetExamineVectors write SetExamineVectors;
  public
    const
      DefaultRotationAccelerationSpeed = 5.0;
      DefaultRotationSpeed = 2.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;

    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; override;
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; override;

    { Drag with this mouse button to rotate the model.

      The default values for MouseButtonRotate (left),
      MouseButtonMove (middle), MouseButtonZoom (right)
      were chosen to 1. match as much as possible behavior
      of other programs (like Blender) and
      2. be accessible to all users (e.g. not everyone has middle
      mouse button in comfortable place, like laptop+touchpad users).
      See castlecameras_default_examine_mouse_buttons.txt for more in-depth analysis.

      Also note that for this navigation:

      @unorderedList(
        @item(pressing left mouse button with Ctrl is considered like
          pressing right mouse button,)
        @item(pressing left mouse button with Shift is considered like
          pressing middle mouse button.)
      )
    }
    property MouseButtonRotate: TCastleMouseButton
      read FMouseButtonRotate write FMouseButtonRotate default buttonLeft;
    { Drag with this mouse button to move the model. }
    property MouseButtonMove: TCastleMouseButton
      read FMouseButtonMove write FMouseButtonMove default buttonMiddle;
    { Drag with this mouse button to zoom the model (look closer / further). }
    property MouseButtonZoom: TCastleMouseButton
      read FMouseButtonZoom write FMouseButtonZoom default buttonRight;

    { Current rotation of the model.
      Rotation is done around ModelBox middle (with @link(Translation) added). }
    property Rotations: TQuaternion read GetRotations write SetRotations;

    { Continuous rotation animation, applied each Update to Rotations. }
    property RotationsAnim: TVector3 read FRotationsAnim write SetRotationsAnim;

    { How fast user moves the scene by mouse/touch dragging. }
    property DragMoveSpeed: Single read FDragMoveSpeed write FDragMoveSpeed {$ifdef FPC}default 1.0{$endif};

    { How fast user moves the scene by pressing keys. }
    property KeysMoveSpeed: Single read FKeysMoveSpeed write FKeysMoveSpeed {$ifdef FPC}default 1.0{$endif};

    {$ifdef FPC}
    property MoveAmount: TVector3 read GetTranslation write SetTranslation;
      deprecated 'use Translation';
    {$endif}

    { How much to move the model. By default, zero. }
    property Translation: TVector3 read GetTranslation write SetTranslation;

    { Turntable rotates the scene around its Y axis instead of current camera axis. }
    property Turntable: boolean
      read FTurntable write FTurntable default false;

    {$ifdef FPC}
    { Scale the projection size. }
    property ScaleFactor: Single
      read GetScaleFactor write SetScaleFactor default 1;
      deprecated 'use Camera.Orthographic.Scale';
    {$endif}
    property ScaleFactorMin: Single
      read FScaleFactorMin write SetScaleFactorMin {$ifdef FPC}default 0.01{$endif};
    property ScaleFactorMax: Single
      read FScaleFactorMax write SetScaleFactorMax {$ifdef FPC}default 100.0{$endif};

    { Initialize most important properties of this class:
      sets ModelBox and goes to a nice view over the entire scene.

      In other words, this is just a shortcut to setting ModelBox,
      setting suitable initial view by SetInitialView,
      and then going to initial view by GoToInitial. }
    procedure Init(const AModelBox: TBox3D; const ARadius: Single);
      deprecated 'use Viewport.Camera.Init, and set ModelBox, Radius manually';

    { Methods performing navigation.
      Usually you want to just leave this for user to control. --------------- }

    { Sets RotationsAnim to zero, stopping the rotation of the model. }
    function StopRotating: boolean;

    procedure Move(coord: integer; const MoveDistance: Single); deprecated 'set Translation instead of using this method';

    { User inputs ------------------------------------------------------------ }

    { Alternative ways to access Input_Move/Rotate(X|Y|Z)(Inc|Dec).
      Index the array (2nd index true means increase) instead of having
      to use the full identifier.
      @groupBegin }
    property Inputs_Move: T3BoolInputs read FInputs_Move;
    property Inputs_Rotate: T3BoolInputs read FInputs_Rotate;
    { @groupEnd }

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

    {$ifdef FPC}
    { @Deprecated Include/exclude niMouseDragging from @link(Input) instead. }
    property MouseNavigation: boolean
      read GetMouseNavigation write SetMouseNavigation default true; deprecated;
    {$endif}

    { Speed to change the rotation acceleration,
      used when RotationAccelerate = @true. }
    property RotationAccelerationSpeed: Single
      read FRotationAccelerationSpeed
      write FRotationAccelerationSpeed
      {$ifdef FPC}default DefaultRotationAccelerationSpeed{$endif};

    { Speed to change the rotation, used when RotationAccelerate = @false. }
    property RotationSpeed: Single
      read FRotationSpeed
      write FRotationSpeed
      {$ifdef FPC}default DefaultRotationSpeed{$endif};
  published
    { Enable rotating the camera around the model by user input.
      When @false, no keys / mouse dragging / 3D mouse etc. can cause a rotation.

      Note that this doesn't prevent from rotating by code, e.g. by setting
      @link(Rotations) property or calling @link(SetView). }
    property RotationEnabled: Boolean read FRotationEnabled write FRotationEnabled default true;

    { Enable moving the camera by user input.
      When @false, no keys / mouse dragging / 3D mouse etc. can make a move.

      Note that this doesn't prevent from moving by code, e.g. by setting
      @link(Translation) property or calling @link(SetView). }
    property MoveEnabled: Boolean read FMoveEnabled write FMoveEnabled default true;

    { Enable zooming the camera on the model by user input.
      Depending on the projection, zooming either moves camera or scales
      the projection size.
      When @false, no keys / mouse dragging / 3d mouse etc. can make a zoom.

      Note that this doesn't prevent from zooming by code, e.g. by setting
      @link(ScaleFactor) property (to scale the projection size)
      or calling @link(SetView) (to move closer to the model). }
    property ZoomEnabled: Boolean read FZoomEnabled write FZoomEnabled default true;

    { When @true, rotation keys make the rotation faster, and the model keeps
      rotating even when you don't hold any keys. When @false, you have to
      hold rotation keys to rotate. }
    property RotationAccelerate: boolean
      read FRotationAccelerate write SetRotationAccelerate default true;

    { In orthographic projection with standard direction/up,
      move the camera exactly as many units as the mouse position change indicates.
      Makes the movemement in standard orthographic view most natural. }
    property ExactMovement: Boolean read FExactMovement write FExactMovement default true;
  end;

  { Navigation most suitable for 2D viewports
    (with orthographic projection and standard direction/up: -Z/+Y). }
  TCastle2DNavigation = class(TCastleExamineNavigation)
  public
    constructor Create(AOwner: TComponent); override;

    property MouseButtonMove default buttonLeft;
    property MouseButtonZoom default buttonMiddle;
  published
    property RotationEnabled default false;
  end;

  TCastleWalkNavigation = class;

  { What mouse dragging does in TCastleWalkNavigation. }
  TMouseDragMode = (
    { Moves avatar continuously in the direction of mouse drag
      (default for TCastleWalkNavigation.MouseDragMode). }
    mdWalk,
    { Rotates the head when mouse is moved. }
    mdRotate,
    { Ignores the dragging. }
    mdNone);

  { Abstract navigation class that can utilize @italic(mouse look),
    during which mouse cursor is hidden and we look at MouseLookDelta every frame. }
  TCastleMouseLookNavigation = class(TCastleNavigation)
  strict private
    FMouseLookHorizontalSensitivity: Single;
    FMouseLookVerticalSensitivity: Single;
    FInvertVerticalMouseLook: boolean;
    FMouseLook: boolean;
    procedure SetMouseLook(const Value: boolean);
  protected
    function UsingMouseLook: Boolean;
    procedure ProcessMouseLookDelta(const Delta: TVector2); virtual;
  public
    const
      DefaultMouseLookHorizontalSensitivity = Pi * 0.1 / 180;
      DefaultMouseLookVerticalSensitivity = Pi * 0.1 / 180;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Motion(const Event: TInputMotion): boolean; override;

    { Use mouse look to navigate (rotate the camera).

      This also makes mouse cursor of Container hidden, and forces
      mouse position to the middle of the window
      (to avoid the situation when mouse movement is blocked by screen borders).

      Setting this property at design-time (in CGE editor) does not activate
      the mouse look in CGE editor.
      It only controls the mouse look once the application is running. }
    property MouseLook: boolean read FMouseLook write SetMouseLook default false;

    { Mouse look sensitivity, if @link(MouseLook) is working.
      These properties specify how much angle change is produced by moving mouse by 1 pixel.
      @groupBegin }
    property MouseLookHorizontalSensitivity: Single
      read FMouseLookHorizontalSensitivity write FMouseLookHorizontalSensitivity
      {$ifdef FPC}default DefaultMouseLookHorizontalSensitivity{$endif};
    property MouseLookVerticalSensitivity: Single
      read FMouseLookVerticalSensitivity write FMouseLookVerticalSensitivity
      {$ifdef FPC}default DefaultMouseLookVerticalSensitivity{$endif};
    { @groupEnd }

    { If this is @true and MouseLook works, then the meaning of vertical mouse
      movement is inverted: when user moves mouse up, he looks down.
      Some players are more comfortable with such configuration. }
    property InvertVerticalMouseLook: boolean
      read FInvertVerticalMouseLook write FInvertVerticalMouseLook
      default false;
  end;

  { Navigation by walking or flying (classic first-person shooter navigation)
    in a 3D scene.
    User can rotate and move camera using various keys, like arrows or AWSD.
    Mouse dragging and mouse look are also supported. }
  TCastleWalkNavigation = class(TCastleMouseLookNavigation)
  strict private
    FRotationHorizontalSpeed, FRotationVerticalSpeed: Single;
    FRotationHorizontalPivot: Single;
    FPreferGravityUpForRotations: boolean;
    FPreferGravityUpForMoving: boolean;
    FIsAbove: boolean;
    FAboveHeight: Single;
    FAboveGround: PTriangle;
    FMouseDragMode: TMouseDragMode;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_RightRotate: TInputShortcut;
    FInput_LeftRotate: TInputShortcut;
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

    FMinAngleFromGravityUp: Single;

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

    procedure RotateAroundGravityUp(const Angle: Single);
    procedure RotateAroundUp(const Angle: Single);
    procedure RotateHorizontal(const Angle: Single);
    procedure RotateVertical(AngleRad: Single);

    { Like Move, but you pass here final ProposedNewPos. }
    function MoveTo(const ProposedNewPos: TVector3;
      const BecauseOfGravity, CheckClimbHeight: boolean): boolean;
    { Try to move from current Position to Position + MoveVector.
      Checks MoveAllowed, also (if CheckClimbHeight is @true)
      checks the ClimbHeight limit.

      Returns @false if move was not possible and Position didn't change.
      Returns @true is some move occured (but don't assume too much:
      possibly we didn't move to exactly Position + MoveVector
      because of wall sliding). }
    function Move(const MoveVector: TVector3;
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
    procedure RotateHorizontalForStrafeMove(const Angle: Single);

    { Call always after horizontal rotation change.
      This will return new Position, applying effect of RotationHorizontalPivot. }
    function AdjustPositionForRotationHorizontalPivot(
      const OldDirection, NewDirection: TVector3): TVector3;

    { Jump.

      Returns if a jump was actually done. For example, you cannot
      jump when there's no gravity, or you're already in the middle
      of the jump. Can be useful to determine if key was handled and such. }
    function Jump: boolean;
  private
    { Private things related to gravity ---------------------------- }

    FFalling: boolean;
    FFallingStartPosition: TVector3;
    FFallSpeedStart: Single;
    FFallSpeed: Single;
    FFallSpeedIncrease: Single;
    FGravity: boolean;
    FGrowSpeed: Single;
    { This is used by FallingEffect to temporary modify Matrix result
      by rotating Up around Direction. In degress. }
    Fde_UpRotate: Single;
    { This is used by FallingEffect to consistently rotate us.
      This is either -1, 0 or +1. }
    Fde_RotateHorizontal: Integer;
    FFallingEffect: boolean;

    FJumpMaxHeight: Single;
    FIsJumping: boolean;
    FJumpHeight: Single;
    FJumpTime: Single;
    FJumpHorizontalSpeedMultiply: Single;

    HeadBobbingPosition: Single;
    function UseHeadBobbing: boolean;
    class procedure CreateComponentFly(Sender: TObject);
  private
    FIsCrouching: boolean;

    FFallingOnTheGround: boolean;
    FFallingOnTheGroundAngleIncrease: boolean;

    FIsOnTheGround: boolean;
    FIsWalkingOnTheGround: boolean;

    FMouseDraggingHorizontalRotationSpeed, FMouseDraggingVerticalRotationSpeed: Single;
    FMouseDraggingMoveSpeed: Single;

    function RealPreferredHeightNoHeadBobbing: Single;
    function RealPreferredHeightMargin: Single;
  protected
    function ReallyEnableMouseDragging: boolean; override;
    procedure ProcessMouseLookDelta(const Delta: TVector2); override;
  public
    const
      DefaultFallSpeedStart = 0.5;
      DefaultGrowSpeed = 1.0;
      DefaultJumpMaxHeight = 1.0;
      DefaultMinAngleFromGravityUp = Pi * 10 / 180;
      DefaultRotationHorizontalSpeed = Pi * 150 / 180;
      DefaultRotationVerticalSpeed = Pi * 100 / 180;
      DefaultFallSpeedIncrease = 13/12;
      DefaultJumpHorizontalSpeedMultiply = 2.0;
      DefaultJumpTime = 1.0 / 8.0;
      DefaultMouseDraggingHorizontalRotationSpeed = Pi * 0.1 / 180;
      DefaultMouseDraggingVerticalRotationSpeed = Pi * 0.1 / 180;
      DefaultMouseDraggingMoveSpeed = 0.01;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function AllowSuspendForInput: boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; override;
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; override;

    { If PreferGravityUpForRotations or PreferGravityUpForMoving
      then various operations are done with respect
      to GravityUp, otherwise they are done with
      respect to current @link(Up).

      With PreferGravityUpForRotations, this affects rotations:
      horizontal rotations (Input_LeftRotate and Input_RightRotate)
      and rotations caused by MouseLook.
      Also vertical rotations are bounded by MinAngleFromGravityUp
      when PreferGravityUpForRotations.

      Note that you can change it freely at runtime,
      and when you set PreferGravityUpForRotations from @false to @true
      then in nearest Update
      calls @link(Up) will be gradually fixed, so that @link(Direction) and @link(Up)
      and GravityUp are on the same plane. Also @link(Direction) may be adjusted
      to honour MinAngleFromGravityUp.

      With PreferGravityUpForMoving, this affects moving:
      horizontal moving (forward, backward, strafe),
      and vertical moving (Input_Jump and Input_Crouch when @link(Gravity) is @false).
      E.g. when PreferGravityUpForMoving then forward/backward keys are tied
      to horizontal plane defined by GravityUp.
      When not PreferGravityUpForMoving then forward/backward try to move
      you just in the @link(Direction). Which is usually more handy when
      e.g. simulating flying.

      @unorderedList(
        @item(
          When there is no "natural" up-or-down feeling in the scene,
          e.g. outer space environment without any gravity,
          then you @bold(may) set
          PreferGravityUpForRotations as @false and you @bold(should)
          leave PreferGravityUpForMoving and @link(Gravity) to @false.
        )

        @item(
          With PreferGravityUpForRotations the "feeling" of GravityUp
          is stronger. Raising/bowing the head doesn't mess with "the general
          sense that there's some vertical axis independent of my movement,
          that doesn't change, and affects how I move".

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
    function DirectionInGravityPlane: TVector3;

    { Set the most important properties of this navigation, in one call.
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
      AInitialUp: TVector3;
      const AGravityUp: TVector3;
      const APreferredHeight: Single;
      const ARadius: Single); overload;
      deprecated 'use Viewport.Camera.Init, and set PreferredHeight, Radius and call CorrectPreferredHeight manually';

    { Alternative Init that sets camera properties such that
      an object inside Box is more or less "visible good".
      Sets InitialCameraXxx properties to make it look right,
      sets current CameraXxx properties to InitialCameraXxx.
      Sets GravityUp to the same thing as InitialUp.
      Sets also PreferredHeight to make it behave "sensibly". }
    procedure Init(const box: TBox3D; const ARadius: Single); overload;
      deprecated 'use Viewport.Camera.Init, and set PreferredHeight, Radius and call CorrectPreferredHeight manually';

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
    property MinAngleFromGravityUp: Single
      read FMinAngleFromGravityUp write FMinAngleFromGravityUp
      {$ifdef FPC}default DefaultMinAngleFromGravityUp{$endif};

    function Motion(const Event: TInputMotion): boolean; override;

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
      {$ifdef FPC}default DefaultFallSpeedStart{$endif};

    { When falling down, the speed increases.
      Set this to 1.0 to fall down with constant speed
      (taken from FallSpeedStart). }
    property FallSpeedIncrease: Single
      read FFallSpeedIncrease write FFallSpeedIncrease
      {$ifdef FPC}default DefaultFallSpeedIncrease{$endif};

    { Are we currently falling down because of gravity. }
    property Falling: boolean read FFalling write FFalling;

    { If Falling, then this will force Falling to false
      @bold(without calling OnFallenDown). It's much like forcing
      the opinion that "camera is not falling down right now".

      Note that if we will find out (e.g. in nearest @link(Update))
      that camera is still too high above the ground,
      then we will start falling down again, setting @link(Falling)
      back to true. (but then we will start falling down from the beginning
      with initial falling down speed).

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
      {$ifdef FPC}default DefaultGrowSpeed{$endif};

    { How high can you jump ?
      The max jump distance is calculated as
      JumpMaxHeight * PreferredHeight, see MaxJumpDistance. }
    property JumpMaxHeight: Single
      read FJumpMaxHeight write FJumpMaxHeight
      {$ifdef FPC}default DefaultJumpMaxHeight{$endif};

    { Returns just JumpMaxHeight * PreferredHeight,
      see JumpMaxHeight for explanation. }
    function MaxJumpDistance: Single;

    { We are in the middle of a "jump" move right now. }
    property IsJumping: boolean read FIsJumping;

    { Scales the speed of horizontal moving during jump. }
    property JumpHorizontalSpeedMultiply: Single
      read FJumpHorizontalSpeedMultiply write FJumpHorizontalSpeedMultiply
      {$ifdef FPC}default DefaultJumpHorizontalSpeedMultiply{$endif};

    { How fast do you jump up. This is the time, in seconds, in takes
      to reach MaxJumpDistance height when jumping. }
    property JumpTime: Single read FJumpTime write FJumpTime
      {$ifdef FPC}default DefaultJumpTime{$endif};

    { Is player crouching right now. }
    property IsCrouching: boolean read FIsCrouching;

    { The PreferredHeight slightly modified by head bobbing
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

    function GetNavigationType: TNavigationType; override;

    { Change up vector, keeping the direction unchanged.
      If necessary, the up vector provided here will be fixed to be orthogonal
      to direction.
      See TCastleTransform.UpPrefer for detailed documentation what this does. }
    procedure UpPrefer(const AUp: TVector3); deprecated 'use Viewport.Camera.UpPrefer';

    { Last known information about whether camera is over the ground.
      Updated by using @link(Height) call. For normal TCastleNavigation descendants,
      this means using OnInternalHeight callback,
      which is handled by @link(TCastleViewport) if you assigned @link(TCastleViewport.Navigation)
      to this navigation.

      These are updated continuously only when @link(Gravity) is @true.

      We do not (and, currently, cannot) track here if
      AboveGround pointer will be eventually released (which may happen
      if you release your 3D scene, or rebuild scene causing octree rebuild).
      This is not a problem for navigation class, since we do not use this
      pointer for anything. But if you use this pointer,
      then you may want to take care to eventually set it to @nil when
      your octree or such is released.

      @groupBegin }
    property IsAbove: boolean read FIsAbove;
    property AboveHeight: Single read FAboveHeight;
    property AboveGround: PTriangle read FAboveGround write FAboveGround;
    { @groupEnd }

    { TODO: Input_Xxx not published. See TCastleExamineNavigation Input_Xxx notes
      for reasoning. }
    { }
    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_LeftRotate: TInputShortcut read FInput_LeftRotate;
    property Input_RightRotate: TInputShortcut read FInput_RightRotate;
    {$ifdef FPC}
    property Input_LeftRot: TInputShortcut read FInput_LeftRotate; deprecated 'use Input_LeftRotate';
    property Input_RightRot: TInputShortcut read FInput_RightRotate; deprecated 'use Input_RightRotate';
    {$endif}
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

    { If @true then all rotation keys
      (Input_RightRotate, Input_LeftRotate, Input_UpRotate, Input_DownRotate)
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

    { Horizontal rotation can rotate around a vector that is RotationHorizontalPivot units
      forward before the camera. This is a poor-mans way to implement some 3rd camera game.
      Note that when non-zero this may (for now) move the camera without actually checking
      OnMoveAllowed. }
    property RotationHorizontalPivot: Single
      read FRotationHorizontalPivot write FRotationHorizontalPivot
      {$ifdef FPC}default 0{$endif};
  published
    property MouseLook;
    property MouseLookHorizontalSensitivity;
    property MouseLookVerticalSensitivity;
    property InvertVerticalMouseLook;

    { Rotation keys speed, in radians per second.
      @groupBegin }
    property RotationHorizontalSpeed: Single
      read FRotationHorizontalSpeed write FRotationHorizontalSpeed
      {$ifdef FPC}default DefaultRotationHorizontalSpeed{$endif};

    property RotationVerticalSpeed: Single
      read FRotationVerticalSpeed write FRotationVerticalSpeed
      {$ifdef FPC}default DefaultRotationVerticalSpeed{$endif};
    { @groupEnd }

    { Speed (radians per pixel delta) of rotations by mouse dragging.
      Relevant only if niMouseDragging in @link(Input), and MouseDragMode is mdRotate or mdWalk.
      Separate for horizontal and vertical, this way you can e.g. limit
      (or disable) vertical rotations, useful for games where you mostly
      look horizontally and accidentally looking up/down is more confusing
      than useful.
      @groupBegin }
    property MouseDraggingHorizontalRotationSpeed: Single
      read FMouseDraggingHorizontalRotationSpeed write FMouseDraggingHorizontalRotationSpeed
      {$ifdef FPC}default DefaultMouseDraggingHorizontalRotationSpeed{$endif};
    property MouseDraggingVerticalRotationSpeed: Single
      read FMouseDraggingVerticalRotationSpeed write FMouseDraggingVerticalRotationSpeed
      {$ifdef FPC}default DefaultMouseDraggingVerticalRotationSpeed{$endif};
    { @groupEnd }

    { Moving speed when mouse dragging.
      Relevant only when @code((MouseDragMode is mdWalk) and (niMouseDragging in UsingInput)). }
    property MouseDraggingMoveSpeed: Single
      read FMouseDraggingMoveSpeed write FMouseDraggingMoveSpeed
      {$ifdef FPC}default DefaultMouseDraggingMoveSpeed{$endif};

    { What mouse dragging does. Used only when niMouseDragging in @link(Input). }
    property MouseDragMode: TMouseDragMode
      read FMouseDragMode write FMouseDragMode default mdWalk;

    { This unlocks a couple of features and automatic behaviors
      related to gravity. Gravity always drags the camera down to
      -GravityUp.

      Summary of things done by gravity:
      @unorderedList(
        @item(It uses OnInternalHeight to get camera height above the ground.)
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
      sensible in all cases. The most important property you need to set yourself is PreferredHeight.
      Everything else should basically work auto-magically.

      Note that Gravity setting is independent from
      PreferGravityUpForRotations or PreferGravityUpForMoving settings ---
      PreferGravityUpXxx say how the player controls work,
      Gravity says what happens to player due to ... well, due to gravity. }
    property Gravity: boolean
      read FGravity write FGravity default true;

    property PreferredHeight;
    property MoveHorizontalSpeed;
    property MoveVerticalSpeed;
    property MoveSpeed;
    property Radius;
  end;

  TUniversalCamera = TCastleNavigation deprecated 'complicated TUniversalCamera class is removed; use TCastleNavigation as base class, or TCastleWalkNavigation or TCastleExamineNavigation for particular type, and Viewport.NavigationType to switch type';

  TCamera = TCastleNavigation deprecated 'use TCastleNavigation';
  TExamineCamera = TCastleExamineNavigation deprecated 'use TCastleExamineNavigation';
  TWalkCamera = TCastleWalkNavigation deprecated 'use TCastleWalkNavigation';

{ See TCastleWalkNavigation.CorrectPreferredHeight.
  This is a global version, sometimes may be useful. }
procedure CorrectPreferredHeight(var PreferredHeight: Single;
  const Radius: Single; const CrouchHeight, HeadBobbing: Single);

const
  { Default camera direction and up vectors, used to define the meaning
    of "camera orientation" for @link(OrientationFromDirectionUp),
    @link(OrientationToDirection), @link(OrientationToUp).
    These match X3D default camera values.
    @groupBegin }
  DefaultCameraDirection: TVector3 = (X: 0; Y: 0; Z: -1);
  DefaultCameraUp       : TVector3 = (X: 0; Y: 1; Z: 0);
  { @groupEnd }

  ciNormal        = niNormal        deprecated 'use niNormal';
  ciMouseDragging = niMouseDragging deprecated 'use niMouseDragging';
  ciGesture       = niGesture       deprecated 'use niGesture';
  ci3dMouse       = ni3dMouse       deprecated 'use ni3dMouse';

{ Convert camera direction and up vectors into a rotation (X3D "orientation" vector).

  Orientation vector expresses Direction and Up as a rotation.
  First three components of the resulting vector are the Axis (normalized) and the
  4th component is the Angle (in radians). If you would rotate the standard
  direction and up (see DefaultCameraDirection, DefaultCameraUp) around Axis
  by the Angle, then you would get Direction and Up back.

  There is an overloaded version where you can pass your custom
  DefaultDirection, DefaultUp to be used instead of
  default DefaultCameraDirection, DefaultCameraUp.

  Given here Direction and Up must be orthogonal and non-zero.
  Their lengths are not relevant (that is, you don't need to normalize them
  before passing here).

  @groupBegin }
function OrientationFromDirectionUp(const Direction, Up: TVector3): TVector4; overload;
procedure OrientationFromDirectionUp(const Direction, Up: TVector3;
  out Axis: TVector3; out Angle: Single); overload;
function OrientationFromDirectionUp(const Direction, Up: TVector3;
  const DefaultDirection, DefaultUp: TVector3): TVector4; overload;
{ @groupEnd }

{ Convert rotation (X3D orientation) to a direction vector,
  reversing the @link(OrientationFromDirectionUp). }
function OrientationToDirection(const OrientationRotation: TVector4): TVector3;

{ Convert rotation (X3D orientation) to an up vector,
  reversing the @link(OrientationFromDirectionUp). }
function OrientationToUp(const OrientationRotation: TVector4): TVector3;

function CamDirUp2Orient(const Direction, Up: TVector3): TVector4; overload;
  deprecated 'use OrientationFromDirectionUp';
procedure CamDirUp2Orient(const Direction, Up: TVector3;
  out Axis: TVector3; out Angle: Single); overload;
  deprecated 'use OrientationFromDirectionUp';

{ Convert camera direction and up vectors into a "rotation quaternion".
  Just like OrientationFromDirectionUp, but the result is a quaternion,
  not an axis-angle vector.
  @groupBegin }
function OrientationQuaternionFromDirectionUp(Direction, Up: TVector3;
  const DefaultDirection, DefaultUp: TVector3): TQuaternion; overload;
function OrientationQuaternionFromDirectionUp(const Direction,
  Up: TVector3): TQuaternion; overload;
{ @groupEnd }

function CamDirUp2OrientQuat(const Direction, Up: TVector3): TQuaternion;
  deprecated 'OrientationQuaternionFromDirectionUp';

{ Calculate sensible camera configuration to see the whole Box.

  WantedDirection and WantedUp indicate desired look direction/up axis
  (0, 1 or 2 for X, Y or Z). WantedDirectionPositive and WantedUpPositive
  indicate if we want the positive axis. Obviously look direction and up
  cannot be parallel, so WantedDirection must be different than WantedUp.

  Returned Direction, Up, GravityUp are normalized. }
procedure CameraViewpointForWholeScene(const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean;
  out Position, Direction, Up, GravityUp: TVector3);

{ Calculate suitable camera to see everything using an orthographic projection.

  Assumes that the camera direction is -Z, and camera up is +Y.
  So the horizontal axis of the world is X,
  vertical axis is Y.
  These are default values of camera set by @link(TCastleViewport.Setup2D).

  The meaning of Origin is the same as @link(TCastleOrthographic.Origin).

  Returns new correct values of
  @link(TCastleOrthographic.Width),
  @link(TCastleOrthographic.Height),
  @link(TCastleCamera.ProjectionFar) and camera position
  (set it like @code(Viewport.Camera.Position := NewPosition;)).
}
procedure CameraOrthoViewpointForWholeScene(const Box: TBox3D;
  const ViewportWidth, ViewportHeight: Single;
  const Origin: TVector2;
  out Position: TVector3;
  out AProjectionWidth, AProjectionHeight, AProjectionFar: Single);

{ TODO: move these consts somewhere more private? }

const
  { Following X3D spec of NavigationType:
    "It is recommended that the near clipping plane be set to one-half
    of the collision radius as specified in the avatarSize field." }
  RadiusToProjectionNear = 0.6;

  { Used when it is necessary to calculate projection far based on world size. }
  WorldBoxSizeToProjectionFar = 20.0;

  { Multiply radius by this to get sensible "preferred height".

    We need to make "preferred height" much larger than Radius * 2, to allow some
    space to decrease (e.g. by Input_DecreasePreferredHeight).
    Remember that CorrectPreferredHeight
    adds a limit to PreferredHeight, around Radius * 2.

    This determines minimal PreferredHeight, it should be used always like
      Max(DefaultPreferredHeight, Radius * RadiusToPreferredHeightMin)
    This way, in case of models that are small, but still follow the standard "1 unit = 1 meter",
    the PreferredHeight will not get weirdly small, it will be DefaultPreferredHeight.
    Testcase: examples/third_person_navigation/data/level/level-dungeon.gltf open with view3dscene.
  }
  RadiusToPreferredHeightMin = 4.0;

  { Multiply world bounding box AverageSize by this to get sensible radius. }
  WorldBoxSizeToRadius = 0.005;

implementation

uses Math,
  CastleStringUtils, CastleLog, CastleViewport,
  CastleComponentSerialize;

{ TCastle2DNavigation -------------------------------------------------------- }

constructor TCastle2DNavigation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  RotationEnabled := false;
  MouseButtonMove := buttonLeft;
  MouseButtonZoom := buttonMiddle;
end;

{ TCastlePerspective --------------------------------------------------------- }

constructor TCastlePerspective.Create(AOwner: TComponent);
begin
  inherited;
  FFieldOfView := DefaultFieldOfView;
  FFieldOfViewAxis := DefaultFieldOfViewAxis;
end;

procedure TCastlePerspective.SetFieldOfView(const Value: Single);
begin
  if FFieldOfView <> Value then
  begin
    FFieldOfView := Value;
    Camera.VisibleChange;
  end;
end;

procedure TCastlePerspective.SetFieldOfViewAxis(const Value: TFieldOfViewAxis);
begin
  if FFieldOfViewAxis <> Value then
  begin
    FFieldOfViewAxis := Value;
    Camera.VisibleChange;
  end;
end;

function TCastlePerspective.IsStoredFieldOfView: Boolean;
begin
  { Seems like this is the only way to avoid always serializing FieldOfView.
    Possibly displaying it in object inspector always modifies it a bit,
    due to rounding when displaying? }
  Result := not SameValue(FFieldOfView, DefaultFieldOfView);
end;

function TCastlePerspective.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'FieldOfView') or
     (PropertyName = 'FieldOfViewAxis') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TCastleOrthographic --------------------------------------------------------- }

constructor TCastleOrthographic.Create(AOwner: TComponent);
begin
  inherited;
  FScale := 1;
  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleOrthographic.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleOrthographic.SetOrigin(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FOrigin, Value) then
  begin
    FOrigin := Value;
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.SetWidth(const Value: Single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.SetHeight(const Value: Single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.SetScale(const Value: Single);
begin
  if FScale <> Value then
  begin
    if Value <= 0 then
      WritelnWarning('Orthographic projection scale (Camera.Orthographic.Scale) should be > 0, but is being set to %f', [
        Value
      ]);
    FScale := Value;
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Camera.VisibleChange;
  end;
end;

procedure TCastleOrthographic.InternalSetEffectiveSize(const W, H: Single);
begin
  if ((W <= 0) or (H <= 0)) and (not WarningEffectiveSizeZeroDone) then
  begin
    WritelnWarning('Orthographic projection effective width and height (calculated based on Camera.Orthographic.Width,Height,Scale and viewport size) should be > 0, but are %f x %f (further warnings about it will be supressed, to not spam log)', [
      W, H
    ]);
    WarningEffectiveSizeZeroDone := true;
  end;
  FEffectiveWidth := W;
  FEffectiveHeight := H;
end;

function TCastleOrthographic.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Width') or
     (PropertyName = 'Height') or
     (PropertyName = 'OriginPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleorthographic_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleCamera -------------------------------------------------------------- }

constructor TCastleCamera.Create(AOwner: TComponent);
begin
  inherited;
  FProjectionType := ptPerspective;
  FInitialPosition  := TVector3.Zero;
  FInitialDirection := DefaultCameraDirection;
  FInitialUp        := DefaultCameraUp;
  FPosition  := FInitialPosition;
  FDirection := FInitialDirection;
  FUp        := FInitialUp;
  FGravityUp := FInitialUp;
  FProjectionMatrix := TMatrix4.Identity; // any sensible initial value
  FFrustum.Init(TMatrix4.Identity); // any sensible initial value

  FPerspective := TCastlePerspective.Create(Self);
  FPerspective.Camera := Self;
  FPerspective.Name := 'Perspective';
  FPerspective.SetSubComponent(true);

  FOrthographic := TCastleOrthographic.Create(Self);
  FOrthographic.Camera := Self;
  FOrthographic.Name := 'Orthographic';
  FOrthographic.SetSubComponent(true);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleCamera.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleCamera.Assign(Source: TPersistent);
var
  SourceCamera: TCastleCamera;
begin
  if Source is TCastleCamera then
  begin
    SourceCamera := TCastleCamera(Source);

    { Copies non-temporary properties (in particular, the published properties). }
    GravityUp := SourceCamera.GravityUp;
    SetInitialView(
      SourceCamera.InitialPosition,
      SourceCamera.InitialDirection,
      SourceCamera.InitialUp,
      false);
    SetView(SourceCamera.Position, SourceCamera.Direction, SourceCamera.Up);
    ProjectionNear              := SourceCamera.ProjectionNear;
    ProjectionFar               := SourceCamera.ProjectionFar;
    ProjectionType              := SourceCamera.ProjectionType;
    Perspective.FieldOfView     := SourceCamera.Perspective.FieldOfView;
    Perspective.FieldOfViewAxis := SourceCamera.Perspective.FieldOfViewAxis;
    Orthographic.Origin         := SourceCamera.Orthographic.Origin;
    Orthographic.Width          := SourceCamera.Orthographic.Width;
    Orthographic.Height         := SourceCamera.Orthographic.Height;
  end else
    { Call inherited ONLY when you cannot handle Source class,
      to raise EConvertError from TPersistent.Assign. }
    inherited Assign(Source);
end;

procedure TCastleCamera.GetView(out APos, ADir, AUp: TVector3);
begin
  APos := FPosition;
  ADir := FDirection;
  AUp  := FUp;
end;

procedure TCastleCamera.GetView(out APos, ADir, AUp, AGravityUp: TVector3);
begin
  GetView(APos, ADir, AUp);
  AGravityUp := FGravityUp;
end;

procedure TCastleCamera.SetView(const ADir, AUp: TVector3;
  const AdjustUp: boolean);
begin
  FDirection := ADir.Normalize;
  FUp := AUp.Normalize;
  if AdjustUp then
    MakeVectorsOrthoOnTheirPlane(FUp, FDirection)
  else
    MakeVectorsOrthoOnTheirPlane(FDirection, FUp);

  VisibleChange;
end;

procedure TCastleCamera.SetView(const APos, ADir, AUp: TVector3;
  const AdjustUp: boolean);
begin
  FPosition := APos;
  SetView(ADir, AUp, AdjustUp); // calls VisibleChange at the end
end;

procedure TCastleCamera.SetView(const APos, ADir, AUp, AGravityUp: TVector3;
  const AdjustUp: boolean = true);
begin
  GravityUp := AGravityUp;
  SetView(APos, ADir, AUp, AdjustUp);
end;

procedure TCastleCamera.SetGravityUp(const Value: TVector3);
begin
  FGravityUp := Value.Normalize;
end;

procedure TCastleCamera.SetPosition(const Value: TVector3);
begin
  FPosition := Value;
  VisibleChange;
end;

procedure TCastleCamera.SetDirection(const Value: TVector3);
begin
  FDirection := Value.Normalize;
  MakeVectorsOrthoOnTheirPlane(FUp, FDirection);
  VisibleChange;
end;

procedure TCastleCamera.SetUp(const Value: TVector3);
begin
  FUp := Value.Normalize;
  MakeVectorsOrthoOnTheirPlane(FDirection, FUp);
  VisibleChange;
end;

procedure TCastleCamera.SetInitialPosition(const Value: TVector3);
begin
  FInitialPosition := Value;
end;

procedure TCastleCamera.SetInitialDirection(const Value: TVector3);
begin
  FInitialDirection := Value.Normalize;
  MakeVectorsOrthoOnTheirPlane(FInitialUp, FInitialDirection);
end;

procedure TCastleCamera.SetInitialUp(const Value: TVector3);
begin
  FInitialUp := Value.Normalize;
  MakeVectorsOrthoOnTheirPlane(FInitialDirection, FInitialUp);
end;

procedure TCastleCamera.UpPrefer(const AUp: TVector3);
begin
  FUp := AUp.Normalize;
  MakeVectorsOrthoOnTheirPlane(FUp, FDirection);
  VisibleChange;
end;

procedure TCastleCamera.VisibleChange;
begin
  if InternalViewport <> nil then
    (InternalViewport as TCastleViewport).InternalCameraChanged;
end;

function TCastleCamera.Matrix: TMatrix4;
begin
  Result := LookDirMatrix(FPosition, FDirection, FUp);
end;

function TCastleCamera.RotationMatrix: TMatrix4;
begin
  Result := FastLookDirMatrix(FDirection, FUp);
end;

function TCastleCamera.MatrixInverse: TMatrix4;
begin
  if not Matrix.TryInverse(Result) then
    raise Exception.Create('Cannot invert camera matrix, possibly it contains scaling to zero');
end;

procedure TCastleCamera.RecalculateFrustum;
begin
  FFrustum.Init(ProjectionMatrix, Matrix);
end;

procedure TCastleCamera.SetProjectionMatrix(const Value: TMatrix4);
begin
  FProjectionMatrix := Value;
  RecalculateFrustum;
end;

procedure TCastleCamera.CustomRay(
  const ViewportRect: TFloatRectangle;
  const WindowPosition: TVector2;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3);
var
  APos, ADir, AUp: TVector3;
begin
  GetView(APos, ADir, AUp);

  PrimaryRay(
    WindowPosition[0] - ViewportRect.Left,
    WindowPosition[1] - ViewportRect.Bottom,
    ViewportRect.Width, ViewportRect.Height,
    APos, ADir, AUp,
    Projection,
    RayOrigin, RayDirection);
end;

procedure TCastleCamera.Update(const SecondsPassed: Single);
begin
  if FAnimation then
  begin
    AnimationCurrentTime := AnimationCurrentTime + SecondsPassed;
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

procedure TCastleCamera.AnimateTo(const APos, ADir, AUp: TVector3; const Time: TFloatTime);
begin
  GetView(
    AnimationBeginPosition,
    AnimationBeginDirection,
    AnimationBeginUp);

  AnimationEndPosition := APos;
  AnimationEndDirection := ADir;
  AnimationEndUp := AUp;

  AnimationEndTime := Time;
  AnimationCurrentTime := 0;
  { No point in doing animation (especially since it blocks camera movement
    for Time seconds) if we're already there. }
  FAnimation := not (
    TVector3.Equals(AnimationBeginPosition , AnimationEndPosition) and
    TVector3.Equals(AnimationBeginDirection, AnimationEndDirection) and
    TVector3.Equals(AnimationBeginUp       , AnimationEndUp));
end;

procedure TCastleCamera.AnimateTo(const OtherCamera: TCastleCamera; const Time: TFloatTime);
var
  APos, ADir, AUp: TVector3;
begin
  OtherCamera.GetView(APos, ADir, AUp);
  AnimateTo(APos, ADir, AUp, Time);
end;

function TCastleCamera.Animation: boolean;
begin
  Result := FAnimation;
end;

procedure TCastleCamera.SetInitialView(
  const AInitialPosition: TVector3;
  AInitialDirection, AInitialUp: TVector3;
  const TransformCurrentCamera: boolean);
var
  OldInitialOrientation, NewInitialOrientation, Orientation: TQuaternion;
  APos, ADir, AUp: TVector3;
begin
  AInitialDirection := AInitialDirection.Normalize;
  AInitialUp := AInitialUp.Normalize;
  MakeVectorsOrthoOnTheirPlane(AInitialUp, AInitialDirection);

  if TransformCurrentCamera then
  begin
    GetView(APos, ADir, AUp);

    APos := APos + AInitialPosition - FInitialPosition;

    if not (TVector3.PerfectlyEquals(FInitialDirection, AInitialDirection) and
            TVector3.PerfectlyEquals(FInitialUp       , AInitialUp ) ) then
    begin
      OldInitialOrientation := OrientationQuaternionFromDirectionUp(FInitialDirection, FInitialUp);
      NewInitialOrientation := OrientationQuaternionFromDirectionUp(AInitialDirection, AInitialUp);
      Orientation           := OrientationQuaternionFromDirectionUp(ADir, AUp);

      { I want new Orientation :=
          (Orientation - OldInitialOrientation) + NewInitialOrientation. }
      Orientation := OldInitialOrientation.Conjugate * Orientation;
      Orientation := NewInitialOrientation * Orientation;

      { Now that we have Orientation, transform it into new Dir/Up. }
      ADir := Orientation.Rotate(DefaultCameraDirection);
      AUp  := Orientation.Rotate(DefaultCameraUp);
    end;

    SetView(APos, ADir, AUp);
  end;

  FInitialPosition  := AInitialPosition;
  FInitialDirection := AInitialDirection;
  FInitialUp        := AInitialUp;
end;

procedure TCastleCamera.GoToInitial;
begin
  SetView(FInitialPosition, FInitialDirection, FInitialUp);
end;

procedure TCastleCamera.Init(
  const AInitialPosition, AInitialDirection, AInitialUp, AGravityUp: TVector3);
begin
  SetInitialView(AInitialPosition, AInitialDirection, AInitialUp, false);
  GravityUp := AGravityUp;
  GoToInitial;
end;

procedure TCastleCamera.Loaded;
begin
  inherited;
  GoToInitial;
end;

procedure TCastleCamera.SetProjectionNear(const Value: Single);
begin
  if FProjectionNear <> Value then
  begin
    FProjectionNear := Value;
    VisibleChange;
  end;
end;

procedure TCastleCamera.SetProjectionFar(const Value: Single);
begin
  if FProjectionFar <> Value then
  begin
    FProjectionFar := Value;
    VisibleChange;
  end;
end;

procedure TCastleCamera.SetProjectionType(const Value: TProjectionType);
begin
  if FProjectionType <> Value then
  begin
    FProjectionType := Value;
    VisibleChange;
  end;
end;

procedure TCastleCamera.Free;
begin
end;

procedure TCastleCamera.InternalSetEffectiveProjection(
  const AEffectiveProjectionNear, AEffectiveProjectionFar: Single);
begin
  FEffectiveProjectionNear := AEffectiveProjectionNear;
  FEffectiveProjectionFar := AEffectiveProjectionFar;
end;

function TCastleCamera.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'InitialPositionPersistent') or
     (PropertyName = 'InitialDirectionPersistent') or
     (PropertyName = 'InitialUpPersistent') or
     (PropertyName = 'GravityUpPersistent') or
     (PropertyName = 'ProjectionFar') or
     (PropertyName = 'ProjectionNear') or
     (PropertyName = 'ProjectionType') or
     (PropertyName = 'Orthographic') or
     (PropertyName = 'Perspective') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlecamera_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleNavigation ------------------------------------------------------------ }

constructor TCastleNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FRadius := DefaultRadius;
  FPreferredHeight := DefaultPreferredHeight;
  FInput := DefaultInput;
  FModelBox := TBox3D.Empty;
  FHeadBobbing := DefaultHeadBobbing;
  FHeadBobbingTime := DefaultHeadBobbingTime;
  FMoveHorizontalSpeed := 1;
  FMoveVerticalSpeed := 1;
  FMoveSpeed := 1;
  FCrouchHeight := DefaultCrouchHeight;

  // interaction state
  MouseDraggingStarted := -1;

  FullSize := true;
end;

function TCastleNavigation.MoveAllowed(
  const OldPos: TVector3; ProposedNewPos: TVector3; out NewPos: TVector3;
  const Radius: Single; const BecauseOfGravity: Boolean): Boolean;
begin
  Result := true;
  NewPos := ProposedNewPos;

  if Result and Assigned(OnInternalMoveAllowed) then
  begin
    Result := OnInternalMoveAllowed(Self, Camera.Position, ProposedNewPos, NewPos, Radius, BecauseOfGravity);
    // update ProposedNewPos for OnMoveAllowed call
    if Result then
      ProposedNewPos := NewPos;
  end;
  if Result and Assigned(OnMoveAllowed) then
    Result := OnMoveAllowed(Self, Camera.Position, ProposedNewPos, NewPos, Radius, BecauseOfGravity);
end;

procedure TCastleNavigation.Height(const APosition: TVector3;
  out AIsAbove: Boolean;
  out AnAboveHeight: Single; out AnAboveGround: PTriangle);
begin
  if Assigned(OnInternalHeight) then
    AIsAbove := OnInternalHeight(Self, APosition, AnAboveHeight, AnAboveGround) else
  begin
    AIsAbove := false;
    AnAboveHeight := MaxSingle;
    AnAboveGround := nil;
  end;
end;

function TCastleNavigation.GetNavigationType: TNavigationType;
begin
  Result := ntNone;
end;

function TCastleNavigation.Camera: TCastleCamera;
begin
  if InternalViewport = nil then
    raise EViewportNotAssigned.Create('Viewport not assigned, cannot get Camera properties');
  Result := (InternalViewport as TCastleViewport).Camera;
end;

procedure TCastleNavigation.Ray(const WindowPosition: TVector2;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3);
begin
  Assert(ContainerSizeKnown, 'Camera container size not known yet (probably camera not added to Controls list), cannot use TCastleNavigation.Ray');
  Camera.CustomRay(FloatRectangle(ContainerRect), WindowPosition, Projection, RayOrigin, RayDirection);
end;

procedure TCastleNavigation.MouseRay(
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3);
begin
  Assert(ContainerSizeKnown, 'Camera container size not known yet (probably camera not added to Controls list), cannot use TCastleNavigation.MouseRay');
  Camera.CustomRay(FloatRectangle(ContainerRect), Container.MousePosition, Projection, RayOrigin, RayDirection);
end;

procedure TCastleNavigation.CustomRay(
  const ViewportRect: TFloatRectangle;
  const WindowPosition: TVector2;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3);
begin
  Camera.CustomRay(ViewportRect, WindowPosition, Projection, RayOrigin, RayDirection);
end;

procedure TCastleNavigation.CustomRay(
  const ViewportRect: TRectangle;
  const WindowPosition: TVector2;
  const Projection: TProjection;
  out RayOrigin, RayDirection: TVector3);
begin
  Camera.CustomRay(FloatRectangle(ViewportRect),
    WindowPosition, Projection, RayOrigin, RayDirection);
end;

procedure TCastleNavigation.AnimateTo(const APos, ADir, AUp: TVector3; const Time: TFloatTime);
begin
  Camera.AnimateTo(APos, ADir, AUp, Time);
end;

procedure TCastleNavigation.AnimateTo(const OtherNavigation: TCastleNavigation; const Time: TFloatTime);
var
  APos, ADir, AUp: TVector3;
begin
  OtherNavigation.Camera.GetView(APos, ADir, AUp);
  Camera.AnimateTo(APos, ADir, AUp, Time);
end;

procedure TCastleNavigation.AnimateTo(const OtherCamera: TCastleCamera; const Time: TFloatTime);
begin
  Camera.AnimateTo(OtherCamera, Time);
end;

function TCastleNavigation.Animation: boolean;
begin
  Result := Camera.Animation;
end;

function TCastleNavigation.InitialPosition: TVector3;
begin
  Result := Camera.InitialPosition;
end;

function TCastleNavigation.InitialDirection: TVector3;
begin
  Result := Camera.InitialDirection;
end;

function TCastleNavigation.InitialUp: TVector3;
begin
  Result := Camera.InitialUp;
end;

procedure TCastleNavigation.SetInitialView(
  const AInitialPosition: TVector3;
  AInitialDirection, AInitialUp: TVector3;
  const TransformCurrentCamera: boolean);
begin
  Camera.SetInitialView(AInitialPosition,
    AInitialDirection, AInitialUp, TransformCurrentCamera);
end;

procedure TCastleNavigation.GoToInitial;
begin
  Camera.GoToInitial;
end;

function TCastleNavigation.GetIgnoreAllInputs: boolean;
begin
  Result := Input = [];
end;

procedure TCastleNavigation.SetIgnoreAllInputs(const Value: boolean);
begin
  if Value then
    Input := []
  else
    Input := DefaultInput;
end;

function TCastleNavigation.ReallyEnableMouseDragging: boolean;
begin
  Result := (niMouseDragging in UsingInput) and
    { Is mouse dragging allowed by viewport.
      This is an additional condition to enable mouse dragging,
      above the existing niMouseDragging in UsingInput.
      It is used to prevent camera navigation by
      dragging when we already drag a 3D item (like X3D TouchSensor). }
    ( (InternalViewport = nil) or
      not (InternalViewport as TCastleViewport).InternalPointingDeviceDragging );
end;

function TCastleNavigation.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if (Event.EventType = itMouseButton) and
     ReallyEnableMouseDragging then
  begin
    MouseDraggingStart := Container.MousePosition;
    MouseDraggingStarted := Event.FingerIndex;
    { TODO: Not setting Result to true below is a hack, to allow TCastleViewport
      to receive presses anyway. A cleaner solution would be to use
      PreviewPress in TCastleViewport, but this causes other problems,
      for unknown reason clicking on TouchSensor then still allows navigation like Walk
      to receive mouse dragging.
      Testcase: demo-models, touch_sensor_tests.x3dv }
    // Exit(ExclusiveEvents);
  end;
end;

function TCastleNavigation.Release(const Event: TInputPressRelease): boolean;
begin
  if Event.EventType = itMouseButton then
    MouseDraggingStarted := -1;
  Result := inherited;
end;

procedure TCastleNavigation.CorrectPreferredHeight;
begin
  CastleCameras.CorrectPreferredHeight(
    FPreferredHeight, Radius, CrouchHeight, HeadBobbing);
end;

procedure TCastleNavigation.Assign(Source: TPersistent);
var
  SourceNav: TCastleNavigation;
begin
  if Source is TCastleNavigation then
  begin
    SourceNav := TCastleNavigation(Source);
    Radius              := SourceNav.Radius             ;
    Input               := SourceNav.Input              ;
    { The Cursor should be synchronized with TCastleMouseLookNavigation.MouseLook,
      do not blindly copy it from TCastleWalkNavigation to TCastleExamineNavigation. }
    // Cursor              := SourceNav.Cursor             ;
    PreferredHeight     := SourceNav.PreferredHeight    ;
    MoveHorizontalSpeed := SourceNav.MoveHorizontalSpeed;
    MoveVerticalSpeed   := SourceNav.MoveVerticalSpeed  ;
    MoveSpeed           := SourceNav.MoveSpeed          ;
    HeadBobbing         := SourceNav.HeadBobbing        ;
    HeadBobbingTime     := SourceNav.HeadBobbingTime    ;
    ClimbHeight         := SourceNav.ClimbHeight        ;
    ModelBox            := SourceNav.ModelBox           ;
    CrouchHeight        := SourceNav.CrouchHeight       ;

    { Always call CorrectPreferredHeight after changing Radius or PreferredHeight }
    CorrectPreferredHeight;
  end else
    { Call inherited ONLY when you cannot handle Source class,
      to raise EConvertError from TPersistent.Assign. }
    inherited Assign(Source);
end;

function TCastleNavigation.UsingInput: TNavigationInputs;
begin
  { Behave like Input=[] on a paused viewport }
  if (InternalViewport <> nil) and
     ((InternalViewport as TCastleViewport).Items.Paused) then
    Result := []
  else
    Result := Input;
end;

procedure TCastleNavigation.GetView(out APos, ADir, AUp: TVector3);
begin
  Camera.GetView(APos, ADir, AUp);
end;

procedure TCastleNavigation.GetView(out APos, ADir, AUp, AGravityUp: TVector3);
begin
  Camera.GetView(APos, ADir, AUp, AGravityUp);
end;

procedure TCastleNavigation.SetView(const APos, ADir, AUp: TVector3;
  const AdjustUp: boolean);
begin
  Camera.SetView(APos, ADir, AUp, AdjustUp);
end;

procedure TCastleNavigation.SetView(const APos, ADir, AUp, AGravityUp: TVector3;
  const AdjustUp: boolean);
begin
  Camera.SetView(APos, ADir, AUp, AGravityUp, AdjustUp);
end;

function TCastleNavigation.GetPosition: TVector3;
begin
  Result := Camera.Position;
end;

function TCastleNavigation.GetDirection: TVector3;
begin
  Result := Camera.Direction;
end;

function TCastleNavigation.GetUp: TVector3;
begin
  Result := Camera.Up;
end;

function TCastleNavigation.GetGravityUp: TVector3;
begin
  Result := Camera.GravityUp;
end;

procedure TCastleNavigation.SetPosition(const Value: TVector3);
begin
  Camera.Position := Value;
end;

procedure TCastleNavigation.SetDirection(const Value: TVector3);
begin
  Camera.Direction := Value;
end;

procedure TCastleNavigation.SetUp(const Value: TVector3);
begin
  Camera.Up := Value;
end;

procedure TCastleNavigation.SetGravityUp(const Value: TVector3);
begin
  Camera.GravityUp := Value;
end;

function TCastleNavigation.Matrix: TMatrix4;
begin
  Result := Camera.Matrix;
end;

function TCastleNavigation.RotationMatrix: TMatrix4;
begin
  Result := Camera.RotationMatrix;
end;

function TCastleNavigation.MatrixInverse: TMatrix4;
begin
  Result := Camera.MatrixInverse;
end;

function TCastleNavigation.GetProjectionMatrix: TMatrix4;
begin
  Result := Camera.ProjectionMatrix;
end;

procedure TCastleNavigation.SetProjectionMatrix(const Value: TMatrix4);
begin
  Camera.ProjectionMatrix := Value;
end;

function TCastleNavigation.GetFrustum: TFrustum;
begin
  Result := Camera.Frustum;
end;

function TCastleNavigation.GoodModelBox: TBox3D;
begin
  { Try hard to return non-empty bounding box, otherwise examine navigation
    doesn't work sensibly, as movement and zooming speed must depend on box
    sizes.
    This is important in case you use TCastleExamineNavigation without
    setting it's ModelBox explicitly, which happens e.g. when CGE editor
    adds TCastleExamineNavigation. }
  if FModelBox.IsEmpty and
     (InternalViewport <> nil) then
    Result := (InternalViewport as TCastleViewport).Items.BoundingBox
  else
    Result := FModelBox;
end;

function TCastleNavigation.PropertySections(
  const PropertyName: String): TPropertySections;
begin
// not sure if useful enough:
  // case PropertyName of
  //   'Input':
  //     Result := [psBasic];
  //   else
      Result := inherited PropertySections(PropertyName);
  // end;
end;

{ TCastleExamineNavigation ------------------------------------------------------------ }

constructor TCastleExamineNavigation.Create(AOwner: TComponent);
type
  T3BoolKeys = array [0..2, boolean] of TKey;
const
  DefaultInputs_Move: T3BoolKeys =
    ((keyArrowLeft, keyArrowRight), (keyArrowDown, keyArrowUp), (keyNone, keyNone));
  DefaultInputs_Rotate: T3BoolKeys =
    ((keyArrowUp, keyArrowDown), (keyArrowLeft, keyArrowRight), (keyNone, keyNone));
  CoordToStr: array [0..2] of string = ('X', 'Y', 'Z');
  IncreaseToStr: array [boolean] of string = ('Dec', 'Inc');
var
  I: Integer;
  B: boolean;
begin
  inherited;

  FRotationEnabled := true;
  FMoveEnabled := true;
  FZoomEnabled := true;
  FRotationsAnim := TVector3.Zero;
  FDragMoveSpeed := 1;
  FKeysMoveSpeed := 1;
  FScaleFactorMin := 0.01;
  FScaleFactorMax := 100.0;
  FRotationAccelerate := true;
  FRotationAccelerationSpeed := DefaultRotationAccelerationSpeed;
  FRotationSpeed := DefaultRotationSpeed;
  FPinchGestureRecognizer := TCastlePinchPanGestureRecognizer.Create;
  FPinchGestureRecognizer.OnGestureChanged := {$ifdef FPC}@{$endif}OnGestureRecognized;

  FMouseButtonRotate := buttonLeft;
  FMouseButtonMove := buttonMiddle;
  FMouseButtonZoom := buttonRight;
  FExactMovement := true;

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
   Input_ScaleLarger.Assign(keyNumpadPlus, keyNone, '+');

  FInput_ScaleSmaller := TInputShortcut.Create(Self);
   Input_ScaleSmaller.Name := 'Input_ScaleSmaller';
   Input_ScaleSmaller.SetSubComponent(true);
   Input_ScaleSmaller.Assign(keyNumpadMinus, keyNone, '-');

  FInput_Home := TInputShortcut.Create(Self);
   Input_Home.Name := 'Input_Home';
   Input_Home.SetSubComponent(true);
   Input_Home.Assign(keyNone);

  FInput_StopRotating := TInputShortcut.Create(Self);
   Input_StopRotating.Name := 'Input_StopRotating';
   Input_StopRotating.SetSubComponent(true);
   Input_StopRotating.Assign(keySpace, keyNone, '', true, buttonLeft);
end;

destructor TCastleExamineNavigation.Destroy;
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
  FreeAndNil(FPinchGestureRecognizer);
  inherited;
end;

function TCastleExamineNavigation.GetExamineVectors: TExamineVectors;
var
  APos, ADir, AUp: TVector3;
begin
  Camera.GetView(APos, ADir, AUp);

  Result.Translation := -APos;

  Result.Rotations := OrientationQuaternionFromDirectionUp(ADir, AUp).Conjugate;

  { We have to fix our Translation, since our TCastleExamineNavigation.Matrix
    applies our move *first* before applying rotation
    (and this is good, as it allows rotating around object center,
    not around camera).

    Alternative implementation of this would call QuatToRotationMatrix and
    then simulate multiplying this rotation matrix * translation matrix
    of Translation. But we can do this directly.

    We also note at this point that rotation is done around
    (Translation + CenterOfRotation). But CenterOfRotation is not
    included in Translation. }
  Result.Translation := Result.Rotations.Rotate(Result.Translation + CenterOfRotation)
    - CenterOfRotation;
end;

procedure TCastleExamineNavigation.SetExamineVectors(const Value: TExamineVectors);
var
  MInverse: TMatrix4;
begin
  MInverse :=
    TranslationMatrix(CenterOfRotation) *
    Value.Rotations.Conjugate.ToRotationMatrix *
    TranslationMatrix(-(Value.Translation + CenterOfRotation));

  { These MultPoint/Direction should never fail with ETransformedResultInvalid.
    That's because M is composed from translations, rotations, scaling,
    which preserve points/directions (4th component in homogeneous coordinates)
    nicely. }
  Camera.SetView(
    MInverse.MultPoint(TVector3.Zero),
    MInverse.MultDirection(DefaultCameraDirection),
    MInverse.MultDirection(DefaultCameraUp)
  );
end;

procedure TCastleExamineNavigation.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  V: TExamineVectors;

  { Increase speed of rotating, or just rotation angle
    (depending on RotationAccelerate). Direction must be -1 or +1. }
  procedure RotateSpeedOrAngle(const Coord: Integer; const Direction: Integer);
  const
    MaxRotationSpeed = 6.0; { this prevents rotations getting too wild speed }
  begin
    if not RotationEnabled then Exit;

    if RotationAccelerate then
      FRotationsAnim.InternalData[coord] :=
        Clamped(FRotationsAnim.InternalData[coord] +
          RotationAccelerationSpeed * SecondsPassed * Direction,
          -MaxRotationSpeed, MaxRotationSpeed)
    else
      V.Rotations := QuatFromAxisAngle(TVector3.One[Coord],
        RotationSpeed * SecondsPassed * Direction) * V.Rotations;
  end;

var
  i: integer;
  MoveChange: Single;
  ModsDown: TModifierKeys;
  RotChange: Single;
  MoveChangeVector: TVector3;
const
  KeyZoomSpeed = 10.0;
begin
  inherited;

  { Do not handle keys or rotations etc. }
  if Camera.Animation then Exit;

  V := ExamineVectors;

  if RotationEnabled and (not FRotationsAnim.IsPerfectlyZero) then
  begin
    RotChange := SecondsPassed;

    if FRotationsAnim[0] <> 0 then
      V.Rotations := QuatFromAxisAngle(TVector3.One[0],
        FRotationsAnim[0] * RotChange) * V.Rotations;

    if FRotationsAnim[1] <> 0 then
    begin
      if Turntable then
        V.Rotations := V.Rotations * QuatFromAxisAngle(TVector3.One[1],
          FRotationsAnim[1] * RotChange) else
        V.Rotations := QuatFromAxisAngle(TVector3.One[1],
          FRotationsAnim[1] * RotChange) * V.Rotations;
    end;

    if FRotationsAnim[2] <> 0 then
      V.Rotations := QuatFromAxisAngle(TVector3.One[2],
        FRotationsAnim[2] * RotChange) * V.Rotations;

    V.Rotations.LazyNormalizeMe;
  end;

  if HandleInput and (niNormal in UsingInput) then
  begin
    if GoodModelBox.IsEmptyOrZero then
      MoveChange := KeysMoveSpeed * SecondsPassed
    else
      MoveChange := KeysMoveSpeed * GoodModelBox.AverageSize * SecondsPassed;

    ModsDown := ModifiersDown(Container.Pressed);

    if MoveEnabled and (ModsDown = [mkCtrl]) then
    begin
      for i := 0 to 2 do
      begin
        if Inputs_Move[i, true ].IsPressed(Container) then
        begin
          MoveChangeVector := TVector3.Zero;
          MoveChangeVector.InternalData[I] := MoveChange;
          V.Translation := V.Translation + MoveChangeVector;

          HandleInput := not ExclusiveEvents;
        end;
        if Inputs_Move[i, false].IsPressed(Container) then
        begin
          MoveChangeVector := TVector3.Zero;
          MoveChangeVector.InternalData[I] := -MoveChange;
          V.Translation := V.Translation + MoveChangeVector;

          HandleInput := not ExclusiveEvents;
        end;
      end;
    end else
    if RotationEnabled and (ModsDown = []) then
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
  end;

  ExamineVectors := V;

  { process things that do not set ExamineVectors }
  if HandleInput and (niNormal in UsingInput) then
  begin
    if Input_ScaleLarger.IsPressed(Container) then
    begin
      Zoom(KeyZoomSpeed * SecondsPassed);
      HandleInput := not ExclusiveEvents;
    end;
    if Input_ScaleSmaller.IsPressed(Container) then
    begin
      Zoom(-KeyZoomSpeed * SecondsPassed);
      HandleInput := not ExclusiveEvents;
    end;
  end;
end;

function TCastleExamineNavigation.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

procedure TCastleExamineNavigation.SetRotationAccelerate(const Value: boolean);
begin
  if FRotationAccelerate <> Value then
  begin
    FRotationAccelerate := Value;
    FRotationsAnim := TVector3.Zero;
  end;
end;

function TCastleExamineNavigation.StopRotating: boolean;
begin
  Result := not FRotationsAnim.IsPerfectlyZero;
  if Result then
    FRotationsAnim := TVector3.Zero;
end;

procedure TCastleExamineNavigation.Move(coord: integer; const MoveDistance: Single);
var
  V: TVector3;
begin
  V := TVector3.Zero;
  V.InternalData[Coord] := MoveDistance;
  Translation := Translation + V;
end;

function TCastleExamineNavigation.SensorTranslation(const X, Y, Z, Length: Double;
  const SecondsPassed: Single): boolean;
var
  Size: Single;
  MoveSize: Double;
begin
  if not (ni3dMouse in UsingInput) then Exit(false);
  if not MoveEnabled then Exit(false);
  if GoodModelBox.IsEmptyOrZero then Exit(false);
  Result := true;

  Size := GoodModelBox.AverageSize;
  MoveSize := Length * SecondsPassed / 5000;

  if Abs(X) > 5 then   { left / right }
    Translation := Translation + Vector3(Size * X * MoveSize, 0, 0);

  if Abs(Y) > 5 then   { up / down }
    Translation := Translation + Vector3(0, Size * Y * MoveSize, 0);

  if Abs(Z) > 5 then   { backward / forward }
    Zoom(Z * MoveSize / 2);
end;

function TCastleExamineNavigation.SensorRotation(const X, Y, Z, Angle: Double;
  const SecondsPassed: Single): boolean;
var
  Moved: boolean;
  RotationSize: Double;
  V: TExamineVectors;
begin
  if not (ni3dMouse in UsingInput) then Exit(false);
  if not RotationEnabled then Exit(false);
  Result := true;

  Moved := false;
  RotationSize := SecondsPassed * Angle;
  V := ExamineVectors;

  if Abs(X) > 0.4 then      { tilt forward / backward}
  begin
    V.Rotations := QuatFromAxisAngle(Vector3(1, 0, 0), X * RotationSize) * V.Rotations;
    Moved := true;
  end;

  if Abs(Y) > 0.4 then      { rotate }
  begin
    if Turntable then
      V.Rotations := V.Rotations *
        QuatFromAxisAngle(Vector3(0, 1, 0), Y * RotationSize) else
      V.Rotations := QuatFromAxisAngle(Vector3(0, 1, 0), Y * RotationSize) *
        V.Rotations;
    Moved := true;
  end;

  if (Abs(Z) > 0.4) and (not Turntable) then      { tilt sidewards }
  begin
    V.Rotations := QuatFromAxisAngle(Vector3(0, 0, 1), Z * RotationSize) * V.Rotations;
    Moved := true;
  end;

  { Assign ExamineVectors only if some change occurred }
  if Moved then
    ExamineVectors := V;
end;

procedure TCastleExamineNavigation.Init(const AModelBox: TBox3D; const ARadius: Single);
var
  APos, ADir, AUp, NewGravityUp: TVector3;
begin
  FModelBox := AModelBox; // set using FModelBox, as there's no need to preserve view
  Radius := ARadius;

  CameraViewpointForWholeScene(ModelBox, 2, 1, false, true,
    APos, ADir, AUp, NewGravityUp);

  Camera.Init(APos, ADir, AUp, NewGravityUp);
  Camera.ProjectionNear := Radius * RadiusToProjectionNear;
end;

{ TCastleExamineNavigation.Set* properties }

procedure TCastleExamineNavigation.SetRotationsAnim(const Value: TVector3);
begin
  FRotationsAnim := Value;
end;

function TCastleExamineNavigation.GetRotations: TQuaternion;
begin
  Result := ExamineVectors.Rotations;
end;

procedure TCastleExamineNavigation.SetRotations(const Value: TQuaternion);
var
  V: TExamineVectors;
begin
  V := ExamineVectors;
  V.Rotations := Value;
  ExamineVectors := V;
end;

function TCastleExamineNavigation.GetScaleFactor: Single;
begin
  Result := Camera.Orthographic.Scale;
end;

procedure TCastleExamineNavigation.SetScaleFactor(const Value: Single);
begin
  Camera.Orthographic.Scale := Value;
end;

procedure TCastleExamineNavigation.SetScaleFactorMin(const Value: Single);
begin
  if FScaleFactorMin <> Value then
  begin
    FScaleFactorMin := Value;
    { Correct ScaleFactor now }
    Camera.Orthographic.Scale := Clamped(Camera.Orthographic.Scale, FScaleFactorMin, FScaleFactorMax);
  end;
end;

procedure TCastleExamineNavigation.SetScaleFactorMax(const Value: Single);
begin
  if FScaleFactorMax <> Value then
  begin
    FScaleFactorMax := Value;
    { Correct ScaleFactor now }
    Camera.Orthographic.Scale := Clamped(Camera.Orthographic.Scale, FScaleFactorMin, FScaleFactorMax);
  end;
end;

function TCastleExamineNavigation.GetTranslation: TVector3;
begin
  Result := ExamineVectors.Translation;
end;

procedure TCastleExamineNavigation.SetTranslation(const Value: TVector3);
var
  V: TExamineVectors;
begin
  V := ExamineVectors;
  V.Translation := Value;
  ExamineVectors := V;
end;

function TCastleExamineNavigation.CenterOfRotation: TVector3;
var
  B: TBox3D;
begin
  B := GoodModelBox;
  if B.IsEmpty then
    Result := Vector3(0, 0, 0) { any dummy value }
  else
    Result := B.Center;
end;

function TCastleExamineNavigation.Press(const Event: TInputPressRelease): boolean;
var
  ZoomScale: Single;
begin
  Result := inherited;
  if Result or
     Camera.Animation or
     (ModifiersDown(Container.Pressed) <> []) then
    Exit;

  if (niGesture in UsingInput) and FPinchGestureRecognizer.Press(Event) then
    Exit(ExclusiveEvents);

  if not (niNormal in UsingInput) then Exit;

  if Event.EventType <> itMouseWheel then
  begin
    if Input_StopRotating.IsEvent(Event) then
    begin
      { If StopRotating was useless, do not mark the event as "handled".
        This is necessary to avoid having mouse clicks "stolen" by the TCastleExamineNavigation
        when an empty TCastleViewport is being used
        (and thus, mouse clicks could instead be used by other control).
        It was necessary with deprecated TCastleControl/TCastleWindow:
        on empty window, mouse clicks would be "mysteriously" intercepted,
        since the default scene manager creates
        examine camera, and it captures left mouse click as Input_StopRotating. }
      if StopRotating then
        Result := ExclusiveEvents;
    end else
    if Input_Home.IsEvent(Event) then
    begin
      Camera.GoToInitial;
      Result := ExclusiveEvents;
    end else
      Result := false;
  end else
  if ZoomEnabled then
  begin
    { For now, doing Zoom on mouse wheel is hardcoded, we don't call EventDown here }

    if Turntable then
      ZoomScale := 30 else
      ZoomScale := 10;
    if Zoom(Event.MouseWheelScroll / ZoomScale) then
       Result := ExclusiveEvents;
  end;
end;

function TCastleExamineNavigation.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if (niGesture in UsingInput) and FPinchGestureRecognizer.Release(Event) then
    Exit(ExclusiveEvents);
end;

function TCastleExamineNavigation.Zoom(const Factor: Single): boolean;

  function OrthographicProjection: Boolean;
  begin
    { See how perspective (and more flexible frustum) projection matrices
      look like in CastleProjection, they have always -1 in this field. }
    Result := Camera.ProjectionMatrix.Data[2, 3] = 0;
  end;

var
  Size: Single;
  OldTranslation, OldPosition: TVector3;
  B: TBox3D;
begin
  B := GoodModelBox;
  Result := not B.IsEmptyOrZero;
  if Result then
  begin
    if OrthographicProjection then
    begin
      { In case of OrthographicProjection, changing Translation
        would have no effect. So instead scale the projection size. }
      Camera.Orthographic.Scale := Camera.Orthographic.Scale * Exp(-Factor);
    end else
    begin
      { zoom by changing Translation }
      Size := B.AverageSize;

      OldTranslation := Translation;
      OldPosition := Camera.Position;

      Translation := Translation + Vector3(0, 0, Size * Factor);

      { Cancel zoom in, don't allow to go to the other side of the model too far.
        Note that TBox3D.PointDistance = 0 when you're inside the box,
        so zoomin in/out inside the box is still always allowed.
        See http://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&t=24 }
      if (Factor > 0) and
         (B.PointDistance(Camera.Position) >
          B.PointDistance(OldPosition)) then
      begin
        Translation := OldTranslation;
        Exit(false);
      end;
    end;
  end;
end;

function TCastleExamineNavigation.Motion(const Event: TInputMotion): boolean;
var
  Size: Single;
  ModsDown: TModifierKeys;
  MoveDivConst: Single;
  Dpi: Single;

  procedure DragRotation;
  var
    V: TExamineVectors;

    { Returns new rotation }
    function XYRotation(const Scale: Single): TQuaternion;
    begin
      if Turntable then
        Result :=
          QuatFromAxisAngle(Vector3(1, 0, 0), Scale * (Event.OldPosition[1] - Event.Position[1]) / MoveDivConst) *
          V.Rotations *
          QuatFromAxisAngle(Vector3(0, 1, 0), Scale * (Event.Position[0] - Event.OldPosition[0]) / MoveDivConst)
      else
        Result :=
          QuatFromAxisAngle(Vector3(1, 0, 0), Scale * (Event.OldPosition[1] - Event.Position[1]) / MoveDivConst) *
          QuatFromAxisAngle(Vector3(0, 1, 0), Scale * (Event.Position[0] - Event.OldPosition[0]) / MoveDivConst) *
          V.Rotations;
    end;

  var
    W2, H2, AvgX, AvgY, ZRotAngle, ZRotRatio: Single;
  begin
    V := ExamineVectors;

    if (not ContainerSizeKnown) then
    begin
      V.Rotations := XYRotation(1);
    end else
    if Turntable then
    begin
      //Result := XYRotation(0.5); // this matches the rotation speed of ntExamine
      { Do one turn around Y axis by dragging from one viewport side to another
        (so it does not depend on viewport size)  }
      V.Rotations := XYRotation(2 * Pi * MoveDivConst / Container.Width);
    end else
    begin
      { When the cursor is close to the window edge, make rotation around Z axis.
        This is called "virtual trackball" on
        http://audilab.bme.mcgill.ca/~funnell/graphics/graphics3dview.html . }
      { clamp, since mouse positions may be wild }
      AvgX := (Event.Position[0] + Event.OldPosition[0]) / 2;
      AvgY := (Event.Position[1] + Event.OldPosition[1]) / 2;
      { let physical size affect scaling speed }
      W2 := Container.Width / 2;
      H2 := Container.Height / 2;
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
      V.Rotations :=
        QuatFromAxisAngle(Vector3(0, 0, -1), ZRotRatio * ZRotAngle) *
        XYRotation(1 - ZRotRatio);
    end;

    ExamineVectors := V;
  end;

var
  DraggingMouseButton: TCastleMouseButton;
begin
  Result := inherited;
  if Result then Exit;

  if Container <> nil then
    Dpi := Container.Dpi
  else
    Dpi := DefaultDpi;

  if (niGesture in UsingInput) and FPinchGestureRecognizer.Motion(Event, Dpi) then
    Exit(ExclusiveEvents);

  MoveDivConst := Dpi;

  { When dragging should be ignored, or (it's an optimization to check it
    here early, Motion occurs very often) when nothing pressed, do nothing. }
  if (Container.MousePressed = []) or
     (not ReallyEnableMouseDragging) or
     (MouseDraggingStarted <> Event.FingerIndex) or
     Camera.Animation then
    Exit;

  ModsDown := ModifiersDown(Container.Pressed) * [mkShift, mkCtrl];

  { Look at Container.MousePressed and ModsDown to determine
    which mouse button is "dragging" now. }
  if (buttonLeft in Container.MousePressed) and (ModsDown = []) then
    DraggingMouseButton := buttonLeft
  else
  if ((buttonRight in Container.MousePressed) and (ModsDown = [])) or
     ((buttonLeft in Container.MousePressed) and (ModsDown = [mkCtrl])) then
    DraggingMouseButton := buttonRight
  else
  if ((buttonMiddle in Container.MousePressed) and (ModsDown = [])) or
     ((buttonLeft in Container.MousePressed) and (ModsDown = [mkShift])) then
    DraggingMouseButton := buttonMiddle
  else
    Exit;

  { Rotating }
  if RotationEnabled and
     (MouseButtonRotate = DraggingMouseButton) then
  begin
    DragRotation;
    Result := ExclusiveEvents;
  end;

  if ZoomEnabled and
     (MouseButtonZoom = DraggingMouseButton) then
  begin
    if Zoom((Event.OldPosition[1] - Event.Position[1]) / (2*MoveDivConst)) then
      Result := ExclusiveEvents;
  end;

  { Moving uses box size, so requires non-empty box. }
  if MoveEnabled and
     (not GoodModelBox.IsEmpty) and
     (MouseButtonMove = DraggingMouseButton) then
  begin
    if ExactMovement and
       (Camera.ProjectionType = ptOrthographic) and
       TVector3.Equals(Camera.Direction, DefaultCameraDirection) and
       TVector3.Equals(Camera.Up, DefaultCameraUp) and
       (InternalViewport <> nil) then
    begin
      Translation := Translation + Vector3(
        (InternalViewport as TCastleViewport).PositionTo2DWorld(Event.Position, true) -
        (InternalViewport as TCastleViewport).PositionTo2DWorld(Event.OldPosition, true),
        0);
    end else
    begin
      Size := GoodModelBox.AverageSize;
      Translation := Translation - Vector3(
        DragMoveSpeed * Size * (Event.OldPosition[0] - Event.Position[0])
        / (2 * MoveDivConst),
        DragMoveSpeed * Size * (Event.OldPosition[1] - Event.Position[1])
        / (2 * MoveDivConst),
        0);
    end;
    Result := ExclusiveEvents;
  end;
end;

procedure TCastleExamineNavigation.OnGestureRecognized(Sender: TObject);
var
  Recognizer: TCastlePinchPanGestureRecognizer;
  Factor, Size, MoveDivConst, ZoomScale: Single;
begin
  Recognizer := Sender as TCastlePinchPanGestureRecognizer;
  if Recognizer = nil then Exit;

  if Container <> nil then
    MoveDivConst := Container.Dpi else
    MoveDivConst := 100;

  if ZoomEnabled and (Recognizer.Gesture = gtPinch) then
  begin
    if Recognizer.PinchScaleFactor > 1.0 then
      Factor := 40 * (Recognizer.PinchScaleFactor - 1.0)
    else
      Factor := -40 * (1.0/Recognizer.PinchScaleFactor - 1.0);
    if Turntable then
      ZoomScale := 30
    else
      ZoomScale := 10;
    Zoom(Factor / ZoomScale);
  end;

  if MoveEnabled and (not GoodModelBox.IsEmpty) and (Recognizer.Gesture = gtPan) then
  begin
    Size := GoodModelBox.AverageSize;
    Translation := Translation - Vector3(
      DragMoveSpeed * Size * (Recognizer.PanOldOffset.X - Recognizer.PanOffset.X) / (2*MoveDivConst),
      DragMoveSpeed * Size * (Recognizer.PanOldOffset.Y - Recognizer.PanOffset.Y) / (2*MoveDivConst),
      0);
  end;
end;

function TCastleExamineNavigation.GetInput_MoveXInc: TInputShortcut; begin Result := Inputs_Move[0, true ] end;
function TCastleExamineNavigation.GetInput_MoveXDec: TInputShortcut; begin Result := Inputs_Move[0, false] end;
function TCastleExamineNavigation.GetInput_MoveYInc: TInputShortcut; begin Result := Inputs_Move[1, true ] end;
function TCastleExamineNavigation.GetInput_MoveYDec: TInputShortcut; begin Result := Inputs_Move[1, false] end;
function TCastleExamineNavigation.GetInput_MoveZInc: TInputShortcut; begin Result := Inputs_Move[2, true ] end;
function TCastleExamineNavigation.GetInput_MoveZDec: TInputShortcut; begin Result := Inputs_Move[2, false] end;
function TCastleExamineNavigation.GetInput_RotateXInc: TInputShortcut; begin Result := Inputs_Rotate[0, true ] end;
function TCastleExamineNavigation.GetInput_RotateXDec: TInputShortcut; begin Result := Inputs_Rotate[0, false] end;
function TCastleExamineNavigation.GetInput_RotateYInc: TInputShortcut; begin Result := Inputs_Rotate[1, true ] end;
function TCastleExamineNavigation.GetInput_RotateYDec: TInputShortcut; begin Result := Inputs_Rotate[1, false] end;
function TCastleExamineNavigation.GetInput_RotateZInc: TInputShortcut; begin Result := Inputs_Rotate[2, true ] end;
function TCastleExamineNavigation.GetInput_RotateZDec: TInputShortcut; begin Result := Inputs_Rotate[2, false] end;

function TCastleExamineNavigation.GetMouseNavigation: boolean;
begin
  Result := niMouseDragging in UsingInput;
end;

procedure TCastleExamineNavigation.SetMouseNavigation(const Value: boolean);
begin
  if Value then
    Input := Input + [niMouseDragging]
  else
    Input := Input - [niMouseDragging];
end;

function TCastleExamineNavigation.GetNavigationType: TNavigationType;
begin
  if Input = [] then
    Result := ntNone
  else
  if Turntable then
    Result := ntTurntable
  else
    Result := ntExamine;
end;

function TCastleExamineNavigation.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'MoveEnabled') or
     (PropertyName = 'RotationEnabled') or
     (PropertyName = 'ZoomEnabled') or
     (PropertyName = 'RotationAccelerate') or
     (PropertyName = 'ExactMovement') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{ TCastleMouseLookNavigation ------------------------------------------------- }

constructor TCastleMouseLookNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FMouseLookHorizontalSensitivity := DefaultMouseLookHorizontalSensitivity;
  FMouseLookVerticalSensitivity := DefaultMouseLookVerticalSensitivity;
  FInvertVerticalMouseLook := false;
end;

procedure TCastleMouseLookNavigation.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  procedure MouseLookUpdate;
  begin
    if UsingMouseLook and (Container <> nil) then
      Container.MouseLookUpdate;
  end;

begin
  inherited;
  MouseLookUpdate;
end;

procedure TCastleMouseLookNavigation.SetMouseLook(const Value: boolean);
begin
  if FMouseLook <> Value then
  begin
    FMouseLook := Value;
    if UsingMouseLook then
    begin
      Cursor := mcForceNone;
      if Container <> nil then
        Container.MouseLookPress;
    end else
      Cursor := mcDefault;
  end;
end;

procedure TCastleMouseLookNavigation.ProcessMouseLookDelta(const Delta: TVector2);
begin
  // nothing in this class
end;

function TCastleMouseLookNavigation.Motion(const Event: TInputMotion): boolean;

  procedure HandleMouseLook;
  var
    MouseChange: TVector2;
  begin
    MouseChange := Container.MouseLookDelta(Event);

    if not MouseChange.IsPerfectlyZero then
    begin
      if InvertVerticalMouseLook then
        MouseChange.Y := -MouseChange.Y;
      MouseChange.X := MouseChange.X * MouseLookHorizontalSensitivity;
      MouseChange.Y := MouseChange.Y * MouseLookVerticalSensitivity;
      ProcessMouseLookDelta(MouseChange);
      Result := ExclusiveEvents;
    end;
  end;

begin
  Result := inherited;
  if Result or (Event.FingerIndex <> 0) then Exit;

  if UsingMouseLook and
    Container.Focused and
    ContainerSizeKnown and
    (not Camera.Animation) then
  begin
    HandleMouseLook;
    Exit;
  end;
end;

function TCastleMouseLookNavigation.UsingMouseLook: Boolean;
begin
  Result := MouseLook and (niNormal in UsingInput) and not CastleDesignMode;
end;

{ TCastleWalkNavigation ---------------------------------------------------------------- }

constructor TCastleWalkNavigation.Create(AOwner: TComponent);
begin
  inherited;

  FRotationHorizontalSpeed := DefaultRotationHorizontalSpeed;
  FRotationVerticalSpeed := DefaultRotationVerticalSpeed;
  FFallSpeedStart := DefaultFallSpeedStart;
  FFallSpeedIncrease := DefaultFallSpeedIncrease;
  FPreferGravityUpForRotations := true;
  FPreferGravityUpForMoving := true;
  FGravity := true;
  FGrowSpeed := DefaultGrowSpeed;
  FFallingEffect := true;
  FIsJumping := false;
  FJumpMaxHeight := DefaultJumpMaxHeight;
  FMinAngleFromGravityUp := DefaultMinAngleFromGravityUp;
  FAllowSlowerRotations := true;
  FCheckModsDown := true;
  FJumpHorizontalSpeedMultiply := DefaultJumpHorizontalSpeedMultiply;
  FJumpTime := DefaultJumpTime;
  FMouseDraggingHorizontalRotationSpeed := DefaultMouseDraggingHorizontalRotationSpeed;
  FMouseDraggingVerticalRotationSpeed := DefaultMouseDraggingVerticalRotationSpeed;
  FMouseDraggingMoveSpeed := DefaultMouseDraggingMoveSpeed;

  FInput_Forward                 := TInputShortcut.Create(Self);
  FInput_Backward                := TInputShortcut.Create(Self);
  FInput_LeftRotate              := TInputShortcut.Create(Self);
  FInput_RightRotate             := TInputShortcut.Create(Self);
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

  Input_Forward                 .Assign(keyW, keyArrowUp);
  Input_Backward                .Assign(keyS, keyArrowDown);
  Input_LeftRotate              .Assign(keyArrowLeft);
  Input_RightRotate             .Assign(keyArrowRight);
  Input_LeftStrafe              .Assign(keyA);
  Input_RightStrafe             .Assign(keyD);
  Input_UpRotate                .Assign(keyNone);
  Input_DownRotate              .Assign(keyNone);
  Input_IncreasePreferredHeight .Assign(keyInsert);
  Input_DecreasePreferredHeight .Assign(keyDelete);
  Input_GravityUp               .Assign(keyNone);
  { For move speed we use also character codes +/-, as numpad
    may be hard to reach on some keyboards (e.g. on laptops). }
  Input_MoveSpeedInc            .Assign(keyNumpadPlus , keyNone, '+');
  Input_MoveSpeedDec            .Assign(keyNumpadMinus, keyNone, '-');
  Input_Jump                    .Assign(keySpace);
  Input_Crouch                  .Assign(keyC);
  Input_Run                     .Assign(keyShift);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_LeftRotate             .SetSubComponent(true);
  Input_RightRotate            .SetSubComponent(true);
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
  Input_LeftRotate             .Name := 'Input_LeftRotate';
  Input_RightRotate            .Name := 'Input_RightRotate';
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

destructor TCastleWalkNavigation.Destroy;
begin
  inherited;
end;

function TCastleWalkNavigation.UseHeadBobbing: boolean;
begin
  Result := Gravity and (HeadBobbing <> 0.0);
end;

function TCastleWalkNavigation.RealPreferredHeightNoHeadBobbing: Single;
begin
  Result := PreferredHeight;

  if IsCrouching then
    Result := Result * CrouchHeight;
end;

function TCastleWalkNavigation.RealPreferredHeight: Single;
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

    BobbingModifier := BobbingModifier * (Result * HeadBobbing);
    Result := Result + BobbingModifier;
  end;
end;

function TCastleWalkNavigation.RealPreferredHeightMargin: Single;
begin
  { I tried using here something smaller like
    SingleEpsilon, but this was not good. }
  Result := RealPreferredHeight * 0.01;
end;

function TCastleWalkNavigation.AdjustPositionForRotationHorizontalPivot(
  const OldDirection, NewDirection: TVector3): TVector3;
var
  Pivot, OldDirectionInGravityPlane, NewDirectionInGravityPlane: TVector3;
begin
  Result := Camera.Position;
  if RotationHorizontalPivot <> 0 then
  begin
    if PreferGravityUpForRotations then
    begin
      Pivot := Camera.Position + OldDirection * RotationHorizontalPivot;
      Result := Pivot - NewDirection * RotationHorizontalPivot;
    end else
    begin
      OldDirectionInGravityPlane := OldDirection;
      if not VectorsParallel(OldDirectionInGravityPlane, Camera.GravityUp) then
        MakeVectorsOrthoOnTheirPlane(OldDirectionInGravityPlane, Camera.GravityUp);
      NewDirectionInGravityPlane := NewDirection;
      if not VectorsParallel(NewDirectionInGravityPlane, Camera.GravityUp) then
        MakeVectorsOrthoOnTheirPlane(NewDirectionInGravityPlane, Camera.GravityUp);
      Pivot := Camera.Position + OldDirectionInGravityPlane * RotationHorizontalPivot;
      Result := Pivot - NewDirectionInGravityPlane * RotationHorizontalPivot;
    end;
  end;
end;

procedure TCastleWalkNavigation.RotateAroundGravityUp(const Angle: Single);
var
  Axis, OldDirection, NewPosition, NewDirection, NewUp: TVector3;
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
  if AngleRadBetweenVectors(Camera.Up, Camera.GravityUp) > Pi/2 then
    Axis := -Camera.GravityUp
  else
    Axis := Camera.GravityUp;

  NewUp := RotatePointAroundAxisRad(Angle, Camera.Up, Axis);

  OldDirection := Camera.Direction;
  NewDirection := RotatePointAroundAxisRad(Angle, Camera.Direction, Axis);

  NewPosition := AdjustPositionForRotationHorizontalPivot(OldDirection, NewDirection);

  Camera.SetView(NewPosition, NewDirection, NewUp);
end;

procedure TCastleWalkNavigation.RotateAroundUp(const Angle: Single);
var
  OldDirection, NewPosition, NewDirection: TVector3;
begin
  { We know that RotatePointAroundAxisRad below doesn't change the length
    of the Direction (so it will remain normalized) and it will keep
    Direction and Up vectors orthogonal. }
  OldDirection := Camera.Direction;
  NewDirection := RotatePointAroundAxisRad(Angle, Camera.Direction, Camera.Up);

  NewPosition := AdjustPositionForRotationHorizontalPivot(OldDirection, NewDirection);

  Camera.SetView(NewPosition, NewDirection, Camera.Up);
end;

procedure TCastleWalkNavigation.RotateHorizontal(const Angle: Single);
begin
  if PreferGravityUpForRotations then
    RotateAroundGravityUp(Angle)
  else
    RotateAroundUp(Angle);
end;

procedure TCastleWalkNavigation.RotateVertical(AngleRad: Single);
var
  Side: TVector3;
  NewDirection, NewUp: TVector3;

  procedure DoRealRotate;
  begin
    { Rotate Up around Side }
    NewUp        := RotatePointAroundAxisRad(AngleRad, Camera.Up,        Side);
    { Rotate Direction around Side }
    NewDirection := RotatePointAroundAxisRad(AngleRad, Camera.Direction, Side);
  end;

var
  AngleRadBetween: Single;
begin
  if PreferGravityUpForRotations and (MinAngleFromGravityUp <> 0.0) then
  begin
    Side := TVector3.CrossProduct(Camera.Direction, Camera.GravityUp);
    if Side.IsZero then
    begin
      { Brutally adjust Direction and Up to be correct.
        This should happen only if your code was changing values of
        PreferGravityUpForRotations and MinAngleFromGravityUp at runtime.
        E.g. first you let Direction and Up to be incorrect,
        and then you set PreferGravityUpForRotations to @true and
        MinAngleFromGravityUp
        to > 0 --- and suddenly we find that Up can be temporarily bad. }
      NewDirection := Camera.InitialDirection;
      NewUp := Camera.InitialUp;

      { Now check Side again. If it's still bad, this means that the
        InitialDirection is parallel to GravityUp. This shouldn't
        happen if you correctly set InitialDirection and GravityUp.
        So just pick any sensible NewDirection to satisfy MinAngleFromGravityUp
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
      Side := TVector3.CrossProduct(Camera.Direction, Camera.GravityUp);
      if Side.IsZero then
      begin
        NewDirection := AnyOrthogonalVector(Camera.GravityUp);
        NewUp := Camera.GravityUp;
      end;
    end else
    begin
      { Calculate AngleRadBetween, and possibly adjust AngleRad. }
      AngleRadBetween := AngleRadBetweenVectors(Camera.Direction, Camera.GravityUp);
      if AngleRadBetween - AngleRad < MinAngleFromGravityUp then
        AngleRad := AngleRadBetween - MinAngleFromGravityUp else
      if AngleRadBetween - AngleRad > Pi - MinAngleFromGravityUp then
        AngleRad := AngleRadBetween - (Pi - MinAngleFromGravityUp);

      DoRealRotate;
    end;
  end else
  begin
    Side := TVector3.CrossProduct(Camera.Direction, Camera.Up);
    DoRealRotate;
  end;

  Camera.SetView(NewDirection, NewUp);
end;

function TCastleWalkNavigation.MoveTo(const ProposedNewPos: TVector3;
  const BecauseOfGravity, CheckClimbHeight: boolean): boolean;
var
  NewPos: TVector3;
  NewIsAbove: boolean;
  NewAboveHeight, OldAbsoluteHeight, NewAbsoluteHeight: Single;
  NewAboveGround: PTriangle;
begin
  Result := MoveAllowed(Camera.Position, ProposedNewPos, NewPos, Radius, BecauseOfGravity);

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
      OldAbsoluteHeight := TVector3.DotProduct(Camera.GravityUp, Camera.Position);
      NewAbsoluteHeight := TVector3.DotProduct(Camera.GravityUp, NewPos);
      Result := not (
        AboveHeight - NewAboveHeight - (OldAbsoluteHeight - NewAbsoluteHeight) >
        ClimbHeight );
      // useful log to test ClimbHeight, but too spammy to be enabled by default
      // if Log and not Result then
      //   WritelnLog('Camera', 'Blocked move because of ClimbHeight (%f).', [ClimbHeight]);
    end;
  end;

  if Result then
    Camera.Position := NewPos;
end;

function TCastleWalkNavigation.Move(const MoveVector: TVector3;
  const BecauseOfGravity, CheckClimbHeight: boolean): boolean;
begin
  Result := MoveTo(Camera.Position + MoveVector, BecauseOfGravity, CheckClimbHeight);
end;

procedure TCastleWalkNavigation.MoveHorizontal(const SecondsPassed: Single; const Multiply: Integer = 1);
var
  Dir: TVector3;
  Multiplier: Single;
begin
  Multiplier := MoveSpeed * MoveHorizontalSpeed * SecondsPassed * Multiply;
  if IsJumping then
    Multiplier := Multiplier * JumpHorizontalSpeedMultiply;
  if Input_Run.IsPressed(Container) then
    Multiplier := Multiplier * 2;

  { Update HeadBobbingPosition }
  if (not IsJumping) and UseHeadBobbing and (not HeadBobbingAlreadyDone) then
  begin
    HeadBobbingPosition := HeadBobbingPosition + (SecondsPassed / HeadBobbingTime);
    HeadBobbingAlreadyDone := true;
  end;

  MoveHorizontalDone := true;

  if PreferGravityUpForMoving then
    Dir := DirectionInGravityPlane else
    Dir := Camera.Direction;

  Move(Dir * Multiplier, false, true);
end;

procedure TCastleWalkNavigation.MoveVertical(const SecondsPassed: Single; const Multiply: Integer);

  { Provided PreferredUpVector must be already normalized. }
  procedure MoveVerticalCore(const PreferredUpVector: TVector3);
  var
    Multiplier: Single;
  begin
    Multiplier := MoveSpeed * MoveVerticalSpeed * SecondsPassed * Multiply;
    if Input_Run.IsPressed(Container) then
      Multiplier := Multiplier * 2;
    Move(PreferredUpVector * Multiplier, false, false);
  end;

begin
  if not Gravity then
  begin
    if PreferGravityUpForMoving then
      MoveVerticalCore(Camera.GravityUp)
    else
      MoveVerticalCore(Camera.Up);
  end;
end;

procedure TCastleWalkNavigation.RotateHorizontalForStrafeMove(const Angle: Single);
begin
  if PreferGravityUpForMoving then
    RotateAroundGravityUp(Angle)
  else
    RotateAroundUp(Angle);
end;

function TCastleWalkNavigation.ReallyEnableMouseDragging: boolean;
begin
  Result := (inherited ReallyEnableMouseDragging) and not UsingMouseLook;
end;

procedure TCastleWalkNavigation.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  { Check are keys for left/right/down/up rotations are pressed, and handle them.
    SpeedScale = 1 indicates a normal rotation speed, you can use it to scale
    the rotation speed to specific purposes. }
  procedure CheckRotates(SpeedScale: Single);
  begin
    if Input_RightRotate.IsPressed(Container) then
      RotateHorizontal(-RotationHorizontalSpeed * SecondsPassed * SpeedScale);
    if Input_LeftRotate.IsPressed(Container) then
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
        FJumpHeight := FJumpHeight + ThisJumpHeight;

        if FJumpHeight > MaxJumpDistance then
          FIsJumping := false else
          { do jumping }
          Move(Camera.GravityUp * ThisJumpHeight, false, false);
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

        Move(Camera.GravityUp * GrowingVectorLength, true, false);

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
      PositionBefore: TVector3;
      FallingVectorLength: Single;
    begin
      Result := false;

      { Note that if we got here, then TryGrow returned false,
        which means that (assuming OnInternalHeight is correctly assigned)
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
      PositionBefore := Camera.Position;

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

      if Move(Camera.GravityUp * (- FallingVectorLength), true, false) and
        (not TVector3.PerfectlyEquals(Camera.Position, PositionBefore)) then
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
            that actually called VisibleChange that possibly
            called OnVisibleChange.
            And OnVisibleChange is user callback and user could do there
            things like
            - Changing FallSpeedStart (but still it's unspecified
              whether we have to apply this change, right ?)
            - Calling CancelFalling and *then* changing FallSpeedStart.
              And in this case, we *must* honour it, because here user
              expects that we will use FallSpeedStart if we want
              to fall down. (of course, one call to "Move" with old
              "FallSpeedStart" was already done, that's unavoidable...).

            TODO: Is the above reasoning still valid? Now only TCastleCamera
            calls VisibleChange.
          }
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
              RotateAroundGravityUp(DegToRad(Fde_RotateHorizontal *
                Fde_HorizontalRotateDeviation * SecondsPassed));
            end;

            if Fde_UpRotate < 0 then
              Fde_UpRotate := Fde_UpRotate - (Fde_VerticalRotateDeviation * SecondsPassed) else
            if Fde_UpRotate > 0 then
              Fde_UpRotate := Fde_UpRotate + (Fde_VerticalRotateDeviation * SecondsPassed) else
              Fde_UpRotate := RandomPlusMinus *
                              Fde_VerticalRotateDeviation * SecondsPassed;
          end;

          { Note that when changing FFallSpeed below I'm using SecondsPassed * 50.
            And also above when using FFallSpeed, I multipled
            FFallSpeed * SecondsPassed * 50. This is correct:
            - changing position based on FallSpeed is a "velocity"
            - changing FallSpeed below is "acceleration"
            And both acceleration and velocity must be time-based. }
          if FallSpeedIncrease <> 1.0 then
            FFallSpeed := FFallSpeed * (Power(FallSpeedIncrease, SecondsPassed * 50));
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
          Fde_UpRotate := Min(Fde_UpRotate + Change, 0.0)
        else
          Fde_UpRotate := Max(Fde_UpRotate - Change, 0.0);
      end;
    end;

    function TryFallingOnTheGround: boolean;
    var
      Angle, AngleRotate: Single;
    begin
      Result := FFallingOnTheGround;
      if not Result then
        Exit;

      Angle := AngleRadBetweenVectors(Camera.Up, Camera.GravityUp);

      if SameValue(Angle, HalfPi, 0.01) then
      begin
        { FallingOnTheGround effect stops here. }
        FFallingOnTheGround := false;
        Exit;
      end;

      AngleRotate := SecondsPassed * 5;
      MinVar(AngleRotate, Abs(Angle - HalfPi));
      if not FFallingOnTheGroundAngleIncrease then
        AngleRotate := -AngleRotate;

      Camera.Up := RotatePointAroundAxisRad(AngleRotate, Camera.Up, DirectionInGravityPlane);
    end;

    procedure DoFall;
    var
      BeginPos, EndPos, FallVector: TVector3;
    begin
      if Assigned(OnFall) then
      begin
        { Project Position and FFallingStartPosition
          onto GravityUp vector to calculate fall height. }
        BeginPos := PointOnLineClosestToPoint(TVector3.Zero, Camera.GravityUp, FFallingStartPosition);
        EndPos   := PointOnLineClosestToPoint(TVector3.Zero, Camera.GravityUp, Camera.Position);
        FallVector := BeginPos - EndPos;

        { Because of various growing and jumping effects (imagine you jump up
          onto a taller pillar) it may turn out that we're higher at the end
          at the end of fall. Do not report it to OnFall event in this case. }
        if TVector3.DotProduct(Camera.GravityUp, FallVector.Normalize) <= 0 then
          Exit;

        OnFall(Self, FallVector.Length);
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
          than SingleEpsilon, just to be on the safe side
          and avoid any "corner cases", when HeadBobbingPosition
          would switch between going up and down repeatedly. }
        FracHeadBobbingPosition := Frac(HeadBobbingPosition);
        if FracHeadBobbingPosition > 0.5 then
        begin
          if 1 - FracHeadBobbingPosition > SingleEpsilon then
            HeadBobbingPosition := HeadBobbingPosition +
              Min(HeadBobbingGoingDownSpeed * SecondsPassed,
                  1 - FracHeadBobbingPosition);
        end else
        begin
          if FracHeadBobbingPosition > SingleEpsilon then
            HeadBobbingPosition := HeadBobbingPosition -
              Min(HeadBobbingGoingDownSpeed * SecondsPassed,
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
      Height(Camera.Position, FIsAbove, FAboveHeight, FAboveGround);

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
    TargetPlane: TVector4;
    TargetPlaneDir: TVector3 absolute TargetPlane;
    TargetUp: TVector3;
    AngleRadBetweenTargetAndGravity: Single;
    AngleRadBetweenTarget, AngleRadBetweenTargetChange: Single;
    NewUp: TVector3;
  begin
    if PreferGravityUp then
    begin
      { TODO: Correcting MinAngleFromGravityUp }

      { Correct Up such that GravityUp, Direction and Up
        are on the same plane.

        Math:
          TargetPlane := common plane of GravityUp and Direction,
          given by (A, B, C) = TVector3.CrossProduct(GravityUp, Direction)
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

      TargetPlaneDir := TVector3.CrossProduct(GravityUp, Direction);
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
        if SameValue(AngleRadBetweenTargetAndGravity, HalfPi) then
          TargetUp := GravityUp else
        if AngleRadBetweenTargetAndGravity > HalfPi then
          TargetUp := -TargetUp;

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
  end;

  procedure MoveViaMouseDragging(Delta: TVector2);
  var
    MoveSizeX, MoveSizeY: Single;
  const
    Tolerance = 5;  { 5px tolerance for not-moving }
  begin
    MoveSizeX := 0;
    MoveSizeY := 0;

    if Abs(Delta.X) < Tolerance then
      Delta.X := 0
    else
      MoveSizeX := (Abs(Delta.X) - Tolerance) * MouseDraggingMoveSpeed;

    if Abs(Delta.Y) < Tolerance then
      Delta.Y := 0
    else
      MoveSizeY := (Abs(Delta.Y) - Tolerance) * MouseDraggingMoveSpeed;

    if buttonLeft in Container.MousePressed then
    begin
      if Delta.Y < -Tolerance then
        MoveHorizontal(-MoveSizeY * SecondsPassed, 1); { forward }
      if Delta.Y > Tolerance then
        MoveHorizontal(-MoveSizeY * SecondsPassed, -1); { backward }

      if Abs(Delta.X) > Tolerance then
        RotateHorizontal(-Delta.X * SecondsPassed * MouseDraggingHorizontalRotationSpeed); { rotate }
    end
    else if buttonRight in Container.MousePressed then
    begin
      if Delta.X < -Tolerance then
      begin
        RotateHorizontalForStrafeMove(HalfPi);
        MoveHorizontal(MoveSizeX * SecondsPassed, 1);  { strife left }
        RotateHorizontalForStrafeMove(-HalfPi);
      end;
      if Delta.X > Tolerance then
      begin
        RotateHorizontalForStrafeMove(-HalfPi);
        MoveHorizontal(MoveSizeX * SecondsPassed, 1);  { strife right }
        RotateHorizontalForStrafeMove(HalfPi);
      end;

      if Delta.Y < -5 then
        MoveVertical(-MoveSizeY * SecondsPassed, 1);    { fly up }
      if Delta.Y > 5 then
        MoveVertical(-MoveSizeY * SecondsPassed, -1);   { fly down }
    end;
  end;

var
  ModsDown: TModifierKeys;
begin
  inherited;

  { update Cursor every frame, in case InternalViewport.Paused changed
    (which changes UsingInput and UsingMouseLook) }
  if UsingMouseLook then
    Cursor := mcForceNone
  else
    Cursor := mcDefault;

  { Do not handle keys or gravity etc. }
  if Camera.Animation then Exit;

  ModsDown := ModifiersDown(Container.Pressed);

  HeadBobbingAlreadyDone := false;
  MoveHorizontalDone := false;

  if HandleInput then
  begin
    if niNormal in UsingInput then
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
          RotateHorizontalForStrafeMove(-HalfPi);
          MoveHorizontal(SecondsPassed, 1);
          RotateHorizontalForStrafeMove(HalfPi);
        end;

        if Input_LeftStrafe.IsPressed(Container) then
        begin
          RotateHorizontalForStrafeMove(HalfPi);
          MoveHorizontal(SecondsPassed, 1);
          RotateHorizontalForStrafeMove(-HalfPi);
        end;

        { A simple implementation of Input_Jump was
            RotateVertical(HalfPi); Move(MoveVerticalSpeed * MoveSpeed * SecondsPassed); RotateVertical(-HalfPi)
          Similarly, simple implementation of Input_Crouch was
            RotateVertical(-HalfPi); Move(MoveVerticalSpeed * MoveSpeed * SecondsPassed); RotateVertical(HalfPi)
          But this is not good, because when PreferGravityUp, we want to move
          along the GravityUp. (Also later note: RotateVertical is now bounded by
          MinAngleFromGravityUp). }

        if Input_Jump.IsPressed(Container) then
          MoveVertical(SecondsPassed, 1);
        if Input_Crouch.IsPressed(Container) then
          MoveVertical(SecondsPassed, -1);

        { How to apply SecondsPassed here ?
          I can't just ignore SecondsPassed, but I can't also write
            FMoveSpeed := FMoveSpeed * (10 * SecondsPassed);
          What I want is such continuous function that e.g.
            F(FMoveSpeed, 10) = F(F(FMoveSpeed, 1), 1)
          I.e. SecondsPassed = 10 should work just like doing the same change twice.
          So F is FMoveSpeed * Power(10, SecondsPassed)
          Easy!
        }
        if Input_MoveSpeedInc.IsPressed(Container) then
          MoveSpeed := MoveSpeed * Power(10, SecondsPassed);

        if Input_MoveSpeedDec.IsPressed(Container) then
          MoveSpeed := MoveSpeed / Power(10, SecondsPassed);
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
       ((buttonLeft in Container.MousePressed) or (buttonRight in Container.MousePressed)) and
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

  { Disable gravity in design mode (in the future we may add optional way to enable them) }
  if not CastleDesignMode then
    GravityUpdate;
end;

function TCastleWalkNavigation.Jump: boolean;
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
  Height(Camera.Position, FIsAbove, FAboveHeight, FAboveGround);

  if AboveHeight > RealPreferredHeight + RealPreferredHeightMargin then
    Exit;

  FIsJumping := true;
  FJumpHeight := 0.0;
  Result := true;
end;

function TCastleWalkNavigation.AllowSuspendForInput: boolean;
begin
  Result := false;
end;

function TCastleWalkNavigation.Press(const Event: TInputPressRelease): boolean;

  procedure SetUpToGravityUp;
  var
    NewDirection, NewUp: TVector3;
  begin
    if VectorsParallel(Camera.Direction, Camera.GravityUp) then
    begin
      { We can't carelessly set Up to something parallel to GravityUp
        in this case.

        Yes, this situation can happen: for example open a model with
        no viewpoint in VRML in view3dscene (so default viewpoint,
        both gravity and Up = +Y is used). Then change GravityUp
        by menu and press Home (Input_GravityUp). }

      NewUp := Camera.GravityUp;
      NewDirection := AnyOrthogonalVector(NewUp);
      Camera.SetView(NewDirection, NewUp);
    end else
      Camera.Up := Camera.GravityUp;
  end;

const
  MouseWheelScrollSpeed = Pi * 3 / 180.0;
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
    RotateVertical(-Event.MouseWheelScroll * MouseWheelScrollSpeed);
    Result := true;
    Exit;
  end;

  if (not (niNormal in UsingInput)) or Camera.Animation then Exit(false);

  if Input_GravityUp.IsEvent(Event) then
  begin
    SetUpToGravityUp;
    Result := ExclusiveEvents;
  end else
  if Input_Jump.IsEvent(Event) then
  begin
    Result := Jump and ExclusiveEvents;
  end else
    Result := false;
end;

function TCastleWalkNavigation.SensorTranslation(const X, Y, Z, Length: Double;
  const SecondsPassed: Single): boolean;
var
  MoveSize: Double;
begin
  if not (ni3dMouse in UsingInput) then Exit(false);
  Result := true;

  MoveSize := Length * SecondsPassed / 5000;

  if Z > 5 then
    MoveHorizontal(Z * MoveSize, -1); { backward }
  if Z < -5 then
    MoveHorizontal(-Z * MoveSize, 1); { forward }

  if X > 5 then
  begin
    RotateHorizontalForStrafeMove(-HalfPi);
    MoveHorizontal(X * MoveSize, 1);  { right }
    RotateHorizontalForStrafeMove(HalfPi);
  end;
  if X < -5 then
  begin
    RotateHorizontalForStrafeMove(HalfPi);
    MoveHorizontal(-X * MoveSize, 1); { left }
    RotateHorizontalForStrafeMove(-HalfPi);
  end;

  if Y > 5 then
    MoveVertical(Y * MoveSize, 1);    { up }
  if Y < -5 then
    MoveVertical(-Y * MoveSize, -1);  { down }
end;

function TCastleWalkNavigation.SensorRotation(const X, Y, Z, Angle: Double;
  const SecondsPassed: Single): boolean;
const
  SpeedSensor = 2;
begin
  if not (ni3dMouse in UsingInput) then Exit(false);
  Result := true;

  if Abs(X) > 0.4 then      { tilt forward / backward }
    RotateVertical(X * Angle * SpeedSensor * SecondsPassed);
  if Abs(Y) > 0.4 then      { rotate }
    RotateHorizontal(Y * Angle * SpeedSensor * SecondsPassed);
  {if Abs(Z) > 0.4 then ?} { tilt sidewards }
end;

procedure TCastleWalkNavigation.Init(
  const AInitialPosition, AInitialDirection, AInitialUp: TVector3;
  const AGravityUp: TVector3;
  const APreferredHeight: Single;
  const ARadius: Single);
begin
  PreferredHeight := APreferredHeight;
  Radius := ARadius;
  CorrectPreferredHeight;

  Camera.Init(AInitialPosition, AInitialDirection, AInitialUp, AGravityUp);
  Camera.ProjectionNear := Radius * RadiusToProjectionNear;
end;

procedure TCastleWalkNavigation.Init(const Box: TBox3D; const ARadius: Single);
var
  Pos: TVector3;
  AvgSize: Single;
begin
  if Box.IsEmptyOrZero then
  begin
    Radius := ARadius;
    PreferredHeight := Max(DefaultPreferredHeight, RadiusToPreferredHeightMin * ARadius);
    CorrectPreferredHeight;

    Camera.ProjectionNear := Radius * RadiusToProjectionNear;

    Camera.Init(TVector3.Zero,
      DefaultCameraDirection,
      DefaultCameraUp,
      DefaultCameraUp);
  end else
  begin
    Radius := ARadius;
    AvgSize := Box.AverageSize;
    PreferredHeight := AvgSize * 5;
    CorrectPreferredHeight;

    Camera.ProjectionNear := Radius * RadiusToProjectionNear;

    Pos := Vector3(
      Box.Data[0].X - AvgSize,
      (Box.Data[0].Y + Box.Data[1].Y) / 2,
      (Box.Data[0].Z + Box.Data[1].Z) / 2
    );
    Camera.Init(Pos,
      DefaultCameraDirection,
      DefaultCameraUp,
      DefaultCameraUp);
  end;
end;

procedure TCastleWalkNavigation.UpPrefer(const AUp: TVector3);
begin
  Camera.UpPrefer(AUp);
end;

function TCastleWalkNavigation.MaxJumpDistance: Single;
begin
  Result := JumpMaxHeight * PreferredHeight;
end;

function TCastleWalkNavigation.DirectionInGravityPlane: TVector3;
begin
  Result := Camera.Direction;

  if not VectorsParallel(Result, Camera.GravityUp) then
    MakeVectorsOrthoOnTheirPlane(Result, Camera.GravityUp);
end;

procedure TCastleWalkNavigation.FallOnTheGround;
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

procedure TCastleWalkNavigation.CancelFalling;
begin
  { Fortunately implementation of this is brutally simple right now. }
  FFalling := false;
end;

procedure TCastleWalkNavigation.ProcessMouseLookDelta(const Delta: TVector2);
begin
  inherited;
  RotateHorizontal(-Delta.X);
  RotateVertical(Delta.Y);
end;

function TCastleWalkNavigation.Motion(const Event: TInputMotion): boolean;

  procedure HandleMouseDrag;
  var
    MouseChange: TVector2;
  begin
    MouseChange := Event.Position - Container.MousePosition;
    if MouseChange[0] <> 0 then
      RotateHorizontal(-MouseChange[0] * MouseDraggingHorizontalRotationSpeed);
    if MouseChange[1] <> 0 then
      RotateVertical(MouseChange[1] * MouseDraggingVerticalRotationSpeed);
  end;

begin
  Result := inherited;
  if Result or (Event.FingerIndex <> 0) then Exit;

  if (MouseDraggingStarted <> -1) and
    // Not need to check here ReallyEnableMouseDragging, as MouseDraggingStarted is already <> -1
    // ReallyEnableMouseDragging and
    (MouseDragMode = mdRotate) and
    (not Camera.Animation) and
    (not UsingMouseLook) then
  begin
    HandleMouseDrag;
    Result := ExclusiveEvents;
  end;
end;

function TCastleWalkNavigation.GetNavigationType: TNavigationType;
begin
  if Input = [] then
    Result := ntNone
  else
  if Gravity then
    Result := ntWalk
  else
    Result := ntFly;
end;

function TCastleWalkNavigation.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Gravity') or
     (PropertyName = 'MoveSpeed') or
     (PropertyName = 'Radius') or
     (PropertyName = 'PreferredHeight') or
     (PropertyName = 'MoveHorizontalSpeed') or
     (PropertyName = 'MoveVerticalSpeed') or
     (PropertyName = 'MouseDraggingHorizontalRotationSpeed' ) or
     (PropertyName = 'MouseDraggingVerticalRotationSpeed' ) or
     (PropertyName = 'MouseDraggingMoveSpeed') or
     (PropertyName = 'MouseDragMode') or
     (PropertyName = 'RotationHorizontalSpeed') or
     (PropertyName = 'RotationVerticalSpeed') or
     (PropertyName = 'MouseLook') or
     (PropertyName = 'MouseLookHorizontalSensitivity') or
     (PropertyName = 'MouseLookVerticalSensitivity') or
     (PropertyName = 'InvertVerticalMouseLook') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

class procedure TCastleWalkNavigation.CreateComponentFly(Sender: TObject);
begin
  (Sender as TCastleWalkNavigation).Gravity := false;
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

function OrientationQuaternionFromDirectionUp(const Direction, Up: TVector3): TQuaternion;
begin
  Result := OrientationQuaternionFromDirectionUp(Direction, Up,
    DefaultCameraDirection, DefaultCameraUp);
end;

function OrientationQuaternionFromDirectionUp(Direction, Up: TVector3;
  const DefaultDirection, DefaultUp: TVector3): TQuaternion;

{ This was initially based on Stephen Chenney's ANSI C code orient.c,
  available still from here: http://vrmlworks.crispen.org/tools.html
  I rewrote it a couple of times, possibly removing and possibly adding
  some bugs :)

  Idea: we want to convert Direction and Up into VRML orientation,
  which is a rotation from DefaultDirection/DefaultUp into Direction/Up.

  1) Take vector orthogonal to standard DefaultDirection and Direction.
     Rotate around it, to match DefaultDirection with Direction.

  2) Now rotate around Direction such that standard up (already rotated
     by 1st transform) matches with Up. We know it's possible,
     since Direction and Up are orthogonal and normalized,
     just like standard DefaultDirection/DefaultUp.

  Combine these two rotations and you have the result.

  How to combine two rotations, such that in the end you get nice
  single rotation? That's where quaternions rule.
}

  function QuatFromAxisAngleCos(const Axis: TVector3;
    const AngleRadCos: Single): TQuaternion;
  begin
    Result := QuatFromAxisAngle(Axis, ArcCos(Clamped(AngleRadCos, -1.0, 1.0)));
  end;

var
  Rot1Axis, Rot2Axis, DefaultUpAfterRot1: TVector3;
  Rot1Quat, Rot2Quat: TQuaternion;
  Rot1CosAngle, Rot2CosAngle: Single;
begin
  Direction := Direction.Normalize;
  Up := Up.Normalize;

  { calculate Rot1Quat }
  Rot1Axis := TVector3.CrossProduct(DefaultDirection, Direction);
  { Rot1Axis may be zero if DefaultDirection and Direction are parallel.
    When they point in the same direction, then it doesn't matter
    (rotation will be by 0 angle anyway), but when they are in opposite
    direction we want to do some rotation, so we need some non-zero
    sensible Rot1Axis. }
  if Rot1Axis.IsZero then
    Rot1Axis := DefaultUp
  else
    { Normalize *after* checking ZeroVector, otherwise normalization
      could change some almost-zero vector into a (practically random)
      vector of length 1. }
    Rot1Axis := Rot1Axis.Normalize;
  Rot1CosAngle := TVector3.DotProduct(DefaultDirection, Direction);
  Rot1Quat := QuatFromAxisAngleCos(Rot1Axis, Rot1CosAngle);

  { calculate Rot2Quat }
  DefaultUpAfterRot1 := Rot1Quat.Rotate(DefaultUp);
  { We know Rot2Axis should be either Direction or -Direction. But how do we know
    which one? (To make the rotation around it in correct direction.)
    Calculating Rot2Axis below is a solution. }
  Rot2Axis := TVector3.CrossProduct(DefaultUpAfterRot1, Up);

  (*We could now do Rot2Axis := Rot2Axis.Normalize,
    after making sure it's not zero. Like

    { we need larger epsilon for ZeroVector below, in case
      DefaultUpAfterRot1 is = -Up.
      testcameras.pas contains testcases that require it. }
    if Rot2Axis.IsZero(0.001) then
      Rot2Axis := Direction else
      { Normalize *after* checking ZeroVector, otherwise normalization
        could change some almost-zero vector into a (practically random)
        vector of length 1. }
      Rot2Axis := Rot2Axis.Normalize;

    And later do

      { epsilon for TVector3.Equals 0.001 is too small }
      Assert( TVector3.Equals(Rot2Axis,  Direction, 0.01) or
              TVector3.Equals(Rot2Axis, -Direction, 0.01),
        Format('OrientationQuaternionFromDirectionUp failed for Direction, Up: (%s), (%s)',
          [ Direction.ToRawString, Up.ToRawString ]));

    However, as can be seen in above comments, this requires some careful
    adjustments of epsilons, so it's somewhat numerically unstable.
    It's better to just use now the knowledge that Rot2Axis
    is either Direction or -Direction, and choose one of them. *)
  if AreParallelVectorsSameDirection(Rot2Axis, Direction) then
    Rot2Axis :=  Direction else
    Rot2Axis := -Direction;

  Rot2CosAngle := TVector3.DotProduct(DefaultUpAfterRot1, Up);
  Rot2Quat := QuatFromAxisAngleCos(Rot2Axis, Rot2CosAngle);

  { calculate Result = combine Rot1 and Rot2 (yes, the order
    for QuatMultiply is reversed) }
  Result := Rot2Quat * Rot1Quat;
end;

procedure OrientationFromDirectionUp(const Direction, Up: TVector3;
  out Axis: TVector3; out Angle: Single);
begin
  { Call OrientationQuaternionFromDirectionUp,
    and extract the axis and angle from the quaternion. }
  OrientationQuaternionFromDirectionUp(Direction, Up).ToAxisAngle(Axis, Angle);
end;

function OrientationFromDirectionUp(const Direction, Up: TVector3): TVector4;
var
  Axis: TVector3;
  Angle: Single;
begin
  OrientationFromDirectionUp(Direction, Up, Axis, Angle);
  Result := Vector4(Axis, Angle);
end;

function OrientationFromDirectionUp(const Direction, Up,
  DefaultDirection, DefaultUp: TVector3): TVector4;
var
  Axis: TVector3;
  Angle: Single;
begin
  OrientationQuaternionFromDirectionUp(Direction, Up, DefaultDirection, DefaultUp).
    ToAxisAngle(Axis, Angle);
  Result := Vector4(Axis, Angle);
end;

function OrientationToDirection(const OrientationRotation: TVector4): TVector3;
begin
  Result := RotatePointAroundAxis(OrientationRotation, DefaultCameraDirection);
end;

function OrientationToUp(const OrientationRotation: TVector4): TVector3;
begin
  Result := RotatePointAroundAxis(OrientationRotation, DefaultCameraUp);
end;

function CamDirUp2Orient(const Direction, Up: TVector3): TVector4;
begin
  Result := OrientationFromDirectionUp(Direction, Up);
end;

procedure CamDirUp2Orient(const Direction, Up: TVector3;
  out Axis: TVector3; out Angle: Single);
begin
  OrientationFromDirectionUp(Direction, Up, Axis, Angle);
end;

function CamDirUp2OrientQuat(const Direction, Up: TVector3): TQuaternion;
begin
  Result := OrientationQuaternionFromDirectionUp(Direction, Up);
end;

procedure CameraViewpointForWholeScene(const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean;
  out Position, Direction, Up, GravityUp: TVector3);
var
  Offset: Single;
begin
  Direction := TVector3.One[WantedDirection];
  if not WantedDirectionPositive then Direction := -Direction;

  Up := TVector3.One[WantedUp];
  if not WantedUpPositive then Up := -Up;

  if Box.IsEmpty then
  begin
    { If box is empty, choose the default X3D viewpoint.
      This is least surprising, see https://github.com/castle-engine/view3dscene/issues/3 }
    Position  := Vector3(0, 0, 10); // DefaultX3DCameraPosition[cvVrml2_X3d];
  end else
  begin
    Position := Box.Center;
    Offset := 2 * Box.AverageSize;

    if WantedDirectionPositive then
      Position.InternalData[WantedDirection] := Box.Data[0].InternalData[WantedDirection] - Offset
    else
      Position.InternalData[WantedDirection] := Box.Data[1].InternalData[WantedDirection] + Offset;
  end;

  { GravityUp is just always equal Up here. }
  GravityUp := Up;
end;

procedure CameraOrthoViewpointForWholeScene(const Box: TBox3D;
  const ViewportWidth, ViewportHeight: Single;
  const Origin: TVector2;
  out Position: TVector3;
  out AProjectionWidth, AProjectionHeight, AProjectionFar: Single);

  { Calculate Position.XY and AProjectionWidth, AProjectionHeight. }
  function PositionXY: TVector2;
  var
    Rect: TFloatRectangle;
    EffectiveProjectionWidth, EffectiveProjectionHeight: Single;
  begin
    if Box.IsEmpty then
    begin
      Result := Vector2(0, 0);
      AProjectionWidth := 0;
      AProjectionHeight := 0;
    end else
    begin
      Rect := Box.RectangleXY;

      if ViewportWidth / ViewportHeight >
         Rect.Width / Rect.Height then
      begin
        AProjectionWidth := 0;
        AProjectionHeight := Rect.Height;
        { Calculate EffectiveProjectionXxx
          the same way that TCastleOrthographic.EffectiveWidth/Height would be calculated. }
        EffectiveProjectionWidth := Rect.Height * ViewportWidth / ViewportHeight;
        EffectiveProjectionHeight := AProjectionHeight;
      end else
      begin
        AProjectionWidth := Rect.Width;
        AProjectionHeight := 0;
        { Calculate EffectiveProjectionXxx
          the same way that TCastleOrthographic.EffectiveWidth/Height would be calculated. }
        EffectiveProjectionWidth := AProjectionWidth;
        EffectiveProjectionHeight := Rect.Width * ViewportHeight / ViewportWidth;
      end;

      // calculate PositionXY
      Result := Rect.Center +
        (Origin - Vector2(0.5, 0.5)) *
        Vector2(EffectiveProjectionWidth, EffectiveProjectionHeight);
    end;
  end;

  { Calculate Position.Z and AProjectionFar. }
  function PositionZ: Single;
  const
    Default2DProjectionFar = TCastleViewport.Default2DProjectionFar;
    Default2DCameraZ = TCastleViewport.Default2DCameraZ;
  var
    MinZ, MaxZ: Single;
  begin
    if Box.IsEmpty then
    begin
      Result := Default2DCameraZ;
      AProjectionFar := Default2DProjectionFar;
    end else
    begin
      MinZ := Box.Min.Z;
      MaxZ := Box.Max.Z;
      if (MinZ > - Default2DProjectionFar / 2) and
         (MaxZ < Default2DProjectionFar / 2) then
      begin
        // prefer to use Default2DProjectionFar and Default2DCameraZ, if the scene fits inside
        Result := Default2DCameraZ;
        AProjectionFar := Default2DProjectionFar;
      end else
      begin
        Result := MaxZ + 1;
        AProjectionFar := MaxZ - MinZ;
      end;
    end;
  end;

begin
  Position := Vector3(PositionXY, PositionZ);
end;

var
  R: TRegisteredComponent;
initialization
  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleWalkNavigation;
  R.Caption := 'Fly (Walk with Gravity=false)';
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleWalkNavigation{$ifdef FPC}(nil){$endif}.CreateComponentFly;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleWalkNavigation, 'Walk');
  RegisterSerializableComponent(TCastleExamineNavigation, 'Examine');
  RegisterSerializableComponent(TCastle2DNavigation, '2D');
end.
