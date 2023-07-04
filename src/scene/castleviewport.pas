{
  Copyright 2009-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Viewport to display scenes (TCastleViewport). }
unit CastleViewport;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleVectors, X3DNodes, CastleInternalBaseTriangleOctree, CastleScene,
  CastleSceneCore, CastleCameras, CastleRenderOptions,
  CastleInternalGLShadowVolumes, CastleUIControls, CastleTransform, CastleTriangles,
  CastleKeysMouse, CastleBoxes, CastleInternalBackgroundRenderer, CastleUtils,
  CastleClassUtils, CastleShapes,
  CastleGLShaders, CastleGLImages, CastleTimeUtils, CastleControls,
  CastleInputs, CastleRectangles, CastleColors, CastleComponentSerialize,
  CastleProjection, CastleScreenEffects, CastleInternalShapesRenderer,
  CastleInternalScreenEffects;

type
  TCastleViewport = class;
  TCastleSceneManager = class;

  TRenderOnePassEvent = procedure (Viewport: TCastleViewport;
    const Params: TRenderParams) of object;

  { Event for @link(TCastleViewport.OnProjection). }
  TProjectionEvent = procedure (var Parameters: TProjection) of object;

  TInternalDesignNavigationType = (dnFly, dnExamine, dn2D);

  { Viewport displays a tree of scenes and transformations
    (TCastleTransform and descendants of it, like TCastleScene).
    Add the scenes and transformations to @link(Items).
    See https://castle-engine.io/viewport_and_scenes .

    Each viewport has a @link(Camera) with a position and orientation.
    Viewport may have multiple cameras.

    Viewport may also have a navigation that allows to move
    camera by keyboard, mouse and other inputs.
    You can use any navigation method implemented in the engine
    (TCastleExamineNavigation, TCastleWalkNavigation, TCastleThirdPersonNavigation)
    or implement your own
    (you can create your own descendants of @link(TCastleNavigation),
    or just move/rotate the camera by calling @link(TCastleTransform.SetWorldView Viewport.Camera.SetWorldView) from anywhere).
    Just add @link(TCastleNavigation) as a child of @link(TCastleViewport).

    Viewport may display a background.
    It may be a solid color, a 3D skybox or a gradient.
    See @link(Background), @link(BackgroundColor) properties and
    https://castle-engine.io/background .
    Alternatively, viewport background may be transparent
    (so other TCastleUserInterface underneath will be visible)
    if @link(Transparent).

    Multiple viewports can display the same world.
    To do this, simply copy @link(Items) reference from one
    TCastleViewport to another. You can also just create your own @link(TCastleRootTransform)
    instance and then assign it to multiple viewports.
    This allows e.g. to make a split-screen game (played by 2 people,
    with 2 viewports, on a single monitor).
    Or you can show in a 3D FPS game an additional view from some security camera,
    or from a flying rocket.
    See https://castle-engine.io/multiple_viewports_to_display_one_world . }
  TCastleViewport = class(TCastleScreenEffects)
  strict private
    type
      TSSAOScreenEffect = class(TGLSLScreenEffect)
        Viewport: TCastleViewport;
        function SetupUniforms(var BoundTextureUnits: Cardinal): Boolean; override;
      end;
      TSSRScreenEffect = class(TGLSLScreenEffect)
        Viewport: TCastleViewport;
        function SetupUniforms(var BoundTextureUnits: Cardinal): Boolean; override;
      end;
      TViewportRenderParams = class(TRenderParams)
      private
        FGlobalLights: TLightInstancesList;
      public
        constructor Create;
        destructor Destroy; override;
        function GlobalLights: TAbstractLightInstancesList; override;
      end;

    var
      FCamera: TCastleCamera;
      FCameraObserver: TFreeNotificationObserver;
      FRenderParams: TViewportRenderParams;
      FPrepareParams: TPrepareParams;
      FBackgroundWireframe: boolean;
      FBackgroundColor: TCastleColor;
      FUseGlobalLights, FUseGlobalFog: boolean;
      FApproximateActivation: boolean;
      FTransparent, FClearDepth: boolean;
      LastPressEvent: TInputPressRelease;
      FOnProjection: TProjectionEvent;
      FEnableParentDragging: boolean;
      AssignDefaultCameraDone: Boolean;
      FAutoCamera: Boolean;

      FShadowVolumes: boolean;
      FShadowVolumesRender: boolean;

      FScreenSpaceAmbientOcclusion: boolean;
      SSAOShader: TSSAOScreenEffect;
      SSAOShaderInitialized: Boolean;

      FScreenSpaceReflections: Boolean;
      SSRShader: TSSRScreenEffect;
      SSRShaderInitialized: Boolean;
      FScreenSpaceReflectionsSurfaceGlossiness: Single;

      FOnCameraChanged: TNotifyEvent;
      { Renderer of shadow volumes. You can use this to optimize rendering
        of your shadow quads in RenderShadowVolume, and you can read
        it's statistics (TGLShadowVolumeRenderer.Count and related properties).

        @nil when OpenGL context is not yet initialized. }
      FShadowVolumeRenderer: TGLShadowVolumeRenderer;
      FItems: TCastleRootTransform;
      FItemsObserver: TFreeNotificationObserver;
      LastVisibleStateIdForVisibleChange: TFrameId;

      FOnBoundViewpointChanged: TNotifyEvent;
      FOnBoundNavigationInfoChanged: TNotifyEvent;
      FMouseRayHit: TRayCollision;
      MouseRayOrigin, MouseRayDirection: TVector3;
      FAvoidNavigationCollisions: TCastleTransform;
      PrepareResourcesDone: Boolean;
      FPreventInfiniteFallingDown: Boolean;
      FCapturePointingDevice: TCastleTransform;
      FCapturePointingDeviceObserver: TFreeNotificationObserver;
      FLastSeenMainScene: TCastleScene; // only used by editor
      FBackground: TCastleBackground;
      FBackgroundObserver: TFreeNotificationObserver;
      FFog: TCastleFog;
      FFogObserver: TFreeNotificationObserver;
      // reused between frames for speed
      FRenderWithoutScreenEffectsRenderingCamera: TRenderingCamera;
      FMissingCameraRect: TCastleRectangleControl;
      FMissingCameraLabel: TCastleLabel;
      FInternalDesignManipulation: Boolean;
      FInternalDesignNavigationType: TInternalDesignNavigationType;
      FInternalDesignNavigations: array [TInternalDesignNavigationType] of TCastleNavigation;
      FInternalGridAxis: Boolean;
      FGizmoGridAxis: TInternalCastleEditorGizmo;
      FWarningZFarInfinityDone: Boolean;
      FDynamicBatching: Boolean;
      FOcclusionCulling: Boolean;
      FOcclusionSort: TShapeSort;
      FBlendingSort: TShapeSort;
      FOnCustomShapeSort: TShapeSortEvent;

      ShapesCollector: TShapesCollector;
      ShapesRenderer: TShapesRenderer;

    procedure CommonCreate(const AOwner: TComponent; const ADesignManipulation: Boolean);
    function FillsWholeContainer: boolean;
    procedure SetScreenSpaceAmbientOcclusion(const Value: boolean);
    procedure SetScreenSpaceReflections(const Value: Boolean);
    procedure SetScreenSpaceReflectionsSurfaceGlossiness(const Value: Single);
    procedure SetupDesignTimeCamera;
    procedure SetupChildren2D;
    procedure SetupChildren3D;
    procedure SSAOShaderInitialize;
    procedure SSRShaderInitialize;
    procedure SetAutoCamera(const Value: Boolean);
    procedure SetItems(Value: TCastleRootTransform);
    function GetPaused: Boolean;
    procedure SetPaused(const Value: Boolean);
    procedure SetBackground(const Value: TCastleBackground);
    procedure BackgroundFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetFog(const Value: TCastleFog);
    procedure FogFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetInternalDesignNavigationType(const Value: TInternalDesignNavigationType);
    procedure SetInternalGridAxis(const Value: Boolean);

    { Callbacks when MainCamera is notified that MainScene changes camera/navigation }
    procedure MainSceneAndCamera_BoundViewpointChanged(Sender: TObject);
    procedure MainSceneAndCamera_BoundViewpointVectorsChanged(Sender: TObject);
    procedure MainSceneAndCamera_BoundNavigationInfoChanged(Sender: TObject);

    procedure SetMouseRayHit(const Value: TRayCollision);
    function MouseRayHitContains(const Item: TCastleTransform): boolean;
    procedure SetAvoidNavigationCollisions(const Value: TCastleTransform);

    function GetNavigation: TCastleNavigation;
    procedure SetNavigation(const Value: TCastleNavigation);

    { Cast a ray that can collide with whole world (except AvoidNavigationCollisions).
      Given parameters are in world coordinates. }
    function CameraRayCollision(const RayOrigin, RayDirection: TVector3): TRayCollision;

    procedure SetSceneManager(const Value: TCastleSceneManager);
    { Get current Container.MousePosition.
      Secured in case Container not assigned (returns @false)
      or when Navigation uses MouseLook (in which case, returns the middle of our area,
      the actual mouse position should not be even visible to the user). }
    function GetMousePosition(out MousePosition: TVector2): Boolean;

    procedure SetCapturePointingDevice(const Value: TCastleTransform);
    { Once a TCastleTransform will handle a PointingDevicePress event,
      we make sure to pass subsequent PointingDeviceMove and PointingDeviceRelease to it too,
      before other TCastleTransform instances,
      and regardless if ray hits this TCastleTransform still.
      This way TCastleTransform can handle e.g. dragging with mouse/touch,
      regardless if mouse/touch stays over this transform.

      The reason and implementation of this mechanism is similar to
      TCastleContainer.FCaptureInput. }
    property CapturePointingDevice: TCastleTransform
      read FCapturePointingDevice write SetCapturePointingDevice;
    procedure CapturePointingDeviceFreeNotification(const Sender: TFreeNotificationObserver);

    { Initialize TRayCollisionNode to specify collision with given TCastleTransform
      without any specific Triangle/Point.
      The given RayOrigin / RayOrigin are in world coordinates
      (internally we will convert them to TCastleTransform coordinates). }
    function FakeRayCollisionNode(const RayOriginWorld, RayDirectionWorld: TVector3;
      const Item: TCastleTransform): TRayCollisionNode;

    { Whether to look at AvoidNavigationCollisions.
      Checks that AvoidNavigationCollisions is set, and it is part of current @link(Items). }
    function UseAvoidNavigationCollisions: Boolean;

    { Ensure Camera and FProjection are initialized for PositionToXxx family of methods. }
    procedure PositionToPrerequisites;

    { Notification done by TCastleCamera when our Camera state changed. }
    procedure InternalCameraChanged(Sender: TObject);

    procedure SetCamera(const Value: TCastleCamera);
    procedure CameraFreeNotification(const Sender: TFreeNotificationObserver);
    procedure ItemsFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetDynamicBatching(const Value: Boolean);
    procedure SetOcclusionCulling(const Value: Boolean);
  private
    var
      FProjection: TProjection;
      FSceneManager: TCastleSceneManager;

    { Make sure to call AssignDefaultCamera, if needed because of AutoCamera. }
    procedure EnsureCameraDetected;

    class procedure CreateComponentWithChildren2D(Sender: TObject);
    class procedure CreateComponentWithChildren3D(Sender: TObject);

    procedure RecalculateCursor(Sender: TObject);

    { Bounding box of everything non-design.
      Similar to just using Items.BoundingBox, but

      1. handles Items=nil case OK

      2. ignores bbox at design-time of gizmos (lights, cameras, visualize transform).
      This is important to avoid AutoCamera at design-time to calculate something unexpected
      (move camera far away), because it would adjust to the camera and lights gizmo bbox
      (see TTestCastleViewport.TestAutoCameraIgnoresGizmos). }
    function ItemsBoundingBox: TBox3D;

    { Bounding box of everything, including design-time gizmos (because you also want to see
      them accounted for in design-time ortho camera ProjectionNear/Far).
      Similar to just usign Items.BoundingBox, but handles Items=nil case OK. }
    function ItemsWithGizmosBoundingBox: TBox3D;

    { Set the projection parameters and matrix.
      Used by our Render method.

      This cooperates closely with current @link(Camera) definition.

      If AutoCamera then the initial and current @link(Camera) vectors
      are also initialized here (see TCastleCamera.Init
      and @link(AssignDefaultCamera).

      This takes care to always update Camera.ProjectionMatrix, Projection. }
    procedure ApplyProjection; virtual;

    { Render shadow quads for all the things rendered by @link(RenderOnePass).
      You can use here ShadowVolumeRenderer instance, which is guaranteed
      to be initialized with TGLShadowVolumeRenderer.InitFrustumAndLight,
      so you can do shadow volumes culling. }
    procedure RenderShadowVolume(const Params: TRenderParams);

    { Detect position/direction of the main light that produces shadow volumes.
      Looks at MainScene.InternalMainLightForShadowVolumes.
      Returns light position (or direction, if W = 0) in world space. }
    function MainLightForShadowVolumes(out AMainLightPosition: TVector4): boolean;

    { Pass pointing device (mouse or touch) press event
      to TCastleTransform instances in @link(Items).
      Depends that MouseRayHit, MouseRayOrigin, MouseRayDirection are already updated. }
    function PointingDevicePress: boolean;

    { Pass pointing device (mouse or touch) press event
      to TCastleTransform instances in @link(Items).
      Depends that MouseRayHit, MouseRayOrigin, MouseRayDirection are already updated. }
    function PointingDeviceRelease: boolean;

    { Pass pointing device (mouse or touch) move event
      to TCastleTransform instances in @link(Items).
      Depends that MouseRayHit, MouseRayOrigin, MouseRayDirection are already updated. }
    function PointingDeviceMove: boolean;

    { Update MouseRayHit. }
    procedure UpdateMouseRayHit;
  protected
    { @exclude }
    function InternalOverride2DProjectionSizing: TCastleUserInterface; virtual;

    { Calculate projection parameters. Determines if the view is perspective
      or orthogonal and exact field of view parameters.
      Called each time at the beginning of rendering.

      The default implementation of this method in TCastleViewport
      calculates projection based on the @link(Camera) parameters.

      In turn, the @link(Camera) parameters may be automatically
      calculated (if @link(AutoCamera))
      based on the nodes in the @link(TCastleRootTransform.MainScene).
      Nodes like TViewpointNode or TOrthoViewpointNode or TNavigationInfoNode
      determine the default camera and projection details.

      You can override this method, or assign the @link(OnProjection) event
      to adjust the projection settings.
      But please note: instead of overriding this method,
      it's usually easier (and more advised) to simply change the @link(Camera) properties,
      like @link(TCastleCamera.ProjectionType Camera.ProjectionType)
      or @link(TCastleOrthographic.Width Camera.Orthographic.Width)
      or @link(TCastlePerspective.FieldOfView Camera.Perspective.FieldOfView). }
    function CalculateProjection: TProjection; virtual;

    { Prepare lights shining on everything.
      GlobalLights contents should be initialized here.
      The implementation in this class adds headlight determined
      by the @link(TCastleRootTransform.UseHeadlight Items.UseHeadlight) value. }
    procedure InitializeGlobalLights(const GlobalLights: TLightInstancesList); virtual;

    procedure InitializeLights(const GlobalLights: TLightInstancesList); deprecated 'use InitializeGlobalLights';

    { Render everything from given camera view (as TRenderingCamera).
      Given RenderingCamera.Target says to where we generate the image.
      This method must take care of making many rendering passes
      for shadow volumes, but doesn't take care of updating generated textures. }
    procedure RenderFromViewEverything(const RenderingCamera: TRenderingCamera); virtual;

    { Render the scene, assuming that buffers were already cleared and background
      was rendered. Called by RenderFromViewEverything at the end.
      Lights are calculated in Params at this point.

      This will change Params.Transparent, Params.InShadow and Params.ShadowVolumesReceivers
      as needed. Their previous values do not matter. }
    procedure RenderFromView3D(const Params: TRenderParams); virtual;

    { Render one pass, with current camera and parameters (e.g. only transparent
      or only opaque shapes).
      All current camera settings are saved in RenderParams.RenderingCamera.
      @param(Params Rendering parameters, see @link(TRenderParams).) }
    procedure RenderOnePass(const Params: TRenderParams); virtual;

    procedure Render3D(const Params: TRenderParams); virtual; deprecated 'use RenderOnePass';

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function InternalExtraGetScreenEffects(
      const Index: Integer): TGLSLProgram; override;
    function InternalExtraScreenEffectsCount: Integer; override;
    function InternalExtraScreenEffectsNeedDepth: Boolean; override;

    { Called when PointingDevicePress was not handled by any TCastleTransform object.
      You can override this to make a message / sound signal to notify user
      that his Input_Interact click was not successful. }
    procedure PointingDevicePressFailed; virtual;

    procedure BoundNavigationInfoChanged; virtual;
    procedure BoundViewpointChanged; virtual;

    procedure RenderWithoutScreenEffects; override;

    procedure Loaded; override;
  public
    const
      DefaultScreenSpaceAmbientOcclusion = false;
      DefaultScreenSpaceReflections = False;
      DefaultScreenSpaceReflectionsSurfaceGlossiness = 0.5;
      DefaultUseGlobalLights = true;
      DefaultUseGlobalFog = true;
      DefaultShadowVolumes = true;
      DefaultBackgroundColor: TVector4 = (X: 0.1; Y: 0.1; Z: 0.1; W: 1);
      {$warnings off} // referencing deprecated in deprecated
      Default2DProjectionFar = CastleTransform.Default2DProjectionFar deprecated 'this default is not used; ProjectionFar in orthographic projection is now automatically adjusted to what you display';
      Default2DProjectionNear = CastleTransform.Default2DProjectionNear deprecated 'this default is not used; ProjectionNear in orthographic projection is now automatically adjusted to what you display';
      {$warnings on}
      Default2DCameraZ = CastleTransform.Default2DCameraZ;
      DefaultPrepareOptions = [prRenderSelf, prRenderClones, prBackground, prBoundingBox, prScreenEffects];
      { @exclude }
      DefaultInternalDesignNavigationType = dnFly;
      DefaultInternalGridAxis = false;

    var
      { Rendering pass, for user purposes.
        Useful to keep shaders cached when you render the same scene multiple times
        in the same frame (under different lighting conditions or other things
        that change shaders).
        By default this is always 0, the engine doesn't modify this.
        You can set this field manually. }
      CustomRenderingPass: TUserRenderingPass;

      { Set these to non-1 to deliberately distort field of view / aspect ratio.
        This is useful for special effects when you want to create unrealistic
        projection.
        @exclude }
      InternalDistortFieldOfViewY, InternalDistortViewAspect: Single;

      { Do not navigate by dragging when we're already dragging a TCastleTransform item.
        This means that if you drag

        - X3D sensors like TouchSensor,
        - gizmo in CGE editor,

        ... then your dragging will not simultaneously also affect the navigation
        (which would be very disorienting).

        Set to true when some TCastleTransform handles PointingDevicePress,
        set to false in PointingDeviceRelease.
        @exclude }
      InternalPointingDeviceDragging: Boolean;

      { At design-time (in CGE editor), this is the camera used by the editor.
        Usually use InternalCamera, to automatically pick either design-time camera
        or runtime Camera. }
      InternalDesignCamera: TCastleCamera;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AllowSuspendForInput: boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    procedure BeforeRender; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;

    { Return either @link(Camera) or @link(InternalDesignCamera),
      depending on whether we're in design mode (InternalDesignManipulation) or not.
      May return @nil.
      @exclude }
    function InternalCamera: TCastleCamera;

    { Determines if viewport is in design-mode.

      Initialized from CastleDesignMode, unless viewport is created using InternalCreateNonDesign
      in which case it is always false.
      (useful only in CGE editor, when it wants to use viewports
      with "normal" Camera and Navigation treatment, without
      InternalDesignCamera / InternalDesignNavigation).

      When false, the InternalDesignCamera and InternalDesignNavigation don't matter,
      viewport behaves as regular viewport.

      Note that you cannot change it after creation, it would not be so easy
      (we'd have to destroy InternalDesignCamera, InternalDesignNavigation,
      figure out what should be Items.MainCamera...).

      @exclude }
    property InternalDesignManipulation: Boolean read FInternalDesignManipulation;

    { Change design-time navigation.
      @exclude }
    property InternalDesignNavigationType: TInternalDesignNavigationType
      read FInternalDesignNavigationType write SetInternalDesignNavigationType
      default DefaultInternalDesignNavigationType;

    { At design-time (in CGE editor), this is the navigation used by the editor.
      @exclude }
    function InternalDesignNavigation: TCastleNavigation;

    { At design-time, show grid and axis visualization.
      @exclude }
    property InternalGridAxis: Boolean read FInternalGridAxis write SetInternalGridAxis
      default DefaultInternalGridAxis;

    { Constructor that disables special design-mode viewport camera/navigation.
      Useful in editor.
      @exclude }
    constructor InternalCreateNonDesign(AOwner: TComponent);

    function GetMainScene: TCastleScene; deprecated 'use Items.MainScene';

    {$ifdef FPC}
    { Current projection parameters,
      calculated by last @link(CalculateProjection) call,
      adjusted by @link(OnProjection).
      @bold(This is read only). To change the projection parameters,
      override @link(CalculateProjection) or handle event @link(OnProjection). }
    property Projection: TProjection read FProjection;
      deprecated 'in most cases, you can instead read Camera parameters, like Camera.Orthographic.EffectiveWidth, Camera.Orthographic.EffectiveHeight';
    {$endif}

    { Assign current camera vectors and projection.
      This only fills existing @link(Camera) with contents, it doesn't change
      the @link(Camera) to new component.

      This is automatically used at first rendering if @link(AutoCamera).
      You can also use it explicitly. }
    procedure AssignDefaultCamera; virtual;

    { Does the graphic card support our ScreenSpaceAmbientOcclusion shader.
      This does @italic(not) depend on the current state of
      ScreenSpaceAmbientOcclusion property.
      You can use it e.g. to disable the menu item to switch SSAO in 3D viewer. }
    function ScreenSpaceAmbientOcclusionAvailable: boolean;

    { Does the graphic card support our ScreenSpaceReflections shader.
      This does @italic(not) depend on the current state of
      ScreenSpaceReflections property.
      You can use it e.g. to disable the menu item to switch SSR in 3D viewer. }
    function ScreenSpaceReflectionsAvailable: boolean;

    procedure GLContextOpen; override;
    procedure GLContextClose; override;

    { Parameters to prepare items that are to be rendered
      within this world. This should be passed to
      @link(TCastleTransform.PrepareResources).

      Note: Instead of using @link(TCastleTransform.PrepareResources),
      and this method,
      it's usually easier to call @link(TCastleViewport.PrepareResources).
      Then the appropriate TPrepareParams will be passed automatically. }
    function PrepareParams: TPrepareParams; deprecated 'use TCastleViewport.PrepareResources to prepare transforms';

    function BaseLights: TLightInstancesList; deprecated 'this is internal info, you should not need this; use PrepareParams to get opaque information to pass to TCastleTransform.PrepareResources';

    { Statistics about last rendering frame. See TRenderStatistics docs. }
    function Statistics: TRenderStatistics;

    { Background color.
      Displayed only if @link(Background) is @nil and
      @link(TCastleRootTransform.MainScene MainScene) doesn't define any background either.

      Ignored if @link(Transparent).

      Dark gray (DefaultBackgroundColor) by default. }
    property BackgroundColor: TCastleColor
      read FBackgroundColor write FBackgroundColor;

    { Current 3D triangle under the mouse cursor.
      Updated in every mouse move. May be @nil. }
    function TriangleHit: PTriangle;

    { Instance for headlight that should be used for this scene.
      Uses @link(InternalHeadlight) method, applies appropriate camera position/direction.
      Returns @true only if @link(InternalHeadlight) method returned @true
      and a suitable camera was present.

      Instance should be considered undefined ("out" parameter)
      when we return @false.

      @exclude }
    function HeadlightInstance(out Instance: TLightInstance): boolean;
      deprecated 'internal information, do not use this';

    { Camera that makes a headlight (if only TCastleRootTransform.UseHeadlight).
      @exclude}
    function InternalHeadlightCamera: TCastleCamera; virtual;

    { Enable built-in SSAO screen effect in the world. }
    property ScreenSpaceAmbientOcclusion: boolean
      read FScreenSpaceAmbientOcclusion write SetScreenSpaceAmbientOcclusion
      default DefaultScreenSpaceAmbientOcclusion;

    { Enable built-in SSR screen effect in the world. }
    property ScreenSpaceReflections: Boolean
      read FScreenSpaceReflections write SetScreenSpaceReflections
      default DefaultScreenSpaceReflections;

    { Adjust SSR default surface glossiness. }
    property ScreenSpaceReflectionsSurfaceGlossiness: Single
      read FScreenSpaceReflectionsSurfaceGlossiness write SetScreenSpaceReflectionsSurfaceGlossiness;

    { Called on any camera change. }
    property OnCameraChanged: TNotifyEvent read FOnCameraChanged write FOnCameraChanged;

    { Utility method to set camera to a suitable state for 2D games.

      @unorderedList(
        @item(
          Sets both initial and current camera vectors like this:
          @unorderedList(
            @itemSpacing compact
            @item Position camera at @code((0, 0, 500)),
            @item Looks along the -Z direction,
            @item "Up" vector is in +Y.
          )

          This way the 2D world spans horizontally in X and vertically in Y.
          The Z (depth) can be used to put things in front/behind each other.

          Since this initialized the camera sensibly,
          we also set @link(AutoCamera) to false.
        )

        @item(
          Sets orthographic projection for the camera
          (@link(TCastleCamera.ProjectionType) set to ptOrthographic).

          By default our visible X range is @code([0..viewport width in pixels]),
          visible Y range is @code([0..viewport height in pixels]).
          Use the properties of @link(TCastleOrthographic Camera.Orthographic)
          to control the projection.
          For example set
          @link(TCastleOrthographic.Width Camera.Orthographic.Width) and/or
          @link(TCastleOrthographic.Height Camera.Orthographic.Height)
          to define visible projection size (horizontal or vertical) explicitly,
          regardless of the viewport size.

          Setting @link(TCastleOrthographic.Origin Camera.Orthographic.Origin)
          is also often useful, e.g. set it to (0.5,0.5) to make the things positioned
          at (0,0) in the world visible at the middle of the viewport.

          By default our visible Z range is [-1500, 500],
          because this sets ProjectionNear to -1000, ProjectionFar to 1000,
          and camera default depth (@code(Camera.Position.Z)) is 500.
          This was chosen to be comfortable for all cases -- you can
          keep camera Z unchanged and comfortably position things around [-500, 500],
          or set camera Z to zero and then comfortably position things around [-1000, 1000].
        )
      )
    }
    procedure Setup2D;

    { Convert 2D position on the viewport into 3D "world coordinates",
      by colliding camera ray with a plane parallel to the viewport at given Depth.
      "World coordinates" are coordinates
      space seen by TCastleTransform / TCastleScene inside viewport @link(Items).
      Use Depth > 0 for positions in front of the camera.

      This is similar to "Unproject" GLU routine.
      It allows to map points from the 2D viewport back to the 3D space inside viewport.

      The interpretation of Position depends on ContainerCoordinates:

      @unorderedList(
        @item(When ContainerCoordinates = @true,
          then Position is relative to the whole container
          (like TCastleWindow or TCastleControl).

          And it is expressed in real device coordinates,
          just like @link(TInputPressRelease.Position)
          when mouse is being clicked, or like @link(TInputMotion.Position)
          when mouse is moved.
        )

        @item(When ContainerCoordinates = @false,
          then Position is relative to this UI control.

          And it is expressed in coordinates after UI scaling.
          IOW, if the size of this control is @link(Width) = 100,
          then Position.X between 0 and 100 reflects the visible range of this control.
        )
      )

      Returns true and sets 3D PlanePosition if such intersection is found.
      Returns false if it's not possible to determine such point (which
      should not be possible, unless camera field of view is larger than 180 degrees).
    }
    function PositionToCameraPlane(const Position: TVector2;
      const ContainerCoordinates: Boolean;
      const Depth: Single; out PlanePosition: TVector3): Boolean;

    { Convert 2D position on the viewport into 3D ray.

      The interpretation of Position depends on ContainerCoordinates:

      @unorderedList(
        @item(When ContainerCoordinates = @true,
          then Position is relative to the whole container
          (like TCastleWindow or TCastleControl).

          And it is expressed in real device coordinates,
          just like @link(TInputPressRelease.Position)
          when mouse is being clicked, or like @link(TInputMotion.Position)
          when mouse is moved.
        )

        @item(When ContainerCoordinates = @false,
          then Position is relative to this UI control.

          And it is expressed in coordinates after UI scaling.
          IOW, if the size of this control is @link(Width) = 100,
          then Position.X between 0 and 100 reflects the visible range of this control.
        )
      )
    }
    procedure PositionToRay(const Position: TVector2;
      const ContainerCoordinates: Boolean; out RayOrigin, RayDirection: TVector3);

    { Convert 2D position on the viewport into 3D "world coordinates",
      by colliding camera ray with a plane at constant X, Y or Z.
      "World coordinates" are coordinates
      space seen by TCastleTransform / TCastleScene inside viewport @link(Items).

      This works with any projection (perspective or orthographic).

      You can use it to pick a point on any plane with constant X (PlaneConstCoord = 0),
      constant Y (PlaneConstCoord = 1), constant Z (PlaneConstCoord = 2).

      This intersects the ray cast by @link(Camera) with a plane where given coordinate (PlaneConstCoord)
      has value equal to PlaneConstValue.
      The parameter names and interpretation is consistent e.g. with
      @link(TrySimplePlaneRayIntersection).

      The interpretation of Position depends on ContainerCoordinates:

      @unorderedList(
        @item(When ContainerCoordinates = @true,
          then Position is relative to the whole container
          (like TCastleWindow or TCastleControl).

          And it is expressed in real device coordinates,
          just like @link(TInputPressRelease.Position)
          when mouse is being clicked, or like @link(TInputMotion.Position)
          when mouse is moved.
        )

        @item(When ContainerCoordinates = @false,
          then Position is relative to this UI control.

          And it is expressed in coordinates after UI scaling.
          IOW, if the size of this control is @link(Width) = 100,
          then Position.X between 0 and 100 reflects the visible range of this control.
        )
      )

      Returns true and sets 3D PlanePosition (the PlaneConstCoord component of this vector
      must always be equal to PlaneConstValue) if such intersection is found.
      Returns false if it's not possible to determine such point (when
      the camera looks in the other direction).
    }
    function PositionToWorldPlane(const Position: TVector2;
      const ContainerCoordinates: Boolean;
      const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
      out PlanePosition: TVector3): Boolean; overload;

    function PositionToWorldPlane(const Position: TVector2;
      const ContainerCoordinates: Boolean;
      const PlaneConstValue: Single;
      out PlanePosition: TVector3): Boolean; overload;
      deprecated 'use PositionToWorldPlane overload with PlaneConstCoord parameter; this version assumes PlaneConstCoord = 2';

    { Convert 2D position into "world coordinates", which is the coordinate
      space seen by TCastleTransform / TCastleScene inside viewport @link(Items),
      assuming that we use orthographic projection in XY axes.

      The interpretation of Position depends on ContainerCoordinates:

      @unorderedList(
        @item(When ContainerCoordinates = @true,
          then Position is relative to the whole container
          (like TCastleWindow or TCastleControl).

          And it is expressed in real device coordinates,
          just like @link(TInputPressRelease.Position)
          when mouse is being clicked, or like @link(TInputMotion.Position)
          when mouse is moved.
        )

        @item(When ContainerCoordinates = @false,
          then Position is relative to this UI control.

          And it is expressed in coordinates after UI scaling.
          IOW, if the size of this control is @link(Width) = 100,
          then Position.X between 0 and 100 reflects the visible range of this control.
        )
      )

      This assumes that camera "up vector" is +Y, and it is looking along the negative Z
      axis. It also assumes orthographic projection (@link(TCastleCamera.ProjectionType Camera.ProjectionType)
      equal @link(ptOrthographic)).
      These are default camera direction, up and projection types set
      by @link(Setup2D). }
    function PositionTo2DWorld(const Position: TVector2;
      const ContainerCoordinates: Boolean): TVector2;

    { Invert @link(PositionTo2DWorld), converting the "world coordinates" (the coordinate
      space seen by TCastleTransform / TCastleScene inside viewport @link(Items))
      into final screen position.

      Just like @link(PositionTo2DWorld),
      this assumes that camera "up vector" is +Y, and it is looking along the negative Z
      axis. It also assumes orthographic projection (@link(TCastleCamera.ProjectionType Camera.ProjectionType)
      equal @link(ptOrthographic)).
      These are default camera direction, up and projection types set
      by @link(Setup2D). }
    function PositionFrom2DWorld(const WorldPosition: TVector2;
      const ContainerCoordinates: Boolean): TVector2;

    { Project 3D coordinates (the coordinate
      space seen by TCastleTransform / TCastleScene inside viewport @link(Items))
      into 2D position on the viewport UI.

      The resulting position is relative to this viewport control
      and it is expressed in coordinates after UI scaling.
      IOW, if the size of this control is @link(EffectiveWidth) = 100,
      then Position.X between 0 and 100 reflects the visible range of this control.
      This is like using ContainerCoordinates = @false with PositionToRay. }
    function PositionFromWorld(const WorldPosition: TVector3): TVector2;

    procedure PrepareResources(const DisplayProgressTitle: string;
      const Options: TPrepareResourcesOptions = DefaultPrepareOptions); overload;
      deprecated 'use overload without DisplayProgressTitle: String';
    procedure PrepareResources(const Item: TCastleTransform;
      const DisplayProgressTitle: string;
      Options: TPrepareResourcesOptions = DefaultPrepareOptions); overload;
      deprecated 'use overload without DisplayProgressTitle: String';

    { Prepare resources, to make various methods (like @link(Render)) execute fast.
      Call it only when rendering context is initialized (ApplicationProperties.IsGLContextOpen). }
    procedure PrepareResources(
      const Options: TPrepareResourcesOptions = DefaultPrepareOptions); overload;
    procedure PrepareResources(const Item: TCastleTransform;
      Options: TPrepareResourcesOptions = DefaultPrepareOptions); overload; virtual;

    { Current object (TCastleTransform hierarchy) under the mouse cursor.
      Updated in every mouse move. May be @nil.

      The returned list (if not @nil) contains TCastleTransform instances
      that collided with the ray (from the deepest instance in the @link(Items) tree
      to the root), along with some additional information.
      See TRayCollision for details. }
    property MouseRayHit: TRayCollision read FMouseRayHit;

    { Current object (TCastleTransform instance) under the mouse cursor.

      This corresponds to the first @italic(not hidden) instance on the MouseRayHit list.
      This makes the behavior most intuitive: it returns the TCastleTransform
      instance you have explicitly created, like TCastleScene, TCastlePlane or TCastleImageTransform.
      It will not return hidden (with csTransient flag) scenes that are internal
      e.g. inside TCastlePlane or TCastleImageTransform.

      Updated in every mouse move. May be @nil. }
    function TransformUnderMouse: TCastleTransform;

    { Do not collide with this object when moving by @link(Navigation).
      It makes sense to put here player avatar (in 3rd person view)
      or player collision volume (in 1st person view)
      to allow player to move, not colliding with its own body. }
    property AvoidNavigationCollisions: TCastleTransform
      read FAvoidNavigationCollisions
      write SetAvoidNavigationCollisions;

    {$ifdef FPC}
    { See @link(TCastleAbstractRootTransform.Paused). }
    property Paused: boolean read GetPaused write SetPaused default false;
      deprecated 'use Items.Paused';

    property SceneManager: TCastleSceneManager read FSceneManager write SetSceneManager;
      deprecated 'assign Items from one TCastleViewport to another to view the same world from multiple viewports';
    {$endif}

    { Create new camera and make it used by this viewport.
      This creates new unnamed TCastleCamera, owned by this viewport,
      assigns it to @link(Camera) and adds it to @link(Items).

      Call this only when @link(Camera) is @nil.

      This is done automatically when creating TCastleViewport (not in design-mode,
      not at deserialization) using the standard TCastleViewport.Create constructor.
      So you likely don't need to call this in typical applications.

      Note: Even when used from CGE editor (CastleDesignMode), this creates non-design camera.
      This way viewports created with InternalCreateNonDesign have also non-design cameras.
      This is important, so that e.g. camera has no bounding box, and thus
      AssignDefaultCamera in sprite sheet editor doesn't put camera further and further away. }
    procedure SetupCamera;

    { Navigation method is an optional component that handles
      the user input to control the camera.

      @deprecated
      This is a deprecated property.
      There's no point in assigning current navigation like this.
      Instead add the @link(TCastleNavigation) instance
      as a child of TCastleViewport and it will affect the viewport
      automatically.
      This property is now only a shortcut to

      - get the current @link(TCastleNavigation) child (if multiple present,
        it is undefined which is returned)

      - make the given set @link(TCastleNavigation) as the only child.

      You can assign here an instance of @link(TCastleNavigation),
      like @link(TCastleWalkNavigation) or @link(TCastleExamineNavigation).
      Or you can leave it as @nil. }
    property Navigation: TCastleNavigation read GetNavigation write SetNavigation
      stored false;
      {$ifdef FPC}deprecated 'instead of this property: to get, just remember current navigation instance on your side; to set, add it like "MyViewport.InsertBack(MyNavigation)"';{$endif}

    { Check collisions (for move) with whole world (except AvoidNavigationCollisions).
      Given parameters are in world coordinates.
      @exclude }
    function InternalNavigationMoveAllowed(const Sender: TCastleNavigation;
      const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
      const Radius: Single; const BecauseOfGravity: Boolean): Boolean;

    { Check collisions (to query height) with whole world (except AvoidNavigationCollisions).
      Given parameters are in world coordinates.
      @exclude }
    function InternalNavigationHeight(const Sender: TCastleNavigation;
      const Position: TVector3;
      out AboveHeight: Single; out AboveGround: PTriangle): Boolean;
  published
    { Transformations and scenes visible in this viewport.
      You should add here your @link(TCastleTransform) and @link(TCastleScene)
      instances.

      It is by default created (not @nil), but you can also assign here your own
      TCastleRootTransform instance.
      You can also copy a TCastleRootTransform from one TCastleViewport to another,
      that is multiple TCastleViewport can refer to the same TCastleRootTransform
      instance.

      Note that assigning here @nil is allowed, but under the hood it just creates
      an empty TCastleRootTransform instance. So this property is never @nil when reading.
      But try to not depend on it for the future, at some point this property may be
      allowed to be @nil, for consistency. }
    property Items: TCastleRootTransform read FItems write SetItems;

    { Camera determines the viewer position and orientation.

      You should create TCastleCamera, assign to this property,
      and add it somewhere to @link(Items).

      Note that camera can be placed anywhere within @link(Items),
      not necessary as direct child of @link(Items).
      For example it can be a children of some deeper TCastleTransform,
      this way you can attach camera e.g. to some bone or to a moving object.

      Generally, TCastleCamera is a regular component that can be added and removed
      freely from @link(Items), renamed, freed etc.
      And setting this property is an independent action
      that doesn't add/remove any camera from @link(Items).
      This property can also be set to @nil.

      For convenience, there are however some moments when we set up this property and
      add camera to @link(Items) automatically:

      @orderedList(
        @item(
          When you create TCastleViewport from code (not by deserializing some
          @code(xxx.castle-user-interface) file, not in design-mode in editor)
          then we automatically add new (unnamed) camera and set
          Viewport.Camera and add it to Viewport.Items.)

        @item(
          Only at design-time (in editor): creating new viewport by editor menu
          also adds a default camera to it.
          In this case camera component is named and owned by the same owner as viewport.
          This is done right after TCastleViewport creation and only at design-time.)

        @item(
          For backward compatibility, after deserializing file from previous engine versions,
          we add the camera from it to Viewport.Items.
          This is made strictly for backward compatibility, only when deserializing,
          and it shouldn't occur when reading new design files.)
      )

      It's important to note that when deserializing camera from new engine versions, there's no "magic".
      We just expect that Viewport.Camera and Viewport.Items are good.
      Trying to do something automatic/smart in this case turned out to be quite troublesome
      and breaking more than helping. }
    property Camera: TCastleCamera read FCamera write SetCamera;

    { Should we render with shadow volumes.
      You can change this at any time, to switch rendering shadows on/off.

      This works only if OpenGL context actually can render shadow volumes,
      checked by GLFeatures.ShadowVolumesPossible, which means that you have
      to initialize OpenGL context with stencil buffer.

      The shadow volumes algorithm is used only if shadow caster
      is 2-manifold, that is has a correctly closed volume.
      Also you need a light source
      marked as the main shadow volumes light (shadowVolumes = shadowVolumesMain = TRUE).
      See [https://castle-engine.io/x3d_extensions.php#section_ext_shadows]
      for details. }
    property ShadowVolumes: boolean
      read FShadowVolumes write FShadowVolumes default DefaultShadowVolumes;

    { Actually draw the shadow volumes to the color buffer, for debugging.
      If shadows are rendered (see GLFeatures.ShadowVolumesPossible and ShadowVolumes),
      you can use this to actually see shadow volumes, for debug / demo
      purposes. Shadow volumes will be rendered on top of the scene,
      as yellow blended polygons. }
    property ShadowVolumesRender: boolean read FShadowVolumesRender write FShadowVolumesRender default false;

    { Background (like a skybox) to display behind the @link(Items).
      Displayed only when not @link(Transparent).
      See https://castle-engine.io/background }
    property Background: TCastleBackground read FBackground write SetBackground;

    { Fog to use to display viewport contents in @link(Items).
      See https://castle-engine.io/fog }
    property Fog: TCastleFog read FFog write SetFog;

    { If @true then the background (from @link(Background) or
      @link(TCastleRootTransform.MainScene MainScene)) will be rendered wireframe,
      over the solid background filled with BackgroundColor.
      Useful for debugging when you want to see how your background
      geometry looks like.

      Ignored if @link(Transparent). }
    property BackgroundWireframe: boolean
      read FBackgroundWireframe write FBackgroundWireframe default false;

    { If @true then the viewport will not draw a background and the UI underneath
      will be visible.
      The UI underneath is visible at pixels which are not covered by items on @link(Items),
      and where items @link(Items) have partially-transparent materials.

      If @true, the @link(Background), @link(BackgroundColor), @link(BackgroundWireframe),
      and any background defined inside
      @link(TCastleRootTransform.MainScene MainScene) will be ignored. }
    property Transparent: boolean read FTransparent write FTransparent default false;

    { At the beginning of rendering, viewport by default clears
      the depth buffer. This makes every viewport draw everything
      on top of the previous 2D and 3D stuff (including on top
      of previous viewport), like a layer.

      You can disable this, which allows to combine together the 3D objects rendered
      by various viewports (and by custom OpenGL rendering),
      such that the 3D positions determime what overlaps what.
      This only makes sense if you have a number of TCastleViewport instances,
      that share the same size and position on the screen,
      same projection and the same camera.

      It's your responsibility in such case to clear the depth buffer.
      E.g. place one viewport in the back that has ClearDepth = @true.
      Or place a TCastleUserInterface descendant in the back, that calls
      @code(TRenderContext.Clear RenderContext.Clear) in overridden
      @link(TCastleUserInterface.Render).

      Note: to disable clearning the color buffer, set @link(Transparent)
      to @false.

      Note: if you use shadow volumes, we will still clear the stencil buffer
      at the beginning of rendering.
    }
    property ClearDepth: boolean read FClearDepth write FClearDepth default true;

    { Let lights in MainScene shine on every other TCastleScene, not only
      MainScene. This is an easy way to lit your whole world with lights
      defined inside MainScene file. Be sure to set X3D lights global=TRUE. }
    property UseGlobalLights: boolean
      read FUseGlobalLights write FUseGlobalLights default DefaultUseGlobalLights;
      {$ifdef FPC} deprecated 'if you need to tweak this, then do not use MainScene; use regular TCastleScene and set CastGlobalLights as needed'; {$endif}

    { Let the fog defined in MainScene affect all objects, not only MainScene.
      This is consistent with @link(UseGlobalLights), that allows lights
      from MainScene to shine on all objects. }
    property UseGlobalFog: boolean
      read FUseGlobalFog write FUseGlobalFog default DefaultUseGlobalFog;
      {$ifdef FPC} deprecated 'configure fog by assigning to TCastleViewport.Fog component; leave deprecated TCastleViewport.MainScene nil'; {$endif}

    { Help user to activate pointing device sensors and pick items.
      Every time you press Input_Interact (by default
      just left mouse button), we look if current mouse/touch position hits an object (TCastleTransform)
      that actually does something on activation. The object may do various stuff
      inside TCastleTransform.PointingDevicePress, generally this causes various
      picking/interaction with the object (like pulling a level, opening a door),
      possibly dragging, possibly with the help of VRML/X3D pointing device
      and drag sensors.

      When this is @true, we try harder to hit some 3D object that handles
      PointingDevicePress. If there's nothing interesting under mouse/touch,
      we will retry a couple of other positions around the current mouse/touch.

      This should be usually used when you use TCastleMouseLookNavigation.MouseLook,
      or other navigation when mouse cursor is hidden.
      It allows user to only approximately look at interesting item and hit
      interaction button or key.
      Otherwise, activating a small object is difficult,
      as you don't see the cursor. }
    property ApproximateActivation: boolean
      read FApproximateActivation write FApproximateActivation default false;

    {$ifdef FPC}
    { Adjust the projection parameters. This event is called before every render.
      See the @link(CalculateProjection) for a description how to default
      projection parameters are calculated. }
    property OnProjection: TProjectionEvent read FOnProjection write FOnProjection;
      deprecated 'adjust projection by changing Camera.ProjectionType and other projection parameters inside Camera';
    {$endif}

    { Enable to drag a parent control, for example to drag a TCastleScrollView
      that contains this TCastleViewport, even when the scene inside contains
      clickable elements (using TouchSensor node).

      To do this, you need to turn on
      TCastleScrollView.EnableDragging, and set EnableParentDragging=@true
      here. In effect, viewport will cancel the click operation
      once you start dragging, which allows the parent to handle
      all the motion events for dragging. }
    property EnableParentDragging: boolean
      read FEnableParentDragging write FEnableParentDragging default false;

    { Assign initial camera properties
      (initial position, direction, up, TCastleCamera.ProjectionNear)
      by looking at the initial world (@link(Items)) when rendering the first frame.

      The @link(AssignDefaultCamera) is automatically called only if this property is @true.

      Also, only if this property is @true, we synchronize
      camera when X3D Viewpoint node changes, or a new X3D Viewpoint node is bound.

      By default it is @false, which means you control @link(Camera) properties on your own.
    }
    property AutoCamera: Boolean
      read FAutoCamera write SetAutoCamera default false;
      {$ifdef FPC} deprecated 'it is simpler to set camera at design-time explicitly, or use CameraViewpointForWholeScene to auto-adjust camera; if you want to animate the camera, attach TCastleCamera to a bone transformation exposed by Scene.ExposeTransforms'; {$endif}

    { Called when bound Viewpoint node changes.
      Called exactly when TCastleSceneCore.ViewpointStack.OnBoundChanged is called. }
    property OnBoundViewpointChanged: TNotifyEvent read FOnBoundViewpointChanged write FOnBoundViewpointChanged;

    { Called when bound NavigationInfo changes (to a different node,
      or just a field changes). }
    property OnBoundNavigationInfoChanged: TNotifyEvent read FOnBoundNavigationInfoChanged write FOnBoundNavigationInfoChanged;

    { Protect from falling down because of gravity when position is outside
      of world bounding box.
      This is a nice thing for general model viewers (like view3dscene),
      to prevent from accidentally falling down when using "Walk" mode.

      This is only used by navigations performing gravity internally,
      that is right now: @link(TCastleWalkNavigation) (when @link(TCastleWalkNavigation.Gravity) = @true). }
    property PreventInfiniteFallingDown: Boolean
      read FPreventInfiniteFallingDown write FPreventInfiniteFallingDown default false;

    { Combine (right before rendering) multiple shapes with a similar appearance into one.
      This can drastically reduce the number of "draw calls",
      making rendering much faster.

      To debug effectiveness of this, display @link(TCastleViewport.Statistics).
      In CGE editor (at design-time), use menu item "Edit -> Show Statistics" (F8).
      When dynamic batching works, the number of rendered shapes
      and the number of "Draw Calls" should be much smaller. }
    property DynamicBatching: Boolean
      read FDynamicBatching write SetDynamicBatching default false;

    { Use the occlusion culling to optimize the rendering.
      The shapes obscured by other shapes will not be rendered.
      This makes sense when in your view, many shapes are typically obscured by others.

      See the https://castle-engine.io/occlusion_culling
      for details how does this work.

      To debug effectiveness of this, display @link(TCastleViewport.Statistics).
      In CGE editor (at design-time), use menu item "Edit -> Show Statistics" (F8).
      If you look closely at a wall that obscures many shapes behind it,
      you should see the number of rendered shapes drop significantly
      using occlusion culling.

      This is ignored if GPU doesn't support the necessary functionality
      (@link(TGLFeatures.OcclusionQuery)). }
    property OcclusionCulling: boolean
      read FOcclusionCulling write SetOcclusionCulling default false;

    { Sort the opaque shapes when rendering, from front to back.
      This may make a speedup when big shapes in front of camera obscure
      many shapes behind.
      This is only an optimization: regardless of the value of this,
      rendering opaque objects will be always correct.
      Different values of this properly may merely increase / decrease
      rendering performance (frames per second).

      In general this is an independent optimization from @link(OcclusionCulling),
      albeit it makes sense in similar situations.

      When combined with @link(OcclusionCulling), it makes occlusion culling
      even more effective. Occlusion culling can reject more shapes,
      because the things that obscure them are rendered earlier to the depth buffer.

      See https://castle-engine.io/occlusion_culling#occlusion_sort for more details.

      The default value, sortAuto, for now is equivalent to sortNone.
      It may change in the future to perform sorting esp. when OcclusionCulling
      is @true, as OcclusionCulling and sorting are a natural pair,
      sorting make occlusion culling even more effective.
    }
    property OcclusionSort: TShapeSort
      read FOcclusionSort write FOcclusionSort default sortAuto;

    { Sort the blending (partially-transparent) shapes when rendering,
      from back to front.

      This makes blending correct when there are multiple partially-transparent
      objects visible.
      See @url(https://castle-engine.io/blending blending manual).
      Invalid value of this may cause rendering artifacts when rendering
      multiple partially-transparent objects.

      The default value, sortAuto, automatically detects if camera
      is 2D (orthographic, looking in -Z) and if yes, it behaves like sort2D.
      Otherwise it behaves like sort3D. }
    property BlendingSort: TShapeSort
      read FBlendingSort write FBlendingSort default sortAuto;

    { Used to sort shapes, if @link(TCastleViewport.BlendingSort) or
      @link(TCastleViewport.OcclusionSort) indicate sortCustom.
      See TShapeSortEvent for usage details and example. }
    property OnCustomShapeSort: TShapeSortEvent
      read FOnCustomShapeSort write FOnCustomShapeSort;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleviewport_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleViewportList = class({$ifdef FPC}specialize{$endif} TObjectList<TCastleViewport>)
  private
    SceneManager: TCastleSceneManager;
  protected
    procedure Notify({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} Value: TCastleViewport;
      Action: TCollectionNotification); override;
  end deprecated 'internal for TCastleSceneManager';

{$define read_interface}
{$I castleviewport_autonavigation.inc}
{$I castleviewport_scenemanager.inc}
{$I castleviewport_touchnavigation.inc}
{$I castleviewport_serialize.inc}
{$I castleviewport_design_navigation.inc}
{$undef read_interface}

var
  { Key/mouse combination to interact with clickable things in TCastleViewport.Items.
    More precisely, this input will activate pointing device sensors in X3D,
    which are used to touch (click) or drag things.
    By default this is left mouse button click.

    You can change it to any other mouse button or even to key combination.
    Simply change properties like TInputShortcut.Key1
    or TInputShortcut.MouseButtonUse. }
  Input_Interact: TInputShortcut;

implementation

uses DOM, Math, TypInfo,
  CastleGLUtils, CastleLog, CastleStringUtils,
  CastleSoundEngine, CastleGLVersion, CastleTextureImages,
  CastleInternalSettings, CastleXMLUtils, CastleURIUtils, CastleInternalRenderer,
  CastleRenderContext, CastleApplicationProperties, X3DLoad, CastleInternalGLUtils;

{$define read_implementation}
{$I castleviewport_autonavigation.inc}
{$I castleviewport_scenemanager.inc}
{$I castleviewport_touchnavigation.inc}
{$I castleviewport_warmup_cache.inc}
{$I castleviewport_serialize.inc}
{$I castleviewport_design_navigation.inc}
{$undef read_implementation}

{ TViewportRenderParams ------------------------------------------------------- }

constructor TCastleViewport.TViewportRenderParams.Create;
begin
  inherited;
  FGlobalLights := TLightInstancesList.Create;
end;

destructor TCastleViewport.TViewportRenderParams.Destroy;
begin
  FreeAndNil(FGlobalLights);
  inherited;
end;

function TCastleViewport.TViewportRenderParams.GlobalLights: TAbstractLightInstancesList;
begin
  Result := FGlobalLights;
end;

{ TSSAOScreenEffect ---------------------------------------------------------- }

function TCastleViewport.TSSAOScreenEffect.SetupUniforms(var BoundTextureUnits: Cardinal): Boolean;
begin
  Result := inherited;

  { set special uniforms for SSAO shader }

  { TODO: use actual projection near/far values, instead of hardcoded ones.
    Assignment below works, but it seems that effect is much less noticeable
    then?

    Note: Viewport.ProjectionFar may be ZFarInfinity, this will also have to be accounted for,
    maybe use ItemsBoundingBox.PointsDistance (max distance to camera) or MaxSize.

  WritelnLog('setting near to %f', [Viewport.ProjectionNear]); // testing
  WritelnLog('setting far to %f', [Viewport.ProjectionFar]); // testing
  Uniform('near').SetValue(Viewport.ProjectionNear);
  Uniform('far').SetValue(Viewport.ProjectionFar);
  }

  Uniform('near').SetValue(1.0);
  Uniform('far').SetValue(1000.0);
end;

{ TSSRScreenEffect ---------------------------------------------------------- }

function TCastleViewport.TSSRScreenEffect.SetupUniforms(var BoundTextureUnits: Cardinal): Boolean;
var
  ViewProjectionMatrix,
  ViewProjectionMatrixInverse: TMatrix4;
begin
  Result := inherited;

  { set special uniforms for SSR shader }

  { TODO: instead of relying on Viewport.InternalCamera,
    it should get and use RenderingCamera reference. }

  Uniform('near').SetValue(0.1);
  Uniform('far').SetValue(1000.0);
  ViewProjectionMatrix := Viewport.InternalCamera.ProjectionMatrix * Viewport.InternalCamera.Matrix;
  if not ViewProjectionMatrix.TryInverse(ViewProjectionMatrixInverse) then
    ViewProjectionMatrixInverse := TMatrix4.Identity;
  Uniform('defaultSurfaceGlossiness').SetValue(Viewport.ScreenSpaceReflectionsSurfaceGlossiness);
  Uniform('castle_ViewProjectionMatrix').SetValue(ViewProjectionMatrix);
  Uniform('castle_ViewProjectionMatrixInverse').SetValue(ViewProjectionMatrixInverse);
  Uniform('castle_CameraPosition').SetValue(Viewport.InternalCamera.WorldTranslation);
end;

{ TCastleViewport ------------------------------------------------------- }

procedure TCastleViewport.CommonCreate(const AOwner: TComponent; const ADesignManipulation: Boolean);
begin
  inherited Create(AOwner);

  FBackgroundColor := DefaultBackgroundColor;
  FUseGlobalLights := DefaultUseGlobalLights;
  FUseGlobalFog := DefaultUseGlobalFog;
  FRenderParams := TViewportRenderParams.Create;
  FPrepareParams := TPrepareParams.Create;
  FRenderWithoutScreenEffectsRenderingCamera := TRenderingCamera.Create;
  FShadowVolumes := DefaultShadowVolumes;
  FScreenSpaceReflectionsSurfaceGlossiness := DefaultScreenSpaceReflectionsSurfaceGlossiness;
  FClearDepth := true;
  InternalDistortFieldOfViewY := 1;
  InternalDistortViewAspect := 1;
  ShapesCollector := TShapesCollector.Create;
  ShapesRenderer := TShapesRenderer.Create;

  FItems := TCastleRootTransform.Create(Self);
  FItems.SetSubComponent(true);
  FItems.Name := 'Items';
  FItems.OnCursorChange := {$ifdef FPC}@{$endif} RecalculateCursor;

  FCapturePointingDeviceObserver := TFreeNotificationObserver.Create(Self);
  FCapturePointingDeviceObserver.OnFreeNotification := {$ifdef FPC}@{$endif} CapturePointingDeviceFreeNotification;

  FBackgroundObserver := TFreeNotificationObserver.Create(Self);
  FBackgroundObserver.OnFreeNotification := {$ifdef FPC}@{$endif} BackgroundFreeNotification;

  FFogObserver := TFreeNotificationObserver.Create(Self);
  FFogObserver.OnFreeNotification := {$ifdef FPC}@{$endif} FogFreeNotification;

  FCameraObserver := TFreeNotificationObserver.Create(Self);
  FCameraObserver.OnFreeNotification := {$ifdef FPC}@{$endif} CameraFreeNotification;

  FItemsObserver := TFreeNotificationObserver.Create(Self);
  FItemsObserver.OnFreeNotification := {$ifdef FPC}@{$endif} ItemsFreeNotification;

  FMissingCameraRect := TCastleRectangleControl.Create(Self);
  FMissingCameraRect.SetTransient;
  FMissingCameraRect.Exists := false;
  FMissingCameraRect.FullSize := true;
  InsertBack(FMissingCameraRect);

  FMissingCameraLabel := TCastleLabel.Create(Self);
  FMissingCameraLabel.Caption :=
    'No camera selected.' + NL +
    NL +
    'To see the viewport contents,' + NL +
    'set TCastleViewport.Camera.';
  FMissingCameraLabel.SetTransient;
  FMissingCameraLabel.Alignment := hpMiddle;
  FMissingCameraLabel.FontSize := 20;
  FMissingCameraLabel.Anchor(hpMiddle);
  FMissingCameraLabel.Anchor(vpMiddle);
  FMissingCameraRect.InsertFront(FMissingCameraLabel);

  FInternalDesignManipulation := ADesignManipulation;

  { only when not deserializing, and not in editor: create automatic Camera
    - unnamed, to not collide with name of sthg else
    - owned by Self (viewport), to not assume anything about AOwner }
  if (InternalLoadingComponent = 0) and (not InternalDesignManipulation) then
    SetupCamera;

  if InternalDesignManipulation then
  begin
    { We need to use TCastleCamera.InternalCreateNonDesign,
      otherwise InternalDesignCamera would be visible in preview of other camera. }
    InternalDesignCamera := TCastleCamera.InternalCreateNonDesign(Self);
    InternalDesignCamera.SetTransient;
    // this somewhat replicates what happens at SetCamera
    InternalDesignCamera.InternalOnCameraChanged := {$ifdef FPC}@{$endif} InternalCameraChanged;
    Items.Add(InternalDesignCamera);

    { when InternalDesignManipulation,
      then Items.MainCamera just always follows InternalDesignCamera,
      ignoring Camera. This way
      - gizmos adjust to design-time camera, so they have proper size
      - billboards adjust to design-time camera, so e.g. light gizmos look OK. }
    Items.MainCamera := InternalDesignCamera;

    FInternalDesignNavigationType := DefaultInternalDesignNavigationType;
    FInternalGridAxis := DefaultInternalGridAxis;

    FInternalDesignNavigations[dnFly] := TCastleWalkNavigationDesign.Create(Self);
    FInternalDesignNavigations[dnFly].SetTransient;
    FInternalDesignNavigations[dnFly].Exists := FInternalDesignNavigationType = dnFly;
    InsertControl(0, FInternalDesignNavigations[dnFly]);

    FInternalDesignNavigations[dnExamine] := TCastleExamineNavigationDesign.Create(Self);
    FInternalDesignNavigations[dnExamine].SetTransient;
    FInternalDesignNavigations[dnExamine].Exists := FInternalDesignNavigationType = dnExamine;
    InsertControl(0, FInternalDesignNavigations[dnExamine]);

    FInternalDesignNavigations[dn2D] := TCastle2DNavigationDesign.Create(Self);
    FInternalDesignNavigations[dn2D].SetTransient;
    FInternalDesignNavigations[dn2D].Exists := FInternalDesignNavigationType = dn2D;
    InsertControl(0, FInternalDesignNavigations[dn2D]);

    FGizmoGridAxis := TInternalCastleEditorGizmo.Create(Self);
    if InternalCastleDesignData <> '' then
      FGizmoGridAxis.LoadVisualization(LoadNode(InternalCastleDesignData + 'gizmos/grid_axis/grid_axis.gltf'));
    FGizmoGridAxis.Exists := InternalGridAxis;
    { Note: This will not work (Gizmo state and our property InternalGridAxis
      will become desynchronized) if Items are shared across other viewports.
      But it doesn't matter for CGE editor now, because you cannot share
      Items in this case. }
    Items.Add(FGizmoGridAxis);
  end;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleviewport_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

constructor TCastleViewport.Create(AOwner: TComponent);
begin
  CommonCreate(AOwner, CastleDesignMode);
end;

constructor TCastleViewport.InternalCreateNonDesign(AOwner: TComponent);
begin
  CommonCreate(AOwner, false);
end;

function TCastleViewport.InternalCamera: TCastleCamera;
begin
  if InternalDesignManipulation then
    Result := InternalDesignCamera
  else
    Result  := Camera;
end;

procedure TCastleViewport.SetInternalDesignNavigationType(const Value: TInternalDesignNavigationType);
begin
  if FInternalDesignNavigationType <> Value then
  begin
    if InternalDesignManipulation then
      FInternalDesignNavigations[FInternalDesignNavigationType].Exists := false;
    FInternalDesignNavigationType := Value;
    if InternalDesignManipulation then
      FInternalDesignNavigations[FInternalDesignNavigationType].Exists := true;
  end;
end;

procedure TCastleViewport.SetInternalGridAxis(const Value: Boolean);
begin
  if FInternalGridAxis <> Value then
  begin
    FInternalGridAxis := Value;
    if FGizmoGridAxis <> nil then
      FGizmoGridAxis.Exists := Value;
  end;
end;

function TCastleViewport.InternalDesignNavigation: TCastleNavigation;
begin
  Result := FInternalDesignNavigations[FInternalDesignNavigationType];
end;

procedure TCastleViewport.SetupCamera;
var
  NewCamera: TCastleCamera;
begin
  { This could actually work fine also when previous Camera <> nil.
    But for now, leave it not allowed -- we need to decide whether,
    in case of Camera <> nil, it would:
    - do nothing
    - or replace existing camera. }
  Assert(Camera = nil);

  NewCamera := TCastleCamera.InternalCreateNonDesign(Self);
  Camera := NewCamera;
  Items.Add(NewCamera);
end;

procedure TCastleViewport.SetupDesignTimeCamera;
var
  NewCamera: TCastleCamera;
begin
  { Do this only when adding viewport in editor.
    Otherwise, we'd always create some useless camera,
    each time when deserializing the viewport.

    This makes the camera a regular user-controlled component,
    that user can freely
    - remove (and eventually free),
    - rename,
    - see in editor (e.g. in dropdown Viewport.Camera),
    - access at runtime using TCastleView.DesignedComponent }

  Assert(Owner <> nil); // Use SetupDesignTimeCamera only on viewports with owner

  NewCamera := TCastleCamera.Create(Owner);
  NewCamera.Name := ProposeComponentName(TCastleCamera, Owner);
  Camera := NewCamera;
  Assert(Camera = NewCamera);

  { FItems.MainCamera is left *unsynchronized* with NewCamera,
    because in InternalDesignManipulation the design-time camera is set as MainCamera. }
  if InternalDesignManipulation then
    Assert(FItems.MainCamera <> Camera);

  // SetCamera doesn't add to World automatically
  Assert(Camera.World = nil);
  Assert(Camera.Parent = nil);
  Items.Add(NewCamera);
  Assert(Camera.World = Items);
  Assert(Camera.Parent = Items);
end;

destructor TCastleViewport.Destroy;
begin
  {$ifdef FPC}
  {$warnings off} // only to keep deprecated feature working
  SceneManager := nil; { remove Self from SceneManager.Viewports }
  {$warnings on}
  {$endif}

  { unregister free notification from these objects }
  SetMouseRayHit(nil);
  AvoidNavigationCollisions := nil;

  FreeAndNil(FRenderParams);
  FreeAndNil(FPrepareParams);
  FreeAndNil(FRenderWithoutScreenEffectsRenderingCamera);
  FreeAndNil(ShapesCollector);
  FreeAndNil(ShapesRenderer);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleviewport_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleViewport.SetCamera(const Value: TCastleCamera);
var
  WasMainCamera: Boolean;
begin
  if FCamera <> Value then
  begin
    { Note that this is also true when both FCamera and Items.MainCamera are nil,
      useful at initialization of TCastleViewport. }
    WasMainCamera := (Items <> nil) and (FCamera = Items.MainCamera);

    if FCamera <> nil then
    begin
      Check(TMethod(FCamera.InternalOnCameraChanged).Data = TMethod(FCamera.InternalOnSceneBoundViewpointChanged).Data, 'Inconsistent values of internal TCastleCamera callbacks; do not modify TCastleCamera.InternalXxx callbacks manually.');
      Check(TMethod(FCamera.InternalOnCameraChanged).Data = TMethod(FCamera.InternalOnSceneBoundViewpointVectorsChanged).Data, 'Inconsistent values of internal TCastleCamera callbacks; do not modify TCastleCamera.InternalXxx callbacks manually.');
      Check(TMethod(FCamera.InternalOnCameraChanged).Data = TMethod(FCamera.InternalOnSceneBoundNavigationInfoChanged).Data, 'Inconsistent values of internal TCastleCamera callbacks; do not modify TCastleCamera.InternalXxx callbacks manually.');

      { Note that we don't remove/add Camera to Viewport.Items.
        This would conflict with the deserialization, when camera is *already* added explicitly
        to Viewport.Items. }

      FCamera.InternalOnCameraChanged := nil;
      FCamera.InternalOnSceneBoundViewpointChanged := nil;
      FCamera.InternalOnSceneBoundViewpointVectorsChanged := nil;
      FCamera.InternalOnSceneBoundNavigationInfoChanged := nil;
    end;

    FCamera := Value;
    FCameraObserver.Observed := Value;

    if FCamera <> nil then
    begin
      FCamera.InternalOnCameraChanged := {$ifdef FPC}@{$endif} InternalCameraChanged;
      FCamera.InternalOnSceneBoundViewpointChanged := {$ifdef FPC}@{$endif} MainSceneAndCamera_BoundViewpointChanged;
      FCamera.InternalOnSceneBoundViewpointVectorsChanged := {$ifdef FPC}@{$endif} MainSceneAndCamera_BoundViewpointVectorsChanged;
      FCamera.InternalOnSceneBoundNavigationInfoChanged := {$ifdef FPC}@{$endif} MainSceneAndCamera_BoundNavigationInfoChanged;
    end;

    if WasMainCamera then
      Items.MainCamera := FCamera;
  end;
end;

procedure TCastleViewport.CameraFreeNotification(const Sender: TFreeNotificationObserver);
begin
  Camera := nil;
end;

procedure TCastleViewport.Loaded;

{
  function ComponentDebugStr(const C: TComponent): String;
  begin
    if C = nil then
      Result := 'nil'
    else
      Result := C.Name + ' (' + C.ClassName + ')';
  end;
}

var
  InitialPos, InitialDir, InitialUp: TVector3;
begin
  inherited;

  { To read old designs (before Camera was assignable, before Camera was a TCastleTransform):
    - add Camera to Items (so that it doesn't disappear when saving the design)
    - invent name for Camera (default 'Camera' could conflict e.g. in case of multiple viewports)
  }
  if (Camera <> nil) and
     (Camera.World = nil) and
     { We used to make exception when "Camera.Owner <> Owner", like this:

         if Camera.Owner <> Owner then
         begin
           raise EInternalError.CreateFmt('After loading design, Camera and Viewport must have equal owner. But Camera = %s, Viewport = %s, Camera.Owner = %s, Viewport.Owner = %s', [
             ComponentDebugStr(Camera),
             ComponentDebugStr(Self),
             ComponentDebugStr(Camera.Owner),
             ComponentDebugStr(Owner)
           ]);
         end;

       But now, this situation is valid in new designs (after we made TCastleCamera
       a normal component, assignable to Viewport.Items, settable in Viewport.Camera).
       Viewport.Camera may be just not resolved yet, if it is in another Viewport.Items
       (see examples/viewport_and_scenes/multiple_viewports/ for valid example).

       Ignore this case -- this is not an "old design", nothing to fix. }
     (Camera.Owner = Owner) then
  begin
    Items.Add(Camera);
    WritelnLog('Camera in viewport "%s" was not part of Viewport.Items, adding it to Viewport.Items', [
      Name
    ]);

    if InternalDesignManipulation then
    begin
      { If Camera was orthographic,
        then design-time camera should also be orthographic.
        This makes better experience when opening old designs. }
      InternalDesignCamera.ProjectionType := Camera.ProjectionType;

      { Copy all camera projection (field of view) parameters, just like
        TDesignFrame.CameraSynchronize does.
        This makes the design-time view behave just as run-time,
        when opening old designs, which is best for backward compatibility.
      }
      InternalDesignCamera.Perspective.FieldOfView     := Camera.Perspective.FieldOfView;
      InternalDesignCamera.Perspective.FieldOfViewAxis := Camera.Perspective.FieldOfViewAxis;
      InternalDesignCamera.Orthographic.Origin  := Camera.Orthographic.Origin;
      InternalDesignCamera.Orthographic.Width   := Camera.Orthographic.Width;
      InternalDesignCamera.Orthographic.Height  := Camera.Orthographic.Height;

      { Assign useful InternalDesignCamera vectors, because in case of reading old designs --
        TCastleViewport.CustomSerialization could not read any useful InternalDesignCamera
        from design file.

        Note known limitation: This will not adjust InternalCamera to final Camera view
        if AutoCamera is used. AutoCamera is applied later.
        This will only adjust InternalCamera to Camera view that is serialized. }
      Camera.GetWorldView(InitialPos, InitialDir, InitialUp);
      if Camera.ProjectionType = ptOrthographic then
      begin
        InternalDesignCamera.SetWorldView(
          { We move Z back, to be able to see from design-time camera the runtime camera gizmo.
            Note that 2D Camera.EffectiveProjectionNear is negative by default (after Setup2D),
            so "- Camera.EffectiveProjectionNear" actually does "+ 1000". }
          InitialPos + Vector3(0, 0, - Camera.EffectiveProjectionNear + 100),
          InitialDir,
          InitialUp);

        // best navigation for 2D
        InternalDesignNavigationType := dn2D;
      end else
      begin
        InternalDesignCamera.SetWorldView(InitialPos, InitialDir, InitialUp);

        // best navigation for 3D
        InternalDesignNavigationType := dnFly;
      end;
    end;

    if InternalDesignManipulation and (Camera.Name = 'Camera') and (Owner <> nil) then
    begin
      Camera.Name := ProposeComponentName(TCastleCamera, Owner);
      WritelnLog('Camera in viewport "%s" renamed to "%s" to not conflict with other components', [
        Name,
        Camera.Name
      ]);
    end;
  end;

  if InternalDesignManipulation then
  begin
    { Make sure InternalDesignCamera, InternalDesignNavigation are on children.
      As TCastleUserInterface.SerializeChildrenClear, TCastleTransform.SerializeChildrenClear
      do not touch csTransient children, they should be OK always. }
    Assert(Items.List.IndexOf(InternalDesignCamera) <> -1);
    Assert(IndexOfControl(InternalDesignNavigation) <> -1);
  end;

  {$warnings off} // using deprecated to warn about it
  if AutoCamera then
    WritelnWarning('AutoCamera is deprecated (on TCastleViewport named "%s"). Instead: It is simpler to set camera at design-time explicitly, or use CameraViewpointForWholeScene from code to auto-adjust camera.' + ' If you want to animate the camera, attach TCastleCamera to a bone transformation exposed by Scene.ExposeTransforms', [
      Name
    ]);
  if UseGlobalFog <> DefaultUseGlobalFog then
    WritelnWarning('UseGlobalFog is deprecated (on TCastleViewport named "%s"). Instead: Assign TCastleViewport.Fog to use fog, and leave deprecated TCastleViewport.MainScene = nil', [
      Name
    ]);
  if UseGlobalLights <> DefaultUseGlobalLights then
    WritelnWarning('UseGlobalLights is deprecated (on TCastleViewport named "%s"). Instead: If you need to tweak lighting, then use regular TCastleScene and set CastGlobalLights as needed; leave deprecated TCastleViewport.MainScene = nil', [
      Name
    ]);
  {$warnings on}
end;

procedure TCastleViewport.SetCapturePointingDevice(const Value: TCastleTransform);
begin
  if FCapturePointingDevice <> Value then
  begin
    FCapturePointingDevice := Value;
    FCapturePointingDeviceObserver.Observed := Value;
  end;
end;

procedure TCastleViewport.CapturePointingDeviceFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  CapturePointingDevice := nil;
end;

procedure TCastleViewport.SetBackground(const Value: TCastleBackground);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    FBackgroundObserver.Observed := Value;
  end;
end;

procedure TCastleViewport.BackgroundFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Background := nil;
end;

procedure TCastleViewport.SetFog(const Value: TCastleFog);
begin
  if FFog <> Value then
  begin
    FFog := Value;
    FFogObserver.Observed := Value;
  end;
end;

procedure TCastleViewport.FogFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Fog := nil;
end;

function TCastleViewport.FillsWholeContainer: boolean;
begin
  if Container = nil then
    Result := FullSize
  else
    Result := RenderRect.Round.Equals(Container.Rect);
end;

function TCastleViewport.GetNavigation: TCastleNavigation;
var
  I: Integer;
begin
  for I := ControlsCount - 1 downto 0 do
    if Controls[I] is TCastleNavigation then
      Exit(TCastleNavigation(Controls[I]));
  Result := nil;
end;

procedure TCastleViewport.SetNavigation(const Value: TCastleNavigation);
var
  I: Integer;
begin
  for I := ControlsCount - 1 downto 0 do
    if Controls[I] is TCastleNavigation then
      RemoveControl(Controls[I]);
  if Value <> nil then
    InsertBack(Value);
end;

procedure TCastleViewport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if (AComponent is TCastleTransform) and
       MouseRayHitContains(TCastleTransform(AComponent)) then
    begin
      { MouseRayHit cannot be used in subsequent RecalculateCursor. }
      SetMouseRayHit(nil);
    end;

    if AComponent = FAvoidNavigationCollisions then
      AvoidNavigationCollisions := nil;
  end;
end;

function TCastleViewport.Press(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result or Items.Paused then Exit;

  { Make MouseRayHit valid, as our PointingDevicePress uses it.
    Also this makes MouseRayHit valid during TCastleTransform.PointingDevicePress calls.
    Although implementors should rather use information passed
    as TCastleTransform.PointingDevicePress argument, not look at Viewport.MouseRayHit. }
  UpdateMouseRayHit;

  LastPressEvent := Event;

  if Items.InternalPressReleaseListeners <> nil then
    // use downto, to work in case some Press will remove transform from list
    for I := Items.InternalPressReleaseListeners.Count - 1 downto 0 do
      if Items.InternalPressReleaseListeners[I].Press(Event) then
        Exit(true);

  if Input_Interact.IsEvent(Event) and
     PointingDevicePress then
  begin
    InternalPointingDeviceDragging := true;
    Exit(true);
  end;
end;

function TCastleViewport.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result or Items.Paused then Exit;

  { Make MouseRayHit valid, as our PointingDeviceRelease uses it. }
  UpdateMouseRayHit;

  if Items.InternalPressReleaseListeners <> nil then
    // use downto, to work in case some Release will remove transform from list
    for I := Items.InternalPressReleaseListeners.Count - 1 downto 0 do
      if Items.InternalPressReleaseListeners[I].Release(Event) then
        Exit(true);

  if Input_Interact.IsEvent(Event) then
  begin
    InternalPointingDeviceDragging := false;
    if PointingDeviceRelease then
      Exit(true);
  end;
end;

function TCastleViewport.Motion(const Event: TInputMotion): boolean;

  { Call Transform.PointingDeviceRelease with CancelAction = true. }
  procedure PointingDeviceCancel(const Transform: TCastleTransform);
  var
    RayOrigin, RayDirection: TVector3;
    MousePosition: TVector2;
  begin
    if GetMousePosition(MousePosition) then
    begin
      PositionToRay(MousePosition, true, RayOrigin, RayDirection);
      Transform.PointingDeviceRelease(FakeRayCollisionNode(RayOrigin, RayDirection, Transform), MaxSingle, true { CancelAction });
    end;
  end;

  function IsTouchSensorActiveInScene(const Scene: TCastleTransform): boolean;
  var
    ActiveSensorsList: TX3DNodeList;
    I: Integer;
  begin
    Result := false;
    if not (Scene is TCastleSceneCore) then
      Exit;
    ActiveSensorsList := (Scene as TCastleSceneCore).PointingDeviceActiveSensors;
    for I := 0 to ActiveSensorsList.Count -1 do
    begin
      if ActiveSensorsList.Items[I] is TTouchSensorNode then
        Exit(true);
    end;
  end;

const
  DistanceToHijackDragging = 5 * 96;
var
  TopMostTransform: TCastleTransform;
begin
  Result := inherited;
  if (not Result) and (not Items.Paused) then
  begin
    {$warnings off} // TODO: using deprecated Navigation for now
    if Navigation <> nil then
    begin
      TopMostTransform := TransformUnderMouse;

      { Test if dragging TTouchSensorNode. In that case cancel its dragging
        and let navigation move instead. }
      if (TopMostTransform <> nil) and
         IsTouchSensorActiveInScene(TopMostTransform) and
         (PointsDistance(LastPressEvent.Position, Event.Position) >
          DistanceToHijackDragging / Container.Dpi) then
      begin
        PointingDeviceCancel(TopMostTransform);

        if EnableParentDragging then
        begin
          { Without ReleaseCapture, the parent (like TCastleScrollView) would still
            not receive the following motion events. }
          Container.ReleaseCapture(Navigation);
        end;

        Navigation.Press(LastPressEvent);
      end;
    end;
    {$warnings on}

    UpdateMouseRayHit;

    { Note: we ignore PointingDeviceMove result.
      Maybe we should use PointingDeviceMove result as our Motion result?
      Answer unknown. Historically we do not do this, and I found no practical
      use-case when it would be useful to do this. }
    PointingDeviceMove;
  end;

  { update the cursor, since TCastleTransform object under the cursor possibly changed.

    Accidentally, this also workarounds the problem of TCastleViewport:

    When the TCastleTransform object has its Cursor value changed,
    Items.OnCursorChange notify only 1st TCastleViewport that owned the Items.
    The other viewports are not notified, as SetItems doesn't set
    FItems.OnCursorChange (as we only have 1 OnCursorChange for now).

    But thanks to doing RecalculateCursor below, this isn't
    a problem, as we'll update cursor to follow TCastleTransform anyway,
    as long as it changes only during mouse move. }
  RecalculateCursor(Self);
end;

procedure TCastleViewport.UpdateMouseRayHit;
var
  MousePosition: TVector2;
begin
  if GetMousePosition(MousePosition) and
     // PositionToRay assumes InternalCamera <> nil
     (InternalCamera <> nil) and
     // do not update MouseRayHit if camera doesn't exist
     InternalCamera.ExistsInRoot then
  begin
    PositionToRay(MousePosition, true, MouseRayOrigin, MouseRayDirection);

    { Update MouseRayHit.
      We know that MouseRayDirection is normalized now, which is important
      to get correct MouseRayHit.Distance. }
    SetMouseRayHit(CameraRayCollision(MouseRayOrigin, MouseRayDirection));
  end;
end;

function TCastleViewport.TransformUnderMouse: TCastleTransform;
var
  I: Integer;
begin
  if MouseRayHit <> nil then
    for I := 0 to MouseRayHit.Count - 1 do
    begin
      Result := MouseRayHit[I].Item;
      if not (csTransient in Result.ComponentStyle) then
        Exit;
    end;

  // Return nil if all items on MouseRayHit list are csTransient, or MouseRayHit = nil
  Result := nil;
end;

procedure TCastleViewport.RecalculateCursor(Sender: TObject);
var
  T: TCastleTransform;
begin
  if { Be prepared for Items=nil case, as it may happen e.g. at destruction. }
     (Items = nil) or
     { This may be called from
       TCastleTransformList.Notify when removing stuff owned by other
       stuff, in particular during our own destructor when FItems is freed
       and we're in half-destructed state. }
     (csDestroying in Items.ComponentState) or
     { When Paused, then Press and Motion events are not passed to Navigation,
       or to Items inside. So it's sensible that they also don't control the cursor
       anymore.
       In particular, it means cursor is no longer hidden by Navigation.MouseLook
       when the Paused is switched to true. }
     Items.Paused then
  begin
    Cursor := mcDefault;
    Exit;
  end;

  { We show mouse cursor from top-most TCastleTransform.
    This is sensible, if multiple TCastleTransform scenes obscure each other at the same
    pixel --- the one "on the top" (visible by the player at that pixel)
    determines the mouse cursor.

    We ignore Cursor value of other TCastleTransform along
    the MouseRayHit list. Maybe we should browse Cursor values along the way,
    and choose the first non-none? }

  T := TransformUnderMouse;
  if T <> nil then
    Cursor := T.Cursor
  else
    Cursor := mcDefault;
end;

function TCastleViewport.TriangleHit: PTriangle;
begin
  if (MouseRayHit <> nil) and
     (MouseRayHit.Count <> 0) then
    Result := MouseRayHit.First.Triangle
  else
    Result := nil;
end;

procedure TCastleViewport.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  SecondsPassedScaled: Single;

  procedure ItemsUpdate;
  var
    RemoveItem: TRemoveType;
  begin
    if Items.Exists then
    begin
      RemoveItem := rtNone;

      { Note that Items.Update do not take HandleInput
        parameter, as it would not be controllable for them: TCastleTransform objects do not
        have strict front-to-back order, so we would not know in what order
        call their Update methods, so we have to let many Items handle keys anyway.
        So, it's consistent to just treat TCastleTransform objects as "cannot definitely
        mark keys/mouse as handled". }

      Items.Update(SecondsPassedScaled, RemoveItem);
      { we ignore RemoveItem --- main Items list cannot be removed }
    end;
  end;

  procedure UpdateVisibleChange;
  begin
    { when some TCastleTransform calls TCastleTransform.VisibleChangeHere,
      in effect TCastleViewport will call VisibleChange. }
    if LastVisibleStateIdForVisibleChange < Items.InternalVisibleStateId then
    begin
      LastVisibleStateIdForVisibleChange := Items.InternalVisibleStateId;
      VisibleChange([chRender]);
    end;
  end;

  procedure WatchMainSceneChange;
  begin
    if InternalDesignManipulation then
    begin
      {$warnings off} // using deprecated to keep it working
      if FLastSeenMainScene <> Items.MainScene then
      begin
        FLastSeenMainScene := Items.MainScene;
        AssignDefaultCameraDone := false;
      end;
      {$warnings on}
    end;
  end;

  { Prepare information used by TCastleScene.UpdateGeneratedTextures, called from TCastleScene.Update }
  procedure PrepareUpdateGeneratedTexturesParameters;
  begin
    if (FProjection.ProjectionNear = 0) or
       (FProjection.ProjectionFar = 0) then // in case ApplyProjection was not called yet
      FProjection := CalculateProjection;
    Items.InternalRenderEverythingEvent := {$ifdef FPC}@{$endif}RenderFromViewEverything;
    Items.InternalProjectionNear := FProjection.ProjectionNear;
    Items.InternalProjectionFar := FProjection.ProjectionFar;
  end;

begin
  inherited;

  if Items.Paused then
    Exit;

  SecondsPassedScaled := SecondsPassed * Items.TimeScale;

  PrepareUpdateGeneratedTexturesParameters;
  ItemsUpdate;
  UpdateVisibleChange;
  WatchMainSceneChange;
end;

function TCastleViewport.AllowSuspendForInput: boolean;
begin
  Result := Items.Paused;
end;

procedure TCastleViewport.EnsureCameraDetected;
begin
  if Camera <> nil then
  begin
    {$warnings off} // using deprecated to warn about it
    if AutoCamera and not AssignDefaultCameraDone then
      AssignDefaultCamera;
    {$warnings on}
    { Set AssignDefaultCameraDone to done,
      regardless if AssignDefaultCameraDone was done or not.
      Otherwise later setting AutoCamera to true would suddenly
      reinitialize camera (initial and current vectors) in the middle of game. }
    AssignDefaultCameraDone := true;
  end;
end;

procedure TCastleViewport.ApplyProjection;
var
  Viewport: TRectangle;
  AspectRatio: Single;
  M: TMatrix4;
begin
  EnsureCameraDetected;

  { We need to know container size now. }
  Check(ContainerSizeKnown, ClassName + ' did not receive "Resize" event yet, cannnot apply projection. This usually means you try to call "Render" method with a container that does not yet have an open context.');

  Viewport := RenderRect.Round;
  RenderContext.Viewport := Viewport;

  FProjection := CalculateProjection;
  {$ifdef FPC}
  {$warnings off} // using deprecated to keep it working
  if Assigned(OnProjection) then
    OnProjection(FProjection);
  {$warnings on}
  {$endif}

  { take into account InternalDistort* properties }
  AspectRatio := InternalDistortViewAspect * Viewport.Width / Viewport.Height;
  FProjection.PerspectiveAnglesRad.Y := InternalDistortFieldOfViewY * FProjection.PerspectiveAnglesRad.Y;

  { Apply new FProjection values }
  M := FProjection.Matrix(AspectRatio);
  if InternalCamera <> nil then // as InternalCamera is assignable, tolerate InternalCamera = nil
    InternalCamera.ProjectionMatrix := M;
  RenderContext.ProjectionMatrix := M;
end;

function TCastleViewport.ItemsBoundingBox: TBox3D;
begin
  if Items <> nil then
  begin
    Inc(TInternalCastleEditorGizmo.EmptyBoundingBox);
    try
      Result := Items.BoundingBox;
    finally Dec(TInternalCastleEditorGizmo.EmptyBoundingBox) end;
  end else
    Result := TBox3D.Empty;
end;

function TCastleViewport.ItemsWithGizmosBoundingBox: TBox3D;
begin
  if Items <> nil then
  begin
    Inc(InternalGizmoBoundingBox);
    try
      Result := Items.BoundingBox;
    finally Dec(InternalGizmoBoundingBox) end;
  end else
    Result := TBox3D.Empty;
end;

procedure TCastleViewport.SetAutoCamera(const Value: Boolean);
begin
  if FAutoCamera <> Value then
  begin
    FAutoCamera := Value;

    (*
    At one point I had an idea to do this:

    { When setting AutoCamera to false, then back to true,
      call AssignDefaultCamera again (thus resetting camera initial and current
      vectors).
      This provides a way to cause AssignDefaultCamera initialization again. }
    if Value then
      AssignDefaultCameraDone := false;

    Docs in interface:
    Setting it back to @true will make the initialization again at nearest
    render.

    Later:
    This does not seem useful, and makes more confusion,
    because it overrides both initial and current camera vectors.
    You would not expect that

    if AutoCamera then
    begin
      AutoCamera := false;
      AutoCamera := true;
    end;

    does something, but here it would do something.
    Doing the same thing with AutoNavigation doesn't
    recreate navigation (if Navigation <> nil, it will stay as it was).

    Later yet: This seems very useful in editor though, to see the effect
    in editor immediately, without reloading file.
    *)

    if InternalDesignManipulation and Value then
      AssignDefaultCameraDone := false;
  end;
end;

function TCastleViewport.InternalOverride2DProjectionSizing: TCastleUserInterface;
begin
  Result := nil;
end;

function TCastleViewport.CalculateProjection: TProjection;
var
  ViewportWidth, ViewportHeight: Single;
begin
  if (InternalOverride2DProjectionSizing <> nil)
     { We could use InternalOverride2DProjectionSizing only when really necessary,
       but it more consistent and easier to test to use it always when available.
       TODO test }
     { and
     (InternalCamera <> nil) and
     (InternalCamera.ProjectionType = ptOrthographic) and
     (InternalCamera.Orthographic.Width = 0) and
     (InternalCamera.Orthographic.Height = 0) } then
  begin
    { Bugfix for camera preview in CGE editor, when the selected viewport has
      Orthographic.Width = 0 and Orthographic.Height = 0.
      The camera preview uses the same camera (and items) as selected viewport,
      but the orthographic size should be then derived from selected viewport size,
      not size of camera preview. }
    ViewportWidth := InternalOverride2DProjectionSizing.EffectiveWidthForChildren;
    ViewportHeight := InternalOverride2DProjectionSizing.EffectiveHeightForChildren;
  end else
  begin
    ViewportWidth := EffectiveWidthForChildren;
    ViewportHeight := EffectiveHeightForChildren;
  end;

  if (InternalCamera = nil) or (not InternalCamera.ExistsInRoot) then
  begin
    { As InternalCamera is assignable, and may be nil, be tolerant and handle it.
      Just use default perspective settings. }
    Result.ProjectionType := ptPerspective;
    Result.PerspectiveAnglesRad := TViewpointNode.InternalFieldOfView(
      TCastlePerspective.DefaultFieldOfView,
      TCastlePerspective.DefaultFieldOfViewAxis,
      ViewportWidth,
      ViewportHeight);
    Result.ProjectionNear := 1;
    Result.ProjectionFar := 1000;
    Exit;
  end;

  Result := InternalCamera.InternalProjection({$ifdef FPC}@{$endif} ItemsWithGizmosBoundingBox,
    ViewportWidth, ViewportHeight,
    InternalCamera = InternalDesignCamera);
end;

function TCastleViewport.MainLightForShadowVolumes(out AMainLightPosition: TVector4): boolean;

  { Check does scene define light for shadow volumes,
    if yes - calculate the position/direction of it in world space to AMainLightPosition
    and return @true.

    May modify AMainLightPosition even when returns @false, the value AMainLightPosition
    is undefined after returning @false. }
  function LightForShadowVolumesFromScene(const Scene: TCastleScene;
    out AMainLightPosition: TVector4): Boolean;
  begin
    Result :=
      Scene.InternalMainLightForShadowVolumes(AMainLightPosition) and
      { We need WorldTransform for below conversion local<->world space.
        It may not be available, if
        - Scene is present multiple times in Items
        - Scene is not present in Items at all, temporarily, but is still a MainScene.
          Testcase: castle-game, change from level to level using debug menu.
      }
      Scene.HasWorldTransform;
    { Transform AMainLightPosition to world space. }
    if Result then
    begin
      if AMainLightPosition.W = 0 then
        AMainLightPosition.XYZ := Scene.LocalToWorldDirection(AMainLightPosition.XYZ)
      else
        AMainLightPosition := Vector4(
          Scene.LocalToWorld(AMainLightPosition.XYZ / AMainLightPosition.W), 1.0);
    end;
  end;

var
  SceneCastingLights: TCastleScene;
begin
  Result := false;

  {$warnings off} // using deprecated MainScene to keep it working
  if Items.MainScene <> nil then
  begin
    Result := LightForShadowVolumesFromScene(Items.MainScene, AMainLightPosition);
    if Result then
      Exit;
  end;

  { scan InternalScenesCastGlobalLights }
  if Items.InternalScenesCastGlobalLights <> nil then
    for SceneCastingLights in Items.InternalScenesCastGlobalLights do
      if Items.MainScene <> SceneCastingLights then // MainScene is already accounted for above
      begin
        Result := LightForShadowVolumesFromScene(SceneCastingLights, AMainLightPosition);
        if Result then
          Exit;
      end;
  {$warnings on}
end;

procedure TCastleViewport.Render3D(const Params: TRenderParams);
begin
end;

procedure TCastleViewport.RenderOnePass(const Params: TRenderParams);

  { Based on BlendingSort, determine
    ShapesRenderer.BlendingSort, making sure that sortAuto is handled correctly.

    Note: This is copy-pasted now to TestCastleViewport for testing. }
  function EffectiveBlendingSort: TShapeSortNoAuto;
  begin
    if BlendingSort = sortAuto then
    begin
      if (Camera <> nil) and
        (Camera.ProjectionType = ptOrthographic) and
        (TVector3.Equals(Camera.Direction, DefaultCameraDirection)) then
        Result := sort2D
      else
        Result := sort3D;
    end else
      Result := BlendingSort;
  end;

  { Based on OcclusionSort, determine
    ShapesRenderer.OcclusionSort, making sure that sortAuto is handled correctly. }
  function EffectiveOcclusionSort: TShapeSortNoAuto;
  begin
    if OcclusionSort = sortAuto then
    begin
      Result := sortNone;
    end else
      Result := OcclusionSort;
  end;

begin
  ShapesCollector.Clear;
  Assert(Params.Collector = ShapesCollector);

  {$warnings off} // keep deprecated working
  Render3D(Params);
  {$warnings on}

  Params.Frustum := @Params.RenderingCamera.Frustum;
  Items.Render(Params);

  ShapesRenderer.OcclusionSort := EffectiveOcclusionSort;
  ShapesRenderer.BlendingSort := EffectiveBlendingSort;
  ShapesRenderer.OnCustomShapeSort := OnCustomShapeSort;
  ShapesRenderer.Render(ShapesCollector, Params);
end;

procedure TCastleViewport.RenderShadowVolume(const Params: TRenderParams);
begin
  Params.Frustum := @Params.RenderingCamera.Frustum;
  Items.RenderShadowVolume(Params, FShadowVolumeRenderer);
end;

procedure TCastleViewport.InitializeGlobalLights(const GlobalLights: TLightInstancesList);
var
  HI: TLightInstance;
begin
  {$warnings off}
  if HeadlightInstance(HI) then
    GlobalLights.Add(HI);
  {$warnings on}
end;

procedure TCastleViewport.InitializeLights(const GlobalLights: TLightInstancesList);
begin
  InitializeGlobalLights(GlobalLights);
end;

function TCastleViewport.InternalHeadlightCamera: TCastleCamera;
begin
  { At design-time: As an exception, this *does not* use design-time camera for headlight
    pos/dir/up, as this would prevent observing headlight from various other points of view.
    So below condition actually *avoids* using InternalDesignCamera when InternalDesignManipulation. }
  if InternalDesignManipulation then
    Result := Camera
  else
    Result := Items.MainCamera;

  // do not cast headlight from camera that does not exist
  if (Result <> nil) and (not Result.ExistsInRoot) then
    Result := nil;
end;

function TCastleViewport.HeadlightInstance(out Instance: TLightInstance): boolean;
var
  Node: TAbstractLightNode;

  procedure PrepareInstance(const HC: TCastleCamera);
  var
    Position, Direction, Up: TVector3;
  begin
    Assert(Node <> nil);
    Node.InternalHeadlight := true;

    HC.GetWorldView(Position, Direction, Up);

    { set location/direction of Node }
    if Node is TAbstractPositionalLightNode then
    begin
      TAbstractPositionalLightNode(Node).FdLocation.Send(Position);
      if Node is TSpotLightNode then
        TSpotLightNode(Node).FdDirection.Send(Direction) else
      if Node is TSpotLightNode_1 then
        TSpotLightNode_1(Node).FdDirection.Send(Direction);
    end else
    if Node is TAbstractDirectionalLightNode then
      TAbstractDirectionalLightNode(Node).FdDirection.Send(Direction);

    Instance.Node := Node;
    Instance.Location := Position;
    Instance.Direction := Direction;
    Instance.Transform := TMatrix4.Identity;
    Instance.TransformScale := 1;
    Instance.Radius := MaxSingle;
    Instance.WorldCoordinates := true;
  end;

var
  HC: TCastleCamera;
begin
  Result := false;
  Node := Items.InternalHeadlight;
  if Node <> nil then
  begin
    HC := InternalHeadlightCamera;
    if HC <> nil then
    begin
      PrepareInstance(HC);
      Result := true;
    end;
  end;
end;

function TCastleViewport.PrepareParams: TPrepareParams;
{ Note: you cannot refer to PrepareParams inside
  the TCastleTransform.PrepareResources or TCastleTransform.Render implementation,
  as they may change the referenced PrepareParams.GlobalLights value.
}
begin
  { We just reuse FRenderParams.FGlobalLights below as a temporary
    TLightInstancesList that we already have created. }

  { initialize FPrepareParams.GlobalLights }
  FRenderParams.FGlobalLights.Clear;
  InitializeGlobalLights(FRenderParams.FGlobalLights);
  FPrepareParams.GlobalLights := FRenderParams.FGlobalLights;

  { initialize FPrepareParams.GlobalFog }
  if Fog <> nil then
    FPrepareParams.GlobalFog := Fog.InternalFogNode
  else
  {$warnings off} // using deprecated MainScene to keep it working
  if UseGlobalFog and
     (Items.MainScene <> nil) then
    FPrepareParams.GlobalFog := Items.MainScene.FogStack.Top
  {$warnings on}
  else
    FPrepareParams.GlobalFog := nil;

  FPrepareParams.RendererToPrepareShapes := ShapesRenderer.Renderer;

  Result := FPrepareParams;
end;

function TCastleViewport.BaseLights: TLightInstancesList;
begin
  {$warnings off} // using deprecated in deprecated, to keep it working
  Result := PrepareParams.GlobalLights as TLightInstancesList;
  {$warnings on}
end;

procedure TCastleViewport.RenderFromView3D(const Params: TRenderParams);

  procedure RenderNoShadowVolumes;
  begin
    { We must first render all non-transparent objects,
      then all transparent objects. Otherwise transparent objects
      (that must be rendered without updating depth buffer) could get brutally
      covered by non-transparent objects (that are in fact further away from
      the camera). }

    Params.InShadow := false;

    Params.Transparent := false; Params.ShadowVolumesReceivers := [false, true]; RenderOnePass(Params);
    Params.Transparent := true ; Params.ShadowVolumesReceivers := [false, true]; RenderOnePass(Params);
  end;

  procedure RenderWithShadowVolumes(const MainLightPosition: TVector4);
  begin
    if (FProjection.ProjectionFar <> ZFarInfinity) and (not FWarningZFarInfinityDone) then
    begin
      FWarningZFarInfinityDone := true;
      WritelnWarning('Rendering with Shadow Volumes, but ProjectionFar is not ZFarInfinity. Shadow volumes require ProjectionFar = ZFarInfinity. Leave TCastleCamera.ProjectionFar = 0.');
    end;

    { Initialize FShadowVolumeRenderer if needed, along with its OpenGL resources.
      This way we never even create FShadowVolumeRenderer if we will never render with shadow volumes. }
    if FShadowVolumeRenderer = nil then
    begin
      FShadowVolumeRenderer := TGLShadowVolumeRenderer.Create;
      FShadowVolumeRenderer.PrepareRenderingResources;
    end;
    FShadowVolumeRenderer.DebugRender := ShadowVolumesRender;
    FShadowVolumeRenderer.InitFrustumAndLight(Params.RenderingCamera.Frustum, MainLightPosition);
    FShadowVolumeRenderer.Render(Params,
      {$ifdef FPC}@{$endif}RenderOnePass,
      {$ifdef FPC}@{$endif}RenderShadowVolume);
  end;

var
  MainLightPosition: TVector4;
begin
  if GLFeatures.ShadowVolumesPossible and
     ShadowVolumes and
     MainLightForShadowVolumes(MainLightPosition) then
    RenderWithShadowVolumes(MainLightPosition)
  else
    RenderNoShadowVolumes;
end;

procedure TCastleViewport.RenderFromViewEverything(const RenderingCamera: TRenderingCamera);

  { Call RenderContext.Clear with proper options. }
  procedure RenderClear;
  var
    ClearBuffers: TClearBuffers;
    ClearColor: TCastleColor;
    MainLightPosition: TVector4; { ignored }
  begin
    { Make ClearColor anything defined.
      If we will include cbColor in ClearBuffers, it will actually always
      be adjusted to something appropriate. }
    ClearColor := Black;
    ClearBuffers := [];

    if ClearDepth then
      Include(ClearBuffers, cbDepth);

    if RenderingCamera.Target = rtVarianceShadowMap then
    begin
      { When rendering to VSM, we want to clear the screen to max depths (1, 1^2). }
      Include(ClearBuffers, cbColor);
      ClearColor := Vector4(1, 1, 0, 1);
    end else
    if not Transparent then
    begin
      { Note that we clear cbColor regardless whether some BackgroundRenderer
        is used in RenderBackground.
        This is more reliable, in case BackgroundRenderer rendering is transparent,

        - e.g. TImageBackgroundNode can be completely transparent or partially-transparent
          in a couple of ways. When ImageBackground.color has alpha < 1,
          when ImageBackground.texture is transparent,
          when ImageBackground.texture is NULL...

        - likewise, TBackgroundNode rendering can be transparent.
          E.g. if one of the textures on 6 cube sides didn't load.
          Or when BackgroundWireframe.
      }
      Include(ClearBuffers, cbColor);
      ClearColor := BackgroundColor;
    end;

    if GLFeatures.ShadowVolumesPossible and
       ShadowVolumes and
       MainLightForShadowVolumes(MainLightPosition) then
      Include(ClearBuffers, cbStencil);

    RenderContext.Clear(ClearBuffers, ClearColor);
  end;

  procedure RenderBackground;
  var
    BackgroundRenderer: TBackgroundRenderer;
  begin
    if Transparent then
      Exit;

    if Background <> nil then
      BackgroundRenderer := Background.InternalBackgroundRenderer
    else
    {$warnings off} // using deprecated MainScene to keep it working
    if Items.MainScene <> nil then
      BackgroundRenderer := Items.MainScene.InternalBackgroundRenderer
    else
    {$warnings on}
      BackgroundRenderer := nil;

    if BackgroundRenderer <> nil then
    begin
      if GLFeatures.EnableFixedFunction then
      begin
        {$ifndef OpenGLES}
        glLoadMatrix(RenderingCamera.RotationMatrix);
        {$endif}
      end;
      RenderingCamera.RotationOnly := true;
      { TODO: BackgroundRenderer should have its own ShapesRenderer,
        ShapesCollector. }
      BackgroundRenderer.Render(RenderingCamera, BackgroundWireframe,
        RenderRect, FProjection, ShapesCollector, ShapesRenderer);
      RenderingCamera.RotationOnly := false;
    end;
  end;

  procedure AddGlobalLightsFromScene(const SceneCastingLights: TCastleScene);
  var
    J: Integer;
    NewGlobalLight: PLightInstance;
  begin
    if not SceneCastingLights.ExistsInRoot then
      Exit;

    for J := 0 to SceneCastingLights.InternalGlobalLights.Count - 1 do
    begin
      NewGlobalLight := PLightInstance(FRenderParams.FGlobalLights.Add);
      NewGlobalLight^ := SceneCastingLights.InternalGlobalLights.L[J];
      { make NewGlobalLight^ in world coordinates }
      NewGlobalLight^.Transform := SceneCastingLights.WorldTransform * NewGlobalLight^.Transform;
      NewGlobalLight^.TransformScale := Approximate3DScale(SceneCastingLights.WorldTransform) * NewGlobalLight^.TransformScale;
      NewGlobalLight^.WorldCoordinates := true;
      NewGlobalLight^.Node.UpdateLightInstance(NewGlobalLight^);
    end;
  end;

var
  SceneCastingLights: TCastleScene;
begin
  RenderClear;
  RenderBackground;

  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    glLoadMatrix(RenderingCamera.Matrix);
    {$endif}
  end;

  { clear FRenderParams instance }
  FRenderParams.InternalPass := 0;
  FillChar(FRenderParams.Statistics, SizeOf(FRenderParams.Statistics), #0);

  { various FRenderParams initialization }
  FRenderParams.UserPass := CustomRenderingPass;
  FRenderParams.RenderingCamera := RenderingCamera;
  FRenderParams.Collector := ShapesCollector;
  FRenderParams.RendererToPrepareShapes := ShapesRenderer.Renderer;

  { calculate FRenderParams.Projection*, simplified from just like CalculateProjection does }
  FRenderParams.ProjectionBox := {$ifdef FPC}@{$endif} ItemsWithGizmosBoundingBox;
  FRenderParams.ProjectionViewportWidth := EffectiveWidthForChildren;
  FRenderParams.ProjectionViewportHeight := EffectiveHeightForChildren;

  { calculate FRenderParams.FGlobalLights }
  FRenderParams.FGlobalLights.Clear;
  { Add headlight }
  InitializeGlobalLights(FRenderParams.FGlobalLights);
  { Add lights from MainScene  }
  {$warnings off} // using deprecated MainScene to keep it working
  if Items.MainScene <> nil then
    AddGlobalLightsFromScene(Items.MainScene);
  { Add lights from all scenes with CastGlobalLights }
  if Items.InternalScenesCastGlobalLights <> nil then
    for SceneCastingLights in Items.InternalScenesCastGlobalLights do
      if Items.MainScene <> SceneCastingLights then // MainScene is already accounted for above
        AddGlobalLightsFromScene(SceneCastingLights);
  {$warnings on}

  { calculate FRenderParams.GlobalFog }
  if Fog <> nil then
    FRenderParams.GlobalFog := Fog.InternalFogNode
  else
  {$warnings off} // using deprecated MainScene to keep it working
  if UseGlobalFog and
     (Items.MainScene <> nil) then
    FRenderParams.GlobalFog := Items.MainScene.FogStack.Top
  else
  {$warnings on}
    FRenderParams.GlobalFog := nil;

  { Start with DefaultDepthRange,
    as this is the meaning of rlParent on Viewport.Items. }
  FRenderParams.DepthRange := DefaultDepthRange;

  RenderFromView3D(FRenderParams);
end;

procedure TCastleViewport.RenderWithoutScreenEffects;

  { Render everything (by RenderFromViewEverything) on the screen.
    Takes care to set RenderingCamera (Target = rtScreen and camera as given),
    and takes care to apply Scissor if not FillsWholeContainer,
    and calls RenderFromViewEverything.

    Always call ApplyProjection before this, to set correct
    projection matrix. }
  procedure RenderOnScreen(ACamera: TCastleCamera);
  var
    RenderingCamera: TRenderingCamera;
  begin
    { We reuse FRenderWithoutScreenEffectsRenderingCamera,
      but to be clean (to not use it when it's not really initialized)
      this is the only place where we refer to FRenderWithoutScreenEffectsRenderingCamera field.
      Rest of code should get RenderingCamera by parameters. }
    RenderingCamera := FRenderWithoutScreenEffectsRenderingCamera;

    RenderingCamera.Target := rtScreen;
    RenderingCamera.FromCameraObject(ACamera);

    if (not FillsWholeContainer) and (not RenderScreenEffects) then
      { Use Scissor to limit what RenderContext.Clear clears. }
      RenderContext.ScissorEnable(
        RenderRect.Translate(Vector2(RenderContext.ViewportDelta)).Round);

    RenderFromViewEverything(RenderingCamera);

    if (not FillsWholeContainer) and (not RenderScreenEffects) then
      RenderContext.ScissorDisable;
  end;

begin
  inherited;

  // as Camera is assignable, gracefully handle the case of Camera = nil
  FMissingCameraRect.Exists := (InternalCamera = nil) or (not InternalCamera.ExistsInRoot);
  if FMissingCameraRect.Exists then
  begin
    { We show the "No camera selected" using UI controls, as this is most flexible.

      Note: in particular, we do not clear viewport using RenderContext.Clear here,
      as RenderContext.Clear requires logic of RenderContext.ScissorEnable/Disable
      around it to be properly limited.
      The FMissingCameraRect clears the viewport with BackgroundColor already. }

    FMissingCameraRect.Color := BackgroundColor;
    FMissingCameraLabel.Color := Vector4(WhiteRGB - BackgroundColor.XYZ, 1);
    FMissingCameraLabel.MaxWidth := FMissingCameraRect.EffectiveWidthForChildren;
    Exit;
  end;

  { What happens if Camera is not part of our Items?

    In principle we could tolerate this right now and
    even render from camera from unrelated viewport.
    But this is not something we want to support in the long run -- it's hard to define
    what should be the behavior (maybe camera from another viewport should show
    another viewport?). As there doesn't seem to be a useful usecase to allow this,
    better to prohibit it now. }
  (*
  if (InternalCamera.World <> Items) and not WarningCameraInvalidItemsDone then
  begin
    WritelnWarning('Camera "%s" of viewport "%s" is not part of this viewport Items hierarchy. You should add it to %s.Items.', [
      InternalCamera.Name,
      Name,
      Name
    ]);
    WarningCameraInvalidItemsDone := true; // avoid flooding log with warnings about it
  end;
  *)
  if InternalCamera.World <> Items then
    raise Exception.CreateFmt('Camera "%s" of viewport "%s" is not part of this viewport''s Items. You must add the camera to %s.Items.', [
      InternalCamera.Name,
      Name,
      Name
    ]);

  ApplyProjection;
  RenderOnScreen(InternalCamera);
end;

function TCastleViewport.InternalExtraGetScreenEffects(const Index: Integer): TGLSLProgram;
begin
  if ScreenSpaceAmbientOcclusion then
    SSAOShaderInitialize;
  if ScreenSpaceReflections then
    SSRShaderInitialize;

  {$warnings off} // using deprecated MainScene to keep it working
  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) and ScreenSpaceReflections and (SSRShader <> nil) then
  begin
    if Index = 0 then
      Result := SSAOShader
    else
    if Index = 1 then
      Result := SSRShader
    else
      Result := Items.MainScene.InternalScreenEffects(Index - 2);
  end else
  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
  begin
    if Index = 0 then
      Result := SSAOShader
    else
      Result := Items.MainScene.InternalScreenEffects(Index - 1);
  end else
  if ScreenSpaceReflections and (SSRShader <> nil) then
  begin
    if Index = 0 then
      Result := SSRShader
    else
      Result := Items.MainScene.InternalScreenEffects(Index - 1);
  end else
  if Items.MainScene <> nil then
    Result := Items.MainScene.InternalScreenEffects(Index)
  else
    { no Index is valid, since ScreenEffectsCount = 0 in this class }
    Result := nil;
  {$warnings on}
end;

function TCastleViewport.InternalExtraScreenEffectsCount: Integer;
begin
  if ScreenSpaceAmbientOcclusion then
    SSAOShaderInitialize;
  if ScreenSpaceReflections then
    SSRShaderInitialize;

  {$warnings off} // using deprecated MainScene to keep it working
  if Items.MainScene <> nil then
    Result := Items.MainScene.InternalScreenEffectsCount
  else
    Result := 0;
  {$warnings off}

  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
    Inc(Result);
  if ScreenSpaceReflections and (SSRShader <> nil) then
    Inc(Result);
end;

function TCastleViewport.InternalExtraScreenEffectsNeedDepth: Boolean;
begin
  if ScreenSpaceAmbientOcclusion then
    SSAOShaderInitialize;
  if ScreenSpaceReflections then
    SSRShaderInitialize;

  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
    Exit(true);
  if ScreenSpaceReflections and (SSRShader <> nil) then
    Exit(true);
  {$warnings off} // using deprecated MainScene to keep it working
  if Items.MainScene <> nil then
    Result := Items.MainScene.InternalScreenEffectsNeedDepth
  else
  {$warnings on}
    Result := false;
end;

procedure TCastleViewport.SSAOShaderInitialize;
begin
  { Do not retry creating SSAOShader if SSAOShaderInitialize was already called.
    Even if SSAOShader is nil (when SSAOShaderInitialize = true but
    SSAOShader = nil it means that compiling SSAO shader fails on this GPU). }
  if SSAOShaderInitialized then Exit;

  // SSAOShaderInitialized = false implies SSAOShader = nil
  Assert(SSAOShader = nil);

  if GLFeatures.Shaders then
  begin
    try
      SSAOShader := TSSAOScreenEffect.Create;
      SSAOShader.Viewport := Self;
      SSAOShader.NeedsDepth := true;
      SSAOShader.ScreenEffectShader := {$I ssao.glsl.inc};
      SSAOShader.Link;
    except
      on E: EGLSLError do
      begin
        WritelnLog('GLSL', 'Error when initializing GLSL shader for ScreenSpaceAmbientOcclusionShader: ' + E.Message);
        FreeAndNil(SSAOShader);
        ScreenSpaceAmbientOcclusion := false;
      end;
    end;
  end;
  SSAOShaderInitialized := true;
end;

procedure TCastleViewport.SSRShaderInitialize;
begin
  { Do not retry creating SSRShader if SSRShaderInitialize was already called.
    Even if SSRShader is nil (when SSRShaderInitialize = true but
    SSRShader = nil it means that compiling SSR shader fails on this GPU). }
  if SSRShaderInitialized then Exit;

  // SSAOShaderInitialized = false implies SSRShader = nil
  Assert(SSRShader = nil);
  if GLFeatures.Shaders then
  begin
    try
      SSRShader := TSSRScreenEffect.Create;
      SSRShader.Viewport := Self;
      SSRShader.NeedsDepth := True;
      SSRShader.ScreenEffectShader := {$I ssr.glsl.inc};
      SSRShader.Link;
    except
      on E: EGLSLError do
      begin
        WritelnLog('GLSL', 'Error when initializing GLSL shader for ScreenSpaceReflectionsShader: ' + E.Message);
        FreeAndNil(SSRShader);
        ScreenSpaceReflections := False;
      end;
    end;
  end;
  SSRShaderInitialized := True;
end;

procedure TCastleViewport.GLContextOpen;
begin
  inherited;
end;

procedure TCastleViewport.GLContextClose;
begin
  FreeAndNil(FShadowVolumeRenderer);

  FreeAndNil(SSAOShader);
  SSAOShaderInitialized := false;

  if ShapesRenderer <> nil then
    ShapesRenderer.GLContextClose;

  inherited;
end;

function TCastleViewport.ScreenSpaceAmbientOcclusionAvailable: boolean;
begin
  SSAOShaderInitialize;
  Result := (SSAOShader <> nil);
end;

function TCastleViewport.ScreenSpaceReflectionsAvailable: Boolean;
begin
  SSRShaderInitialize;
  Result := (SSRShader <> nil);
end;

procedure TCastleViewport.SetScreenSpaceAmbientOcclusion(const Value: boolean);
begin
  if FScreenSpaceAmbientOcclusion <> Value then
  begin
    FScreenSpaceAmbientOcclusion := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleViewport.SetScreenSpaceReflections(const Value: Boolean);
begin
  if FScreenSpaceReflections <> Value then
  begin
    FScreenSpaceReflections := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleViewport.SetScreenSpaceReflectionsSurfaceGlossiness(const Value: Single);
begin
  if FScreenSpaceReflectionsSurfaceGlossiness <> Value then
  begin
    FScreenSpaceReflectionsSurfaceGlossiness := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleViewport.AssignDefaultCamera;
var
  Box: TBox3D;
  Scene: TCastleScene;
  APos, ADir, AUp, NewGravityUp: TVector3;
begin
  if Camera = nil then
    Exit; // abort, until you assign Camera

  Box := ItemsBoundingBox;
  {$warnings off} // using deprecated MainScene to keep it working
  Scene := Items.MainScene;
  {$warnings on}
  if Scene <> nil then
  begin
    Scene.InternalUpdateCamera(Camera, Box, false, false);
  end else
  begin
    CameraViewpointForWholeScene(Box, 2, 1, false, true,
      APos, ADir, AUp, NewGravityUp);
    Camera.SetWorldView(APos, ADir, AUp);
    Camera.GravityUp := NewGravityUp;
  end;

  { Mark it as done, so that next EnsureCameraDetected does nothing
    if you manually called this earlier.
    This is consistent with AssignDefaultNavigation,
    that always sets Navigation <> nil thus it is no longer auto-detected
    if you call AssignDefaultNavigation again. }
  AssignDefaultCameraDone := true;
end;

function TCastleViewport.Statistics: TRenderStatistics;
begin
  Result := FRenderParams.Statistics;
end;

procedure TCastleViewport.Setup2D;
begin
  Camera.SetWorldView(
    { pos } Vector3(0, 0, Default2DCameraZ),
    { dir } Vector3(0, 0, -1),
    { up } Vector3(0, 1, 0));
  Camera.GravityUp := Vector3(0, 1, 0);
  Camera.ProjectionType := ptOrthographic;
  {$warnings off} // using deprecated MainScene to keep it working
  AutoCamera := false;
  {$warnings on}
end;

procedure TCastleViewport.PositionToPrerequisites;
begin
  { Note that we need to call this every time before PositionToXxx,
    not just only once (in case ApplyProjection did not yet happen).
    That's because Camera settings, that determine how CalculateProjection calculates
    FProjection, may change at any moment, e.g. when doing a sequence

      Viewport.PositionToCameraPlane(...);
      Camera.Orthographic.Scale := Camera.Orthographic.Scale * ...;
      Viewport.PositionToCameraPlane(...);
  }
  if (EffectiveWidth = 0) or
     (EffectiveHeight = 0) then
    raise Exception.Create('Cannot use TCastleViewport.PositionToXxx when viewport has effectively empty size. ' + 'The typical solution is to add TCastleViewport to some UI hierarchy, like "Window.Container.InsertFront(MyViewport)", although you could also set TCastleViewport.Width/Height explicitly.');

  EnsureCameraDetected;

  FProjection := CalculateProjection;
  {$ifdef FPC}
  {$warnings off} // using deprecated to keep it working
  if Assigned(OnProjection) then
    OnProjection(FProjection);
  {$warnings on}
  {$endif}
end;

procedure TCastleViewport.PositionToRay(const Position: TVector2;
  const ContainerCoordinates: Boolean;
  out RayOrigin, RayDirection: TVector3);
var
  R: TFloatRectangle;
  ContainerPosition: TVector2;
begin
  PositionToPrerequisites;

  R := RenderRect;

  if ContainerCoordinates then
    ContainerPosition := Position
  else
    ContainerPosition := LocalToContainerPosition(Position);

  Assert(InternalCamera <> nil);
  InternalCamera.CustomRay(R, ContainerPosition, FProjection, RayOrigin, RayDirection);
end;

function TCastleViewport.PositionToCameraPlane(const Position: TVector2;
  const ContainerCoordinates: Boolean;
  const Depth: Single; out PlanePosition: TVector3): Boolean;
var
  RayOrigin, RayDirection: TVector3;
  Plane: TVector4;
begin
  PositionToRay(Position, ContainerCoordinates, RayOrigin, RayDirection);

  Plane := Vector4(InternalCamera.Direction,
    { We know that InternalCamera.Direction, which is used as Plane.XYZ, is normalized.
      Calculate Plane[3] such that point RayOrigin + InternalCamera.Direction * Depth
      satisfies the plane equation. }
    - TVector3.DotProduct(RayOrigin + InternalCamera.Direction * Depth, InternalCamera.Direction));

  Result := TryPlaneRayIntersection(PlanePosition, Plane, RayOrigin, RayDirection);
end;

function TCastleViewport.PositionToWorldPlane(const Position: TVector2;
  const ContainerCoordinates: Boolean;
  const PlaneConstValue: Single;
  out PlanePosition: TVector3): Boolean;
begin
  Result := PositionToWorldPlane(Position, ContainerCoordinates,
    2, PlaneConstValue, PlanePosition);
end;

function TCastleViewport.PositionToWorldPlane(const Position: TVector2;
  const ContainerCoordinates: Boolean;
  const PlaneConstCoord: T3DAxis; const PlaneConstValue: Single;
  out PlanePosition: TVector3): Boolean;
var
  RayOrigin, RayDirection: TVector3;
begin
  PositionToRay(Position, ContainerCoordinates, RayOrigin, RayDirection);
  Result := TrySimplePlaneRayIntersection(PlanePosition, PlaneConstCoord, PlaneConstValue,
    RayOrigin, RayDirection);
end;

function TCastleViewport.PositionTo2DWorld(const Position: TVector2;
  const ContainerCoordinates: Boolean): TVector2;

{ Version 1:
  This makes sense, but ignores TCastleExamineNavigation.ScaleFactor (assumes unscaled camera).

var
  P: TVector2;
  Proj: TProjection;
  ProjRect: TFloatRectangle;
begin
  if ContainerCoordinates then
    P := (Position - RenderRect.LeftBottom) / UIScale
  else
    P := Position;

  Proj := Projection;
  if Proj.ProjectionType <> ptOrthographic then
    raise Exception.Create('TCastleViewport.PositionTo2DWorld assumes an orthographic projection, like the one set by TCastle2DSceneManager.CalculateProjection');
  ProjRect := Proj.Dimensions;

  if Navigation <> nil then
    ProjRect := ProjRect.Translate(Navigation.Position.XY);

  Result := Vector2(
    MapRange(P.X, 0, EffectiveWidth , ProjRect.Left  , ProjRect.Right),
    MapRange(P.Y, 0, EffectiveHeight, ProjRect.Bottom, ProjRect.Top)
  );
end; }

{ Version 2:
  This also makes sense, but also
  ignores TCastleExamineNavigation.ScaleFactor (assumes unscaled camera).
  PositionToRay looks only at camera pos/dir/up and ignores scaling.

var
  RayOrigin, RayDirection: TVector3;
begin
  PositionToRay(MousePosition, true, RayOrigin, RayDirection);
  Result := RayOrigin.XY;
end; }

{ Version 3:
  Should work, but
  1. Cannot invert projection matrix,
  2. Also it's not efficient, since camera has ready InverseMatrix calculated
     more efficiently.

var
  WorldToScreenMatrix: TMatrix4;
  ScreenToWorldMatrix: TMatrix4;
  P: TVector2;
begin
  WorldToScreenMatrix := RequiredNavigation.ProjectionMatrix * RequiredNavigation.Matrix;
  if not WorldToScreenMatrix.TryInverse(ScreenToWorldMatrix) then
    raise Exception.Create('Cannot invert projection * camera matrix. Possibly one of them was not initialized, or camera contains scale to zero.');

  if ContainerCoordinates then
    P := (Position - RenderRect.LeftBottom) / UIScale
  else
    P := Position;
  P := Vector2(
    MapRange(P.X, 0, EffectiveWidth , -1, 1),
    MapRange(P.Y, 0, EffectiveHeight, -1, 1)
  );

  Result := ScreenToWorldMatrix.MultPoint(Vector3(P, 0)).XY;
end; }

var
  CameraToWorldMatrix: TMatrix4;
  P: TVector2;
begin
  PositionToPrerequisites;

  CameraToWorldMatrix := InternalCamera.MatrixInverse;

  if ContainerCoordinates then
    P := ContainerToLocalPosition(Position)
  else
    P := Position;
  P := Vector2(
    MapRange(P.X, 0, EffectiveWidth , FProjection.Dimensions.Left  , FProjection.Dimensions.Right),
    MapRange(P.Y, 0, EffectiveHeight, FProjection.Dimensions.Bottom, FProjection.Dimensions.Top)
  );

  Result := CameraToWorldMatrix.MultPoint(Vector3(P, 0)).XY;
end;

function TCastleViewport.PositionFrom2DWorld(const WorldPosition: TVector2;
  const ContainerCoordinates: Boolean): TVector2;
var
  CameraFromWorldMatrix: TMatrix4;
  P: TVector2;
begin
  PositionToPrerequisites;

  CameraFromWorldMatrix := InternalCamera.Matrix;
  P := CameraFromWorldMatrix.MultPoint(Vector3(WorldPosition, 0)).XY;
  P := Vector2(
    MapRange(P.X, FProjection.Dimensions.Left  , FProjection.Dimensions.Right, 0, EffectiveWidth ),
    MapRange(P.Y, FProjection.Dimensions.Bottom, FProjection.Dimensions.Top  , 0, EffectiveHeight)
  );
  if ContainerCoordinates then
    Result := LocalToContainerPosition(P)
  else
    Result := P;
end;

function TCastleViewport.PositionFromWorld(const WorldPosition: TVector3): TVector2;
var
  Matrix: TMatrix4;
  P: TVector3;
begin
  PositionToPrerequisites;

  Matrix := InternalCamera.ProjectionMatrix * InternalCamera.Matrix;
  P := Matrix.MultPoint(WorldPosition);
  Result := Vector2(
    MapRange(P.X, -1, 1, 0, EffectiveWidth ),
    MapRange(P.Y, -1, 1, 0, EffectiveHeight)
  );
end;

procedure TCastleViewport.SetItems(Value: TCastleRootTransform);
begin
  if FItems <> Value then
  begin
    { Do not allow to set this to nil, for now, unless csDestroying. }
    if (Value = nil) and not (csDestroying in ComponentState) then
    begin
      Value := TCastleRootTransform.Create(Self);
      Value.OnCursorChange := {$ifdef FPC}@{$endif} RecalculateCursor;
    end;

    if InternalDesignManipulation then
      if FItems <> nil then
        FItems.Remove(InternalDesignCamera);

    FItems := Value;
    FItemsObserver.Observed := Value;

    { Keep InternalDesignCamera part of current Items.
      Note: This is actually never executed in reality, for now.
      Because we don't allow to do SetItems on viewports in designs in CGE editor.
      And only they have InternalDesignManipulation = true.
      Maybe it will become useful in the future, once we allow to set Items in editor. }
    if InternalDesignManipulation then
      if FItems <> nil then
        FItems.Add(InternalDesignCamera);

    LastVisibleStateIdForVisibleChange := 0;

    { TODO: do the same thing we did when creating internal FItems:
    FItems.OnCursorChange := @RecalculateCursor;

    // No need to change this, it's documented that MainCamera has to be manually managed if you reuse items
    // FItems.MainCamera := Camera;
    }
  end;
end;

procedure TCastleViewport.ItemsFreeNotification(const Sender: TFreeNotificationObserver);
begin
  Items := nil;
end;

function TCastleViewport.GetPaused: Boolean;
begin
  Result := Items.Paused;
end;

procedure TCastleViewport.SetPaused(const Value: Boolean);
begin
  Items.Paused := Value;
end;

function TCastleViewport.GetMainScene: TCastleScene;
begin
  {$warnings off} // using deprecated MainScene to keep it working
  Result := Items.MainScene;
  {$warnings on}
end;

function TCastleViewport.MouseRayHitContains(const Item: TCastleTransform): boolean;
begin
  Result := (MouseRayHit <> nil) and
            (MouseRayHit.IndexOfItem(Item) <> -1);
end;

procedure TCastleViewport.SetMouseRayHit(const Value: TRayCollision);
var
  I: Integer;
begin
  if FMouseRayHit <> Value then
  begin
    { Always keep FreeNotification on every 3D item inside MouseRayHit.
      When it's destroyed, our MouseRayHit must be freed too,
      it cannot be used in subsequent RecalculateCursor. }

    if FMouseRayHit <> nil then
    begin
      for I := 0 to FMouseRayHit.Count - 1 do
      begin
        { leave free notification for 3D item if it's also present somewhere else }
        if (FMouseRayHit[I].Item <> FAvoidNavigationCollisions) then
          FMouseRayHit[I].Item.RemoveFreeNotification(Self);
      end;
      FreeAndNil(FMouseRayHit);
    end;

    FMouseRayHit := Value;

    if FMouseRayHit <> nil then
    begin
      for I := 0 to FMouseRayHit.Count - 1 do
        FMouseRayHit[I].Item.FreeNotification(Self);
    end;
  end;
end;

procedure TCastleViewport.SetAvoidNavigationCollisions(const Value: TCastleTransform);
begin
  if FAvoidNavigationCollisions <> Value then
  begin
    if FAvoidNavigationCollisions <> nil then
    begin
      { leave free notification for FAvoidNavigationCollisions if it's also present somewhere else }
      if (not MouseRayHitContains(FAvoidNavigationCollisions)) then
        FAvoidNavigationCollisions.RemoveFreeNotification(Self);
    end;

    FAvoidNavigationCollisions := Value;

    if FAvoidNavigationCollisions <> nil then
      FAvoidNavigationCollisions.FreeNotification(Self);
  end;
end;

procedure TCastleViewport.PrepareResources(const DisplayProgressTitle: string;
  const Options: TPrepareResourcesOptions);
begin
  PrepareResources(Options);
end;

procedure TCastleViewport.PrepareResources(const Item: TCastleTransform;
  const DisplayProgressTitle: string;
  Options: TPrepareResourcesOptions);
begin
  PrepareResources(Item, Options);
end;

procedure TCastleViewport.PrepareResources(const Item: TCastleTransform;
  Options: TPrepareResourcesOptions);
var
  MainLightPosition: TVector4; // value of this is ignored
begin
  if not ApplicationProperties.IsGLContextOpen then
  begin
    WritelnWarning('It is best to call PrepareResources only once rendering context is initialized, to allow preparing all rendering resources.' + NL +
      'Various events and virtual methods can be used to wait for the context:' + NL +
      '- (if you use CastleWindow) Application.OnInitialize' + NL +
      '- TCastleUserInterface.GLContextOpen' + NL +
      'We will continue, but some rendering resources may need to be prepared on-demand later.'
    );
    // despite the warning, allow PrepareResources to run, to make it easy for users
  end;

  if (GLFeatures <> nil) and
     GLFeatures.ShadowVolumesPossible and
     ShadowVolumes and
     MainLightForShadowVolumes(MainLightPosition) then
    Include(Options, prShadowVolume);

  { call TCastleScreenEffects.PrepareResources. }
  inherited PrepareResources;

  if ContainerSizeKnown then
  begin
    { TODO: This is possibly not necessary now.

      It used to be necessary, to update MainScene.BackgroundSkySphereRadius,
      and in effect make preparation of "prBackground" useful.
      But we removed the need for MainScene.BackgroundSkySphereRadius. }
    ApplyProjection;
  end;

  {$warnings off} // using deprecated, this should be internal
  Item.PrepareResources(Options, PrepareParams);
  {$warnings on}

  ShapesRenderer.PrepareResources;
end;

procedure TCastleViewport.PrepareResources(
  const Options: TPrepareResourcesOptions);
begin
  PrepareResources(Items, Options);
end;

procedure TCastleViewport.BeforeRender;
begin
  inherited;

  { Do it only once, otherwise BeforeRender eats time each frame
    (traversing TCastleTransform tree one more time, usually doing nothing if
    the TCastleScene are already prepared). }
  if not PrepareResourcesDone then
  begin
    PrepareResources;
    PrepareResourcesDone := true;
  end;
end;

function TCastleViewport.GetMousePosition(out MousePosition: TVector2): Boolean;
var
  C: TCastleContainer;
begin
  C := Container;
  Result := C <> nil;
  if Result then
  begin
    {$warnings off} // TODO: using deprecated Navigation for now
    if (Navigation is TCastleMouseLookNavigation) and
       { Note: We need to check InternalUsingMouseLook, not just MouseLook,
         to prevent from honoring MouseLook on user-designed TCastleWalkNavigation component
         at design-time. At design-time, only MouseLook on TCastleWalkNavigationDesign
         should have any effect.
         See https://forum.castle-engine.io/t/gizmos-for-transforming-objects-stopped-working/643/5 . }
       TCastleMouseLookNavigation(Navigation).InternalUsingMouseLook then
    {$warnings on}
      MousePosition := RenderRect.Center
    else
      MousePosition := C.MousePosition;
  end;
end;

function TCastleViewport.FakeRayCollisionNode(const RayOriginWorld, RayDirectionWorld: TVector3;
  const Item: TCastleTransform): TRayCollisionNode;
var
  RayOrigin, RayDirection: TVector3;
begin
  if Item.HasWorldTransform and
     (Item.Parent <> nil) and
     Item.Parent.HasWorldTransform then
  begin
    RayOrigin := Item.Parent.WorldToLocal(RayOriginWorld);
    RayDirection := Item.Parent.WorldToLocalDirection(RayDirectionWorld);
  end else
  begin
    WritelnWarning('TODO: Item %s(%s) is not part of World, or is present in the World multiple times. PointingDeviceXxx events will receive ray in world coordinates, while they should be in local.', [
      Item.Name,
      Item.ClassName
    ]);
    RayOrigin := RayOriginWorld;
    RayDirection := RayDirectionWorld;
  end;

  { sets Triangle and Point (and any potential future fields) to zero }
  FillChar(Result, SizeOf(Result), #0);
  Result.Item := Item;
  Result.RayOrigin := RayOrigin;
  Result.RayDirection := RayDirection;
end;

function TCastleViewport.PointingDevicePress: Boolean;

  { Pass pointing device (mouse or touch) press event
    to TCastleTransform instances in @link(Items) hit by RayHit. }
  function PointingDevicePressCore(const RayHit: TRayCollision; const RayOrigin, RayDirection: TVector3): boolean;

    function CallPress(const Pick: TRayCollisionNode; const Distance: Single): Boolean;
    begin
      if not Pick.Item.Exists then // prevent calling Pick.Item.PointingDeviceXxx when item Exists=false
        Result := false
      else
        Result := Pick.Item.PointingDevicePress(Pick, Distance);
    end;

  var
    Distance: Single;
    I: Integer;
  begin
    Result := false;

    if RayHit <> nil then
      Distance := RayHit.Distance
    else
      Distance := MaxSingle;

    // call TCastleTransform.PointingDevicePress on all items on RayHit
    if RayHit <> nil then
      for I := 0 to RayHit.Count - 1 do
      begin
        Result := CallPress(RayHit[I], Distance);
        if Result then
        begin
          { This check avoids assigning to CapturePointingDevice something
            that is no longer part of our Items after it handled PointingDevicePress. }
          if RayHit[I].Item.World = Items then
            CapturePointingDevice := RayHit[I].Item;
          Exit;
        end;
      end;
  end;

var
  MousePosition: TVector2;

  { Try PointingDevicePressCore on stuff hit by ray,
    with ray moved by given number of screen pixels from current mouse position.
    Call only if MousePosition is already assigned. }
  function TryActivateAround(const Change: TVector2): boolean;
  var
    RayOrigin, RayDirection: TVector3;
    RayHit: TRayCollision;
  begin
    PositionToRay(MousePosition + Change, true, RayOrigin, RayDirection);
    RayHit := CameraRayCollision(RayOrigin, RayDirection);
    try
      Result := PointingDevicePressCore(RayHit, RayOrigin, RayDirection);
    finally FreeAndNil(RayHit) end;
  end;

  function TryActivateAroundSquare(const Change: Single): boolean;
  begin
    Result := TryActivateAround(Vector2(-Change, -Change)) or
              TryActivateAround(Vector2(-Change, +Change)) or
              TryActivateAround(Vector2(+Change, +Change)) or
              TryActivateAround(Vector2(+Change, -Change)) or
              TryActivateAround(Vector2(      0, -Change)) or
              TryActivateAround(Vector2(      0, +Change)) or
              TryActivateAround(Vector2(-Change,       0)) or
              TryActivateAround(Vector2(+Change,       0));
  end;

begin
  Result := PointingDevicePressCore(MouseRayHit, MouseRayOrigin, MouseRayDirection);

  if not Result then
  begin
    if ApproximateActivation and GetMousePosition(MousePosition) then
      Result := TryActivateAroundSquare(25) or
                TryActivateAroundSquare(50) or
                TryActivateAroundSquare(100) or
                TryActivateAroundSquare(200);
  end;

  if not Result then
    PointingDevicePressFailed;
end;

procedure TCastleViewport.PointingDevicePressFailed;
begin
  {$warnings off} // just to keep deprecated working
  SoundEngine.Play(stPlayerInteractFailed);
  {$warnings on}
end;

function TCastleViewport.PointingDeviceRelease: Boolean;

  { Pass pointing device (mouse or touch) release event
    to TCastleTransform instances in @link(Items) hit by RayHit. }
  function PointingDeviceReleaseCore(const RayHit: TRayCollision; const RayOrigin, RayDirection: TVector3): boolean;

    function CallRelease(const Pick: TRayCollisionNode; const Distance: Single): Boolean;
    begin
      if not Pick.Item.Exists then // prevent calling Pick.Item.PointingDeviceXxx when item Exists=false
        Result := false
      else
        Result := Pick.Item.PointingDeviceRelease(Pick, Distance, false);
    end;

  var
    Distance: Single;
    NodeIndex, I: Integer;
  begin
    Result := false;

    if RayHit <> nil then
      Distance := RayHit.Distance
    else
      Distance := MaxSingle;

    if CapturePointingDevice <> nil then
    begin
      // call CapturePointingDevice.PointingDeviceRelease
      if RayHit <> nil then
        NodeIndex := RayHit.IndexOfItem(CapturePointingDevice)
      else
        NodeIndex := -1;
      if NodeIndex <> -1 then
        Result := CallRelease(RayHit[NodeIndex], Distance)
      else
        Result := CallRelease(FakeRayCollisionNode(RayOrigin, RayDirection, CapturePointingDevice), Distance);
      CapturePointingDevice := nil; // no longer capturing, after release
      if Result then Exit;
    end;

    // call TCastleTransform.PointingDeviceRelease on remaining items on RayHit
    if RayHit <> nil then
      for I := 0 to RayHit.Count - 1 do
        if (CapturePointingDevice = nil) or
           (CapturePointingDevice <> RayHit[I].Item) then
        begin
          Result := CallRelease(RayHit[I], Distance);
          if Result then Exit;
        end;
  end;

begin
  Result := PointingDeviceReleaseCore(MouseRayHit, MouseRayOrigin, MouseRayDirection);
end;

function TCastleViewport.PointingDeviceMove: boolean;

  { Pass pointing device (mouse or touch) move event
    to TCastleTransform instances in @link(Items) hit by RayHit. }
  function PointingDeviceMoveCore(const RayHit: TRayCollision; const RayOrigin, RayDirection: TVector3): boolean;

    function CallMove(const Pick: TRayCollisionNode; const Distance: Single): Boolean;
    begin
      if not Pick.Item.Exists then // prevent calling Pick.Item.PointingDeviceXxx when item Exists=false
        Result := false
      else
        Result := Pick.Item.PointingDeviceMove(Pick, Distance);
    end;

  var
    Distance: Single;
    I, NodeIndex: Integer;
  begin
    Result := false;

    if RayHit <> nil then
      Distance := RayHit.Distance
    else
      Distance := MaxSingle;

    if CapturePointingDevice <> nil then
    begin
      // call CapturePointingDevice.PointingDeviceMove
      if RayHit <> nil then
        NodeIndex := RayHit.IndexOfItem(CapturePointingDevice)
      else
        NodeIndex := -1;
      if NodeIndex <> -1 then
        Result := CallMove(RayHit[NodeIndex], Distance)
      else
        Result := CallMove(FakeRayCollisionNode(RayOrigin, RayDirection, CapturePointingDevice), Distance);
      if Result then Exit;
    end;

    // call TCastleTransform.PointingDeviceMove on remaining items on RayHit
    if RayHit <> nil then
      for I := 0 to RayHit.Count - 1 do
        if (CapturePointingDevice = nil) or
           (CapturePointingDevice <> RayHit[I].Item) then
        begin
          Result := CallMove(RayHit[I], Distance);
          if Result then Exit;
        end;

    // call MainScene.PointingDeviceMove, to allow to update X3D sensors "isOver"
    {$warnings off} // using deprecated MainScene to keep it working
    if (Items.MainScene <> nil) and
       (CapturePointingDevice <> Items.MainScene) and
       ((RayHit = nil) or (RayHit.IndexOfItem(Items.MainScene) = -1)) then
      CallMove(FakeRayCollisionNode(RayOrigin, RayDirection, Items.MainScene), Distance);
    {$warnings on}
  end;

begin
  Result := PointingDeviceMoveCore(MouseRayHit, MouseRayOrigin, MouseRayDirection);
end;

procedure TCastleViewport.InternalCameraChanged(Sender: TObject);
var
  Pos, Dir, Up: TVector3;
  SenderCamera: TCastleCamera;
  MainCamera: TCastleCamera;
begin
  SenderCamera := Sender as TCastleCamera; // may be Camera or InternalDesignCamera
  MainCamera := Items.MainCamera;
  if SenderCamera = MainCamera then
  begin
    Inc(Items.InternalMainCameraStateId);

    // we should redraw soon, this will cause VisibleChange() call
    Inc(Items.InternalVisibleStateId);

    // we changed MainCamera which makes headlight -> so lighting changed
    if Items.InternalHeadlight <> nil then
      Inc(Items.InternalVisibleNonGeometryStateId);

    SenderCamera.GetView(Pos, Dir, Up);
    SoundEngine.InternalUpdateListener(Pos, Dir, Up);
  end;

  if Assigned(OnCameraChanged) then
    OnCameraChanged(Self);
end;

function TCastleViewport.UseAvoidNavigationCollisions: Boolean;
begin
  { Only use AvoidNavigationCollisions when it is part of current world.
    Otherwise using methods like TCastleTransform.Ray (by AvoidNavigationCollisions.Ray)
    would try to access AvoidNavigationCollisions's World,
    which is not correct now (nil, or points to something else).
    See https://github.com/castle-engine/castle-engine/pull/220 ,
    https://trello.com/c/iz7uvjKN/79-frogger3d-crash-when-pressing-enter-on-game-over-message-box . }
  Result := (AvoidNavigationCollisions <> nil) and (AvoidNavigationCollisions.World = Items);
end;

function TCastleViewport.InternalNavigationMoveAllowed(const Sender: TCastleNavigation;
  const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
  const Radius: Single; const BecauseOfGravity: Boolean): Boolean;

  function PositionOutsideBoundingBox: Boolean;
  var
    Box: TBox3D;
    GravityCoordinate, Coord1, Coord2: T3DAxis;
  begin
    Box := ItemsBoundingBox;
    if Box.IsEmpty then Exit(false);
    GravityCoordinate := MaxAbsVectorCoord(NewPos - OldPos);
    RestOf3DCoords(GravityCoordinate, Coord1, Coord2);
    { Do not fall down if OldPos is outside of Box.
      But in case OldPos is *above* Box, consider it *inside* the box
      (gravity will work there).
      This is useful to allow jumping on top of the scene work naturally. }
    Result :=
      (OldPos[Coord1] < Box.Data[0][Coord1]) or
      (OldPos[Coord1] > Box.Data[1][Coord1]) or
      (OldPos[Coord2] < Box.Data[0][Coord2]) or
      (OldPos[Coord2] > Box.Data[1][Coord2]) or
      (OldPos[GravityCoordinate] < Box.Data[0][GravityCoordinate]);
  end;

var
  SavedExists: Boolean;
begin
  // take into account PreventInfiniteFallingDown
  if BecauseOfGravity and
     PreventInfiniteFallingDown and
     PositionOutsideBoundingBox then
    Exit(false);

  if UseAvoidNavigationCollisions then
  begin
    SavedExists := AvoidNavigationCollisions.Exists;
    AvoidNavigationCollisions.Exists := false;
  end else
    SavedExists := false; // avoid Delphi warnings about SavedDepthRange uninitialized

  Result := Items.WorldMoveAllowed(OldPos, ProposedNewPos, NewPos, true, Radius,
    { We prefer to resolve collisions with navigation using sphere.
      But for TCastleTransform implementations that can't use sphere, we can construct box. }
    Box3DAroundPoint(OldPos, Radius * 2),
    Box3DAroundPoint(ProposedNewPos, Radius * 2), BecauseOfGravity);

  if UseAvoidNavigationCollisions then
    AvoidNavigationCollisions.Exists := SavedExists;
end;

function TCastleViewport.InternalNavigationHeight(const Sender: TCastleNavigation;
  const Position: TVector3;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
var
  SavedExists: Boolean;
begin
  if UseAvoidNavigationCollisions then
  begin
    SavedExists := AvoidNavigationCollisions.Exists;
    AvoidNavigationCollisions.Exists := false;
  end else
    SavedExists := false; // avoid Delphi warnings about SavedDepthRange uninitialized

  Result := Items.WorldHeight(Position, AboveHeight, AboveGround);

  if UseAvoidNavigationCollisions then
    AvoidNavigationCollisions.Exists := SavedExists;
end;

function TCastleViewport.CameraRayCollision(const RayOrigin, RayDirection: TVector3): TRayCollision;
var
  SavedExists: Boolean;
begin
  if UseAvoidNavigationCollisions then
  begin
    SavedExists := AvoidNavigationCollisions.Exists;
    AvoidNavigationCollisions.Exists := false;
  end else
    SavedExists := false; // avoid Delphi warnings about SavedDepthRange uninitialized

  Result := Items.WorldRay(RayOrigin, RayDirection);

  if UseAvoidNavigationCollisions then
    AvoidNavigationCollisions.Exists := SavedExists;
end;

procedure TCastleViewport.BoundViewpointChanged;
begin
  if Assigned(OnBoundViewpointChanged) then
    OnBoundViewpointChanged(Self);
end;

procedure TCastleViewport.BoundNavigationInfoChanged;
begin
  if Assigned(OnBoundNavigationInfoChanged) then
    OnBoundNavigationInfoChanged(Self);
end;

procedure TCastleViewport.MainSceneAndCamera_BoundViewpointChanged(Sender: TObject);
begin
  {$warnings off} // using deprecated AutoCamera and MainScene to keep it working
  if AutoCamera then
  begin
    Items.MainScene.InternalUpdateCamera(Camera, ItemsBoundingBox, false, true);
    BoundViewpointChanged;
  end;
  {$warnings on}
end;

procedure TCastleViewport.MainSceneAndCamera_BoundNavigationInfoChanged(Sender: TObject);
begin
  BoundNavigationInfoChanged;
end;

procedure TCastleViewport.MainSceneAndCamera_BoundViewpointVectorsChanged(Sender: TObject);
begin
  {$warnings off} // using deprecated AutoCamera and MainScene to keep it working
  if AutoCamera { or AnimateCameraByViewpoint } then
    Items.MainScene.InternalUpdateCamera(Camera, ItemsBoundingBox, true,
      { AllowTransitionAnimate - this would break Viewpoint animation from X3D if true } false);
  {$warnings on}
end;

class procedure TCastleViewport.CreateComponentWithChildren3D(Sender: TObject);
begin
  (Sender as TCastleViewport).SetupChildren3D;
end;

class procedure TCastleViewport.CreateComponentWithChildren2D(Sender: TObject);
begin
  (Sender as TCastleViewport).SetupChildren2D;
end;

procedure TCastleViewport.SetupChildren2D;
var
  Plane: TCastlePlane;
begin
  FullSize := true;
  SetupDesignTimeCamera;
  { Better Origin default, makes things in center }
  Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  { Set Orthographic.Height,
    as we advise users to set Camera.Orthographic.Width/Height to non-zero.

    This way the displayed items don't depend on your viewport size.
    So you can change viewport size (even to something very small,
    e.g. to squeeze it into some TCastleButton) and keep the same fov.
    You can also change UI scaling and keep the same fov. }
  Camera.Orthographic.Height := 1000;
  Setup2D;

  { purpose: initial 2D object,
    that you can see as a whole in initial view,
    allows to see screen corner - also has distance from edge. }
  Plane := TCastlePlane.Create(Owner);
  Plane.Name := ProposeComponentName(TCastlePlane, Owner);
  Plane.Axis := 2;
  Plane.Size := Vector2(200, 200);
  Plane.Material := pmUnlit;
  Items.Add(Plane);

  if InternalDesignManipulation then
  begin
    { Similar to Setup2D, but here done on design-time camera. }
    InternalDesignCamera.ProjectionType := ptOrthographic;
    InternalDesignCamera.SetWorldView(
      { We move Z back, to be able to see from design-time camera the runtime camera gizmo.
        Note that 2D Camera.EffectiveProjectionNear is negative by default (after Setup2D),
        so "- Camera.EffectiveProjectionNear" actually does "+ 1000". }
      { pos } Vector3(0, 0, Camera.Translation.Z - Camera.EffectiveProjectionNear + 100),
      { dir } Vector3(0, 0, -1),
      { up } Vector3(0, 1, 0));
    { Better Origin default, makes things in center.
      Makes ViewSelected, ViewAll sensible in 2D }
    InternalDesignCamera.Orthographic.Origin := Vector2(0.5, 0.5);
    { Just like for run-time camera,
      we set non-zero Orthographic.Height for design-time camera.
      This is a bit larger, this way it is clear what happens when creating new 2D viewport. }
    InternalDesignCamera.Orthographic.Height := Camera.Orthographic.Height + 200;

    InternalDesignNavigationType := dn2D;
  end;

  { Darker than 0.5, which is default in CGE editor background.
    But not so dark as to look like black. }
  BackgroundColor := Vector4(0.25, 0.25, 0.25, 1);
end;

procedure TCastleViewport.SetupChildren3D;
var
  Light: TCastlePointLight;
  Plane: TCastlePlane;
  NewBackground: TCastleBackground;
begin
  FullSize := true;
  SetupDesignTimeCamera;
  Camera.Translation := Vector3(0.00, 2.00, 4.00);

  { purpose: initial light,
    bright to make PBR colors visible (e.g. yellow sphere should visibly be really yellow),
    low above ground to see the attenuation on floor.  }
  Light := TCastlePointLight.Create(Owner);
  Light.Name := ProposeComponentName(TCastlePointLight, Owner);
  Light.Translation := Vector3(4.00, 1.00, 1.00);
  Light.Intensity := 10;
  Items.Add(Light);

  { purpose: initial 3D object,
    that you can see as a whole in initial view,
    serves as floor to place new 3D stuff }
  Plane := TCastlePlane.Create(Owner);
  Plane.Name := ProposeComponentName(TCastlePlane, Owner);
  Plane.Size := Vector2(10, 10);
  Items.Add(Plane);

  if InternalDesignManipulation then
  begin
    InternalDesignCamera.Translation := Vector3(
      7.7023463249206543E+000,
      3.3161113262176514E+000,
      8.0032939910888672E+000);
    InternalDesignCamera.Rotation := Vector4(
      -2.9199448227882385E-001,
      9.4821619987487793E-001,
      1.2500144541263580E-001,
      8.6496734619140625E-001);
  end;

  NewBackground := TCastleBackground.Create(Owner);
  NewBackground.Name := ProposeComponentName(TCastleBackground, Owner);
  AddNonVisualComponent(NewBackground);
  Background := NewBackground;
end;

procedure TCastleViewport.SetSceneManager(const Value: TCastleSceneManager);
begin
  {$ifdef FPC}
  {$warnings off} // only to keep deprecated feature working
  if Value <> FSceneManager then
  begin
    if SceneManager <> nil then
      SceneManager.Viewports.Remove(Self);
    FSceneManager := Value;
    if SceneManager <> nil then
      SceneManager.Viewports.Add(Self);
  end;
  {$warnings on}
  {$endif}
end;

function TCastleViewport.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
        'Transparent', 'Camera', 'Navigation', 'Background',
        'Fog', 'BackgroundColorPersistent', 'DynamicBatching', 'Items',
        'OcclusionSort', 'OcclusionCulling', 'BlendingSort'
      ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleViewport.CustomSerialization(const SerializationProcess: TSerializationProcess);

  { If PreserveDataAcrossUndo contains a viewport from which we should copy design data
    (camera, navigation at design-time), return @true and set CopyFromViewport. }
  function CopyDesignDataFromViewport(const PreserveDataAcrossUndo: TComponent;
    out CopyFromViewport: TCastleViewport): Boolean;
  var
    CopyFrom: TComponent;
  begin
    Result := false;
    CopyFromViewport := nil;
    if (PreserveDataAcrossUndo <> nil) and
       (Name <> '') then
    begin
      CopyFrom := PreserveDataAcrossUndo.FindComponent(Name);
      if CopyFrom is TCastleViewport then // also checks CopyFrom <> nil
      begin
        CopyFromViewport := TCastleViewport(CopyFrom);
        Result := true;
      end;
    end;
  end;

var
  Nav: TInternalDesignNavigationType;
  InternalDesignNavigationTypeInt: Integer;
  CopyFromViewport: TCastleViewport;
  AutoNavigation, InternalGridAxisVar: Boolean;
begin
  inherited;

  { Note: We are not loading/saving design-time params when InternalDesignManipulation=false.

    Reason: the instances of design-time cameras/navigation are not even available.

    Consequence: This means that loading+saving a design at runtime may result in losing
    design-time information. There's no easy way around it. If we would create dummy
    instances of InternalDesignCamera just to carry this info, we would waste time/memory
    only to support a special case without a certain use-case. }

  if InternalDesignManipulation then
  begin
    if CopyDesignDataFromViewport(SerializationProcess.InternalPreserveDataAcrossUndo, CopyFromViewport) then
    begin
      { This copies the same things that are (de)serialized below when
        CopyDesignDataFromViewport returns false. }
      InternalAssignUsingSerialization(InternalDesignCamera, CopyFromViewport.InternalDesignCamera);
      InternalDesignNavigationType := CopyFromViewport.InternalDesignNavigationType;
      for Nav := Low(Nav) to High(Nav) do
        InternalAssignUsingSerialization(FInternalDesignNavigations[Nav],
          CopyFromViewport.FInternalDesignNavigations[Nav]);
      InternalGridAxis := CopyFromViewport.InternalGridAxis;
    end else
    begin
      { We want to serialize
        - camera pos,dir,up
        - projection properties, changed by TDesignFrame.CameraSynchronize
      }
      SerializationProcess.ReadWriteSubComponent('InternalDesignCamera', InternalDesignCamera, true);
      InternalGridAxisVar := InternalGridAxis;
      SerializationProcess.ReadWriteBoolean('InternalGridAxis', InternalGridAxisVar,
        InternalGridAxis <> DefaultInternalGridAxis);
      InternalGridAxis := InternalGridAxisVar;

      InternalDesignNavigationTypeInt := Ord(InternalDesignNavigationType);
      SerializationProcess.ReadWriteInteger('InternalDesignNavigationType',
        InternalDesignNavigationTypeInt,
        InternalDesignNavigationType <> DefaultInternalDesignNavigationType);
      InternalDesignNavigationType := TInternalDesignNavigationType(InternalDesignNavigationTypeInt);

      for Nav := Low(Nav) to High(Nav) do
        SerializationProcess.ReadWriteSubComponent('InternalDesignNavigations[' +
          GetEnumName(TypeInfo(TInternalDesignNavigationType), Ord(Nav)) + ']',
          FInternalDesignNavigations[Nav], true);
    end;
  end;

  AutoNavigation := false;
  SerializationProcess.ReadWriteBoolean('AutoNavigation', AutoNavigation, false);
  if AutoNavigation then
  begin
    WritelnWarning('Viewport named "%s" uses AutoNavigation, this is no longer supported. Instead: 1. (Advised) Add explicit navigation instance to viewport. 2. (Eventually, a temporary solution) Use TCastleAutoNavigationViewport.', [
      Name
    ]);
  end;
end;

procedure TCastleViewport.SetDynamicBatching(const Value: Boolean);
begin
  if FDynamicBatching <> Value then
  begin
    FDynamicBatching := Value;
    if ShapesRenderer <> nil then
      ShapesRenderer.DynamicBatching := Value;
  end;
end;

procedure TCastleViewport.SetOcclusionCulling(const Value: Boolean);
begin
  if FOcclusionCulling <> Value then
  begin
    FOcclusionCulling := Value;
    if ShapesRenderer <> nil then
      ShapesRenderer.OcclusionCulling := Value;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleviewport_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleViewportList -------------------------------------------------- }

procedure TCastleViewportList.Notify({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} Value: TCastleViewport;
  Action: TCollectionNotification);
begin
  inherited;

  { Note that SceneManager itself is also on SceneManager.Viewports list,
    depending on SceneManager.DefaultViewport value.
    Ignore it below, since we don't want to change SceneManager.Items value. }
  if Value <> SceneManager then
  begin
    if Action = cnAdded then
      Value.Items := SceneManager.Items
    else
      // Action in [cnRemoved, cnExtracted]
      Value.Items := TCastleRootTransform.Create(Value);
  end;
end;

var
  R: TRegisteredComponent;
initialization
  Input_Interact := TInputShortcut.Create(nil, 'Interact (press, open door)', 'interact', igOther);
  Input_Interact.Assign(keyNone, keyNone, '', true, buttonLeft);

  R := TRegisteredComponent.Create;
  {$warnings off} // using deprecated, to keep reading it from castle-user-interface working
  R.ComponentClass := TCastleSceneManager;
  {$warnings on}
  R.Caption := ['Scene Manager'];
  R.IsDeprecated := true;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleTouchNavigation, 'Touch Navigation');

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleViewport;
  R.Caption := ['Viewport (3D)'];
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleViewport.CreateComponentWithChildren3D;
  RegisterSerializableComponent(R);

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleViewport;
  R.Caption := ['Viewport (2D)'];
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleViewport.CreateComponentWithChildren2D;
  RegisterSerializableComponent(R);

  InitializeWarmupCache;
end.
