{
  Copyright 2009-2022 Michalis Kamburelis.

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
  CastleSceneCore, CastleCameras,
  CastleInternalGLShadowVolumes, CastleUIControls, CastleTransform, CastleTriangles,
  CastleKeysMouse, CastleBoxes, CastleInternalBackgroundRenderer, CastleUtils, CastleClassUtils,
  CastleGLShaders, CastleGLImages, CastleTimeUtils, CastleControls,
  CastleInputs, CastleRectangles, CastleColors, CastleComponentSerialize,
  CastleProjection, CastleScreenEffects;

type
  TCastleViewport = class;
  TCastleSceneManager = class;

  TRenderOnePassEvent = procedure (Viewport: TCastleViewport;
    const Params: TRenderParams) of object;

  { Event for @link(TCastleViewport.OnProjection). }
  TProjectionEvent = procedure (var Parameters: TProjection) of object;

  { Viewport displays a tree of scenes and transformations
    (TCastleTransform and TCastleScene).
    Add the scenes and transformations to @link(Items).

    For some features, that require a notion of "main" scene,
    it is useful to set @link(TCastleRootTransform.MainScene Items.MainScene).
    See the @link(TCastleRootTransform.MainScene) docs.

    Each viewport has a @link(Camera) with a position and orientation.
    The initial camera may be auto-detected if @link(AutoCamera).

    Viewport may also have a @link(Navigation) that allows to move
    camera by keyboard, mouse and other inputs.
    You can use any navigation method implemented in the engine
    (TCastleExamineNavigation, TCastleWalkNavigation, TCastleThirdPersonNavigation)
    or implement your own
    (you can create your own descendants of @link(TCastleNavigation),
    or just move/rotate the camera by calling @link(TCastleCamera.SetView) from anywhere).
    The inital navigation method may be auto-detected if @link(AutoNavigation).

    Viewport may clear its area at the beginning of rendering,
    with a solid color or using a TAbstractBackgroundNode defined in
    @link(TCastleRootTransform.MainScene Items.MainScene).
    This way you can use e.g. a skybox.
    You can control this using @link(Transparent), @link(BackgroundColor) properties.

    Viewport may also add headlight to the scene,
    see @link(TCastleRootTransform.UseHeadlight Items.UseHeadlight).

    Multiple viewports can display the same world.
    To do this, simply copy @link(Items) reference from one
    TCastleViewport to another. You can also just create your own @link(TCastleRootTransform)
    instance and then assign it to multiple viewports.
    This allows e.g. to make a split-screen game (played by 2 people,
    with 2 viewports, on a single monitor).
    Or you can show in a 3D FPS game an additional view from some security camera,
    or from a flying rocket.
    For examples of using multiple viewports see:

    @unorderedList(
      @item(Docs: https://castle-engine.io/multiple_viewports_to_display_one_world )
      @item(CGE example: examples/viewport_and_scenes/multiple_viewports/)
      @item(CGE example: examples/fps_game/)
    )
  }
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
      FRenderParams: TViewportRenderParams;
      FPrepareParams: TPrepareParams;
      FBackgroundWireframe: boolean;
      FBackgroundColor: TCastleColor;
      FUseGlobalLights, FUseGlobalFog: boolean;
      FApproximateActivation: boolean;
      FDefaultVisibilityLimit: Single;
      FTransparent, FClearDepth: boolean;
      FInternalExamineNavigation: TCastleExamineNavigation;
      FInternalWalkNavigation: TCastleWalkNavigation;
      FWithinSetNavigationType: boolean;
      LastPressEvent: TInputPressRelease;
      FOnProjection: TProjectionEvent;
      FEnableParentDragging: boolean;
      AssignDefaultCameraDone: Boolean;
      FAutoCamera: Boolean;
      FAutoNavigation: Boolean;

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
      LastVisibleStateIdForVisibleChange: TFrameId;

      FOnBoundViewpointChanged, FOnBoundNavigationInfoChanged: TNotifyEvent;
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
      // reused between frames for speed
      FRenderWithoutScreenEffectsRenderingCamera: TRenderingCamera;

    function FillsWholeContainer: boolean;
    function IsStoredNavigation: Boolean;
    procedure SetScreenSpaceAmbientOcclusion(const Value: boolean);
    procedure SetScreenSpaceReflections(const Value: Boolean);
    procedure SetScreenSpaceReflectionsSurfaceGlossiness(const Value: Single);
    procedure SSAOShaderInitialize;
    procedure SSRShaderInitialize;
    function GetNavigationType: TNavigationType;
    procedure SetAutoCamera(const Value: Boolean);
    procedure SetAutoNavigation(const Value: Boolean);
    { Make sure to call AssignDefaultCamera, if needed because of AutoCamera. }
    procedure EnsureCameraDetected;
    procedure SetItems(const Value: TCastleRootTransform);
    function GetPaused: Boolean;
    procedure SetPaused(const Value: Boolean);
    procedure SetBackground(const Value: TCastleBackground);
    procedure BackgroundFreeNotification(const Sender: TFreeNotificationObserver);

    { Callbacks when MainCamera is notified that MainScene changes camera/navigation }
    procedure MainSceneAndCamera_BoundViewpointChanged(Sender: TObject);
    procedure MainSceneAndCamera_BoundViewpointVectorsChanged(Sender: TObject);
    procedure MainSceneAndCamera_BoundNavigationInfoChanged(Sender: TObject);

    procedure SetMouseRayHit(const Value: TRayCollision);
    function MouseRayHitContains(const Item: TCastleTransform): boolean;
    procedure SetAvoidNavigationCollisions(const Value: TCastleTransform);
    procedure SetNavigationType(const Value: TNavigationType);

    { Handle navigation events by checking collisions with @link(Items).
      @groupBegin }
    function NavigationMoveAllowed(const Sender: TCastleNavigation;
      const OldPos, ProposedNewPos: TVector3; out NewPos: TVector3;
      const Radius: Single; const BecauseOfGravity: Boolean): Boolean;
    function NavigationHeight(const Sender: TCastleNavigation;
      const Position: TVector3;
      out AboveHeight: Single; out AboveGround: PTriangle): Boolean;
    { @groupEnd }

    procedure SetNavigation(const Value: TCastleNavigation);
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
  private
    var
      FNavigation: TCastleNavigation;
      FProjection: TProjection;
      FSceneManager: TCastleSceneManager;

    class procedure CreateComponentSetup2D(Sender: TObject);

    procedure RecalculateCursor(Sender: TObject);
    function ItemsBoundingBox: TBox3D;

    { Set the projection parameters and matrix.
      Used by our Render method.

      This cooperates closely with current @link(Camera) definition.

      If AutoCamera then the initial and current @link(Camera) vectors
      are also initialized here (see TCastleCamera.Init
      and @link(AssignDefaultCamera).
      If AutoNavigation then the @link(Navigation) is automatically created here,
      see @link(AssignDefaultNavigation).

      This takes care to always update Camera.ProjectionMatrix, Projection. }
    procedure ApplyProjection;

    { Render shadow quads for all the things rendered by @link(RenderOnePass).
      You can use here ShadowVolumeRenderer instance, which is guaranteed
      to be initialized with TGLShadowVolumeRenderer.InitFrustumAndLight,
      so you can do shadow volumes culling. }
    procedure RenderShadowVolume(const Params: TRenderParams);

    { Detect position/direction of the main light that produces shadows.
      Looks at MainScene.InternalMainLightForShadows.
      Returns light position (or direction, if W = 0) in world space. }
    function MainLightForShadows(out AMainLightPosition: TVector4): boolean;

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

    function GetScreenEffects(const Index: Integer): TGLSLProgram; virtual;

    function InternalExtraGetScreenEffects(const Index: Integer): TGLSLProgram; override;
    function InternalExtraScreenEffectsCount: Integer; override;
    function InternalExtraScreenEffectsNeedDepth: Boolean; override;

    { Called when PointingDevicePress was not handled by any TCastleTransform object.
      You can override this to make a message / sound signal to notify user
      that his Input_Interact click was not successful. }
    procedure PointingDevicePressFailed; virtual;

    procedure BoundNavigationInfoChanged; virtual;
    procedure BoundViewpointChanged; virtual;

    procedure RenderWithoutScreenEffects; override;
  public
    const
      DefaultScreenSpaceAmbientOcclusion = false;
      DefaultScreenSpaceReflections = False;
      DefaultScreenSpaceReflectionsSurfaceGlossiness = 0.5;
      DefaultUseGlobalLights = true;
      DefaultUseGlobalFog = true;
      DefaultShadowVolumes = true;
      DefaultBackgroundColor: TVector4 = (X: 0.1; Y: 0.1; Z: 0.1; W: 1);
      Default2DProjectionFar = 1000.0;
      Default2DCameraZ = Default2DProjectionFar / 2;
      DefaultPrepareOptions = [prRenderSelf, prRenderClones, prBackground, prBoundingBox, prScreenEffects];

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
        set to false in PointingDeviceRelease. }
      InternalPointingDeviceDragging: Boolean;

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

    function GetMainScene: TCastleScene; deprecated 'use Items.MainScene';

    { Notification done by TCastleCamera when our Camera state changed.
      @exclude }
    procedure InternalCameraChanged;

    {$ifdef FPC}
    { Current projection parameters,
      calculated by last @link(CalculateProjection) call,
      adjusted by @link(OnProjection).
      @bold(This is read only). To change the projection parameters,
      override @link(CalculateProjection) or handle event @link(OnProjection). }
    property Projection: TProjection read FProjection;
      deprecated 'in most cases, you can instead read Camera parameters, like Camera.Orthographic.EffectiveWidth, Camera.Orthographic.EffectiveHeight';
    {$endif}

    { Return current navigation. Automatically creates it if missing. }
    function RequiredNavigation: TCastleNavigation; deprecated 'use Camera to set camera properties; if you require Navigation to be <> nil, just create own instance of TCastleWalkNavigation/TCastleExamineNavigation and assign it, or call AssignDefaultNavigation';
    function RequiredCamera: TCastleNavigation; deprecated 'use Camera to set camera properties; if you require Navigation to be <> nil, just create own instance of TCastleWalkNavigation/TCastleExamineNavigation and assign it, or call AssignDefaultNavigation';

    { Return the currently used camera as TCastleWalkNavigation, making sure that current
      NavigationType is something using TCastleWalkNavigation.

      @unorderedList(
        @item(
          When SwitchNavigationTypeIfNeeded is @true (the default),
          this method makes sure that the @link(NavigationType) corresponds to a type
          handled by TCastleWalkNavigation, creating and adjusting the camera if necessary.

          If the current NavigationType does not use TCastleWalkNavigation
          (see @link(TNavigationType) documentation for information which
          navigation types use which TCastleNavigation descendants),
          then it's switched to ntWalk.
        )

        @item(
          When SwitchNavigationTypeIfNeeded is @false,
          then we return @nil if the current camera is not already
          a TCastleWalkNavigation instance.

          We @italic(never) create a new camera in this case
          (even if the NavigatinInfo node in MainScene would indicate
          that the new camera would be a TCastleWalkNavigation).
        )
      )
    }
    function WalkNavigation(const SwitchNavigationTypeIfNeeded: boolean = true): TCastleWalkNavigation;
      deprecated 'create own instance of TCastleWalkNavigation, and assign it to Viewport.Navigation, this is more flexible and predictable';
    function WalkCamera(const SwitchNavigationTypeIfNeeded: boolean = true): TCastleWalkNavigation;
      deprecated 'create own instance of TCastleWalkNavigation, and assign it to Viewport.Navigation, this is more flexible and predictable';

    { Return the currently used camera as TCastleExamineNavigation, making sure that current
      NavigationType is something using TCastleExamineNavigation.

      @unorderedList(
        @item(
          When SwitchNavigationTypeIfNeeded is @true (the default),
          this method makes sure that the @link(NavigationType) corresponds to a type
          handled by TCastleExamineNavigation, creating and adjusting the camera if necessary.

          If the current NavigationType does not use TCastleExamineNavigation
          (see @link(TNavigationType) documentation for information which
          navigation types use which TCastleNavigation descendants),
          then it's switched to ntExamine.
        )

        @item(
          When SwitchNavigationTypeIfNeeded is @false,
          then we return @nil if the current camera is not already
          a TCastleExamineNavigation instance.

          We @italic(never) create a new camera in this case
          (even if the NavigatinInfo node in MainScene would indicate
          that the new camera would be a TCastleExamineNavigation).
        )
      )
    }
    function ExamineNavigation(const SwitchNavigationTypeIfNeeded: boolean = true): TCastleExamineNavigation;
      deprecated 'create own instance of TCastleExamineNavigation, and assign it to Viewport.Navigation, this is more flexible and predictable';
    function ExamineCamera(const SwitchNavigationTypeIfNeeded: boolean = true): TCastleExamineNavigation;
      deprecated 'create own instance of TCastleExamineNavigation, and assign it to Viewport.Navigation, this is more flexible and predictable';

    { Make @link(Navigation) @nil.
      The actual creation may be caused by calling
      @link(ExamineCamera), @link(WalkCamera),
      @link(InternalExamineCamera), @link(InternalWalkCamera),
      or by setting @link(NavigationType).

      In all cases, these methods will create a new camera instance
      after a @name call. No previous cached camera instance will be used. }
    procedure ClearCameras; deprecated 'just set Navigation to nil instead of using this method; to avoid reusing previous instance, do not use WalkNavigation/ExamineNavigation methods, instead create and destroy your own TCastleWalkNavigation/TCastleExamineNavigation whenever you want';

    { Camera instances used by this viewport.
      Using these methods automatically creates these instances
      (so they are never @nil).

      Using these methods @italic(does not) make these
      camera instances current (in contast to calling @link(ExamineCamera),
      @link(WalkCamera) or setting @link(NavigationType)).

      When you switch navigation types by calling @link(ExamineCamera),
      @link(WalkCamera) or setting @link(NavigationType)
      the viewport keeps using these instances of cameras,
      instead of creating new camera instances.
      This way all the camera properties
      (not only those copied by TCastleNavigation.Assign) are preserved when you switch
      e.g. NavigationType from ntWalk to ntExamine to ntWalk again.

      @deprecated This is deprecated now, because it causes auto-detection
      of navigation parameters, which is (in general) more surprising than helpful.
      E.g. it adjusts camera radius, speed and more properties.

      @groupBegin }
    function InternalExamineNavigation: TCastleExamineNavigation;
      deprecated 'create own instance of TCastleExamineNavigation instead of using this one, it results in more obvious code';
    function InternalWalkNavigation: TCastleWalkNavigation;
      deprecated 'create own instance of TCastleWalkNavigation instead of using this one, it results in more obvious code';
    function InternalExamineCamera: TCastleExamineNavigation; deprecated 'use InternalExamineNavigation';
    function InternalWalkCamera: TCastleWalkNavigation; deprecated 'use InternalWalkNavigation';
    { @groupEnd }

    { Assign @link(Navigation) to a default TCastleNavigation suitable
      for navigating in this scene.

      This is automatically used when @link(Navigation) is @nil
      and @link(AutoNavigation).
      You can also use it explicitly.

      The implementation in base TCastleViewport uses
      @link(TCastleSceneCore.NavigationTypeFromNavigationInfo MainScene.NavigationTypeFromNavigationInfo)
      and
      @link(TCastleSceneCore.InternalUpdateNavigation MainScene.InternalUpdateNavigation),
      thus it follows your X3D scene NavigationInfo.
      If MainScene is not assigned, we create a simple
      navigation in Examine mode. }
    procedure AssignDefaultNavigation; virtual;

    { Assign initial and current camera vectors and projection.

      This is automatically used at first rendering if @link(AutoCamera).
      You can also use it explicitly. }
    procedure AssignDefaultCamera; virtual;

    { Screen effects are shaders that post-process the rendered screen.
      If any screen effects are active, we will automatically render
      screen to a temporary texture, processing it with
      each shader.

      By default, screen effects come from MainScene.ScreenEffects,
      so the effects may be defined by VRML/X3D author using ScreenEffect
      nodes (see docs: [https://castle-engine.io/x3d_extensions_screen_effects.php]).
      Descendants may override GetScreenEffects, ScreenEffectsCount,
      and ScreenEffectsNeedDepth to add screen effects by code.
      Each viewport may have it's own, different screen effects.

      @groupBegin }
    property ScreenEffects [const Index: Integer]: TGLSLProgram read GetScreenEffects;
    function ScreenEffectsCount: Integer; virtual;
    function ScreenEffectsNeedDepth: boolean; virtual;
    { @groupEnd }

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
    function PrepareParams: TPrepareParams;

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

    { Set @link(Navigation) and some of its' parameters
      (like TCastleWalkNavigation.Gravity and so on).

      If @link(AutoNavigation), the initial @link(Navigation)
      as well as initial value of this property are automatically determined
      by the currently bound X3D NavigatinInfo node in the @link(TCastleRootTransform.MainScene MainScene),
      and world bounding box.
      They are also automatically adjusted e.g. when current NavigatinInfo
      node changes.

      But you can set @link(Navigation), or this property,
      manually to override the detected navigation.
      You should set @link(AutoNavigation) to @false to take control
      of @link(Navigation) and this property completely (no auto-detection
      based on @link(TCastleRootTransform.MainScene MainScene) will then take place).

      Note that you can also affect the current NavigationType by directly
      changing the camera properties,
      e.g. you can directly change @link(TCastleWalkNavigation.Gravity) from @false to @true,
      and thus you effectively switch from ntFly to ntWalk navigation types.
      When you read the NavigationType property, we determine the current navigation
      type from current camera properties.

      Setting this sets:
      @unorderedList(
        @itemSpacing compact
        @item @link(TCastleNavigation.Input)
        @item @link(TCastleExamineNavigation.Turntable), only in case of @link(TCastleExamineNavigation)
        @item @link(TCastleWalkNavigation.Gravity), only in case of @link(TCastleWalkNavigation)
        @item @link(TCastleWalkNavigation.PreferGravityUpForRotations), only in case of @link(TCastleWalkNavigation)
        @item @link(TCastleWalkNavigation.PreferGravityUpForMoving), only in case of @link(TCastleWalkNavigation)
      )

      If you write to NavigationType, then you @italic(should not) touch the
      above properties directly. That's because not every combination of
      above properties correspond to some sensible value of NavigationType.
      If you directly set some weird configuration, reading NavigationType will
      try it's best to determine the closest TNavigationType value
      that is similar to your configuration. }
    property NavigationType: TNavigationType
      read GetNavigationType write SetNavigationType
      default ntNone; {$ifdef FPC}deprecated 'create own instances of TCastleNavigation descendants to change the navigation';{$endif}

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

      The interpretation of Position depends on ContainerCoordinates,
      and is similar to e.g. @link(TCastleTiledMapControl.PositionToTile):

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
      by colliding camera ray with a plane at constant Z.
      "World coordinates" are coordinates
      space seen by TCastleTransform / TCastleScene inside viewport @link(Items).

      This is a more general version of @link(PositionTo2DWorld),
      that works with any projection (perspective or orthographic).
      This is often useful if your game is "close to 2D", which means that you
      use 3D (and maybe even perspective camera),
      but most of the game world is placed around some plane with constant Z.

      The interpretation of Position depends on ContainerCoordinates,
      and is similar to e.g. @link(TCastleTiledMapControl.PositionToTile):

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

      This intersects the ray cast by @link(Camera)
      with a plane at Z = PlaneZ.

      Returns true and sets 3D PlanePosition (the Z component of this vector
      must always be equal to PlaneZ) if such intersection is found.
      Returns false if it's not possible to determine such point (when
      the camera looks in the other direction).
    }
    function PositionToWorldPlane(const Position: TVector2;
      const ContainerCoordinates: Boolean;
      const PlaneZ: Single; out PlanePosition: TVector3): Boolean;

    { Convert 2D position into "world coordinates", which is the coordinate
      space seen by TCastleTransform / TCastleScene inside viewport @link(Items),
      assuming that we use orthographic projection in XY axes.

      The interpretation of Position depends on ContainerCoordinates,
      and is similar to e.g. @link(TCastleTiledMapControl.PositionToTile):

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

    { Prepare resources, to make various methods (like @link(Render)) execute fast.
      Call it only when rendering context is initialized (ApplicationProperties.IsGLContextOpen).
      If DisplayProgressTitle <> '', we will display progress bar during loading. }
    procedure PrepareResources(const DisplayProgressTitle: string = '';
      const Options: TPrepareResourcesOptions = DefaultPrepareOptions); overload;
    procedure PrepareResources(const Item: TCastleTransform;
      const DisplayProgressTitle: string = '';
      Options: TPrepareResourcesOptions = DefaultPrepareOptions); overload; virtual;

    { Current object (TCastleTransform hierarchy) under the mouse cursor.
      Updated in every mouse move. May be @nil.

      The returned list (if not @nil) contains TCastleTransform instances
      that collided with the ray (from the deepest instance in the @link(Items) tree
      to the root), along with some additional information.
      See TRayCollision for details. }
    property MouseRayHit: TRayCollision read FMouseRayHit;

    { Current object (TCastleTransform instance) under the mouse cursor.
      Updated in every mouse move. May be @nil. }
    function TransformUnderMouse: TCastleTransform;

    { Do not collide with this object when moving by @link(Navigation).
      It makes sense to put here player avatar (in 3rd person view)
      or player collision volume (in 1st person view)
      to allow player to move, not colliding with its own body.

      In case of using @link(TLevel), this is automatically
      set when you set @link(TLevel.Player). }
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
  published
    { Transformations and scenes visible in this viewport.
      You should add here your @link(TCastleTransform) and @link(TCastleScene)
      instances.

      It is by default created (not nil), but you can also assign here your own
      TCastleRootTransform instance.
      You can also copy a TCastleRootTransform from one TCastleViewport to another,
      that is multiple TCastleViewport can refer to the same TCastleRootTransform
      instance. }
    property Items: TCastleRootTransform read FItems write SetItems;

    { Camera determines the viewer position and orientation.
      The given camera instance is always available and connected with this viewport. }
    property Camera: TCastleCamera read FCamera;

    { Navigation method is an optional component that handles
      the user input to control the camera.

      You can assign here an instance of @link(TCastleNavigation),
      like @link(TCastleWalkNavigation) or @link(TCastleExamineNavigation).
      Or you can leave it as @nil.

      Note that, if you leave it as @nil and have @link(AutoNavigation) as @true
      (default) then a default navigation will be calculated
      right before the first rendering. It will take into account the 3D world
      initialized in Viewport.Items, e.g. the NavigatinInfo inside Viewport.Items.MainScene.
      Set @link(AutoNavigation) to false to avoid this automatic detection.

      Note that assigning @link(NavigationType) also implicitly sets
      this property to an internal instance of
      @link(TCastleWalkNavigation) or @link(TCastleExamineNavigation).
      Setting @link(NavigationType) to @nil sets this property to @nil.

      @seealso OnCameraChanged }
    property Navigation: TCastleNavigation read FNavigation write SetNavigation
      stored IsStoredNavigation;

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
      Displayed only when not @link(Transparent). }
    property Background: TCastleBackground read FBackground write SetBackground;

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
    { Visibility limit of your 3D world. This is the distance the far projection
      clipping plane.

      The default @link(CalculateProjection) implementation
      calculates the final visibility limit as follows:

      @unorderedList(
        @item(First of all, if (GLFeatures.ShadowVolumesPossible and ShadowVolumes),
          then it's infinity.)
        @item(Then we look NavigationInfo.visibilityLimit value inside MainScene.
          This allows your 3D data creators to set this inside VRML/X3D data.

          Only if MainScene is not set, or doesn't contain NavigationInfo node,
          or NavigationInfo.visibilityLimit is left at (default) zero,
          we look further.)
        @item(We use this property, DefaultVisibilityLimit, if it's not zero.)
        @item(Finally, as a last resort we calculate something suitable looking
          at the 3D bounding box of items inside our 3D world.)
      )
    }
    property DefaultVisibilityLimit: Single
      read FDefaultVisibilityLimit write FDefaultVisibilityLimit default 0.0;
      deprecated 'use Camera.ProjectionFar, and set AutoCamera to false';

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

    { Assign sensible @link(Navigation) looking
      at the initial world (@link(Items)) if it is not assigned.

      This also allows to later synchronize navigation properties when X3D NavigationInfo
      node changes, or a new NavigationInfo node is bound.

      By default it is @false, which means that you control @link(Navigation) on your own.
    }
    property AutoNavigation: Boolean
      read FAutoNavigation write SetAutoNavigation default false; {$ifdef FPC}deprecated 'unless you implement an X3D browser, it is better to configure Navigation explicitly';{$endif}

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

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleviewport_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

  TCastleViewportList = class({$ifdef FPC}specialize{$endif} TObjectList<TCastleViewport>)
  private
    SceneManager: TCastleSceneManager;
  protected
    procedure Notify({$ifdef FPC}constref{$else}const{$endif} Value: TCastleViewport;
      Action: TCollectionNotification); override;
  end deprecated 'internal for TCastleSceneManager';

  { Deprecated way to manage transformatiosn and scenes.
    To upgrade:
    - use @link(TCastleViewport)
    - set FullSize, AutoCamera and AutoNavigation to true. }
  TCastleSceneManager = class(TCastleViewport)
  strict private
    FDefaultViewport: boolean;
    {$warnings off} // using deprecated in deprecated
    FViewports: TCastleViewportList;
    {$warnings on}

    function GetMoveLimit: TBox3D;
    procedure SetMoveLimit(const Value: TBox3D);
    function GetTimeScale: Single;
    procedure SetTimeScale(const Value: Single);
    procedure SetDefaultViewport(const Value: boolean);
    function GetMainCamera: TCastleCamera;
    procedure SetMainCamera(const Value: TCastleCamera);
    function GetMainSceneInternal: TCastleScene;
    procedure SetMainScene(const Value: TCastleScene);
    function GetUseHeadlight: TUseHeadlight;
    procedure SetUseHeadlight(const Value: TUseHeadlight);
    function GetHeadlightNode: TAbstractLightNode;
    procedure SetHeadlightNode(const Value: TAbstractLightNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Render; override;

    {$ifdef FPC}
    { Limit the movement allowed by @link(TCastleAbstractRootTransform.WorldMoveAllowed).
      Ignored when empty (default).
      @seealso TCastleAbstractRootTransform.MoveLimit }
    property MoveLimit: TBox3D read GetMoveLimit write SetMoveLimit;
      deprecated 'use Items.MoveLimit';
    {$endif}

    { List of viewports connected to this scene manager.
      This contains all TCastleViewport instances that have
      TCastleViewport.SceneManager set to us. Also it contains Self
      (this very scene manager) if and only if DefaultViewport = @true
      (because when DefaultViewport, scene manager acts as an
      additional viewport too).

      This list is read-only from the outside! It's automatically managed
      in this unit (when you change TCastleViewport.SceneManager
      or TCastleSceneManager.DefaultViewport, we automatically update this list
      as appropriate). }
    property Viewports: TCastleViewportList read FViewports;

    { Up vector, according to gravity. Gravity force pulls in -GravityUp direction. }
    function GravityUp: TVector3; deprecated 'use Camera.GravityUp';

    {$ifdef FPC}
    { See @link(TCastleRootTransform.HeadlightNode). }
    property HeadlightNode: TAbstractLightNode
      read GetHeadlightNode write SetHeadlightNode;
      deprecated 'use Items.HeadlightNode';

    { See @link(TCastleAbstractRootTransform.MainCamera). }
    property MainCamera: TCastleCamera read GetMainCamera write SetMainCamera;
      deprecated 'use Items.MainCamera';

    { See @link(TCastleAbstractRootTransform.PhysicsProperties). }
    function PhysicsProperties: TPhysicsProperties;
      deprecated 'use Items.PhysicsProperties';

    { See @link(TCastleAbstractRootTransform.TimeScale). }
    property TimeScale: Single read GetTimeScale write SetTimeScale default 1;
      deprecated 'use Items.TimeScale';

    { See @link(TCastleRootTransform.MainScene). }
    property MainScene: TCastleScene read GetMainSceneInternal write SetMainScene;
      deprecated 'use Items.MainScene';

    { See @link(TCastleRootTransform.UseHeadlight). }
    property UseHeadlight: TUseHeadlight
      read GetUseHeadlight write SetUseHeadlight default hlMainScene;
      deprecated 'use Items.UseHeadlight';
    {$endif}
  published
    { Should we render the 3D world in a default viewport that covers
      the whole window. This is usually what you want. For more complicated
      uses, you can turn this off, and use explicit TCastleViewport
      (connected to this scene manager by TCastleViewport.SceneManager property)
      for making your world visible. }
    property DefaultViewport: boolean
      read FDefaultViewport write SetDefaultViewport default true;

    property FullSize default true;
    property AutoCamera default true;
    property AutoNavigation default true;
  end {$ifdef FPC}deprecated 'use TCastleViewport to render scenes. To have the same initial behavior, set FullSize, AutoCamera and AutoNavigation to true'{$endif};

var
  { Key/mouse combination to interact with clickable things in 3D world.
    More precisely, this input will activate pointing device sensors in VRML/X3D,
    which are used to touch (click) or drag 3D things.
    By default this is left mouse button click.

    You can change it to any other mouse button or even to key combination.
    Simply change properties like TInputShortcut.Key1
    or TInputShortcut.MouseButtonUse. }
  Input_Interact: TInputShortcut;

{$define read_interface}
{$I castleviewport_touchnavigation.inc}
{$I castleviewport_serialize.inc}
{$undef read_interface}

implementation

uses DOM, Math,
  CastleGLUtils, CastleProgress, CastleLog, CastleStringUtils,
  CastleSoundEngine, CastleGLVersion, CastleShapes, CastleTextureImages,
  CastleInternalSettings, CastleXMLUtils, CastleURIUtils,
  CastleRenderContext, CastleApplicationProperties;

{$define read_implementation}
{$I castleviewport_touchnavigation.inc}
{$I castleviewport_warmup_cache.inc}
{$I castleviewport_serialize.inc}
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

  WritelnLog('setting near to %f', [Viewport.ProjectionNear]); // testing
  WritelnLog('setting far to %f', [Viewport.ProjectionFarFinite]); // testing
  Uniform('near').SetValue(Viewport.ProjectionNear);
  Uniform('far').SetValue(Viewport.ProjectionFarFinite);
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

  Uniform('near').SetValue(0.1);
  Uniform('far').SetValue(1000.0);
  ViewProjectionMatrix := Viewport.Camera.ProjectionMatrix * Viewport.Camera.Matrix;
  if not ViewProjectionMatrix.TryInverse(ViewProjectionMatrixInverse) then
    ViewProjectionMatrixInverse := TMatrix4.Identity;
  Uniform('defaultSurfaceGlossiness').SetValue(Viewport.ScreenSpaceReflectionsSurfaceGlossiness);
  Uniform('castle_ViewProjectionMatrix').SetValue(ViewProjectionMatrix);
  Uniform('castle_ViewProjectionMatrixInverse').SetValue(ViewProjectionMatrixInverse);
  Uniform('castle_CameraPosition').SetValue(Viewport.Camera.Position);
end;

{ TCastleViewport ------------------------------------------------------- }

constructor TCastleViewport.Create(AOwner: TComponent);
begin
  inherited;
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

  FCamera := TCastleCamera.Create(Self);
  FCamera.InternalViewport := Self;
  FCamera.SetSubComponent(true);
  FCamera.Name := 'Camera';
  FCamera.InternalOnSceneBoundViewpointChanged := {$ifdef FPC}@{$endif}MainSceneAndCamera_BoundViewpointChanged;
  FCamera.InternalOnSceneBoundViewpointVectorsChanged := {$ifdef FPC}@{$endif}MainSceneAndCamera_BoundViewpointVectorsChanged;
  FCamera.InternalOnSceneBoundNavigationInfoChanged := {$ifdef FPC}@{$endif}MainSceneAndCamera_BoundNavigationInfoChanged;

  FItems := TCastleRootTransform.Create(Self);
  FItems.SetSubComponent(true);
  FItems.Name := 'Items';
  FItems.OnCursorChange := {$ifdef FPC}@{$endif}RecalculateCursor;
  FItems.MainCamera := Camera;

  FCapturePointingDeviceObserver := TFreeNotificationObserver.Create(Self);
  FCapturePointingDeviceObserver.OnFreeNotification := {$ifdef FPC}@{$endif}CapturePointingDeviceFreeNotification;

  FBackgroundObserver := TFreeNotificationObserver.Create(Self);
  FBackgroundObserver.OnFreeNotification := {$ifdef FPC}@{$endif}BackgroundFreeNotification;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleviewport_persistent_vectors.inc}
  {$undef read_implementation_constructor}
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

  { unregister self from Navigation callbacs, etc.

    This includes setting FNavigation to nil.
    Yes, this setting FNavigation to nil is needed, it's not just paranoia.

    Consider e.g. when our Navigation is owned by Self.
    This means that this navigation will be freed in "inherited" destructor call
    below. Since we just did FNavigation.RemoveFreeNotification, we would have
    no way to set FNavigation to nil, and FNavigation would then remain as invalid
    pointer.

    And when Viewport is freed it sends a free notification
    (this is also done in "inherited" destructor) to TCastleWindow instance,
    which causes removing us from TCastleWindow.Controls list,
    which causes SetContainer(nil) call that tries to access Navigation.

    This scenario would cause segfault, as FNavigation pointer is invalid
    at this time. }
  Navigation := nil;

  FreeAndNil(FRenderParams);
  FreeAndNil(FPrepareParams);
  FreeAndNil(FRenderWithoutScreenEffectsRenderingCamera);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleviewport_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
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

function TCastleViewport.FillsWholeContainer: boolean;
begin
  if Container = nil then
    Result := FullSize
  else
    Result := RenderRect.Round.Equals(Container.Rect);
end;

procedure TCastleViewport.SetNavigation(const Value: TCastleNavigation);
begin
  { Viewport will handle passing events to their camera,
    and will also pass our own Container to Camera.Container.
    This is desired, this way events are correctly passed
    and interpreted before passing them to 3D objects.
    And this way we avoid the question whether camera should be before
    or after the viewport on the Controls list (as there's really
    no perfect ordering for them).

    Note that one Navigation instance can be assigned only to one TCastleViewport.
    Not only the viewport "takes over" some navigation events,
    and sets Navigation.Viewport,
    but also Navigation (as any other TCastleUserInterface) cannot be present
    multiple times in UI hierarchy. }

  if FNavigation <> Value then
  begin
    { Check csDestroying, as this may be called from Notification,
      which may be called by navigation destructor *after* TCastleNavigation
      after freed it's fields. }
    if (FNavigation <> nil) and not (csDestroying in FNavigation.ComponentState) then
    begin
      FNavigation.OnInternalMoveAllowed := nil;
      FNavigation.OnInternalHeight := nil;
      FNavigation.RemoveFreeNotification(Self);
      { For easy backward-compatibility, leave Viewport assigned on
        FInternalWalkNavigation and FInternalExamineNavigation. }
      if (FNavigation <> FInternalWalkNavigation) and
         (FNavigation <> FInternalExamineNavigation) then
        { Note that InternalViewport has no setter to, in turn, set Viewport.Navigation.
          This would cause a recursive call from which we should protect.
          But InternalViewport is a trivial internal field now, so no need to protect,
          user code should not touch it. }
        FNavigation.InternalViewport := nil;
      RemoveControl(FNavigation);
    end;

    FNavigation := Value;

    if FNavigation <> nil then
    begin
      FNavigation.OnInternalMoveAllowed := {$ifdef FPC}@{$endif}NavigationMoveAllowed;
      FNavigation.OnInternalHeight := {$ifdef FPC}@{$endif}NavigationHeight;
      FNavigation.FreeNotification(Self);
      FNavigation.InternalViewport := Self;
      { Check IndexOfControl first, in case the FNavigation is already part
        of our controls. This happens when deserializing: "Navigation" field
        points to an instance that is within our GetChildren. }
      if IndexOfControl(FNavigation) = -1 then
        InsertControl(0, FNavigation);
    end;

    { Call OnBoundNavigationInfoChanged when Navigation instance changed.
      This allows code that observes NavigationType to work. }
    BoundNavigationInfoChanged;
  end;
end;

procedure TCastleViewport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FNavigation then
    begin
      { set to nil by SetNavigation, to clean nicely }
      Navigation := nil;
    end;
    // Note that we don't register on FInternalExamine/WalkNavigation destruction
    // when they are not current, so they should never be freed in that case.
    if AComponent = FInternalWalkNavigation then
      FInternalWalkNavigation := nil;
    if AComponent = FInternalExamineNavigation then
      FInternalExamineNavigation := nil;

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
        Exit(ExclusiveEvents);

  if Input_Interact.IsEvent(Event) and
     PointingDevicePress then
  begin
    InternalPointingDeviceDragging := true;
    Exit(ExclusiveEvents);
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
        Exit(ExclusiveEvents);

  if Input_Interact.IsEvent(Event) then
  begin
    InternalPointingDeviceDragging := false;
    if PointingDeviceRelease then
      Exit(ExclusiveEvents);
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

    UpdateMouseRayHit;

    { Note: we ignore PointingDeviceMove result.
      Maybe we should use PointingDeviceMove result as our Motion result?
      Answer unknown. Historically we do not do this, and I found no practical
      use-case when it would be useful to do this. }
    PointingDeviceMove;
  end;

  { update the cursor, since 3D object under the cursor possibly changed.

    Accidentally, this also workarounds the problem of TCastleViewport:
    when the 3D object stayed the same but it's Cursor value changed,
    Items.CursorChange notify only TCastleSceneManager (not custom viewport).
    But thanks to doing RecalculateCursor below, this isn't
    a problem for now, as we'll update cursor anyway, as long as it changes
    only during mouse move.

    TODO: this comment is probably no longer valid, TCastleViewport
    is now used for everything. }
  RecalculateCursor(Self);
end;

procedure TCastleViewport.UpdateMouseRayHit;
var
  MousePosition: TVector2;
begin
  if GetMousePosition(MousePosition) then
  begin
    PositionToRay(MousePosition, true, MouseRayOrigin, MouseRayDirection);

    { Update MouseRayHit.
      We know that MouseRayDirection is normalized now, which is important
      to get correct MouseRayHit.Distance. }
    SetMouseRayHit(CameraRayCollision(MouseRayOrigin, MouseRayDirection));
  end;
end;

function TCastleViewport.TransformUnderMouse: TCastleTransform;
begin
  if (MouseRayHit <> nil) and
     (MouseRayHit.Count <> 0) then
    Result := MouseRayHit.First.Item
  else
    Result := nil;
end;

procedure TCastleViewport.RecalculateCursor(Sender: TObject);
var
  T: TCastleTransform;
begin
  if { This may be called from TCastleViewport without SceneManager assigned. }
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

  { We show mouse cursor from top-most 3D object.
    This is sensible, if multiple 3D scenes obscure each other at the same
    pixel --- the one "on the top" (visible by the player at that pixel)
    determines the mouse cursor.

    We ignore Cursor value of other 3d stuff along
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
        parameter, as it would not be controllable for them: 3D objects do not
        have strict front-to-back order, so we would not know in what order
        call their Update methods, so we have to let many Items handle keys anyway.
        So, it's consistent to just treat 3D objects as "cannot definitely
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
    if CastleDesignMode then
    begin
      if FLastSeenMainScene <> Items.MainScene then
      begin
        FLastSeenMainScene := Items.MainScene;
        AssignDefaultCameraDone := false;
      end;
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

  { Note that TCastleCamera.Update doesn't process any input
    (only TCastleNavigation processes inputs),
    so passing HandleInput there is not necessary. }
  Camera.Update(SecondsPassedScaled);

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
  if AutoCamera and not AssignDefaultCameraDone then
    AssignDefaultCamera;
  { Set AssignDefaultCameraDone to done,
    regardless if AssignDefaultCameraDone was done or not.
    Otherwise later setting AutoCamera to true would suddenly
    reinitialize camera (initial and current vectors) in the middle of game. }
  AssignDefaultCameraDone := true;
end;

procedure TCastleViewport.ApplyProjection;
var
  Viewport: TRectangle;
  AspectRatio: Single;
  M: TMatrix4;
begin
  {$warnings off} // using deprecated to keep it working
  if AutoNavigation and (Navigation = nil) then
    AssignDefaultNavigation; // create Navigation if necessary
  {$warnings on}

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
  Camera.ProjectionMatrix := M;
  RenderContext.ProjectionMatrix := M;
end;

function TCastleViewport.ItemsBoundingBox: TBox3D;
begin
  if Items <> nil then
    Result := Items.BoundingBox
  else
    Result := TBox3D.Empty;
end;

function TCastleViewport.IsStoredNavigation: Boolean;
begin
  { Do not store Navigation when it is an internal instance
    (set by AutoNavigation being true at some point).
    This is consistent with what editor shows: internal instances
    have csTransient, so they are not shown. }
  Result := (Navigation <> nil) and
    (not (csTransient in Navigation.ComponentStyle));
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

    if CastleDesignMode and Value then
      AssignDefaultCameraDone := false;
  end;
end;

procedure TCastleViewport.SetAutoNavigation(const Value: Boolean);
begin
  if FAutoNavigation <> Value then
  begin
    FAutoNavigation := Value;
    { Not necessary, and we actually don't have AssignDefaultNavigationDone.
      The navigation will be auto-assigned when it is nil. }
    // if CastleDesignMode and Value then
    //   AssignDefaultNavigationDone := false;
  end;
end;

function TCastleViewport.CalculateProjection: TProjection;
var
  Box: TBox3D;
  Viewport: TFloatRectangle;

  { Update Result.Dimensions and Camera.Orthographic.EffectiveXxx
    based on Camera.Orthographic.Width, Height and current control size. }
  procedure UpdateOrthographicDimensions;
  var
    ControlWidth, ControlHeight, EffectiveProjectionWidth, EffectiveProjectionHeight: Single;

    procedure CalculateDimensions;
    begin
      { Apply Camera.Orthographic.Scale here,
        this way it scales around Origin (e.g. around middle, when Origin is 0.5,0.5) }
      EffectiveProjectionWidth  := EffectiveProjectionWidth * Camera.Orthographic.Scale;
      EffectiveProjectionHeight := EffectiveProjectionHeight * Camera.Orthographic.Scale;

      Result.Dimensions.Width  := EffectiveProjectionWidth;
      Result.Dimensions.Height := EffectiveProjectionHeight;
      Result.Dimensions.Left   := - Camera.Orthographic.Origin.X * EffectiveProjectionWidth;
      Result.Dimensions.Bottom := - Camera.Orthographic.Origin.Y * EffectiveProjectionHeight;
    end;

  begin
    ControlWidth := EffectiveWidthForChildren;
    ControlHeight := EffectiveHeightForChildren;

    if (Camera.Orthographic.Width = 0) and
       (Camera.Orthographic.Height = 0) then
    begin
      EffectiveProjectionWidth := ControlWidth;
      EffectiveProjectionHeight := ControlHeight;
      CalculateDimensions;
    end else
    if Camera.Orthographic.Width = 0 then
    begin
      EffectiveProjectionWidth := Camera.Orthographic.Height * ControlWidth / ControlHeight;
      EffectiveProjectionHeight := Camera.Orthographic.Height;
      CalculateDimensions;
    end else
    if Camera.Orthographic.Height = 0 then
    begin
      EffectiveProjectionWidth := Camera.Orthographic.Width;
      EffectiveProjectionHeight := Camera.Orthographic.Width * ControlHeight / ControlWidth;
      CalculateDimensions;
    end else
    begin
      EffectiveProjectionWidth := Camera.Orthographic.Width;
      EffectiveProjectionHeight := Camera.Orthographic.Height;

      CalculateDimensions;

      if not Camera.Orthographic.Stretch then
        Result.Dimensions := TOrthoViewpointNode.InternalFieldOfView(
          Result.Dimensions,
          Viewport.Width,
          Viewport.Height);

      EffectiveProjectionWidth := Result.Dimensions.Width;
      EffectiveProjectionHeight := Result.Dimensions.Height;
    end;

    Assert(Result.Dimensions.Width  = EffectiveProjectionWidth);
    Assert(Result.Dimensions.Height = EffectiveProjectionHeight);

    Camera.Orthographic.InternalSetEffectiveSize(
      Result.Dimensions.Width,
      Result.Dimensions.Height);
  end;

  { Calculate reasonable perspective projection near, looking at Box. }
  function GetDefaultProjectionNear: Single;
  var
    Radius: Single;
  begin
    Radius := Box.AverageSize(false, 1) * WorldBoxSizeToRadius;
    Result := Radius * RadiusToProjectionNear;
  end;

  { Calculate reasonable perspective projection far, looking at Box. }
  function GetDefaultProjectionFar: Single;
  begin
    { Note that when box is empty (or has 0 sizes),
      ProjectionFar cannot be simply "any dummy value".
      It must be appropriately larger than GetDefaultProjectionNear
      to provide sufficient space for rendering Background node. }
    Result := Box.AverageSize(false, 1) * WorldBoxSizeToProjectionFar;
  end;

begin
  Box := ItemsBoundingBox;
  Viewport := RenderRect;

  Result.ProjectionType := Camera.ProjectionType;

  Result.PerspectiveAnglesRad := TViewpointNode.InternalFieldOfView(
    Camera.Perspective.FieldOfView,
    Camera.Perspective.FieldOfViewAxis,
    Viewport.Width,
    Viewport.Height);

  { calculate Result.ProjectionNear }
  Result.ProjectionNear := Camera.ProjectionNear;
  if (Result.ProjectionType = ptPerspective) and
     (Result.ProjectionNear <= 0) then
  begin
    Result.ProjectionNear := GetDefaultProjectionNear;
    Assert(Result.ProjectionNear > 0);
  end;

  { calculate Result.ProjectionFar, algorithm documented at DefaultVisibilityLimit }
  Result.ProjectionFar := Camera.ProjectionFar;
  {$ifdef FPC}
  {$warnings off} // using deprecated to keep it working
  if Result.ProjectionFar <= 0 then
    Result.ProjectionFar := DefaultVisibilityLimit;
  {$warnings on}
  {$endif}
  if Result.ProjectionFar <= 0 then
    Result.ProjectionFar := GetDefaultProjectionFar;
  Assert(Result.ProjectionFar > 0);

  { update ProjectionFarFinite.
    ProjectionFar may be later changed to ZFarInfinity. }
  Result.ProjectionFarFinite := Result.ProjectionFar;

  { We need infinite ZFar in case of shadow volumes.
    But only perspective projection supports ZFar in infinity. }
  if (Result.ProjectionType = ptPerspective) and
     { Check "GLFeatures = nil" to allow using CalculateProjection and
       things depending on it when no OpenGL context available.

       Testcase: open CGE editor, open a project with any sprite sheet,
       open sprite sheet editor with some .castle-sprite-sheet file,
       then do "Close Project" (without closing sprite sheet editor
       explicitly). It should not crash. }
     ((GLFeatures = nil) or GLFeatures.ShadowVolumesPossible) and
     ShadowVolumes then
    Result.ProjectionFar := ZFarInfinity;

  Camera.InternalSetEffectiveProjection(
    Result.ProjectionNear,
    Result.ProjectionFar);

  { Calculate Result.Dimensions regardless of Result.ProjectionType,
    this way OnProjection can easily change projection type to orthographic. }
  UpdateOrthographicDimensions;

{
  WritelnLogMultiline('Projection', Format(
    'ProjectionType: %s' + NL +
    'Perspective Field of View (in degrees): %f x %f' + NL +
    'Orthographic Dimensions: %s' + NL +
    'Near: %f' + NL +
    'Far: %f', [
    ProjectionTypeToStr(Result.ProjectionType),
    Result.PerspectiveAngles.X,
    Result.PerspectiveAngles.Y,
    Result.Dimensions.ToString,
    Result.ProjectionNear,
    Result.ProjectionFar
  ]));
}
end;

function TCastleViewport.MainLightForShadows(out AMainLightPosition: TVector4): boolean;
var
  AMainLightPosition3D: PVector3;
begin
  if Items.MainScene <> nil then
  begin
    Result :=
      Items.MainScene.InternalMainLightForShadows(AMainLightPosition) and
      { We need WorldTransform for below conversion local<->world space.
        It may not be available, if
        - MainScene is present multiple times in Items
        - MainScene is not present in Items at all, temporarily, but is still a MainScene.
        Testcase: castle-game, change from level to level using debug menu.
      }
      Items.MainScene.HasWorldTransform;
    { Transform AMainLightPosition to world space.
      This matters in case MainScene (that contains shadow-casting light) has some transformation. }
    if Result then
    begin
      AMainLightPosition3D := PVector3(@AMainLightPosition);
      if AMainLightPosition.W = 0 then
        AMainLightPosition3D^ := Items.MainScene.LocalToWorldDirection(AMainLightPosition3D^)
      else
        AMainLightPosition3D^ := Items.MainScene.LocalToWorld(AMainLightPosition3D^);
    end;
  end else
    Result := false;
end;

procedure TCastleViewport.Render3D(const Params: TRenderParams);
begin
end;

procedure TCastleViewport.RenderOnePass(const Params: TRenderParams);
begin
  {$warnings off} // keep deprecated working
  Render3D(Params);
  {$warnings on}

  Params.Frustum := @Params.RenderingCamera.Frustum;
  Items.Render(Params);
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

function TCastleViewport.HeadlightInstance(out Instance: TLightInstance): boolean;
var
  Node: TAbstractLightNode;
  HC: TCastleCamera;

  procedure PrepareInstance;
  var
    Position, Direction, Up: TVector3;
  begin
    Assert(Node <> nil);
    Node.InternalHeadlight := true;

    HC.GetView(Position, Direction, Up);

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

begin
  Result := false;
  Node := Items.InternalHeadlight;
  if Node <> nil then
  begin
    HC := Items.MainCamera;
    if HC <> nil then
    begin
      PrepareInstance;
      Result := true;
    end;
  end;
end;

function TCastleViewport.PrepareParams: TPrepareParams;
{ Note: you cannot refer to PrepareParams inside
  the TCastleTransform.PrepareResources or TCastleTransform.Render implementation,
  as they may change the referenced PrepareParams.InternalGlobalLights value.
}
begin
  { We just reuse FRenderParams.FGlobalLights below as a temporary
    TLightInstancesList that we already have created. }

  { initialize FPrepareParams.InternalGlobalLights }
  FRenderParams.FGlobalLights.Clear;
  InitializeGlobalLights(FRenderParams.FGlobalLights);
  FPrepareParams.InternalGlobalLights := FRenderParams.FGlobalLights;

  { initialize FPrepareParams.InternalGlobalFog }
  if UseGlobalFog and
     (Items.MainScene <> nil) then
    FPrepareParams.InternalGlobalFog := Items.MainScene.FogStack.Top
  else
    FPrepareParams.InternalGlobalFog := nil;

  Result := FPrepareParams;
end;

function TCastleViewport.BaseLights: TLightInstancesList;
begin
  Result := PrepareParams.InternalGlobalLights as TLightInstancesList;
end;

procedure TCastleViewport.RenderFromView3D(const Params: TRenderParams);

  procedure RenderNoShadows;
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

  procedure RenderWithShadows(const MainLightPosition: TVector4);
  begin
    FShadowVolumeRenderer.InitFrustumAndLight(Params.RenderingCamera.Frustum, MainLightPosition);
    FShadowVolumeRenderer.Render(Params,
      {$ifdef FPC}@{$endif}RenderOnePass,
      {$ifdef FPC}@{$endif}RenderShadowVolume, ShadowVolumesRender);
  end;

var
  MainLightPosition: TVector4;
begin
  if GLFeatures.ShadowVolumesPossible and
     ShadowVolumes and
     MainLightForShadows(MainLightPosition) then
    RenderWithShadows(MainLightPosition) else
    RenderNoShadows;
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
       MainLightForShadows(MainLightPosition) then
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
    if Items.MainScene <> nil then
      BackgroundRenderer := Items.MainScene.InternalBackgroundRenderer
    else
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
      BackgroundRenderer.Render(RenderingCamera, BackgroundWireframe, RenderRect, FProjection);
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
      NewGlobalLight^ := SceneCastingLights.InternalGlobalLights.List^[J];
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
  FRenderParams.UserPass := CustomRenderingPass;
  FRenderParams.RenderingCamera := RenderingCamera;
  FillChar(FRenderParams.Statistics, SizeOf(FRenderParams.Statistics), #0);

  { Calculate FRenderParams.FGlobalLights }
  FRenderParams.FGlobalLights.Clear;
  { Add headlight }
  InitializeGlobalLights(FRenderParams.FGlobalLights);
  { Add lights from MainScene  }
  if Items.MainScene <> nil then
    AddGlobalLightsFromScene(Items.MainScene);
  { Add lights from all scenes with CastGlobalLights }
  if Items.InternalScenesCastGlobalLights <> nil then
    for SceneCastingLights in Items.InternalScenesCastGlobalLights do
      if Items.MainScene <> SceneCastingLights then // MainScene is already accounted for above
        AddGlobalLightsFromScene(SceneCastingLights);

  { initialize FRenderParams.GlobalFog }
  if UseGlobalFog and
     (Items.MainScene <> nil) then
    FRenderParams.GlobalFog := Items.MainScene.FogStack.Top
  else
    FRenderParams.GlobalFog := nil;

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
  ApplyProjection;
  RenderOnScreen(Camera);
end;

function TCastleViewport.InternalExtraGetScreenEffects(const Index: Integer): TGLSLProgram;
begin
  Result := GetScreenEffects(Index);
end;

function TCastleViewport.InternalExtraScreenEffectsCount: Integer;
begin
  Result := ScreenEffectsCount;
end;

function TCastleViewport.InternalExtraScreenEffectsNeedDepth: Boolean;
begin
  Result := ScreenEffectsNeedDepth;
end;

function TCastleViewport.GetScreenEffects(const Index: Integer): TGLSLProgram;
begin
  if ScreenSpaceAmbientOcclusion then
    SSAOShaderInitialize;
  if ScreenSpaceReflections then
    SSRShaderInitialize;

  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) and ScreenSpaceReflections and (SSRShader <> nil) then
  begin
    if Index = 0 then
      Result := SSAOShader
    else
    if Index = 1 then
      Result := SSRShader
    else
      Result := Items.MainScene.ScreenEffects(Index - 2);
  end else
  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
  begin
    if Index = 0 then
      Result := SSAOShader
    else
      Result := Items.MainScene.ScreenEffects(Index - 1);
  end else
  if ScreenSpaceReflections and (SSRShader <> nil) then
  begin
    if Index = 0 then
      Result := SSRShader
    else
      Result := Items.MainScene.ScreenEffects(Index - 1);
  end else
  if Items.MainScene <> nil then
    Result := Items.MainScene.ScreenEffects(Index)
  else
    { no Index is valid, since ScreenEffectsCount = 0 in this class }
    Result := nil;
end;

function TCastleViewport.ScreenEffectsCount: Integer;
begin
  if ScreenSpaceAmbientOcclusion then
    SSAOShaderInitialize;
  if ScreenSpaceReflections then
    SSRShaderInitialize;

  if Items.MainScene <> nil then
    Result := Items.MainScene.ScreenEffectsCount
  else
    Result := 0;
  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
    Inc(Result);
  if ScreenSpaceReflections and (SSRShader <> nil) then
    Inc(Result);
end;

function TCastleViewport.ScreenEffectsNeedDepth: boolean;
begin
  if ScreenSpaceAmbientOcclusion then
    SSAOShaderInitialize;
  if ScreenSpaceReflections then
    SSRShaderInitialize;

  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
    Exit(true);
  if ScreenSpaceReflections and (SSRShader <> nil) then
    Exit(true);
  if Items.MainScene <> nil then
    Result := Items.MainScene.ScreenEffectsNeedDepth
  else
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

  if GLFeatures.Shaders <> gsNone then
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
  if GLFeatures.Shaders <> gsNone then
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

  { We actually need to do it only if GLFeatures.ShadowVolumesPossible
    and ShadowVolumes for any viewport.
    But we can as well do it always, it's harmless (just checks some GL
    extensions). (Otherwise we'd have to handle SetShadowVolumes.) }
  if FShadowVolumeRenderer = nil then
  begin
    FShadowVolumeRenderer := TGLShadowVolumeRenderer.Create;
    FShadowVolumeRenderer.GLContextOpen;
  end;
end;

procedure TCastleViewport.GLContextClose;
begin
  FreeAndNil(FShadowVolumeRenderer);

  FreeAndNil(SSAOShader);
  SSAOShaderInitialized := false;

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

function TCastleViewport.RequiredCamera: TCastleNavigation;
begin
  {$warnings off} // using deprecated in deprecated
  Result := RequiredNavigation;
  {$warnings on}
end;

function TCastleViewport.RequiredNavigation: TCastleNavigation;
begin
  { For backward-compatibility, this also initializes Camera vectors
    (even though the method docs only guarantee that it initializes Navigation,
    but in the past Camera and Navigation were the same thing). }
  EnsureCameraDetected;

  if Navigation = nil then
    AssignDefaultNavigation;
  {$warnings off} // using deprecated in deprecated
  // Since AssignDefaultNavigation may leave Navigation nil, make sure it is assigned now
  if Navigation = nil then
    Result := InternalExamineNavigation;
  {$warnings on}
  Result := Navigation;
end;

function TCastleViewport.InternalExamineCamera: TCastleExamineNavigation;
begin
  {$warnings off} // using deprecated in deprecated
  Result := InternalExamineNavigation;
  {$warnings on}
end;

function TCastleViewport.InternalWalkCamera: TCastleWalkNavigation;
begin
  {$warnings off} // using deprecated in deprecated
  Result := InternalWalkNavigation;
  {$warnings on}
end;

function TCastleViewport.InternalExamineNavigation: TCastleExamineNavigation;
begin
  if FInternalExamineNavigation = nil then
  begin
    FInternalExamineNavigation := TCastleExamineNavigation.Create(Self);
    FInternalExamineNavigation.SetTransient;
    { For easy backward-compatibility, Viewport is assigned here for the
      entire lifetime of FInternalExamineNavigation instance,
      even before calling SetNavigation on it. }
    FInternalExamineNavigation.InternalViewport := Self;
  end;
  Result := FInternalExamineNavigation;
end;

function TCastleViewport.InternalWalkNavigation: TCastleWalkNavigation;
begin
  if FInternalWalkNavigation = nil then
  begin
    FInternalWalkNavigation := TCastleWalkNavigation.Create(Self);
    FInternalWalkNavigation.SetTransient;
    { For easy backward-compatibility, Viewport is assigned here for the
      entire lifetime of FInternalExamineNavigation instance,
      even before calling SetNavigation on it. }
    FInternalWalkNavigation.InternalViewport := Self;
  end;
  Result := FInternalWalkNavigation;
end;

function TCastleViewport.ExamineNavigation(const SwitchNavigationTypeIfNeeded: boolean): TCastleExamineNavigation;
var
  NewNavigation: TCastleExamineNavigation;
begin
  if not (Navigation is TCastleExamineNavigation) then
  begin
    if not SwitchNavigationTypeIfNeeded then
      Exit(nil);

    { For backward-compatibility, this also initializes Camera vectors
      (even though the method docs only guarantee that it initializes Navigation,
      but in the past Camera and Navigation were the same thing). }
    EnsureCameraDetected;

    {$warnings off} // using deprecated in deprecated
    NewNavigation := InternalExamineNavigation;
    {$warnings on}
    if Navigation = nil then
      AssignDefaultNavigation; // initialize defaults from MainScene
    // AssignDefaultNavigation could leave Navigation at nil, in which case ignor
    if Navigation <> nil then
      NewNavigation.Assign(Navigation);
    Navigation := NewNavigation;
    { make sure it's in ntExamine mode (as we possibly reuse old navigation,
      by reusing InternalExamineNavigation, so we're not sure what state it's in. }
    {$warnings off} // using deprecated to keep it working
    NavigationType := ntExamine;
    {$warnings on}
  end;
  Result := Navigation as TCastleExamineNavigation;
end;

function TCastleViewport.ExamineCamera(const SwitchNavigationTypeIfNeeded: boolean): TCastleExamineNavigation;
begin
  {$warnings off} // using deprecated in deprecated
  Result := ExamineNavigation(SwitchNavigationTypeIfNeeded);
  {$warnings on}
end;

function TCastleViewport.WalkNavigation(const SwitchNavigationTypeIfNeeded: boolean): TCastleWalkNavigation;
var
  NewNavigation: TCastleWalkNavigation;
begin
  if not (Navigation is TCastleWalkNavigation) then
  begin
    if not SwitchNavigationTypeIfNeeded then
      Exit(nil);

    { For backward-compatibility, this also initializes Camera vectors
      (even though the method docs only guarantee that it initializes Navigation,
      but in the past Camera and Navigation were the same thing). }
    EnsureCameraDetected;

    {$warnings off} // using deprecated in deprecated
    NewNavigation := InternalWalkNavigation;
    {$warnings on}
    if Navigation = nil then
      AssignDefaultNavigation; // initialize defaults from MainScene
    // AssignDefaultNavigation could leave Navigation at nil, in which case ignor
    if Navigation <> nil then
      NewNavigation.Assign(Navigation);
    Navigation := NewNavigation;
    { make sure it's in ntWalk mode (as we possibly reuse old navigation,
      by reusing InternalWalkNavigation, so we're not sure what state it's in. }
    {$warnings off} // using deprecated to keep it working
    NavigationType := ntWalk;
    {$warnings on}
  end;
  Result := Navigation as TCastleWalkNavigation;
end;

function TCastleViewport.WalkCamera(const SwitchNavigationTypeIfNeeded: boolean): TCastleWalkNavigation;
begin
  {$warnings off} // using deprecated in deprecated
  Result := WalkNavigation(SwitchNavigationTypeIfNeeded);
  {$warnings on}
end;

function TCastleViewport.GetNavigationType: TNavigationType;
var
  C: TCastleNavigation;
begin
  C := Navigation;
  { We are using here Navigation, not RequiredNavigation, as automatically
    creating Navigation could have surprising consequences.
    E.g. it means that SetNavigation(nil) may recreate the navigation,
    as BoundNavigationInfoChanged calls something that checks
    NavigationType. }
  if C = nil then
    Result := ntNone
  else
    Result := C.GetNavigationType;
end;

procedure TCastleViewport.SetNavigationType(const Value: TNavigationType);
var
  E: TCastleExamineNavigation;
  W: TCastleWalkNavigation;
begin
  { Do this even if "Value = GetNavigationType".
    This makes sense, in case you set some weird values.
    On the other hand, it makes "NavigationType := NavigationType" sometimes
    a sensible operation that changes something.

    It also avoids recursive loop when first assigning navigation
    in AssignDefaultNavigation. }

  { do not change NavigationType when
    SetNavigationType is called from ExamineNavigation or WalkNavigation
    that were already called by NavigationType.
    It's actually harmless, but still useless. }
  if FWithinSetNavigationType then
    Exit;
  FWithinSetNavigationType := true;

  case Value of
    ntExamine:
      begin
        {$warnings off} // TODO: this should be internal
        E := ExamineNavigation;
        {$warnings on}
        E.Input := TCastleNavigation.DefaultInput;
        E.Turntable := false;
      end;
    ntTurntable:
      begin
        {$warnings off} // TODO: this should be internal
        E := ExamineNavigation;
        {$warnings on}
        E.Input := TCastleNavigation.DefaultInput;
        E.Turntable := true;
      end;
    ntWalk:
      begin
        {$warnings off} // TODO: this should be internal
        W := WalkNavigation;
        {$warnings on}
        W.Input := TCastleNavigation.DefaultInput;
        W.PreferGravityUpForRotations := true;
        W.PreferGravityUpForMoving := true;
        W.Gravity := true;
      end;
    ntFly:
      begin
        {$warnings off} // TODO: this should be internal
        W := WalkNavigation;
        {$warnings on}
        W.Input := TCastleNavigation.DefaultInput;
        W.PreferGravityUpForRotations := true;
        W.PreferGravityUpForMoving := false;
        W.Gravity := false;
      end;
    ntNone:
      begin
        { Advantage: This way setting NavigationType to ntNone (default NavigationType value)
          will restore Navigation to nil, which is Navigation default value. }
        // Navigation := nil;

        { Advantage: This way of setting NavigationType to ntNone (by making Navigation non-nil)
          explicitly will prevent
          Navigation from being auto-created (in case AutoNavigation remains @true),
          which would make setting "NavigationType := ntNone" moot. }
        {$warnings off} // TODO: this should be internal
        W := WalkNavigation;
        {$warnings on}
        W.Input := [];
        W.Gravity := false;
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleViewport.SetNavigationType: Value?');
    {$endif}
  end;

  { This assertion should be OK. It is commented out only to prevent
    GetNavigationType from accidentally creating something intermediate,
    and thus making debug and release behavior different) }
  // Assert(GetNavigationType = Value);

  FWithinSetNavigationType := false;

  { Call OnBoundNavigationInfoChanged when NavigationType changed. }
  BoundNavigationInfoChanged;
end;

procedure TCastleViewport.ClearCameras;
begin
  Navigation := nil;
  FreeAndNil(FInternalExamineNavigation);
  FreeAndNil(FInternalWalkNavigation);
end;

procedure TCastleViewport.AssignDefaultNavigation;
var
  Box: TBox3D;
  Scene: TCastleScene;
  C: TCastleExamineNavigation;
  Nav: TNavigationType;
begin
  Box := ItemsBoundingBox;
  Scene := Items.MainScene;
  if Scene <> nil then
  begin
    Nav := Scene.NavigationTypeFromNavigationInfo;

    { Set Navigation explicitly, otherwise SetNavigationType below could call
      ExamineNavigation / WalkNavigation that call AssignDefaultNavigation when Navigation = nil,
      and we would have infinite AssignDefaultNavigation calls loop. }
    {$warnings off} // TODO: this should be internal
    if Nav in [ntExamine, ntTurntable] then
      Navigation := InternalExamineNavigation
    else
      Navigation := InternalWalkNavigation;
    {$warnings on}

    {$warnings off} // using deprecated to keep it working
    NavigationType := Nav;
    {$warnings on}
    Scene.InternalUpdateNavigation(Navigation, Box);
  end else
  begin
    {$warnings off} // TODO: this should be internal
    C := InternalExamineNavigation;
    {$warnings on}
    C.ModelBox := Box;
    C.Radius := Box.AverageSize(false, 1.0) * WorldBoxSizeToRadius;
    Navigation := C;
  end;
end;

procedure TCastleViewport.AssignDefaultCamera;
var
  Box: TBox3D;
  Scene: TCastleScene;
  APos, ADir, AUp, NewGravityUp: TVector3;
begin
  Box := ItemsBoundingBox;
  Scene := Items.MainScene;
  if Scene <> nil then
  begin
    Scene.InternalUpdateCamera(Camera, Box, false, false);
  end else
  begin
    CameraViewpointForWholeScene(Box, 2, 1, false, true,
      APos, ADir, AUp, NewGravityUp);
    Camera.Init(APos, ADir, AUp, NewGravityUp);
  end;

  { Mark it as done, so that next EnsureCameraDetected does nothing
    if you manually call this.
    This is consistent with AssignDefaultNavigation,
    that sets Navigation <> nil thus it is no longer auto-detected
    if you call AssignDefaultNavigation. }
  AssignDefaultCameraDone := true;
end;

function TCastleViewport.Statistics: TRenderStatistics;
begin
  Result := FRenderParams.Statistics;
end;

procedure TCastleViewport.Setup2D;
begin
  Camera.Init(
    { pos } Vector3(0, 0, Default2DCameraZ),
    { dir } Vector3(0, 0, -1),
    { up } Vector3(0, 1, 0),
    { gravity up } Vector3(0, 1, 0)
  ); // sets both initial and current vectors
  Camera.ProjectionNear := -Default2DProjectionFar;
  Camera.ProjectionFar := Default2DProjectionFar;
  Camera.ProjectionType := ptOrthographic;
  AutoCamera := false;
end;

procedure TCastleViewport.PositionToPrerequisites;
begin
  { Note that we need to call this every time before PositionToXxx,
    not just only once (in case ApplyProjection did not yet happen).
    That's because Camera settings, that determine how CalculateProjection calculates
    FProjection, may change at any moment, e.g. when doing a sequence

      ViewPort.PositionToCameraPlane(...);
      Camera.Orthographic.Scale := Camera.Orthographic.Scale * ...;
      ViewPort.PositionToCameraPlane(...);
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

  Camera.CustomRay(R, ContainerPosition, FProjection, RayOrigin, RayDirection);
end;

function TCastleViewport.PositionToCameraPlane(const Position: TVector2;
  const ContainerCoordinates: Boolean;
  const Depth: Single; out PlanePosition: TVector3): Boolean;
var
  RayOrigin, RayDirection: TVector3;
  Plane: TVector4;
begin
  PositionToRay(Position, ContainerCoordinates, RayOrigin, RayDirection);

  Plane := Vector4(Camera.Direction,
    { We know that Camera.Direction, which is used as Plane.XYZ, is normalized.
      Calculate Plane[3] such that point RayOrigin + Camera.Direction * Depth
      satisfies the plane equation. }
    - TVector3.DotProduct(RayOrigin + Camera.Direction * Depth, Camera.Direction));

  Result := TryPlaneRayIntersection(PlanePosition, Plane, RayOrigin, RayDirection);
end;

function TCastleViewport.PositionToWorldPlane(const Position: TVector2;
  const ContainerCoordinates: Boolean;
  const PlaneZ: Single; out PlanePosition: TVector3): Boolean;
var
  RayOrigin, RayDirection: TVector3;
begin
  PositionToRay(Position, ContainerCoordinates, RayOrigin, RayDirection);

  Result := TrySimplePlaneRayIntersection(PlanePosition, 2, PlaneZ,
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

  CameraToWorldMatrix := Camera.MatrixInverse;

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

  CameraFromWorldMatrix := Camera.Matrix;
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

  Matrix := Camera.ProjectionMatrix * Camera.Matrix;
  P := Matrix.MultPoint(WorldPosition);
  Result := Vector2(
    MapRange(P.X, -1, 1, 0, EffectiveWidth ),
    MapRange(P.Y, -1, 1, 0, EffectiveHeight)
  );
end;

procedure TCastleViewport.SetItems(const Value: TCastleRootTransform);
begin
  if FItems <> Value then
  begin
    if Value = nil then
      raise EInternalError.Create('Cannot set TCastleSceneManager.Items to nil');
    FItems := Value;
    LastVisibleStateIdForVisibleChange := 0;

    { TODO: do the same thing we did when creating internal FItems:
    FItems.OnCursorChange := @RecalculateCursor;

    // No need to change this, it's documented that MainCamera has to be manually managed if you reuse items
    // FItems.MainCamera := Camera;
    }

  end;
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
  Result := Items.MainScene;
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

procedure TCastleViewport.PrepareResources(const Item: TCastleTransform;
  const DisplayProgressTitle: string;
  Options: TPrepareResourcesOptions);
var
  MainLightPosition: TVector4; // value of this is ignored
begin
  if not ApplicationProperties.IsGLContextOpen then
    raise Exception.Create('PrepareResources can only be called when rendering context is initialized.' + NL +
      'Various events and virtual methods can be used to wait for the context:' + NL +
      '- (if you use CastleWindow) Application.OnInitialize' + NL +
      '- (if you use CastleWindow) TCastleWindow.OnOpen' + NL +
      '- (if you use LCL CastleControl) TCastleControl.OnOpen' + NL +
      '- TCastleUserInterface.GLContextOpen'
    );

  if GLFeatures.ShadowVolumesPossible and
     ShadowVolumes and
     MainLightForShadows(MainLightPosition) then
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

  if DisplayProgressTitle <> '' then
  begin
    Progress.Init(Items.PrepareResourcesSteps, DisplayProgressTitle, true);
    try
      Item.PrepareResources(Options, true, PrepareParams);
    finally Progress.Fini end;
  end else
    Item.PrepareResources(Options, false, PrepareParams);
end;

procedure TCastleViewport.PrepareResources(const DisplayProgressTitle: string;
  const Options: TPrepareResourcesOptions);
begin
  PrepareResources(Items, DisplayProgressTitle, Options);
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
    if (Navigation is TCastleMouseLookNavigation) and
       TCastleMouseLookNavigation(Navigation).MouseLook then
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
    if (Items.MainScene <> nil) and
       (CapturePointingDevice <> Items.MainScene) and
       ((RayHit = nil) or (RayHit.IndexOfItem(Items.MainScene) = -1)) then
      CallMove(FakeRayCollisionNode(RayOrigin, RayDirection, Items.MainScene), Distance);
  end;

begin
  Result := PointingDeviceMoveCore(MouseRayHit, MouseRayOrigin, MouseRayDirection);
end;

procedure TCastleViewport.InternalCameraChanged;
var
  Pos, Dir, Up: TVector3;
  MC: TCastleCamera;
begin
  MC := Items.MainCamera;
  if MC = Camera then
  begin
    Inc(Items.InternalMainCameraStateId);

    // we should redraw soon, this will cause VisibleChange() call
    Inc(Items.InternalVisibleStateId);

    // we changed MainCamera which makes headlight -> so lighting changed
    if Items.InternalHeadlight <> nil then
      Inc(Items.InternalVisibleNonGeometryStateId);

    Camera.GetView(Pos, Dir, Up);
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

function TCastleViewport.NavigationMoveAllowed(const Sender: TCastleNavigation;
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

begin
  { Both version result in calling WorldMoveAllowed.
    AvoidNavigationCollisions version adds AvoidNavigationCollisions.Disable/Enable around. }

  // take into account PreventInfiniteFallingDown
  if BecauseOfGravity and
     PreventInfiniteFallingDown and
     PositionOutsideBoundingBox then
    Exit(false);

  if UseAvoidNavigationCollisions then
    Result := AvoidNavigationCollisions.MoveAllowed(OldPos, ProposedNewPos, NewPos, BecauseOfGravity)
  else
    Result := Items.WorldMoveAllowed(OldPos, ProposedNewPos, NewPos, true, Radius,
      { We prefer to resolve collisions with navigation using sphere.
        But for TCastleTransform implementations that can't use sphere, we can construct box. }
      Box3DAroundPoint(OldPos, Radius * 2),
      Box3DAroundPoint(ProposedNewPos, Radius * 2), BecauseOfGravity);
end;

function TCastleViewport.NavigationHeight(const Sender: TCastleNavigation;
  const Position: TVector3;
  out AboveHeight: Single; out AboveGround: PTriangle): boolean;
begin
  { Both version result in calling WorldHeight.
    AvoidNavigationCollisions version adds AvoidNavigationCollisions.Disable/Enable around. }

  if UseAvoidNavigationCollisions then
    Result := AvoidNavigationCollisions.Height(Position, AboveHeight, AboveGround)
  else
    Result := Items.WorldHeight(Position, AboveHeight, AboveGround);
end;

function TCastleViewport.CameraRayCollision(const RayOrigin, RayDirection: TVector3): TRayCollision;
begin
  { Both version result in calling WorldRay.
    AvoidNavigationCollisions version adds AvoidNavigationCollisions.Disable/Enable around. }

  if UseAvoidNavigationCollisions then
    Result := AvoidNavigationCollisions.Ray(RayOrigin, RayDirection)
  else
    Result := Items.WorldRay(RayOrigin, RayDirection);
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
  if AutoCamera then
  begin
    Items.MainScene.InternalUpdateCamera(Camera, ItemsBoundingBox, false);
    BoundViewpointChanged;
  end;
end;

procedure TCastleViewport.MainSceneAndCamera_BoundNavigationInfoChanged(Sender: TObject);
begin
  {$warnings off} // using deprecated to keep it working
  if AutoNavigation and (Navigation <> nil) then
  begin
    NavigationType := Items.MainScene.NavigationTypeFromNavigationInfo;
    Items.MainScene.InternalUpdateNavigation(Navigation, Items.BoundingBox);
  end;
  {$warnings on}
  BoundNavigationInfoChanged;
end;

procedure TCastleViewport.MainSceneAndCamera_BoundViewpointVectorsChanged(Sender: TObject);
begin
  { TODO: It may be useful to enable camera animation by some specific property,
    like AnimateCameraByViewpoint (that works even when AutoCamera = false,
    as we advise for new viewports). }
  if AutoCamera { or AnimateCameraByViewpoint } then
    Items.MainScene.InternalUpdateCamera(Camera, ItemsBoundingBox, true);
end;

class procedure TCastleViewport.CreateComponentSetup2D(Sender: TObject);
begin
  (Sender as TCastleViewport).Setup2D;
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
  if (PropertyName = 'Transparent') or
     (PropertyName = 'Camera') or
     (PropertyName = 'Navigation') or
     (PropertyName = 'Background') or
     (PropertyName = 'BackgroundColorPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleviewport_persistent_vectors.inc}
{$undef read_implementation_methods}

{ TCastleViewportList -------------------------------------------------- }

procedure TCastleViewportList.Notify({$ifdef FPC}constref{$else}const{$endif} Value: TCastleViewport;
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

{ TCastleSceneManager -------------------------------------------------------- }

constructor TCastleSceneManager.Create(AOwner: TComponent);
begin
  inherited;

  FDefaultViewport := true;
  FullSize := true;
  AutoNavigation := true;
  AutoCamera := true;

  {$warnings off} // using deprecated in deprecated
  FViewports := TCastleViewportList.Create(false);
  {$warnings on}
  FViewports.SceneManager := Self;
  if DefaultViewport then FViewports.Add(Self);
end;

destructor TCastleSceneManager.Destroy;
var
  I: Integer;
begin
  if FViewports <> nil then
  begin
    for I := 0 to FViewports.Count - 1 do
      if FViewports[I] is TCastleViewport then
      begin
        {$ifdef FPC}
        {$warnings off} // using deprecated in deprecated
        Assert(
          (TCastleViewport(FViewports[I]).SceneManager = Self) or
          (TCastleViewport(FViewports[I]).SceneManager = nil)
        );
        {$warnings on}
        {$endif}

        { Set SceneManager by direct field (FSceneManager),
          otherwise TCastleViewport.SetSceneManager would try to update
          our Viewports list, that we iterate over right now... }
        TCastleViewport(FViewports[I]).FSceneManager := nil;
      end;
    FreeAndNil(FViewports);
  end;

  inherited;
end;

procedure TCastleSceneManager.Render;
begin
  if not DefaultViewport then Exit;
  inherited;
end;

procedure TCastleSceneManager.SetDefaultViewport(const Value: boolean);
begin
  if Value <> FDefaultViewport then
  begin
    FDefaultViewport := Value;
    if DefaultViewport then
      Viewports.Add(Self)
    else
      Viewports.Remove(Self);
  end;
end;

function TCastleSceneManager.GetMainCamera: TCastleCamera;
begin
  Result := Items.MainCamera;
end;

procedure TCastleSceneManager.SetMainCamera(const Value: TCastleCamera);
begin
  Items.MainCamera := Value;
end;

function TCastleSceneManager.GetMainSceneInternal: TCastleScene;
begin
  Result := Items.MainScene;
end;

procedure TCastleSceneManager.SetMainScene(const Value: TCastleScene);
begin
  Items.MainScene := Value;
end;

function TCastleSceneManager.GetUseHeadlight: TUseHeadlight;
begin
  Result := Items.UseHeadlight;
end;

procedure TCastleSceneManager.SetUseHeadlight(const Value: TUseHeadlight);
begin
  Items.UseHeadlight := Value;
end;

function TCastleSceneManager.GravityUp: TVector3;
begin
  Result := Camera.GravityUp;
end;

function TCastleSceneManager.GetHeadlightNode: TAbstractLightNode;
begin
  Result := Items.HeadlightNode;
end;

procedure TCastleSceneManager.SetHeadlightNode(const Value: TAbstractLightNode);
begin
  Items.HeadlightNode := Value;
end;

function TCastleSceneManager.GetMoveLimit: TBox3D;
begin
  Result := Items.MoveLimit;
end;

procedure TCastleSceneManager.SetMoveLimit(const Value: TBox3D);
begin
  Items.MoveLimit := Value;
end;

{$ifdef FPC}
function TCastleSceneManager.PhysicsProperties: TPhysicsProperties;
begin
  Result := Items.PhysicsProperties;
end;
{$endif}

function TCastleSceneManager.GetTimeScale: Single;
begin
  Result := Items.TimeScale;
end;

procedure TCastleSceneManager.SetTimeScale(const Value: Single);
begin
  Items.TimeScale := Value;
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
  R.Caption := 'Scene Manager';
  R.IsDeprecated := true;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleTouchNavigation, 'Touch Navigation');
  RegisterSerializableComponent(TCastleViewport, 'Viewport');

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleViewport;
  R.Caption := 'Viewport (Configured For 2D)';
  R.OnCreate := {$ifdef FPC}@{$endif}TCastleViewport{$ifdef FPC}(nil){$endif}.CreateComponentSetup2D;
  RegisterSerializableComponent(R);

  InitializeWarmupCache;
end.
