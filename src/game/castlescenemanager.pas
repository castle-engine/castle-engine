{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Scene manager (TCastleSceneManager) and viewport (TCastleViewport) classes. }
unit CastleSceneManager;

{$I castleconf.inc}

interface

uses SysUtils, Classes, FGL,
  CastleVectors, X3DNodes, X3DTriangles, CastleScene, CastleSceneCore, CastleCameras,
  CastleGLShadowVolumes, CastleUIControls, Castle3D, CastleTriangles,
  CastleKeysMouse, CastleBoxes, CastleBackground, CastleUtils, CastleClassUtils,
  CastleGLShaders, CastleGLImages, CastleTimeUtils, CastleSectors,
  CastleInputs, CastlePlayer, CastleRectangles, CastleColors, CastleGL,
  CastleRays, CastleScreenEffects;

type
  TCastleAbstractViewport = class;
  TCastleSceneManager = class;

  EViewportSceneManagerMissing = class(Exception);

  TRender3DEvent = procedure (Viewport: TCastleAbstractViewport;
    const Params: TRenderParams) of object;

  { Internal, special TRenderParams descendant that can return different
    set of base lights for some scenes. Used to implement GlobalLights,
    where MainScene and other objects need different lights.
    @exclude. }
  TManagerRenderParams = class(TRenderParams)
  private
    MainScene: T3D;
    FBaseLights: array [boolean { is main scene }] of TLightInstancesList;
  public
    constructor Create;
    destructor Destroy; override;
    function BaseLights(Scene: T3D): TAbstractLightInstancesList; override;
  end;

  { Event for TCastleSceneManager.OnMoveAllowed. }
  TWorldMoveAllowedEvent = procedure (Sender: TCastleSceneManager;
    var Allowed: boolean;
    const OldPosition, NewPosition: TVector3Single;
    const BecauseOfGravity: boolean) of object;

  { Common abstract class for things that may act as a viewport:
    TCastleSceneManager and TCastleViewport. }
  TCastleAbstractViewport = class(TUIControlSizeable)
  private
    type
      TScreenPoint = packed record
        Position: TVector2Single;
        TexCoord: TVector2Single;
      end;
    var
    FCamera: TCamera;
    FPaused: boolean;
    FRenderParams: TManagerRenderParams;

    FShadowVolumes: boolean;
    FShadowVolumesRender: boolean;

    FBackgroundWireframe: boolean;
    FBackgroundColor: TCastleColor;
    FOnRender3D: TRender3DEvent;
    FHeadlightFromViewport: boolean;
    FUseGlobalLights: boolean;

    { If a texture for screen effects is ready, then
      ScreenEffectTextureDest/Src/Depth/Target are non-zero and
      ScreenEffectRTT is non-nil.
      Also, ScreenEffectTextureWidth/Height indicate size of the texture,
      as well as ScreenEffectRTT.Width/Height. }
    ScreenEffectTextureDest, ScreenEffectTextureSrc: TGLuint;
    ScreenEffectTextureTarget: TGLenum;
    ScreenEffectTextureDepth: TGLuint;
    ScreenEffectRTT: TGLRenderToTexture;
    ScreenEffectTextureWidth: Cardinal;
    ScreenEffectTextureHeight: Cardinal;
    { Saved ScreenEffectsCount/NeedDepth result, during rendering. }
    CurrentScreenEffectsCount: Integer;
    CurrentScreenEffectsNeedDepth: boolean;
    ScreenPointVbo: TGLuint;
    ScreenPoint: packed array [0..3] of TScreenPoint;

    FApproximateActivation: boolean;
    FDefaultVisibilityLimit: Single;
    FTransparent: boolean;

    FScreenSpaceAmbientOcclusion: boolean;
    SSAOShader: TGLSLScreenEffect;

    { Set these to non-1 to deliberately distort field of view / aspect ratio.
      This is useful for special effects when you want to create unrealistic
      projection. Used by ApplyProjection. }
    DistortFieldOfViewY, DistortViewAspect: Single;
    SickProjectionTime: TFloatTime;

    FProjection: TProjection;

    procedure RecalculateCursor(Sender: TObject);
    function PlayerNotBlocked: boolean;
    procedure SetScreenSpaceAmbientOcclusion(const Value: boolean);

    { Render everything (by RenderFromViewEverything) on the screen.
      Takes care to set RenderingCamera (Target = rtScreen and camera as given),
      and takes care to apply Scissor if not FullSize,
      and calls RenderFromViewEverything.

      Takes care of using ScreenEffects. For this,
      before we render to the actual screen,
      we may render a couple times to a texture by a framebuffer.

      Always call ApplyProjection right before this, to set correct
      projection matrix. And before ApplyProjection you should also
      call UpdateGeneratedTexturesIfNeeded. }
    procedure RenderOnScreen(ACamera: TCamera);

    procedure RenderWithScreenEffectsCore;
    function RenderWithScreenEffects: boolean;

    { Set the projection parameters and matrix.
      Used by our Render method.

      This cooperates closely with current @link(Camera) definition.
      Viewport's @link(Camera), if not assigned, is automatically created here,
      see @link(Camera) and CreateDefaultCamera.
      This takes care to always update Camera.ProjectionMatrix,
      Projection, GetMainScene.BackgroundSkySphereRadius.
      In turn, this expects Camera.Radius to be already properly set
      (usually by CreateDefaultCamera). }
    procedure ApplyProjection;

    procedure SetPaused(const Value: boolean);
  protected
    { The projection parameters. Determines if the view is perspective
      or orthogonal and exact field of view parameters.
      Used by our Render method.

      The default implementation is TCastleAbstractViewport
      calculates projection based on MainScene currently bound Viewpoint,
      NavigationInfo and used @link(Camera).
      If scene manager's MainScene is not assigned, we use some default
      sensible perspective projection. }
    function CalculateProjection: TProjection; virtual;

    { Render one pass, with current camera and parameters.
      All current camera settings are saved in RenderingCamera,
      and the camera matrix is already loaded to OpenGL.

      If you want to display something 3D during rendering,
      this is the simplest method to override. (Or you can use OnRender3D
      event, which is called at the end of this method.)
      Alternatively, you can create new T3D descendant and add it
      to the @link(GetItems) list.

      @param(Params Parameters specify what lights should be used
        (Params.BaseLights, Params.InShadow), and which parts of the 3D scene
        should be rendered (Params.Transparent, Params.ShadowVolumesReceivers
        --- only matching 3D objects should be rendered by this method).) }
    procedure Render3D(const Params: TRenderParams); virtual;

    { Render shadow quads for all the things rendered by @link(Render3D).
      You can use here ShadowVolumeRenderer instance, which is guaranteed
      to be initialized with TGLShadowVolumeRenderer.InitFrustumAndLight,
      so you can do shadow volumes culling. }
    procedure RenderShadowVolume; virtual;

    { Render everything from current (in RenderingCamera) camera view.
      Current RenderingCamera.Target says to where we generate the image.
      Takes method must take care of making many rendering passes
      for shadow volumes, but doesn't take care of updating generated textures. }
    procedure RenderFromViewEverything; virtual;

    { Prepare lights shining on everything.
      BaseLights contents should be initialized here.

      The implementation in this class adds headlight determined
      by the @link(Headlight) method. By default, this looks at the MainScene,
      and follows NavigationInfo.headlight and
      KambiNavigationInfo.headlightNode properties. }
    procedure InitializeLights(const Lights: TLightInstancesList); virtual;

    { Headlight used to light the scene. Returns non-nil headlight node,
      if headlight is present, or @nil when no headlight.

      Default implementation of this method in TCastleSceneManager
      looks at the MainScene headlight. We return if MainScene is assigned
      and TCastleSceneCore.HeadlightOn is @true.
      (HeadlightOn in turn looks
      at information in VRML/X3D file (NavigationInfo.headlight)
      and you can also always set HeadlightOn explicitly by code.)
      The custom light node
      is obtained from TCastleSceneCore.CustomHeadlight,
      eventually using a default directional light node for a simple headlight.

      Default implementation of this method in TCastleViewport looks at
      SceneManager.Headlight.

      You can override this method to determine the headlight in any other way. }
    function Headlight: TAbstractLightNode; virtual; abstract;

    { Render the 3D part of scene. Called by RenderFromViewEverything at the end,
      when everything (clearing, background, headlight, loading camera
      matrix) is done and all that remains is to pass to OpenGL actual 3D world.

      This will change Params.Transparent, Params.InShadow and Params.ShadowVolumesReceivers
      as needed. Their previous values do not matter. }
    procedure RenderFromView3D(const Params: TRenderParams); virtual;

    { The background used during rendering.
      @nil if no background should be rendered.

      The default implementation in this class does what is usually
      most natural: return MainScene.Background, if MainScene assigned. }
    function Background: TBackground; virtual;

    { Detect position/direction of the main light that produces shadows.
      The default implementation in this class looks at
      MainScene.MainLightForShadows.

      @seealso TCastleSceneCore.MainLightForShadows }
    function MainLightForShadows(
      out AMainLightPosition: TVector4Single): boolean; virtual;

    procedure SetCamera(const Value: TCamera); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetContainer(const Value: TUIContainer); override;

    { Information about the 3D world.
      For scene maager, these methods simply return it's own properties.
      For TCastleViewport, these methods refer to scene manager.
      @groupBegin }
    function GetItems: T3DWorld; virtual; abstract;
    function GetMainScene: TCastleScene; virtual; abstract;
    function GetShadowVolumeRenderer: TGLShadowVolumeRenderer; virtual; abstract;
    function GetMouseRayHit: TRayCollision; virtual; abstract;
    function GetHeadlightCamera: TCamera; virtual; abstract;
    function GetPlayer: TPlayer; virtual; abstract;
    function GetTimeScale: Single; virtual; abstract;
    { @groupEnd }

    { Pass pointing device (mouse) move event to 3D world. }
    function PointingDeviceMove(const RayOrigin, RayDirection: TVector3Single): boolean; virtual; abstract;
    { Pass pointing device (mouse) activation/deactivation event to 3D world. }
    function PointingDeviceActivate(const Active: boolean): boolean; virtual; abstract;

    { Handle camera events.

      Scene manager implements collisions by looking at 3D scene,
      custom viewports implements collisions by calling their scene manager.

      @groupBegin }
    function CameraMoveAllowed(ACamera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; virtual; abstract;
    function CameraHeight(ACamera: TWalkCamera; const Position: TVector3Single;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; virtual; abstract;
    function CameraRayCollision(const RayOrigin, RayDirection: TVector3Single): TRayCollision; virtual; abstract;
    procedure CameraVisibleChange(ACamera: TObject); virtual; abstract;
    { @groupEnd }

    function GetScreenEffects(const Index: Integer): TGLSLProgram; virtual;
  public
    const
      DefaultScreenSpaceAmbientOcclusion = false;
      DefaultUseGlobalLights = true;
      DefaultShadowVolumes = true;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Resize; override;

    function AllowSuspendForInput: boolean; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    function SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean; override;
    function SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;

    { Current projection parameters,
      calculated by last @link(CalculateProjection) call.
      @bold(Read only), change these parameters only by overriding CalculateProjection. }
    property Projection: TProjection read FProjection;

    { Create default TCamera suitable for navigating in this scene.
      This is automatically used to initialize @link(Camera) property
      when @link(Camera) is @nil at ApplyProjection call.

      The implementation in base TCastleSceneManager uses MainScene.CreateCamera
      (so it will follow your VRML/X3D scene Viewpoint, NavigationInfo and such).
      If MainScene is not assigned, we will just create a simple
      TUniversalCamera in (initially) Examine mode.

      The implementation in TCastleViewport simply calls
      SceneManager.CreateDefaultCamera. So by default all the viewport's
      cameras are created the same way, by refering to the scene manager.
      If you want you can override it to specialize CreateDefaultCamera
      for specific viewport classes.

      Overloaded version without any parameters just uses Self as owner
      of the camera.

      @groupBegin }
    function CreateDefaultCamera(AOwner: TComponent): TCamera; virtual; abstract; overload;
    function CreateDefaultCamera: TCamera; overload;
    { @groupEnd }

    { Return current camera. Automatically creates it if missing. }
    function RequiredCamera: TCamera;

    { Smoothly animate current @link(Camera) to a default camera settings.

      Default camera settings are determined by calling CreateDefaultCamera.
      See TCamera.AnimateTo for details what and how is animated.

      Current @link(Camera) is created by CreateDefaultCamera if not assigned
      yet at this point. (And the animation isn't done, since such camera
      already stands at the default position.) This makes this method
      consistent: after calling it, you always know that @link(Camera) is
      assigned and going to the default position. }
    procedure CameraAnimateToDefault(const Time: TFloatTime);

    { Screen effects are shaders that post-process the rendered screen.
      If any screen effects are active, we will automatically render
      screen to a temporary texture, processing it with
      each shader.

      By default, screen effects come from GetMainScene.ScreenEffects,
      so the effects may be defined by VRML/X3D author using ScreenEffect
      nodes (see docs: [http://castle-engine.sourceforge.net/x3d_extensions_screen_effects.php]).
      Descendants may override GetScreenEffects, ScreenEffectsCount,
      and ScreenEffectsNeedDepth to add screen effects by code.
      Each viewport may have it's own, different screen effects.

      @groupBegin }
    property ScreenEffects [Index: Integer]: TGLSLProgram read GetScreenEffects;
    function ScreenEffectsCount: Integer; virtual;
    function ScreenEffectsNeedDepth: boolean; virtual;
    { @groupEnd }

    { Does the graphic card support our ScreenSpaceAmbientOcclusion shader.
      This does @italic(not) depend on the current state of
      ScreenSpaceAmbientOcclusion property.
      You can use it e.g. to disable the menu item to switch SSAO in 3D viewer. }
    function ScreenSpaceAmbientOcclusionAvailable: boolean;

    procedure GLContextOpen; override;
    procedure GLContextClose; override;

    { Instance for headlight that should be used for this scene.
      Uses @link(Headlight) method, applies appropriate camera position/direction.
      Returns @true only if @link(Headlight) method returned @true
      and a suitable camera was present.

      Instance should be considered undefined ("out" parameter)
      when we return @false. }
    function HeadlightInstance(out Instance: TLightInstance): boolean;

    { Base lights used for rendering. Uses InitializeLights,
      and returns instance owned and managed by this scene manager.
      You can only use this outside PrepareResources or Render,
      as they may change this instance. }
    function BaseLights: TLightInstancesList;

    { Statistics about last rendering frame. See TRenderStatistics docs. }
    function Statistics: TRenderStatistics;

    { Background color, displayed behind the 3D world.
      Unless the MainScene has a Background node defined, in which
      case the Background (colored and/or textured) of the 3D scene is used.

      Black by default. }
    property BackgroundColor: TCastleColor
      read FBackgroundColor write FBackgroundColor;

    { Current 3D triangle under the mouse cursor.
      Updated in every mouse move. May be @nil. }
    function TriangleHit: PTriangle;
  published
    { Camera used to render this viewport.

      Note this property may be @nil before rendering.
      If you don't assign anything here, we'll create a default camera
      when necessary (usually at the ApplyProjection which
      happens before the rendering).
      Use RequiredCamera instead of this property to get a camera
      that is never @nil.

      For many purposes, you can directly operate on this camera,
      for example you can change it's @link(TCamera.Position Position).
      An exception to this is assigning events to the camera instance.
      The scene manager or viewport will "hijack" some Camera events:
      TCamera.OnVisibleChange, TWalkCamera.OnMoveAllowed,
      TWalkCamera.OnHeight, TCamera.OnCursorChange.
      We will handle them in a proper way. Do not assign them yourself.

      @italic(Comments for TCastleViewport only:)
      The TCastleViewport's camera is slightly less important than
      TCastleSceneManager.Camera, because TCastleSceneManager.Camera may be treated
      as a "central" camera. Viewport's camera may not (because you may
      have many viewports and they all deserve fair treatment).
      So e.g. headlight is done only from TCastleSceneManager.Camera
      (for mirror textures, there must be one headlight for your 3D world).
      Also VRML/X3D ProximitySensors receive events only from
      TCastleSceneManager.Camera.

      @seealso TCastleSceneManager.OnCameraChanged }
    property Camera: TCamera read FCamera write SetCamera;

    { For scene manager: you can pause everything inside your 3D world,
      for viewport: you can make the camera of this viewpoint paused
      (not responsive).

      @italic(For scene manager:)

      "Paused" means that no events (key, mouse, @link(Update)) are passed to any
      @link(TCastleSceneManager.Items) or the @link(Camera).
      This is suitable if you really want to totally, unconditionally,
      make your 3D world view temporary still (for example,
      useful when entering some modal dialog box and you want
      3D scene to behave as a still background).

      You can of course still directly change some scene property,
      and then 3D world will change.
      But no change will be initialized automatically by scene manager events.

      @italic(See also): For less drastic pausing methods,
      there are other methods of pausing / disabling
      some events processing for the 3D world:

      @unorderedList(
        @item(You can set TCastleScene.TimePlaying or TCastlePrecalculatedAnimation.TimePlaying
          to @false. This is roughly equivalent to not running their
          @link(Update) methods.
          This means that time will "stand still" for them,
          so their animations will not play. Although they may
          still react and change in response to mouse clicks / key presses,
          if TCastleScene.ProcessEvents.)

        @item(You can set TCastleScene.ProcessEvents to @false.
          This means that scene will not receive and process any
          key / mouse and other events (through VRML/X3D sensors).
          Some animations (not depending on VRML/X3D events processing)
          may still run, for example MovieTexture will still animate,
          if only TCastleScene.TimePlaying.)

        @item(For cameras, you can set @code(TCamera.Input := []) to ignore
          key / mouse clicks.)
      ) }
    property Paused: boolean read FPaused write SetPaused default false;

    { See Render3D method. }
    property OnRender3D: TRender3DEvent read FOnRender3D write FOnRender3D;

    { Should we render with shadow volumes.
      You can change this at any time, to switch rendering shadows on/off.

      This works only if OpenGL context actually can render shadow volumes,
      checked by GLFeatures.ShadowVolumesPossible, which means that you have
      to initialize OpenGL context with stencil buffer.

      The shadow volumes algorithm is used only if shadow caster
      is 2-manifold, that is has a correctly closed volume.
      Also you need a light source
      marked as the main shadow volumes light (shadowVolumes = shadowVolumesMain = TRUE).
      See [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows]
      for details. }
    property ShadowVolumes: boolean
      read FShadowVolumes write FShadowVolumes default DefaultShadowVolumes;

    { Actually draw the shadow volumes to the color buffer, for debugging.
      If shadows are rendered (see GLFeatures.ShadowVolumesPossible and ShadowVolumes),
      you can use this to actually see shadow volumes, for debug / demo
      purposes. Shadow volumes will be rendered on top of the scene,
      as yellow blended polygons. }
    property ShadowVolumesRender: boolean read FShadowVolumesRender write FShadowVolumesRender default false;

    { If yes then the scene background will be rendered wireframe,
      over the background filled with BackgroundColor.

      There's a catch here: this works only if the background is actually
      internally rendered as a geometry. If the background is rendered
      by clearing the screen (this is an optimized case of sky color
      being just one simple color, and no textures),
      then it will just cover the screen as normal, like without wireframe.
      This is uncertain situation anyway (what should the wireframe
      look like in this case anyway?), so I don't consider it a bug.

      Useful especially for debugging when you want to see how your background
      geometry looks like. }
    property BackgroundWireframe: boolean
      read FBackgroundWireframe write FBackgroundWireframe default false;

    { If yes then we will not draw any background, letting the window contents
      underneath be visible (in places where we do not draw our own 3D geometry,
      or where our own geometry is transparent, e.g. by Material.transparency).
      For this to make sense, make sure that you always place some other 2D control
      under this viewport, that actually draws something predictable underneath.

      The normal background, derived from @link(Background) will be ignored.
      We will also not do any RenderContext.Clear on color buffer.
      Also BackgroundWireframe and BackgroundColor doesn't matter in this case. }
    property Transparent: boolean read FTransparent write FTransparent default false;

    { When @true then headlight is always rendered from custom viewport's
      (TCastleViewport) camera, not from central camera (the one in scene manager).
      This is meaningless in TCastleSceneManager.

      By default this is @false, which means that when rendering
      custom viewport (TCastleViewport) we render headlight from
      TCastleViewport.SceneManager.Camera (not from current viewport's
      TCastleViewport.Camera). On one hand, this is sensible: there is exactly one
      headlight in your 3D world, and it shines from a central camera
      in SceneManager.Camera. When SceneManager.Camera is @nil (which
      may happen if you set SceneManager.DefaultViewport := false and you
      didn't assign SceneManager.Camera explicitly) headlight is never done.
      This means that when observing 3D world from other cameras,
      you will see a light shining from SceneManager.Camera.
      This is also the only way to make headlight lighting correctly reflected
      in mirror textures (like GeneratedCubeMapTexture) --- since we render
      to one mirror texture, we need a knowledge of "cental" camera for this.

      When this is @true, then each viewport actually renders headlight
      from it's current camera. This means that actually each viewport
      has it's own, independent headlight (althoug they all follow VRML/X3D
      NavigationInfo.headlight and KambiNavigationInfo settings).
      This may allow you to light your view better (if you only use
      headlight to "just make the view brighter"), but it's not entirely
      correct (in particular, mirror reflections of the headlight are
      undefined then).

      @deprecated This is deprecated, since HeadlightFromViewport = @true
      is not really nicely defined, and it's not practically that useful either. }
    property HeadlightFromViewport: boolean
      read FHeadlightFromViewport write FHeadlightFromViewport default false; deprecated;

    { Let MainScene.GlobalLights shine on every 3D object, not only
      MainScene. This is an easy way to lit your whole world with lights
      defined inside MainScene file. Be sure to set lights global=TRUE.

      Note that for now this assumes that MainScene coordinates equal
      world coordinates. This means that you should not transform
      the MainScene, it should be placed inside @link(TCastleSceneManager.Items)
      without wrapping in any T3DTransform. }
    property UseGlobalLights: boolean
      read FUseGlobalLights write FUseGlobalLights default DefaultUseGlobalLights;

    { Help user to activate pointing device sensors and pick items.
      Every time you press or release Input_Interact (by default
      just left mouse button), we look if current mouse position hits 3D object
      that actually does something on activation. 3D objects may do various stuff
      inside T3D.PointingDeviceActivate, generally this causes various
      picking/interaction with the 3D object (like pulling a level, opening a door),
      possibly dragging, possibly with the help of VRML/X3D pointing device
      and drag sensors.

      When this is @true, we try harder to hit some 3D object that handles
      PointingDeviceActivate. If there's nothing interesting under mouse,
      we will retry a couple of other positions arount the current mouse.

      This should be usually used when you use TWalkCamera.MouseLook,
      or other navigation when mouse cursor is hidden.
      It allows user to only approximately look at interesting item and hit
      interaction button or key.
      Otherwise, activating a small 3D object is difficult,
      as you don't see the mouse cursor. }
    property ApproximateActivation: boolean
      read FApproximateActivation write FApproximateActivation default false;

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

    { Enable built-in SSAO screen effect in the world. }
    property ScreenSpaceAmbientOcclusion: boolean
      read FScreenSpaceAmbientOcclusion write SetScreenSpaceAmbientOcclusion
      default DefaultScreenSpaceAmbientOcclusion;

    { Viewports are by default full size (fill the parent container completely). }
    property FullSize default true;
  end;

  TCastleAbstractViewportList = class(specialize TFPGObjectList<TCastleAbstractViewport>)
  public
    { Does any viewport on the list has shadow volumes all set up? }
    function UsesShadowVolumes: boolean;
  end;

  { Scene manager that knows about all 3D things inside your world.

    Single scenes/models (like TCastleScene or TCastlePrecalculatedAnimation instances)
    can be rendered directly, but it's not always comfortable.
    Scenes have to assume that they are "one of the many" inside your 3D world,
    which means that multi-pass rendering techniques have to be implemented
    at a higher level. This concerns the need for multiple passes from
    the same camera (for shadow volumes) and multiple passes from different
    cameras (for generating textures for shadow maps, cube map environment etc.).

    Scene manager overcomes this limitation. A single SceneManager object
    knows about all 3D things in your world, and renders them all for you,
    taking care of doing multiple rendering passes for particular features.
    Naturally, it also serves as container for all your visible 3D scenes.

    @link(Items) property keeps a tree of T3D objects.
    All our 3D objects, like TCastleSceneCore (and so also TCastleScene)
    and TCastlePrecalculatedAnimation descend from
    T3D, and you can add them to the scene manager.
    And naturally you can implement your own T3D descendants,
    representing any 3D (possibly dynamic, animated and even interactive) object.

    TCastleSceneManager.Render can assume that it's the @italic(only) manager rendering
    to the screen (although you can safely render more 3D geometry *after*
    calling TCastleSceneManager.Render). So it's Render method takes care of

    @unorderedList(
      @item(clearing the screen,)
      @item(rendering the background of the scene,)
      @item(rendering the headlight,)
      @item(rendering the scene from given camera,)
      @item(and making multiple passes for shadow volumes and generated textures.)
    )

    For some of these features, you'll have to set the @link(MainScene) property.

    This is a TUIControl descendant, which means it's advised usage
    is to add this to TCastleWindowCustom.Controls or TCastleControlCustom.Controls.
    This passes relevant TUIControl events to all the T3D objects inside.
    Note that even when you set DefaultViewport = @false
    (and use custom viewports, by TCastleViewport class, to render your 3D world),
    you still should add scene manager to the controls list
    (this allows e.g. 3D items to receive @link(Update) events). }
  TCastleSceneManager = class(TCastleAbstractViewport)
  private
    FMainScene: TCastleScene;
    FItems: T3DWorld;
    FDefaultViewport: boolean;
    FViewports: TCastleAbstractViewportList;
    FTimeScale: Single;

    FOnCameraChanged: TNotifyEvent;
    FOnBoundViewpointChanged, FOnBoundNavigationInfoChanged: TNotifyEvent;
    FMoveLimit: TBox3D;
    FShadowVolumeRenderer: TGLShadowVolumeRenderer;

    FMouseRayHit: TRayCollision;

    FPlayer: TPlayer;

    { calculated by every PrepareResources }
    ChosenViewport: TCastleAbstractViewport;
    NeedsUpdateGeneratedTextures: boolean;

    FWater: TBox3D;
    FOnMoveAllowed: TWorldMoveAllowedEvent;
    LastSoundRefresh: TTimerResult;
    DefaultHeadlightNode: TDirectionalLightNode;

    ScheduledVisibleChangeNotification: boolean;
    ScheduledVisibleChangeNotificationChanges: TVisibleChanges;

    { Call at the beginning of Render (from both scene manager and custom viewport),
      to make sure UpdateGeneratedTextures was done before actual drawing.
      It *can* carelessly change the OpenGL projection matrix (but not viewport). }
    procedure UpdateGeneratedTexturesIfNeeded;

    procedure SetMainScene(const Value: TCastleScene);
    procedure SetDefaultViewport(const Value: boolean);

    procedure ItemsVisibleChange(const Sender: T3D; const Changes: TVisibleChanges);

    { scene callbacks }
    procedure SceneBoundViewpointChanged(Scene: TCastleSceneCore);
    procedure SceneBoundViewpointVectorsChanged(Scene: TCastleSceneCore);
    procedure SceneBoundNavigationInfoChanged(Scene: TCastleSceneCore);

    procedure SetMouseRayHit(const Value: TRayCollision);
    function MouseRayHitContains(const Item: T3D): boolean;
    procedure SetPlayer(const Value: TPlayer);
  protected
    FSectors: TSectorList;
    Waypoints: TWaypointList;

    procedure SetCamera(const Value: TCamera); override;

    { Triangles to ignore by all collision detection in scene manager.
      The default implementation in this class resturns always @false,
      so nothing is ignored. You can override it e.g. to ignore your "water"
      material, when you want player to dive under the water. }
    function CollisionIgnoreItem(const Sender: TObject;
      const Triangle: P3DTriangle): boolean; virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function CameraMoveAllowed(ACamera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; override;
    function CameraHeight(ACamera: TWalkCamera; const Position: TVector3Single;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; override;
    function CameraRayCollision(const RayOrigin, RayDirection: TVector3Single): TRayCollision; override;
    procedure CameraVisibleChange(ACamera: TObject); override;

    function GetItems: T3DWorld; override;
    function GetMainScene: TCastleScene; override;
    function GetShadowVolumeRenderer: TGLShadowVolumeRenderer; override;
    function GetMouseRayHit: TRayCollision; override;
    function GetHeadlightCamera: TCamera; override;
    function GetPlayer: TPlayer; override;
    function GetTimeScale: Single; override;
    function PointingDeviceActivate(const Active: boolean): boolean; override;
    function PointingDeviceMove(const RayOrigin, RayDirection: TVector3Single): boolean; override;
    { Called when PointingDeviceActivate was not handled by any 3D object.
      You can override this to make a message / sound signal to notify user
      that his Input_Interact click was not successful. }
    procedure PointingDeviceActivateFailed(const Active: boolean); virtual;

    { Handle pointing device (mouse) activation/deactivation event over a given 3D
      object. See T3D.PointingDeviceActivate method for description how it
      should be handled. Default implementation in TCastleSceneManager
      just calls T3D.PointingDeviceActivate. }
    function PointingDeviceActivate3D(const Item: T3D; const Active: boolean;
      const Distance: Single): boolean; virtual;

    { Handle OnMoveAllowed and default MoveLimit algorithm.
      See the description of OnMoveAllowed property for information.

      When this is called, collision detection determined that this move
      is allowed. The default implementation in TCastleSceneManager
      calculates the result using the algorithm described at the MoveLimit
      property, then calls OnMoveAllowed event. }
    function MoveAllowed(const OldPosition, NewPosition: TVector3Single;
      const BecauseOfGravity: boolean): boolean; virtual;

    procedure BoundNavigationInfoChanged; virtual;
    procedure BoundViewpointChanged; virtual;
    function Headlight: TAbstractLightNode; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GLContextOpen; override;
    procedure GLContextClose; override;

    { Prepare resources, to make various methods (like @link(Render))
      execute fast.

      If DisplayProgressTitle <> '', we will display progress bar during
      loading. This is especially useful for long precalculated animations
      (TCastlePrecalculatedAnimation with a lot of ScenesCount), they show nice
      linearly increasing progress bar. }
    procedure PrepareResources(const DisplayProgressTitle: string = '');
    procedure PrepareResources(const Item: T3D;
      const DisplayProgressTitle: string = ''); virtual;

    procedure BeforeRender; override;
    procedure Render; override;

    { What changes happen when camera changes.
      You may want to use it when calling Scene.CameraChanged.

      Implementation in this class is correlated with RenderHeadlight. }
    function CameraToChanges: TVisibleChanges; virtual;

    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;

    function CreateDefaultCamera(AOwner: TComponent): TCamera; override;

    { Where the 3D items (player, creatures, items) can move,
      and where the gravity works. In case of 1st-person view
      (always, for now) limiting the player position also implies limiting
      the camera position.
      Intuitively, this is the "sensible" part of 3D space where normal physics
      should work.

      TODO: When you activate 3rd-person camera (not implemented yet),
      this limit will apply to the Player.Position, not Camera.Position.

      @unorderedList(
        @item(When MoveLimit is an empty box (this is the default situation)
          then movement is limited to not fall because of gravity
          outside of Items.BoundingBox. Still, we can freely move anywhere
          (only gravity effect is limited to the Items.BoundingBox).

          This is the safest behavior for general 3D model browsers,
          it prevents camera from falling into an infinite abyss of our 3D space,
          since gravity will always stop at the Items.BoundingBox border.)

        @item(When MoveLimit is not an empty box,
          then position cannot go outside of this box.

          Note that the TGameSceneManager.LoadLevel always,
          automatically, assigns this property to be non-empty.
          It's either determined by CasMoveLimit placeholder
          in the level 3D model, or it's automatically calculated
          to include level bounding box + some space for flying.)
      ) }
    property MoveLimit: TBox3D read FMoveLimit write FMoveLimit;

    { Renderer of shadow volumes. You can use this to optimize rendering
      of your shadow quads in RenderShadowVolume, and you can control
      it's statistics (TGLShadowVolumeRenderer.Count and related properties).

      This is internally initialized by scene manager. It's @nil when
      OpenGL context is not yet initialized (or scene manager is not
      added to @code(Controls) list yet). }
    property ShadowVolumeRenderer: TGLShadowVolumeRenderer
      read FShadowVolumeRenderer;

    { Current 3D objects under the mouse cursor.
      Updated in every mouse move. May be @nil. }
    property MouseRayHit: TRayCollision read FMouseRayHit;

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
    property Viewports: TCastleAbstractViewportList read FViewports;

    { Up vector, according to gravity. Gravity force pulls in -GravityUp direction. }
    function GravityUp: TVector3Single;

    { Sectors and waypoints of this world, for AI in 3D.
      Initialized by TGameSceneManager.LoadLevel.
      @nil if you never call TGameSceneManager.LoadLevel.

      A generic AI code should work regardless if these are @nil or not.
      But if you're making a game and you know you will always call
      TGameSceneManager.LoadLevel, you can just use them straight away. }
    property Sectors: TSectorList read FSectors;

    { Water volume in the scene. It may be used by various 3D objects
      to indicate appropriate behavior --- some things swim,
      some things drown and such. For now, this is only used by TPlayer
      class to detect swimming (and make appropriate sounds, special rendering,
      drowning and such).

      For now, this is just a simple TBox3D. It will
      be extended to represent a set of flexible 3D volumes in the future.

      Empty initially. Initialize it however you want. }
    property Water: TBox3D read FWater write FWater;

    function Rect: TRectangle; override;
  published
    { Time scale used when not @link(Paused). }
    property TimeScale: Single read FTimeScale write FTimeScale default 1;

    { Tree of 3D objects within your world. This is the place where you should
      add your scenes to have them handled by scene manager.
      You may also set your main TCastleScene (if you have any) as MainScene.

      T3DList is also T3D instance, so yes --- this may be a tree
      of T3D, not only a flat list. }
    property Items: T3DWorld read FItems;

    { The main scene of your 3D world. It's not necessary to set this
      (after all, your 3D world doesn't even need to have any TCastleScene
      instance). This @italic(must be) also added to our @link(Items)
      (otherwise things will work strangely).

      When set, this is used for a couple of things:

      @unorderedList(
        @item Decides what headlight is used (by TCastleScene.Headlight).

        @item(Decides what background is rendered.
          @italic(Notes for implementing descendants of this class:)
          You can change this by overriding Background method.)

        @item(Decides if, and where, the main light casting shadows is.
          @italic(Notes for implementing descendants of this class:)
          You can change this by overriding MainLightForShadows method.)

        @item(Determines projection for viewing (if you use
          default @link(CalculateProjection) implementation).)

        @item(Synchronizes our @link(Camera) with VRML/X3D viewpoints
          and navigation info.
          This means that @link(Camera) will be updated when VRML/X3D events
          change current Viewpoint or NavigationInfo, for example
          you can animate the camera by animating the viewpoint
          (or it's transformation) or bind camera to a viewpoint.

          Note that scene manager "hijacks" some Scene events:
          TCastleSceneCore.OnBoundViewpointVectorsChanged,
          TCastleSceneCore.ViewpointStack.OnBoundChanged,
          TCastleSceneCore.NavigationInfoStack.OnBoundChanged
          for this purpose. If you want to know when viewpoint changes,
          you can use scene manager's event OnBoundViewpointChanged,
          OnBoundNavigationInfoChanged.)
      )

      The above stuff is only sensible when done once per scene manager,
      that's why we need MainScene property to indicate this.
      (We cannot just use every 3D object from @link(Items) for this.)

      Freeing MainScene will automatically set this to @nil. }
    property MainScene: TCastleScene read FMainScene write SetMainScene;

    { Called on any camera change. Exactly when TCamera generates it's
      OnVisibleChange event. }
    property OnCameraChanged: TNotifyEvent read FOnCameraChanged write FOnCameraChanged;

    { Called when bound Viewpoint node changes.
      Called exactly when TCastleSceneCore.ViewpointStack.OnBoundChanged is called. }
    property OnBoundViewpointChanged: TNotifyEvent read FOnBoundViewpointChanged write FOnBoundViewpointChanged;

    { Called when bound NavigationInfo changes (to a different node,
      or just a field changes). }
    property OnBoundNavigationInfoChanged: TNotifyEvent read FOnBoundNavigationInfoChanged write FOnBoundNavigationInfoChanged;

    { Should we render the 3D world in a default viewport that covers
      the whole window. This is usually what you want. For more complicated
      uses, you can turn this off, and use explicit TCastleViewport
      (connected to this scene manager by TCastleViewport.SceneManager property)
      for making your world visible. }
    property DefaultViewport: boolean
      read FDefaultViewport write SetDefaultViewport default true;

    { Player in this 3D world. This currently serves various purposes:

      @unorderedList(
        @item(In the 1st person view, this 3D object guides the camera and
          it never collides with the camera. That is, our CameraMoveAllowed
          and similar methods simply call Player.MoveAllowed,
          that in turn calls World.WorldMoveAllowed making sure
          that player is temporarily disabled (does not collide with itself).

          TGameSceneManager.LoadLevel will set Player.Camera to
          TCastleSceneManager.Camera. This means that user can directly
          control Player.Camera view (position, direction, up),
          which in turn is always synchronized with Player view (that
          is, TPlayer.Direction always equals TPlayer.Camera.Direction and so on).)

        @item(For simple AI in CastleCreatures, hostile creatures will attack
          this player. So this determines the target position that
          creatures try to reach, where they shoot missiles etc.
          More advanced AI, with friendlies/companions, or cooperating
          factions of creatures, may have other mechanisms to determine who
          wants to attack who.)

        @item(For items on level in CastleItems, this player will pick up the items
          lying on the ground, and will be able to equip weapons.
          This functionality may be generalized in the future, to allow
          anyone to pick up and carry and equip items.)
      )
    }
    property Player: TPlayer read FPlayer write SetPlayer;

    (*Enable or disable movement of the player, items and creatures.
      This applies to all 3D objects using T3D.WorldMoveAllowed for movement.
      In case of 1st-person view (always for now),
      limiting the player position also implies limiting the camera position.

      When this event is called at all, the basic collision detection
      already decided that the move is allowed (so object does not collide with
      other collidable 3D features).
      You can now implement additional rules to say when the move is,
      or is not, allowed.

      Callback parameters:

      @unorderedList(
        @item(@bold(Allowed):

          Initially, the Allowed parameter is set following the algorithm
          described at the MoveLimit property.
          Your event can use this, and e.g. do something like

          @longCode(#  Allowed := Allowed and (my custom move rule); #)

          Or you can simply ignore the default Allowed value,
          thus ignoring the algorithm described at the MoveLimit property,
          and simply always set Allowed to your own decision.
          For example, setting

          @longCode(#  Allowed := true; #)

          will make gravity and movement work everywhere.)

        @item(@bold(BecauseOfGravity):

          @true if this move was caused by gravity, that is: given object
          is falling down. You can use this to limit gravity to some box,
          but keep other movement unlimited, like

          @longCode(#
            { Allow movement everywhere, but limit gravity to a box. }
            Allowed := (not BecauseOfGravity) or MyGravityBox.PointInside(NewPos);
          #)
        )
      ) *)
    property OnMoveAllowed: TWorldMoveAllowedEvent
      read FOnMoveAllowed write FOnMoveAllowed;
  end;

  { Custom 2D viewport showing 3D world. This uses assigned SceneManager
    to show 3D world on the screen.

    For simple games, using this is not needed, because TCastleSceneManager
    also acts as a viewport (when TCastleSceneManager.DefaultViewport is @true,
    which is the default).
    Using custom viewports (implemented by this class)
    is useful when you want to have more than one viewport showing
    the same 3D world. Different viewports may have different cameras,
    but they always share the same 3D world (in scene manager).

    You can control the size of this viewport by
    @link(TUIControlSizeable.FullSize FullSize),
    @link(TUIControl.Left Left),
    @link(TUIControl.Bottom Bottom),
    @link(TUIControlSizeable.Width Width),
    @link(TUIControlSizeable.Height Height) properties. For custom
    viewports, you often want to set FullSize = @false
    and control viewport's position and size explicitly.

    Viewports may be overlapping, that is one viewport may (partially)
    obscure another viewport. Just like with any other TUIControl,
    position of viewport on the Controls list
    (like TCastleControlCustom.Controls or TCastleWindowCustom.Controls)
    is important: Controls are specified in the back-to-front order.
    That is, if the viewport A may obscure viewport B,
    then A must be after B on the Controls list.

    The viewports are a cool feature for many cases.
    For example typical 3D modeling programs have 4 viewports to view the model
    from various sides.
    Or you can make a split-screen game, played by 2 people on a single monitor.
    Or you can show in a 3D FPS game an additional view from some security camera,
    or from a flying rocket.
    For examples of using viewports see:

    @unorderedList(
      @item(Explanation with an example:
        http://castle-engine.sourceforge.net/tutorial_2d_user_interface.php#section_viewport)
      @item(Example in engine sources: examples/3d_rendering_processing/multiple_viewports.lpr)
      @item(Example in engine sources: examples/fps_game/)
    )
  }
  TCastleViewport = class(TCastleAbstractViewport)
  private
    FSceneManager: TCastleSceneManager;
    procedure SetSceneManager(const Value: TCastleSceneManager);
    procedure CheckSceneManagerAssigned;
  protected
    function GetItems: T3DWorld; override;
    function GetMainScene: TCastleScene; override;
    function GetShadowVolumeRenderer: TGLShadowVolumeRenderer; override;
    function GetMouseRayHit: TRayCollision; override;
    function GetHeadlightCamera: TCamera; override;
    function GetPlayer: TPlayer; override;
    function GetTimeScale: Single; override;
    function PointingDeviceActivate(const Active: boolean): boolean; override;
    function PointingDeviceMove(const RayOrigin, RayDirection: TVector3Single): boolean; override;

    function CameraMoveAllowed(ACamera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean; override;
    function CameraHeight(ACamera: TWalkCamera; const Position: TVector3Single;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; override;
    function CameraRayCollision(const RayOrigin, RayDirection: TVector3Single): TRayCollision; override;
    procedure CameraVisibleChange(ACamera: TObject); override;
    function Headlight: TAbstractLightNode; override;
  public
    destructor Destroy; override;

    procedure Render; override;

    function CreateDefaultCamera(AOwner: TComponent): TCamera; override;
  published
    property SceneManager: TCastleSceneManager read FSceneManager write SetSceneManager;
  end;

procedure Register;

var
  { Key/mouse combination to interact with clickable things in 3D world.
    More precisely, this input will activate pointing device sensors in VRML/X3D,
    which are used to touch (click) or drag 3D things.
    By default this is left mouse button click.

    You can change it to any other mouse button or even to key combination.
    Simply change properties like TInputShortcut.Key1
    or TInputShortcut.MouseButtonUse. }
  Input_Interact: TInputShortcut;

  { Key/mouse combination to operate on Player and it's inventory.
    They are used only when Player is assigned, and only when it's
    not Dead and not Blocked (see T3DAlive.Dead, TPlayer.Blocked).
    Also other TCastleAbstractViewport rules for processing
    inputs must be satisfied, of course (TCastleAbstractViewport must exist,
    according to TCastleAbstractViewport.GetExists, and not be paused, see
    TCastleAbstractViewport.Paused). The event must also not be handled
    first by something else, like camera.
    @groupBegin }
  Input_Attack: TInputShortcut;
  Input_InventoryShow: TInputShortcut; //< No key/mouse associated by default.
  Input_InventoryPrevious: TInputShortcut;
  Input_InventoryNext: TInputShortcut;
  Input_UseItem: TInputShortcut;
  Input_DropItem: TInputShortcut; //< No key/mouse associated by default.
  Input_CancelFlying: TInputShortcut; //< No key/mouse associated by default.
  { @groupEnd }

implementation

uses CastleRenderingCamera, CastleGLUtils, CastleProgress,
  CastleLog, CastleStringUtils, CastleSoundEngine, Math,
  CastleGLVersion, CastleShapes;

procedure Register;
begin
  { For engine 3.0.0, TCastleSceneManager is not registered on palette,
    as the suggested usage for everyone is to take TCastleControl with
    scene manager instance already created.
    See castlecontrol.pas comments in Register. }
  { RegisterComponents('Castle', [TCastleSceneManager]); }
end;

{ TManagerRenderParams ------------------------------------------------------- }

constructor TManagerRenderParams.Create;
begin
  inherited;
  FBaseLights[false] := TLightInstancesList.Create;
  FBaseLights[true ] := TLightInstancesList.Create;
end;

destructor TManagerRenderParams.Destroy;
begin
  FreeAndNil(FBaseLights[false]);
  FreeAndNil(FBaseLights[true ]);
  inherited;
end;

function TManagerRenderParams.BaseLights(Scene: T3D): TAbstractLightInstancesList;
begin
  { Use Scene.Shared, not just Scene, for comparison.
    This way all scenes within a single TCastlePrecalculatedAnimation
    are treated the same, which makes UseGlobalLights work correctly
    in case when you render TCastlePrecalculatedAnimation and MainScene
    refers to the 1st animation scene.

    Testcase: demo-models/castle-anim-frames/simple/raptor.castle-anim-frames,
    without this fix the lights woud be duplicated on non-first animation scene. }
  Result := FBaseLights[(Scene.Shared = MainScene) or Scene.ExcludeFromGlobalLights];
end;

{ TCastleAbstractViewport ------------------------------------------------------- }

constructor TCastleAbstractViewport.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundColor := Black;
  FUseGlobalLights := DefaultUseGlobalLights;
  FRenderParams := TManagerRenderParams.Create;
  FShadowVolumes := DefaultShadowVolumes;
  DistortFieldOfViewY := 1;
  DistortViewAspect := 1;
  FullSize := true;
end;

destructor TCastleAbstractViewport.Destroy;
begin
  { unregister self from Camera callbacs, etc.

    This includes setting FCamera to nil.
    Yes, this setting FCamera to nil is needed, it's not just paranoia.

    Consider e.g. when our Camera is owned by Self
    (e.g. because it was created in ApplyProjection by CreateDefaultCamera).
    This means that this camera will be freed in "inherited" destructor call
    below. Since we just did FCamera.RemoveFreeNotification, we would have
    no way to set FCamera to nil, and FCamera would then remain as invalid
    pointer.

    And when SceneManager is freed it sends a free notification
    (this is also done in "inherited" destructor) to TCastleWindowCustom instance,
    which causes removing us from TCastleWindowCustom.Controls list,
    which causes SetContainer(nil) call that tries to access Camera.

    This scenario would cause segfault, as FCamera pointer is invalid
    at this time. }
  Camera := nil;

  FreeAndNil(FRenderParams);

  inherited;
end;

procedure TCastleAbstractViewport.SetCamera(const Value: TCamera);
begin
  { This camera @italic(cannot) be inside some other container
    (like on TCastleWindowCustom.Controls or TCastleControlCustom.Controls list),
    and even cannot be now (TCamera is not a TUIControl anymore).

    Scene manager / viewport will handle passing events to the camera on it's own,
    we will also pass our own Container to Camera.Container.
    This is desired, this way events are correctly passed
    and interpreted before passing them to 3D objects.
    And this way we avoid the question whether camera should be before
    or after the scene manager / viewport on the Controls list (as there's really
    no perfect ordering for them).

    TODO: In the future it should be possible (even encouraged) to assign
    one of your custom viewport cameras also to TCastleSceneManager.Camera.
    It should also be possible to share one camera instance among a couple
    of viewports.
    For now, it doesn't work (last viewport/scene manager will hijack some
    camera events making it not working in other ones). }

  if FCamera <> Value then
  begin
    { Check csDestroying, as this may be called from Notification,
      which may be called by camera destructor *after* TUniversalCamera
      after freed it's fields. }
    if (FCamera <> nil) and not (csDestroying in FCamera.ComponentState) then
    begin
      FCamera.OnVisibleChange := nil;
      FCamera.OnCursorChange := nil;
      if FCamera is TWalkCamera then
      begin
        TWalkCamera(FCamera).OnMoveAllowed := nil;
        TWalkCamera(FCamera).OnHeight := nil;
      end else
      if FCamera is TUniversalCamera then
      begin
        TUniversalCamera(FCamera).Walk.OnMoveAllowed := nil;
        TUniversalCamera(FCamera).Walk.OnHeight := nil;
      end;

      FCamera.RemoveFreeNotification(Self);
      FCamera.Container := nil;
    end;

    FCamera := Value;

    if FCamera <> nil then
    begin
      { Unconditionally change FCamera.OnVisibleChange callback,
        to override TCastleWindowCustom / TCastleControlCustom that also try
        to "hijack" this camera's event. }
      FCamera.OnVisibleChange := @CameraVisibleChange;
      FCamera.OnCursorChange := @RecalculateCursor;
      if FCamera is TWalkCamera then
      begin
        TWalkCamera(FCamera).OnMoveAllowed := @CameraMoveAllowed;
        TWalkCamera(FCamera).OnHeight := @CameraHeight;
      end else
      if FCamera is TUniversalCamera then
      begin
        TUniversalCamera(FCamera).Walk.OnMoveAllowed := @CameraMoveAllowed;
        TUniversalCamera(FCamera).Walk.OnHeight := @CameraHeight;
      end;

      FCamera.FreeNotification(Self);
      FCamera.Container := Container;
      if ContainerSizeKnown then
        FCamera.Resize;
    end;
  end;
end;

procedure TCastleAbstractViewport.SetContainer(const Value: TUIContainer);
begin
  inherited;

  { Keep Camera.Container always the same as our Container }
  if Camera <> nil then
    Camera.Container := Container;
end;

procedure TCastleAbstractViewport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FCamera then
    begin
      { set to nil by SetCamera, to clean nicely }
      Camera := nil;
    end;
  end;
end;

procedure TCastleAbstractViewport.Resize;
begin
  inherited;

  if Camera <> nil then
    Camera.Resize;
end;

function TCastleAbstractViewport.PlayerNotBlocked: boolean;
var
  P: TPlayer;
begin
  P := GetPlayer;
  Result := (P = nil) or (not (P.Blocked or P.Dead));
end;

function TCastleAbstractViewport.Press(const Event: TInputPressRelease): boolean;
var
  P: TPlayer;
begin
  Result := inherited;
  if Result or Paused or (not GetExists) then Exit;

  { Call PointingDeviceMove, set MouseHitRay and such --- otherwise
    the 1st mouse down event over a 3D object (like a TouchSensor) will be ignored
    if it happens before any mouse move (which is normal on touch devices).
    OldX,OldY and NewX,NewY are equal for the fake Motion call,
    in case a camera would base some movement based on the position delta. }
  Motion(InputMotion(Container.MousePosition, Container.MousePosition, Container.MousePressed, 0));

  Result := GetItems.Press(Event);
  if Result then Exit;

  if PlayerNotBlocked and Input_Interact.IsEvent(Event) then
  begin
    Result := PointingDeviceActivate(true);
    if Result then Exit;
  end;

  P := GetPlayer;
  if (P <> nil) and not (P.Blocked or P.Dead) then
  begin
    if Input_Attack.IsEvent(Event) then
      begin Result := ExclusiveEvents; P.Attack; end;
    if Input_CancelFlying.IsEvent(Event) then
      begin Result := ExclusiveEvents; P.Flying := false; end;
    if Input_InventoryShow.IsEvent(Event) then
      begin Result := ExclusiveEvents; P.InventoryVisible := not P.InventoryVisible; end;
    if Input_InventoryPrevious.IsEvent(Event) then
      begin Result := ExclusiveEvents; P.ChangeInventoryCurrentItem(-1); end;
    if Input_InventoryNext.IsEvent(Event) then
      begin Result := ExclusiveEvents; P.ChangeInventoryCurrentItem(+1); end;
    if Input_DropItem.IsEvent(Event) then
      begin Result := ExclusiveEvents; P.DropCurrentItem; end;
    if Input_UseItem.IsEvent(Event) then
      begin Result := ExclusiveEvents; P.UseCurrentItem; end;
    if Result then Exit;
  end;

  { Let Camera only work after PointingDeviceActivate, to let pointing
    device sensors under camera work, even when camera allows to navigate
    by dragging. }
  if Camera <> nil then
  begin
    Result := Camera.Press(Event);
    if Result then Exit;
  end;
end;

function TCastleAbstractViewport.Release(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result or Paused or (not GetExists) then Exit;

  Result := GetItems.Release(Event);
  if Result then Exit;

  if PlayerNotBlocked and Input_Interact.IsEvent(Event) then
  begin
    Result := PointingDeviceActivate(false);
    if Result then Exit;
  end;

  { Let Camera only work after PointingDeviceActivate, to let pointing
    device sensors under camera work, even when camera allows to navigate
    by dragging. }
  if Camera <> nil then
  begin
    Result := Camera.Release(Event);
    if Result then Exit;
  end;
end;

function TCastleAbstractViewport.Motion(const Event: TInputMotion): boolean;
var
  RayOrigin, RayDirection: TVector3Single;
begin
  Result := inherited;
  if (not Result) and (not Paused) and GetExists and (Camera <> nil) then
  begin
    Camera.EnableDragging := not GetItems.Dragging;
    { Do not navigate by dragging (regardless of ciMouseDragging in Camera.Input)
      when we're already dragging a 3D item.
      This means that if you drag X3D sensors like TouchSensor, then your
      dragging will not simultaneously also affect the camera (which would be very
      disorienting). }
    Result := Camera.Motion(Event);
    if not Result then
    begin
      Camera.CustomRay(ScreenRect, Event.Position, FProjection, RayOrigin, RayDirection);
      { TODO: do Result := PointingDeviceMove below? }
      PointingDeviceMove(RayOrigin, RayDirection);
    end;
  end;

  { update the cursor, since 3D object under the cursor possibly changed.

    Accidentaly, this also workarounds the problem of TCastleViewport:
    when the 3D object stayed the same but it's Cursor value changed,
    Items.CursorChange notify only TCastleSceneManager (not custom viewport).
    But thanks to doing RecalculateCursor below, this isn't
    a problem for now, as we'll update cursor anyway, as long as it changes
    only during mouse move. }
  RecalculateCursor(Self);
end;

procedure TCastleAbstractViewport.SetPaused(const Value: boolean);
begin
  if FPaused <> Value then
  begin
    FPaused := Value;
    { update the cursor when Paused changed. }
    RecalculateCursor(Self);
  end;
end;

procedure TCastleAbstractViewport.RecalculateCursor(Sender: TObject);
begin
  if { This may be called from T3DListCore.Notify when removing stuff owned by other
       stuff, in particular during our own destructor when FItems is freed
       and we're in half-destructed state. }
     (csDestroying in GetItems.ComponentState) or
     { When Paused, then Press and Motion events are not passed to Camera,
       or to Items inside. So it's sensible that they also don't control the cursor
       anymore.
       In particular, it means cursor is no longer hidden by Camera.MouseLook
       when the Paused is switched to true. }
     Paused then
  begin
    Cursor := mcDefault;
    Exit;
  end;

  { We have to treat Camera.Cursor specially:
    - mcForceNone because of mouse look means result is unconditionally mcForceNone.
      Other Items.Cursor, MainScene.Cursor etc. is ignored then.
    - otherwise, Camera.Cursor is ignored, show 3D objects cursor. }
  if (Camera <> nil) and (Camera.Cursor = mcForceNone) then
  begin
    Cursor := mcForceNone;
    Exit;
  end;

  { We show mouse cursor from top-most 3D object.
    This is sensible, if multiple 3D scenes obscure each other at the same
    pixel --- the one "on the top" (visible by the player at that pixel)
    determines the mouse cursor.

    We ignore Cursor value of other 3d stuff along
    the MouseRayHit list. Maybe we should browse Cursor values along the way,
    and choose the first non-none? }

  if (GetMouseRayHit <> nil) and
     (GetMouseRayHit.Count <> 0) then
    Cursor := GetMouseRayHit.First.Item.Cursor else
    Cursor := mcDefault;
end;

function TCastleAbstractViewport.TriangleHit: PTriangle;
begin
  if (GetMouseRayHit <> nil) and
     (GetMouseRayHit.Count <> 0) then
    { This should always be castable to TTriangle class. }
    Result := PTriangle(GetMouseRayHit.First.Triangle)
  else
    Result := nil;
end;

procedure TCastleAbstractViewport.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  P: TPlayer;
  S, C: Extended;
  SecondsPassedScaled: Single;
begin
  inherited;

  if Paused or (not GetExists) then
    Exit;

  SecondsPassedScaled := SecondsPassed * GetTimeScale;

  { As for HandleInput: let Camera decide.
    By default, camera (like all TUIControl) has ExclusiveEvents = true
    and so will cause HandleInput := false, so things under this
    viewport do not get keys/mouse events. This is good when you have
    one viewport covering another, like in fps_game. This means pressing
    e.g. the "up arrow key" only moves camera in one viewport.

    Note about Items.Update (called in TCastleSceneManager.Update):
    Our Items.Update do not have HandleInput
    parameter, as it would not be controllable for them: 3D objects do not
    have strict front-to-back order, so we would not know in what order
    call their Update methods, so we have to let many Items handle keys anyway.
    So, it's consistent to just treat 3D objects as "cannot definitely
    mark keys/mouse as handled". Besides, currently 3D objects do not
    get Pressed information (which keys/mouse buttons are pressed) at all,
    so they could not process keys/mouse anyway. }

  if Camera <> nil then
    Camera.Update(SecondsPassedScaled, HandleInput);

  DistortFieldOfViewY := 1;
  DistortViewAspect := 1;
  P := GetPlayer;
  if (P <> nil) and (P.Swimming = psUnderWater) then
  begin
    SickProjectionTime += SecondsPassedScaled;
    SinCos(SickProjectionTime * P.SickProjectionSpeed, S, C);
    DistortFieldOfViewY += C * 0.03;
    DistortViewAspect += S * 0.03;
  end;
end;

function TCastleAbstractViewport.AllowSuspendForInput: boolean;
begin
  Result := (Camera = nil) or Paused or (not GetExists) or Camera.AllowSuspendForInput;
end;

procedure TCastleAbstractViewport.ApplyProjection;
var
  Viewport: TRectangle;
begin
  RequiredCamera; // create Camera if necessary

  { We need to know container size now. }
  Assert(ContainerSizeKnown, ClassName + ' did not receive Resize event yet, cannnot apply OpenGL projection');

  Viewport := ScreenRect;
  glViewport(Viewport);

  FProjection := CalculateProjection;

  { Apply new FProjection values }

  case FProjection.ProjectionType of
    ptPerspective:
      Camera.ProjectionMatrix := PerspectiveProjection(
        DistortFieldOfViewY * FProjection.PerspectiveAngles[1],
        DistortViewAspect * Viewport.Width / Viewport.Height,
        FProjection.ProjectionNear,
        FProjection.ProjectionFar);
    ptOrthographic:
      Camera.ProjectionMatrix := OrthoProjection(
        { Beware: order of OrthoViewpoint.fieldOfView and OrthoDimensions
          is different than typical OpenGL and our OrthoProjection params. }
        FProjection.OrthoDimensions[0],
        FProjection.OrthoDimensions[2],
        FProjection.OrthoDimensions[1],
        FProjection.OrthoDimensions[3],
        FProjection.ProjectionNear,
        FProjection.ProjectionFarFinite);
    else raise EInternalError.Create('TCastleAbstractViewport.ApplyProjection:ProjectionType?');
  end;

  { Calculate BackgroundSkySphereRadius here,
    using ProjectionFar that is *not* ZFarInfinity }
  if GetMainScene <> nil then
    GetMainScene.BackgroundSkySphereRadius :=
      TBackground.NearFarToSkySphereRadius(
        FProjection.ProjectionNear,
        FProjection.ProjectionFarFinite,
        GetMainScene.BackgroundSkySphereRadius);
end;

function TCastleAbstractViewport.CalculateProjection: TProjection;
var
  Box: TBox3D;
  Viewport: TRectangle;
  ViewpointNode: TAbstractViewpointNode;
  PerspectiveFieldOfView: Single;

  procedure DoPerspective;
  begin
    { Only perspective projection supports z far in infinity. }
    if GLFeatures.ShadowVolumesPossible and ShadowVolumes then
      Result.ProjectionFar := ZFarInfinity;

    { Note that Result.PerspectiveAngles is already calculated here,
      because we calculate correct PerspectiveAngles regardless
      of whether we actually apply perspective or orthogonal projection. }
  end;

  procedure DoOrthographic;
  var
    FieldOfView: TSingleList;
    MaxSize: Single;
  begin
    MaxSize := Box.MaxSize(false, { any dummy value } 1.0);

    { default Result.OrthoDimensions, when not OrthoViewpoint }
    Result.OrthoDimensions[0] := -MaxSize / 2;
    Result.OrthoDimensions[1] := -MaxSize / 2;
    Result.OrthoDimensions[2] :=  MaxSize / 2;
    Result.OrthoDimensions[3] :=  MaxSize / 2;

    { update OrthoDimensions using OrthoViewpoint.fieldOfView }
    if (ViewpointNode <> nil) and
       (ViewpointNode is TOrthoViewpointNode) then
    begin
      { default OrthoDimensions, for OrthoViewpoint }
      Result.OrthoDimensions[0] := -1;
      Result.OrthoDimensions[1] := -1;
      Result.OrthoDimensions[2] :=  1;
      Result.OrthoDimensions[3] :=  1;

      FieldOfView := TOrthoViewpointNode(ViewpointNode).FdFieldOfView.Items;
      if FieldOfView.Count > 0 then Result.OrthoDimensions[0] := FieldOfView.Items[0];
      if FieldOfView.Count > 1 then Result.OrthoDimensions[1] := FieldOfView.Items[1];
      if FieldOfView.Count > 2 then Result.OrthoDimensions[2] := FieldOfView.Items[2];
      if FieldOfView.Count > 3 then Result.OrthoDimensions[3] := FieldOfView.Items[3];
    end else
    if (ViewpointNode <> nil) and
       (ViewpointNode is TOrthographicCameraNode_1) then
    begin
      Result.OrthoDimensions[0] := -TOrthographicCameraNode_1(ViewpointNode).FdHeight.Value / 2;
      Result.OrthoDimensions[1] := -TOrthographicCameraNode_1(ViewpointNode).FdHeight.Value / 2;
      Result.OrthoDimensions[2] :=  TOrthographicCameraNode_1(ViewpointNode).FdHeight.Value / 2;
      Result.OrthoDimensions[3] :=  TOrthographicCameraNode_1(ViewpointNode).FdHeight.Value / 2;
    end;

    TOrthoViewpointNode.AspectFieldOfView(Result.OrthoDimensions,
      Viewport.Width / Viewport.Height);
  end;

var
  ProjectionType: TProjectionType;
begin
  Box := GetItems.BoundingBox;
  Viewport := ScreenRect;

  { calculate ViewpointNode }
  if GetMainScene <> nil then
    ViewpointNode := GetMainScene.ViewpointStack.Top else
    ViewpointNode := nil;

  if (ViewpointNode <> nil) and
     (ViewpointNode is TViewpointNode) then
    PerspectiveFieldOfView := TViewpointNode(ViewpointNode).FdFieldOfView.Value else
  if (ViewpointNode <> nil) and
     (ViewpointNode is TPerspectiveCameraNode_1) then
    PerspectiveFieldOfView := TPerspectiveCameraNode_1(ViewpointNode).FdHeightAngle.Value else
    PerspectiveFieldOfView := DefaultViewpointFieldOfView;

  Result.PerspectiveAngles[0] := RadToDeg(TViewpointNode.ViewpointAngleOfView(
    PerspectiveFieldOfView, Viewport.Width / Viewport.Height));

  Result.PerspectiveAngles[1] := AdjustViewAngleDegToAspectRatio(
    Result.PerspectiveAngles[0], Viewport.Height / Viewport.Width);

  { Tests:
    Writeln(Format('Angle of view: x %f, y %f', [PerspectiveAngles[0], PerspectiveAngles[1]])); }

  Assert(Camera.Radius > 0, 'Camera.Radius must be > 0 when using TCastleAbstractViewport.ApplyProjection');
  Result.ProjectionNear := Camera.Radius * 0.6;

  { calculate Result.ProjectionFar, algorithm documented at DefaultVisibilityLimit }
  Result.ProjectionFar := 0;
  if (GetMainScene <> nil) and
     (GetMainScene.NavigationInfoStack.Top <> nil) then
    Result.ProjectionFar := GetMainScene.NavigationInfoStack.Top.FdVisibilityLimit.Value;
  if Result.ProjectionFar <= 0 then
    Result.ProjectionFar := DefaultVisibilityLimit;
  if Result.ProjectionFar <= 0 then
    Result.ProjectionFar := Box.AverageSize(false,
      { When box is empty (or has 0 sizes), ProjectionFar is not simply "any dummy value".
        It must be appropriately larger than ProjectionNear
        to provide sufficient space for rendering Background node. }
      Result.ProjectionNear) * 20.0;

  { At some point, I was using here larger projection near when
    (ACamera is TExamineCamera). Reasoning: you do not get so close
    to the model with Examine view, and you do not need collision detection.
    Both arguments are wrong now, you can switch between Examine/Walk
    in view3dscene and easily get close to the model, and collision detection
    in Examine mode will be some day implemented (VRML/X3D spec require this). }

  if ViewpointNode <> nil then
    ProjectionType := ViewpointNode.ProjectionType else
    ProjectionType := ptPerspective;

  { update ProjectionFarFinite.
    ProjectionFar may be later changed to ZFarInfinity. }
  Result.ProjectionFarFinite := Result.ProjectionFar;

  Result.ProjectionType := ProjectionType;

  case ProjectionType of
    ptPerspective: DoPerspective;
    ptOrthographic: DoOrthographic;
    else EInternalError.Create('TCastleAbstractViewport.Projection-ProjectionType?');
  end;
end;

function TCastleAbstractViewport.Background: TBackground;
begin
  if GetMainScene <> nil then
    Result := GetMainScene.Background else
    Result := nil;
end;

function TCastleAbstractViewport.MainLightForShadows(
  out AMainLightPosition: TVector4Single): boolean;
begin
  if GetMainScene <> nil then
    Result := GetMainScene.MainLightForShadows(AMainLightPosition) else
    Result := false;
end;

procedure TCastleAbstractViewport.Render3D(const Params: TRenderParams);
begin
  GetItems.Render(RenderingCamera.Frustum, Params);
  if Assigned(OnRender3D) then
    OnRender3D(Self, Params);
end;

procedure TCastleAbstractViewport.RenderShadowVolume;
begin
  GetItems.RenderShadowVolume(GetShadowVolumeRenderer, true, IdentityMatrix4Single);
end;

function TCastleAbstractViewport.HeadlightInstance(out Instance: TLightInstance): boolean;
var
  Node: TAbstractLightNode;
  HC: TCamera;

  procedure PrepareInstance;
  var
    Position, Direction, Up: TVector3Single;
  begin

    Assert(Node <> nil);

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
    Instance.Transform := IdentityMatrix4Single;
    Instance.TransformScale := 1;
    Instance.Radius := MaxSingle;
    Instance.WorldCoordinates := true;
  end;

begin
  Result := false;
  Node := Headlight;
  if Node <> nil then
  begin
    {$warnings off}
    if HeadlightFromViewport then
      HC := Camera else
      HC := GetHeadlightCamera;
    {$warnings on}

    { GetHeadlightCamera (SceneManager.Camera) may be nil here, when
      rendering is done by a custom viewport and HeadlightFromViewport = false.
      So check HC <> nil.
      When nil we have to assume headlight doesn't shine.

      We don't want to use camera settings from current viewport
      (unless HeadlightFromViewport = true, which is a hack).
      This would mean that mirror textures (like GeneratedCubeMapTexture)
      will need to have different contents in different viewpoints,
      which isn't possible. We also want to use scene manager's camera,
      to have it tied with scene manager's CameraToChanges implementation.

      So if you use custom viewports and want headlight Ok,
      be sure to explicitly set TCastleSceneManager.Camera
      (probably, to one of your viewpoints' cameras).
      Or use a hacky HeadlightFromViewport. }

    if HC <> nil then
    begin
      PrepareInstance;
      Result := true;
    end;
  end;
end;

procedure TCastleAbstractViewport.InitializeLights(const Lights: TLightInstancesList);
var
  HI: TLightInstance;
begin
  if HeadlightInstance(HI) then
    Lights.Add(HI);
end;

function TCastleAbstractViewport.BaseLights: TLightInstancesList;
begin
  { We just reuse FRenderParams.FBaseLights[false] below as a temporary
    TLightInstancesList that we already have created. }
  Result := FRenderParams.FBaseLights[false];
  Result.Clear;
  InitializeLights(Result);
end;

procedure TCastleAbstractViewport.RenderFromView3D(const Params: TRenderParams);

  procedure RenderNoShadows;
  begin
    { We must first render all non-transparent objects,
      then all transparent objects. Otherwise transparent objects
      (that must be rendered without updating depth buffer) could get brutally
      covered by non-transparent objects (that are in fact further away from
      the camera). }

    Params.InShadow := false;

    Params.Transparent := false; Params.ShadowVolumesReceivers := false; Render3D(Params);
    Params.Transparent := false; Params.ShadowVolumesReceivers := true ; Render3D(Params);
    Params.Transparent := true ; Params.ShadowVolumesReceivers := false; Render3D(Params);
    Params.Transparent := true ; Params.ShadowVolumesReceivers := true ; Render3D(Params);
  end;

  procedure RenderWithShadows(const MainLightPosition: TVector4Single);
  begin
    GetShadowVolumeRenderer.InitFrustumAndLight(RenderingCamera.Frustum, MainLightPosition);
    GetShadowVolumeRenderer.Render(Params, @Render3D, @RenderShadowVolume, ShadowVolumesRender);
  end;

var
  MainLightPosition: TVector4Single;
begin
  if GLFeatures.ShadowVolumesPossible and
     ShadowVolumes and
     MainLightForShadows(MainLightPosition) then
    RenderWithShadows(MainLightPosition) else
    RenderNoShadows;
end;

procedure TCastleAbstractViewport.RenderFromViewEverything;
var
  ClearBuffers: TClearBuffers;
  ClearColor: TCastleColor;
  UsedBackground: TBackground;
  MainLightPosition: TVector4Single; { ignored }
  SavedProjectionMatrix: TMatrix4Single;
begin
  ClearBuffers := [cbDepth];

  if RenderingCamera.Target = rtVarianceShadowMap then
  begin
    { When rendering to VSM, we want to clear the screen to max depths (1, 1^2). }
    Include(ClearBuffers, cbColor);
    ClearColor := Vector4Single(1, 1, 0, 1);
  end else
  if not Transparent then
  begin
    UsedBackground := Background;
    if UsedBackground <> nil then
    begin
      {$ifndef OpenGLES}
      glLoadMatrix(RenderingCamera.RotationMatrix);
      {$endif}

      { The background rendering doesn't like custom OrthoDimensions.
        They could make the background sky box very small, such that it
        doesn't fill the screen. See e.g. x3d/empty_with_background_ortho.x3dv
        testcase. So temporary set good perspective projection. }
      if FProjection.ProjectionType = ptOrthographic then
      begin
        SavedProjectionMatrix := ProjectionMatrix;
        PerspectiveProjection(45, Rect.Width / Rect.Height,
          FProjection.ProjectionNear,
          FProjection.ProjectionFar);
      end;

      UsedBackground.Render(BackgroundWireframe, RenderingCamera.Frustum);

      if FProjection.ProjectionType = ptOrthographic then
        ProjectionMatrix := SavedProjectionMatrix;
    end else
    begin
      Include(ClearBuffers, cbColor);
      ClearColor := BackgroundColor;
    end;
  end;

  if GLFeatures.ShadowVolumesPossible and
     ShadowVolumes and
     MainLightForShadows(MainLightPosition) then
    Include(ClearBuffers, cbStencil);

  RenderContext.Clear(ClearBuffers, ClearColor);

  {$ifndef OpenGLES}
  glLoadMatrix(RenderingCamera.Matrix);
  {$endif}

  { clear FRenderParams instance }

  FRenderParams.Pass := 0;
  FillChar(FRenderParams.Statistics, SizeOf(FRenderParams.Statistics), #0);

  FRenderParams.FBaseLights[false].Clear;
  InitializeLights(FRenderParams.FBaseLights[false]);
  if UseGlobalLights and
     (GetMainScene <> nil) and
     (GetMainScene.GlobalLights.Count <> 0) then
  begin
    FRenderParams.MainScene := GetMainScene;
    { For MainScene, BaseLights are only the ones calculated by InitializeLights }
    FRenderParams.FBaseLights[true].Assign(FRenderParams.FBaseLights[false]);
    { For others than MainScene, BaseLights are calculated by InitializeLights
      summed with GetMainScene.GlobalLights. }
    FRenderParams.FBaseLights[false].AppendInWorldCoordinates(GetMainScene.GlobalLights);
  end else
    { Do not use Params.FBaseLights[true] }
    FRenderParams.MainScene := nil;

  RenderFromView3D(FRenderParams);
end;

procedure TCastleAbstractViewport.RenderWithScreenEffectsCore;

  procedure RenderOneEffect(Shader: TGLSLProgram);
  var
    BoundTextureUnits: Cardinal;
    AttribVertex, AttribTexCoord: TGLSLAttribute;
  begin
    if ScreenPointVbo = 0 then
    begin
      { generate and fill ScreenPointVbo. It's contents are constant. }
      glGenBuffers(1, @ScreenPointVbo);
      ScreenPoint[0].TexCoord := Vector2Single(0, 0);
      ScreenPoint[0].Position := Vector2Single(-1, -1);
      ScreenPoint[1].TexCoord := Vector2Single(1, 0);
      ScreenPoint[1].Position := Vector2Single( 1, -1);
      ScreenPoint[2].TexCoord := Vector2Single(1, 1);
      ScreenPoint[2].Position := Vector2Single( 1,  1);
      ScreenPoint[3].TexCoord := Vector2Single(0, 1);
      ScreenPoint[3].Position := Vector2Single(-1,  1);
      glBindBuffer(GL_ARRAY_BUFFER, ScreenPointVbo);
      glBufferData(GL_ARRAY_BUFFER, SizeOf(ScreenPoint), @(ScreenPoint[0]), GL_STATIC_DRAW);
    end;

    glBindBuffer(GL_ARRAY_BUFFER, ScreenPointVbo);

    glActiveTexture(GL_TEXTURE0); // GLFeatures.UseMultiTexturing is already checked
    glBindTexture(ScreenEffectTextureTarget, ScreenEffectTextureSrc);
    BoundTextureUnits := 1;

    if CurrentScreenEffectsNeedDepth then
    begin
      glActiveTexture(GL_TEXTURE1);
      glBindTexture(ScreenEffectTextureTarget, ScreenEffectTextureDepth);
      Inc(BoundTextureUnits);
    end;

    CurrentProgram := Shader;
    Shader.Uniform('screen').SetValue(0);
    if CurrentScreenEffectsNeedDepth then
      Shader.Uniform('screen_depth').SetValue(1);
    Shader.Uniform('screen_width').SetValue(TGLint(ScreenEffectTextureWidth));
    Shader.Uniform('screen_height').SetValue(TGLint(ScreenEffectTextureHeight));

    { set special uniforms for SSAO shader }
    if Shader = SSAOShader then
    begin
      { TODO: use actual projection near/far values, instead of hardcoded ones.
        Assignment below works, but it seems that effect is much less noticeable
        then?

      Writeln('setting near to ', ProjectionNear:0:10); // testing
      Writeln('setting far to ', ProjectionFarFinite:0:10); // testing
      Shader.Uniform('near').SetValue(ProjectionNear);
      Shader.Uniform('far').SetValue(ProjectionFarFinite);
      }

      Shader.Uniform('near').SetValue(1.0);
      Shader.Uniform('far').SetValue(1000.0);
    end;

    { Note that we ignore SetupUniforms result --- if some texture
      could not be bound, it will be undefined for shader.
      I don't see anything much better to do now. }
    Shader.SetupUniforms(BoundTextureUnits);

    { Note that there's no need to worry about Rect.Left or Rect.Bottom,
      here or inside RenderWithScreenEffectsCore, because we're already within
      glViewport that takes care of this. }

    AttribVertex := Shader.Attribute('vertex');
    AttribVertex.EnableArrayVector2Single(SizeOf(TScreenPoint),
      OffsetUInt(ScreenPoint[0].Position, ScreenPoint[0]));
    AttribTexCoord := Shader.Attribute('tex_coord');
    AttribTexCoord.EnableArrayVector2Single(SizeOf(TScreenPoint),
      OffsetUInt(ScreenPoint[0].TexCoord, ScreenPoint[0]));

    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    AttribVertex.DisableArray;
    AttribTexCoord.DisableArray;
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;

var
  I: Integer;
begin
  { Render all except the last screen effects: from texture
    (ScreenEffectTextureDest/Src) and to texture (using ScreenEffectRTT) }
  for I := 0 to CurrentScreenEffectsCount - 2 do
  begin
    ScreenEffectRTT.RenderBegin;
    ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
    RenderOneEffect(ScreenEffects[I]);
    ScreenEffectRTT.RenderEnd;

    SwapValues(ScreenEffectTextureDest, ScreenEffectTextureSrc);
  end;

  { Restore glViewport set by ApplyProjection }
  if not FullSize then
    glViewport(ScreenRect);

  { the last effect gets a texture, and renders straight into screen }
  RenderOneEffect(ScreenEffects[CurrentScreenEffectsCount - 1]);
end;

function TCastleAbstractViewport.RenderWithScreenEffects: boolean;

  { Create and setup new OpenGL texture for screen effects.
    Depends on ScreenEffectTextureWidth, ScreenEffectTextureHeight being set. }
  function CreateScreenEffectTexture(const Depth: boolean): TGLuint;

    { Create new OpenGL texture for screen effect.
      Calls glTexImage2D or glTexImage2DMultisample
      (depending on multi-sampling requirements).

      - image contents are always unallocated (pixels = nil for glTexImage2D).
        For screen effects, we never need to load initial image contents,
        and we also do not have to care about pixel packing.
      - level for mipmaps is always 0
      - border is always 0
      - image target is ScreenEffectTextureTarget
      - size is ScreenEffectTextureWidth/Height }
    procedure TexImage2D(const InternalFormat: TGLint;
      const Format, AType: TGLenum);
    begin
      {$ifndef OpenGLES}
      if (GLFeatures.CurrentMultiSampling > 1) and GLFeatures.FBOMultiSampling then
        glTexImage2DMultisample(ScreenEffectTextureTarget,
          GLFeatures.CurrentMultiSampling, InternalFormat,
          ScreenEffectTextureWidth,
          ScreenEffectTextureHeight,
          { fixedsamplelocations = TRUE are necessary in case we use
            this with cbColor mode, where FBO will also have renderbuffer
            for depth (and maybe stencil). In this case,
            https://www.opengl.org/registry/specs/ARB/texture_multisample.txt
            says that

              if the attached images are a mix of
              renderbuffers and textures, the value of
              TEXTURE_FIXED_SAMPLE_LOCATIONS must be TRUE for all attached
              textures.

            which implies that this parameter must be true.
            See https://sourceforge.net/p/castle-engine/tickets/22/ . }
          GL_TRUE) else
      {$endif}
        glTexImage2D(ScreenEffectTextureTarget, 0, InternalFormat,
          ScreenEffectTextureWidth,
          ScreenEffectTextureHeight, 0, Format, AType, nil);
    end;

  begin
    glGenTextures(1, @Result);
    glBindTexture(ScreenEffectTextureTarget, Result);
    {$ifndef OpenGLES}
    { for multisample texture, these cannot be configured (OpenGL makes
      "invalid enumerant" error) }
    if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
    {$endif}
    begin
      { TODO: NEAREST or LINEAR? Allow to config this and eventually change
        before each screen effect? }
      SetTextureFilter(ScreenEffectTextureTarget, TextureFilter(minNearest, magNearest));
      glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_WRAP_S, GLFeatures.CLAMP_TO_EDGE);
      glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_WRAP_T, GLFeatures.CLAMP_TO_EDGE);
    end;
    if Depth then
    begin
      {$ifndef OpenGLES}
      // TODO-es What do we use here? See TGLRenderToTexture TODO at similar place
      if GLFeatures.ShadowVolumesPossible and GLFeatures.PackedDepthStencil then
        TexImage2D(GL_DEPTH24_STENCIL8_EXT, GL_DEPTH_STENCIL_EXT, GL_UNSIGNED_INT_24_8_EXT) else
      {$endif}
        TexImage2D(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT,
          { On OpenGLES, using GL_UNSIGNED_BYTE will result in FBO failing
            with INCOMPLETE_ATTACHMENT.
            http://www.khronos.org/registry/gles/extensions/OES/OES_depth_texture.txt
            allows only GL_UNSIGNED_SHORT or GL_UNSIGNED_INT for depth textures. }
          {$ifdef OpenGLES} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_BYTE {$endif});
      //glTexParameteri(ScreenEffectTextureTarget, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE);
      //glTexParameteri(ScreenEffectTextureTarget, GL_DEPTH_TEXTURE_MODE_ARB, GL_LUMINANCE);
    end else
      TexImage2D({$ifdef OpenGLES} GL_RGB {$else} GL_RGB8 {$endif},
        GL_RGB, GL_UNSIGNED_BYTE);

    TextureMemoryProfiler.Allocate(Result, 'screen-contents', '', { TODO } 0, false,
      ScreenEffectTextureWidth, ScreenEffectTextureHeight, 1);
  end;

var
  SR: TRectangle;
begin
  { save ScreenEffectsCount/NeedDepth result, to not recalculate it,
    and also to make the following code stable --- this way we know
    CurrentScreenEffects* values are constant, even if overridden
    ScreenEffects* methods do something weird. }
  CurrentScreenEffectsCount := ScreenEffectsCount;
  SR := ScreenRect;

  Result := GLFeatures.VertexBufferObject { for screen quad } and
    { check IsTextureSized, to gracefully work (without screen effects)
      on old desktop OpenGL that does not support NPOT textures. }
    IsTextureSized(SR.Width, SR.Height, tsAny) and
    GLFeatures.UseMultiTexturing and
    (CurrentScreenEffectsCount <> 0);

  if Result then
  begin
    CurrentScreenEffectsNeedDepth := ScreenEffectsNeedDepth;
    if CurrentScreenEffectsNeedDepth and not GLFeatures.TextureDepth then
      { We support only screen effects that do not require depth.
        TODO: It would be cleaner to still enable screen effects not using
        depth (and only them), instead of just disabling all screen effects. }
      Exit(false);

    { We need a temporary texture, for screen effect. }
    if (ScreenEffectTextureDest = 0) or
       (ScreenEffectTextureSrc = 0) or
       (CurrentScreenEffectsNeedDepth <> (ScreenEffectTextureDepth <> 0)) or
       (ScreenEffectRTT = nil) or
       (ScreenEffectTextureWidth  <> SR.Width ) or
       (ScreenEffectTextureHeight <> SR.Height) then
    begin
      glFreeTexture(ScreenEffectTextureDest);
      glFreeTexture(ScreenEffectTextureSrc);
      glFreeTexture(ScreenEffectTextureDepth);
      FreeAndNil(ScreenEffectRTT);

      {$ifndef OpenGLES}
      if (GLFeatures.CurrentMultiSampling > 1) and GLFeatures.FBOMultiSampling then
        ScreenEffectTextureTarget := GL_TEXTURE_2D_MULTISAMPLE else
      {$endif}
        ScreenEffectTextureTarget := GL_TEXTURE_2D;

      ScreenEffectTextureWidth  := SR.Width;
      ScreenEffectTextureHeight := SR.Height;
      { We use two textures: ScreenEffectTextureDest is the destination
        of framebuffer, ScreenEffectTextureSrc is the source to render.

        Although for some effects one texture (both src and dest) is enough.
        But when you have > 1 effect and one of the effects has non-local
        operations (they read color values that can be modified by operations
        of the same shader, so it's undefined (depends on how shaders are
        executed in parallel) which one is first) then the artifacts are
        visible. For example, use view3dscene "Edge Detect" effect +
        any other effect. }
      ScreenEffectTextureDest := CreateScreenEffectTexture(false);
      ScreenEffectTextureSrc := CreateScreenEffectTexture(false);
      if CurrentScreenEffectsNeedDepth then
        ScreenEffectTextureDepth := CreateScreenEffectTexture(true);

      { create new TGLRenderToTexture (usually, framebuffer object) }
      ScreenEffectRTT := TGLRenderToTexture.Create(
        ScreenEffectTextureWidth, ScreenEffectTextureHeight);
      ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
      ScreenEffectRTT.CompleteTextureTarget := ScreenEffectTextureTarget;
      { use the same multi-sampling strategy as container }
      ScreenEffectRTT.MultiSampling := GLFeatures.CurrentMultiSampling;
      if CurrentScreenEffectsNeedDepth then
      begin
        ScreenEffectRTT.Buffer := tbColorAndDepth;
        ScreenEffectRTT.DepthTexture := ScreenEffectTextureDepth;
        ScreenEffectRTT.DepthTextureTarget := ScreenEffectTextureTarget;
      end else
        ScreenEffectRTT.Buffer := tbColor;
      ScreenEffectRTT.Stencil := GLFeatures.ShadowVolumesPossible;
      ScreenEffectRTT.GLContextOpen;

      if Log then
        WritelnLog('Screen effects', Format('Created texture for screen effects, with size %d x %d, with depth texture: %s',
          [ ScreenEffectTextureWidth,
            ScreenEffectTextureHeight,
            BoolToStr(CurrentScreenEffectsNeedDepth, true) ]));
    end;

    { We have to adjust glViewport.
      It will be restored from RenderWithScreenEffectsCore right before actually
      rendering to screen. }
    if not FullSize then
      glViewport(Rectangle(0, 0, SR.Width, SR.Height));

    ScreenEffectRTT.RenderBegin;
    ScreenEffectRTT.SetTexture(ScreenEffectTextureDest, ScreenEffectTextureTarget);
    RenderFromViewEverything;
    ScreenEffectRTT.RenderEnd;

    SwapValues(ScreenEffectTextureDest, ScreenEffectTextureSrc);

    {$ifndef OpenGLES}
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glDisable(GL_DEPTH_TEST);
    {$endif}

      {$ifndef OpenGLES}
      glActiveTexture(GL_TEXTURE0);
      glDisable(GL_TEXTURE_2D);
      if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
        glEnable(ScreenEffectTextureTarget);

      if CurrentScreenEffectsNeedDepth then
      begin
        glActiveTexture(GL_TEXTURE1);
        glDisable(GL_TEXTURE_2D);
        if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
          glEnable(ScreenEffectTextureTarget);
      end;
      {$endif}

      OrthoProjection(0, SR.Width, 0, SR.Height);

      RenderWithScreenEffectsCore;

      {$ifndef OpenGLES}
      if CurrentScreenEffectsNeedDepth then
      begin
        glActiveTexture(GL_TEXTURE1);
        if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
          glDisable(ScreenEffectTextureTarget); // TODO: should be done by glPopAttrib, right? enable_bit contains it?
      end;

      glActiveTexture(GL_TEXTURE0);
      if ScreenEffectTextureTarget <> GL_TEXTURE_2D_MULTISAMPLE then
        glDisable(ScreenEffectTextureTarget); // TODO: should be done by glPopAttrib, right? enable_bit contains it?
      {$endif}

      { at the end, we left active texture as default GL_TEXTURE0 }
    {$ifndef OpenGLES}
    glPopAttrib;
    {$endif}
  end;
end;

procedure TCastleAbstractViewport.RenderOnScreen(ACamera: TCamera);
begin
  RenderingCamera.Target := rtScreen;
  RenderingCamera.FromCameraObject(ACamera, nil);

  if not RenderWithScreenEffects then
  begin
    { Rendering directly to the screen, when no screen effects are used. }
    if not FullSize then
      { Use Scissor to limit what RenderContext.Clear clears. }
      RenderContext.ScissorEnable(ScreenRect);

    RenderFromViewEverything;

    if not FullSize then
      RenderContext.ScissorDisable;
  end;
end;

function TCastleAbstractViewport.GetScreenEffects(const Index: Integer): TGLSLProgram;
begin
  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
  begin
    if Index = 0 then
      Result := SSAOShader else
      Result := GetMainScene.ScreenEffects(Index - 1);
  end else
  if GetMainScene <> nil then
    Result := GetMainScene.ScreenEffects(Index) else
    { no Index is valid, since ScreenEffectsCount = 0 in this class }
    Result := nil;
end;

function TCastleAbstractViewport.ScreenEffectsCount: Integer;
begin
  if GetMainScene <> nil then
    Result := GetMainScene.ScreenEffectsCount else
    Result := 0;
  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
    Inc(Result);
end;

function TCastleAbstractViewport.ScreenEffectsNeedDepth: boolean;
begin
  if ScreenSpaceAmbientOcclusion and (SSAOShader <> nil) then
    Exit(true);
  if GetMainScene <> nil then
    Result := GetMainScene.ScreenEffectsNeedDepth else
    Result := false;
end;

procedure TCastleAbstractViewport.GLContextOpen;
begin
  inherited;

  if SSAOShader = nil then
  begin
    if TGLSLProgram.ClassSupport <> gsNone then
    begin
      try
        SSAOShader := TGLSLScreenEffect.Create;
        SSAOShader.NeedsDepth := true;
        SSAOShader.ScreenEffectShader := {$I ssao.glsl.inc};
        SSAOShader.Link;
      except
        on E: EGLSLError do
        begin
          if Log then
            WritelnLog('GLSL', 'Error when initializing GLSL shader for ScreenSpaceAmbientOcclusionShader: ' + E.Message);
          FreeAndNil(SSAOShader);
          ScreenSpaceAmbientOcclusion := false;
        end;
      end;
    end;
  end;
end;

procedure TCastleAbstractViewport.GLContextClose;
begin
  glFreeTexture(ScreenEffectTextureDest);
  glFreeTexture(ScreenEffectTextureSrc);
  glFreeTexture(ScreenEffectTextureDepth);
  ScreenEffectTextureTarget := 0; //< clear, for safety
  FreeAndNil(ScreenEffectRTT);
  FreeAndNil(SSAOShader);
  glFreeBuffer(ScreenPointVbo);
  inherited;
end;

function TCastleAbstractViewport.ScreenSpaceAmbientOcclusionAvailable: boolean;
begin
  Result := (SSAOShader<>nil);
end;

procedure TCastleAbstractViewport.SetScreenSpaceAmbientOcclusion(const Value: boolean);
begin
  if FScreenSpaceAmbientOcclusion <> Value then
  begin
    FScreenSpaceAmbientOcclusion := Value;
    VisibleChange;
  end;
end;

procedure TCastleAbstractViewport.CameraAnimateToDefault(const Time: TFloatTime);
var
  DefCamera: TCamera;
begin
  if Camera = nil then
    Camera := CreateDefaultCamera(nil) else
  begin
    DefCamera := CreateDefaultCamera(nil);
    try
      Camera.AnimateTo(DefCamera, Time);
    finally FreeAndNil(DefCamera) end;
  end;
end;

function TCastleAbstractViewport.CreateDefaultCamera: TCamera;
begin
  Result := CreateDefaultCamera(Self);
end;

function TCastleAbstractViewport.RequiredCamera: TCamera;
begin
  if Camera = nil then
    Camera := CreateDefaultCamera(Self);
  Result := Camera;
end;

function TCastleAbstractViewport.Statistics: TRenderStatistics;
begin
  Result := FRenderParams.Statistics;
end;

function TCastleAbstractViewport.SensorRotation(const X, Y, Z, Angle: Double; const SecondsPassed: Single): boolean;
begin
  Result := (Camera <> nil) and Camera.SensorRotation(X, Y, Z, Angle, SecondsPassed * GetTimeScale);
end;

function TCastleAbstractViewport.SensorTranslation(const X, Y, Z, Length: Double; const SecondsPassed: Single): boolean;
begin
  Result := (Camera <> nil) and Camera.SensorTranslation(X, Y, Z, Length, SecondsPassed * GetTimeScale);
end;

{ TCastleAbstractViewportList -------------------------------------------------- }

function TCastleAbstractViewportList.UsesShadowVolumes: boolean;
var
  I: Integer;
  MainLightPosition: TVector4Single; { ignored }
  V: TCastleAbstractViewport;
begin
  for I := 0 to Count - 1 do
  begin
    V := Items[I];
    if GLFeatures.ShadowVolumesPossible and
       V.ShadowVolumes and
       V.MainLightForShadows(MainLightPosition) then
      Exit(true);
  end;
  Result := false;
end;

{ T3DWorldConcrete ----------------------------------------------------------- }

type
  { Root of T3D hierarchy lists.
    Owner is always non-nil, always a TCastleSceneManager. }
  T3DWorldConcrete = class(T3DWorld)
    function Owner: TCastleSceneManager;
    function CollisionIgnoreItem(const Sender: TObject;
      const Triangle: P3DTriangle): boolean; override;
    function GravityUp: TVector3Single; override;
    function Player: T3DAlive; override;
    function BaseLights: TAbstractLightInstancesList; override;
    function Sectors: TSectorList; override;
    function Water: TBox3D; override;
    function WorldMoveAllowed(
      const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; override;
    function WorldMoveAllowed(
      const OldPos, NewPos: TVector3Single;
      const IsRadius: boolean; const Radius: Single;
      const OldBox, NewBox: TBox3D;
      const BecauseOfGravity: boolean): boolean; override;
    function WorldHeight(const Position: TVector3Single;
      out AboveHeight: Single; out AboveGround: P3DTriangle): boolean; override;
    function WorldLineOfSight(const Pos1, Pos2: TVector3Single): boolean; override;
    function WorldRay(const RayOrigin, RayDirection: TVector3Single): TRayCollision; override;
  end;

function T3DWorldConcrete.Owner: TCastleSceneManager;
begin
  Result := TCastleSceneManager(inherited Owner);
end;

function T3DWorldConcrete.CollisionIgnoreItem(const Sender: TObject;
  const Triangle: P3DTriangle): boolean;
begin
  Result := Owner.CollisionIgnoreItem(Sender, Triangle);
end;

function T3DWorldConcrete.GravityUp: TVector3Single;
begin
  Result := Owner.GravityUp;
end;

function T3DWorldConcrete.Player: T3DAlive;
begin
  Result := Owner.Player;
end;

function T3DWorldConcrete.BaseLights: TAbstractLightInstancesList;
begin
  Result := Owner.BaseLights;
end;

function T3DWorldConcrete.Sectors: TSectorList;
begin
  Result := Owner.Sectors;
end;

function T3DWorldConcrete.Water: TBox3D;
begin
  Result := Owner.Water;
end;

function T3DWorldConcrete.WorldMoveAllowed(
  const OldPos, ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := MoveCollision(OldPos, ProposedNewPos, NewPos, IsRadius, Radius,
    OldBox, NewBox, @CollisionIgnoreItem);
  if Result then
    Result := Owner.MoveAllowed(OldPos, NewPos, BecauseOfGravity);
end;

function T3DWorldConcrete.WorldMoveAllowed(
  const OldPos, NewPos: TVector3Single;
  const IsRadius: boolean; const Radius: Single;
  const OldBox, NewBox: TBox3D;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := MoveCollision(OldPos, NewPos, IsRadius, Radius,
    OldBox, NewBox, @CollisionIgnoreItem);
  if Result then
    Result := Owner.MoveAllowed(OldPos, NewPos, BecauseOfGravity);
end;

function T3DWorldConcrete.WorldHeight(const Position: TVector3Single;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
begin
  Result := HeightCollision(Position, Owner.GravityUp, @CollisionIgnoreItem,
    AboveHeight, AboveGround);
end;

function T3DWorldConcrete.WorldLineOfSight(const Pos1, Pos2: TVector3Single): boolean;
begin
  Result := not SegmentCollision(Pos1, Pos2,
    { Ignore transparent materials, this means that creatures can see through
      glass --- even though they can't walk through it.
      CollisionIgnoreItem doesn't matter for LineOfSight. }
    @TBaseTrianglesOctree(nil).IgnoreTransparentItem,
    true);
end;

function T3DWorldConcrete.WorldRay(
  const RayOrigin, RayDirection: TVector3Single): TRayCollision;
begin
  Result := RayCollision(RayOrigin, RayDirection,
    { Do not use CollisionIgnoreItem here,
      as this is for picking, so the first object should win --- usually.
      May be configurable in the future. } nil);
end;

{ TCastleSceneManager -------------------------------------------------------- }

constructor TCastleSceneManager.Create(AOwner: TComponent);
begin
  inherited;

  FItems := T3DWorldConcrete.Create(Self);
  { Items is displayed and streamed with TCastleSceneManager
    (and in the future this should allow design Items.List by IDE),
    so make it a correct sub-component. }
  FItems.SetSubComponent(true);
  FItems.Name := 'Items';
  FItems.OnCursorChange := @RecalculateCursor;
  FItems.OnVisibleChange := @ItemsVisibleChange;

  FMoveLimit := EmptyBox3D;
  FWater := EmptyBox3D;
  FTimeScale := 1;

  FDefaultViewport := true;

  FViewports := TCastleAbstractViewportList.Create(false);
  if DefaultViewport then FViewports.Add(Self);
end;

destructor TCastleSceneManager.Destroy;
var
  I: Integer;
begin
  { unregister self from MainScene callbacs,
    make MainScene.RemoveFreeNotification(Self)... this is all
    done by SetMainScene(nil) already. }
  MainScene := nil;

  { unregister free notification from these objects }
  SetMouseRayHit(nil);
  Player := nil;

  if FViewports <> nil then
  begin
    for I := 0 to FViewports.Count - 1 do
      if FViewports[I] is TCastleViewport then
      begin
        Assert(TCastleViewport(FViewports[I]).SceneManager = Self);
        { Set SceneManager by direct field (FSceneManager),
          otherwise TCastleViewport.SetSceneManager would try to update
          our Viewports list, that we iterate over right now... }
        TCastleViewport(FViewports[I]).FSceneManager := nil;
      end;
    FreeAndNil(FViewports);
  end;

  FreeAndNil(FSectors);
  FreeAndNil(Waypoints);
  FreeAndNil(DefaultHeadlightNode);

  inherited;
end;

procedure TCastleSceneManager.ItemsVisibleChange(const Sender: T3D; const Changes: TVisibleChanges);
begin
  { merely schedule broadcasting this change to a later time.
    This way e.g. animating a lot of transformations doesn't cause a lot of
    "visible change notifications" repeatedly on the same 3D object within
    the same frame. }
  ScheduledVisibleChangeNotification := true;
  ScheduledVisibleChangeNotificationChanges += Changes;
end;

procedure TCastleSceneManager.GLContextOpen;
begin
  inherited;

  { We actually need to do it only if GLFeatures.ShadowVolumesPossible
    and ShadowVolumes for any viewport.
    But we can as well do it always, it's harmless (just checks some GL
    extensions). (Otherwise we'd have to handle SetShadowVolumes.) }
  if ShadowVolumeRenderer = nil then
  begin
    FShadowVolumeRenderer := TGLShadowVolumeRenderer.Create;
    ShadowVolumeRenderer.GLContextOpen;
  end;
end;

procedure TCastleSceneManager.GLContextClose;
begin
  Items.GLContextClose;

  FreeAndNil(FShadowVolumeRenderer);

  inherited;
end;

function TCastleSceneManager.CreateDefaultCamera(AOwner: TComponent): TCamera;
var
  Box: TBox3D;
  Position, Direction, Up, GravUp: TVector3Single;
begin
  Box := Items.BoundingBox;
  if MainScene <> nil then
    Result := MainScene.CreateCamera(AOwner, Box) else
  begin
    CameraViewpointForWholeScene(Box, 2, 1, false, true,
      Position, Direction, Up, GravUp);

    { by default, create TUniversalCamera, as this is the most versatile camera,
      and it's also what TCastleSceneCore creates (so it's good for consistency,
      simple 3D viewers may in practice assume they always have TUniversalCamera).
      Operations below set the same stuff as
      TExamineCamera.Init and TWalkCamera.Init (but we try to do most by
      using TUniversalCamera methods, so that everything is known by
      TUniversalCamera fields too). }
    Result := TUniversalCamera.Create(AOwner);
    (Result as TUniversalCamera).Radius := Box.AverageSize(false, 1.0) * 0.005;
    (Result as TUniversalCamera).Examine.ModelBox := Box;
    (Result as TUniversalCamera).Walk.GravityUp := GravUp;
    (Result as TUniversalCamera).SetInitialView(Position, Direction, Up, false);
    (Result as TUniversalCamera).GoToInitial;
  end;
end;

function TCastleSceneManager.MouseRayHitContains(const Item: T3D): boolean;
begin
  Result := (MouseRayHit <> nil) and
            (MouseRayHit.IndexOfItem(Item) <> -1);
end;

procedure TCastleSceneManager.SetMainScene(const Value: TCastleScene);
begin
  if FMainScene <> Value then
  begin
    if FMainScene <> nil then
    begin
      { When FMainScene = FPlayer or inside MouseRayHit, leave free notification }
      if (not MouseRayHitContains(FMainScene)) { and
         // impossible, as FMainScene is TCastleScene and FPlayer is TPlayer
         (FMainScene <> FPlayer) } then
        FMainScene.RemoveFreeNotification(Self);
      FMainScene.OnBoundViewpointVectorsChanged := nil;
      FMainScene.OnBoundNavigationInfoFieldsChanged := nil;
      { this SetMainScene may happen from MainScene destruction notification,
        when *Stack is already freed. }
      if FMainScene.ViewpointStack <> nil then
        FMainScene.ViewpointStack.OnBoundChanged := nil;
      if FMainScene.NavigationInfoStack <> nil then
        FMainScene.NavigationInfoStack.OnBoundChanged := nil;
    end;

    FMainScene := Value;

    if FMainScene <> nil then
    begin
      FMainScene.FreeNotification(Self);
      FMainScene.OnBoundViewpointVectorsChanged := @SceneBoundViewpointVectorsChanged;
      FMainScene.OnBoundNavigationInfoFieldsChanged := @SceneBoundNavigationInfoChanged;
      FMainScene.ViewpointStack.OnBoundChanged := @SceneBoundViewpointChanged;
      FMainScene.NavigationInfoStack.OnBoundChanged := @SceneBoundNavigationInfoChanged;

      { Call initial CameraChanged (this allows ProximitySensors to work
        as soon as ProcessEvents becomes true).

        TODO: actually, we should call CameraChanged on all newly added
        T3D to Items. This would fix the problem of 1st frame not using BlendingSort
        for non-MainScene scenes, as in trees_blending/CW_demo.lpr testcase
        from Eugene.
      }
      if Camera <> nil then
      begin
        MainScene.CameraChanged(Camera);
        ItemsVisibleChange(MainScene, CameraToChanges);
      end;
    end;
  end;
end;

procedure TCastleSceneManager.SetMouseRayHit(const Value: TRayCollision);
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
        if (FMouseRayHit[I].Item <> FMainScene) and
           (FMouseRayHit[I].Item <> FPlayer) then
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

procedure TCastleSceneManager.SetPlayer(const Value: TPlayer);
begin
  if FPlayer <> Value then
  begin
    if FPlayer <> nil then
    begin
      { leave free notification for FPlayer if it's also present somewhere else }
      if { // impossible, as FMainScene is TCastleScene and FPlayer is TPlayer
         (FPlayer <> FMainScene) and }
         (not MouseRayHitContains(FPlayer)) then
        FPlayer.RemoveFreeNotification(Self);
    end;

    FPlayer := Value;

    if FPlayer <> nil then
      FPlayer.FreeNotification(Self);
  end;
end;

procedure TCastleSceneManager.SetCamera(const Value: TCamera);
begin
  if FCamera <> Value then
  begin
    inherited;

    if FCamera <> nil then
    begin
      { Call initial CameraChanged (this allows ProximitySensors to work
        as soon as ProcessEvents becomes true). }
      Items.CameraChanged(Camera);
      ItemsVisibleChange(Items, CameraToChanges);
    end;

    { Changing camera changes also the view rapidly. }
    if MainScene <> nil then
      MainScene.ViewChangedSuddenly;

    { Call OnBoundNavigationInfoChanged when camera instance changed.
      This allows code that observes Camera.NavigationType to work,
      otherwise OnBoundNavigationInfoChanged may be called only
      when Camera = nil (at loading). }
    BoundNavigationInfoChanged;
  end else
    inherited; { not really needed for now, but for safety --- always call inherited }
end;

procedure TCastleSceneManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    { set to nil by methods (like SetMainScene), to clean nicely }
    if AComponent = FMainScene then
      MainScene := nil;

    if (AComponent is T3D) and MouseRayHitContains(T3D(AComponent)) then
    begin
      { MouseRayHit cannot be used in subsequent RecalculateCursor. }
      SetMouseRayHit(nil);
    end;

    if AComponent = FPlayer then
      Player := nil;
  end;
end;

procedure TCastleSceneManager.PrepareResources(const Item: T3D;
  const DisplayProgressTitle: string);
var
  Options: TPrepareResourcesOptions;
begin
  ChosenViewport := nil;
  NeedsUpdateGeneratedTextures := false;

  { This preparation is done only once, before rendering all viewports.
    No point in doing this when no viewport is configured.
    Also, we'll need to use one of viewport's projection here. }
  if Viewports.Count <> 0 then
  begin
    Options := [prRender, prBackground, prBoundingBox, prScreenEffects];

    if Viewports.UsesShadowVolumes then
      Include(Options, prShadowVolume);

    { We need one viewport, to setup it's projection and to setup it's camera.
      There's really no perfect choice, although in practice any viewport
      should do just fine. For now: use the 1st one on the list.
      Maybe in the future we'll need more intelligent method of choosing. }
    ChosenViewport := Viewports[0];

    { Apply projection now, as TCastleScene.GLProjection calculates
      BackgroundSkySphereRadius, which is used by MainScene.Background.
      Otherwise our preparations of "prBackground" here would be useless,
      as BackgroundSkySphereRadius will change later, and MainScene.Background
      will have to be recreated. }
    ChosenViewport.ApplyProjection;

    { RenderingCamera properties must be already set,
      since PrepareResources may do some operations on texture gen modes
      in WORLDSPACE*. }
    RenderingCamera.FromCameraObject(ChosenViewport.Camera, nil);

    if DisplayProgressTitle <> '' then
    begin
      Progress.Init(Items.PrepareResourcesSteps, DisplayProgressTitle, true);
      try
        Item.PrepareResources(Options, true, BaseLights);
      finally Progress.Fini end;
    end else
      Item.PrepareResources(Options, false, BaseLights);

    NeedsUpdateGeneratedTextures := true;
  end;
end;

procedure TCastleSceneManager.PrepareResources(const DisplayProgressTitle: string);
begin
  PrepareResources(Items, DisplayProgressTitle);
end;

procedure TCastleSceneManager.BeforeRender;
begin
  inherited;
  if not GetExists then Exit;
  PrepareResources;
end;

function TCastleSceneManager.CameraToChanges: TVisibleChanges;
begin
  if (MainScene <> nil) and MainScene.HeadlightOn then
    Result := [vcVisibleNonGeometry] else
    Result := [];
end;

procedure TCastleSceneManager.UpdateGeneratedTexturesIfNeeded;
begin
  if NeedsUpdateGeneratedTextures then
  begin
    NeedsUpdateGeneratedTextures := false;

    { We depend here that right before Render, BeforeRender was called.
      We depend on BeforeRender (actually PrepareResources) to set
      ChosenViewport and make ChosenViewport.ApplyProjection.

      This way below we can use sensible projection near/far calculated
      by previous ChosenViewport.ApplyProjection,
      and restore viewport used by previous ChosenViewport.ApplyProjection.

      This could be moved to PrepareResources without problems, but we want
      time needed to render textures be summed into "FPS frame time". }
    Items.UpdateGeneratedTextures(@RenderFromViewEverything,
      ChosenViewport.FProjection.ProjectionNear,
      ChosenViewport.FProjection.ProjectionFar,
      ChosenViewport.ScreenRect);
  end;
end;

procedure TCastleSceneManager.Render;
begin
  if not GetExists then Exit;

  UpdateGeneratedTexturesIfNeeded;

  inherited;
  if not DefaultViewport then Exit;
  ApplyProjection;
  RenderOnScreen(Camera);
end;

function TCastleSceneManager.PointingDeviceActivate(const Active: boolean): boolean;

  { Try PointingDeviceActivate on 3D stuff hit by RayHit }
  function TryActivate(RayHit: TRayCollision): boolean;
  var
    PassToMainScene: boolean;
    I: Integer;
  begin
    { call T3D.PointingDeviceActivate on everything, calculate Result }
    Result := false;
    PassToMainScene := true;

    if RayHit <> nil then
      for I := 0 to RayHit.Count - 1 do
      begin
        if RayHit[I].Item = MainScene then
          PassToMainScene := false;
        Result := PointingDeviceActivate3D(RayHit[I].Item, Active, RayHit.Distance);
        if Result then
        begin
          PassToMainScene := false;
          Break;
        end;
      end;

    if PassToMainScene and (MainScene <> nil) then
      Result := PointingDeviceActivate3D(MainScene, Active, MaxSingle);
  end;

var
  MousePosition: TVector2Single;

  { Try PointingDeviceActivate on 3D stuff hit by ray moved by given number
    of screen pixels from current mouse position.
    Call only if Camera and MousePosition already assigned. }
  function TryActivateAround(const Change: TVector2Single): boolean;
  var
    RayOrigin, RayDirection: TVector3Single;
    RayHit: TRayCollision;
  begin
    Camera.CustomRay(ScreenRect, MousePosition + Change,
      FProjection, RayOrigin, RayDirection);

    RayHit := CameraRayCollision(RayOrigin, RayDirection);

    { We do not really have to check "RayHit <> nil" below,
      as TryActivate can (and should) work even with RayHit=nil case.
      However, we know that TryActivate will not do anything new if RayHit=nil
      (it will just pass this to MainScene, which was already done before
      trying ApproximateActivation). }

    Result := (RayHit <> nil) and TryActivate(RayHit);

    FreeAndNil(RayHit);
  end;

  function TryActivateAroundSquare(const Change: Single): boolean;
  begin
    Result := TryActivateAround(Vector2Single(-Change, -Change)) or
              TryActivateAround(Vector2Single(-Change, +Change)) or
              TryActivateAround(Vector2Single(+Change, +Change)) or
              TryActivateAround(Vector2Single(+Change, -Change)) or
              TryActivateAround(Vector2Single(      0, -Change)) or
              TryActivateAround(Vector2Single(      0, +Change)) or
              TryActivateAround(Vector2Single(-Change,       0)) or
              TryActivateAround(Vector2Single(+Change,       0));
  end;

  { If Container assigned, set local MousePosition. }
  function GetMousePosition: boolean;
  var
    C: TUIContainer;
  begin
    C := Container;
    Result := C <> nil;
    if Result then
      MousePosition := C.MousePosition;
  end;

begin
  Result := TryActivate(MouseRayHit);
  if not Result then
  begin
    if ApproximateActivation and (Camera <> nil) and GetMousePosition then
      Result := TryActivateAroundSquare(25) or
                TryActivateAroundSquare(50) or
                TryActivateAroundSquare(100) or
                TryActivateAroundSquare(200);
  end;

  if not Result then
    PointingDeviceActivateFailed(Active);
end;

function TCastleSceneManager.PointingDeviceActivate3D(const Item: T3D;
  const Active: boolean; const Distance: Single): boolean;
begin
  Result := Item.PointingDeviceActivate(Active, Distance);
end;

procedure TCastleSceneManager.PointingDeviceActivateFailed(const Active: boolean);
begin
  if Active then
    SoundEngine.Sound(stPlayerInteractFailed);
end;

function TCastleSceneManager.PointingDeviceMove(
  const RayOrigin, RayDirection: TVector3Single): boolean;
var
  PassToMainScene: boolean;
  I: Integer;
  MainSceneNode: TRayCollisionNode;
begin
  { update MouseRayHit.
    We know that RayDirection is normalized now, which is important
    to get correct MouseRayHit.Distance. }
  SetMouseRayHit(CameraRayCollision(RayOrigin, RayDirection));

  { call T3D.PointingDeviceMove on everything, calculate Result }
  Result := false;
  PassToMainScene := true;

  if MouseRayHit <> nil then
    for I := 0 to MouseRayHit.Count - 1 do
    begin
      if MouseRayHit[I].Item = MainScene then
        PassToMainScene := false;
      Result := MouseRayHit[I].Item.PointingDeviceMove(MouseRayHit[I], MouseRayHit.Distance);
      if Result then
      begin
        PassToMainScene := false;
        Break;
      end;
    end;

  if PassToMainScene and (MainScene <> nil) then
  begin
    MainSceneNode.Item := MainScene;
    { if ray hit something, then the outermost 3D object should just be our Items,
      and it contains the 3D point picked.
      This isn't actually used by anything now --- TRayCollisionNode.Point
      is for now used only by TCastleSceneCore, and only when Triangle <> nil. }
    if MouseRayHit <> nil then
      MainSceneNode.Point := MouseRayHit.Last.Point else
      MainSceneNode.Point := ZeroVector3Single;
    MainSceneNode.RayOrigin := RayOrigin;
    MainSceneNode.RayDirection := RayDirection;
    MainSceneNode.Triangle := nil;
    Result := MainScene.PointingDeviceMove(MainSceneNode, MaxSingle);
  end;
end;

procedure TCastleSceneManager.Update(const SecondsPassed: Single;
  var HandleInput: boolean);

  procedure DoScheduledVisibleChangeNotification;
  var
    Changes: TVisibleChanges;
  begin
    if ScheduledVisibleChangeNotification then
    begin
      { reset state first, in case some VisibleChangeNotification will post again
        another visible change. }
      ScheduledVisibleChangeNotification := false;
      Changes := ScheduledVisibleChangeNotificationChanges;
      ScheduledVisibleChangeNotificationChanges := [];

      { pass visible change notification "upward" (as a TUIControl, to container) }
      VisibleChange;
      { pass visible change notification "downward", to all children T3D }
      Items.VisibleChangeNotification(Changes);
    end;
  end;

const
  { Delay between calling SoundEngine.Refresh, in seconds. }
  SoundRefreshDelay = 0.1;
var
  RemoveItem: TRemoveType;
  TimeNow: TTimerResult;
  SecondsPassedScaled: Single;
begin
  inherited;

  SecondsPassedScaled := SecondsPassed * TimeScale;

  if (not Paused) and GetExists then
  begin
    RemoveItem := rtNone;
    Items.Update(SecondsPassedScaled, RemoveItem);
    { we ignore RemoveItem --- main Items list cannot be removed }

    { Calling SoundEngine.Refresh relatively often is important,
      to call OnRelease for sound sources that finished playing.
      Some of the engine features depend that sounds OnRelease is called
      in a timely fashion. Notably: footsteps sound (done in TPlayer.Update)
      relies on the fact that OnRelease of it's source will be reported
      quickly after sound stopped. }
    if SoundEngine.ALActive then
    begin
      TimeNow := Timer;
      if TimerSeconds(TimeNow, LastSoundRefresh) > SoundRefreshDelay then
      begin
        LastSoundRefresh := TimeNow;
        SoundEngine.Refresh;
      end;
    end;
  end;

  DoScheduledVisibleChangeNotification;
end;

procedure TCastleSceneManager.CameraVisibleChange(ACamera: TObject);
var
  Pos, Dir, Up: TVector3Single;
begin
  (ACamera as TCamera).GetView(Pos, Dir, Up);

  if ACamera = Camera then
  begin
    { Call CameraChanged for all Items, not just MainScene.
      This allows ProximitySensor and Billboard and such nodes
      to work in all 3D scenes, not just in MainScene. }
    Items.CameraChanged(Camera);
    { ItemsVisibleChange will also cause our own VisibleChange. }
    ItemsVisibleChange(Items, CameraToChanges);
  end else
    VisibleChange;

  SoundEngine.UpdateListener(Pos, Dir, Up);

  if Assigned(OnCameraChanged) then
    OnCameraChanged(ACamera);
end;

function TCastleSceneManager.CollisionIgnoreItem(const Sender: TObject;
  const Triangle: P3DTriangle): boolean;
begin
  Result := false;
end;

function TCastleSceneManager.CameraMoveAllowed(ACamera: TWalkCamera;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  { Both version result in calling WorldMoveAllowed.
    Player version adds Player.Disable/Enable around, so don't collide with self. }
  if Player <> nil then
    Result := Player.MoveAllowed(ACamera.Position, ProposedNewPos, NewPos, BecauseOfGravity) else
    Result := Items.WorldMoveAllowed(ACamera.Position, ProposedNewPos, NewPos,
      true, ACamera.Radius,
      { We prefer to resolve collisions with camera using sphere.
        But for T3D implementations that can't use sphere, we can construct box. }
      Box3DAroundPoint(ACamera.Position, ACamera.Radius * 2),
      Box3DAroundPoint(ProposedNewPos, ACamera.Radius * 2), BecauseOfGravity);
end;

function TCastleSceneManager.CameraHeight(ACamera: TWalkCamera;
  const Position: TVector3Single;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
begin
  { Both version result in calling WorldHeight.
    Player version adds Player.Disable/Enable around, so don't collide with self. }
  if Player <> nil then
    Result := Player.Height(Position, AboveHeight, AboveGround) else
    Result := Items.WorldHeight(Position, AboveHeight, AboveGround);
end;

function TCastleSceneManager.CameraRayCollision(const RayOrigin, RayDirection: TVector3Single): TRayCollision;
begin
  { Both version result in calling WorldRay.
    Player version adds Player.Disable/Enable around, so don't collide with self. }
  if Player <> nil then
    Result := Player.Ray(RayOrigin, RayDirection) else
    Result := Items.WorldRay(RayOrigin, RayDirection);
end;

procedure TCastleSceneManager.BoundViewpointChanged;
begin
  if Assigned(OnBoundViewpointChanged) then
    OnBoundViewpointChanged(Self);
end;

procedure TCastleSceneManager.BoundNavigationInfoChanged;
begin
  if Assigned(OnBoundNavigationInfoChanged) then
    OnBoundNavigationInfoChanged(Self);
end;

procedure TCastleSceneManager.SceneBoundViewpointChanged(Scene: TCastleSceneCore);
begin
  if Camera <> nil then
    Scene.CameraFromViewpoint(Camera, false);
  BoundViewpointChanged;
end;

procedure TCastleSceneManager.SceneBoundNavigationInfoChanged(Scene: TCastleSceneCore);
begin
  if Camera <> nil then
    Scene.CameraFromNavigationInfo(Camera, Items.BoundingBox);
  BoundNavigationInfoChanged;
end;

procedure TCastleSceneManager.SceneBoundViewpointVectorsChanged(Scene: TCastleSceneCore);
begin
  if Camera <> nil then
    Scene.CameraFromViewpoint(Camera, true);
end;

function TCastleSceneManager.GetItems: T3DWorld;
begin
  Result := Items;
end;

function TCastleSceneManager.GetMainScene: TCastleScene;
begin
  Result := MainScene;
end;

function TCastleSceneManager.GetShadowVolumeRenderer: TGLShadowVolumeRenderer;
begin
  Result := ShadowVolumeRenderer;
end;

function TCastleSceneManager.GetMouseRayHit: TRayCollision;
begin
  Result := MouseRayHit;
end;

function TCastleSceneManager.GetHeadlightCamera: TCamera;
begin
  Result := Camera;
end;

function TCastleSceneManager.GetPlayer: TPlayer;
begin
  Result := Player;
end;

function TCastleSceneManager.GetTimeScale: Single;
begin
  Result := TimeScale;
end;

procedure TCastleSceneManager.SetDefaultViewport(const Value: boolean);
begin
  if Value <> FDefaultViewport then
  begin
    FDefaultViewport := Value;
    if DefaultViewport then
      Viewports.Add(Self) else
      Viewports.Remove(Self);
  end;
end;

function TCastleSceneManager.GravityUp: TVector3Single;
begin
  if Camera <> nil then
    Result := Camera.GetGravityUp else
    Result := DefaultCameraUp;
end;

function TCastleSceneManager.MoveAllowed(const OldPosition, NewPosition: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := true;

  if MoveLimit.IsEmpty then
  begin
    { Don't let objects/camera fall outside of the box because of gravity,
      as then they would fall into infinity. }
    if BecauseOfGravity then
      Result := Items.BoundingBox.PointInside(NewPosition);
  end else
    Result := MoveLimit.PointInside(NewPosition);

  if Assigned(OnMoveAllowed) then
    OnMoveAllowed(Self, Result, OldPosition, NewPosition, BecauseOfGravity);
end;

function TCastleSceneManager.Headlight: TAbstractLightNode;
begin
  if (MainScene <> nil) and MainScene.HeadlightOn then
  begin
    Result := MainScene.CustomHeadlight;
    if Result = nil then
    begin
      if DefaultHeadlightNode = nil then
        { Nothing more needed, all DirectionalLight default properties
          are suitable for default headlight. }
        DefaultHeadlightNode := TDirectionalLightNode.Create;
      Result := DefaultHeadlightNode;
    end;
    Assert(Result <> nil);
  end else
    Result := nil;
end;

function TCastleSceneManager.Rect: TRectangle;
begin
  if DefaultViewport then
    Result := inherited Rect else
    Result := TRectangle.Empty;
end;

{ TCastleViewport --------------------------------------------------------------- }

destructor TCastleViewport.Destroy;
begin
  SceneManager := nil; { remove Self from SceneManager.Viewports }
  inherited;
end;

procedure TCastleViewport.CheckSceneManagerAssigned;
begin
  if SceneManager = nil then
    raise EViewportSceneManagerMissing.Create('TCastleViewport.SceneManager is required, but not assigned yet');
end;

procedure TCastleViewport.CameraVisibleChange(ACamera: TObject);
begin
  VisibleChange;
end;

function TCastleViewport.CameraMoveAllowed(ACamera: TWalkCamera;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  if SceneManager <> nil then
    Result := SceneManager.CameraMoveAllowed(
      ACamera, ProposedNewPos, NewPos, BecauseOfGravity) else
  begin
    Result := true;
    NewPos := ProposedNewPos;
  end;
end;

function TCastleViewport.CameraHeight(ACamera: TWalkCamera;
  const Position: TVector3Single;
  out AboveHeight: Single; out AboveGround: P3DTriangle): boolean;
begin
  if SceneManager <> nil then
    Result := SceneManager.CameraHeight(ACamera, Position, AboveHeight, AboveGround) else
  begin
    Result := false;
    AboveHeight := MaxSingle;
    AboveGround := nil;
  end;
end;

function TCastleViewport.CameraRayCollision(const RayOrigin, RayDirection: TVector3Single): TRayCollision;
begin
  if SceneManager <> nil then
    Result := SceneManager.CameraRayCollision(RayOrigin, RayDirection) else
    Result := nil;
end;

function TCastleViewport.CreateDefaultCamera(AOwner: TComponent): TCamera;
begin
  CheckSceneManagerAssigned;
  Result := SceneManager.CreateDefaultCamera(AOwner);
end;

function TCastleViewport.GetItems: T3DWorld;
begin
  CheckSceneManagerAssigned;
  Result := SceneManager.Items;
end;

function TCastleViewport.GetMainScene: TCastleScene;
begin
  CheckSceneManagerAssigned;
  Result := SceneManager.MainScene;
end;

function TCastleViewport.GetShadowVolumeRenderer: TGLShadowVolumeRenderer;
begin
  CheckSceneManagerAssigned;
  Result := SceneManager.ShadowVolumeRenderer;
end;

function TCastleViewport.GetMouseRayHit: TRayCollision;
begin
  CheckSceneManagerAssigned;
  Result := SceneManager.MouseRayHit;
end;

function TCastleViewport.GetHeadlightCamera: TCamera;
begin
  CheckSceneManagerAssigned;
  Result := SceneManager.Camera;
end;

function TCastleViewport.GetPlayer: TPlayer;
begin
  CheckSceneManagerAssigned;
  Result := SceneManager.Player;
end;

function TCastleViewport.GetTimeScale: Single;
begin
  CheckSceneManagerAssigned;
  Result := SceneManager.TimeScale;
end;

procedure TCastleViewport.Render;
begin
  if not GetExists then Exit;

  CheckSceneManagerAssigned;
  SceneManager.UpdateGeneratedTexturesIfNeeded;

  inherited;
  ApplyProjection;
  RenderOnScreen(Camera);
end;

function TCastleViewport.PointingDeviceActivate(const Active: boolean): boolean;
begin
  Result := (SceneManager <> nil) and
    SceneManager.PointingDeviceActivate(Active);
end;

function TCastleViewport.PointingDeviceMove(
  const RayOrigin, RayDirection: TVector3Single): boolean;
begin
  Result := (SceneManager <> nil) and
    SceneManager.PointingDeviceMove(RayOrigin, RayDirection);
end;

procedure TCastleViewport.SetSceneManager(const Value: TCastleSceneManager);
begin
  if Value <> FSceneManager then
  begin
    if SceneManager <> nil then
      SceneManager.Viewports.Remove(Self);
    FSceneManager := Value;
    if SceneManager <> nil then
      SceneManager.Viewports.Add(Self);
  end;
end;

function TCastleViewport.Headlight: TAbstractLightNode;
begin
  CheckSceneManagerAssigned;
  { Using the SceneManager.Headlight allows to share a DefaultHeadlightNode
    with all viewports sharing the same SceneManager.
    This is useful for tricks like view3dscene scene manager,
    that like to have a headlight node common for the 3D world,
    regardless if it's coming from MainScene or from DefaultHeadlightNode. }
  Result := SceneManager.Headlight;
end;

initialization
  { Basic shortcuts. }
  Input_Attack := TInputShortcut.Create(nil, 'Attack', 'attack', igBasic);
  Input_Attack.Assign(K_Ctrl, K_None, #0, false, mbLeft);
  Input_Attack.GroupOrder := -100; { before other (player) shortcuts }

  { Items shortcuts. }
  Input_InventoryShow := TInputShortcut.Create(nil, 'Inventory show / hide', 'inventory_toggle', igItems);
  Input_InventoryShow.Assign(K_None, K_None, #0, false, mbLeft);
  Input_InventoryPrevious := TInputShortcut.Create(nil, 'Select previous item', 'inventory_previous', igItems);
  Input_InventoryPrevious.Assign(K_LeftBracket, K_None, #0, false, mbLeft, mwUp);
  Input_InventoryNext := TInputShortcut.Create(nil, 'Select next item', 'inventory_next', igItems);
  Input_InventoryNext.Assign(K_RightBracket, K_None, #0, false, mbLeft, mwDown);
  Input_UseItem := TInputShortcut.Create(nil, 'Use (or equip) selected item', 'item_use', igItems);
  Input_UseItem.Assign(K_Enter, K_None, #0, false, mbLeft);
  Input_DropItem := TInputShortcut.Create(nil, 'Drop selected item', 'item_drop', igItems);
  Input_DropItem.Assign(K_None, K_None, #0, false, mbLeft);

  { Other shortcuts. }
  Input_Interact := TInputShortcut.Create(nil, 'Interact (press, open door)', 'interact', igOther);
  Input_Interact.Assign(K_None, K_None, #0, true, mbLeft);
  Input_CancelFlying := TInputShortcut.Create(nil, 'Cancel flying spell', 'cancel_flying', igOther);
  Input_CancelFlying.Assign(K_None, K_None, #0, false, mbLeft);
end.
