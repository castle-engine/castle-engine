{
  Copyright 2003-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering of scenes (TCastleScene). }
unit CastleScene;

{$I castleconf.inc}
{$modeswitch nestedprocvars}{$H+}

interface

uses SysUtils, Classes, Generics.Collections,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleVectors, CastleBoxes, X3DNodes, CastleClassUtils,
  CastleUtils, CastleSceneCore, CastleInternalRenderer, CastleInternalBackground,
  CastleGLUtils, CastleInternalShapeOctree, CastleInternalGLShadowVolumes, X3DFields,
  CastleTriangles, CastleShapes, CastleFrustum, CastleTransform, CastleGLShaders,
  CastleRectangles, CastleCameras, CastleRendererInternalShader, CastleColors,
  CastleSceneInternalShape, CastleSceneInternalOcclusion, CastleSceneInternalBlending,
  CastleInternalBatchShapes, CastleRenderOptions, CastleTimeUtils;

{$define read_interface}

type
  TCastleSceneList = class;

  TBeforeShapeRenderProc = procedure (Shape: TShape) of object;

  TRenderingAttributesEvent = TCastleRenderOptionsEvent deprecated 'use TCastleRenderOptionsEvent';
  TSceneRenderingAttributes = TCastleRenderOptions deprecated 'use TCastleRenderOptions';

  TPrepareResourcesOption = CastleTransform.TPrepareResourcesOption;
  TPrepareResourcesOptions = CastleTransform.TPrepareResourcesOptions;

const
  prRenderSelf = CastleTransform.prRenderSelf;
  prRenderClones = CastleTransform.prRenderClones;
  prBackground = CastleTransform.prBackground;
  prBoundingBox = CastleTransform.prBoundingBox;
  prShadowVolume = CastleTransform.prShadowVolume;

type
  { Possible checks done while frustum culling.

    This is used by TCastleScene.FrustumCulling (what checks
    should be done when shapes octree is not available) and
    TCastleScene.OctreeFrustumCulling (what checks
    should be done when shapes octree is available).

    In the second case, checks done by TFrustumCulling are applied
    after octree traverse. That is, octree already eliminated some shapes,
    and fully included some other shapes while traversing.
    TFrustumCulling are used in this
    case only as a "last resort", to check only the shapes in octree leaves
    that are in "possibly-colliding" state with frustum.

    Generally, more checks mean that more shapes may be eliminated but
    also that we waste more time on checks themselves. What is optimal
    depends on given 3D model, and how you expect the player to view it
    (e.g. if player usually sees the whole model, then TFrustumCulling
    checks may be useless waste of time; OTOH, if player stands inside
    the model composed from many shapes then TFrustumCulling may help). }
  TFrustumCulling = (
    { No checks.

      Setting this as TCastleScene.FrustumCulling
      turns off frustum culling entirely, which is usually not a wise thing
      to do. Setting this as TCastleScene.OctreeFrustumCulling
      means that frustum culling is only done during octree traversal
      (we only visit octree nodes possibly colliding with frustum),
      this is also not optimal. }
    fcNone,

    { Check shape's bounding sphere for collision with frustum. }
    fcSphere,

    { Check shape's bounding box for collision with frustum. }
    fcBox,

    { Check shape's bounding sphere, and then box, for collision with frustum.
      This is the most rigoristic check, but usually this is a waste of time:
      in most cases, when bounding sphere collides, then bounding box
      collides too. }
    fcBoth
  );

  { Complete loading, processing and rendering of a scene.
    This is a descendant of @link(TCastleSceneCore) that adds efficient rendering. }
  TCastleScene = class(TCastleSceneCore)
  strict private
    type
      TCustomShaders = record
        Shader: TX3DShaderProgramBase;
        ShaderAlphaTest: TX3DShaderProgramBase;
        procedure Initialize(const VertexCode, FragmentCode: string);
        procedure Free;
      end;

      TSceneRenderOptions = class(TCastleRenderOptions)
      private
        OwnerScene: TCastleScene;
      protected
        procedure ReleaseCachedResources; override;
      end;

    var
      { Used by UpdateGeneratedTextures, to prevent rendering the shape
        for which reflection texture is generated. (This wouldn't cause
        recursive loop in our engine, but still it's bad --- rendering
        from the inside of the object usually obscures the world around...). }
      AvoidShapeRendering: TGLShape;

      { Used by UpdateGeneratedTextures, to prevent rendering non-shadow casters
        for shadow maps. }
      AvoidNonShadowCasterRendering: boolean;

      { Used by UpdateGeneratedTextures, to avoid updating twice during the same render. }
      UpdateGeneratedTexturesFrameId: TFrameId;

      VarianceShadowMapsProgram, ShadowMapsProgram: TCustomShaders;
      FDistanceCulling: Single;

      FReceiveShadowVolumes: boolean;
      RegisteredGLContextCloseListener: boolean;
      FTempPrepareParams: TPrepareParams;
      { Camera position, in local scene coordinates, known during the Render call. }
      RenderCameraPosition: TVector3;

      { Used by LocalRenderInside }
      FilteredShapes: TShapeList;

      InternalScenePass: TInternalSceneRenderingPass;
      FBatching: TBatchShapes;

      { Valid only during TCastleScene.LocalRender.
        Callbacks assigned to ShapeCullingFunc and ShapeCullingOctreeFunc may use it. }
      FrustumForShapeCulling: PFrustum;

      FFrustumCulling: TFrustumCulling;
      FOctreeFrustumCulling: TFrustumCulling;
      FShapeFrustumCulling, FSceneFrustumCulling: Boolean;
      ShapeCullingFunc: TTestShapeVisibility;
      ShapeCullingOctreeFunc: TTestShapeVisibility;

      OcclusionQueryUtilsRenderer: TOcclusionQueryUtilsRenderer;
      SimpleOcclusionQueryRenderer: TSimpleOcclusionQueryRenderer;
      HierarchicalOcclusionQueryRenderer: THierarchicalOcclusionQueryRenderer;
      BlendingRenderer: TBlendingRenderer;

    { Render everything using Renderer.

      Calls Renderer.RenderBegin.
      Then on all potentially visible Shapes[] calls RenderShape.
      "Potentially visible" is decided by TestShapeVisibility
      (shape is visible if TestShapeVisibility is @nil or returns
      @true for this shape) and Params.Transparent value must include
      given shape. At the end calls Renderer.RenderEnd.

      Additionally this implements blending, looking at RenderOptions.Blending*,
      setting appropriate OpenGL state and rendering partially transparent
      shape before all opaque objects.

      Updates Params.Statistics. }
    procedure LocalRenderInside(const TestShapeVisibility: TTestShapeVisibility;
      const Params: TRenderParams);

    { Render everything using LocalRenderInside.
      The rendering parameters are configurable by @link(RenderOptions).

      For more details about rendering, see @link(CastleRenderer) unit comments.
      This method internally uses TGLRenderer instance, additionally
      handling the blending:

      @unorderedList(
        @item(
          OpenGL state of glDepthMask, glEnable/Disable(GL_BLEND), glBlendFunc
          is controlled by this function. This function will unconditionally
          change (and restore later to original value) this state,
          to perform correct blending (transparency rendering).

          To make a correct rendering, we always
          render transparent shapes at the end (after all opaque),
          and with depth-buffer in read-only mode.)

        @item(Only a subset of shapes indicated by Params.Transparent is rendered.
          This is necessary if you want to mix in one 3D world many scenes
          (like TCastleScene instances), and each of them may have some opaque
          and some transparent
          parts. In such case, you want to render everything opaque
          (from every scene) first, and only then render everything transparent.
          For shadow volumes, this is even more complicated.)

        @item(Note that when RenderOptions.Blending is @false then everything
          is always opaque, so tgOpaque renders everything and tgTransparent
          renders nothing.)
      )

      @param(TestShapeVisibility Filters which shapes are visible.

        Note that shapes are further filtered by optimizations
        like frustum culling (@link(ShapeFrustumCulling))
        or distance culling (@link(DistanceCulling)).
      ) }
    procedure LocalRenderOutside(
      const TestShapeVisibility: TTestShapeVisibility;
      const Params: TRenderParams);

    procedure GLContextCloseEvent(Sender: TObject);

    function Batching: TBatchShapes;

    { Fog for this shape. @nil if none. }
    function ShapeFog(const Shape: TShape; const GlobalFog: TFogNode): TFogFunctionality;
    function EffectiveBlendingSort: TBlendingSort;

    function FrustumCulling_None(Shape: TShape): boolean;
    function FrustumCulling_Sphere(Shape: TShape): boolean;
    function FrustumCulling_Box(Shape: TShape): boolean;
    function FrustumCulling_Both(Shape: TShape): boolean;
    function DistanceCulling_FrustumCulling_None(Shape: TShape): boolean;
    function DistanceCulling_FrustumCulling_Sphere(Shape: TShape): boolean;
    function DistanceCulling_FrustumCulling_Box(Shape: TShape): boolean;
    function DistanceCulling_FrustumCulling_Both(Shape: TShape): boolean;

    function DistanceCullingCheck(Shape: TShape): boolean;

    procedure UpdateShapeCullingCallbacks;
    procedure SetFrustumCulling(const Value: TFrustumCulling);
    procedure SetOctreeFrustumCulling(const Value: TFrustumCulling);
    procedure SetShapeFrustumCulling(const Value: Boolean);
    procedure SetDistanceCulling(const Value: Single);

    function RenderFrustumOctree_TestShape(Shape: TShape): boolean;
    procedure RenderWithOctree_CheckShapeCulling(
      ShapeIndex: Integer; CollidesForSure: boolean);

    { Turn off lights that are not supposed to light in the shadow.
      This simply turns LightOn to @false if the light has
      shadowVolumes = TRUE (see
      [https://castle-engine.io/x3d_extensions.php#section_ext_shadows]).

      It's useful to pass this as LightRenderEvent to @link(Render)
      when you use shadow algorithm that requires
      you to make a first pass rendering the scene all shadowed. }
    class procedure LightRenderInShadow(const Light: TLightInstance;
      var LightOn: boolean);

    function GetRenderOptions: TCastleRenderOptions;
  private
    PreparedShapesResources, PreparedRender: Boolean;
    Renderer: TGLRenderer;
    class procedure CreateComponent2D(Sender: TObject);
  protected
    function CreateShape(const AGeometry: TAbstractGeometryNode;
      const AState: TX3DGraphTraverseState;
      const ParentInfo: PTraversingInfo): TShape; override;
    procedure InternalInvalidateBackground; override;

    procedure LocalRender(const Params: TRenderParams); override;

    { Render shadow volume (sides and caps) of this scene, for shadow volume
      algorithm. Uses ShadowVolumeRenderer for rendering, and to detect if rendering
      is necessary at all.
      It will calculate current bounding box (looking at ParentTransform,
      ParentTransformIsIdentity and LocalBoundingBox method).

      It always uses silhouette optimization. This is the usual,
      fast method of rendering shadow volumes.
      Will not do anything (treat scene like not casting shadows,
      like CastShadowVolumes = false) if the model is not perfect 2-manifold,
      i.e. has some BorderEdges (although we could handle some BorderEdges
      for some points of view, this could leading to rendering artifacts).

      All shadow quads are generated from scene triangles transformed
      by ParentTransform. We must be able to correctly detect front and
      back facing triangles with respect to light position,
      so ShadowVolumeRenderer.LightPosition and
      "this scene transformed by ParentTransform" must be in the same coordinate system.
      If ParentTransformIsIdentity then ParentTransform value is ignored and
      everything works like ParentTransform = identity matrix (and is a little
      faster in this special case).

      Uses TrianglesListShadowCasters and ManifoldEdges and BorderEdges
      (so you may prefer to prepare it before, e.g. by calling PrepareResources
      with prShadowVolume included).

      We look at some RenderOptions, like RenderOptions.Blending, because transparent
      triangles have to be handled a little differently, and when
      RenderOptions.Blending = false then all triangles are forced to be opaque.
      In other words, this takes RenderOptions into account, to cooperate with
      our Render method.

      ShadowVolumeRenderer.LightPosition is the light position.
      ShadowVolumeRenderer.LightPosition[3] must be 1
      (to indicate positional light) or 0 (a directional light).
      It's expected that ShadowVolumeRenderer is already initialized by
      ShadowVolumeRenderer.InitFrustumAndLight.

      Faces (both shadow quads and caps) are rendered such that
      CCW <=> you're looking at it from outside
      (i.e. it's considered front face of this shadow volume). }
    procedure LocalRenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    { Destroy any associations of this object with current OpenGL context.
      For example, release any allocated texture names.

      Generally speaking, destroys everything that is allocated by
      PrepareResources call. It's harmless to call this
      method when there are already no associations with current OpenGL context.
      This is called automatically from the destructor. }
    procedure GLContextClose; override;

    procedure PrepareResources(const Options: TPrepareResourcesOptions;
      const ProgressStep: boolean; const Params: TPrepareParams); override;

    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); override;

    { Adjust parameters for rendering 2D scenes. Sets BlendingSort := bs2D,
      which is good when your transparent objects have proper order along the Z axis
      (useful e.g. for Spine animations). }
    procedure Setup2D;
  private
    FBackgroundSkySphereRadius: Single;
    { Node for which FBackground is currently prepared. }
    FBackgroundNode: TAbstractBindableNode;
    { Cached Background value }
    FBackground: TBackground;
    { Is FBackground valid ? We can't use "nil" FBackground value to flag this
      (bacause nil is valid value for Background function).
      If not FBackgroundValid then FBackground must always be nil.
      Never set FBackgroundValid to false directly - use InternalInvalidateBackground,
      this will automatically call FreeAndNil(FBackground) before setting
      FBackgroundValid to false. }
    FBackgroundValid: boolean;
    procedure SetBackgroundSkySphereRadius(const Value: Single);
    procedure PrepareBackground;
  public
    { Internal hack to avoid checking frustum at rendering in some situations. }
    InternalIgnoreFrustum: boolean;
    { Internal override test visibility. }
    InternalVisibilityTest: TTestShapeVisibility;

    procedure FreeResources(Resources: TSceneFreeResources); override;

    property BackgroundSkySphereRadius: Single
      read FBackgroundSkySphereRadius write SetBackgroundSkySphereRadius
      default 1;

    { TBackground instance to render current background. Current background
      is the top node on the BackgroundStack of this scene, following X3D
      specifications, and can be animated.
      The TCastleViewport should use this to render background.

      You should not access the background this way in your own code.
      This is public only because our own TCastleViewport needs to access it.

      If you want to change the background,
      instead of using this internal reference,
      access X3D background nodes in @code(BackgroundStack).
      You can modify existing background node by @code(BackgroundStack.Top),
      or you can push a different background node by adding @link(TBackgroundNode)
      to @link(RootNode) and setting @link(TAbstractBindableNode.Bound Background.Bound) to @true.

      Returns @nil if there is no currently bound (and supported) background node
      in this scene.

      This instance is managed (automatically created/freed
      and so on) by this TCastleScene instance. It is cached
      (so that it's recreated only when relevant things change,
      like VRML/X3D nodes affecting this background,
      or changes to BackgroundSkySphereRadius, or OpenGL context is closed).

      @exclude }
    function InternalBackground: TBackground;

    function Attributes: TCastleRenderOptions; deprecated 'use RenderOptions';

    procedure ViewChangedSuddenly; override;

    procedure InternalCameraChanged; override;

    { Screen effects information, used by TCastleViewport.ScreenEffects.
      ScreenEffectsCount may actually prepare screen effects.
      @groupBegin }
    function ScreenEffects(Index: Integer): TGLSLProgram;
    function ScreenEffectsCount: Integer;
    function ScreenEffectsNeedDepth: boolean;
    { @groupEnd }

    { Create a scene with the same contents (X3D scene graph) as this one.
      The created scene has exactly the same class as this one
      (we use ClassType.Create to call a virtual constructor).

      Note that this @bold(does not copy other scene attributes),
      like @link(ProcessEvents) or @link(Spatial) or rendering attributes
      in @link(RenderOptions). }
    function Clone(const AOwner: TComponent): TCastleScene;

    { What kind of per-shape frustum culling do when
      ShapeFrustumCulling is @true,
      and we don't have octree (ssRendering is not included in @link(TCastleSceneCore.Spatial)). }
    property FrustumCulling: TFrustumCulling
      read FFrustumCulling write SetFrustumCulling default fcBox;
      deprecated 'use simpler ShapeFrustumCulling';

    { What kind of per-shape frustum culling do when
      ShapeFrustumCulling is @true,
      and we have octree (ssRendering is included in @link(TCastleSceneCore.Spatial)). }
    property OctreeFrustumCulling: TFrustumCulling
      read FOctreeFrustumCulling write SetOctreeFrustumCulling default fcBox;
      deprecated 'use simpler ShapeFrustumCulling';
  published
    { Improve performance of rendering by checking for each shape whether
      it is inside frustum (camera pyramid of view) before rendering.

      This is almost always a good idea.
      Exception may be when, in the most common scene position,
      all the shapes are inside the frustum,
      or all the shapes are outside the frustum.
      In this case this check is wasting time,
      and it matters if you have a @italic(lot of shapes).
      In such case, @link(SceneFrustumCulling) will be enough. }
    property ShapeFrustumCulling: Boolean
      read FShapeFrustumCulling write SetShapeFrustumCulling default true;

    { Improve performance of rendering by checking for the whole scene
      whether it is inside frustum (camera pyramid of view) before rendering.

      This is almost always a good idea.
      Exception may be when the scene is almost always within the frustum,
      and you have a @italic(lot of scenes). In such case, this check may be
      a waste of time. }
    property SceneFrustumCulling: Boolean
      read FSceneFrustumCulling write FSceneFrustumCulling default true;

    { Does this scene receive shadows by shadow volumes. }
    property ReceiveShadowVolumes: boolean
      read FReceiveShadowVolumes write FReceiveShadowVolumes default true;

    { Cull shapes farther than this distance. Ignored if <= 0. }
    property DistanceCulling: Single
      read FDistanceCulling write SetDistanceCulling default 0;

    { Rendering options.
      You are free to change them at any time. }
    property RenderOptions: TCastleRenderOptions read GetRenderOptions;
  end;

  TCastleSceneClass = class of TCastleScene;

  TCastleSceneList = class(specialize TObjectList<TCastleScene>)
  end;

  TTriangle4List = specialize TStructList<TTriangle4>;

  { @exclude Internal.

    Basic non-abstract implementation of render params for calling
    TCastleTransform.LocalRender.

    @bold(This is exposed here only to support some experiments with non-standard
    rendering in engine example programs. Do not use this in your own code.)

    This can be used when you have to call TCastleTransform.LocalRender,
    but you don't use TCastleViewport.
    Usually this should not be needed.
    This class may be removed at some point!
    You should always try to use TCastleViewport to manage and render
    3D stuff in new programs, and then TCastleViewport will take care of creating
    proper render params instance for you. }
  TBasicRenderParams = class(TRenderParams)
  public
    FBaseLights: TLightInstancesList;
    constructor Create;
    destructor Destroy; override;
    function BaseLights(Scene: TCastleTransform): TLightInstancesList; override;
  end;

procedure Register;

var
  { Global OpenGL context cache.
    This caches common things, like textures, shapes, and much more.
    Our OpenGL resources are currently shared across all OpenGL contexts,
    and they all automatically share this cache. }
  GLContextCache: TGLRendererContextCache;

  InternalEnableRendering: Boolean = true;

  { Combine (right before rendering) multiple shapes with a similar appearance into one.
    This can drastically reduce the number of "draw calls",
    making rendering much faster. }
  DynamicBatching: Boolean = false;

const
  { We recommend using CastleRenderOptions unit to get these types.
    But for backward compatibility, they are also available here. }
  bsNone = CastleRenderOptions.bsNone;
  bs2D = CastleRenderOptions.bs2D;
  bs3D = CastleRenderOptions.bs3D;

  weNormal = CastleRenderOptions.weNormal;
  weWireframeOnly = CastleRenderOptions.weWireframeOnly;
  weSolidWireframe = CastleRenderOptions.weSolidWireframe;
  weSilhouette = CastleRenderOptions.weSilhouette;

  paDefault = CastleSceneCore.paDefault;
  paForceLooping = CastleSceneCore.paForceLooping;
  paForceNotLooping = CastleSceneCore.paForceNotLooping;
  paLooping = CastleSceneCore.paLooping;
  paNotLooping = CastleSceneCore.paNotLooping;

  ssRendering = CastleSceneCore.ssRendering;
  ssDynamicCollisions = CastleSceneCore.ssDynamicCollisions;
  ssVisibleTriangles = CastleSceneCore.ssVisibleTriangles;
  ssStaticCollisions = CastleSceneCore.ssStaticCollisions;

{$define read_interface}
{$I castlescene_roottransform.inc}
{$I castlescene_abstractprimitive.inc}
{$I castlescene_text.inc}
{$I castlescene_box.inc}
{$I castlescene_sphere.inc}
{$I castlescene_plane.inc}
{$I castlescene_cone.inc}
{$I castlescene_cylinder.inc}
{$undef read_interface}

implementation

{$warnings off}
// TODO: This unit temporarily uses RenderingCamera singleton,
// to keep TBasicRenderParams working for backward compatibility.
uses CastleGLVersion, CastleImages, CastleLog,
  CastleStringUtils, CastleApplicationProperties,
  CastleRenderingCamera, CastleShapeInternalRenderShadowVolumes,
  CastleComponentSerialize, CastleRenderContext, CastleFilesUtils;
{$warnings on}

{$define read_implementation}
{$I castlescene_roottransform.inc}
{$I castlescene_abstractprimitive.inc}
{$I castlescene_text.inc}
{$I castlescene_box.inc}
{$I castlescene_sphere.inc}
{$I castlescene_plane.inc}
{$I castlescene_cone.inc}
{$I castlescene_cylinder.inc}
{$undef read_implementation}

procedure Register;
begin
  {$ifdef CASTLE_REGISTER_ALL_COMPONENTS_IN_LAZARUS}
  RegisterComponents('Castle', [TCastleScene]);
  {$endif}
end;

{ TGLSceneShape -------------------------------------------------------------- }

type
  { TGLShape that can access internal data of TCastleScene. }
  TGLSceneShape = class(TGLShape)
  public
    function Renderer: TGLRenderer; override;
    procedure SchedulePrepareResources; override;
  end;

function TGLSceneShape.Renderer: TGLRenderer;
begin
  Result := TCastleScene(ParentScene).Renderer;
end;

procedure TGLSceneShape.SchedulePrepareResources;
begin
  TCastleScene(ParentScene).PreparedShapesResources := false;
end;

{ TCastleScene.TCustomShaders ------------------------------------------------ }

procedure TCastleScene.TCustomShaders.Initialize(const VertexCode, FragmentCode: string);

  procedure DoInitialize(const VertexCode, FragmentCode: string);
  begin
    { create programs if needed }
    if Shader = nil then
    begin
      Shader := TX3DShaderProgramBase.Create;
      Shader.AttachVertexShader(VertexCode);
      Shader.AttachFragmentShader(FragmentCode);
      Shader.Link;
    end;

    if ShaderAlphaTest = nil then
    begin
      ShaderAlphaTest := TX3DShaderProgramBase.Create;
      ShaderAlphaTest.AttachVertexShader('#define ALPHA_TEST' + NL + VertexCode);
      ShaderAlphaTest.AttachFragmentShader('#define ALPHA_TEST' + NL + FragmentCode);
      ShaderAlphaTest.Link;
    end;
  end;

begin
  try
    DoInitialize(VertexCode, FragmentCode);
  except
    on E: EGLSLError do
    begin
      FreeAndNil(Shader);
      FreeAndNil(ShaderAlphaTest);

      WritelnWarning('Scene', 'Error compiling/linking GLSL shaders for shadow maps: %s',
        [E.Message]);

      DoInitialize({$I fallback.vs.inc}, {$I fallback.fs.inc});
    end;
  end;
end;

procedure TCastleScene.TCustomShaders.Free;
begin
  FreeAndNil(Shader);
  FreeAndNil(ShaderAlphaTest);
end;

{ TCastleScene.TSceneRenderOptions ------------------------------------------- }

procedure TCastleScene.TSceneRenderOptions.ReleaseCachedResources;
begin
  inherited;

  { We have to do at least Renderer.UnprepareAll.
    Actually, we have to do more: TCastleScene must also be disconnected
    from OpenGL, to release screen effects (referencing renderer shaders)
    and such. So full GLContextClose is needed. }

  OwnerScene.GLContextClose;

  { If OcclusionQuery just changed:
    If you switch OcclusionQuery on, then off, then move around the scene
    a lot, then switch OcclusionQuery back on --- you don't want to use
    results from previous query that was done many frames ago. }
  OwnerScene.ViewChangedSuddenly;
end;

{ TCastleScene ------------------------------------------------------------ }

constructor TCastleScene.Create(AOwner: TComponent);
begin
  { inherited Create *may* call some virtual things overriden here
    (although right now it doesn't): it may bind new viewpoint which
    may call ViewChangedSuddenly which is overridden here and uses RenderOptions.
    That's why I have to initialize them *before* "inherited Create" }

  Renderer := TGLRenderer.Create(TSceneRenderOptions, GLContextCache);

  { Setup RenderOptions as proper sub-component.
    Note that this calls Renderer.RenderOptions, so use this only after initializing Renderer. }
  (RenderOptions as TSceneRenderOptions).OwnerScene := Self;
  RenderOptions.SetSubComponent(true);
  RenderOptions.Name := 'RenderOptions';

  inherited Create(AOwner);

  FBackgroundSkySphereRadius := 1.0;
  FBackgroundValid := false;
  FBackgroundNode := nil;
  FBackground := nil;

  FSceneFrustumCulling := true;
  FShapeFrustumCulling := true;
  FFrustumCulling := fcBox;
  FOctreeFrustumCulling := fcBox;
  UpdateShapeCullingCallbacks;

  FReceiveShadowVolumes := true;

  FilteredShapes := TShapeList.Create;
  FTempPrepareParams := TPrepareParams.Create;

  OcclusionQueryUtilsRenderer := TOcclusionQueryUtilsRenderer.Create;
  SimpleOcclusionQueryRenderer := TSimpleOcclusionQueryRenderer.Create(
    Self, OcclusionQueryUtilsRenderer);
  HierarchicalOcclusionQueryRenderer := THierarchicalOcclusionQueryRenderer.Create(
    Self, OcclusionQueryUtilsRenderer);
  BlendingRenderer := TBlendingRenderer.Create(Self);
end;

destructor TCastleScene.Destroy;
begin
  FreeAndNil(HierarchicalOcclusionQueryRenderer);
  FreeAndNil(SimpleOcclusionQueryRenderer);
  FreeAndNil(OcclusionQueryUtilsRenderer);
  FreeAndNil(BlendingRenderer);
  FreeAndNil(FilteredShapes);
  FreeAndNil(FTempPrepareParams);
  FreeAndNil(FBatching);

  if RegisteredGLContextCloseListener and
     (ApplicationProperties <> nil) then
  begin
    ApplicationProperties.OnGLContextCloseObject.Remove(@GLContextCloseEvent);
    RegisteredGLContextCloseListener := false;
  end;

  GLContextClose;

  { Note that this calls Renderer.RenderOptions, so use this before
    deinitializing Renderer. }
  if Renderer <> nil then
    (RenderOptions as TSceneRenderOptions).OwnerScene := nil;

  { We must release all connections between RootNode and Renderer first.
    Reason: when freeing RootNode, image references (from texture nodes)
    are decremented. So cache used when loading these images must be
    available.

    If we used custom renderer, then this is not
    our problem: if OwnsRootNode then RootNode will be freed soon
    by "inherited", if not OwnsRootNode then it's the using programmer
    responsibility to free both RootNode and CustomRenderer
    in exactly this order.

    If we used our own renderer (actually, this is needed only if we used
    own own cache, so caller didn't provide a renderer and didn't provide
    a cache (ACache = nil for constructor), but we don't store this information
    for now) : we must make sure that freeing RootNode is safe.

    If OwnsRootNode then we know that inherited will free RootNode
    and so the simpler solution, to just FreeAndNil(Renderer) after
    inherited, would be possible. But it's not possible, since
    OwnsRootNode may be false and then programmer may want to free
    RootNode at undefined later time.

    So we have to guarantee, *now*, that freeing RootNode is safe ---
    no dangling references to Renderer.Cache. }
  FreeResources([frTextureDataInNodes, frBackgroundImageInNodes]);

  FreeAndNil(Renderer);

  inherited;
end;

function TCastleScene.CreateShape(const AGeometry: TAbstractGeometryNode;
  const AState: TX3DGraphTraverseState; const ParentInfo: PTraversingInfo): TShape;
begin
  Result := TGLSceneShape.Create(Self, AGeometry, AState, ParentInfo);
end;

procedure TCastleScene.GLContextClose;

  { This must be coded carefully, because
    - it's called by ChangedAll, and so may be called when our constructor
      didn't do it's work yet.
    - moreover it's called from destructor, so may be called if our
      constructor terminated with exception.
    So e.g. we have to check Renderer <> nil, Shapes <> nil here. }

  { Call TGLShape.GLContextClose. }
  procedure ShapesGLContextClose;
  var
    ShapeList: TShapeList;
    Shape: TShape;
  begin
    if Shapes <> nil then
    begin
      { Iterate even over non-visible shapes too, for safety:
        since this GLContextClose may happen after some
        "visibility" changed, that is you changed proxy
        or such by event. }
      ShapeList := Shapes.TraverseList(false, false);
      for Shape in ShapeList do
        TGLShape(Shape).GLContextClose;
    end;
  end;

  { Release screen effects OpenGL stuff. }
  procedure ScreenEffectsGLContextClose;
  var
    I: Integer;
    Node: TScreenEffectNode;
  begin
    if ScreenEffectNodes <> nil then
      for I := 0 to ScreenEffectNodes.Count - 1 do
      begin
        Node := TScreenEffectNode(ScreenEffectNodes[I]);
        { The TGLSLProgram instance here will be released by Rendered.UnprepareAll,
          that eventually calls GLSLRenderers.UnprepareAll,
          that eventually calls Cache.GLSLProgram_DecReference on this shader,
          that eventuallly destroys TGLSLProgram instance.
          So below only set it to nil. }
        Node.Shader := nil;
        Node.ShaderLoaded := false;
      end;
  end;

  { When the OpenGL(ES) context is lost, generated textures contents are lost.
    Make sure to regenerate them when entering context again.
    Testcase: Silhouette on Android, switch from application and back. }
  procedure ScheduleUpdateGeneratedTextures;
  var
    I: Integer;
  begin
    if GeneratedTextures <> nil then
      for I := 0 to GeneratedTextures.Count - 1 do
        GeneratedTextures.List^[I].Handler.InternalUpdateNeeded := true;
  end;

begin
  inherited;

  PreparedRender := false;
  PreparedShapesResources := false;

  ScreenEffectsGLContextClose;

  ShapesGLContextClose;

  if Renderer <> nil then
    Renderer.UnprepareAll;

  VarianceShadowMapsProgram.Free;
  ShadowMapsProgram.Free;

  ScheduleUpdateGeneratedTextures;

  InternalInvalidateBackground;

  if OcclusionQueryUtilsRenderer <> nil then
    OcclusionQueryUtilsRenderer.GLContextClose;

  if FBatching <> nil then
    FBatching.GLContextClose;
end;

procedure TCastleScene.GLContextCloseEvent(Sender: TObject);
begin
  GLContextClose;
end;

function TCastleScene.ShapeFog(const Shape: TShape; const GlobalFog: TFogNode): TFogFunctionality;
begin
  Result := nil;

  if {(Result = nil) and} (Shape.State.LocalFog <> nil) then
    Result := Shape.State.LocalFog.Functionality(TFogFunctionality) as TFogFunctionality;
  if (Result = nil) and (FogStack.Top <> nil) then
    Result := FogStack.Top.Functionality(TFogFunctionality) as TFogFunctionality;
  if (Result = nil) and (GlobalFog <> nil) then
    Result := GlobalFog.Functionality(TFogFunctionality) as TFogFunctionality;
end;

function TCastleScene.EffectiveBlendingSort: TBlendingSort;
begin
  if (NavigationInfoStack.Top <> nil) and
     (NavigationInfoStack.Top.BlendingSort <> obsDefault) then
  begin
    case NavigationInfoStack.Top.BlendingSort of
      obsNone: Result := bsNone;
      obs2D  : Result := bs2D;
      obs3D  : Result := bs3D;
      else raise EInternalError.Create('TCastleScene.EffectiveBlendingSort:NavigationInfoStack.Top.BlendingSort?');
    end;
  end else
    Result := RenderOptions.BlendingSort;
end;

procedure TCastleScene.LocalRenderInside(
  const TestShapeVisibility: TTestShapeVisibility;
  const Params: TRenderParams);
var
  ModelView: TMatrix4;

  { Transformation of Params.Transform and current RenderingCamera
    expressed as a single combined matrix. }
  function GetModelViewTransform: TMatrix4;
  var
    CameraMatrix: PMatrix4;
  begin
    if Params.RenderingCamera.RotationOnly then
      CameraMatrix := @Params.RenderingCamera.RotationMatrix
    else
      CameraMatrix := @Params.RenderingCamera.Matrix;

    if Params.TransformIdentity then
      Result := CameraMatrix^
    else
      Result := CameraMatrix^ * Params.Transform^;
  end;

  { Renders Shape, caling unconditionally Renderer.RenderShape. }
  procedure RenderShape_NoTests(const Shape: TGLShape);
  begin
    Shape.ModelView := ModelView;
    Shape.Fog := ShapeFog(Shape, Params.GlobalFog as TFogNode);

    OcclusionQueryUtilsRenderer.OcclusionBoxStateEnd;

    if (Params.InternalPass = 0) and not ExcludeFromStatistics then
      Inc(Params.Statistics.ShapesRendered);

    BlendingRenderer.BeforeRenderShape(Shape);
    Renderer.RenderShape(Shape);
    IsVisibleNow := true;
  end;

  { Renders Shape, testing only Batching.Collect before RenderShape_NoTests.
    This sets Shape.ModelView and other properties necessary right before rendering. }
  procedure RenderShape_BatchingTest(const Shape: TGLShape);
  begin
    if not (DynamicBatching and Batching.Collect(Shape)) then
      RenderShape_NoTests(Shape);
  end;

  procedure BatchingCommit;
  var
    Shape: TShape;
  begin
    if DynamicBatching then
    begin
      Batching.Commit;
      for Shape in Batching.Collected do
      begin
        TGLShape(Shape).PrepareResources; // otherwise, shapes from batching FPool would never have PrepareResources called?
        RenderShape_NoTests(TGLShape(Shape));
      end;
      Batching.FreeCollected;
    end;
  end;

  { Render Shape if all tests pass, except TestShapeVisibility callback is ignored.
    It assumes that filtering by TestShapeVisibility is already done. }
  procedure RenderShape_SomeTests(const Shape: TGLShape);
  begin
    if (Shape <> AvoidShapeRendering) and
       ( (not AvoidNonShadowCasterRendering) or Shape.ShadowCaster) and
       ( { implement Shape node "render" field here, by a trivial check }
         (Shape.Node = nil) or Shape.Node.Render
       ) then
    begin
      { We do not make occlusion query when rendering to something else
        than screen (like shadow map or cube map environment for mirror).
        Such views are drastically different from normal camera view,
        so the whole idea that "what is visible in this frame is similar
        to what was visible in previous frame" breaks down there.

        TODO: In the future, this could be solved nicer, by having separate
        occlusion query states for different views. But this isn't easy
        to implement, as occlusion query state is part of TShape and
        octree nodes (for hierarchical occ query), so all these things
        should have a map "target->oq state" for various rendering targets. }

      if ReallyOcclusionQuery(RenderOptions) and
         (Params.RenderingCamera.Target = rtScreen) then
      begin
        SimpleOcclusionQueryRenderer.Render(Shape, @RenderShape_BatchingTest, Params);
      end else
      {$warnings off}
      if RenderOptions.DebugHierOcclusionQueryResults and
         RenderOptions.HierarchicalOcclusionQuery then
      {$warnings on}
      begin
        if HierarchicalOcclusionQueryRenderer.WasLastVisible(Shape) then
          RenderShape_BatchingTest(Shape);
      end else
        { No occlusion query-related stuff. Just render the shape. }
        RenderShape_BatchingTest(Shape);
    end;
  end;

  { Render Shape if all tests pass. }
  procedure RenderShape_AllTests(const Shape: TShape);
  begin
    if ( (not Assigned(TestShapeVisibility)) or
         TestShapeVisibility(TGLShape(Shape))) then
      RenderShape_SomeTests(TGLShape(Shape));
  end;

  { Render Shape if all tests pass, and it is opaque. }
  procedure RenderShape_AllTests_Opaque(const Shape: TShape);
  begin
    if not TGLShape(Shape).UseBlending then
      RenderShape_AllTests(Shape);
  end;

  { Render Shape if all tests pass, and it is using blending. }
  procedure RenderShape_AllTests_Blending(const Shape: TShape);
  begin
    if TGLShape(Shape).UseBlending then
      RenderShape_AllTests(Shape);
  end;

  procedure RenderAllAsOpaque(
    const IgnoreShapesWithBlending: Boolean = false;
    const BlendingPipeline: Boolean = false);
  begin
    if BlendingPipeline = Params.Transparent then
    begin
      if IgnoreShapesWithBlending then
        Shapes.Traverse(@RenderShape_AllTests_Opaque, true, true)
      else
        Shapes.Traverse(@RenderShape_AllTests, true, true);
    end;
  end;

  procedure UpdateVisibilitySensors;
  var
    J: Integer;
    Instances: TVisibilitySensorInstanceList;
    NewActive: boolean;
    VisibilitySensorsPair: TVisibilitySensors.TDictionaryPair;
  begin
    { optimize for common case: exit early if nothing to do }
    if VisibilitySensors.Count = 0 then Exit;

    if ProcessEvents then
    begin
      BeginChangesSchedule;
      try
        for VisibilitySensorsPair in VisibilitySensors do
          if VisibilitySensorsPair.Key.Enabled then
          begin
            { calculate NewActive }
            NewActive := false;
            Instances := VisibilitySensorsPair.Value;
            for J := 0 to Instances.Count - 1 do
              if Params.Frustum^.Box3DCollisionPossibleSimple(Instances[J].Box) then
              begin
                NewActive := true;
                Break;
              end;
            { Note that NextEventTime below increases time tick for every
              VisibilitySensor, which is good,
              otherwise sensors_environmental/visibility_sensor.x3dv
              has a problem at initialization, when multiple sensors
              send isActive = TRUE, and X3D mechanism to avoid loops
              kicks in. }
            VisibilitySensorsPair.Key.SetIsActive(NewActive, NextEventTime);
          end;
      finally EndChangesSchedule; end;
    end;
  end;

  { Render for RenderOptions.Mode = rmFull }
  procedure RenderModeFull;
  var
    I: Integer;
  begin
    if ReallyHierarchicalOcclusionQuery(RenderOptions) and
       (not RenderOptions.DebugHierOcclusionQueryResults) and
       (Params.RenderingCamera.Target = rtScreen) and
       (InternalOctreeRendering <> nil) then
    begin
      HierarchicalOcclusionQueryRenderer.Render(@RenderShape_SomeTests,
        Params, RenderCameraPosition);
    end else
    begin
      if RenderOptions.Blending then
      begin
        if not Params.Transparent then
        begin
          { draw fully opaque objects }
          if ReallyOcclusionQuery(RenderOptions) or RenderOptions.OcclusionSort then
          begin
            ShapesFilterBlending(Shapes, true, true, false,
              TestShapeVisibility, FilteredShapes, false);

            { ShapesSplitBlending already filtered shapes through
              TestShapeVisibility callback, so later we can render them
              with RenderShape_SomeTests to skip checking TestShapeVisibility
              twice. This is a good thing: it means that sorting below has
              much less shapes to consider. }
            FilteredShapes.SortFrontToBack(RenderCameraPosition);
            if DynamicBatching then
              Batching.PreserveShapeOrder := true;
            for I := 0 to FilteredShapes.Count - 1 do
              RenderShape_SomeTests(TGLShape(FilteredShapes[I]));
          end else
            Shapes.Traverse(@RenderShape_AllTests_Opaque, true, true, false);
        end else
        { this means Params.Transparent = true }
        begin
          { draw partially transparent objects }
          BlendingRenderer.RenderBegin;

          { sort for blending, if BlendingSort not bsNone.
            Note that bs2D does not require knowledge of the camera,
            RenderCameraPosition is unused in this case by FilteredShapes.SortBackToFront }
          if EffectiveBlendingSort in [bs3D, bs2D] then
          begin
            ShapesFilterBlending(Shapes, true, true, false,
              TestShapeVisibility, FilteredShapes, true);
            FilteredShapes.SortBackToFront(RenderCameraPosition, EffectiveBlendingSort = bs3D);
            if DynamicBatching then
              Batching.PreserveShapeOrder := true;
            for I := 0 to FilteredShapes.Count - 1 do
              RenderShape_SomeTests(TGLShape(FilteredShapes[I]));
          end else
            Shapes.Traverse(@RenderShape_AllTests_Blending, true, true, false);
        end;

      end else
        RenderAllAsOpaque;
    end;
  end;

var
  LightRenderEvent: TLightRenderEvent;
begin
  { We update XxxVisible only for one value of Params.Transparent.
    Otherwise, we would increase it twice.
    This method is always called first with Params.Transparent = false,
    then Params.Transparent = true during a single frame. }
  if (not Params.Transparent) and (Params.InternalPass = 0) then
  begin
    if not ExcludeFromStatistics then
      Params.Statistics.ShapesVisible += ShapesActiveVisibleCount;
    { also do this only once per frame }
    UpdateVisibilitySensors;
  end;

  if Params.InShadow then
    LightRenderEvent := @LightRenderInShadow
  else
    LightRenderEvent := nil;

  ModelView := GetModelViewTransform;

  { update OcclusionQueryUtilsRenderer.ModelViewProjectionMatrix if necessary }
  if ReallyOcclusionQuery(RenderOptions) or
     ReallyHierarchicalOcclusionQuery(RenderOptions) then
  begin
    OcclusionQueryUtilsRenderer.ModelViewProjectionMatrix :=
      RenderContext.ProjectionMatrix * ModelView;
    OcclusionQueryUtilsRenderer.ModelViewProjectionMatrixChanged := true;
  end;

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    glPushMatrix;
    glLoadMatrix(ModelView);
  end;
  {$endif}

  Renderer.RenderBegin(Params.BaseLights(Self) as TLightInstancesList,
    Params.RenderingCamera,
    LightRenderEvent, Params.InternalPass, InternalScenePass, Params.UserPass);
  try
    case RenderOptions.Mode of
      rmDepth:
        { When not rmFull, we don't want to do anything with glDepthMask
          or GL_BLEND enable state. Just render everything
          (except: don't render partially transparent stuff for shadow maps). }
        RenderAllAsOpaque(true);
      rmSolidColor:
        RenderAllAsOpaque(false, RenderOptions.SolidColorBlendingPipeline);
      rmFull:
        RenderModeFull;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('RenderOptions.Mode?');
      {$endif}
    end;

    BatchingCommit;

    { this must be called after BatchingCommit,
      since BatchingCommit may render some shapes }
    BlendingRenderer.RenderEnd;

    { Each RenderShape_SomeTests inside could set OcclusionBoxState.

      TODO: in case of fixed-function path,
      glPopAttrib inside could restore now
      glDepthMask(GL_TRUE) and glDisable(GL_BLEND).
      This problem will disappear when we'll get rid of fixed-function
      possibility in OcclusionBoxStateEnd. }
    OcclusionQueryUtilsRenderer.OcclusionBoxStateEnd;
  finally Renderer.RenderEnd end;

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
    glPopMatrix;
  {$endif}
end;

procedure TCastleScene.PrepareResources(
  const Options: TPrepareResourcesOptions;
  const ProgressStep: boolean; const Params: TPrepareParams);

  procedure PrepareShapesResources;
  var
    ShapeList: TShapeList;
    Shape: TShape;
    I: Integer;
  begin
    ShapeList := Shapes.TraverseList(false, false);
    for Shape in ShapeList do
      TGLShape(Shape).PrepareResources;

    if DynamicBatching then
      for I := 0 to Batching.PoolShapesCount - 1 do
        Batching.PoolShapes[I].PrepareResources;
  end;

  procedure PrepareRenderShapes;
  var
    ShapeList: TShapeList;
    Shape: TShape;
    BaseLights: TLightInstancesList;
    GoodParams, OwnParams: TPrepareParams;
    DummyCamera: TRenderingCamera;
    I: Integer;
  begin
    if LogRenderer then
      WritelnLog('Renderer', 'Preparing rendering of all shapes');

    { Note: we prepare also not visible shapes, in case they become visible. }
    ShapeList := Shapes.TraverseList(false, false);

    { Prepare resources by doing rendering.
      But with Renderer.RenderMode set to rmPrepareRenderXxx so nothing will be actually drawn. }

    if prRenderSelf in Options then
      Renderer.RenderMode := rmPrepareRenderSelf
    else
    begin
      Assert(prRenderClones in Options);
      Renderer.RenderMode := rmPrepareRenderClones;
    end;

    { calculate OwnParams, GoodParams }
    if Params = nil then
    begin
      WritelnWarning('PrepareResources', 'Do not pass Params=nil to TCastleScene.PrepareResources or T3DResource.Prepare or friends. Get the params from Viewport.PrepareParams (create a temporary TCastleViewport if you need to).');
      OwnParams := TPrepareParams.Create;
      GoodParams := OwnParams;
    end else
    begin
      OwnParams := nil;
      GoodParams := Params;
    end;

    BaseLights := GoodParams.InternalBaseLights as TLightInstancesList;

    { We need some non-nil TRenderingCamera instance to be able
      to render with lights. }
    DummyCamera := TRenderingCamera.Create;
    try
      { Set matrix to be anything sensible.
        Otherwise opening a scene with shadow maps makes a warning
        that camera matrix is all 0,
        and cannot be inverted, since
        TTextureCoordinateRenderer.RenderCoordinateBegin does
        RenderingCamera.InverseMatrixNeeded.
        Testcase: silhouette. }
      DummyCamera.FromMatrix(TVector3.Zero,
        TMatrix4.Identity, TMatrix4.Identity, TMatrix4.Identity);

      Renderer.RenderBegin(BaseLights, DummyCamera, nil, 0, 0, 0);

      for Shape in ShapeList do
      begin
        { set sensible Shape.ModelView, otherwise it is zero
          and TShader.EnableClipPlane will raise an exception since
          PlaneTransform(Plane, SceneModelView); will fail,
          with SceneModelView matrix = zero. }
        TGLShape(Shape).ModelView := TMatrix4.Identity;
        TGLShape(Shape).Fog := ShapeFog(Shape, GoodParams.InternalGlobalFog as TFogNode);
        Renderer.RenderShape(TGLShape(Shape));
      end;

      if DynamicBatching then
        for I := 0 to Batching.PoolShapesCount - 1 do
        begin
          Shape := Batching.PoolShapes[I];
          TGLShape(Shape).ModelView := TMatrix4.Identity;
          TGLShape(Shape).Fog := ShapeFog(Shape, GoodParams.InternalGlobalFog as TFogNode);
          Renderer.RenderShape(TGLShape(Shape));
        end;

      Renderer.RenderEnd;
    finally FreeAndNil(DummyCamera) end;

    FreeAndNil(OwnParams);

    Renderer.RenderMode := rmRender; // restore Renderer.RenderMode
  end;

var
  I: Integer;
  PossiblyTimeConsuming: Boolean;
  TimeStart: TCastleProfilerTime;
begin
  inherited;

  if InternalDirty <> 0 then Exit;

  if not ApplicationProperties.IsGLContextOpen then
  begin
    WritelnLog('PrepareResources', 'OpenGL context not available, skipping preparing TCastleScene OpenGL resources');
    Exit;
  end;

  if not RegisteredGLContextCloseListener then
  begin
    RegisteredGLContextCloseListener := true;
    ApplicationProperties.OnGLContextCloseObject.Add(@GLContextCloseEvent);
  end;

  { When preparing resources, files (like textures) may get loaded,
    causing progress bar (for example from CastleDownload).
    Right now we're not ready to display the (partially loaded) scene
    during this time, so we use InternalDirty to prevent it.

    Test http://svn.code.sf.net/p/castle-engine/code/trunk/demo_models/navigation/transition_multiple_viewpoints.x3dv
    Most probably problems are caused because shapes are initially
    without a texture, so their arrays (including VBOs) are generated
    without texture coordinates, and we do not mark them to be prepared
    correctly later. Correct fix is unsure:
    - Marking relevant shapes to be prepared again seems easiest,
      but this means that potentially everything is prepared 2 times
      --- once before resources (like textures) are ready, 2nd time with.
    - It would be best to pas texture coordinates even when no texture is loaded?
      Ideally, the renderer operations should be the same regardless if texture
      is loaded or not.
      It remains to carefully see whether it's possible in all cases.
  }

  Inc(InternalDirty);
  try
    PossiblyTimeConsuming := (not PreparedShapesResources) or (not PreparedRender);

    if PossiblyTimeConsuming then
      TimeStart := Profiler.Start('Prepare Scene Resources ' + URL);

    if not PreparedShapesResources then
    begin
      { Use PreparedShapesResources to avoid expensive (for large scenes)
        iteration over all shapes in every TCastleScene.PrepareResources call. }
      PreparedShapesResources := true;
      PrepareShapesResources;
    end;

    if ([prRenderSelf, prRenderClones] * Options <> []) and not PreparedRender then
    begin
      { We use PreparedRender to avoid potentially expensive iteration
        over shapes and expensive Renderer.RenderBegin/End. }
      PreparedRender := true;
      PrepareRenderShapes;
    end;

    if prBackground in Options then
      PrepareBackground;

    if prScreenEffects in Options then
    begin
      for I := 0 to ScreenEffectNodes.Count - 1 do
        Renderer.PrepareScreenEffect(ScreenEffectNodes[I] as TScreenEffectNode);
    end;

    if PossiblyTimeConsuming then
      Profiler.Stop(TimeStart);
  finally Dec(InternalDirty) end;
end;

procedure TCastleScene.LocalRenderOutside(
  const TestShapeVisibility: TTestShapeVisibility;
  const Params: TRenderParams);

  procedure RenderNormal;
  begin
    LocalRenderInside(TestShapeVisibility, Params);
  end;

  {$ifndef OpenGLES} // TODO-es For OpenGLES, wireframe must be done differently
  { This code uses a lot of deprecated stuff. It is already marked with TODO above. }
  {$warnings off}
  procedure RenderWireframe(UseWireframeColor: boolean);
  var
    SavedMode: TRenderingMode;
    SavedSolidColor: TCastleColorRGB;
  begin
    glPushAttrib(GL_POLYGON_BIT or GL_CURRENT_BIT or GL_ENABLE_BIT);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); { saved by GL_POLYGON_BIT }

      if UseWireframeColor then
      begin
        SavedMode := RenderOptions.Mode;
        SavedSolidColor := RenderOptions.SolidColor;
        RenderOptions.Mode := rmSolidColor;
        RenderOptions.SolidColor := RenderOptions.WireframeColor;
      end;

      RenderNormal;

      if UseWireframeColor then
      begin
        RenderOptions.Mode := SavedMode;
        RenderOptions.SolidColor := SavedSolidColor;
      end;
    glPopAttrib;
  end;
  {$warnings on}
  {$endif}

  { Render taking RenderOptions.WireframeEffect into account.
    Also controls InternalScenePass,
    this way shaders from RenderNormal and RenderWireframe can coexist,
    which avoids FPS drops e.g. at weSilhouette rendering a single 3D model. }
  procedure RenderWithWireframeEffect;
  // TODO-es For OpenGLES, wireframe must be done differently
  {$ifndef OpenGLES}
  { This code uses a lot of deprecated stuff. It is already marked with TODO above. }
  {$warnings off}
  begin
    case RenderOptions.WireframeEffect of
      weNormal:
        begin
          InternalScenePass := 0;
          RenderNormal;
        end;
      weWireframeOnly:
        begin
          InternalScenePass := 1;
          RenderWireframe(RenderOptions.Mode = rmSolidColor);
        end;
      weSolidWireframe:
        begin
          InternalScenePass := 0;
          glPushAttrib(GL_POLYGON_BIT);
            { enable polygon offset for everything (whole scene) }
            glEnable(GL_POLYGON_OFFSET_FILL); { saved by GL_POLYGON_BIT }
            glEnable(GL_POLYGON_OFFSET_LINE); { saved by GL_POLYGON_BIT }
            glEnable(GL_POLYGON_OFFSET_POINT); { saved by GL_POLYGON_BIT }
            glPolygonOffset(RenderOptions.SolidWireframeScale, RenderOptions.SolidWireframeBias); { saved by GL_POLYGON_BIT }
            RenderNormal;
          glPopAttrib;

          InternalScenePass := 1;
          RenderWireframe(true);
        end;
      weSilhouette:
        begin
          InternalScenePass := 0;
          RenderNormal;

          InternalScenePass := 1;
          glPushAttrib(GL_POLYGON_BIT);
            glEnable(GL_POLYGON_OFFSET_LINE); { saved by GL_POLYGON_BIT }
            glPolygonOffset(RenderOptions.SilhouetteScale, RenderOptions.SilhouetteBias); { saved by GL_POLYGON_BIT }

            (* Old idea, may be resurrected one day:

            { rmSolidColor still does backface culling.
              This is very good in this case. When rmSolidColor and weSilhouette,
              and objects are solid (so backface culling is used) we can
              significantly improve the effect by reverting glFrontFace,
              this way we will cull *front* faces. This will not be noticed
              in case of rmSolidColor will single solid color, and it will
              improve the silhouette look, since front-face edges will not be
              rendered at all (no need to even hide them by glPolygonOffset,
              which is somewhat sloppy).

              TODO: this is probably incorrect now, that some meshes
              may have FrontFaceCcw = false.
              What we really would like to is to negate the FrontFaceCcw
              interpretation inside this RenderWireframe call.
            }
            if RenderOptions.Mode = rmSolidColor then
              glFrontFace(GL_CW); { saved by GL_POLYGON_BIT }
            *)

            RenderWireframe(true);
          glPopAttrib;
        end;
      else raise EInternalError.Create('Render: RenderOptions.WireframeEffect ?');
    end;
  {$warnings on}
  {$else}
  begin
    InternalScenePass := 0;
    RenderNormal;
  {$endif}
  end;

  { Render, doing some special tricks when rendering to shadow maps. }
  procedure RenderWithShadowMaps;
  var
    SavedMode: TRenderingMode;
    SavedShaders, NewShaders: TCustomShaders;
  begin
    { For shadow maps, speed up rendering by using only features that affect
      depth output. Also set up specialized shaders. }
    if Params.RenderingCamera.Target in [rtVarianceShadowMap, rtShadowMap] then
    begin
      SavedMode := RenderOptions.Mode;
      RenderOptions.Mode := rmDepth;

      if Params.RenderingCamera.Target = rtVarianceShadowMap then
      begin
        VarianceShadowMapsProgram.Initialize(
          '#define VARIANCE_SHADOW_MAPS' + NL + {$I shadow_map_generate.vs.inc},
          '#define VARIANCE_SHADOW_MAPS' + NL + {$I shadow_map_generate.fs.inc});
        NewShaders := VarianceShadowMapsProgram;
      end else
      begin
        ShadowMapsProgram.Initialize(
          {$I shadow_map_generate.vs.inc},
          {$I shadow_map_generate.fs.inc});
        NewShaders := ShadowMapsProgram;
      end;

      {$warnings off}
      SavedShaders.Shader          := RenderOptions.CustomShader as TX3DShaderProgramBase;
      SavedShaders.ShaderAlphaTest := RenderOptions.CustomShaderAlphaTest as TX3DShaderProgramBase;
      RenderOptions.CustomShader          := NewShaders.Shader;
      RenderOptions.CustomShaderAlphaTest := NewShaders.ShaderAlphaTest;
      {$warnings on}
    end;

    RenderWithWireframeEffect;

    if Params.RenderingCamera.Target in [rtVarianceShadowMap, rtShadowMap] then
    begin
      RenderOptions.Mode := SavedMode;
      {$warnings off}
      RenderOptions.CustomShader          := SavedShaders.Shader;
      RenderOptions.CustomShaderAlphaTest := SavedShaders.ShaderAlphaTest;
      {$warnings on}
    end;
  end;

begin
  { This is usually called by LocalRender(Params) that probably
    already did tests below. But it may also be called directly,
    so do the checks below anyway. (The checks are trivial, so no speed harm.) }
  if GetVisible and
     (InternalDirty = 0) and
     (ReceiveShadowVolumes in Params.ShadowVolumesReceivers) then
  begin
    { I used to make here more complex "prepare" mechanism, that was trying
      to prepare for particular shapes only right before they are rendered
      (so instead of calling PrepareResources below, I was calling PrepareShape
      at the beginning of each RenderShape and such).

      After a while, it turns out this was a useless complication of code
      logic. There are many things that *have* to be prepared before whole
      rendering, for example
      - UseBlending must be calculated for all shapes.
      - Occlusion query id must be generated (as we may start occlusion query
        before actually rendering the shape).

      It's much simpler to just call PrepareResources at the beginning.
      The PrepareResources is already optimized to do nothing,
      if everything is ready. }
    FTempPrepareParams.InternalBaseLights := Params.BaseLights(Self);
    FTempPrepareParams.InternalGlobalFog := Params.GlobalFog;
    PrepareResources([prRenderSelf], false, FTempPrepareParams);

    RenderWithShadowMaps;
  end;
end;

class procedure TCastleScene.LightRenderInShadow(const Light: TLightInstance;
  var LightOn: boolean);
begin
  if Light.Node.FdShadowVolumes.Value then
    LightOn := false;
end;

class procedure TCastleScene.CreateComponent2D(Sender: TObject);
begin
  (Sender as TCastleScene).Setup2D;
end;

procedure TCastleScene.BeforeNodesFree(const InternalChangedAll: boolean);
begin
  { Release all associations with OpenGL context before freeing the nodes.
    This means vrml nodes are still valid during GLRenderer unprepare
    calls.

    Although we don't really want to lose our connection with OpenGL
    context, in fact that's the only sensible thing to do now: since
    everything possibly changed, we have to unprepare all now.

    This is done before inherited, as inherited may clear Shapes tree
    (clearing per-shape information about referenced vbos etc.). }
  GLContextClose;

  inherited;
end;

{ Shadow volumes ------------------------------------------------------------- }

procedure TCastleScene.LocalRenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4);
var
  SceneBox, ShapeBox: TBox3D;
  SVRenderer: TGLShadowVolumeRenderer;
  ShapeList: TShapeList;
  Shape: TShape;
  T: TMatrix4;
  ForceOpaque: boolean;
begin
  if GetVisible and CastShadowVolumes then
  begin
    SVRenderer := ShadowVolumeRenderer as TGLShadowVolumeRenderer;

    ForceOpaque := not (RenderOptions.Blending and (RenderOptions.Mode = rmFull));

    { calculate and check SceneBox }
    SceneBox := LocalBoundingBox;
    if not ParentTransformIsIdentity then
      SceneBox := SceneBox.Transform(ParentTransform);
    SVRenderer.InitCaster(SceneBox);
    if SVRenderer.CasterShadowPossiblyVisible then
    begin
      { shadows are cast only by visible scene parts
        (not e.g. invisible collision box of castle-anim-frames) }
      ShapeList := Shapes.TraverseList({ OnlyActive } true, { OnlyVisible } true);
      for Shape in ShapeList do
      begin
        ShapeBox := Shape.BoundingBox;
        if not ParentTransformIsIdentity then
          ShapeBox := ShapeBox.Transform(ParentTransform);
        SVRenderer.InitCaster(ShapeBox);
        if SVRenderer.CasterShadowPossiblyVisible then
        begin
          if ParentTransformIsIdentity then
            T :=                   Shape.State.Transformation.Transform
          else
            T := ParentTransform * Shape.State.Transformation.Transform;
          Shape.InternalShadowVolumes.RenderSilhouetteShadowVolume(
            SVRenderer.LightPosition, T,
            SVRenderer.ZFailAndLightCap,
            SVRenderer.ZFail,
            ForceOpaque);
        end;
      end;
    end;
  end;
end;

{ Frustum culling ------------------------------------------------------------ }

function TCastleScene.FrustumCulling_None(Shape: TShape): boolean;
begin
  Result := true;
end;

function TCastleScene.FrustumCulling_Sphere(Shape: TShape): boolean;
begin
  Result :=
    Shape.FrustumBoundingSphereCollisionPossibleSimple(FrustumForShapeCulling^);
end;

function TCastleScene.FrustumCulling_Box(Shape: TShape): boolean;
begin
  Result :=
    FrustumForShapeCulling^.Box3DCollisionPossibleSimple(Shape.BoundingBox);
end;

function TCastleScene.FrustumCulling_Both(Shape: TShape): boolean;
begin
  Result :=
    Shape.FrustumBoundingSphereCollisionPossibleSimple(
      FrustumForShapeCulling^) and
    FrustumForShapeCulling^.Box3DCollisionPossibleSimple(
      Shape.BoundingBox);
end;

function TCastleScene.DistanceCulling_FrustumCulling_None(Shape: TShape): boolean;
begin
  Result := DistanceCullingCheck(Shape);
end;

function TCastleScene.DistanceCulling_FrustumCulling_Sphere(Shape: TShape): boolean;
begin
  Result := DistanceCullingCheck(Shape) and
    Shape.FrustumBoundingSphereCollisionPossibleSimple(FrustumForShapeCulling^);
end;

function TCastleScene.DistanceCulling_FrustumCulling_Box(Shape: TShape): boolean;
begin
  Result := DistanceCullingCheck(Shape) and
    FrustumForShapeCulling^.Box3DCollisionPossibleSimple(Shape.BoundingBox);
end;

function TCastleScene.DistanceCulling_FrustumCulling_Both(Shape: TShape): boolean;
begin
  Result := DistanceCullingCheck(Shape) and
    Shape.FrustumBoundingSphereCollisionPossibleSimple(
      FrustumForShapeCulling^) and
    FrustumForShapeCulling^.Box3DCollisionPossibleSimple(
      Shape.BoundingBox);
end;

function TCastleScene.DistanceCullingCheck(Shape: TShape): boolean;
begin
  // This should be only called when DistanceCulling indicates this check is necessary
  Assert(DistanceCulling > 0);
  Result :=
    (PointsDistanceSqr(Shape.BoundingSphereCenter, RenderCameraPosition) <=
     Sqr(DistanceCulling + Shape.BoundingSphereRadius))
end;

procedure TCastleScene.UpdateShapeCullingCallbacks;

  function ShapeCullingToCallback(
    const FC: TFrustumCulling;
    const DoDistanceCulling, MustBeAssigned: Boolean): TTestShapeVisibility;
  begin
    if DoDistanceCulling then
      case FC of
        fcNone  : Result := @DistanceCulling_FrustumCulling_None;
        fcSphere: Result := @DistanceCulling_FrustumCulling_Sphere;
        fcBox   : Result := @DistanceCulling_FrustumCulling_Box;
        fcBoth  : Result := @DistanceCulling_FrustumCulling_Both;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('ShapeCullingToCallback:FC?');
        {$endif}
      end
    else
      case FC of
        fcNone  :
          if MustBeAssigned then
            Result := @FrustumCulling_None
          else
            Result := nil; // FrustumCulling_None always returns true
        fcSphere: Result := @FrustumCulling_Sphere;
        fcBox   : Result := @FrustumCulling_Box;
        fcBoth  : Result := @FrustumCulling_Both;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('ShapeCullingToCallback:FC?');
        {$endif}
      end;
  end;

var
  FC: TFrustumCulling;
  DoDistanceCulling: Boolean;
begin
  DoDistanceCulling := FDistanceCulling > 0;

  FC := FFrustumCulling;
  if not FShapeFrustumCulling then
    // when FShapeFrustumCulling = false, we always behave like FFrustumCulling = fcNone
    FC := fcNone;
  ShapeCullingFunc := ShapeCullingToCallback(FC, DoDistanceCulling, false);

  FC := FOctreeFrustumCulling;
  if not FShapeFrustumCulling then
    // when FShapeFrustumCulling = false, we always behave like FOctreeFrustumCulling = fcNone
    FC := fcNone;
  ShapeCullingOctreeFunc := ShapeCullingToCallback(FC, DoDistanceCulling, true);
end;

procedure TCastleScene.SetShapeFrustumCulling(const Value: Boolean);
begin
  if FShapeFrustumCulling <> Value then
  begin
    FShapeFrustumCulling := Value;
    UpdateShapeCullingCallbacks;
  end;
end;

procedure TCastleScene.SetFrustumCulling(const Value: TFrustumCulling);
begin
  if FFrustumCulling <> Value then
  begin
    FFrustumCulling := Value;
    UpdateShapeCullingCallbacks;
  end;
end;

procedure TCastleScene.SetOctreeFrustumCulling(const Value: TFrustumCulling);
begin
  if FOctreeFrustumCulling <> Value then
  begin
    FOctreeFrustumCulling := Value;
    UpdateShapeCullingCallbacks;
  end;
end;

procedure TCastleScene.SetDistanceCulling(const Value: Single);
begin
  if FDistanceCulling <> Value then
  begin
    FDistanceCulling := Value;
    UpdateShapeCullingCallbacks;
  end;
end;

{ Render --------------------------------------------------------------------- }

function TCastleScene.RenderFrustumOctree_TestShape(
  Shape: TShape): boolean;
begin
  { We know that all shapes passed here are TGLShape, so we can cast }
  Result := TGLShape(Shape).PassedShapeCulling;
end;

procedure TCastleScene.RenderWithOctree_CheckShapeCulling(
  ShapeIndex: Integer; CollidesForSure: boolean);
var
  Shape: TGLShape;
begin
  Shape := TGLShape(InternalOctreeRendering.ShapesList[ShapeIndex]);

  if not Shape.PassedShapeCulling then
  begin
    if CollidesForSure then
      // frustum culling already passed, but still check distance culling
      Shape.PassedShapeCulling := (DistanceCulling <= 0) or DistanceCullingCheck(Shape)
    else
      // this function performs frustum culling and distance culling too
      Shape.PassedShapeCulling := ShapeCullingOctreeFunc(Shape);
  end;
end;

procedure TCastleScene.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  { Update generated textures, like generated cubemaps/shadow maps. }
  procedure UpdateGeneratedTextures(
    const RenderFunc: TRenderFromViewFunction;
    const ProjectionNear, ProjectionFar: Single);
  var
    I: Integer;
    Shape: TGLShape;
    TextureNode: TAbstractTextureNode;
    Handler: TGeneratedTextureHandler;
    CamPos, CamDir, CamUp: TVector3;
  begin
    if GeneratedTextures.Count = 0 then
      Exit; // optimize away common case

    FrameProfiler.Start(fmUpdateGeneratedTextures);

    { Avoid doing this two times within the same FrameId.
      Important if
      - the same scene is present multiple times in one viewport,
      - or when Viewport.Items are shared across multiple viewports
        (thus scene is present in multiple viewports). }
    if UpdateGeneratedTexturesFrameId = TFramesPerSecond.FrameId then
      Exit;
    UpdateGeneratedTexturesFrameId := TFramesPerSecond.FrameId;

    if World.MainCamera <> nil then
    begin
      CamPos := World.MainCamera.Position;
      CamDir := World.MainCamera.Direction;
      CamUp  := World.MainCamera.Up;
    end else
    begin
      CamPos := TVector3.Zero;
      CamDir := DefaultCameraDirection;
      CamUp  := DefaultCameraUp;
    end;

    for I := 0 to GeneratedTextures.Count - 1 do
    begin
      Shape := TGLShape(GeneratedTextures.L[I].Shape);
      TextureNode := GeneratedTextures.L[I].TextureNode;
      Handler := GeneratedTextures.L[I].Handler;

      { update Handler.UpdateNeeded }
      if TextureNode is TGeneratedShadowMapNode then
      begin
        { For TGeneratedShadowMapNode, only geometry change requires to regenerate it. }
        if Handler.InternalLastStateId < World.InternalVisibleGeometryStateId then
        begin
          Handler.InternalLastStateId := World.InternalVisibleGeometryStateId;
          Handler.InternalUpdateNeeded := true;
        end;
      end else
      begin
        { For TRenderedTextureNode, TGeneratedCubeMapTextureNode etc.
          any visible change indicates to regenerate it. }
        if Handler.InternalLastStateId < World.InternalVisibleStateId then
        begin
          Handler.InternalLastStateId := World.InternalVisibleStateId;
          Handler.InternalUpdateNeeded := true;
        end;
      end;

      if TextureNode is TGeneratedCubeMapTextureNode then
        AvoidShapeRendering := Shape else
      if TextureNode is TGeneratedShadowMapNode then
        AvoidNonShadowCasterRendering := true;

      Renderer.UpdateGeneratedTextures(Shape, TextureNode,
        RenderFunc, ProjectionNear, ProjectionFar,
        ViewpointStack.Top,
        World.MainCamera <> nil, CamPos, CamDir, CamUp);

      AvoidShapeRendering := nil;
      AvoidNonShadowCasterRendering := false;
    end;

    FrameProfiler.Stop(fmUpdateGeneratedTextures);
  end;

begin
  inherited;

  { This will do FrameProfiler.Start/Stop with fmUpdateGeneratedTextures }
  if World <> nil then
    UpdateGeneratedTextures(
      World.InternalRenderEverythingEvent,
      World.InternalProjectionNear,
      World.InternalProjectionFar);
end;

procedure TCastleScene.LocalRender(const Params: TRenderParams);

{ Call LocalRenderOutside, choosing TTestShapeVisibility function
  suitable for our Params.Frustum, octrees and some settings.

  If InternalOctreeRendering is initialized (so be sure to include
  ssRendering in @link(Spatial)), this octree will be used to quickly
  find visible Shapes. Otherwise, we will just enumerate all
  Shapes (which may be slower if you really have a lot of Shapes). }

  procedure TestOctreeWithFrustum(Octree: TShapeOctree);

    procedure ResetShapeVisible(const Shape: TShape);
    begin
      TGLShape(Shape).PassedShapeCulling := false;
    end;

  begin
    Shapes.Traverse(@ResetShapeVisible, false, true);
    Octree.EnumerateCollidingOctreeItems(Params.Frustum^,
      @RenderWithOctree_CheckShapeCulling);
  end;

begin
  inherited;

  if InternalEnableRendering and
     GetVisible and
     (InternalDirty = 0) and
     (ReceiveShadowVolumes in Params.ShadowVolumesReceivers) then
  begin
    FrameProfiler.Start(fmRenderScene);

    if (not Params.Transparent) and
       (Params.InternalPass = 0) and
       (not ExcludeFromStatistics) then
      Inc(Params.Statistics.ScenesVisible);

    if FSceneFrustumCulling and not InternalIgnoreFrustum then
    begin
      if not Params.Frustum^.Box3DCollisionPossibleSimple(LocalBoundingBox) then
      begin
        FrameProfiler.Stop(fmRenderScene);
        Exit;
      end;
    end;

    if (not Params.Transparent) and
       (Params.InternalPass = 0) and
       (not ExcludeFromStatistics) then
      Inc(Params.Statistics.ScenesRendered);

    FrustumForShapeCulling := Params.Frustum;
    RenderCameraPosition := Params.InverseTransform^.MultPoint(Params.RenderingCamera.Position);

    if Assigned(InternalVisibilityTest) then
      LocalRenderOutside(InternalVisibilityTest, Params)
    else
    if InternalIgnoreFrustum then
      LocalRenderOutside(nil, Params)
    else
    if (InternalOctreeRendering <> nil) and ShapeFrustumCulling then
    begin
      { Check above ShapeFrustumCulling, since the InternalOctreeRendering
        does per-shape frustum culling automatically, even before
        ShapeCullingOctreeFunc test. Thanks to octree, many shapes
        don't even reach the stage when ShapeCullingOctreeFunc could be called. }
      TestOctreeWithFrustum(InternalOctreeRendering);
      LocalRenderOutside(@RenderFrustumOctree_TestShape, Params);
    end else
      LocalRenderOutside(ShapeCullingFunc, Params);

    FrameProfiler.Stop(fmRenderScene);
  end;
end;

{ Background-related things -------------------------------------------------- }

procedure TCastleScene.InternalInvalidateBackground;
begin
  FreeAndNil(FBackground);
  FBackgroundNode := nil;
  FBackgroundValid := false;
end;

procedure TCastleScene.SetBackgroundSkySphereRadius(const Value: Single);
begin
  if Value <> FBackgroundSkySphereRadius then
  begin
    InternalInvalidateBackground;
    FBackgroundSkySphereRadius := Value;
  end;
end;

procedure TCastleScene.PrepareBackground;
{ Always after PrepareBackground => FBackgroundValid = true }
begin
  if FBackgroundValid and (BackgroundStack.Top = FBackgroundNode) then
    Exit;

  { Background is created, but not suitable for current
    BackgroundStack.Top. So destroy it. }
  if FBackgroundValid then
    InternalInvalidateBackground;

  if BackgroundStack.Top <> nil then
    FBackground := CreateBackground(BackgroundStack.Top, BackgroundSkySphereRadius)
  else
    FBackground := nil;

  FBackgroundNode := BackgroundStack.Top;
  FBackgroundValid := true;
end;

function TCastleScene.InternalBackground: TBackground;
var
  BackgroundNode: TAbstractBackgroundNode;
begin
  PrepareBackground;
  Result := FBackground;

  { If background transform changed, we have to update the FBackground
    scene. Note that we check Result <> nil always, since not every
    TAbstractBackgroundNode may be supported. }
  BackgroundNode := BackgroundStack.Top;
  if (BackgroundNode <> nil) and (Result <> nil) then
    Result.UpdateRotation(BackgroundNode.TransformRotation);
end;

function TCastleScene.Attributes: TCastleRenderOptions;
begin
  Result := RenderOptions;
end;

function TCastleScene.GetRenderOptions: TCastleRenderOptions;
begin
  Result := Renderer.RenderOptions;
end;

procedure TCastleScene.ViewChangedSuddenly;
var
  ShapeList: TShapeList;
  Shape: TShape;
begin
  inherited;

  if ReallyOcclusionQuery(RenderOptions) then
  begin
    WritelnLog('Occlusion query', 'View changed suddenly');

    { Set OcclusionQueryAsked := false for all shapes. }
    ShapeList := Shapes.TraverseList(false, false, false);
    for Shape in ShapeList do
      TGLShape(Shape).OcclusionQueryAsked := false;
  end;
end;

procedure TCastleScene.InternalCameraChanged;
var
  I: Integer;
begin
  inherited;

  for I := 0 to GeneratedTextures.Count - 1 do
    if GeneratedTextures.L[I].TextureNode is TRenderedTextureNode then
      { Camera change causes regenerate of RenderedTexture,
        as RenderedTexture with viewpoint = NULL uses current camera.
        See demo_models/rendered_texture/rendered_texture_no_headlight.x3dv
        testcase. }
      GeneratedTextures.L[I].Handler.InternalUpdateNeeded := true;
end;

function TCastleScene.ScreenEffectsCount: Integer;
var
  I: Integer;
  SE: TScreenEffectNode;
begin
  Result := 0;

  { This ties our scene to OpenGL (by calling Renderer.PrepareScreenEffect),
    so we must be notified when OpenGL is closed.
    Testcase: otherwise the noise1 texture of the screen effect in
    "The Unholy Society" is not released from OpenGL, we get warning from
    TextureMemoryProfiler. }
  if not RegisteredGLContextCloseListener then
  begin
    RegisteredGLContextCloseListener := true;
    ApplicationProperties.OnGLContextCloseObject.Add(@GLContextCloseEvent);
  end;

  for I := 0 to ScreenEffectNodes.Count - 1 do
  begin
    SE := TScreenEffectNode(ScreenEffectNodes[I]);
    Renderer.PrepareScreenEffect(SE);
    if SE.Shader <> nil then
      Inc(Result);
  end;
end;

function TCastleScene.ScreenEffects(Index: Integer): TGLSLProgram;
var
  I: Integer;
  SE: TScreenEffectNode;
begin
  { No need for PrepareScreenEffect here, ScreenEffectsCount (that does
    PrepareScreenEffect) is always called first, otherwise the caller
    would not know that this Index is valid. }

  for I := 0 to ScreenEffectNodes.Count - 1 do
  begin
    SE := TScreenEffectNode(ScreenEffectNodes[I]);
    if SE.Shader <> nil then
      if Index = 0 then
        Exit(TGLSLProgram(SE.Shader)) else
        Dec(Index);
  end;

  raise EInternalError.Create('TCastleScene.ScreenEffects: Invalid index');
end;

function TCastleScene.ScreenEffectsNeedDepth: boolean;
var
  I: Integer;
begin
  { For now: No need for PrepareScreenEffect here, ScreenEffectsCount
    is always called first. But actually for some scenarios we should do
    here PrepareScreenEffect? }

  for I := 0 to ScreenEffectNodes.Count - 1 do
    if (TScreenEffectNode(ScreenEffectNodes[I]).Shader <> nil) and
        TScreenEffectNode(ScreenEffectNodes[I]).FdNeedsDepth.Value then
      Exit(true);
  Exit(false);
end;

procedure TCastleScene.FreeResources(Resources: TSceneFreeResources);
begin
  inherited;

  if (frBackgroundImageInNodes in Resources) and
     (FBackground <> nil) then
    FBackground.FreeResources;
end;

function TCastleScene.Clone(const AOwner: TComponent): TCastleScene;
begin
  Result := (inherited Clone(AOwner)) as TCastleScene;
end;

function TCastleScene.Batching: TBatchShapes;
begin
  if FBatching = nil then
    FBatching := TBatchShapes.Create({$ifdef CASTLE_OBJFPC}@{$endif} CreateShape);
  Result := FBatching;
end;

procedure TCastleScene.Setup2D;
begin
  RenderOptions.BlendingSort := bs2D;
end;

{ TBasicRenderParams --------------------------------------------------------- }

constructor TBasicRenderParams.Create;
begin
  inherited;
  FBaseLights := TLightInstancesList.Create;
  InShadow := false;
  ShadowVolumesReceivers := [false, true];
  { Transparent does not have good default value.
    User of TBasicRenderParams should call Render method with both Transparent values,
    to really render everything correctly.
    We just set them here to capture most 3D objects
    (as using TBasicRenderParams for anything is a discouraged hack anyway). }
  Transparent := false;
  RenderingCamera := CastleRenderingCamera.RenderingCamera;
  Frustum := @RenderingCamera.Frustum;
end;

destructor TBasicRenderParams.Destroy;
begin
  FreeAndNil(FBaseLights);
  inherited;
end;

function TBasicRenderParams.BaseLights(Scene: TCastleTransform): TLightInstancesList;
begin
  Result := FBaseLights;
end;

var
  R: TRegisteredComponent;
initialization
  GLContextCache := TGLRendererContextCache.Create;

  RegisterSerializableComponent(TCastleScene, 'Scene');

  R := TRegisteredComponent.Create;
  R.ComponentClass := TCastleScene;
  R.Caption := 'Scene (Optimal Blending for 2D Models)';
  R.OnCreate := @TCastleScene(nil).CreateComponent2D;
  RegisterSerializableComponent(R);

  RegisterSerializableComponent(TCastleBox, 'Box');
  RegisterSerializableComponent(TCastleSphere, 'Sphere');
  RegisterSerializableComponent(TCastlePlane, 'Plane');
  RegisterSerializableComponent(TCastleText, 'Text');
  RegisterSerializableComponent(TCastleCone, 'Cone');
  RegisterSerializableComponent(TCastleCylinder, 'Cylinder');
finalization
  FreeAndNil(GLContextCache);
end.
