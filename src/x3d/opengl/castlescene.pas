{
  Copyright 2003-2018 Michalis Kamburelis.

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
  CastleUtils, CastleSceneCore, CastleRenderer, CastleBackground,
  CastleGLUtils, CastleInternalShapeOctree, CastleGLShadowVolumes, X3DFields,
  CastleTriangles, CastleShapes, CastleFrustum, CastleTransform, CastleGLShaders,
  CastleRectangles, CastleCameras, CastleRendererInternalShader, CastleColors,
  CastleSceneInternalShape, CastleSceneInternalOcclusion, CastleSceneInternalBlending;

{$define read_interface}

type
  TSceneRenderingAttributes = class;
  TCastleSceneList = class;

  { Values for TSceneRenderingAttributes.WireframeEffect.

    Generally, two other attributes may affect the way wireframe is rendered:
    TSceneRenderingAttributes.WireframeColor and
    TSceneRenderingAttributes.LineWidth, quite self-explanatory. }
  TWireframeEffect = (

    { Default setting, model polygons are simply passed to OpenGL.
      Whether this results in filled or wireframe look, depends on OpenGL
      glPolygonMode setting, filled by default. }
    weNormal,

    { The model is rendered in wireframe mode.

      LineWidth is used as wireframe line width (regardless of
      TSceneRenderingAttributes.Mode).

      Depending on TSceneRenderingAttributes.Mode value:

      @unorderedList(
        @item(If <> rmFull then WireframeColor is used as wireframe
          line color.)

        @item(If rmFull, then lines are colored
          and potentially lighted and textured just like their corresponding
          triangles would be colored. So you can control lighting using
          Lighting, UseSceneLights etc. attributes, and you
          can control texturing by EnableTextures attribute.)
      ) }
    weWireframeOnly,

    { The model is rendered as normal, with it's wireframe version visible
      on top. This is most often called "solid wireframe", since the intention
      is too see wireframe version of the model but still render shapes
      solid (e.g. filled polygons with depth test).

      @link(TSceneRenderingAttributes.WireframeColor Scene.Attributes.WireframeColor) and
      @link(TRenderingAttributes.LineWidth Scene.Attributes.LineWidth) determine the color and width
      of lines.

      This is often used together with the
      @link(TRenderingAttributes.Mode Attributes.Mode)
      set to rmSolidColor. In such case,
      Then @link(TRenderingAttributes.SolidColor) determinesthe fill color. }
    weSolidWireframe,

    { The model is rendered as normal, with silhouette outlined around it.
      This works quite like weSolidWireframe, except that weSolidWireframe
      makes the wireframe mesh slightly in front the model, while weSilhouette
      makes the wireframe mesh slightly at the back of the model. This way
      only the silhouette is visible from the wireframe rendering.

      @link(TSceneRenderingAttributes.WireframeColor Scene.Attributes.WireframeColor) and
      @link(TRenderingAttributes.LineWidth Scene.Attributes.LineWidth) determine the color and width
      of silhouette lines.

      This is often used together with the
      @link(TRenderingAttributes.Mode Attributes.Mode)
      set to rmSolidColor. In such case,
      Then @link(TRenderingAttributes.SolidColor) determinesthe fill color. }
    weSilhouette);

  TBeforeShapeRenderProc = procedure (Shape: TShape) of object;

  TRenderingAttributesEvent = procedure (Attributes: TSceneRenderingAttributes) of object;

  { Rendering attributes,
    available for every scene through @link(TCastleScene.Attributes). }
  TSceneRenderingAttributes = class(TRenderingAttributes)
  private
    { Scenes that use Renderer with this TSceneRenderingAttributes instance. }
    FScenes: TCastleSceneList;

    FBlending: boolean;
    FBlendingSourceFactor: TBlendingSourceFactor;
    FBlendingDestinationFactor: TBlendingDestinationFactor;
    FBlendingSort: TBlendingSort;
    FOcclusionSort: boolean;
    FControlBlending: boolean;
    FWireframeColor: TCastleColorRGB;
    FWireframeEffect: TWireframeEffect;
    FUseOcclusionQuery: boolean;
    FUseHierarchicalOcclusionQuery: boolean;
    FDebugHierOcclusionQueryResults: boolean;
    FSolidWireframeScale: Single;
    FSolidWireframeBias: Single;
    FSilhouetteScale: Single;
    FSilhouetteBias: Single;
  protected
    procedure ReleaseCachedResources; override;

    procedure SetBlending(const Value: boolean); virtual;
    procedure SetBlendingSourceFactor(const Value: TBlendingSourceFactor); virtual;
    procedure SetBlendingDestinationFactor(const Value: TBlendingDestinationFactor); virtual;
    procedure SetBlendingSort(const Value: TBlendingSort); virtual;
    procedure SetControlBlending(const Value: boolean); virtual;
    procedure SetUseOcclusionQuery(const Value: boolean); virtual;

    procedure SetPhongShading(const Value: boolean); override;
  public
    const
      { }
      DefaultBlendingSourceFactor = bsSrcAlpha;

      { Default value of Attributes.BlendingDestinationFactor.
        See TSceneRenderingAttributes.BlendingDestinationFactor.

        Using bdOneMinusSrcAlpha is the standard value for 3D graphic stuff,
        often producing best results. However, it causes troubles when
        multiple transparent shapes are visible on the same screen pixel.
        For closed convex 3D objects, using backface culling
        (solid = TRUE for geometry) helps. For multiple transparent shapes,
        sorting the transparent shapes helps,
        see @link(TSceneRenderingAttributes.BlendingSort).
        Sometimes, no solution works for all camera angles.

        Another disadvantage of bdOneMinusSrcAlpha may be that
        the color of opaque shapes disappears too quickly from
        resulting image (since bdOneMinusSrcAlpha scales it down).
        So the image may be darker than you like.

        You can instead consider using bdOne, that doesn't require sorting
        and never has problems with multiple transparent shapes.
        On the other hand, it only adds to the color,
        often making too bright results. }
      DefaultBlendingDestinationFactor = bdOneMinusSrcAlpha;

      { Default value of @link(TSceneRenderingAttributes.BlendingSort). }
      DefaultBlendingSort = bs3D;

      DefaultWireframeColor: TCastleColorRGB = (Data: (0, 0, 0));

      DefaultSolidWireframeScale = 1;
      DefaultSolidWireframeBias = 1;
      DefaultSilhouetteScale = 5;
      DefaultSilhouetteBias = 5;

    var
      { Adjust attributes of all loaded resources. }
      OnCreate: TRenderingAttributesEvent; static;

    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    { Render partially transparent objects.

      More precisely: if this is @true, all shapes with
      transparent materials or textures with non-trivial (not only yes/no)
      alpha channel will be rendered using OpenGL blending
      (with depth test off, like they should for OpenGL).

      If this attribute is @false, everything will be rendered as opaque. }
    property Blending: boolean
      read FBlending write SetBlending default true;

    { Blending function parameters, used when @link(Blending).
      Note that this is only a default, VRML/X3D model can override this
      for specific shapes by using our extension BlendMode node.
      See [https://castle-engine.io/x3d_extensions.php#section_ext_blending].
      @groupBegin }
    property BlendingSourceFactor: TBlendingSourceFactor
      read FBlendingSourceFactor write SetBlendingSourceFactor
      default DefaultBlendingSourceFactor;
    property BlendingDestinationFactor: TBlendingDestinationFactor
      read FBlendingDestinationFactor write SetBlendingDestinationFactor
      default DefaultBlendingDestinationFactor;
    { @groupEnd }

    { How to sort the rendered objects using blending (partial transparency).
      See the @link(TBlendingSort) documentation for possible values.

      This may be overridden in a specific 3D models
      by using NavigationInfo node with blendingSort field,
      see TNavigationInfoNode.BlendingSort. }
    property BlendingSort: TBlendingSort
      read FBlendingSort write SetBlendingSort
      default DefaultBlendingSort;

    { Sort the opaque objects when rendering.
      This may generate speedup on some scenes. }
    property OcclusionSort: boolean read FOcclusionSort write FOcclusionSort;

    { Setting this to @false disables any modification of OpenGL
      blending (and depth mask) state by TCastleScene.
      This makes every other @link(Blending) setting ignored,
      and is useful only if you set your own OpenGL blending parameters
      when rendering this scene. }
    property ControlBlending: boolean
      read FControlBlending write SetControlBlending default true;

    { You can use this to turn on some effects related to rendering model
      in special modes.

      When this is weNormal (default), nothing special is
      done, which means that model polygons are simply passed to OpenGL.
      Whether this results in filled or wireframe, depends on OpenGL
      glPolygonMode setting, filled by default.

      How the wireframe effects work when Mode = rmDepth is undefined now.
      Just don't use Mode = rmDepth if you're unsure.

      See description of TWireframeEffect for what other modes do. }
    property WireframeEffect: TWireframeEffect
      read FWireframeEffect write FWireframeEffect default weNormal;

    property SolidWireframeScale: Single read FSolidWireframeScale write FSolidWireframeScale default DefaultSolidWireframeScale;
    property SolidWireframeBias: Single read FSolidWireframeBias write FSolidWireframeBias default DefaultSolidWireframeBias;
    property SilhouetteScale: Single read FSilhouetteScale write FSilhouetteScale default DefaultSilhouetteScale;
    property SilhouetteBias: Single read FSilhouetteBias write FSilhouetteBias default DefaultSilhouetteBias;

    { Wireframe color, used with some WireframeEffect values.
      Default value is DefaultWireframeColor. }
    property WireframeColor: TCastleColorRGB
      read FWireframeColor write FWireframeColor;

    { Should we use ARB_occlusion_query (if available) to avoid rendering
      shapes that didn't pass occlusion test in previous frame.
      Ignored if GPU doesn't support ARB_occlusion_query.

      @true may give you a large speedup in some scenes.
      OTOH, a lag of one frame may happen between an object should
      be rendered and it actually appears.

      When you render more than once the same instance of TCastleScene scene,
      you should not activate it (as the occlusion query doesn't make sense
      if each following render of the scene takes place at totally different
      translation). Also, when rendering something more than just
      one TCastleScene scene (maybe many times the same TCastleScene instance,
      maybe many different TCastleScene instances, maybe some other
      3D objects) you should try to sort rendering order
      from the most to the least possible occluder (otherwise occlusion
      query will not be as efficient at culling).

      This is ignored if UseHierarchicalOcclusionQuery. }
    property UseOcclusionQuery: boolean
      read FUseOcclusionQuery write SetUseOcclusionQuery default false;

    { Should we use ARB_occlusion_query (if available) with
      a hierarchical algorithm  to avoid rendering
      shapes that didn't pass occlusion test in previous frame.
      Ignored if GPU doesn't support ARB_occlusion_query.

      @true may give you a large speedup in some scenes.

      This method doesn't impose any lag of one frame (like UseOcclusionQuery).

      This requires the usage of ssRendering in TCastleSceneCore.Spatial.
      Also, it always does frustum culling (like fcBox for now),
      regardless of TCastleScene.OctreeFrustumCulling setting.

      The algorithm used underneath is "Coherent Hierarchical Culling",
      described in detail in "GPU Gems 2",
      Chapter 6: "Hardware Occlusion Queries Made Useful",
      by Michael Wimmer and Jiri Bittner. Online on
      [http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter06.html].

      @exclude
      @bold(Experimental):
      Using the "Hierarchical Occlusion Query" is not adviced in the current implementation,
      it is slow and it does not treat transparent shapes correctly.
    }
    property UseHierarchicalOcclusionQuery: boolean
      read FUseHierarchicalOcclusionQuery
      write FUseHierarchicalOcclusionQuery default false;
      experimental;

    { View only the shapes that were detected as visible by occlusion query
      in last Render.

      Use this only after render with UseHierarchicalOcclusionQuery.
      TODO: for UseOcclusionQuery I would also like to make it work,
      for now not done as frustum information is gone.
      This will disable actual occlusion query,
      instead reusing results from last occlusion
      query done when this debug flag was @false.

      Useful to quickly visualize the benefits of occlusion query. }
    property DebugHierOcclusionQueryResults: boolean
      read FDebugHierOcclusionQueryResults
      write FDebugHierOcclusionQueryResults default false;

    { Checks UseOcclusionQuery, existence of GL_ARB_occlusion_query,
      and GLQueryCounterBits > 0. If @false, ARB_occlusion_query just cannot
      be used.

      Also, returns @false when UseHierarchicalOcclusionQuery is @true
      --- because then UseHierarchicalOcclusionQuery should take precedence.

      @exclude Internal. }
    function ReallyUseOcclusionQuery: boolean;

    { Checks UseHierarchicalOcclusionQuery, existence of GL_ARB_occlusion_query,
      and GLQueryCounterBits > 0. If @false, ARB_occlusion_query just cannot
      be used.

      @exclude Internal. }
    function ReallyUseHierarchicalOcclusionQuery: boolean;
  end;

type
  TPrepareResourcesOption = CastleTransform.TPrepareResourcesOption;
  TPrepareResourcesOptions = CastleTransform.TPrepareResourcesOptions;

const
  prRender = CastleTransform.prRender;
  prBackground = CastleTransform.prBackground;
  prBoundingBox = CastleTransform.prBoundingBox;
  prShadowVolume = CastleTransform.prShadowVolume;

type
  { Possible checks done while frustum culling.

    This is used for TCastleScene.FrustumCulling (what checks
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
  private
    Renderer: TGLRenderer;
    FReceiveShadowVolumes: boolean;
    RegisteredGLContextCloseListener: boolean;
    FTempPrepareParams: TPrepareParams;
    RenderCameraKnown: boolean;
    { Camera position, in local scene coordinates, known (if RenderCameraKnown)
      during the Render call. }
    RenderCameraPosition: TVector3;

    { used by LocalRenderInside }
    FilteredShapes: TShapeList;

    { Render everything using Renderer.

      Calls Renderer.RenderBegin.
      Then on all potentially visible Shapes[] calls RenderShape.
      "Potentially visible" is decided by TestShapeVisibility
      (shape is visible if TestShapeVisibility is @nil or returns
      @true for this shape) and Params.Transparent value must include
      given shape. At the end calls Renderer.RenderEnd.

      Additionally this implements blending, looking at Attributes.Blending*,
      setting appropriate OpenGL state and rendering partially transparent
      shape before all opaque objects.

      Updates Params.Statistics. }
    procedure LocalRenderInside(const TestShapeVisibility: TTestShapeVisibility;
      const Params: TRenderParams);

    { Render everything using LocalRenderInside.
      The rendering parameters are configurable
      by @link(Attributes), see TSceneRenderingAttributes and
      TRenderingAttributes.

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

        @item(Note that when Attributes.Blending is @false then everything
          is always opaque, so tgOpaque renders everything and tgTransparent
          renders nothing.)
      )

      @param(TestShapeVisibility Filters which shapes are visible.

        You can use this to optimize rendering. For example
        you can reject shapes because their bounding volume
        (bounding boxes or bounding spheres) doesn't intersect with frustum
        or such. This is called frustum culling, and in fact is done
        automatically by other overloaded Render methods in this class,
        see FrustumCulling and OctreeFrustumCulling.

        TestShapeVisibility callback may be used to implement frustum
        culling, or some other visibility algorithm.) }
    procedure LocalRenderOutside(
      const TestShapeVisibility: TTestShapeVisibility;
      const Params: TRenderParams);

    { Destroy any associations of Renderer with OpenGL context.

      This also destroys associations with OpenGL context in this class
      @italic(that were made using Renderer). This doesn't destroy other
      associations, like Background.

      This is useful to call when we change something in Attributes,
      since changing most Attributes (besides color modulators ?)
      requires that we disconnect Renderer from OpenGL context.
      Other things, like Background, don't have to be destroyed in this case. }
    procedure CloseGLRenderer;
    procedure GLContextCloseEvent(Sender: TObject);
  private
    { Fog for this shape. @nil if none. }
    function ShapeFog(const Shape: TShape; const GlobalFog: TAbstractFogNode): IAbstractFogObject;
    function EffectiveBlendingSort: TBlendingSort;
  private
    type
      TCustomShaders = record
        Shader: TX3DShaderProgramBase;
        ShaderAlphaTest: TX3DShaderProgramBase;
        procedure Initialize(const VertexCode, FragmentCode: string);
        procedure Free;
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

      PreparedShapesResources, PreparedRender: boolean;
      VarianceShadowMapsProgram, ShadowMapsProgram: TCustomShaders;
      FDistanceCulling: Single;

    { Private things for RenderFrustum --------------------------------------- }

    function FrustumCulling_None(Shape: TShape): boolean;
    function FrustumCulling_Sphere(Shape: TShape): boolean;
    function FrustumCulling_Box(Shape: TShape): boolean;
    function FrustumCulling_Both(Shape: TShape): boolean;

    function DistanceCullingCheck(Shape: TShape): boolean;
  private
          FFrustumCulling: TFrustumCulling;
    FOctreeFrustumCulling: TFrustumCulling;
    procedure       SetFrustumCulling(const Value: TFrustumCulling);
    procedure SetOctreeFrustumCulling(const Value: TFrustumCulling);
  private
          FrustumCullingFunc: TTestShapeVisibility;
    OctreeFrustumCullingFunc: TTestShapeVisibility;

    RenderFrustum_Frustum: PFrustum;

    function RenderFrustumOctree_TestShape(Shape: TShape): boolean;
    procedure RenderFrustumOctree_EnumerateShapes(
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
  private
    OcclusionQueryUtilsRenderer: TOcclusionQueryUtilsRenderer;
    SimpleOcclusionQueryRenderer: TSimpleOcclusionQueryRenderer;
    HierarchicalOcclusionQueryRenderer: THierarchicalOcclusionQueryRenderer;
    BlendingRenderer: TBlendingRenderer;
  protected
    function CreateShape(AGeometry: TAbstractGeometryNode;
      AState: TX3DGraphTraverseState; ParentInfo: PTraversingInfo): TShape; override;
    procedure InvalidateBackground; override;

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

      We look at some Attributes, like Attributes.Blending, because transparent
      triangles have to be handled a little differently, and when
      Attributes.Blending = false then all triangles are forced to be opaque.
      In other words, this takes Attributes into account, to cooperate with
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
  private
    FBackgroundSkySphereRadius: Single;
    { Node for which FBackground is currently prepared. }
    FBackgroundNode: TAbstractBindableNode;
    { Cached Background value }
    FBackground: TBackground;
    { Is FBackground valid ? We can't use "nil" FBackground value to flag this
      (bacause nil is valid value for Background function).
      If not FBackgroundValid then FBackground must always be nil.
      Never set FBackgroundValid to false directly - use InvalidateBackground,
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
      is the top node on the BackgroundStack of this scene, following VRML/X3D
      specifications, and can be dynamic.
      The scene manager should use this to render background.

      You should not access the background this way in your own code.
      This is public only because our own TCastleSceneManager needs to access it.
      And this is not marked "internal" because you may want to implement custom
      TCastleScene descendants that override this.

      @bold(If you simply use TCastleScene to render 3D stuff,
      and want to change background properties,
      then do not use this method.) To change the background,
      find (or add) an X3D @code(Background) node (@link(TBackgroundNode) instance)
      in the X3D graph in @link(RootNode), and change it's properties
      or "bind" to change current backround.

      We use the current value of BackgroundSkySphereRadius.

      Returns @nil if there is no currently bound background node
      in this scene, or if the bound background is not supported for now
      (the latter case right now happens with TextureBakckground).

      This instance is managed (automatically created/freed
      and so on) by this TCastleScene instance. It is cached
      (so that it's recreated only when relevant things change,
      like VRML/X3D nodes affecting this background,
      or changes to BackgroundSkySphereRadius, or OpenGL context is closed). }
    function Background: TBackground;

    { Rendering attributes.

      You are free to change them all at any time.
      Although note that changing some attributes (the ones defined
      in base TRenderingAttributes class) may be a costly operation
      (next PrepareResources with prRender, or Render call, may need
      to recalculate some things). }
    function Attributes: TSceneRenderingAttributes;

    procedure UpdateGeneratedTextures(
      const RenderFunc: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      const OriginalViewport: TRectangle); override;

    procedure ViewChangedSuddenly; override;

    procedure VisibleChangeNotification(const Changes: TVisibleChanges); override;
    procedure CameraChanged(ACamera: TCamera); override;

    { Screen effects information, used by TCastleAbstractViewport.ScreenEffects.
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
      in @link(Attributes). }
    function Clone(const AOwner: TComponent): TCastleScene;
  published
    { Fine-tune performance of rendering when
      ssRendering is @italic(not) in @link(TCastleSceneCore.Spatial).

      @link(Render) tests each Shape for collision with given Frustum
      before rendering this Shape. It can use Shape.BoundingBox
      or Shape.BoundingSphere or both.
      See TFrustumCulling.

      Shape.BoundingBox is (in a current implementation) always
      a better approximation of shape geometry than Shape.BoundingSphere.
      So advantage of using Shape.BoundingBox is that more Shapes
      may be eliminated. Advantage of using Shape.BoundingSphere
      is that checking for collision Frustum<->Sphere is faster,
      so you don't waste so much time on testing for collisions between
      frustum and Shape. }
    property FrustumCulling: TFrustumCulling
      read FFrustumCulling write SetFrustumCulling default fcBox;

    { Fine-tune performance of rendering when
      ssRendering is included in @link(TCastleSceneCore.Spatial).

      See TFrustumCulling. }
    property OctreeFrustumCulling: TFrustumCulling
      read FOctreeFrustumCulling write SetOctreeFrustumCulling default fcBox;

    property ReceiveShadowVolumes: boolean
      read FReceiveShadowVolumes write FReceiveShadowVolumes default true;

    { Cull things farther than this distance. Ignored if <= 0. }
    property DistanceCulling: Single
      read FDistanceCulling write FDistanceCulling default 0;
  end;

  TCastleSceneList = class(specialize TObjectList<TCastleScene>)
  private
    { Just call InvalidateBackground or CloseGLRenderer on all items.
      These methods are private, because corresponding methods in
      TCastleScene are also private and we don't want to expose
      them here. }
    procedure InvalidateBackground;
    procedure CloseGLRenderer;
  public
    { Just call GLContextClose on all items. }
    procedure GLContextClose;

    { Just call ViewChangedSuddenly on all items. }
    procedure ViewChangedSuddenly;
  end;

  TTriangle4List = specialize TStructList<TTriangle4>;

  { @exclude Internal.

    Basic non-abstact implementation of render params for calling
    TCastleTransform.LocalRender.

    @bold(This is exposed here only to support some experiments with non-standard
    rendering in engine example programs. Do not use this in your own code.)

    This can be used when you have to call TCastleTransform.LocalRender,
    but you don't use scene manager.
    Usually this should not be needed.
    This class may be removed at some point!
    You should always try to use TCastleSceneManager to manage and render
    3D stuff in new programs, and then TCastleSceneManager will take care of creating
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

const
  bsNone = CastleBoxes.bsNone;
  bs2D = CastleBoxes.bs2D;
  bs3D = CastleBoxes.bs3D;

  paDefault = CastleSceneCore.paDefault;
  paForceLooping = CastleSceneCore.paForceLooping;
  paForceNotLooping = CastleSceneCore.paForceNotLooping;
  paLooping = CastleSceneCore.paLooping;
  paNotLooping = CastleSceneCore.paNotLooping;

  ssRendering = CastleSceneCore.ssRendering;
  ssDynamicCollisions = CastleSceneCore.ssDynamicCollisions;
  ssVisibleTriangles = CastleSceneCore.ssVisibleTriangles;
  ssStaticCollisions = CastleSceneCore.ssStaticCollisions;

implementation

{$warnings off}
// TODO: This unit temporarily uses RenderingCamera singleton,
// to keep TBasicRenderParams working for backward compatibility.
uses CastleGLVersion, CastleImages, CastleLog,
  CastleStringUtils, CastleApplicationProperties,
  CastleRenderingCamera, CastleShapeInternalRenderShadowVolumes;
{$warnings on}

var
  TemporaryAttributeChange: Cardinal = 0;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleScene]);
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

{ TCastleScene ------------------------------------------------------------ }

constructor TCastleScene.Create(AOwner: TComponent);
begin
  { inherited Create *may* call some virtual things overriden here
    (although right now it doesn't): it may bind new viewpoint which
    may call ViewChangedSuddenly which is overridden here and uses Attributes.
    That's why I have to initialize them *before* "inherited Create" }

  Renderer := TGLRenderer.Create(TSceneRenderingAttributes, GLContextCache);
  Assert(Renderer.Attributes is TSceneRenderingAttributes);

  { Note that this calls Renderer.Attributes, so use this after
    initializing Renderer. }
  Attributes.FScenes.Add(Self);

  inherited Create(AOwner);

  FBackgroundSkySphereRadius := 1.0;
  FBackgroundValid := false;
  FBackgroundNode := nil;
  FBackground := nil;

  FFrustumCulling := fcBoth;
   FrustumCulling := fcBox; { set through property setter }

  FOctreeFrustumCulling := fcBoth;
   OctreeFrustumCulling := fcBox; { set through property setter }

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

  if RegisteredGLContextCloseListener and
     (ApplicationProperties <> nil) then
  begin
    ApplicationProperties.OnGLContextCloseObject.Remove(@GLContextCloseEvent);
    RegisteredGLContextCloseListener := false;
  end;

  GLContextClose;

  { Note that this calls Renderer.Attributes, so use this before
    deinitializing Renderer. }
  if Renderer <> nil then
    Attributes.FScenes.Remove(Self);

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

function TCastleScene.CreateShape(AGeometry: TAbstractGeometryNode;
  AState: TX3DGraphTraverseState; ParentInfo: PTraversingInfo): TShape;
begin
  Result := TGLSceneShape.Create(Self, AGeometry, AState, ParentInfo);
end;

procedure TCastleScene.CloseGLRenderer;
{ This must be coded carefully, because
  - it's called by ChangedAll, and so may be called when our constructor
    didn't do it's work yet.
  - moreover it's called from destructor, so may be called if our
    constructor terminated with exception.
  This explains that we have to check Renderer <> nil, Shapes <> nil. }

  procedure CloseGLScreenEffect(Node: TScreenEffectNode);
  begin
    { The TGLSLProgram instance here will be released by Rendered.UnprepareAll,
      that eventually calls GLSLRenderers.UnprepareAll,
      that eventually calls Cache.GLSLProgram_DecReference on this shader,
      that eventuallly destroys TGLSLProgram instance.
      So below only set it to nil. }
    Node.Shader := nil;
    Node.ShaderLoaded := false;
  end;

var
  SI: TShapeTreeIterator;
  S: TGLShape;
  I: Integer;
  Pass: TRenderingPass;
begin
  PreparedRender := false;
  PreparedShapesResources := false;

  { Free Arrays and Vbo of all shapes. }
  if (Renderer <> nil) and (Shapes <> nil) then
  begin
    { Iterate even over non-visible shapes, for safety:
      since this CloseGLRenderer may happen after some
      "visibility" changed, that is you changed proxy
      or such by event. }
    SI := TShapeTreeIterator.Create(Shapes, false, false);
    try
      while SI.GetNext do
      begin
        S := TGLShape(SI.Current);
        if S.Cache <> nil then
          Renderer.Cache.Shape_DecReference(S.Cache);
        for Pass := Low(Pass) to High(Pass) do
          if S.ProgramCache[Pass] <> nil then
            Renderer.Cache.Program_DecReference(S.ProgramCache[Pass]);
      end;
    finally FreeAndNil(SI) end;
  end;

  if ScreenEffectNodes <> nil then
    for I := 0 to ScreenEffectNodes.Count - 1 do
      CloseGLScreenEffect(TScreenEffectNode(ScreenEffectNodes[I]));

  if Renderer <> nil then
    Renderer.UnprepareAll;

  if Shapes <> nil then
  begin
    SI := TShapeTreeIterator.Create(Shapes, false, true);
    try
      while SI.GetNext do
        TGLShape(SI.Current).GLContextClose;
    finally FreeAndNil(SI) end;
  end;

  VarianceShadowMapsProgram.Free;
  ShadowMapsProgram.Free;
end;

procedure TCastleScene.GLContextClose;
begin
  inherited;
  CloseGLRenderer;
  InvalidateBackground;
  if OcclusionQueryUtilsRenderer <> nil then
    OcclusionQueryUtilsRenderer.GLContextClose;
end;

procedure TCastleScene.GLContextCloseEvent(Sender: TObject);
begin
  GLContextClose;
end;

function TCastleScene.ShapeFog(const Shape: TShape; const GlobalFog: TAbstractFogNode): IAbstractFogObject;
begin
  Result := Shape.State.LocalFog;
  if Result = nil then
    Result := FogStack.Top;
  if Result = nil then
    Result := GlobalFog as TFogNode;
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
    Result := Attributes.BlendingSort;
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

  { Renders Shape, by calling Renderer.RenderShape. }
  procedure RenderShape_NoTests(Shape: TGLShape);
  begin
    { implement Shape node "render" field here, by a trivial check }
    if (Shape.Node <> nil) and (not Shape.Node.Render) then Exit;

    OcclusionQueryUtilsRenderer.OcclusionBoxStateEnd;

    if (Params.Pass = 0) and not ExcludeFromStatistics then
      Inc(Params.Statistics.ShapesRendered);

    { Optionally free Shape arrays data now, if they need to be regenerated. }
    if (Assigned(Attributes.OnVertexColor) or
        Assigned(Attributes.OnRadianceTransfer)) and
       (Shape.Cache <> nil) then
      Shape.Cache.FreeArrays([vtAttribute]);

    Shape.ModelView := ModelView;
    Renderer.RenderShape(Shape, ShapeFog(Shape, Params.GlobalFog));
    IsVisibleNow := true;
  end;

  { Call RenderShape if some tests succeed.
    It assumes that test with TestShapeVisibility is already done. }
  procedure RenderShape_SomeTests(Shape: TGLShape);
  begin
    if (Shape <> AvoidShapeRendering) and
       ( (not AvoidNonShadowCasterRendering) or Shape.ShadowCaster) then
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

      if Attributes.ReallyUseOcclusionQuery and
         (Params.RenderingCamera.Target = rtScreen) then
      begin
        SimpleOcclusionQueryRenderer.Render(Shape, @RenderShape_NoTests, Params);
      end else
      {$warnings off}
      if Attributes.DebugHierOcclusionQueryResults and
         Attributes.UseHierarchicalOcclusionQuery then
      {$warnings on}
      begin
        if HierarchicalOcclusionQueryRenderer.WasLastVisible(Shape) then
          RenderShape_NoTests(Shape);
      end else
        { No occlusion query-related stuff. Just render the shape. }
        RenderShape_NoTests(Shape);
    end;
  end;

  { Call RenderShape if many tests, including TestShapeVisibility,
    succeed. }
  procedure RenderShape_AllTests(Shape: TShape);
  begin
    if ( (not Assigned(TestShapeVisibility)) or
         TestShapeVisibility(TGLShape(Shape))) then
      RenderShape_SomeTests(TGLShape(Shape));
  end;

  procedure RenderShape_AllTests_Opaque(Shape: TShape);
  begin
    if not TGLShape(Shape).UseBlending then
      RenderShape_AllTests(Shape);
  end;

  procedure RenderShape_AllTests_Blending(Shape: TShape);
  begin
    if TGLShape(Shape).UseBlending then
    begin
      BlendingRenderer.BeforeRenderShape(Shape);
      RenderShape_AllTests(Shape);
    end;
  end;

  procedure RenderAllAsOpaque(const IgnoreShapesWithBlending: boolean = false);
  begin
    if not Params.Transparent then
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

var
  I: Integer;
  LightRenderEvent: TLightRenderEvent;
begin
  { We update ShapesVisible only for one value of Params.Transparent.
    Otherwise, we would increase it twice.
    This method is always called first with Params.Transparent = false,
    then Params.Transparent = true during a single frame. }
  if (not Params.Transparent) and (Params.Pass = 0) then
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
  OcclusionQueryUtilsRenderer.ModelViewProjectionMatrix :=
    RenderContext.ProjectionMatrix * ModelView;
  OcclusionQueryUtilsRenderer.ModelViewProjectionMatrixChanged := true;

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    glPushMatrix;
    glLoadMatrix(ModelView);
  end;
  {$endif}

  Renderer.RenderBegin(Params.BaseLights(Self) as TLightInstancesList,
    LightRenderEvent, Params.Pass);
  try
    if Attributes.Mode <> rmFull then
    begin
      { When not rmFull, we don't want to do anything with glDepthMask
        or GL_BLEND enable state. Just render everything
        (except: don't render partially transparent stuff for shadow maps). }
      RenderAllAsOpaque(Attributes.Mode = rmDepth);

      { Each RenderShape_SomeTests inside could set OcclusionBoxState }
      OcclusionQueryUtilsRenderer.OcclusionBoxStateEnd;
    end else
    if Attributes.ReallyUseHierarchicalOcclusionQuery and
       (not Attributes.DebugHierOcclusionQueryResults) and
       (Params.RenderingCamera.Target = rtScreen) and
       (InternalOctreeRendering <> nil) then
    begin
      HierarchicalOcclusionQueryRenderer.Render(@RenderShape_SomeTests,
        Params, RenderCameraKnown, RenderCameraPosition);

      { Inside we could set OcclusionBoxState }
      OcclusionQueryUtilsRenderer.OcclusionBoxStateEnd;
    end else
    begin
      if Attributes.Blending then
      begin
        if not Params.Transparent then
        begin
          { draw fully opaque objects }
          if RenderCameraKnown and
            (Attributes.ReallyUseOcclusionQuery or Attributes.OcclusionSort) then
          begin
            ShapesFilterBlending(Shapes, true, true, false,
              TestShapeVisibility, FilteredShapes, false);

            { ShapesSplitBlending already filtered shapes through
              TestShapeVisibility callback, so later we can render them
              with RenderShape_SomeTests to skip checking TestShapeVisibility
              twice. This is a good thing: it means that sorting below has
              much less shapes to consider. }
            FilteredShapes.SortFrontToBack(RenderCameraPosition);

            for I := 0 to FilteredShapes.Count - 1 do
              RenderShape_SomeTests(TGLShape(FilteredShapes[I]));
          end else
            Shapes.Traverse(@RenderShape_AllTests_Opaque, true, true, false);
        end else
        { this means Params.Transparent = true }
        begin
          { draw partially transparent objects }
          glDepthMask(GL_FALSE);
          glEnable(GL_BLEND);

          BlendingRenderer.RenderBegin;

          { sort for blending, if BlendingSort not bsNone.
            Note that bs2D does not require knowledge of the camera,
            CameraPosition is unused in this case by FilteredShapes.SortBackToFront }
          if ((EffectiveBlendingSort = bs3D) and RenderCameraKnown) or
              (EffectiveBlendingSort = bs2D) then
          begin
            ShapesFilterBlending(Shapes, true, true, false,
              TestShapeVisibility, FilteredShapes, true);
            FilteredShapes.SortBackToFront(RenderCameraPosition, EffectiveBlendingSort = bs3D);
            for I := 0 to FilteredShapes.Count - 1 do
            begin
              BlendingRenderer.BeforeRenderShape(FilteredShapes[I]);
              RenderShape_SomeTests(TGLShape(FilteredShapes[I]));
            end;
          end else
            Shapes.Traverse(@RenderShape_AllTests_Blending, true, true, false);

          { restore glDepthMask and blending state to default values }
          glDepthMask(GL_TRUE);
          glDisable(GL_BLEND);
        end;
      end else
        RenderAllAsOpaque;

      { Each RenderShape_SomeTests inside could set OcclusionBoxState.

        TODO: in case of fixed-function path,
        glPopAttrib inside could restore now
        glDepthMask(GL_TRUE) and glDisable(GL_BLEND).
        This problem will disappear when we'll get rid of fixed-function
        possibility in OcclusionBoxStateEnd. }
      OcclusionQueryUtilsRenderer.OcclusionBoxStateEnd;
    end;
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
    SI: TShapeTreeIterator;
  begin
    SI := TShapeTreeIterator.Create(Shapes, false, false);
    try
      while SI.GetNext do
        TGLShape(SI.Current).PrepareResources;
    finally FreeAndNil(SI) end;
  end;

  procedure PrepareRenderShapes;
  var
    SI: TShapeTreeIterator;
    Shape: TGLShape;
    BaseLights: TLightInstancesList;
    GoodParams, OwnParams: TPrepareParams;
  begin
    if Log and LogRenderer then
      WritelnLog('Renderer', 'Preparing rendering of all shapes');

    { Note: we prepare also not visible shapes, in case they become visible. }
    SI := TShapeTreeIterator.Create(Shapes, false, false);
    try
      Inc(Renderer.PrepareRenderShape);
      try
        { calculate OwnParams, GoodParams }
        if Params = nil then
        begin
          WritelnWarning('PrepareResources', 'Do not pass Params=nil to TCastleScene.PrepareResources or T3DResource.Prepare or friends. Get the params from SceneManager.PrepareParams (create a temporary TCastleSceneManager if you need to).');
          OwnParams := TPrepareParams.Create;
          GoodParams := OwnParams;
        end else
        begin
          OwnParams := nil;
          GoodParams := Params;
        end;

        { prepare resources by doing rendering (but with
          Renderer.PrepareRenderShape <> 0, so nothing will be actually drawn). }
        BaseLights := GoodParams.InternalBaseLights as TLightInstancesList;
        Renderer.RenderBegin(BaseLights, nil, 0);
        while SI.GetNext do
        begin
          Shape := TGLShape(SI.Current);
          Renderer.RenderShape(Shape, ShapeFog(Shape, GoodParams.InternalGlobalFog));
        end;
        Renderer.RenderEnd;

        FreeAndNil(OwnParams);
      finally Dec(Renderer.PrepareRenderShape) end;
    finally FreeAndNil(SI) end;
  end;

var
  I: Integer;
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
    if not PreparedShapesResources then
    begin
      { Use PreparedShapesResources to avoid expensive (for large scenes)
        iteration over all shapes in every TCastleScene.PrepareResources call. }
      PreparedShapesResources := true;
      PrepareShapesResources;
    end;

    if (prRender in Options) and not PreparedRender then
    begin
      { We use PreparedRender to avoid potentially expensive iteration
        over shapes and expensive Renderer.RenderBegin/End. }
      PreparedRender := true;

      { Do not prepare when OnVertexColor or OnRadianceTransfer used,
        as we can only call these callbacks during render (otherwise they
        may be unprepared, like no texture for dynamic_ambient_occlusion.lpr). }
      if not
        (Assigned(Attributes.OnVertexColor) or
         Assigned(Attributes.OnRadianceTransfer)) then
        PrepareRenderShapes;
    end;

    if prBackground in Options then
      PrepareBackground;

    if prScreenEffects in Options then
    begin
      for I := 0 to ScreenEffectNodes.Count - 1 do
        Renderer.PrepareScreenEffect(ScreenEffectNodes[I] as TScreenEffectNode);
    end;
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
        SavedMode := Attributes.Mode;
        SavedSolidColor := Attributes.SolidColor;
        Attributes.Mode := rmSolidColor;
        Attributes.SolidColor := Attributes.WireframeColor;
      end;

      RenderNormal;

      if UseWireframeColor then
      begin
        Attributes.Mode := SavedMode;
        Attributes.SolidColor := SavedSolidColor;
      end;
    glPopAttrib;
  end;
  {$warnings on}
  {$endif}

  { Render taking Attributes.WireframeEffect into account. }
  procedure RenderWithWireframeEffect;
  // TODO-es For OpenGLES, wireframe must be done differently
  {$ifndef OpenGLES}
  { This code uses a lot of deprecated stuff. It is already marked with TODO above. }
  {$warnings off}
  begin
    case Attributes.WireframeEffect of
      weNormal: RenderNormal;
      weWireframeOnly: RenderWireframe(Attributes.Mode = rmSolidColor);
      weSolidWireframe:
        begin
          glPushAttrib(GL_POLYGON_BIT);
            { enable polygon offset for everything (whole scene) }
            glEnable(GL_POLYGON_OFFSET_FILL); { saved by GL_POLYGON_BIT }
            glEnable(GL_POLYGON_OFFSET_LINE); { saved by GL_POLYGON_BIT }
            glEnable(GL_POLYGON_OFFSET_POINT); { saved by GL_POLYGON_BIT }
            glPolygonOffset(Attributes.SolidWireframeScale, Attributes.SolidWireframeBias); { saved by GL_POLYGON_BIT }
            RenderNormal;
          glPopAttrib;
          RenderWireframe(true);
        end;
      weSilhouette:
        begin
          RenderNormal;
          glPushAttrib(GL_POLYGON_BIT);
            glEnable(GL_POLYGON_OFFSET_LINE); { saved by GL_POLYGON_BIT }
            glPolygonOffset(Attributes.SilhouetteScale, Attributes.SilhouetteBias); { saved by GL_POLYGON_BIT }

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
            if Attributes.Mode = rmSolidColor then
              glFrontFace(GL_CW); { saved by GL_POLYGON_BIT }
            *)

            RenderWireframe(true);
          glPopAttrib;
        end;
      else raise EInternalError.Create('Render: Attributes.WireframeEffect ?');
    end;
  {$warnings on}
  {$else}
  begin
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
      SavedMode := Attributes.Mode;
      Attributes.Mode := rmDepth;

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

      SavedShaders.Shader          := Attributes.CustomShader;
      SavedShaders.ShaderAlphaTest := Attributes.CustomShaderAlphaTest;
      Attributes.CustomShader          := NewShaders.Shader;
      Attributes.CustomShaderAlphaTest := NewShaders.ShaderAlphaTest;
    end;

    RenderWithWireframeEffect;

    if Params.RenderingCamera.Target in [rtVarianceShadowMap, rtShadowMap] then
    begin
      Attributes.Mode := SavedMode;
      Attributes.CustomShader          := SavedShaders.Shader;
      Attributes.CustomShaderAlphaTest := SavedShaders.ShaderAlphaTest;
    end;
  end;

begin
  { This is usually called by LocalRender(Params) that probably
    already did tests below. But it may also be called directly,
    so do the checks below anyway. (The checks are trivial, so no speed harm.) }
  if GetVisible and
     (InternalDirty = 0) and
     (ReceiveShadowVolumes = Params.ShadowVolumesReceivers) then
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

      It's much simpler to just call PrepareResources at the beginning. }
    FTempPrepareParams.InternalBaseLights := Params.BaseLights(Self);
    FTempPrepareParams.InternalGlobalFog := Params.GlobalFog;
    PrepareResources([prRender], false, FTempPrepareParams);

    RenderWithShadowMaps;
  end;
end;

class procedure TCastleScene.LightRenderInShadow(const Light: TLightInstance;
  var LightOn: boolean);
begin
  if Light.Node.FdShadowVolumes.Value then
    LightOn := false;
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
  SI: TShapeTreeIterator;
  T: TMatrix4;
  ForceOpaque: boolean;
begin
  if GetVisible and CastShadowVolumes then
  begin
    SVRenderer := ShadowVolumeRenderer as TGLShadowVolumeRenderer;

    ForceOpaque := not (Attributes.Blending and (Attributes.Mode = rmFull));

    { calculate and check SceneBox }
    SceneBox := LocalBoundingBox;
    if not ParentTransformIsIdentity then
      SceneBox := SceneBox.Transform(ParentTransform);
    SVRenderer.InitCaster(SceneBox);
    if SVRenderer.CasterShadowPossiblyVisible then
    begin
      { shadows are cast only by visible scene parts
        (not e.g. invisible collision box of castle-anim-frames) }
      SI := TShapeTreeIterator.Create(Shapes, { OnlyActive } true, { OnlyVisible } true);
      try
        while SI.GetNext do
        begin
          ShapeBox := SI.Current.BoundingBox;
          if not ParentTransformIsIdentity then
            ShapeBox := ShapeBox.Transform(ParentTransform);
          SVRenderer.InitCaster(ShapeBox);
          if SVRenderer.CasterShadowPossiblyVisible then
          begin
            if ParentTransformIsIdentity then
              T :=                   SI.Current.State.Transform else
              T := ParentTransform * SI.Current.State.Transform;
            SI.Current.InternalShadowVolumes.RenderSilhouetteShadowVolume(
              SVRenderer.LightPosition, T,
              SVRenderer.ZFailAndLightCap,
              SVRenderer.ZFail,
              ForceOpaque);
          end;
        end;
      finally FreeAndNil(SI) end;
    end;
  end;
end;

{ Frustum culling ------------------------------------------------------------ }

function TCastleScene.FrustumCulling_None(Shape: TShape): boolean;
begin
  Result := DistanceCullingCheck(Shape);
end;

function TCastleScene.FrustumCulling_Sphere(Shape: TShape): boolean;
begin
  Result := DistanceCullingCheck(Shape) and
    Shape.FrustumBoundingSphereCollisionPossibleSimple(RenderFrustum_Frustum^);
end;

function TCastleScene.FrustumCulling_Box(Shape: TShape): boolean;
begin
  Result := DistanceCullingCheck(Shape) and
    RenderFrustum_Frustum^.Box3DCollisionPossibleSimple(Shape.BoundingBox);
end;

function TCastleScene.FrustumCulling_Both(Shape: TShape): boolean;
begin
  Result := DistanceCullingCheck(Shape) and
    Shape.FrustumBoundingSphereCollisionPossibleSimple(
      RenderFrustum_Frustum^) and
    RenderFrustum_Frustum^.Box3DCollisionPossibleSimple(
      Shape.BoundingBox);
end;

function TCastleScene.DistanceCullingCheck(Shape: TShape): boolean;
begin
  Result :=
    (DistanceCulling <= 0) or
    (not RenderCameraKnown) or
    (PointsDistanceSqr(Shape.BoundingSphereCenter, RenderCameraPosition) <=
     Sqr(DistanceCulling + Shape.BoundingSphereRadius))
end;

procedure TCastleScene.SetFrustumCulling(const Value: TFrustumCulling);
begin
  if Value <> FFrustumCulling then
  begin
    FFrustumCulling := Value;
    case Value of
      { FrustumCullingFunc may be @nil (unlike OctreeFrustumCullingFunc). }
      fcNone  : FrustumCullingFunc := nil;
      fcSphere: FrustumCullingFunc := @FrustumCulling_Sphere;
      fcBox   : FrustumCullingFunc := @FrustumCulling_Box;
      fcBoth  : FrustumCullingFunc := @FrustumCulling_Both;
      else raise EInternalError.Create('SetFrustumCulling?');
    end;
  end;
end;

procedure TCastleScene.SetOctreeFrustumCulling(const Value: TFrustumCulling);
begin
  if Value <> FOctreeFrustumCulling then
  begin
    FOctreeFrustumCulling := Value;
    case Value of
      fcNone  : OctreeFrustumCullingFunc := @FrustumCulling_None;
      fcSphere: OctreeFrustumCullingFunc := @FrustumCulling_Sphere;
      fcBox   : OctreeFrustumCullingFunc := @FrustumCulling_Box;
      fcBoth  : OctreeFrustumCullingFunc := @FrustumCulling_Both;
      else raise EInternalError.Create('SetOctreeFrustumCulling?');
    end;
  end;
end;

{ Render --------------------------------------------------------------------- }

function TCastleScene.RenderFrustumOctree_TestShape(
  Shape: TShape): boolean;
begin
  { We know that all shapes passed here are TGLShape, so we can cast }
  Result := TGLShape(Shape).RenderFrustumOctree_Visible and DistanceCullingCheck(Shape);
end;

procedure TCastleScene.RenderFrustumOctree_EnumerateShapes(
  ShapeIndex: Integer; CollidesForSure: boolean);
var
  Shape: TGLShape;
begin
  Shape := TGLShape(InternalOctreeRendering.ShapesList[ShapeIndex]);

  if (not Shape.RenderFrustumOctree_Visible) and
     ( CollidesForSure or
       OctreeFrustumCullingFunc(Shape) ) then
    Shape.RenderFrustumOctree_Visible := true;
end;

procedure TCastleScene.LocalRender(const Params: TRenderParams);

{ Call LocalRenderOutside, choosing TTestShapeVisibility function
  suitable for our Params.Frustum, octrees and some settings.

  If InternalOctreeRendering is initialized (so be sure to include
  ssRendering in @link(Spatial)), this octree will be used to quickly
  find visible Shapes. Otherwise, we will just enumerate all
  Shapes (which may be slower if you really have a lot of Shapes). }

  procedure TestOctreeWithFrustum(Octree: TShapeOctree);

    procedure ResetShapeVisible(Shape: TShape);
    begin
      TGLShape(Shape).RenderFrustumOctree_Visible := false;
    end;

  begin
    Shapes.Traverse(@ResetShapeVisible, false, true);
    Octree.EnumerateCollidingOctreeItems(Params.Frustum^,
      @RenderFrustumOctree_EnumerateShapes);
  end;

begin
  inherited;

  if GetVisible and (InternalDirty = 0) and
     (ReceiveShadowVolumes = Params.ShadowVolumesReceivers) then
  begin
    RenderFrustum_Frustum := Params.Frustum;
    RenderCameraKnown := (World <> nil) and World.CameraKnown;
    if RenderCameraKnown then
      RenderCameraPosition := Params.InverseTransform^.MultPoint(World.CameraPosition);

    if Assigned(InternalVisibilityTest) then
      LocalRenderOutside(InternalVisibilityTest, Params)
    else
    if InternalIgnoreFrustum then
      LocalRenderOutside(nil, Params)
    else
    if InternalOctreeRendering <> nil then
    begin
      TestOctreeWithFrustum(InternalOctreeRendering);
      LocalRenderOutside(@RenderFrustumOctree_TestShape, Params);
    end else
      LocalRenderOutside(FrustumCullingFunc, Params);

    { Nothing should even try to access camera outside of Render...
      But for security, set RenderCameraKnown to false. }
    RenderCameraKnown := false;
  end;
end;

{ Background-related things -------------------------------------------------- }

procedure TCastleScene.InvalidateBackground;
begin
  FreeAndNil(FBackground);
  FBackgroundNode := nil;
  FBackgroundValid := false;
end;

procedure TCastleScene.SetBackgroundSkySphereRadius(const Value: Single);
begin
  if Value <> FBackgroundSkySphereRadius then
  begin
    InvalidateBackground;
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
    InvalidateBackground;

  if BackgroundStack.Top <> nil then
  begin
    // if Log then
    //   WritelnLog('Background', Format('OpenGL background recreated, with radius %f',
    //     [BackgroundSkySphereRadius]));

    { In the future we could use FBackground.Update without recreating
      the instance. }
    FBackground := TBackground.Create;
    FBackground.Update(BackgroundStack.Top, BackgroundSkySphereRadius);
  end else
    FBackground := nil;

  FBackgroundNode := BackgroundStack.Top;
  FBackgroundValid := true;
end;

function TCastleScene.Background: TBackground;
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
    Result.UpdateTransform(BackgroundNode.TransformRotation);
end;

function TCastleScene.Attributes: TSceneRenderingAttributes;
begin
  Result := Renderer.Attributes as TSceneRenderingAttributes;
end;

procedure TCastleScene.UpdateGeneratedTextures(
  const RenderFunc: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const OriginalViewport: TRectangle);
var
  I: Integer;
  NeedsRestoreViewport: boolean;
  Shape: TGLShape;
  TextureNode: TAbstractTextureNode;
begin
  NeedsRestoreViewport := false;

  for I := 0 to GeneratedTextures.Count - 1 do
  begin
    Shape := TGLShape(GeneratedTextures.L[I].Shape);
    TextureNode := GeneratedTextures.L[I].TextureNode;

    if TextureNode is TGeneratedCubeMapTextureNode then
      AvoidShapeRendering := Shape else
    if TextureNode is TGeneratedShadowMapNode then
      AvoidNonShadowCasterRendering := true;

    Renderer.UpdateGeneratedTextures(Shape, TextureNode,
      RenderFunc, ProjectionNear, ProjectionFar, NeedsRestoreViewport,
      ViewpointStack.Top,
      World.CameraKnown,
      World.CameraPosition,
      World.CameraDirection,
      World.CameraUp);

    AvoidShapeRendering := nil;
    AvoidNonShadowCasterRendering := false;
  end;

  if NeedsRestoreViewport then
    RenderContext.Viewport := OriginalViewport;
end;

procedure TCastleScene.ViewChangedSuddenly;
var
  SI: TShapeTreeIterator;
begin
  inherited;

  if Attributes.ReallyUseOcclusionQuery then
  begin
    if Log then
      WritelnLog('Occlusion query', 'View changed suddenly');

    { Set OcclusionQueryAsked := false for all shapes. }
    SI := TShapeTreeIterator.Create(Shapes, false, false, false);
    try
      while SI.GetNext do
        TGLShape(SI.Current).OcclusionQueryAsked := false;
    finally FreeAndNil(SI) end;
  end;
end;

procedure TCastleScene.VisibleChangeNotification(const Changes: TVisibleChanges);
var
  I: Integer;
begin
  inherited;

  if Changes <> [] then
  begin
    for I := 0 to GeneratedTextures.Count - 1 do
    begin
      if GeneratedTextures.L[I].TextureNode is TGeneratedCubeMapTextureNode then
      begin
        if [vcVisibleGeometry, vcVisibleNonGeometry] * Changes <> [] then
          GeneratedTextures.L[I].Handler.UpdateNeeded := true;
      end else
      if GeneratedTextures.L[I].TextureNode is TGeneratedShadowMapNode then
      begin
        if vcVisibleGeometry in Changes then
          GeneratedTextures.L[I].Handler.UpdateNeeded := true;
      end else
        { For TRenderedTextureNode (and any other future generated textures),
          any visible change indicates to regenerate it. }
        GeneratedTextures.L[I].Handler.UpdateNeeded := true;
    end;
  end;
end;

procedure TCastleScene.CameraChanged(ACamera: TCamera);
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
      GeneratedTextures.L[I].Handler.UpdateNeeded := true;
end;

function TCastleScene.ScreenEffectsCount: Integer;
var
  I: Integer;
  SE: TScreenEffectNode;
begin
  Result := 0;

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
  Result := TComponentClass(ClassType).Create(AOwner) as TCastleScene;
  if RootNode <> nil then
    Result.Load(RootNode.DeepCopy as TX3DRootNode, true);
end;

{ TSceneRenderingAttributes ---------------------------------------------- }

constructor TSceneRenderingAttributes.Create;
begin
  inherited;

  FBlending := true;
  FBlendingSourceFactor := DefaultBlendingSourceFactor;
  FBlendingDestinationFactor := DefaultBlendingDestinationFactor;
  FBlendingSort := DefaultBlendingSort;
  FOcclusionSort := false;
  FControlBlending := true;
  FSolidWireframeScale := DefaultSolidWireframeScale;
  FSolidWireframeBias := DefaultSolidWireframeBias;
  FSilhouetteScale := DefaultSilhouetteScale;
  FSilhouetteBias := DefaultSilhouetteBias;
  FWireframeEffect := weNormal;
  FWireframeColor := DefaultWireframeColor;

  FScenes := TCastleSceneList.Create(false);

  if Assigned(OnCreate) then
    OnCreate(Self);
end;

destructor TSceneRenderingAttributes.Destroy;
begin
  FreeAndNil(FScenes);
  inherited;
end;

procedure TSceneRenderingAttributes.Assign(Source: TPersistent);
var
  S: TSceneRenderingAttributes;
begin
  if Source is TSceneRenderingAttributes then
  begin
    S := TSceneRenderingAttributes(Source);
    Blending := S.Blending;
    BlendingSourceFactor := S.BlendingSourceFactor;
    BlendingDestinationFactor := S.BlendingDestinationFactor;
    BlendingSort := S.BlendingSort;
    OcclusionSort := S.OcclusionSort;
    ControlBlending := S.ControlBlending;
    UseOcclusionQuery := S.UseOcclusionQuery;
    {$warnings off}
    UseHierarchicalOcclusionQuery := S.UseHierarchicalOcclusionQuery;
    {$warnings on}
    inherited;
  end else
    inherited;
end;

procedure TSceneRenderingAttributes.ReleaseCachedResources;
begin
  inherited;

  { We have to do at least Renderer.UnprepareAll.
    Actually, we have to do more: TCastleScene must also be disconnected
    from OpenGL, to release screen effects (referencing renderer shaders)
    and such. So full CloseGLRenderer is needed. }

  if TemporaryAttributeChange = 0 then
    FScenes.CloseGLRenderer;
end;

procedure TSceneRenderingAttributes.SetBlending(const Value: boolean);
begin
  FBlending := Value;
end;

procedure TSceneRenderingAttributes.SetBlendingSourceFactor(
  const Value: TBlendingSourceFactor);
begin
  FBlendingSourceFactor := Value;
end;

procedure TSceneRenderingAttributes.SetBlendingDestinationFactor(
  const Value: TBlendingDestinationFactor);
begin
  FBlendingDestinationFactor := Value;
end;

procedure TSceneRenderingAttributes.SetBlendingSort(const Value: TBlendingSort);
begin
  FBlendingSort := Value;
end;

procedure TSceneRenderingAttributes.SetControlBlending(const Value: boolean);
begin
  FControlBlending := Value;
end;

procedure TSceneRenderingAttributes.SetUseOcclusionQuery(const Value: boolean);
var
  I: Integer;
begin
  if UseOcclusionQuery <> Value then
  begin
    FUseOcclusionQuery := Value;

    if UseOcclusionQuery then
    begin
      { If you switch UseOcclusionQuery on, then off, then move around the scene
        a lot, then switch UseOcclusionQuery back on --- you don't want to use
        results from previous query that was done many frames ago. }
      FScenes.ViewChangedSuddenly;

      { Make PrepareShapesResources again, to cause TGLShape.PrepareResources
        that initializes OcclusionQueryId for each shape }
      if TemporaryAttributeChange = 0 then
        for I := 0 to FScenes.Count - 1 do
          if FScenes[I] <> nil then
            FScenes[I].PreparedShapesResources := false;
    end;
  end;
end;

function TSceneRenderingAttributes.ReallyUseOcclusionQuery: boolean;
begin
  {$warnings off}
  Result := UseOcclusionQuery and (not UseHierarchicalOcclusionQuery) and
    GLFeatures.ARB_occlusion_query and
    GLFeatures.VertexBufferObject and
    (GLFeatures.QueryCounterBits > 0);
  {$warnings on}
end;

function TSceneRenderingAttributes.ReallyUseHierarchicalOcclusionQuery: boolean;
begin
  {$warnings off}
  Result := UseHierarchicalOcclusionQuery and
    GLFeatures.ARB_occlusion_query and
    GLFeatures.VertexBufferObject and
    (GLFeatures.QueryCounterBits > 0);
  {$warnings on}
end;

procedure TSceneRenderingAttributes.SetPhongShading(const Value: boolean);
var
  I: Integer;
begin
  if PhongShading <> Value then
  begin
    inherited;
    { When switching this we want to force generating necessary
      shaders at the next PrepareResources call. Otherwise shaders would
      be prepared only when shapes come into view, which means that navigating
      awfully stutters for some time after changing this property. }
    if TemporaryAttributeChange = 0 then
      for I := 0 to FScenes.Count - 1 do
        if FScenes[I] <> nil then
          FScenes[I].PreparedRender := false;
  end;
end;

{ TCastleSceneList ------------------------------------------------------ }

procedure TCastleSceneList.GLContextClose;
{ This may be called from various destructors,
  so we are extra careful here and check Items[I] <> nil. }
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] <> nil then
      Items[I].GLContextClose;
end;

procedure TCastleSceneList.InvalidateBackground;
{ This may be called from various destructors,
  so we are extra careful here and check Items[I] <> nil. }
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] <> nil then
      Items[I].InvalidateBackground;
end;

procedure TCastleSceneList.CloseGLRenderer;
{ This may be called from various destructors,
  so we are extra careful here and check Items[I] <> nil. }
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] <> nil then
      Items[I].CloseGLRenderer;
end;

procedure TCastleSceneList.ViewChangedSuddenly;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] <> nil then
      Items[I].ViewChangedSuddenly;
end;

{ TBasicRenderParams --------------------------------------------------------- }

constructor TBasicRenderParams.Create;
begin
  inherited;
  FBaseLights := TLightInstancesList.Create;
  InShadow := false;
  { Transparent and ShadowVolumesReceivers do not have good default values.
    User of TBasicRenderParams should call Render method with
    all 4 combinations of them, to really render everything correctly.
    We just set them here to capture most 3D objects
    (as using TBasicRenderParams for anything is a discouraged hack anyway). }
  ShadowVolumesReceivers := true;
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

initialization
  GLContextCache := TGLRendererContextCache.Create;
finalization
  FreeAndNil(GLContextCache);
end.
