{
  Copyright 2003-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML/X3D complete scene handling and OpenGL rendering (TCastleScene). }
unit CastleScene;

{$modeswitch nestedprocvars}{$H+}

interface

uses SysUtils, Classes, CastleVectors, CastleBoxes, X3DNodes, CastleClassUtils,
  CastleUtils, CastleSceneCore, CastleRenderer, GL, GLU, GLExt, CastleBackground,
  CastleGLUtils, CastleShapeOctree, CastleGLShadowVolumes, X3DFields, CastleTriangles,
  CastleRendererLights, CastleShapes, CastleFrustum, Castle3D, CastleGLShaders, FGL,
  CastleGenericLists;

{$define read_interface}

type
  TGLShape = class;
  TSceneRenderingAttributes = class;
  TCastleSceneList = class;

  TTestShapeVisibility = function (Shape: TGLShape): boolean of object;

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

      WireframeColor and LineWidth are used as wireframe
      line color/width (regardless of current scene
      @link(TRenderingAttributes.Mode Attributes.Mode) value).

      This usually gives best results when
      @link(TRenderingAttributes.Mode Attributes.Mode) = rmPureGeometry.
      Then current glColor sets the color of the solid model
      (and, like said before, WireframeColor sets wireframe color). }
    weSolidWireframe,

    { The model is rendered as normal, with silhouette outlined around it.
      This works quite like weSolidWireframe, except that weSolidWireframe
      makes the wireframe mesh slightly in front the model, while weSilhouette
      makes the wireframe mesh slightly at the back of the model. This way
      only the silhouette is visible from the wireframe rendering.

      WireframeColor and LineWidth are used as silhouette
      line color/width (regardless of current scene
      @link(TRenderingAttributes.Mode Attributes.Mode) value).

      This is sometimes sensible to use with
      @link(TRenderingAttributes.Mode Attributes.Mode) = rmPureGeometry.
      Then current glColor sets the color of the solid model
      (and, like said before, WireframeColor sets wireframe color). }
    weSilhouette);

  TBeforeShapeRenderProc = procedure (Shape: TShape) of object;

  TRenderingAttributesEvent = procedure (Attributes: TSceneRenderingAttributes) of object;

  TSceneRenderingAttributes = class(TRenderingAttributes)
  private
    { Scenes that use Renderer with this TSceneRenderingAttributes instance. }
    FScenes: TCastleSceneList;

    FBlending: boolean;
    FBlendingSourceFactor: TGLenum;
    FBlendingDestinationFactor: TGLenum;
    FBlendingSort: boolean;
    FControlBlending: boolean;
    FWireframeColor: TVector3Single;
    FWireframeEffect: TWireframeEffect;
    FUseOcclusionQuery: boolean;
    FUseHierarchicalOcclusionQuery: boolean;
    FDebugHierOcclusionQueryResults: boolean;

    { Checks UseOcclusionQuery, existence of GL_ARB_occlusion_query,
      and GLQueryCounterBits > 0. If @false, ARB_occlusion_query just cannot
      be used.

      Also, returns @false when UseHierarchicalOcclusionQuery is @true
      --- because then UseHierarchicalOcclusionQuery should take precedence. }
    function ReallyUseOcclusionQuery: boolean;

    { Checks UseHierarchicalOcclusionQuery, existence of GL_ARB_occlusion_query,
      and GLQueryCounterBits > 0. If @false, ARB_occlusion_query just cannot
      be used. }
    function ReallyUseHierarchicalOcclusionQuery: boolean;
  protected
    procedure ReleaseCachedResources; override;

    procedure SetBlending(const Value: boolean); virtual;
    procedure SetBlendingSourceFactor(const Value: TGLenum); virtual;
    procedure SetBlendingDestinationFactor(const Value: TGLenum); virtual;
    procedure SetBlendingSort(const Value: boolean); virtual;
    procedure SetControlBlending(const Value: boolean); virtual;
    procedure SetUseOcclusionQuery(const Value: boolean); virtual;

    procedure SetShaders(const Value: TShadersRendering); override;
  public
    const
      { }
      DefaultBlendingSourceFactor = GL_SRC_ALPHA;

      { Default value of Attributes.BlendingDestinationFactor.
        See TSceneRenderingAttributes.BlendingDestinationFactor.

        Using ONE_MINUS_SRC_ALPHA is the standard value for 3D graphic stuff,
        often producing best results. However, it causes troubles when
        multiple transparent shapes are visible on the same screen pixel.
        For closed convex 3D objects, using backface culling
        (solid = TRUE for geometry) helps. For multiple transparent shapes,
        sorting the transparent shapes helps,
        see TSceneRenderingAttributes.BlendingSort.
        Sometimes, no solution works for all camera angles.

        Another disadvantage of ONE_MINUS_SRC_ALPHA may be that
        the color of opaque shapes disappears too quickly from
        resulting image (since GL_ONE_MINUS_SRC_ALPHA scales it down).
        So the image may be darker than you like.

        You can instead consider using GL_ONE, that doesn't require sorting
        and never has problems with multiple transparent shapes.
        On the other hand, it only adds to the color,
        often making too bright results. }
      DefaultBlendingDestinationFactor = GL_ONE_MINUS_SRC_ALPHA;

      { }
      DefaultBlendingSort = false;

      DefaultWireframeColor: TVector3Single = (0, 0, 0);

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
      See OpenGL documentation of glBlendFunc for possible values here.

      See also DefaultBlendingDestinationFactor for comments about
      GL_ONE and GL_ONE_MINUS_SRC_ALPHA.

      Note that this is only a default, VRML/X3D model can override this
      for specific shapes by using our extension BlendMode node.
      See [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_blending].

      @groupBegin }
    property BlendingSourceFactor: TGLenum
      read FBlendingSourceFactor write SetBlendingSourceFactor
      default DefaultBlendingSourceFactor;
    property BlendingDestinationFactor: TGLenum
      read FBlendingDestinationFactor write SetBlendingDestinationFactor
      default DefaultBlendingDestinationFactor;
    property BlendingSort: boolean
      read FBlendingSort write SetBlendingSort
      default DefaultBlendingSort;
    { @groupEnd }

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

    { Wireframe color, used with some WireframeEffect values.
      Default value is DefaultWireframeColor. }
    property WireframeColor: TVector3Single
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

      This requires the usage of TCastleSceneCore.OctreeRendering.
      Also, it always does frustum culling (like fcBox for now),
      regardless of TCastleScene.OctreeFrustumCulling setting.

      The algorithm used underneath is "Coherent Hierarchical Culling",
      described in detail in "GPU Gems 2",
      Chapter 6: "Hardware Occlusion Queries Made Useful",
      by Michael Wimmer and Jiri Bittner. Online on
      [http://http.developer.nvidia.com/GPUGems2/gpugems2_chapter06.html]. }
    property UseHierarchicalOcclusionQuery: boolean
      read FUseHierarchicalOcclusionQuery
      write FUseHierarchicalOcclusionQuery default false;

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
  end;

  { TShape descendant for usage within TCastleScene.
    Basically, this is just the same thing as TShape, with some
    internal information needed by TCastleScene. }
  TGLShape = class(TX3DRendererShape)
  private
    { Keeps track if this shape was passed to Renderer.Prepare. }
    PreparedForRenderer: boolean;

    UseBlending: boolean;
    { Is UseBlending calculated and current. }
    PreparedUseBlending: boolean;

    procedure PrepareResources;
  private
    { Private things only for RenderFrustumOctree ---------------------- }
    RenderFrustumOctree_Visible: boolean;

    { ------------------------------------------------------------
      Private things used only when Attributes.ReallyUseOcclusionQuery }

    { OcclusionQueryId is 0 if not initialized yet.
      When it's 0, value of OcclusionQueryAsked doesn't matter,
      OcclusionQueryAsked is always reset to @false when initializing
      OcclusionQueryId. }
    OcclusionQueryId: TGLint;
    OcclusionQueryAsked: boolean;

    { For Hierarchical Occlusion Culling }
    RenderedFrameId: Cardinal;
  public
    procedure Changed(const InactiveOnly: boolean;
      const Changes: TX3DChanges); override;
  end;

type
  TPrepareResourcesOption = Castle3D.TPrepareResourcesOption;
  TPrepareResourcesOptions = Castle3D.TPrepareResourcesOptions;

const
  prRender = Castle3D.prRender;
  prBackground = Castle3D.prBackground;
  prBoundingBox = Castle3D.prBoundingBox;
  prTrianglesListShadowCasters = Castle3D.prTrianglesListShadowCasters;
  prManifoldAndBorderEdges = Castle3D.prManifoldAndBorderEdges;

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

  { Basic non-abstact implementation of render params for calling T3D.Render.

    @exclude
    @bold(This is a hack, exposed here only to support some really weird
    OpenGL tricks in engine example programs. Do not use this in your own code.)
    To be used when you have to call T3D.Render, but you don't use scene manager.
    Usually this should not be needed, and this class may be removed at some
    point! You should always try to use TCastleSceneManager to manage and render
    3D stuff in new programs, and then TCastleSceneManager will take care of creating
    proper render params instance for you. }
  TBasicRenderParams = class(TRenderParams)
  public
    FBaseLights: TLightInstancesList;
    constructor Create;
    destructor Destroy; override;
    function BaseLights(Scene: T3D): TLightInstancesList; override;
  end;

  { Complete handling and rendering of a 3D VRML/X3D scene.
    This is a descendant of TCastleSceneCore that adds efficient rendering
    using OpenGL.

    This uses internal @link(TGLRenderer) instance,
    adding some features and comfortable methods on top of it (like blending).
    See @link(Render) method for some details.

    This class also provides comfortable management for
    @link(TBackground) instance associated with this VRML model,
    that may be used to render currenly bound VRML background.
    See @link(Background) function.

    Calling methods PrepareResources, Render or Background connects this
    class with current OpenGL context. Which means that all the following
    calls should be done with the same OpenGL context set as current.
    Calling GLContextClose or the destructor removes this connection. }
  TCastleScene = class(TCastleSceneCore)
  private
    Renderer: TGLRenderer;
    FReceiveShadowVolumes: boolean;

    { Cache used by this scene. Always initialized to non-nil by constructor. }
    Cache: TGLRendererContextCache;

    { used by RenderScene }
    FilteredShapes: TShapeList;

    { Render everything.

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
    procedure RenderScene(TestShapeVisibility: TTestShapeVisibility;
      const Frustum: TFrustum;
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
  private
    FOwnsRenderer: boolean;

    { Fog for this shape. @nil if none. }
    function ShapeFog(Shape: TShape): IAbstractFogObject;
  private
    { Used by UpdateGeneratedTextures, to prevent rendering the shape
      for which reflection texture is generated. (This wouldn't cause
      recursive loop in our engine, but still it's bad --- rendering
      from the inside of the object usually obscures the world around...). }
    AvoidShapeRendering: TGLShape;

    { Used by UpdateGeneratedTextures, to prevent rendering non-shadow casters
      for shadow maps. }
    AvoidNonShadowCasterRendering: boolean;

    PreparedShapesResouces, PreparedRender: boolean;
    VarianceShadowMapsProgram: array [boolean] of TGLSLProgram;

    { Private things for RenderFrustum --------------------------------------- }

    function FrustumCulling_None(Shape: TGLShape): boolean;
    function FrustumCulling_Sphere(Shape: TGLShape): boolean;
    function FrustumCulling_Box(Shape: TGLShape): boolean;
    function FrustumCulling_Both(Shape: TGLShape): boolean;
  private
          FFrustumCulling: TFrustumCulling;
    FOctreeFrustumCulling: TFrustumCulling;
    procedure       SetFrustumCulling(const Value: TFrustumCulling);
    procedure SetOctreeFrustumCulling(const Value: TFrustumCulling);
  private
          FrustumCullingFunc: TTestShapeVisibility;
    OctreeFrustumCullingFunc: TTestShapeVisibility;

    RenderFrustum_Frustum: PFrustum;

    function RenderFrustumOctree_TestShape(Shape: TGLShape): boolean;
    procedure RenderFrustumOctree_EnumerateShapes(
      ShapeIndex: Integer; CollidesForSure: boolean);

    { Turn off lights that are not supposed to light in the shadow.
      This simply turns LightOn to @false if the light has
      kambiShadows = TRUE (see
      [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_shadows]).

      It's useful to pass this as LightRenderEvent to @link(Render)
      when you use shadow algorithm that requires
      you to make a first pass rendering the scene all shadowed. }
    class procedure LightRenderInShadow(const Light: TLightInstance;
      var LightOn: boolean);

    { shadow things ---------------------------------------------------------- }

    { Rendering shadow volumes.

      There are two algorithms here:

      @orderedList(
        @item(Rendering with silhouette optimization.

          This renders shadow quads of silhouette edge. Edges from ManifoldEdges
          list are used to find silhouette edge. Additionally edges from
          BorderEdges always produce shadow quads, i.e. we treat them
          like they would always be silhouette edges.

          The very idea of this optimization is that most edges are in
          ManifoldEdges and so only real silhouette edges produce shadow quads.
          In other words, BorderEdges list should not contain too many items.
          When BorderEdges contains all edges (ManifoldEdges is empty), then
          this method degenerates to a naive rendering without silhouette
          optimization. So you should try to make your models as much as
          possible resembling nice 2-manifolds. Ideally, if your mesh
          is a number of perfectly closed manifolds, and vertex ordering
          is consistent, then BorderEdges is empty, and this works perfect.

          Usually, most models are mostly 2-manifold (only the real border
          edges are, well, in BorderEdges), and this works great.
          See "VRML engine documentation" on
          [http://castle-engine.sourceforge.net/engine_doc.php],
          chapter "Shadows", for description and pictures of possible artifacts
          when trying to use this on models that are not 2-manifold.)

        @item(Without silhouette optimization.
          This is the naive approach that just renders
          3 shadow quads for each triangle.

          It is so slow that there's no public way to actually
          get this behavior, you have to edit source code and set
          AllowSilhouetteOptimization constant to false if you really want this.
          In all real uses, it's better to
          fix your 3D model to be correct 2-manifold.)
      )

      LightCap and DarkCap say whether you want to cap your shadow volume.
      LightCap is the cap at the caster position, DarkCap is the cap in infinity.
      This is needed by z-fail method, you should set them both to @true.
      To be more optimal, you can request LightCap only if z-fail @italic(and
      the caster is inside camera frustum). For directional lights, DarkCap is
      ignored, since the volume is always closed by a single point in infinity.
    }
    procedure RenderSilhouetteShadowVolume(
      const LightPos: TVector4Single;
      const TransformIsIdentity: boolean;
      const Transform: TMatrix4Single;
      const LightCap, DarkCap: boolean);

    procedure RenderAllShadowVolume(
      const LightPos: TVector4Single;
      const TransformIsIdentity: boolean;
      const Transform: TMatrix4Single;
      LightCap, DarkCap: boolean);

  private
    { For Hierarchical Occlusion Culling }
    FrameId: Cardinal;
  protected
    function CreateShape(AGeometry: TAbstractGeometryNode;
      AState: TX3DGraphTraverseState; ParentInfo: PTraversingInfo): TShape; override;
    procedure InvalidateBackground; override;
  public
    constructor Create(AOwner: TComponent); override;

    constructor CreateCustomCache(AOwner: TComponent; ACache: TGLRendererContextCache);

    { A very special constructor, that forces this class to use
      provided ACustomRenderer. ACustomRenderer must be <> @nil.

      Note that this renderer must be created with AttributesClass
      = TSceneRenderingAttributes.

      @italic(Don't use this unless you really know what you're doing!)
      In all normal circumstances you should use normal @link(Create)
      constructor, that will internally create and use internal renderer object.
      If you use this constructor you will have to understand how internally
      this class synchronizes itself with underlying Renderer object.

      Once again, if you're not sure, then simply don't use this
      constructor. It's for internal use --- namely it's internally used
      by TCastlePrecalculatedAnimation, this way all scenes of the animation share
      the same renderer which means that they also share the same
      information about textures and images loaded into OpenGL.
      And this is crucial for TCastlePrecalculatedAnimation, otherwise animation with
      100 scenes would load the same texture to OpenGL 100 times. }
    constructor CreateCustomRenderer(AOwner: TComponent;
      ACustomRenderer: TGLRenderer);

    destructor Destroy; override;

    { Destroy any associations of this object with current OpenGL context.
      For example, release any allocated texture names.

      Generally speaking, destroys everything that is allocated by
      PrepareResources call. It's harmless to call this
      method when there are already no associations with current OpenGL context.
      This is called automatically from the destructor. }
    procedure GLContextClose; override;

    procedure PrepareResources(Options: TPrepareResourcesOptions;
      ProgressStep: boolean; BaseLights: TAbstractLightInstancesList); override;

    { Render for OpenGL. The rendering parameters are configurable
      by @link(Attributes), see TSceneRenderingAttributes and
      TRenderingAttributes.

      For more details about rendering, see @link(GLRenderer) unit comments.
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
    procedure Render(TestShapeVisibility: TTestShapeVisibility;
      const Frustum: TFrustum;
      const Params: TRenderParams);

    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;

    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); override;

    { Render shadow volume (sides and caps) of this scene, for shadow volume
      algorithm. Checks ShadowVolumeRenderer.InitScene to know if the shadow
      needs to be rendered at all.
      It will calculate current bounding box (looking at ParentTransform,
      ParentTransformIsIdentity and BoundingBox method).

      It always uses silhouette optimization. This is the usual,
      fast method of rendering shadow volumes.
      Will not do anything (treat scene like not casting shadows,
      like CastShadowVolumes = false) if the model is not perfect 2-manifold,
      i.e. has some BorderEdges (although we could handle some BorderEdges
      for some points of view, this was always inherently dangerous
      and leading to rendering artifacts).
      See RenderSilhouetteShadowVolume, RenderAllShadowVolume comments in code
      for more explanation.

      All shadow quads are generated from scene triangles transformed
      by ParentTransform. We must be able to correctly detect front and
      back facing triangles with respect to light position,
      so ShadowVolumeRenderer.LightPosition and
      "this scene transformed by ParentTransform" must be in the same coordinate system.
      That's why explicit ParentTransform parameter is needed, you can't get away
      with simply doing glPush/PopMatrix and glMultMatrix around RenderShadowVolume
      call. If ParentTransformIsIdentity then ParentTransform value is ignored and
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
      (i.e. it's considered front face of this shadow volume).

      All the commands passed to OpenGL by this methods are:
      glBegin, sequence of glVertex, then glEnd. }
    procedure RenderShadowVolume(
      ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
      const ParentTransformIsIdentity: boolean;
      const ParentTransform: TMatrix4Single); override;

    { Render silhouette edges.
      Silhouette is determined from the ObserverPos.

      Whole scene is transformed by Transform (before checking which
      edges are silhouette and before rendering). In other words,
      Transform must transform the scene to the same coord space where
      given ObserverPos is. When they are is the same space, just use
      IdentityMatrix4Single.

      This implicitly creates and uses ManifoldEdges. In fact, one of the uses
      of this is to visually see that ManifoldEdges are coorect. }
    procedure RenderSilhouetteEdges(
      const ObserverPos: TVector4Single;
      const Transform: TMatrix4Single);

    { Render all border edges (the edges without neighbor).

      This implicitly creates and uses BorderEdges. In fact, one of the uses
      of this is to visually see that BorderEdges are coorect. }
    procedure RenderBorderEdges(
      const Transform: TMatrix4Single);
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
  public
    property BackgroundSkySphereRadius: Single
      read FBackgroundSkySphereRadius write SetBackgroundSkySphereRadius
      default 1;

    procedure PrepareBackground;

    { Returns TBackground instance for this scene. Background's properties
      are based on the attributes of currently bound X3DBackgroundNode
      VRML node in the RootNode scene (and on his place in scene transformations).
      When events are not concerned, this is simply the first X3DBackgroundNode
      found in the scene (when events work, this may change through set_bind
      events).
      They are also based on current value of BackgroundSkySphereRadius.

      If there is no currently bound background node in VRML scene this
      function returns nil.
      TODO: Also, if currently bound background cannot be rendered,
      we return @nil. For now this happens for TextureBakckground,
      that temporarily cannot be rendered.

      Note: this Background object is managed (automatically created/freed
      etc.) by this TCastleScene object but it is NOT used anywhere
      in this class, e.g. Render does not call Background.Render. If you want to
      use this Background somehow, you have to do this yourself.

      The results of this function are internally cached. Cache is invalidated
      on such situations as change in RootNode scene, changes to
      BackgroundSkySphereRadius, GLContextClose.

      PrepareBackground (and PrepareResources with prBackground)
      automatically validate this cache.

      Remember that this cache is connected with the current OpenGL context.
      So you HAVE to call GLContextClose to disconnent this object from
      current OpenGL context after you used this function. }
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
      const OriginalViewportX, OriginalViewportY: LongInt;
      const OriginalViewportWidth, OriginalViewportHeight: Cardinal); override;

    procedure ViewChangedSuddenly; override;

    procedure VisibleChangeNotification(const Changes: TVisibleChanges); override;

    { Screen effects information, used by TCastleAbstractViewport.ScreenEffects.
      ScreenEffectsCount may actually prepare screen effects.
      @groupBegin }
    function ScreenEffects(Index: Integer): TGLSLProgram;
    function ScreenEffectsCount: Integer;
    function ScreenEffectsNeedDepth: boolean;
    { @groupEnd }
  published
    { Fine-tune performance of @link(Render) when
      OctreeRendering is @italic(not) available.

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

    { Fine-tune performance of @link(Render) when
      OctreeRendering @italic(is available).

      See TFrustumCulling. }
    property OctreeFrustumCulling: TFrustumCulling
      read FOctreeFrustumCulling write SetOctreeFrustumCulling default fcBox;

    property ReceiveShadowVolumes: boolean
      read FReceiveShadowVolumes write FReceiveShadowVolumes default true;
  end;

  TCastleSceneList = class(specialize TFPGObjectList<TCastleScene>)
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

const
  { Options to pass to TCastleScene.PrepareResources to make
    sure that rendering with shadow volumes is as fast as possible.

    For now this actually could be equal to prManifoldEdges
    (prTrianglesListShadowCasters has to be prepared while preparing
    ManifoldEdges edges anyway). But for the future shadow volumes
    optimizations, it's best to use this constant. }
  prShadowVolume = [prTrianglesListShadowCasters, prManifoldAndBorderEdges];

type
  TTriangle4SingleList = specialize TGenericStructList<TTriangle4Single>;

procedure Register;

var
  { Global OpenGL context cache.
    This caches common things, like textures, shapes, and much more.
    Our OpenGL resources are currently shared across all OpenGL contexts,
    and they all automatically share this cache. }
  GLContextCache: TGLRendererContextCache;

implementation

uses CastleGLVersion, CastleImages, CastleLog, CastleWarnings,
  CastleStringUtils, CastleRenderingCamera;

var
  TemporaryAttributeChange: Cardinal = 0;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleScene]);
end;

{ TGLShape --------------------------------------------------------------- }

procedure TGLShape.Changed(const InactiveOnly: boolean;
  const Changes: TX3DChanges);
var
  GLScene: TCastleScene;
begin
  inherited;

  GLScene := TCastleScene(ParentScene);

  if Cache <> nil then
  begin
    { Ignore changes that don't affect prepared arrays,
      like transformation, clip planes and everything else that is applied
      by renderer every time, and doesn't affect TGeometryArrays. }
    if Changes * [chCoordinate] <> [] then
      Cache.FreeArrays([vtCoordinate]) else
    if Changes * [chVisibleVRML1State, chGeometryVRML1State,
      chColorNode, chTextureCoordinate, chGeometry, chFontStyle] <> [] then
      Cache.FreeArrays(AllVboTypes);
  end;

  if Changes * [chTextureImage, chTextureRendererProperties] <> [] then
  begin
    GLScene.Renderer.UnprepareTexture(State.Texture);
    PreparedForRenderer := false;
    PreparedUseBlending := false;
    { PreparedShapesResouces must be reset, otherwise scene will not even
      call our PrepareResources next time. }
    GLScene.PreparedShapesResouces := false;
  end;

  { When Material.transparency changes, recalculate UseBlending. }
  if chUseBlending in Changes then
  begin
    PreparedUseBlending := false;
    { PreparedShapesResouces must be reset, otherwise scene will not even
      call our PrepareResources next time. }
    GLScene.PreparedShapesResouces := false;
  end;
end;

procedure TGLShape.PrepareResources;
var
  GLScene: TCastleScene;
begin
  GLScene := TCastleScene(ParentScene);

  if not PreparedForRenderer then
  begin
    GLScene.Renderer.Prepare(State);
    PreparedForRenderer := true;
  end;

  if not PreparedUseBlending then
  begin
    { UseBlending is used by RenderScene to decide is Blending used for given
      shape. }
    UseBlending := Transparent;
    PreparedUseBlending := true;
  end;

  if GLScene.Attributes.ReallyUseOcclusionQuery and
     (OcclusionQueryId = 0) then
  begin
    glGenQueriesARB(1, @OcclusionQueryId);
    OcclusionQueryAsked := false;
  end;
end;

{ ShapesSplitBlending ---------------------------------------------------- }

{ Fill a TShapeList with only opaque (UseBlending = @false) or
  only transparent shapes (UseBlending = @true). }
procedure ShapesFilterBlending(
  Tree: TShapeTree;
  const OnlyActive, OnlyVisible, OnlyCollidable: boolean;
  TestShapeVisibility: TTestShapeVisibility;
  const FilteredShapes: TShapeList; const UseBlending: boolean);

  procedure AddToList(Shape: TShape);
  begin
    if TGLShape(Shape).UseBlending = UseBlending then
      FilteredShapes.Add(Shape);
  end;

  procedure AddToListIfTested(Shape: TShape);
  begin
    if (TGLShape(Shape).UseBlending = UseBlending) and
       TestShapeVisibility(TGLShape(Shape)) then
      FilteredShapes.Add(Shape);
  end;

var
  Capacity: Integer;
begin
  FilteredShapes.Clear;

  { Set Capacity to max value at the beginning, to speed adding items later. }
  Capacity := Tree.ShapesCount(OnlyActive, OnlyVisible, OnlyCollidable);
  FilteredShapes.Capacity := Capacity;

  if Assigned(TestShapeVisibility) then
    Tree.Traverse(@AddToListIfTested, OnlyActive, OnlyVisible, OnlyCollidable) else
    Tree.Traverse(@AddToList, OnlyActive, OnlyVisible, OnlyCollidable);
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
end;

destructor TBasicRenderParams.Destroy;
begin
  FreeAndNil(FBaseLights);
  inherited;
end;

function TBasicRenderParams.BaseLights(Scene: T3D): TLightInstancesList;
begin
  Result := FBaseLights;
end;

{ TCastleScene ------------------------------------------------------------ }

constructor TCastleScene.Create(AOwner: TComponent);
begin
  { inherited Create *may* call some virtual things overriden here
    (although right now it doesn't): it may bind new viewpoint which
    may call ViewChangedSuddenly which is overridden here and uses Attributes.
    That's why I have to initialize them *before* "inherited Create" }

  { Cache may be already assigned, when we came here from
    CreateCustomRenderer or CreateCustomCache. }
  if Cache = nil then
    Cache := GLContextCache;

  { Renderer may be already assigned, when we came here from
    CreateCustomRenderer. }
  if Renderer = nil then
  begin
    FOwnsRenderer := true;
    Renderer := TGLRenderer.Create(TSceneRenderingAttributes, Cache);
  end;

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
end;

constructor TCastleScene.CreateCustomCache(
  AOwner: TComponent; ACache: TGLRendererContextCache);
begin
  Assert(ACache <> nil);
  Cache := ACache;

  Create(AOwner);
end;

constructor TCastleScene.CreateCustomRenderer(
  AOwner: TComponent; ACustomRenderer: TGLRenderer);
begin
  FOwnsRenderer := false;
  Renderer := ACustomRenderer;

  CreateCustomCache(AOwner, ACustomRenderer.Cache);
end;

destructor TCastleScene.Destroy;
begin
  FreeAndNil(FilteredShapes);

  GLContextClose;

  { Note that this calls Renderer.Attributes, so use this before
    deinitializing Renderer. }
  if Renderer <> nil then
    Attributes.FScenes.Remove(Self);

  if FOwnsRenderer then
  begin
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
  end else
    Renderer := nil;

  Cache := nil; // just for safety

  inherited;
end;

function TCastleScene.CreateShape(AGeometry: TAbstractGeometryNode;
  AState: TX3DGraphTraverseState; ParentInfo: PTraversingInfo): TShape;
begin
  Result := TGLShape.Create(Self, AGeometry, AState, ParentInfo);
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
  PreparedShapesResouces := false;

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

  { TODO: if FOwnsRenderer then we should do something more detailed
    then just Renderer.UnprepareAll. It's not needed for TCastlePrecalculatedAnimation
    right now, so it's not implemented. }
  if Renderer <> nil then Renderer.UnprepareAll;

  if Shapes <> nil then
  begin
    SI := TShapeTreeIterator.Create(Shapes, false, true);
    try
      while SI.GetNext do
      begin
        S := TGLShape(SI.Current);

        S.PreparedForRenderer := false;
        S.PreparedUseBlending := false;

        if S.OcclusionQueryId <> 0 then
        begin
          glDeleteQueriesARB(1, @(S.OcclusionQueryId));
          S.OcclusionQueryId := 0;
        end;
      end;
    finally FreeAndNil(SI) end;
  end;

  if VarianceShadowMapsProgram[false] <> nil then
    FreeAndNil(VarianceShadowMapsProgram[false]);
  if VarianceShadowMapsProgram[true] <> nil then
    FreeAndNil(VarianceShadowMapsProgram[true]);
end;

procedure TCastleScene.GLContextClose;
begin
  inherited;
  CloseGLRenderer;
  InvalidateBackground;
end;

function TCastleScene.ShapeFog(Shape: TShape): IAbstractFogObject;
begin
  Result := Shape.State.LocalFog;
  if Result = nil then
    Result := FogNode;
end;

{ Given blending name (as defined by VRML BlendMode node spec,
  http://www.instantreality.org/documentation/nodetype/BlendMode/),
  returns @true and corresponding OpenGL constant as Factor.

  Returns @false if S doesn't match any known name, or it's "none",
  or it's not supported by current OpenGL implementation (some factors
  may require newer OpenGL versions), or it's not for this kind
  (which means it's not for source factor if Source = true,
  or it's not for dest factor is Source = false).

  If returns @true, then also updates NeedsConstXxx.
  "Updates" means that always does something like
    NeedsConstXxx := NeedsConstXxx or <this factor needs them>;
  so can only change from false to true.
}
function BlendingFactorNameToStr(S: string;
  out Factor: TGLEnum;
  var NeedsConstColor, NeedsConstAlpha: boolean;
  Source: boolean): boolean;

type
  TBlendingFactor = record
    Name: string;
    GL: TGLEnum;
    Source, Dest: boolean;
    NeedsConstColor, NeedsConstAlpha: boolean;
  end;

const
  BlendingFactors: array [0..15] of TBlendingFactor =
  (
    { Three most frequently used values are placed at the beginning of the list,
      for speedup. }
    (Name: 'src_alpha'               ; GL: GL_SRC_ALPHA               ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one_minus_src_alpha'     ; GL: GL_ONE_MINUS_SRC_ALPHA     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one'                     ; GL: GL_ONE                     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),

    (Name: 'none'                    ; GL: GL_NONE                    ; Source: false; Dest: false; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'zero'                    ; GL: GL_ZERO                    ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'dst_color'               ; GL: GL_DST_COLOR               ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'src_color'               ; GL: GL_SRC_COLOR               ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one_minus_dst_color'     ; GL: GL_ONE_MINUS_DST_COLOR     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one_minus_src_color'     ; GL: GL_ONE_MINUS_SRC_COLOR     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'dst_alpha'               ; GL: GL_DST_ALPHA               ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'one_minus_dst_alpha'     ; GL: GL_ONE_MINUS_DST_ALPHA     ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: false),
    (Name: 'src_alpha_saturate'      ; GL: GL_SRC_ALPHA_SATURATE      ; Source: true ; Dest: false; NeedsConstColor: false; NeedsConstAlpha: false),

    (Name: 'constant_color'          ; GL: GL_CONSTANT_COLOR          ; Source: true ; Dest: true ; NeedsConstColor: true ; NeedsConstAlpha: false),
    (Name: 'one_minus_constant_color'; GL: GL_ONE_MINUS_CONSTANT_COLOR; Source: true ; Dest: true ; NeedsConstColor: true ; NeedsConstAlpha: false),
    (Name: 'constant_alpha'          ; GL: GL_CONSTANT_ALPHA          ; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: true ),
    (Name: 'one_minus_constant_alpha'; GL: GL_ONE_MINUS_CONSTANT_ALPHA; Source: true ; Dest: true ; NeedsConstColor: false; NeedsConstAlpha: true )
  );
  SourceToStr: array [boolean] of string = ('destination', 'source');
var
  I: Integer;
begin
  Result := false;

  S := LowerCase(S);

  for I := Low(BlendingFactors) to High(BlendingFactors) do
    if BlendingFactors[I].Name = S then
    begin
      if Source then
        Result := BlendingFactors[I].Source else
        Result := BlendingFactors[I].Dest;

      if Result then
      begin
        Factor := BlendingFactors[I].GL;

        { check is GL version enough, or some GL extensions available
          for more exotic factors. }

        if BlendingFactors[I].NeedsConstColor or
           BlendingFactors[I].NeedsConstAlpha then
        begin
          if (not (GL_ARB_imaging or GL_version_1_4)) or GLVersion.Fglrx then
          begin
            if Log then
              WritelnLog('Blending', Format('Blending factor "%s" requires OpenGL 1.4 or ARB_imaging extension, and is known to not work with fglrx (ATI Linux drivers)', [S]));
            Exit(false);
          end;
        end;

        if not GL_version_1_4 then
        begin
          if ((Factor = GL_SRC_COLOR) or
              (Factor = GL_ONE_MINUS_SRC_COLOR)) and Source then
          begin
            if Log then
              WritelnLog('Blending', Format('Blending factor "%s" as "source" requires OpenGL 1.4', [S]));
            Exit(false);
          end;

          if ((Factor = GL_DST_COLOR) or
              (Factor = GL_ONE_MINUS_DST_COLOR)) and not Source then
          begin
            if Log then
              WritelnLog('Blending', Format('Blending factor "%s" as "destination" requires OpenGL 1.4', [S]));
            Exit(false);
          end;
        end;

        NeedsConstColor := NeedsConstColor or BlendingFactors[I].NeedsConstColor;
        NeedsConstAlpha := NeedsConstAlpha or BlendingFactors[I].NeedsConstAlpha;
      end;

      Break;
    end;

  if not Result then
    OnWarning(wtMajor, 'VRML/X3D', Format('Unknown blending %s factor name "%s"',
      [ SourceToStr[Source], S ]));
end;

type
  TOcclusionQuery = class
  public
    constructor Create;
    destructor Destroy; override;

  public
    Id: TGLuint;

    Node: TShapeOctreeNode;

    function Available: LongBool;
    function GetResult: TGLuint;
  end;

constructor TOcclusionQuery.Create;
begin
  inherited;
  glGenQueriesARB(1, @Id);
end;

destructor TOcclusionQuery.Destroy;
begin
  glDeleteQueriesARB(1, @Id);
  inherited;
end;

function TOcclusionQuery.Available: LongBool;
begin
  Assert(SizeOf(LongBool) = SizeOf(TGLuint));
  glGetQueryObjectuivARB(Id, GL_QUERY_RESULT_AVAILABLE_ARB, @Result);
end;

function TOcclusionQuery.GetResult: TGLuint;
begin
  glGetQueryObjectuivARB(Id, GL_QUERY_RESULT_ARB, @Result);
end;

procedure TCastleScene.RenderScene(
  TestShapeVisibility: TTestShapeVisibility;
  const Frustum: TFrustum; const Params: TRenderParams);
var
  OcclusionBoxState: boolean;

  procedure OcclusionBoxStateBegin;
  begin
    if not OcclusionBoxState then
    begin
      glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or
        GL_ENABLE_BIT or GL_LIGHTING_BIT);

      glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE); { saved by GL_COLOR_BUFFER_BIT }
      glDepthMask(GL_FALSE); { saved by GL_DEPTH_BUFFER_BIT }

      { A lot of state should be disabled. Remember that this is done
        in the middle of TGLRenderer rendering, between
        RenderBegin/End, and TGLRenderer doesn't need to
        restore state after each shape render. So e.g. texturing
        and alpha test may be enabled, which could lead to very
        strange effects (box would be rendered with random texel,
        possibly alpha tested and rejected...).

        Also, some state should be disabled just to speed up
        rendering. E.g. lighting is totally not needed here. }

      glDisable(GL_LIGHTING); { saved by GL_ENABLE_BIT }
      glDisable(GL_CULL_FACE); { saved by GL_ENABLE_BIT }
      glDisable(GL_COLOR_MATERIAL); { saved by GL_ENABLE_BIT }
      glDisable(GL_ALPHA_TEST); { saved by GL_ENABLE_BIT }
      glDisable(GL_FOG); { saved by GL_ENABLE_BIT }
      glDisable(GL_TEXTURE_2D); { saved by GL_ENABLE_BIT }
      if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
      if GL3DTextures  <> gsNone then glDisable(GL_TEXTURE_3D);

      glShadeModel(GL_FLAT); { saved by GL_LIGHTING_BIT }

      glEnableClientState(GL_VERTEX_ARRAY);

      OcclusionBoxState := true;
    end;
  end;

  procedure OcclusionBoxStateEnd;
  begin
    if OcclusionBoxState then
    begin
      glDisableClientState(GL_VERTEX_ARRAY);
      glPopAttrib;
      OcclusionBoxState := false;
    end;
  end;

  { Call RenderShape if some tests succeed.
    It assumes that test with TestShapeVisibility is already done. }
  procedure RenderShape_SomeTests(Shape: TGLShape);

    procedure DoRenderShape;

      { Renders Shape, by calling Renderer.RenderShape. }
      procedure RenderShape(Shape: TGLShape);
      begin
        { Optionally free Shape arrays data now, if they need to be regenerated. }
        if (Assigned(Attributes.OnVertexColor) or
            Assigned(Attributes.OnRadianceTransfer)) and
           (Shape.Cache <> nil) then
          Shape.Cache.FreeArrays([vtAttribute]);

        Renderer.RenderShape(Shape, ShapeFog(Shape));
      end;

    begin
      OcclusionBoxStateEnd;

      if Params.Pass = 0 then Inc(Params.Statistics.ShapesRendered);
      RenderShape(Shape);
    end;

  var
    SampleCount: TGLuint;
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
         (RenderingCamera.Target = rtScreen) then
      begin
        Assert(Shape.OcclusionQueryId <> 0);
        if Shape.OcclusionQueryAsked then
          glGetQueryObjectuivARB(Shape.OcclusionQueryId, GL_QUERY_RESULT_ARB,
            @SampleCount) else
          SampleCount := 1; { if not asked, assume it's visible }

        { Do not do occlusion query (although still use results from previous
          query) if we're within stencil test (like in InShadow = false pass
          of shadow volumes). This would incorrectly mark some shapes
          as non-visible (just because they don't pass stencil test on any pixel),
          while in fact they should be visible in the very next
          (with InShadow = true) render pass. }

        if Params.StencilTest = 0 then
          glBeginQueryARB(GL_SAMPLES_PASSED_ARB, Shape.OcclusionQueryId);

          if SampleCount > 0 then
          begin
            DoRenderShape;
          end else
          begin
            { Object was not visible in the last frame.
              In this frame, only render it's bounding box, to test
              occlusion query. This is the speedup of using occlusion query:
              we render only bbox. }

            OcclusionBoxStateBegin;
            glDrawBox3DSimple(Shape.BoundingBox);
            if Params.Pass = 0 then Inc(Params.Statistics.BoxesOcclusionQueriedCount);
          end;

        if Params.StencilTest = 0 then
        begin
          glEndQueryARB(GL_SAMPLES_PASSED_ARB);
          Shape.OcclusionQueryAsked := true;
        end;
      end else
      if Attributes.DebugHierOcclusionQueryResults and
         Attributes.UseHierarchicalOcclusionQuery then
      begin
        if Shape.RenderedFrameId = FrameId then
          DoRenderShape;
      end else
        { No occlusion query-related stuff. Just render the shape. }
        DoRenderShape;
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
    if not TGLShape(Shape).UseBlending then RenderShape_AllTests(Shape);
  end;

  procedure RenderShape_AllTests_Transparent(Shape: TShape);
  begin
    if TGLShape(Shape).UseBlending then RenderShape_AllTests(Shape);
  end;

  procedure RenderAllAsOpaque;
  begin
    if not Params.Transparent then
      Shapes.Traverse(@RenderShape_AllTests, true, true);
  end;

  { Determine what blending source/destination factors to use for rendering Shape
    (looking at Attributes.BlendingXxx and Appearance.blendMode of VRML node).
    If different than currently set, then change BlendingXxxFactorSet and update
    by glBlendFunc. This way, we avoid calling glBlendFunc (which is potentially costly,
    since it changes GL state) too often. }
  procedure AdjustBlendFunc(Shape: TShape;
    var BlendingSourceFactorSet, BlendingDestinationFactorSet: TGLEnum);
  var
    B: TBlendModeNode;
    NewSrc, NewDest: TGLEnum;
    NeedsConstColor, NeedsConstAlpha: boolean;
  begin
    NeedsConstColor := false;
    NeedsConstAlpha := false;

    B := Shape.State.BlendMode;
    if B <> nil then
    begin
      if not BlendingFactorNameToStr(B.FdSrcFactor.Value, NewSrc, NeedsConstColor, NeedsConstAlpha, true) then
        NewSrc := Attributes.BlendingSourceFactor;
      if not BlendingFactorNameToStr(B.FdDestFactor.Value, NewDest, NeedsConstColor, NeedsConstAlpha, false) then
        NewDest := Attributes.BlendingDestinationFactor;
    end else
    begin
      NewSrc := Attributes.BlendingSourceFactor;
      NewDest := Attributes.BlendingDestinationFactor;
    end;

    if (BlendingSourceFactorSet <> NewSrc) or
       (BlendingDestinationFactorSet <> NewDest) then
    begin
      BlendingSourceFactorSet := NewSrc;
      BlendingDestinationFactorSet := NewDest;
      glBlendFunc(BlendingSourceFactorSet, BlendingDestinationFactorSet);
    end;

    { We track last source/dest factor, but we don't track last constant color/alpha.
      So just set them always, if needed. }
    if GL_ARB_imaging then
    begin
      if NeedsConstColor then
      begin
        Assert(B <> nil);
        glBlendColor(
          B.FdColor.Value[0],
          B.FdColor.Value[1],
          B.FdColor.Value[2],
          1 - B.FdColorTransparency.Value);
      end else
      if NeedsConstAlpha then
      begin
        Assert(B <> nil);
        glBlendColor(0, 0, 0, 1 - B.FdColorTransparency.Value);
      end;
    end;
  end;

  procedure DoHierarchicalOcclusionQuery;
  var
    { Stack of TShapeOctreeNode.

      Although queue would also work not so bad, stack is better.
      The idea is that it should try to keep front-to-back order,
      assuming that Node.PushChildren* keeps this order.
      Stack gives more chance to process front shapes first. }
    TraversalStack: TCastleObjectStack;

    procedure TraverseNode(Node: TShapeOctreeNode);
    var
      I: Integer;
      Shape: TGLShape;
    begin
      if Node.IsLeaf then
      begin
        { Render all shapes within this leaf, taking care to render
          shape only once within this frame (FrameId is useful here). }
        for I := 0 to Node.ItemsIndices.Count - 1 do
        begin
          Shape := TGLShape(OctreeRendering.ShapesList[Node.ItemsIndices.L[I]]);
          if Shape.RenderedFrameId <> FrameId then
          begin
            RenderShape_SomeTests(Shape);
            Shape.RenderedFrameId := FrameId;
          end;
        end;
      end else
      begin
        { Push Node children onto TraversalStack.
          We want to Pop them front-first, to (since this is a stack)
          we want to push back first. }
        if CameraViewKnown then
          Node.PushChildrenBackToFront(TraversalStack, CameraPosition) else
          Node.PushChildren(TraversalStack);
      end;
    end;

    procedure PullUpVisibility(Node: TShapeOctreeNode);
    begin
      while not Node.Visible do
      begin
        Node.Visible := true;
        Node := Node.ParentNode;
        if Node = nil then Break;
      end;
    end;

    procedure RenderLeafNodeVolume(Node: TShapeOctreeNode);
    var
      I: Integer;
      Shape: TGLShape;
      Box: TBox3D;
    begin
      OcclusionBoxStateBegin;

      { How to render bounding volume of leaf for occlusion query?

        - Simple version is just to render Node.Box. But this may be
          much greater than actual box of shapes inside, Box of our
          octree node is not adjusted to be tight.

        - Another version is to render boxes of all shapes within this leaf.
          This is much tighter than Node.Box, and results in much less
          shapes quialified as visible. (See e.g. bzwgen city view behind
          building 1 when trying to walk towards the city center.)
          Unfortunately, this produces really a lot of boxes, so the
          overhead of drawing glDrawBox3DSimple becomes large then.

        - Compromise: calculate tight bounding box here, and use it.
          Works best: number of both visible shapes and cull boxes
          is small.

        Note that we can render here boxes of only non-rendered shapes,
        that's Ok and may actually speed up. }
      Box := EmptyBox3D;

      for I := 0 to Node.ItemsIndices.Count - 1 do
      begin
        Shape := TGLShape(OctreeRendering.ShapesList[Node.ItemsIndices.L[I]]);
        if Shape.RenderedFrameId <> FrameId then
          Box.Add(Shape.BoundingBox);
      end;

      glDrawBox3DSimple(Box);
      if Params.Pass = 0 then Inc(Params.Statistics.BoxesOcclusionQueriedCount);
    end;

  const
    VisibilityThreshold = 0;
  { $define VISIBILITY_KEEP_FRAMES}
  {$ifdef VISIBILITY_KEEP_FRAMES}
    VisibilityKeepFrames = 10;
  {$endif}
  var
    { queue of TOcclusionQuery }
    QueryQueue: TCastleObjectQueue;
    Q: TOcclusionQuery;
    Node: TShapeOctreeNode;
    WasVisible, LeafOrWasInvisible: boolean;
  begin
    {$include norqcheckbegin.inc}
    Inc(FrameId);
    {$include norqcheckend.inc}

    TraversalStack := TCastleObjectStack.Create;
    TraversalStack.Capacity := OctreeRendering.ShapesList.Count;

    QueryQueue := TCastleObjectQueue.Create;
    QueryQueue.Capacity := OctreeRendering.ShapesList.Count;

    try
      TraversalStack.Push(OctreeRendering.TreeRoot);

      repeat
        if (QueryQueue.Count <> 0) and
           ( (TOcclusionQuery(QueryQueue.Peek).Available) or
             (TraversalStack.Count = 0) ) then
        begin
          Q := TOcclusionQuery(QueryQueue.Pop);
          if Q.GetResult > VisibilityThreshold then
          begin
            PullUpVisibility(Q.Node);
            TraverseNode(Q.Node);
          end;
          FreeAndNil(Q);
        end;

        if TraversalStack.Count <> 0 then
        begin
          Node := TShapeOctreeNode(TraversalStack.Pop);
          if Node.FrustumCollisionPossible(RenderFrustum_Frustum^) then
          begin
            {$ifdef VISIBILITY_KEEP_FRAMES}
            { There was a resigned idea below (maybe useful later) to do
              "or (Node.Depth >= 5)", to assume visible = true below some
              octree depth. }

            if (Node.Visible and (Node.LastVisitedFrameId >= FrameId - VisibilityKeepFrames)) then
            begin
              { Visible somewhere during VisibilityKeepFrames.
                Just assume it's still visible.
                (This is the optimization described in 6.6.4
                "Conservative Visibility Testing") }
              TraverseNode(Node);
            end else
            {$endif VISIBILITY_KEEP_FRAMES}
            begin
              WasVisible := Node.Visible and (Node.LastVisitedFrameId = FrameId - 1);
              LeafOrWasInvisible := (not WasVisible) or Node.IsLeaf;

              Node.Visible := false;
              Node.LastVisitedFrameId := FrameId;

              { Original logic goes like:

                  if LeafOrWasInvisible then
                    Add query with Node.Box;
                  if WasVisible then
                    TraverseNode(Node);

                But this is not optimal: it would always query using bounding
                boxes. Even for the case when we have a visible leaf,
                then the above version would query using box of this leaf
                and then render this leaf.
                But in this case we can query using actual geometry.

                So a modification is to do

                  if LeafOrWasInvisible then
                  begin
                    if Leaf and WasVisible then
                      Add query for Node and render the leaf else
                      Add query with Node.Box;
                  end else
                  if WasVisible then
                    TraverseNode(Node);

                This exhausts all possibilities, since if
                LeafOrWasInvisible and WasVisible then only leaf nodes
                could satisfy this.

                There's additional note about this:
                rendering inside TraverseNode may render
                only part of the leaf's items (or even none at all).
                This is needed (although in original paper they write
                about rendering single shape there, unline my many-shapes-in-leaf
                approach, but still they have to safeguard against rendering
                the same node many times, since visible leaf confirmed to
                be visible may be passed twice to Render).

                But this means that object may be classified as invisible
                (because it didn't have any unrendered shapes), while in fact
                it's visible. That's not a problem, since we check our
                query in the next frame, and the object will be found
                then visible again (or again invisible if other leafs
                will render it's shapes, but then it's not a problem). }

              if LeafOrWasInvisible then
              begin
                Q := TOcclusionQuery.Create;
                Q.Node := Node;

                glBeginQueryARB(GL_SAMPLES_PASSED_ARB, Q.Id);
                  if Node.IsLeaf and WasVisible then
                    TraverseNode(Node) else
                  if Node.IsLeaf then
                    { Leaf nodes have optimized version of rendering their
                      bounding volume for occlusion query. }
                    RenderLeafNodeVolume(Node) else
                  begin
                    OcclusionBoxStateBegin;
                    glDrawBox3DSimple(Node.Box);
                    if Params.Pass = 0 then Inc(Params.Statistics.BoxesOcclusionQueriedCount);
                  end;
                glEndQueryARB(GL_SAMPLES_PASSED_ARB);

                QueryQueue.Push(Q);
              end else
              if WasVisible then
                TraverseNode(Node);
            end;
          end;
        end;

      until (TraversalStack.Count = 0) and (QueryQueue.Count = 0);
    finally
      FreeAndNil(TraversalStack);
      FreeAndNil(QueryQueue);
    end;
  end;

  procedure UpdateVisibilitySensors;
  var
    I, J: Integer;
    Instances: TVisibilitySensorInstanceList;
    NewActive: boolean;
  begin
    { optimize for common case: exit early if nothing to do }
    if VisibilitySensors.Count = 0 then Exit;

    if ProcessEvents then
    begin
      BeginChangesSchedule;
      try
        for I := 0 to VisibilitySensors.Count - 1 do
          if VisibilitySensors.Keys[I].FdEnabled.Value then
          begin
            { increment timestamp for each VisibilitySensor,
              otherwise sensors_environmental/visibility_sensor.x3dv
              has a problem at initialization, when multiple sensors
              send isActive = TRUE, and X3D mechanism to avoid loops
              kicks in. }
            IncreaseTimeTick;
            { calculate NewActive }
            NewActive := false;
            Instances := VisibilitySensors.Data[I];
            for J := 0 to Instances.Count - 1 do
              if Frustum.Box3DCollisionPossibleSimple(Instances[J].Box) then
              begin
                NewActive := true;
                Break;
              end;
            VisibilitySensors.Keys[I].SetIsActive(NewActive, Time);
          end;
      finally EndChangesSchedule; end;
    end;
  end;

var
  BlendingSourceFactorSet, BlendingDestinationFactorSet: TGLEnum;
  I: Integer;
  LightRenderEvent: TVRMLLightRenderEvent;
begin
  { We update ShapesVisible only for one value of Params.Transparent.
    Otherwise, we would increase it twice.
    This method is always called first with Params.Transparent = false,
    then Params.Transparent = true during a single frame. }
  if (not Params.Transparent) and (Params.Pass = 0) then
  begin
    Params.Statistics.ShapesVisible += ShapesActiveVisibleCount;
    { also do this only once per frame }
    UpdateVisibilitySensors;
  end;

  OcclusionBoxState := false;

  if Params.InShadow then
    LightRenderEvent := @LightRenderInShadow else
    LightRenderEvent := nil;

  if not Params.RenderTransformIdentity then
  begin
    glPushMatrix;
    glMultMatrix(Params.RenderTransform);
  end;

  Renderer.RenderBegin(Params.BaseLights(Self) as TLightInstancesList,
    LightRenderEvent, Params.Pass);
  try
    if Attributes.Mode <> rmFull then
    begin
      { When not rmFull, we don't want to do anything with glDepthMask
        or GL_BLEND enable state. Just render everything. }
      RenderAllAsOpaque;

      { Each RenderShape_SomeTests inside could set OcclusionBoxState }
      OcclusionBoxStateEnd;
    end else
    if Attributes.ReallyUseHierarchicalOcclusionQuery and
       (not Attributes.DebugHierOcclusionQueryResults) and
       (RenderingCamera.Target = rtScreen) and
       (OctreeRendering <> nil) then
    begin
      DoHierarchicalOcclusionQuery;

      { Inside we could set OcclusionBoxState }
      OcclusionBoxStateEnd;
    end else
    begin
      glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      try
        if Attributes.ControlBlending and Attributes.Blending then
        begin
          if not Params.Transparent then
          begin
            { draw fully opaque objects }

            glDepthMask(GL_TRUE);
            glDisable(GL_BLEND);

            if CameraViewKnown and Attributes.ReallyUseOcclusionQuery then
            begin
              ShapesFilterBlending(Shapes, true, true, false,
                TestShapeVisibility, FilteredShapes, false);

              { ShapesSplitBlending already filtered shapes through
                TestShapeVisibility callback, so later we can render them
                with RenderShape_SomeTests to skip checking TestShapeVisibility
                twice. This is a good thing: it means that sorting below has
                much less shapes to consider. }
              FilteredShapes.SortFrontToBack(CameraPosition);

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

            { Set glBlendFunc using Attributes.BlendingXxxFactor }
            BlendingSourceFactorSet := Attributes.BlendingSourceFactor;
            BlendingDestinationFactorSet := Attributes.BlendingDestinationFactor;
            glBlendFunc(BlendingSourceFactorSet, BlendingDestinationFactorSet);

            if CameraViewKnown and Attributes.BlendingSort then
            begin
              ShapesFilterBlending(Shapes, true, true, false,
                TestShapeVisibility, FilteredShapes, true);
              FilteredShapes.SortBackToFront(CameraPosition);
              for I := 0 to FilteredShapes.Count - 1 do
              begin
                AdjustBlendFunc(TGLShape(FilteredShapes[I]),
                  BlendingSourceFactorSet, BlendingDestinationFactorSet);
                RenderShape_SomeTests(TGLShape(FilteredShapes[I]));
              end;
            end else
              Shapes.Traverse(@RenderShape_AllTests_Transparent, true, true, false);
          end;
        end else
        begin
          if Attributes.ControlBlending then
          begin
            glDepthMask(GL_TRUE);
            glDisable(GL_BLEND);
          end;

          RenderAllAsOpaque;
        end;

        { Each RenderShape_SomeTests inside could set OcclusionBoxState.
          Finish it now, before following glPopAttrib. }
        OcclusionBoxStateEnd;
      finally glPopAttrib end;
    end;
  finally Renderer.RenderEnd end;

  if not Params.RenderTransformIdentity then
    glPopMatrix;
end;

procedure TCastleScene.PrepareResources(
  Options: TPrepareResourcesOptions; ProgressStep: boolean;
  BaseLights: TAbstractLightInstancesList);

  procedure PrepareShapesResouces;
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
  begin
    if Log and LogRenderer then
      WritelnLog('Renderer', 'Preparing rendering of all shapes');

    { Note: we prepare also not visible shapes, in case they become visible. }
    SI := TShapeTreeIterator.Create(Shapes, false, false);
    try
      Inc(Renderer.PrepareRenderShape);
      try
        Renderer.RenderBegin(BaseLights as TLightInstancesList, nil, 0);
        while SI.GetNext do
        begin
          Shape := TGLShape(SI.Current);
          Renderer.RenderShape(Shape, ShapeFog(Shape));
        end;
        Renderer.RenderEnd;
      finally Dec(Renderer.PrepareRenderShape) end;
    finally FreeAndNil(SI) end;
  end;

var
  I: Integer;
begin
  inherited;

  if Dirty <> 0 then Exit;

  if not PreparedShapesResouces then
  begin
    { Use PreparedShapesResouces to avoid expensive (for large scenes)
      iteration over all shapes in every TCastleScene.PrepareResources call. }
    PreparedShapesResouces := true;
    PrepareShapesResouces;
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
end;

procedure TCastleScene.Render(
  TestShapeVisibility: TTestShapeVisibility;
  const Frustum: TFrustum; const Params: TRenderParams);

  procedure RenderNormal;
  begin
    RenderScene(TestShapeVisibility, Frustum, Params);
  end;

  procedure RenderWireframe(UseWireframeColor: boolean);
  var
    SavedMode: TRenderingMode;
  begin
    glPushAttrib(GL_POLYGON_BIT or GL_CURRENT_BIT or GL_ENABLE_BIT);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); { saved by GL_POLYGON_BIT }

      if UseWireframeColor then
      begin
        glColorv(Attributes.WireframeColor); { saved by GL_CURRENT_BIT }
        glDisable(GL_TEXTURE_2D); { saved by GL_CURRENT_BIT }
        glDisable(GL_LIGHTING); { saved by GL_CURRENT_BIT }
        SavedMode := Attributes.Mode;
        Attributes.Mode := rmPureGeometry;
      end;

      RenderNormal;

      if UseWireframeColor then
        Attributes.Mode := SavedMode;
    glPopAttrib;
  end;

  { Render taking Attributes.WireframeEffect into account. }
  procedure RenderWithWireframeEffect;
  begin
    case Attributes.WireframeEffect of
      weNormal: RenderNormal;
      weWireframeOnly: RenderWireframe(Attributes.Mode = rmPureGeometry);
      weSolidWireframe:
        begin
          glPushAttrib(GL_POLYGON_BIT);
            { enable polygon offset for everything (whole scene) }
            glEnable(GL_POLYGON_OFFSET_FILL); { saved by GL_POLYGON_BIT }
            glEnable(GL_POLYGON_OFFSET_LINE); { saved by GL_POLYGON_BIT }
            glEnable(GL_POLYGON_OFFSET_POINT); { saved by GL_POLYGON_BIT }
            glPolygonOffset(1, 1); { saved by GL_POLYGON_BIT }
            RenderNormal;
          glPopAttrib;
          RenderWireframe(true);
        end;
      weSilhouette:
        begin
          RenderNormal;
          glPushAttrib(GL_POLYGON_BIT);
            glEnable(GL_POLYGON_OFFSET_LINE); { saved by GL_POLYGON_BIT }
            glPolygonOffset(5, 5); { saved by GL_POLYGON_BIT }
            { rmPureGeometry still does backface culling.
              This is very good in this case. When rmPureGeometry and weSilhouette,
              and objects are solid (so backface culling is used) we can
              significantly improve the effect by reverting glFrontFace,
              this way we will cull *front* faces. This will not be noticed
              in case of rmPureGeometry will single solid color, and it will
              improve the silhouette look, since front-face edges will not be
              rendered at all (no need to even hide them by glPolygonOffset,
              which is somewhat sloppy). }
            if Attributes.Mode = rmPureGeometry then
              glFrontFace(GL_CW); { saved by GL_POLYGON_BIT }
            RenderWireframe(true);
          glPopAttrib;
        end;
      else raise EInternalError.Create('Render: Attributes.WireframeEffect ?');
    end;
  end;

  { Render, doing some special tricks when rendering to shadow maps. }
  procedure RenderWithShadowMaps;
  var
    SavedMode: TRenderingMode;
    SavedCustomShader, SavedCustomShaderAlphaTest: TGLSLProgram;
  begin
    { For shadow maps, speed up rendering by using only features that affect
      depth output. This also disables user shaders (for both classic
      and VSM shadow maps, consistently). }
    if RenderingCamera.Target in [rtVarianceShadowMap, rtShadowMap] then
    begin
      SavedMode := Attributes.Mode;
      Attributes.Mode := rmDepth;
    end;

    { When rendering to Variance Shadow Map, we need special shader. }
    if RenderingCamera.Target = rtVarianceShadowMap then
    begin
      { create VarianceShadowMapsProgram if needed }
      if VarianceShadowMapsProgram[false] = nil then
      begin
        VarianceShadowMapsProgram[false] := TGLSLProgram.Create;
        VarianceShadowMapsProgram[false].AttachFragmentShader({$I variance_shadow_map_generate.fs.inc});
        VarianceShadowMapsProgram[false].Link(true);
      end;

      if VarianceShadowMapsProgram[true] = nil then
      begin
        VarianceShadowMapsProgram[true] := TGLSLProgram.Create;
        VarianceShadowMapsProgram[true].AttachFragmentShader(
          '#define ALPHA_TEST' + NL + {$I variance_shadow_map_generate.fs.inc});
        VarianceShadowMapsProgram[true].Link(true);
      end;

      SavedCustomShader          := Attributes.CustomShader;
      SavedCustomShaderAlphaTest := Attributes.CustomShaderAlphaTest;
      Attributes.CustomShader          := VarianceShadowMapsProgram[false];
      Attributes.CustomShaderAlphaTest := VarianceShadowMapsProgram[true];
    end;

    RenderWithWireframeEffect;

    if RenderingCamera.Target in [rtVarianceShadowMap, rtShadowMap] then
      Attributes.Mode := SavedMode;
    if RenderingCamera.Target = rtVarianceShadowMap then
    begin
      Attributes.CustomShader          := SavedCustomShader;
      Attributes.CustomShaderAlphaTest := SavedCustomShaderAlphaTest;
    end;
  end;

begin
  { This is usually called by Render(Frustum, Params) that probably
    already did tests below. But it may also be called directly,
    so do the checks below anyway. (The checks are trivial, so no speed harm.) }
  if GetExists and (Dirty = 0) and
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
    PrepareResources([prRender], false, Params.BaseLights(Self));

    RenderWithShadowMaps;
  end;
end;

class procedure TCastleScene.LightRenderInShadow(const Light: TLightInstance;
  var LightOn: boolean);
begin
  if Light.Node.FdKambiShadows.Value then
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

{ shadow quads --------------------------------------------------------------- }

{ This returns vertex Original extruded into infinity, as seen from light
  at position LightPos.

  This is designed to work only with LightPos[3] = 1. In the future, when
  need arises, this may be improved to work with any LightPos[3] <> 0.

  For LightPos[3] = 0, i.e. directional light,
  don't use this, and there's no need to do it,
  since then the extruded point is just LightPos (for any vertex).
  RenderXxxShadowVolume want to treat it specially anyway (to optimize
  drawing, since then quads degenerate to triangles). }
function ExtrudeVertex(
  const Original: TVector3Single;
  const LightPos: TVector4Single): TVector4Single;
var
  LightPos3: TVector3Single absolute LightPos;
begin
  { Below is the moment when we require that
    if LightPos[3] <> 0 then LightPos[3] = 1 (not any other non-zero value).
    Otherwise we would have to divide here LightPos3 by LightPos[3].
    Maybe in the future this requirement will be removed and we'll work
    for any LightPos in homogeneous coordinates, for now it's not really
    needed. }
  Result[0] := Original[0] -  LightPos3[0];
  Result[1] := Original[1] -  LightPos3[1];
  Result[2] := Original[2] -  LightPos3[2];
  Result[3] := 0;
end;

procedure TCastleScene.RenderAllShadowVolume(
  const LightPos: TVector4Single;
  const TransformIsIdentity: boolean;
  const Transform: TMatrix4Single;
  LightCap, DarkCap: boolean);
var
  TrianglesForLightCap: TTriangle3SingleList;
  TrianglesForDarkCap: TTriangle4SingleList;

  procedure RenderShadowQuad(
    const P0, P1: TVector3Single;
    const PExtruded0, PExtruded1: TVector4Single); overload;
  begin
    //glNormalv(TriangleNormal(P0, P1, PExtruded1));
    glVertexv(P0);
    glVertexv(P1);
    glVertexv(PExtruded1);
    glVertexv(PExtruded0);
  end;

  procedure RenderShadowQuad(
    const P0, P1: TVector3Single;
    const PExtruded: TVector4Single); overload;
  begin
    glVertexv(P0);
    glVertexv(P1);
    glVertexv(PExtruded);
  end;

  procedure HandleTriangle(const T: TTriangle3Single);
  var
    TExtruded: TTriangle4Single;
    Plane: TVector4Single;
    PlaneSide: Single;
  begin
    { We want to have consistent CCW orientation of shadow quads faces,
      so that face is oriented CCW <=> you're looking at it from outside
      (i.e. it's considered front face of this shadow quad).
      This is needed, since user of this method may want to do culling
      to eliminate back or front faces.

      If TriangleDir(T) indicates direction that goes from CCW triangle side.
      If TriangleDir(T) points in the same direction as LightPos then
      1st quad should be T1, T0, TExtruded0, TExtruded1.
      If TriangleDir(T) points in the opposite direction as LightPos then
      1st quad should be T0, T1, TExtruded1, TExtruded0.
      And so on.

      Note that this works for any LightPos[3].
      - For LightPos[3] = 1 this is  normal check.
      - For other LightPos[3] > 0 this is equivalent to normal check.
      - For LightPos[3] = 0, this calculates dot between light direction
        and plane direction. Plane direction points outwards, so PlaneSide > 0
        indicates that light is from the outside. So it matches results for
        LightPos[3] = 1.
      - For LightPos[3] < 0, is seems that the test has to be reversed !
        I.e. add "if LightPos[3] < 0 then PlaneSide := -PlaneSide;".
        This will be done when we'll have to do accept any homogeneous
        coords for LightPos, right now it's not needed.
    }
    Plane := TrianglePlane(T);
    PlaneSide := Plane[0] * LightPos[0] +
                 Plane[1] * LightPos[1] +
                 Plane[2] * LightPos[2] +
                 Plane[3] * LightPos[3];

    { Don't render quads on caps if LightPos lies on the Plane
      (which means that PlaneSide = 0) }
    if PlaneSide = 0 then
      Exit;

    if LightPos[3] <> 0 then
    begin
      TExtruded[0] := ExtrudeVertex(T[0], LightPos);
      TExtruded[1] := ExtrudeVertex(T[1], LightPos);
      TExtruded[2] := ExtrudeVertex(T[2], LightPos);

      if PlaneSide > 0 then
      begin
        RenderShadowQuad(T[1], T[0], TExtruded[1], TExtruded[0]);
        RenderShadowQuad(T[0], T[2], TExtruded[0], TExtruded[2]);
        RenderShadowQuad(T[2], T[1], TExtruded[2], TExtruded[1]);
      end else
      begin
        RenderShadowQuad(T[0], T[1], TExtruded[0], TExtruded[1]);
        RenderShadowQuad(T[1], T[2], TExtruded[1], TExtruded[2]);
        RenderShadowQuad(T[2], T[0], TExtruded[2], TExtruded[0]);
      end;

      if DarkCap then
      begin
        { reverse TExtruded dir, we want to render caps CCW outside always.

          Note that the test for reversing here is "PlaneSide > 0", while
          test for reversing LightCaps is "PlaneSide < 0": that's as it should
          be, as DarkCap triangle should always be in reversed direction
          than corresponding LightCap triangle (since they both should be
          CCW outside). }
        if PlaneSide > 0 then
          SwapValues(TExtruded[0], TExtruded[2]);
        TrianglesForDarkCap.Add(TExtruded);
      end;
    end else
    begin
      { For directional lights, this gets a little simpler, since
        all extruded points are the same and equal just LightPos. }
      if PlaneSide > 0 then
      begin
        RenderShadowQuad(T[1], T[0], LightPos);
        RenderShadowQuad(T[0], T[2], LightPos);
        RenderShadowQuad(T[2], T[1], LightPos);
      end else
      begin
        RenderShadowQuad(T[0], T[1], LightPos);
        RenderShadowQuad(T[1], T[2], LightPos);
        RenderShadowQuad(T[2], T[0], LightPos);
      end;
    end;

    if LightCap then
    begin
      { reverse T dir, we want to render caps CCW outside always }
      if PlaneSide < 0 then
        TrianglesForLightCap.Add(Triangle3Single(T[2], T[1], T[0])) else
        TrianglesForLightCap.Add(T);
    end;
  end;

  procedure RenderTriangle3Single(const T: TTriangle3Single);
  begin
    glVertexv(T[0]);
    glVertexv(T[1]);
    glVertexv(T[2]);
  end;

  procedure RenderTriangle4Single(const T: TTriangle4Single);
  begin
    glVertexv(T[0]);
    glVertexv(T[1]);
    glVertexv(T[2]);
  end;

var
  I: Integer;
  Triangles: TTriangle3SingleList;
  TransformedTri: TTriangle3Single;
  TPtr: PTriangle3Single;
  T4Ptr: PTriangle4Single;
begin
  TrianglesForLightCap := nil;
  TrianglesForDarkCap := nil;

  { Note that we require that all triangles on Triangles list are valid
    (have non-zero area). That's Ok, TrianglesListShadowCasters guarantees it.
    Otherwise, degenerate triangles could cause artifacts --- image a degenerate
    triangle on the silhouette edge, it will cause two shadow quads (with all
    it's neighboring triangles) where there should be one. }
  Triangles := TrianglesListShadowCasters;

  { If light is directional, no need to render dark cap }
  DarkCap := DarkCap and (LightPos[3] <> 0);

  { It's a not nice that we have to create a structure in memory
    to hold TrianglesForLight/DarkCap. But that's because they have to be rendered
    after rendering normal shadow quads (because shadow quads may be
    quads or triangles, caps are only triangles, and are rendered in
    glDepthFunc(GL_NEVER) mode. }

  if LightCap then
  begin
    TrianglesForLightCap := TTriangle3SingleList.Create;
    TrianglesForLightCap.Capacity := Triangles.Count;
  end;

  if DarkCap then
  begin
    TrianglesForDarkCap := TTriangle4SingleList.Create;
    TrianglesForDarkCap.Capacity := Triangles.Count;
  end;

  try

    if LightPos[3] <> 0 then
      glBegin(GL_QUADS) else
      glBegin(GL_TRIANGLES);

    TPtr := PTriangle3Single(Triangles.List);

    if TransformIsIdentity then
    begin
      for I := 0 to Triangles.Count - 1 do
      begin
        HandleTriangle(TPtr^);
        Inc(TPtr);
      end;
    end else
    begin
      for I := 0 to Triangles.Count - 1 do
      begin
        { calculate TransformedTri := Triangles[I] transformed by Transform }
        TransformedTri[0] := MatrixMultPoint(Transform, TPtr^[0]);
        TransformedTri[1] := MatrixMultPoint(Transform, TPtr^[1]);
        TransformedTri[2] := MatrixMultPoint(Transform, TPtr^[2]);

        HandleTriangle(TransformedTri);
        Inc(TPtr);
      end;
    end;

    glEnd;

    if LightCap or DarkCap then
    begin
      { See RenderSilhouetteShadowVolume for explanation why caps
        should be rendered with glDepthFunc(GL_NEVER). }
      glPushAttrib(GL_DEPTH_BUFFER_BIT); { to save glDepthFunc call below }
      glDepthFunc(GL_NEVER);
      glBegin(GL_TRIANGLES);

      if LightCap then
      begin
        TPtr := PTriangle3Single(TrianglesForLightCap.List);
        for I := 0 to TrianglesForLightCap.Count - 1 do
        begin
          RenderTriangle3Single(TPtr^);
          Inc(TPtr);
        end;
      end;

      if DarkCap then
      begin
        T4Ptr := PTriangle4Single(TrianglesForDarkCap.List);
        for I := 0 to TrianglesForDarkCap.Count - 1 do
        begin
          RenderTriangle4Single(T4Ptr^);
          Inc(T4Ptr);
        end;
      end;

      glEnd;
      glPopAttrib;
    end;
  finally
    FreeAndNil(TrianglesForLightCap);
    FreeAndNil(TrianglesForDarkCap);
  end;
end;

procedure TCastleScene.RenderSilhouetteShadowVolume(
  const LightPos: TVector4Single;
  const TransformIsIdentity: boolean;
  const Transform: TMatrix4Single;
  const LightCap, DarkCap: boolean);

{ Is it worth preparing ManifoldEdges list: yes.

  At the beginning we used here the simple algorithm from
  [http://www.gamedev.net/reference/articles/article1873.asp].
  For each triangle with dot > 0, add it to the Edges list
  --- unless it's already there, in which case remove it.
  This way, at the end Edges contain all edges that have on one
  side triangle with dot > 0 and on the other side triangle with dot <= 0.
  In other words, all sihouette edges.
  (This is all assuming that model is 2-manifold,
  so each edge has exactly 2 neighbor triangles).

  But this algorithm proved to be unacceptably slow for many cases.
  While it generated much less shadow quads than naive
  RenderAllShadowVolume, the time spent in detecting the silhouette edges
  made the total time even worse than RenderAllShadowVolume.
  Obviously, that's because we started from the list of triangles,
  without any explicit information about the edges.
  The time of this algorithm was n*m, if n is the number of triangles
  and m the number of edges, and on 2-manifold n*3/2 = m so
  the time is n^2. Terrible, if you take complicated shadow caster.

  To make this faster, we have to know the connections inside the model:
  that's what ManifoldEdges list is all about. It allows us to
  implement this in time proportional to the number of edges.
}

var
  Triangles: TTrianglesShadowCastersList;

  procedure RenderShadowQuad(EdgePtr: PManifoldEdge;
    const P0Index, P1Index: Cardinal); overload;
  var
    V0, V1: TVector3Single;
    EdgeV0, EdgeV1: PVector3Single;
    TrianglePtr: PTriangle3Single;
  begin
    TrianglePtr := Addr(Triangles.L[EdgePtr^.Triangles[0]]);
    EdgeV0 := @TrianglePtr^[(EdgePtr^.VertexIndex + P0Index) mod 3];
    EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + P1Index) mod 3];

    if TransformIsIdentity then
    begin
      V0 := EdgeV0^;
      V1 := EdgeV1^;
    end else
    begin
      V0 := MatrixMultPoint(Transform, EdgeV0^);
      V1 := MatrixMultPoint(Transform, EdgeV1^);
    end;

    glVertexv(V0);
    glVertexv(V1);

    if LightPos[3] <> 0 then
    begin
      glVertexv(ExtrudeVertex(V1, LightPos));
      glVertexv(ExtrudeVertex(V0, LightPos));
    end else
      glVertexv(LightPos);
  end;

  procedure RenderShadowQuad(EdgePtr: PBorderEdge;
    const P0Index, P1Index: Cardinal); overload;
  var
    V0, V1: TVector3Single;
    EdgeV0, EdgeV1: PVector3Single;
    TrianglePtr: PTriangle3Single;
  begin
    TrianglePtr := Addr(Triangles.L[EdgePtr^.TriangleIndex]);
    EdgeV0 := @TrianglePtr^[(EdgePtr^.VertexIndex + P0Index) mod 3];
    EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + P1Index) mod 3];

    if TransformIsIdentity then
    begin
      V0 := EdgeV0^;
      V1 := EdgeV1^;
    end else
    begin
      V0 := MatrixMultPoint(Transform, EdgeV0^);
      V1 := MatrixMultPoint(Transform, EdgeV1^);
    end;

    glVertexv(V0);
    glVertexv(V1);
    if LightPos[3] <> 0 then
    begin
      glVertexv(ExtrudeVertex(V1, LightPos));
      glVertexv(ExtrudeVertex(V0, LightPos));
    end else
      glVertexv(LightPos);
  end;

  { We initialize TrianglesPlaneSide and render caps in one step,
    this way we have to iterate over Triangles only once, and in case
    of PlaneSide_NotIdentity and rendering caps --- we have to transform
    each triangle only once. }
  procedure InitializeTrianglesPlaneSideAndRenderCaps(
    TrianglesPlaneSide: TBooleanList;
    LightCap, DarkCap: boolean);

    procedure RenderCaps(const T: TTriangle3Single);
    begin
      if LightCap then
      begin
        glVertexv(T[0]);
        glVertexv(T[1]);
        glVertexv(T[2]);
      end;

      if DarkCap then
      begin
        glVertexv(ExtrudeVertex(T[2], LightPos));
        glVertexv(ExtrudeVertex(T[1], LightPos));
        glVertexv(ExtrudeVertex(T[0], LightPos));
      end;
    end;

    function PlaneSide_Identity(const T: TTriangle3Single): boolean;
    var
      Plane: TVector4Single;
    begin
      Plane := TrianglePlane(T);
      Result := (Plane[0] * LightPos[0] +
                 Plane[1] * LightPos[1] +
                 Plane[2] * LightPos[2] +
                 Plane[3] * LightPos[3]) > 0;
      if Result then RenderCaps(T);
    end;

    function PlaneSide_NotIdentity(const T: TTriangle3Single): boolean;
    var
      Plane: TVector4Single;
      TriangleTransformed: TTriangle3Single;
    begin
      TriangleTransformed[0] := MatrixMultPoint(Transform, T[0]);
      TriangleTransformed[1] := MatrixMultPoint(Transform, T[1]);
      TriangleTransformed[2] := MatrixMultPoint(Transform, T[2]);
      Plane := TrianglePlane(TriangleTransformed);
      Result := (Plane[0] * LightPos[0] +
                 Plane[1] * LightPos[1] +
                 Plane[2] * LightPos[2] +
                 Plane[3] * LightPos[3]) > 0;
      if Result then RenderCaps(TriangleTransformed);
    end;

    { Comments for Opaque/TransparentTrianglesBegin/End:

      It's crucial to set glDepthFunc(GL_NEVER) for LightCap.
      This way we get proper self-shadowing. Otherwise, LightCap would
      collide in z buffer with the object itself.

      Setting glDepthFunc(GL_NEVER) for DarkCap also is harmless and OK.
      Proof: if there's anything on this pixel, then indeed the depth test
      would fail. If the pixel is empty (nothing was rasterized there),
      then the depth test wouldn't fail... but also, in this case value in
      stencil buffer will not matter, it doesn't matter if this pixel
      is in shadow or not because there's simply nothing there.

      And it allows us to render both LightCap and DarkCap in one
      GL_TRIANGLES pass, in one iteration over Triangles list, which is
      good for speed.

      Some papers propose other solution:
        glEnable(GL_POLYGON_OFFSET_FILL);
        glPolygonOffset(1, 1);
      but this is no good for use, because it cannot be applied
      to DarkCap (otherwise DarkCap in infinity (as done by ExtrudeVertex)
      would go outside of depth range (even for infinite projection,
      as glPolygonOffset works already after the vertex is transformed
      by projection), and this would make DarkCap not rendered
      (outside of depth range)).

      If you consider that some shadow casters and receivers may
      be partially transparent (that is, rendered without writing
      to depth buffer) then the above reasoning is not so simple:

      - There's no way to handle transparent
        objects (that are not recorded in depth buffer) as shadow receivers.
        Rendering them twice with blending would result in wrong blending
        modes applied anyway. So TGLShadowVolumeRenderer.Render renders them
        at the end, as last pass.

        This means that "glDepthFunc(GL_NEVER) for DarkCap" is still
        Ok: if on some pixel there was only transparent object visible,
        then stencil value of this pixel is wrong, but transparent object
        will never be rendered in shadowed state --- so it will not
        look at stencil value.

        For LightCap, situation is worse. Even if the transparent
        object is only shadow caster (not receiver), still problems
        may arise due to glDepthFunc(GL_NEVER): imagine you have
        a transparent object casting shadow on non-transparent object
        (see e.g. demo_models/shadow_volumes/ghost_shadow.wrl).
        This means that you can look through the shadow casting
        (transp) object and see shadow receiving (opaque) object,
        that may or may not be in shadow on speciic pixel.
        Which means that glDepthFunc(GL_NEVER) is wrong for LightCap:
        the transparent object doesn't hide the shadow on the screen,
        and the depth test shouldn't fail. Which means that for transparent
        objects, we cannot do glDepthFunc(GL_NEVER).

      - What to do?

        The trick
          glEnable(GL_POLYGON_OFFSET_FILL);
          glPolygonOffset(1, 1);
        makes light cap rendering working for both transparent and opaque
        objects, but it's not applicable to dark cap. Moreover,
        using glPolygonOffset always feels dirty.

        Solution: we decide to handle transparent objects separately.
        We note that for transparent shadow casters
        actually no tweaks to caps rendering should be done.
        No glPolygonOffset, no glDepthFunc(GL_NEVER) needed: light cap
        should be tested as usual. (Since transparent object is not written
        to depth buffer, it will not collide in depth buffer with it's
        light cap).

        This means that is we'll just split triangles list into
        transparent and opaque ones, then the only complication needed
        is to switch glDepthFunc(GL_NEVER) trick *off* for transparent
        triangles. And all works fast.

      - There's actually one more note: for transparent objects,
        caps are always needed (even with zpass).
        Note that this means that whole 2-manifold part must have
        caps.

        This also means that joining one 2-manifold path from some transparent
        and some opaque triangles will not work. (as then some parts
        may have caps (like transparent ones) and some note
        (like opaque ones with zpass)).

        TODO: implement above. We'll need triangles sorted by transparency,
        with some marker TrianglesOpaqueCount.
    }

    procedure OpaqueTrianglesBegin;
    begin
      if LightCap or DarkCap then
      begin
        glPushAttrib(GL_DEPTH_BUFFER_BIT); { to save glDepthFunc call below }
        glDepthFunc(GL_NEVER);
        glBegin(GL_TRIANGLES);
      end;
    end;

    procedure OpaqueTrianglesEnd;
    begin
      if LightCap or DarkCap then
      begin
        glEnd;
        glPopAttrib;
      end;
    end;

    procedure TransparentTrianglesBegin;
    begin
      { Caps are always needed, doesn't depend on zpass/zfail.
        Well, for dark cap we can avoid them if the light is directional. }
      LightCap := true;
      DarkCap := LightPos[3] <> 0;

      glBegin(GL_TRIANGLES);
    end;

    procedure TransparentTrianglesEnd;
    begin
      glEnd;
    end;

  var
    TrianglePtr: PTriangle3Single;
    I: Integer;
    OpaqueCount: Cardinal;
  begin
    TrianglesPlaneSide.Count := Triangles.Count;
    TrianglePtr := PTriangle3Single(Triangles.List);

    { If light is directional, no need to render dark cap }
    DarkCap := DarkCap and (LightPos[3] <> 0);

    if Attributes.ControlBlending and
       Attributes.Blending and
       (Attributes.Mode = rmFull) then
      OpaqueCount := Triangles.OpaqueCount else
      OpaqueCount := Triangles.Count; { everything is opaque in this case }

    if TransformIsIdentity then
    begin
      OpaqueTrianglesBegin;
      for I := 0 to Integer(OpaqueCount) - 1 do
      begin
        TrianglesPlaneSide.L[I] := PlaneSide_Identity(TrianglePtr^);
        Inc(TrianglePtr);
      end;
      OpaqueTrianglesEnd;

      TransparentTrianglesBegin;
      for I := OpaqueCount to Triangles.Count - 1 do
      begin
        TrianglesPlaneSide.L[I] := PlaneSide_Identity(TrianglePtr^);
        Inc(TrianglePtr);
      end;
      TransparentTrianglesEnd;
    end else
    begin
      OpaqueTrianglesBegin;
      for I := 0 to Integer(OpaqueCount) - 1 do
      begin
        TrianglesPlaneSide.L[I] := PlaneSide_NotIdentity(TrianglePtr^);
        Inc(TrianglePtr);
      end;
      OpaqueTrianglesEnd;

      TransparentTrianglesBegin;
      for I := OpaqueCount to Triangles.Count - 1 do
      begin
        TrianglesPlaneSide.L[I] := PlaneSide_NotIdentity(TrianglePtr^);
        Inc(TrianglePtr);
      end;
      TransparentTrianglesEnd;
    end;
  end;

var
  I: Integer;
  PlaneSide0, PlaneSide1: boolean;
  TrianglesPlaneSide: TBooleanList;
  ManifoldEdgesNow: TManifoldEdgeList;
  ManifoldEdgePtr: PManifoldEdge;
  BorderEdgesNow: TBorderEdgeList;
  BorderEdgePtr: PBorderEdge;
begin
  Assert(ManifoldEdges <> nil);

  { if the model is not perfect 2-manifold, do not render it's shadow volumes.
    We still have here some code to handle BorderEdges, but in practice:
    this just has no chance to work 100% reliably with BorderEdges.
    See demo_models/shadow_volumes/not_manifold/README.txt }
  if BorderEdges.Count <> 0 then Exit;

  Triangles := TrianglesListShadowCasters;

  TrianglesPlaneSide := TBooleanList.Create;
  try
    InitializeTrianglesPlaneSideAndRenderCaps(TrianglesPlaneSide,
      LightCap, DarkCap);

    if LightPos[3] <> 0 then
      glBegin(GL_QUADS) else
      glBegin(GL_TRIANGLES);

      { for each 2-manifold edge, possibly render it's shadow quad }
      ManifoldEdgesNow := ManifoldEdges;
      ManifoldEdgePtr := PManifoldEdge(ManifoldEdgesNow.List);
      for I := 0 to ManifoldEdgesNow.Count - 1 do
      begin
        PlaneSide0 := TrianglesPlaneSide.L[ManifoldEdgePtr^.Triangles[0]];
        PlaneSide1 := TrianglesPlaneSide.L[ManifoldEdgePtr^.Triangles[1]];

        { Only if PlaneSide0 <> PlaneSide1 it's a silhouette edge,
          so only then render it's shadow quad.

          We want to have consistent CCW orientation of shadow quads faces,
          so that face is oriented CCW <=> you're looking at it from outside
          (i.e. it's considered front face of this shadow quad).
          This is needed, since user of this method may want to do culling
          to eliminate back or front faces.

          TriangleDir(T) indicates direction that goes from CCW triangle side
          (that's guaranteed by the way TriangleDir calculates plane dir).
          So PlaneSideX is @true if LightPos is on CCW side of appropriate
          triangle. So if PlaneSide0 the shadow quad is extended
          in reversed Triangles[0] order, i.e. like 1, 0, Extruded0, Extruded1.
          Otherwise, in normal Triangles[0], i.e. 0, 1, Extruded1, Extruded0.

          Just draw it, the triangle corners numbered with 0,1,2 in CCW and
          imagine that you want the shadow quad to be also CCW on the outside,
          it will make sense then :) }
        if PlaneSide0 and not PlaneSide1 then
          RenderShadowQuad(ManifoldEdgePtr, 1, 0) else
        if PlaneSide1 and not PlaneSide0 then
          RenderShadowQuad(ManifoldEdgePtr, 0, 1);

        Inc(ManifoldEdgePtr);
      end;

      { For each border edge, always render it's shadow quad.
        THIS CODE IS NEVER USED NOW (at the beginning of this method,
        we exit if BorderEdges.Count <> 0). That's because rendering
        the shadow quads from border edges doesn't solve the problem fully:
        artifacts are still possible.

        See http://http.developer.nvidia.com/GPUGems3/gpugems3_ch11.html
        for more involved approach. Rendering shadow quads from border edges,
        like below, is only part of the solution. }
      BorderEdgesNow := BorderEdges;
      BorderEdgePtr := PBorderEdge(BorderEdgesNow.List);
      for I := 0 to BorderEdgesNow.Count - 1 do
      begin
        PlaneSide0 := TrianglesPlaneSide.L[BorderEdgePtr^.TriangleIndex];

        { We want to have consistent CCW orientation of shadow quads faces,
          so that face is oriented CCW <=> you're looking at it from outside
          (i.e. it's considered front face of this shadow quad).
          This is needed, since user of this method may want to do culling
          to eliminate back or front faces.

          TriangleDir(T) indicates direction that goes from CCW triangle side
          (that's guaranteed by the way TriangleDir calculates plane dir).
          So PlaneSide0 is true if LightPos is on CCW side of appropriate
          triangle. So if PlaneSide0, the shadow quad is extended
          in the direction of TriangleIndex, like 1, 0, Extruded0, Extruded1. }
        if PlaneSide0 then
          RenderShadowQuad(BorderEdgePtr, 1, 0) else
          RenderShadowQuad(BorderEdgePtr, 0, 1);

        Inc(BorderEdgePtr);
      end;

    glEnd;

  finally FreeAndNil(TrianglesPlaneSide) end;
end;

procedure TCastleScene.RenderShadowVolume(
  ShadowVolumeRenderer: TBaseShadowVolumeRenderer;
  const ParentTransformIsIdentity: boolean;
  const ParentTransform: TMatrix4Single);
var
  Box: TBox3D;
  SVRenderer: TGLShadowVolumeRenderer;
const
  AllowSilhouetteOptimization = true;
begin
  if GetExists and CastShadowVolumes then
  begin
    { calculate Box }
    Box := BoundingBox;
    if not ParentTransformIsIdentity then
      Box := Box.Transform(ParentTransform);

    SVRenderer := ShadowVolumeRenderer as TGLShadowVolumeRenderer;
    SVRenderer.InitScene(Box);

    if SVRenderer.SceneShadowPossiblyVisible then
    begin
      if AllowSilhouetteOptimization then
        RenderSilhouetteShadowVolume(
          SVRenderer.LightPosition, ParentTransformIsIdentity, ParentTransform,
          SVRenderer.ZFailAndLightCap,
          SVRenderer.ZFail) else
        {$warnings off}
        { Do not warn that this code is not reachable because
          AllowSilhouetteOptimization is constant. }
        RenderAllShadowVolume(
          SVRenderer.LightPosition, ParentTransformIsIdentity, ParentTransform,
          SVRenderer.ZFailAndLightCap,
          SVRenderer.ZFail);
        {$warnings on}
    end;
  end;
end;

procedure TCastleScene.RenderSilhouetteEdges(
  const ObserverPos: TVector4Single;
  const Transform: TMatrix4Single);

{ This is actually a modified implementation of
  TCastleScene.RenderSilhouetteShadowQuads: instead of rendering
  shadow quad for each silhouette edge, the edge is simply rendered
  as OpenGL line. }

var
  Triangles: TTriangle3SingleList;
  EdgePtr: PManifoldEdge;

  procedure RenderEdge(
    const P0Index, P1Index: Cardinal);
  var
    V0, V1: TVector3Single;
    EdgeV0, EdgeV1: PVector3Single;
    TrianglePtr: PTriangle3Single;
  begin
    TrianglePtr := Addr(Triangles.L[EdgePtr^.Triangles[0]]);
    EdgeV0 := @TrianglePtr^[(EdgePtr^.VertexIndex + P0Index) mod 3];
    EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + P1Index) mod 3];

    V0 := MatrixMultPoint(Transform, EdgeV0^);
    V1 := MatrixMultPoint(Transform, EdgeV1^);

    glVertexv(V0);
    glVertexv(V1);
  end;

  function PlaneSide(const T: TTriangle3Single): boolean;
  var
    Plane: TVector4Single;
  begin
    Plane := TrianglePlane(
      MatrixMultPoint(Transform, T[0]),
      MatrixMultPoint(Transform, T[1]),
      MatrixMultPoint(Transform, T[2]));
    Result := (Plane[0] * ObserverPos[0] +
               Plane[1] * ObserverPos[1] +
               Plane[2] * ObserverPos[2] +
               Plane[3] * ObserverPos[3]) > 0;
  end;

var
  I: Integer;
  TrianglePtr: PTriangle3Single;
  PlaneSide0, PlaneSide1: boolean;
  TrianglesPlaneSide: TBooleanList;
  Edges: TManifoldEdgeList;
begin
  glBegin(GL_LINES);
    Triangles := TrianglesListShadowCasters;
    Edges := ManifoldEdges;

    TrianglesPlaneSide := TBooleanList.Create;
    try
      { calculate TrianglesPlaneSide array }
      TrianglesPlaneSide.Count := Triangles.Count;
      TrianglePtr := PTriangle3Single(Triangles.List);
      for I := 0 to Triangles.Count - 1 do
      begin
        TrianglesPlaneSide.L[I] := PlaneSide(TrianglePtr^);
        Inc(TrianglePtr);
      end;

      { for each edge, possibly render it's shadow quad }
      EdgePtr := PManifoldEdge(Edges.List);
      for I := 0 to Edges.Count - 1 do
      begin
        PlaneSide0 := TrianglesPlaneSide.L[EdgePtr^.Triangles[0]];
        PlaneSide1 := TrianglesPlaneSide.L[EdgePtr^.Triangles[1]];

        if PlaneSide0 <> PlaneSide1 then
          RenderEdge(0, 1);

        Inc(EdgePtr);
      end;

    finally FreeAndNil(TrianglesPlaneSide) end;
  glEnd;
end;

procedure TCastleScene.RenderBorderEdges(
  const Transform: TMatrix4Single);
var
  Triangles: TTriangle3SingleList;
  EdgePtr: PBorderEdge;

  procedure RenderEdge;
  var
    V0, V1: TVector3Single;
    EdgeV0, EdgeV1: PVector3Single;
    TrianglePtr: PTriangle3Single;
  begin
    TrianglePtr := Addr(Triangles.L[EdgePtr^.TriangleIndex]);
    EdgeV0 := @TrianglePtr^[(EdgePtr^.VertexIndex + 0) mod 3];
    EdgeV1 := @TrianglePtr^[(EdgePtr^.VertexIndex + 1) mod 3];

    V0 := MatrixMultPoint(Transform, EdgeV0^);
    V1 := MatrixMultPoint(Transform, EdgeV1^);

    glVertexv(V0);
    glVertexv(V1);
  end;

var
  I: Integer;
  Edges: TBorderEdgeList;
begin
  glBegin(GL_LINES);
    Triangles := TrianglesListShadowCasters;
    Edges := BorderEdges;

    { for each edge, render it }
    EdgePtr := PBorderEdge(Edges.List);
    for I := 0 to Edges.Count - 1 do
    begin
      RenderEdge;
      Inc(EdgePtr);
    end;
  glEnd;
end;

{ Frustum culling ------------------------------------------------------------ }

function TCastleScene.FrustumCulling_None(Shape: TGLShape): boolean;
begin
  Result := true;
end;

function TCastleScene.FrustumCulling_Sphere(Shape: TGLShape): boolean;
begin
  Result := Shape.FrustumBoundingSphereCollisionPossibleSimple(
    RenderFrustum_Frustum^);
end;

function TCastleScene.FrustumCulling_Box(Shape: TGLShape): boolean;
begin
  Result := RenderFrustum_Frustum^.Box3DCollisionPossibleSimple(
    Shape.BoundingBox);
end;

function TCastleScene.FrustumCulling_Both(Shape: TGLShape): boolean;
begin
  Result :=
    Shape.FrustumBoundingSphereCollisionPossibleSimple(
      RenderFrustum_Frustum^) and
    RenderFrustum_Frustum^.Box3DCollisionPossibleSimple(
      Shape.BoundingBox);
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
  Shape: TGLShape): boolean;
begin
  Result := Shape.RenderFrustumOctree_Visible;
end;

procedure TCastleScene.RenderFrustumOctree_EnumerateShapes(
  ShapeIndex: Integer; CollidesForSure: boolean);
var
  Shape: TGLShape;
begin
  Shape := TGLShape(OctreeRendering.ShapesList[ShapeIndex]);

  if (not Shape.RenderFrustumOctree_Visible) and
     ( CollidesForSure or
       OctreeFrustumCullingFunc(Shape) ) then
    Shape.RenderFrustumOctree_Visible := true;
end;

procedure TCastleScene.Render(const Frustum: TFrustum; const Params: TRenderParams);

{ Call Render with explicit TTestShapeVisibility function.
  That is, choose test function suitable for our Frustum,
  octrees and some settings.

  If OctreeRendering is initialized (so be sure to include
  ssRendering in @link(Spatial)), this octree will be used to quickly
  find visible Shape. Otherwise, we will just enumerate all
  Shapes (which may be slower if you really have a lot of Shapes). }

  procedure RenderFrustumOctree(Octree: TShapeOctree);

    procedure ResetShapeVisible(Shape: TShape);
    begin
      TGLShape(Shape).RenderFrustumOctree_Visible := false;
    end;

  begin
    Shapes.Traverse(@ResetShapeVisible, false, true);
    Octree.EnumerateCollidingOctreeItems(Frustum,
      @RenderFrustumOctree_EnumerateShapes);
    Render(@RenderFrustumOctree_TestShape, Frustum, Params);
  end;

begin
  if GetExists and (Dirty = 0) and
     (ReceiveShadowVolumes = Params.ShadowVolumesReceivers) then
  begin
    RenderFrustum_Frustum := @Frustum;

    if OctreeRendering <> nil then
      RenderFrustumOctree(OctreeRendering) else
      Render(FrustumCullingFunc, Frustum, Params);
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
{ After PrepareBackground assertion FBackgroundValid is valid }
var
  BgNode: TBackgroundNode;
  SkyAngleCount: Integer;
  SkyColorCount: Integer;
  GroundAngleCount: Integer;
  GroundColorCount: Integer;
begin
  if FBackgroundValid and (BackgroundStack.Top = FBackgroundNode) then
    Exit;

  { Background is created, but not suitable for current
    BackgroundStack.Top. So destroy it. }
  if FBackgroundValid then
    InvalidateBackground;

  if (BackgroundStack.Top <> nil) and
     (BackgroundStack.Top is TBackgroundNode) then
  begin
    if Log then
      WritelnLog('Background', Format('OpenGL background recreated, with radius %f',
        [BackgroundSkySphereRadius]));

    BgNode := TBackgroundNode(BackgroundStack.Top);

    SkyAngleCount := BgNode.FdSkyAngle.Count;
    SkyColorCount := BgNode.FdSkyColor.Count;

    if SkyColorCount <= 0 then
    begin
      OnWarning(wtMajor, 'VRML/X3D', 'Background node incorrect: ' +
        'Sky must have at least one color');
      FBackground := nil;
    end else
    begin
      if SkyAngleCount + 1 <> SkyColorCount then
      begin
        OnWarning(wtMajor, 'VRML/X3D', 'Background node incorrect: ' +
          'Sky must have exactly one more Color than Angles');
        { We know now that SkyColorCount >= 1 and
          SkyAngleCount >= 0 (since SkyAngleCount is a count of an array).
          So we correct one of them to be smaller. }
        if SkyAngleCount + 1 > SkyColorCount then
          SkyAngleCount := SkyColorCount - 1 else
          SkyColorCount := SkyAngleCount + 1;
      end;

      GroundAngleCount := BgNode.FdGroundAngle.Count;
      GroundColorCount := BgNode.FdGroundColor.Count;

      if (GroundAngleCount <> 0) and
         (GroundAngleCount + 1 <> GroundColorCount) then
      begin
        OnWarning(wtMajor, 'VRML/X3D', 'Background node incorrect: ' +
          'Ground must have exactly one more Color than Angles');
        { We know now that GroundColorCount >= 1 and
          GroundAngleCount >= 0 (since GroundAngleCount is a count of an array).
          So we correct one of them to be smaller. }
        if GroundAngleCount + 1 > GroundColorCount then
          GroundAngleCount := GroundColorCount - 1 else
          GroundColorCount := GroundAngleCount + 1;
      end;

      FBackground := TBackground.Create(
        PArray_Single(BgNode.FdGroundAngle.Items.List), GroundAngleCount,
        PArray_Vector3Single(BgNode.FdGroundColor.Items.List), GroundColorCount,
        BgNode.BgImages,
        PArray_Single(BgNode.FdSkyAngle.Items.List), SkyAngleCount,
        PArray_Vector3Single(BgNode.FdSkyColor.Items.List), SkyColorCount,
        BackgroundSkySphereRadius);
    end;
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

  BackgroundNode := BackgroundStack.Top as TAbstractBackgroundNode;
  if (BackgroundNode <> nil) and
     { We have to still check Result, since not every TAbstractBackgroundNode
       is supported now, so for some background nodes we still have
       Result = nil. }
     (Result <> nil) then
  begin
    Result.Transform := BackgroundNode.TransformRotation;
  end;
end;

function TCastleScene.Attributes: TSceneRenderingAttributes;
begin
  Result := Renderer.Attributes as TSceneRenderingAttributes;
end;

procedure TCastleScene.UpdateGeneratedTextures(
  const RenderFunc: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const OriginalViewportX, OriginalViewportY: LongInt;
  const OriginalViewportWidth, OriginalViewportHeight: Cardinal);
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
      CameraViewKnown, CameraPosition, CameraDirection, CameraUp);

    AvoidShapeRendering := nil;
    AvoidNonShadowCasterRendering := false;
  end;

  if NeedsRestoreViewport then
    glViewport(OriginalViewportX, OriginalViewportY,
               OriginalViewportWidth, OriginalViewportHeight);
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

  { set UpdateNeeded := true before calling inherited (with VisibleChange
    and OnVisibleChange callback), because inside OnVisibleChange callback
    we'll actually initialize regenerating the textures. }
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
        { Even mere vcCamera causes regenerate of RenderedTexture,
          as RenderedTexture with viewpoint = NULL uses current camera.
          So any Changes <> [] causes regeneration of RenderedTexture.
          Also, for other than RenderedTexture nodes, default is to regenerate
          (safer).  }
        GeneratedTextures.L[I].Handler.UpdateNeeded := true;
    end;
  end;
end;

function TCastleScene.ScreenEffectsCount: Integer;
var
  I: Integer;
  SE: TScreenEffectNode;
begin
  Result := 0;
  if Attributes.Shaders <> srDisable then
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

{ TSceneRenderingAttributes ---------------------------------------------- }

constructor TSceneRenderingAttributes.Create;
begin
  inherited;

  FBlending := true;
  FBlendingSourceFactor := DefaultBlendingSourceFactor;
  FBlendingDestinationFactor := DefaultBlendingDestinationFactor;
  FBlendingSort := DefaultBlendingSort;
  FControlBlending := true;

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
    ControlBlending := S.ControlBlending;
    UseOcclusionQuery := S.UseOcclusionQuery;
    UseHierarchicalOcclusionQuery := S.UseHierarchicalOcclusionQuery;
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
  const Value: TGLenum);
begin
  FBlendingSourceFactor := Value;
end;

procedure TSceneRenderingAttributes.SetBlendingDestinationFactor(
  const Value: TGLenum);
begin
  FBlendingDestinationFactor := Value;
end;

procedure TSceneRenderingAttributes.SetBlendingSort(const Value: boolean);
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

      { Make PrepareShapesResouces again, to cause TGLShape.PrepareResources
        that initializes OcclusionQueryId for each shape }
      if TemporaryAttributeChange = 0 then
        for I := 0 to FScenes.Count - 1 do
          if FScenes[I] <> nil then
            FScenes[I].PreparedShapesResouces := false;
    end;
  end;
end;

function TSceneRenderingAttributes.ReallyUseOcclusionQuery: boolean;
begin
  Result := UseOcclusionQuery and (not UseHierarchicalOcclusionQuery) and
    GL_ARB_occlusion_query and (GLQueryCounterBits > 0);
end;

function TSceneRenderingAttributes.
  ReallyUseHierarchicalOcclusionQuery: boolean;
begin
  Result := UseHierarchicalOcclusionQuery and GL_ARB_occlusion_query and
    (GLQueryCounterBits > 0);
end;

procedure TSceneRenderingAttributes.SetShaders(const Value: TShadersRendering);
var
  I: Integer;
begin
  if Shaders <> Value then
  begin
    inherited;
    { When switching to a higher TShadersRendering value
      (that uses more shaders), we want to force generating necessary
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

initialization
  GLContextCache := TGLRendererContextCache.Create;
finalization
  FreeAndNil(GLContextCache);
end.
