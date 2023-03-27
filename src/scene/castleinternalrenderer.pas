{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ X3D shapes rendering (TGLRenderer).
  Normal user code does not use this directly,
  instead always render through TCastleScene that wraps this renderer.

  @bold(Usage:)

  @orderedList(
    @item(
      Call @link(TGLRenderer.Prepare) for all
      the states that you want to later render. The order of calling TGLRenderer.Prepare
      methods doesn't matter, also you are free to prepare states that you
      will not actually use later. Of course a state, once prepared,
      may be used in rendering as many times as you want.

      It's important that you have to prepare @italic(every state that
      you plan to later render). During rendring the state
      must have exactly the same (fields, properties) values as when
      it was prepared. In particular, it must have the same
      pointers to nodes Last*/Active* and their contents
      also must be the same. TGLRenderer.Prepare
      may save some associations between objects and OpenGL resources,
      so it's important that the same pointer must always point to the
      same object (until it's unprepared).

      TGLRenderer.Prepare requires active OpenGL context. It doesn't modify
      OpenGL state (only allocates some resources like texture names).
      It cannot be called inside a display list.
    )

    @item(
      When you want to release resources, you should call TGLRenderer.Unprepare on
      nodes that you want to change or free. This should be used
      with nodes that were passed as Last*/Active*
      in some State for TGLRenderer.Prepare.

      Note that you cannot free the nodes before unpreparing them.
      The node instance must remain valid while it's prepared.
    )

    @item(
      Surround rendering of multiple scenes with TGLRenderer.ViewportRenderBegin,
      TGLRenderer.ViewportRenderEnd.

      Notes below are for things that render manually inside TCastleTransform.LocalRender
      (and thus are part of TCastleViewport), but are not TCastleScene.
      This affects e.g. TMyMesh that does rendering using TCastleRenderUnlitMesh
      in ../../examples/research_special_rendering_methods/test_rendering_old_opengl/code/gameinitialize.pas .

      1. See TGLRenderer.ViewportRenderEnd for the up-to-date list of state
         (some managed using RenderContext, some using direct OpenGL(ES) calls)
         that is reset at the end of viewport rendering,
         because before each scene or before each shape it may change and *not*
         be reset.

         When you render manually inside TCastleTransform.LocalRender,
         this state is undefined when you start,
         and you can change it carelessly (since next scene or shape
         will adjust it), though we advise you save/restore it (in case it will matter
         in the future, e.g. the state will move to ViewportRenderBegin and shapes
         will assume it).

      2. See TGLRenderer.ViewportRenderBegin for the up-to-date list of state
         (some managed using RenderContext, some using direct OpenGL(ES) calls)
         that is initialized at the beginning of viewport rendering,
         because each shape may assume it is such.

         When you render manually inside TCastleTransform.LocalRender,
         you can also assume the state is set to this value,
         and if you change it -- make sure to save/restore it.
    )

    @item(
      Surround shapes rendering between TGLRenderer.RenderBegin and TGLRenderer.RenderEnd.

      Between these calls, you should not touch OpenGL state or RenderContext
      yourself at all.
      The renderer may depend that every state change goes through it.
    )

    @item(
      Between TGLRenderer.RenderBegin and TGLRenderer.RenderEnd
      you should render the shapes by calling RenderShape.

      Remember that you can render only shapes that have Shape.State
      prepared by TGLRenderer.Prepare.
    )

    @item(
      Since the first prepare / render calls, this renderer assumes it's
      always called in the same OpenGL context. To break association
      with OpenGL context call TGLRenderer.UnprepareAll (this is like calling TGLRenderer.Unprepare
      on every prepared thing + clearing some remaining resources).
    )
  )

  @bold(OpenGL state affecting X3D rendering:)

  Some OpenGL state is unconditionally set by rendering (TGLRenderer.ViewportRenderBegin,
  TGLRenderer.RenderBegin, or just each shape rendering).

  But there's also some OpenGL state that we let affect our rendering.
  This allows you to customize rendering by using normal OpenGL commands
  or RenderContext settings.

  @unorderedList(
    @item(Current glPolygonMode.
      This is used by @link(TCastleScene) to optionally render wireframe.
    )

    @item(Blending settings, controlled using RenderContext.BlendingBegin,
      RenderContext.BlendingEnd.

      Also RenderContext.DepthBufferUpdate.

      These are used by @link(TCastleScene) to render
      scenes with a mix of tranparent and opaque objects.
      Only @link(TCastleScene) deals with it (not this renderer),
      as doing it correctly requires ordering the shapes.
    )
  )

  The renderer uses arrays in GPU-friendly format defined by TGeometryArrays.
}
unit CastleInternalRenderer;

{$I castleconf.inc}

interface

uses Classes, SysUtils, Generics.Collections,
  {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}
  CastleUtils, CastleVectors, X3DFields, X3DNodes, CastleColors,
  CastleInternalX3DLexer, CastleImages, CastleGLUtils, CastleRendererInternalLights,
  CastleGLShaders, CastleGLImages, CastleTextureImages, CastleVideos, X3DTime,
  CastleShapes, CastleInternalGLCubeMaps, CastleClassUtils, CastleInternalCompositeImage,
  CastleInternalGeometryArrays, CastleInternalArraysGenerator, CastleRendererInternalShader,
  CastleRendererInternalTextureEnv, CastleBoxes, CastleTransform, CastleRenderOptions;

{$define read_interface}

type
  TBeforeGLVertexProc = procedure (Node: TAbstractGeometryNode;
    const Vert: TVector3) of object;

  TLightRenderEvent = CastleRendererInternalLights.TLightRenderEvent;

  TTextureImageCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: string;
    FlipVertically: Boolean;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    GUITexture: boolean;
    References: Cardinal;
    GLName: TGLuint;
  end;
  TTextureImageCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureImageCache>;

  TTextureVideoCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: string;
    FlipVertically: Boolean;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    GUITexture: boolean;
    References: Cardinal;
    GLVideo: TGLVideo3D;
  end;
  TTextureVideoCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureVideoCache>;

  TTextureCubeMapCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: string;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    References: Cardinal;
    GLName: TGLuint;
  end;
  TTextureCubeMapCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureCubeMapCache>;

  TTexture3DCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: string;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap3D;
    References: Cardinal;
    GLName: TGLuint;
  end;
  TTexture3DCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTexture3DCache>;

  { Cached depth or float texture.
    For now, depth and float textures require the same fields. }
  TTextureDepthOrFloatCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: string;
    Wrap: TTextureWrap2D;
    References: Cardinal;
    GLName: TGLuint;
  end;
  TTextureDepthOrFloatCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureDepthOrFloatCache>;

  TX3DRendererShape = class;
  TVboType = (vtCoordinate, vtAttribute, vtIndex);
  TVboTypes = set of TVboType;
  TVboArrays = array [TVboType] of TGLuint;

  { Cached shape resources. }
  TShapeCache = class
  private
    RenderOptions: TCastleRenderOptions;
    Geometry: TAbstractGeometryNode;
    State: TX3DGraphTraverseState;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3;
    FogVolumetricVisibilityStart: Single;
    References: Cardinal;

    { An instance of TGeometryArrays, decomposing this shape geometry.
      Used to easily render and process this geometry.

      When VBO are supported (all non-ancient GPUs) then TShapeCache
      uses an instance of it only once (in LoadArraysToVbo,
      to load contents to GPU, and then calls Arrays.FreeData).
      But it is later used by outside code multiple times to make draw calls
      (as it is passed by TBaseCoordinateRenderer).
      So storing it in TShapeCache (to be shared when possible) makes sense.

      TShapeCache owns this -- it will be freed when TShapeCache is freed. }
    Arrays: TGeometryArrays;

    { What VBOs do we need to reload at next LoadArraysToVbo call.
      This also implies that content for this VBOs needs to be created,
      which may mean we need to change Arrays.
      This is extended at each InvalidateVertexData call. }
    VboToReload: TVboTypes;

    Vbo: TVboArrays;
    VboAllocatedUsage: TGLenum;
    VboAllocatedSize: array [TVboType] of Cardinal;
    VboCoordinatePreserveGeometryOrder: Boolean; //< copied from TGeometryArrays.CoordinatePreserveGeometryOrder

    Vao: TVertexArrayObject;

    { Like TX3DRendererShape.LoadArraysToVbo,
      but takes explicit DynamicGeometry. }
    procedure LoadArraysToVbo(const DynamicGeometry: boolean);
    procedure GLContextClose;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InvalidateVertexData(const Changed: TVboTypes);
    { Debug description of this shape cache. }
    function ToString: String; override;
  end;

  TShapeCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TShapeCache>;

  TX3DGLSLProgram = class;

  TShaderProgramCache = class
  public
    { Hash of TShader code when initializing this shader
      by LinkProgram. Used to decide when shader needs to be regenerated,
      and when it can be shared. }
    Hash: TShaderCodeHash;

    { Actual GLSL program. May be @nil (if it failed to link). }
    ShaderProgram: TX3DGLSLProgram;

    References: Cardinal;

    destructor Destroy; override;
  end;

  TShaderProgramCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TShaderProgramCache>;

  TGLRenderer = class;

  { Cache used by TGLRenderer instances to share OpenGL resources,
    like textures. This may save a lot of OpenGL memory when you
    use multiple renderers (for example, multiple TCastleScene instances).

    The cache can only be shared by renderers in the same OpenGL context,
    or in shared OpenGL contexts. It cannot be reused
    in totally alien OpenGL contexts, as the OpenGL identifiers will simply
    not exist there. In practice, this should not be a problem, as our OpenGL
    contexts always share resources, and you will almost always just use
    a single instance of this cache inside @link(GLContextCache). }
  TGLRendererContextCache = class
  private
    FFreeWhenEmpty: Boolean;
    FFreeWhenEmptyInstanceToNil: PObject;
    TextureImageCaches: TTextureImageCacheList;
    TextureVideoCaches: TTextureVideoCacheList;
    TextureCubeMapCaches: TTextureCubeMapCacheList;
    Texture3DCaches: TTexture3DCacheList;
    TextureDepthOrFloatCaches: TTextureDepthOrFloatCacheList;
    ShapeCaches: array [{ DisableSharedCache } Boolean] of TShapeCacheList;
    ProgramCaches: TShaderProgramCacheList;

    function Empty: Boolean;

    { If we are empty, and FFreeWhenEmpty, then free self.
      Beware: calling this procedure may free the current instance.
      Do not access anything from this instance after calling this method. }
    procedure CheckFreeWhenEmpty;

    { Load given texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function TextureImage_IncReference(
      const TextureImage: TEncodedImage;
      const TextureFullUrl: string;
      const Filter: TTextureFilter;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      const CompositeForMipmaps: TCompositeImage;
      const GUITexture: boolean;
      const FlipVertically: Boolean): TGLuint;

    procedure TextureImage_DecReference(
      const TextureGLName: TGLuint);

    function TextureVideo_IncReference(
      const TextureVideo: TVideo;
      const TextureFullUrl: string;
      const FlipVertically: Boolean;
      const Filter: TTextureFilter;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      const GUITexture: boolean): TGLVideo3D;

    procedure TextureVideo_DecReference(
      const TextureVideo: TGLVideo3D);

    { Load given cube texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function TextureCubeMap_IncReference(
      const TextureFullUrl: string;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      const PositiveX, NegativeX,
            PositiveY, NegativeY,
            PositiveZ, NegativeZ: TEncodedImage;
      const CompositeForMipmaps: TCompositeImage): TGLuint;

    procedure TextureCubeMap_DecReference(
      const TextureGLName: TGLuint);

    { Requires GLFeatures.TextureDepthCompare (not just TextureDepth!) before calling this. }
    function TextureDepth_IncReference(
      const TextureFullUrl: string;
      const TextureWrap: TTextureWrap2D;
      const Width, Height: Cardinal): TGLuint;

    procedure TextureDepth_DecReference(
      const TextureGLName: TGLuint);

    { Increase / decrease reference to a float texture.
      Required GLFeatures.TextureFloat before calling this.
      Precision32 = @true requires 32-bit full Single floats,
      Precision32 = @false requires 16-bit (half) floats. }
    function TextureFloat_IncReference(
      const TextureFullUrl: string;
      const Filter: TTextureFilter;
      const TextureWrap: TTextureWrap2D;
      const Width, Height: Cardinal;
      const Precision32: boolean): TGLuint;
    procedure TextureFloat_DecReference(
      const TextureGLName: TGLuint);

    { Load given 3D texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function Texture3D_IncReference(
      const TextureFullUrl: string;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      const TextureWrap: TTextureWrap3D;
      const Image: TEncodedImage;
      const Composite: TCompositeImage): TGLuint;

    procedure Texture3D_DecReference(
      const TextureGLName: TGLuint);
  public
    constructor Create;
    destructor Destroy; override;

    { Shape cache. We return TShapeCache, either taking an existing
      instance from cache or creating and adding a new one.
      Caller is responsible for checking are Arrays / Vbo zero and
      eventually initializing and setting. }
    function Shape_IncReference(const Shape: TX3DRendererShape;
      const ARenderer: TGLRenderer): TShapeCache;

    procedure Shape_DecReference(const Shape: TX3DRendererShape;
      var ShapeCache: TShapeCache);

    { Shader program cache. We return TShaderProgramCache,
      either taking an existing instance from cache or creating and adding
      a new one. If we create a new one, we will use Shader to initialize
      program hash and to create and link actual TX3DGLSLProgram instance. }
    function Program_IncReference(ARenderer: TGLRenderer;
      Shader: TShader; const ShapeNiceName: string): TShaderProgramCache;

    procedure Program_DecReference(var ProgramCache: TShaderProgramCache);

    { Free (and Self <> nil) when the cache will be empty (maybe immediately,
      maybe later).

      Beware: calling this procedure may free the current instance.
      Do not access anything from this instance after calling this method.

      After this procedure, any Xxx_DecReference call also may free
      the cache instance. }
    procedure FreeWhenEmpty(const InstanceToNil: PObject);
  end;

  {$I castleinternalrenderer_resource.inc}
  {$I castleinternalrenderer_texture.inc}
  {$I castleinternalrenderer_glsl.inc}
  {$I castleinternalrenderer_pass.inc}

  { Shape that can be rendered. }
  TX3DRendererShape = class(TShape)
  private
    { Set to SceneModelView combined with particular shape transformation. }
    ShapeModelView: TMatrix4;

    { Generate VBO if needed, and reload VBO contents.
      Assumes GLVertexBufferObject is true.

      Arrays data @italic(must not) be freed (by TGeometryArrays.FreeData)
      before calling this method. Also, this method will always call
      Arrays.FreeData. So do not load the same TGeometryArrays instance
      twice to the Vbo.

      We always keep assertion that Vbo is loaded <=> Arrays data is freed. }
    procedure LoadArraysToVbo;
  public
    { Non-nil means that we have obtained TShaderProgramCache instance,
      with valid Hash and ShaderProgram. Note that ShaderProgram may still
      be @nil, if it failed to link.

      Separate values for each rendering pass, since different rendering
      passes probably have different GlobalLights and so will require different
      shaders. This makes multi-pass rendering, like for shadow volumes,
      play nicely with shaders. Otherwise we could recreate shaders at each
      rendering pass. }
    ProgramCache: array [TTotalRenderingPass] of TShaderProgramCache;

    Cache: TShapeCache;

    { Assign this each time before passing this shape to RenderShape.
      Should contain camera and scene transformation (but not particular shape transformation). }
    SceneModelView: TMatrix4;

    { Assign this each time before passing this shape to RenderShape.
      Should contain only scene transformation (but not particular shape transformation). }
    SceneTransform: TMatrix4;

    { Assign this each time before passing this shape to RenderShape. }
    Fog: TFogFunctionality;

    { For implementing TextureCoordinateGenerator.mode = "MIRROR-PLANE". }
    MirrorPlaneUniforms: TMirrorPlaneUniforms;

    { Is bump mapping allowed by the current shape.
      Fully calculated only after InitMeshRenderer, as determining GeneratorClass
      is needed to set this. }
    BumpMappingAllowed: Boolean;

    { Is bump mapping used for current shape.
      Fully calculated only during render, after BumpMappingAllowed is calculated
      and after textures are applied.
      This is determined by BumpMappingAllowed,
      global BumpMapping, and by the texture information for current
      shape (whether user provided normal map, height map etc.) }
    BumpMappingUsed: Boolean;

    { Along with Shape.BumpMappingUsed, this is calculated (use only if Shape.BumpMappingUsed). }
    BumpMappingTextureCoordinatesId: Cardinal;

    destructor Destroy; override;
  end;

  TGLRenderer = class
  private
    { ---------------------------------------------------------
      GLContext-specific things, so freed (or reset in some other way to default
      uninitialized values) in UnprepareAll. }

    GLTextureNodes: TGLTextureNodes;
    ScreenEffectPrograms: TGLSLProgramList;

    { ------------------------------------------------------------------------ }

    { For speed, we keep a single instance of TShader,
      instead of creating / destroying an instance at each RenderShape.
      This is necessary, otherwise the constructor / destructor of TShader
      would be bottle-necks. }
    PreparedShader: TShader;

    { ------------------------------------------------------------
      Things usable only during Render. }

    { How many texture units are used.

      It's always clamped by the number of available texture units
      (GLFeatures.MaxTextureUnits). Always <= 1 if OpenGL doesn't support
      multitexturing (not GLFeatures.UseMultiTexturing). }
    BoundTextureUnits: Cardinal;

    { For how many texture units do we have to generate tex coords.

      At the end, the idea is that this is <= BoundTextureUnits
      (no point in generating tex coords for not existing textures).
      However during render it may be temporarily > BoundTextureUnits
      (in case we calculate it before actually binding the textures,
      this may happen for textures in ComposedShader custom fields). }
    TexCoordsNeeded: Cardinal;

    { For which texture units we pushed and modified the texture matrix.
      Only inside RenderShape.
      Always <= 1 if not GLFeatures.UseMultiTexturing. }
    TextureTransformUnitsUsed: Cardinal;

    { Additional texture units used,
      in addition to 0..TextureTransformUnitsUsed - 1.
      Cleared by RenderShapeBegin, added by PushTextureUnit,
      used by RenderShapeEnd. }
    TextureTransformUnitsUsedMore: TInt32List;

    function PrepareTexture(Shape: TShape; Texture: TAbstractTextureNode): Pointer;

    {$ifndef OpenGLES}
    { Call glPushMatrix, assuming that current matrix mode is GL_TEXTURE
      and current tex unit is TexUnit (always make sure this is true when
      calling it!).

      It also records this fact, so that RenderShapeEnd will be able to
      make pop texture matrix later.

      In fact this optimizes push/pops on texture matrix stack, such that
      X3D TextureTransform nodes and such together with PushTextureUnit
      will only use only matrix stack place, even if texture will be
      "pushed" multiple times (both by PushTextureUnit and normal
      X3D TextureTransform realized in RenderShapeBegin.) }
    procedure PushTextureUnit(const TexUnit: Cardinal);
    {$endif}

    { Check RenderOptions (like RenderOptions.BumpMapping) and OpenGL
      context capabilities to see if bump mapping can be used. }
    function BumpMapping: TBumpMapping;

    {$I castleinternalrenderer_surfacetextures.inc}
  private
    { ----------------------------------------------------------------- }

    { Available between RenderBegin / RenderEnd. }
    LightsRenderer: TLightsRenderer;

    { Currently set fog parameters, during render. }
    FogFunctionality: TFogFunctionality;
    FogEnabled: boolean;
    FogType: TFogType;
    FogColor: TVector3;
    FogVisibilityRangeScaled: Single;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3;
    FogVolumetricVisibilityStart: Single;

    FRenderOptions: TCastleRenderOptions;

    FCache: TGLRendererContextCache;

    { Lights shining on all shapes, may be @nil. Set in each RenderBegin. }
    GlobalLights: TLightInstancesList;

    { Rendering parameters. Set in each RenderBegin, cleared to nil in RenderEnd. }
    RenderingCamera: TRenderingCamera;
    Statistics: PRenderStatistics;

    { Rendering pass. Set in each RenderBegin. }
    Pass: TTotalRenderingPass;

    { Get X3D fog parameters, based on fog node and RenderOptions. }
    procedure GetFog(const AFogFunctionality: TFogFunctionality;
      out Enabled, Volumetric: boolean;
      out VolumetricDirection: TVector3;
      out VolumetricVisibilityStart: Single);

    { If multitexturing available, this sets currently active texture unit.
      TextureUnit is newly active unit, this is added to GL_TEXTURE0.

      So the only thing that you have to care about is to specify TextureUnit <
      FreeGLTexturesCount.
      Everything else (multitexturing availability, GL_TEXTURE0)
      is taken care of inside here. }
    class procedure ActiveTexture(const TextureUnit: Cardinal);

    { Disable any (fixed-function) texturing (2D, 3D, cube map, and so on)
      on given texture unit. }
    class procedure DisableTexture(const TextureUnit: Cardinal);
    class procedure DisableCurrentTexture;

    procedure RenderShapeLineProperties(const Shape: TX3DRendererShape;
      const Shader: TShader);
    procedure RenderShapeMaterials(const Shape: TX3DRendererShape;
      const Shader: TShader);
    procedure RenderShapeLights(const Shape: TX3DRendererShape;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeFog(const Shape: TX3DRendererShape;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeTextureTransform(const Shape: TX3DRendererShape;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeClipPlanes(const Shape: TX3DRendererShape;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeCreateMeshRenderer(const Shape: TX3DRendererShape;
      const Shader: TShader;
      const Lighting: boolean);
    procedure RenderShapeShaders(const Shape: TX3DRendererShape;
      const Shader: TShader;
      const Lighting: boolean;
      const GeneratorClass: TArraysGeneratorClass;
      const ExposedMeshRenderer: TObject);
    procedure RenderShapeTextures(const Shape: TX3DRendererShape;
      const Shader: TShader;
      const Lighting: boolean;
      const GeneratorClass: TArraysGeneratorClass;
      const ExposedMeshRenderer: TObject;
      const UsedGLSLTexCoordsNeeded: Cardinal);
    procedure RenderShapeInside(const Shape: TX3DRendererShape;
      const Shader: TShader;
      const Lighting: boolean;
      const GeneratorClass: TArraysGeneratorClass;
      const ExposedMeshRenderer: TObject);
  public
    type
      TRenderMode = (
        { Normal rendering. }
        rmRender,
        { Prepare for future rendering of the same shapes. }
        rmPrepareRenderSelf,
        { Prepare for future rendering of the cloned shapes
          (made by TCastleSceneCore.Clone, or TX3DNode.DeepCopy). }
        rmPrepareRenderClones
      );
    var
      RenderMode: TRenderMode;

    { Constructor. Always pass a cache instance --- preferably,
      something created and used by many scenes. }
    constructor Create(const RenderOptionsClass: TCastleRenderOptionsClass;
      const ACache: TGLRendererContextCache);
    destructor Destroy; override;

    { Rendering attributes. You can change them only when renderer
      is not tied to the current OpenGL context, so only after construction
      or after UnprepareAll call (before any Prepare or Render* calls). }
    property RenderOptions: TCastleRenderOptions read FRenderOptions;

    property Cache: TGLRendererContextCache read FCache;

    { Prepare given Shape for rendering.
      Between preparing and unpreparing, nodes passed here are "frozen":
      do not change, do not free them. }
    procedure Prepare(Shape: TX3DRendererShape);

    { Release resources for this texture. }
    procedure UnprepareTexture(Node: TAbstractTextureNode);

    { Release every OpenGL and X3D resource. That is release any knowledge
      connecting us to the current OpenGL context and any knowledge
      about your prepared X3D nodes, states etc.

      Calling UnprepareAll is valid (and ignored) call if everything
      is already released.

      Destructor callls UnprepareAll automatically. So be sure to either
      call UnprepareAll or destroy this renderer
      when your OpenGL context is still active. }
    procedure UnprepareAll;

    { Surround all TGLRenderer usage in ViewportRenderBegin / ViewportRenderEnd calls.

      The idea is that outer component, like TCastleViewport,
      uses these only once for whole TCastleViewport.Render,
      and then you don't need to repeat this in each TCastleScene.LocalRender. }
    class procedure ViewportRenderBegin;
    class procedure ViewportRenderEnd;

    procedure RenderBegin(const AGlobalLights: TLightInstancesList;
      const ARenderingCamera: TRenderingCamera;
      const LightRenderEvent: TLightRenderEvent;
      const AInternalPass: TInternalRenderingPass;
      const AInternalScenePass: TInternalSceneRenderingPass;
      const AUserPass: TUserRenderingPass;
      const AStatistics: PRenderStatistics);
    procedure RenderEnd;

    procedure RenderShape(const Shape: TX3DRendererShape);

    { Update generated texture for this shape.

      The given camera position, direction, up should be in world space
      (that is, in TCastleRootTransform space,
      not in space local to this TCastleScene).
      These camera vectors are used to update TRenderedTextureNode, if any.

      This does not change current viewport or projection matrix. }
    procedure UpdateGeneratedTextures(const Shape: TX3DRendererShape;
      const TextureNode: TAbstractTextureNode;
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      const CurrentViewpoint: TAbstractViewpointNode;
      const CameraViewKnown: boolean;
      const CameraPosition, CameraDirection, CameraUp: TVector3);

    { Load GLSL shader for the ScreenEffect node.
      Makes sure that Node.ShaderLoaded is true.
      When changing Node.ShaderLoaded false to true tries to initialize
      the shader, setting Node.Shader if some GLSL program
      was successfully loaded.

      The GLSL program (TGLSLProgram) will be stored here,
      and will be automatically freed during UnprepareAll call. }
    procedure PrepareScreenEffect(Node: TScreenEffectNode);
  end;

const
  AllVboTypes = [Low(TVboType) .. High(TVboType)];

  rmPureGeometry = rmSolidColor deprecated 'use rmSolidColor';

var
  { Log renderer cache events. Allows to see how the cache performs.
    A @italic(lot) of log messages.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogRendererCache: boolean = false;

  { Log various renderer information.

    Meaningful only if you initialized log (see CastleLog unit) by InitializeLog first. }
  LogRenderer: boolean = false;

{$undef read_interface}

implementation

uses Math,
  CastleStringUtils, CastleGLVersion, CastleLog,
  X3DCameraUtils, CastleProjection, CastleRectangles, CastleTriangles,
  CastleCameras, CastleSceneInternalShape,
  CastleRenderContext, CastleInternalGLUtils;

{$define read_implementation}

{$I castleinternalrenderer_meshrenderer.inc}
{$I castleinternalrenderer_resource.inc}
{$I castleinternalrenderer_texture.inc}
{$I castleinternalrenderer_surfacetextures.inc}
{$I castleinternalrenderer_glsl.inc}

{ TGLRendererContextCache -------------------------------------------- }

constructor TGLRendererContextCache.Create;
var
  B: Boolean;
begin
  inherited;
  TextureImageCaches := TTextureImageCacheList.Create;
  TextureVideoCaches := TTextureVideoCacheList.Create;
  TextureCubeMapCaches := TTextureCubeMapCacheList.Create;
  Texture3DCaches := TTexture3DCacheList.Create;
  TextureDepthOrFloatCaches := TTextureDepthOrFloatCacheList.Create;
  for B := Low(Boolean) to High(Boolean) do
    ShapeCaches[B] := TShapeCacheList.Create;
  ProgramCaches := TShaderProgramCacheList.Create;
end;

destructor TGLRendererContextCache.Destroy;

{.$define ONLY_WARN_ON_CACHE_LEAK}

{$ifdef ONLY_WARN_ON_CACHE_LEAK}
  procedure Assert(const B: boolean; const S: string = '');
  begin
    if not B then
      WritelnWarning('VRML/X3D', 'GLRendererContextCache warning: ' + S);
  end;
{$endif}

var
  B: Boolean;
begin
  if TextureImageCaches <> nil then
  begin
    Assert(TextureImageCaches.Count = 0, 'Some references to texture images still exist' +
      ' when freeing TGLRendererContextCache');
    FreeAndNil(TextureImageCaches);
  end;

  if TextureVideoCaches <> nil then
  begin
    Assert(TextureVideoCaches.Count = 0, 'Some references to texture videos still exist' +
      ' when freeing TGLRendererContextCache');
    FreeAndNil(TextureVideoCaches);
  end;

  if TextureCubeMapCaches <> nil then
  begin
    Assert(TextureCubeMapCaches.Count = 0, 'Some references to texture cubemaps still exist' +
      ' when freeing TGLRendererContextCache');
    FreeAndNil(TextureCubeMapCaches);
  end;

  if Texture3DCaches <> nil then
  begin
    Assert(Texture3DCaches.Count = 0, 'Some references to texture 3D still exist' +
      ' when freeing TGLRendererContextCache');
    FreeAndNil(Texture3DCaches);
  end;

  if TextureDepthOrFloatCaches <> nil then
  begin
    Assert(TextureDepthOrFloatCaches.Count = 0, 'Some references to depth or float texture still exist' +
      ' when freeing TGLRendererContextCache');
    FreeAndNil(TextureDepthOrFloatCaches);
  end;

  for B := Low(Boolean) to High(Boolean) do
    if ShapeCaches[B] <> nil then
    begin
      Assert(ShapeCaches[B].Count = 0, Format('%d references to shapes still exist on ShapeCaches[%s] when freeing TGLRendererContextCache', [
        ShapeCaches[B].Count,
        BoolToStr(B, true)
        // ShapeCaches[B][0].ToString // not printed, risks further SEGFAULT during log output in case invalid reference remained on the list
      ]));
      FreeAndNil(ShapeCaches[B]);
    end;

  if ProgramCaches <> nil then
  begin
    Assert(ProgramCaches.Count = 0, 'Some references to GLSL programs still exist' +
      ' when freeing TGLRendererContextCache');
    FreeAndNil(ProgramCaches);
  end;

  inherited;
end;

function TGLRendererContextCache.TextureImage_IncReference(
  const TextureImage: TEncodedImage;
  const TextureFullUrl: string;
  const Filter: TTextureFilter;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage;
  const GUITexture: boolean;
  const FlipVertically: Boolean): TGLuint;
var
  I: Integer;
  TextureCached: TTextureImageCache;
begin
  for I := 0 to TextureImageCaches.Count - 1 do
  begin
    TextureCached := TextureImageCaches[I];

    { Once I had an idea to make here comparison with
      TextureImage = TextureCached^.Image. Since we have TTexturesVideosCache,
      so images from the same URL would have the same reference, so this
      would work perfectly, and make comparison with TextureURL obsolete, right ?

      But there's a problem with this: Image reference may be freed while
      the corresponding texture is still cached. In fact, it's normal in
      "The Castle", if you use FreeResources([frTexturesInNodes]) feature.
      Which means that Image reference may become invalid, and, worse,
      another Image may be potentially assigned the same reference.

      What would be needed is to automatically set cached Image reference
      to nil (and implement to not use Image reference if it's nil) if
      Image instance is freed. Something like FreeNotification.

      But still, the same FreeResources([frTexturesInNodes]) would prevent
      the texture from sharing, if we would free the texture prematurely
      and later load the same texture, with to different TCastleImage instance.

      For now, I don't use this idea, and rely on TextureFullUrl. }

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.FlipVertically = FlipVertically) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = TextureAnisotropy) and
       (TextureCached.Wrap = TextureWrap) and
       (TextureCached.GUITexture = GUITexture) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'TextureImage %s: %d', [TextureFullUrl, TextureCached.References]);
      Exit(TextureCached.GLName);
    end;
  end;

  { Initialize Result first, before calling TextureImageCaches.Add.
    That's because in case LoadGLTexture raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureImage_DecReference later). }
  Result := LoadGLTexture(TextureImage, Filter, TextureWrap, CompositeForMipmaps, GUITexture);

  TexParameterMaxAnisotropy(GL_TEXTURE_2D, TextureAnisotropy);

  TextureCached := TTextureImageCache.Create;
  TextureImageCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.FlipVertically := FlipVertically;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := TextureAnisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GUITexture := GUITexture;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache then
    WritelnLog('++', 'TextureImage %s: %d', [TextureFullUrl, 1]);
end;

procedure TGLRendererContextCache.TextureImage_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureImageCaches.Count - 1 do
    if TextureImageCaches[I].GLName = TextureGLName then
    begin
      Dec(TextureImageCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'TextureImage %s: %d', [TextureImageCaches[I].FullUrl,
                                    TextureImageCaches[I].References]);
      if TextureImageCaches[I].References = 0 then
      begin
        glFreeTexture(TextureImageCaches[I].GLName);
        TextureImageCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureImage_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.TextureVideo_IncReference(
  const TextureVideo: TVideo;
  const TextureFullUrl: string;
  const FlipVertically: Boolean;
  const Filter: TTextureFilter;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  const GUITexture: boolean): TGLVideo3D;
var
  I: Integer;
  TextureCached: TTextureVideoCache;
begin
  for I := 0 to TextureVideoCaches.Count - 1 do
  begin
    TextureCached := TextureVideoCaches[I];

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.FlipVertically = FlipVertically) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = TextureAnisotropy) and
       (TextureCached.Wrap = TextureWrap) and
       (TextureCached.GUITexture = GUITexture) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'TextureVideo %s: %d', [TextureFullUrl, TextureCached.References]);
      Exit(TextureCached.GLVideo);
    end;
  end;

  { Initialize Result first, before calling TextureVideoCaches.Add.
    That's because in case TGLVideo3D.Create raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureVideo_DecReference later). }
  Result := TGLVideo3D.Create(TextureVideo, Filter, TextureAnisotropy, TextureWrap, GUITexture);

  TextureCached := TTextureVideoCache.Create;
  TextureVideoCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.FlipVertically := FlipVertically;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := TextureAnisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GUITexture := GUITexture;
  TextureCached.References := 1;
  TextureCached.GLVideo := Result;

  if LogRendererCache then
    WritelnLog('++', 'TextureVideo %s: %d', [TextureFullUrl, 1]);
end;

procedure TGLRendererContextCache.TextureVideo_DecReference(
  const TextureVideo: TGLVideo3D);
var
  I: Integer;
begin
  for I := 0 to TextureVideoCaches.Count - 1 do
    if TextureVideoCaches[I].GLVideo = TextureVideo then
    begin
      Dec(TextureVideoCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'TextureVideo %s: %d', [
          TextureVideoCaches[I].FullUrl,
          TextureVideoCaches[I].References
        ]);
      if TextureVideoCaches[I].References = 0 then
      begin
        FreeAndNil(TextureVideoCaches[I].GLVideo);
        TextureVideoCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureVideo_DecReference: no reference ' +
    'found to texture %s', [PointerToStr(TextureVideo)]);
end;

function TGLRendererContextCache.TextureCubeMap_IncReference(
  const TextureFullUrl: string;
  const Filter: TTextureFilter;
  const Anisotropy: TGLfloat;
  const PositiveX, NegativeX,
        PositiveY, NegativeY,
        PositiveZ, NegativeZ: TEncodedImage;
  const CompositeForMipmaps: TCompositeImage): TGLuint;
var
  I: Integer;
  TextureCached: TTextureCubeMapCache;
  DisableMipmaps: Boolean;
begin
  if TextureFullUrl <> '' then // never share texture with FullUrl = '', e.g. from GeneratedCubeMapTexture
    for I := 0 to TextureCubeMapCaches.Count - 1 do
    begin
      TextureCached := TextureCubeMapCaches[I];

      if (TextureCached.FullUrl = TextureFullUrl) and
         (TextureCached.Filter = Filter) and
         (TextureCached.Anisotropy = Anisotropy) then
      begin
        Inc(TextureCached.References);
        if LogRendererCache then
          WritelnLog('++', 'Cube map %s: %d', [
            TextureFullUrl,
            TextureCached.References
          ]);
        Exit(TextureCached.GLName);
      end;
    end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_CUBE_MAP, Result);

  SetTextureFilter(GL_TEXTURE_CUBE_MAP, Filter);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GLFeatures.CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GLFeatures.CLAMP_TO_EDGE);

  glTextureCubeMap(Result,
    PositiveX, NegativeX,
    PositiveY, NegativeY,
    PositiveZ, NegativeZ,
    CompositeForMipmaps,
    Filter.NeedsMipmaps,
    DisableMipmaps);

  if DisableMipmaps then
  begin
    Filter.DisableMipmaps;
    SetTextureFilter(GL_TEXTURE_CUBE_MAP, Filter);
  end;

  TexParameterMaxAnisotropy(GL_TEXTURE_CUBE_MAP, Anisotropy);

  TextureCached := TTextureCubeMapCache.Create;
  TextureCubeMapCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := Anisotropy;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache then
    WritelnLog('++', 'Cube map %s: %d', [TextureFullUrl, 1]);
end;

procedure TGLRendererContextCache.TextureCubeMap_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureCubeMapCaches.Count - 1 do
    if TextureCubeMapCaches[I].GLName = TextureGLName then
    begin
      Dec(TextureCubeMapCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'Cube map %s: %d', [
          TextureCubeMapCaches[I].FullUrl,
          TextureCubeMapCaches[I].References
        ]);
      if TextureCubeMapCaches[I].References = 0 then
      begin
        glFreeTexture(TextureCubeMapCaches[I].GLName);
        TextureCubeMapCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureCubeMap_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.Texture3D_IncReference(
  const TextureFullUrl: string;
  const Filter: TTextureFilter;
  const Anisotropy: TGLfloat;
  const TextureWrap: TTextureWrap3D;
  const Image: TEncodedImage;
  const Composite: TCompositeImage): TGLuint;
var
  I: Integer;
  TextureCached: TTexture3DCache;
begin
  for I := 0 to Texture3DCaches.Count - 1 do
  begin
    TextureCached := Texture3DCaches[I];

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = Anisotropy) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'Texture3D %s: %d', [TextureFullUrl, TextureCached.References]);
      Exit(TextureCached.GLName);
    end;
  end;

  if not GLFeatures.Texture3D then
    raise Exception.Create('3D textures not supported by OpenGL(ES)');

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_3D, Result);

  glTextureImage3d(Result, Image, Filter, Composite);

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, TextureWrap.Data[0]);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, TextureWrap.Data[1]);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, TextureWrap.Data[2]);

  TexParameterMaxAnisotropy(GL_TEXTURE_3D, Anisotropy);

  TextureCached := TTexture3DCache.Create;
  Texture3DCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := Anisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache then
    WritelnLog('++', 'Texture3D %s: %d', [TextureFullUrl, 1]);
end;

procedure TGLRendererContextCache.Texture3D_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to Texture3DCaches.Count - 1 do
    if Texture3DCaches[I].GLName = TextureGLName then
    begin
      Dec(Texture3DCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'Texture3D %s: %d', [
          Texture3DCaches[I].FullUrl,
          Texture3DCaches[I].References
        ]);
      if Texture3DCaches[I].References = 0 then
      begin
        glFreeTexture(Texture3DCaches[I].GLName);
        Texture3DCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.Texture3D_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.TextureDepth_IncReference(
  const TextureFullUrl: String;
  const TextureWrap: TTextureWrap2D;
  const Width, Height: Cardinal): TGLuint;
var
  I: Integer;
  TextureCached: TTextureDepthOrFloatCache;
  Filter: TTextureFilter;
  ImageType: TGLenum;
  ImageFormat: string;
  ImageSize: Int64;
begin
  for I := 0 to TextureDepthOrFloatCaches.Count - 1 do
  begin
    TextureCached := TextureDepthOrFloatCaches[I];

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'Depth texture %s: %d', [
          TextureFullUrl,
          TextureCached.References
        ]);
      Exit(TextureCached.GLName);
    end;
  end;

  Assert(GLFeatures.TextureDepth);
  Assert(GLFeatures.TextureDepthCompare);

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  Filter.Minification := minLinear;
  Filter.Magnification := magLinear;
  SetTextureFilter(GL_TEXTURE_2D, Filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, TextureWrap.Data[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, TextureWrap.Data[1]);

  { OpenGLES allows only GL_UNSIGNED_SHORT or GL_UNSIGNED_INT for depth textures.
    This was true for OES_depth_texture, looks like it is true for OpenGLES 3.0 core too,
    based on "OpenGL ES 3.2 NVIDIA 515.86.01". }
  ImageType := {$ifdef OpenGLES} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_BYTE {$endif};
  ImageFormat := {$ifdef OpenGLES} 'depth-short' {$else} 'depth-byte' {$endif};
  ImageSize := {$ifdef OpenGLES} SizeOf(TGLushort) {$else} SizeOf(TGLbyte) {$endif}
    * Width * Height;

  { Do not init any texture image. Just initialize texture sizes
    and both internal and external formats to GL_DEPTH_COMPONENT_ARB
    (will match depth buffer precision). }
  glTexImage2d(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT,
    Width, Height, 0, GL_DEPTH_COMPONENT, ImageType, nil);
  TextureMemoryProfiler.Allocate(Result, '', ImageFormat, ImageSize, false,
    Width, Height, 1);

  if GLFeatures.TextureDepthCompare then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_REF_TO_TEXTURE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
  end else
    WritelnWarning('VRML/X3D', 'OpenGL doesn''t support GL_ARB_shadow, we cannot set depth comparison for depth texture');

  TextureCached := TTextureDepthOrFloatCache.Create;
  TextureDepthOrFloatCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.References := 1;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GLName := Result;

  if LogRendererCache then
    WritelnLog('++', 'Depth texture %s: %d', [TextureFullUrl, 1]);
end;

procedure TGLRendererContextCache.TextureDepth_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureDepthOrFloatCaches.Count - 1 do
    if TextureDepthOrFloatCaches[I].GLName = TextureGLName then
    begin
      Dec(TextureDepthOrFloatCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'Depth texture %s: %d', [
          TextureDepthOrFloatCaches[I].FullUrl,
          TextureDepthOrFloatCaches[I].References
        ]);
      if TextureDepthOrFloatCaches[I].References = 0 then
      begin
        glFreeTexture(TextureDepthOrFloatCaches[I].GLName);
        TextureDepthOrFloatCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureDepth_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.TextureFloat_IncReference(
  const TextureFullUrl: String;
  const Filter: TTextureFilter;
  const TextureWrap: TTextureWrap2D;
  const Width, Height: Cardinal;
  const Precision32: boolean): TGLuint;
{$ifndef OpenGLES}
var
  I: Integer;
  TextureCached: TTextureDepthOrFloatCache;
  InternalFormat: TGLenum;
begin
  for I := 0 to TextureDepthOrFloatCaches.Count - 1 do
  begin
    TextureCached := TextureDepthOrFloatCaches[I];

    if (TextureFullUrl <> '') and
       (TextureCached.FullUrl = TextureFullUrl) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache then
        WritelnLog('++', 'Float texture %s: %d', [TextureFullUrl, TextureCached.References]);
      Exit(TextureCached.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);
  SetTextureFilter(GL_TEXTURE_2D, Filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, TextureWrap.Data[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, TextureWrap.Data[1]);

  if Precision32 then
    InternalFormat := GL_RGB32F_ARB { same thing as GL_RGB_FLOAT32_ATI } else
    InternalFormat := GL_RGB16F_ARB { same thing as GL_RGB_FLOAT16_ATI };

  { Do not init any texture image. Just initialize texture sizes and formats. }
  glTexImage2d(GL_TEXTURE_2D, 0, InternalFormat,
    Width, Height, 0, GL_RGB, GL_FLOAT, nil);
  TextureMemoryProfiler.Allocate(Result, '', 'rgb-float',
    3 * SizeOf(TGLfloat) * Width * Height, false, Width, Height, 1);

  TextureCached := TTextureDepthOrFloatCache.Create;
  TextureDepthOrFloatCaches.Add(TextureCached);
  TextureCached.FullUrl := TextureFullUrl;
  TextureCached.References := 1;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GLName := Result;
  { Hm, we probably should store Filter, Precision32
    inside TextureCached as well... Ignore this, useless for now ---
    one Node will require only one float texture anyway. }

  if LogRendererCache then
    WritelnLog('++', 'Float texture %s: %d', [TextureFullUrl, 1]);
{$else}
begin
  raise Exception.Create('Float textures not available on OpenGL ES 2.0');
  Result := 0; // silence warning
{$endif}
end;

procedure TGLRendererContextCache.TextureFloat_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureDepthOrFloatCaches.Count - 1 do
    if TextureDepthOrFloatCaches[I].GLName = TextureGLName then
    begin
      Dec(TextureDepthOrFloatCaches[I].References);
      if LogRendererCache then
        WritelnLog('--', 'Float texture %s: %d', [
          TextureDepthOrFloatCaches[I].FullUrl,
          TextureDepthOrFloatCaches[I].References
        ]);
      if TextureDepthOrFloatCaches[I].References = 0 then
      begin
        glFreeTexture(TextureDepthOrFloatCaches[I].GLName);
        TextureDepthOrFloatCaches.Delete(I);
      end;
      CheckFreeWhenEmpty;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureFloat_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.Shape_IncReference(
  const Shape: TX3DRendererShape;
  const ARenderer: TGLRenderer): TShapeCache;
var
  FogEnabled, FogVolumetric: boolean;
  FogVolumetricDirection: TVector3;
  FogVolumetricVisibilityStart: Single;

  function IgnoreStateTransform: boolean;
  begin
    if { Force CacheIgnoresTransform to be false if our shape uses shaders.
         Shaders may depend on coordinates in eye space, which obviously
         may be different for shapes that differ even only on transform. }
      (Shape.Node <> nil) and
      (Shape.Node.Appearance <> nil) and
      (Shape.Node.Appearance.FdShaders.Count <> 0) then
      Exit(false);

    { If we use any features that (may) render shape differently
      if shape's transform (or other stuff handled outside arrays
      and castlerenderer) changes, then Result must be false. }
    Result := not FogVolumetric;
  end;

  function FogVolumetricEqual(
    const Volumetric1: boolean;
    const VolumetricDirection1: TVector3;
    const VolumetricVisibilityStart1: Single;
    const Volumetric2: boolean;
    const VolumetricDirection2: TVector3;
    const VolumetricVisibilityStart2: Single): boolean;
  begin
    Result := (Volumetric1 = Volumetric2) and
      ( (not Volumetric1) or
        ( TVector3.PerfectlyEquals(VolumetricDirection1, VolumetricDirection2) and
          (VolumetricVisibilityStart1 = VolumetricVisibilityStart2) ) );
  end;

var
  I: Integer;
  DisableSharedCache: Boolean;
  Caches: TShapeCacheList;
begin
  ARenderer.GetFog(Shape.Fog, FogEnabled, FogVolumetric,
    FogVolumetricDirection, FogVolumetricVisibilityStart);

  DisableSharedCache := TGLShape(Shape).DisableSharedCache;
  Caches := ShapeCaches[DisableSharedCache];

  if not DisableSharedCache then
    for I := 0 to Caches.Count - 1 do
    begin
      Result := Caches[I];
      if (Result.Geometry = Shape.Geometry) and
         Result.RenderOptions.EqualForShapeCache(ARenderer.RenderOptions) and
         Result.State.Equals(Shape.State, IgnoreStateTransform) and
         FogVolumetricEqual(
           Result.FogVolumetric,
           Result.FogVolumetricDirection,
           Result.FogVolumetricVisibilityStart,
           FogVolumetric,
           FogVolumetricDirection,
           FogVolumetricVisibilityStart) then
      begin
        Inc(Result.References);
        if LogRendererCache then
          WritelnLog('++', Result.ToString);
        Exit(Result);
      end;
    end;

  { not found, so create new }

  Result := TShapeCache.Create;
  Caches.Add(Result);
  Result.RenderOptions := ARenderer.RenderOptions;
  Result.Geometry := Shape.Geometry;
  Result.State := Shape.State;
  Result.FogVolumetric := FogVolumetric;
  Result.FogVolumetricDirection := FogVolumetricDirection;
  Result.FogVolumetricVisibilityStart := FogVolumetricVisibilityStart;
  Result.References := 1;

  if LogRendererCache then
    WritelnLog('++', Result.ToString);
end;

procedure TGLRendererContextCache.Shape_DecReference(const Shape: TX3DRendererShape;
  var ShapeCache: TShapeCache);
var
  I: Integer;
  DisableSharedCache: Boolean;
  Caches: TShapeCacheList;
begin
  DisableSharedCache := TGLShape(Shape).DisableSharedCache;
  Caches := ShapeCaches[DisableSharedCache];
  I := Caches.IndexOf(ShapeCache);

  if I <> -1 then
  begin
    Dec(ShapeCache.References);
    if LogRendererCache then
      WritelnLog('--', ShapeCache.ToString);
    if ShapeCache.References = 0 then
      Caches.Delete(I);
    ShapeCache := nil;
    CheckFreeWhenEmpty;
  end else
    raise EInternalError.Create(
      'TGLRendererContextCache.Shape_DecReference: no reference found');
end;

function TGLRendererContextCache.Program_IncReference(ARenderer: TGLRenderer;
  Shader: TShader; const ShapeNiceName: string): TShaderProgramCache;
var
  I: Integer;
begin
  for I := 0 to ProgramCaches.Count - 1 do
  begin
    Result := ProgramCaches[I];
    if Result.Hash = Shader.CodeHash then
    begin
      Inc(Result.References);
      if LogRendererCache then
        WritelnLog('++', 'Shader program (hash %s): %d', [Result.Hash.ToString, Result.References]);
      Exit(Result);
    end;
  end;

  Result := TShaderProgramCache.Create;
  ProgramCaches.Add(Result);
  Result.References := 1;
  Result.Hash := Shader.CodeHash;

  try
    Result.ShaderProgram := TX3DGLSLProgram.Create(ARenderer);
    Shader.LinkProgram(Result.ShaderProgram, ShapeNiceName);
  except on E: EGLSLError do
    begin
      FreeAndNil(Result.ShaderProgram);
      { Note: leave Result assigned and Result.Hash set,
        to avoid reinitializing this shader next time. }
      WritelnWarning('VRML/X3D', Format('Cannot use GLSL shader for shape "%s": %s',
        [ShapeNiceName, E.Message]));
    end;
  end;

  { We *must* have some GLSL shader when GLFeatures.EnableFixedFunction = false }
  if (not GLFeatures.EnableFixedFunction) and (Result.ShaderProgram = nil) then
  begin
    try
      Result.ShaderProgram := TX3DGLSLProgram.Create(ARenderer);
      Shader.LinkFallbackProgram(Result.ShaderProgram);
    except on E: EGLSLError do
      begin
        { We try to behave nicely when LinkFallbackProgram fails, although in practice
          Android's OpenGLES implementation may just crash... }
        FreeAndNil(Result.ShaderProgram);
        WritelnWarning('VRML/X3D', Format('Cannot use even fallback GLSL shader for shape "%s": %s',
          [ShapeNiceName, E.Message]));
      end;
    end;
  end;

  if LogRendererCache then
    WritelnLog('++', 'Shader program (hash %s): %d', [Result.Hash.ToString, Result.References]);
end;

procedure TGLRendererContextCache.Program_DecReference(var ProgramCache: TShaderProgramCache);
var
  I: Integer;
begin
  for I := 0 to ProgramCaches.Count - 1 do
  begin
    if ProgramCaches[I] = ProgramCache then
    begin
      Dec(ProgramCache.References);
      if LogRendererCache then
        WritelnLog('--', 'Shader program (hash %s): %d', [ProgramCache.Hash.ToString, ProgramCache.References]);
      if ProgramCache.References = 0 then
        ProgramCaches.Delete(I);
      ProgramCache := nil;
      CheckFreeWhenEmpty;
      Exit;
    end;
  end;

  raise EInternalError.Create(
    'TGLRendererContextCache.Program_DecReference: no reference found');
end;

function TGLRendererContextCache.Empty: Boolean;
begin
  Result :=
    (TextureImageCaches.Count = 0) and
    (TextureVideoCaches.Count = 0) and
    (TextureCubeMapCaches.Count = 0) and
    (Texture3DCaches.Count = 0) and
    (TextureDepthOrFloatCaches.Count = 0) and
    (ShapeCaches[false].Count = 0) and
    (ShapeCaches[true].Count = 0) and
    (ProgramCaches.Count = 0);
end;

procedure TGLRendererContextCache.CheckFreeWhenEmpty;
begin
  if FFreeWhenEmpty and Empty then
  begin
    FFreeWhenEmptyInstanceToNil^ := nil; // sets global GLContextCache to nil
    Self.Destroy;
  end;
end;

procedure TGLRendererContextCache.FreeWhenEmpty(const InstanceToNil: PObject);
begin
  if Self <> nil then // hack to allow calling on nil instance, like standard Free
  begin
    FFreeWhenEmptyInstanceToNil := InstanceToNil;
    FFreeWhenEmpty := true;
    CheckFreeWhenEmpty;
  end;
end;

{ TGLRenderer ---------------------------------------------------------- }

constructor TGLRenderer.Create(
  const RenderOptionsClass: TCastleRenderOptionsClass;
  const ACache: TGLRendererContextCache);
begin
  inherited Create;

  FRenderOptions := RenderOptionsClass.Create(nil);

  GLTextureNodes := TGLTextureNodes.Create(false);
  ScreenEffectPrograms := TGLSLProgramList.Create;
  TextureTransformUnitsUsedMore := TInt32List.Create;

  PreparedShader := TShader.Create;

  FCache := ACache;
  Assert(FCache <> nil);
end;

destructor TGLRenderer.Destroy;
begin
  UnprepareAll;

  FreeAndNil(TextureTransformUnitsUsedMore);
  FreeAndNil(GLTextureNodes);
  FreeAndNil(ScreenEffectPrograms);
  FreeAndNil(FRenderOptions);
  FreeAndNil(PreparedShader);

  FCache := nil; // we don't own cache

  inherited;
end;

{ TShapeCache ---------------------------------------------------------------- }

constructor TShapeCache.Create;
begin
  inherited;
  VboToReload := AllVboTypes;
end;

destructor TShapeCache.Destroy;
begin
  FreeAndNil(Arrays);
  GLContextClose;
  inherited;
end;

procedure TShapeCache.GLContextClose;
var
  I: TVboType;
begin
  if Vbo[vtCoordinate] <> 0 then
  begin
    { All Vbo must be zero, or none. }
    for I := Low(I) to High(I) do
      Assert(Vbo[I] <> 0);

    glDeleteBuffers(Ord(High(Vbo)) + 1, @Vbo);

    for I := Low(I) to High(I) do
      Vbo[I] := 0;
  end;

  FreeAndNil(Vao);
end;

procedure TShapeCache.InvalidateVertexData(const Changed: TVboTypes);
begin
  VboToReload := VboToReload + Changed;
end;

procedure TShapeCache.LoadArraysToVbo(const DynamicGeometry: boolean);
var
  DataUsage: TGLenum;
  NewVbos: boolean;

  { Bind Vbo buffer and load data. Updates AllocatedSize.

    Uses glBufferSubData if possible, as it may be faster than glBufferData
    (not confirmed by tests, but OpenGL docs suggest it:
    https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBufferSubData.xhtml
    "When replacing the entire data store, consider using glBufferSubData
    rather than completely recreating the data store with glBufferData.
    This avoids the cost of reallocating the data store. " ). }
  procedure BufferData(const VboType: TVboType;
    const Target: TGLenum; const Size: Cardinal; const Data: Pointer);
  begin
    { It may happen that Data = nil, because Arrays.AttributeSize may be 0
      (some shapes just don't need extra per-vertex attributes).

      We should not proceed then.
      Passing Data = nil to glBufferSubData can make actual bugs:
      see the testcase from
      https://github.com/castle-engine/castle-engine/issues/389 :
      open dragon.json, turn on "Dynamic Batching", run some animation
      -- it will update VBOs each frame, and on at least some systems
      (confirmed on 3 systems with NVidia on Windows) it will cause
      weird problems (like unrelated buffers are trashed with garbage).
      Strangely glBufferData doesn't have a problem with Data = nil,
      but we also avoid it. }
    if Data = nil then
    begin
      Assert(Size = 0);
      Exit;
    end;

    if NewVbos or
       (VboType in VboToReload) or
       { In normal circumstances, when vbo is already loaded,
         it should have already the appropriate size. But through VRML/X3D
         events, user may be able to actually incorrectly change
         coordinates, such that new ones have different size than the old ones
         --- in this case, VboToReload optimization fails, and we have
         to reload data (or we'll get terrible OpenGL segfaults later,
         as it tries to access non-existent data from vertex arrays). }
       (VboAllocatedSize[VboType] <> Size) then
    begin
      glBindBuffer(Target, Vbo[VboType]);
      if NewVbos or
        (VboAllocatedUsage <> DataUsage) or
        (VboAllocatedSize[VboType] <> Size) then
      begin
        glBufferData(Target, Size, Data, DataUsage);
        VboAllocatedSize[VboType] := Size;
      end else
        glBufferSubData(Target, 0, Size, Data);
    end;
  end;

  function VboTypesToStr(const VboTypes: TVboTypes): string;
  const
    Names: array [TVboType] of string =
    ( 'Coordinate', 'Attribute', 'Index' );
  var
    I: TVboType;
  begin
    Result := '';
    for I := Low(I) to High(I) do
      if I in VboTypes then
      begin
        if Result <> '' then Result := Result + ',';
        Result := Result + Names[I];
      end;
    Result := '[' + Result + ']';
  end;

begin
  if GLFeatures.VertexBufferObject then
  begin
    { In case of VBO not supported (ancient GPUs), we do not free data in Arrays,
      it will be used to provide data at each draw call. }
    Assert(not Arrays.DataFreed);

    NewVbos := Vbo[vtCoordinate] = 0;
    if NewVbos then
    begin
      glGenBuffers(Ord(High(Vbo)) + 1, @Vbo);
      if LogRenderer then
        WritelnLog('Renderer', Format('Creating and loading data to VBOs (%d,%d,%d)',
          [Vbo[vtCoordinate], Vbo[vtAttribute], Vbo[vtIndex]]));
    end else
    begin
      if LogRenderer then
        WritelnLog('Renderer', Format('Loading data to existing VBOs (%d,%d,%d), reloading %s',
          [Vbo[vtCoordinate], Vbo[vtAttribute], Vbo[vtIndex],
           VboTypesToStr(VboToReload)]));
    end;

    if DynamicGeometry then
      { GL_STREAM_DRAW is most suitable if we will modify this every frame,
        according to
        https://www.khronos.org/opengl/wiki/Buffer_Object
        https://computergraphics.stackexchange.com/questions/5712/gl-static-draw-vs-gl-dynamic-draw-vs-gl-stream-draw-does-it-matter
      }
      DataUsage := GL_STREAM_DRAW
    else
      DataUsage := GL_STATIC_DRAW;

    VboCoordinatePreserveGeometryOrder := Arrays.CoordinatePreserveGeometryOrder;

    BufferData(vtCoordinate, GL_ARRAY_BUFFER,
      Arrays.Count * Arrays.CoordinateSize, Arrays.CoordinateArray);

    BufferData(vtAttribute, GL_ARRAY_BUFFER,
      Arrays.Count * Arrays.AttributeSize, Arrays.AttributeArray);

    if Arrays.Indexes <> nil then
      BufferData(vtIndex, GL_ELEMENT_ARRAY_BUFFER,
        Arrays.Indexes.Count * SizeOf(TGeometryIndex), Arrays.Indexes.L);

    VboAllocatedUsage := DataUsage;

    Arrays.FreeData;

    if GLFeatures.VertexArrayObject and (Vao = nil) then
      Vao := TVertexArrayObject.Create;
  end;

  VboToReload := [];
end;

function TShapeCache.ToString: String;
var
  ShapeNodeName: String;
begin
  if State.ShapeNode <> nil then
    ShapeNodeName := ' ' + State.ShapeNode.X3DName
  else
    ShapeNodeName := '';
  Result := Format('Shape%s %s (%s): %d', [
    ShapeNodeName,
    PointerToStr(Self),
    Geometry.X3DType,
    References
  ]);
end;

{ TShaderProgramCache -------------------------------------------------------- }

destructor TShaderProgramCache.Destroy;
begin
  FreeAndNil(ShaderProgram);
  inherited;
end;

{ TX3DRendererShape --------------------------------------------------------- }

destructor TX3DRendererShape.Destroy;
begin
  FreeAndNil(MirrorPlaneUniforms);
  inherited;
end;

procedure TX3DRendererShape.LoadArraysToVbo;
begin
  Assert(Cache <> nil);
  Cache.LoadArraysToVbo(DynamicGeometry);
end;

{ Prepare/Unprepare[All] ------------------------------------------------------- }

function TGLRenderer.PrepareTexture(Shape: TShape; Texture: TAbstractTextureNode): Pointer;
begin
  GLTextureNodes.Prepare(Shape.State, Texture, Self);
  Result := nil;
end;

procedure TGLRenderer.Prepare(Shape: TX3DRendererShape);
begin
  Shape.EnumerateTextures({$ifdef FPC}@{$endif}PrepareTexture);
end;

procedure TGLRenderer.PrepareScreenEffect(Node: TScreenEffectNode);

  procedure PrepareIDecls(const IDecls: TX3DInterfaceDeclarationList;
    State: TX3DGraphTraverseState);

    { If TextureNode <> @nil and is a texture node, prepare it. }
    procedure PrepareTexture(TextureNode: TX3DNode);
    begin
      if (TextureNode <> nil) and
         (TextureNode is TAbstractTextureNode) then
        GLTextureNodes.Prepare(State, TAbstractTextureNode(TextureNode), Self);
    end;

  var
    I, J: Integer;
    UniformField: TX3DField;
  begin
    if IDecls = nil then Exit; { ignore nodes without interface declararations }

    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
        begin
          PrepareTexture(TSFNode(UniformField).Value);
        end else
        if UniformField is TMFNode then
        begin
          for J := 0 to TMFNode(UniformField).Count - 1 do
            PrepareTexture(TMFNode(UniformField)[J]);
        end;
      end;
    end;
  end;

  { Prepare all textures within X3D "interface declarations" of the given Nodes. }
  procedure PrepareIDeclsList(Nodes: TMFNode; State: TX3DGraphTraverseState);
  var
    I: Integer;
  begin
    for I := 0 to Nodes.Count - 1 do
      PrepareIDecls(Nodes[I].InterfaceDeclarations, State);
  end;

var
  Shader: TShader;
  ShaderProgram: TX3DGLSLProgram;
  ShaderNode: TComposedShaderNode;
  DummyCamera: TRenderingCamera;
begin
  if not Node.ShaderLoaded then
  begin
    Assert(Node.Shader = nil);
    Node.ShaderLoaded := true;

    { Note: Strictly speaking, we should not need to check
      "not GLFeatures.EnableFixedFunction" below.
      Without this check, TX3DGLSLProgram.Create below will raise an exception
      which will cause to abort.

      But:
      - This is a bit inefficient way to abort.
      - It causes unknown issues: view3dscene --debug-fixed-function,
        enable any (like "visualize depth") screen effect,
        and you may experience crash in TCastleScene.ScreenEffectsCount
        as if ScreenEffectNodes was nil.

        Not debugged to the end:
        situation seems impossible and in debugger mysteriously
        doesn't occur. And logging inconclusive:

          WritelnLog('TCastleScene.ScreenEffectsCount %s', [BoolToStr(ScreenEffectNodes <> nil, true)]);

        sometimes shown we indeed have nil right before crash,
        but sometimes not...
    }

    if Node.FdEnabled.Value and (not GLFeatures.EnableFixedFunction) then
    begin
      { make sure that textures inside shaders are prepared }
      PrepareIDeclsList(Node.FdShaders, Node.StateForShaderPrepare);

      Shader := TShader.Create;
      try
        DummyCamera := TRenderingCamera.Create;
        try
          Shader.RenderingCamera := DummyCamera;

          { for ScreenEffect, we require that some ComposedShader was present.
            Rendering with default TShader shader makes no sense. }
          if Shader.EnableCustomShaderCode(Node.FdShaders, ShaderNode) then
          try
            ShaderProgram := TX3DGLSLProgram.Create(Self);
            Shader.AddScreenEffectCode(Node.FdNeedsDepth.Value);
            Shader.LinkProgram(ShaderProgram, Node.NiceName);

            { We have to ignore invalid uniforms, as it's normal that when
              rendering screen effect we will pass some screen_* variables
              that you will not use. }
            ShaderProgram.UniformMissing := umIgnore;

            Node.Shader := ShaderProgram;
            ScreenEffectPrograms.Add(ShaderProgram);
          except on E: EGLSLError do
            begin
              FreeAndNil(ShaderProgram);
              WritelnWarning('VRML/X3D', Format('Cannot use GLSL shader for ScreenEffect: %s',
                [E.Message]));
            end;
          end;
        finally FreeAndNil(DummyCamera) end;
      finally FreeAndNil(Shader) end;
    end;
  end;
end;

procedure TGLRenderer.UnprepareTexture(Node: TAbstractTextureNode);
begin
  GLTextureNodes.Unprepare(Node);
end;

procedure TGLRenderer.UnprepareAll;
begin
  { Secure here in case various fields are nil, in case
    - we are in the middle of TGLRenderer constructor
      (possible if TCastleRenderOptions.OnCreate fires)
    - TGLRenderer constructor failed, and now we're in destructor.
    Testcase: castle-game. }

  if GLTextureNodes <> nil then
    GLTextureNodes.UnprepareAll;
  if ScreenEffectPrograms <> nil then
    ScreenEffectPrograms.Count := 0; { this will free programs inside }
end;

function TGLRenderer.BumpMapping: TBumpMapping;
begin
  if (RenderOptions.BumpMapping <> bmNone) and
     RenderOptions.Textures and
     (RenderOptions.Mode = rmFull) and
     GLFeatures.UseMultiTexturing and
     GLFeatures.Shaders then
    Result := RenderOptions.BumpMapping else
    Result := bmNone;
end;

{ Render ---------------------------------------------------------------------- }

class procedure TGLRenderer.ActiveTexture(const TextureUnit: Cardinal);
begin
  if GLFeatures.UseMultiTexturing then
    glActiveTexture(GL_TEXTURE0 + TextureUnit);
end;

class procedure TGLRenderer.DisableTexture(const TextureUnit: Cardinal);
begin
  if GLFeatures.EnableFixedFunction then
  begin
    { This must be synchronized, and disable all that can be enabled
      by TShape.EnableTexture }
    ActiveTexture(TextureUnit);
    DisableCurrentTexture;
  end;
end;

class procedure TGLRenderer.DisableCurrentTexture;
begin
  GLEnableTexture(etNone);
end;

procedure TGLRenderer.GetFog(const AFogFunctionality: TFogFunctionality;
  out Enabled, Volumetric: boolean;
  out VolumetricDirection: TVector3;
  out VolumetricVisibilityStart: Single);
begin
  Enabled := (RenderOptions.Mode = rmFull) and
    (AFogFunctionality <> nil) and
    (AFogFunctionality.VisibilityRange <> 0.0);
  Volumetric := Enabled and
    AFogFunctionality.Volumetric and
    // Volumetric fog is now only supported by shaders
    (not GLFeatures.EnableFixedFunction);

  if Volumetric then
  begin
    VolumetricVisibilityStart :=
      AFogFunctionality.VolumetricVisibilityStart *
      AFogFunctionality.TransformScale;
    VolumetricDirection := AFogFunctionality.VolumetricDirection;
  end else
  begin
    { whatever, just set them to any determined values }
    VolumetricVisibilityStart := 0;
    VolumetricDirection := TVector3.Zero;
  end;
end;

class procedure TGLRenderer.ViewportRenderBegin;
{$ifndef OpenGLES}
var
  I: Integer;
{$endif}
begin
  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    { ------------------------------------------------------------------------

      Set fixed-function state that can be assumed by TGLRenderer,
      and may change during rendering but will always be restored later.
      So this secures from other rendering code (outside of TGLRenderer)
      setting this OpenGL state to something unexpected.

      E.g. shape may assume that GL_COLOR_MATERIAL is disabled.
      If a shape does glEnable(GL_COLOR_MATERIAL), then it will for sure also do glDisable(GL_COLOR_MATERIAL)
      at the end, so that state after shape rendering stays as it was.

      This generally sets "typical" OpenGL state, and can be saved/restored during TCastleScene rendering.
    }

    // set texture unit to 0, this determines what is affected by glDisable(GL_TEXTURE_GEN_xxx)
    if GLFeatures.UseMultiTexturing then
    begin
      ActiveTexture(0);
      glClientActiveTexture(GL_TEXTURE0);
    end;
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_GEN_R);
    glDisable(GL_TEXTURE_GEN_Q);

    glDisable(GL_COLOR_MATERIAL);

    for I := 0 to GLFeatures.MaxTextureUnits - 1 do
      DisableTexture(I);

    { ------------------------------------------------------------------------

      Set fixed-function state that can be assumed by TGLRenderer,
      and is constant for all shapes and for all scenes (so it doesn't even depend on RenderOptions),
      and we don't care that we just changed it also
      for stuff outside of TGLRenderer (so we make no attempt to restore
      it at ViewportRenderEnd or even track previous state).

      This generally sets "typical" OpenGL state, and remains constant during TCastleScene rendering. }

    { About using color material (what does glColor mean for materials):
      - We always set diffuse material component from the color.
        This satisfies all cases.
      - TColorPerVertexCoordinateRenderer.RenderCoordinateBegin
        takes care of actually enabling COLOR_MATERIAL, it assumes that
        the state is as set below.
      - We never change glColorMaterial during rendering,
        so no need to call this in RenderEnd. }
    glColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE);

    glShadeModel(GL_SMOOTH);

    { While we don't *really need* to enable GL_NORMALIZE,
      as we always try to provide normalized normals,
      but avoiding GL_NORMALIZE doesn't give us *any* performance benefits.

      And enabling GL_NORMALIZE:
      - make us correct even when glTF/X3D files provided unnormalized normals.
      - in case the object is scaled.
      - it is also consistent with what shader pipeline is doing, it is always
        normalizing normals. }
    glEnable(GL_NORMALIZE);

    glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  end;
  {$endif}
end;

class procedure TGLRenderer.ViewportRenderEnd;
{$ifndef OpenGLES}
var
  I: Integer;
{$endif}
begin
  { Restore defaults, for other rendering code outside of TGLRenderer.

    These RenderContext properties are always set before rendering every shape,
    so ViewportRenderBegin doesn't care to set them to any value,
    and we just reset them in ViewportRenderEnd. }
  RenderContext.CullFace := false;
  RenderContext.FrontFaceCcw := true;
  RenderContext.CurrentProgram := nil;
  RenderContext.FixedFunctionAlphaTestDisable;
  RenderContext.FixedFunctionLighting := false;

  { Restore defaults, for other rendering code outside of TGLRenderer.

    These RenderContext properties are always set in RenderBegin,
    so they are constant for scene shapes.
    So ViewportRenderBegin doesn't care to set them to any value,
    and we just reset them in ViewportRenderEnd. }
  RenderContext.PointSize := 1;
  RenderContext.LineWidth := 1;
  RenderContext.DepthTest := false;

  { Restore default fixed-function state, for other rendering code outside of TGLRenderer.

    These state things are adjusted by each shape,
    and shape rendering assumes that at the beginning it is undefined
    (so we don't care to set it in ViewportRenderBegin),
    and shape doesn't restore it (because next shape will adjust it anyway).
    So we reset them at the end to not affect other objects rendered outside of TGLRenderer.

    This generally sets "typical" OpenGL state,
    for things that may have non-standard values during TCastleScene rendering,
    and each shape is prepared that it has non-standard value (but rendering outside
    of TGLRenderer may not be prepared for it). }

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    for I := 0 to GLFeatures.MaxLights - 1 do
      glDisable(GL_LIGHT0 + I);
    glDisable(GL_FOG);

    { Restore active texture unit to 0.
      This also sets texture unit for subsequent glTexEnvi call. }
    if GLFeatures.UseMultiTexturing then
    begin
      ActiveTexture(0);
      {$ifndef OpenGLES}
      glClientActiveTexture(GL_TEXTURE0);
      {$endif}
    end;

    { Reset GL_TEXTURE_ENV, otherwise it may be left GL_COMBINE
      after rendering X3D model using MultiTexture. }
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  end;
  {$endif}
end;

procedure TGLRenderer.RenderBegin(
  const AGlobalLights: TLightInstancesList;
  const ARenderingCamera: TRenderingCamera;
  const LightRenderEvent: TLightRenderEvent;
  const AInternalPass: TInternalRenderingPass;
  const AInternalScenePass: TInternalSceneRenderingPass;
  const AUserPass: TUserRenderingPass;
  const AStatistics: PRenderStatistics);

  { Combine a set of numbers (each in their own range) into one unique number.
    This is like combining a couple of digits into a whole number,
    but each digit is in a separate numeric system.

    This is used to calculate TTotalRenderingPass from a couple of numbers.
    The goal is that changing *any* number must also change the result,
    so that result is a unique representation of all the numbers. }
  function GetTotalPass(
    const Digits: array of Cardinal;
    const Ranges: array of Cardinal): Cardinal;
  var
    I: Integer;
    Multiplier: Cardinal;
  begin
    Result := 0;
    Multiplier := 1;
    Assert(Length(Digits) = Length(Ranges));
    for I := 0 to Length(Digits) - 1 do
    begin
      Result := Result + Digits[I] * Multiplier;
      Multiplier := Multiplier * Ranges[I];
    end;
  end;

begin
  GlobalLights := AGlobalLights;
  RenderingCamera := ARenderingCamera;
  Statistics := AStatistics;
  Assert(RenderingCamera <> nil);

  Pass := GetTotalPass(
    [     AInternalPass,       AInternalScenePass,       AUserPass ],
    [High(AInternalPass), High(AInternalScenePass), High(AUserPass)]);

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    glPushMatrix;

    if RenderOptions.Mode = rmSolidColor then
      glColorv(RenderOptions.SolidColor);
  end;
  {$endif}

  Assert(FogFunctionality = nil);
  Assert(not FogEnabled);

  LightsRenderer := TLightsRenderer.Create(LightRenderEvent, RenderOptions.MaxLightsPerShape);
  LightsRenderer.RenderingCamera := RenderingCamera;

  { Set RenderContext properties, constant for all shapes within this scene.
    Note that all TCastleTransform rendering code that is not inside TCastleScene
    (e.g. custom TCastleTransform descendant using TCastleRenderUnlitMesh)
    must be prepared that this state may be whatever, and so it must save, set and restore this state.

    ViewportRenderEnd resets these things, so that this state does not "leak"
    for stuff rendered outside of TCastleViewport. }
  RenderContext.PointSize := RenderOptions.PointSize;
  RenderContext.LineWidth := RenderOptions.LineWidth;
  RenderContext.DepthTest := RenderOptions.DepthTest;
end;

procedure TGLRenderer.RenderEnd;
begin
  { Tests:
  Writeln('LightsRenderer stats: light setups done ',
    LightsRenderer.Statistics[true], ' vs avoided ',
    LightsRenderer.Statistics[false]); }

  FreeAndNil(LightsRenderer);

  FogFunctionality := nil;
  FogEnabled := false;

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    glPopMatrix;
  end;
  {$endif}

  { We need this in RenderEnd, not only in ViewportRenderEnd.
    Otherwise some operations could make the current texture unit not bound
    to any valid texture, causing the glUniform1i setting shader uniform for texture
    to raise OpenGL errors.
    Testcase: 3d_model_viewer template. }
  RenderContext.CurrentProgram := nil;

  RenderingCamera := nil;
  Statistics := nil;
end;

procedure TGLRenderer.RenderShape(const Shape: TX3DRendererShape);

  function ShapeUsesEnvironmentLight(const Shape: TX3DRendererShape): boolean;
  var
    I: Integer;
    Lights: TLightInstancesList;
  begin
    Lights := Shape.State.Lights;
    if Lights <> nil then
      for I := 0 to Lights.Count - 1 do
        if Lights.List^[I].Node is TEnvironmentLightNode then
          Exit(true);
    Result := false;
  end;

  function ShapeMaybeUsesShadowMaps(const Shape: TX3DRendererShape): boolean;
  var
    Tex, SubTexture: TX3DNode;
    I: Integer;
  begin
    Result := false;
    if (Shape.Node <> nil) and
       (Shape.Node.Appearance <> nil) then
    begin
      Tex := Shape.Node.Appearance.Texture;

      if Tex is TGeneratedShadowMapNode then
        Exit(true);

      if Tex is TMultiTextureNode then
      begin
        for I := 0 to TMultiTextureNode(Tex).FdTexture.Count - 1 do
        begin
          SubTexture := TMultiTextureNode(Tex).FdTexture[I];
          if SubTexture is TGeneratedShadowMapNode then
            Exit(true);
        end;
      end;
    end;
  end;

  function ShapeMaterialRequiresPhongShading(const Shape: TX3DRendererShape): boolean;
  begin
    Result :=
      (Shape.Node <> nil) and
      (Shape.Node.Material is TPhysicalMaterialNode);
  end;

var
  PhongShading: boolean;
  Shader: TShader;
begin
  { instead of TShader.Create, reuse existing PreparedShader for speed }
  Shader := PreparedShader;
  Shader.Clear;
  Shader.RenderingCamera := RenderingCamera;

  { calculate PhongShading }
  PhongShading := RenderOptions.PhongShading;
  { if Shape specifies Shading = Gouraud or Phong, use it }
  if Shape.Node <> nil then
    if Shape.Node.Shading = shPhong then
      PhongShading := true
    else
    if Shape.Node.Shading = shGouraud then
      PhongShading := false;
  { if some feature requires PhongShading, make it true }
  if ShapeMaybeUsesSurfaceTexture(Shape) or
     ShapeMaybeUsesShadowMaps(Shape) or
     ShapeMaterialRequiresPhongShading(Shape) or
     ShapeUsesEnvironmentLight(Shape) or
     (not Shape.Geometry.Solid) { two-sided lighting required by solid=FALSE } then
    PhongShading := true;
  { As an exception, in case of buggy shader pipeline, force using Gouraud shading.
    We cannot do Phong shading in this case, as it implies using "pure shader pipeline".
    See https://forum.castle-engine.io/t/win64-lcl-simple-shapes-something-got-broken/579/14 }
  if GLVersion.BuggyPureShaderPipeline then
    PhongShading := false;

  Shader.Initialize(PhongShading);

  if PhongShading then
    Shader.ShapeRequiresShaders := true;

  Shader.ShapeBoundingBoxInSceneEvent := {$ifdef FPC}@{$endif} Shape.BoundingBox;
  Shader.SceneTransform := Shape.SceneTransform;
  Shader.ShadowSampling := RenderOptions.ShadowSampling;
  RenderShapeLineProperties(Shape, Shader);
end;

procedure TGLRenderer.RenderShapeLineProperties(const Shape: TX3DRendererShape;
  const Shader: TShader);
var
  LP: TLinePropertiesNode;
begin
  if Shape.Node <> nil then { Shape.Node is nil for VRML <= 1.0 }
    LP := Shape.Node.LineProperties else
    LP := nil;
  if (LP <> nil) and LP.FdApplied.Value then
  begin
    RenderContext.LineWidth := Max(1.0, RenderOptions.LineWidth * LP.FdLineWidthScaleFactor.Value);
    RenderContext.LineType := LP.LineType;
  end else
  begin
    RenderContext.LineWidth := RenderOptions.LineWidth;
    RenderContext.LineType := ltSolid;
  end;

  RenderShapeMaterials(Shape, Shader);
end;

procedure TGLRenderer.RenderShapeMaterials(const Shape: TX3DRendererShape;
  const Shader: TShader);

  {$I castleinternalrenderer_materials.inc}

begin
  RenderMaterialsBegin;
  RenderShapeLights(Shape, Shader, Lighting);
end;

procedure TGLRenderer.RenderShapeLights(const Shape: TX3DRendererShape;
  const Shader: TShader;
  const Lighting: boolean);
var
  SceneLights: TLightInstancesList;
begin
  { This is done after setting Shader.MaterialSpecularColor
    by RenderMaterialsBegin,
    as MaterialSpecularColor must be already set during Shader.EnableLight. }

  Shader.SceneModelView := Shape.SceneModelView;

  { When lighting is off (for either shaders or fixed-function),
    there is no point in setting up lights. }
  if Lighting then
  begin
    if RenderOptions.ReceiveSceneLights then
      SceneLights := Shape.State.Lights
    else
      SceneLights := nil;

    LightsRenderer.Render(GlobalLights, SceneLights, Shader);
  end;

  RenderShapeFog(Shape, Shader, Lighting);
end;

procedure TGLRenderer.RenderShapeFog(const Shape: TX3DRendererShape;
  const Shader: TShader;
  const Lighting: boolean);

const
  FogCoordinateSource: array [boolean { volumetric }] of TFogCoordinateSource =
  ( fcDepth, fcPassedCoordinate );

  { Set OpenGL fog based on given fog node. Returns also fog parameters,
    like GetFog. }
  procedure RenderFog(const AFogFunctionality: TFogFunctionality;
    out Volumetric: boolean;
    out VolumetricDirection: TVector3;
    out VolumetricVisibilityStart: Single);
  var
    VisibilityRangeScaled: Single;
  begin
    GetFog(AFogFunctionality, FogEnabled, Volumetric, VolumetricDirection, VolumetricVisibilityStart);

    if FogEnabled then
    begin
      Assert(AFogFunctionality <> nil);

      VisibilityRangeScaled :=
        AFogFunctionality.VisibilityRange *
        AFogFunctionality.TransformScale;

      { calculate FogType and other Fog parameters }
      FogType := AFogFunctionality.FogType;
      FogColor := AFogFunctionality.Color;
      FogVisibilityRangeScaled := VisibilityRangeScaled;
    end;
  end;

const
  FogConstantDensityFactor = 3.0; //< Necessary for exponential fog in fixed-function
begin
  { Enable / disable fog and set fog parameters if needed }
  if FogFunctionality <> Shape.Fog then
  begin
    FogFunctionality := Shape.Fog;
    RenderFog(FogFunctionality, FogVolumetric,
      FogVolumetricDirection, FogVolumetricVisibilityStart);

    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
    begin
      { Set fixed-function fog parameters. }
      if FogEnabled then
      begin
        glFogv(GL_FOG_COLOR, Vector4(FogColor, 1.0));
        case FogType of
          ftLinear:
            begin
              glFogi(GL_FOG_MODE, GL_LINEAR);
              glFogf(GL_FOG_START, 0);
              glFogf(GL_FOG_END, FogVisibilityRangeScaled);
            end;
          ftExponential:
            begin
              glFogi(GL_FOG_MODE, GL_EXP);
              glFogf(GL_FOG_DENSITY, FogConstantDensityFactor / FogVisibilityRangeScaled);
            end;
          {$ifndef COMPILER_CASE_ANALYSIS}
          else raise EInternalError.Create('TGLRenderer.RenderShapeFog:FogType? 2');
          {$endif}
        end;
        { We want to be able to render any scene --- so we have to be prepared
          that fog interpolation has to be corrected for perspective. }
        glHint(GL_FOG_HINT, GL_NICEST);
        glEnable(GL_FOG);
      end else
        glDisable(GL_FOG);
    end;
    {$endif}
  end;

  if FogEnabled then
    Shader.EnableFog(FogType, FogCoordinateSource[FogVolumetric],
      FogColor, FogVisibilityRangeScaled);
  RenderShapeTextureTransform(Shape, Shader, Lighting);
end;

procedure TGLRenderer.RenderShapeTextureTransform(const Shape: TX3DRendererShape;
  const Shader: TShader; const Lighting: boolean);
var
  TextureTransform: TAbstractTextureTransformNode;
  Child: TX3DNode;
  Transforms: TMFNode;
  I, FirstTexUnit: Integer;
  State: TX3DGraphTraverseState;
  Matrix: TMatrix4;
begin
  TextureTransformUnitsUsed := 0;
  TextureTransformUnitsUsedMore.Count := 0;

  { in case of FontTextureNode <> nil, 1 less texture unit is available.
    Everything we do, must be shifted by 1 texture unit. }
  if Shape.OriginalGeometry.FontTextureNode <> nil then
    FirstTexUnit := 1 else
    FirstTexUnit := 0;

  State := Shape.State;

  if (State.ShapeNode = nil { VRML 1.0, always some texture transform }) or
     (State.ShapeNode.TextureTransform <> nil { VRML 2.0 with tex transform }) then
  begin
    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
    begin
      glMatrixMode(GL_TEXTURE);
    end;
    {$endif}

    { We work assuming that texture matrix before RenderShape was identity.
      Texture transform encoded in VRML/X3D will be multiplied by this.

      This allows the programmer to eventually transform all textures
      by placing non-identity texture matrix (just like a programmer
      can transform whole rendered model by changing modelview matrix).
      So this is a good thing.

      Additional advantage is that we do not have to explicitly "clear"
      the texture matrix if it's an identity transformation in VRML/X3D.
      We just let it stay like it was.

      This also nicely cooperates with X3D MultiTextureTransform desired
      behavior: "If there are too few entries in the textureTransform field,
      identity matrices shall be used for all remaining undefined channels.".
      Which means that looking at MultiTextureTransform node, we know exactly
      on which texture units we have to apply transform: we can leave
      the remaining texture units as they were, regardless of whether
      MultiTexture is used at all and regardless of how many texture units
      are actually used by MultiTexture. }

    { TODO: for bump mapping, TextureTransform should be done on more than one texture unit. }

    if State.ShapeNode = nil then
    begin
      { No multitexturing in VRML 1.0, just always transform first tex unit. }
      if FirstTexUnit < GLFeatures.MaxTextureUnits then
      begin
        TextureTransformUnitsUsed := 1;
        if GLFeatures.EnableFixedFunction then
        begin
          {$ifndef OpenGLES}
          ActiveTexture(FirstTexUnit);
          glPushMatrix;
          glMultMatrix(State.TextureTransform);
          {$endif}
        end;
        Shader.EnableTextureTransform(FirstTexUnit, State.TextureTransform);
      end;
    end else
    begin
      TextureTransform := State.ShapeNode.TextureTransform;
      if TextureTransform <> nil then
      begin
        if TextureTransform is TMultiTextureTransformNode then
        begin
          Transforms := TMultiTextureTransformNode(TextureTransform).FdTextureTransform;

          { Multitexturing, so use as many texture units as there are children in
            MultiTextureTransform.textureTransform.
            Cap by available texture units. }
          TextureTransformUnitsUsed := Min(Transforms.Count,
            GLFeatures.MaxTextureUnits - FirstTexUnit);

          for I := 0 to TextureTransformUnitsUsed - 1 do
          begin
            if GLFeatures.EnableFixedFunction then
            begin
              {$ifndef OpenGLES}
              ActiveTexture(FirstTexUnit + I);
              glPushMatrix;
              {$endif}
            end;
            Child := Transforms[I];
            if (Child <> nil) and
               (Child is TAbstractTextureTransformNode) then
            begin
              if Child is TMultiTextureTransformNode then
                WritelnWarning('VRML/X3D', 'MultiTextureTransform.textureTransform list cannot contain another MultiTextureTransform instance') else
              begin
                Matrix := TAbstractTextureTransformNode(Child).TransformMatrix;
                if GLFeatures.EnableFixedFunction then
                begin
                  {$ifndef OpenGLES}
                  glMultMatrix(Matrix);
                  {$endif}
                end;
                Shader.EnableTextureTransform(FirstTexUnit + I, Matrix);
              end;
            end;
          end;
        end else
        { Check below is done because X3D specification explicitly
          says that MultiTexture is affected *only* by MultiTextureTransform,
          that is normal TextureTransform and such is ignored (treated
          like identity transform, *not* applied to 1st texture unit).

          By the way, we don't do any texture transform if Texture = nil,
          since then no texture is used anyway.

          TODO: what to do with CommonSurfaceShader ?

          TODO: fix for new texture channels, where one TextureTransform
          should work like MultiTextureTransform with one item. }
        if (State.MainTexture <> nil) and
           (not (State.MainTexture is TMultiTextureNode)) then
        begin
          if FirstTexUnit < GLFeatures.MaxTextureUnits then
          begin
            TextureTransformUnitsUsed := 1;
            Matrix := TextureTransform.TransformMatrix;
            if GLFeatures.EnableFixedFunction then
            begin
              {$ifndef OpenGLES}
              ActiveTexture(FirstTexUnit);
              glPushMatrix;
              glMultMatrix(Matrix);
              {$endif}
            end;
            Shader.EnableTextureTransform(FirstTexUnit, Matrix);
          end;
        end;
      end;
    end;

    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction then
    begin
      { restore GL_MODELVIEW }
      glMatrixMode(GL_MODELVIEW);
    end;
    {$endif}
  end;

  RenderShapeClipPlanes(Shape, Shader, Lighting);

  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    if (TextureTransformUnitsUsed <> 0) or
       (TextureTransformUnitsUsedMore.Count <> 0) then
    begin
      glMatrixMode(GL_TEXTURE);

      for I := 0 to TextureTransformUnitsUsed - 1 do
      begin
        { This code is Ok also when not GLFeatures.UseMultiTexturing: then
          TextureTransformUnitsUsed for sure is <= 1 and ActiveTexture
          will be simply ignored. }
        ActiveTexture(FirstTexUnit + I);
        glPopMatrix;
      end;

      for I := 0 to TextureTransformUnitsUsedMore.Count - 1 do
      begin
        ActiveTexture(TextureTransformUnitsUsedMore.List^[I]);
        glPopMatrix;
      end;

      { restore GL_MODELVIEW }
      glMatrixMode(GL_MODELVIEW);
    end;
    {$endif}
  end;
end;

procedure TGLRenderer.RenderShapeClipPlanes(const Shape: TX3DRendererShape;
  const Shader: TShader;
  const Lighting: boolean);
var
  { How many clip planes were enabled (and so, how many must be disabled
    at the end). }
  ClipPlanesEnabled: Cardinal;

  { Initialize OpenGL clip planes, looking at ClipPlanes list.
    We know we're inside GL_MODELVIEW mode,
    and we know all clip planes are currently disabled. }
  procedure ClipPlanesBegin(ClipPlanes: TClipPlaneList);
  var
    I: Integer;
    ClipPlane: PClipPlane;
  begin
    ClipPlanesEnabled := 0;
    { GLMaxClipPlanes should be >= 6 with every conforming OpenGL,
      but still better check. }
    if (GLFeatures.MaxClipPlanes > 0) and (ClipPlanes <> nil) then
      for I := 0 to ClipPlanes.Count - 1 do
      begin
        ClipPlane := PClipPlane(ClipPlanes.Ptr(I));
        if ClipPlane^.Node.FdEnabled.Value then
        begin
          Assert(ClipPlanesEnabled < GLFeatures.MaxClipPlanes);
          Shader.EnableClipPlane(ClipPlanesEnabled,
            PlaneTransform(ClipPlane^.Node.FdPlane.Value, ClipPlane^.Transform));
          Inc(ClipPlanesEnabled);

          { No more clip planes possible, regardless if there are any more
            enabled clip planes on the list. }
          if ClipPlanesEnabled = GLFeatures.MaxClipPlanes then Break;
        end;
      end;
  end;

  { Disable OpenGL clip planes previously initialized by ClipPlanesBegin. }
  procedure ClipPlanesEnd;
  var
    I: Integer;
  begin
    for I := 0 to Integer(ClipPlanesEnabled) - 1 do
      Shader.DisableClipPlane(I);
    ClipPlanesEnabled := 0; { not really needed, but for safety... }
  end;

begin
  { This must be done before "glMultMatrix(Shape.State.Transform)" below,
    as in case of fixed-function pipeline the ClipPlanesBegin
    causes glClipPlane that sets clip plane assuming the current matrix
    contains only camera. }
  ClipPlanesBegin(Shape.State.ClipPlanes);

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    glPushMatrix;
      glMultMatrix(Shape.State.Transformation.Transform);
  end;
  {$endif}

    Shape.ShapeModelView := Shape.SceneModelView * Shape.State.Transformation.Transform;
    RenderShapeCreateMeshRenderer(Shape, Shader, Lighting);

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
    glPopMatrix;
  {$endif}

  ClipPlanesEnd;
end;

procedure TGLRenderer.RenderShapeCreateMeshRenderer(const Shape: TX3DRendererShape;
  const Shader: TShader; const Lighting: boolean);
var
  GeneratorClass: TArraysGeneratorClass;
  MeshRenderer: TMeshRenderer;

  { If Shape.Geometry should be rendered using one of TMeshRenderer
    classes, then create appropriate MeshRenderer and return @true.
    Otherwise return @false and doesn't set MeshRenderer.

    Takes care of initializing MeshRenderer, so you have to call only
    MeshRenderer.Render. }
  function InitMeshRenderer: boolean;
  begin
    GeneratorClass := GetArraysGenerator(Shape.Geometry);
    Result := GeneratorClass <> nil;
    if Result then
    begin
      { If we have GeneratorClass, create TCompleteCoordinateRenderer.
        We'll initialize TCompleteCoordinateRenderer.Arrays later. }
      MeshRenderer := TCompleteCoordinateRenderer.Create(Self, Shape);
      Shape.BumpMappingAllowed := GeneratorClass.BumpMappingAllowed;
    end;
  end;

begin
  { default Shape.BumpMapping* state }
  Shape.BumpMappingAllowed := false;
  Shape.BumpMappingUsed := false;

  { Initalize MeshRenderer to something non-nil. }
  if not InitMeshRenderer then
  begin
    WritelnWarning('VRML/X3D', Format('Rendering of node kind "%s" not implemented',
      [Shape.NiceName]));
    Exit;
  end;

  Assert(MeshRenderer <> nil);

  try
    RenderShapeShaders(Shape, Shader, Lighting,
      GeneratorClass, MeshRenderer);
  finally
    FreeAndNil(MeshRenderer);
  end;
end;

procedure TGLRenderer.RenderShapeShaders(const Shape: TX3DRendererShape;
  const Shader: TShader;
  const Lighting: boolean;
  const GeneratorClass: TArraysGeneratorClass;
  const ExposedMeshRenderer: TObject);
var
  { > 0 means that we had custom shader node *and* it already
    needs given number texture units. Always 0 otherwise. }
  UsedGLSLTexCoordsNeeded: Cardinal;

  function TextureCoordsDefined: Cardinal;
  var
    TexCoord: TX3DNode;
  begin
    if Shape.Geometry.InternalTexCoord(Shape.State, TexCoord) and
       (TexCoord <> nil) then
    begin
      if TexCoord is TMultiTextureCoordinateNode then
        Result := TMultiTextureCoordinateNode(TexCoord).FdTexCoord.Count else
        Result := 1;
    end else
      Result := 0;
  end;

  function TextureUnitsDefined(Node: TComposedShaderNode): Cardinal;

    function TextureUnits(Node: TX3DNode): Cardinal;
    begin
      if Node is TMultiTextureNode then
        Result := TMultiTextureNode(Node).FdTexture.Count else
      if Node is TAbstractTextureNode then
        Result := 1 else
        Result := 0;
    end;

  var
    I, J: Integer;
    UniformField: TX3DField;
    IDecls: TX3DInterfaceDeclarationList;
  begin
    IDecls := Node.InterfaceDeclarations;
    Result := 0;
    Assert(IDecls <> nil);
    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
          Result := Result + TextureUnits(TSFNode(UniformField).Value) else
        if UniformField is TMFNode then
          for J := 0 to TMFNode(UniformField).Count - 1 do
            Result := Result + TextureUnits(TMFNode(UniformField)[J]);
      end;
    end;

    if Shape.OriginalGeometry.FontTextureNode <> nil then
      Inc(Result);
  end;

var
  TCD: Cardinal;
  UsedShaderNode: TComposedShaderNode;
begin
  { Use custom shader code (ComposedShader) if available. }

  UsedGLSLTexCoordsNeeded := 0;

  if (Shape.Node <> nil) and
     (Shape.Node.Appearance <> nil) and
     Shader.EnableCustomShaderCode(Shape.Node.Appearance.FdShaders, UsedShaderNode) then
  begin
    UsedGLSLTexCoordsNeeded := TextureUnitsDefined(UsedShaderNode);

    { Only if we bound texture units defined in shader ComposedShader fields
      (it we have shader but UsedGLSLTexCoordsNeeded = 0 then normal
      texture apply (including normal TexCoordsNeeded calculation)
      will be done):

      Although we bound only UsedGLSLTexCoordsNeeded texture units,
      we want to pass all texture coords defined in texCoord.
      Shaders may use them (even when textures are not bound for them). }

    if UsedGLSLTexCoordsNeeded > 0 then
    begin
      TCD := TextureCoordsDefined;
      if TCD > UsedGLSLTexCoordsNeeded then
        WritelnLog('TexCoord', Format('Texture coords defined in VRML/X3D for %d texture units, using them all, even though we bound only %d texture units. Reason: GLSL shaders may use them',
          [TCD, UsedGLSLTexCoordsNeeded]));
      MaxVar(UsedGLSLTexCoordsNeeded, TCD);
    end;
  end;

  RenderShapeTextures(Shape, Shader, Lighting,
    GeneratorClass, TMeshRenderer(ExposedMeshRenderer), UsedGLSLTexCoordsNeeded);
end;

procedure TGLRenderer.RenderShapeTextures(const Shape: TX3DRendererShape;
  const Shader: TShader;
  const Lighting: boolean;
  const GeneratorClass: TArraysGeneratorClass;
  const ExposedMeshRenderer: TObject;
  const UsedGLSLTexCoordsNeeded: Cardinal);

  function NodeTextured(Node: TAbstractGeometryNode): boolean;
  begin
    Result := not (
      (Node is TPointSetNode) or
      (Node is TIndexedLineSetNode));
  end;

  procedure EnableOneShadowMap(const Texture: TGeneratedShadowMapNode;
    var TexCoordsNeeded: Cardinal; const Shader: TShader);
  var
    GLTexture: TGLTextureNode;
  begin
    GLTexture := GLTextureNodes.TextureNode(Texture);
    if GLTexture <> nil then
      GLTexture.EnableAll(GLFeatures.MaxTextureUnits, TexCoordsNeeded, Shader);
  end;

  procedure EnableShadowMaps(const Texture: TAbstractTextureNode;
    var TexCoordsNeeded: Cardinal; const Shader: TShader);
  var
    I: Integer;
    ChildNode: TX3DNode;
  begin
    if Texture is TGeneratedShadowMapNode then
    begin
      EnableOneShadowMap(TGeneratedShadowMapNode(Texture), TexCoordsNeeded, Shader);
    end else
    if Texture is TMultiTextureNode then
      for I := 0 to TMultiTextureNode(Texture).FdTexture.Count - 1 do
      begin
        { TODO: This is not really correct, the texture unit number
          (TexCoordsNeeded) may not correspond to the texture coordinate number
          where the ProjectedTextureCoordinate node is. }

        ChildNode := TMultiTextureNode(Texture).FdTexture[I];
        if ChildNode is TGeneratedShadowMapNode then
          EnableOneShadowMap(TGeneratedShadowMapNode(ChildNode), TexCoordsNeeded, Shader);
      end;
  end;

  function GetAlphaCutoff(const Shape: TShape): Single;
  begin
    if (Shape.Node <> nil) and
       (Shape.Node.Appearance <> nil) then
      Result := Shape.Node.Appearance.AlphaCutoff
    else
      Result := DefaultAlphaCutoff;
  end;

  procedure RenderTexturesBegin;
  var
    TextureNode: TAbstractTextureNode;
    GLTextureNode: TGLTextureNode;
    AlphaTest: Boolean;
    AlphaCutoff: Single;
    FontTextureNode: TAbstractTexture2DNode;
    GLFontTextureNode: TGLTextureNode;
    MainTextureMapping: Integer;
  begin
    TexCoordsNeeded := 0;
    BoundTextureUnits := 0;

    if RenderOptions.Mode = rmSolidColor then
    begin
      { Make sure each shape sets fixed-function alpha test,
        regardless of RenderOptions.Mode (code for other RenderOptions.Mode values
        is lower in this routine), otherwise it is undefined. }
      RenderContext.FixedFunctionAlphaTestDisable;
      Exit;
    end;

    AlphaTest := false;

    TextureNode := Shape.State.MainTexture(Shape.Geometry, MainTextureMapping);
    GLTextureNode := GLTextureNodes.TextureNode(TextureNode);
    { assert we never have non-nil GLTextureNode and nil TextureNode }
    Assert((GLTextureNode = nil) or (TextureNode <> nil));

    Shader.MainTextureMapping := MainTextureMapping;

    FontTextureNode := Shape.OriginalGeometry.FontTextureNode;
    GLFontTextureNode := GLTextureNodes.TextureNode(FontTextureNode);
    { assert we never have non-nil GLFontTextureNode and nil FontTextureNode }
    Assert((GLFontTextureNode = nil) or (FontTextureNode <> nil));

    if UsedGLSLTexCoordsNeeded > 0 then
    begin
      { Do not bind/enable normal textures. Just set TexCoordsNeeded
        to generate tex coords for textures used in the shader.
        Leave BoundTextureUnits at 0 (BoundTextureUnits will be increased
        later when shader actually binds texture uniform values). }
      TexCoordsNeeded := UsedGLSLTexCoordsNeeded;
    end else
    if RenderOptions.Textures and
       NodeTextured(Shape.Geometry) then
    begin
      AlphaTest := TGLShape(Shape).UseAlphaChannel = acTest;

      if GLFontTextureNode <> nil then
        GLFontTextureNode.EnableAll(GLFeatures.MaxTextureUnits, TexCoordsNeeded, Shader);
      if GLTextureNode <> nil then
        GLTextureNode.EnableAll(GLFeatures.MaxTextureUnits, TexCoordsNeeded, Shader);
      BoundTextureUnits := TexCoordsNeeded;

      if (Shape.Node <> nil) and
         (Shape.Node.Appearance <> nil) and
         (Shape.Node.Appearance.Texture <> nil) and
         (TextureNode <> Shape.Node.Appearance.Texture) then
      begin
        { This means that Shape.State.MainTexture comes
          from CommonSurfaceShader or X3Dv4 Material or PhysicalMaterial.
          Make sure to still enable shadow maps from Shape.Appearance.Texture
          then.
          TODO: shadow maps should be placed in some special slot,
          dedicated for them. }
        EnableShadowMaps(Shape.Node.Appearance.Texture, TexCoordsNeeded, Shader);
      end;

      { If there is special texture like a normalmap, enable it. }
      BumpMappingEnable(Shape, BoundTextureUnits, TexCoordsNeeded, Shader);
      SurfaceTexturesEnable(Shape, BoundTextureUnits, TexCoordsNeeded, Shader);
    end;

    { Set alpha test state (shader and fixed-function) }
    if AlphaTest then
    begin
      { only calculate AlphaCutoff when AlphaTest, otherwise it is useless }
      AlphaCutoff := GetAlphaCutoff(Shape);
      RenderContext.FixedFunctionAlphaTestEnable(AlphaCutoff);
      Shader.EnableAlphaTest(AlphaCutoff);
    end else
      RenderContext.FixedFunctionAlphaTestDisable;

    { Make active texture 0. This is helpful for rendering code of
      some primitives that do not support multitexturing now
      (inside vrmlmeshrenderer_x3d_text.inc),
      this way they will at least define correct texture coordinates
      for texture unit 0. }

    if (TexCoordsNeeded > 0) and GLFeatures.UseMultiTexturing then
      ActiveTexture(0);
  end;

  procedure RenderTexturesEnd;
  var
    I: Integer;
  begin
    for I := 0 to Integer(TexCoordsNeeded) - 1 do
      DisableTexture(I);
  end;

begin
  RenderTexturesBegin;
  try
    RenderShapeInside(Shape, Shader, Lighting, GeneratorClass, TMeshRenderer(ExposedMeshRenderer));
  finally RenderTexturesEnd end;
end;

{ Sometimes the updates indicated by Shape.Cache.VboToReload can be performed without
  actually recreating the Shape.Cache.Arrays (so no need to create TArraysGenerator
  and call TArraysGenerator.GenerateArrays).
  Detect and perform such optimized case.

  This is crucial for skinned animation, that only requires updating coords/normals/tangents
  each frame, no need for updating other attributes. }
function FastUpdateArrays(const Shape: TX3DRendererShape): Boolean;

  { If possible, update the coordinate/normal/tangent data in VBO fast.

    This is carefully implemented to do a specific case of TShapeCache.LoadArraysToVbo.
    It optimizes the case of animating coordinates/normals/tangents using CoordinateInterpolator,
    which is very important to optimize, since that's how glTF skinned animations
    are done.

    Pass non-nil Tangents if VBO should have tangent data,
    pass nil Tangents if VBO should not have it. }
  function FastCoordinateNormalUpdate(const Cache: TShapeCache;
    const Coords, Normals, Tangents: TVector3List): Boolean;
  var
    NewCoordinates, NewCoord: Pointer;
    Count, Size, ItemSize: Cardinal;
    I: Integer;
  begin
    Result := false;

    if Cache.VboCoordinatePreserveGeometryOrder and
       (Normals.Count = Coords.Count) and
       ( (Tangents = nil) or
         (Tangents.Count = Coords.Count) ) then
    begin
      Count := Coords.Count;

      ItemSize := SizeOf(TVector3) * 2;
      if Tangents <> nil then
        ItemSize := ItemSize + SizeOf(TVector3);
      Size := Count * ItemSize;

      if (Cache.VboAllocatedUsage = GL_STREAM_DRAW) and
         { Comparing the byte sizes also makes sure that previous and new coordinates
           count stayed the same.
           (Well, assuming we didn't change the Count, and simultaneously changed
           the Tangent existence, which in theory could cause Size to match
           for different structures.)
         }
         (Cache.VboAllocatedSize[vtCoordinate] = Size) then
      begin
        Result := true;
        Cache.VboToReload := Cache.VboToReload - [vtCoordinate]; // vtCoordinate are done
        if Count <> 0 then
        begin
          // calculate NewCoordinates
          NewCoordinates := GetMem(Size);
          try
            NewCoord := NewCoordinates;

            if Tangents <> nil then
            begin
              for I := 0 to Count - 1 do
              begin
                PVector3(NewCoord)^ := Coords.List^[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);

                PVector3(NewCoord)^ := Normals.List^[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);

                PVector3(NewCoord)^ := Tangents.List^[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);
              end;
            end else
            begin
              for I := 0 to Count - 1 do
              begin
                PVector3(NewCoord)^ := Coords.List^[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);

                PVector3(NewCoord)^ := Normals.List^[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);
              end;
            end;

            // load NewCoordinates to GPU
            glBindBuffer(GL_ARRAY_BUFFER, Cache.Vbo[vtCoordinate]);
            glBufferSubData(GL_ARRAY_BUFFER, 0, Size, NewCoordinates);
          finally FreeMemNiling(NewCoordinates) end;
        end;
      end;
    end;
  end;

var
  Cache: TShapeCache;
  Coords, Normals, Tangents: TVector3List;
begin
  Result := false;

  Cache := Shape.Cache;

  // this only optimizes the specific case of VboToReload = [vtCoordinate] now
  if Cache.VboToReload <> [vtCoordinate] then
    Exit;

  { When VBO of coordinates is 0 then
    - VBOs not supported (ancient GPU) -> we need to really update Arrays contents each time
    - or VBO of coordinates was not initialized yet }
  if Cache.Vbo[vtCoordinate] = 0 then
    Exit;
  if Cache.Arrays = nil then // not initialized yet
    Exit;

  if // Shape has coordinates expressed in simple way
     (Shape.Geometry.CoordField <> nil) and
     (Shape.Geometry.CoordField.Value is TCoordinateNode) then // checks also Value <> nil
  begin
    Coords := TCoordinateNode(Shape.Geometry.CoordField.Value).FdPoint.Items;

    if // Shape has normals expressed in simple way
       (Shape.Geometry.NormalField <> nil) and
       (Shape.Geometry.NormalField.Value is TNormalNode) then // checks also Value <> nil
    begin
      Normals := TNormalNode(Shape.Geometry.NormalField.Value).FdVector.Items;

      // Shape has normals expressed in tangents way, or doesn't use tangents (when no bump mapping used)
      if ( (not Cache.Arrays.HasTangent) or
           ( (Shape.Geometry.TangentField <> nil) and
             (Shape.Geometry.TangentField.Value is TTangentNode) ) ) then // checks also Value <> nil
      begin
        if Cache.Arrays.HasTangent then
          Tangents := TTangentNode(Shape.Geometry.TangentField.Value).FdVector.Items
        else
          Tangents := nil;

        Result := FastCoordinateNormalUpdate(Cache, Coords, Normals, Tangents);
      end;
    end;
  end;
end;

procedure TGLRenderer.RenderShapeInside(const Shape: TX3DRendererShape;
  const Shader: TShader;
  const Lighting: boolean;
  const GeneratorClass: TArraysGeneratorClass;
  const ExposedMeshRenderer: TObject);
var
  Generator: TArraysGenerator;
  CoordinateRenderer: TBaseCoordinateRenderer;
begin
  { No point preparing VBO for clones, clones will need own VBOs anyway. }
  if RenderMode = rmPrepareRenderClones then
    Exit;

  { initialize TBaseCoordinateRenderer.Arrays now }
  if GeneratorClass <> nil then
  begin
    Assert(TMeshRenderer(ExposedMeshRenderer) is TBaseCoordinateRenderer);
    CoordinateRenderer := TBaseCoordinateRenderer(ExposedMeshRenderer);

    { calculate Shape.Cache }
    if Shape.Cache = nil then
      Shape.Cache := Cache.Shape_IncReference(Shape, Self);

    { calculate Shape.Cache.Arrays }
    if (Shape.Cache.Arrays = nil) or
       (Shape.Cache.VboToReload <> []) then
    begin
      if not FastUpdateArrays(Shape) then
      begin
        FreeAndNil(Shape.Cache.Arrays); // we just need to create new Arrays
        Generator := GeneratorClass.Create(Shape);
        try
          Generator.TexCoordsNeeded := TexCoordsNeeded;
          Generator.FogVolumetric := FogVolumetric;
          Generator.FogVolumetricDirection := FogVolumetricDirection;
          Generator.FogVolumetricVisibilityStart := FogVolumetricVisibilityStart;
          Generator.ShapeBumpMappingUsed := Shape.BumpMappingUsed;
          Generator.ShapeBumpMappingTextureCoordinatesId := Shape.BumpMappingTextureCoordinatesId;
          Shape.Cache.Arrays := Generator.GenerateArrays;
        finally FreeAndNil(Generator) end;

        { Always after recreating Shape.Cache.Arrays, reload Shape.Cache.Vbo contents.
          This also clears VboToReload. }
        Shape.LoadArraysToVbo;
      end;
    end;

    if GLFeatures.VertexBufferObject then
    begin
      { Shape.Arrays contents are already loaded,
        so Vbo contents are already loaded too }
      Assert(Shape.Cache.Vbo[vtCoordinate] <> 0);
      CoordinateRenderer.Vbo := Shape.Cache.Vbo;
      CoordinateRenderer.Vao := Shape.Cache.Vao;
    end;

    CoordinateRenderer.Arrays := Shape.Cache.Arrays;
    CoordinateRenderer.Shader := Shader;
    CoordinateRenderer.BoundTextureUnits := BoundTextureUnits;
    CoordinateRenderer.Lighting := Lighting;
  end;

  TMeshRenderer(ExposedMeshRenderer).PrepareRenderShape := RenderMode in [rmPrepareRenderSelf, rmPrepareRenderClones];
  TMeshRenderer(ExposedMeshRenderer).Render;

  if (GeneratorClass <> nil) and GLFeatures.VertexBufferObject then
  begin
    { unbind arrays, to have a clean state on exit.
      TODO: this should not be needed, instead move to RenderEnd.
      Check does occlusion query work Ok when some vbo is bound. }
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end;
end;

{$ifndef OpenGLES}
procedure TGLRenderer.PushTextureUnit(const TexUnit: Cardinal);
begin
  { Only continue if texture unit is not already pushed
    (otherwise glPushMatrix would not be paired by exactly one glPopMatrix
    later). }

  if (TexUnit >= TextureTransformUnitsUsed) and
     (TextureTransformUnitsUsedMore.IndexOf(TexUnit) = -1) then
  begin
    if GLFeatures.EnableFixedFunction then
      glPushMatrix;

    { Simple implementation would just add always TexUnit
      to TextureTransformUnitsUsedMore. But there are optimizations possible,
      knowing that TextureTransformUnitsUsed already takes care of many units,
      and TextureTransformUnitsUsed can only be increased (by this
      very method...) in RenderShape.

      If texture unit is = TextureTransformUnitsUsed, this can be taken care
      of easily, just increase TextureTransformUnitsUsed. (This is an often
      case, as it happens when no texture transform was explicitly defined
      in VRML file, and only one texture unit using WORLDSPACEREFLECTIONVECTOR
      is defined; this is the most common case when using cube env mapping
      with WORLDSPACEREFLECTIONVECTOR.)

      Otherwise, we know (from previous checks) that
      TexUnit > TextureTransformUnitsUsed and it's not mentioned in
      TextureTransformUnitsUsedMore. So add it there. }

    if TexUnit = TextureTransformUnitsUsed then
      Inc(TextureTransformUnitsUsed) else
      TextureTransformUnitsUsedMore.Add(TexUnit);
  end;
end;
{$endif}

procedure TGLRenderer.UpdateGeneratedTextures(const Shape: TX3DRendererShape;
  const TextureNode: TAbstractTextureNode;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  const CurrentViewpoint: TAbstractViewpointNode;
  const CameraViewKnown: boolean;
  const CameraPosition, CameraDirection, CameraUp: TVector3);

  procedure UpdateGeneratedCubeMap(const TexNode: TGeneratedCubeMapTextureNode);
  var
    GLNode: TGLGeneratedCubeMapTextureNode;
  begin
    if TexNode.GenTexFunctionality.NeedsUpdate then
    begin
      { Shape.BoundingBox must be non-empty, otherwise we don't know from what
        3D point to capture environment.

        Note: check Shape.BoundingBox only after CheckUpdate passed.
        This is more optimal, as Shape.BoundingBox may need to iterate over mesh.
        Testcase: examples/mobile/simple_3d_demo/gameinitialize.pas with "toggle cubemap updates" = "off",
        look at how much UpdateGeneratedTextures is eating. Should be 0% if off. }
      if Shape.BoundingBox.IsEmpty then Exit;

      GLNode := TGLGeneratedCubeMapTextureNode(GLTextureNodes.TextureNode(TexNode));
      if GLNode <> nil then
      begin
        GLNode.Update(Render, ProjectionNear, ProjectionFar,
          Shape.BoundingBox.Center + TexNode.FdBias.Value);

        TexNode.GenTexFunctionality.PostUpdate;

        if LogRenderer then
          WritelnLog('CubeMap', TexNode.NiceName + ' texture regenerated');
      end;
    end;
  end;

  procedure UpdateGeneratedShadowMap(TexNode: TGeneratedShadowMapNode);
  var
    GLNode: TGLGeneratedShadowMap;
  begin
    if TexNode.GenTexFunctionality.NeedsUpdate then
    begin
      if TexNode.FdLight.Value is TAbstractPunctualLightNode then
      begin
        GLNode := TGLGeneratedShadowMap(GLTextureNodes.TextureNode(TexNode));
        if GLNode <> nil then
        begin
          GLNode.Update(Render, ProjectionNear, ProjectionFar,
            TAbstractPunctualLightNode(TexNode.FdLight.Value));

          TexNode.GenTexFunctionality.PostUpdate;

          if LogRenderer then
            WritelnLog('GeneratedShadowMap', TexNode.NiceName + ' texture regenerated');
        end;
      end else
        WritelnWarning('VRML/X3D', TexNode.NiceName + ' needs updating, but light = NULL or incorrect');
    end;
  end;

  procedure UpdateRenderedTexture(TexNode: TRenderedTextureNode);
  var
    GLNode: TGLRenderedTextureNode;
  begin
    if TexNode.GenTexFunctionality.NeedsUpdate then
    begin
      GLNode := TGLRenderedTextureNode(GLTextureNodes.TextureNode(TexNode));
      if GLNode <> nil then
      begin
        GLNode.Update(Render, ProjectionNear, ProjectionFar, CurrentViewpoint,
          CameraViewKnown, CameraPosition, CameraDirection, CameraUp, Shape);

        TexNode.GenTexFunctionality.PostUpdate;

        if LogRenderer then
          WritelnLog('RenderedTexture', TexNode.NiceName + ' texture regenerated');
      end;
    end;
  end;

begin
  if TextureNode is TGeneratedCubeMapTextureNode then
    UpdateGeneratedCubeMap(TGeneratedCubeMapTextureNode(TextureNode))
  else
  if TextureNode is TGeneratedShadowMapNode then
    UpdateGeneratedShadowMap(TGeneratedShadowMapNode(TextureNode))
  else
  if TextureNode is TRenderedTextureNode then
    UpdateRenderedTexture(TRenderedTextureNode(TextureNode));
end;

initialization
  TCastleRenderOptions.DefaultMinificationFilter := minLinearMipmapLinear;
  TCastleRenderOptions.DefaultMagnificationFilter := magLinear;
end.
