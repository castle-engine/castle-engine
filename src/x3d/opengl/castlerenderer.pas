{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML / X3D low-level rendering (TGLRenderer).
  You should never use this renderer directly,
  you should always use TCastleScene that wraps this renderer and gives you simple
  method to render whole scene.
  TODO: this unit should be renamed to Internal at some point.

  The overview of the renderer can also be found in engine documentation
  [https://castle-engine.io/engine_doc.php]
  in chapter "OpenGL rendering", section "Basic OpenGL rendering".

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
      To start actual rendering, call TGLRenderer.RenderBegin. To end rendering, call
      TGLRenderer.RenderEnd. Between these calls, you should not touch OpenGL state
      yourself --- the renderer may depend that every state change goes
      through it. At the end of TGLRenderer.RenderEnd, the OpenGL state is restored
      just as it was before TGLRenderer.RenderBegin.
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

  @bold(OpenGL state affecting VRML rendering:)

  Some OpenGL state is unconditionally reset by TGLRenderer.RenderBegin.

  There's also some OpenGL state that we let affect our rendering.
  This allows you to customize rendering by using normal OpenGL commands.

  @unorderedList(
    @item(Current glPolygonMode.
      This is used by @link(TCastleScene) to optionally render wireframe.
    )

    @item(Blending settings (GL_BLEND enabled state, glBlendFunc),
      and glDepthMask.

      This is used by @link(TCastleScene) to render
      scenes with a mix of tranparent and opaque objects.
      Only @link(TCastleScene) deals with it (not this renderer),
      as doing it correctly requires ordering the shapes.
    )
  )

  The renderer uses arrays in GPU-friendly format defined by TGeometryArrays.

  They have the same number of triangles and vertices as
  calculated by TAbstractGeometryNode.Triangulate,
  TAbstractGeometryNode.LocalTriangulate,
  TAbstractGeometryNode.TrianglesCount,
  TAbstractGeometryNode.VerticesCount (with OverTriangulate = @true).
}

unit CastleRenderer;

{$I castleconf.inc}

interface

uses Classes, SysUtils, Generics.Collections,
  {$ifdef CASTLE_OBJFPC} CastleGL, {$else} GL, GLExt, {$endif}
  CastleUtils, CastleVectors, X3DFields, X3DNodes, CastleColors,
  CastleInternalX3DLexer, CastleImages, CastleGLUtils, CastleRendererInternalLights,
  CastleGLShaders, CastleGLImages, CastleTextureImages, CastleVideos, X3DTime,
  CastleShapes, CastleGLCubeMaps, CastleClassUtils, CastleCompositeImage, Castle3D,
  CastleGeometryArrays, CastleArraysGenerator, CastleRendererInternalShader,
  CastleRendererInternalTextureEnv, CastleBoxes;

{$define read_interface}

type
  TBeforeGLVertexProc = procedure (Node: TAbstractGeometryNode;
    const Vert: TVector3) of object;

  TShadersRendering = (srDisable, srWhenRequired, srAlways);

  TBumpMapping = CastleRendererInternalShader.TBumpMapping;
  TLightRenderEvent = CastleRendererInternalLights.TLightRenderEvent;

  { TRenderingAttributes.Mode possible values. }
  TRenderingMode = (
    { Normal rendering features. Everything is enabled
      (as long as other TRenderingAttributes settings allow them). }
    rmFull,

    { Solid color is used for everything. We do not show any color variation,
      materials, lights, fog, textures on surfaces.
      We still do back-face culling and depth test.
      The idea is that we "hit" the same pixels as normal rendering
      (with the exception of alpha test textures, this mode
      doesn't set up alpha test for them).
      But everything has color TRenderingAttributes.SolidColor.

      This is useful for special tricks. }
    rmSolidColor,

    { Only the rendering fetures that affect depth buffer work reliably,
      everything else is undefined (and works as fast as possible).
      This is suitable if you render only to depth buffer, like for shadow maps.

      It's quite similar to rmSolidColor, except alpha testing must work,
      so (at least some) textures must be applied over the model. }
    rmDepth
  );

  { Various properties that control rendering done
    with @link(TGLRenderer).

    They are collected here, in a class separate from @link(TGLRenderer),
    to allow TCastleScene to hide internal @link(TGLRenderer) but still
    expose @link(TRenderingAttributes) instance. }
  TRenderingAttributes = class(TPersistent)
  private
    FOnRadianceTransfer: TRadianceTransferFunction;
    FOnVertexColor: TVertexColorFunction;
    FLighting: boolean;
    FUseSceneLights: boolean;
    FOpacity: Single;
    FEnableTextures: boolean;
    FMinificationFilter: TMinificationFilter;
    FMagnificationFilter: TMagnificationFilter;
    FPointSize: TGLFloat;
    FLineWidth: TGLFloat;
    FBumpMapping: TBumpMapping;
    FCustomShader, FCustomShaderAlphaTest: TX3DShaderProgramBase;
    FMode: TRenderingMode;
    FVertexBufferObject: boolean;
    FShadowSampling: TShadowSampling;
    FVisualizeDepthMap: boolean;
    FDepthTest: boolean;
    FPhongShading: boolean;
    FSolidColor: TCastleColorRGB;
    FSeparateDiffuseTexture: boolean;
    function GetShaders: TShadersRendering;
    procedure SetShaders(const Value: TShadersRendering);
  protected
    { These methods just set the value on given property,
      eventually (some of them) calling ReleaseCachedResources.
      @groupBegin }
    procedure SetOnRadianceTransfer(const Value: TRadianceTransferFunction); virtual;
    procedure SetOnVertexColor(const Value: TVertexColorFunction); virtual;
    procedure SetEnableTextures(const Value: boolean); virtual;
    procedure SetMinificationFilter(const Value: TMinificationFilter); virtual;
    procedure SetMagnificationFilter(const Value: TMagnificationFilter); virtual;
    procedure SetBumpMapping(const Value: TBumpMapping); virtual;
    procedure SetMode(const Value: TRenderingMode); virtual;
    procedure SetShadowSampling(const Value: TShadowSampling); virtual;
    procedure SetVertexBufferObject(const Value: boolean); virtual;
    procedure SetVisualizeDepthMap(const Value: boolean); virtual;
    procedure SetPhongShading(const Value: boolean); virtual;
    { @groupEnd }

    { Called before changing an attribute that requires the release
      of things cached in a renderer. This includes attributes that affect:

      @unorderedList(
        @item(How TShapeCache.Arrays contents are generated.
          For example, Generator uses TexCoordsNeeded, so changing
          any attribute that affects TexCoordsNeeded calls this method.
          Likewise OnVertexColor determines if color array will be loaded at all.)

        @item(How (and if) TShapeCache.Vbo are loaded.)

        @item(How textures are loaded (texture filtering options affect them).)
      ) }
    procedure ReleaseCachedResources; virtual;
  public
    const
      DefaultPointSize = 3.0;
      DefaultLineWidth = 2.0;
      DefaultBumpMapping = bmSteepParallaxShadowing;
      DefaultPhongShading = false;

    constructor Create; virtual;

    procedure Assign(Source: TPersistent); override;

    { Is the second TRenderingAttributes instance on all fields
      that affect TShapeCache, that is things that affect generated geometry
      arrays or vbo. This compares the subset of variables that call
      ReleaseCachedResources --- only the ones that affect TShapeCache. }
    function EqualForShapeCache(SecondValue: TRenderingAttributes): boolean; virtual;

    { Calculate vertex color from radiance transfer.
      If this is assigned, and geometry object has radianceTransfer
      field (see [https://castle-engine.io/x3d_extensions.php#section_ext_radiance_transfer])
      then this is used to calculate the color of each vertex.

      Note that this is evaluated when object is rendered.
      It causes the shapes resources to be regenerated at each render frame,
      since we have to assume that results of this function change. }
    property OnRadianceTransfer: TRadianceTransferFunction
      read FOnRadianceTransfer write SetOnRadianceTransfer;

    { Calculate vertex color for given vertex by a callback.
      If this is assigned, then this is used to calculate
      the color of each vertex.

      Note that this is evaluated when object is rendered.
      It causes the shapes resources to be regenerated at each render frame,
      since we have to assume that results of this function change. }
    property OnVertexColor: TVertexColorFunction
      read FOnVertexColor write SetOnVertexColor;

    { Enable OpenGL lighting when rendering.
      This is @true by default, since it's almost always wanted.

      When Lighting is @false, we disable OpenGL lighting.
      (We had previously a different approach, when we left GL_LIGHTING
      untouched and caller could enable/disable it. But this doesn't really
      work for modern OpenGL, the renderer really has to know if lighting
      is enabled. (to generate proper shaders, and to avoid clumsy
      glPushAttrib / glPopAttrib at some places).) }
    property Lighting: boolean
      read FLighting write FLighting default true;

    { Should we setup VRML/X3D lights as OpenGL lights during rendering.

      VRML/X3D lights are loaded into OpenGL lights. All OpenGL lights
      are always used (we always start from the first OpenGL light 0,
      up to the last available OpenGL light --- this is necessary,
      as shader pipeline must know all the lights anyway).

      Initial OpenGL lights are reserved for BaseLights
      (useful for you to define any lights from outside of the scene).
      Then following OpenGL lights are reserved for the lights defined
      in your scene (if this property is @true).
      The remaining OpenGL lights, if any, are not used (we make sure they
      are disabled for fixed-function pipeline).

      This is independent from the @link(Lighting) property (which merely
      says whether we will turn OpenGL lighting on at all). }
    property UseSceneLights: boolean
      read FUseSceneLights write FUseSceneLights default true;

    { Opacity for all rendered shapes. Setting this to something < 1
      you can make every shape transparent. }
    property Opacity: Single read FOpacity write FOpacity default 1;

    { Take model textures into account. When @true (default),
      then our engine takes care of everything related to texturing
      for you: enabling and using textures for textured parts of the model,
      disabling textures for non-textured parts.

      Otherwise, textures are disabled. }
    property EnableTextures: boolean
      read FEnableTextures write SetEnableTextures default true;

    { Default minification and magnification filters for textures.
      These can be overridden on a per-texture basis in VRML / X3D files
      by X3D TextureProperties node (see X3D specification).

      @groupBegin }
    property MinificationFilter: TMinificationFilter
      read FMinificationFilter write SetMinificationFilter default minLinearMipmapLinear;
    property MagnificationFilter: TMagnificationFilter
      read FMagnificationFilter write SetMagnificationFilter default magLinear;
    function TextureFilter: TTextureFilter;
    { @groupEnd }

    { Size of points. This has an effect on VRML/X3D PointSet rendering.
      Must be > 0. }
    property PointSize: TGLFloat
      read FPointSize write FPointSize default DefaultPointSize;

    { Line width. This has an effect on VRML/X3D LineSet rendering,
      and on wireframe rendering for TSceneRenderingAttributes.WireframeEffect.
      Must be > 0. }
    property LineWidth: Single
      read FLineWidth write FLineWidth default DefaultLineWidth;

    { Use bump mapping. To actually use this, particular shape must also
      provide normal map (and height map, if you want parallax bump mapping).
      This also requires some OpenGL capabilities, in particular GLSL.

      Simple bump mapping (when only normal map is available)
      means that normals are provided in the texture, and lighting
      is calculated per-fragment.

      Parallax bump mapping means that additionally the texture coordinate
      is perturbed, based on height map and camera direction, to create
      illusion of 3D shape instead of flat surface.
      This makes e.g. the bricks on the texture really
      visible as "standing out", in 3D, from the wall. And self-shadowing
      means that these bricks even cast appropriate shadows on each other.

      Steep parallax mapping requires good GPU to work correctly and fast
      enough. }
    property BumpMapping: TBumpMapping
      read FBumpMapping write SetBumpMapping default DefaultBumpMapping;

    { When GLSL shaders are used.
      This is now a deprecated property, better use @link(PhongShading) to determine
      the shading.
      The engine auto-detects whether to use shaders based on OpenGL capabilities,
      particular shape needs (phong shading, bump mapping, shadow maps, compositing shader effects),
      and GLFeatures.EnableFixedFunction. }
    property Shaders: TShadersRendering read GetShaders write SetShaders; deprecated 'use PhongShading';

    { Whether to use Phong shading by default for all shapes.
      Note that each shape may override it by @link(TAbstractShapeNode.Shading) field. }
    property PhongShading: boolean read FPhongShading write SetPhongShading
      default DefaultPhongShading;

    { Custom GLSL shader to use for the whole scene.
      When this is assigned, @link(Shaders) value is ignored.

      @italic(Avoid using this.) It's not easy to create portable shaders,
      that work both with OpenGL and OpenGLES. Try using "compositing shaders" instead
      https://castle-engine.io/compositing_shaders.php which still allow you
      to write GLSL effects, but they are integrated into standard shader code. }
    property CustomShader: TX3DShaderProgramBase read FCustomShader write FCustomShader;

    { Alternative custom GLSL shader used when alpha test is necessary.
      Relevant only if CustomShader <> nil.

      @italic(Do not use this.) This is a temporary hack to enable VSM working
      with alpha test. It's not clean, and should not be used for anything else. }
    property CustomShaderAlphaTest: TX3DShaderProgramBase read FCustomShaderAlphaTest write FCustomShaderAlphaTest;

    { Rendering mode, can be used to disable many rendering features at once. }
    property Mode: TRenderingMode read FMode write SetMode default rmFull;

    { Use OpenGL vertex buffer object.
      This is always a good idea. You can set this to @false
      for debug purposes, e.g. to check how much speedup you get from VBO. }
    property VertexBufferObject: boolean
      read FVertexBufferObject write SetVertexBufferObject default true;

    { Shadow maps sampling. Various approaches result in various quality and speed. }
    property ShadowSampling: TShadowSampling
      read FShadowSampling write SetShadowSampling
      default DefaultShadowSampling;

    { Visualize depths stored in the shadow maps, instead of using them to
      actually make shadow.

      Even without turning this on, VRML author can always activate it
      explicitly for specific lights. For this, you have to use
      @code(X3DLightNode.defaultShadowMap) field,
      and place a GeneratedShadowMap node there. If the
      @code(GeneratedShadowMap.compareMode) is set to @code('NONE'),
      we will always visualize depths of this shadow map.

      Setting this property to @true has the same effect as setting
      compareMode to "NONE" on all (explicit and implicitly created)
      GeneratedShadowMap nodes. }
    property VisualizeDepthMap: boolean
      read FVisualizeDepthMap write SetVisualizeDepthMap default false;

    { By default, we use depth testing to determine which objects are in front
      of the others. This allows to display all 3D content (all TCastleScene
      instances, and all shapes inside them) in any order.

      For very special purposes, you can disable depth testing.
      This means that 3D objects will always be drawn in front of the previous
      ones, in the order in which they are rendered,
      ignoring the contents of the depth buffer. Use only if you know
      what you're doing, if you're sure that the order of rendering will
      always be good. }
    property DepthTest: boolean read FDepthTest write FDepthTest default true;

    { Color used when @link(Mode) is @link(rmSolidColor). }
    property SolidColor: TCastleColorRGB read FSolidColor write FSolidColor;

    { Set to @true to make diffuse texture affect only material diffuse color
      when the shape is lit and shading is Phong.
      This affects both textures from X3D Appearance.texture,
      and textures from CommonSurfaceShader.diffuseTexture.
      This is more correct (following X3D lighting equations),
      and is more impressive (e.g. specular highlights may be better visible,
      as they are not darkened by a dark diffuse texture).

      For historic reasons and for Gouraud shading, by default, this is @false.
      Which means that "diffuse texture" is actually used to multiply
      a complete result of the lighting calculation.
      This is not correct, but it is necessary for Gouraud shading,
      and it is also depended upon by some applications (since the "diffuse texture"
      effectively multiplies all factors, so it also multiplies
      e.g. emissive factor for "pure emissive materials",
      which may be useful sometimes). }
    property SeparateDiffuseTexture: boolean
      read FSeparateDiffuseTexture
      write FSeparateDiffuseTexture default false;
  end;

  TRenderingAttributesClass = class of TRenderingAttributes;

  TTextureImageCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: string;

    { The initial VRML/X3D node that created this cache record.
      This is only the first node, that initiated this
      TTextureImageCache item. Note that many TAbstractTexture2DNode nodes
      may correspond to a single TTextureImageCache (since TTextureImageCache
      only tries to share GLName between them). So this may help during
      _IncReference, but nothing more --- it's *not* an exhaustive list
      of texture nodes related to this video texture!

      It may be currently TAbstractTexture2DNode, or TRenderedTextureNode. }
    InitialNode: TAbstractTextureNode;

    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    GUITexture: boolean;
    References: Cardinal;
    GLName: TGLuint;
  end;
  TTextureImageCacheList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TTextureImageCache>;

  TTextureVideoCache = class
    FullUrl: string;

    { The initial VRML/X3D node that created this cache record.
      This is only the first TMovieTextureNode node, that initiated this
      TTextureVideoCache item. Note that many TMovieTextureNode nodes
      may correspond to a single TTextureVideoCache (since TTextureVideoCache
      only tries to share TGLVideo3D between them, they don't have to share
      other fields like current time etc.). So this may help during
      _IncReference, but nothing more --- it's *not* an exhaustive list
      of MovieTexture nodes related to this video texture! }
    InitialNode: TMovieTextureNode;

    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    GUITexture: boolean;
    References: Cardinal;
    GLVideo: TGLVideo3D;
  end;
  TTextureVideoCacheList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TTextureVideoCache>;

  TTextureCubeMapCache = class
    InitialNode: TAbstractEnvironmentTextureNode;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    References: Cardinal;
    GLName: TGLuint;
  end;
  TTextureCubeMapCacheList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TTextureCubeMapCache>;

  TTexture3DCache = class
    InitialNode: TAbstractTexture3DNode;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap3D;
    References: Cardinal;
    GLName: TGLuint;
  end;
  TTexture3DCacheList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TTexture3DCache>;

  { Cached depth or float texture.
    For now, depth and float textures require the same fields. }
  TTextureDepthOrFloatCache = class
    { The initial VRML/X3D node that created this cache record.
      For now, this may be TGeneratedShadowMapNode or TRenderedTextureNode. }
    InitialNode: TAbstractTextureNode;
    Wrap: TTextureWrap2D;
    References: Cardinal;
    GLName: TGLuint;
  end;
  TTextureDepthOrFloatCacheList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TTextureDepthOrFloatCache>;

  TX3DRendererShape = class;
  TVboType = (vtCoordinate, vtAttribute, vtIndex);
  TVboTypes = set of TVboType;
  TVboArrays = array [TVboType] of TGLuint;

  { Cached shape resources. }
  TShapeCache = class
  private
    Attributes: TRenderingAttributes;
    Geometry: TAbstractGeometryNode;
    State: TX3DGraphTraverseState;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3;
    FogVolumetricVisibilityStart: Single;
    References: Cardinal;

    { An instance of TGeometryArrays, decomposing this shape geometry.
      Used to easily render and process this geometry, if assigned.
      This is managed by TGLRenderer and TCastleScene. }
    Arrays: TGeometryArrays;

    { What Vbos do we need to reload.
      Next time (right after creating arrays) we load vbo contents,
      we'll look at this to know which parts to actually reload to vbo.
      This is extended at each FreeArrays call. }
    VboToReload: TVboTypes;

    Vbo: TVboArrays;
    VboAllocatedUsage: TGLenum;
    VboAllocatedSize: array [TVboType] of Cardinal;

    { Like TX3DRendererShape.LoadArraysToVbo,
      but takes explicit DynamicGeometry. }
    procedure LoadArraysToVbo(DynamicGeometry: boolean);
    procedure FreeVBO;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeArrays(const Changed: TVboTypes);
  end;

  TShapeCacheList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TShapeCache>;

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

  TShaderProgramCacheList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TShaderProgramCache>;

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
    TextureImageCaches: TTextureImageCacheList;
    TextureVideoCaches: TTextureVideoCacheList;
    TextureCubeMapCaches: TTextureCubeMapCacheList;
    Texture3DCaches: TTexture3DCacheList;
    TextureDepthOrFloatCaches: TTextureDepthOrFloatCacheList;
    ShapeCaches: TShapeCacheList;
    ProgramCaches: TShaderProgramCacheList;

    { Load given texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function TextureImage_IncReference(
      const TextureImage: TEncodedImage;
      const TextureFullUrl: string;
      const TextureNode: TAbstractTextureNode;
      const Filter: TTextureFilter;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      const CompositeForMipmaps: TCompositeImage;
      const GUITexture: boolean): TGLuint;

    procedure TextureImage_DecReference(
      const TextureGLName: TGLuint);

    function TextureVideo_IncReference(
      const TextureVideo: TVideo;
      const TextureFullUrl: string;
      const TextureNode: TMovieTextureNode;
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
      Node: TAbstractEnvironmentTextureNode;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      PositiveX, NegativeX,
      PositiveY, NegativeY,
      PositiveZ, NegativeZ: TEncodedImage;
      CompositeForMipmaps: TCompositeImage): TGLuint;

    procedure TextureCubeMap_DecReference(
      const TextureGLName: TGLuint);

    { Required GLFeatures.TextureDepth before calling this.
      For interpreating CompareMode, ARB_shadow will be needed
      (but we'll make nice warning if it's not available). }
    function TextureDepth_IncReference(
      Node: TAbstractTextureNode;
      const TextureWrap: TTextureWrap2D;
      CompareMode: TShadowMapCompareMode;
      const Width, Height: Cardinal;
      const VisualizeDepthMap: boolean): TGLuint;

    procedure TextureDepth_DecReference(
      const TextureGLName: TGLuint);

    { Increase / decrease reference to a float texture.
      Required ARB_texture_float or ATI_texture_float before calling this.
      Precision32 = @true requires 32-bit full Single floats,
      Precision32 = @false requires 16-bit (half) floats. }
    function TextureFloat_IncReference(Node: TAbstractTextureNode;
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
      Node: TAbstractTexture3DNode;
      const Filter: TTextureFilter;
      const Anisotropy: TGLfloat;
      const TextureWrap: TTextureWrap3D;
      Image: TEncodedImage; Composite: TCompositeImage): TGLuint;

    procedure Texture3D_DecReference(
      const TextureGLName: TGLuint);
  public
    constructor Create;
    destructor Destroy; override;

    { Shape cache. We return TShapeCache, either taking an existing
      instance from cache or creating and adding a new one.
      Caller is responsible for checking are Arrays / Vbo zero and
      eventually initializing and setting. }
    function Shape_IncReference(Shape: TX3DRendererShape;
      Fog: IAbstractFogObject; ARenderer: TGLRenderer): TShapeCache;

    procedure Shape_DecReference(var ShapeCache: TShapeCache);

    { Shader program cache. We return TShaderProgramCache,
      either taking an existing instance from cache or creating and adding
      a new one. If we create a new one, we will use Shader to initialize
      program hash and to create and link actual TX3DGLSLProgram instance. }
    function Program_IncReference(ARenderer: TGLRenderer;
      Shader: TShader; const ShapeNiceName: string): TShaderProgramCache;

    procedure Program_DecReference(var ProgramCache: TShaderProgramCache);
  end;

  {$I castlerenderer_resource.inc}
  {$I castlerenderer_texture.inc}
  {$I castlerenderer_glsl.inc}

  { Shape that can be rendered. }
  TX3DRendererShape = class(TShape)
  private
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
      passes probably have different BaseLights and so will require different
      shaders. This makes multi-pass rendering, like for shadow volumes,
      play nicely with shaders. Otherwise we could recreate shaders at each
      rendering pass. }
    ProgramCache: array [TRenderingPass] of TShaderProgramCache;

    Cache: TShapeCache;

    { Assign this each time before passing this shape to RenderShape. }
    ModelView: TMatrix4;

    { For implementing TextureCoordinateGenerator.mode = "MIRROR-PLANE". }
    MirrorPlaneUniforms: TMirrorPlaneUniforms;

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

    { Is bump mapping allowed by the current shape.
      Fully calculated only after InitMeshRenderer, as determining GeneratorClass
      is needed to set this. }
    ShapeBumpMappingAllowed: boolean;
    { Is bump mapping used for current shape.
      This is determined by ShapeBumpMappingAllowed,
      global BumpMapping, and by the texture information for current
      shape (whether user provided normal map, height map etc.) }
    ShapeBumpMappingUsed: boolean;
    ShapeBumpMappingTextureCoordinatesId: Cardinal;

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
    TextureTransformUnitsUsedMore: TLongIntList;

    FSmoothShading: boolean;
    FFixedFunctionLighting: boolean;
    FFixedFunctionAlphaTest: boolean;
    FLineType: TLineType;

    function PrepareTexture(Shape: TShape; Texture: TAbstractTextureNode): Pointer;

    {$ifndef OpenGLES}
    { Call glPushMatrix, assuming that current matrix mode is GL_TEXTURE
      and current tex unit is TexUnit (always make sure this is true when
      calling it!).

      It also records this fact, so that RenderShapeEnd will be able to
      make pop texture matrix later.

      In fact this optimizes push/pops on texture matrix stack, such that
      VRML/X3D TextureTransform nodes and such together with PushTextureUnit
      will only use only matrix stack place, even if texture will be
      "pushed" multiple times (both by PushTextureUnit and normal
      VRML TextureTransform realized in RenderShapeBegin.) }
    procedure PushTextureUnit(const TexUnit: Cardinal);
    {$endif}

    { Check Attributes (like Attributes.BumpMapping) and OpenGL
      context capabilities to see if bump mapping can be used. }
    function BumpMapping: TBumpMapping;

    procedure SetSmoothShading(const Value: boolean);
    procedure SetFixedFunctionLighting(const Value: boolean);
    procedure SetFixedFunctionAlphaTest(const Value: boolean);
    procedure SetLineType(const Value: TLineType);

    { Change glShadeModel by this property. }
    property SmoothShading: boolean read FSmoothShading write SetSmoothShading;
    { Change GL_LIGHTING enabled by this property. }
    property FixedFunctionLighting: boolean read FFixedFunctionLighting write SetFixedFunctionLighting;
    { Change GL_ALPHA_TEST enabled by this property. }
    property FixedFunctionAlphaTest: boolean read FFixedFunctionAlphaTest write SetFixedFunctionAlphaTest;
    property LineType: TLineType read FLineType write SetLineType;

    {$I castlerenderer_surfacetextures.inc}
  private
    { ----------------------------------------------------------------- }

    { Available between RenderBegin / RenderEnd. }
    LightsRenderer: TVRMLGLLightsRenderer;

    { Currently set fog parameters, during render. }
    FogNode: IAbstractFogObject;
    FogEnabled: boolean;
    FogType: TFogType;
    FogColor: TVector3;
    FogLinearEnd: Single;
    FogExpDensity: Single;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3;
    FogVolumetricVisibilityStart: Single;

    FAttributes: TRenderingAttributes;

    FCache: TGLRendererContextCache;

    { Lights shining on all shapes, may be @nil. Set in each RenderBegin. }
    BaseLights: TLightInstancesList;

    { Rendering pass. Set in each RenderBegin. }
    Pass: TRenderingPass;

    { Get VRML/X3D fog parameters, based on fog node and Attributes. }
    procedure GetFog(Node: IAbstractFogObject;
      out Enabled, Volumetric: boolean;
      out VolumetricDirection: TVector3;
      out VolumetricVisibilityStart: Single);

    { If multitexturing available, this sets currently active texture unit.
      TextureUnit is newly active unit, this is added to GL_TEXTURE0.

      So the only thing that you have to care about is to specify TextureUnit <
      FreeGLTexturesCount.
      Everything else (multitexturing availability, GL_TEXTURE0)
      is taken care of inside here. }
    procedure ActiveTexture(const TextureUnit: Cardinal);

    { Disable any (fixed-function) texturing (2D, 3D, cube map, and so on)
      on given texture unit. }
    procedure DisableTexture(const TextureUnit: Cardinal);
    procedure DisableCurrentTexture;

    procedure RenderShapeLineProperties(Shape: TX3DRendererShape;
      Fog: IAbstractFogObject; Shader: TShader);
    procedure RenderShapeMaterials(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader);
    procedure RenderShapeLights(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeFog(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeTextureTransform(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeClipPlanes(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeCreateMeshRenderer(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeShaders(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader;
      const MaterialOpacity: Single; const Lighting: boolean;
      GeneratorClass: TArraysGeneratorClass;
      ExposedMeshRenderer: TObject);
    procedure RenderShapeTextures(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader;
      const MaterialOpacity: Single; const Lighting: boolean;
      GeneratorClass: TArraysGeneratorClass;
      ExposedMeshRenderer: TObject;
      UsedGLSLTexCoordsNeeded: Cardinal);
    procedure RenderShapeInside(Shape: TX3DRendererShape; Fog: IAbstractFogObject;
      Shader: TShader;
      const MaterialOpacity: Single; const Lighting: boolean;
      GeneratorClass: TArraysGeneratorClass;
      ExposedMeshRenderer: TObject);

    { Reset various OpenGL state parameters, done at RenderBegin
      (to prepare state for following RenderShape calls) and at RenderEnd
      (to leave *somewhat* defined state afterwards). }
    procedure RenderCleanState(const Beginning: boolean);
  public
    { If > 0, RenderShape will not actually render, only prepare
      per-shape resources for fast rendering (arrays and vbos). }
    PrepareRenderShape: Cardinal;

    { Constructor. Always pass a cache instance --- preferably,
      something created and used by many scenes. }
    constructor Create(AttributesClass: TRenderingAttributesClass;
      ACache: TGLRendererContextCache);

    destructor Destroy; override;

    { Rendering attributes. You can change them only when renderer
      is not tied to the current OpenGL context, so only after construction
      or after UnprepareAll call (before any Prepare or Render* calls). }
    property Attributes: TRenderingAttributes read FAttributes;

    property Cache: TGLRendererContextCache read FCache;

    { Prepare given Shape for rendering.
      Between preparing and unpreparing, nodes passed here are "frozen":
      do not change, do not free them. }
    procedure Prepare(Shape: TX3DRendererShape);

    { Release resources for this texture. }
    procedure UnprepareTexture(Node: TAbstractTextureNode);

    { Release every OpenGL and VRML resource. That is release any knowledge
      connecting us to the current OpenGL context and any knowledge
      about your prepared VRML nodes, states etc.

      Calling UnprepareAll is valid (and ignored) call if everything
      is already released.

      Destructor callls UnprepareAll automatically. So be sure to either
      call UnprepareAll or destroy this renderer
      when your OpenGL context is still active. }
    procedure UnprepareAll;

    procedure RenderBegin(ABaseLights: TLightInstancesList;
      LightRenderEvent: TLightRenderEvent; const APass: TRenderingPass);
    procedure RenderEnd;

    procedure RenderShape(Shape: TX3DRendererShape; Fog: IAbstractFogObject);

    { Update generated texture for this shape.

      NeedsRestoreViewport will be set to @true if viewport was
      (possibly) changed by this procedure (otherwise, NeedsRestoreViewport
      will not be modified).

      The given camera position, direction, up should be in world space
      (that is, in TCastleSceneManager space,
      not in space local to this TCastleScene). }
    procedure UpdateGeneratedTextures(Shape: TX3DRendererShape;
      TextureNode: TAbstractTextureNode;
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      var NeedsRestoreViewport: boolean;
      CurrentViewpoint: TAbstractViewpointNode;
      CameraViewKnown: boolean;
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

  BumpMappingNames: array [TBumpMapping] of string =
  ( 'None',
    'Basic',
    'Parallax',
    'Steep Parallax',
    'Steep Parallax With Self-Shadowing' );

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

{$warnings off}
// TODO: This unit temporarily uses RenderingCamera singleton,
// to keep it working for backward compatibility.
uses Math,
  CastleStringUtils, CastleGLVersion, CastleLog, CastleRenderingCamera,
  X3DCameraUtils, CastleProjection, CastleRectangles, CastleTriangles;
{$warnings on}

{$define read_implementation}

{$I castlerenderer_meshrenderer.inc}
{$I castlerenderer_resource.inc}
{$I castlerenderer_texture.inc}
{$I castlerenderer_surfacetextures.inc}
{$I castlerenderer_glsl.inc}

{ TGLRendererContextCache -------------------------------------------- }

constructor TGLRendererContextCache.Create;
begin
  inherited;
  TextureImageCaches := TTextureImageCacheList.Create;
  TextureVideoCaches := TTextureVideoCacheList.Create;
  TextureCubeMapCaches := TTextureCubeMapCacheList.Create;
  Texture3DCaches := TTexture3DCacheList.Create;
  TextureDepthOrFloatCaches := TTextureDepthOrFloatCacheList.Create;
  ShapeCaches := TShapeCacheList.Create;
  ProgramCaches := TShaderProgramCacheList.Create;
end;

destructor TGLRendererContextCache.Destroy;

{ $define ONLY_WARN_ON_CACHE_LEAK}

{$ifdef ONLY_WARN_ON_CACHE_LEAK}
  procedure Assert(const B: boolean; const S: string = '');
  begin
    if not B then
      WritelnWarning('VRML/X3D', 'GLRendererContextCache warning: ' + S);
  end;
{$endif}

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

  if ShapeCaches <> nil then
  begin
    Assert(ShapeCaches.Count = 0, 'Some references to Shapes still exist' +
      ' when freeing TGLRendererContextCache');
    FreeAndNil(ShapeCaches);
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
  const TextureNode: TAbstractTextureNode;
  const Filter: TTextureFilter;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  const CompositeForMipmaps: TCompositeImage;
  const GUITexture: boolean): TGLuint;
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

    if ( ( (TextureFullUrl <> '') and
           (TextureCached.FullUrl = TextureFullUrl) ) or
         (TextureCached.InitialNode = TextureNode) ) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = TextureAnisotropy) and
       (TextureCached.Wrap = TextureWrap) and
       (TextureCached.GUITexture = GUITexture) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache and Log then
        WritelnLog('++', '%s: %d', [TextureFullUrl, TextureCached.References]);
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
  TextureCached.InitialNode := TextureNode;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := TextureAnisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GUITexture := GUITexture;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache and Log then
    WritelnLog('++', '%s: %d', [TextureFullUrl, 1]);
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
      if LogRendererCache and Log then
        WritelnLog('--', '%s: %d', [TextureImageCaches[I].FullUrl,
                                    TextureImageCaches[I].References]);
      if TextureImageCaches[I].References = 0 then
      begin
        glFreeTexture(TextureImageCaches[I].GLName);
        TextureImageCaches.Delete(I);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureImage_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.TextureVideo_IncReference(
  const TextureVideo: TVideo;
  const TextureFullUrl: string;
  const TextureNode: TMovieTextureNode;
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

    if ( ( (TextureFullUrl <> '') and
           (TextureCached.FullUrl = TextureFullUrl) ) or
         (TextureCached.InitialNode = TextureNode) ) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = TextureAnisotropy) and
       (TextureCached.Wrap = TextureWrap) and
       (TextureCached.GUITexture = GUITexture) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache and Log then
        WritelnLog('++', '%s: %d', [TextureFullUrl, TextureCached.References]);
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
  TextureCached.InitialNode := TextureNode;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := TextureAnisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GUITexture := GUITexture;
  TextureCached.References := 1;
  TextureCached.GLVideo := Result;

  if LogRendererCache and Log then
    WritelnLog('++', '%s: %d', [TextureFullUrl, 1]);
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
      if LogRendererCache and Log then
        WritelnLog('--', '%s: %d', [TextureVideoCaches[I].FullUrl,
                                    TextureVideoCaches[I].References]);
      if TextureVideoCaches[I].References = 0 then
      begin
        FreeAndNil(TextureVideoCaches[I].GLVideo);
        TextureVideoCaches.Delete(I);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureVideo_DecReference: no reference ' +
    'found to texture %s', [PointerToStr(TextureVideo)]);
end;

function TGLRendererContextCache.TextureCubeMap_IncReference(
  Node: TAbstractEnvironmentTextureNode;
  const Filter: TTextureFilter;
  const Anisotropy: TGLfloat;
  PositiveX, NegativeX,
  PositiveY, NegativeY,
  PositiveZ, NegativeZ: TEncodedImage;
  CompositeForMipmaps: TCompositeImage): TGLuint;
var
  I: Integer;
  TextureCached: TTextureCubeMapCache;
begin
  for I := 0 to TextureCubeMapCaches.Count - 1 do
  begin
    TextureCached := TextureCubeMapCaches[I];

    if (TextureCached.InitialNode = Node) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = Anisotropy) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache and Log then
        WritelnLog('++', 'cube map %s: %d', [PointerToStr(Node), TextureCached.References]);
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
    Filter.NeedsMipmaps);

  TexParameterMaxAnisotropy(GL_TEXTURE_CUBE_MAP, Anisotropy);

  TextureCached := TTextureCubeMapCache.Create;
  TextureCubeMapCaches.Add(TextureCached);
  TextureCached.InitialNode := Node;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := Anisotropy;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache and Log then
    WritelnLog('++', 'cube map %s: %d', [PointerToStr(Node), 1]);
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
      if LogRendererCache and Log then
        WritelnLog('--', 'cube map %s: %d', [PointerToStr(TextureCubeMapCaches[I].InitialNode), TextureCubeMapCaches[I].References]);
      if TextureCubeMapCaches[I].References = 0 then
      begin
        glFreeTexture(TextureCubeMapCaches[I].GLName);
        TextureCubeMapCaches.Delete(I);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureCubeMap_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.Texture3D_IncReference(
  Node: TAbstractTexture3DNode;
  const Filter: TTextureFilter;
  const Anisotropy: TGLfloat;
  const TextureWrap: TTextureWrap3D;
  Image: TEncodedImage; Composite: TCompositeImage): TGLuint;
var
  I: Integer;
  TextureCached: TTexture3DCache;
begin
  for I := 0 to Texture3DCaches.Count - 1 do
  begin
    TextureCached := Texture3DCaches[I];

    if (TextureCached.InitialNode = Node) and
       (TextureCached.Filter = Filter) and
       (TextureCached.Anisotropy = Anisotropy) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache and Log then
        WritelnLog('++', '3d texture %s: %d', [PointerToStr(Node), TextureCached.References]);
      Exit(TextureCached.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  {$ifndef OpenGLES} // TODO-OpenGLES3 (3D textures are only available in OpenGLES3)
  glBindTexture(GL_TEXTURE_3D, Result);

  glTextureImage3d(Result, Image, Filter, Composite);

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, TextureWrap.Data[0]);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, TextureWrap.Data[1]);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, TextureWrap.Data[2]);

  TexParameterMaxAnisotropy(GL_TEXTURE_3D, Anisotropy);
  {$endif}

  TextureCached := TTexture3DCache.Create;
  Texture3DCaches.Add(TextureCached);
  TextureCached.InitialNode := Node;
  TextureCached.Filter := Filter;
  TextureCached.Anisotropy := Anisotropy;
  TextureCached.Wrap := TextureWrap;
  TextureCached.References := 1;
  TextureCached.GLName := Result;

  if LogRendererCache and Log then
    WritelnLog('++', '3d texture %s: %d', [PointerToStr(Node), 1]);
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
      if LogRendererCache and Log then
        WritelnLog('--', '3d texture %s: %d', [PointerToStr(Texture3DCaches[I].InitialNode), Texture3DCaches[I].References]);
      if Texture3DCaches[I].References = 0 then
      begin
        glFreeTexture(Texture3DCaches[I].GLName);
        Texture3DCaches.Delete(I);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.Texture3D_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.TextureDepth_IncReference(
  Node: TAbstractTextureNode;
  const TextureWrap: TTextureWrap2D;
  CompareMode: TShadowMapCompareMode;
  const Width, Height: Cardinal;
  const VisualizeDepthMap: boolean): TGLuint;
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

    if (TextureCached.InitialNode = Node) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache and Log then
        WritelnLog('++', 'Depth texture %s: %d', [PointerToStr(Node), TextureCached.References]);
      Exit(TextureCached.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  Filter.Minification := minLinear;
  Filter.Magnification := magLinear;
  SetTextureFilter(GL_TEXTURE_2D, Filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, TextureWrap.Data[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, TextureWrap.Data[1]);

  { OpenGLES: OES_depth_texture allows only GL_UNSIGNED_SHORT
    or GL_UNSIGNED_INT for depth textures. }
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

  {$ifndef OpenGLES} // TODO-es

  if GLFeatures.ARB_shadow then
  begin
    if VisualizeDepthMap then
      CompareMode := smNone;
    case CompareMode of
      smNone:
        begin
          { Using Attributes.VisualizeDepthMap effectively forces
            every shadow map's compareMode to be NONE.
            Although on some GPUs (Radeon X1600 (fglrx, chantal))
            setting compareMode to NONE is not needed (one can use them
            as sampler2D in shaders anyway, and extract depth as grayscale),
            on other GPUs (NVidia GeForce 450 (kocury)) it is needed
            (otherwise depth map only returns 0/1 values, not grayscale).
            Spec suggests it should be needed. }
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE);
        end;
      smCompareRLEqual:
        begin
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC_ARB, GL_LEQUAL);
        end;
      smCompareRGEqual:
        begin
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC_ARB, GL_GEQUAL);
        end;
      else raise EInternalError.Create('Unhandled value for GeneratedShadowMode.compareMode');
    end;

    glTexParameteri(GL_TEXTURE_2D, GL_DEPTH_TEXTURE_MODE_ARB, GL_LUMINANCE);
  end else
    WritelnWarning('VRML/X3D', 'OpenGL doesn''t support ARB_shadow, we cannot set depth comparison for depth texture');

  {$endif}

  TextureCached := TTextureDepthOrFloatCache.Create;
  TextureDepthOrFloatCaches.Add(TextureCached);
  TextureCached.InitialNode := Node;
  TextureCached.References := 1;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GLName := Result;

  if LogRendererCache and Log then
    WritelnLog('++', 'Depth texture %s: %d', [PointerToStr(Node), 1]);
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
      if LogRendererCache and Log then
        WritelnLog('--', 'Depth texture %s: %d', [PointerToStr(TextureDepthOrFloatCaches[I].InitialNode), TextureDepthOrFloatCaches[I].References]);
      if TextureDepthOrFloatCaches[I].References = 0 then
      begin
        glFreeTexture(TextureDepthOrFloatCaches[I].GLName);
        TextureDepthOrFloatCaches.Delete(I);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureDepth_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.TextureFloat_IncReference(
  Node: TAbstractTextureNode;
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

    if (TextureCached.InitialNode = Node) and
       (TextureCached.Wrap = TextureWrap) then
    begin
      Inc(TextureCached.References);
      if LogRendererCache and Log then
        WritelnLog('++', 'Float texture %s: %d', [PointerToStr(Node), TextureCached.References]);
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
  TextureCached.InitialNode := Node;
  TextureCached.References := 1;
  TextureCached.Wrap := TextureWrap;
  TextureCached.GLName := Result;
  { Hm, we probably should store Filter, Precision32
    inside TextureCached as well... Ignore this, useless for now ---
    one Node will require only one float texture anyway. }

  if LogRendererCache and Log then
    WritelnLog('++', 'Float texture %s: %d', [PointerToStr(Node), 1]);
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
      if LogRendererCache and Log then
        WritelnLog('--', 'Float texture %s: %d', [PointerToStr(TextureDepthOrFloatCaches[I].InitialNode), TextureDepthOrFloatCaches[I].References]);
      if TextureDepthOrFloatCaches[I].References = 0 then
      begin
        glFreeTexture(TextureDepthOrFloatCaches[I].GLName);
        TextureDepthOrFloatCaches.Delete(I);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TGLRendererContextCache.TextureFloat_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TGLRendererContextCache.Shape_IncReference(
  Shape: TX3DRendererShape; Fog: IAbstractFogObject;
  ARenderer: TGLRenderer): TShapeCache;
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

    Result := not (
      { If we use any features that (may) render shape differently
        if shape's transform (or other stuff handled outside arrays
        and castlerenderer) changes, then Result must be false. }
      Assigned(ARenderer.Attributes.OnVertexColor) or
      Assigned(ARenderer.Attributes.OnRadianceTransfer) or
      FogVolumetric);
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
begin
  ARenderer.GetFog(Fog, FogEnabled, FogVolumetric,
    FogVolumetricDirection, FogVolumetricVisibilityStart);

  for I := 0 to ShapeCaches.Count - 1 do
  begin
    Result := ShapeCaches[I];
    if (Result.Geometry = Shape.Geometry) and
       Result.Attributes.EqualForShapeCache(ARenderer.Attributes) and
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
      if LogRendererCache and Log then
        WritelnLog('++', 'Shape %s (%s): %d', [PointerToStr(Result), Result.Geometry.X3DType, Result.References]);
      Exit(Result);
    end;
  end;

  { not found, so create new }

  Result := TShapeCache.Create;
  ShapeCaches.Add(Result);
  Result.Attributes := ARenderer.Attributes;
  Result.Geometry := Shape.Geometry;
  Result.State := Shape.State;
  Result.FogVolumetric := FogVolumetric;
  Result.FogVolumetricDirection := FogVolumetricDirection;
  Result.FogVolumetricVisibilityStart := FogVolumetricVisibilityStart;
  Result.References := 1;

  if LogRendererCache and Log then
    WritelnLog('++', 'Shape %s (%s): %d', [PointerToStr(Result), Result.Geometry.X3DType, Result.References]);
end;

procedure TGLRendererContextCache.Shape_DecReference(var ShapeCache: TShapeCache);
var
  I: Integer;
begin
  for I := 0 to ShapeCaches.Count - 1 do
  begin
    if ShapeCaches[I] = ShapeCache then
    begin
      Dec(ShapeCache.References);
      if LogRendererCache and Log then
        WritelnLog('--', 'Shape %s (%s): %d', [PointerToStr(ShapeCache), ShapeCache.Geometry.X3DType, ShapeCache.References]);
      if ShapeCache.References = 0 then
        ShapeCaches.Delete(I);
      ShapeCache := nil;
      Exit;
    end;
  end;

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
      if LogRendererCache and Log then
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

  if LogRendererCache and Log then
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
      if LogRendererCache and Log then
        WritelnLog('--', 'Shader program (hash %s): %d', [ProgramCache.Hash.ToString, ProgramCache.References]);
      if ProgramCache.References = 0 then
        ProgramCaches.Delete(I);
      ProgramCache := nil;
      Exit;
    end;
  end;

  raise EInternalError.Create(
    'TGLRendererContextCache.Program_DecReference: no reference found');
end;

{ TRenderingAttributes --------------------------------------------------- }

procedure TRenderingAttributes.Assign(Source: TPersistent);
begin
  if Source is TRenderingAttributes then
  begin
    OnRadianceTransfer := TRenderingAttributes(Source).OnRadianceTransfer;
    OnVertexColor := TRenderingAttributes(Source).OnVertexColor;
    Lighting := TRenderingAttributes(Source).Lighting;
    UseSceneLights := TRenderingAttributes(Source).UseSceneLights;
    Opacity := TRenderingAttributes(Source).Opacity;
    EnableTextures := TRenderingAttributes(Source).EnableTextures;
    MinificationFilter := TRenderingAttributes(Source).MinificationFilter;
    MagnificationFilter := TRenderingAttributes(Source).MagnificationFilter;
    PointSize := TRenderingAttributes(Source).PointSize;
    LineWidth := TRenderingAttributes(Source).LineWidth;
  end else
    inherited;
end;

function TRenderingAttributes.EqualForShapeCache(
  SecondValue: TRenderingAttributes): boolean;
begin
  Result :=
    (SecondValue.OnRadianceTransfer = OnRadianceTransfer) and
    (SecondValue.OnVertexColor = OnVertexColor) and
    (SecondValue.EnableTextures = EnableTextures);
end;

constructor TRenderingAttributes.Create;
begin
  inherited;

  FLighting := true;
  FUseSceneLights := true;
  FOpacity := 1;
  FEnableTextures := true;
  FMinificationFilter := minLinearMipmapLinear;
  FMagnificationFilter := magLinear;
  FPointSize := DefaultPointSize;
  FLineWidth := DefaultLineWidth;
  FBumpMapping := DefaultBumpMapping;
  FVertexBufferObject := true;
  FShadowSampling := DefaultShadowSampling;
  FDepthTest := true;
  FPhongShading := DefaultPhongShading;
end;

procedure TRenderingAttributes.ReleaseCachedResources;
begin
  { Nothing to do in this class. }
end;

procedure TRenderingAttributes.SetOnRadianceTransfer(
  const Value: TRadianceTransferFunction);
begin
  if OnRadianceTransfer <> Value then
  begin
    ReleaseCachedResources;
    FOnRadianceTransfer := Value;
  end;
end;

procedure TRenderingAttributes.SetOnVertexColor(
  const Value: TVertexColorFunction);
begin
  if OnVertexColor <> Value then
  begin
    ReleaseCachedResources;
    FOnVertexColor := Value;
  end;
end;

procedure TRenderingAttributes.SetEnableTextures(const Value: boolean);
begin
  if EnableTextures <> Value then
  begin
    ReleaseCachedResources;
    FEnableTextures := Value;
  end;
end;

procedure TRenderingAttributes.SetMinificationFilter(const Value: TMinificationFilter);
begin
  if MinificationFilter <> Value then
  begin
    ReleaseCachedResources;
    FMinificationFilter := Value;
  end;
end;

procedure TRenderingAttributes.SetMagnificationFilter(const Value: TMagnificationFilter);
begin
  if MagnificationFilter <> Value then
  begin
    ReleaseCachedResources;
    FMagnificationFilter := Value;
  end;
end;

function TRenderingAttributes.TextureFilter: TTextureFilter;
begin
  Result.Minification := MinificationFilter;
  Result.Magnification := MagnificationFilter;
end;

procedure TRenderingAttributes.SetBumpMapping(const Value: TBumpMapping);
begin
  if BumpMapping <> Value then
  begin
    ReleaseCachedResources;
    FBumpMapping := Value;
  end;
end;

procedure TRenderingAttributes.SetMode(const Value: TRenderingMode);
begin
  FMode := Value;
end;

procedure TRenderingAttributes.SetShadowSampling(const Value: TShadowSampling);
begin
  if FShadowSampling <> Value then
  begin
    { When swithing between VSM and non-VSM sampling methods,
      we need to ReleaseCachedResources, since shadow maps must be regenerated. }
    if (FShadowSampling = ssVarianceShadowMaps) <>
       (Value           = ssVarianceShadowMaps) then
      ReleaseCachedResources;

    FShadowSampling := Value;
  end;
end;

procedure TRenderingAttributes.SetVertexBufferObject(const Value: boolean);
begin
  if VertexBufferObject <> Value then
  begin
    ReleaseCachedResources;
    FVertexBufferObject := Value;
  end;
end;

procedure TRenderingAttributes.SetVisualizeDepthMap(const Value: boolean);
begin
  if VisualizeDepthMap <> Value then
  begin
    ReleaseCachedResources;
    FVisualizeDepthMap := Value;
  end;
end;

function TRenderingAttributes.GetShaders: TShadersRendering;
begin
  if PhongShading then
    Result := srAlways
  else
    Result := srWhenRequired;
end;

procedure TRenderingAttributes.SetShaders(const Value: TShadersRendering);
begin
  PhongShading := Value = srAlways;
end;

procedure TRenderingAttributes.SetPhongShading(const Value: boolean);
begin
  FPhongShading := Value;
end;

{ TGLRenderer ---------------------------------------------------------- }

constructor TGLRenderer.Create(
  AttributesClass: TRenderingAttributesClass;
  ACache: TGLRendererContextCache);
begin
  inherited Create;

  FAttributes := AttributesClass.Create;

  GLTextureNodes := TGLTextureNodes.Create(false);
  ScreenEffectPrograms := TGLSLProgramList.Create;
  TextureTransformUnitsUsedMore := TLongIntList.Create;

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
  FreeAndNil(FAttributes);
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
  FreeArrays(AllVboTypes);
  FreeVBO;
  inherited;
end;

procedure TShapeCache.FreeVBO;
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
end;

procedure TShapeCache.FreeArrays(const Changed: TVboTypes);
begin
  FreeAndNil(Arrays);
  VboToReload := VboToReload + Changed;
end;

procedure TShapeCache.LoadArraysToVbo(DynamicGeometry: boolean);
var
  DataUsage: TGLenum;
  NewVbos: boolean;

  { Bind Vbo buffer and load data. Updates AllocatedSize.
    Uses glBufferSubData if possible, as it may be faster than glBufferData
    (not confirmed by tests, although OpenGL manuals suggest it). }
  procedure BufferData(const VboType: TVboType;
    const Target: TGLenum; const Size: Cardinal; const Data: Pointer);
  begin
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
  Assert(GLFeatures.VertexBufferObject);
  Assert(not Arrays.DataFreed);

  NewVbos := Vbo[vtCoordinate] = 0;
  if NewVbos then
  begin
    glGenBuffers(Ord(High(Vbo)) + 1, @Vbo);
    if Log and LogRenderer then
      WritelnLog('Renderer', Format('Creating and loading data to VBOs (%d,%d,%d)',
        [Vbo[vtCoordinate], Vbo[vtAttribute], Vbo[vtIndex]]));
  end else
  begin
    if Log and LogRenderer then
      WritelnLog('Renderer', Format('Loading data to existing VBOs (%d,%d,%d), reloading %s',
        [Vbo[vtCoordinate], Vbo[vtAttribute], Vbo[vtIndex],
         VboTypesToStr(VboToReload)]));
  end;

  if DynamicGeometry then
    DataUsage := GL_DYNAMIC_DRAW else
    DataUsage := GL_STATIC_DRAW;

  BufferData(vtCoordinate, GL_ARRAY_BUFFER,
    Arrays.Count * Arrays.CoordinateSize, Arrays.CoordinateArray);

  BufferData(vtAttribute, GL_ARRAY_BUFFER,
    Arrays.Count * Arrays.AttributeSize, Arrays.AttributeArray);

  if Arrays.Indexes <> nil then
    BufferData(vtIndex, GL_ELEMENT_ARRAY_BUFFER,
      Arrays.Indexes.Count * SizeOf(TGeometryIndex), Arrays.Indexes.L);

  VboAllocatedUsage := DataUsage;

  Arrays.FreeData;

  { Vbos are fully loaded now. By setting them to empty here,
    we can later at FreeArrays update VboToReload (and this way things
    work even if you call FreeArrays multiple times, the needed updates
    are summed). }
  VboToReload := [];
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
  Shape.EnumerateTextures(@PrepareTexture);
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
  procedure PrepareIDeclsList(Nodes: TX3DNodeList; State: TX3DGraphTraverseState);
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
begin
  if not Node.ShaderLoaded then
  begin
    Assert(Node.Shader = nil);
    Node.ShaderLoaded := true;
    if Node.FdEnabled.Value then
    begin
      { make sure that textures inside shaders are prepared }
      PrepareIDeclsList(Node.FdShaders.Items, Node.StateForShaderPrepare);

      Shader := TShader.Create;
      try
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
          ShaderProgram.UniformNotFoundAction := uaIgnore;

          Node.Shader := ShaderProgram;
          ScreenEffectPrograms.Add(ShaderProgram);
        except on E: EGLSLError do
          begin
            FreeAndNil(ShaderProgram);
            WritelnWarning('VRML/X3D', Format('Cannot use GLSL shader for ScreenEffect: %s',
              [E.Message]));
          end;
        end;
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
  GLTextureNodes.UnprepareAll;
  ScreenEffectPrograms.Count := 0; { this will free programs inside }
end;

function TGLRenderer.BumpMapping: TBumpMapping;
begin
  if (Attributes.BumpMapping <> bmNone) and
    Attributes.EnableTextures and
    (Attributes.Mode = rmFull) and
    GLFeatures.UseMultiTexturing and
    (GLFeatures.Shaders <> gsNone) then
    Result := Attributes.BumpMapping else
    Result := bmNone;
end;

{ Render ---------------------------------------------------------------------- }

procedure TGLRenderer.ActiveTexture(const TextureUnit: Cardinal);
begin
  if GLFeatures.UseMultiTexturing then
    glActiveTexture(GL_TEXTURE0 + TextureUnit);
end;

procedure TGLRenderer.DisableTexture(const TextureUnit: Cardinal);
begin
  if GLFeatures.EnableFixedFunction then
  begin
    { This must be synchronized, and disable all that can be enabled
      by TShape.EnableTexture }
    ActiveTexture(TextureUnit);
    DisableCurrentTexture;
  end;
end;

procedure TGLRenderer.DisableCurrentTexture;
begin
  GLEnableTexture(etNone);
end;

procedure TGLRenderer.GetFog(Node: IAbstractFogObject;
  out Enabled, Volumetric: boolean;
  out VolumetricDirection: TVector3;
  out VolumetricVisibilityStart: Single);
begin
  Enabled := (Attributes.Mode = rmFull) and
    (Node <> nil) and (Node.FdVisibilityRange.Value <> 0.0);
  Volumetric := Enabled and Node.FdVolumetric.Value
    {$ifndef OpenGLES} and GLFeatures.EXT_fog_coord {$endif};

  if Volumetric then
  begin
    VolumetricVisibilityStart :=
      Node.FdVolumetricVisibilityStart.Value * Node.TransformScale;
    VolumetricDirection := Node.FdVolumetricDirection.Value;
  end else
  begin
    { whatever, just set them to any determined values }
    VolumetricVisibilityStart := 0;
    VolumetricDirection := TVector3.Zero;
  end;
end;

procedure TGLRenderer.RenderCleanState(const Beginning: boolean);

  procedure DisabeAllTextureUnits;
  var
    I: Integer;
  begin
    for I := 0 to GLFeatures.MaxTextureUnits - 1 do
      DisableTexture(I);
  end;

{$ifndef OpenGLES}
var
  I: Integer;
{$endif}
begin
  DisabeAllTextureUnits;

  { Restore active texture unit to 0 }
  if GLFeatures.UseMultiTexturing then
  begin
    ActiveTexture(0);
    {$ifndef OpenGLES}
    glClientActiveTexture(GL_TEXTURE0);
    {$endif}
  end;

  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    glMatrixMode(GL_MODELVIEW);

    { Reset GL_TEXTURE_ENV, otherwise it may be left GL_COMBINE
      after rendering X3D model using MultiTexture. }
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    {$endif}
  end;

  RenderContext.PointSize := Attributes.PointSize;
  RenderContext.LineWidth := Attributes.LineWidth;

  if Beginning then
  begin
    FLineType := ltSolid;
    {$ifndef OpenGLES}
    glDisable(GL_LINE_STIPPLE);
    {$endif}
  end else
    LineType := ltSolid;

  GLSetEnabled(GL_DEPTH_TEST, Beginning and Attributes.DepthTest);

  if GLFeatures.EnableFixedFunction and (Attributes.Mode in [rmDepth, rmFull]) then
  begin
    {$ifndef OpenGLES}
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_GEN_R);
    glDisable(GL_TEXTURE_GEN_Q);
    {$endif}

    { Initialize FFixedFunctionAlphaTest, make sure OpenGL state is appropriate }
    FFixedFunctionAlphaTest := false;
    {$ifndef OpenGLES}
    glDisable(GL_ALPHA_TEST);
    {$endif}

    { We only use glAlphaFunc for textures, and there this value is suitable.
      We never change glAlphaFunc during rendering, so no need to call this in RenderEnd. }
    {$ifndef OpenGLES}
    if Beginning then
      glAlphaFunc(GL_GEQUAL, 0.5);
    {$endif}
  end;

  if GLFeatures.EnableFixedFunction and (Attributes.Mode = rmFull) then
  begin
    {$ifndef OpenGLES}
    glDisable(GL_COLOR_MATERIAL);

    { We don't really need to enable GL_NORMALIZE.
      We always provide normalized normals (that's how arraysgenerator.pas
      and vrmlmeshrenderer.inc always calculate them, and when provided
      in VRML/X3D they should also be already normalized).
      However, turning GL_NORMALIZE off doesn't give us *any* performance
      benefit as far as I tested (with castle gate, on high-end GPUs
      like Radeon X1600 and low-end like Intel).

      So leave GL_NORMALIZE enabled, it's still useful:
      - for invalid VRML/X3D files that have unnomalized normals.
      - in case caller loaded a scaling matrix
        (for example, Examine camera may allow user to scale the object). }
    GLSetEnabled(GL_NORMALIZE, Beginning);

    if not GLVersion.BuggyLightModelTwoSide then
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE) else
    if Log then
      WritelnLog('Lighting', GLVersion.BuggyLightModelTwoSideMessage);

    {$endif}

    { Initialize FSmoothShading, make sure OpenGL state is appropriate }
    FSmoothShading := true;
    {$ifndef OpenGLES}
    glShadeModel(GL_SMOOTH);
    {$endif}

    if Beginning then
    begin
      { Initialize FFixedFunctionLighting, make sure OpenGL state is appropriate }
      FFixedFunctionLighting := Attributes.Lighting;
      {$ifndef OpenGLES}
      GLSetEnabled(GL_LIGHTING, FFixedFunctionLighting);
      {$endif}
    end else
      {$ifndef OpenGLES}
      glDisable(GL_LIGHTING);
      {$endif}

    {$ifndef OpenGLES}

    { No need to disable lights at the beginning.
      LightsRenderer already assumes that state of lights is initially unknown,
      and handles it. }
    if not Beginning then
      for I := 0 to GLFeatures.MaxLights - 1 do
        glDisable(GL_LIGHT0 + I);

    glDisable(GL_FOG);

    { - We always set diffuse material component from the color.
        This satisfies all cases.
      - TColorPerVertexCoordinateRenderer.RenderCoordinateBegin
        takes care of actually enabling COLOR_MATERIAL, it assumes that
        the state is as set below.
      - We never change glColorMaterial during rendering,
        so no need to call this in RenderEnd. }
    if Beginning then
      glColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE);
    {$endif}
  end else
  begin
    FSmoothShading := true;
    if Beginning then
      { Initialize FFixedFunctionLighting, make sure OpenGL state is appropriate }
      FFixedFunctionLighting := Attributes.Lighting;
  end;
end;

procedure TGLRenderer.RenderBegin(ABaseLights: TLightInstancesList;
  LightRenderEvent: TLightRenderEvent; const APass: TRenderingPass);
begin
  BaseLights := ABaseLights;
  Pass := APass;

  RenderCleanState(true);

  if GLFeatures.EnableFixedFunction then
  begin
    { push matrix after RenderCleanState, to be sure we're in modelview mode }
    {$ifndef OpenGLES}
    glPushMatrix;

    if Attributes.Mode = rmSolidColor then
      glColorv(Attributes.SolidColor);
    {$endif}
  end;

  Assert(FogNode = nil);
  Assert(not FogEnabled);

  LightsRenderer := TVRMLGLLightsRenderer.Create(LightRenderEvent);
end;

procedure TGLRenderer.RenderEnd;
begin
  { Tests:
  Writeln('LightsRenderer stats: light setups done ',
    LightsRenderer.Statistics[true], ' vs avoided ',
    LightsRenderer.Statistics[false]); }

  FreeAndNil(LightsRenderer);

  FogNode := nil;
  FogEnabled := false;

  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}
    glPopMatrix;
    {$endif}
  end;

  RenderCleanState(false);

  { restore defaults }
  RenderContext.CullFace := false;
  RenderContext.FrontFaceCcw := true;
  TGLSLProgram.Current := nil;
end;

procedure TGLRenderer.RenderShape(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject);

  function ShapeMaybeUsesShadowMaps(Shape: TX3DRendererShape): boolean;
  var
    Tex, SubTexture: TX3DNode;
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
        for SubTexture in TMultiTextureNode(Tex).FdTexture.Items do
          if SubTexture is TGeneratedShadowMapNode then
            Exit(true);
      end;
    end;
  end;

var
  PhongShading: boolean;
  Shader: TShader;
begin
  { instead of TShader.Create, reuse existing PreparedShader for speed }
  Shader := PreparedShader;
  Shader.Clear;
  Shader.SeparateDiffuseTexture := Attributes.SeparateDiffuseTexture;

  { calculate PhongShading }
  PhongShading := Attributes.PhongShading;
  { if Shape specifies Shading = Gouraud or Phong, use it }
  if Shape.Node <> nil then
    if Shape.Node.Shading = shPhong then
      PhongShading := true
    else
    if Shape.Node.Shading = shGouraud then
      PhongShading := false;
  { if some feature requires PhongShading, make it true }
  if ShapeMaybeUsesPhongSurfaceTexture(Shape) or
     ShapeMaybeUsesShadowMaps(Shape) then
    PhongShading := true;

  Shader.Initialize(PhongShading);

  if PhongShading then
    Shader.ShapeRequiresShaders := true;

  Shader.ShapeBoundingBox := Shape.BoundingBox;
  Shader.ShadowSampling := Attributes.ShadowSampling;
  RenderShapeLineProperties(Shape, Fog, Shader);
end;

procedure TGLRenderer.RenderShapeLineProperties(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader);
var
  LP: TLinePropertiesNode;
begin
  if Shape.Node <> nil then { Shape.Node is nil for VRML <= 1.0 }
    LP := Shape.Node.LineProperties else
    LP := nil;
  if (LP <> nil) and LP.FdApplied.Value then
  begin
    RenderContext.LineWidth := Max(1.0, Attributes.LineWidth * LP.FdLineWidthScaleFactor.Value);
    LineType := LP.LineType;
  end else
  begin
    RenderContext.LineWidth := Attributes.LineWidth;
    LineType := ltSolid;
  end;

  RenderShapeMaterials(Shape, Fog, Shader);
end;

procedure TGLRenderer.RenderShapeMaterials(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader);

  {$I castlerenderer_materials.inc}

begin
  RenderMaterialsBegin;
  RenderShapeLights(Shape, Fog, Shader, MaterialOpacity, Lighting);
end;

procedure TGLRenderer.RenderShapeLights(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader;
  const MaterialOpacity: Single; const Lighting: boolean);
var
  SceneLights: TLightInstancesList;
begin
  { This is done after setting Shader.MaterialSpecularColor
    by RenderMaterialsBegin,
    as MaterialSpecularColor must be already set during Shader.EnableLight. }

  { Shape.ModelView is not yet multiplied by State.Transform,
    and it contains only camera and scene transform. }
  Shader.SceneModelView := Shape.ModelView;

  { When lighting is off (for either shaders or fixed-function),
    there is no point in setting up lights. }
  if Lighting then
  begin
    if Attributes.UseSceneLights then
      SceneLights := Shape.State.Lights else
      SceneLights := nil;

    LightsRenderer.Render(BaseLights, SceneLights, Shader);
  end;

  RenderShapeFog(Shape, Fog, Shader, MaterialOpacity, Lighting);
end;

procedure TGLRenderer.RenderShapeFog(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader;
  const MaterialOpacity: Single; const Lighting: boolean);

const
  FogCoordinateSource: array [boolean { volumetric }] of TFogCoordinateSource =
  ( fcDepth, fcPassedCoordinate );

  { Set OpenGL fog based on given fog node. Returns also fog parameters,
    like GetFog. }
  procedure RenderFog(Node: IAbstractFogObject;
    out Volumetric: boolean;
    out VolumetricDirection: TVector3;
    out VolumetricVisibilityStart: Single);
  var
    VisibilityRangeScaled: Single;
  const
    FogDensityFactor = 3.0;
  begin
    GetFog(Node, FogEnabled, Volumetric, VolumetricDirection, VolumetricVisibilityStart);

    if FogEnabled then
    begin
      Assert(Node <> nil);

      VisibilityRangeScaled := Node.FdVisibilityRange.Value * Node.TransformScale;

      if GLFeatures.EnableFixedFunction then
      begin
        {$ifndef OpenGLES}
        { This code does not need to be executed on shader pipeline at all,
          where we know that fog coord is possible and will be realized by passing
          castle_FogCoord to shader. }

        if Node.FdVolumetric.Value and (not GLFeatures.EXT_fog_coord) then
        begin
          { Try to make normal fog that looks similar. This looks poorly,
            but it's not a real problem --- EXT_fog_coord is supported
            on all sensible GPUs nowadays. Increasing VisibilityRangeScaled
            seems enough. }
          WritelnWarning('VRML/X3D', 'Volumetric fog not supported, your graphic card (OpenGL) doesn''t support EXT_fog_coord');
          VisibilityRangeScaled := VisibilityRangeScaled * 5;
        end;

        if Volumetric then
        begin
          glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);
        end else
        begin
          { If not Volumetric but still GL_EXT_fog_coord, we make sure
            that we're *not* using FogCoord below. }
          if GLFeatures.EXT_fog_coord then
            glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
        end;
        {$endif}
      end;

      { calculate FogType and other Fog parameters }
      FogType := Node.FogType;
      FogColor := Node.FdColor.Value;
      case FogType of
        ftLinear: FogLinearEnd := VisibilityRangeScaled;
        ftExp   : FogExpDensity := FogDensityFactor / VisibilityRangeScaled;
        else raise EInternalError.Create('TGLRenderer.RenderShapeFog:FogType?');
      end;
    end;
  end;

begin
  { Enable / disable fog and set fog parameters if needed }
  if Fog <> FogNode then
  begin
    FogNode := Fog;
    RenderFog(FogNode, FogVolumetric,
      FogVolumetricDirection, FogVolumetricVisibilityStart);

    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      { Set fixed-function fog parameters. }
      if FogEnabled then
      begin
        glFogv(GL_FOG_COLOR, Vector4(FogColor, 1.0));
        case FogType of
          ftLinear:
            begin
              glFogi(GL_FOG_MODE, GL_LINEAR);
              glFogf(GL_FOG_START, 0);
              glFogf(GL_FOG_END, FogLinearEnd);
            end;
          ftExp: begin
              glFogi(GL_FOG_MODE, GL_EXP);
              glFogf(GL_FOG_DENSITY, FogExpDensity);
            end;
          else raise EInternalError.Create('TGLRenderer.RenderShapeFog:FogType? 2');
        end;
        { We want to be able to render any scene --- so we have to be prepared
          that fog interpolation has to be corrected for perspective. }
        glHint(GL_FOG_HINT, GL_NICEST);
        glEnable(GL_FOG);
      end else
        glDisable(GL_FOG);
      {$endif}
    end;
  end;

  if FogEnabled then
    Shader.EnableFog(FogType, FogCoordinateSource[FogVolumetric],
      FogColor, FogLinearEnd, FogExpDensity);
  RenderShapeTextureTransform(Shape, Fog, Shader, MaterialOpacity, Lighting);
end;

procedure TGLRenderer.RenderShapeTextureTransform(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader;
  const MaterialOpacity: Single; const Lighting: boolean);
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
    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      glMatrixMode(GL_TEXTURE);
      {$endif}
    end;

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

          TODO: what to do with CommonSurfaceShader ? }
        if (State.DiffuseAlphaTexture <> nil) and
           (not (State.DiffuseAlphaTexture is TMultiTextureNode)) then
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

    if GLFeatures.EnableFixedFunction then
    begin
      {$ifndef OpenGLES}
      { restore GL_MODELVIEW }
      glMatrixMode(GL_MODELVIEW);
      {$endif}
    end;
  end;

  RenderShapeClipPlanes(Shape, Fog, Shader, MaterialOpacity, Lighting);

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

procedure TGLRenderer.RenderShapeClipPlanes(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader;
  const MaterialOpacity: Single; const Lighting: boolean);
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
        ClipPlane := ClipPlanes.Ptr(I);
        if ClipPlane^.Node.FdEnabled.Value then
        begin
          Assert(ClipPlanesEnabled < GLFeatures.MaxClipPlanes);

          { Note: do *not* multiply

              ClipPlane^.Transform * ClipPlane^.Node.Plane

            The plane equation cannot be transformed like that,
            it's not a 4D vertex / direction.

            OpenGL does smart calculatations such that we can
            provide modelview matrix, and then specify
            plane equation in the local space.
            See e.g. http://www2.imm.dtu.dk/~jab/texgen.pdf .
            glClipPlane docs say that glClipPlane is multiplied by
            the *inverse* of modelview. The wording is crucial here:
            plane is multiplied by the matrix, not the other way around. }

          Shader.EnableClipPlane(ClipPlanesEnabled,
            ClipPlane^.Transform, ClipPlane^.Node.FdPlane.Value);
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
    for I := 0 to ClipPlanesEnabled - 1 do
      Shader.DisableClipPlane(I);
    ClipPlanesEnabled := 0; { not really needed, but for safety... }
  end;

begin
  ClipPlanesBegin(Shape.State.ClipPlanes);

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
  begin
    glPushMatrix;
      glMultMatrix(Shape.State.Transform);
  end;
  {$endif}

    Shape.ModelView := Shape.ModelView * Shape.State.Transform;
    RenderShapeCreateMeshRenderer(Shape, Fog, Shader, MaterialOpacity, Lighting);

  {$ifndef OpenGLES}
  if GLFeatures.EnableFixedFunction then
    glPopMatrix;
  {$endif}

  ClipPlanesEnd;
end;

procedure TGLRenderer.RenderShapeCreateMeshRenderer(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader;
  const MaterialOpacity: Single; const Lighting: boolean);
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
      ShapeBumpMappingAllowed := GeneratorClass.BumpMappingAllowed;
    end;
  end;

begin
  { default ShapeBumpMapping* state }
  ShapeBumpMappingAllowed := false;
  ShapeBumpMappingUsed := false;

  { Initalize MeshRenderer to something non-nil. }
  if not InitMeshRenderer then
  begin
    WritelnWarning('VRML/X3D', Format('Rendering of node kind "%s" not implemented',
      [Shape.NiceName]));
    Exit;
  end;

  Assert(MeshRenderer <> nil);

  try
    RenderShapeShaders(Shape, Fog, Shader, MaterialOpacity, Lighting,
      GeneratorClass, MeshRenderer);
  finally
    FreeAndNil(MeshRenderer);
  end;
end;

{$define MeshRenderer := TMeshRenderer(ExposedMeshRenderer) }

procedure TGLRenderer.RenderShapeShaders(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader;
  const MaterialOpacity: Single; const Lighting: boolean;
  GeneratorClass: TArraysGeneratorClass;
  ExposedMeshRenderer: TObject);
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
      if Log and (TCD > UsedGLSLTexCoordsNeeded) then
        WritelnLog('TexCoord', Format('Texture coords defined in VRML/X3D for %d texture units, using them all, even though we bound only %d texture units. Reason: GLSL shaders may use them',
          [TCD, UsedGLSLTexCoordsNeeded]));
      MaxVar(UsedGLSLTexCoordsNeeded, TCD);
    end;
  end;

  RenderShapeTextures(Shape, Fog, Shader, MaterialOpacity, Lighting,
    GeneratorClass, MeshRenderer, UsedGLSLTexCoordsNeeded);
end;

procedure TGLRenderer.RenderShapeTextures(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader;
  const MaterialOpacity: Single; const Lighting: boolean;
  GeneratorClass: TArraysGeneratorClass;
  ExposedMeshRenderer: TObject;
  UsedGLSLTexCoordsNeeded: Cardinal);

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
    if Texture.CompareMode <> smNone then
    begin
      GLTexture := GLTextureNodes.TextureNode(Texture);
      if GLTexture <> nil then
        GLTexture.EnableAll(GLFeatures.MaxTextureUnits, TexCoordsNeeded, Shader);
    end;
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

  procedure RenderTexturesBegin;
  var
    TextureNode: TAbstractTextureNode;
    GLTextureNode: TGLTextureNode;
    AlphaTest: boolean;
    FontTextureNode: TAbstractTexture2DNode;
    GLFontTextureNode: TGLTextureNode;
    TexturesAlphaChannel: TAlphaChannel;
  begin
    TexCoordsNeeded := 0;
    BoundTextureUnits := 0;

    if Attributes.Mode = rmSolidColor then
      Exit;

    AlphaTest := false;

    TextureNode := Shape.State.DiffuseAlphaTexture;
    GLTextureNode := GLTextureNodes.TextureNode(TextureNode);
    { assert we never have non-nil GLTextureNode and nil TextureNode }
    Assert((GLTextureNode = nil) or (TextureNode <> nil));

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
    if Attributes.EnableTextures and
       NodeTextured(Shape.Geometry) then
    begin
      { This works also for TextureNode being TMultiTextureNode,
        since it has smartly calculated AlphaChannel based on children. }
      TexturesAlphaChannel := acNone;
      if TextureNode <> nil then
        AlphaMaxVar(TexturesAlphaChannel, TextureNode.AlphaChannelFinal);
      if FontTextureNode <> nil then
        AlphaMaxVar(TexturesAlphaChannel, FontTextureNode.AlphaChannelFinal);
      AlphaTest := TexturesAlphaChannel = acTest;

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
        { This means that Shape.State.DiffuseAlphaTexture comes
          from CommonSurfaceShader (or a weird mix of VRML 1.0 and X3D which
          is undefined).
          Make sure to still enable shadow maps from Shape.Appearance.Texture
          then. }
        EnableShadowMaps(Shape.Node.Appearance.Texture, TexCoordsNeeded, Shader);
      end;

      { If there is special texture like a normalmap, enable it. }
      BumpMappingEnable(Shape.State, BoundTextureUnits, TexCoordsNeeded, Shader);
      SurfaceTexturesEnable(Shape.State, BoundTextureUnits, TexCoordsNeeded, Shader);
    end;

    { Set alpha test enabled state for OpenGL (shader and fixed-function).
      We handle here textures with simple (yes/no) alpha channel.

      This is not necessarily perfect, as OpenGL will test the
      final alpha := material alpha mixed with all multi-textures alpha.
      So anything using blending (material using transparency,
      or other texture will full-range alpha channel) will modify the actual
      alpha tested. This isn't really correct --- we would prefer to only
      test the alpha of textures with yes/no alpha channel.
      But there's no way to fix it in fixed-function pipeline.
      May be handled better in shader pipeline someday (alpha test should
      be done for texture colors). }

    FixedFunctionAlphaTest := AlphaTest;
    if AlphaTest then
      Shader.EnableAlphaTest;

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
    for I := 0 to TexCoordsNeeded - 1 do
      DisableTexture(I);
  end;

begin
  RenderTexturesBegin;
  try
    RenderShapeInside(Shape, Fog, Shader, MaterialOpacity, Lighting,
      GeneratorClass, MeshRenderer);
  finally RenderTexturesEnd end;
end;

procedure TGLRenderer.RenderShapeInside(Shape: TX3DRendererShape;
  Fog: IAbstractFogObject; Shader: TShader;
  const MaterialOpacity: Single; const Lighting: boolean;
  GeneratorClass: TArraysGeneratorClass;
  ExposedMeshRenderer: TObject);
var
  Generator: TArraysGenerator;
  CoordinateRenderer: TBaseCoordinateRenderer;
  VBO: boolean;
begin
  { initialize TBaseCoordinateRenderer.Arrays now }
  if GeneratorClass <> nil then
  begin
    Assert(MeshRenderer is TBaseCoordinateRenderer);
    CoordinateRenderer := TBaseCoordinateRenderer(MeshRenderer);

    { calculate Shape.Cache }
    if Shape.Cache = nil then
      Shape.Cache := Cache.Shape_IncReference(Shape, Fog, Self);

    VBO := Attributes.VertexBufferObject and GLFeatures.VertexBufferObject;

    { calculate Shape.Cache.Arrays }
    if Shape.Cache.Arrays = nil then
    begin
      Generator := GeneratorClass.Create(Shape, true);
      try
        Generator.TexCoordsNeeded := TexCoordsNeeded;
        Generator.MaterialOpacity := MaterialOpacity;
        Generator.FogVolumetric := FogVolumetric;
        Generator.FogVolumetricDirection := FogVolumetricDirection;
        Generator.FogVolumetricVisibilityStart := FogVolumetricVisibilityStart;
        Generator.ShapeBumpMappingUsed := ShapeBumpMappingUsed;
        Generator.ShapeBumpMappingTextureCoordinatesId := ShapeBumpMappingTextureCoordinatesId;
        Generator.OnVertexColor := Attributes.OnVertexColor;
        Generator.OnRadianceTransfer := Attributes.OnRadianceTransfer;
        Shape.Cache.Arrays := Generator.GenerateArrays;
      finally FreeAndNil(Generator) end;

      { Always after regenerating Shape.Cache.Arrays, reload Shape.Cache.Vbo contents }
      if VBO then
        Shape.LoadArraysToVbo;
    end else
    begin
      { Arrays contents are already loaded, make sure that Vbo are loaded too
        (in case Arrays were loaded previously, when VBO = false). }
      if VBO and (Shape.Cache.Vbo[vtCoordinate] = 0) then
        Shape.LoadArraysToVbo;
    end;

    if VBO then
    begin
      { Shape.Arrays contents are already loaded,
        so Vbo contents are already loaded too }
      Assert(Shape.Cache.Vbo[vtCoordinate] <> 0);
      CoordinateRenderer.Vbo := Shape.Cache.Vbo;
    end;

    CoordinateRenderer.Arrays := Shape.Cache.Arrays;
    CoordinateRenderer.Shader := Shader;
    CoordinateRenderer.BoundTextureUnits := BoundTextureUnits;
    CoordinateRenderer.Lighting := Lighting;
  end;

  MeshRenderer.PrepareRenderShape := PrepareRenderShape;
  MeshRenderer.Render;

  if (GeneratorClass <> nil) and VBO then
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

procedure TGLRenderer.UpdateGeneratedTextures(Shape: TX3DRendererShape;
  TextureNode: TAbstractTextureNode;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  var NeedsRestoreViewport: boolean;
  CurrentViewpoint: TAbstractViewpointNode;
  CameraViewKnown: boolean;
  const CameraPosition, CameraDirection, CameraUp: TVector3);
var
  { Only for CheckUpdateField and PostUpdateField }
  SavedHandler: TGeneratedTextureHandler;

  { Look at the "update" field's value, decide whether we need updating.
    Will take care of making warning on incorrect "update". }
  function CheckUpdate(Handler: TGeneratedTextureHandler): boolean;
  var
    Update: TTextureUpdate;
  begin
    SavedHandler := Handler; { for PostUpdateField }
    Update := Handler.Update.Value;
    Result :=
        (Update = upNextFrameOnly) or
      ( (Update = upAlways) and Handler.UpdateNeeded );
  end;

  { Call this after CheckUpdateField returned @true and you updated
    the texture.
    Will take care of sending "NONE" after "NEXT_FRAME_ONLY". }
  procedure PostUpdate;
  begin
    if SavedHandler.Update.Value = upNextFrameOnly then
      SavedHandler.Update.Send(upNone);
    SavedHandler.UpdateNeeded := false;
  end;

  procedure UpdateGeneratedCubeMap(TexNode: TGeneratedCubeMapTextureNode);
  var
    GLNode: TGLGeneratedCubeMapTextureNode;
  begin
    { Shape.BoundingBox must be non-empty, otherwise we don't know from what
      3D point to capture environment. }
    if Shape.BoundingBox.IsEmpty then Exit;

    if CheckUpdate(TexNode.GeneratedTextureHandler) then
    begin
      GLNode := TGLGeneratedCubeMapTextureNode(GLTextureNodes.TextureNode(TexNode));
      if GLNode <> nil then
      begin
        GLNode.Update(Render, ProjectionNear, ProjectionFar,
          NeedsRestoreViewport, Shape.BoundingBox.Center + TexNode.FdBias.Value);

        PostUpdate;

        if Log and LogRenderer then
          WritelnLog('CubeMap', TexNode.NiceName + ' texture regenerated');
      end;
    end;
  end;

  procedure UpdateGeneratedShadowMap(TexNode: TGeneratedShadowMapNode);
  var
    GLNode: TGLGeneratedShadowMap;
  begin
    if CheckUpdate(TexNode.GeneratedTextureHandler) then
    begin
      if (TexNode.FdLight.Value <> nil) and
         (TexNode.FdLight.Value is TAbstractLightNode) then
      begin
        GLNode := TGLGeneratedShadowMap(GLTextureNodes.TextureNode(TexNode));
        if GLNode <> nil then
        begin
          GLNode.Update(Render, ProjectionNear, ProjectionFar,
            NeedsRestoreViewport,
            TAbstractLightNode(TexNode.FdLight.Value));

          PostUpdate;

          if Log and LogRenderer then
            WritelnLog('GeneratedShadowMap', TexNode.NiceName + ' texture regenerated');
        end;
      end else
        WritelnWarning('VRML/X3D', TexNode.NiceName + ' needs updating, but light = NULL or incorrect');
    end;
  end;

  procedure UpdateRenderedTexture(TexNode: TRenderedTextureNode);
  var
    GLNode: TGLRenderedTextureNode;
    GeometryCoordsField: TMFVec3f;
    GeometryCoords: TVector3List;
  begin
    if CheckUpdate(TexNode.GeneratedTextureHandler) then
    begin
      GLNode := TGLRenderedTextureNode(GLTextureNodes.TextureNode(TexNode));
      if GLNode <> nil then
      begin
        { calculate GeometryCoords }
        GeometryCoords := nil;
        if Shape.Geometry.InternalCoord(Shape.State, GeometryCoordsField) and
           (GeometryCoordsField <> nil) then
          GeometryCoords := GeometryCoordsField.Items;

        GLNode.Update(Render, ProjectionNear, ProjectionFar,
          NeedsRestoreViewport,
          CurrentViewpoint, CameraViewKnown,
          CameraPosition, CameraDirection, CameraUp,
          Shape.BoundingBox,
          Shape.State.Transform,
          GeometryCoords,
          Shape.MirrorPlaneUniforms);

        PostUpdate;

        if Log and LogRenderer then
          WritelnLog('RenderedTexture', TexNode.NiceName + ' texture regenerated');
      end;
    end;
  end;

begin
  if TextureNode is TGeneratedCubeMapTextureNode then
    UpdateGeneratedCubeMap(TGeneratedCubeMapTextureNode(TextureNode)) else
  if TextureNode is TGeneratedShadowMapNode then
    UpdateGeneratedShadowMap(TGeneratedShadowMapNode(TextureNode)) else
  if TextureNode is TRenderedTextureNode then
    UpdateRenderedTexture(TRenderedTextureNode(TextureNode));
end;

procedure TGLRenderer.SetSmoothShading(const Value: boolean);
begin
  if FSmoothShading <> Value then
  begin
    FSmoothShading := Value;
    {$ifndef OpenGLES} //TODO-es
    if Value then
      glShadeModel(GL_SMOOTH) else
      glShadeModel(GL_FLAT);
    {$endif}
  end;
end;

procedure TGLRenderer.SetFixedFunctionLighting(const Value: boolean);
begin
  if FFixedFunctionLighting <> Value then
  begin
    FFixedFunctionLighting := Value;
    {$ifndef OpenGLES}
    GLSetEnabled(GL_LIGHTING, FixedFunctionLighting);
    {$endif}
  end;
end;

procedure TGLRenderer.SetFixedFunctionAlphaTest(const Value: boolean);
begin
  if FFixedFunctionAlphaTest <> Value then
  begin
    FFixedFunctionAlphaTest := Value;
    {$ifndef OpenGLES}
    GLSetEnabled(GL_ALPHA_TEST, FixedFunctionAlphaTest);
    {$endif}
  end;
end;

procedure TGLRenderer.SetLineType(const Value: TLineType);
begin
  if FLineType <> Value then
  begin
    FLineType := Value;
    {$ifndef OpenGLES}
    case LineType of
      ltSolid: glDisable(GL_LINE_STIPPLE);
      ltDashed      : begin glLineStipple(1, $00FF); glEnable(GL_LINE_STIPPLE); end;
      ltDotted      : begin glLineStipple(1, $CCCC); glEnable(GL_LINE_STIPPLE); end;
      ltDashedDotted: begin glLineStipple(1, $FFCC); glEnable(GL_LINE_STIPPLE); end;
      ltDashDotDot  : begin glLineStipple(1, $FCCC); glEnable(GL_LINE_STIPPLE); end;
      else raise EInternalError.Create('LineType?');
    end;
    {$endif}
  end;
end;

end.
