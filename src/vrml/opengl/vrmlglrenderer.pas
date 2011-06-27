{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML low-level rendering (TVRMLGLRenderer).
  You usually don't want to use this renderer directly, you should
  rather use TVRMLGLScene that wraps this renderer and gives you simple
  method to render whole scene.

  The overview of this class can also be found in my master's thesis
  [http://vrmlengine.sourceforge.net/vrml_engine_doc.php]
  in chapter "OpenGL rendering", section "Basic OpenGL rendering".

  @bold(Usage:)

  @orderedList(
    @item(
      Call @link(TVRMLGLRenderer.Prepare) for all
      the states that you want to later render. The order of calling TVRMLGLRenderer.Prepare
      methods doesn't matter, also you are free to prepare states that you
      will not actually use later. Of course a state, once prepared,
      may be used in rendering as many times as you want.

      It's important that you have to prepare @italic(every state that
      you plan to later render). During rendring the state
      must have exactly the same (fields, properties) values as when
      it was prepared. In particular, it must have the same
      pointers to nodes Last*/Active* and their contents
      also must be the same. TVRMLGLRenderer.Prepare
      may save some associations between objects and OpenGL resources,
      so it's important that the same pointer must always point to the
      same object (until it's unprepared).

      TVRMLGLRenderer.Prepare requires active OpenGL context. It doesn't modify
      OpenGL state (only allocates some resources like texture names).
      It cannot be called inside a display list.
    )

    @item(
      When you want to release resources, you should call TVRMLGLRenderer.Unprepare on
      nodes that you want to change or free. This should be used
      with nodes that were passed as Last*/Active* in some State for TVRMLGLRenderer.Prepare.

      Note: before engine 2.0.0 release, it was allowed to free some VRML nodes
      @italic(before) unpreparing them. This was depending on the fact that
      during unprepare we will not actually dereference pointers
      (not look at nodes contents etc.). This is forbidden since 2010-03-25,
      as it causes some difficult problems (like TVRMLShaderProgram.Destroy
      really needs to access some VRML nodes), and was inherently unclean
      and unsafe (it's not a nice programming practice to have a pointers
      that may be invalid).
    )

    @item(
      To start actual rendering, call TVRMLGLRenderer.RenderBegin. To end rendering, call
      TVRMLGLRenderer.RenderEnd. Between these calls, you should not touch OpenGL state
      yourself --- the renderer may depend that every state change goes
      through it. At the end of TVRMLGLRenderer.RenderEnd, the OpenGL state is restored
      just as it was before TVRMLGLRenderer.RenderBegin.
    )

    @item(
      Between TVRMLGLRenderer.RenderBegin and TVRMLGLRenderer.RenderEnd
      you should render the shapes by calling RenderShape.

      Remember that you can render only shapes that have Shape.State
      prepared by TVRMLGLRenderer.Prepare.
    )

    @item(
      Since the first prepare / render calls, this renderer assumes it's
      always called in the same OpenGL context. To break association
      with OpenGL context call TVRMLGLRenderer.UnprepareAll (this is like calling TVRMLGLRenderer.Unprepare
      on every prepared thing + clearing some remaining resources).
    )
  )

  @bold(OpenGL state affecting VRML rendering:)

  Some OpenGL state is unconditionally reset by TVRMLGLRenderer.RenderBegin.
  See TVRMLRenderingAttributes.PreserveOpenGLState.

  There's also some OpenGL state that we let affect our rendering.
  This allows you to customize VRML rendering by using normal OpenGL commands.

  @unorderedList(
    @item(First of all, current matrix values (MODELVIEW,
      PROJECTION and TEXTURE) affect our rendering as usual.

      So you can move the rendered VRML model by normal OpenGL
      matrix transformations, you can even affect rendered texture coords
      by your own texture matrix etc.)

    @item(Current glLightModel (GL_LIGHT_MODEL_AMBIENT) setting.

      Note that VRML 1.0 specification requires GL_LIGHT_MODEL_AMBIENT
      to be (0.2, 0.2, 0.2), which is equal with default OpenGL value.
      Note that VRML 97 lighting equations suggest that GL_LIGHT_MODEL_AMBIENT
      should be zero.)

    @item(Current glPolygonMode.

      Of course for normal rendering you want to render polygons
      (both sides, GL_FRONT_AND_BACK) with GL_FILL. But you can change
      it to get wireframe model view.)

    @item(Blending settings (GL_BLEND enabled state, glBlendFunc),
      and glDepthMask.

      These are typically controlled by higher-level renderer (VRMLGLScene)
      to allow rendering scenes with both tranparent and opaque objects.
      Only such higher-level renderer may control them, as only it controls
      the order of rendering shapes, which is important for rendering
      tranparent shapes.)

    @item(Current GL_FOG_HINT.

      Just like for any other OpenGL program, you may want to set this
      to GL_NICEST (if you have to render models that may look bad
      when fog is interpolated without perspective correction).)

    @item(glFrontFace is assumed to be CCW (OpenGL default) but not manipulated
      by this unit anywhere.

      So our normals passed to OpenGL always point from CCW side.
      Even if you supplied in VRML file normals pointing from CW
      (indicated e.g. by IndexedFaceSet.ccw = FALSE field in VRML 97),
      we will internally invert them and pass inverted ones to OpenGL.
      And when culling faces, we switch using @code(glCullFace(
      GL_BACK / GL_FRONT)), not by switching front face.

      Why so much work was done to always work with front face = CCW assumption?
      Because this is very handy when you render mirrors by using
      @code(Scale(1, 1, -1)) trick. See
      [http://www.opengl.org/resources/code/samples/mjktips/Reflect.html]
      and example program
      @code(kambi_vrml_game_engine/examples/vrml/plane_mirror_and_shadow.lpr).
      With such strange scale, CCW and CW invert places. Sides that were
      CCW normally are now CW. This means that you want to call @code(glFrontFace(GL_CW))
      temporarily when rendering scene in the mirror. This way scene in the mirror
      will have correct normals and backface culling.

      Since we don't touch @code(glFrontFace) anywhere, this is possible to you.
      And you can reuse resources for the scene in the mirror.
    )
  )

  @bold(Rendered TrianglesCount and VerticesCount:)

  This renderer uses the same triangles and vertices counts as
  calculated by TVRMLGeometryNode.Triangulate,
  TVRMLGeometryNode.LocalTriangulate,
  TVRMLGeometryNode.TrianglesCount,
  TVRMLGeometryNode.VerticesCount, with OverTriangulate = @true.

  Note that it doesn't mean that we actually call TVRMLGeometryNode.Triangulate
  for VRML rendering. In fact, currently we don't, and it allows us to be
  much faster (for starters, rendering by indexes, or quad strips,
  that would not be possible by generic implementation calling
  TVRMLGeometryNode.Triangulate).
  But our rendering methods generate the same triangles
  as TVRMLGeometryNode.Triangulate.

  Although for debug purposes, we have a renderer using
  TVRMLShape.LocalTriangulate, see notes about
  USE_VRML_TRIANGULATION in the source code.

  @bold(About OpenGL extensions:)

  You should always call LoadAllExtensions before using this unit.
  This unit may use various OpenGL extensions and check OpenGL version.
  If you initialize OpenGL context using our GLWindow unit or
  TKamOpenGLControl then this will be done for you automatically during
  GL context initialization.
}

unit VRMLGLRenderer;

{ When you define USE_VRML_TRIANGULATION, an alternative
  rendering method will be used. Each node will be triangulated
  using TVRMLShape.LocalTriangulate, and each generated triangle
  will be passed to OpenGL.

  This is usable only for TVRMLShape.LocalTriangulate testing.
  - It's slower than the normal rendering method,
    as triangles are passed to the OpenGL in the most naive immediate way,
    without any vertex arrays or VBOs. In fact, it will not work with
    OpenGL >= 3.
  - Things that are not expressed as triangles
    (IndexedLineSet, PointSet) will not be rendered at all.
  - It lacks some features, because the triangulating routines
    do not return enough information. For example, multi-texturing
    does not work (correctly), as TTriangleEvent currently only passes
    the coordinates for first texture unit.
}
{ $define USE_VRML_TRIANGULATION}

{$ifdef USE_VRML_TRIANGULATION}
  {$ifdef RELEASE}
    {$fatal Undefine USE_VRML_TRIANGULATION for VRMLGLRenderer,
      you don't want to use this in RELEASE version. }
  {$endif}
{$endif}

{$I kambiconf.inc}

interface

uses
  Classes, SysUtils, KambiUtils, VectorMath, GL, GLU, GLExt,
  VRMLFields, VRMLNodes, VRMLLexer, Boxes3D, OpenGLTTFonts, Images,
  KambiGLUtils, VRMLGLRendererLights, TTFontsTypes,
  VRMLErrors, GLShaders, GLImages, Videos, VRMLTime, VRMLShape,
  GLCubeMap, TextureImages, KambiClassUtils, DDS, Base3D, FGL
  {$ifdef VER2_2}, FGLObjectList22 {$endif},
  GeometryArrays, VRMLArraysGenerator, VRMLShader, VRMLShadowMaps,
  VRMLGLRendererTextureEnv;

{$define read_interface}

type
  TBeforeGLVertexProc = procedure (Node: TVRMLGeometryNode;
    const Vert: TVector3Single) of object;
  TShadersRendering = (srDisable, srWhenRequired, srAlways);
  { Faces to cull (make invisible) during VRML/X3D rendering. }
  TCullFace = (cfNone, cfCW, cfCCW);
  TBumpMapping = VRMLShader.TBumpMapping;

const
  DefaultPointSize = 3.0;
  DefaultLineWidth = 2.0;
  DefaultVarianceShadowMaps = false;
  DefaultShaders = srWhenRequired;
  DefaultBumpMapping = bmBasic;

type
  { Various properties that control rendering done
    with @link(TVRMLGLRenderer).

    They are collected here,
    in a class separate from @link(TVRMLGLRenderer),
    because various things (like TVRMLGLScene and TVRMLGLAnimation)
    wrap @link(TVRMLGLRenderer) instances and hide it,
    but still they want to allow user to change these attributes. }
  TVRMLRenderingAttributes = class(TPersistent)
  private
    FOnRadianceTransfer: TRadianceTransferFunction;
    FOnVertexColor: TVertexColorFunction;
    FLighting: boolean;
    FUseSceneLights: boolean;
    FOpacity: Single;
    FEnableTextures: boolean;
    FTextureMinFilter: TGLint;
    FTextureMagFilter: TGLint;
    FPointSize: TGLFloat;
    FLineWidth: TGLFloat;
    FBumpMapping: TBumpMapping;
    FShaders: TShadersRendering;
    FPureGeometry: boolean;
    FTextureModeGrayscale: TGLenum;
    FTextureModeRGB: TGLenum;
    FVarianceShadowMaps: boolean;
    FVertexBufferObject: boolean;
    FPreserveOpenGLState: boolean;
    FPercentageCloserFiltering: TPercentageCloserFiltering;
    FVisualizeDepthMap: boolean;
  protected
    { These methods just set the value on given property,
      eventually calling ReleaseCachedResources.
      @groupBegin }
    procedure SetOnRadianceTransfer(const Value: TRadianceTransferFunction); virtual;
    procedure SetOnVertexColor(const Value: TVertexColorFunction); virtual;
    procedure SetEnableTextures(const Value: boolean); virtual;
    procedure SetTextureMinFilter(const Value: TGLint); virtual;
    procedure SetTextureMagFilter(const Value: TGLint); virtual;
    procedure SetBumpMapping(const Value: TBumpMapping); virtual;
    procedure SetPureGeometry(const Value: boolean); virtual;
    procedure SetTextureModeGrayscale(const Value: TGLenum); virtual;
    procedure SetTextureModeRGB(const Value: TGLenum); virtual;
    procedure SetVarianceShadowMaps(const Value: boolean); virtual;
    procedure SetVertexBufferObject(const Value: boolean); virtual;
    procedure SetVisualizeDepthMap(const Value: boolean); virtual;
    procedure SetShaders(const Value: TShadersRendering); virtual;
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
    constructor Create; virtual;

    procedure Assign(Source: TPersistent); override;

    { Is the second TVRMLRenderingAttributes instance on all fields
      that affect TShapeCache, that is things that affect generated geometry
      arrays or vbo. This compares the subset of variables that call
      ReleaseCachedResources --- only the ones that affect TShapeCache. }
    function EqualForShapeCache(SecondValue: TVRMLRenderingAttributes): boolean; virtual;

    { Calculate vertex color from radiance transfer.
      If this is assigned, and geometry object has radianceTransfer
      field (see [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_radiance_transfer])
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
    property TextureMinFilter: TGLint
      read FTextureMinFilter write SetTextureMinFilter default GL_LINEAR_MIPMAP_LINEAR;
    property TextureMagFilter: TGLint
      read FTextureMagFilter write SetTextureMagFilter default GL_LINEAR;
    { @groupEnd }

    { Size of points. This has an effect on VRML/X3D PointSet rendering. }
    property PointSize: TGLFloat
      read FPointSize write FPointSize default DefaultPointSize;

    { Line width. This has an effect on VRML/X3D LineSet rendering,
      and on wireframe rendering for TVRMLSceneRenderingAttributes.WireframeEffect. }
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

      @unorderedList(
        @item(srDisable Never use shaders for anything.
          This means that "shaders", "effects" VRML/X3D fields
          are ignored, and various effects are disabled
          (like shadow maps, bump mapping, screen effects).

          In this case, the renderer doesn't even enable/disable
          any shaders, and the caller can enable some GLSL program
          for the whole model.)

        @item(srWhenRequired Enable only for shapes that require it.
          For shapes that don't strictly require shaders
          (don't have ComposedShader, don't use shadow maps,
          don't have any shader effects etc.) use fixed-function pipeline.)

        @item(srAlways Enable for all shapes, render everything by GLSL shaders.
          Everything will look beautiful (per-pixel lighting for all shapes),
          but rendering may be slower.)
      )

      Note that PureGeometry = @true also disables all shaders.
      That is, when PureGeometry = @true, the value of this property
      doesn't matter, it's always treated like srDisable. }
    property Shaders: TShadersRendering read FShaders write FShaders
      default DefaultShaders;

    { Use this to render pure geometry, without any colors, materials,
      lights, textures. If this is @true, only the geometry primitives
      are rendered. We still set correct modelview matrix transformations,
      control face culling and depth test and such.
      The idea is that we "hit" the same pixels as normal rendering
      (with the exception of alpha test textures, that are not used for
      pure geometry rendering --- for now).
      But we do absolutely nothing to set a particular pixel color.
      Which means that the caller controls the color (by default,
      if lighting and everything else is disabled, you just get solid look
      with color from last glColor).

      For example, Renderer will not set any color (no glColor calls),
      will not set any material
      (no glMaterial calls), will not set any texture coordinates and
      will not bind any texture, fog and such.

      This is useful for special tricks, in particular to draw the geometry
      into stencil buffer.
      Another example of use is to render plane-projected shadows,
      see kambi_vrml_game_engine/examples/vrml/plane_projected_shadow_demo.lpr,
      where you have to draw the model with pure black color. }
    property PureGeometry: boolean
      read FPureGeometry write SetPureGeometry default false;

    { Default texture mode for single texturing.
      For X3D MultiTexture nodes, they are always explicitly given
      by MultiTexture.mode field. This field applies only to non-multi textures.

      VRML 2 / X3D specifications say that the mode for TextureModeRGB
      should be GL_REPLACE to be conforming (see "Lighting model"
      spec). So our default value here contradicts the spec --- but,
      practically, it's much more useful, so I decided that, as an exception,
      I can contradict the spec in this case.

      Note that this should specify the mode only on non-alpha channels.
      On the alpha channel, specification says clearly that texture alpha
      should replace (never modulate) alpha channel, if only texture
      has any alpha channel.

      TODO: this is not honored, for now setting below controls mode
      on both alpha and non-alpha channels.

      @groupBegin }
    property TextureModeGrayscale: TGLenum
      read FTextureModeGrayscale write SetTextureModeGrayscale default GL_MODULATE;
    property TextureModeRGB: TGLenum
      read FTextureModeRGB write SetTextureModeRGB default GL_MODULATE;
    { @groupEnd }

    { Try to use variance shadow maps.
      See http://www.punkuser.net/vsm/ . This may generally produce superior
      results, as shadow maps can be then filtered like normal textures
      (bilinear, mipmaps, anisotropic filtering). So shadows look much nicer
      from very close and very far distances.

      TODO: It requires, for now, that your scene doesn't use any
      GLSL shaders (otherwise they will disable the default VSM shader
      that generates proper depths). }
    property VarianceShadowMaps: boolean read FVarianceShadowMaps
      write SetVarianceShadowMaps default DefaultVarianceShadowMaps;

    { Use OpenGL vertex buffer object.
      This is always a good idea. You can set this to @false
      for debug purposes, e.g. to check how much speedup you get from VBO. }
    property VertexBufferObject: boolean
      read FVertexBufferObject write SetVertexBufferObject default true;

    { Do we have to restore OpenGL state at the end of rendering.
      If @true, then RenderEnd restores the exact OpenGL state that was
      before RenderBegin. This is comfortable if you do any other rendering
      besides VRML scene, and depend on some state preserved.
      Unfortunately this is also quite expensive, and can slow down
      the rendering. }
    property PreserveOpenGLState: boolean
      read FPreserveOpenGLState write FPreserveOpenGLState default false;

    { Use Percentage Closer Filtering to improve shadow maps look.
      This decides the sampling method of all depth textures. }
    property PercentageCloserFiltering: TPercentageCloserFiltering
      read FPercentageCloserFiltering write FPercentageCloserFiltering
      default DefaultPercentageCloserFiltering;

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
  end;

  TVRMLRenderingAttributesClass = class of TVRMLRenderingAttributes;

  TGLOutlineFontCache = record
    References: Cardinal;
    Instance: TGLOutlineFont;
  end;

  TTextureImageCache = record
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: string;

    { The initial VRML/X3D node that created this cache record.
      This is only the first node, that initiated this
      TTextureImageCache item. Note that many TVRML2DTextureNode nodes
      may correspond to a single TTextureImageCache (since TTextureImageCache
      only tries to share GLName between them). So this may help during
      _IncReference, but nothing more --- it's *not* an exhaustive list
      of texture nodes related to this video texture!

      It may be currently TVRML2DTextureNode, or TNodeRenderedTexture. }
    InitialNode: TNodeX3DTextureNode;

    MinFilter: TGLint;
    MagFilter: TGLint;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    References: Cardinal;
    GLName: TGLuint;

    { The saved result of TImage.AlphaChannelType.

      Detecting AlphaChannelType is a little time-consuming
      (iteration over all pixels is needed),
      so it's done only once and kept in the cache, just like GLName. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureImageCache = ^TTextureImageCache;

  TDynArrayItem_2 = TTextureImageCache;
  PDynArrayItem_2 = PTextureImageCache;
  {$define DYNARRAY_2_IS_STRUCT}
  {$define DYNARRAY_2_IS_INIT_FINI_TYPE}
  {$I dynarray_2.inc}
  TDynTextureImageCacheArray = class(TDynArray_2)
  end;

  TTextureVideoCache = record
    FullUrl: string;

    { The initial VRML/X3D node that created this cache record.
      This is only the first TNodeMovieTexture node, that initiated this
      TTextureVideoCache item. Note that many TNodeMovieTexture nodes
      may correspond to a single TTextureVideoCache (since TTextureVideoCache
      only tries to share TGLVideo between them, they don't have to share
      other fields like current time etc.). So this may help during
      _IncReference, but nothing more --- it's *not* an exhaustive list
      of MovieTexture nodes related to this video texture! }
    InitialNode: TNodeMovieTexture;

    MinFilter: TGLint;
    MagFilter: TGLint;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    References: Cardinal;
    GLVideo: TGLVideo;

    { The saved result of TVideo.AlphaChannelType.

      Detecting AlphaChannelType is a little time-consuming
      (iteration over all pixels is needed),
      so it's done only once and kept in the cache. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureVideoCache = ^TTextureVideoCache;

  TDynArrayItem_7 = TTextureVideoCache;
  PDynArrayItem_7 = PTextureVideoCache;
  {$define DYNARRAY_7_IS_STRUCT}
  {$define DYNARRAY_7_IS_INIT_FINI_TYPE}
  {$I dynarray_7.inc}
  TDynTextureVideoCacheArray = class(TDynArray_7)
  end;

  TTextureCubeMapCache = record
    InitialNode: TNodeX3DEnvironmentTextureNode;
    MinFilter: TGLint;
    MagFilter: TGLint;
    Anisotropy: TGLfloat;
    References: Cardinal;
    GLName: TGLuint;

    { The saved result of TImage.AlphaChannelType.

      Detecting AlphaChannelType is a little time-consuming
      (iteration over all pixels is needed),
      so it's done only once and kept in the cache, just like GLName. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTextureCubeMapCache = ^TTextureCubeMapCache;

  TDynArrayItem_9 = TTextureCubeMapCache;
  PDynArrayItem_9 = PTextureCubeMapCache;
  {$define DYNARRAY_9_IS_STRUCT}
  {$define DYNARRAY_9_IS_INIT_FINI_TYPE}
  {$I dynarray_9.inc}
  TDynTextureCubeMapCacheArray = class(TDynArray_9)
  end;

  TTexture3DCache = record
    InitialNode: TNodeX3DTexture3DNode;
    MinFilter: TGLint;
    MagFilter: TGLint;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap3D;
    References: Cardinal;
    GLName: TGLuint;

    { The saved result of TImage.AlphaChannelType.

      Detecting AlphaChannelType is a little time-consuming
      (iteration over all pixels is needed),
      so it's done only once and kept in the cache, just like GLName. }
    AlphaChannelType: TAlphaChannelType;
  end;
  PTexture3DCache = ^TTexture3DCache;

  TDynArrayItem_11 = TTexture3DCache;
  PDynArrayItem_11 = PTexture3DCache;
  {$define DYNARRAY_11_IS_STRUCT}
  {$define DYNARRAY_11_IS_INIT_FINI_TYPE}
  {$I dynarray_11.inc}
  TDynTexture3DCacheArray = TDynArray_11;

  { Cached depth or float texture.
    For now, depth and float textures require the same fields.
    TODO: change this into an old-style "object" hierarchy. }
  TTextureDepthOrFloatCache = record
    { The initial VRML/X3D node that created this cache record.
      For now, this may be TNodeGeneratedShadowMap or TNodeRenderedTexture. }
    InitialNode: TNodeX3DTextureNode;
    Wrap: TTextureWrap2D;
    References: Cardinal;
    GLName: TGLuint;
  end;
  PTextureDepthOrFloatCache = ^TTextureDepthOrFloatCache;

  TDynArrayItem_13 = TTextureDepthOrFloatCache;
  PDynArrayItem_13 = PTextureDepthOrFloatCache;
  {$define DYNARRAY_13_IS_STRUCT}
  {$define DYNARRAY_13_IS_INIT_FINI_TYPE}
  {$I dynarray_13.inc}
  TDynTextureDepthOrFloatCacheArray = TDynArray_13;

  TVRMLRendererShape = class;
  TVboType = (vtCoordinate, vtAttribute, vtIndex);
  TVboTypes = set of TVboType;
  TVboArrays = array [TVboType] of TGLuint;

  { Cached shape resources. }
  TShapeCache = class
  private
    Attributes: TVRMLRenderingAttributes;
    Geometry: TVRMLGeometryNode;
    State: TVRMLGraphTraverseState;
    Fog: INodeX3DFogObject;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3Single;
    FogVolumetricVisibilityStart: Single;
    References: Cardinal;

    { An instance of TGeometryArrays, decomposing this shape geometry.
      Used to easily render and process this geometry, if assigned.
      This is managed by TVRMLGLRenderer and TVRMLGLScene. }
    Arrays: TGeometryArrays;

    { What Vbos do we need to reload.
      Next time (right after creating arrays) we load vbo contents,
      we'll look at this to know which parts to actually reload to vbo.
      This is extended at each FreeArrays call. }
    VboToReload: TVboTypes;

    Vbo: TVboArrays;
    VboAllocatedUsage: TGLenum;
    VboAllocatedSize: array [TVboType] of Cardinal;

    { Like TVRMLRendererShape.LoadArraysToVbo,
      but takes explicit DynamicGeometry. }
    procedure LoadArraysToVbo(DynamicGeometry: boolean);
    procedure FreeVBO;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeArrays(const Changed: TVboTypes);
  end;

  TShapeCacheList = specialize TFPGObjectList<TShapeCache>;

  TVRMLGLSLProgram = class;

  TShaderProgramCache = class
  public
    { Hash of TVRMLShader code when initializing this shader
      by LinkProgram. Used to decide when shader needs to be regenerated,
      and when it can be shared. }
    Hash: TShaderCodeHash;

    { Actual GLSL program. May be @nil (if it failed to link). }
    ShaderProgram: TVRMLGLSLProgram;

    References: Cardinal;

    destructor Destroy; override;
  end;

  TShaderProgramCacheList = specialize TFPGObjectList<TShaderProgramCache>;

  TVRMLGLRenderer = class;

  { A cache that may be used by many TVRMLGLRenderer
    instances to share some common OpenGL resources.

    For examples, texture names. Such things can usually be shared by all
    TVRMLGLRenderer instances used within the same OpenGL context.
    And this may save a lot of memory if you use many TVRMLGLRenderer
    instances in your program.

    Instance of this class is tied to particular OpenGL context if and only if
    there are some TVRMLGLRenderer instances using this cache and
    tied to that OpenGL context. }
  TVRMLGLRendererContextCache = class
  private
    Fonts: array[TVRMLFontFamily, boolean, boolean] of TGLOutlineFontCache;
    TextureImageCaches: TDynTextureImageCacheArray;
    TextureVideoCaches: TDynTextureVideoCacheArray;
    TextureCubeMapCaches: TDynTextureCubeMapCacheArray;
    Texture3DCaches: TDynTexture3DCacheArray;
    TextureDepthOrFloatCaches: TDynTextureDepthOrFloatCacheArray;
    ShapeCaches: TShapeCacheList;
    ProgramCaches: TShaderProgramCacheList;

    { Load given texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function TextureImage_IncReference(
      const TextureImage: TEncodedImage;
      const TextureFullUrl: string;
      const TextureNode: TNodeX3DTextureNode;
      const TextureMinFilter, TextureMagFilter: TGLint;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      const DDSForMipmaps: TDDSImage;
      out AlphaChannelType: TAlphaChannelType): TGLuint;

    procedure TextureImage_DecReference(
      const TextureGLName: TGLuint);

    function TextureVideo_IncReference(
      const TextureVideo: TVideo;
      const TextureFullUrl: string;
      const TextureNode: TNodeMovieTexture;
      const TextureMinFilter, TextureMagFilter: TGLint;
      const TextureAnisotropy: TGLfloat;
      const TextureWrap: TTextureWrap2D;
      out AlphaChannelType: TAlphaChannelType): TGLVideo;

    procedure TextureVideo_DecReference(
      const TextureVideo: TGLVideo);

    { Load given cube texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function TextureCubeMap_IncReference(
      Node: TNodeX3DEnvironmentTextureNode;
      const MinFilter, MagFilter: TGLint;
      const Anisotropy: TGLfloat;
      PositiveX, NegativeX,
      PositiveY, NegativeY,
      PositiveZ, NegativeZ: TEncodedImage;
      DDSForMipmaps: TDDSImage;
      out AlphaChannelType: TAlphaChannelType): TGLuint;

    procedure TextureCubeMap_DecReference(
      const TextureGLName: TGLuint);

    { Required ARB_depth_texture before calling this.

      For interpreating DepthCompareField, ARB_shadow will be needed
      (but we'll make nice warning if it's not available).
      DepthCompareField may be @nil, then it's equivalent to "NONE". }
    function TextureDepth_IncReference(
      Node: TNodeX3DTextureNode;
      const TextureWrap: TTextureWrap2D;
      DepthCompareField: TSFString;
      const Width, Height: Cardinal;
      const VisualizeDepthMap: boolean): TGLuint;

    procedure TextureDepth_DecReference(
      const TextureGLName: TGLuint);

    { Increase / decrease reference to a float texture.
      Required ARB_texture_float or ATI_texture_float before calling this.
      Precision32 = @true requires 32-bit full Single floats,
      Precision32 = @false requires 16-bit (half) floats. }
    function TextureFloat_IncReference(Node: TNodeX3DTextureNode;
      const TextureMinFilter, TextureMagFilter: TGLint;
      const TextureWrap: TTextureWrap2D;
      const Width, Height: Cardinal;
      const Precision32: boolean): TGLuint;
    procedure TextureFloat_DecReference(
      const TextureGLName: TGLuint);

    { Load given 3D texture to OpenGL, using our cache.

      @raises(ETextureLoadError If texture cannot be loaded for whatever
      reason.) }
    function Texture3D_IncReference(
      Node: TNodeX3DTexture3DNode;
      const MinFilter, MagFilter: TGLint;
      const Anisotropy: TGLfloat;
      const TextureWrap: TTextureWrap3D;
      Image: TEncodedImage; DDS: TDDSImage;
      out AlphaChannelType: TAlphaChannelType): TGLuint;

    procedure Texture3D_DecReference(
      const TextureGLName: TGLuint);
  public
    constructor Create;
    destructor Destroy; override;

    function Fonts_IncReference(
      fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean;
      TTF_Font: PTrueTypeFont): TGLOutlineFont;

    procedure Fonts_DecReference(
      fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean);

    { Shape cache. We return TShapeCache, either taking an existing
      instance from cache or creating and adding a new one.
      Caller is responsible for checking are Arrays / Vbo zero and
      eventually initializing and setting. }
    function Shape_IncReference(Shape: TVRMLRendererShape;
      Fog: INodeX3DFogObject; ARenderer: TVRMLGLRenderer): TShapeCache;

    procedure Shape_DecReference(var ShapeCache: TShapeCache);

    { Shader program cache. We return TShaderProgramCache,
      either taking an existing instance from cache or creating and adding
      a new one. If we create a new one, we will use Shader to initialize
      program hash and to create and link actual TVRMLGLSLProgram instance. }
    function Program_IncReference(ARenderer: TVRMLGLRenderer;
      Shader: TVRMLShader; const ShapeNiceName: string): TShaderProgramCache;

    procedure Program_DecReference(var ProgramCache: TShaderProgramCache);
  end;

  {$I vrmlglrenderer_resource.inc}
  {$I vrmlglrenderer_texture.inc}
  {$I vrmlglrenderer_bumpmapping.inc}
  {$I vrmlglrenderer_glsl.inc}

  { VRML shape that can be rendered. }
  TVRMLRendererShape = class(TVRMLShape)
  private
    { Generate VBO if needed, and reload VBO contents.
      Assumes (GL_ARB_vertex_buffer_object and not BuggyVBO) is true.

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
  end;

  { Line types (patterns). For ease of implementation, ordered exactly like
    VRML/X3D LineProperties.linetype field. }
  TLineType = (ltSolid,
    ltDashed,
    ltDotted,
    ltDashedDotted,
    ltDashDotDot);

  TVRMLGLRenderer = class
  private
    { ---------------------------------------------------------
      GLContext-specific things, so freed (or reset in some other way to default
      uninitialized values) in UnprepareAll. }

    GLTextureNodes: TGLTextureNodes;
    BumpMappingRenderers: TBumpMappingRenderersList;
    ScreenEffectPrograms: TGLSLProgramsList;

    { To which fonts we made a reference in the cache ? }
    FontsReferences: array [TVRMLFontFamily, boolean, boolean] of boolean;

    { ------------------------------------------------------------------------ }

    { For speed, we keep a single instance of TVRMLShader,
      instead of creating / destroying an instance at each RenderShape.
      This is necessary, otherwise the constructor / destructor of TVRMLShader
      would be bottle-necks. }
    PreparedShader: TVRMLShader;

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

    { How many texture units are used.

      It's always clamped by the number of available texture units
      (GLMaxTextureUnits).
      Always <= 1 if OpenGL doesn't support multitexturing.
      TODO: when ARB_multitexture, but not GLUseMultiTexturing,
      this will be > 1. Is everything ready for this? }
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
      Always <= 1 if not GLUseMultiTexturing. }
    TextureTransformUnitsUsed: Cardinal;

    { Additional texture units used,
      in addition to 0..TextureTransformUnitsUsed - 1.
      Cleared by RenderShapeBegin, added by PushTextureUnit,
      used by RenderShapeEnd. }
    TextureTransformUnitsUsedMore: TDynLongIntArray;

    FCullFace: TCullFace;
    FSmoothShading: boolean;
    FFixedFunctionLighting: boolean;
    FLineWidth: Single;
    FLineType: TLineType;

    { This calls glPushMatrix, assuming that current matrix mode is GL_TEXTURE
      and current tex unit is TexUnit (always make sure this is true when
      calling it!).

      It also records this fact, so that RenderShapeEnd will be able to
      make pop texture matrix later.

      In fact this optimizes push/pops on texture matrix stack, such that
      VRML TextureTransform nodes and such together with PushTextureUnit
      will only use only matrix stack place, even if texture will be
      "pushed" multiple times (both by PushTextureUnit and normal
      VRML TextureTransform realized in RenderShapeBegin.) }
    procedure PushTextureUnit(const TexUnit: Cardinal);

    { Check Attributes (like Attributes.BumpMapping) and OpenGL
      context capabilities to see if bump mapping can be used. }
    function BumpMapping: TBumpMapping;

    procedure SetCullFace(const Value: TCullFace);
    procedure SetSmoothShading(const Value: boolean);
    procedure SetFixedFunctionLighting(const Value: boolean);
    procedure SetLineWidth(const Value: Single);
    procedure SetLineType(const Value: TLineType);

    { Change glCullFace and GL_CULL_FACE enabled by this property.
      This way we avoid redundant state changes. }
    property CullFace: TCullFace read FCullFace write SetCullFace;
    { Change glShadeModel by this property. }
    property SmoothShading: boolean read FSmoothShading write SetSmoothShading;
    { Change GL_LIGHTING enabled by this property. }
    property FixedFunctionLighting: boolean read FFixedFunctionLighting write SetFixedFunctionLighting;
    property LineWidth: Single read FLineWidth write SetLineWidth;
    property LineType: TLineType read FLineType write SetLineType;
  private
    { ----------------------------------------------------------------- }

    { Available between RenderBegin / RenderEnd. }
    LightsRenderer: TVRMLGLLightsRenderer;

    { Currently set fog parameters, during render. }
    FogNode: INodeX3DFogObject;
    FogEnabled: boolean;
    FogType: TFogType;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3Single;
    FogVolumetricVisibilityStart: Single;

    FAttributes: TVRMLRenderingAttributes;

    FCache: TVRMLGLRendererContextCache;

    { Lights shining on all shapes. Set in each RenderBegin. }
    BaseLights: TLightInstancesList;

    { Rendering pass. Set in each RenderBegin. }
    Pass: TRenderingPass;

    { Get VRML/X3D fog parameters, based on fog node and Attributes. }
    procedure GetFog(Node: INodeX3DFogObject;
      out Enabled, Volumetric: boolean;
      out VolumetricDirection: TVector3Single;
      out VolumetricVisibilityStart: Single);

    {$ifdef USE_VRML_TRIANGULATION}
    procedure DrawTriangle(Shape: TObject;
      const Position: TTriangle3Single;
      const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
      const Face: TFaceIndex);
    {$endif}

    { If ARB_multitexturing available, this sets currently active texture unit.
      TextureUnit is newly active unit, this is added to GL_TEXTURE0_ARB.

      So the only thing that you have to care about is to specify TextureUnit <
      FreeGLTexturesCount.
      Everything else (ARB_multitexturing, GL_TEXTURE0_ARB)
      is taken care of inside here. }
    procedure ActiveTexture(const TextureUnit: Cardinal);

    { Disable any (fixed-function) texturing (2D, 3D, cube map, and so on)
      on given texture unit. }
    procedure DisableTexture(const TextureUnit: Cardinal);
    procedure DisableCurrentTexture;

    procedure RenderShapeLineProperties(Shape: TVRMLRendererShape;
      Fog: INodeX3DFogObject; Shader: TVRMLShader);
    procedure RenderShapeMaterials(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader);
    procedure RenderShapeLights(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeFog(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeTextureTransform(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeClipPlanes(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeCreateMeshRenderer(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader;
      const MaterialOpacity: Single; const Lighting: boolean);
    procedure RenderShapeShaders(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader;
      const MaterialOpacity: Single; const Lighting: boolean;
      GeneratorClass: TVRMLArraysGeneratorClass;
      ExposedMeshRenderer: TObject);
    procedure RenderShapeTextures(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader;
      const MaterialOpacity: Single; const Lighting: boolean;
      GeneratorClass: TVRMLArraysGeneratorClass;
      ExposedMeshRenderer: TObject;
      UsedGLSLTexCoordsNeeded: Cardinal);
    procedure RenderShapeInside(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
      Shader: TVRMLShader;
      const MaterialOpacity: Single; const Lighting: boolean;
      GeneratorClass: TVRMLArraysGeneratorClass;
      ExposedMeshRenderer: TObject);

    { Reset various OpenGL state parameters, done at RenderBegin
      (to prepare state for following RenderShape calls) and at RenderEnd
      (to leave *somewhat* defined state afterwards). }
    procedure RenderCleanState(const Beginning: boolean);

    procedure PrepareIDecls(Nodes: TMFNode; State: TVRMLGraphTraverseState);
    procedure PrepareIDecls(Nodes: TVRMLNodesList; State: TVRMLGraphTraverseState);
  public
    { If > 0, RenderShape will not actually render, only prepare
      per-shape resources for fast rendering (arrays and vbos). }
    PrepareRenderShape: Cardinal;

    { Constructor. Always pass a cache instance --- preferably,
      something created and used by many scenes. }
    constructor Create(AttributesClass: TVRMLRenderingAttributesClass;
      ACache: TVRMLGLRendererContextCache);

    destructor Destroy; override;

    { Rendering attributes. You can change them only when renderer
      is not tied to the current OpenGL context, so only after construction
      or after UnprepareAll call (before any Prepare or Render* calls). }
    property Attributes: TVRMLRenderingAttributes read FAttributes;

    property Cache: TVRMLGLRendererContextCache read FCache;

    { Prepare given State, to be able to render shapes with it.
      Between preparing and unpreparing, nodes passed here are "frozen":
      do not change, do not free them. }
    procedure Prepare(State: TVRMLGraphTraverseState);

    { Release resources for this texture. }
    procedure UnprepareTexture(Node: TNodeX3DTextureNode);

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
      LightRenderEvent: TVRMLLightRenderEvent; const APass: TRenderingPass);
    procedure RenderEnd;

    procedure RenderShape(Shape: TVRMLRendererShape; Fog: INodeX3DFogObject);

    { Get calculated TImage.AlphaChannelType for a prepared texture.

      Returns @false if texture is not prepared. If you want to make
      sure the texture is prepared make sure that
      @unorderedList(
        @item(State with this texture was passed to @link(Prepare) method.
          That is, @link(Prepare) was called with this texture
          as State.Texture.

          If the State.Texture was multi-texture,
          then also all it's items (on the list of
          State.Texture.FdTexture) are prepared.
          The main multi-texture node is also considered prepared then,
          it's AlphaChannelType was calculated looking at AlphaChannelType
          of children.
        )
        @item(Attributes.PureGeometry = @false,)
        @item(and node must have some texture data
          (for TVRML2DTextureNode, check TextureNode.IsTextureImage or
          TextureNode.IsTextureVideo))
      ) }
    function PreparedTextureAlphaChannelType(
      TextureNode: TNodeX3DTextureNode;
      out AlphaChannelType: TAlphaChannelType): boolean;

    { Update generated texture for this shape.

      NeedsRestoreViewport will be set to @true if viewport was
      (possibly) changed by this procedure (otherwise, NeedsRestoreViewport
      will not be modified). }
    procedure UpdateGeneratedTextures(Shape: TVRMLShape;
      TextureNode: TNodeX3DTextureNode;
      const Render: TRenderFromViewFunction;
      const ProjectionNear, ProjectionFar: Single;
      var NeedsRestoreViewport: boolean;
      CurrentViewpoint: TVRMLViewpointNode;
      CameraViewKnown: boolean;
      const CameraPosition, CameraDirection, CameraUp: TVector3Single);

    { Load GLSL shader for the ScreenEffect node.
      Makes sure that Node.ShaderLoaded is true.
      When changing Node.ShaderLoaded false to true tries to initialize
      the shader, setting Node.Shader if some GLSL program
      was successfully loaded.

      The GLSL program (TGLSLProgram) will be stored here,
      and will be automatically freed during UnprepareAll call. }
    procedure PrepareScreenEffect(Node: TNodeScreenEffect);
  end;

  EVRMLGLRendererror = class(EVRMLError);

const
  AllVboTypes = [Low(TVboType) .. High(TVboType)];

  BumpMappingNames: array [TBumpMapping] of string =
  ( 'None',
    'Basic',
    'Parallax',
    'Steep Parallax',
    'Steep Parallax With Self-Shadowing' );

var
  { Should we log every event of a cache TVRMLGLRendererContextCache.
    You have to InitializeLog first (see KambiLog) to make this actually
    meaningful. This allows to see how the cache performs. It also causes
    a @italic(lot) of log messages, usually too many even for debugging. }
  LogRendererCache: boolean = false;

{$undef read_interface}

implementation

uses Math, KambiStringUtils, GLVersionUnit, KambiLog,
  RenderingCameraUnit, VRMLCameraUtils, RaysWindow;

{$define read_implementation}
{$I dynarray_2.inc}
{$I dynarray_7.inc}
{$I dynarray_9.inc}
{$I dynarray_11.inc}
{$I dynarray_13.inc}

{$I vrmlmeshrenderer.inc}
{$I vrmlmeshrenderer_x3d_text.inc}

{$I vrmlglrenderer_resource.inc}
{$I vrmlglrenderer_texture.inc}
{$I vrmlglrenderer_bumpmapping.inc}
{$I vrmlglrenderer_glsl.inc}

{ TVRMLGLRendererContextCache -------------------------------------------- }

constructor TVRMLGLRendererContextCache.Create;
begin
  inherited;
  TextureImageCaches := TDynTextureImageCacheArray.Create;
  TextureVideoCaches := TDynTextureVideoCacheArray.Create;
  TextureCubeMapCaches := TDynTextureCubeMapCacheArray.Create;
  Texture3DCaches := TDynTexture3DCacheArray.Create;
  TextureDepthOrFloatCaches := TDynTextureDepthOrFloatCacheArray.Create;
  ShapeCaches := TShapeCacheList.Create;
  ProgramCaches := TShaderProgramCacheList.Create;
end;

destructor TVRMLGLRendererContextCache.Destroy;

{ $define ONLY_WARN_ON_CACHE_LEAK}

{$ifdef ONLY_WARN_ON_CACHE_LEAK}
  procedure Assert(const B: boolean; const S: string = '');
  begin
    if not B then
      VRMLWarning(vwIgnorable, 'GLRendererContextCache warning: ' + S);
  end;
{$endif}

var
  fsfam: TVRMLFontFamily;
  fsbold , fsitalic: boolean;
begin
  for fsfam := Low(fsfam) to High(fsfam) do
    for fsbold := Low(boolean) to High(boolean) do
      for fsitalic := Low(boolean) to High(boolean) do
      begin
        Assert(
          (Fonts[fsfam, fsbold, fsitalic].Instance = nil) =
          (Fonts[fsfam, fsbold, fsitalic].References = 0));
        Assert(Fonts[fsfam, fsbold, fsitalic].Instance = nil,
          'Some references to fonts still exist' +
          ' when freeing TVRMLGLRendererContextCache');
      end;

  if TextureImageCaches <> nil then
  begin
    Assert(TextureImageCaches.Count = 0, 'Some references to texture images still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(TextureImageCaches);
  end;

  if TextureVideoCaches <> nil then
  begin
    Assert(TextureVideoCaches.Count = 0, 'Some references to texture videos still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(TextureVideoCaches);
  end;

  if TextureCubeMapCaches <> nil then
  begin
    Assert(TextureCubeMapCaches.Count = 0, 'Some references to texture cubemaps still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(TextureCubeMapCaches);
  end;

  if Texture3DCaches <> nil then
  begin
    Assert(Texture3DCaches.Count = 0, 'Some references to texture 3D still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(Texture3DCaches);
  end;

  if TextureDepthOrFloatCaches <> nil then
  begin
    Assert(TextureDepthOrFloatCaches.Count = 0, 'Some references to depth or float texture still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(TextureDepthOrFloatCaches);
  end;

  if ShapeCaches <> nil then
  begin
    Assert(ShapeCaches.Count = 0, 'Some references to Shapes still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(ShapeCaches);
  end;

  if ProgramCaches <> nil then
  begin
    Assert(ProgramCaches.Count = 0, 'Some references to GLSL programs still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(ProgramCaches);
  end;

  inherited;
end;

function TVRMLGLRendererContextCache.Fonts_IncReference(
  fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean;
  TTF_Font: PTrueTypeFont): TGLOutlineFont;
begin
  Inc(Fonts[fsfam, fsbold, fsitalic].References);
  if Fonts[fsfam, fsbold, fsitalic].Instance = nil then
    Fonts[fsfam, fsbold, fsitalic].Instance := TGLOutlineFont.Create(TTF_Font);
  Result := Fonts[fsfam, fsbold, fsitalic].Instance;
  if LogRendererCache and Log then
    WritelnLog('++', 'Font: %d', [Fonts[fsfam, fsbold, fsitalic].References]);
end;

procedure TVRMLGLRendererContextCache.Fonts_DecReference(
  fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean);
begin
  Dec(Fonts[fsfam, fsbold, fsitalic].References);
  if Fonts[fsfam, fsbold, fsitalic].References = 0 then
    FreeAndNil(Fonts[fsfam, fsbold, fsitalic].Instance);
  if LogRendererCache and Log then
    WritelnLog('--', 'Font: %d', [Fonts[fsfam, fsbold, fsitalic].References]);
end;

const
  { Parameters for AlphaChannelType to detect textures alpha channel }
  AlphaTolerance = 5;
  AlphaWrongPixelsTolerance = 0.01;

function TVRMLGLRendererContextCache.TextureImage_IncReference(
  const TextureImage: TEncodedImage;
  const TextureFullUrl: string;
  const TextureNode: TNodeX3DTextureNode;
  const TextureMinFilter, TextureMagFilter: TGLint;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  const DDSForMipmaps: TDDSImage;
  out AlphaChannelType: TAlphaChannelType): TGLuint;
var
  I: Integer;
  TextureCached: PTextureImageCache;
begin
  for I := 0 to TextureImageCaches.High do
  begin
    TextureCached := TextureImageCaches.Pointers[I];

    { Once I had an idea to make here comparison with
      TextureImage = TextureCached^.Image. Since we have ImagesCache,
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
      and later load the same texture, with to different TImage instance.

      For now, I don't use this idea, and rely on TextureFullUrl. }

    if ( ( (TextureFullUrl <> '') and
           (TextureCached^.FullUrl = TextureFullUrl) ) or
         (TextureCached^.InitialNode = TextureNode) ) and
       (TextureCached^.MinFilter = TextureMinFilter) and
       (TextureCached^.MagFilter = TextureMagFilter) and
       (TextureCached^.Anisotropy = TextureAnisotropy) and
       (TextureCached^.Wrap = TextureWrap) then
    begin
      Inc(TextureCached^.References);
      if LogRendererCache and Log then
        WritelnLog('++', '%s: %d', [TextureFullUrl, TextureCached^.References]);
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLName);
    end;
  end;

  { Initialize Result first, before calling TextureImageCaches.Add.
    That's because in case LoadGLTexture raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureImage_DecReference later). }
  Result := LoadGLTexture(
    TextureImage, TextureMinFilter, TextureMagFilter,
    TextureWrap, false, DDSForMipmaps);

  TexParameterMaxAnisotropy(GL_TEXTURE_2D, TextureAnisotropy);

  TextureCached := TextureImageCaches.Add;
  TextureCached^.FullUrl := TextureFullUrl;
  TextureCached^.InitialNode := TextureNode;
  TextureCached^.MinFilter := TextureMinFilter;
  TextureCached^.MagFilter := TextureMagFilter;
  TextureCached^.Anisotropy := TextureAnisotropy;
  TextureCached^.Wrap := TextureWrap;
  TextureCached^.References := 1;
  TextureCached^.GLName := Result;

  { calculate and save AlphaChannelType in the cache }
  if TextureNode is TVRML2DTextureNode then
  begin
    TextureCached^.AlphaChannelType := TextureImage.AlphaChannelTypeOverride(
      TVRML2DTextureNode(TextureNode).DetectAlphaChannel,
      AlphaTolerance, AlphaWrongPixelsTolerance);
    if Log and (TextureCached^.AlphaChannelType <> atNone)  then
      WritelnLog('Alpha Detection', 'Alpha texture ' + TextureFullUrl +
        ' detected as simple yes/no alpha channel: ' +
        BoolToStr[TextureCached^.AlphaChannelType = atSimpleYesNo]);
  end else
  begin
    TextureCached^.AlphaChannelType := atNone; { TODO }
  end;

  AlphaChannelType := TextureCached^.AlphaChannelType;

  if LogRendererCache and Log then
    WritelnLog('++', '%s: %d', [TextureFullUrl, 1]);
end;

procedure TVRMLGLRendererContextCache.TextureImage_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureImageCaches.High do
    if TextureImageCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(TextureImageCaches.Items[I].References);
      if LogRendererCache and Log then
        WritelnLog('--', '%s: %d', [TextureImageCaches.Items[I].FullUrl,
                                    TextureImageCaches.Items[I].References]);
      if TextureImageCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(TextureImageCaches.Items[I].GLName));
        TextureImageCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.TextureImage_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLGLRendererContextCache.TextureVideo_IncReference(
  const TextureVideo: TVideo;
  const TextureFullUrl: string;
  const TextureNode: TNodeMovieTexture;
  const TextureMinFilter, TextureMagFilter: TGLint;
  const TextureAnisotropy: TGLfloat;
  const TextureWrap: TTextureWrap2D;
  out AlphaChannelType: TAlphaChannelType): TGLVideo;
var
  I: Integer;
  TextureCached: PTextureVideoCache;
begin
  for I := 0 to TextureVideoCaches.High do
  begin
    TextureCached := TextureVideoCaches.Pointers[I];

    if ( ( (TextureFullUrl <> '') and
           (TextureCached^.FullUrl = TextureFullUrl) ) or
         (TextureCached^.InitialNode = TextureNode) ) and
       (TextureCached^.MinFilter = TextureMinFilter) and
       (TextureCached^.MagFilter = TextureMagFilter) and
       (TextureCached^.Anisotropy = TextureAnisotropy) and
       (TextureCached^.Wrap = TextureWrap) then
    begin
      Inc(TextureCached^.References);
      if LogRendererCache and Log then
        WritelnLog('++', '%s: %d', [TextureFullUrl, TextureCached^.References]);
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLVideo);
    end;
  end;

  { Initialize Result first, before calling TextureVideoCaches.Add.
    That's because in case TGLVideo.Create raises exception,
    we don't want to add texture to cache (because caller would have
    no way to call TextureVideo_DecReference later). }
  Result := TGLVideo.Create(
    TextureVideo, TextureMinFilter, TextureMagFilter, TextureAnisotropy,
    TextureWrap);

  TextureCached := TextureVideoCaches.Add;
  TextureCached^.FullUrl := TextureFullUrl;
  TextureCached^.InitialNode := TextureNode;
  TextureCached^.MinFilter := TextureMinFilter;
  TextureCached^.MagFilter := TextureMagFilter;
  TextureCached^.Anisotropy := TextureAnisotropy;
  TextureCached^.Wrap := TextureWrap;
  TextureCached^.References := 1;
  TextureCached^.GLVideo := Result;

  { calculate and save AlphaChannelType in the cache }
  TextureCached^.AlphaChannelType := TextureVideo.AlphaChannelTypeOverride(
    TextureNode.DetectAlphaChannel,
    AlphaTolerance, AlphaWrongPixelsTolerance);
  if Log and (TextureCached^.AlphaChannelType <> atNone)  then
    WritelnLog('Alpha Detection', 'Alpha texture ' + TextureFullUrl +
      ' detected as simple yes/no alpha channel: ' +
      BoolToStr[TextureCached^.AlphaChannelType = atSimpleYesNo]);

  AlphaChannelType := TextureCached^.AlphaChannelType;

  if LogRendererCache and Log then
    WritelnLog('++', '%s: %d', [TextureFullUrl, 1]);
end;

procedure TVRMLGLRendererContextCache.TextureVideo_DecReference(
  const TextureVideo: TGLVideo);
var
  I: Integer;
begin
  for I := 0 to TextureVideoCaches.High do
    if TextureVideoCaches.Items[I].GLVideo = TextureVideo then
    begin
      Dec(TextureVideoCaches.Items[I].References);
      if LogRendererCache and Log then
        WritelnLog('--', '%s: %d', [TextureVideoCaches.Items[I].FullUrl,
                                    TextureVideoCaches.Items[I].References]);
      if TextureVideoCaches.Items[I].References = 0 then
      begin
        FreeAndNil(TextureVideoCaches.Items[I].GLVideo);
        TextureVideoCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.TextureVideo_DecReference: no reference ' +
    'found to texture %s', [PointerToStr(TextureVideo)]);
end;

function TVRMLGLRendererContextCache.TextureCubeMap_IncReference(
  Node: TNodeX3DEnvironmentTextureNode;
  const MinFilter, MagFilter: TGLint;
  const Anisotropy: TGLfloat;
  PositiveX, NegativeX,
  PositiveY, NegativeY,
  PositiveZ, NegativeZ: TEncodedImage;
  DDSForMipmaps: TDDSImage;
  out AlphaChannelType: TAlphaChannelType): TGLuint;
var
  I: Integer;
  TextureCached: PTextureCubeMapCache;
begin
  for I := 0 to TextureCubeMapCaches.High do
  begin
    TextureCached := TextureCubeMapCaches.Pointers[I];

    if (TextureCached^.InitialNode = Node) and
       (TextureCached^.MinFilter = MinFilter) and
       (TextureCached^.MagFilter = MagFilter) and
       (TextureCached^.Anisotropy = Anisotropy) then
    begin
      Inc(TextureCached^.References);
      if LogRendererCache and Log then
        WritelnLog('++', 'cube map %s: %d', [PointerToStr(Node), TextureCached^.References]);
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, Result);

  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MAG_FILTER, MagFilter);
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MIN_FILTER, MinFilter);

  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_S, KamGL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_WRAP_T, KamGL_CLAMP_TO_EDGE);

  glTextureCubeMap(
    PositiveX, NegativeX,
    PositiveY, NegativeY,
    PositiveZ, NegativeZ,
    DDSForMipmaps,
    TextureMinFilterNeedsMipmaps(MinFilter));

  TexParameterMaxAnisotropy(GL_TEXTURE_CUBE_MAP_ARB, Anisotropy);

  TextureCached := TextureCubeMapCaches.Add;
  TextureCached^.InitialNode := Node;
  TextureCached^.MinFilter := MinFilter;
  TextureCached^.MagFilter := MagFilter;
  TextureCached^.Anisotropy := Anisotropy;
  TextureCached^.References := 1;
  TextureCached^.GLName := Result;

  { calculate and save AlphaChannelType in the cache.
    Use PositiveX image --- it doesn't matter, they all should have
    the same AlphaChannelType. }
  TextureCached^.AlphaChannelType := PositiveX.AlphaChannelType(
    AlphaTolerance, AlphaWrongPixelsTolerance);
  if Log and (TextureCached^.AlphaChannelType <> atNone)  then
    WritelnLog('Alpha Detection', 'Alpha cube map texture ' + PointerToStr(Node) +
      ' detected as simple yes/no alpha channel: ' +
      BoolToStr[TextureCached^.AlphaChannelType = atSimpleYesNo]);

  AlphaChannelType := TextureCached^.AlphaChannelType;

  if LogRendererCache and Log then
    WritelnLog('++', 'cube map %s: %d', [PointerToStr(Node), 1]);
end;

procedure TVRMLGLRendererContextCache.TextureCubeMap_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureCubeMapCaches.High do
    if TextureCubeMapCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(TextureCubeMapCaches.Items[I].References);
      if LogRendererCache and Log then
        WritelnLog('--', 'cube map %s: %d', [PointerToStr(TextureCubeMapCaches.Items[I].InitialNode), TextureCubeMapCaches.Items[I].References]);
      if TextureCubeMapCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(TextureCubeMapCaches.Items[I].GLName));
        TextureCubeMapCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.TextureCubeMap_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLGLRendererContextCache.Texture3D_IncReference(
  Node: TNodeX3DTexture3DNode;
  const MinFilter, MagFilter: TGLint;
  const Anisotropy: TGLfloat;
  const TextureWrap: TTextureWrap3D;
  Image: TEncodedImage; DDS: TDDSImage;
  out AlphaChannelType: TAlphaChannelType): TGLuint;
var
  I: Integer;
  TextureCached: PTexture3DCache;
begin
  for I := 0 to Texture3DCaches.High do
  begin
    TextureCached := Texture3DCaches.Pointers[I];

    if (TextureCached^.InitialNode = Node) and
       (TextureCached^.MinFilter = MinFilter) and
       (TextureCached^.MagFilter = MagFilter) and
       (TextureCached^.Anisotropy = Anisotropy) and
       (TextureCached^.Wrap = TextureWrap) then
    begin
      Inc(TextureCached^.References);
      if LogRendererCache and Log then
        WritelnLog('++', '3d texture %s: %d', [PointerToStr(Node), TextureCached^.References]);
      AlphaChannelType := TextureCached^.AlphaChannelType;
      Exit(TextureCached^.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_3D_EXT, Result);

  glTextureImage3d(Image, MinFilter, MagFilter, DDS);

  glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, TextureWrap[0]);
  glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, TextureWrap[1]);
  glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R, TextureWrap[2]);

  TexParameterMaxAnisotropy(GL_TEXTURE_3D_EXT, Anisotropy);

  TextureCached := Texture3DCaches.Add;
  TextureCached^.InitialNode := Node;
  TextureCached^.MinFilter := MinFilter;
  TextureCached^.MagFilter := MagFilter;
  TextureCached^.Anisotropy := Anisotropy;
  TextureCached^.Wrap := TextureWrap;
  TextureCached^.References := 1;
  TextureCached^.GLName := Result;

  { calculate and save AlphaChannelType in the cache }
  TextureCached^.AlphaChannelType := Image.AlphaChannelType(
    AlphaTolerance, AlphaWrongPixelsTolerance);
  if Log and (TextureCached^.AlphaChannelType <> atNone)  then
    WritelnLog('Alpha Detection', 'Alpha 3D texture ' + PointerToStr(Node) +
      ' detected as simple yes/no alpha channel: ' +
      BoolToStr[TextureCached^.AlphaChannelType = atSimpleYesNo]);

  AlphaChannelType := TextureCached^.AlphaChannelType;

  if LogRendererCache and Log then
    WritelnLog('++', '3d texture %s: %d', [PointerToStr(Node), 1]);
end;

procedure TVRMLGLRendererContextCache.Texture3D_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to Texture3DCaches.High do
    if Texture3DCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(Texture3DCaches.Items[I].References);
      if LogRendererCache and Log then
        WritelnLog('--', '3d texture %s: %d', [PointerToStr(Texture3DCaches.Items[I].InitialNode), Texture3DCaches.Items[I].References]);
      if Texture3DCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(Texture3DCaches.Items[I].GLName));
        Texture3DCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.Texture3D_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLGLRendererContextCache.TextureDepth_IncReference(
  Node: TNodeX3DTextureNode;
  const TextureWrap: TTextureWrap2D;
  DepthCompareField: TSFString;
  const Width, Height: Cardinal;
  const VisualizeDepthMap: boolean): TGLuint;
var
  I: Integer;
  TextureCached: PTextureDepthOrFloatCache;
begin
  for I := 0 to TextureDepthOrFloatCaches.High do
  begin
    TextureCached := TextureDepthOrFloatCaches.Pointers[I];

    if (TextureCached^.InitialNode = Node) and
       (TextureCached^.Wrap = TextureWrap) then
    begin
      Inc(TextureCached^.References);
      if LogRendererCache and Log then
        WritelnLog('++', 'Depth texture %s: %d', [PointerToStr(Node), TextureCached^.References]);
      Exit(TextureCached^.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, TextureWrap[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, TextureWrap[1]);

  { Do not init any texture image. Just initialize texture sizes
    and both internal and external formats to GL_DEPTH_COMPONENT_ARB
    (will match depth buffer precision). }
  glTexImage2d(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT,
    Width, Height, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, nil);

  if GL_ARB_shadow then
  begin
    if DepthCompareField <> nil then
    begin
      if VisualizeDepthMap or (DepthCompareField.Value = 'NONE') then
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
      end else
      if DepthCompareField.Value = 'COMPARE_R_LEQUAL' then
      begin
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC_ARB, GL_LEQUAL);
      end else
      if DepthCompareField.Value = 'COMPARE_R_GEQUAL' then
      begin
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC_ARB, GL_GEQUAL);
      end else
        VRMLWarning(vwSerious, Format('Invalid value for GeneratedShadowMode.compareMode: "%s"', [DepthCompareField.Value]));
    end else
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE);

    glTexParameteri(GL_TEXTURE_2D, GL_DEPTH_TEXTURE_MODE_ARB, GL_LUMINANCE);
  end else
    VRMLWarning(vwIgnorable, 'OpenGL doesn''t support ARB_shadow, we cannot set depth comparison for depth texture');

  TextureCached := TextureDepthOrFloatCaches.Add;
  TextureCached^.InitialNode := Node;
  TextureCached^.References := 1;
  TextureCached^.Wrap := TextureWrap;
  TextureCached^.GLName := Result;

  if LogRendererCache and Log then
    WritelnLog('++', 'Depth texture %s: %d', [PointerToStr(Node), 1]);
end;

procedure TVRMLGLRendererContextCache.TextureDepth_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureDepthOrFloatCaches.High do
    if TextureDepthOrFloatCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(TextureDepthOrFloatCaches.Items[I].References);
      if LogRendererCache and Log then
        WritelnLog('--', 'Depth texture %s: %d', [PointerToStr(TextureDepthOrFloatCaches.Items[I].InitialNode), TextureDepthOrFloatCaches.Items[I].References]);
      if TextureDepthOrFloatCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(TextureDepthOrFloatCaches.Items[I].GLName));
        TextureDepthOrFloatCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.TextureDepth_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLGLRendererContextCache.TextureFloat_IncReference(
  Node: TNodeX3DTextureNode;
  const TextureMinFilter, TextureMagFilter: TGLint;
  const TextureWrap: TTextureWrap2D;
  const Width, Height: Cardinal;
  const Precision32: boolean): TGLuint;
var
  I: Integer;
  TextureCached: PTextureDepthOrFloatCache;
  InternalFormat: TGLenum;
begin
  for I := 0 to TextureDepthOrFloatCaches.High do
  begin
    TextureCached := TextureDepthOrFloatCaches.Pointers[I];

    if (TextureCached^.InitialNode = Node) and
       (TextureCached^.Wrap = TextureWrap) then
    begin
      Inc(TextureCached^.References);
      if LogRendererCache and Log then
        WritelnLog('++', 'Float texture %s: %d', [PointerToStr(Node), TextureCached^.References]);
      Exit(TextureCached^.GLName);
    end;
  end;

  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, TextureMagFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, TextureMinFilter);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, TextureWrap[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, TextureWrap[1]);

  if Precision32 then
    InternalFormat := GL_RGB32F_ARB { same thing as GL_RGB_FLOAT32_ATI } else
    InternalFormat := GL_RGB16F_ARB { same thing as GL_RGB_FLOAT16_ATI };

  { Do not init any texture image. Just initialize texture sizes and formats. }
  glTexImage2d(GL_TEXTURE_2D, 0, InternalFormat,
    Width, Height, 0, GL_RGB, GL_FLOAT, nil);

  TextureCached := TextureDepthOrFloatCaches.Add;
  TextureCached^.InitialNode := Node;
  TextureCached^.References := 1;
  TextureCached^.Wrap := TextureWrap;
  TextureCached^.GLName := Result;
  { Hm, we probably should store TextureMinFilter, TextureMagFilter, Precision32
    inside TextureCached as well... Ignore this, useless for now ---
    one Node will require only one float texture anyway. }

  if LogRendererCache and Log then
    WritelnLog('++', 'Float texture %s: %d', [PointerToStr(Node), 1]);
end;

procedure TVRMLGLRendererContextCache.TextureFloat_DecReference(
  const TextureGLName: TGLuint);
var
  I: Integer;
begin
  for I := 0 to TextureDepthOrFloatCaches.High do
    if TextureDepthOrFloatCaches.Items[I].GLName = TextureGLName then
    begin
      Dec(TextureDepthOrFloatCaches.Items[I].References);
      if LogRendererCache and Log then
        WritelnLog('--', 'Float texture %s: %d', [PointerToStr(TextureDepthOrFloatCaches.Items[I].InitialNode), TextureDepthOrFloatCaches.Items[I].References]);
      if TextureDepthOrFloatCaches.Items[I].References = 0 then
      begin
        glDeleteTextures(1, @(TextureDepthOrFloatCaches.Items[I].GLName));
        TextureDepthOrFloatCaches.Delete(I, 1);
      end;
      Exit;
    end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.TextureFloat_DecReference: no reference ' +
    'found to texture %d', [TextureGLName]);
end;

function TVRMLGLRendererContextCache.Shape_IncReference(
  Shape: TVRMLRendererShape; Fog: INodeX3DFogObject;
  ARenderer: TVRMLGLRenderer): TShapeCache;
var
  FogEnabled, FogVolumetric: boolean;
  FogVolumetricDirection: TVector3Single;
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
        and vrmlmeshrenderer), then Result must be false. }
      Assigned(ARenderer.Attributes.OnVertexColor) or
      Assigned(ARenderer.Attributes.OnRadianceTransfer) or
      FogVolumetric);
  end;

  function FogVolumetricEqual(
    const Volumetric1: boolean;
    const VolumetricDirection1: TVector3Single;
    const VolumetricVisibilityStart1: Single;
    const Volumetric2: boolean;
    const VolumetricDirection2: TVector3Single;
    const VolumetricVisibilityStart2: Single): boolean;
  begin
    Result := (Volumetric1 = Volumetric2) and
      ( (not Volumetric1) or
        ( VectorsPerfectlyEqual(VolumetricDirection1, VolumetricDirection2) and
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
        WritelnLog('++', 'Shape %s (%s): %d', [PointerToStr(Result), Result.Geometry.NodeTypeName, Result.References]);
      Exit(Result);
    end;
  end;

  { not found, so create new }

  Result := TShapeCache.Create;
  ShapeCaches.Add(Result);
  Result.Attributes := ARenderer.Attributes;
  Result.Geometry := Shape.Geometry;
  Result.State := Shape.State;
  Result.Fog := Fog;
  Result.FogVolumetric := FogVolumetric;
  Result.FogVolumetricDirection := FogVolumetricDirection;
  Result.FogVolumetricVisibilityStart := FogVolumetricVisibilityStart;
  Result.References := 1;

  if LogRendererCache and Log then
    WritelnLog('++', 'Shape %s (%s): %d', [PointerToStr(Result), Result.Geometry.NodeTypeName, Result.References]);
end;

procedure TVRMLGLRendererContextCache.Shape_DecReference(var ShapeCache: TShapeCache);
var
  I: Integer;
begin
  for I := 0 to ShapeCaches.Count - 1 do
  begin
    if ShapeCaches[I] = ShapeCache then
    begin
      Dec(ShapeCache.References);
      if LogRendererCache and Log then
        WritelnLog('--', 'Shape %s (%s): %d', [PointerToStr(ShapeCache), ShapeCache.Geometry.NodeTypeName, ShapeCache.References]);
      if ShapeCache.References = 0 then
        ShapeCaches.Delete(I);
      ShapeCache := nil;
      Exit;
    end;
  end;

  raise EInternalError.Create(
    'TVRMLGLRendererContextCache.Shape_DecReference: no reference found');
end;

function TVRMLGLRendererContextCache.Program_IncReference(ARenderer: TVRMLGLRenderer;
  Shader: TVRMLShader; const ShapeNiceName: string): TShaderProgramCache;
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
    Result.ShaderProgram := TVRMLGLSLProgram.Create(ARenderer);
    Shader.LinkProgram(Result.ShaderProgram);
  except on E: EGLSLError do
    begin
      FreeAndNil(Result.ShaderProgram);
      { Note: leave Result assigned and Result.Hash set,
        to avoid reinitializing this shader next time. }
      VRMLWarning(vwIgnorable, Format('Cannot use GLSL shader for shape "%s": %s',
        [ShapeNiceName, E.Message]));
    end;
  end;

  if LogRendererCache and Log then
    WritelnLog('++', 'Shader program (hash %s): %d', [Result.Hash.ToString, Result.References]);
end;

procedure TVRMLGLRendererContextCache.Program_DecReference(var ProgramCache: TShaderProgramCache);
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
    'TVRMLGLRendererContextCache.Program_DecReference: no reference found');
end;

{ TVRMLRenderingAttributes --------------------------------------------------- }

procedure TVRMLRenderingAttributes.Assign(Source: TPersistent);
begin
  if Source is TVRMLRenderingAttributes then
  begin
    OnRadianceTransfer := TVRMLRenderingAttributes(Source).OnRadianceTransfer;
    OnVertexColor := TVRMLRenderingAttributes(Source).OnVertexColor;
    Lighting := TVRMLRenderingAttributes(Source).Lighting;
    UseSceneLights := TVRMLRenderingAttributes(Source).UseSceneLights;
    Opacity := TVRMLRenderingAttributes(Source).Opacity;
    EnableTextures := TVRMLRenderingAttributes(Source).EnableTextures;
    TextureMinFilter := TVRMLRenderingAttributes(Source).TextureMinFilter;
    TextureMagFilter := TVRMLRenderingAttributes(Source).TextureMagFilter;
    PointSize := TVRMLRenderingAttributes(Source).PointSize;
    LineWidth := TVRMLRenderingAttributes(Source).LineWidth;
  end else
    inherited;
end;

function TVRMLRenderingAttributes.EqualForShapeCache(
  SecondValue: TVRMLRenderingAttributes): boolean;
begin
  Result :=
    (SecondValue.OnRadianceTransfer = OnRadianceTransfer) and
    (SecondValue.OnVertexColor = OnVertexColor) and
    (SecondValue.EnableTextures = EnableTextures);
end;

constructor TVRMLRenderingAttributes.Create;
begin
  inherited;

  FLighting := true;
  FUseSceneLights := true;
  FOpacity := 1;
  FEnableTextures := true;
  FTextureMinFilter := GL_LINEAR_MIPMAP_LINEAR;
  FTextureMagFilter := GL_LINEAR;
  FPointSize := DefaultPointSize;
  FLineWidth := DefaultLineWidth;
  FBumpMapping := DefaultBumpMapping;
  FShaders := DefaultShaders;
  FTextureModeGrayscale := GL_MODULATE;
  FTextureModeRGB := GL_MODULATE;
  FVarianceShadowMaps := DefaultVarianceShadowMaps;
  FVertexBufferObject := true;
  FPreserveOpenGLState := false;
  FPercentageCloserFiltering := DefaultPercentageCloserFiltering;
end;

procedure TVRMLRenderingAttributes.ReleaseCachedResources;
begin
  { Nothing to do in this class. }
end;

procedure TVRMLRenderingAttributes.SetOnRadianceTransfer(
  const Value: TRadianceTransferFunction);
begin
  if OnRadianceTransfer <> Value then
  begin
    ReleaseCachedResources;
    FOnRadianceTransfer := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetOnVertexColor(
  const Value: TVertexColorFunction);
begin
  if OnVertexColor <> Value then
  begin
    ReleaseCachedResources;
    FOnVertexColor := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetEnableTextures(const Value: boolean);
begin
  if EnableTextures <> Value then
  begin
    ReleaseCachedResources;
    FEnableTextures := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetTextureMinFilter(const Value: TGLint);
begin
  if TextureMinFilter <> Value then
  begin
    ReleaseCachedResources;
    FTextureMinFilter := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetTextureMagFilter(const Value: TGLint);
begin
  if TextureMagFilter <> Value then
  begin
    ReleaseCachedResources;
    FTextureMagFilter := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetBumpMapping(const Value: TBumpMapping);
begin
  if BumpMapping <> Value then
  begin
    ReleaseCachedResources;
    FBumpMapping := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetPureGeometry(const Value: boolean);
begin
  if PureGeometry <> Value then
  begin
    ReleaseCachedResources;
    FPureGeometry := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetTextureModeGrayscale(const Value: TGLenum);
begin
  FTextureModeGrayscale := Value;
end;

procedure TVRMLRenderingAttributes.SetTextureModeRGB(const Value: TGLenum);
begin
  FTextureModeRGB := Value;
end;

procedure TVRMLRenderingAttributes.SetVarianceShadowMaps(const Value: boolean);
begin
  if VarianceShadowMaps <> Value then
  begin
    ReleaseCachedResources;
    FVarianceShadowMaps := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetVertexBufferObject(const Value: boolean);
begin
  if VertexBufferObject <> Value then
  begin
    ReleaseCachedResources;
    FVertexBufferObject := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetVisualizeDepthMap(const Value: boolean);
begin
  if VisualizeDepthMap <> Value then
  begin
    ReleaseCachedResources;
    FVisualizeDepthMap := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetShaders(const Value: TShadersRendering);
begin
  FShaders := Value;
end;

{ TVRMLGLRenderer ---------------------------------------------------------- }

constructor TVRMLGLRenderer.Create(
  AttributesClass: TVRMLRenderingAttributesClass;
  ACache: TVRMLGLRendererContextCache);
begin
  inherited Create;

  FAttributes := AttributesClass.Create;

  GLTextureNodes := TGLTextureNodes.Create;
  BumpMappingRenderers := TBumpMappingRenderersList.Create;
  ScreenEffectPrograms := TGLSLProgramsList.Create;

  TextureTransformUnitsUsedMore := TDynLongIntArray.Create;

  PreparedShader := TVRMLShader.Create;

  FCache := ACache;
  Assert(FCache <> nil);
end;

destructor TVRMLGLRenderer.Destroy;
begin
  UnprepareAll;

  FreeAndNil(TextureTransformUnitsUsedMore);
  FreeAndNil(GLTextureNodes);
  FreeAndNil(BumpMappingRenderers);
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

    glDeleteBuffersARB(Ord(High(Vbo)) + 1, @Vbo);

    for I := Low(I) to High(I) do
      Vbo[I] := 0;
  end;
end;

procedure TShapeCache.FreeArrays(const Changed: TVboTypes);
begin
  FreeAndNil(Arrays);
  VboToReload += Changed;
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
      glBindBufferARB(Target, Vbo[VboType]);
      if NewVbos or
        (VboAllocatedUsage <> DataUsage) or
        (VboAllocatedSize[VboType] <> Size) then
      begin
        glBufferDataARB(Target, Size, Data, DataUsage);
        VboAllocatedSize[VboType] := Size;
      end else
        glBufferSubDataARB(Target, 0, Size, Data);
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
        if Result <> '' then Result += ',';
        Result += Names[I];
      end;
    Result := '[' + Result + ']';
  end;

begin
  Assert(GL_ARB_vertex_buffer_object and not GLVersion.BuggyVBO);
  Assert(not Arrays.DataFreed);

  NewVbos := Vbo[vtCoordinate] = 0;
  if NewVbos then
  begin
    glGenBuffersARB(Ord(High(Vbo)) + 1, @Vbo);
    if Log then
      WritelnLog('Renderer', Format('Creating and loading data to VBOs (%d,%d,%d)',
        [Vbo[vtCoordinate], Vbo[vtAttribute], Vbo[vtIndex]]));
  end else
  begin
    if Log then
      WritelnLog('Renderer', Format('Loading data to existing VBOs (%d,%d,%d), reloading %s',
        [Vbo[vtCoordinate], Vbo[vtAttribute], Vbo[vtIndex],
         VboTypesToStr(VboToReload)]));
  end;

  if DynamicGeometry then
    DataUsage := GL_DYNAMIC_DRAW_ARB else
    DataUsage := GL_STATIC_DRAW_ARB;

  BufferData(vtCoordinate, GL_ARRAY_BUFFER_ARB,
    Arrays.Count * Arrays.CoordinateSize, Arrays.CoordinateArray);

  BufferData(vtAttribute, GL_ARRAY_BUFFER_ARB,
    Arrays.Count * Arrays.AttributeSize, Arrays.AttributeArray);

  if Arrays.Indexes <> nil then
    BufferData(vtIndex, GL_ELEMENT_ARRAY_BUFFER_ARB,
      Arrays.Indexes.Count * SizeOf(LongInt), Arrays.Indexes.ItemsArray);

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

{ TVRMLRendererShape --------------------------------------------------------- }

procedure TVRMLRendererShape.LoadArraysToVbo;
begin
  Assert(Cache <> nil);
  Cache.LoadArraysToVbo(DynamicGeometry);
end;

{ Prepare/Unprepare[All] ------------------------------------------------------- }

procedure TVRMLGLRenderer.PrepareIDecls(Nodes: TMFNode;
  State: TVRMLGraphTraverseState);
begin
  PrepareIDecls(Nodes.Items, State);
end;

procedure TVRMLGLRenderer.PrepareIDecls(Nodes: TVRMLNodesList;
  State: TVRMLGraphTraverseState);
var
  I: Integer;
begin
  for I := 0 to Nodes.Count - 1 do
    GLTextureNodes.PrepareInterfaceDeclarationsTextures(Nodes[I], State, Self);
end;

procedure TVRMLGLRenderer.Prepare(State: TVRMLGraphTraverseState);

  procedure PrepareFont(
    fsfam: TVRMLFontFamily;
    fsbold, fsitalic: boolean;
    TTF_Font: PTrueTypeFont);
  begin
    if not FontsReferences[fsfam, fsbold, fsitalic] then
    begin
      Cache.Fonts_IncReference(fsfam, fsbold, fsitalic, TTF_Font);
      FontsReferences[fsfam, fsbold, fsitalic] := true;
    end;
  end;

var
  FontStyle: TNodeFontStyle_2;
  I: Integer;
  Lights: TLightInstancesList;
  Texture: TNodeX3DTextureNode;
begin
  { przygotuj font }
  if State.ShapeNode = nil then
    PrepareFont(
      State.LastNodes.FontStyle.Family,
      State.LastNodes.FontStyle.Bold,
      State.LastNodes.FontStyle.Italic,
      State.LastNodes.FontStyle.TTF_Font) else
  if (State.ShapeNode.FdGeometry.Value <> nil) and
     (State.ShapeNode.FdGeometry.Value is TNodeText) then
  begin
    { We know that TNodeText(State.ShapeNode.FdGeometry.Value)
      will be the shape node rendered along with this State.
      That's how it works in VRML 2.0: State actually contains
      reference to Shape that contains reference to geometry node,
      which means that actually State contains rendered node too. }
    FontStyle := TNodeText(State.ShapeNode.FdGeometry.Value).FontStyle;
    if FontStyle = nil then
      PrepareFont(
        TNodeFontStyle_2.DefaultFamily,
        TNodeFontStyle_2.DefaultBold,
        TNodeFontStyle_2.DefaultItalic,
        TNodeFontStyle_2.DefaultTTF_Font) else
      PrepareFont(
        FontStyle.Family,
        FontStyle.Bold,
        FontStyle.Italic,
        FontStyle.TTF_Font);
  end else
  if (State.ShapeNode.FdGeometry.Value <> nil) and
     (State.ShapeNode.FdGeometry.Value is TNodeText3D) then
  begin
    { We know that TNodeText3D(State.ShapeNode.FdGeometry.Value)
      will be the shape node rendered along with this State.
      That's how it works in VRML 2.0: State actually contains
      reference to Shape that contains reference to geometry node,
      which means that actually State contains rendered node too. }
    FontStyle := TNodeText3D(State.ShapeNode.FdGeometry.Value).FontStyle;
    if FontStyle = nil then
      PrepareFont(
        TNodeFontStyle_2.DefaultFamily,
        TNodeFontStyle_2.DefaultBold,
        TNodeFontStyle_2.DefaultItalic,
        TNodeFontStyle_2.DefaultTTF_Font) else
      PrepareFont(
        FontStyle.Family,
        FontStyle.Bold,
        FontStyle.Italic,
        FontStyle.TTF_Font);
  end;

  GLTextureNodes.Prepare(State, State.Texture, Self);

  BumpMappingRenderers.Prepare(State, Self);

  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    PrepareIDecls(State.ShapeNode.Appearance.FdEffects, State);
    PrepareIDecls(State.ShapeNode.Appearance.FdShaders, State);
  end;

  if State.Effects <> nil then
    PrepareIDecls(State.Effects, State);

  Lights := State.Lights;
  if Lights <> nil then
    for I := 0 to Lights.Count - 1 do
      PrepareIDecls(Lights.Items[I].Node.FdEffects, State);

  Texture := State.Texture;
  if Texture <> nil then
  begin
    PrepareIDecls(Texture.FdEffects, State);
    if Texture is TNodeMultiTexture then
      for I := 0 to TNodeMultiTexture(Texture).FdTexture.Count - 1 do
        if TNodeMultiTexture(Texture).FdTexture[I] is TNodeX3DTextureNode then
          PrepareIDecls(TNodeX3DTextureNode(TNodeMultiTexture(Texture).
            FdTexture[I]).FdEffects, State);
  end;
end;

procedure TVRMLGLRenderer.PrepareScreenEffect(Node: TNodeScreenEffect);
var
  Shader: TVRMLShader;
  ShaderProgram: TVRMLGLSLProgram;
  ShaderNode: TNodeComposedShader;
begin
  if not Node.ShaderLoaded then
  begin
    Assert(Node.Shader = nil);
    Node.ShaderLoaded := true;
    if Node.FdEnabled.Value then
    begin
      { make sure that textures inside shaders are prepared }
      PrepareIDecls(Node.FdShaders, Node.StateForShaderPrepare);

      Shader := TVRMLShader.Create;
      try
        { for ScreenEffect, we require that some ComposedShader was present.
          Rendering with default TVRMLShader shader makes no sense. }
        if Shader.EnableCustomShaderCode(Node.FdShaders, ShaderNode) then
        try
          ShaderProgram := TVRMLGLSLProgram.Create(Self);
          Shader.LinkProgram(ShaderProgram);

          { We have to ignore invalid uniforms, as it's normal that when
            rendering screen effect we will pass some screen_* variables
            that you will not use. }
          ShaderProgram.UniformNotFoundAction := uaIgnore;

          Node.Shader := ShaderProgram;
          ScreenEffectPrograms.Add(ShaderProgram);
        except on E: EGLSLError do
          begin
            FreeAndNil(ShaderProgram);
            VRMLWarning(vwIgnorable, Format('Cannot use GLSL shader for ScreenEffect: %s',
              [E.Message]));
          end;
        end;
      finally FreeAndNil(Shader) end;
    end;
  end;
end;

procedure TVRMLGLRenderer.UnprepareTexture(Node: TNodeX3DTextureNode);
begin
  GLTextureNodes.Unprepare(Node);
end;

procedure TVRMLGLRenderer.UnprepareAll;
var
  fsfam: TVRMLFontFamily;
  fsbold , fsitalic: boolean;
begin
  { release fonts }
  for fsfam := Low(fsfam) to High(fsfam) do
    for fsbold := Low(boolean) to High(boolean) do
      for fsitalic := Low(boolean) to High(boolean) do
        if FontsReferences[fsfam, fsbold, fsitalic] then
        begin
          FontsReferences[fsfam, fsbold, fsitalic] := false;
          Cache.Fonts_DecReference(fsfam, fsbold, fsitalic);
        end;

  GLTextureNodes.UnprepareAll;
  BumpMappingRenderers.UnprepareAll;
  ScreenEffectPrograms.Count := 0; { this will free programs inside }
end;

function TVRMLGLRenderer.BumpMapping: TBumpMapping;
begin
  if (Attributes.BumpMapping <> bmNone) and
    Attributes.EnableTextures and
    (not Attributes.PureGeometry) and

    { EXT_texture_env_combine (standard since 1.3) required }
    (GL_EXT_texture_env_combine or GL_version_1_3) and

    { Actually, other extensions also don't have to exist, they are built in
      newer OpenGL version. But this requires getting their procedures under different
      names (without extension suffix). For EXT_texture_env_combine, this is simpler
      since it only defines new constants and these are the same, whether it's extension
      or built-in GL 1.3. }

    { ARB_multitexture required (TODO: standard since 1.3, see above comments) }
    GL_ARB_multitexture and

    { GL >= 1.3 required for GL_SUBTRACT.

      As you see, actually this whole check could be substituted by GL >= 1.3,
      as this allows GL_SUBTRACT and provides all extensions required here. }
    GL_version_1_3 and

    { ARB_texture_env_dot3 required (TODO: standard since 1.3, see above comments) }
    GL_ARB_texture_env_dot3 and

    (TGLSLProgram.ClassSupport <> gsNone) then
    Result := Attributes.BumpMapping else
    Result := bmNone;
end;

function TVRMLGLRenderer.PreparedTextureAlphaChannelType(
  TextureNode: TNodeX3DTextureNode;
  out AlphaChannelType: TAlphaChannelType): boolean;
var
  Index: Integer;
begin
  Index := GLTextureNodes.TextureNodeIndex(TextureNode);
  Result := Index <> -1;

  if Result then
    AlphaChannelType := GLTextureNodes.Items[Index].AlphaChannelType;
end;

{ Render ---------------------------------------------------------------------- }

procedure TVRMLGLRenderer.ActiveTexture(const TextureUnit: Cardinal);
begin
  if GL_ARB_multitexture then
    glActiveTextureARB(GL_TEXTURE0_ARB + TextureUnit);
end;

procedure TVRMLGLRenderer.DisableTexture(const TextureUnit: Cardinal);
begin
  { TODO: what to do for Shader? We cannot disable texture later...
    We should detect it, and do enable only when appropriate }

  { This must be synchronized, and disable all that can be enabled
    by TVRMLShape.EnableTexture }
  ActiveTexture(TextureUnit);
  DisableCurrentTexture;
end;

procedure TVRMLGLRenderer.DisableCurrentTexture;
begin
  glDisable(GL_TEXTURE_2D);
  if GL_ARB_texture_cube_map then glDisable(GL_TEXTURE_CUBE_MAP_ARB);
  if GL_EXT_texture3D        then glDisable(GL_TEXTURE_3D_EXT);
end;

procedure TVRMLGLRenderer.GetFog(Node: INodeX3DFogObject;
  out Enabled, Volumetric: boolean;
  out VolumetricDirection: TVector3Single;
  out VolumetricVisibilityStart: Single);
begin
  Enabled := (not Attributes.PureGeometry) and
    (Node <> nil) and (Node.FdVisibilityRange.Value <> 0.0);
  Volumetric := Enabled and Node.FdVolumetric.Value and GL_EXT_fog_coord;

  if Volumetric then
  begin
    VolumetricVisibilityStart :=
      Node.FdVolumetricVisibilityStart.Value * Node.TransformScale;
    VolumetricDirection := Node.FdVolumetricDirection.Value;
  end else
  begin
    { whatever, just set them to any determined values }
    VolumetricVisibilityStart := 0;
    VolumetricDirection := ZeroVector3Single;
  end;
end;

procedure TVRMLGLRenderer.RenderCleanState(const Beginning: boolean);

  procedure DisabeAllTextureUnits;
  var
    I: Integer;
  begin
    for I := 0 to GLMaxTextureUnits - 1 do
      DisableTexture(I);
  end;

var
  I: Integer;
begin
  DisabeAllTextureUnits;

  { Restore active texture unit to 0 }
  if GLUseMultiTexturing then
  begin
    ActiveTexture(0);
    glClientActiveTextureARB(GL_TEXTURE0_ARB);
  end;

  { init our OpenGL state }
  glMatrixMode(GL_MODELVIEW);

  glPointSize(Attributes.PointSize);

  if Beginning then
  begin
    FLineWidth := Attributes.LineWidth;
    glLineWidth(FLineWidth);
  end else
    LineWidth := Attributes.LineWidth;

  if Beginning then
  begin
    FLineType := ltSolid;
    glDisable(GL_LINE_STIPPLE);
  end else
    LineType := ltSolid;

  { Initialize FCullFace, make sure OpenGL state is set as appropriate }
  FCullFace := cfNone;
  glDisable(GL_CULL_FACE);

  SetGLEnabled(GL_DEPTH_TEST, Beginning);

  if not Attributes.PureGeometry then
  begin
    glDisable(GL_COLOR_MATERIAL);
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_GEN_R);
    glDisable(GL_TEXTURE_GEN_Q);

    { We don't really need to enable GL_NORMALIZE.
      We always provide normalized normals (that's how vrmlarraysgenerator.pas
      and vrmlmeshrenderer.inc always calculate them, and when provided
      in VRML/X3D they should also be already normalized).
      However, turning GL_NORMALIZE doesn't give us *any* performance
      benefit as far as I tested (with castle gate, on high-end GPUs
      like Radeon X1600 and low-end like Intel).

      So leave GL_NORMALIZE enabled, it's still useful:
      - for invalid VRML/X3D files that have unnomalized normals.
      - in case caller loaded a scaling matrix
        (for example, Examine camera may allow user to scale the object). }
    SetGLEnabled(GL_NORMALIZE, Beginning);

    if not GLVersion.BuggyLightModelTwoSide then
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE) else
    if Log then
      WritelnLog('Lighting', GLVersion.BuggyLightModelTwoSideMessage);

    glDisable(GL_ALPHA_TEST);
    { We only use glAlphaFunc for textures, and there this value is suitable.
      We never change glAlphaFunc during rendering, so no need to call this in RenderEnd. }
    if Beginning then
      glAlphaFunc(GL_GEQUAL, 0.5);

    { Initialize FSmoothShading, make sure OpenGL state is appropriate }
    FSmoothShading := true;
    glShadeModel(GL_SMOOTH);

    if Beginning then
    begin
      { Initialize FFixedFunctionLighting, make sure OpenGL state is appropriate }
      FFixedFunctionLighting := Attributes.Lighting;
      SetGLEnabled(GL_LIGHTING, FFixedFunctionLighting);
    end else
      glDisable(GL_LIGHTING);

    { No need to disable lights at the beginning.
      LightsRenderer already assumes that state of lights is initially unknown,
      and handles it. }
    if not Beginning then
      for I := 0 to GLMaxLights - 1 do
        glDisable(GL_LIGHT0 + I);

    glDisable(GL_FOG);

    { - We always set diffuse material component from the color.
        This satisfies all cases.
      - TVRMLShader.EnableMaterialFromColor
        takes care of actually enabling COLOR_MATERIAL, it depends on
        the setting below.
      - We never change glColorMaterial during rendering,
        so no need to call this in RenderEnd. }
    if Beginning then
      glColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE);
  end;
end;

procedure TVRMLGLRenderer.RenderBegin(ABaseLights: TLightInstancesList;
  LightRenderEvent: TVRMLLightRenderEvent; const APass: TRenderingPass);
var
  Attribs: TGLbitfield;
begin
  if Attributes.PreserveOpenGLState then
  begin
    { Push OpenGL attributes that can be changed by RenderCleanState, RenderShape }
    Attribs := GL_COLOR_BUFFER_BIT or GL_CURRENT_BIT or GL_ENABLE_BIT
      or GL_FOG_BIT or GL_LIGHTING_BIT or GL_POLYGON_BIT or GL_TEXTURE_BIT
      or GL_TRANSFORM_BIT or GL_LINE_BIT;
    { When BuggyPointSetAttrib, then glPointSize call "leaks" out.
      But there's nothing we can do about it, we cannot use GL_POINT_BIT
      as it crashes Mesa (and produces "invalid enumerant" error in case
      of debug compile). }
    if not GLVersion.BuggyPointSetAttrib then
      Attribs := Attribs or GL_POINT_BIT;
    glPushAttrib(Attribs);

    { Note that push/pop is not fully correctly done for multitexturing:
      - We should push/pop all texture units matrices.
        Right now, we actually push only 0th texture unit matrix.
      - We should make sure that currently active texture unit is saved?
        I'm not sure, does some glPushAttrib param saves this?

      Push/pop texture state saves environment state of all texture units,
      so at least we got glTexEnv covered. (this says OpenGL manpage for
      glPushAttrib, and it applies to both multitexturing by
      ARB extension and by standard GL).
    }
  end;

  BaseLights := ABaseLights;
  Pass := APass;

  RenderCleanState(true);

  { push matrix after RenderCleanState, to be sure we're in modelview mode }
  glPushMatrix;

  Assert(FogNode = nil);
  Assert(not FogEnabled);

  LightsRenderer := TVRMLGLLightsRenderer.Create(LightRenderEvent);
end;

procedure TVRMLGLRenderer.RenderEnd;
begin
  FreeAndNil(LightsRenderer);

  FogNode := nil;
  FogEnabled := false;

  glPopMatrix;

  { pop attribs }
  if Attributes.PreserveOpenGLState then
    glPopAttrib else
    RenderCleanState(false);

  CurrentProgram := nil;
end;

{$ifdef USE_VRML_TRIANGULATION}
procedure TVRMLGLRenderer.DrawTriangle(Shape: TObject;
  const Position: TTriangle3Single;
  const Normal: TTriangle3Single; const TexCoord: TTriangle4Single;
  const Face: TFaceIndex);
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    glNormalv(Normal[I]);
    glTexCoordv(TexCoord[I]);
    glVertexv(Position[I]);
  end;
end;
{$endif USE_VRML_TRIANGULATION}

procedure TVRMLGLRenderer.RenderShape(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject);
var
  Shader: TVRMLShader;
begin
  { instead of TVRMLShader.Create, reuse existing PreparedShader for speed }
  Shader := PreparedShader;
  Shader.Clear;

  Shader.ShapeBoundingBox := Shape.BoundingBox;
  Shader.PercentageCloserFiltering := Attributes.PercentageCloserFiltering;
  Shader.VarianceShadowMaps := Attributes.VarianceShadowMaps;
  RenderShapeLineProperties(Shape, Fog, Shader);
end;

procedure TVRMLGLRenderer.RenderShapeLineProperties(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader);
var
  LP: TNodeLineProperties;
begin
  if Shape.Node <> nil then { Shape.Node is nil for VRML <= 1.0 }
    LP := Shape.Node.LineProperties else
    LP := nil;
  if (LP <> nil) and LP.FdApplied.Value then
  begin
    LineWidth := Max(1.0, Attributes.LineWidth * LP.FdLineWidthScaleFactor.Value);
    LineType := TLineType(
      Clamped(LP.FdLineType.Value - 1, 0, Integer(High(TLineType))));
  end else
  begin
    LineWidth := Attributes.LineWidth;
    LineType := ltSolid;
  end;

  RenderShapeMaterials(Shape, Fog, Shader);
end;

procedure TVRMLGLRenderer.RenderShapeMaterials(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader);

  {$I vrmlglrenderer_materials.inc}

begin
  RenderMaterialsBegin;
  RenderShapeLights(Shape, Fog, Shader, MaterialOpacity, Lighting);
end;

procedure TVRMLGLRenderer.RenderShapeLights(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader;
  const MaterialOpacity: Single; const Lighting: boolean);
var
  SceneLights: TLightInstancesList;
begin
  { All this is done before loading State.Transform.
    The light renderer assumes current matrix contains only camera +
    scene transform.

    All this is done after setting Shader.MaterialSpecularColor
    by RenderMaterialsBegin,
    as MaterialSpecularColor must be already set during Shader.EnableLight. }

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

procedure TVRMLGLRenderer.RenderShapeFog(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader;
  const MaterialOpacity: Single; const Lighting: boolean);

const
  FogCoordinateSource: array [boolean { volumetric }] of TFogCoordinateSource =
  ( fcDepth, fcPassedCoordinate );

  { Set OpenGL fog based on given fog node. Returns also fog parameters,
    like GetFog. }
  procedure RenderFog(Node: INodeX3DFogObject;
    out Volumetric: boolean;
    out VolumetricDirection: TVector3Single;
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

      if Node.FdVolumetric.Value and (not GL_EXT_fog_coord) then
      begin
        { Try to make normal fog that looks similar. This looks poorly,
          but it's not a real problem --- EXT_fog_coord is supported
          on all sensible GPUs nowadays. Increasing VisibilityRangeScaled
          seems enough. }
        VRMLWarning(vwIgnorable, 'Volumetric fog not supported, your graphic card (OpenGL) doesn''t support EXT_fog_coord');
        VisibilityRangeScaled *= 5;
      end;

      if Volumetric then
      begin
        glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);
      end else
      begin
        { If not Volumetric but still GL_EXT_fog_coord, we make sure
          that we're *not* using FogCoord below. }
        if GL_EXT_fog_coord then
          glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
      end;

      glFogv(GL_FOG_COLOR, Vector4Single(Node.FdColor.Value, 1.0));

      { calculate FogType }
      if Node.FdFogType.Value = 'LINEAR' then
        FogType := ftLinear else
      if Node.FdFogType.Value = 'EXPONENTIAL' then
        FogType := ftExp else
      begin
        VRMLWarning(vwSerious, 'Unknown fog type "' + Node.FdFogType.Value + '"');
        FogType := ftLinear;
      end;

      case FogType of
        ftLinear: begin
            glFogi(GL_FOG_MODE, GL_LINEAR);
            glFogf(GL_FOG_START, 0);
            glFogf(GL_FOG_END, VisibilityRangeScaled);
          end;
        ftExp: begin
            glFogi(GL_FOG_MODE, GL_EXP);
            glFogf(GL_FOG_DENSITY, FogDensityFactor / VisibilityRangeScaled);
          end;
        else raise EInternalError.Create('TVRMLGLRenderer.RenderShapeFog:FogType?');
      end;

      glEnable(GL_FOG);
    end else
      glDisable(GL_FOG);
  end;

begin
  { Enable / disable fog and set fog parameters if needed }
  if Fog <> FogNode then
  begin
    FogNode := Fog;
    RenderFog(FogNode, FogVolumetric,
      FogVolumetricDirection, FogVolumetricVisibilityStart);
  end;

  if FogEnabled then
    Shader.EnableFog(FogType, FogCoordinateSource[FogVolumetric]);

  RenderShapeTextureTransform(Shape, Fog, Shader, MaterialOpacity, Lighting);
end;

procedure TVRMLGLRenderer.RenderShapeTextureTransform(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader;
  const MaterialOpacity: Single; const Lighting: boolean);

  { Pass non-nil TextureTransform that is not a MultiTextureTransform.
    Then this will simply do glMultMatrix (or equivalent) applying
    transformations encoded in this TextureTransform node. }
  procedure TextureMultMatrix(TextureTransform: TNodeX3DTextureTransformNode);
  begin
    if TextureTransform is TNodeTextureTransform then
    begin
      { Optimized version of
          glMultMatrix(TextureTransform.TransformMatrix);
        specially for TNodeTextureTransform. Possibly using OpenGL
        translate etc. commands instead of loading directly 4x4 matrix will
        result in some performance/precision gain (but, not confirmed in
        practice). }
      with TNodeTextureTransform(TextureTransform) do
      begin
        glTranslatef(-FdCenter.Value[0], -FdCenter.Value[1], 0);
        glScalef(FdScale.Value[0], FdScale.Value[1], 1);
        glRotatef(RadToDeg(FdRotation.Value), 0, 0, 1);
        glTranslatef(
          FdTranslation.Value[0] + FdCenter.Value[0],
          FdTranslation.Value[1] + FdCenter.Value[1], 0);
      end;
    end else
      glMultMatrix(TextureTransform.TransformMatrix);
  end;

var
  TextureTransform: TNodeX3DTextureTransformNode;
  Child: TVRMLNode;
  Transforms: TMFNode;
  I: Integer;
  State: TVRMLGraphTraverseState;
begin
  State := Shape.State;

  TextureTransformUnitsUsed := 0;
  TextureTransformUnitsUsedMore.Count := 0;

  if (State.ShapeNode = nil { VRML 1.0, always some texture transform }) or
     (State.ShapeNode.TextureTransform <> nil { VRML 2.0 with tex transform }) then
  begin
    glMatrixMode(GL_TEXTURE);

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
      TextureTransformUnitsUsed := 1;
      ActiveTexture(0);
      glPushMatrix;
      glMultMatrix(State.TextureTransform);
    end else
    begin
      TextureTransform := State.ShapeNode.TextureTransform;
      if TextureTransform <> nil then
      begin
        if TextureTransform is TNodeMultiTextureTransform then
        begin
          Transforms := TNodeMultiTextureTransform(TextureTransform).FdTextureTransform;

          { Multitexturing, so use as many texture units as there are children in
            MultiTextureTransform.textureTransform.
            Cap by available texture units. }
          TextureTransformUnitsUsed := Min(Transforms.Count, GLMaxTextureUnits);

          for I := 0 to TextureTransformUnitsUsed - 1 do
          begin
            ActiveTexture(I);
            glPushMatrix;
            Child := Transforms.Items[I];
            if (Child <> nil) and
               (Child is TNodeX3DTextureTransformNode) then
            begin
              if Child is TNodeMultiTextureTransform then
                VRMLWarning(vwSerious, 'MultiTextureTransform.textureTransform list cannot contain another MultiTextureTransform instance') else
                TextureMultMatrix(TNodeX3DTextureTransformNode(Child));
            end;
          end;
        end else
        { Check below is done because X3D specification explicitly
          says that MultiTexture is affected *only* by MultiTextureTransform,
          that is normal TextureTransform and such is ignored (treated
          like identity transform, *not* applied to 1st texture unit).

          By the way, we don't do any texture transform if Texture = nil,
          since then no texture is used anyway. }
        if (State.Texture <> nil) and
           (not (State.Texture is TNodeMultiTexture)) then
        begin
          TextureTransformUnitsUsed := 1;
          ActiveTexture(0);
          glPushMatrix;
          TextureMultMatrix(TextureTransform);
        end;
      end;
    end;

    { restore GL_MODELVIEW }
    glMatrixMode(GL_MODELVIEW);
  end;

  RenderShapeClipPlanes(Shape, Fog, Shader, MaterialOpacity, Lighting);

  if (TextureTransformUnitsUsed <> 0) or
     (TextureTransformUnitsUsedMore.Count <> 0) then
  begin
    glMatrixMode(GL_TEXTURE);

    for I := 0 to TextureTransformUnitsUsed - 1 do
    begin
      { This code is Ok also when not GLUseMultiTexturing: then
        TextureTransformUnitsUsed for sure is <= 1 and ActiveTexture
        will be simply ignored. }
      ActiveTexture(I);
      glPopMatrix;
    end;

    for I := 0 to TextureTransformUnitsUsedMore.High do
    begin
      ActiveTexture(TextureTransformUnitsUsedMore.Items[I]);
      glPopMatrix;
    end;

    { restore GL_MODELVIEW }
    glMatrixMode(GL_MODELVIEW);
  end;
end;

procedure TVRMLGLRenderer.RenderShapeClipPlanes(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader;
  const MaterialOpacity: Single; const Lighting: boolean);
var
  { How many clip planes were enabled (and so, how many must be disabled
    at the end). }
  ClipPlanesEnabled: Cardinal;

  { Initialize OpenGL clip planes, looking at ClipPlanes list.
    We know we're inside GL_MODELVIEW mode,
    and we know all clip planes are currently disabled. }
  procedure ClipPlanesBegin(ClipPlanes: TDynClipPlaneArray);
  var
    I: Integer;
    ClipPlane: PClipPlane;
  begin
    ClipPlanesEnabled := 0;
    { GLMaxClipPlanes should be >= 6 with every conforming OpenGL,
      but still better check. }
    if (GLMaxClipPlanes > 0) and (ClipPlanes <> nil) then
      for I := 0 to ClipPlanes.Count - 1 do
      begin
        ClipPlane := @(ClipPlanes.Items[I]);
        if ClipPlane^.Node.FdEnabled.Value then
        begin
          Assert(ClipPlanesEnabled < GLMaxClipPlanes);

          { Nope, you should *not* multiply
            ClipPlane^.Transform * plane yourself.
            The plane equation cannot be transformed in the same way
            as you transform normal 4D vertex/direction (Matrix * vector).
            E.g. translating a plane this way, with a standard translation
            matrix, would make nonsense plane as a result.
            This much I understand :)

            So what OpenGL does? Some voodoo to allow you to specify
            plane equation in local (in current modelview) space,
            and not worry about the math :)
            http://www2.imm.dtu.dk/~jab/texgen.pdf sheds some light on this.
            glClipPlane docs say that glClipPlane is multiplied by
            the *inverse* of modelview. The wording is crucial here:
            plane is multiplied by the matrix, not the other way around. }

          glPushMatrix;
            glMultMatrix(ClipPlane^.Transform);
            glClipPlane(GL_CLIP_PLANE0 + ClipPlanesEnabled,
              Vector4Double(ClipPlane^.Node.FdPlane.Value));
            Shader.EnableClipPlane(ClipPlanesEnabled);
          glPopMatrix;

          Inc(ClipPlanesEnabled);

          { No more clip planes possible, regardless if there are any more
            enabled clip planes on the list. }
          if ClipPlanesEnabled = GLMaxClipPlanes then Break;
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

  glPushMatrix;
    glMultMatrix(Shape.State.Transform);
    RenderShapeCreateMeshRenderer(Shape, Fog, Shader, MaterialOpacity, Lighting);
  glPopMatrix;

  ClipPlanesEnd;
end;

procedure TVRMLGLRenderer.RenderShapeCreateMeshRenderer(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader;
  const MaterialOpacity: Single; const Lighting: boolean);
var
  GeneratorClass: TVRMLArraysGeneratorClass;
  MeshRenderer: TVRMLMeshRenderer;

  { If Shape.Geometry should be rendered using one of TVRMLMeshRenderer
    classes, then create appropriate MeshRenderer and return @true.
    Otherwise return @false and doesn't set MeshRenderer.

    Takes care of initializing MeshRenderer, so you have to call only
    MeshRenderer.Render. }
  function InitMeshRenderer: boolean;
  begin
    Result := true;

    GeneratorClass := ArraysGenerator(Shape.Geometry);

    if GeneratorClass = nil then
    begin
      if Shape.Geometry is TNodeAsciiText_1 then
        MeshRenderer := TAsciiTextRenderer.Create(Self, Shape) else
      if Shape.Geometry is TNodeText then
        MeshRenderer := TTextRenderer.Create(Self, Shape) else
      if Shape.Geometry is TNodeText3D then
        MeshRenderer := TText3DRenderer.Create(Self, Shape) else
        Result := false;
    end else
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

  {$ifndef USE_VRML_TRIANGULATION}
  { Initalize MeshRenderer to something non-nil. }
  if not InitMeshRenderer then
  begin
    VRMLWarning(vwSerious, Format('Rendering of node kind "%s" not implemented',
      [Shape.NiceName]));
    Exit;
  end;

  Assert(MeshRenderer <> nil);
  {$else}
  MeshRenderer := nil;
  {$endif}

  try
    RenderShapeShaders(Shape, Fog, Shader, MaterialOpacity, Lighting,
      GeneratorClass, MeshRenderer);
  finally
    FreeAndNil(MeshRenderer);
  end;
end;

{$define MeshRenderer := TVRMLMeshRenderer(ExposedMeshRenderer) }

procedure TVRMLGLRenderer.RenderShapeShaders(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader;
  const MaterialOpacity: Single; const Lighting: boolean;
  GeneratorClass: TVRMLArraysGeneratorClass;
  ExposedMeshRenderer: TObject);
var
  { > 0 means that we had custom shader node *and* it already
    needs given number texture units. Always 0 otherwise. }
  UsedGLSLTexCoordsNeeded: Cardinal;

  function TextureCoordsDefined: Cardinal;
  var
    TexCoord: TVRMLNode;
  begin
    if Shape.Geometry.TexCoord(Shape.State, TexCoord) and
       (TexCoord <> nil) then
    begin
      if TexCoord is TNodeMultiTextureCoordinate then
        Result := TNodeMultiTextureCoordinate(TexCoord).FdTexCoord.Count else
        Result := 1;
    end else
      Result := 0;
  end;

  function TextureUnitsDefined(Node: TNodeComposedShader): Cardinal;

    function TextureUnits(Node: TVRMLNode): Cardinal;
    begin
      if Node is TNodeMultiTexture then
        Result := TNodeMultiTexture(Node).FdTexture.Count else
      if Node is TNodeX3DTextureNode then
        Result := 1 else
        Result := 0;
    end;

  var
    I, J: Integer;
    UniformField: TVRMLField;
    IDecls: TVRMLInterfaceDeclarationsList;
  begin
    IDecls := Node.InterfaceDeclarations;
    Result := 0;
    Assert(IDecls <> nil);
    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
          Result += TextureUnits(TSFNode(UniformField).Value) else
        if UniformField is TMFNode then
          for J := 0 to TMFNode(UniformField).Count - 1 do
            Result += TextureUnits(TMFNode(UniformField)[J]);
      end;
    end;
  end;

var
  TCD: Cardinal;
  UsedShaderNode: TNodeComposedShader;
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
      MaxTo1st(UsedGLSLTexCoordsNeeded, TCD);
    end;
  end;

  RenderShapeTextures(Shape, Fog, Shader, MaterialOpacity, Lighting,
    GeneratorClass, MeshRenderer, UsedGLSLTexCoordsNeeded);
end;

procedure TVRMLGLRenderer.RenderShapeTextures(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader;
  const MaterialOpacity: Single; const Lighting: boolean;
  GeneratorClass: TVRMLArraysGeneratorClass;
  ExposedMeshRenderer: TObject;
  UsedGLSLTexCoordsNeeded: Cardinal);

  function NodeTextured(Node: TVRMLGeometryNode): boolean;
  begin
    Result := not (
      (Node is TNodePointSet_2) or
      (Node is TNodeIndexedLineSet_2));
  end;

  procedure RenderTexturesBegin;
  var
    TextureNode: TNodeX3DTextureNode;
    GLTextureNode: TGLTextureNode;
    AlphaTest: boolean;
  begin
    TexCoordsNeeded := 0;
    BoundTextureUnits := 0;

    if Attributes.PureGeometry then
      Exit;

    AlphaTest := false;
    TextureNode := Shape.State.Texture;
    GLTextureNode := GLTextureNodes.TextureNode(TextureNode);

    if UsedGLSLTexCoordsNeeded > 0 then
    begin
      { Do not bind/enable normal textures. Just set TexCoordsNeeded
        to generate tex coords for textures used in the shader.
        Leave BoundTextureUnits at 0 (BoundTextureUnits will be increased
        later when shader actually binds texture uniform values). }
      TexCoordsNeeded := UsedGLSLTexCoordsNeeded;
    end else
    if (TextureNode <> nil) and
       Attributes.EnableTextures and
       NodeTextured(Shape.Geometry) and
       (GLTextureNode <> nil) then
    begin
      { This works also for TextureNode being TNodeMultiTexture,
        since it has smartly calculated AlphaChannelType. }
      AlphaTest := GLTextureNode.AlphaChannelType = atSimpleYesNo;

      GLTextureNode.EnableAll(GLMaxTextureUnits, TexCoordsNeeded, Shader);
      BoundTextureUnits := TexCoordsNeeded;

      { If there is any texture, and we have room for one more texture,
        try enabling bump mapping. Note that we don't increase
        TexCoordsNeeded for this, as bump mapping uses the existing
        texture coord. }
      if (TexCoordsNeeded > 0) and
         (TexCoordsNeeded < GLMaxTextureUnits) then
        BumpMappingRenderers.Enable(Shape.State, BoundTextureUnits, Shader);
    end;

    { Set ALPHA_TEST enabled state.

      This is not necessarily perfect for multitexturing,
      but there's really no way to set it automatically correct for
      multitexturing, as various operations may effectively flatten
      alpha anyway.
      So we only care to make it correct for a single texture case.

      Note that if GL_MODULATE is combined with ALPHA_TEST and material
      has some transparency (non-1 alpha) too, then the alpha
      actually tested will not be the alpha of the texture alone ---
      it will be the alpha of the texture mixed with the alpha of the material.
      (Alpha of the light source isn't multiplied here, AFAIK,
      I don't really know where light source alpha is used,
      and VRML author cannot set it anyway.)
      I don't see any sensible way to solve this with fixed-function OpenGL
      pipeline, that's just how GL_MODULATE with GL_ALPHA_TEST work. }

    if AlphaTest then
      Shader.EnableAlphaTest else
      glDisable(GL_ALPHA_TEST);

    { Make active texture 0. This is helpful for rendering code of
      some primitives that do not support multitexturing now
      (inside vrmlmeshrenderer_x3d_text.inc),
      this way they will at least define correct texture coordinates
      for texture unit 0. }

    if (TexCoordsNeeded > 0) and GLUseMultiTexturing then
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

procedure TVRMLGLRenderer.RenderShapeInside(Shape: TVRMLRendererShape;
  Fog: INodeX3DFogObject; Shader: TVRMLShader;
  const MaterialOpacity: Single; const Lighting: boolean;
  GeneratorClass: TVRMLArraysGeneratorClass;
  ExposedMeshRenderer: TObject);
var
  Generator: TVRMLArraysGenerator;
  CoordinateRenderer: TBaseCoordinateRenderer;
  VBO: boolean;
begin
  {$ifdef USE_VRML_TRIANGULATION}
  { Simple rendering using LocalTriangulate. }
  glBegin(GL_TRIANGLES);
  Shape.LocalTriangulate(true, @DrawTriangle);
  glEnd;
  {$else}

  { initialize TBaseCoordinateRenderer.Arrays now }
  if GeneratorClass <> nil then
  begin
    Assert(MeshRenderer is TBaseCoordinateRenderer);
    CoordinateRenderer := TBaseCoordinateRenderer(MeshRenderer);

    { calculate Shape.Cache }
    if Shape.Cache = nil then
      Shape.Cache := Cache.Shape_IncReference(Shape, Fog, Self);

    VBO := Attributes.VertexBufferObject and GL_ARB_vertex_buffer_object
      and not GLVersion.BuggyVBO;

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
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, 0);
  end;

  {$endif USE_VRML_TRIANGULATION}
end;

procedure TVRMLGLRenderer.PushTextureUnit(const TexUnit: Cardinal);
begin
  { Only continue if texture unit is not already pushed
    (otherwise glPushMatrix would not be paired by exactly one glPopMatrix
    later). }

  if (TexUnit >= TextureTransformUnitsUsed) and
     (TextureTransformUnitsUsedMore.IndexOf(TexUnit) = -1) then
  begin
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

procedure TVRMLGLRenderer.UpdateGeneratedTextures(Shape: TVRMLShape;
  TextureNode: TNodeX3DTextureNode;
  const Render: TRenderFromViewFunction;
  const ProjectionNear, ProjectionFar: Single;
  var NeedsRestoreViewport: boolean;
  CurrentViewpoint: TVRMLViewpointNode;
  CameraViewKnown: boolean;
  const CameraPosition, CameraDirection, CameraUp: TVector3Single);

var
  { Only for CheckUpdateField and PostUpdateField }
  UpdateIndex: Integer;
  SavedHandler: TGeneratedTextureHandler;

  { Look at the "update" field's value, decide whether we need updating.
    Will take care of making warning on incorrect "update". }
  function CheckUpdate(Handler: TGeneratedTextureHandler): boolean;
  begin
    SavedHandler := Handler; { for PostUpdateField }

    UpdateIndex := ArrayPosStr(LowerCase(Handler.FdUpdate.Value),
      { Names below must be lowercase }
      ['none', 'next_frame_only', 'always']);

    { Only if update = 'NEXT_FRAME_ONLY',
      or 'ALWAYS' (and UpdateNeeded) remake the texture. }
    Result := (UpdateIndex = 1) or
      ( (UpdateIndex = 2) and Handler.UpdateNeeded );

    if UpdateIndex = -1 then
      VRMLWarning(vwSerious, Format('%s.update invalid field value "%s", will be treated like "NONE"',
        [TextureNode.NodeTypeName, Handler.FdUpdate.Value]));
  end;

  { Call this after CheckUpdateField returned @true and you updated
    the texture.
    Will take care of sending "NONE" after "NEXT_FRAME_ONLY". }
  procedure PostUpdate;
  begin
    { If update = 'NEXT_FRAME_ONLY', change it to 'NONE' now }
    if UpdateIndex = 1 then
      SavedHandler.FdUpdate.Send('NONE');

    SavedHandler.UpdateNeeded := false;
  end;

  procedure UpdateGeneratedCubeMap(TexNode: TNodeGeneratedCubeMapTexture);
  var
    GLNode: TGLGeneratedCubeMapTextureNode;
  begin
    { Shape.BoundingBox must be non-empty, otherwise we don't know from what
      3D point to capture environment. }
    if IsEmptyBox3D(Shape.BoundingBox) then Exit;

    if CheckUpdate(TexNode.GeneratedTextureHandler) then
    begin
      GLNode := TGLGeneratedCubeMapTextureNode(GLTextureNodes.TextureNode(TexNode));
      if GLNode <> nil then
      begin
        GLNode.Update(Render, ProjectionNear, ProjectionFar,
          NeedsRestoreViewport,
          Box3DMiddle(Shape.BoundingBox));

        PostUpdate;

        if Log then
          WritelnLog('CubeMap', 'GeneratedCubeMapTexture texture regenerated');
      end;
    end;
  end;

  procedure UpdateGeneratedShadowMap(TexNode: TNodeGeneratedShadowMap);
  var
    GLNode: TGLGeneratedShadowMap;
  begin
    if CheckUpdate(TexNode.GeneratedTextureHandler) then
    begin
      if (TexNode.FdLight.Value <> nil) and
         (TexNode.FdLight.Value is TNodeX3DLightNode) then
      begin
        GLNode := TGLGeneratedShadowMap(GLTextureNodes.TextureNode(TexNode));
        if GLNode <> nil then
        begin
          GLNode.Update(Render, ProjectionNear, ProjectionFar,
            NeedsRestoreViewport,
            TNodeX3DLightNode(TexNode.FdLight.Value));

          PostUpdate;

          if Log then
            WritelnLog('GeneratedShadowMap', 'GeneratedShadowMap texture regenerated');
        end;
      end else
        VRMLWarning(vwSerious, 'GeneratedShadowMap needs updating, but light = NULL or incorrect');
    end;
  end;

  procedure UpdateRenderedTexture(TexNode: TNodeRenderedTexture);
  var
    GLNode: TGLRenderedTextureNode;
  begin
    if CheckUpdate(TexNode.GeneratedTextureHandler) then
    begin
      GLNode := TGLRenderedTextureNode(GLTextureNodes.TextureNode(TexNode));
      if GLNode <> nil then
      begin
        GLNode.Update(Render, ProjectionNear, ProjectionFar,
          NeedsRestoreViewport,
          CurrentViewpoint, CameraViewKnown,
          CameraPosition, CameraDirection, CameraUp);

        PostUpdate;

        if Log then
          WritelnLog('RenderedTexture', 'RenderedTexture texture regenerated');
      end;
    end;
  end;

begin
  if TextureNode is TNodeGeneratedCubeMapTexture then
    UpdateGeneratedCubeMap(TNodeGeneratedCubeMapTexture(TextureNode)) else
  if TextureNode is TNodeGeneratedShadowMap then
    UpdateGeneratedShadowMap(TNodeGeneratedShadowMap(TextureNode)) else
  if TextureNode is TNodeRenderedTexture then
    UpdateRenderedTexture(TNodeRenderedTexture(TextureNode));
end;

procedure TVRMLGLRenderer.SetCullFace(const Value: TCullFace);
begin
  if FCullFace <> Value then
  begin
    FCullFace := Value;

    { We do not want to touch OpenGL glFrontFace (this will be useful
      for planar mirrors, where caller should be able to control glFrontFace).
      So we use only glCullFace. We assume that glFrontFace = always CCW,
      so we know how to call glCullFace. }

    case Value of
      cfNone: glDisable(GL_CULL_FACE);
      cfCW:  begin glCullFace(GL_BACK);  glEnable(GL_CULL_FACE); end;
      cfCCW: begin glCullFace(GL_FRONT); glEnable(GL_CULL_FACE); end;
      else raise EInternalError.Create('SetCullFace:Value?');
    end;
  end;
end;

procedure TVRMLGLRenderer.SetSmoothShading(const Value: boolean);
begin
  if FSmoothShading <> Value then
  begin
    FSmoothShading := Value;
    if Value then
      glShadeModel(GL_SMOOTH) else
      glShadeModel(GL_FLAT);
  end;
end;

procedure TVRMLGLRenderer.SetFixedFunctionLighting(const Value: boolean);
begin
  if FFixedFunctionLighting <> Value then
  begin
    FFixedFunctionLighting := Value;
    SetGLEnabled(GL_LIGHTING, FixedFunctionLighting);
  end;
end;

procedure TVRMLGLRenderer.SetLineWidth(const Value: Single);
begin
  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    glLineWidth(LineWidth);
  end;
end;

procedure TVRMLGLRenderer.SetLineType(const Value: TLineType);
begin
  if FLineType <> Value then
  begin
    FLineType := Value;
    case LineType of
      ltSolid: glDisable(GL_LINE_STIPPLE);
      ltDashed      : begin glLineStipple(1, $00FF); glEnable(GL_LINE_STIPPLE); end;
      ltDotted      : begin glLineStipple(1, $CCCC); glEnable(GL_LINE_STIPPLE); end;
      ltDashedDotted: begin glLineStipple(1, $FFCC); glEnable(GL_LINE_STIPPLE); end;
      ltDashDotDot  : begin glLineStipple(1, $FCCC); glEnable(GL_LINE_STIPPLE); end;
      else raise EInternalError.Create('LineType?');
    end;
  end;
end;

end.
