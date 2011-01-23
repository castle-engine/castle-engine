{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML low-level rendering (TVRMLGLRenderer).
  You usually don't want to use this renderer directly, you rather want
  to use TVRMLGLScene that hides the dirty details of this renderer.

  The overview of this class can also be found in my master's thesis
  [http://vrmlengine.sourceforge.net/vrml_engine_doc.php]
  in chapter "OpenGL rendering", section "Basic OpenGL rendering".

  @bold(Usage:)

  @orderedList(
    @item(
      First you have to call @link(TVRMLGLRenderer.Prepare) for all
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

      TVRMLGLRenderer.Prepare requires active OpenGL context. It doesn't modify OpenGL
      state (only allocates some resources like texture names and display lists).
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
      as it causes some difficult problems (like GLSLProgram_DecReference really
      needs to look at VRML nodes), and was inherently unclean and unsafe
      (it's not a nice programming practice to have a pointers that
      may be invalid).
    )

    @item(
      To start actual rendering, call TVRMLGLRenderer.RenderBegin. To end rendering, call
      TVRMLGLRenderer.RenderEnd. Between these calls, you should not touch OpenGL state
      yourself --- the renderer may depend that every state change goes
      through it. At the end of TVRMLGLRenderer.RenderEnd, the OpenGL state is restored
      just as it was before TVRMLGLRenderer.RenderBegin.

      Any part of rendering may be saved in a display list.
      You can use separate display lists for TVRMLGLRenderer.RenderBegin, each RenderShape,
      TVRMLGLRenderer.RenderEnd etc., you can put whole rendering in one display list,
      whatever you need.

      It's guarenteed that TVRMLGLRenderer.RenderBegin and TVRMLGLRenderer.RenderEnd work doesn't depend
      on RenderShape along the way, and each RenderShape is independent.
      That is, if you save separate display list for TVRMLGLRenderer.RenderBegin,
      for TVRMLGLRenderer.RenderEnd, and for some shapes, you can later reuse them ---
      using saved TVRMLGLRenderer.RenderBegin / TVRMLGLRenderer.RenderEnd for rendering other shapes,
      using saved shapes to render them again in any order etc.
    )

    @item(
      Between TVRMLGLRenderer.RenderBegin and TVRMLGLRenderer.RenderEnd you should render the shapes by:

@longCode(#
  RenderShapeLights(LightsRenderer, Shape.State);
  RenderShape(Shape);
#)

      Remember that you can render only shapes that have Shape.State
      prepared by TVRMLGLRenderer.Prepare.

      Alternatively, instead of simple RenderShape you can call

@longCode(#
  RenderShapeBegin(Shape);
  try
    RenderShapeInside(Shape);
  finally RenderShapeEnd(Shape) end;
#)

      This is equivalent to RenderShape
      (but sometimes it's better as it allows you to place
      RenderShapeInside on a separate display list, that can be more
      shared (because it doesn't take some transformations into account)).

      Make sure that VRML2ActiveLights are properly initialized if you
      plan to render VRML 2.0 nodes. TVRMLScene and descendants do
      this for you usually.
    )

    @item(
      Since the first prepare / render calls, this renderer assumes it's
      always called in the same OpenGL context. To break association
      with OpenGL context call TVRMLGLRenderer.UnprepareAll (this is like calling TVRMLGLRenderer.Unprepare
      on every prepared thing + clearing some remaining resources).
    )
  )

  @bold(OpenGL state affecting VRML rendering:)

  Some OpenGL state is unconditionally reset by TVRMLGLRenderer.RenderBegin
  (to protect us from doing something nonsensible; don't worry,
  TVRMLGLRenderer.RenderEnd restores it). But there's also some OpenGL state that
  we let affect our rendering. This allows you to customize VRML rendering
  by using normal OpenGL commands.

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
      And you can reuse display lists etc. for the scene in the mirror.
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
  TVRMLGeometryNode.Triangulate, see notes about
  USE_VRML_NODES_TRIANGULATION in the source code.

  @bold(About OpenGL extensions:)

  You should always call LoadAllExtensions before using this unit.
  This unit may use various OpenGL extensions and check OpenGL version.
  If you initialize OpenGL context using our GLWindow unit or
  TKamOpenGLControl then this will be done for you automatically during
  GL context initialization.
}

unit VRMLGLRenderer;

{ When you define USE_VRML_NODES_TRIANGULATION, an alternative
  rendering method will be used. Each node will be triangulated
  using TVRMLGeometryNode.LocalTriangulate and then this triangle
  will be passed to OpenGL.

  This is a proof-of-concept implementation that shows that
  using TVRMLGeometryNode.LocalTriangulate we can render all
  nodes in the same manner --- no need to write separate rendering
  routines for various TVRMLGeometryNode descendants.
  All you have to do is to implement triangulating.

  This is mainly for testing purposes, it allows you to test
  LocalTriangulate and allows you to render nodes that don't have
  specialized rendering procedure done yet. It has a couple of
  practical disadvantages:
  1) It's slower, and always will be, than dedicated rendering
     procedures for each node.
  2) Things that are not expressed as triangles
     (IndexedLineSet, PointSet) will not be rendered at all.
  3) It lacks some features, because the triangulating routines
     do not return enough information. For example, textures
     are not applied (texture coords are not generated),
     flat shading is always used (because each triangle has
     always the same normal vector).
     This disadvantage could be removed (by extending information
     that triangulate method returns for each node),
     but it will always be non-optimal anyway --- see point 1) above.
}
{ $define USE_VRML_NODES_TRIANGULATION}

{$ifdef USE_VRML_NODES_TRIANGULATION}
  {$ifdef RELEASE}
    {$fatal Undefine USE_VRML_NODES_TRIANGULATION
      for VRMLGLRenderer ---
      you don't want to use this in RELEASE version. }
  {$endif}
{$endif}

{$I kambiconf.inc}

interface

uses
  Classes, SysUtils, KambiUtils, VectorMath, GL, GLU, GLExt,
  VRMLFields, VRMLNodes, VRMLLexer, Boxes3D, OpenGLTTFonts, Images,
  KambiGLUtils, VRMLGLLightSet, TTFontsTypes,
  VRMLErrors, GLShaders, GLImages, Videos, VRMLTime, VRMLShape,
  GLCubeMap, TextureImages, KambiClassUtils, DDS, Base3D, FGL,
  GeometryArrays;

{$define read_interface}

const
  { }
  DefaultBumpMappingLightAmbientColor: TVector4Single = (0, 0, 0, 1);
  DefaultBumpMappingLightDiffuseColor: TVector4Single = (1, 1, 1, 1);

type
  TBeforeGLVertexProc = procedure (Node: TVRMLGeometryNode;
    const Vert: TVector3Single) of object;

  TRadianceTransferFunction = function (Node: TVRMLGeometryNode;
    RadianceTransfer: PVector3Single;
    const RadianceTransferCount: Cardinal): TVector3Single of object;

  { Callback used by TVRMLRenderingAttributes.OnVertexColorFunction.
    Passed here VertexPosition is in local coordinates (that is,
    local of this object, multiply by State.Transform to get scene coords).
    VertexIndex is the direct index to Node.Coordinates. }
  TVertexColorFunction = procedure (var Color: TVector3Single;
    Shape: TVRMLShape; const VertexPosition: TVector3Single;
    VertexIndex: Integer) of object;

  { Various bump mapping methods. Generally sorted from worst one
    (bmNone, which does no bump mapping) to the best.
    Which one is chosen is determined at runtime, based on OpenGL capabilities,
    and on TVRMLRenderingAttributes.BumpMappingMaximum. }
  TBumpMappingMethod = (
    { No bump mapping done. }
    bmNone,

    { Normal (calculate light by "dot" per pixel) bump mapping using GLSL
      shader. This requires OpenGL that supports GLSL.

      @orderedList(
        @item(Light position in tangent space is calculated by shader program.
          In short, this means that you can use good renderer optimization
          even if you change BumpMappingLightPosition every frame. )

        @item(Honours material ambient and diffuse properties
          like it should.)

        @item(Honours properties like BumpMappingLightAmbientColor,
          BumpMappingLightDiffuseColor,
          so you can control the light more like normal OpenGL light.)
      ) }
    bmGLSLNormal,

    { This is like bmGLSLNormal, but additionally (if the heightMap of the surface
      is available) this will do parallax mapping.
      Steep parallax mapping with self-shadowing,
      if supported by hardware, otherwise classic parallax mapping
      (with offset limiting, i.e. with E.z component removed).

      Parallax mapping, in short, means that the texture coordinate is perturbed,
      based on texture heightMap topology and camera direction, to create
      illusion of 3D shape instead of flat texture.
      This makes e.g. the bricks on the texture really
      visible as "standing out", in 3D, from the wall. And self-shadowing
      means that these bricks even cast appropriate shadows on each other. }
    bmGLSLParallax);

const
  DefaultBumpMappingMaximum = bmNone;
  DefaultFirstGLFreeLight = 1;
  DefaultVarianceShadowMaps = false;

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
    FSmoothShading: boolean;
    FLighting: boolean;
    FUseSceneLights: boolean;
    FFirstGLFreeLight: Cardinal;
    FLastGLFreeLight: integer;
    FControlMaterials: boolean;
    FControlTextures: boolean;
    FEnableTextures: boolean;
    FFirstGLFreeTexture: Cardinal;
    FLastGLFreeTexture: integer;
    FTextureMinFilter: TGLint;
    FTextureMagFilter: TGLint;
    FPointSize: TGLFloat;
    FUseFog: boolean;
    FBumpMappingMaximum: TBumpMappingMethod;
    FGLSLShaders: boolean;
    FPureGeometry: boolean;
    FTextureModeGrayscale: TGLenum;
    FTextureModeRGB: TGLenum;
    FVarianceShadowMaps: boolean;
  protected
    { These methods just set the value on given property,
      eventually calling BeforeChange.

      In descendants you can do something more here, like automatic
      calling UnprepareAll of related TVRMLGLRenderer
      (this is not done here, as this would be dangerous ---
      caller must be aware that TVRMLGLRenderer was unprepared,
      and must prepare it again, otherwise rendering will fail).
      @groupBegin }
    procedure SetOnRadianceTransfer(const Value: TRadianceTransferFunction); virtual;
    procedure SetOnVertexColor(const Value: TVertexColorFunction); virtual;
    procedure SetSmoothShading(const Value: boolean); virtual;
    procedure SetLighting(const Value: boolean); virtual;
    procedure SetUseSceneLights(const Value: boolean); virtual;
    procedure SetFirstGLFreeLight(const Value: Cardinal); virtual;
    procedure SetLastGLFreeLight(const Value: integer); virtual;
    procedure SetControlMaterials(const Value: boolean); virtual;
    procedure SetControlTextures(const Value: boolean); virtual;
    procedure SetEnableTextures(const Value: boolean); virtual;
    procedure SetFirstGLFreeTexture(const Value: Cardinal); virtual;
    procedure SetLastGLFreeTexture(const Value: integer); virtual;
    procedure SetTextureMinFilter(const Value: TGLint); virtual;
    procedure SetTextureMagFilter(const Value: TGLint); virtual;
    procedure SetPointSize(const Value: TGLFloat); virtual;
    procedure SetUseFog(const Value: boolean); virtual;
    procedure SetBumpMappingMaximum(const Value: TBumpMappingMethod); virtual;
    procedure SetGLSLShaders(const Value: boolean); virtual;
    procedure SetPureGeometry(const Value: boolean); virtual;
    procedure SetTextureModeGrayscale(const Value: TGLenum); virtual;
    procedure SetTextureModeRGB(const Value: TGLenum); virtual;
    procedure SetVarianceShadowMaps(const Value: boolean); virtual;
    { @groupEnd }

    { Called always before a rendering attribute (that is, any property
      of this class that has an effect on rendering) changes. }
    procedure BeforeChange; virtual;
  public
    constructor Create; virtual;

    procedure Assign(Source: TPersistent); override;

    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$else} virtual; {$endif}

    { Calculate vertex color from radiance transfer.
      If this is assigned, and geometry object has radianceTransfer
      field (see [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_radiance_transfer])
      then this is used to calculate the color of each vertex.

      Note that this is evaluated when object is rendered.
      If your object has dynamic lighting, you want to use roNone optimization,
      otherwise colors returned by this are saved on display list and
      never change. }
    property OnRadianceTransfer: TRadianceTransferFunction
      read FOnRadianceTransfer write SetOnRadianceTransfer;

    { Calculate vertex color for given vertex by a callback.
      If this is assigned, then this is used to calculate
      the color of each vertex.

      Note that this is evaluated when object is rendered.
      If this changes dynamically (for example, it calculates some dynamic
      lighting), you want to use roNone optimization,
      otherwise colors returned by this are saved on display list and
      never change. }
    property OnVertexColor: TVertexColorFunction
      read FOnVertexColor write SetOnVertexColor;

    { Use smooth (Gouraud) shading.
      This controls whether we set OpenGL glShadeModel GL_SMOOTH or GL_FLAT.
      Also, this affects the normal vectors generated during rendering
      (always per-face for flat shading; passing smooth normals and alllowing
      OpenGL to just choose one normal per face would not be 100% correct). }
    property SmoothShading: boolean
      read FSmoothShading write SetSmoothShading default true;

    { Should we will enable OpenGL lighting when rendering.
      This is @true by default, since it's wanted almost always.

      When Lighting is @false, we do not enable OpenGL lighting,
      but you can still manually enable OpenGL lighting yourself.
      Regardless of this, we always pass to OpenGL information about
      materials and colors, so our rendering looks as good as possible
      both with and without OpenGL lighting. }
    property Lighting: boolean
      read FLighting write SetLighting default true;

    { Should we setup VRML/X3D lights as OpenGL lights during rendering.

      VRML/X3D lights are loaded into OpenGL lights using the range of
      FirstGLFreeLight ... LastGLFreeLight. LastGLFreeLight = -1 means
      "the last possible OpenGL light", that is glGet(GL_MAX_LIGHT)-1.
      Note that by default we treat all lights except the 0th (typically
      useful for making headlight) as "free to use" for VRML lights.

      This is independent from the @link(Lighting) property (which merely
      says whether we will turn OpenGL lighting on at all).

      You can always use your own OpenGL lights to light our 3D model
      (we always render 3D geometry normals and materials, regardless
      of @link(Lighting) and @link(UseSceneLights) values.)
      You can use your own lights instead of scene lights (set UseSceneLights
      to @false), or in addition to scene lights (leave UseSceneLights
      as @true and adjust FirstGLFreeLight / LastGLFreeLight).

      @groupBegin }
    property UseSceneLights: boolean
      read FUseSceneLights write SetUseSceneLights default true;
    property FirstGLFreeLight: Cardinal
      read FFirstGLFreeLight write SetFirstGLFreeLight default DefaultFirstGLFreeLight;
    property LastGLFreeLight: integer
      read FLastGLFreeLight write SetLastGLFreeLight default -1;
    { @groupEnd }

    { Should we take care of applying appropriate
      materials and colors on your model.

      For special purposes, you can set this to @false.
      Then you are expected to set glColor/glMaterial yourself. }
    property ControlMaterials: boolean
      read FControlMaterials write SetControlMaterials default true;

    { Should we take care of everything related
      to texturing your model. Textures will be automatically activated
      (for multitexturing), enabled/disabled, bound, and texture coordinates
      will be used/generated, according to your VRML model data
      (and EnableTextures attribute).

      For special purposes, you can set this to @false.
      Then our engine assumes no control over the
      enabled/disabled state of OpenGL texturing and the currently bound texture.
      Texture coordinates will still be generated, if applicable.
      This is useful when your model specifies texture coordinates but
      still you want to control from your program which (if any) texture is
      currently bound and enabled. }
    property ControlTextures: boolean
      read FControlTextures write SetControlTextures default true;

    { If ControlTextures is @true, then this property determines
      whether we should actually take model textures into account.
      In other words:

      @unorderedList(
        @item(When ControlTextures = EnableTextures = @true (default),
          then our engine takes care of everything related to texturing
          for you: enabling and using textures for textured parts of the model,
          disabling textures for non-textured parts.)

        @item(When ControlTextures = @true but EnableTextures = @false,
          you force the engine to ignore textures in your model.
          The whole scene will be rendered with glDisable(GL_TEXTURE_*),
          texture coordinates will not be generated etc.
          This is for special purposes.)

        @item(When ControlTextures = @false, value of EnableTextures
          doesn't matter. See ControlTextures for description.)
      ) }
    property EnableTextures: boolean
      read FEnableTextures write SetEnableTextures default true;

    { Which OpenGL texture units are free to use.

      Note that for now we assume that at least one texture unit is free.
      If OpenGL multitexturing is not available, we will just use the default
      texture unit.

      LastGLFreeTexture = -1 means that up to the end, all texture units
      are available. In other words, -1 is equivalent to
      glGetInteger(GL_MAX_TEXTURE_UNITS_ARB) - 1.

      @groupBegin }
    property FirstGLFreeTexture: Cardinal
      read FFirstGLFreeTexture write SetFirstGLFreeTexture default 0;
    property LastGLFreeTexture: Integer
      read FLastGLFreeTexture write SetLastGLFreeTexture default -1;
    { @groupEnd }

    { Default minification and magnification filters for textures.
      These can be overridden on a per-texture basis in VRML / X3D files
      by X3D TextureProperties node (see X3D specification).

      @groupBegin }
    property TextureMinFilter: TGLint
      read FTextureMinFilter write SetTextureMinFilter default GL_LINEAR_MIPMAP_LINEAR;
    property TextureMagFilter: TGLint
      read FTextureMagFilter write SetTextureMagFilter default GL_LINEAR;
    { @groupEnd }

    { How large OpenGL points should be.
      This has currently effect only on VRML/X3D PointSet rendering. }
    property PointSize: TGLFloat
      read FPointSize write SetPointSize default 3.0;

    { Should we control fog, rendering fog following VRML/X3D defined fog.
      If @true then we will enable/disable and set all the properties
      of OpenGL fog as necessary.

      If @false, we don't touch fog settings. You can control it yourself,
      or just leave it disabled (OpenGL defaults). }
    property UseFog: boolean
      read FUseFog write SetUseFog default true;

    { Enable bump mapping. This sets maximum allowed bump mapping method
      (actual method used may be lower, depending on OpenGL capabilities
      and information provided in VRML model, like normalMap and heightMap).
      Set to bmNone (default) to disable using bump mapping.
      Set to High(TBumpMappingMethod) to enable best implemented bump mapping.

      To actually use this, it requires also
      some OpenGL capabilities (some extensions present, and enough texture
      units available). And naturally it comes to use only if
      VRML model will specify normalMap field for some shapes nodes.
      For parallax mapping, heightMap is also needed.

      You have to update Renderer.BumpMappingLightPosition if enable bump
      mappping (that is, you set BumpMappingMaximum to something <> bmNone),
      to actually specify how bumps should appear.
      See TVRMLGLRenderer.BumpMappingLightPosition, or
      TVRMLGLScene.BumpMappingLightPosition for more comfortable version.
      See also other TVRMLGLRenderer.BumpMappingLightXxx properties,
      like TVRMLGLRenderer.BumpMappingLightDiffuseColor.

      TODO: For each texture, there must always be the same
      normalMap and heightMap
      (since we store it in the same TTextureImageReference).
    }
    property BumpMappingMaximum: TBumpMappingMethod
      read FBumpMappingMaximum write SetBumpMappingMaximum
      default bmNone;

    { Use GLSL shaders defined in the VRML/X3D model.

      When this is @false, the renderer does not control GLSL shaders
      (it does not set any GLSL program active etc.). Which means that
      the caller is free to apply any shader for the whole rendered scene. }
    property GLSLShaders: boolean read FGLSLShaders write SetGLSLShaders
      default true;

    { Use this to render pure geometry, without any colors, materials,
      lights, etc. If this is @true, only pure geometry will be rendered.
      This means that the rendering of the model will be equivalent to
      calling only @code(glBegin(...)), then @code(glVertex(...)),
      then @code(glEnd). Actually, some additional calls may be done
      (to push/pop/multiply current modelview matrix, and to realize
      drawing of vertexes by vertex arrays).
      But the point is that no OpenGL state, besides the absolute minimum to render
      polygons at their correct places, will be touched.
      Oh, and backface culling will be correctly enabled (glCullFace mode,
      GL_CULL_FACE flag) as appropriate.

      For example, Renderer will not set any color (no glColor calls),
      will not set any material
      (no glMaterial calls), will not set any texture coordinates and
      will not bind any texture, will not enable any depth testing, fog and
      everything else.

      This is only useful for some special OpenGL tricks. For example if you
      want, for whatever reason, to set glColor and/or glMaterial
      and/or texturing by yourself, for the whole model.
      A practical example of use is to render plane-projected shadows,
      see kambi_vrml_game_engine/examples/vrml/plane_projected_shadow_demo.lpr.
      In this program, we must be able to render any VRML model with pure
      black color, possibly (when using stenciling) withuout even depth
      testing. }
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

  { Cached VRML/X3D shape display list.
    Note that Attributes and State are owned by this record
    (TVRMLGLRendererContextCache will make sure about creating/destroying
    them), but GeometryNode and FogNode are a references somewhere to the scene
    (they will be supplied to TVRMLGLRendererContextCache instance)
    and we don't own them. }
  TShapeCache = record
    Attributes: TVRMLRenderingAttributes;
    GeometryNode: TVRMLGeometryNode;
    State: TVRMLGraphTraverseState;
    FogNode: TNodeFog;
    FogDistanceScaling: Single;

    GLList: TGLuint;
    References: Cardinal;
  end;
  PShapeCache = ^TShapeCache;

  TDynArrayItem_3 = TShapeCache;
  PDynArrayItem_3 = PShapeCache;
  {$define DYNARRAY_3_IS_STRUCT}
  {$I dynarray_3.inc}
  TDynShapeCacheArray = class(TDynArray_3)
  end;

  TRenderBeginEndCache = record
    Attributes: TVRMLRenderingAttributes;
    FogNode: TNodeFog;
    FogDistanceScaling: Single;

    GLList: TGLuint;
    References: Cardinal;
  end;
  PRenderBeginEndCache = ^TRenderBeginEndCache;

  TDynArrayItem_4 = TRenderBeginEndCache;
  PDynArrayItem_4 = PRenderBeginEndCache;
  {$define DYNARRAY_4_IS_STRUCT}
  {$I dynarray_4.inc}
  TDynRenderBeginEndCacheArray = class(TDynArray_4)
  end;

  TVRMLGLRenderer = class;

  { GLSL program integrated with VRML renderer. Adds ability to bind
    VRML textures to uniform variables of GLSL shader. }
  TVRMLGLSLProgram = class(TGLSLProgram)
  private
    Renderer: TVRMLGLRenderer;
    Node: TNodeComposedShader;
  public
    function SetupUniforms(var BoundTextureUnits: Cardinal): boolean; override;
  end;

  TGLSLProgramCache = record
    ProgramNode: TNodeComposedShader;
    { GLSL program for this shader. Always non-nil. }
    GLSLProgram: TVRMLGLSLProgram;
    References: Cardinal;
  end;
  PGLSLProgramCache = ^TGLSLProgramCache;

  TDynArrayItem_5 = TGLSLProgramCache;
  PDynArrayItem_5 = PGLSLProgramCache;
  {$define DYNARRAY_5_IS_STRUCT}
  {$I dynarray_5.inc}
  TDynGLSLProgramCacheArray = class(TDynArray_5)
  end;

  { A cache that may be used by many TVRMLGLRenderer
    instances to share some common OpenGL resources.

    For examples, texture names and OpenGL display lists
    for fonts. Such things can usually be shared by all
    TVRMLGLRenderer instances used within the same OpenGL context.
    And this may save a lot of memory if you use many TVRMLGLRenderer
    instances in your program.

    Instance of this class is tied to particular OpenGL context if and only if
    there are some TVRMLGLRenderer instances using this cache and
    tied to that OpenGL context. }
  TVRMLGLRendererContextCache = class(TTexturesImagesVideosCache)
  private
    Fonts: array[TVRMLFontFamily, boolean, boolean] of TGLOutlineFontCache;
    TextureImageCaches: TDynTextureImageCacheArray;
    TextureVideoCaches: TDynTextureVideoCacheArray;
    TextureCubeMapCaches: TDynTextureCubeMapCacheArray;
    Texture3DCaches: TDynTexture3DCacheArray;
    TextureDepthOrFloatCaches: TDynTextureDepthOrFloatCacheArray;
    ShapeCaches: TDynShapeCacheArray;
    RenderBeginCaches: TDynRenderBeginEndCacheArray;
    RenderEndCaches: TDynRenderBeginEndCacheArray;
    GLSLProgramCaches: TDynGLSLProgramCacheArray;

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
      const Width, Height: Cardinal): TGLuint;

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

    { Set GLSLProgram uniform variable from VRML field value.
      Uniform name is contained in UniformName. UniformValue indicates
      uniform type and new value (UniformValue.Name is not used).

      This ignores SFNode / MFNode fields (these will be set each
      time when enabling this shader). }
    procedure SetUniformFromField(
      GLSLProgram: TGLSLProgram; UniformName: string;
      UniformValue: TVRMLField);

    procedure EventReceiveGLSLUniform(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);

    { Creates and links appropriate TGLSLProgram.
      Takes care of sending ComposedShader.isValid event (but not isSelected).
      @raises EGLSLError In case program cannot be linked. }
    function GLSLProgram_IncReference_Core(
      ProgramNode: TNodeComposedShader;
      AAttributes: TVRMLRenderingAttributes): TVRMLGLSLProgram;
    { Creates and links appropriate TGLSLProgram.
      Takes care of sending ComposedShader.isSelected, isValid event.
      Returns nil (and does VRMLWarning) if program cannot be linked. }
    function GLSLProgram_IncReference(
      ProgramNode: TNodeComposedShader;
      AAttributes: TVRMLRenderingAttributes): TVRMLGLSLProgram;
    procedure GLSLProgram_DecReference(const GLSLProgram: TVRMLGLSLProgram);
  public
    constructor Create;
    destructor Destroy; override;

    function Fonts_IncReference(
      fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean;
      TTF_Font: PTrueTypeFont): TGLOutlineFont;

    procedure Fonts_DecReference(
      fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean);

    { Display lists caches.
      These will be used by TVRMLGLScene.

      Note that we have two versions of Shape_IncReference,
      because if the list will already exist in the cache then we don't want to
      waste time on creating and immediately freeing unnecessary list.
      you should call Shape_IncReference_Existing, and if @false
      then you should build display list and call
      Shape_IncReference_New. }

    { }
    function Shape_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      CacheIgnoresTransform: boolean;
      out AGLList: TGLuint): boolean;

    procedure Shape_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AGeometryNode: TVRMLGeometryNode;
      AState: TVRMLGraphTraverseState;
      AFogNode: TNodeFog;
      AGLList: TGLuint);

    procedure Shape_DecReference(
      const GLList: TGLuint);

    function RenderBegin_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AFogNode: TNodeFog;
      out AGLList: TGLuint): boolean;

    procedure RenderBegin_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AFogNode: TNodeFog;
      AGLList: TGLuint);

    procedure RenderBegin_DecReference(
      const GLList: TGLuint);

    function RenderEnd_IncReference_Existing(
      AAttributes: TVRMLRenderingAttributes;
      AFogNode: TNodeFog;
      out AGLList: TGLuint): boolean;

    procedure RenderEnd_IncReference_New(
      AAttributes: TVRMLRenderingAttributes;
      AFogNode: TNodeFog;
      AGLList: TGLuint);

    procedure RenderEnd_DecReference(
      const GLList: TGLuint);
  end;

  {$I resourcerenderer.inc}
  {$I vrmltexturerenderer.inc}
  {$I vrmlbumpmappingrenderer.inc}
  {$I vrmlglslrenderer.inc}

  { VRML shape that can be rendered. }
  TVRMLRendererShape = class(TVRMLShape)
  private
    FArrays: TGeometryArrays;
  public
    destructor Destroy; override;

    { Arrays to render this shape. Managed by TVRMLGLRenderer and TVRMLGLScene. }
    property Arrays: TGeometryArrays read FArrays write FArrays;
    procedure FreeArrays;
  end;

  TVRMLGLRenderer = class
  private
    { ---------------------------------------------------------
      GLContext-specific things, so freed (or reset in some other way to default
        uninitialized values) in UnprepareAll. }

    { FLastGLFreeLight = -1 if not calculated.
      GLContext - specific, so it is reset to -1 in UnprepareAll.
      Use always LastGLFreeLight, not FLastGLFreeLight or Attributes.LastGLFreeLight
      to get LastGLFreeLight. LastGLFreeLight function will not ever return -1
      and will minimize amount of calls to glGetInteger() }
    FLastGLFreeLight: integer;

    { Use always LastGLFreeTexture, this will never return -1.
      Will return Attributes.LastGLFreeTexture, or
      glGetInteger(GL_MAX_TEXTURE_UNITS_ARB) -1 if -1.

      To minimize number of glGetInteger calls, the result of this is cached
      in FLastGLFreeTexture. }
    FLastGLFreeTexture: Integer;
    function LastGLFreeTexture: Cardinal;

    { Number of available texture units.
      Just a shortcut for LastGLFreeTexture - FirstGLFreeTexture + 1,
      always >= 0. }
    function FreeGLTexturesCount: Cardinal;
  private
    BumpMappingMethodCached: TBumpMappingMethod;
    BumpMappingMethodIsCached: boolean;

    { Will be created by some prepare, if BumpMappingMethod <> bmNone
      and it's detected that bump mapping will be actually used.

      Boolean index indicates whether it's the version with parallax
      mapping. }
    BmGLSLProgram: array[boolean] of TGLSLProgram;

    { Only if BumpMappingMethod  <> bmNone and BmGLSLProgram[true] is prepared
      (GLSL program for bump mapping with parallax mapping)
      this will indicate whether we prepared version with or without
      "steep" parallax mapping.

      Note that I didn't add another TBumpMappingMethod value, like
      bmGLSLSteepParallax, because availability of steep parallax mapping
      is checked only when it's program is actually compiled.
      Failure to compile causes us to fallback to normal parallax, without
      steep improvements. }
    BmSteepParallaxMapping: boolean;

    GLTextureNodes: TGLTextureNodes;
    BumpMappingRenderers: TBumpMappingRenderersList;
    GLSLRenderers: TGLSLRenderersList;

    { To which fonts we made a reference in the cache ? }
    FontsReferences: array[TVRMLFontFamily, boolean, boolean] of boolean;

    { ------------------------------------------------------------
      Things usable only during Render. }

    { Our mesh renderer. Actually of TVRMLMeshRenderer class, but I really
      don't want to expose TVRMLMeshRenderer class in the interface. }
    ExposedMeshRenderer: TObject;

    { During Render, values of current Shape, Shape.State, Shape.Geometry
      and such. }
    CurrentShape: TVRMLShape;
    CurrentState: TVRMLGraphTraverseState;
    CurrentGeometry: TVRMLGeometryNode;
    UsedGLSL: TGLSLRenderer;

    { Is bump mapping allowed by the current shape.
      Fully calculated only after InitMeshRenderer, as determining GeneratorClass
      is needed to set this. }
    ShapeBumpMappingAllowed: boolean;
    { Is bump mapping used, and what method is used, for current shape.
      This is determined by ShapeBumpMappingAllowed,
      global BumpMappingMethod, and by the texture information for current
      shape (whether user provided normal map, height map etc.) }
    ShapeBumpMappingUsed: TBumpMappingMethod;

    { Variables internal for MeterialsBegin/End, BindMaterial }
    MaterialLit: boolean;
    Material1BoundNumber: integer;
    MaterialTemporaryDisabledFog: boolean;
    MaterialOpacity: Single;

    procedure Render_MaterialsBegin;
    procedure Render_MaterialsEnd;
    procedure Render_BindMaterial_1(MatNum: integer);
    procedure Render_MaterialsBegin_2(Material: TNodeMaterial_2);
    procedure Render_Material(
      const AmbientColor, DiffuseColor, SpecularColor,
        EmissiveColor: TVector3Single;
      const UnLitColor: TVector3Single;
      const ShininessExp, Opacity: Single);
  private
    { For how many texture units does Render have to generate tex coords?

      This is the number of texture units used.
      Always <= 1 if OpenGL doesn't support multitexturing
      (GLUseMultiTexturing = @false).
      It's also already clamped by the number of available texture units
      (determined from First/LastGLFreeTexture).

      Always = 1 if bump mapping is used (our multitexturing setup is
      special then, we will actually use more texture units for special
      purposes). }
    TexCoordsNeeded: Cardinal;

    { Set by RenderShapeBegin, used by RenderShapeEnd. Tells for which
      texture units we pushed and modified the texture matrix.
      Always <= 1 if not GLUseMultiTexturing. }
    TextureTransformUnitsUsed: Cardinal;

    { Set by RenderShapeBegin, used by RenderShapeEnd. Tells how many
      clip planes were enabled (and so, how many must be disabled
      at the end). }
    ClipPlanesEnabled: Cardinal;

    { Additional texture units used,
      in addition to 0..TextureTransformUnitsUsed - 1.
      Cleared by RenderShapeBegin, added by PushTextureUnit,
      used by RenderShapeEnd. }
    TextureTransformUnitsUsedMore: TDynLongIntArray;

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
  private
    { ----------------------------------------------------------------- }

    { Inited in RenderBegin, according to our FogNode.
      If not UseFog then it's always false. }
    FogVolumetric: boolean;
    FogEnabled: boolean;
    FogVolumetricDirection: TVector3Single;
    FogVolumetricVisibilityStart: Single;

    FAttributes: TVRMLRenderingAttributes;

    FCache: TVRMLGLRendererContextCache;
    OwnsCache: boolean;

    { Initialize VRML/X3D fog, based on fog node.
      Looks at Attributes.UseFog.

      If ActuallyApply = @false then we only calculate the "out" parameters
      (Enabled and such), not actually setting up the fog parameters
      for OpenGL. }
    procedure InitializeFog(Node: TNodeFog;
      const ActuallyApply: boolean;
      out Enabled, Volumetric: boolean;
      out VolumetricDirection: TVector3Single;
      out VolumetricVisibilityStart: Single);

    {$ifdef USE_VRML_NODES_TRIANGULATION}
    procedure DrawTriangle(const Tri: TTriangle3Single;
      Shape: TObject; State: TVRMLGraphTraverseState;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
    {$endif}

    { If ARB_multitexturing available, this sets currently active texture unit.
      TextureUnit is newly active unit, this is added to GL_TEXTURE0_ARB
      + FirstGLFreeTexture.

      So the only thing that you have to care about is to specify TextureUnit <
      FreeGLTexturesCount.
      Everything else (ARB_multitexturing, GL_TEXTURE0_ARB,
      FirstGLFreeTexture values) is taken care of inside here. }
    procedure ActiveTexture(const TextureUnit: Cardinal);

    { If ARB_multitexturing available, sets texture coordinate for texture
      unit TextureUnit (by appropriate glMultiTexCoord).
      Otherwise (when no multitexturing), sets texture coordinate for
      the only texture unit (glTexCoord).

      The only thing that you have to care about is to specify TextureUnit <
      FreeGLTexturesCount. Everything else (whether ARB_multitexturing
      exists, and shifting TextureUnit by GL_TEXTURE0_ARB +
      FirstGLFreeTexture values) is taken care of inside here. }
    procedure MultiTexCoord(const TextureUnit: Cardinal; const TexCoord: TVector4f);
  private
    FBumpMappingLightPosition: TVector3Single;
    procedure SetBumpMappingLightPosition(const Value: TVector3Single);
  private
    FBumpMappingLightAmbientColor: TVector4Single;
    procedure SetBumpMappingLightAmbientColor(const Value: TVector4Single);
  private
    FBumpMappingLightDiffuseColor: TVector4Single;
    procedure SetBumpMappingLightDiffuseColor(const Value: TVector4Single);
  public
    { Constructor.

      Passing nil as Cache will cause the private cache instance
      to be created and used for this TVRMLGLRenderer.
      I.e. no cache will be shared between different TVRMLGLRenderer
      instances. Otherwise you can pass here your Cache. Of course
      it has to remain created for the whole lifetime while
      this TVRMLGLRenderer is created. }
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

    { Release resources for this node. }
    procedure Unprepare(Node: TVRMLNode);

    { Release every OpenGL and VRML resource. That is release any knowledge
      connecting us to the current OpenGL context and any knowledge
      about your prepared VRML nodes, states etc.

      Calling UnprepareAll is valid (and ignored) call if everything
      is already released.

      Destructor callls UnprepareAll automatically. So be sure to either
      call UnprepareAll or destroy this renderer
      when your OpenGL context is still active. }
    procedure UnprepareAll;

    procedure RenderBegin(FogNode: TNodeFog);
    procedure RenderEnd;

    procedure RenderShapeLights(
      LightsRenderer: TVRMLGLLightsCachingRenderer;
      State: TVRMLGraphTraverseState);
    procedure RenderShapeBegin(Shape: TVRMLRendererShape);
    procedure RenderShapeInside(Shape: TVRMLRendererShape);
    procedure RenderShapeEnd(Shape: TVRMLRendererShape);

    procedure RenderShape(Shape: TVRMLRendererShape);

    { Check Attributes (mainly Attributes.BumpMappingMaximum) and OpenGL
      context capabilities to see which bump mapping method (if any)
      should be used.

      More precisely: this checks Attributes.BumpMappingMaximum,
      Attributes.ControlTextures, Attributes.EnableTextures.
      Then checks are appropriate OpenGL capabilities
      present (GL_ARB_multitexture and friends, GLSL for better methods).
      Then checks are enough texture units available (using First/LastGLFreeTexture).

      This method is mainly for debugging purposes, as this class handles everything
      related to bump mapping inside. This function may be usable for you
      only to display to user this property. Note that calling this
      ties us to current OpenGL context (just like any PrepareXxx or RenderXxx
      call). }
    function BumpMappingMethod: TBumpMappingMethod;

    { How we would support bump mapping in current OpenGL context, with given
      Attributes values.

      The contract is that if you @italic(create TVRMLGLRenderer in current
      OpenGL context) and @italic(set it's Attributes just like parameters to
      this method) then @italic(created TVRMLGLRenderer will
      have BumpMappingMethod the same as what this function tells).

      This is helpful if you don't have TVRMLGLRenderer and it's
      attributes instances created yet, but you want to know right now
      what bump mapping will be available. }
    class function GLContextBumpMappingMethod(
      const FirstGLFreeTexture: Cardinal;
      ALastGLFreeTexture: Integer;
      const AttributesBumpMappingMaximum: TBumpMappingMethod;
      const AttributesControlTextures, AttributesEnableTextures, AttributesPureGeometry: boolean):
      TBumpMappingMethod;

    { Sets light position used for bump mapping.
      This is meaningful if BumpMappingMethod <> bmNone.

      If the bump mapping shader is already prepared, then setting this property
      simply sets the uniform value of this shader. And light direction
      in tangent space is calculated by the shader. So you can simply reuse
      your display lists. (If the bump mapping shader is not prepared yet,
      then value set here will be used at preparation... so things work without
      problems in any case.) }
    property BumpMappingLightPosition: TVector3Single
      read FBumpMappingLightPosition write SetBumpMappingLightPosition;

    { Ambient color of the light calculated when doing bump mapping.

      When doing bump mapping, we don't use VRML lights. Instead some
      properties of the light are controlled by BumpMappingLightPosition
      (or TVRMLGLScene.BumpMappingLightPosition) and attributes like
      this one.

      Default value is DefaultBumpMappingLightAmbientColor.

      4th component of the color has the same meaning and use as 4th color
      component for OpenGL lights. Usually, just set this to 1.0. }
    property BumpMappingLightAmbientColor: TVector4Single
      read FBumpMappingLightAmbientColor
      write SetBumpMappingLightAmbientColor;

    { Diffuse color of the light calculated when doing bump mapping.
      See also comments at BumpMappingLightAmbientColor.

      Default value is DefaultBumpMappingLightDiffuseColor. }
    property BumpMappingLightDiffuseColor: TVector4Single
      read FBumpMappingLightDiffuseColor
      write SetBumpMappingLightDiffuseColor;

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

    { Last available OpenGL light number.

      This is never -1, in contrast to Attributes.LastGLFreeLight.
      In other words, if Attributes.LastGLFreeLight, then this method
      will actually make appropriate glGetInteger call to get number of
      available lights in this OpenGL context. }
    function LastGLFreeLight: integer;

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

    { Should CacheIgnoresTransform be passed to
      Shape_IncReference_Existing. }
    function CacheIgnoresTransform(Node: TNodeFog): boolean;

    { Load the ScreenEffect node GLSL shader.
      Will use Node.StateForShaderPrepare.
      Will make sure that Node.ShaderLoaded is true.
      If necessary (when Node.ShaderLoaded changes from false to true),
      it may set Node.Shader if some GLSL program was successfully loaded.

      The GLSL program (TGLSLProgram), along with it's ComposedShader node reference,
      will be stored here (on GLSLRenderers list). So they will be automatically
      unprepared during UnprepareAll call. }
    procedure PrepareScreenEffect(Node: TNodeScreenEffect);
  end;

  EVRMLGLRendererror = class(EVRMLError);

const
  BumpMappingMethodNames: array [TBumpMappingMethod] of string =
  ( 'None',
    'Dot (by GLSL)',
    'Dot with steep parallax (by GLSL)' );

{ Compare two saved fog parameters.
  For the 1st fog node, we supply a scaling separately,
  to account for the fact that scaling may change while fog reference
  remains the same.
  For the 2nd fog node, transform scale is taken from
  current FogNode2.TransformScale. }
function FogParametersEqual(
  FogNode1: TNodeFog; const FogDistanceScaling1: Single;
  FogNode2: TNodeFog): boolean;

{$undef read_interface}

implementation

uses Math, KambiStringUtils, GLVersionUnit, KambiLog,
  RenderStateUnit, VRMLCameraUtils, RaysWindow, VRMLShadowMaps,
  VRMLArraysGenerator;

{$define read_implementation}
{$I dynarray_2.inc}
{$I dynarray_3.inc}
{$I dynarray_4.inc}
{$I dynarray_5.inc}
{$I dynarray_7.inc}
{$I dynarray_9.inc}
{$I dynarray_11.inc}
{$I dynarray_13.inc}

{$I vrmlmeshrenderer.inc}
{$I vrmlmeshrenderer_x3d_text.inc}

{$I resourcerenderer.inc}
{$I vrmltexturerenderer.inc}
{$I vrmlbumpmappingrenderer.inc}
{$I vrmlglslrenderer.inc}

{ TVRMLGLRendererContextCache -------------------------------------------- }

{ $define DEBUG_VRML_RENDERER_CACHE}

constructor TVRMLGLRendererContextCache.Create;
begin
  inherited;
  TextureImageCaches := TDynTextureImageCacheArray.Create;
  TextureVideoCaches := TDynTextureVideoCacheArray.Create;
  TextureCubeMapCaches := TDynTextureCubeMapCacheArray.Create;
  Texture3DCaches := TDynTexture3DCacheArray.Create;
  TextureDepthOrFloatCaches := TDynTextureDepthOrFloatCacheArray.Create;
  ShapeCaches := TDynShapeCacheArray.Create;
  RenderBeginCaches := TDynRenderBeginEndCacheArray.Create;
  RenderEndCaches := TDynRenderBeginEndCacheArray.Create;
  GLSLProgramCaches := TDynGLSLProgramCacheArray.Create;
end;

destructor TVRMLGLRendererContextCache.Destroy;

{ Tests:
  procedure Assert(const B: boolean; const S: string = '');
  begin
    if not B then
      Writeln(ErrOutput, 'GLRendererContextCache warning: ' + S);
  end;
}

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

  if RenderBeginCaches <> nil then
  begin
    Assert(RenderBeginCaches.Count = 0, 'Some references to RenderBegins still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(RenderBeginCaches);
  end;

  if RenderEndCaches <> nil then
  begin
    Assert(RenderEndCaches.Count = 0, 'Some references to RenderEnds still exist' +
      ' when freeing TVRMLGLRendererContextCache');
    FreeAndNil(RenderEndCaches);
  end;

  if GLSLProgramCaches <> nil then
  begin
    Assert(GLSLProgramCaches.Count = 0, 'Some references to GLSLProgram' +
      '  still exist when freeing TVRMLGLRendererContextCache');
    FreeAndNil(GLSLProgramCaches);
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
  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : Font : ', Fonts[fsfam, fsbold, fsitalic].References);
  {$endif}
end;

procedure TVRMLGLRendererContextCache.Fonts_DecReference(
  fsfam: TVRMLFontFamily; fsbold: boolean; fsitalic: boolean);
begin
  Dec(Fonts[fsfam, fsbold, fsitalic].References);
  if Fonts[fsfam, fsbold, fsitalic].References = 0 then
    FreeAndNil(Fonts[fsfam, fsbold, fsitalic].Instance);
  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('-- : Font : ', Fonts[fsfam, fsbold, fsitalic].References);
  {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ', TextureFullUrl, ' : ', TextureCached^.References);
      {$endif}
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

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : ', TextureFullUrl, ' : ', 1);
  {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : ', TextureImageCaches.Items[I].FullUrl, ' : ',
                       TextureImageCaches.Items[I].References);
      {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : ', TextureFullUrl, ' : ', TextureCached^.References);
      {$endif}
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

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : ', TextureFullUrl, ' : ', 1);
  {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : ', TextureVideoCaches.Items[I].FullUrl, ' : ',
                       TextureVideoCaches.Items[I].References);
      {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : cube map ', PointerToStr(Node), ' : ', TextureCached^.References);
      {$endif}
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

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : cube map ', PointerToStr(Node), ' : ', 1);
  {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : cube map ', PointerToStr(TextureCubeMapCaches.Items[I].InitialNode), ' : ', TextureCubeMapCaches.Items[I].References);
      {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : 3d texture ', PointerToStr(Node), ' : ', TextureCached^.References);
      {$endif}
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

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : 3d texture ', PointerToStr(Node), ' : ', 1);
  {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : 3d texture ', PointerToStr(Texture3DCaches.Items[I].InitialNode), ' : ', Texture3DCaches.Items[I].References);
      {$endif}
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
  const Width, Height: Cardinal): TGLuint;
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : Depth texture ', PointerToStr(Node), ' : ', TextureCached^.References);
      {$endif}
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
      if DepthCompareField.Value = 'NONE' then
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_NONE) else
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

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : Depth texture ', PointerToStr(Node), ' : ', 1);
  {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : Depth texture ', PointerToStr(TextureDepthOrFloatCaches.Items[I].InitialNode), ' : ', TextureDepthOrFloatCaches.Items[I].References);
      {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : Float texture ', PointerToStr(Node), ' : ', TextureCached^.References);
      {$endif}
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

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : Float texture ', PointerToStr(Node), ' : ', 1);
  {$endif}
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
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : Float texture ', PointerToStr(TextureDepthOrFloatCaches.Items[I].InitialNode), ' : ', TextureDepthOrFloatCaches.Items[I].References);
      {$endif}
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

procedure TVRMLGLRendererContextCache.SetUniformFromField(
  GLSLProgram: TGLSLProgram; UniformName: string;
  UniformValue: TVRMLField);
var
  TempF: TDynSingleArray;
  TempVec2f: TDynVector2SingleArray;
  TempVec3f: TDynVector3SingleArray;
  TempVec4f: TDynVector4SingleArray;
  TempMat3f: TDynMatrix3SingleArray;
  TempMat4f: TDynMatrix4SingleArray;
begin
  { program must be active to set uniform values. }
  GLSLProgram.Enable;

  if UniformValue is TSFBool then
    GLSLProgram.SetUniform(UniformName, TSFBool(UniformValue).Value) else
  if UniformValue is TSFLong then
    { Handling of SFLong also takes care of SFInt32. }
    GLSLProgram.SetUniform(UniformName, TSFLong(UniformValue).Value) else
  if UniformValue is TSFVec2f then
    GLSLProgram.SetUniform(UniformName, TSFVec2f(UniformValue).Value) else
  { Check TSFColor first, otherwise TSFVec3f would also catch and handle
    TSFColor. And we don't want this: for GLSL, color is passed
    as vec4 (so says the spec, I guess that the reason is that for GLSL most
    input/output colors are vec4). }
  if UniformValue is TSFColor then
    GLSLProgram.SetUniform(UniformName, Vector4Single(TSFColor(UniformValue).Value, 1.0)) else
  if UniformValue is TSFVec3f then
    GLSLProgram.SetUniform(UniformName, TSFVec3f(UniformValue).Value) else
  if UniformValue is TSFVec4f then
    GLSLProgram.SetUniform(UniformName, TSFVec4f(UniformValue).Value) else
  if UniformValue is TSFRotation then
    GLSLProgram.SetUniform(UniformName, TSFRotation(UniformValue).Value) else
  if UniformValue is TSFMatrix3f then
    GLSLProgram.SetUniform(UniformName, TSFMatrix3f(UniformValue).Value) else
  if UniformValue is TSFMatrix4f then
    GLSLProgram.SetUniform(UniformName, TSFMatrix4f(UniformValue).Value) else
  if UniformValue is TSFFloat then
    GLSLProgram.SetUniform(UniformName, TSFFloat(UniformValue).Value) else
  if UniformValue is TSFDouble then
    { SFDouble also takes care of SFTime }
    GLSLProgram.SetUniform(UniformName, TSFDouble(UniformValue).Value) else

  { Double-precision vector and matrix types.

    Note that X3D spec specifies only mapping for SF/MFVec3d, 4d
    (not specifying any mapping for SF/MFVec2d, and all matrix types).
    And it specifies that they map to types float3, float4 ---
    which are not valid types in GLSL?

    So I simply ignore non-sensible specification, and take
    the reasonable approach: support all double-precision vectors and matrices,
    just like single-precision. }
  if UniformValue is TSFVec2d then
    GLSLProgram.SetUniform(UniformName, Vector2Single(TSFVec2d(UniformValue).Value)) else
  if UniformValue is TSFVec3d then
    GLSLProgram.SetUniform(UniformName, Vector3Single(TSFVec3d(UniformValue).Value)) else
  if UniformValue is TSFVec4d then
    GLSLProgram.SetUniform(UniformName, Vector4Single(TSFVec4d(UniformValue).Value)) else
  if UniformValue is TSFMatrix3d then
    GLSLProgram.SetUniform(UniformName, Matrix3Single(TSFMatrix3d(UniformValue).Value)) else
  if UniformValue is TSFMatrix4d then
    GLSLProgram.SetUniform(UniformName, Matrix4Single(TSFMatrix4d(UniformValue).Value)) else

  { Now repeat this for array types }
  if UniformValue is TMFBool then
    GLSLProgram.SetUniform(UniformName, TMFBool(UniformValue).Items) else
  if UniformValue is TMFLong then
    GLSLProgram.SetUniform(UniformName, TMFLong(UniformValue).Items) else
  if UniformValue is TMFVec2f then
    GLSLProgram.SetUniform(UniformName, TMFVec2f(UniformValue).Items) else
  if UniformValue is TMFColor then
  begin
    TempVec4f := TMFColor(UniformValue).Items.ToVector4Single(1.0);
    try
      GLSLProgram.SetUniform(UniformName, TempVec4f);
    finally FreeAndNil(TempVec4f) end;
  end else
  if UniformValue is TMFVec3f then
    GLSLProgram.SetUniform(UniformName, TMFVec3f(UniformValue).Items) else
  if UniformValue is TMFVec4f then
    GLSLProgram.SetUniform(UniformName, TMFVec4f(UniformValue).Items) else
  if UniformValue is TMFRotation then
    GLSLProgram.SetUniform(UniformName, TMFRotation(UniformValue).Items) else
  if UniformValue is TMFMatrix3f then
    GLSLProgram.SetUniform(UniformName, TMFMatrix3f(UniformValue).Items) else
  if UniformValue is TMFMatrix4f then
    GLSLProgram.SetUniform(UniformName, TMFMatrix4f(UniformValue).Items) else
  if UniformValue is TMFFloat then
    GLSLProgram.SetUniform(UniformName, TMFFloat(UniformValue).Items) else
  if UniformValue is TMFDouble then
  begin
    TempF := TMFDouble(UniformValue).Items.ToSingle;
    try
      GLSLProgram.SetUniform(UniformName, TempF);
    finally FreeAndNil(TempF) end;
  end else
  if UniformValue is TMFVec2d then
  begin
    TempVec2f := TMFVec2d(UniformValue).Items.ToVector2Single;
    try
      GLSLProgram.SetUniform(UniformName, TempVec2f);
    finally FreeAndNil(TempVec2f) end;
  end else
  if UniformValue is TMFVec3d then
  begin
    TempVec3f := TMFVec3d(UniformValue).Items.ToVector3Single;
    try
      GLSLProgram.SetUniform(UniformName, TempVec3f);
    finally FreeAndNil(TempVec3f) end;
  end else
  if UniformValue is TMFVec4d then
  begin
    TempVec4f := TMFVec4d(UniformValue).Items.ToVector4Single;
    try
      GLSLProgram.SetUniform(UniformName, TempVec4f);
    finally FreeAndNil(TempVec4f) end;
  end else
  if UniformValue is TMFMatrix3d then
  begin
    TempMat3f := TMFMatrix3d(UniformValue).Items.ToMatrix3Single;
    try
      GLSLProgram.SetUniform(UniformName, TempMat3f);
    finally FreeAndNil(TempMat3f) end;
  end else
  if UniformValue is TMFMatrix4d then
  begin
    TempMat4f := TMFMatrix4d(UniformValue).Items.ToMatrix4Single;
    try
      GLSLProgram.SetUniform(UniformName, TempMat4f);
    finally FreeAndNil(TempMat4f) end;
  end else
  if (UniformValue is TSFNode) or
     (UniformValue is TMFNode) then
  begin
    { Nothing to do, these will be set by TGLSLRenderer.Enable }
  end else
    { TODO: other field types, full list is in X3D spec in
      "OpenGL shading language (GLSL) binding".
      Remaining:
      SF/MFImage }
    VRMLWarning(vwSerious, 'Setting uniform GLSL variable from X3D field type "' + UniformValue.VRMLTypeName + '" not supported');

  { TODO: this should restore previously bound program }
  GLSLProgram.Disable;
end;

procedure TVRMLGLRendererContextCache.EventReceiveGLSLUniform(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  I: Integer;
  UniformName: string;
  GLSLProgramCache: PGLSLProgramCache;
  EventsEngine: TVRMLEventsEngine;
begin
  if Event.ParentExposedField = nil then
    UniformName := Event.Name else
    UniformName := Event.ParentExposedField.Name;

  { We need to find GLSLProgram instance, to know which GLSL program
    actually has this uniform variable. We can do it: Event.ParentNode
    should point to appropriate ComposedShader node, so we can find
    corresponding GLSLProgramCaches item, and this will contain
    needed GLSLProgram. }

  for I := 0 to GLSLProgramCaches.High do
  begin
    GLSLProgramCache := GLSLProgramCaches.Pointers[I];

    if GLSLProgramCache^.ProgramNode = Event.ParentNode then
    begin
      SetUniformFromField(GLSLProgramCache^.GLSLProgram, UniformName, Value);

      { Although ExposedEvents implementation already sends notification
        about changes to EventsEngine, we can also get here
        by eventIn invocation (which doesn't trigger
        EventsEngine.ChangedField, since it doesn't change a field...).
        So we should explicitly do VisibleChangeHere here, to make sure
        it gets called when uniform changed. }

      EventsEngine := GLSLProgramCache^.ProgramNode.EventsEngine;
      if EventsEngine <> nil then
        EventsEngine.VisibleChangeHere([vcVisibleGeometry, vcVisibleNonGeometry]);

      Exit;
    end;
  end;

  VRMLWarning(vwSerious, Format(
    'INTERNAL ERROR, we can continue but please report a bug: uniform variable "%s" should be set from event now, but it turns out that GLSL program for this event''s ComposedShader node is not in the cache',
    [Event.Name]));
end;

function TVRMLGLRendererContextCache.GLSLProgram_IncReference_Core(
  ProgramNode: TNodeComposedShader;
  AAttributes: TVRMLRenderingAttributes): TVRMLGLSLProgram;

  procedure LoadGLSLProgram(GLSLProgram: TGLSLProgram;
    ProgramNode: TNodeComposedShader);
  var
    I: Integer;
    Part: TNodeShaderPart;
    PartType, Source: String;
    HasAnyShader: boolean;
    IDecls: TVRMLInterfaceDeclarationsList;
    UniformField: TVRMLField;
    UniformEvent: TVRMLEvent;
  begin
    HasAnyShader := false;

    { Iterate over ProgramNode.FdParts, looking for vertex shaders
      and fragment shaders. Note that more than one vertex/fragment shader
      is OK (as long as each has only one main() entry, OpenGL will check
      this when linking program). }

    for I := 0 to ProgramNode.FdParts.Count - 1 do
      if ProgramNode.FdParts.Items[I] is TNodeShaderPart then
      begin
        Part := TNodeShaderPart(ProgramNode.FdParts.Items[I]);

        if Part is TNodeShaderPartShadowMap then
          TNodeShaderPartShadowMap(Part).VarianceShadowMapsEnabled :=
            TGLGeneratedShadowMap.ClassVarianceShadowMaps(AAttributes);

        PartType := UpperCase(Part.FdType.Value);
        if PartType <> Part.FdType.Value then
          VRMLWarning(vwSerious, Format('ShaderPart.type should be uppercase, but is not: "%s"', [
            Part.FdType.Value]));

        if PartType = 'VERTEX' then
        begin
          Source := Part.LoadContents;
          if Part.UsedFullUrl <> '' then
          begin
            GLSLProgram.AttachVertexShader(Source);
            HasAnyShader := true;
          end;
        end else

        if PartType = 'FRAGMENT' then
        begin
          Source := Part.LoadContents;
          if Part.UsedFullUrl <> '' then
          begin
            GLSLProgram.AttachFragmentShader(Source);
            HasAnyShader := true;
          end;
        end else

          VRMLWarning(vwSerious, Format('Unknown type for ShaderPart: "%s"',
            [PartType]));
      end;

    if not HasAnyShader then
      raise EGLSLError.Create('No vertex and no fragment shader for GLSL program');

    GLSLProgram.Link(true);

    { X3D spec "OpenGL shading language (GLSL) binding" says
      "If the name is not available as a uniform variable in the
      provided shader source, the values of the node shall be ignored"
      (although it says when talking about "Vertex attributes",
      seems they mixed attributes and uniforms meaning in spec?).

      So we do not allow EGLSLUniformNotFound to be raised.
      GLSLProgram.SetUniform will go to DataWarning always (ignored by default).

      Also type errors, when variable exists in shader but has different type,
      will be send to DataWarning. }
    GLSLProgram.UniformNotFoundAction := uaWarning;
    GLSLProgram.UniformTypeMismatchAction := utWarning;

    IDecls := ProgramNode.InterfaceDeclarations;

    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;
      UniformEvent := IDecls.Items[I].Event;

      { Set initial value for this GLSL uniform variable,
        from VRML field or exposedField }

      if UniformField <> nil then
      begin
        { Ok, we have a field with a value (interface declarations with
          fields inside ComposedShader always have a value).
          So set GLSL uniform variable from this field. }
        SetUniformFromField(GLSLProgram, UniformField.Name, UniformField);
      end;

      { Allow future changing of this GLSL uniform variable,
        from VRML eventIn or exposedField }
      if (UniformField <> nil) and UniformField.Exposed then
        UniformField.ExposedEvents[false].OnReceive.Add(@EventReceiveGLSLUniform) else
      if (UniformEvent <> nil) and UniformEvent.InEvent then
        UniformEvent.OnReceive.Add(@EventReceiveGLSLUniform);
    end;
  end;

var
  I: Integer;
  GLSLProgramCache: PGLSLProgramCache;
begin
  for I := 0 to GLSLProgramCaches.High do
  begin
    GLSLProgramCache := GLSLProgramCaches.Pointers[I];

    if GLSLProgramCache^.ProgramNode = ProgramNode then
    begin
      Inc(GLSLProgramCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : GLSL program ' + ProgramNode.DescribeUsedUrls + ' : ',
        GLSLProgramCache^.References);
      {$endif}
      Exit(GLSLProgramCache^.GLSLProgram);
    end;
  end;

  { Initialize Result first, before calling GLSLProgramCaches.Add.
    That's because in case of loading problems,
    we don't want to add program to cache (because caller would have
    no way to call GLSLProgram_DecReference later). }

  Result := TVRMLGLSLProgram.Create;
  try
    LoadGLSLProgram(Result, ProgramNode);
    ProgramNode.EventIsValid.Send(true);
  except
    { In case of problems with initializing GLSL program, free the program
      and reraise exception. Outer GLSLProgram_IncReference
      will take care of converting it to VRMLWarning. }
    FreeAndNil(Result);
    ProgramNode.EventIsValid.Send(false);
    raise;
  end;

  GLSLProgramCache := GLSLProgramCaches.Add;
  GLSLProgramCache^.ProgramNode := ProgramNode;
  GLSLProgramCache^.References := 1;
  GLSLProgramCache^.GLSLProgram := Result;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : GLSL program ' + ProgramNode.DescribeUsedUrls + ' : ', 1);
  {$endif}
end;

function TVRMLGLRendererContextCache.GLSLProgram_IncReference(
  ProgramNode: TNodeComposedShader;
  AAttributes: TVRMLRenderingAttributes): TVRMLGLSLProgram;
begin
  try
    Result := GLSLProgram_IncReference_Core(ProgramNode, AAttributes);
    ProgramNode.EventIsSelected.Send(true);
  except
    { EGLSLError catches errors from Cache.GLSLProgram_IncReference_Core,
      including GLShaders errors like
      EGLSLShaderCompileError or EGLSLProgramLinkError }
    on E: EGLSLError do
    begin
      VRMLWarning(vwSerious, 'Error when initializing GLSL shader : ' + E.Message);
      Result := nil;
      ProgramNode.EventIsSelected.Send(false);
    end;
  end;
end;

procedure TVRMLGLRendererContextCache.GLSLProgram_DecReference(
  const GLSLProgram: TVRMLGLSLProgram);
var
  I, J: Integer;
  GLSLProgramCache: PGLSLProgramCache;
  IDecls: TVRMLInterfaceDeclarationsList;
  UniformField: TVRMLField;
  UniformEvent: TVRMLEvent;
begin
  for I := 0 to GLSLProgramCaches.High do
  begin
    GLSLProgramCache := GLSLProgramCaches.Pointers[I];
    if GLSLProgramCache^.GLSLProgram = GLSLProgram then
    begin
      Dec(GLSLProgramCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : GLSLProgram ' + GLSLProgramCache^.ProgramNode.DescribeUsedUrls + ' : ',
        GLSLProgramCache^.References);
      {$endif}
      if GLSLProgramCache^.References = 0 then
      begin
        { Remove EventReceiveGLSLUniform callback,
          reverting the work done in GLSLProgram_IncReference. }
        IDecls := GLSLProgramCache^.ProgramNode.InterfaceDeclarations;
        for J := 0 to IDecls.Count - 1 do
        begin
          UniformField := IDecls.Items[J].Field;
          UniformEvent := IDecls.Items[J].Event;

          if (UniformField <> nil) and UniformField.Exposed then
            UniformField.ExposedEvents[false].OnReceive.Remove(@EventReceiveGLSLUniform) else
          if (UniformEvent <> nil) and UniformEvent.InEvent then
            UniformEvent.OnReceive.Remove(@EventReceiveGLSLUniform);
        end;

        FreeAndNil(GLSLProgramCache^.GLSLProgram);
        GLSLProgramCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.Create(
    'TVRMLGLRendererContextCache.GLSLProgram_DecReference: no reference ' +
    'found to GLSL program');
end;

function TVRMLGLRendererContextCache.Shape_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  CacheIgnoresTransform: boolean;
  out AGLList: TGLuint): boolean;

  { Compares two VRML/X3D states by
      State1.EqualsNoTransform(State2)
    or
      State1.Equals(State2)
    Which one is used, depends on whether two shapes with different
    transformation can be considered equal. This depends on whether
    we have volumetric fog, based on global coords. }
  function StatesEqual(State1, State2: TVRMLGraphTraverseState): boolean;
  begin
    if CacheIgnoresTransform then
      Result := State1.EqualsNoTransform(State2) else
      Result := State1.Equals(State2);
  end;

var
  I: Integer;
  SSCache: PShapeCache;
begin
  { Force CacheIgnoresTransform to be false if our shape uses shaders.
    Shaders may depend on coordinates in eye space, which obviously
    may be different for shapes that differ even only on transform. }
  if CacheIgnoresTransform and
     (AState.ShapeNode <> nil) and
     (AState.ShapeNode.Appearance <> nil) and
     (AState.ShapeNode.Appearance.FdShaders.Count <> 0) then
    CacheIgnoresTransform := false;

  for I := 0 to ShapeCaches.High do
  begin
    SSCache := ShapeCaches.Pointers[I];
    if (SSCache^.Attributes.Equals(AAttributes)) and
       (SSCache^.GeometryNode = AGeometryNode) and
       StatesEqual(SSCache^.State, AState) and
       FogParametersEqual(
         SSCache^.FogNode, SSCache^.FogDistanceScaling, AFogNode) then
    begin
      Inc(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : Shape ', SSCache^.GLList, ' : ',
        SSCache^.References);
      {$endif}
      AGLList := SSCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLGLRendererContextCache.Shape_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AGeometryNode: TVRMLGeometryNode;
  AState: TVRMLGraphTraverseState;
  AFogNode: TNodeFog;
  AGLList: TGLuint);
var
  SSCache: PShapeCache;
begin
  SSCache := ShapeCaches.Add;
  SSCache^.Attributes := AAttributes;
  SSCache^.GeometryNode := AGeometryNode;
  SSCache^.State := AState;
  SSCache^.FogNode := AFogNode;
  if AFogNode <> nil then
    SSCache^.FogDistanceScaling := AFogNode.TransformScale else
    SSCache^.FogDistanceScaling := 0;
  SSCache^.GLList := AGLList;
  SSCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : Shape ', SSCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLGLRendererContextCache.Shape_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  SSCache: PShapeCache;
begin
  for I := 0 to ShapeCaches.High do
  begin
    SSCache := ShapeCaches.Pointers[I];
    if SSCache^.GLList = GLList then
    begin
      Dec(SSCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : Shape ', SSCache^.GLList, ' : ',
        SSCache^.References);
      {$endif}
      if SSCache^.References = 0 then
      begin
        FreeAndNil(SSCache^.Attributes);
        FreeAndNil(SSCache^.State);
        glFreeDisplayList(SSCache^.GLList);
        ShapeCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.Shape_DecReference: ' +
    'no reference ' +
    'found for display list %d', [GLList]);
end;

function TVRMLGLRendererContextCache.RenderBegin_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AFogNode: TNodeFog;
  out AGLList: TGLuint): boolean;
var
  I: Integer;
  RenderCache: PRenderBeginEndCache;
begin
  for I := 0 to RenderBeginCaches.High do
  begin
    RenderCache := RenderBeginCaches.Pointers[I];
    if (RenderCache^.Attributes.Equals(AAttributes)) and
      FogParametersEqual(
        RenderCache^.FogNode, RenderCache^.FogDistanceScaling, AFogNode) then
    begin
      Inc(RenderCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : RenderBegin ', RenderCache^.GLList, ' : ', RenderCache^.References);
      {$endif}
      AGLList := RenderCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLGLRendererContextCache.RenderBegin_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AFogNode: TNodeFog;
  AGLList: TGLuint);
var
  RenderCache: PRenderBeginEndCache;
begin
  RenderCache := RenderBeginCaches.Add;
  RenderCache^.Attributes := AAttributes;
  RenderCache^.FogNode := AFogNode;
  if AFogNode <> nil then
    RenderCache^.FogDistanceScaling := AFogNode.TransformScale else
    RenderCache^.FogDistanceScaling := 0;
  RenderCache^.GLList := AGLList;
  RenderCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : RenderBegin ', RenderCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLGLRendererContextCache.RenderBegin_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  RenderCache: PRenderBeginEndCache;
begin
  for I := 0 to RenderBeginCaches.High do
  begin
    RenderCache := RenderBeginCaches.Pointers[I];
    if RenderCache^.GLList = GLList then
    begin
      Dec(RenderCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : RenderBegin ', RenderCache^.GLList, ' : ', RenderCache^.References);
      {$endif}
      if RenderCache^.References = 0 then
      begin
        FreeAndNil(RenderCache^.Attributes);
        glFreeDisplayList(RenderCache^.GLList);
        RenderBeginCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.RenderBegin_DecReference: no reference ' +
    'found for display list %d', [GLList]);
end;

function TVRMLGLRendererContextCache.RenderEnd_IncReference_Existing(
  AAttributes: TVRMLRenderingAttributes;
  AFogNode: TNodeFog;
  out AGLList: TGLuint): boolean;
var
  I: Integer;
  RenderCache: PRenderBeginEndCache;
begin
  for I := 0 to RenderEndCaches.High do
  begin
    RenderCache := RenderEndCaches.Pointers[I];
    if (RenderCache^.Attributes.Equals(AAttributes)) and
      FogParametersEqual(
        RenderCache^.FogNode, RenderCache^.FogDistanceScaling, AFogNode) then
    begin
      Inc(RenderCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('++ : RenderEnd ', RenderCache^.GLList, ' : ', RenderCache^.References);
      {$endif}
      AGLList := RenderCache^.GLList;
      Exit(true);
    end;
  end;

  Exit(false);
end;

procedure TVRMLGLRendererContextCache.RenderEnd_IncReference_New(
  AAttributes: TVRMLRenderingAttributes;
  AFogNode: TNodeFog;
  AGLList: TGLuint);
var
  RenderCache: PRenderBeginEndCache;
begin
  RenderCache := RenderEndCaches.Add;
  RenderCache^.Attributes := AAttributes;
  RenderCache^.FogNode := AFogNode;
  if AFogNode <> nil then
    RenderCache^.FogDistanceScaling := AFogNode.TransformScale else
    RenderCache^.FogDistanceScaling := 0;
  RenderCache^.GLList := AGLList;
  RenderCache^.References := 1;

  {$ifdef DEBUG_VRML_RENDERER_CACHE}
  Writeln('++ : RenderEnd ', RenderCache^.GLList, ' : ', 1);
  {$endif}
end;

procedure TVRMLGLRendererContextCache.RenderEnd_DecReference(
  const GLList: TGLuint);
var
  I: Integer;
  RenderCache: PRenderBeginEndCache;
begin
  for I := 0 to RenderEndCaches.High do
  begin
    RenderCache := RenderEndCaches.Pointers[I];
    if RenderCache^.GLList = GLList then
    begin
      Dec(RenderCache^.References);
      {$ifdef DEBUG_VRML_RENDERER_CACHE}
      Writeln('-- : RenderEnd ', RenderCache^.GLList, ' : ', RenderCache^.References);
      {$endif}
      if RenderCache^.References = 0 then
      begin
        FreeAndNil(RenderCache^.Attributes);
        glFreeDisplayList(RenderCache^.GLList);
        RenderEndCaches.Delete(I, 1);
      end;
      Exit;
    end;
  end;

  raise EInternalError.CreateFmt(
    'TVRMLGLRendererContextCache.RenderEnd_DecReference: no reference ' +
    'found for display list %d', [GLList]);
end;

{ TVRMLRenderingAttributes --------------------------------------------------- }

procedure TVRMLRenderingAttributes.Assign(Source: TPersistent);
begin
  if Source is TVRMLRenderingAttributes then
  begin
    OnRadianceTransfer := TVRMLRenderingAttributes(Source).OnRadianceTransfer;
    OnVertexColor := TVRMLRenderingAttributes(Source).OnVertexColor;
    SmoothShading := TVRMLRenderingAttributes(Source).SmoothShading;
    Lighting := TVRMLRenderingAttributes(Source).Lighting;
    UseSceneLights := TVRMLRenderingAttributes(Source).UseSceneLights;
    FirstGLFreeLight := TVRMLRenderingAttributes(Source).FirstGLFreeLight;
    LastGLFreeLight := TVRMLRenderingAttributes(Source).LastGLFreeLight;
    ControlMaterials := TVRMLRenderingAttributes(Source).ControlMaterials;
    ControlTextures := TVRMLRenderingAttributes(Source).ControlTextures;
    EnableTextures := TVRMLRenderingAttributes(Source).EnableTextures;
    FirstGLFreeTexture := TVRMLRenderingAttributes(Source).FirstGLFreeTexture;
    LastGLFreeTexture := TVRMLRenderingAttributes(Source).LastGLFreeTexture;
    TextureMinFilter := TVRMLRenderingAttributes(Source).TextureMinFilter;
    TextureMagFilter := TVRMLRenderingAttributes(Source).TextureMagFilter;
    PointSize := TVRMLRenderingAttributes(Source).PointSize;
    UseFog := TVRMLRenderingAttributes(Source).UseFog;
  end else
    inherited;
end;

function TVRMLRenderingAttributes.Equals(SecondValue: TObject): boolean;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TVRMLRenderingAttributes) and
    (TVRMLRenderingAttributes(SecondValue).OnRadianceTransfer = OnRadianceTransfer) and
    (TVRMLRenderingAttributes(SecondValue).OnVertexColor = OnVertexColor) and
    (TVRMLRenderingAttributes(SecondValue).SmoothShading = SmoothShading) and
    (TVRMLRenderingAttributes(SecondValue).Lighting = Lighting) and
    (TVRMLRenderingAttributes(SecondValue).UseSceneLights = UseSceneLights) and
    (TVRMLRenderingAttributes(SecondValue).FirstGLFreeLight = FirstGLFreeLight) and
    (TVRMLRenderingAttributes(SecondValue).LastGLFreeLight = LastGLFreeLight) and
    (TVRMLRenderingAttributes(SecondValue).ControlMaterials = ControlMaterials) and
    (TVRMLRenderingAttributes(SecondValue).ControlTextures = ControlTextures) and
    (TVRMLRenderingAttributes(SecondValue).EnableTextures = EnableTextures) and
    (TVRMLRenderingAttributes(SecondValue).FirstGLFreeTexture = FirstGLFreeTexture) and
    (TVRMLRenderingAttributes(SecondValue).LastGLFreeTexture = LastGLFreeTexture) and
    (TVRMLRenderingAttributes(SecondValue).TextureMinFilter = TextureMinFilter) and
    (TVRMLRenderingAttributes(SecondValue).TextureMagFilter = TextureMagFilter) and
    (TVRMLRenderingAttributes(SecondValue).PointSize = PointSize) and
    (TVRMLRenderingAttributes(SecondValue).UseFog = UseFog);
end;

constructor TVRMLRenderingAttributes.Create;
begin
  inherited;

  FSmoothShading := true;
  FLighting := true;
  FUseSceneLights := true;
  FFirstGLFreeLight := DefaultFirstGLFreeLight;
  FLastGLFreeLight := -1;
  FFirstGLFreeTexture := 0;
  FLastGLFreeTexture := -1;
  FControlMaterials := true;
  FControlTextures := true;
  FEnableTextures := true;
  FTextureMinFilter := GL_LINEAR_MIPMAP_LINEAR;
  FTextureMagFilter := GL_LINEAR;
  FPointSize := 3.0;
  FUseFog := true;
  FBumpMappingMaximum := bmNone;
  FGLSLShaders := true;
  FTextureModeGrayscale := GL_MODULATE;
  FTextureModeRGB := GL_MODULATE;
  FVarianceShadowMaps := DefaultVarianceShadowMaps;
end;

procedure TVRMLRenderingAttributes.BeforeChange;
begin
  { Nothing to do in this class. }
end;

procedure TVRMLRenderingAttributes.SetOnRadianceTransfer(
  const Value: TRadianceTransferFunction);
begin
  if OnRadianceTransfer <> Value then
  begin
    BeforeChange;
    FOnRadianceTransfer := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetOnVertexColor(
  const Value: TVertexColorFunction);
begin
  if OnVertexColor <> Value then
  begin
    BeforeChange;
    FOnVertexColor := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetSmoothShading(const Value: boolean);
begin
  if SmoothShading <> Value then
  begin
    BeforeChange;
    FSmoothShading := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetLighting(const Value: boolean);
begin
  if Lighting <> Value then
  begin
    BeforeChange;
    FLighting := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetUseSceneLights(const Value: boolean);
begin
  if UseSceneLights <> Value then
  begin
    BeforeChange;
    FUseSceneLights := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetFirstGLFreeLight(const Value: Cardinal);
begin
  if FirstGLFreeLight <> Value then
  begin
    BeforeChange;
    FFirstGLFreeLight := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetLastGLFreeLight(const Value: integer);
begin
  if LastGLFreeLight <> Value then
  begin
    BeforeChange;
    FLastGLFreeLight := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetControlMaterials(const Value: boolean);
begin
  if ControlMaterials <> Value then
  begin
    BeforeChange;
    FControlMaterials := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetControlTextures(const Value: boolean);
begin
  if ControlTextures <> Value then
  begin
    BeforeChange;
    FControlTextures := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetEnableTextures(const Value: boolean);
begin
  if EnableTextures <> Value then
  begin
    BeforeChange;
    FEnableTextures := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetFirstGLFreeTexture(const Value: Cardinal);
begin
  if FirstGLFreeTexture <> Value then
  begin
    BeforeChange;
    FFirstGLFreeTexture := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetLastGLFreeTexture(const Value: integer);
begin
  if LastGLFreeTexture <> Value then
  begin
    BeforeChange;
    FLastGLFreeTexture := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetTextureMinFilter(const Value: TGLint);
begin
  if TextureMinFilter <> Value then
  begin
    BeforeChange;
    FTextureMinFilter := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetTextureMagFilter(const Value: TGLint);
begin
  if TextureMagFilter <> Value then
  begin
    BeforeChange;
    FTextureMagFilter := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetPointSize(const Value: TGLFloat);
begin
  if PointSize <> Value then
  begin
    BeforeChange;
    FPointSize := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetUseFog(const Value: boolean);
begin
  if UseFog <> Value then
  begin
    BeforeChange;
    FUseFog := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetBumpMappingMaximum(
  const Value: TBumpMappingMethod);
begin
  if BumpMappingMaximum <> Value then
  begin
    BeforeChange;
    FBumpMappingMaximum := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetGLSLShaders(const Value: boolean);
begin
  if GLSLShaders <> Value then
  begin
    { TODO: this is a huge hack for VSM: changing GLSLShaders is done
      inside TVRMLGLScene.Render, to allow using our own VSM shader
      generating depth. In that case, we really do not want to trigger
      BeforeChange when changing Attributes.GLSLShaders value
      (this would cause CloseGLRenderer on all scenes, thus freeing
      VSM texture and shader resources currently used... bad idea).

      Eventually, this should be solved one day by throwing away
      display lists, using only VBO, and allowing changes to Attributes
      happen without any speed penalty (no need for any costly BeforeChange
      and such). }
    if not VarianceShadowMaps then
      BeforeChange;
    FGLSLShaders := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetPureGeometry(const Value: boolean);
begin
  if PureGeometry <> Value then
  begin
    BeforeChange;
    FPureGeometry := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetTextureModeGrayscale(const Value: TGLenum);
begin
  if TextureModeGrayscale <> Value then
  begin
    BeforeChange;
    FTextureModeGrayscale := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetTextureModeRGB(const Value: TGLenum);
begin
  if TextureModeRGB <> Value then
  begin
    BeforeChange;
    FTextureModeRGB := Value;
  end;
end;

procedure TVRMLRenderingAttributes.SetVarianceShadowMaps(const Value: boolean);
begin
  if VarianceShadowMaps <> Value then
  begin
    BeforeChange;
    FVarianceShadowMaps := Value;
  end;
end;

{ TVRMLGLRenderer ---------------------------------------------------------- }

constructor TVRMLGLRenderer.Create(
  AttributesClass: TVRMLRenderingAttributesClass;
  ACache: TVRMLGLRendererContextCache);
begin
  inherited Create;

  { This is something different than FAttributes.FLastGLFreeLight.
    See LastGLFreeLight function. }
  FLastGLFreeLight := -1;

  FLastGLFreeTexture := -1;

  FBumpMappingLightAmbientColor := DefaultBumpMappingLightAmbientColor;
  FBumpMappingLightDiffuseColor := DefaultBumpMappingLightDiffuseColor;

  { asumme that "steep" version of parallax mapping is possible }
  BmSteepParallaxMapping := true;

  FAttributes := AttributesClass.Create;

  GLTextureNodes := TGLTextureNodes.Create;
  BumpMappingRenderers := TBumpMappingRenderersList.Create;
  GLSLRenderers := TGLSLRenderersList.Create;

  TextureTransformUnitsUsedMore := TDynLongIntArray.Create;

  OwnsCache := ACache = nil;
  if OwnsCache then
    FCache := TVRMLGLRendererContextCache.Create else
    FCache := ACache;
end;

destructor TVRMLGLRenderer.Destroy;
begin
  UnprepareAll;

  FreeAndNil(TextureTransformUnitsUsedMore);
  FreeAndNil(GLTextureNodes);
  FreeAndNil(BumpMappingRenderers);
  FreeAndNil(GLSLRenderers);
  FreeAndNil(FAttributes);

  if OwnsCache then
    FreeAndNil(FCache);

  inherited;
end;

{ TVRMLRendererShape --------------------------------------------------------- }

procedure TVRMLRendererShape.FreeArrays;
begin
  FreeAndNil(FArrays);
end;

destructor TVRMLRendererShape.Destroy;
begin
  FreeArrays;
  inherited;
end;

{ Prepare/Unprepare[All] ------------------------------------------------------- }

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

  GLSLRenderers.Prepare(State, Self);
end;

procedure TVRMLGLRenderer.PrepareScreenEffect(Node: TNodeScreenEffect);
var
  ShaderProgram: TGLSLProgram;
begin
  if not Node.ShaderLoaded then
  begin
    Assert(Node.Shader = nil);
    Node.ShaderLoaded := true;
    if Node.FdEnabled.Value then
    begin
      GLSLRenderers.Prepare(Node.StateForShaderPrepare, Self,
        Node.FdShaders, ShaderProgram);
      if ShaderProgram <> nil then
      begin
        { We have to ignore invalid uniforms, as it's normal that when
          rendering screen effect we will pass some screen_* variables
          that you will not use. }
        ShaderProgram.UniformNotFoundAction := uaIgnore;
        Node.Shader := ShaderProgram;
      end;
    end;
  end;
end;

procedure TVRMLGLRenderer.Unprepare(Node: TVRMLNode);
begin
  { Note that fonts (for LastNode is TNodeFontStyle) are not unprepared
    here, since Cache.Fonts are shared by all font nodes. }

  if Node is TNodeX3DTextureNode then
    GLTextureNodes.Unprepare(Node);

  if Node is TNodeKambiAppearance then
    BumpMappingRenderers.Unprepare(Node);

  if Node is TNodeComposedShader then
    GLSLRenderers.Unprepare(Node);
end;

procedure TVRMLGLRenderer.UnprepareAll;
var
  fsfam: TVRMLFontFamily;
  fsbold , fsitalic: boolean;
begin
  FLastGLFreeLight := -1;
  FLastGLFreeTexture := -1;
  BumpMappingMethodIsCached := false;

  { asumme that "steep" version of parallax mapping is possible }
  BmSteepParallaxMapping := true;

  {niszcz fonty}
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
  GLSLRenderers.UnprepareAll;

  { unprepare BmGLSLProgram }
  FreeAndNil(BmGLSLProgram[false]);
  FreeAndNil(BmGLSLProgram[true]);
end;

function TVRMLGLRenderer.LastGLFreeLight: integer;
begin
 if FLastGLFreeLight = -1 then
 begin
  {jezeli jeszcze nie pobrane FLastGLFreeLight to pobierz je teraz:}
  if Attributes.LastGLFreeLight = -1 then
   FLastGLFreeLight := glGetInteger(GL_MAX_LIGHTS)-1 else
   FLastGLFreeLight := Attributes.LastGLFreeLight;
 end;
 result := FLastGLFreeLight;
end;

function TVRMLGLRenderer.LastGLFreeTexture: Cardinal;
begin
  if FLastGLFreeTexture = -1 then
  begin
    if Attributes.LastGLFreeTexture = -1 then
    begin
      { actually get this from OpenGL }
      if GL_ARB_multitexture then
        FLastGLFreeTexture := GLMaxTextureUnitsARB - 1 else
        FLastGLFreeTexture := 0;
    end else
      FLastGLFreeTexture := Attributes.LastGLFreeTexture;
  end;
  Result := FLastGLFreeTexture;
end;

function TVRMLGLRenderer.FreeGLTexturesCount: Cardinal;
begin
  if LastGLFreeTexture >= Attributes.FirstGLFreeTexture then
    Result := LastGLFreeTexture - Attributes.FirstGLFreeTexture + 1 else
    Result := 0;
end;

class function TVRMLGLRenderer.GLContextBumpMappingMethod(
  const FirstGLFreeTexture: Cardinal;
  ALastGLFreeTexture: Integer;
  const AttributesBumpMappingMaximum: TBumpMappingMethod;
  const AttributesControlTextures, AttributesEnableTextures, AttributesPureGeometry: boolean):
  TBumpMappingMethod;
var
  TextureUnitsAvailable: Cardinal;
begin
  if ALastGLFreeTexture = -1 then
  begin
    { When ALastGLFreeTexture = -1, we get this from OpenGL, thus somewhat
      duplicating functionality that we already implemented in
      TVRMLGLRenderer.LastGLFreeTexture method. However, this is useful:

      - When calling GLContextBumpMappingMethod internally, by BumpMappingMethod,
        TVRMLGLRenderer.LastGLFreeTexture will be passed, so it's never -1.

      - When calling GLContextBumpMappingMethod from other places, in 99%
        of the cases it's very comfortable being able to pass -1 for this. }

    if GL_ARB_multitexture then
      ALastGLFreeTexture := GLMaxTextureUnitsARB - 1 else
      ALastGLFreeTexture := 0;
  end;

  TextureUnitsAvailable := ALastGLFreeTexture - FirstGLFreeTexture + 1;

  if (AttributesBumpMappingMaximum > bmNone) and
     AttributesControlTextures and
     AttributesEnableTextures and
     (not AttributesPureGeometry) and

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

    { At least 2 texture units (original and normal map) }
    (TextureUnitsAvailable >= 2) and

    (TGLSLProgram.ClassSupport <> gsNone) then
  begin
    { parallax mapping requires one more texture, since height map must be
      passed too. }
    if TextureUnitsAvailable >= 3 then
      Result := bmGLSLParallax else
      Result := bmGLSLNormal;
  end else
    Result := bmNone;

  if Result > AttributesBumpMappingMaximum then
    Result := AttributesBumpMappingMaximum;
end;

function TVRMLGLRenderer.BumpMappingMethod: TBumpMappingMethod;
begin
  if not BumpMappingMethodIsCached then
  begin
    BumpMappingMethodCached := GLContextBumpMappingMethod(
      Attributes.FirstGLFreeTexture,
      LastGLFreeTexture,
      Attributes.BumpMappingMaximum,
      Attributes.ControlTextures,
      Attributes.EnableTextures,
      Attributes.PureGeometry);

    if Log then
      WritelnLog('Bump mapping', 'Bump mapping method detected: "' +
        BumpMappingMethodNames[BumpMappingMethodCached] + '"');

    BumpMappingMethodIsCached := true;
  end;

  Result := BumpMappingMethodCached;
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

{$define MeshRenderer := TVRMLMeshRenderer(ExposedMeshRenderer) }

{$I VRMLGLRenderer_render_materials.inc}

procedure TVRMLGLRenderer.ActiveTexture(const TextureUnit: Cardinal);
begin
  if GL_ARB_multitexture then
    glActiveTextureARB(GL_TEXTURE0_ARB +
      Attributes.FirstGLFreeTexture + TextureUnit);
end;

procedure TVRMLGLRenderer.MultiTexCoord(const TextureUnit: Cardinal;
  const TexCoord: TVector4f);
begin
  if GL_ARB_multitexture then
    glMultiTexCoordv(GL_TEXTURE0_ARB +
      Attributes.FirstGLFreeTexture + TextureUnit, TexCoord) else
    glTexCoordv(TexCoord);
end;

procedure TVRMLGLRenderer.InitializeFog(Node: TNodeFog;
  const ActuallyApply: boolean;
  out Enabled, Volumetric: boolean;
  out VolumetricDirection: TVector3Single;
  out VolumetricVisibilityStart: Single);

  { Simple wrappers over GL commands, that look at ActuallyApply. }
  procedure DoGLFogf(pname: TGLEnum; param: TGLfloat);
  begin
    if ActuallyApply then glFogf(pname, param);
  end;

  procedure DoGLFogi(pname: TGLEnum; param: TGLint);
  begin
    if ActuallyApply then glFogi(pname, param);
  end;

  procedure DoGLEnable(cap: TGLenum);
  begin
    if ActuallyApply then glEnable(cap);
  end;

  procedure DoGLDisable(cap: TGLenum);
  begin
    if ActuallyApply then glDisable(cap);
  end;

  procedure DoGLFogv(pname: TGLEnum; const V: TVector4Single);
  begin
    if ActuallyApply then glFogv(pname, V);
  end;

var
  FogType: Integer;
  VisibilityRangeScaled: Single;
const FogDensityFactor = 3.0;
begin
  Volumetric := false;
  Enabled := false;
  if not Attributes.UseFog then Exit;

  if (Node = nil) or (Node.FdVisibilityRange.Value = 0.0) then
   begin DoGLDisable(GL_FOG); Exit end;

  { calculate FogType, check if it's >= 0 }
  FogType := ArrayPosStr(Node.FdFogType.Value, ['LINEAR', 'EXPONENTIAL']);
  if FogType = -1 then
  begin
    VRMLWarning(vwSerious, 'Unknown fog type "' + Node.FdFogType.Value + '"');
    InitializeFog(Node.Alternative, ActuallyApply,
      Enabled, Volumetric,
      VolumetricDirection, VolumetricVisibilityStart);
    Exit;
  end;

  if Node.FdVolumetric.Value and (not GL_EXT_fog_coord) then
  begin
    { Earlier I tried in such cases to just do a normal fog
      that looks "similar". But it turns out to be impossible
      to automatically decide what non-volumetric fog setting (if any) will
      look similar to requested volumetric fog.
      So right now I just resort to "alternative" field. }
    InitializeFog(Node.Alternative, ActuallyApply,
      Enabled, Volumetric,
      VolumetricDirection, VolumetricVisibilityStart);
    Exit;
  end;

  Enabled := true;

  VisibilityRangeScaled := Node.FdVisibilityRange.Value * Node.TransformScale;

  Volumetric := Node.FdVolumetric.Value and GL_EXT_fog_coord;

  if Volumetric then
  begin
    VolumetricVisibilityStart :=
      Node.FdVolumetricVisibilityStart.Value * Node.TransformScale;
    VolumetricDirection := Node.FdVolumetricDirection.Value;
    DoGLFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);
  end else
  begin
    { If not Volumetric but still GL_EXT_fog_coord, we make sure
      that we're *not* using FogCoord below. }
    if GL_EXT_fog_coord then
      DoGLFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
  end;

  DoGLEnable(GL_FOG);
  DoGLFogv(GL_FOG_COLOR, Vector4Single(Node.FdColor.Value, 1.0));
  case FogType of
    0:begin
        DoGLFogi(GL_FOG_MODE, GL_LINEAR);
        DoGLFogf(GL_FOG_START, 0);
        DoGLFogf(GL_FOG_END, VisibilityRangeScaled);
      end;
    1:begin
        DoGLFogi(GL_FOG_MODE, GL_EXP);
        { patrz VRMLNotes.txt po komentarz dlaczego w ten sposob implementuje
          mgle exponential VRMLa w OpenGLu }
        DoGLFogf(GL_FOG_DENSITY, FogDensityFactor / VisibilityRangeScaled);
      end;
  end;
end;

procedure TVRMLGLRenderer.RenderBegin(FogNode: TNodeFog);

  procedure DisabeAllTextureUnits;
  var
    I: Integer;
  begin
    for I := Attributes.FirstGLFreeTexture to LastGLFreeTexture do
    begin
      ActiveTexture(I);
      TGLTextureNode.TextureEnableDisable(etOff);
    end;
  end;

var i: integer;
begin
  { Push attribs and matrices (by pushing attribs FIRST we save also current
    matrix mode).

    Note for BuggyPointSetAttrib: yes, this may cause bugs,
    as glPointSize call "leaks" out. But there's nothing we can do about it,
    we cannot use GL_POINT_BIT as it crashes Mesa (and produces "invalid
    enumerant" error in case of debug compile). }
  if not GLVersion.BuggyPointSetAttrib then
    glPushAttrib(GL_ALL_ATTRIB_BITS) else
    glPushAttrib(GL_ALL_ATTRIB_BITS and (not GL_POINT_BIT));
  glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);

  { TODO: push/pop is not fully correctly done for multitexturing now:
    - We should push/pop all texture units matrices.
      Right now, we actually push only 0th texture unit matrix.
    - Make sure that currently active texture unit is saved ?
      I'm not sure, does some glPushAttrib param saves this ?

    Push/pop texture state saves environment state of all texture units, so at least
    we got glTexEnv covered. (this says OpenGL manpage for glPushAttrib,
    and it applies to both multitexturing by ARB extension and by standard GL).
  }
  DisabeAllTextureUnits;
  ActiveTexture(0);

  {init our OpenGL state}
  glMatrixMode(GL_MODELVIEW);

  if not Attributes.PureGeometry then
  begin
    glDisable(GL_COLOR_MATERIAL);
    if Attributes.ControlTextures then
    begin
      glDisable(GL_TEXTURE_GEN_S);
      glDisable(GL_TEXTURE_GEN_T);
      glDisable(GL_TEXTURE_GEN_Q);
    end;
    glEnable(GL_NORMALIZE);
    glPointSize(Attributes.PointSize);
    glEnable(GL_DEPTH_TEST);

    if not GLVersion.BuggyLightModelTwoSide then
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE) else
    if Log then
      WritelnLog('Lighting', GLVersion.BuggyLightModelTwoSideMessage);

    { While rendering Indexed_Faces_Or_Triangles we may temporarily
      enable/disable GL_CULL_FACE and change glCullFace. We want to make
      sure from what state we start, so we set if here.
      Note that we *do not* set glFrontFace --- see comments at the beginning
      of this unit to know why. }
    glCullFace(GL_BACK);
    glDisable(GL_CULL_FACE);

    glDisable(GL_ALPHA_TEST);
    {AlphaFunc uzywane tylko dla textures i tam taka wartosc jest dobra}
    glAlphaFunc(GL_GEQUAL, 0.5);

    if Attributes.SmoothShading then
     glShadeModel(GL_SMOOTH) else
     glShadeModel(GL_FLAT);

    if Attributes.Lighting then
      glEnable(GL_LIGHTING);

    if Attributes.UseSceneLights then
      for i := Attributes.FirstGLFreeLight to LastGLFreeLight do
        glDisable(GL_LIGHT0+i);

    InitializeFog(FogNode, true,
      FogEnabled, FogVolumetric,
      FogVolumetricDirection, FogVolumetricVisibilityStart);
  end;
end;

procedure TVRMLGLRenderer.RenderEnd;
begin
  ActiveTexture(0);

  {pop matrices and attribs (popping attrib restores also saved matrix mode)}
  glPopClientAttrib;
  glPopAttrib;
end;

{$ifdef USE_VRML_NODES_TRIANGULATION}
procedure TVRMLGLRenderer.DrawTriangle(const Tri: TTriangle3Single;
  Shape: TObject; State: TVRMLGraphTraverseState;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  Render_BindMaterial_1(MatNum, false);

  glNormalv(TriangleNormal(Tri));

  glBegin(GL_TRIANGLES);
    glVertexv(Tri[0]);
    glVertexv(Tri[1]);
    glVertexv(Tri[2]);
  glEnd;
end;
{$endif USE_VRML_NODES_TRIANGULATION}

procedure TVRMLGLRenderer.RenderShapeLights(
  LightsRenderer: TVRMLGLLightsCachingRenderer;
  State: TVRMLGraphTraverseState);
begin
  { We know we're inside GL_MODELVIEW now }

  { Render lights in given State, if Attributes.UseSceneLights.

    This is done in RenderShapeLights, before RenderShapeBegin,
    in particular before loading State.Transform --- this is good,
    as the lights positions/directions are in world coordinates. }

  if Attributes.UseSceneLights then
    LightsRenderer.Render(State.CurrentActiveLights);

    { Without LightsRenderer, we would do it like this:
    glLightsFromVRML(State.CurrentActiveLights,
      Attributes.FirstGLFreeLight, LastGLFreeLight);}
end;

procedure TVRMLGLRenderer.RenderShapeBegin(Shape: TVRMLRendererShape);

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
            glEnable(GL_CLIP_PLANE0 + ClipPlanesEnabled);
          glPopMatrix;

          Inc(ClipPlanesEnabled);

          { No more clip planes possible, regardless if there are any more
            enabled clip planes on the list. }
          if ClipPlanesEnabled = GLMaxClipPlanes then Break;
        end;
      end;
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

    { We work assuming that texture matrix before RenderShapeBegin was identity.
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

            Cap by available texture units (First/LastGLFreeTexture,
            and check GLUseMultiTexturing in case OpenGL cannot support
            multitex at all). }
          TextureTransformUnitsUsed := Min(Transforms.Count, FreeGLTexturesCount);
          if not GLUseMultiTexturing then
            MinTo1st(TextureTransformUnitsUsed, 1);

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

  ClipPlanesBegin(State.ClipPlanes);

  glPushMatrix;
    glMultMatrix(State.Transform);
end;

procedure TVRMLGLRenderer.RenderShapeInside(Shape: TVRMLRendererShape);
var
  GeneratorClass: TVRMLArraysGeneratorClass;

  function NodeTextured(Node: TVRMLGeometryNode): boolean;
  begin
    Result := not (
      (Node is TNodePointSet_2) or
      (Node is TNodeIndexedLineSet_2));
  end;

  { If CurrentGeometry should be rendered using one of TVRMLMeshRenderer
    classes, then create appropriate MeshRenderer and return @true.
    Otherwise return @false and doesn't set MeshRenderer.

    Takes care of initializing MeshRenderer, so you have to call only
    MeshRenderer.Render. }
  function InitMeshRenderer: boolean;
  begin
    Result := true;

    GeneratorClass := ArraysGenerator(CurrentGeometry);

    if GeneratorClass = nil then
    begin
      if CurrentGeometry is TNodeAsciiText_1 then
        ExposedMeshRenderer := TAsciiTextRenderer.Create(Self) else
      if CurrentGeometry is TNodeText then
        ExposedMeshRenderer := TTextRenderer.Create(Self) else
      if CurrentGeometry is TNodeText3D then
        ExposedMeshRenderer := TText3DRenderer.Create(Self) else
        Result := false;
    end else
    begin
      { If we have GeneratorClass, create TCompleteCoordinateRenderer.
        We'll initialize TCompleteCoordinateRenderer.Arrays later. }
      ExposedMeshRenderer := TCompleteCoordinateRenderer.Create(Self);
      ShapeBumpMappingAllowed := GeneratorClass.BumpMappingAllowed;
    end;
  end;

var
  BumpMapping: TBumpMappingRenderer;

  { > 0 means that UsedGLSL is non-nil *and* we already bound needed texture
    units when initializing shader. Always 0 otherwise. }
  UsedGLSLTexCoordsNeeded: Cardinal;

  procedure RenderTexturesBegin;
  var
    TextureNode: TNodeX3DTextureNode;
    GLTextureNode: TGLTextureNode;
    AlphaTest: boolean;
  begin
    { set defaults for non-local variables }
    BumpMapping :=  nil;

    if Attributes.PureGeometry then
    begin
      TexCoordsNeeded := 0;
      Exit;
    end;

    if not Attributes.ControlTextures then
    begin
      { Require texture coordinates for 1st texture, but don't do anything else
        (like setting active texture, enabling/disabling it, don't even look
        at VRML texture node.) }
      TexCoordsNeeded := 1;
      Exit;
    end;

    AlphaTest := false;

    TextureNode := CurrentState.Texture;
    {$ifdef USE_VRML_NODES_TRIANGULATION}
    { We don't generate texture coords, so disable textures. }
    TextureNode := nil;
    {$endif}

    TexCoordsNeeded := 0;

    GLTextureNode := GLTextureNodes.TextureNode(TextureNode);

    if UsedGLSLTexCoordsNeeded > 0 then
    begin
      { Do not bind/enable normal textures. Just set TexCoordsNeeded,
        to generate tex coords for textures used in the shader. }
      TexCoordsNeeded := UsedGLSLTexCoordsNeeded;
    end else
    if (TextureNode <> nil) and
       Attributes.EnableTextures and
       NodeTextured(CurrentGeometry) and
       (GLTextureNode <> nil) then
    begin
      { This works also for TextureNode being TNodeMultiTexture,
        since it has smartly calculated AlphaChannelType. }
      AlphaTest := GLTextureNode.AlphaChannelType = atSimpleYesNo;

      BumpMapping := BumpMappingRenderers.Enable(CurrentState, GLTextureNode);

      if BumpMapping = nil then
      begin
        GLTextureNode.EnableAll(FreeGLTexturesCount, TexCoordsNeeded);
      end else
      begin
        { for bump mapping, always TexCoordsNeeded = 1 }
        TexCoordsNeeded := 1;
        if UsedGLSL <> nil then
          VRMLWarning(vwIgnorable, 'You use both GLSL shader (ComposedShader node) and bump mapping (normalMap, heightMap fields) on a single Shape. Note that this will (usually) not work correctly --- bump mapping has to set up special shader and multitexturing environment to work.');
      end;
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

    SetGLEnabled(GL_ALPHA_TEST, AlphaTest);

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
    if BumpMapping = nil then
    begin
      for I := 0 to TexCoordsNeeded - 1 do
      begin
        ActiveTexture(I);
        TGLTextureNode.TextureEnableDisable(etOff);
      end;
    end else
      BumpMapping.Disable;
  end;

  { Find if some shader is available and prepared for this state.
    If yes, then sets UsedGLSL to non-nil and enables this shader. }
  procedure RenderShadersBegin;

    function TextureCoordsDefined: Cardinal;
    var
      TexCoord: TVRMLNode;
    begin
      if CurrentShape.Geometry.TexCoord(CurrentState, TexCoord) and
         (TexCoord <> nil) then
      begin
        if TexCoord is TNodeMultiTextureCoordinate then
          Result := TNodeMultiTextureCoordinate(TexCoord).FdTexCoord.Count else
          Result := 1;
      end else
        Result := 0;
    end;

  var
    TCD: Cardinal;
  begin
    UsedGLSL := nil;
    UsedGLSLTexCoordsNeeded := 0;

    if not Attributes.PureGeometry then
    begin
      UsedGLSL := GLSLRenderers.Enable(CurrentState, UsedGLSLTexCoordsNeeded);

      { Only if we bound texture units defined in shader ComposedShader fields
        (it we have shader but UsedGLSLTexCoordsNeeded = 0 then normal
        texture apply (including normal TexCoordsNeeded calculation)
        will be done):

        Although we bound only UsedGLSLTexCoordsNeeded texture units,
        we want to pass all texture coords defined in texCoord.
        Shaders may use them (even when textures are not bound for them). }

      if (UsedGLSL <> nil) and (UsedGLSLTexCoordsNeeded > 0) then
      begin
        TCD := TextureCoordsDefined;
        if Log and (TCD > UsedGLSLTexCoordsNeeded) then
          WritelnLog('TexCoord', Format('Texture coords defined in VRML/X3D for %d texture units, using them all, even though we bound only %d texture units. Reason: GLSL shaders may use them',
            [TCD, UsedGLSLTexCoordsNeeded]));
        MaxTo1st(UsedGLSLTexCoordsNeeded, TCD);
      end;
    end;
  end;

  procedure RenderShadersEnd;
  begin
    if UsedGLSL <> nil then
      UsedGLSL.Disable;
  end;

var
  Generator: TVRMLArraysGenerator;
begin
  { make a copy to our class fields }
  CurrentShape := Shape;
  CurrentGeometry := Shape.OriginalGeometry;
  CurrentState := Shape.OriginalState;

  { default ShapeBumpMapping* state }
  ShapeBumpMappingAllowed := false;
  ShapeBumpMappingUsed := bmNone;

  {$ifndef USE_VRML_NODES_TRIANGULATION}
  { We have to initalize MeshRenderer to something non-nil.

    First try to initialize from Shape.OriginalGeometry, only if this fails
    --- try to use Shape.Geometry (possibly through Proxy).
    This is to allow nodes with a Proxy
    still be rendered using direct specialized method, if available. }
  if not InitMeshRenderer then
  begin
    CurrentGeometry := Shape.Geometry;
    CurrentState := Shape.State;
    if not ((CurrentGeometry <> Shape.OriginalGeometry) and
      InitMeshRenderer) then
    begin
      VRMLWarning(vwSerious,
        { We display for user Shape.OriginalGeometry.NodeTypeName, as it's
          Shape.OriginalGeometry that cannot be rendered. User is not interested in
          the implementation fact that possibly actually proxy existed
          and could not be rendered either. }
        'Rendering of node kind "' + Shape.OriginalGeometry.NodeTypeName + '" not implemented');
      Exit;
    end;
  end;

  Assert(MeshRenderer <> nil);
  {$endif}

  try
    RenderShadersBegin;
    try
      RenderTexturesBegin;
      try
        Render_MaterialsBegin;
        try
          {$ifdef USE_VRML_NODES_TRIANGULATION}
          { Simple rendering using LocalTriangulate. }
          Shape.LocalTriangulate(true, @DrawTriangle);
          {$else}

          { initialize TBaseCoordinateRenderer.Arrays now }
          if GeneratorClass <> nil then
          begin
            { calculate Shape.Arrays }
            if Shape.Arrays = nil then
            begin
              Generator := GeneratorClass.Create(Attributes,
                CurrentShape, CurrentState, CurrentGeometry);
              try
                Generator.TexCoordsNeeded := TexCoordsNeeded;
                Generator.MaterialOpacity := MaterialOpacity;
                Generator.FogVolumetric := FogVolumetric;
                Generator.FogVolumetricDirection := FogVolumetricDirection;
                Generator.FogVolumetricVisibilityStart := FogVolumetricVisibilityStart;
                Generator.ShapeBumpMappingUsed := ShapeBumpMappingUsed;
                Generator.BumpMappingLightPosition := BumpMappingLightPosition;
                Shape.Arrays := Generator.GenerateArrays;
              finally FreeAndNil(Generator) end;
            end;

            Assert(MeshRenderer is TBaseCoordinateRenderer);
            TBaseCoordinateRenderer(MeshRenderer).Arrays := Shape.Arrays;
          end;

          MeshRenderer.Render;

          {$endif USE_VRML_NODES_TRIANGULATION}

        finally Render_MaterialsEnd end;
      finally RenderTexturesEnd end;
    finally RenderShadersEnd end;
  finally
    FreeAndNil(ExposedMeshRenderer);

    { Just for safety, force them @nil now }
    CurrentGeometry := nil;
    CurrentState := nil;
  end;
end;

procedure TVRMLGLRenderer.RenderShapeEnd(Shape: TVRMLRendererShape);

  { Disable OpenGL clip planes previously initialized by ClipPlanesBegin. }
  procedure ClipPlanesEnd;
  var
    I: Integer;
  begin
    for I := 0 to ClipPlanesEnabled - 1 do
      glDisable(GL_CLIP_PLANE0 + I);
    ClipPlanesEnabled := 0; { not really needed, but for safety... }
  end;

var
  I: Integer;
begin
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

  glPopMatrix;

  ClipPlanesEnd;

  { at the end, we're in modelview mode }
end;

procedure TVRMLGLRenderer.RenderShape(Shape: TVRMLRendererShape);
begin
  RenderShapeBegin(Shape);
  try
    RenderShapeInside(Shape);
  finally
    RenderShapeEnd(Shape);
  end;
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
      very method...) between RenderShapeBegin/End.

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

procedure TVRMLGLRenderer.SetBumpMappingLightPosition(const Value: TVector3Single);

  procedure SetInProgram(Prog: TGLSLProgram);
  begin
    if Prog <> nil then
    begin
      { so BumpMappingMethod >= bmGLSLNormal and it's already prepared }
      Prog.Enable;
      Prog.SetUniform('light_position_world_space', BumpMappingLightPosition);
      Prog.Disable;
    end;
  end;

begin
  FBumpMappingLightPosition := Value;

  SetInProgram(BmGLSLProgram[false]);
  SetInProgram(BmGLSLProgram[true]);
end;

procedure TVRMLGLRenderer.SetBumpMappingLightAmbientColor(const Value: TVector4Single);

  procedure SetInProgram(Prog: TGLSLProgram);
  begin
    if Prog <> nil then
    begin
      { so BumpMappingMethod >= bmGLSLNormal and it's already prepared }
      Prog.Enable;
      Prog.SetUniform('light_ambient_color', BumpMappingLightAmbientColor);
      Prog.Disable;
    end;
  end;

begin
  FBumpMappingLightAmbientColor := Value;

  SetInProgram(BmGLSLProgram[false]);
  SetInProgram(BmGLSLProgram[true]);
end;

procedure TVRMLGLRenderer.SetBumpMappingLightDiffuseColor(const Value: TVector4Single);

  procedure SetInProgram(Prog: TGLSLProgram);
  begin
    if Prog <> nil then
    begin
      { so BumpMappingMethod >= bmGLSLNormal and it's already prepared }
      Prog.Enable;
      Prog.SetUniform('light_diffuse_color', BumpMappingLightDiffuseColor);
      Prog.Disable;
    end;
  end;

begin
  FBumpMappingLightDiffuseColor := Value;

  SetInProgram(BmGLSLProgram[false]);
  SetInProgram(BmGLSLProgram[true]);
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

function TVRMLGLRenderer.CacheIgnoresTransform(Node: TNodeFog): boolean;
var
  Enabled, Volumetric: boolean;
  VolumetricDirection: TVector3Single;
  VolumetricVisibilityStart: Single;
begin
  InitializeFog(Node, false, Enabled, Volumetric,
    VolumetricDirection, VolumetricVisibilityStart);

  Result := not (
    { If we use any features that (may) render shape differently
      if shape's transform (or other stuff handled in RenderShapeBegin/End),
      then Result must be false. }
    Assigned(Attributes.OnVertexColor) or
    Assigned(Attributes.OnRadianceTransfer) or
    (BumpMappingMethod <> bmNone) or
    Volumetric);
end;

function FogParametersEqual(
  FogNode1: TNodeFog; const FogDistanceScaling1: Single;
  FogNode2: TNodeFog): boolean;
begin
  Result := (FogNode1 = FogNode2);
  { If both fog nodes are nil, don't compare scaling }
  if Result and (FogNode1 <> nil) then
    Result := FogDistanceScaling1 = FogNode2.TransformScale;
end;

end.
