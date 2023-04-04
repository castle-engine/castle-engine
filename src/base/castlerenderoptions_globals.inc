{%MainUnit castlerenderoptions.pas}
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

{$ifdef read_interface}

type
  { Shader types. }
  TShaderType = (stVertex, stGeometry, stFragment);

  { What to do when shader uniform variable is set
    but doesn't exist in the shader.
    See @link(TGLSLProgram.UniformMissing). }
  TUniformMissing = (
    { Report that uniform variable not found to WritelnWarning. }
    umWarning,
    { Report that uniform variable not found by raising EGLSLUniformNotFound.

      This was implemented in the past, but was not useful - we converted it to warnings anyway.
      Because OpenGL aggressively removes unused variables from shader code,
      it was common for perfectly valid shader code to raise this
      (e.g. when using shader for lit objects, but no light is present on the scene).
    }
    //uaException,
    { Ignore the fact that uniform variable doesn't exist in the GLSL shader.
      Do not warn anywhere. }
    umIgnore
  );

  { Type of @link(TAbstractColorNode.Mode). }
  TColorMode = (cmReplace, cmModulate);

  { Type of @link(ToneMapping). }
  TToneMapping = (
    tmNone,

    { Uncharted 2 tone map.
      See http://filmicworlds.com/blog/filmic-tonemapping-operators/ }
    tmUncharted,

    { Hejl Richard tone map.
      See http://filmicworlds.com/blog/filmic-tonemapping-operators/ }
    tmHejlRichard,

    { ACES tone map.
      See https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/ }
    tmACES
  );

  { Color space. Determines in what color space do we make lighting calculations
    and do we gamma-correct colors when reading textures and writing to screen.
    See https://castle-engine.io/color_space . }
  TColorSpace = (
    (*
    { For now we decided to not implement per-scene RenderOptions.ColorSpace.
      As such csDefault, and special
        TColorSpaceNotDefault = csSRGB..csLinear;
      are not useful. }
    csDefault,
    *)

    { Color calculation is done in sRGB color space.
      That is, gamma correction is @italic(not done).

      This results in marginally faster but also incorrect lighting calculation.

      This was the "old, traditional" way of calculating lighting in 3D applications,
      before linear space (with gamma correction) became popular.
      Some 3D models and their textures may assume it and can look weird
      with calculation in linear space. }
    csSRGB,

    { Color calculation is done in linear space for materials using PBR (TPhysicalMaterialNode),
      otherwise (for unlit and Phong lighting -- @link(TUnlitMaterialNode), @link(TMaterialNode))
      the calculation is done in sRGB space.

      This is like using csSRGB for @link(TUnlitMaterialNode), @link(TMaterialNode),
      and csLinear for TPhysicalMaterialNode.
      It is the default color space for Castle Game Engine,
      as it means that typical materials in modern 3D models (PBR) will behave nicely,
      while also keeping backward compatibility (for Phong and unlit materials). }
    csLinearWhenPhysicalMaterial,

    { Color calculation is always done in linear space. This results in correct lighting calculation.

      It means that texture colors (like for @link(TPhysicalMaterialNode.BaseTexture)
      or @link(TMaterialNode.DiffuseTexture) or
      @link(TAbstractOneSidedMaterialNode.EmissiveTexture TUnlitMaterialNode.EmissiveTexture))
      are converted from sRGB into linear space before using them (to multiply with material
      and lighting colors) and at the end the resulting color is converted back from linear
      to display space (likely sRGB) for proper display.

      See https://castle-engine.io/color_space and
      https://github.com/michaliskambi/x3d-tests/wiki/Gamma-correction-in-X3D-and-glTF
      for more details.

      In other words, @italic(gamma correction is performed). }
    csLinear
  );

  { @deprecated Deprecated name for @link(TColorSpace). }
  TGammaCorrection = (
    { Never do gamma correction. }
    gcNone,
    { Gamma correction only for PhysicalMaterial. }
    gcPhysicalMaterial,
    { Always do gamma correction. }
    gcAlways
  ) deprecated 'use TColorSpace';

  TShadersRendering = (srDisable, srWhenRequired, srAlways) deprecated 'this was only useful with TCastleRenderOptions.Shaders, which is now deprecated in favor of TCastleRenderOptions.PhongShading';

  { Possible bump mapping options.
    Use for @link(TCastleRenderOptions.BumpMapping).
    See https://castle-engine.io/bump_mapping . }
  TBumpMapping = (
    { Do not perform bump mapping. The normal map texture is completely ignored. }
    bmNone,

    { Simple bump mapping: use the information in normal map RGB channels
      to change the normal vector at each pixel.
      Achieves a realistic "rough" surface look.

      See https://en.wikipedia.org/wiki/Bump_mapping . }
    bmBasic,

    { In addition to bmBasic, the texture coordinates
      are perturbed based on the height map and camera direction, to create
      illusion of 3D shape instead of flat surface.

      This makes e.g. the bricks on the texture really
      visible as "standing out", in 3D, from the wall.

      We rely on the height map information which should be in the alpha channel
      of the normal map texture.
      The @link(TCastleRenderOptions.BumpMappingParallaxHeight) can be used
      to scale that information.

      See https://en.wikipedia.org/wiki/Parallax_mapping . }
    bmParallax,

    { Improves the parallax effect, by tracing the ray as it goes through the height map.
      This is more costly, but makes the parallax effect better.

      See http://graphics.cs.brown.edu/games/SteepParallax/index.html . }
    bmSteepParallax,

    { Improves the bmSteepParallax even more, by adding additional shadow calculation.

      See http://graphics.cs.brown.edu/games/SteepParallax/index.html . }
    bmSteepParallaxShadowing
  );

  { Possible values of @link(TCastleRenderOptions.Mode). }
  TRenderingMode = (
    { Normal rendering features. Everything is enabled
      (as long as other TCastleRenderOptions settings allow them). }
    rmFull,

    { Solid color is used for everything. We do not show any color variation,
      materials, lights, fog, textures on surfaces, alpha test.
      We still do back-face culling and depth test.
      The idea is that we "hit" the same pixels as normal rendering
      (with the exception of alpha test textures, this mode
      doesn't set up alpha test since it doesn't set up textures).
      But everything has color TCastleRenderOptions.SolidColor.

      This is useful for special tricks. }
    rmSolidColor,

    { Only the rendering fetures that affect depth buffer work reliably,
      everything else is undefined (and works as fast as possible).
      This is suitable if you render only to depth buffer, like for shadow maps.

      It's quite similar to rmSolidColor, except alpha testing must work,
      so (at least some) textures must be applied over the model. }
    rmDepth
  );

  { Values for @link(TCastleRenderOptions.WireframeEffect).

    Generally, two other properties may affect the way wireframe is rendered:
    TCastleRenderOptions.WireframeColor and
    TCastleRenderOptions.LineWidth, quite self-explanatory. }
  TWireframeEffect = (

    { Default setting, polygons are rendered as filled polygons. }
    weNormal,

    { Polygons are rendered as wireframe.

      LineWidth is used as wireframe line width (regardless of
      TCastleRenderOptions.Mode).

      Depending on TCastleRenderOptions.Mode value:

      @unorderedList(
        @item(If TCastleRenderOptions.Mode <> rmFull then WireframeColor is used as wireframe
          line color.)

        @item(If rmFull, then lines are colored
          and potentially lighted and textured just like their corresponding
          triangles would be colored. So you can control lighting using
          @link(TCastleRenderOptions.Lighting),
          @link(TCastleRenderOptions.ReceiveSceneLights),
          @link(TCastleRenderOptions.ReceiveGlobalLights) properties, and you
          can control texturing by @link(TCastleRenderOptions.Textures) property.)
      ) }
    weWireframeOnly,

    { The model is rendered as normal, with it's wireframe version visible
      on top. This is most often called "solid wireframe", since the intention
      is too see wireframe version of the model but still render shapes
      solid (e.g. filled polygons with depth test).

      @link(TCastleRenderOptions.WireframeColor Scene.RenderOptions.WireframeColor) and
      @link(TCastleRenderOptions.LineWidth Scene.RenderOptions.LineWidth) determine the color and width
      of lines.

      This is often used together with the
      @link(TCastleRenderOptions.Mode RenderOptions.Mode)
      set to rmSolidColor. In such case,
      Then @link(TCastleRenderOptions.SolidColor) determinesthe fill color. }
    weSolidWireframe,

    { The model is rendered as normal, with silhouette outlined around it.
      This works quite like weSolidWireframe, except that weSolidWireframe
      makes the wireframe mesh slightly in front the model, while weSilhouette
      makes the wireframe mesh slightly at the back of the model. This way
      only the silhouette is visible from the wireframe rendering.

      @link(TCastleRenderOptions.WireframeColor Scene.RenderOptions.WireframeColor) and
      @link(TCastleRenderOptions.LineWidth Scene.RenderOptions.LineWidth) determine the color and width
      of silhouette lines.

      This is often used together with the
      @link(TCastleRenderOptions.Mode RenderOptions.Mode)
      set to rmSolidColor. In such case,
      Then @link(TCastleRenderOptions.SolidColor) determinesthe fill color. }
    weSilhouette
  );

  { Values for @link(TCastleRenderOptions.ShadowSampling). }
  TShadowSampling = (
    { One sample to shadow map. }
    ssSimple,

    { Percentage Closer Filtering improve shadow maps look, by sampling
      the depth map a couple of times. They also make shadow more blurry
      (increase shadow map size to counteract this), and a little slower.
      They may also introduce new artifacts (due to bad interaction
      with the "polygon offset" of shadow map). }
    ssPCF4,
    ssPCF4Bilinear,
    ssPCF16,

    { Variance Shadow Maps, see http://www.punkuser.net/vsm/ .
      This may generally produce superior
      results, as shadow maps can be then filtered like normal textures
      (bilinear, mipmaps, anisotropic filtering). So shadows look much nicer
      from very close and very far distances.
      However, this requires new GPU, and may cause artifacts on some scenes. }
    ssVarianceShadowMaps
  );

  { Texture minification filter (what happens when many texture pixels
    are squeezed in one screen pixel). }
  TAutoMinificationFilter = (
    minNearest,
    minLinear,
    minNearestMipmapNearest,
    minNearestMipmapLinear,
    minLinearMipmapNearest,
    minLinearMipmapLinear,

    { Interpretation of this filter depends on current
      @link(TCastleRenderOptions.MinificationFilter Scene.RenderOptions.MinificationFilter).
      If that is also minDefault, it depends on current
      @link(TCastleRenderOptions.DefaultMinificationFilter). }
    minDefault,
    { Alias for minNearest. }
    minFastest,
    { Alias for minLinearMipmapLinear. }
    minNicest
  );
  TMinificationFilter = minNearest..minLinearMipmapLinear;

  { Texture magnification filter (what happens when a single texture pixel
    in stretched over many screen pixels). }
  TAutoMagnificationFilter = (
    magNearest,
    magLinear,

    { Interpretation of this filter depends on current
      @link(TCastleRenderOptions.MagnificationFilter Scene.RenderOptions.MagnificationFilter).
      If that is also magDefault, it depends on current
      @link(TCastleRenderOptions.DefaultMagnificationFilter). }
    magDefault,
    { Alias for magnNearest. }
    magFastest,
    { Alias for magLinear. }
    magNicest
  );
  TMagnificationFilter = magNearest..magLinear;

  TBlendingSourceFactor = (
    bsSrcAlpha,
    bsOneMinusSrcAlpha,
    bsZero,
    bsOne,

    bsDstColor,
    bsSrcColor, //< As a source factor only since GL 1.4, check @code(GLFeatures.Version_1_4)
    bsDstAlpha,
    bsOneMinusDstColor,
    bsOneMinusSrcColor, //< As a source factor only since GL 1.4, check @code(GLFeatures.Version_1_4)
    bsOneMinusDstAlpha,

    bsSrcAlphaSaturate,

    bsConstantColor,
    bsOneMinusConstantColor,
    bsConstantAlpha,
    bsOneMinusConstantAlpha
  );
  TBlendingDestinationFactor = (
    bdSrcAlpha,
    bdOneMinusSrcAlpha,
    bdZero,
    bdOne,

    bdDstColor, //< As a destination factor only since GL 1.4, check @code(GLFeatures.Version_1_4)
    bdSrcColor,
    bdDstAlpha,
    bdOneMinusDstColor, //< As a destination factor only since GL 1.4, check @code(GLFeatures.Version_1_4)
    bdOneMinusSrcColor,
    bdOneMinusDstAlpha,

    // not supported by OpenGL for destination factor: bsSrcAlphaSaturate
    { }
    bdConstantColor,
    bdOneMinusConstantColor,
    bdConstantAlpha,
    bdOneMinusConstantAlpha
  );

  { Various ways to sort the objects, useful to correctly
    render partially-transparent objects.
    See https://castle-engine.io/blending .
    @seealso TCastleRenderOptions.BlendingSort
    @seealso TCastleAbstractRootTransform.BlendingSort }
  TBlendingSort = (
    { Do not sort.
      This is of course fastest, but will cause artifacts if multiple
      partially-transparent objects may be visible on top of each other. }
    bsNone,

    { Sort objects by their Z coordinate.
      Good for 2D worlds, with flat 2D objects
      that have zero (or near-zero) size in the Z axis,
      and they are moved in the Z axis to specify which is on top for another.

      More precisely, we take the minimum bounding box Z coordinate
      of two objects. (We don't bother calculating the middle Z coordinate,
      as we assume that the bounding box is infinitely small along the Z axis.)
      The one with @italic(larger) Z coordinate is considered to be
      @italic(closer), this is consistent with the right-handed coordinate system.

      Note that the camera position doesn't matter for this sorting.
      So the 2D object will look OK, @italic(even if viewed from an angle,
      even if viewed from the other side). }
    bs2D,

    { Sort objects by their Y coordinate and YSortOffset.The sorting is recursive.}
    bsYSort,

    { Sort objects by the (3D) distance of their bounding box to the camera.
      This is the best universal sorting method for 3D
      scenes with many partially-transparent objects.

      The distance is measured from the middle
      of the bounding box to the camera posotion. }
    bs3D,

    { Sort objects by the (3D) distance of their origin point (point (0,0,0) in local coordinates)
      to the camera.
      This is a useful sorting method for 3D
      scenes with many partially-transparent objects.

      This is an alternative to bs3D, it is more under your control
      (because the origin doesn't change e.g. because of animations
      or using TCastleBillboard, contrary to bounding box,
      see https://github.com/castle-engine/castle-engine/issues/427#issuecomment-1365862663 ).
      The downside is that you need to be more aware where is your object origin. }
    bs3DOrigin,

    { Sort objects by the (3D) distance of their origin point (point (0,0,0) in local coordinates)
      to the camera, projected on the ground (Y=0).

      Similar to bs3DOrigin, but the height of the object (above Y) doesn't matter. }
    bs3DGround
  );

  T3DCoord = 0..2;
  T3DCoords = set of T3DCoord;

  { Render layer for @link(TCastleTransform.RenderLayer). }
  TRenderLayer = (rlParent, rlBack, rlFront);

  { Supported line types (patterns), for @link(TLinePropertiesNode.LineType)
    or @link(TRenderContext.LineType). }
  TLineType = (
    ltSolid,
    ltDashed,
    ltDotted,
    ltDashedDotted,
    ltDashDotDot
  );

const
  ShadowSamplingNames: array [TShadowSampling] of string =
  ( 'Simple', 'PCF 4', 'PCF 4 Bilinear', 'PCF 16', 'Variance Shadow Maps (Experimental)' );

  ShaderTypeName: array [TShaderType] of string =
  ( 'Vertex', 'Geometry', 'Fragment' );

  BumpMappingNames: array [TBumpMapping] of string = (
    'None',
    'Basic',
    'Parallax',
    'Steep Parallax',
    'Steep Parallax With Self-Shadowing'
  );

  { Names for TBlendingSourceFactor used by TBlendModeNode when it is read/written in X3D file.
    Deliberately compatible with (a subset of) X3DOM BlendMode specification on
    https://doc.x3dom.org/author/Shape/BlendMode.html . }
  BlendingSourceFactorNames: array [TBlendingSourceFactor] of string =
  (
    'SRC_ALPHA',
    'ONE_MINUS_SRC_ALPHA',
    'ZERO',
    'ONE',

    'DST_COLOR',
    'SRC_COLOR',
    'DST_ALPHA',

    'ONE_MINUS_DST_COLOR',
    'ONE_MINUS_SRC_COLOR',
    'ONE_MINUS_DST_ALPHA',

    'SRC_ALPHA_SATURATE',

    'CONSTANT_COLOR',
    'ONE_MINUS_CONSTANT_COLOR',
    'CONSTANT_ALPHA',
    'ONE_MINUS_CONSTANT_ALPHA'
  );

  { Names for TBlendingDestinationFactor used by TBlendModeNode when it is read/written in X3D file.
    Deliberately compatible with (a subset of) X3DOM BlendMode specification on
    https://doc.x3dom.org/author/Shape/BlendMode.html . }
  BlendingDestinationFactorNames: array [TBlendingDestinationFactor] of string =
  (
    'SRC_ALPHA',
    'ONE_MINUS_SRC_ALPHA',
    'ZERO',
    'ONE',

    'DST_COLOR',
    'SRC_COLOR',
    'DST_ALPHA',

    'ONE_MINUS_DST_COLOR',
    'ONE_MINUS_SRC_COLOR',
    'ONE_MINUS_DST_ALPHA',

    // 'SRC_ALPHA_SATURATE', // NOT SUPPORTED AS DESTINATION FACTOR

    'CONSTANT_COLOR',
    'ONE_MINUS_CONSTANT_COLOR',
    'CONSTANT_ALPHA',
    'ONE_MINUS_CONSTANT_ALPHA'
  );

  DefaultColorSpace = csLinearWhenPhysicalMaterial;

  { Default for TCastleWindow.StencilBits and TCastleControl.StencilBits.

    Non-zero now, which means that in both TCastleWindow and TCastleControl,
    shadow volumes should work by default.

    Note that TCastleWindow implementation makes a smooth fallback:
    if we cannot get stencil, we retry initialization without stencil buffer.
    This way we will open the window reliably, and in the worst case -- shadow volumes
    will not work. }
  DefaultStencilBits = 8;

var
  { Log shadow volume information.
    See https://castle-engine.io/manual_log.php about the log. }
  LogShadowVolumes: Boolean = false;

  { Determines if color calculation is done in linear space (thus performing
    gamma correction when reading textures and when storing pixel color to screen)
    or sRGB (thus not performing gamma correction).

    Using linear color space follows reality better and is advised
    for modern games.

    Whatever you choose, your assets (their colors and textures)
    have to be prepared with the given color space in mind.

    See https://castle-engine.io/color_space
    and possible TColorSpace values documentation. }
  ColorSpace: TColorSpace = DefaultColorSpace;

  { Change the colors you render, to make them visually better.

    Contrary to @link(ColorSpace), tone mapping is not about realism.
    It's just about changing all colors using some visually-pleasing equation.
    Just as with <i@link(ColorSpace), tone mapping is applied to TCastleScene rendering.
    It doesn't affect UI rendering.

    Do you want to implement your own color-changing operation?
    There are two ways to process colors with shaders:

    @orderedList(
      @item(You can define your own color processing using
        our compositing shaders ( https://castle-engine.io/compositing_shaders.php ),
        use PLUG_fragment_modify ( https://castle-engine.io/compositing_shaders_doc/html/section.fragment_plugs.html ).
        See https://github.com/castle-engine/demo-models/blob/master/compositing_shaders/tone_mapping.x3dv
        example.
      )
      @item(You can use screen effects (see https://castle-engine.io/x3d_extensions_screen_effects.php )
        to apply post-processing in screen space.)
      )
    ) }
  ToneMapping: TToneMapping = tmNone;

  { Force given rendering for all scenes, if not weNormal. }
  InternalForceWireframe: TWireframeEffect;

{$ifdef FPC}
function GetGammaCorrection: TGammaCorrection;
  deprecated 'use ColorSpace';
procedure SetGammaCorrection(const Value: TGammaCorrection);
  deprecated 'use ColorSpace';
property GammaCorrection: TGammaCorrection read GetGammaCorrection write SetGammaCorrection;
  {deprecated 'use ColorSpace';} { cannot deprecate global properties in FPC }
{$endif}

{$endif read_interface}

{$ifdef read_implementation}

{$ifdef FPC}
function GetGammaCorrection: TGammaCorrection;
begin
  case ColorSpace of
    csSRGB                      : Result := gcNone;
    csLinearWhenPhysicalMaterial: Result := gcPhysicalMaterial;
    csLinear                    : Result := gcAlways;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('GetGammaCorrection:ColorSpace?');
    {$endif}
  end;
end;

procedure SetGammaCorrection(const Value: TGammaCorrection);
begin
  case Value of
    gcNone            : ColorSpace := csSRGB;
    gcPhysicalMaterial: ColorSpace := csLinearWhenPhysicalMaterial;
    gcAlways          : ColorSpace := csLinear;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('SetGammaCorrection:Value?');
    {$endif}
  end;
end;
{$endif}

{$endif read_implementation}
