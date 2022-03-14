{
  Copyright 2016-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Configure rendering options.
  The @link(TCastleRenderOptions) component configures the rendering at each particular scene,
  and is usually accessed through @link(TCastleScene.RenderOptions).
  This unit contains also related types, constants and some variables. }
unit CastleRenderOptions;

{$I castleconf.inc}

interface

uses Classes, CastleColors;

type
  { Shader types. }
  TShaderType = (stVertex, stGeometry, stFragment);

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

  { Type of @link(GammaCorrection).
    See https://castle-engine.io/manual_gamma_correction.php . }
  TGammaCorrection = (
    { Never do gamma correction. }
    gcNone,
    { Gamma correction only for PhysicalMaterial. }
    gcPhysicalMaterial,
    { Always do gamma correction. }
    gcAlways
  );

  TShadersRendering = (srDisable, srWhenRequired, srAlways) deprecated 'this was only useful with TCastleRenderOptions.Shaders, which is now deprecated in favor of TCastleRenderOptions.PhongShading';

  { Possible bump mapping options. Use with @link(TCastleRenderOptions.BumpMapping). }
  TBumpMapping = (bmNone, bmBasic, bmParallax, bmSteepParallax, bmSteepParallaxShadowing);

  { Possible values of @link(TCastleRenderOptions.Mode). }
  TRenderingMode = (
    { Normal rendering features. Everything is enabled
      (as long as other TCastleRenderOptions settings allow them). }
    rmFull,

    { Solid color is used for everything. We do not show any color variation,
      materials, lights, fog, textures on surfaces.
      We still do back-face culling and depth test.
      The idea is that we "hit" the same pixels as normal rendering
      (with the exception of alpha test textures, this mode
      doesn't set up alpha test for them).
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

    { Default setting, model polygons are simply passed to OpenGL.
      Whether this results in filled or wireframe look, depends on OpenGL
      glPolygonMode setting, filled by default. }
    weNormal,

    { The model is rendered in wireframe mode.

      LineWidth is used as wireframe line width (regardless of
      TCastleRenderOptions.Mode).

      Depending on TCastleRenderOptions.Mode value:

      @unorderedList(
        @item(If <> rmFull then WireframeColor is used as wireframe
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

  { Various ways to sort the 3D objects, in particular useful to correctly
    render the partially-transparent objects.
    @seealso TCastleRenderOptions.BlendingSort }
  TBlendingSort = (
    { Do not sort.
      Using this for @link(TCastleRenderOptions.BlendingSort Scene.RenderOptions.BlendingSort)
      is fastest, but will cause artifacts if multiple
      partially-transparent objects may be visible on top of each other. }
    bsNone,

    { Sort objects by their Z coordinate.
      Using this for @link(TCastleRenderOptions.BlendingSort Scene.RenderOptions.BlendingSort)
      is very useful for 2D worlds, with flat 2D objects
      that have zero (or near-zero) size in the Z axis,
      and they are moved in the Z axis to specify which is on top for another.

      More precisely, we take the minimum bounding box Z coordinate
      of two objects. (We don't bother calculating the middle Z coordinate,
      as we assume that the bounding box is infinitely small along the Z axis.)
      The one with @italic(larger) Z coordinate is considered to be
      @italic(closer), this is consistent with the right-handed coordinate system.

      Note that the actual camera position doesn't matter for this sorting.
      So the 2D object will look OK, @italic(even if viewed from an angle,
      even if viewed from the other side). }
    bs2D,

    { Sort objects by the (3D) distance to the camera.
      Using this for @link(TCastleRenderOptions.BlendingSort Scene.RenderOptions.BlendingSort)
      is the best sorting method for 3D
      scenes with many partially-transparent objects.

      The distance is measured from the middle
      of the bounding box to the camera posotion. }
    bs3D
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

var
  { Log shadow volume information.
    See https://castle-engine.io/manual_log.php about the log. }
  LogShadowVolumes: Boolean = false;

  { Gamma correction makes color calculation follow reality better.
    Use it when you desire more realistic rendering.
    It changes the colors, and your assets (their colors and textures)
    need to be prepared with this in mind.
    See https://castle-engine.io/manual_gamma_correction.php . }
  GammaCorrection: TGammaCorrection = gcPhysicalMaterial;

  { Change the colors you render, to make them visually better.
    See https://castle-engine.io/manual_gamma_correction.php . }
  ToneMapping: TToneMapping = tmNone;

{$define read_interface}
{$I castlerenderoptions_renderoptions.inc}
{$undef read_interface}

implementation

uses SysUtils;

{$define read_implementation}
{$I castlerenderoptions_renderoptions.inc}

end.
