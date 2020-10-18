{
  Copyright 2016-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base types and concepts related to rendering.
  Independent from OpenGL, X3D and other higher-level types. }
unit CastleRendererBaseTypes;

{$I castleconf.inc}

interface

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

  TShadersRendering = (srDisable, srWhenRequired, srAlways) deprecated 'this was only useful with TRenderingAttributes.Shaders, which is now deprecated in favor of TRenderingAttributes.PhongShading';

  { Possible bump mapping options. Use with @link(TRenderOptions.BumpMapping). }
  TBumpMapping = (bmNone, bmBasic, bmParallax, bmSteepParallax, bmSteepParallaxShadowing);

  { Possible values of @link(TRenderOptions.Mode). }
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

  { Values for @link(TRenderOptions.WireframeEffect).

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
    weSilhouette
  );

const
  ShaderTypeName: array [TShaderType] of string =
  ( 'Vertex', 'Geometry', 'Fragment' );

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

implementation

end.
