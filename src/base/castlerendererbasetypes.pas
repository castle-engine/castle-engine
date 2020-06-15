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
