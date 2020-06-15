/*
  Copyright 2020-2020 glTF-Sample-Viewer and Michalis Kamburelis.

  This file is part of "Castle Game Engine".
  (And it is heavily based on glTF-Sample-Viewer,
  on a compatible open-source license.)

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Shader code used for gamma correction and tone mapping.
*/

// Not used by CGE now:
// uniform float u_Exposure;

const float GAMMA = 2.2;
const float INV_GAMMA = 1.0 / GAMMA;

// sRGB to linear approximation
// see http://chilliant.blogspot.com/2012/08/srgb-approximations-for-hlsl.html
vec4 castle_texture_color_to_linear(const in vec4 srgbIn)
{
#ifdef CASTLE_GAMMA_CORRECTION
  return vec4(pow(srgbIn.xyz, vec3(GAMMA)), srgbIn.w);
#else
  return srgbIn;
#endif
}

vec3 castle_texture_color_to_linear(const in vec3 srgbIn)
{
#ifdef CASTLE_GAMMA_CORRECTION
  return pow(srgbIn, vec3(GAMMA));
#else
  return srgbIn;
#endif
}

// linear to sRGB approximation
// see http://chilliant.blogspot.com/2012/08/srgb-approximations-for-hlsl.html
vec3 LINEARtoSRGB(const in vec3 color)
{
#ifdef CASTLE_GAMMA_CORRECTION
  return pow(color, vec3(INV_GAMMA));
#else
  return color;
#endif
}

// Uncharted 2 tone map
// see: http://filmicworlds.com/blog/filmic-tonemapping-operators/
vec3 toneMapUncharted2Impl(const in vec3 color)
{
  const float A = 0.15;
  const float B = 0.50;
  const float C = 0.10;
  const float D = 0.20;
  const float E = 0.02;
  const float F = 0.30;
  return ((color*(A*color+C*B)+D*E)/(color*(A*color+B)+D*F))-E/F;
}

vec3 toneMapUncharted(in vec3 color)
{
  const float W = 11.2;
  color = toneMapUncharted2Impl(color * 2.0);
  vec3 whiteScale = 1.0 / toneMapUncharted2Impl(vec3(W));
  color = color * whiteScale;
  #ifdef CASTLE_GAMMA_CORRECTION
  color = LINEARtoSRGB(color);
  #endif
  return color;
}

// Hejl Richard tone map
// see: http://filmicworlds.com/blog/filmic-tonemapping-operators/
vec3 toneMapHejlRichard(in vec3 color)
{
  color = max(vec3(0.0), color - vec3(0.004));
  color = (color*(6.2*color+.5))/(color*(6.2*color+1.7)+0.06);
  #ifndef CASTLE_GAMMA_CORRECTION
  // The above calculation did equivalent of "pow(color, vec3(1 / 2.2))" already.
  // So invert it, if no gamma correction.
  color = pow(color, vec3(GAMMA));
  #endif
  return color;
}

// ACES tone map
// see: https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
vec3 toneMapACES(in vec3 color)
{
  const float A = 2.51;
  const float B = 0.03;
  const float C = 2.43;
  const float D = 0.59;
  const float E = 0.14;
  color = clamp((color * (A * color + B)) / (color * (C * color + D) + E), 0.0, 1.0);
  #ifdef CASTLE_GAMMA_CORRECTION
  color = LINEARtoSRGB(color);
  #endif
  return color;
}

vec3 castle_linear_to_screen(const in vec3 color)
{
  // color *= u_Exposure;

  #ifdef CASTLE_TONE_MAPPING
    #ifdef CASTLE_TONE_MAPPING_UNCHARTED
      return toneMapUncharted(color);
    #endif

    #ifdef CASTLE_TONE_MAPPING_HEJLRICHARD
      return toneMapHejlRichard(color);
    #endif

    #ifdef CASTLE_TONE_MAPPING_ACES
      return toneMapACES(color);
    #endif
  #endif

  return LINEARtoSRGB(color);
}
