{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/tone_mapping.fs" }
'/*' + LineEnding +
'  Copyright 2020-2020 glTF-Sample-Viewer and Michalis Kamburelis.' + LineEnding +
'' + LineEnding +
'  This file is part of "Castle Game Engine".' + LineEnding +
'  (And it is heavily based on glTF-Sample-Viewer,' + LineEnding +
'  on a compatible open-source license.)' + LineEnding +
'' + LineEnding +
'  "Castle Game Engine" is free software; see the file COPYING.txt,' + LineEnding +
'  included in this distribution, for details about the copyright.' + LineEnding +
'' + LineEnding +
'  "Castle Game Engine" is distributed in the hope that it will be useful,' + LineEnding +
'  but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding +
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.' + LineEnding +
'' + LineEnding +
'  ----------------------------------------------------------------------------' + LineEnding +
'' + LineEnding +
'  Shader code used for gamma correction and tone mapping.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'// Not used by CGE now:' + LineEnding +
'// uniform float u_Exposure;' + LineEnding +
'' + LineEnding +
'const float GAMMA = 2.2;' + LineEnding +
'const float INV_GAMMA = 1.0 / GAMMA;' + LineEnding +
'' + LineEnding +
'// sRGB to linear approximation' + LineEnding +
'// see http://chilliant.blogspot.com/2012/08/srgb-approximations-for-hlsl.html' + LineEnding +
'vec4 castle_texture_color_to_linear(const in vec4 srgbIn)' + LineEnding +
'{' + LineEnding +
'#ifdef CASTLE_GAMMA_CORRECTION' + LineEnding +
'  return vec4(pow(srgbIn.xyz, vec3(GAMMA)), srgbIn.w);' + LineEnding +
'#else' + LineEnding +
'  return srgbIn;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec3 castle_texture_color_to_linear(const in vec3 srgbIn)' + LineEnding +
'{' + LineEnding +
'#ifdef CASTLE_GAMMA_CORRECTION' + LineEnding +
'  return pow(srgbIn, vec3(GAMMA));' + LineEnding +
'#else' + LineEnding +
'  return srgbIn;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// linear to sRGB approximation' + LineEnding +
'// see http://chilliant.blogspot.com/2012/08/srgb-approximations-for-hlsl.html' + LineEnding +
'vec3 LINEARtoSRGB(const in vec3 color)' + LineEnding +
'{' + LineEnding +
'#ifdef CASTLE_GAMMA_CORRECTION' + LineEnding +
'  return pow(color, vec3(INV_GAMMA));' + LineEnding +
'#else' + LineEnding +
'  return color;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// Uncharted 2 tone map' + LineEnding +
'// see: http://filmicworlds.com/blog/filmic-tonemapping-operators/' + LineEnding +
'vec3 toneMapUncharted2Impl(const in vec3 color)' + LineEnding +
'{' + LineEnding +
'  const float A = 0.15;' + LineEnding +
'  const float B = 0.50;' + LineEnding +
'  const float C = 0.10;' + LineEnding +
'  const float D = 0.20;' + LineEnding +
'  const float E = 0.02;' + LineEnding +
'  const float F = 0.30;' + LineEnding +
'  return ((color*(A*color+C*B)+D*E)/(color*(A*color+B)+D*F))-E/F;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec3 toneMapUncharted(in vec3 color)' + LineEnding +
'{' + LineEnding +
'  const float W = 11.2;' + LineEnding +
'  color = toneMapUncharted2Impl(color * 2.0);' + LineEnding +
'  vec3 whiteScale = 1.0 / toneMapUncharted2Impl(vec3(W));' + LineEnding +
'  color = color * whiteScale;' + LineEnding +
'  #ifdef CASTLE_GAMMA_CORRECTION' + LineEnding +
'  color = LINEARtoSRGB(color);' + LineEnding +
'  #endif' + LineEnding +
'  return color;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// Hejl Richard tone map' + LineEnding +
'// see: http://filmicworlds.com/blog/filmic-tonemapping-operators/' + LineEnding +
'vec3 toneMapHejlRichard(in vec3 color)' + LineEnding +
'{' + LineEnding +
'  color = max(vec3(0.0), color - vec3(0.004));' + LineEnding +
'  color = (color*(6.2*color+.5))/(color*(6.2*color+1.7)+0.06);' + LineEnding +
'  #ifndef CASTLE_GAMMA_CORRECTION' + LineEnding +
'  // The above calculation did equivalent of "pow(color, vec3(1 / 2.2))" already.' + LineEnding +
'  // So invert it, if no gamma correction.' + LineEnding +
'  color = pow(color, vec3(GAMMA));' + LineEnding +
'  #endif' + LineEnding +
'  return color;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// ACES tone map' + LineEnding +
'// see: https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/' + LineEnding +
'vec3 toneMapACES(in vec3 color)' + LineEnding +
'{' + LineEnding +
'  const float A = 2.51;' + LineEnding +
'  const float B = 0.03;' + LineEnding +
'  const float C = 2.43;' + LineEnding +
'  const float D = 0.59;' + LineEnding +
'  const float E = 0.14;' + LineEnding +
'  color = clamp((color * (A * color + B)) / (color * (C * color + D) + E), 0.0, 1.0);' + LineEnding +
'  #ifdef CASTLE_GAMMA_CORRECTION' + LineEnding +
'  color = LINEARtoSRGB(color);' + LineEnding +
'  #endif' + LineEnding +
'  return color;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec3 castle_linear_to_screen(const in vec3 color)' + LineEnding +
'{' + LineEnding +
'  // color *= u_Exposure;' + LineEnding +
'' + LineEnding +
'  #ifdef CASTLE_TONE_MAPPING' + LineEnding +
'    #ifdef CASTLE_TONE_MAPPING_UNCHARTED' + LineEnding +
'      return toneMapUncharted(color);' + LineEnding +
'    #endif' + LineEnding +
'' + LineEnding +
'    #ifdef CASTLE_TONE_MAPPING_HEJLRICHARD' + LineEnding +
'      return toneMapHejlRichard(color);' + LineEnding +
'    #endif' + LineEnding +
'' + LineEnding +
'    #ifdef CASTLE_TONE_MAPPING_ACES' + LineEnding +
'      return toneMapACES(color);' + LineEnding +
'    #endif' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  return LINEARtoSRGB(color);' + LineEnding +
'}' + LineEnding +
''
