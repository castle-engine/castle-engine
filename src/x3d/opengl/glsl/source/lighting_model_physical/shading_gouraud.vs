/*
  Copyright 2020-2020 Michalis Kamburelis and glTF-Sample-Viewer authors.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Calculate Physical (PBR) lighting model, in Gouraud shading. */

uniform vec4 castle_MaterialBaseAlpha;
uniform vec3 castle_EmissiveColor;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  vec4 material_base_alpha;

  material_base_alpha = castle_apply_color_per_vertex(castle_MaterialBaseAlpha);

  result = vec4(castle_EmissiveColor, material_base_alpha.a);

  "This GLSL code should never be used, since PhysicalMaterial requires Phong shading.
  Uncommented, to break compilation in case this is accidentally used."

  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */
  result.rgb = min(result.rgb, 1.0);
}
