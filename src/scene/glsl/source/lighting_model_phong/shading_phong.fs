/*
  Copyright 2010-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Calculate Phong lighting model, in Phong shading. */

uniform vec4 castle_MaterialDiffuseAlpha;
uniform vec3 castle_MaterialAmbient;
uniform vec3 castle_MaterialSpecular;
uniform float castle_MaterialShininess;
uniform vec3 castle_MaterialEmissive;
uniform vec3 castle_GlobalAmbient;

uniform vec4 castle_UnlitColor;

/* Material ambient color * global (light model) ambient. */
vec3 get_ambient_color()
{
  vec3 ambient = castle_MaterialAmbient;
  /* PLUG: material_light_ambient (ambient) */
  return ambient * castle_GlobalAmbient;
}

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
#ifdef LIT
  MaterialInfo material_info;

  material_info.ambient = castle_MaterialAmbient;
  /* PLUG: material_ambient (material_info.ambient) */
  material_info.specular = castle_MaterialSpecular;
  /* PLUG: material_specular (material_info.specular) */
  material_info.shininess = castle_MaterialShininess;
  /* PLUG: material_shininess (material_info.shininess) */

  material_info.diffuse_alpha = castle_apply_color_per_vertex(castle_MaterialDiffuseAlpha);

  main_texture_apply(material_info.diffuse_alpha, normal_eye);

  result = vec4(get_ambient_color(), material_info.diffuse_alpha.a);

  /* PLUG: add_light (result, vertex_eye, normal_eye, material_info) */

  /* PLUG: material_occlusion (result) */

  vec3 emissive = castle_MaterialEmissive;
  /* PLUG: material_emissive (emissive) */
  result.rgb += emissive;

  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.
     This isn't really mandatory, but scenes with many lights could easily
     have colors > 1 and then the textures will look "burned out".
     Of course, for future HDR rendering we will turn this off. */
  result.rgb = min(result.rgb, 1.0);
#else
  /* Unlit case.
     This is only an optimization of the "lit" case
     (e.g. no need to do per-light-source processing).

     Diffuse color is zero in this case, and we know that it's not set to non-zero
     by castle_ColorPerVertexFragment (since that would make the shape "lit").
     So we don't use castle_apply_color_per_vertex,
     and we don't apply RGB of main_texture_apply (as it would be multiplied by zero).
  */
  result = castle_UnlitColor;

  // Apply emissiveTexture, if exists
  /* PLUG: material_emissive (result.rgb) */

  // Apply alpha from the diffuseTexture (or Appearance.texture).
  // We need to call main_texture_apply for this.
  vec4 color_alpha = result;
  main_texture_apply(color_alpha, normal_eye);
  result.a = color_alpha.a;
#endif
}
