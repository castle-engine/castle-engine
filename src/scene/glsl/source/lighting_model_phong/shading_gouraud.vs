/*
  Copyright 2010-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Calculate Phong lighting model, in Gouraud shading. */

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
  return ambient * castle_GlobalAmbient;
}

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
#ifdef LIT
  /* Two-sided lighting in Gouraud shading:
     flip the normal vector to correspond to the face side that we actually see.

     Note that we don't flip the normal_eye (we only flip the
     normal_for_lighting), as normal_eye may be useful also for other
     calculations, e.g. cubemap reflections, that don't want this flippping
     (testcase: demo-models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv )

     This is commented out, because it's not perfect, and I'm not sure can
     we efficiently do artifact-free version of two-sided lighting.
     Reproduction of the problem:
     - demo-models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv,
       look at the back side of the box.
     - demo-models/fog/fog_linear, rotate in Examine and look at the thin water
       edges.

     The problem: We base our flipping on normal_eye,
     which may be a smoothed (per-vertex) normal vector.

     - We cannot calculate here reliably per-face vector (fragment shaders
       can do a trick with dFdx, see
       https://makc3d.wordpress.com/2015/09/17/alternative-to-gl_frontfacing/ ,
       but dFdx is only available in fragment shader).

     - Fully-correct solutions are inefficient:
       - To pass to vertex shader a face_normal in a special uniform
         means that we have to pass extra data, and also that we have to
         split vertexes to not share vertexes across faces.
       - Calculating light 2x times and then letting fragment shader to choose
         which side to show (this is what fixed-function does, I think).

     - If you're OK with being correct (not fast), you can use Phong shading
       where two-sided lighting works easily.
  */
  /* vec3 normal_for_lighting = (normal_eye.z > 0.0 ? normal_eye : -normal_eye); */

  MaterialInfo material_info;

  material_info.ambient = castle_MaterialAmbient;
  /* PLUG: material_ambient (material_info.ambient) */
  material_info.specular = castle_MaterialSpecular;
  /* PLUG: material_specular (material_info.specular) */
  material_info.shininess = castle_MaterialShininess;
  /* PLUG: material_shininess (material_info.shininess) */

  material_info.diffuse_alpha = castle_apply_color_per_vertex(castle_MaterialDiffuseAlpha);

  result = vec4(get_ambient_color(), material_info.diffuse_alpha.a);

  /* PLUG: add_light (result, vertex_eye, normal_eye, material_info) */

  vec3 emissive = castle_MaterialEmissive;
  result.rgb += emissive;

  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */
  result.rgb = min(result.rgb, 1.0);
#else
  /* Unlit case.
     See shading_phong.fs for comments when this is used.
  */
  result = castle_UnlitColor;
#endif
}
