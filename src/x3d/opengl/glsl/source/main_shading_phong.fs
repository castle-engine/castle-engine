/* Phong shading GLSL fragment shader.
   Used by ../castlerendererinternalshader.pas to construct the final shader.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

#ifdef GL_ES
precision mediump float;
#endif

/* PLUG-DECLARATIONS */

#ifdef HAS_GEOMETRY_SHADER
  #define castle_vertex_eye castle_vertex_eye_geoshader
  #define castle_normal_eye castle_normal_eye_geoshader
#endif

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;

uniform vec4 castle_MaterialDiffuseAlpha;
uniform vec3 castle_MaterialAmbient;
uniform vec3 castle_MaterialSpecular;
uniform float castle_MaterialShininess;

// TODO: define it from code only when necessary
#define HAS_EMISSIVE_OR_AMBIENT_TEXTURE
#ifdef HAS_EMISSIVE_OR_AMBIENT_TEXTURE
uniform vec3 castle_MaterialEmissive;
uniform vec3 castle_GlobalAmbient;
#else
/* In this case we can optimize it.
   Color summed with all the lights:
   Material emissive color + material ambient color * global (light model) ambient.
   (similar to old gl_Front/BackLightModelProduct.sceneColor in deprecated GLSL versions.)
*/
uniform vec3 castle_SceneColor;
#endif

uniform vec4 castle_UnlitColor;

#ifdef COLOR_PER_VERTEX
varying vec4 castle_ColorPerVertexFragment;
#endif

/* Include fragment shader utilities used by both Gouraud and Phong shading. */
/* CASTLE-COMMON-CODE */

/* Calculate color summed with all the lights:
   Material emissive color + material ambient color * global (light model) ambient.
*/
vec3 get_scene_color()
{
#ifdef HAS_EMISSIVE_OR_AMBIENT_TEXTURE
  vec3 ambient = castle_MaterialAmbient;
  /* PLUG: material_light_ambient (ambient) */
  vec3 emissive = castle_MaterialEmissive;
  /* PLUG: material_emissive (emissive) */
  return emissive + ambient * castle_GlobalAmbient;
#else
  return castle_SceneColor;
#endif
}

void main_texture_apply(inout vec4 fragment_color,
  const in vec3 normal_eye)
{
  /* PLUG: main_texture_apply (fragment_color, normal_eye) */
}

/* Calculated color from
   Material.diffuseColor/transparency (or ColorRGBA node) * diffuse texture.
   Contains complete "diffuse/transparency" information that is independent of light source. */
vec4 castle_material_complete_diffuse_alpha;

void main(void)
{
  vec3 normal_eye_fragment = normalize(castle_normal_eye);

#ifndef CASTLE_BUGGY_FRONT_FACING
  if (gl_FrontFacing)
    /* Avoid AMD bug http://forums.amd.com/devforum/messageview.cfm?catid=392&threadid=148827&enterthread=y
       Observed on fglrx (proprietary ATI Linux driver),
       with ATI Mobility Radeon HD 4300 (castle computer "czarny"),
       since Ubuntu 11.4 (fglrx OpenGL version 3.3.10665).

       It causes both (gl_FrontFacing) and (!gl_FrontFacing) to be true...
       To minimize the number of problems, never use "if (!gl_FrontFacing)",
       only "if (gl_FrontFacing)".

       See also https://makc3d.wordpress.com/2015/09/17/alternative-to-gl_frontfacing/ ,
       gl_FrontFacing seems unreliable sometimes.
    */ ; else
    normal_eye_fragment = -normal_eye_fragment;
#endif

  /* PLUG: fragment_eye_space (castle_vertex_eye, normal_eye_fragment) */

#ifdef LIT
  #ifdef COLOR_PER_VERTEX
  castle_material_complete_diffuse_alpha = castle_ColorPerVertexFragment;
  #else
  castle_material_complete_diffuse_alpha = castle_MaterialDiffuseAlpha;
  #endif

  main_texture_apply(castle_material_complete_diffuse_alpha, normal_eye_fragment);

  vec4 fragment_color = vec4(get_scene_color(), castle_material_complete_diffuse_alpha.a);

  /* PLUG: add_light (fragment_color, castle_vertex_eye, normal_eye_fragment) */

  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.
     This isn't really mandatory, but scenes with many lights could easily
     have colors > 1 and then the textures will look "burned out".
     Of course, for future HDR rendering we will turn this off. */
  fragment_color.rgb = min(fragment_color.rgb, 1.0);
#else
  // Unlit case

  vec4 fragment_color = castle_UnlitColor;
  /* TODO: This is not strictly correct,
     as ColorRGBA should only be used for unlit when Material=NULL.
     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set).

     TODO: Also we multiply ColorRGBA, while it should replace by default in X3D?
     But e.g. Spine rendering assumes we multiply. */
  #ifdef COLOR_PER_VERTEX
  fragment_color *= castle_ColorPerVertexFragment;
  #endif
  /* TODO: This is not strictly correct,
     as Appearance.texture should only be used for unlit when Material=NULL.
     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set). */
  main_texture_apply(fragment_color, normal_eye_fragment);
#endif

  /* PLUG: lighting_apply (fragment_color, castle_vertex_eye, normal_eye_fragment) */
  /* PLUG: steep_parallax_shadow_apply (fragment_color) */
  /* PLUG: fog_apply (fragment_color, normal_eye_fragment) */

  /* NVidia GeForce 450 GTS (kocury) fails to compile a shader when
     we pass gl_FragColor as inout parameter to functions
     (testcase even fresnel_and_toon.x3dv).
     Although on Radeon X1600 (fglrx, chantal) it works OK.
     So we just use fragment_color everywhere above, and only assign it
     to gl_FragColor at the end. */
  gl_FragColor = fragment_color;

  /* PLUG: fragment_end (gl_FragColor) */
}
