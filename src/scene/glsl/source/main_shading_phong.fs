/* Phong shading GLSL fragment shader.
   Used by ../castlerendererinternalshader.pas to construct the final shader.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* PLUG-DECLARATIONS */

#ifdef HAS_GEOMETRY_SHADER
  #define castle_vertex_eye castle_vertex_eye_geoshader
  #define castle_normal_eye castle_normal_eye_geoshader
#endif

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;

#if defined(COLOR_PER_VERTEX_RGB)
varying vec3 castle_ColorPerVertexFragment;
#elif defined(COLOR_PER_VERTEX_RGB_ALPHA)
varying vec4 castle_ColorPerVertexFragment;
#endif

/* Apply per-vertex color, over the base/diffuse/emissive color + alpha. */
vec4 castle_apply_color_per_vertex(vec4 color)
{
  return
    #if defined(COLOR_PER_VERTEX_REPLACE)
      #if defined(COLOR_PER_VERTEX_RGB)
      vec4(castle_ColorPerVertexFragment, color.a);
      #elif defined(COLOR_PER_VERTEX_RGB_ALPHA)
      castle_ColorPerVertexFragment;
      #endif
    #elif defined(COLOR_PER_VERTEX_MODULATE)
      #if defined(COLOR_PER_VERTEX_RGB)
      vec4(castle_ColorPerVertexFragment * color.rgb, color.a);
      #elif defined(COLOR_PER_VERTEX_RGB_ALPHA)
      castle_ColorPerVertexFragment * color;
      #endif
    #else
    color;
    #endif
}

/* Include fragment shader utilities used by both Gouraud and Phong shading. */
/* CASTLE-COMMON-CODE */

void main_texture_apply(inout vec4 fragment_color,
  const in vec3 normal_eye)
{
  /* PLUG: main_texture_apply (fragment_color, normal_eye) */
  /* PLUG: texture_apply (fragment_color, normal_eye) */ // deprecated
}

/* CASTLE-LIGHTING-MODEL */

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

  vec4 fragment_color;
  calculate_lighting(fragment_color, castle_vertex_eye, normal_eye_fragment);

  /* PLUG: lighting_apply (fragment_color, castle_vertex_eye, normal_eye_fragment) */
  /* PLUG: steep_parallax_shadow_apply (fragment_color) */
  /* PLUG: fragment_modify (fragment_color) */

  #ifdef CASTLE_GAMMA_CORRECTION
  fragment_color.rgb = castle_linear_to_screen(fragment_color.rgb);
  #else
  #ifdef CASTLE_TONE_MAPPING
  fragment_color.rgb = castle_linear_to_screen(fragment_color.rgb);
  #endif
  /* Optimization to not call castle_linear_to_screen always:
     it does nothing when neither CASTLE_GAMMA_CORRECTION nor CASTLE_TONE_MAPPING */
  #endif

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
