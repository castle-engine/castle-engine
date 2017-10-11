/* Generic GLSL fragment shader.
   Used by ../castlerendererinternalshader.pas to construct the final shader.

   This is converted to template.fs.inc, and is then compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* PLUG-DECLARATIONS */

#ifdef HAS_GEOMETRY_SHADER
  #define castle_vertex_eye castle_vertex_eye_geoshader
  #define castle_normal_eye castle_normal_eye_geoshader
#endif

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;

uniform float castle_MaterialDiffuseAlpha;
uniform float castle_MaterialShininess;
/* Color summed with all the lights.
   Like gl_Front/BackLightModelProduct.sceneColor:
   material emissive color + material ambient color * global (light model) ambient.
*/
uniform vec3 castle_SceneColor;
uniform vec4 castle_UnlitColor;

#ifdef COLOR_PER_VERTEX
varying vec4 castle_ColorPerVertexFragment;
#endif

/* Wrapper for calling PLUG texture_coord_shift */
vec2 texture_coord_shifted(in vec2 tex_coord)
{
  /* PLUG: texture_coord_shift (tex_coord) */
  return tex_coord;
}

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
    */ ; else
    normal_eye_fragment = -normal_eye_fragment;
#endif

  /* PLUG: fragment_eye_space (castle_vertex_eye, normal_eye_fragment) */

#ifdef LIT
  /* Comparing this with Gouraud shading (template_mobile.vs/fs:
     this variable performs both the role of castle_Color and fragment_color
     from that GLSL code). */
  vec4 fragment_color;

  fragment_color = vec4(castle_SceneColor, 1.0);
  /* PLUG: add_light_contribution (fragment_color, castle_vertex_eye, normal_eye_fragment, castle_MaterialShininess) */

  /* Apply alpha. Otherwise, alpha is usually large after previous add_light_contribution,
     and it's always opaque.
     Using diffuse.a is actually exactly what fixed-function pipeline does
     too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */
  #ifdef COLOR_PER_VERTEX
    // Apply castle_ColorPerVertexFragment alpha here.
    // The RGB portion of castle_ColorPerVertexFragment was
    // already applied in template_light.glsl .
    fragment_color.a = castle_ColorPerVertexFragment.a;
  #else
    fragment_color.a = castle_MaterialDiffuseAlpha;
  #endif

  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.
     This isn't really mandatory, but scenes with many lights could easily
     have colors > 1 and then the textures will look "burned out".
     Of course, for future HDR rendering we will turn this off. */
  fragment_color.rgb = min(fragment_color.rgb, 1.0);
#else
  vec4 fragment_color = castle_UnlitColor
#ifdef COLOR_PER_VERTEX
    /* Apply COLOR_PER_VERTEX, when unlit.
       (When lit, then the analogous multiplication is done
        inside template_add_light.glsl) */
    * castle_ColorPerVertexFragment
#endif
  ;

#endif

  /* PLUG: lighting_apply (fragment_color, castle_vertex_eye, normal_eye_fragment) */

  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */
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
