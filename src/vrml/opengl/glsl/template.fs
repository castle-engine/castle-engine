/* Generic GLSL fragment shader template. It will be used
   by vrmlshadergenerator.pas to construct final shader.

   This is converted to template.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

varying vec4 vertex_eye;
varying vec3 normal_eye;

/* Forward declare shadow maps utilities. */
float shadow(sampler2DShadow shadowMap, vec4 shadowMapCoord,
  const in float size);
float shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord);

/* PLUG-DECLARATIONS */

void main(void)
{
  /* NVidia GeForce 450 GTS (kocury) fails to compile a shader when
     we pass gl_FragColor as inout parameter (testcase even fresnel_and_toon.x3dv).
     Radeon X1600 (fglrx, chantal) works with it OK.
     So we just use fragment_color everywhere, and only assign it
     to gl_FragColor at the end. */
  vec4 fragment_color = gl_FrontLightModelProduct.sceneColor;

  vec3 normal_eye_fragment = normalize(normal_eye);
  /* PLUG: fragment_eye_space (vertex_eye, normal_eye_fragment) */

#ifdef LIT
  if (gl_FrontFacing)
  {
    fragment_color += gl_FrontMaterial.emission;
    /* PLUG: add_light_contribution_front (fragment_color, vertex_eye, normal_eye_fragment, gl_FrontMaterial) */

    /* Otherwise, alpha is usually large after previous add_light_contribution,
       and it's always opaque.
       Using diffuse.a is actually exactly what fixed-function pipeline does
       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */
    fragment_color.a = gl_FrontMaterial.diffuse.a;
  } else
  {
    fragment_color += gl_BackMaterial.emission;
    normal_eye_fragment = -normal_eye_fragment;
    /* PLUG: add_light_contribution_back (fragment_color, vertex_eye, normal_eye_fragment, gl_BackMaterial) */
    fragment_color.a = gl_BackMaterial.diffuse.a;
  }
#else
  fragment_color = gl_Color;
#endif

  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */
  /* PLUG: fog_apply (fragment_color, normal_eye_fragment) */
  gl_FragColor = fragment_color;

  /* PLUG: fragment_end (gl_FragColor) */
}
