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
  gl_FragColor = gl_FrontLightModelProduct.sceneColor;

  vec3 normal_eye_fragment = normalize(normal_eye);
  /* PLUG: fragment_normal_eye (normal_eye_fragment) */
  /* (inout vec3 normal_eye_fragment) */

  if (gl_FrontFacing)
  {
    gl_FragColor += gl_FrontMaterial.emission;
    /* PLUG: add_light_contribution_front (gl_FragColor, vertex_eye, normal_eye_fragment, gl_FrontMaterial) */

    /* Otherwise, alpha is usually large after previous add_light_contribution,
       and it's always opaque.
       Using diffuse.a is actually exactly what fixed-function pipeline does
       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */
    gl_FragColor.a = gl_FrontMaterial.diffuse.a;
  } else
  {
    gl_FragColor += gl_BackMaterial.emission;
    normal_eye_fragment = -normal_eye_fragment;
    /* PLUG: add_light_contribution_back (gl_FragColor, vertex_eye, normal_eye_fragment, gl_BackMaterial) */
    gl_FragColor.a = gl_BackMaterial.diffuse.a;
  }

  /* NVidia GeForce 450 GTS (kocury) fails to compile a shader when
     we pass gl_FragColor as inout parameter (testcase even fresnel_and_toon.x3dv).
     Radeon X1600 (fglrx, chantal) works with it OK.
     The simplest workaround: use a temp variable to hold it. */

  vec4 fragment_color = gl_FragColor;
  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */
  /* PLUG: fog_apply (fragment_color, normal_eye_fragment) */
  gl_FragColor = fragment_color;

  /* PLUG: fragment_end (gl_FragColor) */
}
