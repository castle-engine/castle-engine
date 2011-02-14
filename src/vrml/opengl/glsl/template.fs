/* Generic GLSL fragment shader template. It will be used
   by vrmlshadergenerator.pas to construct final shader.

   This is converted to template.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

varying vec4 vertex_eye;
varying vec3 normal_eye;

/* PLUG: $declare-variables$ */
/* PLUG: $declare-procedures$ */

void main(void)
{
  gl_FragColor = gl_FrontLightModelProduct.sceneColor;

  vec3 normal_eye_fragment = normalize(normal_eye);
  /* PLUG: fragment_normal_eye (normal_eye_fragment) */
  /* (inout vec3 normal_eye_fragment) */

  if (gl_FrontFacing)
  {
    gl_FragColor += gl_FrontMaterial.emission;
    /* PLUG: add_light_contribution_front (gl_FragColor, normal_eye_fragment, gl_FrontMaterial) */
    /* (inout vec4 color, const in vec3 normal_eye, const in gl_MaterialParameters material) */

    /* Otherwise, alpha is usually large after previous add_light_contribution,
       and it's always opaque.
       Using diffuse.a is actually exactly what fixed-function pipeline does
       too, according to http://www.sjbaker.org/steve/omniv/opengl_lighting.html */
    gl_FragColor.a = gl_FrontMaterial.diffuse.a;
  } else
  {
    gl_FragColor += gl_BackMaterial.emission;
    normal_eye_fragment = -normal_eye_fragment;
    /* PLUG: add_light_contribution_back (gl_FragColor, normal_eye_fragment, gl_BackMaterial) */
    /* (inout vec4 color, const in vec3 normal_eye, const in gl_MaterialParameters material) */
    gl_FragColor.a = gl_BackMaterial.diffuse.a;
  }

  /* PLUG: texture_apply (gl_FragColor) */

  /* PLUG: fragment_end (gl_FragColor) */
}
