/* Gouraud shading GLSL fragment shader. */

#ifdef GL_ES
precision mediump float;
#endif

/* PLUG-DECLARATIONS */

#ifdef HAS_GEOMETRY_SHADER
  #define castle_vertex_eye castle_vertex_eye_geoshader
  #define castle_normal_eye castle_normal_eye_geoshader
  #define castle_Color      castle_Color_geoshader
#endif

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;
varying vec4 castle_Color;

/* Include fragment shader utilities used by both Gouraud and Phong shading. */
/* CASTLE-COMMON-CODE */

void main(void)
{
  vec4 fragment_color = castle_Color;

/* Fragment shader in Gouraud doesn't get a normal vector, for speed. */
#define normal_eye_fragment vec3(0.0)

  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */
  /* PLUG: steep_parallax_shadow_apply (fragment_color) */
  /* PLUG: fog_apply (fragment_color, normal_eye_fragment) */

#undef normal_eye_fragment

  gl_FragColor = fragment_color;

  /* PLUG: fragment_end (gl_FragColor) */
}
