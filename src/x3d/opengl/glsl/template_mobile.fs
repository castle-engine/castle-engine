/* Generic GLSL fragment shader, used on OpenGL ES. */

precision mediump float;

/* PLUG-DECLARATIONS */

#ifdef HAS_GEOMETRY_SHADER
  #define castle_vertex_eye castle_vertex_eye_geoshader
  #define castle_normal_eye castle_normal_eye_geoshader
#endif

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;
varying vec4 castle_Color;

/* Wrapper for calling PLUG texture_coord_shift */
vec2 texture_coord_shifted(in vec2 tex_coord)
{
  /* PLUG: texture_coord_shift (tex_coord) */
  return tex_coord;
}

void main(void)
{
  vec4 fragment_color = castle_Color;

/* Fragment shader on mobile doesn't get a normal vector now, for speed. */
#define normal_eye_fragment vec3(0.0)

  /* PLUG: texture_apply (fragment_color, normal_eye_fragment) */
  /* PLUG: steep_parallax_shadow_apply (fragment_color) */
  /* PLUG: fog_apply (fragment_color, normal_eye_fragment) */

#undef normal_eye_fragment

  gl_FragColor = fragment_color;

  /* PLUG: fragment_end (gl_FragColor) */
}
