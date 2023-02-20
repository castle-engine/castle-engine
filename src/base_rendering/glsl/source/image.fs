varying vec2 tex_coord_frag;
uniform sampler2D image_texture;

#ifdef COLOR_UNIFORM
uniform vec4 color;
#endif

#ifdef CLIP_LINE
uniform vec3 clip_line;
/* Not using "gl_FragCoord​.xy / gl_FragCoord​.w", for unknown reason it doesn't
 * compile (tested on Mesa OpenGL ES, and Nexus 5 OpenGL ES). */
varying vec2 frag_coord;
#endif

void main(void)
{
#ifdef COLOR_UNIFORM
  gl_FragColor = color;
#ifdef TEXTURE_HAS_ONLY_ALPHA
  gl_FragColor.a *= texture2D(image_texture, tex_coord_frag).a;
#else
  gl_FragColor *= texture2D(image_texture, tex_coord_frag);
#endif
#else
  gl_FragColor = texture2D(image_texture, tex_coord_frag);
#endif

#ifdef ALPHA_TEST
  if (gl_FragColor.a < 0.5) discard;
#endif
#ifdef CLIP_LINE
  if (dot(clip_line.xy, tex_coord_frag) + clip_line.z < 0.0) discard;
#endif
}
