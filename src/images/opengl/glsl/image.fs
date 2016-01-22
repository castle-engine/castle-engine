#ifdef GL_ES
precision mediump float;
#endif

varying vec2 tex_coord_frag;
uniform sampler2D texture;

#ifdef COLOR_UNIFORM
uniform vec4 color;
#endif

void main(void)
{
#ifdef COLOR_UNIFORM
  gl_FragColor = color;
#ifdef TEXTURE_HAS_ONLY_ALPHA
  gl_FragColor.a *= texture2D(texture, tex_coord_frag).a;
#else
  gl_FragColor *= texture2D(texture, tex_coord_frag);
#endif
#else
  gl_FragColor = texture2D(texture, tex_coord_frag);
#endif

#ifdef ALPHA_TEST
  if (gl_FragColor.a < 0.5) discard;
#endif
}
