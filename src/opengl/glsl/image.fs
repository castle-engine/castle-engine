varying vec2 tex_coord_frag;
uniform sampler2D texture;

void main(void)
{
  gl_FragColor = texture2D(texture, tex_coord_frag);
}
