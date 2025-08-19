attribute vec2 vertex;
attribute vec2 tex_coord;
uniform vec2 viewport_size;
varying vec2 tex_coord_frag;
void main(void)
{
  gl_Position = vec4(vertex * 2.0 / viewport_size - vec2(1.0), 0.0, 1.0);
  tex_coord_frag = tex_coord;
}
