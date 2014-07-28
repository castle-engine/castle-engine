/* Simple GLSL shader to apply 2D screen quad.
   Must be suitable also for GLES20, so don't use any deprecated gl_Xxx
   variables. */

attribute vec2 vertex; // must be already in clip coordinates, -1..1
attribute vec2 tex_coord;
varying vec2 tex_coord_frag;

void main(void)
{
  gl_Position = vec4(vertex, 0.0, 1.0);
  tex_coord_frag = tex_coord;
}
