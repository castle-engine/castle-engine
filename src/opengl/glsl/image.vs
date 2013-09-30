attribute vec2 vertex;
attribute vec2 tex_coord;
uniform mat4 projection_matrix;
varying vec2 tex_coord_frag;

/* Simple GLSL shader to apply 2D texture.
   Must be suitable also for GLES20, so don't use any deprecated gl_Xxx
   variables. */

void main(void)
{
  gl_Position = projection_matrix * vec4(vertex, 0.0, 1.0);
  tex_coord_frag = tex_coord;
}
