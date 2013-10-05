attribute vec2 vertex;
uniform mat4 projection_matrix;

/* Simple GLSL shader to draw 2D rectangle. */

void main(void)
{
  gl_Position = projection_matrix * vec4(vertex, 0.0, 1.0);
}
