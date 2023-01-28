attribute vec2 vertex;
uniform vec2 viewport_size;

/* Simple GLSL shader to draw 2D primitive (like a rectangle). */

void main(void)
{
  gl_Position = vec4(vertex * 2.0 / viewport_size - vec2(1.0), 0.0, 1.0);
}
