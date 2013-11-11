/* Generic GLSL vertex shader, used on OpenGL ES. */

uniform mat4 castle_ModelViewProjectionMatrix;
attribute vec3 castle_Vertex;

void main(void)
{
  gl_Position = castle_ModelViewProjectionMatrix * vec4(castle_Vertex, 1.0);
}
