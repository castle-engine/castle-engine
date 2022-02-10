/* Simplest GLSL vertex shader.
   Used also when we're OK with the simplest shader that transforms
   vertexes with projection + modelview and really nothing more.
*/

uniform mat4 castle_ModelViewProjectionMatrix;
attribute vec4 castle_Vertex;

void main(void)
{
  gl_Position = castle_ModelViewProjectionMatrix * castle_Vertex;
}
