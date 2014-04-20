/* Fallback GLSL vertex shader,
   used for OpenGL ES when normal shader did not compile.
   This is merely used to avoid crashing the application (you cannot leave
   the shader empty in OpenGLES). */

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
attribute vec4 castle_Vertex;

void main(void)
{
  gl_Position = castle_ProjectionMatrix * (castle_ModelViewMatrix * castle_Vertex);
}
