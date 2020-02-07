/* Fallback GLSL vertex shader.

   Used when we need a shader (e.g. because it is OpenGLES or
   GLFeatures.EnableFixedFunction = false),
   but the default shader did not compile.
   You cannot leave the shader empty in OpenGLES.
*/

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
attribute vec4 castle_Vertex;

void main(void)
{
  gl_Position = castle_ProjectionMatrix * (castle_ModelViewMatrix * castle_Vertex);
}
