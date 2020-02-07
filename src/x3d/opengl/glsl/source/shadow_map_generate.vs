/* Vertex shader when generating shadow maps. */

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
attribute vec4 castle_Vertex;

#ifdef ALPHA_TEST
attribute vec4 castle_MultiTexCoord0;
varying vec2 castle_TexCoord0_XY;
#endif

void main(void)
{
  gl_Position = castle_ProjectionMatrix * (castle_ModelViewMatrix * castle_Vertex);

  #ifdef ALPHA_TEST
  castle_TexCoord0_XY = castle_MultiTexCoord0.xy;
  #endif
}
