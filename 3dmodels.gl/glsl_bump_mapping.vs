/* GLSL vertex shader to do bump mapping.

   This is converted to glsl_bump_mapping.vs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

varying vec3 interpolated_normal;
varying vec3 vertex_world;

void main(void)
{
  /* Is normalization here needed ? Probably not, but it improves the
     numerical precision of interpolation ? */
  interpolated_normal = normalize(gl_NormalMatrix * gl_Normal);

  vertex_world = vec3(gl_ModelViewMatrix * gl_Vertex);

  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;

  gl_Position = ftransform();
}
