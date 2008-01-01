/* GLSL vertex shader to do bump mapping.

   This is converted to glsl_bump_mapping.vs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* TODO: calc light_dir_tangent_v from tangents and lightpos
   VRMLOpenGLRenderer.Attributes */

attribute vec3 light_position_object_space;
attribute mat3 object_space_to_tangent;
varying vec3 light_dir_tangent;

void main(void)
{
  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;

  /* Calculate light_dir_tangent, which is crucial for bump mapping.
     This in some way does the same as LightDirectionInTangentSpace
     function in VRMLOpenGLRenderer, but this works in shader. */
  vec3 light_dir_object_space = light_position_object_space - gl_Vertex;
  light_dir_tangent = object_space_to_tangent * light_dir_object_space;
  light_dir_tangent.y = -light_dir_tangent.y;

  gl_Position = ftransform();
}
