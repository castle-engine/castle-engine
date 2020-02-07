/* Bump mapping shader effect.
   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.
*/

attribute mat3 castle_tangent_to_object_space;
varying mat3 castle_tangent_to_eye_space;

// avoid redeclaring variables when no "separate compilation units" available (OpenGLES)
#ifndef GL_ES
uniform mat3 castle_NormalMatrix;
#endif

void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)
{
  castle_tangent_to_eye_space = castle_NormalMatrix * castle_tangent_to_object_space;
  /* PLUG: bump_mapping_tangent_space_calculations (vertex_eye, castle_tangent_to_object_space) */
}
