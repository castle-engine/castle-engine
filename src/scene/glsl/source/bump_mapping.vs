/* Bump mapping shader effect.
   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.
*/

attribute vec4 castle_Tangent;
varying mat3 castle_tangent_to_eye_space;

// avoid redeclaring variables when no "separate compilation units" available (OpenGLES)
#ifndef GL_ES
  #if defined(CASTLE_HAS_NORMALS)
    uniform mat3 castle_NormalMatrix;
  #endif
#endif

void PLUG_vertex_eye_space_extended(
  const in vec4 vertex_eye, const in vec3 normal_eye, const in vec3 normal_object)
{
  #if defined(CASTLE_HAS_NORMALS)
  vec4 tangent_object = castle_Tangent;
  /* Potentially modify tangent_object by skinned animation */
  /* PLUG: tangent_object_space (tangent_object) */
  vec3 bitangent = cross(normal_object, tangent_object.xyz) * tangent_object.w;
  mat3 castle_tangent_to_object_space = mat3(
    tangent_object.xyz,
    bitangent,
    normal_object
  );
  castle_tangent_to_eye_space = castle_NormalMatrix * castle_tangent_to_object_space;
  #else
  /* Fallback in case you use bump mapping with unlit material that ignores normals. */
  castle_tangent_to_eye_space = mat3(1.0);
  #endif

  /* PLUG: bump_mapping_tangent_space_calculations (vertex_eye, castle_tangent_to_object_space) */
}
