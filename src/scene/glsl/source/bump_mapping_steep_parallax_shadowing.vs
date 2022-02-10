/* Steep parallax with self-shading bump mapping shader effect.
   This is added right after bump_mapping.vs and bump_mapping_parallax.vs.
   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.
*/

varying vec3 castle_light_direction_tangent_space;

/* Note that there's no need to protect castle_LightSource0Position
   from redeclaring, which is usually a problem when
   no "separate compilation units" are available (on OpenGLES).
   In this particular case, using bump mapping forces Phong shading,
   and then lights are calculated in the fragment shader.
   The declaration below is in vertex shader, so it never conflicts. */
#ifdef GL_ES
uniform mediump vec3 castle_LightSource0Position;
#else
uniform vec3 castle_LightSource0Position;
#endif

void PLUG_bump_mapping_parallax_tangent_space_calculations(
  const in vec4 vertex_eye, const in mat3 eye_to_tangent_space)
{
  vec3 light_dir = castle_LightSource0Position;
#ifdef CASTLE_LIGHT0_POSITIONAL
  light_dir -= vec3(vertex_eye);
#endif
  light_dir = normalize(light_dir);
  castle_light_direction_tangent_space = eye_to_tangent_space * light_dir;
}
