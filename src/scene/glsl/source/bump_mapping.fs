/* Bump mapping shader effect.
   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.
*/

varying mat3 castle_tangent_to_eye_space;

// declare castle_normal_map, avoiding redeclaring it for GL_ES
#ifdef GL_ES
  #ifndef castle_normal_map_defined
  #define castle_normal_map_defined
  uniform sampler2D castle_normal_map;
  uniform float castle_normalScale;
  #endif
#else
  uniform sampler2D castle_normal_map;
  uniform float castle_normalScale;
#endif

// avoid redeclaring for GL_ES
#ifndef GL_ES
varying vec4 castle_TexCoord<NormalMapTextureCoordinatesId>;
#endif

void PLUG_fragment_eye_space(const vec4 vertex, inout vec3 normal_eye_fragment)
{
  // Read normal from the texture (the core idea of bump mapping).
  // Unpack normals, they are in texture in [0..1] range and we want in [-1..1].
  vec3 normal_tangent = texture2D(castle_normal_map,
    castle_TexCoord<NormalMapTextureCoordinatesId>.st).xyz * 2.0 - vec3(1.0);

  normal_tangent *= vec3(castle_normalScale, castle_normalScale, 1.0);

  /* We have to take two-sided lighting into account here, in tangent space.
     Simply negating whole normal in eye space (like we do without bump mapping)
     would not work good,
     check e.g. insides of demo_models/bump_mapping/room_for_parallax_final.wrl. */
  if (gl_FrontFacing)
    /* Avoid AMD bug http://forums.amd.com/devforum/messageview.cfm?catid=392&threadid=148827&enterthread=y
       It causes both (gl_FrontFacing) and (!gl_FrontFacing) to be true...
       To minimize the number of problems, never use "if (!gl_FrontFacing)",
       only "if (gl_FrontFacing)".
       See template_phong.fs for more comments.
    */ ; else
    normal_tangent.z = -normal_tangent.z;

  normal_eye_fragment = normalize(castle_tangent_to_eye_space * normal_tangent);
}
