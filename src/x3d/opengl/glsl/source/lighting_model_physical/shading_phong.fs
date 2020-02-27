uniform vec4 castle_MaterialBaseAlpha;
uniform vec3 castle_MaterialEmissive;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  vec4 material_base_alpha;

  #ifdef COLOR_PER_VERTEX
  material_base_alpha = castle_ColorPerVertexFragment;
  #else
  material_base_alpha = castle_MaterialBaseAlpha;
  #endif

  main_texture_apply(material_base_alpha, normal_eye);

  vec3 emissive = castle_MaterialEmissive;
  /* PLUG: material_emissive (emissive) */

  result = vec4(emissive, material_base_alpha.a);

  /* PLUG: add_light (result, vertex_eye, normal_eye, material_base_alpha) */

  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.
     This isn't really mandatory, but scenes with many lights could easily
     have colors > 1 and then the textures will look "burned out".
     Of course, for future HDR rendering we will turn this off. */
  result.rgb = min(result.rgb, 1.0);
}
