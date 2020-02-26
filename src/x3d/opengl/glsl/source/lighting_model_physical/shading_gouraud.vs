uniform vec4 castle_MaterialBaseAlpha;
uniform vec3 castle_EmissiveColor;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  vec4 material_base_alpha;

  #ifdef COLOR_PER_VERTEX
  material_base_alpha = castle_ColorPerVertex;
  #else
  material_base_alpha = castle_MaterialBaseAlpha;
  #endif

  result = vec4(castle_EmissiveColor, material_base_alpha.a);

  /* PLUG: add_light (result, vertex_eye, normal_eye, material_base_alpha) */

  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */
  result.rgb = min(result.rgb, 1.0);
}
