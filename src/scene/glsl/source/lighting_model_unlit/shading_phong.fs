uniform vec4 castle_MaterialEmissiveAlpha;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  /* In case of UnlitMaterial, Color/ColorRGBA replaces emissive color.*/
  result = castle_apply_color_per_vertex(castle_MaterialEmissiveAlpha);

  /* In case of UnlitMaterial, main texture (emissiveTexture or Appearance.texture)
     is mixed with emissive color. */
  main_texture_apply(result, normal_eye);

  #ifdef CASTLE_LIGHT_CONTRIBUTION_ONLY
  /* Unlit shapes are not affected by lights, the base pass draws them. */
  result.rgb = vec3(0.0);
  #endif
}
