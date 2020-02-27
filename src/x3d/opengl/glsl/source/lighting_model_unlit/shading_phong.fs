uniform vec4 castle_MaterialEmissiveAlpha;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  #ifdef COLOR_PER_VERTEX
  /* In case of UnlitMaterial, Color/ColorRGBA replaces emissive color.*/
  result = castle_ColorPerVertexFragment;
  #else
  result = castle_MaterialEmissiveAlpha;
  #endif

  /* In case of UnlitMaterial, main texture (emissiveTexture or Appearance.texture)
     is mixed with emissive color. */
  main_texture_apply(result, normal_eye);
}
