uniform vec4 castle_UnlitColor;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  result = castle_UnlitColor;
  /* In case of UnlitMaterial, Color/ColorRGBA is mixed with emissive color.
     TODO: Should this be replace? */
  #ifdef COLOR_PER_VERTEX
  result *= castle_ColorPerVertexFragment;
  #endif
  /* In case of UnlitMaterial, main texture (emissiveTexture or Appearance.texture)
     is mixed with emissive color. */
  main_texture_apply(result, normal_eye);
}
