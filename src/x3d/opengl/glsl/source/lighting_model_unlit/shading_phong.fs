uniform vec4 castle_MaterialEmissiveAlpha;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  result =
    #if defined(COLOR_PER_VERTEX_REPLACE)
    /* In case of UnlitMaterial, Color/ColorRGBA replaces emissive color.*/
    castle_ColorPerVertexFragment;
    #elif defined(COLOR_PER_VERTEX_MODULATE)
    castle_ColorPerVertexFragment * castle_MaterialEmissiveAlpha;
    #else
    castle_MaterialEmissiveAlpha;
    #endif

  /* In case of UnlitMaterial, main texture (emissiveTexture or Appearance.texture)
     is mixed with emissive color. */
  main_texture_apply(result, normal_eye);
}
