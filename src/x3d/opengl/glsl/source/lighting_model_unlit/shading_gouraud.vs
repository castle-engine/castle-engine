uniform vec4 castle_MaterialEmissiveAlpha;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  result =
    #if defined(COLOR_PER_VERTEX_REPLACE)
    /* In case of UnlitMaterial, Color/ColorRGBA replaces emissive color.*/
    castle_ColorPerVertex;
    #elif defined(COLOR_PER_VERTEX_MODULATE)
    castle_ColorPerVertex * castle_MaterialEmissiveAlpha;
    #else
    castle_MaterialEmissiveAlpha;
    #endif
}
