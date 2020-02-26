uniform vec4 castle_UnlitColor;

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  result = castle_UnlitColor;
  #ifdef COLOR_PER_VERTEX
  result *= castle_ColorPerVertex;
  #endif
}
