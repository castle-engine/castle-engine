/* Additional code to make Gouraud shading work with geometry shaders.
   We need to pass the castle_Color.
*/

in vec4 castle_Color[CASTLE_GEOMETRY_INPUT_SIZE];
out vec4 castle_Color_geoshader;

void PLUG_geometry_vertex_set(const int index)
{
  castle_Color_geoshader = castle_Color[index];
}

void PLUG_geometry_vertex_zero()
{
  castle_Color_geoshader = vec4(0.0);
}

void PLUG_geometry_vertex_add(const int index, const float scale)
{
  castle_Color_geoshader += castle_Color[index] * scale;
}
