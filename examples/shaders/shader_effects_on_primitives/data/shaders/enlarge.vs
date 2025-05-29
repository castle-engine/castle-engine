// Modify the vertex to make object appear larger.
// See https://castle-engine.io/shaders .

void PLUG_vertex_object_space_change(
  inout vec4 vertex_object,
  inout vec3 normal_object)
{
  float enlarge_factor = 1.2;
  vertex_object.xyz += normal_object * enlarge_factor;
}
