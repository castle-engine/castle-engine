/* Use the plug vertex_object_space to pass the vertex position in object
   space to the fragment shader.
*/

varying vec4 water_vertex_object;

void PLUG_vertex_object_space(const in vec4 vertex_object, inout vec3 normal_object)
{
  water_vertex_object = vertex_object;
}
