/* Vertex shader.
   Just a helper for texture_effect.fs (fragment shader):
   this passes "pass_vertex_object" information to fragment shader.
*/

// Vertex coordinates in object-space coordinate system.
varying vec4 my_vertex_object;

void PLUG_vertex_object_space(
  const in vec4 vertex_object,
  inout vec3 normal_object)
{
  my_vertex_object = vertex_object;
}
