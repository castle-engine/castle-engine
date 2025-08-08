/* Shader effect for a terrain.
   Modified version of terrain.vs that uses heightTexture
   to move the vertexes to show the actual geometry of the terrain. */

varying vec3 terrain_position;
varying vec3 terrain_normal;

uniform sampler2D heightTexture;
uniform vec2 terrain_size;

void PLUG_vertex_object_space_change(
  inout vec4 vertex_object,
  inout vec3 normal_object)
{
  vec2 texCoord = vec2(vertex_object.x/terrain_size.x, 1 - vertex_object.z/terrain_size.y);
  vertex_object.y = texture2D(heightTexture, texCoord).r * 10.0;

  terrain_position = vec3(vertex_object);
  terrain_normal = normal_object;
}
