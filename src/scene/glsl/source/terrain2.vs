/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,
   see https://castle-engine.io/compositing_shaders.php ),
   applied over terrain.

   This simply saves position and normal in object space,
   to be used by terrain.fs code. */

varying vec3 terrain_position;
varying vec3 terrain_normal;

uniform sampler2D heightTexture;
uniform vec2 terrain_size;

void PLUG_vertex_object_space_change(
  inout vec4 vertex_object,
  inout vec3 normal_object)
{
  //if (vertex_object.x < 50.)
  //  vertex_object.y = 32.0;
  vec2 texCoord = vec2(vertex_object.x/terrain_size.x, 1 - vertex_object.z/terrain_size.y);
  vertex_object.y = texture2D(heightTexture, texCoord).r * 10.0;
  

  terrain_position = vec3(vertex_object);
  terrain_normal = normal_object;
}
