/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,
   see https://castle-engine.io/compositing_shaders.php ),
   applied over terrain.

   This simply saves position and normal in object space,
   to be used by terrain.fs code. */

varying vec3 terrain_position;
varying vec3 terrain_normal;

void PLUG_vertex_object_space(
  const in vec4 vertex_object, const in vec3 normal_object)
{
  terrain_position = vec3(vertex_object);
  terrain_normal = normal_object;
}
