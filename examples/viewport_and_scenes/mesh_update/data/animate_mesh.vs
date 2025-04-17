/* Shader effect, in GLSL language, to animate the mesh.
   This shader changes the vertex position (in object space)
   based on the time, to make a simple wave-like animation.

   This piece of shader code depends on Castle Game Engine
   "Shader Effects" feature documented on
   https://castle-engine.io/compositing_shaders_doc/html/ .
   The routine name PLUG_vertex_object_space_change is special --
   it will be recognized by Castle Game Engine and called during regular
   CGE shaders.
*/

uniform float time;

/* Process the 3D vertex position, changing vertex Y.
   This is equivalent to what Pascal calculation in
   TViewMain.UpdateCoordinateNode is doing, when we animate mesh without
   shader. */
vec3 process_vertex(vec3 position)
{
  return vec3(
    position.x,
    sin((position.x + time * 20.0) * 0.5) +
    sin((position.z + time * 15.0) * 0.3) * 0.5,
    position.z);
}

void PLUG_vertex_object_space_change(
  inout vec4 vertex_object,
  inout vec3 normal_object)
{
  /* We assume here vertex_object.w == 1.0, so we don't change it.
     This is true for the normal vertex shader input from TCoordinateNode
     where vertex is defined in 3D coordinates.
     So we only read/write "vertex_object.xyz". */
  vertex_object.xyz = process_vertex(vertex_object.xyz);

  /* We have to also update normal_object, otherwise light on the object
     will be wrong -- original normals assume a flat surface, not wavy. */
  const float shift = 0.1;
  normal_object = normalize(cross(
    process_vertex(vertex_object.xyz + vec3(0, 0, shift)) - vertex_object.xyz,
    process_vertex(vertex_object.xyz + vec3(shift, 0, 0)) - vertex_object.xyz));
}
