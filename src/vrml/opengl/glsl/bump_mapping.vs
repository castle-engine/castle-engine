/* GLSL vertex shader to do bump mapping.

   This is converted to glsl_bump_mapping.vs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

uniform vec3 light_position_world_space;
uniform mat4 world_space_to_object;

attribute mat3 object_space_to_tangent;
attribute vec2 tex_coord;

varying vec3 light_dir_tangent;

void main(void)
{
  gl_TexCoord[0] = gl_TextureMatrix[0] * vec4(tex_coord, 0.0, 1.0);

  /* Calculate light_dir_tangent, which is crucial for bump mapping. */
  vec3 light_position_object_space =
    vec3(world_space_to_object * vec4(light_position_world_space, 1));
  vec3 light_dir_object_space = light_position_object_space - vec3(gl_Vertex);
  light_dir_tangent = object_space_to_tangent * light_dir_object_space;

  gl_Position = ftransform();
}
