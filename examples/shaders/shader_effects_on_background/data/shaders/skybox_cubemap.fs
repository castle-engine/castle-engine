uniform samplerCube test_cube_map;

vec3 direction_eye_to_world_space(vec3 direction_eye);

// Save value obtained in PLUG_fragment_eye_space to use in PLUG_fragment_modify.
vec3 vertex_dir_world;

// Get the direction to the vertex in world space.
void PLUG_fragment_eye_space(
  const vec4 vertex_eye,
  inout vec3 normal_eye)
{
  // In eye space, the camera is at (0,0,0),
  // so vertex_eye.xyz is just direction to the vertex (in eye space).
  // We only need to convert it to world space.
  vertex_dir_world = direction_eye_to_world_space(vertex_eye.xyz);
}

void PLUG_fragment_modify(inout vec4 fragment_color)
{
  // Quick test that vertex_dir_world makes sense.
  //fragment_color.rgb = vertex_dir_world;

  // Swap y and z to match the cube map orientation.
  // The reason for doing this is just the oddity of the cube map in test_cubemap.dds,
  // it assumes "up" is in the Z, not Y.
  vec3 vertex_dir_for_cubemap = vertex_dir_world.zxy;

  vec4 cube_map_color = textureCube(test_cube_map, vertex_dir_for_cubemap);
  fragment_color.rgb = mix(fragment_color.rgb, cube_map_color.rgb,
    // 0.5);
    // mix with non-equal weights for ever weirder effect.
    vec3(0.7, 0.3, 0.1));
}
