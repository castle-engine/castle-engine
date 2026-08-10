uniform mat4 castle_CameraMatrix;
uniform mat4 castle_CameraInverseMatrix;
uniform mat3 castle_CameraRotationMatrix;
uniform mat3 castle_CameraRotationInverseMatrix;

vec4 position_world_to_eye_space(vec4 position_world)
{
  return castle_CameraMatrix * position_world;
}

vec4 position_eye_to_world_space(vec4 position_eye)
{
  return castle_CameraInverseMatrix * position_eye;
}

vec3 direction_world_to_eye_space(vec3 direction_world)
{
  return castle_CameraRotationMatrix * direction_world;
}

vec3 direction_eye_to_world_space(vec3 direction_eye)
{
  return castle_CameraRotationInverseMatrix * direction_eye;
}
