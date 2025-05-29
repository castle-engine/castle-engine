{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/shader_libraries/EyeWorldSpace.glsl" }
'uniform mat4 castle_CameraMatrix;' + LineEnding +
'uniform mat4 castle_CameraInverseMatrix;' + LineEnding +
'uniform mat3 castle_CameraRotationMatrix;' + LineEnding +
'uniform mat3 castle_CameraRotationInverseMatrix;' + LineEnding +
'' + LineEnding +
'vec4 position_world_to_eye_space(vec4 position_world)' + LineEnding +
'{' + LineEnding +
'  return castle_CameraMatrix * position_world;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec4 position_eye_to_world_space(vec4 position_eye)' + LineEnding +
'{' + LineEnding +
'  return castle_CameraInverseMatrix * position_eye;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec3 direction_world_to_eye_space(vec3 direction_world)' + LineEnding +
'{' + LineEnding +
'  return castle_CameraRotationMatrix * direction_world;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec3 direction_eye_to_world_space(vec3 direction_eye)' + LineEnding +
'{' + LineEnding +
'  return castle_CameraRotationInverseMatrix * direction_eye;' + LineEnding +
'}' + LineEnding +
''
