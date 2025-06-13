/*
  Perform skin animation on GPU.
  See https://castle-engine.io/skin,
  the implementation is almost literally taken from
  https://www.khronos.org/files/gltf20-reference-guide.pdf
*/

/* Note: We need to specify array size, otherwise we get
   "error C7559: OpenGL requires constant indexes for unsized array access(castle_JointMatrix)"
*/
uniform mat4 castle_JointMatrix[128];
attribute vec4 castle_SkinJoints0;
attribute vec4 castle_SkinWeights0;

void PLUG_vertex_object_space_change(
  inout vec4 vertex_object,
  inout vec3 normal_object)
{
  mat4 skinMatrix =
    castle_SkinWeights0.x * castle_JointMatrix[int(castle_SkinJoints0.x)] +
    castle_SkinWeights0.y * castle_JointMatrix[int(castle_SkinJoints0.y)] +
    castle_SkinWeights0.z * castle_JointMatrix[int(castle_SkinJoints0.z)] +
    castle_SkinWeights0.w * castle_JointMatrix[int(castle_SkinJoints0.w)];
  vertex_object = skinMatrix * vertex_object;
  normal_object = mat3(skinMatrix) * normal_object;
}
