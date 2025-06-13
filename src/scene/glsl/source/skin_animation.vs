/*
  Perform skin animation on GPU.
  See https://castle-engine.io/skin,
  the implementation is almost literally taken from
  https://www.khronos.org/files/gltf20-reference-guide.pdf
*/

/* Note: We need to specify array size, otherwise we get
   "error C7559: OpenGL requires constant indexes for unsized array access(jointMatrix)"
*/
uniform mat4 jointMatrix[128];
attribute vec4 skinJoints0;
attribute vec4 skinWeights0;

void PLUG_vertex_object_space_change(
  inout vec4 vertex_object,
  inout vec3 normal_object)
{
  mat4 skinMatrix =
    skinWeights0.x * jointMatrix[int(skinJoints0.x)] +
    skinWeights0.y * jointMatrix[int(skinJoints0.y)] +
    skinWeights0.z * jointMatrix[int(skinJoints0.z)] +
    skinWeights0.w * jointMatrix[int(skinJoints0.w)];
  vertex_object = skinMatrix * vertex_object;
  normal_object = mat3(skinMatrix) * normal_object;
}
