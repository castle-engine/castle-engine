{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/skin_animation.vs" }
'/*' + LineEnding +
'  Perform skin animation on GPU.' + LineEnding +
'  See https://castle-engine.io/skin,' + LineEnding +
'  the implementation is almost literally taken from' + LineEnding +
'  https://www.khronos.org/files/gltf20-reference-guide.pdf' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'/* Note: We need to specify array size, otherwise we get' + LineEnding +
'   "error C7559: OpenGL requires constant indexes for unsized array access(jointMatrix)"' + LineEnding +
'*/' + LineEnding +
'uniform mat4 jointMatrix[128];' + LineEnding +
'attribute vec4 skinJoints0;' + LineEnding +
'attribute vec4 skinWeights0;' + LineEnding +
'' + LineEnding +
'void PLUG_vertex_object_space_change(' + LineEnding +
'  inout vec4 vertex_object,' + LineEnding +
'  inout vec3 normal_object)' + LineEnding +
'{' + LineEnding +
'  mat4 skinMatrix =' + LineEnding +
'    skinWeights0.x * jointMatrix[int(skinJoints0.x)] +' + LineEnding +
'    skinWeights0.y * jointMatrix[int(skinJoints0.y)] +' + LineEnding +
'    skinWeights0.z * jointMatrix[int(skinJoints0.z)] +' + LineEnding +
'    skinWeights0.w * jointMatrix[int(skinJoints0.w)];' + LineEnding +
'  vertex_object = skinMatrix * vertex_object;' + LineEnding +
'  normal_object = mat3(skinMatrix) * normal_object;' + LineEnding +
'}' + LineEnding +
''
