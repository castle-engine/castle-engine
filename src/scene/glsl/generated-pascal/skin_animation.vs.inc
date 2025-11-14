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
'   "error C7559: OpenGL requires constant indexes for unsized array access(castle_JointMatrix)"' + LineEnding +
'*/' + LineEnding +
'uniform mat4 castle_JointMatrix[CASTLE_MAX_SKIN_JOINTS];' + LineEnding +
'attribute vec4 castle_SkinJoints0;' + LineEnding +
'attribute vec4 castle_SkinWeights0;' + LineEnding +
'' + LineEnding +
'void PLUG_vertex_object_space_change(' + LineEnding +
'  inout vec4 vertex_object,' + LineEnding +
'  inout vec3 normal_object)' + LineEnding +
'{' + LineEnding +
'  mat4 skinMatrix =' + LineEnding +
'    /* Special case to handle weights=(0,0,0,0) to workaround Blender -> glTF' + LineEnding +
'       exporter bug. See for explanation and testcase:' + LineEnding +
'       https://github.com/castle-engine/demo-models/tree/master/animation/blender_skinned_animation/blender_zero_weights_bug */' + LineEnding +
'    ( castle_SkinWeights0 == vec4(0.0)' + LineEnding +
'      ?' + LineEnding +
'      castle_JointMatrix[0]' + LineEnding +
'      :' + LineEnding +
'      castle_SkinWeights0.x * castle_JointMatrix[int(castle_SkinJoints0.x)] +' + LineEnding +
'      castle_SkinWeights0.y * castle_JointMatrix[int(castle_SkinJoints0.y)] +' + LineEnding +
'      castle_SkinWeights0.z * castle_JointMatrix[int(castle_SkinJoints0.z)] +' + LineEnding +
'      castle_SkinWeights0.w * castle_JointMatrix[int(castle_SkinJoints0.w)]' + LineEnding +
'    );' + LineEnding +
'  vertex_object = skinMatrix * vertex_object;' + LineEnding +
'  normal_object = mat3(skinMatrix) * normal_object;' + LineEnding +
'}' + LineEnding +
''
