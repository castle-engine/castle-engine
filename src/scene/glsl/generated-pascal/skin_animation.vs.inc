{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/skin_animation.vs" }
'/*' + LineEnding +
'  Perform skin animation on GPU.' + LineEnding +
'  See https://castle-engine.io/skin .' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'#ifdef CASTLE_JOINT_TEXTURE' + LineEnding +
'  uniform sampler2D castle_JointTexture;' + LineEnding +
'#else' + LineEnding +
'  /* Note: We need to specify array size, otherwise we get' + LineEnding +
'    "error C7559: OpenGL requires constant indexes for unsized array access(castle_JointMatrix)"' + LineEnding +
'  */' + LineEnding +
'  uniform mat4 castle_JointMatrix[CASTLE_MAX_SKIN_JOINTS];' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'attribute vec4 castle_SkinJoints0;' + LineEnding +
'attribute vec4 castle_SkinWeights0;' + LineEnding +
'' + LineEnding +
'/* Calculated and used by vertex_object_space_change below,' + LineEnding +
'   potentially used again by tangent_object_space */' + LineEnding +
'mat4 skinMatrix;' + LineEnding +
'' + LineEnding +
'#ifdef CASTLE_JOINT_TEXTURE' + LineEnding +
'mat4 getJointMatrix(int jointIndex)' + LineEnding +
'{' + LineEnding +
'  return mat4(' + LineEnding +
'    texelFetch(castle_JointTexture, ivec2(0, jointIndex), 0),' + LineEnding +
'    texelFetch(castle_JointTexture, ivec2(1, jointIndex), 0),' + LineEnding +
'    texelFetch(castle_JointTexture, ivec2(2, jointIndex), 0),' + LineEnding +
'    texelFetch(castle_JointTexture, ivec2(3, jointIndex), 0)' + LineEnding +
'  );' + LineEnding +
'}' + LineEnding +
'#else' + LineEnding +
'mat4 getJointMatrix(int jointIndex)' + LineEnding +
'{' + LineEnding +
'  return castle_JointMatrix[jointIndex];' + LineEnding +
'}' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void PLUG_vertex_object_space_change(' + LineEnding +
'  inout vec4 vertex_object,' + LineEnding +
'  inout vec3 normal_object)' + LineEnding +
'{' + LineEnding +
'  skinMatrix =' + LineEnding +
'    /* Special case to handle weights=(0,0,0,0) to workaround Blender -> glTF' + LineEnding +
'       exporter bug. See for explanation and testcase:' + LineEnding +
'       https://github.com/castle-engine/demo-models/tree/master/animation/blender_skinned_animation/blender_zero_weights_bug */' + LineEnding +
'    ( castle_SkinWeights0 == vec4(0.0)' + LineEnding +
'      ?' + LineEnding +
'      getJointMatrix(0)' + LineEnding +
'      :' + LineEnding +
'      /* This is the core of the implementation, where 4 joints' + LineEnding +
'         with their 4 weights determine the final skinMatrix' + LineEnding +
'         that determines the vertex position.' + LineEnding +
'         This implementation follows' + LineEnding +
'         https://www.khronos.org/files/gltf20-reference-guide.pdf' + LineEnding +
'      */' + LineEnding +
'      castle_SkinWeights0.x * getJointMatrix(int(castle_SkinJoints0.x)) +' + LineEnding +
'      castle_SkinWeights0.y * getJointMatrix(int(castle_SkinJoints0.y)) +' + LineEnding +
'      castle_SkinWeights0.z * getJointMatrix(int(castle_SkinJoints0.z)) +' + LineEnding +
'      castle_SkinWeights0.w * getJointMatrix(int(castle_SkinJoints0.w))' + LineEnding +
'    );' + LineEnding +
'  vertex_object = skinMatrix * vertex_object;' + LineEnding +
'  normal_object = mat3(skinMatrix) * normal_object;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void PLUG_tangent_object_space(inout vec4 tangent_object)' + LineEnding +
'{' + LineEnding +
'  /* Modify tangent_object.xyz (direction),' + LineEnding +
'     leaving tangent_object.w (handedness) unchanged.' + LineEnding +
'' + LineEnding +
'     We need to animate tangent direction, since we also animate normals,' + LineEnding +
'     when the skinned animation is active.' + LineEnding +
'     The visual difference is subtle, but is there,' + LineEnding +
'     testcase: lizard hands when "walk" plays in' + LineEnding +
'     demo-models/bump_mapping/lizardman/lizardman.gltf .' + LineEnding +
'' + LineEnding +
'     See also' + LineEnding +
'     https://github.com/KhronosGroup/glTF-Sample-Viewer , actually' + LineEnding +
'     https://github.com/KhronosGroup/glTF-Sample-Renderer , animating tangents' + LineEnding +
'     in' + LineEnding +
'     https://github.com/KhronosGroup/glTF-Sample-Renderer/blob/4deade77ce977dcd1e7918c949c2289e80eac365/source/Renderer/shaders/primitive.vert#L102 .' + LineEnding +
'  */' + LineEnding +
'  tangent_object.xyz = mat3(skinMatrix) * tangent_object.xyz;' + LineEnding +
'}' + LineEnding +
''
