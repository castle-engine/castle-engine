/*
  Perform skin animation on GPU.
  See https://castle-engine.io/skin .
*/

#ifdef CASTLE_JOINT_TEXTURE
  uniform sampler2D castle_JointTexture;
#else
  /* Note: We need to specify array size, otherwise we get
    "error C7559: OpenGL requires constant indexes for unsized array access(castle_JointMatrix)"
  */
  uniform mat4 castle_JointMatrix[CASTLE_MAX_SKIN_JOINTS];
#endif

attribute vec4 castle_SkinJoints0;
attribute vec4 castle_SkinWeights0;

/* Calculated and used by vertex_object_space_change below,
   potentially used again by tangent_object_space */
mat4 skinMatrix;

#ifdef CASTLE_JOINT_TEXTURE
mat4 getJointMatrix(int jointIndex)
{
  return mat4(
    texelFetch(castle_JointTexture, ivec2(0, jointIndex), 0),
    texelFetch(castle_JointTexture, ivec2(1, jointIndex), 0),
    texelFetch(castle_JointTexture, ivec2(2, jointIndex), 0),
    texelFetch(castle_JointTexture, ivec2(3, jointIndex), 0)
  );
}
#else
mat4 getJointMatrix(int jointIndex)
{
  return castle_JointMatrix[jointIndex];
}
#endif

void PLUG_vertex_object_space_change(
  inout vec4 vertex_object,
  inout vec3 normal_object)
{
  skinMatrix =
    /* Special case to handle weights=(0,0,0,0) to workaround Blender -> glTF
       exporter bug. See for explanation and testcase:
       https://github.com/castle-engine/demo-models/tree/master/animation/blender_skinned_animation/blender_zero_weights_bug */
    ( castle_SkinWeights0 == vec4(0.0)
      ?
      getJointMatrix(0)
      :
      /* This is the core of the implementation, where 4 joints
         with their 4 weights determine the final skinMatrix
         that determines the vertex position.
         This implementation follows
         https://www.khronos.org/files/gltf20-reference-guide.pdf
      */
      castle_SkinWeights0.x * getJointMatrix(int(castle_SkinJoints0.x)) +
      castle_SkinWeights0.y * getJointMatrix(int(castle_SkinJoints0.y)) +
      castle_SkinWeights0.z * getJointMatrix(int(castle_SkinJoints0.z)) +
      castle_SkinWeights0.w * getJointMatrix(int(castle_SkinJoints0.w))
    );
  vertex_object = skinMatrix * vertex_object;
  normal_object = mat3(skinMatrix) * normal_object;
}

void PLUG_tangent_object_space(inout vec4 tangent_object)
{
  /* Modify tangent_object.xyz (direction),
     leaving tangent_object.w (handedness) unchanged.

     We need to animate tangent direction, since we also animate normals,
     when the skinned animation is active.
     The visual difference is subtle, but is there,
     testcase: lizard hands when "walk" plays in
     demo-models/bump_mapping/lizardman/lizardman.gltf .

     See also
     https://github.com/KhronosGroup/glTF-Sample-Viewer , actually
     https://github.com/KhronosGroup/glTF-Sample-Renderer , animating tangents
     in
     https://github.com/KhronosGroup/glTF-Sample-Renderer/blob/4deade77ce977dcd1e7918c949c2289e80eac365/source/Renderer/shaders/primitive.vert#L102 .
  */
  tangent_object.xyz = mat3(skinMatrix) * tangent_object.xyz;
}
