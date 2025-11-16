/*
  Perform skin animation on GPU.
  See https://castle-engine.io/skin,
  the implementation is almost literally taken from
  https://www.khronos.org/files/gltf20-reference-guide.pdf
*/

/* Note: We need to specify array size, otherwise we get
   "error C7559: OpenGL requires constant indexes for unsized array access(castle_JointMatrix)"
*/
uniform mat4 castle_JointMatrix[CASTLE_MAX_SKIN_JOINTS];
attribute vec4 castle_SkinJoints0;
attribute vec4 castle_SkinWeights0;

/* Calculated and used by vertex_object_space_change below,
   potentially used again by tangent_object_space */
mat4 skinMatrix;

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
      castle_JointMatrix[0]
      :
      castle_SkinWeights0.x * castle_JointMatrix[int(castle_SkinJoints0.x)] +
      castle_SkinWeights0.y * castle_JointMatrix[int(castle_SkinJoints0.y)] +
      castle_SkinWeights0.z * castle_JointMatrix[int(castle_SkinJoints0.z)] +
      castle_SkinWeights0.w * castle_JointMatrix[int(castle_SkinJoints0.w)]
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
