/* Default geometry shader code.
   Defines geometryVertexXxx functions,
   to be called from user code, and respective geometry_vertex_xxx plugs,
   to be extended by user (and internal) effects.

   It does not define the main() entry point for geometry shaders.
   User will have to provide it (in ComposedShader or Effect),
   or we will discard partial geometry shader code. */

/* Our implementation supports only geometry shaders for GLSL >= 1.50.
   Add "compatibility" profile, as (for now) we want to be able to pass
   also deprecated gl_Xxx from vertex to fragment stages. */
#version 150 compatibility

/* PLUG-DECLARATIONS */

/* Standard varying, always used by our shaders */
in vec4 castle_vertex_eye[CASTLE_GEOMETRY_INPUT_SIZE];
out vec4 castle_vertex_eye_geoshader;
in vec3 castle_normal_eye[CASTLE_GEOMETRY_INPUT_SIZE];
out vec3 castle_normal_eye_geoshader;

void geometryVertexSet(const int index)
{
  castle_vertex_eye_geoshader = castle_vertex_eye[index];
  castle_normal_eye_geoshader = castle_normal_eye[index];
  /* PLUG: geometry_vertex_set (index) */
}

void geometryVertexZero()
{
  castle_vertex_eye_geoshader = vec4(0.0);
  castle_normal_eye_geoshader = vec3(0.0);
  /* PLUG: geometry_vertex_zero () */
}

void geometryVertexAdd(const int index, const float scale)
{
  castle_vertex_eye_geoshader += castle_vertex_eye[index] * scale;
  castle_normal_eye_geoshader += castle_normal_eye[index] * scale;
  /* PLUG: geometry_vertex_add (index, scale) */
}
