/* Basic geometry shader.
   It doesn't contain main() --- it will have to be provided by user.
   This only defines geometryVertexXxx functions,
   to be called by user, and respective geometry_vertex_xxx plugs,
   to be extended by user (and internal) effects. */

/* Our implementation supports only geometry shaders for GLSL >= 1.50.
   Add "compatibility" profile, as (for now) we want to be able to pass
   also deprecated gl_Xxx from vertex to fragment stages. */
#version 150 compatibility

/* PLUG-DECLARATIONS */

void geometryVertexSet(const int index)
{
  /* PLUG: geometry_vertex_set (index) */
}

void geometryVertexZero()
{
  /* PLUG: geometry_vertex_zero () */
}

void geometryVertexAdd(const int index, const float scale)
{
  /* PLUG: geometry_vertex_add (index, scale) */
}
