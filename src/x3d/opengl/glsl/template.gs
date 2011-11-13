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
