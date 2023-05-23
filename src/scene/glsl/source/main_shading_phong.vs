/* Phong shading GLSL vertex shader.
   Used by ../castlerendererinternalshader.pas to construct the final shader.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* Plug into PLUG-DECLARATIONS-EARLY things that need to be defined
   before uniforms below, like definition of CASTLE_HAS_NORMALS.
   TODO: Can we unify this with PLUG-DECLARATIONS? */
/* PLUG-DECLARATIONS-EARLY */

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
attribute vec4 castle_Vertex;

#if defined(CASTLE_HAS_NORMALS)
uniform mat3 castle_NormalMatrix;
attribute vec3 castle_Normal;
#endif

/* PLUG-DECLARATIONS */

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;

#if defined(COLOR_PER_VERTEX_RGB)
attribute vec3 castle_ColorPerVertex;
varying vec3 castle_ColorPerVertexFragment;
#elif defined(COLOR_PER_VERTEX_RGB_ALPHA)
attribute vec4 castle_ColorPerVertex;
varying vec4 castle_ColorPerVertexFragment;
#endif

/* Include fragment shader utilities used by both Gouraud and Phong shading. */
/* CASTLE-COMMON-CODE */

void main(void)
{
  #ifdef COLOR_PER_VERTEX
  castle_ColorPerVertexFragment = castle_ColorPerVertex;
  #endif

  vec4 vertex_object = castle_Vertex;
  vec3 normal_object =
    #if defined(CASTLE_HAS_NORMALS)
    castle_Normal;
    #else
    /* When CASTLE_HAS_NORMALS not defined, then TShader.NeedsNormals = false.
       Renderer may then not define castle_Normal attribute at all,
       so we cannot use it (using it causes invisible objects on ATI GPUs,
       even though undefined normal_object value is not used by anything;
       see
       https://github.com/castle-engine/castle-engine/issues/462
       https://trello.com/c/QH9d9A8o/92-bug-unable-to-see-and-use-gizmos )
    */
    vec3(0.0, 0.0, 1.0);
    #endif

  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;

  castle_normal_eye =
    #if defined(CASTLE_HAS_NORMALS)
    /* Although we will normalize it again in the fragment shader
       (otherwise interpolated result could be shorter < 1, imagine a case
       when normals point the almost opposite directions on the opposite
       vertexes), we also have to normalize it in vertex shader (otherwise
       a much longer normal on one vertex would pull all the interpolated
       normals, thus making their direction invalid in fragment shaders). */
    normalize(castle_NormalMatrix * normal_object);
    #else
    vec3(0.0, 0.0, 1.0);
    #endif

  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */

  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;
}
