/* Phong shading GLSL vertex shader.
   Used by ../castlerendererinternalshader.pas to construct the final shader.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
uniform mat3 castle_NormalMatrix;
attribute vec4 castle_Vertex;
attribute vec3 castle_Normal;

/* PLUG-DECLARATIONS */

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;

#ifdef COLOR_PER_VERTEX
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
  vec3 normal_object = castle_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;
  /* Although we will normalize it again in the fragment shader
     (otherwise interpolated result could be shorter < 1, imagine a case
     when normals point the almost opposite directions on the opposite
     vertexes), we also have to normalize it in vertex shader (otherwise
     a much longer normal on one vertex would pull all the interpolated
     normals, thus making their direction invalid in fragment shaders). */
  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);

  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */

  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;
}
