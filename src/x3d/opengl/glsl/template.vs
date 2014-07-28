/* Generic GLSL vertex shader.
   Used by ../castlerendererinternalshader.pas to construct the final shader.

   This is converted to template.vs.inc, and is then compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* PLUG-DECLARATIONS */

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;

void main(void)
{
  vec4 vertex_object = gl_Vertex;
  vec3 normal_object = gl_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  castle_vertex_eye = gl_ModelViewMatrix * vertex_object;
  /* Although we will normalize it again in the fragment shader
     (otherwise interpolated result could be shorter < 1, imagine a case
     when normals point the almost opposite directions on the opposite
     vertexes), we also have to normalize it in vertex shader (otherwise
     a much longer normal on one vertex would pull all the interpolated
     normals, thus making their direction invalid in fragment shaders). */
  castle_normal_eye = normalize(gl_NormalMatrix * normal_object);

  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */

#ifndef LIT
  gl_FrontColor = gl_Color;
  gl_BackColor = gl_Color;
#endif

#ifdef VERTEX_OBJECT_SPACE_CHANGED
  gl_Position = gl_ProjectionMatrix * castle_vertex_eye;
#else
  gl_Position = ftransform();
#endif
}
