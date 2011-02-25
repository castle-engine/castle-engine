/* Generic GLSL vertex shader template. It will be used
   by vrmlshadergenerator.pas to construct final shader.

   This is converted to template.vs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* PLUG-DECLARATIONS */

varying vec4 vertex_eye;
varying vec3 normal_eye;

void main(void)
{
  vec4 vertex_object = gl_Vertex;
  vec3 normal_object = gl_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  vertex_eye = gl_ModelViewMatrix * vertex_object;
  /* Although we will normalize it again in the fragment shader
     (otherwise interpolated result could be shorter < 1, imagine a case
     when normals point the almost opposite directions on the opposite
     vertexes), we also have to normalize it in vertex shader (otherwise
     a much longer normal on one vertex would pull all the interpolated
     normals, thus making their direction invalid in fragment shaders). */
  normal_eye = normalize(gl_NormalMatrix * normal_object);

  /* PLUG: vertex_eye_space (vertex_eye, normal_eye) */

#ifdef VERTEX_OBJECT_SPACE_CHANGED
  gl_Position = gl_ProjectionMatrix * vertex_eye;
#else
  gl_Position = ftransform();
#endif
}
