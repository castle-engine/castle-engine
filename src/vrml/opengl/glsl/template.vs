/* Generic GLSL vertex shader template. It will be used
   by vrmlshadergenerator.pas to construct final shader.

   This is converted to template.vs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* PLUG: $declare-variables$ */
/* PLUG: $declare-procedures$ */

varying vec4 vertex_eye;
varying vec3 normal_eye;

void main(void)
{
  vec4 vertex_object = gl_Vertex;
  vec3 normal_object = gl_Normal;
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  vertex_eye = gl_ModelViewMatrix * vertex_object;
  /* Yes, we need to normalize here,
     and then fragment shader will also normalize?
     TODO: think, make sure. */
  normal_eye = normalize(gl_NormalMatrix * normal_object);

  /* PLUG: vertex_process (vertex_eye, normal_eye) */
  /* (const in vec4 vertex_eye, const in vec3 normal_eye) */

#ifdef VERTEX_OBJECT_SPACE_CHANGED
  gl_Position = gl_ProjectionMatrix * vertex_eye;
#else
  gl_Position = ftransform();
#endif
}
