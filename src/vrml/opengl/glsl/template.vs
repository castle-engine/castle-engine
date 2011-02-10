/* Generic GLSL vertex shader template. It will be used
   by vrmlshadergenerator.pas to construct final shader.

   This is converted to template.vs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

/* PLUG: vertex-declare declaration */

varying vec4 vertex_eye;
varying vec3 normal_eye;

void main(void)
{
  vertex_eye = gl_ModelViewMatrix * gl_Vertex;
  /* Yes, we need to normalize here,
     and then fragment shader will also normalize?
     TODO: think, make sure. */
  normal_eye = normalize(gl_NormalMatrix * gl_Normal);

  /* PLUG: vertex-process void %s(const in vec4 vertex_eye, const in vec3 normal_eye) */

  gl_Position = ftransform();
}
