/* Generic GLSL vertex shader, used on OpenGL ES. */

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
attribute vec4 castle_Vertex;

/* PLUG-DECLARATIONS */

varying vec4 castle_vertex_eye;
//varying vec3 castle_normal_eye;

void main(void)
{
  vec4 vertex_object = castle_Vertex;
  //vec3 normal_object = castle_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;
  //castle_normal_eye = normalize(castle_NormalMatrix * normal_object);

  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */

  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;
}
