/* Generic GLSL vertex shader, used on OpenGL ES. */

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
uniform mat3 castle_NormalMatrix;
attribute vec4 castle_Vertex;
attribute vec3 castle_Normal;

/* PLUG-DECLARATIONS */

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;
varying vec4 castle_Color;

uniform float castle_MaterialDiffuseAlpha;
uniform float castle_MaterialShininess;

void main(void)
{
  vec4 vertex_object = castle_Vertex;
  vec3 normal_object = castle_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;
  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);

  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */

#ifdef LIT
  castle_Color = vec4(0.0, 0.0, 0.0, castle_MaterialDiffuseAlpha);
  /* PLUG: add_light_contribution (castle_Color, castle_vertex_eye, castle_normal_eye, castle_MaterialShininess) */
  castle_Color.a = castle_MaterialDiffuseAlpha;

  /* Clamp sum of lights colors to be <= 1. See template.fs for comments. */
  castle_Color.rgb = min(castle_Color.rgb, 1.0);
#else
  castle_Color = vec4(1.0, 1.0, 1.0, 1.0);
#endif

  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;
}
