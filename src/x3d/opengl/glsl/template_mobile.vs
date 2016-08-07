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
/* Color summed with all the lights.
   Like gl_Front/BackLightModelProduct.sceneColor:
   material emissive color + material ambient color * global (light model) ambient.
*/
uniform vec3 castle_SceneColor;
uniform vec4 castle_UnlitColor;

#ifdef COLOR_PER_VERTEX
attribute vec4 castle_ColorPerVertex;
#endif

void main(void)
{
  vec4 vertex_object = castle_Vertex;
  vec3 normal_object = castle_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  #ifdef CASTLE_BUGGY_GLSL_READ_VARYING
  /* use local variables, instead of reading + writing to varying variables,
     when VARYING_NOT_READABLE */
  vec4 temp_castle_vertex_eye;
  vec3 temp_castle_normal_eye;
  vec4 temp_castle_Color;
  #define castle_vertex_eye temp_castle_vertex_eye
  #define castle_normal_eye temp_castle_normal_eye
  #define castle_Color      temp_castle_Color
  #endif

  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;
  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);

  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */

#ifdef LIT
  castle_Color = vec4(castle_SceneColor, 1.0);
  /* PLUG: add_light_contribution (castle_Color, castle_vertex_eye, castle_normal_eye, castle_MaterialShininess) */
  castle_Color.a = castle_MaterialDiffuseAlpha;

  /* Clamp sum of lights colors to be <= 1. See template.fs for comments. */
  castle_Color.rgb = min(castle_Color.rgb, 1.0);
#else
  castle_Color = castle_UnlitColor
#ifdef COLOR_PER_VERTEX
    * castle_ColorPerVertex
#endif
  ;
#endif

  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;

  #ifdef CASTLE_BUGGY_GLSL_READ_VARYING
  #undef castle_vertex_eye
  #undef castle_normal_eye
  #undef castle_Color
  castle_vertex_eye = temp_castle_vertex_eye;
  castle_normal_eye = temp_castle_normal_eye;
  castle_Color      = temp_castle_Color;
  #endif
}
