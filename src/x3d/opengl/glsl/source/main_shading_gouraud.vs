/* Gouraud shading GLSL vertex shader. */

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
uniform mat3 castle_NormalMatrix;
attribute vec4 castle_Vertex;
attribute vec3 castle_Normal;

/* PLUG-DECLARATIONS */

varying vec4 castle_vertex_eye;
varying vec3 castle_normal_eye;
varying vec4 castle_Color;

#if defined(COLOR_PER_VERTEX_RGB)
attribute vec3 castle_ColorPerVertex;
#elif defined(COLOR_PER_VERTEX_RGB_ALPHA)
attribute vec4 castle_ColorPerVertex;
#endif

/* Apply per-vertex color, over the base/diffuse/emissive color + alpha. */
vec4 castle_apply_color_per_vertex(vec4 color)
{
  return
    #if defined(COLOR_PER_VERTEX_REPLACE)
      #if defined(COLOR_PER_VERTEX_RGB)
      vec4(castle_ColorPerVertex, color.a);
      #elif defined(COLOR_PER_VERTEX_RGB_ALPHA)
      castle_ColorPerVertex;
      #endif
    #elif defined(COLOR_PER_VERTEX_MODULATE)
      #if defined(COLOR_PER_VERTEX_RGB)
      vec4(castle_ColorPerVertex * color.rgb, color.a);
      #elif defined(COLOR_PER_VERTEX_RGB_ALPHA)
      castle_ColorPerVertex * color;
      #endif
    #else
    color;
    #endif
}

/* Include fragment shader utilities used by both Gouraud and Phong shading. */
/* CASTLE-COMMON-CODE */

/* CASTLE-LIGHTING-MODEL */

void main(void)
{
  vec4 vertex_object = castle_Vertex;
  vec3 normal_object = castle_Normal;
  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */
  /* PLUG: vertex_object_space (vertex_object, normal_object) */

  #ifdef CASTLE_BUGGY_GLSL_READ_VARYING
  /* use local variables, instead of reading + writing to varying variables,
     in this case */
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

  calculate_lighting(castle_Color, castle_vertex_eye, castle_normal_eye);

  /* PLUG: lighting_apply (castle_Color, castle_vertex_eye, castle_normal_eye) */

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
