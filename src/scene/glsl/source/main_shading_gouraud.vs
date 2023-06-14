/* Gouraud shading GLSL vertex shader. */

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
  castle_normal_eye =
    #if defined(CASTLE_HAS_NORMALS)
    normalize(castle_NormalMatrix * normal_object);
    #else
    vec3(0.0, 0.0, 1.0);
    #endif

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
