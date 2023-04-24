{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/main_shading_gouraud.vs" }
'/* Gouraud shading GLSL vertex shader. */' + LineEnding +
'' + LineEnding +
'/* Plug into PLUG-DECLARATIONS-EARLY things that need to be defined' + LineEnding +
'   before uniforms below, like definition of CASTLE_HAS_NORMALS.' + LineEnding +
'   TODO: Can we unify this with PLUG-DECLARATIONS? */' + LineEnding +
'/* PLUG-DECLARATIONS-EARLY */' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewMatrix;' + LineEnding +
'uniform mat4 castle_ProjectionMatrix;' + LineEnding +
'attribute vec4 castle_Vertex;' + LineEnding +
'' + LineEnding +
'#if defined(CASTLE_HAS_NORMALS)' + LineEnding +
'uniform mat3 castle_NormalMatrix;' + LineEnding +
'attribute vec3 castle_Normal;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'/* PLUG-DECLARATIONS */' + LineEnding +
'' + LineEnding +
'varying vec4 castle_vertex_eye;' + LineEnding +
'varying vec3 castle_normal_eye;' + LineEnding +
'varying vec4 castle_Color;' + LineEnding +
'' + LineEnding +
'#if defined(COLOR_PER_VERTEX_RGB)' + LineEnding +
'attribute vec3 castle_ColorPerVertex;' + LineEnding +
'#elif defined(COLOR_PER_VERTEX_RGB_ALPHA)' + LineEnding +
'attribute vec4 castle_ColorPerVertex;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'/* Apply per-vertex color, over the base/diffuse/emissive color + alpha. */' + LineEnding +
'vec4 castle_apply_color_per_vertex(vec4 color)' + LineEnding +
'{' + LineEnding +
'  return' + LineEnding +
'    #if defined(COLOR_PER_VERTEX_REPLACE)' + LineEnding +
'      #if defined(COLOR_PER_VERTEX_RGB)' + LineEnding +
'      vec4(castle_ColorPerVertex, color.a);' + LineEnding +
'      #elif defined(COLOR_PER_VERTEX_RGB_ALPHA)' + LineEnding +
'      castle_ColorPerVertex;' + LineEnding +
'      #endif' + LineEnding +
'    #elif defined(COLOR_PER_VERTEX_MODULATE)' + LineEnding +
'      #if defined(COLOR_PER_VERTEX_RGB)' + LineEnding +
'      vec4(castle_ColorPerVertex * color.rgb, color.a);' + LineEnding +
'      #elif defined(COLOR_PER_VERTEX_RGB_ALPHA)' + LineEnding +
'      castle_ColorPerVertex * color;' + LineEnding +
'      #endif' + LineEnding +
'    #else' + LineEnding +
'    color;' + LineEnding +
'    #endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Include fragment shader utilities used by both Gouraud and Phong shading. */' + LineEnding +
'/* CASTLE-COMMON-CODE */' + LineEnding +
'' + LineEnding +
'/* CASTLE-LIGHTING-MODEL */' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  vec4 vertex_object = castle_Vertex;' + LineEnding +
'  vec3 normal_object =' + LineEnding +
'    #if defined(CASTLE_HAS_NORMALS)' + LineEnding +
'    castle_Normal;' + LineEnding +
'    #else' + LineEnding +
'    /* When CASTLE_HAS_NORMALS not defined, then TShader.NeedsNormals = false.' + LineEnding +
'       Renderer may then not define castle_Normal attribute at all,' + LineEnding +
'       so we cannot use it (using it causes invisible objects on ATI GPUs,' + LineEnding +
'       even though undefined normal_object value is not used by anything;' + LineEnding +
'       see' + LineEnding +
'       https://github.com/castle-engine/castle-engine/issues/462' + LineEnding +
'       https://trello.com/c/QH9d9A8o/92-bug-unable-to-see-and-use-gizmos )' + LineEnding +
'    */' + LineEnding +
'    vec3(0.0, 0.0, 1.0);' + LineEnding +
'    #endif' + LineEnding +
'' + LineEnding +
'  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */' + LineEnding +
'  /* PLUG: vertex_object_space (vertex_object, normal_object) */' + LineEnding +
'' + LineEnding +
'  #ifdef CASTLE_BUGGY_GLSL_READ_VARYING' + LineEnding +
'  /* use local variables, instead of reading + writing to varying variables,' + LineEnding +
'     in this case */' + LineEnding +
'  vec4 temp_castle_vertex_eye;' + LineEnding +
'  vec3 temp_castle_normal_eye;' + LineEnding +
'  vec4 temp_castle_Color;' + LineEnding +
'  #define castle_vertex_eye temp_castle_vertex_eye' + LineEnding +
'  #define castle_normal_eye temp_castle_normal_eye' + LineEnding +
'  #define castle_Color      temp_castle_Color' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;' + LineEnding +
'  castle_normal_eye =' + LineEnding +
'    #if defined(CASTLE_HAS_NORMALS)' + LineEnding +
'    normalize(castle_NormalMatrix * normal_object);' + LineEnding +
'    #else' + LineEnding +
'    vec3(0.0, 0.0, 1.0);' + LineEnding +
'    #endif' + LineEnding +
'' + LineEnding +
'  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */' + LineEnding +
'' + LineEnding +
'  calculate_lighting(castle_Color, castle_vertex_eye, castle_normal_eye);' + LineEnding +
'' + LineEnding +
'  /* PLUG: lighting_apply (castle_Color, castle_vertex_eye, castle_normal_eye) */' + LineEnding +
'' + LineEnding +
'  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;' + LineEnding +
'' + LineEnding +
'  #ifdef CASTLE_BUGGY_GLSL_READ_VARYING' + LineEnding +
'  #undef castle_vertex_eye' + LineEnding +
'  #undef castle_normal_eye' + LineEnding +
'  #undef castle_Color' + LineEnding +
'  castle_vertex_eye = temp_castle_vertex_eye;' + LineEnding +
'  castle_normal_eye = temp_castle_normal_eye;' + LineEnding +
'  castle_Color      = temp_castle_Color;' + LineEnding +
'  #endif' + LineEnding +
'}' + LineEnding +
''
