{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_mobile.vs" }
'/* Generic GLSL vertex shader, used on OpenGL ES. */' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewMatrix;' + LineEnding +
'uniform mat4 castle_ProjectionMatrix;' + LineEnding +
'uniform mat3 castle_NormalMatrix;' + LineEnding +
'attribute vec4 castle_Vertex;' + LineEnding +
'attribute vec3 castle_Normal;' + LineEnding +
'' + LineEnding +
'/* PLUG-DECLARATIONS */' + LineEnding +
'' + LineEnding +
'varying vec4 castle_vertex_eye;' + LineEnding +
'varying vec3 castle_normal_eye;' + LineEnding +
'varying vec4 castle_Color;' + LineEnding +
'' + LineEnding +
'uniform float castle_MaterialDiffuseAlpha;' + LineEnding +
'uniform float castle_MaterialShininess;' + LineEnding +
'/* Color summed with all the lights.' + LineEnding +
'   Like gl_Front/BackLightModelProduct.sceneColor:' + LineEnding +
'   material emissive color + material ambient color * global (light model) ambient.' + LineEnding +
'*/' + LineEnding +
'uniform vec3 castle_SceneColor;' + LineEnding +
'uniform vec4 castle_UnlitColor;' + LineEnding +
'' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'attribute vec4 castle_ColorPerVertex;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  vec4 vertex_object = castle_Vertex;' + LineEnding +
'  vec3 normal_object = castle_Normal;' + LineEnding +
'  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */' + LineEnding +
'  /* PLUG: vertex_object_space (vertex_object, normal_object) */' + LineEnding +
'' + LineEnding +
'  #ifdef CASTLE_BUGGY_GLSL_READ_VARYING' + LineEnding +
'  /* use local variables, instead of reading + writing to varying variables,' + LineEnding +
'     when VARYING_NOT_READABLE */' + LineEnding +
'  vec4 temp_castle_vertex_eye;' + LineEnding +
'  vec3 temp_castle_normal_eye;' + LineEnding +
'  vec4 temp_castle_Color;' + LineEnding +
'  #define castle_vertex_eye temp_castle_vertex_eye' + LineEnding +
'  #define castle_normal_eye temp_castle_normal_eye' + LineEnding +
'  #define castle_Color      temp_castle_Color' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;' + LineEnding +
'  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);' + LineEnding +
'' + LineEnding +
'  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */' + LineEnding +
'' + LineEnding +
'#ifdef LIT' + LineEnding +
'  castle_Color = vec4(castle_SceneColor, 1.0);' + LineEnding +
'  /* PLUG: add_light_contribution (castle_Color, castle_vertex_eye, castle_normal_eye, castle_MaterialShininess) */' + LineEnding +
'  castle_Color.a = castle_MaterialDiffuseAlpha;' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. See template.fs for comments. */' + LineEnding +
'  castle_Color.rgb = min(castle_Color.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  castle_Color = castle_UnlitColor' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'    * castle_ColorPerVertex' + LineEnding +
'#endif' + LineEnding +
'  ;' + LineEnding +
'#endif' + LineEnding +
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
