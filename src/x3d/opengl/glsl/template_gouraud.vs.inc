{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_gouraud.vs" }
'/* Gouraud shading GLSL vertex shader. */' + LineEnding +
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
'/* Include fragment shader utilities used by both Gouraud and Phong shading. */' + LineEnding +
'/* CASTLE-COMMON-CODE */' + LineEnding +
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
'  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);' + LineEnding +
'' + LineEnding +
'  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */' + LineEnding +
'' + LineEnding +
'#ifdef LIT' + LineEnding +
'  castle_Color = vec4(castle_SceneColor, 1.0);' + LineEnding +
'' + LineEnding +
'  /* Two-sided lighting in Gouraud shading:' + LineEnding +
'     flip the normal vector to correspond to the face side that we actually see.' + LineEnding +
'' + LineEnding +
'     Note that we don''t flip the castle_normal_eye (we only flip the' + LineEnding +
'     normal_for_lighting), as castle_normal_eye may be useful also for other' + LineEnding +
'     calculations, e.g. cubemap reflections, that don''t want this flippping' + LineEnding +
'     (testcase: demo-models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv )' + LineEnding +
'' + LineEnding +
'     This is commented out, because it''s not perfect, and I''m not sure can' + LineEnding +
'     we efficiently do artifact-free version of two-sided lighting.' + LineEnding +
'     Reproduction of the problem:' + LineEnding +
'     - demo-models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv,' + LineEnding +
'       look at the back side of the box.' + LineEnding +
'     - demo-models/fog/fog_linear, rotate in Examine and look at the thin water' + LineEnding +
'       edges.' + LineEnding +
'' + LineEnding +
'     The problem: We base our flipping on castle_normal_eye,' + LineEnding +
'     which may be a smoothed (per-vertex) normal vector.' + LineEnding +
'' + LineEnding +
'     - We cannot calculate here reliably per-face vector (fragment shaders' + LineEnding +
'       can do a trick with dFdx, see' + LineEnding +
'       https://makc3d.wordpress.com/2015/09/17/alternative-to-gl_frontfacing/ ,' + LineEnding +
'       but dFdx is only available in fragment shader).' + LineEnding +
'' + LineEnding +
'     - Fully-correct solutions are inefficient:' + LineEnding +
'       - To pass to vertex shader a face_normal in a special uniform' + LineEnding +
'         means that we have to pass extra data, and also that we have to' + LineEnding +
'         split vertexes to not share vertexes across faces.' + LineEnding +
'       - Calculating light 2x times and then letting fragment shader to choose' + LineEnding +
'         which side to show (this is what fixed-function does, I think).' + LineEnding +
'' + LineEnding +
'     - If you''re OK with being correct (not fast), you can use Phong shading' + LineEnding +
'       where two-sided lighting works easily.' + LineEnding +
'  */' + LineEnding +
'  /* vec3 normal_for_lighting = (castle_normal_eye.z > 0.0 ? castle_normal_eye : -castle_normal_eye); */' + LineEnding +
'' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'    /* PLUG: add_light_contribution (castle_Color, castle_vertex_eye, castle_normal_eye, castle_MaterialShininess, castle_ColorPerVertex) */' + LineEnding +
'    castle_Color.a = castle_ColorPerVertex.a;' + LineEnding +
'  #else' + LineEnding +
'    /* PLUG: add_light_contribution (castle_Color, castle_vertex_eye, castle_normal_eye, castle_MaterialShininess, vec4(0.0)) */' + LineEnding +
'    castle_Color.a = castle_MaterialDiffuseAlpha;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */' + LineEnding +
'  castle_Color.rgb = min(castle_Color.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  castle_Color = castle_UnlitColor' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'    /* Apply COLOR_PER_VERTEX, when unlit.' + LineEnding +
'       (When lit, then the analogous multiplication is done' + LineEnding +
'        inside template_light.glsl) */' + LineEnding +
'    * castle_ColorPerVertex' + LineEnding +
'#endif' + LineEnding +
'  ;' + LineEnding +
'#endif' + LineEnding +
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
