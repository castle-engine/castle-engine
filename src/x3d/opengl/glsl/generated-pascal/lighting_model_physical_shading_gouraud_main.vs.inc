{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/shading_gouraud_main.vs" }
'#ifdef LIT' + LineEnding +
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
'  vec4 material_diffuse_alpha;' + LineEnding +
'' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  material_diffuse_alpha = castle_ColorPerVertex;' + LineEnding +
'  #else' + LineEnding +
'  material_diffuse_alpha = castle_MaterialBaseAlpha;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  castle_Color = vec4(castle_SceneColor, material_diffuse_alpha.a);' + LineEnding +
'' + LineEnding +
'  /* PLUG: add_light (castle_Color, castle_vertex_eye, castle_normal_eye, material_diffuse_alpha) */' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */' + LineEnding +
'  castle_Color.rgb = min(castle_Color.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  // Unlit case' + LineEnding +
'  castle_Color = castle_UnlitColor;' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  castle_Color *= castle_ColorPerVertex;' + LineEnding +
'  #endif' + LineEnding +
'#endif' + LineEnding +
''
