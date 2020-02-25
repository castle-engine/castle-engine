{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/shading_phong_main.fs" }
'#ifdef LIT' + LineEnding +
'  vec4 material_diffuse_alpha;' + LineEnding +
'' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  material_diffuse_alpha = castle_ColorPerVertexFragment;' + LineEnding +
'  #else' + LineEnding +
'  material_diffuse_alpha = castle_MaterialDiffuseAlpha;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  main_texture_apply(material_diffuse_alpha, normal_eye_fragment);' + LineEnding +
'' + LineEnding +
'  fragment_color = vec4(get_scene_color(), material_diffuse_alpha.a);' + LineEnding +
'' + LineEnding +
'  /* PLUG: add_light (fragment_color, castle_vertex_eye, normal_eye_fragment, material_diffuse_alpha) */' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.' + LineEnding +
'     This isn''t really mandatory, but scenes with many lights could easily' + LineEnding +
'     have colors > 1 and then the textures will look "burned out".' + LineEnding +
'     Of course, for future HDR rendering we will turn this off. */' + LineEnding +
'  fragment_color.rgb = min(fragment_color.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  // Unlit case' + LineEnding +
'' + LineEnding +
'  fragment_color = castle_UnlitColor;' + LineEnding +
'  /* TODO: This is not strictly correct,' + LineEnding +
'     as ColorRGBA should only be used for unlit when Material=NULL.' + LineEnding +
'     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set).' + LineEnding +
'' + LineEnding +
'     TODO: Also we multiply ColorRGBA, while it should replace by default in X3D. */' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  fragment_color *= castle_ColorPerVertexFragment;' + LineEnding +
'  #endif' + LineEnding +
'  /* TODO: This is not strictly correct,' + LineEnding +
'     as Appearance.texture should only be used for unlit when Material=NULL.' + LineEnding +
'     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set). */' + LineEnding +
'  main_texture_apply(fragment_color, normal_eye_fragment);' + LineEnding +
'#endif' + LineEnding +
''
