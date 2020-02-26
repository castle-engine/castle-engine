{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_phong/shading_phong.fs" }
'uniform vec4 castle_MaterialDiffuseAlpha;' + LineEnding +
'uniform vec3 castle_MaterialAmbient;' + LineEnding +
'uniform vec3 castle_MaterialSpecular;' + LineEnding +
'uniform float castle_MaterialShininess;' + LineEnding +
'' + LineEnding +
'#ifdef HAS_EMISSIVE_OR_AMBIENT_TEXTURE' + LineEnding +
'uniform vec3 castle_MaterialEmissive;' + LineEnding +
'uniform vec3 castle_GlobalAmbient;' + LineEnding +
'#else' + LineEnding +
'/* In this case we can optimize it.' + LineEnding +
'   Color summed with all the lights:' + LineEnding +
'   Material emissive color + material ambient color * global (light model) ambient.' + LineEnding +
'   (similar to old gl_Front/BackLightModelProduct.sceneColor in deprecated GLSL versions.)' + LineEnding +
'*/' + LineEnding +
'uniform vec3 castle_SceneColor;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'uniform vec4 castle_UnlitColor;' + LineEnding +
'' + LineEnding +
'/* Calculate color summed with all the lights:' + LineEnding +
'   Material emissive color + material ambient color * global (light model) ambient.' + LineEnding +
'*/' + LineEnding +
'vec3 get_scene_color()' + LineEnding +
'{' + LineEnding +
'#ifdef HAS_EMISSIVE_OR_AMBIENT_TEXTURE' + LineEnding +
'  vec3 ambient = castle_MaterialAmbient;' + LineEnding +
'  /* PLUG: material_light_ambient (ambient) */' + LineEnding +
'  vec3 emissive = castle_MaterialEmissive;' + LineEnding +
'  /* PLUG: material_emissive (emissive) */' + LineEnding +
'  return emissive + ambient * castle_GlobalAmbient;' + LineEnding +
'#else' + LineEnding +
'  return castle_SceneColor;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'#ifdef LIT' + LineEnding +
'  vec4 material_diffuse_alpha;' + LineEnding +
'' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  material_diffuse_alpha = castle_ColorPerVertexFragment;' + LineEnding +
'  #else' + LineEnding +
'  material_diffuse_alpha = castle_MaterialDiffuseAlpha;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  main_texture_apply(material_diffuse_alpha, normal_eye);' + LineEnding +
'' + LineEnding +
'  result = vec4(get_scene_color(), material_diffuse_alpha.a);' + LineEnding +
'' + LineEnding +
'  /* PLUG: add_light (result, vertex_eye, normal_eye, material_diffuse_alpha) */' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.' + LineEnding +
'     This isn''t really mandatory, but scenes with many lights could easily' + LineEnding +
'     have colors > 1 and then the textures will look "burned out".' + LineEnding +
'     Of course, for future HDR rendering we will turn this off. */' + LineEnding +
'  result.rgb = min(result.rgb, 1.0);' + LineEnding +
'#else' + LineEnding +
'  // Unlit case' + LineEnding +
'' + LineEnding +
'  result = castle_UnlitColor;' + LineEnding +
'  /* TODO: This is not strictly correct,' + LineEnding +
'     as ColorRGBA should only be used for unlit when Material=NULL.' + LineEnding +
'     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set).' + LineEnding +
'' + LineEnding +
'     TODO: Also we multiply ColorRGBA, while it should replace by default in X3D. */' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  result *= castle_ColorPerVertexFragment;' + LineEnding +
'  #endif' + LineEnding +
'  /* TODO: This is not strictly correct,' + LineEnding +
'     as Appearance.texture should only be used for unlit when Material=NULL.' + LineEnding +
'     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set). */' + LineEnding +
'  main_texture_apply(result, normal_eye);' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
''
