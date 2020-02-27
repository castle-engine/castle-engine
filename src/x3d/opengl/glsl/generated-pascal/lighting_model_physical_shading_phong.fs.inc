{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/shading_phong.fs" }
'uniform vec4 castle_MaterialBaseAlpha;' + LineEnding +
'uniform vec3 castle_MaterialEmissive;' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  vec4 material_base_alpha;' + LineEnding +
'' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  material_base_alpha = castle_ColorPerVertexFragment;' + LineEnding +
'  #else' + LineEnding +
'  material_base_alpha = castle_MaterialBaseAlpha;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  main_texture_apply(material_base_alpha, normal_eye);' + LineEnding +
'' + LineEnding +
'  vec3 emissive = castle_MaterialEmissive;' + LineEnding +
'  /* PLUG: material_emissive (emissive) */' + LineEnding +
'' + LineEnding +
'  result = vec4(emissive, material_base_alpha.a);' + LineEnding +
'' + LineEnding +
'  /* PLUG: add_light (result, vertex_eye, normal_eye, material_base_alpha) */' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.' + LineEnding +
'     This isn''t really mandatory, but scenes with many lights could easily' + LineEnding +
'     have colors > 1 and then the textures will look "burned out".' + LineEnding +
'     Of course, for future HDR rendering we will turn this off. */' + LineEnding +
'  result.rgb = min(result.rgb, 1.0);' + LineEnding +
'}' + LineEnding +
''
