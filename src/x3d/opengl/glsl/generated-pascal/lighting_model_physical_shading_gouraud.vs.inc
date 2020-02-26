{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/shading_gouraud.vs" }
'uniform vec4 castle_MaterialBaseAlpha;' + LineEnding +
'uniform vec3 castle_EmissiveColor;' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  vec4 material_base_alpha;' + LineEnding +
'' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  material_base_alpha = castle_ColorPerVertex;' + LineEnding +
'  #else' + LineEnding +
'  material_base_alpha = castle_MaterialBaseAlpha;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  result = vec4(castle_EmissiveColor, material_base_alpha.a);' + LineEnding +
'' + LineEnding +
'  /* PLUG: add_light (result, vertex_eye, normal_eye, material_base_alpha) */' + LineEnding +
'' + LineEnding +
'  /* Clamp sum of lights colors to be <= 1. See template_phong.fs for comments. */' + LineEnding +
'  result.rgb = min(result.rgb, 1.0);' + LineEnding +
'}' + LineEnding +
''
