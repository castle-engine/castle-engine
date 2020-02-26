{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_unlit/shading_gouraud.vs" }
'uniform vec4 castle_MaterialEmissiveAlpha;' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  /* In case of UnlitMaterial, Color/ColorRGBA replaces emissive color.*/' + LineEnding +
'  result = castle_ColorPerVertex;' + LineEnding +
'  #else' + LineEnding +
'  result = castle_MaterialEmissiveAlpha;' + LineEnding +
'  #endif' + LineEnding +
'}' + LineEnding +
''
