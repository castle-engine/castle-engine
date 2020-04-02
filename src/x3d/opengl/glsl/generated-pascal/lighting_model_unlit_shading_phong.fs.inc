{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_unlit/shading_phong.fs" }
'uniform vec4 castle_MaterialEmissiveAlpha;' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  result =' + LineEnding +
'    #if defined(COLOR_PER_VERTEX_REPLACE)' + LineEnding +
'    /* In case of UnlitMaterial, Color/ColorRGBA replaces emissive color.*/' + LineEnding +
'    castle_ColorPerVertexFragment;' + LineEnding +
'    #elif defined(COLOR_PER_VERTEX_MODULATE)' + LineEnding +
'    castle_ColorPerVertexFragment * castle_MaterialEmissiveAlpha;' + LineEnding +
'    #else' + LineEnding +
'    castle_MaterialEmissiveAlpha;' + LineEnding +
'    #endif' + LineEnding +
'' + LineEnding +
'  /* In case of UnlitMaterial, main texture (emissiveTexture or Appearance.texture)' + LineEnding +
'     is mixed with emissive color. */' + LineEnding +
'  main_texture_apply(result, normal_eye);' + LineEnding +
'}' + LineEnding +
''
