{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/shading_phong_declare.fs" }
'uniform vec4 castle_MaterialBaseAlpha;' + LineEnding +
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
''
