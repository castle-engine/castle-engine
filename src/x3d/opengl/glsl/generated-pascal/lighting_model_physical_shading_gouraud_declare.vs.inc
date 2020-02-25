{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/shading_gouraud_declare.vs" }
'uniform vec4 castle_MaterialBaseAlpha;' + LineEnding +
'uniform vec3 castle_MaterialAmbient;' + LineEnding +
'uniform vec3 castle_MaterialSpecular;' + LineEnding +
'uniform float castle_MaterialShininess;' + LineEnding +
'/* Color summed with all the lights:' + LineEnding +
'   Material emissive color + material ambient color * global (light model) ambient.' + LineEnding +
'   (similar to old gl_Front/BackLightModelProduct.sceneColor in deprecated GLSL versions.)' + LineEnding +
'*/' + LineEnding +
'uniform vec3 castle_SceneColor;' + LineEnding +
'uniform vec4 castle_UnlitColor;' + LineEnding +
''
