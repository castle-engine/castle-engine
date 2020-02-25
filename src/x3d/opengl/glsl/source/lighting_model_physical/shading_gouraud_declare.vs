uniform vec4 castle_MaterialBaseAlpha;
uniform vec3 castle_MaterialAmbient;
uniform vec3 castle_MaterialSpecular;
uniform float castle_MaterialShininess;
/* Color summed with all the lights:
   Material emissive color + material ambient color * global (light model) ambient.
   (similar to old gl_Front/BackLightModelProduct.sceneColor in deprecated GLSL versions.)
*/
uniform vec3 castle_SceneColor;
uniform vec4 castle_UnlitColor;
