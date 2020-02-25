uniform vec4 castle_MaterialDiffuseAlpha;
uniform vec3 castle_MaterialAmbient;
uniform vec3 castle_MaterialSpecular;
uniform float castle_MaterialShininess;

#ifdef HAS_EMISSIVE_OR_AMBIENT_TEXTURE
uniform vec3 castle_MaterialEmissive;
uniform vec3 castle_GlobalAmbient;
#else
/* In this case we can optimize it.
   Color summed with all the lights:
   Material emissive color + material ambient color * global (light model) ambient.
   (similar to old gl_Front/BackLightModelProduct.sceneColor in deprecated GLSL versions.)
*/
uniform vec3 castle_SceneColor;
#endif

uniform vec4 castle_UnlitColor;

/* Calculate color summed with all the lights:
   Material emissive color + material ambient color * global (light model) ambient.
*/
vec3 get_scene_color()
{
#ifdef HAS_EMISSIVE_OR_AMBIENT_TEXTURE
  vec3 ambient = castle_MaterialAmbient;
  /* PLUG: material_light_ambient (ambient) */
  vec3 emissive = castle_MaterialEmissive;
  /* PLUG: material_emissive (emissive) */
  return emissive + ambient * castle_GlobalAmbient;
#else
  return castle_SceneColor;
#endif
}
