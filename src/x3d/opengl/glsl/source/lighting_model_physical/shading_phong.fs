uniform vec4 castle_MaterialBaseAlpha;
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

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
#ifdef LIT
  vec4 material_diffuse_alpha;

  #ifdef COLOR_PER_VERTEX
  material_diffuse_alpha = castle_ColorPerVertexFragment;
  #else
  material_diffuse_alpha = castle_MaterialBaseAlpha;
  #endif

  main_texture_apply(material_diffuse_alpha, normal_eye);

  result = vec4(get_scene_color(), material_diffuse_alpha.a);

  /* PLUG: add_light (result, vertex_eye, normal_eye, material_diffuse_alpha) */

  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.
     This isn't really mandatory, but scenes with many lights could easily
     have colors > 1 and then the textures will look "burned out".
     Of course, for future HDR rendering we will turn this off. */
  result.rgb = min(result.rgb, 1.0);
#else
  // Unlit case

  result = castle_UnlitColor;
  /* TODO: This is not strictly correct,
     as ColorRGBA should only be used for unlit when Material=NULL.
     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set).

     TODO: Also we multiply ColorRGBA, while it should replace by default in X3D. */
  #ifdef COLOR_PER_VERTEX
  result *= castle_ColorPerVertexFragment;
  #endif
  /* TODO: This is not strictly correct,
     as Appearance.texture should only be used for unlit when Material=NULL.
     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set). */
  main_texture_apply(result, normal_eye);
#endif
}
