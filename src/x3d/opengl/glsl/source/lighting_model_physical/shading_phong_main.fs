#ifdef LIT
  vec4 material_diffuse_alpha;

  #ifdef COLOR_PER_VERTEX
  material_diffuse_alpha = castle_ColorPerVertexFragment;
  #else
  material_diffuse_alpha = castle_MaterialBaseAlpha;
  material_diffuse_alpha.rgb *= 2.0; // TODO just for test
  #endif

  main_texture_apply(material_diffuse_alpha, normal_eye_fragment);

  fragment_color = vec4(get_scene_color(), material_diffuse_alpha.a);

  /* PLUG: add_light (fragment_color, castle_vertex_eye, normal_eye_fragment, material_diffuse_alpha) */

  /* Clamp sum of lights colors to be <= 1. Fixed-function OpenGL does it too.
     This isn't really mandatory, but scenes with many lights could easily
     have colors > 1 and then the textures will look "burned out".
     Of course, for future HDR rendering we will turn this off. */
  fragment_color.rgb = min(fragment_color.rgb, 1.0);
#else
  // Unlit case

  fragment_color = castle_UnlitColor;
  /* TODO: This is not strictly correct,
     as ColorRGBA should only be used for unlit when Material=NULL.
     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set).

     TODO: Also we multiply ColorRGBA, while it should replace by default in X3D. */
  #ifdef COLOR_PER_VERTEX
  fragment_color *= castle_ColorPerVertexFragment;
  #endif
  /* TODO: This is not strictly correct,
     as Appearance.texture should only be used for unlit when Material=NULL.
     But we also enter this clause when Material<>NULL, but is unlit (only emissiveColor is set). */
  main_texture_apply(fragment_color, normal_eye_fragment);
#endif
