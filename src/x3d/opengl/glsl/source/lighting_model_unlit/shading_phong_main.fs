fragment_color = castle_UnlitColor;
/* In case of UnlitMaterial, Color/ColorRGBA is mixed with emissive color.
   TODO: Should this be replace? */
#ifdef COLOR_PER_VERTEX
fragment_color *= castle_ColorPerVertexFragment;
#endif
/* In case of UnlitMaterial, main texture (emissiveTexture or Appearance.texture)
   is mixed with emissive color. */
main_texture_apply(fragment_color, normal_eye_fragment);
