{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_unlit/shading_phong_main.fs" }
'fragment_color = castle_UnlitColor;' + LineEnding +
'/* In case of UnlitMaterial, Color/ColorRGBA is mixed with emissive color.' + LineEnding +
'   TODO: Should this be replace? */' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'fragment_color *= castle_ColorPerVertexFragment;' + LineEnding +
'#endif' + LineEnding +
'/* In case of UnlitMaterial, main texture (emissiveTexture or Appearance.texture)' + LineEnding +
'   is mixed with emissive color. */' + LineEnding +
'main_texture_apply(fragment_color, normal_eye_fragment);' + LineEnding +
''
