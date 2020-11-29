uniform vec3 color;

// Modify the fragment color by "color" uniform.
void PLUG_main_texture_apply(inout vec4 fragment_color, const in vec3 normal)
{
  // fragment_color.rgb *= color;

  // Something more fancy
  fragment_color.rgb = mix(fragment_color.rgb, fragment_color.rgb * color, 0.5);
}
