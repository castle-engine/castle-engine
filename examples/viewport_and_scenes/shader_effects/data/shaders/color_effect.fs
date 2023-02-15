uniform vec3 color;

const float threshold_to_modify = 0.05;

// Modify the fragment color by "color" uniform.
void PLUG_fragment_modify(inout vec4 fragment_color)
{
  // fragment_color.rgb *= color;

  // Something more fancy: if average color component > threshold_to_modify, then modify
  if (fragment_color.r +
      fragment_color.g +
      fragment_color.b > threshold_to_modify * 3.0) {
    fragment_color.rgb = mix(fragment_color.rgb, fragment_color.rgb * color, 0.9);
  }
}
