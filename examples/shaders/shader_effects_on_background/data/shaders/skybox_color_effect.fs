uniform float intensity;

// Modify the fragment color by tinting it with a reddish color.
void PLUG_fragment_modify(inout vec4 fragment_color)
{
  const vec3 color_tint = vec3(2.0, 0.5, 0.5); // reddish tint
  fragment_color.rgb = mix(
    fragment_color.rgb,
    fragment_color.rgb * color_tint,
    intensity
  );
}
