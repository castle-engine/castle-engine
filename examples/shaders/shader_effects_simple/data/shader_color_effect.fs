// Modify the fragment color.
// See https://castle-engine.io/shaders .
void PLUG_fragment_modify(inout vec4 fragment_color)
{
  // Make the color "more red" by setting
  // - the red color channel to the overall intensity
  // - the green and blue channels to zero.
  float intensity = (fragment_color.r + fragment_color.g + fragment_color.b) / 3.0;
  fragment_color = vec4(intensity, 0.0, 0.0, fragment_color.a);
}
