// Modify the fragment color to make it yellowish.
// See https://castle-engine.io/shaders .

float from_01_to_range(float value, float min, float max)
{
  return min + value * (max - min);
}

void PLUG_fragment_modify(inout vec4 fragment_color)
{
  // Make the color yellowish, by bumping up red and green, and reducing blue.
  fragment_color.r = from_01_to_range(fragment_color.r, 0.6, 1.0);
  fragment_color.g = from_01_to_range(fragment_color.g, 0.6, 1.0);
  fragment_color.b = from_01_to_range(fragment_color.b, 0.0, 0.4);
}
