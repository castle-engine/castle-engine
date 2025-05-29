// Modify the fragment color.
// See https://castle-engine.io/shaders .
void PLUG_fragment_modify(inout vec4 fragment_color)
{
  // Make the color red by setting
  // - the red color channel to the overall intensity
  // - the green and blue channels to zero.
  float intensity = (fragment_color.r + fragment_color.g + fragment_color.b) / 3.0;
  fragment_color = vec4(intensity, 0.0, 0.0, fragment_color.a);
}

// --------------------------------------------------------------------------------
// Alternative shader code version, commented out by default.
// Use this shader code version when ANIMATE_RED_INTENSITY is defined in Pascal.

/*

// Declare the uniform variable that will receive data from Pascal.
uniform float red_intensity;

// Modify the fragment color.
void PLUG_fragment_modify(inout vec4 fragment_color)
{
  // Make the color red by setting
  // - the red color channel to the overall intensity
  // - the green and blue channels to zero.
  float intensity = (fragment_color.r + fragment_color.g + fragment_color.b) / 3.0;
  vec4 reddish_color = vec4(intensity, 0.0, 0.0, fragment_color.a);
  fragment_color = mix(fragment_color, reddish_color, red_intensity);
}

*/