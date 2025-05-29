// Declare function from noise.glsl
float noise(vec2 p);

#ifndef GL_ES
// avoid redeclaring castle_TexCoord0 when GLSL doesn't support separate compilation units
varying vec4 castle_TexCoord0;
#endif

void PLUG_fragment_modify(inout vec4 fragment_color)
{
  const float noise_granularity = 1000.0;
  float noise_output = noise(castle_TexCoord0.xy * noise_granularity);

  const float noise_intensity = 0.1;
  fragment_color.rgb += vec3(noise_output * noise_intensity);
}
