/* Fragment shader when generating shadow maps. */

#ifdef ALPHA_TEST
uniform sampler2D castle_texture_0;
varying vec2 castle_TexCoord0_XY;
#endif

void main(void)
{
  #ifdef ALPHA_TEST
  /* Alpha test texture.
     TODO: This assumes the alpha-tested texture is 2D and is in 0th channel. */
  if (texture2D(castle_texture_0, castle_TexCoord0_XY).a < 0.5)
    discard;
  #endif

  #ifdef VARIANCE_SHADOW_MAPS
  /* Encode depth is format necessary for Variance Shadow Maps.

     Note: No need to divide gl_FragCoord.z by gl_FragCoord.w,
     as far as I understand GLSL spec. */
  gl_FragColor = vec4(gl_FragCoord.z, gl_FragCoord.z * gl_FragCoord.z, 0.0, 1.0);
  #endif

  /* For classic shadow maps (not Variance Shadow Maps),
     it doesn't matter what we write to the color buffer. */
  // gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);
}
