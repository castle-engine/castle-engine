#ifdef ALPHA_TEST
uniform sampler2D tex;
#endif

void main(void)
{

#ifdef ALPHA_TEST
  /* work with alpha test textures.
     For now we assume the texture is only in 0th channel. */
  if (texture2D(tex, gl_TexCoord[0].st).a < 0.5)
    discard;
#endif

  /* No need to divide gl_FragCoord.z by gl_FragCoord.w,
     as far as I understand GLSL spec. */
  gl_FragColor = vec4(gl_FragCoord.z, gl_FragCoord.z * gl_FragCoord.z, 0.0, 1.0);
}
