void main(void)
{
  /* No need to divide gl_FragCoord.z by gl_FragCoord.w,
     as far as I understand GLSL spec. */
  gl_FragColor = vec4(gl_FragCoord.z, gl_FragCoord.z * gl_FragCoord.z, 0.0, 1.0);
}
