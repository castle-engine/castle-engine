void main()
{
  if (gl_TexCoord[0].x > 0.5)
    gl_FragColor = vec4(1, 1, 1, 1); else
    gl_FragColor = vec4(0, 0, 0, 1);
}
