uniform sampler2DShadow shadowMap;

void main(void)
{
  gl_FragColor = shadow2DProj(shadowMap, gl_TexCoord[0]).r * gl_Color;
  /* add some ambient term */
  gl_FragColor += vec4(0.1, 0.1, 0.1, 1);
}
