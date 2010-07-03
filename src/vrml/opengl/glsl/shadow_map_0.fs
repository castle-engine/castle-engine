uniform sampler2DShadow shadowMap;

float shadow(sampler2DShadow shadowMap, vec4 shadowMapCoord);

void main(void)
{
  gl_FragColor = shadow(shadowMap, gl_TexCoord[0]) * gl_Color;
  /* add some ambient term */
  gl_FragColor += vec4(0.1, 0.1, 0.1, 1);
}
