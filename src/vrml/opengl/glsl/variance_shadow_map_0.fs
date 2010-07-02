uniform sampler2D shadowMap;

float variance_shadow(sampler2D shadowMap, vec4 shadowMapCoord);

void main(void)
{
  gl_FragColor = variance_shadow(shadowMap, gl_TexCoord[0]) * gl_Color;
  /* add some ambient term */
  gl_FragColor += vec4(0.1, 0.1, 0.1, 1);
}
