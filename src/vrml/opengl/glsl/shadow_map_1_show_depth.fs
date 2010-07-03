uniform sampler2D texture0;
uniform sampler2D shadowMap;

float shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord);

void main(void)
{
  /* Always get alpha from the 1st tex, to honour alpha-test of leaves */
  float alpha = texture2D(texture0, gl_TexCoord[0].st).a;
  float d = shadow_depth(shadowMap, gl_TexCoord[1]);
  gl_FragColor = vec4(d, d, d, alpha);
}
