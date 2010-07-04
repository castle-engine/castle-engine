uniform sampler2D shadowMap;

float shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord);

void main(void)
{
  float d = shadow_depth(shadowMap, gl_TexCoord[0]);
  gl_FragColor = vec4(d, d, d, 1.0);
}
