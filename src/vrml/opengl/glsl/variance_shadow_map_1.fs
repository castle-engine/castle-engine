uniform sampler2D texture0;
uniform sampler2D shadowMap;

void main(void)
{
  float shadow = variance_shadow(shadowMap, gl_TexCoord[1]);

  gl_FragColor = texture2D(texture0, gl_TexCoord[0].st) * gl_Color;
  gl_FragColor.rgb = mix(gl_FragColor.rgb / 2.0, gl_FragColor.rgb, shadow);
}
