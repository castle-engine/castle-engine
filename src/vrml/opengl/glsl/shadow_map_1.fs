uniform sampler2D texture0;
uniform sampler2DShadow shadowMap;

float shadow(sampler2DShadow shadowMap, vec4 shadowMapCoord);

void main(void)
{
  gl_FragColor = texture2D(texture0, gl_TexCoord[0].st) * gl_Color;
  gl_FragColor.rgb = mix(gl_FragColor.rgb / 4.0, gl_FragColor.rgb,
    shadow(shadowMap, gl_TexCoord[1]));

  /* Alternative ideas:

  /* Use max to make texture and gl_Color slightly visible even inside shadows.
     Do not write 0.1 directly, to avoid fglrx bugs. */
  // gl_FragColor.rgb *= max(1.0 / 10.0, shadow);
  /* add some ambient light */
  // gl_FragColor += vec4(0.05, 0.05, 0.05, 0.0);
}
