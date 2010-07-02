/* Debug shader, to display depth (distances) of the texture directly.
   See shadow_map_1_show_depth.fs for usage comments. */

uniform sampler2D texture0;
uniform sampler2D shadowMap;

float variance_shadow(sampler2D shadowMap, vec4 shadowMapCoord);

void main(void)
{
  /* Always get alpha from the 1st tex, to honour alpha-test of leaves */
  float alpha = texture2D(texture0, gl_TexCoord[0].st).a;

  vec2 shadowMapCoord = gl_TexCoord[1].st / gl_TexCoord[1].q;

  float d;
  /* When shadowMapCoord is outside (0, 0) - (1, 1) square, set d = 0.
     Otherwise texture would be visible stretched due to clamping. */
  if (shadowMapCoord.s < 0.0 || shadowMapCoord.s > 1.0 ||
      shadowMapCoord.t < 0.0 || shadowMapCoord.t > 1.0)
    d = 0.0; else
    d = texture2D(shadowMap, shadowMapCoord).x;

  // d = pow(d, 4.0); // makes shadows a little more contrasting

  gl_FragColor = vec4(d, d, d, alpha);
}
