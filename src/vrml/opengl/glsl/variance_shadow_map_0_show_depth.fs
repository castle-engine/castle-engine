/* Debug shader, to display depth (distances) of the texture directly.
   See shadow_map_1_show_depth.fs for usage comments. */

uniform sampler2D shadowMap;

float variance_shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord);

void main(void)
{
  float d = variance_shadow_depth(shadowMap, gl_TexCoord[0]);
  gl_FragColor = vec4(d, d, d, 1.0);
}
