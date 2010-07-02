/* Debug shader, to display depth (distances) of the texture directly.

   Just
   1. replace standard shadow_map_1.fs with this in VRML/X3D source
   2. use compareMode "NONE" inside GeneratedShadowMap (otherwise getting
      it as sampler2D may not be sensible, depends on GPU)
   to see the depths.

   Or just define VISUALIZE_SHADOW_MAP_DEPTHS inside sunny_street_process.lpr
   and "make", and all will be done for you.

   Note: it's possible to see the depths without the GLSL at all too:
   1. use compareMode "NONE" inside GeneratedShadowMap
   2. to avoid mixing with the normal color texture and material
      (that make it difficult to actually see the depth variations)
      you can use "REPLACE" mode mapping with GeneratedShadowMap.

      For example, replace all
        texture MultiTexture {
      with
        texture MultiTexture { mode [ "REPLACE / MODULATE", "REPLACE / MODULATE" ]
      inside sunny_street_processed.x3dv.
      By using "REPLACE / MODULATE" instead of "REPLACE" we allow to
      modulate alpha (and only replace RGB channels), this allows
      alpha-test texture of the leaves still work Ok.
*/

uniform sampler2D texture0;
uniform sampler2D shadowMap;

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
    d = texture2D(shadowMap, shadowMapCoord).z;

  // d = pow(d, 4.0); // makes shadows a little more contrasting

  gl_FragColor = vec4(d, d, d, alpha);
}
