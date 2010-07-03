/* Define exactly one of PCF* symbols, or none (to not use any PCF).
   Note: this is handled by the VRMLShadowMap unit, when including this shader,
   automatically. */
//#define PCF4
//#define PCF4_BILINEAR
//#define PCF16

/* This should match texture size in pixels, to make perfect PCF. */
#define SIZE 1024.0

float shadow(sampler2DShadow shadowMap, vec4 shadowMapCoord)
{
  /* When coord2 is outside (0, 0) - (1, 1) square,
     it's always in the shadow. Otherwise shadows would be stretched
     over whole scene, due to clamping. */
  vec2 coord2 = shadowMapCoord.st / shadowMapCoord.q;
  if (coord2.s < 0.0 || coord2.s > 1.0 ||
      coord2.t < 0.0 || coord2.t > 1.0)
    return 0.0;

#ifdef PCF4_BILINEAR

  /* We have to scale up/down by texture SIZE to make the floor/fract
     perform real bilinear filtering.
     This also means that we have to handle xy and z separately. */
  vec2 tc_full = SIZE * coord2;
  float z = shadowMapCoord.z / shadowMapCoord.w;

  vec2 tc = floor(tc_full.xy);
  vec2 f = fract(tc_full.xy);
  vec2 f1 = vec2(1.0, 1.0) - f;

  return (
    shadow2D(shadowMap, vec3( tc.x        / SIZE,  tc.y        / SIZE, z)).r * f1.x * f1.y +
    shadow2D(shadowMap, vec3( tc.x        / SIZE, (tc.y + 1.0) / SIZE, z)).r * f1.x *  f.y +
    shadow2D(shadowMap, vec3((tc.x + 1.0) / SIZE,  tc.y        / SIZE, z)).r *  f.x * f1.y +
    shadow2D(shadowMap, vec3((tc.x + 1.0) / SIZE, (tc.y + 1.0) / SIZE, z)).r *  f.x *  f.y
    ) / 2.0; /* TODO: why /2.0 instead of /4.0 needed? */

#elif defined(PCF4)

  /* PCF with 2x2 kernel */
  float offset = shadowMapCoord.w / SIZE;
  return (
    shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset, -offset, 0.0, 0.0)).r +
    shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset,  offset, 0.0, 0.0)).r +
    shadow2DProj(shadowMap, shadowMapCoord + vec4( offset,  offset, 0.0, 0.0)).r +
    shadow2DProj(shadowMap, shadowMapCoord + vec4( offset, -offset, 0.0, 0.0)).r
    ) / 4.0;

#elif defined(PCF16)

  float offset = shadowMapCoord.w / SIZE;
  return
    (
      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5, -offset * 1.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5, -offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5, -offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5, -offset * 1.5, 0.0, 0.0)).r +

      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5,  offset * 1.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5,  offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5,  offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5,  offset * 1.5, 0.0, 0.0)).r +

      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5,  offset * 1.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5,  offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5,  offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5,  offset * 1.5, 0.0, 0.0)).r +

      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5, -offset * 1.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5, -offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5, -offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5, -offset * 1.5, 0.0, 0.0)).r
    )
    / 16.0;
#else
  /* No PCF */
  return shadow2DProj(shadowMap, shadowMapCoord).r;
#endif

}

/* Debug function, to display depth (distances) of the texture directly.

   This way shadow_map_1_show_depth.fs may be used instead of shadow_map_1.fs.
   Note that you have to use compareMode "NONE" inside GeneratedShadowMap
   (otherwise getting it as sampler2D may not be sensible, depends on GPU).

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
float shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord)
{
  vec2 coord2 = shadowMapCoord.st / shadowMapCoord.q;

  /* When coord2 is outside (0, 0) - (1, 1) square, set d = 0.
     Otherwise texture would be visible stretched due to clamping. */
  if (coord2.s < 0.0 || coord2.s > 1.0 ||
      coord2.t < 0.0 || coord2.t > 1.0)
    return 0.0; else
    return texture2D(shadowMap, coord2).z;

  // d = pow(d, 4.0); // makes shadows a little more contrasting
}
