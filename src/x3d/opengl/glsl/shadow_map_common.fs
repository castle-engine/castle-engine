/* Shadow map functions. */

/* Define exactly one of PCF* symbols, or none (to not use any PCF).
   This is defined (or not) when including this shader in Pascal code,
   automatically. */
//#define PCF4
//#define PCF4_BILINEAR
//#define PCF16

float shadow(sampler2DShadow shadowMap, const vec4 shadowMapCoord,
  const in float size)
{
  /* Avoid back-projecting shadows. */
  if (shadowMapCoord.z < 0.0) return 0.0;

  /* When coord2 is outside (0, 0) - (1, 1) square,
     it's always in the shadow. Otherwise shadows would be stretched
     over whole scene, due to clamping. */
  vec2 coord2 = shadowMapCoord.st / shadowMapCoord.q;
  if (coord2.s < 0.0 || coord2.s > 1.0 ||
      coord2.t < 0.0 || coord2.t > 1.0)
    return 0.0;

#ifdef PCF4_BILINEAR

  /* We have to scale up/down by texture size to make the floor/fract
     perform real bilinear filtering.
     This also means that we have to handle xy and z separately. */
  vec2 tc_full = size * coord2;
  float z = shadowMapCoord.z / shadowMapCoord.w;

  vec2 tc = floor(tc_full.xy);
  vec2 f = fract(tc_full.xy);
  vec2 f1 = vec2(1.0, 1.0) - f;

  return
    shadow2D(shadowMap, vec3( tc.x        / size,  tc.y        / size, z)).r * f1.x * f1.y +
    shadow2D(shadowMap, vec3( tc.x        / size, (tc.y + 1.0) / size, z)).r * f1.x *  f.y +
    shadow2D(shadowMap, vec3((tc.x + 1.0) / size,  tc.y        / size, z)).r *  f.x * f1.y +
    shadow2D(shadowMap, vec3((tc.x + 1.0) / size, (tc.y + 1.0) / size, z)).r *  f.x *  f.y;

#elif defined(PCF4)

  /* PCF with 2x2 kernel */
  float offset = shadowMapCoord.w / size;
  return (
    shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset, -offset, 0.0, 0.0)).r +
    shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset,  offset, 0.0, 0.0)).r +
    shadow2DProj(shadowMap, shadowMapCoord + vec4( offset,  offset, 0.0, 0.0)).r +
    shadow2DProj(shadowMap, shadowMapCoord + vec4( offset, -offset, 0.0, 0.0)).r
    ) / 4.0;

#elif defined(PCF16)

  float offset = shadowMapCoord.w / size;
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

   Note that you have to use compareMode "NONE" inside GeneratedShadowMap
   (otherwise getting it as sampler2D may not be sensible, depends on GPU;
   Looks like Radeon tolerated any compareMode, but NVidia requires "NONE".).
*/
float shadow_depth(sampler2D shadowMap, const vec4 shadowMapCoord)
{
  /* Avoid back-projecting shadows. */
  if (shadowMapCoord.z < 0.0) return 0.0;

  vec2 coord2 = shadowMapCoord.st / shadowMapCoord.q;

  /* When coord2 is outside (0, 0) - (1, 1) square, set d = 0.
     Otherwise texture would be visible stretched due to clamping. */
  if (coord2.s < 0.0 || coord2.s > 1.0 ||
      coord2.t < 0.0 || coord2.t > 1.0)
    return 0.0; else
    return texture2D(shadowMap, coord2).z;

  // d = pow(d, 4.0); // makes shadows a little more contrasting
}
