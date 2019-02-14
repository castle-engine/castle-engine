/* Shadow map functions. */

/* Define exactly one of PCF* symbols, or none (to not use any PCF).
   This is defined (or not) when including this shader in Pascal code,
   automatically. */
//#define PCF4
//#define PCF4_BILINEAR
//#define PCF16

#ifdef GL_ES
  #define sampler2DShadow sampler2D

  // This should no longer be necessary, we use glPolygonOffset on OpenGLES
  // const float castleShadowBias = 0.005;

  /* Same as shadow2D: compare shadowMap
     sampled at shadowMapCoord.xy
     with shadowMapCoord.z. */
  float castleShadow2D(sampler2DShadow shadowMap, const vec3 shadowMapCoord)
  {
    float distanceToLightObstacle = texture2D(shadowMap, shadowMapCoord.xy).r;
    // Return 0 if in shadow, 1 if not in shadow.
    return float(distanceToLightObstacle/* + castleShadowBias*/ >= shadowMapCoord.z);
  }

  /* Same as shadow2DProj: compare shadowMap
     sampled at shadowMapCoord.xy/shadowMapCoord.w
     with shadowMapCoord.z. */
  float castleShadow2DProj(sampler2DShadow shadowMap, const vec4 shadowMapCoord)
  {
    /* Note that texture2DProj effectively uses
       "shadowMapCoord.xy / shadowMapCoord.w" as 2D coordinate.
       It ignores shadowMapCoord.z. */
    float distanceToLightObstacle = texture2DProj(shadowMap, shadowMapCoord).r;
    // Return 0 if in shadow, 1 if not in shadow.
    return float(distanceToLightObstacle/* + castleShadowBias*/ >= shadowMapCoord.z);
  }
#else
  float castleShadow2D(sampler2DShadow shadowMap, const vec3 shadowMapCoord)
  {
    return shadow2D(shadowMap, shadowMapCoord).r;
  }

  float castleShadow2DProj(sampler2DShadow shadowMap, const vec4 shadowMapCoord)
  {
    return shadow2DProj(shadowMap, shadowMapCoord).r;
  }
#endif

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
    castleShadow2D(shadowMap, vec3( tc.x        / size,  tc.y        / size, z)) * f1.x * f1.y +
    castleShadow2D(shadowMap, vec3( tc.x        / size, (tc.y + 1.0) / size, z)) * f1.x *  f.y +
    castleShadow2D(shadowMap, vec3((tc.x + 1.0) / size,  tc.y        / size, z)) *  f.x * f1.y +
    castleShadow2D(shadowMap, vec3((tc.x + 1.0) / size, (tc.y + 1.0) / size, z)) *  f.x *  f.y;

#elif defined(PCF4)

  /* PCF with 2x2 kernel */
  float offset = shadowMapCoord.w / size;
  return (
    castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset, -offset, 0.0, 0.0)) +
    castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset,  offset, 0.0, 0.0)) +
    castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset,  offset, 0.0, 0.0)) +
    castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset, -offset, 0.0, 0.0))
    ) / 4.0;

#elif defined(PCF16)

  float offset = shadowMapCoord.w / size;
  return
    (
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5, -offset * 1.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5, -offset * 0.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5, -offset * 0.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5, -offset * 1.5, 0.0, 0.0)) +

      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5,  offset * 1.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5,  offset * 0.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5,  offset * 0.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5,  offset * 1.5, 0.0, 0.0)) +

      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5,  offset * 1.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5,  offset * 0.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5,  offset * 0.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5,  offset * 1.5, 0.0, 0.0)) +

      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5, -offset * 1.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5, -offset * 0.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5, -offset * 0.5, 0.0, 0.0)) +
      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5, -offset * 1.5, 0.0, 0.0))
    )
    / 16.0;
#else
  /* No PCF */
  return castleShadow2DProj(shadowMap, shadowMapCoord);
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
