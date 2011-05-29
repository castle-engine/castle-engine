/* Extract shadow (how light is the point) from variance shadow map.
   This closely follows VSM presentation, slide 16. */

float shadow(sampler2D shadowMap, const vec4 shadowMapCoord, const in float size)
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

  vec4 moments = texture2D(shadowMap, coord2);
  float distance_to_light = shadowMapCoord.z / shadowMapCoord.q;

  if (distance_to_light <= moments[0])
    return 1.0; else
  {
    float E_x2 = moments[1];
    float Ex_2 = moments[0] * moments[0];
    float variance = E_x2 - Ex_2;
    float m_d = moments[0] - distance_to_light;
    return variance / (variance + m_d * m_d);
  }
}

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
    return texture2D(shadowMap, coord2).x;
}
