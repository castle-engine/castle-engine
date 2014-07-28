{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "shadow_map_common.fs" }
'/* Shadow map functions. */' + LineEnding +
'' + LineEnding +
'/* Define exactly one of PCF* symbols, or none (to not use any PCF).' + LineEnding +
'   This is defined (or not) when including this shader in Pascal code,' + LineEnding +
'   automatically. */' + LineEnding +
'//#define PCF4' + LineEnding +
'//#define PCF4_BILINEAR' + LineEnding +
'//#define PCF16' + LineEnding +
'' + LineEnding +
'float shadow(sampler2DShadow shadowMap, const vec4 shadowMapCoord,' + LineEnding +
'  const in float size)' + LineEnding +
'{' + LineEnding +
'  /* Avoid back-projecting shadows. */' + LineEnding +
'  if (shadowMapCoord.z < 0.0) return 0.0;' + LineEnding +
'' + LineEnding +
'  /* When coord2 is outside (0, 0) - (1, 1) square,' + LineEnding +
'     it''s always in the shadow. Otherwise shadows would be stretched' + LineEnding +
'     over whole scene, due to clamping. */' + LineEnding +
'  vec2 coord2 = shadowMapCoord.st / shadowMapCoord.q;' + LineEnding +
'  if (coord2.s < 0.0 || coord2.s > 1.0 ||' + LineEnding +
'      coord2.t < 0.0 || coord2.t > 1.0)' + LineEnding +
'    return 0.0;' + LineEnding +
'' + LineEnding +
'#ifdef PCF4_BILINEAR' + LineEnding +
'' + LineEnding +
'  /* We have to scale up/down by texture size to make the floor/fract' + LineEnding +
'     perform real bilinear filtering.' + LineEnding +
'     This also means that we have to handle xy and z separately. */' + LineEnding +
'  vec2 tc_full = size * coord2;' + LineEnding +
'  float z = shadowMapCoord.z / shadowMapCoord.w;' + LineEnding +
'' + LineEnding +
'  vec2 tc = floor(tc_full.xy);' + LineEnding +
'  vec2 f = fract(tc_full.xy);' + LineEnding +
'  vec2 f1 = vec2(1.0, 1.0) - f;' + LineEnding +
'' + LineEnding +
'  return' + LineEnding +
'    shadow2D(shadowMap, vec3( tc.x        / size,  tc.y        / size, z)).r * f1.x * f1.y +' + LineEnding +
'    shadow2D(shadowMap, vec3( tc.x        / size, (tc.y + 1.0) / size, z)).r * f1.x *  f.y +' + LineEnding +
'    shadow2D(shadowMap, vec3((tc.x + 1.0) / size,  tc.y        / size, z)).r *  f.x * f1.y +' + LineEnding +
'    shadow2D(shadowMap, vec3((tc.x + 1.0) / size, (tc.y + 1.0) / size, z)).r *  f.x *  f.y;' + LineEnding +
'' + LineEnding +
'#elif defined(PCF4)' + LineEnding +
'' + LineEnding +
'  /* PCF with 2x2 kernel */' + LineEnding +
'  float offset = shadowMapCoord.w / size;' + LineEnding +
'  return (' + LineEnding +
'    shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset, -offset, 0.0, 0.0)).r +' + LineEnding +
'    shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset,  offset, 0.0, 0.0)).r +' + LineEnding +
'    shadow2DProj(shadowMap, shadowMapCoord + vec4( offset,  offset, 0.0, 0.0)).r +' + LineEnding +
'    shadow2DProj(shadowMap, shadowMapCoord + vec4( offset, -offset, 0.0, 0.0)).r' + LineEnding +
'    ) / 4.0;' + LineEnding +
'' + LineEnding +
'#elif defined(PCF16)' + LineEnding +
'' + LineEnding +
'  float offset = shadowMapCoord.w / size;' + LineEnding +
'  return' + LineEnding +
'    (' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5, -offset * 1.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5, -offset * 0.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5, -offset * 0.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5, -offset * 1.5, 0.0, 0.0)).r +' + LineEnding +
'' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5,  offset * 1.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5,  offset * 0.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5,  offset * 0.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5,  offset * 1.5, 0.0, 0.0)).r +' + LineEnding +
'' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5,  offset * 1.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5,  offset * 0.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5,  offset * 0.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5,  offset * 1.5, 0.0, 0.0)).r +' + LineEnding +
'' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5, -offset * 1.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5, -offset * 0.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5, -offset * 0.5, 0.0, 0.0)).r +' + LineEnding +
'      shadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5, -offset * 1.5, 0.0, 0.0)).r' + LineEnding +
'    )' + LineEnding +
'    / 16.0;' + LineEnding +
'#else' + LineEnding +
'  /* No PCF */' + LineEnding +
'  return shadow2DProj(shadowMap, shadowMapCoord).r;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Debug function, to display depth (distances) of the texture directly.' + LineEnding +
'' + LineEnding +
'   Note that you have to use compareMode "NONE" inside GeneratedShadowMap' + LineEnding +
'   (otherwise getting it as sampler2D may not be sensible, depends on GPU;' + LineEnding +
'   Looks like Radeon tolerated any compareMode, but NVidia requires "NONE".).' + LineEnding +
'*/' + LineEnding +
'float shadow_depth(sampler2D shadowMap, const vec4 shadowMapCoord)' + LineEnding +
'{' + LineEnding +
'  /* Avoid back-projecting shadows. */' + LineEnding +
'  if (shadowMapCoord.z < 0.0) return 0.0;' + LineEnding +
'' + LineEnding +
'  vec2 coord2 = shadowMapCoord.st / shadowMapCoord.q;' + LineEnding +
'' + LineEnding +
'  /* When coord2 is outside (0, 0) - (1, 1) square, set d = 0.' + LineEnding +
'     Otherwise texture would be visible stretched due to clamping. */' + LineEnding +
'  if (coord2.s < 0.0 || coord2.s > 1.0 ||' + LineEnding +
'      coord2.t < 0.0 || coord2.t > 1.0)' + LineEnding +
'    return 0.0; else' + LineEnding +
'    return texture2D(shadowMap, coord2).z;' + LineEnding +
'' + LineEnding +
'  // d = pow(d, 4.0); // makes shadows a little more contrasting' + LineEnding +
'}' + LineEnding +
''
