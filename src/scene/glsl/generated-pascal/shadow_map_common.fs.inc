{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/shadow_map_common.fs" }
'/* Shadow map functions. */' + LineEnding +
'' + LineEnding +
'/* Define exactly one of PCF* symbols, or none (to not use any PCF).' + LineEnding +
'   This is defined (or not) when including this shader in Pascal code,' + LineEnding +
'   automatically. */' + LineEnding +
'//#define PCF4' + LineEnding +
'//#define PCF4_BILINEAR' + LineEnding +
'//#define PCF16' + LineEnding +
'' + LineEnding +
'float castleShadow2D(sampler2DShadow shadowMap, const vec3 shadowMapCoord)' + LineEnding +
'{' + LineEnding +
'  /* On OpenGLES, we require GLSL "300 es" that allows (and requires) to query' + LineEnding +
'     sampler2DShadow using texture*, not shadow2D* functions.' + LineEnding +
'     See https://stackoverflow.com/a/22426507' + LineEnding +
'     https://www.khronos.org/opengl/wiki/Sampler_(GLSL)' + LineEnding +
'' + LineEnding +
'     Modern GLSL (from #version 140) also requires it.' + LineEnding +
'  */' + LineEnding +
'  #ifdef CASTLE_GLSL_VERSION_UPGRADE' + LineEnding +
'  return texture(shadowMap, shadowMapCoord);' + LineEnding +
'  #else' + LineEnding +
'  return shadow2D(shadowMap, shadowMapCoord).r;' + LineEnding +
'  #endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'float castleShadow2DProj(sampler2DShadow shadowMap, const vec4 shadowMapCoord)' + LineEnding +
'{' + LineEnding +
'  #ifdef CASTLE_GLSL_VERSION_UPGRADE' + LineEnding +
'  return textureProj(shadowMap, shadowMapCoord);' + LineEnding +
'  #else' + LineEnding +
'  return shadow2DProj(shadowMap, shadowMapCoord).r;' + LineEnding +
'  #endif' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'float shadow(sampler2DShadow shadowMap, const vec4 shadowMapCoord,' + LineEnding +
'  const in float size)' + LineEnding +
'{' + LineEnding +
'  /* Avoid back-projecting shadows. */' + LineEnding +
'  if (shadowMapCoord.z < 0.0) return 0.0;' + LineEnding +
'' + LineEnding +
'  /* When coord2 is outside (0, 0) - (1, 1) square, it''s never in the shadow.' + LineEnding +
'     This makes sense, see' + LineEnding +
'     https://github.com/castle-engine/castle-engine/issues/308' + LineEnding +
'     and demo-models/shadow_maps/explicit_projection_parameters/' + LineEnding +
'  */' + LineEnding +
'  vec2 coord2 = shadowMapCoord.st / shadowMapCoord.q;' + LineEnding +
'  if (coord2.s < 0.0 || coord2.s > 1.0 ||' + LineEnding +
'      coord2.t < 0.0 || coord2.t > 1.0)' + LineEnding +
'    return 1.0;' + LineEnding +
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
'    castleShadow2D(shadowMap, vec3( tc.x        / size,  tc.y        / size, z)) * f1.x * f1.y +' + LineEnding +
'    castleShadow2D(shadowMap, vec3( tc.x        / size, (tc.y + 1.0) / size, z)) * f1.x *  f.y +' + LineEnding +
'    castleShadow2D(shadowMap, vec3((tc.x + 1.0) / size,  tc.y        / size, z)) *  f.x * f1.y +' + LineEnding +
'    castleShadow2D(shadowMap, vec3((tc.x + 1.0) / size, (tc.y + 1.0) / size, z)) *  f.x *  f.y;' + LineEnding +
'' + LineEnding +
'#elif defined(PCF4)' + LineEnding +
'' + LineEnding +
'  /* PCF with 2x2 kernel */' + LineEnding +
'  float offset = shadowMapCoord.w / size;' + LineEnding +
'  return (' + LineEnding +
'    castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset, -offset, 0.0, 0.0)) +' + LineEnding +
'    castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset,  offset, 0.0, 0.0)) +' + LineEnding +
'    castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset,  offset, 0.0, 0.0)) +' + LineEnding +
'    castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset, -offset, 0.0, 0.0))' + LineEnding +
'    ) / 4.0;' + LineEnding +
'' + LineEnding +
'#elif defined(PCF16)' + LineEnding +
'' + LineEnding +
'  float offset = shadowMapCoord.w / size;' + LineEnding +
'  return' + LineEnding +
'    (' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5, -offset * 1.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5, -offset * 0.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5, -offset * 0.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5, -offset * 1.5, 0.0, 0.0)) +' + LineEnding +
'' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5,  offset * 1.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 1.5,  offset * 0.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5,  offset * 0.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4(-offset * 0.5,  offset * 1.5, 0.0, 0.0)) +' + LineEnding +
'' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5,  offset * 1.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5,  offset * 0.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5,  offset * 0.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5,  offset * 1.5, 0.0, 0.0)) +' + LineEnding +
'' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5, -offset * 1.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 1.5, -offset * 0.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5, -offset * 0.5, 0.0, 0.0)) +' + LineEnding +
'      castleShadow2DProj(shadowMap, shadowMapCoord + vec4( offset * 0.5, -offset * 1.5, 0.0, 0.0))' + LineEnding +
'    )' + LineEnding +
'    / 16.0;' + LineEnding +
'#else' + LineEnding +
'  /* No PCF */' + LineEnding +
'  return castleShadow2DProj(shadowMap, shadowMapCoord);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'}' + LineEnding +
''
