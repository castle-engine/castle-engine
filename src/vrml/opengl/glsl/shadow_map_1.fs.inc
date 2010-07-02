{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "shadow_map_1.fs" }
'#define PCF16' + LineEnding + 
'' + LineEnding + 
'uniform sampler2D texture0;' + LineEnding + 
'uniform sampler2DShadow shadowMap;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  float shadow;' + LineEnding + 
'' + LineEnding + 
'#ifdef PCF16' + LineEnding + 
'  float offset = gl_TexCoord[1].w / 512.0;' + LineEnding + 
'' + LineEnding + 
'  shadow =' + LineEnding + 
'    (' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 1.5, -offset * 1.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 1.5, -offset * 0.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 0.5, -offset * 0.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 0.5, -offset * 1.5, 0.0, 0.0)).r +' + LineEnding + 
'' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 1.5,  offset * 1.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 1.5,  offset * 0.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 0.5,  offset * 0.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 0.5,  offset * 1.5, 0.0, 0.0)).r +' + LineEnding + 
'' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 1.5,  offset * 1.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 1.5,  offset * 0.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 0.5,  offset * 0.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 0.5,  offset * 1.5, 0.0, 0.0)).r +' + LineEnding + 
'' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 1.5, -offset * 1.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 1.5, -offset * 0.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 0.5, -offset * 0.5, 0.0, 0.0)).r +' + LineEnding + 
'      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 0.5, -offset * 1.5, 0.0, 0.0)).r' + LineEnding + 
'    )' + LineEnding + 
'    / 16.0;' + LineEnding + 
'#else' + LineEnding + 
'  shadow = shadow2DProj(shadowMap, gl_TexCoord[1]).r;' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  /* When shadowMapCoord is outside (0, 0) - (1, 1) square,' + LineEnding + 
'     it''s always in the shadow. Otherwise shadows would be stretched' + LineEnding + 
'     over whole scene, due to clamping. */' + LineEnding + 
'  vec2 shadowMapCoord = gl_TexCoord[1].st / gl_TexCoord[1].q;' + LineEnding + 
'  if (shadowMapCoord.s < 0.0 || shadowMapCoord.s > 1.0 ||' + LineEnding + 
'      shadowMapCoord.t < 0.0 || shadowMapCoord.t > 1.0)' + LineEnding + 
'    shadow = 0.0;' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor = texture2D(texture0, gl_TexCoord[0].st) * gl_Color;' + LineEnding + 
'  gl_FragColor.rgb = mix(gl_FragColor.rgb / 2.0, gl_FragColor.rgb, shadow);' + LineEnding + 
'}' + LineEnding + 
''
