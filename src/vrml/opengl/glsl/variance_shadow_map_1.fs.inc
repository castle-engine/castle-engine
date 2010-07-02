{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "variance_shadow_map_1.fs" }
'uniform sampler2D texture0;' + LineEnding + 
'uniform sampler2D shadowMap;' + LineEnding + 
'' + LineEnding + 
'float variance_shadow(sampler2D shadowMap, vec4 shadowMapCoord);' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  float shadow = variance_shadow(shadowMap, gl_TexCoord[1]);' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor = texture2D(texture0, gl_TexCoord[0].st) * gl_Color;' + LineEnding + 
'  gl_FragColor.rgb = mix(gl_FragColor.rgb / 2.0, gl_FragColor.rgb, shadow);' + LineEnding + 
'}' + LineEnding + 
''
