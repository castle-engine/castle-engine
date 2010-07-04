{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "shadow_map_0_show_depth.fs" }
'uniform sampler2D shadowMap;' + LineEnding + 
'' + LineEnding + 
'float shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord);' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  float d = shadow_depth(shadowMap, gl_TexCoord[0]);' + LineEnding + 
'  gl_FragColor = vec4(d, d, d, 1.0);' + LineEnding + 
'}' + LineEnding + 
''
