{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "variance_shadow_map_0_show_depth.fs" }
'/* Debug shader, to display depth (distances) of the texture directly.' + LineEnding + 
'   See shadow_map_1_show_depth.fs for usage comments. */' + LineEnding + 
'' + LineEnding + 
'uniform sampler2D shadowMap;' + LineEnding + 
'' + LineEnding + 
'float variance_shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord);' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  float d = variance_shadow_depth(shadowMap, gl_TexCoord[0]);' + LineEnding + 
'  gl_FragColor = vec4(d, d, d, 1.0);' + LineEnding + 
'}' + LineEnding + 
''
