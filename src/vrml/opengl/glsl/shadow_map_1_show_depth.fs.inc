{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "shadow_map_1_show_depth.fs" }
'uniform sampler2D texture0;' + LineEnding + 
'uniform sampler2D shadowMap;' + LineEnding + 
'' + LineEnding + 
'float shadow_depth(sampler2D shadowMap, vec4 shadowMapCoord);' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  /* Always get alpha from the 1st tex, to honour alpha-test of leaves */' + LineEnding + 
'  float alpha = texture2D(texture0, gl_TexCoord[0].st).a;' + LineEnding + 
'  float d = shadow_depth(shadowMap, gl_TexCoord[1]);' + LineEnding + 
'  gl_FragColor = vec4(d, d, d, alpha);' + LineEnding + 
'}' + LineEnding + 
''
