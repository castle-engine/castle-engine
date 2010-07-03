{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "shadow_map_0.fs" }
'uniform sampler2DShadow shadowMap;' + LineEnding + 
'' + LineEnding + 
'float shadow(sampler2DShadow shadowMap, vec4 shadowMapCoord);' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_FragColor = shadow(shadowMap, gl_TexCoord[0]) * gl_Color;' + LineEnding + 
'  /* add some ambient term */' + LineEnding + 
'  gl_FragColor += vec4(0.1, 0.1, 0.1, 1);' + LineEnding + 
'}' + LineEnding + 
''
