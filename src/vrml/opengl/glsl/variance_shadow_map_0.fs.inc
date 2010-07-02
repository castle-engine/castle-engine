{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "variance_shadow_map_0.fs" }
'uniform sampler2D shadowMap;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_FragColor = variance_shadow(shadowMap, gl_TexCoord[0]) * gl_Color;' + LineEnding + 
'  /* add some ambient term */' + LineEnding + 
'  gl_FragColor += vec4(0.1, 0.1, 0.1, 1);' + LineEnding + 
'}' + LineEnding + 
''
