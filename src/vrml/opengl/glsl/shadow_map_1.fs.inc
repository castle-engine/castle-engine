{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "shadow_map_1.fs" }
'uniform sampler2D texture0;' + LineEnding + 
'uniform sampler2DShadow shadowMap;' + LineEnding + 
'' + LineEnding + 
'float shadow(sampler2DShadow shadowMap, vec4 shadowMapCoord);' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_FragColor = texture2D(texture0, gl_TexCoord[0].st) * gl_Color;' + LineEnding + 
'  gl_FragColor.rgb = mix(gl_FragColor.rgb / 2.0, gl_FragColor.rgb,' + LineEnding + 
'    shadow(shadowMap, gl_TexCoord[1]));' + LineEnding + 
'' + LineEnding + 
'  /* Alternative ideas:' + LineEnding + 
'' + LineEnding + 
'  /* Use max to make texture and gl_Color slightly visible even inside shadows.' + LineEnding + 
'     Do not write 0.1 directly, to avoid fglrx bugs. */' + LineEnding + 
'  // gl_FragColor.rgb *= max(1.0 / 10.0, shadow);' + LineEnding + 
'  /* add some ambient light */' + LineEnding + 
'  // gl_FragColor += vec4(0.05, 0.05, 0.05, 0.0);' + LineEnding + 
'}' + LineEnding + 
''
