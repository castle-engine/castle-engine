{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "variance_shadow_map_1_show_depth.fs" }
'/* Debug shader, to display depth (distances) of the texture directly.' + LineEnding + 
'   See shadow_map_1_show_depth.fs for usage comments. */' + LineEnding + 
'' + LineEnding + 
'uniform sampler2D texture0;' + LineEnding + 
'uniform sampler2D shadowMap;' + LineEnding + 
'' + LineEnding + 
'float variance_shadow(sampler2D shadowMap, vec4 shadowMapCoord);' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  /* Always get alpha from the 1st tex, to honour alpha-test of leaves */' + LineEnding + 
'  float alpha = texture2D(texture0, gl_TexCoord[0].st).a;' + LineEnding + 
'' + LineEnding + 
'  vec2 shadowMapCoord = gl_TexCoord[1].st / gl_TexCoord[1].q;' + LineEnding + 
'' + LineEnding + 
'  float d;' + LineEnding + 
'  /* When shadowMapCoord is outside (0, 0) - (1, 1) square, set d = 0.' + LineEnding + 
'     Otherwise texture would be visible stretched due to clamping. */' + LineEnding + 
'  if (shadowMapCoord.s < 0.0 || shadowMapCoord.s > 1.0 ||' + LineEnding + 
'      shadowMapCoord.t < 0.0 || shadowMapCoord.t > 1.0)' + LineEnding + 
'    d = 0.0; else' + LineEnding + 
'    d = texture2D(shadowMap, shadowMapCoord).x;' + LineEnding + 
'' + LineEnding + 
'  // d = pow(d, 4.0); // makes shadows a little more contrasting' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor = vec4(d, d, d, alpha);' + LineEnding + 
'}' + LineEnding + 
''
