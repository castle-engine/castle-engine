{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "shadow_map_1_show_depth.fs" }
'/* Debug shader, to display depth (distances) of the texture directly.' + LineEnding + 
'' + LineEnding + 
'   Just' + LineEnding + 
'   1. replace standard shadow_map_1.fs with this in VRML/X3D source' + LineEnding + 
'   2. use compareMode "NONE" inside GeneratedShadowMap (otherwise getting' + LineEnding + 
'      it as sampler2D may not be sensible, depends on GPU)' + LineEnding + 
'   to see the depths.' + LineEnding + 
'' + LineEnding + 
'   Or just define VISUALIZE_SHADOW_MAP_DEPTHS inside sunny_street_process.lpr' + LineEnding + 
'   and "make", and all will be done for you.' + LineEnding + 
'' + LineEnding + 
'   Note: it''s possible to see the depths without the GLSL at all too:' + LineEnding + 
'   1. use compareMode "NONE" inside GeneratedShadowMap' + LineEnding + 
'   2. to avoid mixing with the normal color texture and material' + LineEnding + 
'      (that make it difficult to actually see the depth variations)' + LineEnding + 
'      you can use "REPLACE" mode mapping with GeneratedShadowMap.' + LineEnding + 
'' + LineEnding + 
'      For example, replace all' + LineEnding + 
'        texture MultiTexture {' + LineEnding + 
'      with' + LineEnding + 
'        texture MultiTexture { mode [ "REPLACE / MODULATE", "REPLACE / MODULATE" ]' + LineEnding + 
'      inside sunny_street_processed.x3dv.' + LineEnding + 
'      By using "REPLACE / MODULATE" instead of "REPLACE" we allow to' + LineEnding + 
'      modulate alpha (and only replace RGB channels), this allows' + LineEnding + 
'      alpha-test texture of the leaves still work Ok.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'uniform sampler2D texture0;' + LineEnding + 
'uniform sampler2D shadowMap;' + LineEnding + 
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
'    d = texture2D(shadowMap, shadowMapCoord).z;' + LineEnding + 
'' + LineEnding + 
'  // d = pow(d, 4.0); // makes shadows a little more contrasting' + LineEnding + 
'' + LineEnding + 
'  gl_FragColor = vec4(d, d, d, alpha);' + LineEnding + 
'}' + LineEnding + 
''
