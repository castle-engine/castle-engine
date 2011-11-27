{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "variance_shadow_map_generate.fs" }
'#ifdef ALPHA_TEST' + LineEnding + 
'uniform sampler2D tex;' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'' + LineEnding + 
'#ifdef ALPHA_TEST' + LineEnding + 
'  /* work with alpha test textures.' + LineEnding + 
'     For now we ssume the texture is only in 0th channel. */' + LineEnding + 
'  if (texture2D(tex, gl_TexCoord[0].st).a < 0.5)' + LineEnding + 
'    discard;' + LineEnding + 
'#endif' + LineEnding + 
'' + LineEnding + 
'  /* No need to divide gl_FragCoord.z by gl_FragCoord.w,' + LineEnding + 
'     as far as I understand GLSL spec. */' + LineEnding + 
'  gl_FragColor = vec4(gl_FragCoord.z, gl_FragCoord.z * gl_FragCoord.z, 0.0, 1.0);' + LineEnding + 
'}' + LineEnding + 
''
