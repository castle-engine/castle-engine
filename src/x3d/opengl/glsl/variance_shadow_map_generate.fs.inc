{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "variance_shadow_map_generate.fs" }
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  /* No need to divide gl_FragCoord.z by gl_FragCoord.w,' + LineEnding + 
'     as far as I understand GLSL spec. */' + LineEnding + 
'  gl_FragColor = vec4(gl_FragCoord.z, gl_FragCoord.z * gl_FragCoord.z, 0.0, 1.0);' + LineEnding + 
'}' + LineEnding + 
''
