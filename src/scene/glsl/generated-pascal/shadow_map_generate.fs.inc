{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/shadow_map_generate.fs" }
'/* Fragment shader when generating shadow maps. */' + LineEnding +
'' + LineEnding +
'#ifdef ALPHA_TEST' + LineEnding +
'uniform sampler2D castle_texture_0;' + LineEnding +
'varying vec2 castle_TexCoord0_XY;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  #ifdef ALPHA_TEST' + LineEnding +
'  /* Alpha test texture.' + LineEnding +
'     TODO: This assumes the alpha-tested texture is 2D and is in 0th channel. */' + LineEnding +
'  if (texture2D(castle_texture_0, castle_TexCoord0_XY).a < 0.5)' + LineEnding +
'    discard;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  #ifdef VARIANCE_SHADOW_MAPS' + LineEnding +
'  /* Encode depth is format necessary for Variance Shadow Maps.' + LineEnding +
'' + LineEnding +
'     Note: No need to divide gl_FragCoord.z by gl_FragCoord.w,' + LineEnding +
'     as far as I understand GLSL spec. */' + LineEnding +
'  gl_FragColor = vec4(gl_FragCoord.z, gl_FragCoord.z * gl_FragCoord.z, 0.0, 1.0);' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  /* For classic shadow maps (not Variance Shadow Maps),' + LineEnding +
'     it doesn''t matter what we write to the color buffer. */' + LineEnding +
'  // gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);' + LineEnding +
'}' + LineEnding +
''
