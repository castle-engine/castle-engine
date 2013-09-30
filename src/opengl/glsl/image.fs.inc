{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "image.fs" }
'varying vec2 tex_coord_frag;' + LineEnding +
'uniform sampler2D texture;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_FragColor = texture2D(texture, tex_coord_frag);' + LineEnding +
'#ifdef ALPHA_TEST' + LineEnding +
'  if (gl_FragColor.a < 0.5) discard;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
''
