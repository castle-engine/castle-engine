{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "image.fs" }
'#ifdef GL_ES' + LineEnding +
'precision mediump float;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'varying vec2 tex_coord_frag;' + LineEnding +
'uniform sampler2D texture;' + LineEnding +
'' + LineEnding +
'#ifdef COLOR_UNIFORM' + LineEnding +
'uniform vec4 color;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'#ifdef COLOR_UNIFORM' + LineEnding +
'  gl_FragColor = color;' + LineEnding +
'#ifdef TEXTURE_HAS_ONLY_ALPHA' + LineEnding +
'  gl_FragColor.a *= texture2D(texture, tex_coord_frag).a;' + LineEnding +
'#else' + LineEnding +
'  gl_FragColor *= texture2D(texture, tex_coord_frag);' + LineEnding +
'#endif' + LineEnding +
'#else' + LineEnding +
'  gl_FragColor = texture2D(texture, tex_coord_frag);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef ALPHA_TEST' + LineEnding +
'  if (gl_FragColor.a < 0.5) discard;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
''
