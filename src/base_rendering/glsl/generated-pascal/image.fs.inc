{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/image.fs" }
'varying vec2 tex_coord_frag;' + LineEnding +
'uniform sampler2D image_texture;' + LineEnding +
'' + LineEnding +
'#ifdef COLOR_UNIFORM' + LineEnding +
'uniform vec4 color;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef CLIP_LINE' + LineEnding +
'uniform vec3 clip_line;' + LineEnding +
'/* Not using "gl_FragCoord​.xy / gl_FragCoord​.w", for unknown reason it doesn''t' + LineEnding +
' * compile (tested on Mesa OpenGL ES, and Nexus 5 OpenGL ES). */' + LineEnding +
'varying vec2 frag_coord;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'#ifdef COLOR_UNIFORM' + LineEnding +
'  gl_FragColor = color;' + LineEnding +
'#ifdef TEXTURE_HAS_ONLY_ALPHA' + LineEnding +
'  gl_FragColor.a *= texture2D(image_texture, tex_coord_frag).a;' + LineEnding +
'#else' + LineEnding +
'  gl_FragColor *= texture2D(image_texture, tex_coord_frag);' + LineEnding +
'#endif' + LineEnding +
'#else' + LineEnding +
'  gl_FragColor = texture2D(image_texture, tex_coord_frag);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef ALPHA_TEST' + LineEnding +
'  if (gl_FragColor.a < 0.5) discard;' + LineEnding +
'#endif' + LineEnding +
'#ifdef CLIP_LINE' + LineEnding +
'  if (dot(clip_line.xy, tex_coord_frag) + clip_line.z < 0.0) discard;' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
''
