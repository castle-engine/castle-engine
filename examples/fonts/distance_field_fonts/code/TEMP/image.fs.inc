{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/image.fs" }
'varying vec2 tex_coord_frag;' + LineEnding +
'uniform sampler2D image_texture;' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'gl_FragColor = texture2D(image_texture, tex_coord_frag);' + LineEnding +
'if (gl_FragColor.x < 0.75) discard; else gl_FragColor = vec4(1.0,1.0,1.0,1.0);' +
'}' + LineEnding +
''
