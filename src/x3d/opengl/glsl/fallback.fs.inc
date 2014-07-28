{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "fallback.fs" }
'/* Fallback GLSL shader, used for OpenGL ES, when normal shader did not compile.' + LineEnding +
'   This is merely used to avoid crashing the application (you cannot leave' + LineEnding +
'   the shader empty). */' + LineEnding +
'' + LineEnding +
'precision mediump float;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0); // ugly color to stand out' + LineEnding +
'}' + LineEnding +
''
