{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/fallback.fs" }
'/* Fallback GLSL fragment shader,' + LineEnding +
'   used when we need a shader (e.g. because it is OpenGLES or' + LineEnding +
'   GLFeatures.EnableFixedFunction = false)' + LineEnding +
'   but the default shader did not compile.' + LineEnding +
'   You cannot leave the shader empty in OpenGLES. */' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0); // ugly color to stand out' + LineEnding +
'}' + LineEnding +
''
