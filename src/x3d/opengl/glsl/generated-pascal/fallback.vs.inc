{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/fallback.vs" }
'/* Fallback GLSL vertex shader.' + LineEnding +
'' + LineEnding +
'   Used when we need a shader (e.g. because it is OpenGLES or' + LineEnding +
'   GLFeatures.EnableFixedFunction = false),' + LineEnding +
'   but the default shader did not compile.' + LineEnding +
'   You cannot leave the shader empty in OpenGLES.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewMatrix;' + LineEnding +
'uniform mat4 castle_ProjectionMatrix;' + LineEnding +
'attribute vec4 castle_Vertex;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = castle_ProjectionMatrix * (castle_ModelViewMatrix * castle_Vertex);' + LineEnding +
'}' + LineEnding +
''
