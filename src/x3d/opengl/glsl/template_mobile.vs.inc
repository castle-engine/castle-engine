{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_mobile.vs" }
'/* Generic GLSL vertex shader, used on OpenGL ES. */' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewProjectionMatrix;' + LineEnding +
'attribute vec3 castle_Vertex;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = castle_ModelViewProjectionMatrix * vec4(castle_Vertex, 1.0);' + LineEnding +
'}' + LineEnding +
''
