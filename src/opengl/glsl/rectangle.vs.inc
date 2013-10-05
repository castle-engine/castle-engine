{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "rectangle.vs" }
'attribute vec2 vertex;' + LineEnding +
'uniform mat4 projection_matrix;' + LineEnding +
'' + LineEnding +
'/* Simple GLSL shader to draw 2D rectangle. */' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = projection_matrix * vec4(vertex, 0.0, 1.0);' + LineEnding +
'}' + LineEnding +
''
