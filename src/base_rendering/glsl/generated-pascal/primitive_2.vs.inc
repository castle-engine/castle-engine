{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "primitive_2.vs" }
'attribute vec2 vertex;' + LineEnding +
'uniform vec2 viewport_size;' + LineEnding +
'' + LineEnding +
'/* Simple GLSL shader to draw 2D primitive (like a rectangle). */' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = vec4(vertex * 2.0 / viewport_size - vec2(1.0), 0.0, 1.0);' + LineEnding +
'}' + LineEnding +
''
