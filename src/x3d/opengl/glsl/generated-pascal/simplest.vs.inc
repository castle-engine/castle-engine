{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/simplest.vs" }
'/* Simplest GLSL vertex shader.' + LineEnding +
'   Used also when we''re OK with the simplest shader that transforms' + LineEnding +
'   vertexes with projection + modelview and really nothing more.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewProjectionMatrix;' + LineEnding +
'attribute vec4 castle_Vertex;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = castle_ModelViewProjectionMatrix * castle_Vertex;' + LineEnding +
'}' + LineEnding +
''
