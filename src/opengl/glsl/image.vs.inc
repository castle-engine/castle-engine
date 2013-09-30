{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "image.vs" }
'attribute vec2 vertex;' + LineEnding +
'attribute vec2 tex_coord;' + LineEnding +
'uniform mat4 projection_matrix;' + LineEnding +
'varying vec2 tex_coord_frag;' + LineEnding +
'' + LineEnding +
'/* Simple GLSL shader to apply 2D texture.' + LineEnding +
'   Must be suitable also for GLES20, so don''t use any deprecated gl_Xxx' + LineEnding +
'   variables. */' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = projection_matrix * vec4(vertex, 0.0, 1.0);' + LineEnding +
'  tex_coord_frag = tex_coord;' + LineEnding +
'}' + LineEnding +
''
