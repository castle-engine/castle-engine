{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "screen_effect.vs" }
'/* Simple GLSL shader to apply 2D screen quad.' + LineEnding +
'   Must be suitable also for GLES20, so don''t use any deprecated gl_Xxx' + LineEnding +
'   variables. */' + LineEnding +
'' + LineEnding +
'attribute vec2 vertex; // must be already in clip coordinates, -1..1' + LineEnding +
'attribute vec2 tex_coord;' + LineEnding +
'varying vec2 tex_coord_frag;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = vec4(vertex, 0.0, 1.0);' + LineEnding +
'  tex_coord_frag = tex_coord;' + LineEnding +
'}' + LineEnding +
''
