{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/terrain_edit_brush.vs" }
'attribute vec2 vertex;' + LineEnding +
'attribute vec2 tex_coord;' + LineEnding +
'uniform vec2 viewport_size;' + LineEnding +
'varying vec2 tex_coord_frag;' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  gl_Position = vec4(vertex * 2.0 / viewport_size - vec2(1.0), 0.0, 1.0);' + LineEnding +
'  tex_coord_frag = tex_coord;' + LineEnding +
'}' + LineEnding +
''
