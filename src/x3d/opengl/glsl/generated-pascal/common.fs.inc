{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "common.fs" }
'/* Fragment shader utilities used by both Gouraud and Phong shading.' + LineEnding +
'' + LineEnding +
'   Used by ../castlerendererinternalshader.pas to construct the final shader.' + LineEnding +
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'/* Wrapper for calling PLUG texture_coord_shift */' + LineEnding +
'vec2 texture_coord_shifted(in vec2 tex_coord)' + LineEnding +
'{' + LineEnding +
'  /* PLUG: texture_coord_shift (tex_coord) */' + LineEnding +
'  return tex_coord;' + LineEnding +
'}' + LineEnding +
''
