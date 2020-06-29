{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/common.fs" }
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
'' + LineEnding +
'// Declare functions defined in tone_mapping.fs' + LineEnding +
'vec4 castle_texture_color_to_linear(const in vec4 srgbIn);' + LineEnding +
'vec3 castle_texture_color_to_linear(const in vec3 srgbIn);' + LineEnding +
'vec3 castle_linear_to_screen(const in vec3 color);' + LineEnding +
''
