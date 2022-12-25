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
'' + LineEnding +
'/* sRGB to linear conversion.' + LineEnding +
'   Leaves alpha unmodified.' + LineEnding +
'' + LineEnding +
'   Input is returned unchanged if CASTLE_GAMMA_CORRECTION not defined.' + LineEnding +
'*/' + LineEnding +
'vec4 castle_texture_color_to_linear(const in vec4 srgbIn);' + LineEnding +
'' + LineEnding +
'/* sRGB to linear conversion.' + LineEnding +
'   Input is returned unchanged if CASTLE_GAMMA_CORRECTION not defined.' + LineEnding +
'*/' + LineEnding +
'vec3 castle_texture_color_to_linear(const in vec3 srgbIn);' + LineEnding +
'' + LineEnding +
'/* Linear to sRGB conversion and applying tone mapping.' + LineEnding +
'   Input is returned unchanged if' + LineEnding +
'   neither CASTLE_GAMMA_CORRECTION nor CASTLE_TONE_MAPPING are defined.' + LineEnding +
'*/' + LineEnding +
'vec3 castle_linear_to_screen(const in vec3 color);' + LineEnding +
''
