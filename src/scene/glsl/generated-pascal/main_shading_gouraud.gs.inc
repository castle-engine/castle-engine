{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/main_shading_gouraud.gs" }
'/* Additional code to make Gouraud shading work with geometry shaders.' + LineEnding +
'   We need to pass the castle_Color.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'in vec4 castle_Color[CASTLE_GEOMETRY_INPUT_SIZE];' + LineEnding +
'out vec4 castle_Color_geoshader;' + LineEnding +
'' + LineEnding +
'void PLUG_geometry_vertex_set(const int index)' + LineEnding +
'{' + LineEnding +
'  castle_Color_geoshader = castle_Color[index];' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void PLUG_geometry_vertex_zero()' + LineEnding +
'{' + LineEnding +
'  castle_Color_geoshader = vec4(0.0);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void PLUG_geometry_vertex_add(const int index, const float scale)' + LineEnding +
'{' + LineEnding +
'  castle_Color_geoshader += castle_Color[index] * scale;' + LineEnding +
'}' + LineEnding +
''
