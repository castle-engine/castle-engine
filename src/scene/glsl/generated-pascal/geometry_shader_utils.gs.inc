{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/geometry_shader_utils.gs" }
'/* Default geometry shader code.' + LineEnding +
'   Defines geometryVertexXxx functions,' + LineEnding +
'   to be called from user code, and respective geometry_vertex_xxx plugs,' + LineEnding +
'   to be extended by user (and internal) effects.' + LineEnding +
'' + LineEnding +
'   It does not define the main() entry point for geometry shaders.' + LineEnding +
'   User will have to provide it (in ComposedShader or Effect). */' + LineEnding +
'' + LineEnding +
'/* SHADER-MOVE-TO-LAST */' + LineEnding +
'' + LineEnding +
'/* PLUG-DECLARATIONS */' + LineEnding +
'' + LineEnding +
'void geometryVertexSet(const int index)' + LineEnding +
'{' + LineEnding +
'  /* PLUG: geometry_vertex_set (index) */' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void geometryVertexZero()' + LineEnding +
'{' + LineEnding +
'  /* PLUG: geometry_vertex_zero () */' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void geometryVertexAdd(const int index, const float scale)' + LineEnding +
'{' + LineEnding +
'  /* PLUG: geometry_vertex_add (index, scale) */' + LineEnding +
'}' + LineEnding +
''
