{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template.gs" }
'/* Our implementation supports only geometry shaders for GLSL >= 1.50.' + LineEnding + 
'   Add "compatibility" profile, as (for now) we want to be able to pass' + LineEnding + 
'   also deprecated gl_Xxx from vertex to fragment stages. */' + LineEnding + 
'' + LineEnding + 
'#version 150 compatibility' + LineEnding + 
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
