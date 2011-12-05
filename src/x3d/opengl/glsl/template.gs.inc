{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template.gs" }
'/* Default geometry shader code.' + LineEnding + 
'   Defines geometryVertexXxx functions,' + LineEnding + 
'   to be called from user code, and respective geometry_vertex_xxx plugs,' + LineEnding + 
'   to be extended by user (and internal) effects.' + LineEnding + 
'' + LineEnding + 
'   It does not define the main() entry point for geometry shaders.' + LineEnding + 
'   User will have to provide it (in ComposedShader or Effect),' + LineEnding + 
'   or we will discard partial geometry shader code. */' + LineEnding + 
'' + LineEnding + 
'/* Our implementation supports only geometry shaders for GLSL >= 1.50.' + LineEnding + 
'   Add "compatibility" profile, as (for now) we want to be able to pass' + LineEnding + 
'   also deprecated gl_Xxx from vertex to fragment stages. */' + LineEnding + 
'#version 150 compatibility' + LineEnding + 
'' + LineEnding + 
'/* PLUG-DECLARATIONS */' + LineEnding + 
'' + LineEnding + 
'/* Standard varying, always used by our shaders */' + LineEnding + 
'in vec4 castle_vertex_eye[CASTLE_GEOMETRY_INPUT_SIZE];' + LineEnding + 
'out vec4 castle_vertex_eye_geoshader;' + LineEnding + 
'in vec3 castle_normal_eye[CASTLE_GEOMETRY_INPUT_SIZE];' + LineEnding + 
'out vec3 castle_normal_eye_geoshader;' + LineEnding + 
'' + LineEnding + 
'void geometryVertexSet(const int index)' + LineEnding + 
'{' + LineEnding + 
'  castle_vertex_eye_geoshader = castle_vertex_eye[index];' + LineEnding + 
'  castle_normal_eye_geoshader = castle_normal_eye[index];' + LineEnding + 
'  /* PLUG: geometry_vertex_set (index) */' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'void geometryVertexZero()' + LineEnding + 
'{' + LineEnding + 
'  castle_vertex_eye_geoshader = vec4(0.0);' + LineEnding + 
'  castle_normal_eye_geoshader = vec3(0.0);' + LineEnding + 
'  /* PLUG: geometry_vertex_zero () */' + LineEnding + 
'}' + LineEnding + 
'' + LineEnding + 
'void geometryVertexAdd(const int index, const float scale)' + LineEnding + 
'{' + LineEnding + 
'  castle_vertex_eye_geoshader += castle_vertex_eye[index] * scale;' + LineEnding + 
'  castle_normal_eye_geoshader += castle_normal_eye[index] * scale;' + LineEnding + 
'  /* PLUG: geometry_vertex_add (index, scale) */' + LineEnding + 
'}' + LineEnding + 
''
