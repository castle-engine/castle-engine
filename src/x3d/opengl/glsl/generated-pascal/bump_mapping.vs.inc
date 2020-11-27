{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/bump_mapping.vs" }
'/* Bump mapping shader effect.' + LineEnding +
'   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'attribute vec3 castle_tangent;' + LineEnding +
'attribute vec3 castle_bitangent;' + LineEnding +
'varying mat3 castle_tangent_to_eye_space;' + LineEnding +
'' + LineEnding +
'// avoid redeclaring variables when no "separate compilation units" available (OpenGLES)' + LineEnding +
'#ifndef GL_ES' + LineEnding +
'uniform mat3 castle_NormalMatrix;' + LineEnding +
'attribute vec3 castle_Normal;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void PLUG_vertex_eye_space(const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  mat3 castle_tangent_to_object_space = mat3(' + LineEnding +
'    castle_tangent,' + LineEnding +
'    castle_bitangent,' + LineEnding +
'    castle_Normal' + LineEnding +
'  );' + LineEnding +
'  castle_tangent_to_eye_space = castle_NormalMatrix * castle_tangent_to_object_space;' + LineEnding +
'  /* PLUG: bump_mapping_tangent_space_calculations (vertex_eye, castle_tangent_to_object_space) */' + LineEnding +
'}' + LineEnding +
''
