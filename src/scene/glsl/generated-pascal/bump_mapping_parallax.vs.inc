{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/bump_mapping_parallax.vs" }
'/* Parallax bump mapping shader effect.' + LineEnding +
'   This is added right after bump_mapping.fs.' + LineEnding +
'   Included by EnableShaderBumpMapping in ../castlerendererinternalshader.pas unit.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'/* Make castleTranspose() available.' + LineEnding +
'' + LineEnding +
'   For desktop OpenGL, we could use "#version 120" and get transpose().' + LineEnding +
'' + LineEnding +
'   But on OpenGLES, it seems easiest to just implement your own transpose().' + LineEnding +
'   Or we could require version 3.00,' + LineEnding +
'   https://www.khronos.org/registry/OpenGL-Refpages/es3.1/html/transpose.xhtml ,' + LineEnding +
'   but this also requires other GLSL changes,' + LineEnding +
'   and it seems that iOS would be troublesome anyway:' + LineEnding +
'   https://stackoverflow.com/questions/18034677/transpose-a-mat4-in-opengl-es-2-0-glsl' + LineEnding +
'*/' + LineEnding +
'mat3 castleTranspose(const in mat3 m)' + LineEnding +
'{' + LineEnding +
'  return mat3(' + LineEnding +
'    vec3(m[0].x, m[1].x, m[2].x),' + LineEnding +
'    vec3(m[0].y, m[1].y, m[2].y),' + LineEnding +
'    vec3(m[0].z, m[1].z, m[2].z)' + LineEnding +
'  );' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// avoid redeclaring variables when no "separate compilation units" available (OpenGLES)' + LineEnding +
'#ifndef GL_ES' + LineEnding +
'uniform mat4 castle_ModelViewMatrix;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'varying vec3 castle_vertex_to_eye_in_tangent_space;' + LineEnding +
'' + LineEnding +
'void PLUG_bump_mapping_tangent_space_calculations(' + LineEnding +
'  const in vec4 vertex_eye, const in mat3 tangent_to_object_space)' + LineEnding +
'{' + LineEnding +
'  mat3 object_to_tangent_space = castleTranspose(tangent_to_object_space);' + LineEnding +
'  mat3 eye_to_object_space = mat3(' + LineEnding +
'    castle_ModelViewMatrix[0][0], castle_ModelViewMatrix[1][0], castle_ModelViewMatrix[2][0],' + LineEnding +
'    castle_ModelViewMatrix[0][1], castle_ModelViewMatrix[1][1], castle_ModelViewMatrix[2][1],' + LineEnding +
'    castle_ModelViewMatrix[0][2], castle_ModelViewMatrix[1][2], castle_ModelViewMatrix[2][2]);' + LineEnding +
'  mat3 eye_to_tangent_space = object_to_tangent_space * eye_to_object_space;' + LineEnding +
'  /* Theoretically faster implementation below, not fully correct ---' + LineEnding +
'     assume that transpose is enough to invert this matrix. Tests proved:' + LineEnding +
'     - results seem the same' + LineEnding +
'     - but it''s not really faster. */' + LineEnding +
'  // mat3 eye_to_tangent_space = castleTranspose(castle_tangent_to_eye_space);' + LineEnding +
'  castle_vertex_to_eye_in_tangent_space = normalize(eye_to_tangent_space * (-vec3(vertex_eye)) );' + LineEnding +
'' + LineEnding +
'  /* PLUG: bump_mapping_parallax_tangent_space_calculations (vertex_eye, eye_to_tangent_space) */' + LineEnding +
'}' + LineEnding +
''
