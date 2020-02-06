{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_phong.vs" }
'/* Phong shading GLSL vertex shader.' + LineEnding +
'   Used by ../castlerendererinternalshader.pas to construct the final shader.' + LineEnding +
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewMatrix;' + LineEnding +
'uniform mat4 castle_ProjectionMatrix;' + LineEnding +
'uniform mat3 castle_NormalMatrix;' + LineEnding +
'attribute vec4 castle_Vertex;' + LineEnding +
'attribute vec3 castle_Normal;' + LineEnding +
'' + LineEnding +
'/* PLUG-DECLARATIONS */' + LineEnding +
'' + LineEnding +
'varying vec4 castle_vertex_eye;' + LineEnding +
'varying vec3 castle_normal_eye;' + LineEnding +
'' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'attribute vec4 castle_ColorPerVertex;' + LineEnding +
'varying vec4 castle_ColorPerVertexFragment;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'/* Include fragment shader utilities used by both Gouraud and Phong shading. */' + LineEnding +
'/* CASTLE-COMMON-CODE */' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  #ifdef COLOR_PER_VERTEX' + LineEnding +
'  castle_ColorPerVertexFragment = castle_ColorPerVertex;' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  vec4 vertex_object = castle_Vertex;' + LineEnding +
'  vec3 normal_object = castle_Normal;' + LineEnding +
'  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */' + LineEnding +
'  /* PLUG: vertex_object_space (vertex_object, normal_object) */' + LineEnding +
'' + LineEnding +
'  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;' + LineEnding +
'  /* Although we will normalize it again in the fragment shader' + LineEnding +
'     (otherwise interpolated result could be shorter < 1, imagine a case' + LineEnding +
'     when normals point the almost opposite directions on the opposite' + LineEnding +
'     vertexes), we also have to normalize it in vertex shader (otherwise' + LineEnding +
'     a much longer normal on one vertex would pull all the interpolated' + LineEnding +
'     normals, thus making their direction invalid in fragment shaders). */' + LineEnding +
'  castle_normal_eye = normalize(castle_NormalMatrix * normal_object);' + LineEnding +
'' + LineEnding +
'  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */' + LineEnding +
'' + LineEnding +
'  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;' + LineEnding +
'}' + LineEnding +
''
