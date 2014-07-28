{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template.vs" }
'/* Generic GLSL vertex shader.' + LineEnding +
'   Used by ../castlerendererinternalshader.pas to construct the final shader.' + LineEnding +
'' + LineEnding +
'   This is converted to template.vs.inc, and is then compiled' + LineEnding +
'   in program''s binary.' + LineEnding +
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'/* PLUG-DECLARATIONS */' + LineEnding +
'' + LineEnding +
'varying vec4 castle_vertex_eye;' + LineEnding +
'varying vec3 castle_normal_eye;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  vec4 vertex_object = gl_Vertex;' + LineEnding +
'  vec3 normal_object = gl_Normal;' + LineEnding +
'  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */' + LineEnding +
'  /* PLUG: vertex_object_space (vertex_object, normal_object) */' + LineEnding +
'' + LineEnding +
'  castle_vertex_eye = gl_ModelViewMatrix * vertex_object;' + LineEnding +
'  /* Although we will normalize it again in the fragment shader' + LineEnding +
'     (otherwise interpolated result could be shorter < 1, imagine a case' + LineEnding +
'     when normals point the almost opposite directions on the opposite' + LineEnding +
'     vertexes), we also have to normalize it in vertex shader (otherwise' + LineEnding +
'     a much longer normal on one vertex would pull all the interpolated' + LineEnding +
'     normals, thus making their direction invalid in fragment shaders). */' + LineEnding +
'  castle_normal_eye = normalize(gl_NormalMatrix * normal_object);' + LineEnding +
'' + LineEnding +
'  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */' + LineEnding +
'' + LineEnding +
'#ifndef LIT' + LineEnding +
'  gl_FrontColor = gl_Color;' + LineEnding +
'  gl_BackColor = gl_Color;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef VERTEX_OBJECT_SPACE_CHANGED' + LineEnding +
'  gl_Position = gl_ProjectionMatrix * castle_vertex_eye;' + LineEnding +
'#else' + LineEnding +
'  gl_Position = ftransform();' + LineEnding +
'#endif' + LineEnding +
'}' + LineEnding +
''
