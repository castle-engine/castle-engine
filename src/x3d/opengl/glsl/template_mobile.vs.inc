{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_mobile.vs" }
'/* Generic GLSL vertex shader, used on OpenGL ES. */' + LineEnding +
'' + LineEnding +
'uniform mat4 castle_ModelViewMatrix;' + LineEnding +
'uniform mat4 castle_ProjectionMatrix;' + LineEnding +
'attribute vec4 castle_Vertex;' + LineEnding +
'' + LineEnding +
'/* PLUG-DECLARATIONS */' + LineEnding +
'' + LineEnding +
'varying vec4 castle_vertex_eye;' + LineEnding +
'//varying vec3 castle_normal_eye;' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  vec4 vertex_object = castle_Vertex;' + LineEnding +
'  //vec3 normal_object = castle_Normal;' + LineEnding +
'  /* PLUG: vertex_object_space_change (vertex_object, normal_object) */' + LineEnding +
'  /* PLUG: vertex_object_space (vertex_object, normal_object) */' + LineEnding +
'' + LineEnding +
'  castle_vertex_eye = castle_ModelViewMatrix * vertex_object;' + LineEnding +
'  //castle_normal_eye = normalize(castle_NormalMatrix * normal_object);' + LineEnding +
'' + LineEnding +
'  /* PLUG: vertex_eye_space (castle_vertex_eye, castle_normal_eye) */' + LineEnding +
'' + LineEnding +
'  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;' + LineEnding +
'}' + LineEnding +
''
