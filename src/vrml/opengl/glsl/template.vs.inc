{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template.vs" }
'/* Generic GLSL vertex shader template. It will be used' + LineEnding + 
'   by vrmlshadergenerator.pas to construct final shader.' + LineEnding + 
'' + LineEnding + 
'   This is converted to template.vs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'/* PLUG: $declare-variables$ */' + LineEnding + 
'/* PLUG: $declare-procedures$ */' + LineEnding + 
'' + LineEnding + 
'varying vec4 vertex_eye;' + LineEnding + 
'varying vec3 normal_eye;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  vec4 vertex_object = gl_Vertex;' + LineEnding + 
'  vec3 normal_object = gl_Normal;' + LineEnding + 
'  /* PLUG: vertex_object_space (vertex_object, normal_object) */' + LineEnding + 
'' + LineEnding + 
'  vertex_eye = gl_ModelViewMatrix * vertex_object;' + LineEnding + 
'  /* Yes, we need to normalize here,' + LineEnding + 
'     and then fragment shader will also normalize?' + LineEnding + 
'     TODO: think, make sure. */' + LineEnding + 
'  normal_eye = normalize(gl_NormalMatrix * gl_Normal);' + LineEnding + 
'' + LineEnding + 
'  /* PLUG: vertex_process (vertex_eye, normal_eye) */' + LineEnding + 
'  /* (const in vec4 vertex_eye, const in vec3 normal_eye) */' + LineEnding + 
'' + LineEnding + 
'  gl_Position = ftransform();' + LineEnding + 
'}' + LineEnding + 
''
