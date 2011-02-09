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
'varying vec4 vertex_eye;' + LineEnding + 
'varying vec3 normal_eye;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  vertex_eye = gl_ModelViewMatrix * gl_Vertex;' + LineEnding + 
'  /* Yes, we need to normalize here,' + LineEnding + 
'     and then fragment shader will also normalize?' + LineEnding + 
'     TODO: think, make sure. */' + LineEnding + 
'  normal_eye = normalize(gl_NormalMatrix * gl_Normal);' + LineEnding + 
'' + LineEnding + 
'  /* This may use vertex_eye for texgen */' + LineEnding + 
'  /* *** VERTEX-PROCESSING *** */' + LineEnding + 
'' + LineEnding + 
'  gl_Position = ftransform();' + LineEnding + 
'}' + LineEnding + 
''
