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
'//  gl_TexCoord[0] = gl_TextureMatrix[0] * vec4(tex_coord, 0.0, 1.0);' + LineEnding + 
'  vertex_eye = gl_ModelViewMatrix * gl_Vertex;' + LineEnding + 
'  /* Yes, we need to normalize here,' + LineEnding + 
'     and then fragment shader will also normalize?' + LineEnding + 
'     TODO: think, make sure. */' + LineEnding + 
'  normal_eye = normalize(gl_NormalMatrix * gl_Normal);' + LineEnding + 
'  gl_Position = ftransform();' + LineEnding + 
'}' + LineEnding + 
''
