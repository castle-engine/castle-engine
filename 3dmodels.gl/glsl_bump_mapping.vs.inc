{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "glsl_bump_mapping.vs" }
'/* GLSL vertex shader to do bump mapping.' + LineEnding + 
'' + LineEnding + 
'   This is converted to glsl_bump_mapping.vs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'varying vec3 interpolated_normal;' + LineEnding + 
'varying vec3 vertex_world;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  /* Is normalization here needed ? Probably not, but it improves the' + LineEnding + 
'     numerical precision of interpolation ? */' + LineEnding + 
'  interpolated_normal = normalize(gl_NormalMatrix * gl_Normal);' + LineEnding + 
'' + LineEnding + 
'  vertex_world = vec3(gl_ModelViewMatrix * gl_Vertex);' + LineEnding + 
'' + LineEnding + 
'  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;' + LineEnding + 
'' + LineEnding + 
'  gl_Position = ftransform();' + LineEnding + 
'}' + LineEnding + 
''
