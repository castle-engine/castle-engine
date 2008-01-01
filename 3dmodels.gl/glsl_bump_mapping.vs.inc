{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "glsl_bump_mapping.vs" }
'/* GLSL vertex shader to do bump mapping.' + LineEnding + 
'' + LineEnding + 
'   This is converted to glsl_bump_mapping.vs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'/* TODO: calc light_dir_tangent_v from tangents and lightpos' + LineEnding + 
'   VRMLOpenGLRenderer.Attributes */' + LineEnding + 
'' + LineEnding + 
'attribute vec3 light_position_object_space;' + LineEnding + 
'attribute mat3 object_space_to_tangent;' + LineEnding + 
'varying vec3 light_dir_tangent;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;' + LineEnding + 
'' + LineEnding + 
'  /* Calculate light_dir_tangent, which is crucial for bump mapping.' + LineEnding + 
'     This in some way does the same as LightDirectionInTangentSpace' + LineEnding + 
'     function in VRMLOpenGLRenderer, but this works in shader. */' + LineEnding + 
'  vec3 light_dir_object_space = light_position_object_space - gl_Vertex;' + LineEnding + 
'  light_dir_tangent = object_space_to_tangent * light_dir_object_space;' + LineEnding + 
'  light_dir_tangent.y = -light_dir_tangent.y;' + LineEnding + 
'' + LineEnding + 
'  gl_Position = ftransform();' + LineEnding + 
'}' + LineEnding + 
''
