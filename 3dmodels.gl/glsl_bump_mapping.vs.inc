{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "glsl_bump_mapping.vs" }
'/* GLSL vertex shader to do bump mapping.' + LineEnding + 
'' + LineEnding + 
'   This is converted to glsl_bump_mapping.vs.inc, and is them compiled' + LineEnding + 
'   in program''s binary.' + LineEnding + 
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding + 
'*/' + LineEnding + 
'' + LineEnding + 
'uniform vec3 light_position_world_space;' + LineEnding + 
'uniform mat4 world_space_to_object;' + LineEnding + 
'' + LineEnding + 
'attribute mat3 object_space_to_tangent;' + LineEnding + 
'' + LineEnding + 
'varying vec3 light_dir_tangent;' + LineEnding + 
'' + LineEnding + 
'void main(void)' + LineEnding + 
'{' + LineEnding + 
'  gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;' + LineEnding + 
'' + LineEnding + 
'  /* Calculate light_dir_tangent, which is crucial for bump mapping.' + LineEnding + 
'     This in some way does the same as LightDirectionInTangentSpace' + LineEnding + 
'     function in VRMLOpenGLRenderer, but this works in shader. */' + LineEnding + 
'  vec3 light_position_object_space =' + LineEnding + 
'    vec3(world_space_to_object * vec4(light_position_world_space, 1));' + LineEnding + 
'  vec3 light_dir_object_space = light_position_object_space - vec3(gl_Vertex);' + LineEnding + 
'  light_dir_tangent = object_space_to_tangent * light_dir_object_space;' + LineEnding + 
'' + LineEnding + 
'  gl_Position = ftransform();' + LineEnding + 
'}' + LineEnding + 
''
